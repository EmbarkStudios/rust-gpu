//! SPIR-T pass infrastructure and supporting utilities.

pub(crate) mod diagnostics;
mod fuse_selects;
mod reduce;

use lazy_static::lazy_static;
use rustc_data_structures::fx::{FxHashMap, FxHashSet, FxIndexSet};
use spirt::func_at::FuncAt;
use spirt::transform::InnerInPlaceTransform;
use spirt::visit::{InnerVisit, Visitor};
use spirt::{
    spv, AttrSet, Const, Context, ControlNode, ControlNodeKind, ControlRegion, DataInstDef,
    DataInstKind, DeclDef, EntityOrientedDenseMap, Func, FuncDefBody, GlobalVar, Module, Type,
    Value,
};
use std::collections::VecDeque;
use std::hash::Hash;
use std::iter;

// HACK(eddyb) `spv::spec::Spec` with extra `WellKnown`s (that should be upstreamed).
macro_rules! def_spv_spec_with_extra_well_known {
    ($($group:ident: $ty:ty = [$($entry:ident),+ $(,)?]),+ $(,)?) => {
        struct SpvSpecWithExtras {
            __base_spec: &'static spv::spec::Spec,

            well_known: SpvWellKnownWithExtras,
        }

        #[allow(non_snake_case)]
        pub struct SpvWellKnownWithExtras {
            __base_well_known: &'static spv::spec::WellKnown,

            $($(pub $entry: $ty,)+)+
        }

        impl std::ops::Deref for SpvSpecWithExtras {
            type Target = spv::spec::Spec;
            fn deref(&self) -> &Self::Target {
                self.__base_spec
            }
        }

        impl std::ops::Deref for SpvWellKnownWithExtras {
            type Target = spv::spec::WellKnown;
            fn deref(&self) -> &Self::Target {
                self.__base_well_known
            }
        }

        impl SpvSpecWithExtras {
            #[inline(always)]
            #[must_use]
            pub fn get() -> &'static SpvSpecWithExtras {
                lazy_static! {
                    static ref SPEC: SpvSpecWithExtras = {
                        #[allow(non_camel_case_types)]
                        struct PerWellKnownGroup<$($group),+> {
                            $($group: $group),+
                        }

                        let spv_spec = spv::spec::Spec::get();
                        let wk = &spv_spec.well_known;

                        let decorations = match &spv_spec.operand_kinds[wk.Decoration] {
                            spv::spec::OperandKindDef::ValueEnum { variants } => variants,
                            _ => unreachable!(),
                        };

                        let lookup_fns = PerWellKnownGroup {
                            opcode: |name| spv_spec.instructions.lookup(name).unwrap(),
                            operand_kind: |name| spv_spec.operand_kinds.lookup(name).unwrap(),
                            decoration: |name| decorations.lookup(name).unwrap().into(),
                        };

                        SpvSpecWithExtras {
                            __base_spec: spv_spec,

                            well_known: SpvWellKnownWithExtras {
                                __base_well_known: &spv_spec.well_known,

                                $($($entry: (lookup_fns.$group)(stringify!($entry)),)+)+
                            },
                        }
                    };
                }
                &SPEC
            }
        }
    };
}
def_spv_spec_with_extra_well_known! {
    opcode: spv::spec::Opcode = [
        OpConstantComposite,

        OpBitcast,
        OpCompositeInsert,
        OpCompositeExtract,
    ],
    operand_kind: spv::spec::OperandKind = [
        ExecutionModel,
    ],
    decoration: u32 = [
        UserTypeGOOGLE,
    ],
}

/// Run intra-function passes on all `Func` definitions in the `Module`.
//
// FIXME(eddyb) introduce a proper "pass manager".
// FIXME(eddyb) why does this focus on functions, it could just be module passes??
pub(super) fn run_func_passes<P>(
    module: &mut Module,
    passes: &[impl AsRef<str>],
    // FIXME(eddyb) this is a very poor approximation of a "profiler" abstraction.
    mut before_pass: impl FnMut(&'static str, &Module) -> P,
    mut after_pass: impl FnMut(&'static str, &Module, P),
) {
    let cx = &module.cx();

    // FIXME(eddyb) reuse this collection work in some kind of "pass manager".
    let all_funcs = {
        let mut collector = ReachableUseCollector {
            cx,
            module,

            seen_types: FxIndexSet::default(),
            seen_consts: FxIndexSet::default(),
            seen_global_vars: FxIndexSet::default(),
            seen_funcs: FxIndexSet::default(),
        };
        for (export_key, &exportee) in &module.exports {
            export_key.inner_visit_with(&mut collector);
            exportee.inner_visit_with(&mut collector);
        }
        collector.seen_funcs
    };

    for name in passes {
        let name = name.as_ref();

        // HACK(eddyb) not really a function pass.
        if name == "qptr" {
            let layout_config = &spirt::qptr::LayoutConfig {
                abstract_bool_size_align: (1, 1),
                logical_ptr_size_align: (4, 4),
                ..spirt::qptr::LayoutConfig::VULKAN_SCALAR_LAYOUT
            };

            let profiler = before_pass("qptr::lower_from_spv_ptrs", module);
            spirt::passes::qptr::lower_from_spv_ptrs(module, layout_config);
            after_pass("qptr::lower_from_spv_ptrs", module, profiler);

            let profiler = before_pass("qptr::analyze_uses", module);
            spirt::passes::qptr::analyze_uses(module, layout_config);
            after_pass("qptr::analyze_uses", module, profiler);

            let profiler = before_pass("qptr::lift_to_spv_ptrs", module);
            spirt::passes::qptr::lift_to_spv_ptrs(module, layout_config);
            after_pass("qptr::lift_to_spv_ptrs", module, profiler);

            continue;
        }

        let (full_name, pass_fn): (_, fn(_, &mut _)) = match name {
            "reduce" => ("spirt_passes::reduce", reduce::reduce_in_func),
            "fuse_selects" => (
                "spirt_passes::fuse_selects",
                fuse_selects::fuse_selects_in_func,
            ),
            _ => panic!("unknown `--spirt-passes={name}`"),
        };

        let profiler = before_pass(full_name, module);
        for &func in &all_funcs {
            if let DeclDef::Present(func_def_body) = &mut module.funcs[func].def {
                pass_fn(cx, func_def_body);

                // FIXME(eddyb) avoid doing this except where changes occurred.
                remove_unused_values_in_func(func_def_body);
            }
        }
        after_pass(full_name, module, profiler);
    }
}

// FIXME(eddyb) this is just copy-pasted from `spirt` and should be reusable.
struct ReachableUseCollector<'a> {
    cx: &'a Context,
    module: &'a Module,

    // FIXME(eddyb) build some automation to avoid ever repeating these.
    seen_types: FxIndexSet<Type>,
    seen_consts: FxIndexSet<Const>,
    seen_global_vars: FxIndexSet<GlobalVar>,
    seen_funcs: FxIndexSet<Func>,
}

impl Visitor<'_> for ReachableUseCollector<'_> {
    // FIXME(eddyb) build some automation to avoid ever repeating these.
    fn visit_attr_set_use(&mut self, _attrs: AttrSet) {}
    fn visit_type_use(&mut self, ty: Type) {
        if self.seen_types.insert(ty) {
            self.visit_type_def(&self.cx[ty]);
        }
    }
    fn visit_const_use(&mut self, ct: Const) {
        if self.seen_consts.insert(ct) {
            self.visit_const_def(&self.cx[ct]);
        }
    }

    fn visit_global_var_use(&mut self, gv: GlobalVar) {
        if self.seen_global_vars.insert(gv) {
            self.visit_global_var_decl(&self.module.global_vars[gv]);
        }
    }
    fn visit_func_use(&mut self, func: Func) {
        if self.seen_funcs.insert(func) {
            self.visit_func_decl(&self.module.funcs[func]);
        }
    }
}

// FIXME(eddyb) maybe this should be provided by `spirt::visit`.
struct VisitAllControlRegionsAndNodes<S, VCR, VCN> {
    state: S,
    visit_control_region: VCR,
    visit_control_node: VCN,
}
const _: () = {
    use spirt::{func_at::*, visit::*, *};

    impl<
            'a,
            S,
            VCR: FnMut(&mut S, FuncAt<'a, ControlRegion>),
            VCN: FnMut(&mut S, FuncAt<'a, ControlNode>),
        > Visitor<'a> for VisitAllControlRegionsAndNodes<S, VCR, VCN>
    {
        // FIXME(eddyb) this is excessive, maybe different kinds of
        // visitors should exist for module-level and func-level?
        fn visit_attr_set_use(&mut self, _: AttrSet) {}
        fn visit_type_use(&mut self, _: Type) {}
        fn visit_const_use(&mut self, _: Const) {}
        fn visit_global_var_use(&mut self, _: GlobalVar) {}
        fn visit_func_use(&mut self, _: Func) {}

        fn visit_control_region_def(&mut self, func_at_control_region: FuncAt<'a, ControlRegion>) {
            (self.visit_control_region)(&mut self.state, func_at_control_region);
            func_at_control_region.inner_visit_with(self);
        }
        fn visit_control_node_def(&mut self, func_at_control_node: FuncAt<'a, ControlNode>) {
            (self.visit_control_node)(&mut self.state, func_at_control_node);
            // HACK(eddyb) accidentally private `inner_visit_with` method.
            fn control_node_inner_visit_with<'a>(
                self_: FuncAt<'a, ControlNode>,
                visitor: &mut impl Visitor<'a>,
            ) {
                let ControlNodeDef { kind, outputs } = self_.def();

                match kind {
                    ControlNodeKind::Block { insts } => {
                        for func_at_inst in self_.at(*insts) {
                            visitor.visit_data_inst_def(func_at_inst.def());
                        }
                    }
                    ControlNodeKind::Select {
                        kind: SelectionKind::BoolCond | SelectionKind::SpvInst(_),
                        scrutinee,
                        cases,
                    } => {
                        visitor.visit_value_use(scrutinee);
                        for &case in cases {
                            visitor.visit_control_region_def(self_.at(case));
                        }
                    }
                    ControlNodeKind::Loop {
                        initial_inputs,
                        body,
                        repeat_condition,
                    } => {
                        for v in initial_inputs {
                            visitor.visit_value_use(v);
                        }
                        visitor.visit_control_region_def(self_.at(*body));
                        visitor.visit_value_use(repeat_condition);
                    }
                }
                for output in outputs {
                    output.inner_visit_with(visitor);
                }
            }
            control_node_inner_visit_with(func_at_control_node, self);
        }
    }
};

// HACK(eddyb) this works around the accidental lack of `spirt::Value: Hash`.
#[derive(Copy, Clone, PartialEq, Eq)]
struct HashableValue(Value);
#[allow(clippy::derived_hash_with_manual_eq)]
impl Hash for HashableValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use spirt::*;
        #[derive(Hash)]
        enum ValueH {
            Const(Const),
            ControlRegionInput {
                region: ControlRegion,
                input_idx: u32,
            },
            ControlNodeOutput {
                control_node: ControlNode,
                output_idx: u32,
            },
            DataInstOutput(DataInst),
        }
        match self.0 {
            Value::Const(ct) => ValueH::Const(ct),
            Value::ControlRegionInput { region, input_idx } => {
                ValueH::ControlRegionInput { region, input_idx }
            }
            Value::ControlNodeOutput {
                control_node,
                output_idx,
            } => ValueH::ControlNodeOutput {
                control_node,
                output_idx,
            },
            Value::DataInstOutput(inst) => ValueH::DataInstOutput(inst),
        }
        .hash(state);
    }
}

// FIXME(eddyb) maybe this should be provided by `spirt::transform`.
struct ReplaceValueWith<F>(F);
const _: () = {
    use spirt::{transform::*, *};

    impl<F: FnMut(Value) -> Option<Value>> Transformer for ReplaceValueWith<F> {
        fn transform_value_use(&mut self, v: &Value) -> Transformed<Value> {
            self.0(*v).map_or(Transformed::Unchanged, Transformed::Changed)
        }
    }
};

/// Clean up after a pass by removing unused (pure) `Value` definitions from
/// a function body (both `DataInst`s and `ControlRegion` inputs/outputs).
//
// FIXME(eddyb) should this be a dedicated pass?
fn remove_unused_values_in_func(func_def_body: &mut FuncDefBody) {
    // Avoid having to support unstructured control-flow.
    if func_def_body.unstructured_cfg.is_some() {
        return;
    }

    let wk = &SpvSpecWithExtras::get().well_known;

    struct Propagator {
        func_body_region: ControlRegion,

        // FIXME(eddyb) maybe this kind of "parent map" should be provided by SPIR-T?
        loop_body_to_loop: EntityOrientedDenseMap<ControlRegion, ControlNode>,

        // FIXME(eddyb) entity-keyed dense sets might be better for performance,
        // but would require separate sets/maps for separate `Value` cases.
        used: FxHashSet<HashableValue>,

        queue: VecDeque<Value>,
    }
    impl Propagator {
        fn mark_used(&mut self, v: Value) {
            if let Value::Const(_) = v {
                return;
            }
            if let Value::ControlRegionInput {
                region,
                input_idx: _,
            } = v
            {
                if region == self.func_body_region {
                    return;
                }
            }
            if self.used.insert(HashableValue(v)) {
                self.queue.push_back(v);
            }
        }
        fn propagate_used(&mut self, func: FuncAt<'_, ()>) {
            while let Some(v) = self.queue.pop_front() {
                match v {
                    Value::Const(_) => unreachable!(),
                    Value::ControlRegionInput { region, input_idx } => {
                        let loop_node = self.loop_body_to_loop[region];
                        let initial_inputs = match &func.at(loop_node).def().kind {
                            ControlNodeKind::Loop { initial_inputs, .. } => initial_inputs,
                            // NOTE(eddyb) only `Loop`s' bodies can have inputs right now.
                            _ => unreachable!(),
                        };
                        self.mark_used(initial_inputs[input_idx as usize]);
                        self.mark_used(func.at(region).def().outputs[input_idx as usize]);
                    }
                    Value::ControlNodeOutput {
                        control_node,
                        output_idx,
                    } => {
                        let cases = match &func.at(control_node).def().kind {
                            ControlNodeKind::Select { cases, .. } => cases,
                            // NOTE(eddyb) only `Select`s can have outputs right now.
                            _ => unreachable!(),
                        };
                        for &case in cases {
                            self.mark_used(func.at(case).def().outputs[output_idx as usize]);
                        }
                    }
                    Value::DataInstOutput(inst) => {
                        for &input in &func.at(inst).def().inputs {
                            self.mark_used(input);
                        }
                    }
                }
            }
        }
    }

    // HACK(eddyb) it's simpler to first ensure `loop_body_to_loop` is computed,
    // just to allow the later unordered propagation to always work.
    let propagator = {
        let mut visitor = VisitAllControlRegionsAndNodes {
            state: Propagator {
                func_body_region: func_def_body.body,
                loop_body_to_loop: Default::default(),
                used: Default::default(),
                queue: Default::default(),
            },
            visit_control_region: |_: &mut _, _| {},
            visit_control_node:
                |propagator: &mut Propagator, func_at_control_node: FuncAt<'_, ControlNode>| {
                    if let ControlNodeKind::Loop { body, .. } = func_at_control_node.def().kind {
                        propagator
                            .loop_body_to_loop
                            .insert(body, func_at_control_node.position);
                    }
                },
        };
        func_def_body.inner_visit_with(&mut visitor);
        visitor.state
    };

    // HACK(eddyb) this kind of random-access is easier than using `spirt::transform`.
    let mut all_control_nodes = vec![];

    let used_values = {
        let mut visitor = VisitAllControlRegionsAndNodes {
            state: propagator,
            visit_control_region: |_: &mut _, _| {},
            visit_control_node:
                |propagator: &mut Propagator, func_at_control_node: FuncAt<'_, ControlNode>| {
                    all_control_nodes.push(func_at_control_node.position);

                    let mut mark_used_and_propagate = |v| {
                        propagator.mark_used(v);
                        propagator.propagate_used(func_at_control_node.at(()));
                    };
                    match func_at_control_node.def().kind {
                        ControlNodeKind::Block { insts } => {
                            for func_at_inst in func_at_control_node.at(insts) {
                                // Ignore pure instructions (i.e. they're only used
                                // if their output value is used, from somewhere else).
                                if let DataInstKind::SpvInst(spv_inst) = &func_at_inst.def().kind {
                                    // HACK(eddyb) small selection relevant for now,
                                    // but should be extended using e.g. a bitset.
                                    if [wk.OpNop, wk.OpCompositeInsert].contains(&spv_inst.opcode) {
                                        continue;
                                    }
                                }
                                mark_used_and_propagate(Value::DataInstOutput(
                                    func_at_inst.position,
                                ));
                            }
                        }

                        ControlNodeKind::Select { scrutinee: v, .. }
                        | ControlNodeKind::Loop {
                            repeat_condition: v,
                            ..
                        } => mark_used_and_propagate(v),
                    }
                },
        };
        func_def_body.inner_visit_with(&mut visitor);

        let mut propagator = visitor.state;
        for &v in &func_def_body.at_body().def().outputs {
            propagator.mark_used(v);
            propagator.propagate_used(func_def_body.at(()));
        }

        assert!(propagator.queue.is_empty());
        propagator.used
    };

    // FIXME(eddyb) entity-keyed dense maps might be better for performance,
    // but would require separate maps for separate `Value` cases.
    let mut value_replacements = FxHashMap::default();

    // Remove anything that didn't end up marked as used (directly or indirectly).
    for control_node in all_control_nodes {
        let control_node_def = func_def_body.at(control_node).def();
        match &control_node_def.kind {
            &ControlNodeKind::Block { insts } => {
                let mut all_nops = true;
                let mut func_at_inst_iter = func_def_body.at_mut(insts).into_iter();
                while let Some(mut func_at_inst) = func_at_inst_iter.next() {
                    if let DataInstKind::SpvInst(spv_inst) = &func_at_inst.reborrow().def().kind {
                        if spv_inst.opcode == wk.OpNop {
                            continue;
                        }
                    }
                    if !used_values
                        .contains(&HashableValue(Value::DataInstOutput(func_at_inst.position)))
                    {
                        // Replace the removed `DataInstDef` itself with `OpNop`,
                        // removing the ability to use its "name" as a value.
                        *func_at_inst.def() = DataInstDef {
                            attrs: Default::default(),
                            kind: DataInstKind::SpvInst(wk.OpNop.into()),
                            output_type: None,
                            inputs: iter::empty().collect(),
                        };
                        continue;
                    }
                    all_nops = false;
                }
                // HACK(eddyb) because we can't remove list elements yet, we
                // instead replace blocks of `OpNop`s with empty ones.
                if all_nops {
                    func_def_body.at_mut(control_node).def().kind = ControlNodeKind::Block {
                        insts: Default::default(),
                    };
                }
            }

            ControlNodeKind::Select { cases, .. } => {
                // FIXME(eddyb) remove this cloning.
                let cases = cases.clone();

                let mut new_idx = 0;
                for original_idx in 0..control_node_def.outputs.len() {
                    let original_output = Value::ControlNodeOutput {
                        control_node,
                        output_idx: original_idx as u32,
                    };

                    if !used_values.contains(&HashableValue(original_output)) {
                        // Remove the output definition and corresponding value from all cases.
                        func_def_body
                            .at_mut(control_node)
                            .def()
                            .outputs
                            .remove(new_idx);
                        for &case in &cases {
                            func_def_body.at_mut(case).def().outputs.remove(new_idx);
                        }
                        continue;
                    }

                    // Record remappings for any still-used outputs that got "shifted over".
                    if original_idx != new_idx {
                        let new_output = Value::ControlNodeOutput {
                            control_node,
                            output_idx: new_idx as u32,
                        };
                        value_replacements.insert(HashableValue(original_output), new_output);
                    }
                    new_idx += 1;
                }
            }
            ControlNodeKind::Loop {
                body,
                initial_inputs,
                ..
            } => {
                let body = *body;

                let mut new_idx = 0;
                for original_idx in 0..initial_inputs.len() {
                    let original_input = Value::ControlRegionInput {
                        region: body,
                        input_idx: original_idx as u32,
                    };

                    if !used_values.contains(&HashableValue(original_input)) {
                        // Remove the input definition and corresponding values.
                        match &mut func_def_body.at_mut(control_node).def().kind {
                            ControlNodeKind::Loop { initial_inputs, .. } => {
                                initial_inputs.remove(new_idx);
                            }
                            _ => unreachable!(),
                        }
                        let body_def = func_def_body.at_mut(body).def();
                        body_def.inputs.remove(new_idx);
                        body_def.outputs.remove(new_idx);
                        continue;
                    }

                    // Record remappings for any still-used inputs that got "shifted over".
                    if original_idx != new_idx {
                        let new_input = Value::ControlRegionInput {
                            region: body,
                            input_idx: new_idx as u32,
                        };
                        value_replacements.insert(HashableValue(original_input), new_input);
                    }
                    new_idx += 1;
                }
            }
        }
    }

    if !value_replacements.is_empty() {
        func_def_body.inner_in_place_transform_with(&mut ReplaceValueWith(|v| match v {
            Value::Const(_) => None,
            _ => value_replacements.get(&HashableValue(v)).copied(),
        }));
    }
}
