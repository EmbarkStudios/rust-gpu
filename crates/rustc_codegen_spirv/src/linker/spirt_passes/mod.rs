//! SPIR-T pass infrastructure and supporting utilities.

use rustc_data_structures::fx::FxIndexSet;
use spirt::visit::{InnerVisit, Visitor};
use spirt::{AttrSet, Const, Context, DeclDef, Func, GlobalVar, Module, Type};

/// Run intra-function passes on all `Func` definitions in the `Module`.
//
// FIXME(eddyb) introduce a proper "pass manager".
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
        for &exportee in module.exports.values() {
            exportee.inner_visit_with(&mut collector);
        }
        collector.seen_funcs
    };

    for name in passes {
        let name = name.as_ref();
        let (full_name, pass_fn): (_, fn(_, &mut _)) = match name {
            _ => panic!("unknown `--spirt-passes={}`", name),
        };

        let profiler = before_pass(full_name, module);
        for &func in &all_funcs {
            if let DeclDef::Present(func_def_body) = &mut module.funcs[func].def {
                pass_fn(cx, func_def_body);
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
