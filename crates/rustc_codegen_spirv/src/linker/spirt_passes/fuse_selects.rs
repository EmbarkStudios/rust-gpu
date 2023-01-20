use spirt::func_at::FuncAt;
use spirt::transform::InnerInPlaceTransform;
use spirt::visit::InnerVisit;
use spirt::{Context, ControlNodeKind, ControlRegion, FuncDefBody, SelectionKind, Value};
use std::mem;

use super::{ReplaceValueWith, VisitAllControlRegionsAndNodes};

/// Combine consecutive `Select`s in `func_def_body`.
pub(crate) fn fuse_selects_in_func(_cx: &Context, func_def_body: &mut FuncDefBody) {
    // HACK(eddyb) this kind of random-access is easier than using `spirt::transform`.
    let mut all_regions = vec![];

    func_def_body.inner_visit_with(&mut VisitAllControlRegionsAndNodes {
        state: (),
        visit_control_region: |_: &mut (), func_at_control_region: FuncAt<'_, ControlRegion>| {
            all_regions.push(func_at_control_region.position);
        },
        visit_control_node: |_: &mut (), _| {},
    });

    for region in all_regions {
        let mut func_at_children_iter = func_def_body.at_mut(region).at_children().into_iter();
        while let Some(func_at_child) = func_at_children_iter.next() {
            let base_control_node = func_at_child.position;
            if let ControlNodeKind::Select {
                kind: SelectionKind::BoolCond,
                scrutinee,
                cases,
            } = &func_at_child.def().kind
            {
                let &base_cond = scrutinee;
                let base_cases = cases.clone();

                // Scan ahead for candidate `Select`s (with the same condition).
                let mut fusion_candidate_iter = func_at_children_iter.reborrow();
                while let Some(func_at_fusion_candidate) = fusion_candidate_iter.next() {
                    let fusion_candidate = func_at_fusion_candidate.position;
                    let mut func = func_at_fusion_candidate.at(());
                    let fusion_candidate_def = func.reborrow().at(fusion_candidate).def();
                    match &fusion_candidate_def.kind {
                        // HACK(eddyb) ignore empty blocks (created by
                        // e.g. `remove_unused_values_in_func`).
                        ControlNodeKind::Block { insts } if insts.is_empty() => {}

                        ControlNodeKind::Select {
                            kind: SelectionKind::BoolCond,
                            scrutinee: fusion_candidate_cond,
                            cases: fusion_candidate_cases,
                        } if *fusion_candidate_cond == base_cond => {
                            // FIXME(eddyb) handle outputs from the second `Select`.
                            if !fusion_candidate_def.outputs.is_empty() {
                                break;
                            }

                            let cases_to_fuse = fusion_candidate_cases.clone();

                            // Concatenate the `Select`s' respective cases
                            // ("then" with "then", "else" with "else", etc.).
                            for (&base_case, &case_to_fuse) in base_cases.iter().zip(&cases_to_fuse)
                            {
                                let children_of_case_to_fuse =
                                    mem::take(&mut func.reborrow().at(case_to_fuse).def().children);

                                // Replace uses of the outputs of the first `Select`,
                                // in the second one's case, with the specific values
                                // (e.g. `let y = if c { x } ...; if c { f(y) }`
                                // has to become `let y = if c { f(x); x } ...`).
                                //
                                // FIXME(eddyb) avoid cloning here.
                                let outputs_of_base_case =
                                    func.reborrow().at(base_case).def().outputs.clone();
                                func.reborrow()
                                    .at(children_of_case_to_fuse)
                                    .into_iter()
                                    .inner_in_place_transform_with(&mut ReplaceValueWith(
                                        |v| match v {
                                            Value::ControlNodeOutput {
                                                control_node,
                                                output_idx,
                                            } if control_node == base_control_node => {
                                                Some(outputs_of_base_case[output_idx as usize])
                                            }

                                            _ => None,
                                        },
                                    ));

                                func.control_regions[base_case]
                                    .children
                                    .append(children_of_case_to_fuse, func.control_nodes);
                            }

                            // HACK(eddyb) because we can't remove list elements yet,
                            // we instead replace the `Select` with an empty `Block`.
                            func.reborrow().at(fusion_candidate).def().kind =
                                ControlNodeKind::Block {
                                    insts: Default::default(),
                                };
                        }

                        _ => break,
                    }
                }
            }
        }
    }
}
