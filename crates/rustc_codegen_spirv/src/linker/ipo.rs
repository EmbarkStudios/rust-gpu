//! Tools for interprocedural optimizations (aka "IPO"s).

// FIXME(eddyb) perhaps make all IPOs sub-modules of this module?

use indexmap::IndexSet;
use rspirv::dr::Module;
use rspirv::spirv::Op;
use rustc_data_structures::fx::FxHashMap;

// FIXME(eddyb) use newtyped indices and `IndexVec`.
type FuncIdx = usize;

pub struct CallGraph {
    pub entry_points: IndexSet<FuncIdx>,

    /// `callees[i].contains(j)` implies `functions[i]` calls `functions[j]`.
    pub callees: Vec<IndexSet<FuncIdx>>,
}

impl CallGraph {
    pub fn collect(module: &Module) -> Self {
        let func_id_to_idx: FxHashMap<_, _> = module
            .functions
            .iter()
            .enumerate()
            .map(|(i, func)| (func.def_id().unwrap(), i))
            .collect();
        let entry_points = module
            .entry_points
            .iter()
            .map(|entry| {
                assert_eq!(entry.class.opcode, Op::EntryPoint);
                func_id_to_idx[&entry.operands[1].unwrap_id_ref()]
            })
            .collect();
        let callees = module
            .functions
            .iter()
            .map(|func| {
                func.all_inst_iter()
                    .filter(|inst| inst.class.opcode == Op::FunctionCall)
                    .filter_map(|inst| {
                        // FIXME(eddyb) `func_id_to_idx` should always have an
                        // entry for a callee ID, but when ran early enough
                        // (before zombie removal), the callee ID might not
                        // point to an `OpFunction` (unsure what, `OpUndef`?).
                        func_id_to_idx
                            .get(&inst.operands[0].unwrap_id_ref())
                            .copied()
                    })
                    .collect()
            })
            .collect();
        Self {
            entry_points,
            callees,
        }
    }

    /// Order functions using a post-order traversal, i.e. callees before callers.
    // FIXME(eddyb) replace this with `rustc_data_structures::graph::iterate`
    // (or similar).
    pub fn post_order(&self) -> Vec<FuncIdx> {
        let num_funcs = self.callees.len();

        // FIXME(eddyb) use a proper bitset.
        let mut visited = vec![false; num_funcs];
        let mut post_order = Vec::with_capacity(num_funcs);

        // Visit the call graph with entry points as roots.
        for &entry in &self.entry_points {
            self.post_order_step(entry, &mut visited, &mut post_order);
        }

        // Also visit any functions that were not reached from entry points
        // (they might be dead but they should be processed nonetheless).
        for func in 0..num_funcs {
            if !visited[func] {
                self.post_order_step(func, &mut visited, &mut post_order);
            }
        }

        post_order
    }

    fn post_order_step(&self, func: FuncIdx, visited: &mut [bool], post_order: &mut Vec<FuncIdx>) {
        if visited[func] {
            return;
        }
        visited[func] = true;

        for &callee in &self.callees[func] {
            self.post_order_step(callee, visited, post_order);
        }

        post_order.push(func);
    }
}
