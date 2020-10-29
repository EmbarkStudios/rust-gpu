use crate::error;

#[derive(Default)]
pub struct ToolOptimizer {
    target_env: crate::TargetEnv,
    passes: Vec<super::Passes>,
    use_perf_passes: bool,
    use_size_passes: bool,
    //use_vulkan_to_webgpu: bool,
    //use_webgpu_to_vulkan: bool,
    legalize_hlsl: bool,
}

use super::Optimizer;

impl Optimizer for ToolOptimizer {
    fn with_env(target_env: crate::TargetEnv) -> Self {
        Self {
            target_env,
            ..Default::default()
        }
    }

    fn optimize<MC: error::MessageCallback>(
        &self,
        input: &[u32],
        msg_callback: &mut MC,
        options: Option<super::Options>,
    ) -> Result<crate::binary::Binary, crate::Error> {
        let mut cmd = std::process::Command::new("spirv-opt");
        cmd.arg("--target-env").arg(self.target_env.to_string());

        cmd.args(
            self.passes
                .iter()
                .filter_map(|p| pass_to_string(*p).map(|s| format!("--{}", s))),
        );

        if self.use_perf_passes {
            cmd.arg("-O");
        }

        if self.use_size_passes {
            cmd.arg("-Os");
        }

        if self.legalize_hlsl {
            cmd.arg("--legalize-hlsl");
        }

        if let Some(opts) = options {
            if let Some(max_id_bound) = opts.max_id_bound {
                cmd.arg(format!("--max-id-bound={}", max_id_bound));
            }

            if opts.preserve_bindings {
                cmd.arg("--preserve-bindings");
            }

            if opts.preserve_spec_constants {
                cmd.arg("--preserve-spec-constants");
            }

            if let Some(vopts) = opts.validator_options {
                crate::val::tool::add_options(&mut cmd, vopts);
            }
        }

        let input = crate::util::from_binary(input);

        let cmd_output = crate::cmd::exec(cmd, Some(input), crate::cmd::Output::Retrieve)?;

        for msg in cmd_output.messages {
            msg_callback.on_message(msg);
        }

        use std::convert::TryFrom;
        crate::binary::Binary::try_from(cmd_output.binary)
    }

    /// Register a single pass with the the optimizer.
    #[inline]
    fn register_pass(&mut self, pass: super::Passes) -> &mut Self {
        self.passes.push(pass);
        self
    }

    /// Registers passes that attempt to improve performance of generated code.
    /// This sequence of passes is subject to constant review and will change
    /// from time to time.
    #[inline]
    fn register_performance_passes(&mut self) -> &mut Self {
        self.use_perf_passes = true;
        self
    }

    /// Registers passes that attempt to improve the size of generated code.
    /// This sequence of passes is subject to constant review and will change
    /// from time to time.
    #[inline]
    fn register_size_passes(&mut self) -> &mut Self {
        self.use_size_passes = true;
        self
    }

    /// Registers passes that attempt to legalize the generated code.
    ///
    /// Note: this recipe is specially designed for legalizing SPIR-V. It should be
    /// used by compilers after translating HLSL source code literally. It should
    /// *not* be used by general workloads for performance or size improvement.
    ///
    /// This sequence of passes is subject to constant review and will change
    /// from time to time.
    #[inline]
    fn register_hlsl_legalization_passes(&mut self) -> &mut Self {
        self.legalize_hlsl = true;
        self
    }
}

fn pass_to_string(pass: super::Passes) -> Option<&'static str> {
    use super::Passes::*;

    Some(match pass {
        Null => return None,
        StripAtomicCounterMemory => "strip-atomic-counter-memory",
        StripDebugInfo => "strip-debug",
        StripReflectInfo => "strip-reflect",
        EliminateDeadFunctions => "eliminate-dead-functions",
        EliminateDeadMembers => "eliminate-dead-members",
        FlattenDecoration => "flatten-decorations",
        FreezeSpecConstantValue => "freeze-spec-const",
        FoldSpecConstantOpAndComposite => "fold-spec-const-op-composite",
        UnifyConstant => "unify-const",
        EliminateDeadConstant => "eliminate-dead-const",
        StrengthReduction => "strength-reduction",
        BlockMerge => "merge-blocks",
        InlineExhaustive => "inline-entry-points-exhaustive",
        InlineOpaque => "inline-entry-points-opaque",
        LocalSingleBlockLoadStoreElim => "eliminate-local-single-block",
        DeadBranchElim => "eliminate-dead-branches",
        LocalMultiStoreElim => "eliminate-local-multi-store",
        LocalAccessChainConvert => "convert-local-access-chains",
        LocalSingleStoreElim => "eliminate-local-single-store",
        InsertExtractElim => "eliminate-insert-extract",
        DeadInsertElim => "eliminate-dead-inserts",
        AggressiveDCE => "eliminate-dead-code-aggressive",
        PropagateLineInfo => "propagate-line-info",
        RedundantLineInfoElim => "eliminate-redundant-line-info",
        CompactIds => "compact-ids",
        RemoveDuplicates => "remove-duplicates",
        CFGCleanup => "cfg-cleanup",
        DeadVariableElimination => "eliminate-dead-variables",
        MergeReturn => "merge-return",
        LocalRedundancyElimination => "local-redundancy-elimination",
        LoopInvariantCodeMotion => "loop-invariant-code-motion",
        LoopPeeling => "loop-peeling",
        LoopUnswitch => "loop-unswitch",
        RedundancyElimination => "redundancy-elimination",
        PrivateToLocal => "private-to-local",
        ConditionalConstantPropagation => "ccp",
        Workaround1209 => "workaround-1209",
        IfConversion => "if-conversion",
        ReplaceInvalidOpcode => "replace-invalid-opcode",
        Simplification => "simplify-instructions",
        SSARewrite => "ssa-rewrite",
        ConvertRelaxedToHalf => "convert-relaxed-to-half",
        RelaxFloatOps => "relax-float-ops",
        CopyPropagateArrays => "copy-propagate-arrays",
        VectorDCE => "vector-dce",
        ReduceLoadSize => "reduce-load-size",
        CombineAccessChains => "combine-access-chains",
        UpgradeMemoryModel => "upgrade-memory-model",
        CodeSinking => "code-sink",
        GenerateWebGPUInitializers => "generate-webgpu-initializers",
        FixStorageClass => "fix-storage-class",
        LegalizeVectorShuffle => "legalize-vector-shuffle",
        DecomposeInitializedVariables => "decompose-initialized-variables",
        SplitInvalidUnreachable => "split-invalid-unreachable",
        GraphicsRobustAccess => "graphics-robust-access",
        DescriptorScalarReplacement => "descriptor-scalar-replacement",
        WrapOpKill => "wrap-opkill",
        AmdExtToKhr => "amd-ext-to-khr",
    })
}
