#include "spirv-tools/optimizer.hpp"
#include <cstring>

struct Optimus;

enum Passes {
    Null,
    StripAtomicCounterMemory,
    StripDebugInfo,
    StripReflectInfo,
    EliminateDeadFunctions,
    EliminateDeadMembers,
    FlattenDecoration,
    FreezeSpecConstantValue,
    FoldSpecConstantOpAndComposite,
    UnifyConstant,
    EliminateDeadConstant,
    StrengthReduction,
    BlockMerge,
    InlineExhaustive,
    InlineOpaque,
    LocalSingleBlockLoadStoreElim,
    DeadBranchElim,
    LocalMultiStoreElim,
    LocalAccessChainConvert,
    LocalSingleStoreElim,
    InsertExtractElim,
    DeadInsertElim,
    AggressiveDCE,
    PropagateLineInfo,
    RedundantLineInfoElim,
    CompactIds,
    RemoveDuplicates,
    CFGCleanup,
    DeadVariableElimination,
    MergeReturn,
    LocalRedundancyElimination,
    LoopInvariantCodeMotion,
    LoopPeeling,
    LoopUnswitch,
    RedundancyElimination,
    PrivateToLocal,
    CCP,
    Workaround1209,
    IfConversion,
    ReplaceInvalidOpcode,
    Simplification,
    SSARewrite,
    ConvertRelaxedToHalf,
    RelaxFloatOps,
    CopyPropagateArrays,
    VectorDCE,
    ReduceLoadSize,
    CombineAccessChains,
    UpgradeMemoryModel,
    CodeSinking,
    GenerateWebGPUInitializers,
    FixStorageClass,
    LegalizeVectorShuffle,
    DecomposeInitializedVariables,
    SplitInvalidUnreachable,
    GraphicsRobustAccess,
    DescriptorScalarReplacement,
    WrapOpKill,
    AmdExtToKhr,
};

typedef void (*message_callback)(
    spv_message_level_t level,
    const char* source,
    const spv_position_t* position,
    const char* message,
    void* ctx
);

extern "C" {
    SPIRV_TOOLS_EXPORT Optimus* optimizer_create(spv_target_env target_env) {
        auto* optimizer = new spvtools::Optimizer(target_env);

        return (Optimus*)optimizer;
    }

    SPIRV_TOOLS_EXPORT void optimizer_destroy(Optimus* optimizer) {
        delete (spvtools::Optimizer*)optimizer;
    }

    SPIRV_TOOLS_EXPORT spv_result_t optimizer_run(
        const Optimus* optimizer,
        const uint32_t* input_ptr,
        size_t input_size,
        spv_binary* out_binary,
        message_callback msg_callback,
        void* ctx,
        const spv_optimizer_options options
    ) {
        if (input_ptr == nullptr) {
            return SPV_ERROR_INVALID_POINTER;
        }

        if (out_binary == nullptr) {
            return SPV_ERROR_INVALID_POINTER;
        }

        auto op = (spvtools::Optimizer*)optimizer;

        if (msg_callback) {
            op->SetMessageConsumer([msg_callback, ctx](
                spv_message_level_t level,
                const char* source,
                const spv_position_t& position,
                const char* message) {
                msg_callback(level, source, &position, message, ctx);
            });
        } else {
            // The optimizer keeps the message consumer as state, so if no
            // callback is passed to us, we insert a noop callback to ensure
            // we don't use the state from a previous optimizer run
            op->SetMessageConsumer([](
                spv_message_level_t,
                const char*,
                const spv_position_t&,
                const char*)
                {}
            );
        }

        auto output_buff = std::vector<uint32_t>();
        bool success = false;
        if (options == nullptr) {
            success = op->Run(input_ptr, input_size, &output_buff);
        } else {
            success = op->Run(input_ptr, input_size, &output_buff, options);
        }

        if (!success) {
            return SPV_ERROR_INTERNAL;
        }

        uint32_t* data = new uint32_t[output_buff.size()];
        if (data == nullptr) {
            return SPV_ERROR_OUT_OF_MEMORY;
        }

        spv_binary binary = new spv_binary_t();
        if (binary == nullptr) {
            delete[] data;
            return SPV_ERROR_OUT_OF_MEMORY;
        }

        memcpy(data, output_buff.data(), output_buff.size());
        *out_binary = binary;

        return SPV_SUCCESS;
    }

    SPIRV_TOOLS_EXPORT void optimizer_register_pass(Optimus* optimizer, Passes pass) {
        #define PASTEB(a, b) a ## b
        #define PASTEA(a, b) PASTEB(a, b)
        #define PASS(name) \
            case name: \
                op->RegisterPass(spvtools::PASTEA(PASTEA(Create, name), Pass)()); \
                break;

        spvtools::Optimizer* op = (spvtools::Optimizer*)optimizer;

        switch (pass) {
            PASS(Null)
            PASS(StripAtomicCounterMemory)
            PASS(StripDebugInfo)
            PASS(StripReflectInfo)
            PASS(EliminateDeadFunctions)
            PASS(EliminateDeadMembers)
            PASS(FlattenDecoration)
            PASS(FreezeSpecConstantValue)
            PASS(FoldSpecConstantOpAndComposite)
            PASS(UnifyConstant)
            PASS(EliminateDeadConstant)
            PASS(StrengthReduction)
            PASS(BlockMerge)
            PASS(InlineExhaustive)
            PASS(InlineOpaque)
            PASS(LocalSingleBlockLoadStoreElim)
            PASS(DeadBranchElim)
            PASS(LocalMultiStoreElim)
            PASS(LocalAccessChainConvert)
            PASS(LocalSingleStoreElim)
            PASS(InsertExtractElim)
            PASS(DeadInsertElim)
            PASS(AggressiveDCE)
            PASS(PropagateLineInfo)
            PASS(RedundantLineInfoElim)
            PASS(CompactIds)
            PASS(RemoveDuplicates)
            PASS(CFGCleanup)
            PASS(DeadVariableElimination)
            PASS(MergeReturn)
            PASS(LocalRedundancyElimination)
            PASS(LoopInvariantCodeMotion)
            PASS(LoopPeeling)
            PASS(LoopUnswitch)
            PASS(RedundancyElimination)
            PASS(PrivateToLocal)
            PASS(CCP)
            PASS(Workaround1209)
            PASS(IfConversion)
            PASS(ReplaceInvalidOpcode)
            PASS(Simplification)
            PASS(SSARewrite)
            PASS(ConvertRelaxedToHalf)
            PASS(RelaxFloatOps)
            PASS(CopyPropagateArrays)
            PASS(VectorDCE)
            PASS(ReduceLoadSize)
            PASS(CombineAccessChains)
            PASS(UpgradeMemoryModel)
            PASS(CodeSinking)
            PASS(GenerateWebGPUInitializers)
            PASS(FixStorageClass)
            PASS(LegalizeVectorShuffle)
            PASS(DecomposeInitializedVariables)
            PASS(SplitInvalidUnreachable)
            PASS(GraphicsRobustAccess)
            PASS(DescriptorScalarReplacement)
            PASS(WrapOpKill)
            PASS(AmdExtToKhr)
        }
    }

    SPIRV_TOOLS_EXPORT void optimizer_register_performance_passes(Optimus* optimizer) {
        ((spvtools::Optimizer*)optimizer)->RegisterPerformancePasses();
    }

    SPIRV_TOOLS_EXPORT void optimizer_register_size_passes(Optimus* optimizer) {
        ((spvtools::Optimizer*)optimizer)->RegisterSizePasses();
    }

    SPIRV_TOOLS_EXPORT void optimizer_register_vulkan_to_webgpu_passes(Optimus* optimizer) {
        ((spvtools::Optimizer*)optimizer)->RegisterVulkanToWebGPUPasses();
    }

    SPIRV_TOOLS_EXPORT void optimizer_register_webgpu_to_vulkan_passes(Optimus* optimizer) {
        ((spvtools::Optimizer*)optimizer)->RegisterWebGPUToVulkanPasses();
    }

    SPIRV_TOOLS_EXPORT void optimizer_register_legalization_passes(Optimus* optimizer) {
        ((spvtools::Optimizer*)optimizer)->RegisterLegalizationPasses();
    }
}
