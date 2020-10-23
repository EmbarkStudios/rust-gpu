#include "spirv-tools/libspirv.hpp"

struct Tool;

extern "C" {
    SPIRV_TOOLS_EXPORT Tool* tool_create(spv_target_env target_env) {
        auto* tools = new spvtools::SpirvTools(target_env);

        return (Tool*)tools;
    }
    
    SPIRV_TOOLS_EXPORT void tool_destroy(Tool* tool) {
        delete (spvtools::SpirvTools*)tool;
    }

    SPIRV_TOOLS_EXPORT bool validate(
        const Tool* tool,
        const uint32_t* binary,
        size_t size,
        spv_validator_options options
    ) {
        auto* tools = (spvtools::SpirvTools*)tool;

        return tools->Validate(binary, size, options);
    }
}
