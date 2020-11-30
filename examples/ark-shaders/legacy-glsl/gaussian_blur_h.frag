#version 450
#extension GL_ARB_shading_language_420pack : enable
#extension GL_EXT_samplerless_texture_functions : enable

layout (set = 0, binding = 0) uniform texture2DArray in_image;
layout (set = 0, binding = 1) uniform sampler bilinearSampler;

layout (location = 0) in vec3 uv_layer;
layout (location = 0) out vec4 outColor;

// Some old clamped gaussians I had lying around.
// Uses bilinear filtering to do two taps in one fetch.
const float offsets[18] = float[]( 0.000000, 1.494118, 3.486275, 5.478431, 7.470588, 9.462745, 11.454902, 13.447059, 15.439216, 17.431373, 19.423529, 21.415686, 23.407843, 25.400000, 27.392157, 29.384314, 31.376471, 33.368627 );
const float weights[18] = float[]( 0.050014, 0.098093, 0.090721, 0.078815, 0.064315, 0.049293, 0.035478, 0.023975, 0.015210, 0.009056, 0.005059, 0.002651, 0.001303, 0.000600, 0.000259, 0.000105, 0.000040, 0.000014 );

// http://rastergrid.com/blog/2010/09/efficient-gaussian-blur-with-linear-sampling/
void main() {
	vec2 uv = uv_layer.xy;
	int layer = int(uv_layer.z);
	vec2 delta = vec2(1.0) / vec2(textureSize(in_image, 0));

	vec4 fragmentColor = texture(sampler2DArray(in_image, bilinearSampler), vec3(uv, float(layer))) * weights[0];
    for (int i = 1; i < offsets.length(); i++) {
        fragmentColor += texture(sampler2DArray(in_image, bilinearSampler), vec3(uv.x + offsets[i] * delta.x, uv.y, float(layer))) * weights[i];
        fragmentColor += texture(sampler2DArray(in_image, bilinearSampler), vec3(uv.x - offsets[i] * delta.x, uv.y, float(layer))) * weights[i];
    }
	outColor = fragmentColor;
}
