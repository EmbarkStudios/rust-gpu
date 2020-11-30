#version 450
#extension GL_ARB_shading_language_420pack : enable

layout (set = 0, binding = 0) uniform SkyUniforms {
    mat4x4 view_proj_inverse;
    mat4x4 view_proj_inverse_stereo;
	vec4 sun_dir;
	vec4 sun_color;
	vec4 ground_color;
	vec4 sky_color;
	vec4 horizon_color;
} skyUniforms;

layout (location = 0) in vec4 projPos;
layout (location = 0) out vec4 outColor;

void main() {
	vec3 rayDir = normalize(projPos.xyz);

	vec3 horizonColor = skyUniforms.horizon_color.xyz;
	vec3 upColor = skyUniforms.sky_color.xyz;
	vec3 downColor = skyUniforms.ground_color.xyz;

	float y = rayDir.y;
	if (y > 0.0) {
		// Same hardcoded sun dir as the mesh renderer
		vec3 lightDir = normalize(skyUniforms.sun_dir.xyz);
		float sun = max(0.0, dot(lightDir, rayDir) - 0.998) / (1.0 - 0.998) * 30.0;
		float sunHalo = pow(max(0.0, dot(lightDir, rayDir) - 0.920) / (1.0 - 0.920), 3.0) * 0.5;
		sun += sunHalo;
		vec4 sunColor = vec4(skyUniforms.sun_color.xyz * sun * 1.0, 1.0);

		// Sky gradient
		outColor = vec4(mix(horizonColor, upColor, y), 1.0) + sunColor;
	} else {
		// Downwards gradient
		y = -y;
		outColor = vec4(mix(horizonColor, downColor, y*y), 1.0);
	}
}
