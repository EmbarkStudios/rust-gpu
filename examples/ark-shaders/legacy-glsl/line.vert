#version 450

#extension GL_EXT_multiview : enable

layout (location = 0) in vec3 inPos;
layout (location = 1) in vec4 inColor;

layout (set = 0, binding = 0) uniform GlobalUniforms {
    mat4x4 viewProjMatrix;
    mat4x4 viewProjMatrix_stereo;
} globalUniforms;

layout (location = 0) out vec4 diffuseColor;

out gl_PerVertex {
    vec4 gl_Position;
};

void main() {
    diffuseColor = inColor;

	if (gl_ViewIndex == 0) {
		gl_Position = globalUniforms.viewProjMatrix * vec4(inPos, 1.0);
	} else {
		gl_Position = globalUniforms.viewProjMatrix_stereo * vec4(inPos, 1.0);
	}

    gl_Position.y = -gl_Position.y; // TODO: Change to use negative viewport height instead
}