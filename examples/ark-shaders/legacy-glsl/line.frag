#version 450
#extension GL_ARB_shading_language_420pack : enable

layout (location = 0) in vec4 diffuseColor;
layout (location = 0) out vec4 outColor;

void main() {
	outColor = diffuseColor;
}
