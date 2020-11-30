#version 450

#extension GL_EXT_multiview : enable

layout (location = 0) out vec4 projPos;

layout (set = 0, binding = 0) uniform SkyUniforms {
    mat4x4 view_proj_inverse;
    mat4x4 view_proj_inverse_stereo;
} skyUniforms;

out gl_PerVertex {
    vec4 gl_Position;
};

void main() {
    float x = -1.0 + float((gl_VertexIndex & 1) << 2);
    float y = -1.0 + float((gl_VertexIndex & 2) << 1);
    vec4 position = vec4(x, y, 0.0, 1.0);

    if (gl_ViewIndex == 0) {
        projPos = skyUniforms.view_proj_inverse * position;
    } else {
        projPos = skyUniforms.view_proj_inverse_stereo * position;
    }

    gl_Position = position;
    gl_Position.y = -gl_Position.y; // TODO: Change to use negative viewport height instead
}