#version 450

#extension GL_EXT_multiview : enable

layout (location = 0) out vec3 uv;

out gl_PerVertex {
    vec4 gl_Position;
};

void main() {
    float x = -1.0 + float((gl_VertexIndex & 1) << 2);
    float y = -1.0 + float((gl_VertexIndex & 2) << 1);
    vec4 position = vec4(x, y, 0, 1);

    uv.xy = (vec2(x, y) + 1.0) * 0.5;
    uv.y = 1.0 - uv.y;
    uv.z = float(gl_ViewIndex);
    gl_Position = position;
    gl_Position.y = -gl_Position.y; // TODO: Change to use negative viewport height instead
}