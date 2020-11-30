#version 450

// TODO: Replace with fullscreen_triangle.vert

layout (location = 0) out vec2 uv;

out gl_PerVertex {
    vec4 gl_Position;
};

void main() {
    float x = -1.0 + float((gl_VertexIndex & 1) << 2);
    float y = -1.0 + float((gl_VertexIndex & 2) << 1);
    vec4 position = vec4(x, y, 0.0, 1.0);
    uv = (position.xy + 1.0) * 0.5;

    gl_Position = position;

    // No Y negation here! Would be double negation.
}