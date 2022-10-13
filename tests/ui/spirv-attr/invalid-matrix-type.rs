// Tests that matrix type inference fails correctly
// build-fail

use spirv_std as _;

#[rust_gpu::spirv(matrix)]
pub struct _FewerFields {
    _v: glam::Vec3,
}

#[rust_gpu::spirv(matrix)]
pub struct _NotVectorField {
    _x: f32,
    _y: f32,
    _z: f32,
}

#[rust_gpu::spirv(matrix)]
pub struct _DifferentType {
    _x: glam::Vec3,
    _y: glam::Vec2,
}

#[rust_gpu::spirv(fragment)]
pub fn _entry(_arg1: _FewerFields, _arg2: _NotVectorField, _arg3: _DifferentType) {}
