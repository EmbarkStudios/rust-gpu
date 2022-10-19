// Tests that matrix type inference fails correctly
// build-fail

use spirv_std::spirv;

#[spirv(matrix)]
pub struct _FewerFields {
    _v: glam::Vec3,
}

#[spirv(matrix)]
pub struct _NotVectorField {
    _x: f32,
    _y: f32,
    _z: f32,
}

#[spirv(matrix)]
pub struct _DifferentType {
    _x: glam::Vec3,
    _y: glam::Vec2,
}

#[spirv(fragment)]
pub fn _entry(_arg1: _FewerFields, _arg2: _NotVectorField, _arg3: _DifferentType) {}
