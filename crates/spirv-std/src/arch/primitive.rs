/// Emits the current values of all output variables to the current output
/// primitive. After execution, the values of all output variables
/// are undefined.  Requires capability `Geometry`.
///
/// # Safety
/// This instruction must only be used when only one stream is present.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpEmitVertex")]
#[inline]
pub unsafe fn emit_vertex() {
    asm! {
        "OpEmitVertex",
    }
}

/// Finish the current primitive and start a new one. No vertex is emitted.
/// Requires capability `Geometry`.
///
/// # Safety
/// This instruction must only be used when only one stream is present.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpEndPrimitive")]
#[inline]
pub unsafe fn end_primitive() {
    asm! {
        "OpEndPrimitive",
    }
}

/// Emits the current values of all output variables to the current output
/// primitive. After execution, the values of all output variables
/// are undefined.
///
/// `STREAM` is the output-primitive stream number.
///
/// Requires capability `GeometryStreams`.
///
/// # Safety
/// This instruction must only be used when multiple streams are present.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpEmitStreamVertex")]
#[inline]
pub unsafe fn emit_stream_vertex<const STREAM: i64>() {
    asm! {
        "%i64 = OpTypeInt 64 1",
        "%stream = OpConstant %i64 {stream}",
        "OpEmitStreamVertex %stream",
        stream = const STREAM,
    }
}

/// Finish the current primitive and start a new one. No vertex is emitted.
///
/// `STREAM` is the output-primitive stream number.
///
/// Requires capability `GeometryStreams`.
///
/// # Safety
/// This instruction must only be used when multiple streams are present.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpEndStreamPrimitive")]
#[inline]
pub unsafe fn end_stream_primitive<const STREAM: i64>() {
    asm! {
        "%i64 = OpTypeInt 64 1",
        "%stream = OpConstant %i64 {stream}",
        "OpEndStreamPrimitive %stream",
        stream = const STREAM,
    }
}
