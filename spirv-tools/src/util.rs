pub fn from_binary(bin: &[u32]) -> &[u8] {
    unsafe {
        std::slice::from_raw_parts(
            bin.as_ptr() as *const u8,
            bin.len() * std::mem::size_of::<u32>(),
        )
    }
}

pub fn to_binary(bytes: &[u8]) -> Result<&[u32], crate::Error> {
    if bytes.len() % std::mem::size_of::<u32>() != 0 {
        return Err(crate::Error {
            inner: spirv_tools_sys::shared::SpirvResult::InvalidBinary,
            diagnostic: None,
        });
    }

    Ok(unsafe {
        std::slice::from_raw_parts(
            bytes.as_ptr() as *const u32,
            bytes.len() / std::mem::size_of::<u32>(),
        )
    })
}
