#[cfg(feature = "use-compiled-tools")]
pub mod external {
    use spirv_tools_sys::shared;

    pub struct ExternalBinary {
        inner: *mut shared::Binary,
    }

    impl ExternalBinary {
        pub(crate) fn new(bin: *mut shared::Binary) -> Self {
            Self { inner: bin }
        }
    }

    impl AsRef<[u32]> for ExternalBinary {
        fn as_ref(&self) -> &[u32] {
            unsafe { std::slice::from_raw_parts((*self.inner).code, (*self.inner).size) }
        }
    }

    impl AsRef<[u8]> for ExternalBinary {
        fn as_ref(&self) -> &[u8] {
            unsafe {
                std::slice::from_raw_parts(
                    (*self.inner).code as *const u8,
                    (*self.inner).size * std::mem::size_of::<u32>(),
                )
            }
        }
    }

    impl Drop for ExternalBinary {
        fn drop(&mut self) {
            unsafe {
                shared::binary_destroy(self.inner);
            }
        }
    }
}

pub enum Binary {
    #[cfg(feature = "use-compiled-tools")]
    External(self::external::ExternalBinary),
    OwnedU32(Vec<u32>),
    OwnedU8(Vec<u8>),
}

impl std::convert::TryFrom<Vec<u8>> for Binary {
    type Error = crate::Error;

    fn try_from(v: Vec<u8>) -> Result<Self, Self::Error> {
        if v.len() % std::mem::size_of::<u32>() != 0 {
            Err(crate::Error {
                inner: spirv_tools_sys::shared::SpirvResult::InvalidBinary,
                diagnostic: None,
            })
        } else {
            Ok(Binary::OwnedU8(v))
        }
    }
}

impl AsRef<[u32]> for Binary {
    fn as_ref(&self) -> &[u32] {
        match self {
            #[cfg(feature = "use-compiled-tools")]
            Self::External(bin) => bin.as_ref(),
            Self::OwnedU32(v) => &v,
            Self::OwnedU8(v) => {
                // If you hit a panic here it's because try_from wasn't used ;)
                crate::util::to_binary(&v).unwrap()
            }
        }
    }
}

impl AsRef<[u8]> for Binary {
    fn as_ref(&self) -> &[u8] {
        match self {
            #[cfg(feature = "use-compiled-tools")]
            Self::External(bin) => bin.as_ref(),
            Self::OwnedU32(v) => crate::util::from_binary(&v),
            Self::OwnedU8(v) => &v,
        }
    }
}
