// Hack: u8 requires the Int8 capability, so instead of compiling this to a u8, compile it to a
// u32. It's a little less efficient, but doesn't require the Int8 cap (and Arrayed values
// shouldn't be stored in memory anyway, so it's no less memory used)
#[repr(u32)]
/// The access permissions for the image.
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum AccessQualifier {
    /// A read only image.
    ReadOnly = 0,
    /// A write only image.
    WriteOnly = 1,
    /// A readable and writable image.
    ReadWrite = 2,
}

/// Whether the image uses arrayed content.
#[repr(u32)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Arrayed {
    /// The image uses not arrayed content.
    False = 0,
    /// The image uses arrayed content.
    True = 1,
}

impl From<bool> for Arrayed {
    fn from(val: bool) -> Self {
        if val {
            Self::True
        } else {
            Self::False
        }
    }
}

/// The dimension of the image.
#[repr(u32)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Dimensionality {
    /// 1D
    OneD = 0,
    /// 2D
    TwoD = 1,
    /// 3D
    ThreeD = 2,
    /// 2D Cubemap texture
    Cube = 3,
    /// 2D Rectangle texture
    Rect = 4,
    /// 1D Buffer texture
    Buffer = 5,
    /// Vulkan subpass buffer
    SubpassData = 6,
}

/// Whether a given image contains [depth] information. **Note** Whether or not
/// to perform depth comparisons is a property of the sampling code, not of this
/// type.
///
/// [depth]: https://en.wikipedia.org/wiki/Depth_map
#[repr(u32)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum ImageDepth {
    /// Indicates that the image does not contain depth information.
    False = 0,
    /// Indicates that the image contains depth information.
    True = 1,
    /// Indicates that is not known ahead of time whether the image has depth
    /// information or not.
    Unknown = 2,
}

#[cfg(any(not(target_arch = "spirv"), target_feature = "Int8"))]
impl From<Option<bool>> for ImageDepth {
    fn from(val: Option<bool>) -> Self {
        match val {
            Some(true) => Self::True,
            Some(false) => Self::False,
            None => Self::Unknown,
        }
    }
}

impl From<bool> for ImageDepth {
    fn from(val: bool) -> Self {
        match val {
            true => Self::True,
            false => Self::False,
        }
    }
}

/// Whether the image uses arrayed content.
#[repr(u32)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Multisampled {
    /// The image contains single-sampled content.
    False = 0,
    /// The image contains multisampled content.
    True = 1,
}

impl From<bool> for Multisampled {
    fn from(val: bool) -> Self {
        if val {
            Self::True
        } else {
            Self::False
        }
    }
}

/// Whether or not the image will be accessed in combination with a sampler.
#[repr(u32)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Sampled {
    /// Indicates that it is not known ahead of time whether the image will use
    /// a sampler or not.
    Unknown = 0,
    /// The image will be used with a sampler.
    Yes = 1,
    /// The image will not be used with a sampler.
    No = 2,
}

#[cfg(not(target_arch = "spirv"))]
impl From<Option<bool>> for Sampled {
    fn from(val: Option<bool>) -> Self {
        match val {
            Some(true) => Self::Yes,
            Some(false) => Self::No,
            None => Self::Unknown,
        }
    }
}

impl From<bool> for Sampled {
    fn from(val: bool) -> Self {
        match val {
            true => Self::Yes,
            false => Self::No,
        }
    }
}

/// The underlying internal representation of the image.
#[repr(u32)]
#[derive(PartialEq, Eq)]
pub enum ImageFormat {
    /// Representation not known at compile time.
    Unknown,
    /// RGBA channels, 32 bit floating point per channel.
    Rgba32f,
    /// RGBA channels, 16 bit floating point per channel.
    Rgba16f,
    /// Single red channel, 32 bit floating point.
    R32f,
    /// RGBA channels, 8 bit unsigned normalized integer per channel.
    Rgba8,
    /// RGBA channels, 8 bit signed normalized integer per channel.
    Rgba8Snorm,
    /// Red+Green channels, 32 bit floating point per channel.
    Rg32f,
    /// Red+Green channels, 16 bit floating point per channel.
    Rg16f,
    /// 32 bits containing two 11 bit floating point numbers for the Red and Green
    /// channels, and a 10 bit floating point number for the Blue channel.
    R11fG11fB10f,
    /// Single red channel, 16 bit floating point.
    R16f,
    /// RGBA channels, 16 bit unsigned normalized integer per channel.
    Rgba16,
    /// 32 bits containing three 10 bit unsigned normalized integers for the Red, Green, and Blue
    /// channels, and a 2 bit unsigned normalized integer for the Alpha channel.
    Rgb10A2,
    /// Red+Green channels, 16 bit unsigned normalized integer per channel.
    Rg16,
    /// Red+Green channels, 8 bit unsigned normalized integer per channel.
    Rg8,
    /// Single red channel, 16 bit unsigned normalized integer.
    R16,
    /// Single red channel, 8 bit unsigned normalized integer.
    R8,
    /// RGBA channels, 16 bit signed normalized integer per channel.
    Rgba16Snorm,
    /// Red+Green channels, 16 bit signed normalized integer per channel.
    Rg16Snorm,
    /// Red+Green channels, 8 bit signed normalized integer per channel.
    Rg8Snorm,
    /// Single red channel, 16 bit signed normalized integer.
    R16Snorm,
    /// Single red channel, 8 bit signed normalized integer.
    R8Snorm,
    /// RGBA channels, 32 bit signed integer per channel (not normalized).
    Rgba32i,
    /// RGBA channels, 16 bit signed integer per channel (not normalized).
    Rgba16i,
    /// RGBA channels, 8 bit signed integer per channel (not normalized).
    Rgba8i,
    /// Single red channel, 32 bit signed integer (not normalized).
    R32i,
    /// Red+Green channels, 32 bit signed integer per channel (not normalized).
    Rg32i,
    /// Red+Green channels, 16 bit signed integer per channel (not normalized).
    Rg16i,
    /// Red+Green channels, 8 bit signed integer per channel (not normalized).
    Rg8i,
    /// Single red channel, 16 bit signed integer (not normalized).
    R16i,
    /// Single red channel, 8 bit signed integer (not normalized).
    R8i,
    /// RGBA channels, 32 bit unsigned integer per channel (not normalized).
    Rgba32ui,
    /// RGBA channels, 16 bit unsigned integer per channel (not normalized).
    Rgba16ui,
    /// RGBA channels, 8 bit unsigned integer per channel (not normalized).
    Rgba8ui,
    /// Single red channel, 32 bit unsigned integer (not normalized).
    R32ui,
    /// 32 bits containing three 10 bit unsigned integers for the Red, Green, and Blue channels,
    /// and a 2 bit unsigned integer for the Alpha channel.
    Rgb10A2ui,
    /// Red+Green channels, 32 bit unsigned integer per channel (not normalized).
    Rg32ui,
    /// Red+Green channels, 16 bit unsigned integer per channel (not normalized).
    Rg16ui,
    /// Red+Green channels, 8 bit unsigned integer per channel (not normalized).
    Rg8ui,
    /// Single red channel, 16 bit unsigned integer (not normalized).
    R16ui,
    /// Single red channel, 8 bit unsigned integer (not normalized).
    R8ui,
    /// Single red channel, 64 bit unsigned integer (not normalized).
    R64ui,
    /// Single red channel, 64 bit signed integer (not normalized).
    R64i,
}
