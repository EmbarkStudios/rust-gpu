use proc_macro2::Ident;

use quote::{quote, TokenStreamExt};
use spirv_types::image_params::*;
use syn::parse::{Parse, ParseStream};

use self::params::SampledType;

mod kw {
    syn::custom_keyword!(u8);
    syn::custom_keyword!(u16);
    syn::custom_keyword!(u32);
    syn::custom_keyword!(u64);
    syn::custom_keyword!(i8);
    syn::custom_keyword!(i16);
    syn::custom_keyword!(i32);
    syn::custom_keyword!(i64);
    syn::custom_keyword!(f32);
    syn::custom_keyword!(f64);
}

const MISSING_SAMPLE_ERROR: &str = "Expected either `type` or `format` to be \
specified. Use `type=<sampled_type>` (e.g. `type=f32`) to specify the sampled \
type, or use `format` to set the image to a specific image format.";

/// Creates an `Image` type using the following syntax.
pub struct ImageType {
    arrayed: Arrayed,
    crate_root: Option<syn::Path>,
    depth: ImageDepth,
    dimensionality: Dimensionality,
    format: ImageFormat,
    multisampled: Multisampled,
    sampled: Sampled,
    sampled_type: SampledType,
}

impl Parse for ImageType {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut sampled_type = None;
        let mut dimensionality = None;
        let mut arrayed = None;
        let mut depth: Option<ImageDepth> = None;
        let mut format = None;
        let mut multisampled = None;
        let mut sampled: Option<Sampled> = None;
        let mut crate_root = None;

        let starting_span = input.span();

        macro_rules! set_unique {
            ($id:ident = $ex:expr) => {{
                if $id.replace($ex).is_some() {
                    return Err(syn::Error::new(
                        input.span(),
                        concat!("Unexpected duplicate parameter for `", stringify!($id), "`"),
                    ));
                }
            }};
        }

        macro_rules! peek_and_eat_value {
            ($typ:ty) => {{
                if input.peek(syn::Token![=]) {
                    input.parse::<syn::Token![=]>()?;
                    Some(input.parse::<$typ>()?)
                } else {
                    None
                }
            }}
        }

        while !input.is_empty() {
            if input.peek(syn::LitInt) {
                let int = input.parse::<syn::LitInt>().unwrap();
                set_unique!(
                    dimensionality = match (int.base10_digits(), int.suffix()) {
                        ("1", "D") | ("1", "d") => Dimensionality::OneD,
                        ("2", "D") | ("2", "d") => Dimensionality::TwoD,
                        ("3", "D") | ("3", "d") => Dimensionality::ThreeD,
                        _ => return Err(syn::Error::new(int.span(), "Unexpected integer")),
                    }
                );
            } else if input.peek(syn::Ident) {
                let ident = input.parse::<Ident>().unwrap();

                if ident == "buffer" {
                    set_unique!(dimensionality = Dimensionality::Buffer);
                } else if ident == "cube" {
                    set_unique!(dimensionality = Dimensionality::Cube);
                } else if ident == "rect" {
                    set_unique!(dimensionality = Dimensionality::Rect);
                } else if ident == "subpass" {
                    set_unique!(dimensionality = Dimensionality::SubpassData);
                } else if ident == "arrayed" {
                    set_unique!(
                        arrayed = peek_and_eat_value!(syn::LitBool)
                            .as_ref()
                            .map(syn::LitBool::value)
                            .map_or(Arrayed::True, From::from)
                    );
                } else if ident == "multisampled" {
                    set_unique!(
                        multisampled = peek_and_eat_value!(syn::LitBool)
                            .as_ref()
                            .map(syn::LitBool::value)
                            .map_or(Multisampled::True, From::from)
                    );
                } else if ident == "sampled" {
                    set_unique!(
                        sampled = peek_and_eat_value!(syn::LitBool)
                            .as_ref()
                            .map(syn::LitBool::value)
                            .map_or(Sampled::Yes, From::from)
                    );
                } else if ident == "depth" {
                    set_unique!(
                        depth = peek_and_eat_value!(syn::LitBool)
                            .as_ref()
                            .map(syn::LitBool::value)
                            .map_or(ImageDepth::True, From::from)
                    );
                } else if ident == "format" {
                    let value = peek_and_eat_value!(syn::Ident);

                    if value.is_none() {
                        return Err(syn::Error::new(
                            ident.span(),
                            "Expected argument for `format`.",
                        ));
                    }

                    let value = params::image_format_from_str(&value.unwrap().to_string());

                    if let Err(err) = value {
                        return Err(syn::Error::new(ident.span(), err));
                    }

                    format = value.ok();
                } else if ident == "__crate_root" {
                    input.parse::<syn::Token![=]>()?;
                    crate_root = Some(input.parse::<syn::Path>()?);
                }
            } else if input.peek(syn::token::Type) {
                input.parse::<syn::token::Type>()?;
                input.parse::<syn::Token![=]>()?;

                sampled_type = Some(if input.peek(kw::u8) {
                    input.parse::<kw::u8>()?;

                    SampledType::U8
                } else if input.peek(kw::u16) {
                    input.parse::<kw::u16>()?;

                    SampledType::U16
                } else if input.peek(kw::u32) {
                    input.parse::<kw::u32>()?;

                    SampledType::U32
                } else if input.peek(kw::u64) {
                    input.parse::<kw::u64>()?;

                    SampledType::U64
                } else if input.peek(kw::i8) {
                    input.parse::<kw::i8>()?;

                    SampledType::I8
                } else if input.peek(kw::i16) {
                    input.parse::<kw::i16>()?;

                    SampledType::I16
                } else if input.peek(kw::i32) {
                    input.parse::<kw::i32>()?;

                    SampledType::I32
                } else if input.peek(kw::i64) {
                    input.parse::<kw::i64>()?;

                    SampledType::I64
                } else if input.peek(kw::f32) {
                    input.parse::<kw::f32>()?;

                    SampledType::F32
                } else if input.peek(kw::f64) {
                    input.parse::<kw::f64>()?;

                    SampledType::F64
                } else {
                    return Err(syn::Error::new(
                        input.span(),
                        "Unknown value provided to `unknown(_)`.",
                    ));
                });
            }

            if input.peek(syn::Token![,]) {
                input.parse::<syn::Token![,]>()?;
                continue;
            } else {
                break;
            }
        }

        if !input.is_empty() {
            return Err(syn::Error::new(
                input.span(),
                "Unexpected trailing arguments.",
            ));
        }

        let dimensionality = dimensionality.ok_or_else(|| {
            syn::Error::new(
                starting_span,
                "Expected either `1D`, `2D`, `3D`, `cube`, `rect`, `buffer`, \
                or `subpass` to be present",
            )
        })?;

        if format.is_some() && sampled_type.is_some() {
            if format != Some(ImageFormat::Unknown) {
                return Err(syn::Error::new(
                    starting_span,
                    "Can't specify `type` with a known image format. Either \
                        specify just the `format` or use `format=unknown`.",
                ));
            }
        } else if sampled_type.is_some() {
            format = Some(ImageFormat::Unknown);
        } else if let Some(format) = &format {
            sampled_type = Some(match format {
                ImageFormat::Rgba32f
                | ImageFormat::Rgba16f
                | ImageFormat::R32f
                | ImageFormat::Rgba8
                | ImageFormat::Rgba8Snorm
                | ImageFormat::Rg32f
                | ImageFormat::Rg16f
                | ImageFormat::R11fG11fB10f
                | ImageFormat::R16f
                | ImageFormat::Rgba16
                | ImageFormat::Rgb10A2
                | ImageFormat::Rg16
                | ImageFormat::Rg8
                | ImageFormat::R16
                | ImageFormat::R8
                | ImageFormat::Rgba16Snorm
                | ImageFormat::Rg16Snorm
                | ImageFormat::Rg8Snorm
                | ImageFormat::R16Snorm
                | ImageFormat::R8Snorm => SampledType::F32,

                ImageFormat::Rgba32i
                | ImageFormat::Rgba16i
                | ImageFormat::Rgba8i
                | ImageFormat::R32i
                | ImageFormat::Rg32i
                | ImageFormat::Rg16i
                | ImageFormat::Rg8i
                | ImageFormat::R16i
                | ImageFormat::R8i => SampledType::I32,

                ImageFormat::Rgba32ui
                | ImageFormat::Rgba16ui
                | ImageFormat::Rgba8ui
                | ImageFormat::R32ui
                | ImageFormat::Rgb10A2ui
                | ImageFormat::Rg32ui
                | ImageFormat::Rg16ui
                | ImageFormat::Rg8ui
                | ImageFormat::R16ui
                | ImageFormat::R8ui => SampledType::U32,

                ImageFormat::R64ui => SampledType::U64,

                ImageFormat::R64i => SampledType::I64,

                ImageFormat::Unknown => unreachable!(),
            });
        }

        let sampled_type =
            sampled_type.ok_or_else(|| syn::Error::new(starting_span, MISSING_SAMPLE_ERROR))?;
        let format = format.ok_or_else(|| syn::Error::new(starting_span, MISSING_SAMPLE_ERROR))?;
        let depth = depth.unwrap_or(ImageDepth::Unknown);
        let arrayed = arrayed.unwrap_or(Arrayed::False);
        let multisampled = multisampled.unwrap_or(Multisampled::False);
        let sampled = sampled.unwrap_or(Sampled::Unknown);

        Ok(Self {
            arrayed,
            crate_root,
            depth,
            dimensionality,
            format,
            multisampled,
            sampled,
            sampled_type,
        })
    }
}

impl quote::ToTokens for ImageType {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = self.crate_root.clone().unwrap_or_else(|| syn::Path {
            leading_colon: None,
            segments: {
                let mut punct = syn::punctuated::Punctuated::new();
                punct.push(Ident::new("spirv_std", proc_macro2::Span::mixed_site()).into());

                punct
            },
        });
        let dimensionality = params::dimensionality_to_tokens(&self.dimensionality);
        let arrayed = params::arrayed_to_tokens(&self.arrayed);
        let depth = params::image_depth_to_tokens(&self.depth);
        let format = params::image_format_to_tokens(&self.format);
        let multisampled = params::multisampled_to_tokens(&self.multisampled);
        let sampled = params::sampled_to_tokens(&self.sampled);
        let sampled_type = &self.sampled_type;

        tokens.append_all(quote::quote! {
            #crate_root::image::Image<
                #crate_root::image::__private::#sampled_type,
                { #crate_root::image::#dimensionality as u32 },
                { #crate_root::image::#depth as u32 },
                { #crate_root::image::#arrayed as u32 },
                { #crate_root::image::#multisampled as u32 },
                { #crate_root::image::#sampled as u32 },
                { #crate_root::image::#format as u32 },
            >
        });
    }
}

mod params {
    use super::*;
    use proc_macro2::TokenStream;

    pub fn image_format_from_str(s: &str) -> Result<ImageFormat, &'static str> {
        Ok(match s {
            "rgba32f" => ImageFormat::Rgba32f,
            "rgba16f" => ImageFormat::Rgba16f,
            "r32f" => ImageFormat::R32f,
            "rgba8" => ImageFormat::Rgba8,
            "rgba8_snorm" => ImageFormat::Rgba8Snorm,
            "rg32f" => ImageFormat::Rg32f,
            "rg16f" => ImageFormat::Rg16f,
            "r11f_g11f_b10f" => ImageFormat::R11fG11fB10f,
            "r16f" => ImageFormat::R16f,
            "rgba16" => ImageFormat::Rgba16,
            "rgb10_a2" => ImageFormat::Rgb10A2,
            "rg16" => ImageFormat::Rg16,
            "rg8" => ImageFormat::Rg8,
            "r16" => ImageFormat::R16,
            "r8" => ImageFormat::R8,
            "rgba16_snorm" => ImageFormat::Rgba16Snorm,
            "rg16_snorm" => ImageFormat::Rg16Snorm,
            "rg8_snorm" => ImageFormat::Rg8Snorm,
            "r16_snorm" => ImageFormat::R16Snorm,
            "r8_snorm" => ImageFormat::R8Snorm,
            "rgba32i" => ImageFormat::Rgba32i,
            "rgba16i" => ImageFormat::Rgba16i,
            "rgba8i" => ImageFormat::Rgba8i,
            "r32i" => ImageFormat::R32i,
            "rg32i" => ImageFormat::Rg32i,
            "rg16i" => ImageFormat::Rg16i,
            "rg8i" => ImageFormat::Rg8i,
            "r16i" => ImageFormat::R16i,
            "r8i" => ImageFormat::R8i,
            "rgba32ui" => ImageFormat::Rgba32ui,
            "rgba16ui" => ImageFormat::Rgba16ui,
            "rgba8ui" => ImageFormat::Rgba8ui,
            "r32ui" => ImageFormat::R32ui,
            "rgb10_a2ui" => ImageFormat::Rgb10A2ui,
            "rg32ui" => ImageFormat::Rg32ui,
            "rg16ui" => ImageFormat::Rg16ui,
            "rg8ui" => ImageFormat::Rg8ui,
            "r16ui" => ImageFormat::R16ui,
            "r8ui" => ImageFormat::R8ui,
            "r64ui" => ImageFormat::R64ui,
            "r64i" => ImageFormat::R64i,
            _ => return Err(
                "Unknown specified image format. Use `type=<type>` instead if this is intentional.",
            ),
        })
    }

    /// The sampled type of an unknown image format.
    pub enum SampledType {
        U8,
        U16,
        U32,
        U64,
        I8,
        I16,
        I32,
        I64,
        F32,
        F64,
    }

    impl quote::ToTokens for SampledType {
        fn to_tokens(&self, stream: &mut TokenStream) {
            stream.append_all(match self {
                Self::U8 => quote!(u8),
                Self::U16 => quote!(u16),
                Self::U32 => quote!(u32),
                Self::U64 => quote!(u64),
                Self::I8 => quote!(i8),
                Self::I16 => quote!(i16),
                Self::I32 => quote!(i32),
                Self::I64 => quote!(i64),
                Self::F32 => quote!(f32),
                Self::F64 => quote!(f64),
            });
        }
    }

    pub fn image_depth_to_tokens(id: &ImageDepth) -> TokenStream {
        match id {
            ImageDepth::True => quote!(ImageDepth::True),
            ImageDepth::False => quote!(ImageDepth::False),
            ImageDepth::Unknown => quote!(ImageDepth::Unknown),
        }
    }

    pub fn arrayed_to_tokens(arrayed: &Arrayed) -> TokenStream {
        match arrayed {
            Arrayed::True => quote!(Arrayed::True),
            Arrayed::False => quote!(Arrayed::False),
        }
    }

    pub fn dimensionality_to_tokens(dim: &Dimensionality) -> TokenStream {
        match dim {
            Dimensionality::OneD => quote!(Dimensionality::OneD),
            Dimensionality::TwoD => quote!(Dimensionality::TwoD),
            Dimensionality::ThreeD => quote!(Dimensionality::ThreeD),
            Dimensionality::Rect => quote!(Dimensionality::Rect),
            Dimensionality::Cube => quote!(Dimensionality::Cube),
            Dimensionality::Buffer => quote!(Dimensionality::Buffer),
            Dimensionality::SubpassData => quote!(Dimensionality::SubpassData),
        }
    }

    pub fn multisampled_to_tokens(multisampled: &Multisampled) -> TokenStream {
        match multisampled {
            Multisampled::True => quote!(Multisampled::True),
            Multisampled::False => quote!(Multisampled::False),
        }
    }

    pub fn sampled_to_tokens(sampled: &Sampled) -> TokenStream {
        match sampled {
            Sampled::Yes => quote!(Sampled::Yes),
            Sampled::No => quote!(Sampled::No),
            Sampled::Unknown => quote!(Sampled::Unknown),
        }
    }

    pub fn image_format_to_tokens(format: &ImageFormat) -> proc_macro2::TokenStream {
        let variant = {
            let variant = match format {
                ImageFormat::Unknown => "Unknown",
                ImageFormat::Rgba32f => "Rgba32f",
                ImageFormat::Rgba16f => "Rgba16f",
                ImageFormat::R32f => "R32f",
                ImageFormat::Rgba8 => "Rgba8",
                ImageFormat::Rgba8Snorm => "Rgba8Snorm",
                ImageFormat::Rg32f => "Rg32f",
                ImageFormat::Rg16f => "Rg16f",
                ImageFormat::R11fG11fB10f => "R11fG11fB10f",
                ImageFormat::R16f => "R16f",
                ImageFormat::Rgba16 => "Rgba16",
                ImageFormat::Rgb10A2 => "Rgb10A2",
                ImageFormat::Rg16 => "Rg16",
                ImageFormat::Rg8 => "Rg8",
                ImageFormat::R16 => "R16",
                ImageFormat::R8 => "R8",
                ImageFormat::Rgba16Snorm => "Rgba16Snorm",
                ImageFormat::Rg16Snorm => "Rg16Snorm",
                ImageFormat::Rg8Snorm => "Rg8Snorm",
                ImageFormat::R16Snorm => "R16Snorm",
                ImageFormat::R8Snorm => "R8Snorm",
                ImageFormat::Rgba32i => "Rgba32i",
                ImageFormat::Rgba16i => "Rgba16i",
                ImageFormat::Rgba8i => "Rgba8i",
                ImageFormat::R32i => "R32i",
                ImageFormat::Rg32i => "Rg32i",
                ImageFormat::Rg16i => "Rg16i",
                ImageFormat::Rg8i => "Rg8i",
                ImageFormat::R16i => "R16i",
                ImageFormat::R8i => "R8i",
                ImageFormat::Rgba32ui => "Rgba32ui",
                ImageFormat::Rgba16ui => "Rgba16ui",
                ImageFormat::Rgba8ui => "Rgba8ui",
                ImageFormat::R32ui => "R32ui",
                ImageFormat::Rgb10A2ui => "Rgb10A2ui",
                ImageFormat::Rg32ui => "Rg32ui",
                ImageFormat::Rg16ui => "Rg16ui",
                ImageFormat::Rg8ui => "Rg8ui",
                ImageFormat::R16ui => "R16ui",
                ImageFormat::R8ui => "R8ui",
                ImageFormat::R64ui => "R64ui",
                ImageFormat::R64i => "R64i",
            };

            let variant = proc_macro2::Ident::new(variant, proc_macro2::Span::mixed_site());

            quote!(#variant)
        };

        quote!(ImageFormat::#variant)
    }
}
