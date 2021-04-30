use rustc_span::symbol::Symbol;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TargetFeature {
    Extension(Symbol),
    Capability(rspirv::spirv::Capability),
}

impl std::str::FromStr for TargetFeature {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        const EXT_PREFIX: &str = "ext:";

        if let Some(input) = input.strip_prefix(EXT_PREFIX) {
            Ok(Self::Extension(Symbol::intern(input)))
        } else {
            Ok(Self::Capability(input.parse().map_err(|_err| {
                format!("Invalid Capability: `{}`", input)
            })?))
        }
    }
}
