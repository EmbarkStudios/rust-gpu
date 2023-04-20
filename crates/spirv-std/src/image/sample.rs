/// Helper trait to mimic `Option<T>`, but where the variant are types
pub trait OptionTy {
    /// Whether this is a `NoneTy` (when false) or a `SomeTy<T>` (when true)
    const EXISTS: bool;
}

impl OptionTy for NoneTy {
    const EXISTS: bool = false;
}

impl<T> OptionTy for SomeTy<T> {
    const EXISTS: bool = true;
}
/// Helper struct that denotes that the type doesn't exist, analog to `Option::None`
pub struct NoneTy;

/// Helper struct that denotes that the type does exist and is of type T, analog to `Option::Some(T)`
pub struct SomeTy<T>(T);

/// Helper struct that allows building image operands. Start with a global function that returns this
/// struct, and then chain additional calls.
/// Example: `image.sample_with(coords, params::bias(3.0).sample_index(1))`
pub struct SampleParams<B: OptionTy, L: OptionTy, S: OptionTy> {
    bias: B,
    lod: L,
    sample_index: S,
}

/// Sets the 'Bias' image operand
pub fn bias<B>(bias: B) -> SampleParams<SomeTy<B>, NoneTy, NoneTy> {
    SampleParams {
        bias: SomeTy(bias),
        lod: NoneTy,
        sample_index: NoneTy,
    }
}

/// Sets the 'Lod' image operand
pub fn lod<L>(lod: L) -> SampleParams<NoneTy, SomeTy<L>, NoneTy> {
    SampleParams {
        bias: NoneTy,
        lod: SomeTy(lod),
        sample_index: NoneTy,
    }
}

/// Sets the 'Sample' image operand
pub fn sample_index<S>(sample_index: S) -> SampleParams<NoneTy, NoneTy, SomeTy<S>> {
    SampleParams {
        bias: NoneTy,
        lod: NoneTy,
        sample_index: SomeTy(sample_index),
    }
}

impl<L: OptionTy, S: OptionTy> SampleParams<NoneTy, L, S> {
    /// Sets the 'Bias' image operand
    pub fn bias<B>(self, bias: B) -> SampleParams<SomeTy<B>, L, S> {
        SampleParams {
            bias: SomeTy(bias),
            lod: self.lod,
            sample_index: self.sample_index,
        }
    }
}

impl<B: OptionTy, S: OptionTy> SampleParams<B, NoneTy, S> {
    /// Sets the 'Lod' image operand
    pub fn lod<L>(self, lod: L) -> SampleParams<B, SomeTy<L>, S> {
        SampleParams {
            bias: self.bias,
            lod: SomeTy(lod),
            sample_index: self.sample_index,
        }
    }
}

impl<B: OptionTy, L: OptionTy> SampleParams<B, L, NoneTy> {
    /// Sets the 'Sample' image operand
    pub fn sample_index<S>(self, sample_index: S) -> SampleParams<B, L, SomeTy<S>> {
        SampleParams {
            bias: self.bias,
            lod: self.lod,
            sample_index: SomeTy(sample_index),
        }
    }
}
