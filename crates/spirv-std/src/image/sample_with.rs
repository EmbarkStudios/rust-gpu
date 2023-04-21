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
pub struct SomeTy<T>(pub T);

/// Helper struct that allows building image operands. Start with a global function that returns this
/// struct, and then chain additional calls. No care is taken to avoid stating multiple operands that,
/// together, make no sense, such as Lod and Grad.
/// Example: `image.sample_with(coords, sample_with::bias(3.0).sample_index(1))`
pub struct SampleParams<B: OptionTy, L: OptionTy, G: OptionTy, S: OptionTy> {
    /// 'Bias' image operand
    pub bias: B,

    /// 'Lod' image operand
    pub lod: L,

    /// 'Grad' image operand
    pub grad: G,

    /// 'Sample' image operandy
    pub sample_index: S,
}

/// Sets the 'Bias' image operand
pub fn bias<B>(bias: B) -> SampleParams<SomeTy<B>, NoneTy, NoneTy, NoneTy> {
    SampleParams {
        bias: SomeTy(bias),
        lod: NoneTy,
        grad: NoneTy,
        sample_index: NoneTy,
    }
}

/// Sets the 'Lod' image operand
pub fn lod<L>(lod: L) -> SampleParams<NoneTy, SomeTy<L>, NoneTy, NoneTy> {
    SampleParams {
        bias: NoneTy,
        lod: SomeTy(lod),
        grad: NoneTy,
        sample_index: NoneTy,
    }
}

/// Sets the 'Grad' image operand
pub fn grad<T>(grad_x: T, grad_y: T) -> SampleParams<NoneTy, NoneTy, SomeTy<(T, T)>, NoneTy> {
    SampleParams {
        bias: NoneTy,
        lod: NoneTy,
        grad: SomeTy((grad_x, grad_y)),
        sample_index: NoneTy,
    }
}

/// Sets the 'Sample' image operand
pub fn sample_index<S>(sample_index: S) -> SampleParams<NoneTy, NoneTy, NoneTy, SomeTy<S>> {
    SampleParams {
        bias: NoneTy,
        lod: NoneTy,
        grad: NoneTy,
        sample_index: SomeTy(sample_index),
    }
}

impl<L: OptionTy, G: OptionTy, S: OptionTy> SampleParams<NoneTy, L, G, S> {
    /// Sets the 'Bias' image operand
    pub fn bias<B>(self, bias: B) -> SampleParams<SomeTy<B>, L, G, S> {
        SampleParams {
            bias: SomeTy(bias),
            lod: self.lod,
            grad: self.grad,
            sample_index: self.sample_index,
        }
    }
}

impl<B: OptionTy, G: OptionTy, S: OptionTy> SampleParams<B, NoneTy, G, S> {
    /// Sets the 'Lod' image operand
    pub fn lod<L>(self, lod: L) -> SampleParams<B, SomeTy<L>, G, S> {
        SampleParams {
            bias: self.bias,
            lod: SomeTy(lod),
            grad: self.grad,
            sample_index: self.sample_index,
        }
    }
}

impl<B: OptionTy, L: OptionTy, S: OptionTy> SampleParams<B, L, NoneTy, S> {
    /// Sets the 'Lod' image operand
    pub fn grad<T>(self, grad_x: T, grad_y: T) -> SampleParams<B, L, SomeTy<(T, T)>, S> {
        SampleParams {
            bias: self.bias,
            lod: self.lod,
            grad: SomeTy((grad_x, grad_y)),
            sample_index: self.sample_index,
        }
    }
}

impl<B: OptionTy, L: OptionTy, G: OptionTy> SampleParams<B, L, G, NoneTy> {
    /// Sets the 'Sample' image operand
    pub fn sample_index<S>(self, sample_index: S) -> SampleParams<B, L, G, SomeTy<S>> {
        SampleParams {
            bias: self.bias,
            lod: self.lod,
            grad: self.grad,
            sample_index: SomeTy(sample_index),
        }
    }
}
