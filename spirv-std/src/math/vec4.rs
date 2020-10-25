use crate::math::{MathExt, Vec3};
use core::{f32, ops::*};

/// A 4-dimensional vector.
///
/// A 4-dimensional vector.
///
/// This type is 16 byte aligned unless the `scalar-math` feature is enabed.
#[derive(Clone, Copy, PartialEq, PartialOrd, Debug, Default)]
// if compiling with simd enabled assume alignment needs to match the simd type
#[repr(simd)]
pub struct Vec4(pub f32, pub f32, pub f32, pub f32);

/// Creates a `Vec4`.
#[inline]
pub fn vec4(x: f32, y: f32, z: f32, w: f32) -> Vec4 {
    Vec4::new(x, y, z, w)
}

impl Vec4 {
    /// Creates a new `Vec4`.
    #[inline]
    pub const fn new(x: f32, y: f32, z: f32, w: f32) -> Self {
        Self(x, y, z, w)
    }

    /// Creates a `Vec4` with all elements set to `0.0`.
    #[inline]
    pub const fn zero() -> Self {
        Self::splat(0.0)
    }

    /// Creates a `Vec4` with all elements set to `1.0`.
    #[inline]
    pub const fn one() -> Self {
        Self::splat(1.0)
    }

    /// Creates a `Vec4` with values `[x: 1.0, y: 0.0, z: 0.0, w: 0.0]`.
    #[inline]
    pub const fn unit_x() -> Self {
        Self::new(1.0, 0.0, 0.0, 0.0)
    }

    /// Creates a `Vec4` with values `[x: 0.0, y: 1.0, z: 0.0, w: 0.0]`.
    #[inline]
    pub const fn unit_y() -> Self {
        Self::new(0.0, 1.0, 0.0, 0.0)
    }

    /// Creates a `Vec4` with values `[x: 0.0, y: 0.0, z: 1.0, w: 0.0]`.
    #[inline]
    pub const fn unit_z() -> Self {
        Self::new(0.0, 0.0, 1.0, 0.0)
    }

    /// Creates a `Vec4` with values `[x: 0.0, y: 0.0, z: 0.0, w: 1.0]`.
    #[inline]
    pub const fn unit_w() -> Self {
        Self::new(0.0, 0.0, 0.0, 1.0)
    }

    /// Creates a `Vec4` with all elements set to `v`.
    #[inline]
    pub const fn splat(v: f32) -> Self {
        Self(v, v, v, v)
    }

    /// Creates a `Vec3` from the first three elements of `self`,
    /// removing `w`.
    #[inline]
    pub fn truncate(self) -> Vec3 {
        Vec3::new(self.0, self.1, self.2)
    }

    /// Returns element `x`.
    #[inline]
    pub fn x(self) -> f32 {
        self.0
    }

    /// Returns element `y`.
    #[inline]
    pub fn y(self) -> f32 {
        self.1
    }

    /// Returns element `z`.
    #[inline]
    pub fn z(self) -> f32 {
        self.2
    }

    /// Returns element `w`.
    #[inline]
    pub fn w(self) -> f32 {
        self.3
    }

    /// Returns a mutable reference to element `x`.
    #[inline]
    pub fn x_mut(&mut self) -> &mut f32 {
        &mut self.0
    }

    /// Returns a mutable reference to element `y`.
    #[inline]
    pub fn y_mut(&mut self) -> &mut f32 {
        &mut self.1
    }

    /// Returns a mutable reference to element `z`.
    #[inline]
    pub fn z_mut(&mut self) -> &mut f32 {
        &mut self.2
    }

    /// Returns a mutable reference to element `w`.
    #[inline]
    pub fn w_mut(&mut self) -> &mut f32 {
        &mut self.3
    }

    /// Sets element `x`.
    #[inline]
    pub fn set_x(&mut self, x: f32) {
        self.0 = x;
    }

    /// Sets element `y`.
    #[inline]
    pub fn set_y(&mut self, y: f32) {
        self.1 = y;
    }

    /// Sets element `z`.
    #[inline]
    pub fn set_z(&mut self, z: f32) {
        self.2 = z;
    }

    /// Sets element `w`.
    #[inline]
    pub fn set_w(&mut self, w: f32) {
        self.3 = w;
    }

    /// Returns a `Vec4` with all elements set to the value of element `x`.
    #[inline]
    pub fn dup_x(self) -> Self {
        Self(self.0, self.0, self.0, self.0)
    }

    /// Returns a `Vec4` with all elements set to the value of element `y`.
    #[inline]
    pub fn dup_y(self) -> Self {
        Self(self.1, self.1, self.1, self.1)
    }

    /// Returns a `Vec4` with all elements set to the value of element `z`.
    #[inline]
    pub fn dup_z(self) -> Self {
        Self(self.2, self.2, self.2, self.2)
    }

    /// Returns a `Vec4` with all elements set to the value of element `w`.
    #[inline]
    pub fn dup_w(self) -> Self {
        Self(self.3, self.3, self.3, self.3)
    }

    /// Computes the 4D dot product of `self` and `other`.
    #[inline]
    pub fn dot(self, other: Self) -> f32 {
        (self.0 * other.0) + (self.1 * other.1) + (self.2 * other.2) + (self.3 * other.3)
    }

    /// Computes the 4D length of `self`.
    #[inline]
    pub fn length(self) -> f32 {
        self.dot(self).sqrt()
    }

    /// Computes the squared 4D length of `self`.
    ///
    /// This is generally faster than `Vec4::length()` as it avoids a square
    /// root operation.
    #[inline]
    pub fn length_squared(self) -> f32 {
        self.dot(self)
    }

    #[deprecated(since = "0.9.5", note = "please use `Vec4::length_recip` instead")]
    #[inline(always)]
    pub fn length_reciprocal(self) -> f32 {
        self.length_recip()
    }

    /// Computes `1.0 / Vec4::length()`.
    ///
    /// For valid results, `self` must _not_ be of length zero.
    #[inline]
    pub fn length_recip(self) -> f32 {
        self.length().recip()
    }

    /// Returns `self` normalized to length 1.0.
    ///
    /// For valid results, `self` must _not_ be of length zero.
    #[inline]
    pub fn normalize(self) -> Self {
        self * self.length_recip()
    }

    /// Returns the vertical minimum of `self` and `other`.
    ///
    /// In other words, this computes
    /// `[x: min(x1, x2), y: min(y1, y2), z: min(z1, z2), w: min(w1, w2)]`,
    /// taking the minimum of each element individually.
    #[inline]
    pub fn min(self, other: Self) -> Self {
        Self(
            self.0.min(other.0),
            self.1.min(other.1),
            self.2.min(other.2),
            self.3.min(other.3),
        )
    }

    /// Returns the vertical maximum of `self` and `other`.
    ///
    /// In other words, this computes
    /// `[x: max(x1, x2), y: max(y1, y2), z: max(z1, z2), w: max(w1, w2)]`,
    /// taking the maximum of each element individually.
    #[inline]
    pub fn max(self, other: Self) -> Self {
        Self(
            self.0.max(other.0),
            self.1.max(other.1),
            self.2.max(other.2),
            self.3.max(other.3),
        )
    }

    /// Returns the horizontal minimum of `self`'s elements.
    ///
    /// In other words, this computes `min(x, y, z, w)`.
    #[inline]
    pub fn min_element(self) -> f32 {
        self.0.min(self.1.min(self.2.min(self.3)))
    }

    /// Returns the horizontal maximum of `self`'s elements.
    ///
    /// In other words, this computes `max(x, y, z, w)`.
    #[inline]
    pub fn max_element(self) -> f32 {
        self.0.max(self.1.max(self.2.min(self.3)))
    }

    /// Creates a `Vec4` from the first four values in `slice`.
    ///
    /// # Panics
    ///
    /// Panics if `slice` is less than four elements long.
    #[inline]
    pub fn from_slice_unaligned(slice: &[f32]) -> Self {
        Self(slice[0], slice[1], slice[2], slice[3])
    }

    /// Writes the elements of `self` to the first four elements in `slice`.
    ///
    /// # Panics
    ///
    /// Panics if `slice` is less than four elements long.
    #[inline]
    pub fn write_to_slice_unaligned(self, slice: &mut [f32]) {
        slice[0] = self.0;
        slice[1] = self.1;
        slice[2] = self.2;
        slice[3] = self.3;
    }

    /// Per element multiplication/addition of the three inputs: b + (self * a)
    #[inline]
    pub fn mul_add(self, a: Self, b: Self) -> Self {
        Self(
            (self.0 * a.0) + b.0,
            (self.1 * a.1) + b.1,
            (self.2 * a.2) + b.2,
            (self.3 * a.3) + b.3,
        )
    }

    /// Returns a `Vec4` containing the absolute value of each element of `self`.
    #[inline]
    pub fn abs(self) -> Self {
        Self(self.0.abs(), self.1.abs(), self.2.abs(), self.3.abs())
    }

    /// Returns a `Vec4` containing the nearest integer to a number for each element of `self`.
    /// Round half-way cases away from 0.0.
    #[inline]
    pub fn round(self) -> Self {
        Self(
            self.0.round(),
            self.1.round(),
            self.2.round(),
            self.3.round(),
        )
    }

    /// Returns a `Vec4` containing the largest integer less than or equal to a number for each
    /// element of `self`.
    #[inline]
    pub fn floor(self) -> Self {
        Self(
            self.0.floor(),
            self.1.floor(),
            self.2.floor(),
            self.3.floor(),
        )
    }

    /// Returns a `Vec4` containing this vector raised to the power of `power`
    #[inline]
    pub fn pow(self, power: f32) -> Self {
        Self(
            self.0.pow(power),
            self.1.pow(power),
            self.2.pow(power),
            self.3.pow(power),
        )
    }

    /// Returns a `Vec4` containing this vector exp'd
    #[inline]
    pub fn exp(self) -> Self {
        Self(self.0.exp(), self.1.exp(), self.2.exp(), self.3.exp())
    }

    /// Returns a `Vec4` containing the smallest integer greater than or equal to a number for each
    /// element of `self`.
    #[inline]
    pub fn ceil(self) -> Self {
        Self(self.0.ceil(), self.1.ceil(), self.2.ceil(), self.3.ceil())
    }

    #[deprecated(since = "0.9.5", note = "please use `Vec4::recip` instead")]
    #[inline(always)]
    pub fn reciprocal(self) -> Self {
        self.recip()
    }

    /// Returns a `Vec4` containing the reciprocal `1.0/n` of each element of `self`.
    #[inline]
    pub fn recip(self) -> Self {
        // TODO: Optimize
        Self::one() / self
    }

    /// Performs a linear interpolation between `self` and `other` based on
    /// the value `s`.
    ///
    /// When `s` is `0.0`, the result will be equal to `self`.  When `s`
    /// is `1.0`, the result will be equal to `other`.
    #[inline]
    pub fn lerp(self, other: Self, s: f32) -> Self {
        self + ((other - self) * s)
    }
}

impl Div<Vec4> for Vec4 {
    type Output = Self;
    #[inline]
    fn div(self, other: Self) -> Self {
        {
            Self(
                self.0 / other.0,
                self.1 / other.1,
                self.2 / other.2,
                self.3 / other.3,
            )
        }
    }
}

impl DivAssign<Vec4> for Vec4 {
    #[inline]
    fn div_assign(&mut self, other: Self) {
        {
            self.0 /= other.0;
            self.1 /= other.1;
            self.2 /= other.2;
            self.3 /= other.3;
        }
    }
}

impl Div<f32> for Vec4 {
    type Output = Self;
    #[inline]
    fn div(self, other: f32) -> Self {
        {
            Self(
                self.0 / other,
                self.1 / other,
                self.2 / other,
                self.3 / other,
            )
        }
    }
}

impl DivAssign<f32> for Vec4 {
    #[inline]
    fn div_assign(&mut self, other: f32) {
        {
            self.0 /= other;
            self.1 /= other;
            self.2 /= other;
            self.3 /= other;
        }
    }
}

impl Div<Vec4> for f32 {
    type Output = Vec4;
    #[inline]
    fn div(self, other: Vec4) -> Vec4 {
        {
            Vec4(
                self / other.0,
                self / other.1,
                self / other.2,
                self / other.3,
            )
        }
    }
}

impl Mul<Vec4> for Vec4 {
    type Output = Self;
    #[inline]
    fn mul(self, other: Self) -> Self {
        {
            Self(
                self.0 * other.0,
                self.1 * other.1,
                self.2 * other.2,
                self.3 * other.3,
            )
        }
    }
}

impl MulAssign<Vec4> for Vec4 {
    #[inline]
    fn mul_assign(&mut self, other: Self) {
        {
            self.0 *= other.0;
            self.1 *= other.1;
            self.2 *= other.2;
            self.3 *= other.3;
        }
    }
}

impl Mul<f32> for Vec4 {
    type Output = Self;
    #[inline]
    fn mul(self, other: f32) -> Self {
        {
            Self(
                self.0 * other,
                self.1 * other,
                self.2 * other,
                self.3 * other,
            )
        }
    }
}

impl MulAssign<f32> for Vec4 {
    #[inline]
    fn mul_assign(&mut self, other: f32) {
        {
            self.0 *= other;
            self.1 *= other;
            self.2 *= other;
            self.3 *= other;
        }
    }
}

impl Mul<Vec4> for f32 {
    type Output = Vec4;
    #[inline]
    fn mul(self, other: Vec4) -> Vec4 {
        {
            Vec4(
                self * other.0,
                self * other.1,
                self * other.2,
                self * other.3,
            )
        }
    }
}

impl Add for Vec4 {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        {
            Self(
                self.0 + other.0,
                self.1 + other.1,
                self.2 + other.2,
                self.3 + other.3,
            )
        }
    }
}

impl AddAssign for Vec4 {
    #[inline]
    fn add_assign(&mut self, other: Self) {
        {
            self.0 += other.0;
            self.1 += other.1;
            self.2 += other.2;
            self.3 += other.3;
        }
    }
}

impl Sub for Vec4 {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        {
            Self(
                self.0 - other.0,
                self.1 - other.1,
                self.2 - other.2,
                self.3 - other.3,
            )
        }
    }
}

impl SubAssign for Vec4 {
    #[inline]
    fn sub_assign(&mut self, other: Self) {
        {
            self.0 -= other.0;
            self.1 -= other.1;
            self.2 -= other.2;
            self.3 -= other.3;
        }
    }
}

impl Neg for Vec4 {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        {
            Self(-self.0, -self.1, -self.2, -self.3)
        }
    }
}

impl From<(f32, f32, f32, f32)> for Vec4 {
    #[inline]
    fn from(t: (f32, f32, f32, f32)) -> Self {
        Self::new(t.0, t.1, t.2, t.3)
    }
}

impl From<Vec4> for (f32, f32, f32, f32) {
    #[inline]
    fn from(v: Vec4) -> Self {
        {
            (v.0, v.1, v.2, v.3)
        }
    }
}

impl From<[f32; 4]> for Vec4 {
    #[inline]
    fn from(a: [f32; 4]) -> Self {
        {
            Self(a[0], a[1], a[2], a[3])
        }
    }
}

impl From<Vec4> for [f32; 4] {
    #[inline]
    fn from(v: Vec4) -> Self {
        {
            [v.0, v.1, v.2, v.3]
        }
    }
}

#[test]
fn test_vec4_private() {
    assert_eq!(
        vec4(1.0, 1.0, 1.0, 1.0).mul_add(vec4(0.5, 2.0, -4.0, 0.0), vec4(-1.0, -1.0, -1.0, -1.0)),
        vec4(-0.5, 1.0, -5.0, -1.0)
    );
    assert_eq!(vec4(1.0, 2.0, 3.0, 4.0).dup_x(), vec4(1.0, 1.0, 1.0, 1.0));
    assert_eq!(vec4(1.0, 2.0, 3.0, 4.0).dup_y(), vec4(2.0, 2.0, 2.0, 2.0));
    assert_eq!(vec4(1.0, 2.0, 3.0, 4.0).dup_z(), vec4(3.0, 3.0, 3.0, 3.0));
    assert_eq!(vec4(1.0, 2.0, 4.0, 4.0).dup_w(), vec4(4.0, 4.0, 4.0, 4.0));
}
