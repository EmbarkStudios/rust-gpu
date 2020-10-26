use super::{Vec2, Vec4};
use core::ops::{Add, Mul, Sub};

/// Creates a `Mat2` from two column vectors.
#[inline]
pub fn mat2(x_axis: Vec2, y_axis: Vec2) -> Mat2 {
    Mat2::from_cols(x_axis, y_axis)
}

/// A 2x2 column major matrix.
#[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
pub struct Mat2(pub Vec4);

impl Default for Mat2 {
    #[inline]
    fn default() -> Self {
        Self::identity()
    }
}

impl Mat2 {
    /// Creates a 2x2 matrix with all elements set to `0.0`.
    #[inline]
    pub const fn zero() -> Self {
        Self(Vec4::zero())
    }

    /// Creates a 2x2 identity matrix.
    #[inline]
    pub const fn identity() -> Self {
        Self(Vec4::new(1.0, 0.0, 0.0, 1.0))
    }

    /// Creates a 2x2 matrix from two column vectors.
    #[inline]
    pub fn from_cols(x_axis: Vec2, y_axis: Vec2) -> Self {
        Self(Vec4::new(x_axis.x(), x_axis.y(), y_axis.x(), y_axis.y()))
    }

    /// Creates a 2x2 matrix from a `[f32; 4]` stored in column major order.  If
    /// your data is stored in row major you will need to `transpose` the
    /// returned matrix.
    #[inline]
    pub fn from_cols_array(m: &[f32; 4]) -> Self {
        Self(Vec4::new(m[0], m[1], m[2], m[3]))
    }

    /// Creates a `[f32; 4]` storing data in column major order.
    /// If you require data in row major order `transpose` the matrix first.
    #[inline]
    pub fn to_cols_array(&self) -> [f32; 4] {
        self.0.into()
    }

    /// Creates a 2x2 matrix from a `[[f32; 2]; 2]` stored in column major
    /// order.  If your data is in row major order you will need to `transpose`
    /// the returned matrix.
    #[inline]
    pub fn from_cols_array_2d(m: &[[f32; 2]; 2]) -> Self {
        Self(Vec4::new(m[0][0], m[0][1], m[1][0], m[1][1]))
    }

    /// Creates a `[[f32; 2]; 2]` storing data in column major order.
    /// If you require data in row major order `transpose` the matrix first.
    #[inline]
    pub fn to_cols_array_2d(&self) -> [[f32; 2]; 2] {
        let (x0, y0, x1, y1) = self.0.into();
        [[x0, y0], [x1, y1]]
    }

    /// Creates a 2x2 matrix containing the given non-uniform `scale`.
    #[inline]
    pub fn from_scale(scale: Vec2) -> Self {
        let (x, y) = scale.into();
        Self(Vec4::new(x, 0.0, 0.0, y))
    }

    /// Sets the first column, the `x` axis.
    #[inline]
    pub fn set_x_axis(&mut self, x: Vec2) {
        (self.0).0 = x.x();
        (self.0).1 = x.y();
    }

    /// Sets the second column, the `y` axis.
    #[inline]
    pub fn set_y_axis(&mut self, y: Vec2) {
        (self.0).2 = y.x();
        (self.0).3 = y.y();
    }

    /// Returns the first column, the `x` axis.
    #[inline]
    pub fn x_axis(&self) -> Vec2 {
        let (x, y, _, _) = self.0.into();
        Vec2::new(x, y)
    }

    /// Returns the second column, the `y` axis.
    #[inline]
    pub fn y_axis(&self) -> Vec2 {
        let (_, _, x, y) = self.0.into();
        Vec2::new(x, y)
    }

    // #[inline]
    // pub(crate) fn col(&self, index: usize) -> Vec2 {
    //     match index {
    //         0 => self.x_axis(),
    //         1 => self.y_axis(),
    //         _ => panic!(
    //             "index out of bounds: the len is 2 but the index is {}",
    //             index
    //         ),
    //     }
    // }

    // #[inline]
    // pub(crate) fn col_mut(&mut self, index: usize) -> &mut Vec2 {
    //     match index {
    //         0 => unsafe { &mut *(self.0.as_mut().as_mut_ptr() as *mut Vec2) },
    //         1 => unsafe { &mut *(self.0.as_mut()[2..].as_mut_ptr() as *mut Vec2) },
    //         _ => panic!(
    //             "index out of bounds: the len is 2 but the index is {}",
    //             index
    //         ),
    //     }
    // }

    /// Returns the transpose of `self`.
    #[inline]
    pub fn transpose(&self) -> Self {
        let (m00, m01, m10, m11) = self.0.into();
        Self(Vec4::new(m00, m10, m01, m11))
    }

    /// Returns the determinant of `self`.
    #[inline]
    pub fn determinant(&self) -> f32 {
        let (a, b, c, d) = self.0.into();
        a * d - b * c
    }

    /// Returns the inverse of `self`.
    ///
    /// If the matrix is not invertible the returned matrix will be invalid.
    #[inline]
    pub fn inverse(&self) -> Self {
        let (a, b, c, d) = self.0.into();
        let det = a * d - b * c;
        let tmp = Vec4::new(1.0, -1.0, -1.0, 1.0) / det;
        Self(Vec4::new(d, b, c, a) * tmp)
    }

    /// Transforms a `Vec2`.
    #[inline]
    pub fn mul_vec2(&self, other: Vec2) -> Vec2 {
        // TODO: SSE2
        let other = Vec4::new(other.x(), other.x(), other.y(), other.y());
        let tmp = self.0 * other;
        let (x0, y0, x1, y1) = tmp.into();
        Vec2::new(x0 + x1, y0 + y1)
    }

    /// Multiplies two 2x2 matrices.
    #[inline]
    pub fn mul_mat2(&self, other: &Self) -> Self {
        // TODO: SSE2
        let (x0, y0, x1, y1) = other.0.into();
        Self::from_cols(
            self.mul_vec2(Vec2::new(x0, y0)),
            self.mul_vec2(Vec2::new(x1, y1)),
        )
    }

    /// Adds two 2x2 matrices.
    #[inline]
    pub fn add_mat2(&self, other: &Self) -> Self {
        Self(self.0 + other.0)
    }

    /// Subtracts two 2x2 matrices.
    #[inline]
    pub fn sub_mat2(&self, other: &Self) -> Self {
        Self(self.0 - other.0)
    }

    /// Multiplies a 2x2 matrix by a scalar.
    #[inline]
    pub fn mul_scalar(&self, other: f32) -> Self {
        let s = Vec4::splat(other);
        Self(self.0 * s)
    }
}

impl Add<Mat2> for Mat2 {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        self.add_mat2(&other)
    }
}

impl Sub<Mat2> for Mat2 {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        self.sub_mat2(&other)
    }
}

impl Mul<Mat2> for Mat2 {
    type Output = Self;
    #[inline]
    fn mul(self, other: Self) -> Self {
        self.mul_mat2(&other)
    }
}

impl Mul<Vec2> for Mat2 {
    type Output = Vec2;
    #[inline]
    fn mul(self, other: Vec2) -> Vec2 {
        self.mul_vec2(other)
    }
}

impl Mul<Mat2> for f32 {
    type Output = Mat2;
    #[inline]
    fn mul(self, other: Mat2) -> Mat2 {
        other.mul_scalar(self)
    }
}

impl Mul<f32> for Mat2 {
    type Output = Self;
    #[inline]
    fn mul(self, other: f32) -> Self {
        self.mul_scalar(other)
    }
}
