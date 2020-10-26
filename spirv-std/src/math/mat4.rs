use super::{Vec3, Vec4};
use core::ops::{Add, Mul, Sub};

/// Creates a `Mat4` from four column vectors.
#[inline]
pub fn mat4(x_axis: Vec4, y_axis: Vec4, z_axis: Vec4, w_axis: Vec4) -> Mat4 {
    Mat4 {
        x_axis,
        y_axis,
        z_axis,
        w_axis,
    }
}

/// A 4x4 column major matrix.
///
/// This type is 16 byte aligned.
#[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
pub struct Mat4 {
    pub x_axis: Vec4,
    pub y_axis: Vec4,
    pub z_axis: Vec4,
    pub w_axis: Vec4,
}

impl Default for Mat4 {
    #[inline]
    fn default() -> Self {
        Self::identity()
    }
}

impl Mat4 {
    /// Creates a 4x4 matrix with all elements set to `0.0`.
    #[inline]
    pub const fn zero() -> Self {
        Self {
            x_axis: Vec4::zero(),
            y_axis: Vec4::zero(),
            z_axis: Vec4::zero(),
            w_axis: Vec4::zero(),
        }
    }

    /// Creates a 4x4 identity matrix.
    #[inline]
    pub const fn identity() -> Self {
        Self {
            x_axis: Vec4::new(1.0, 0.0, 0.0, 0.0),
            y_axis: Vec4::new(0.0, 1.0, 0.0, 0.0),
            z_axis: Vec4::new(0.0, 0.0, 1.0, 0.0),
            w_axis: Vec4::new(0.0, 0.0, 0.0, 1.0),
        }
    }

    /// Creates a 4x4 matrix from four column vectors.
    #[inline]
    pub fn from_cols(x_axis: Vec4, y_axis: Vec4, z_axis: Vec4, w_axis: Vec4) -> Self {
        Self {
            x_axis,
            y_axis,
            z_axis,
            w_axis,
        }
    }

    /// Creates a 4x4 matrix from a `[f32; 16]` stored in column major order.
    /// If your data is stored in row major you will need to `transpose` the
    /// returned matrix.
    #[inline]
    pub fn from_cols_array(m: &[f32; 16]) -> Self {
        Self {
            x_axis: Vec4::new(m[0], m[1], m[2], m[3]),
            y_axis: Vec4::new(m[4], m[5], m[6], m[7]),
            z_axis: Vec4::new(m[8], m[9], m[10], m[11]),
            w_axis: Vec4::new(m[12], m[13], m[14], m[15]),
        }
    }

    /// Creates a `[f32; 16]` storing data in column major order.
    /// If you require data in row major order `transpose` the matrix first.
    #[inline]
    pub fn to_cols_array(&self) -> [f32; 16] {
        *self.as_ref()
    }

    /// Creates a 4x4 matrix from a `[[f32; 4]; 4]` stored in column major
    /// order.  If your data is in row major order you will need to `transpose`
    /// the returned matrix.
    #[inline]
    pub fn from_cols_array_2d(m: &[[f32; 4]; 4]) -> Self {
        Self {
            x_axis: m[0].into(),
            y_axis: m[1].into(),
            z_axis: m[2].into(),
            w_axis: m[3].into(),
        }
    }

    /// Creates a `[[f32; 4]; 4]` storing data in column major order.
    /// If you require data in row major order `transpose` the matrix first.
    #[inline]
    pub fn to_cols_array_2d(&self) -> [[f32; 4]; 4] {
        [
            self.x_axis.into(),
            self.y_axis.into(),
            self.z_axis.into(),
            self.w_axis.into(),
        ]
    }

    /// Creates a 4x4 homogeneous transformation matrix from the given `translation`.
    #[inline]
    pub fn from_translation(translation: Vec3) -> Self {
        Self {
            x_axis: Vec4::unit_x(),
            y_axis: Vec4::unit_y(),
            z_axis: Vec4::unit_z(),
            w_axis: translation.extend(1.0),
        }
    }

    /// Creates a 4x4 homogeneous transformation matrix containing the given
    /// non-uniform `scale`.
    #[inline]
    pub fn from_scale(scale: Vec3) -> Self {
        // Do not panic as long as any component is non-zero
        let (x, y, z) = scale.into();
        Self {
            x_axis: Vec4::new(x, 0.0, 0.0, 0.0),
            y_axis: Vec4::new(0.0, y, 0.0, 0.0),
            z_axis: Vec4::new(0.0, 0.0, z, 0.0),
            w_axis: Vec4::unit_w(),
        }
    }

    /// Sets the first column, the `x` axis.
    #[inline]
    pub fn set_x_axis(&mut self, x: Vec4) {
        self.x_axis = x;
    }

    /// Sets the second column, the `y` axis.
    #[inline]
    pub fn set_y_axis(&mut self, y: Vec4) {
        self.y_axis = y;
    }

    /// Sets the third column, the `z` axis.
    #[inline]
    pub fn set_z_axis(&mut self, z: Vec4) {
        self.z_axis = z;
    }

    /// Sets the fourth column, the `w` axis.
    #[inline]
    pub fn set_w_axis(&mut self, w: Vec4) {
        self.w_axis = w;
    }

    /// Returns the first column, the `x` axis.
    #[inline]
    pub fn x_axis(&self) -> Vec4 {
        self.x_axis
    }

    /// Returns the second column, the `y` axis.
    #[inline]
    pub fn y_axis(&self) -> Vec4 {
        self.y_axis
    }

    /// Returns the third column, the `z` axis.
    #[inline]
    pub fn z_axis(&self) -> Vec4 {
        self.z_axis
    }

    /// Returns the fourth column, the `w` axis.
    #[inline]
    pub fn w_axis(&self) -> Vec4 {
        self.w_axis
    }

    /// Returns a mutable reference to the first column, the `x` axis.
    #[inline]
    pub fn x_axis_mut(&mut self) -> &mut Vec4 {
        &mut self.x_axis
    }

    /// Returns a mutable reference to the second column, the `y` axis.
    #[inline]
    pub fn y_axis_mut(&mut self) -> &mut Vec4 {
        &mut self.y_axis
    }

    /// Returns a mutable reference to the third column, the `z` axis.
    #[inline]
    pub fn z_axis_mut(&mut self) -> &mut Vec4 {
        &mut self.z_axis
    }

    /// Returns a mutable reference to the fourth column, the `w` axis.
    #[inline]
    pub fn w_axis_mut(&mut self) -> &mut Vec4 {
        &mut self.w_axis
    }

    // #[inline]
    // pub(crate) fn col(&self, index: usize) -> Vec4 {
    //     match index {
    //         0 => self.x_axis,
    //         1 => self.y_axis,
    //         2 => self.z_axis,
    //         3 => self.w_axis,
    //         _ => panic!(
    //             "index out of bounds: the len is 4 but the index is {}",
    //             index
    //         ),
    //     }
    // }

    // #[inline]
    // pub(crate) fn col_mut(&mut self, index: usize) -> &mut Vec4 {
    //     match index {
    //         0 => &mut self.x_axis,
    //         1 => &mut self.y_axis,
    //         2 => &mut self.z_axis,
    //         3 => &mut self.w_axis,
    //         _ => panic!(
    //             "index out of bounds: the len is 4 but the index is {}",
    //             index
    //         ),
    //     }
    // }

    /// Returns the transpose of `self`.
    #[inline]
    pub fn transpose(&self) -> Self {
        let (m00, m01, m02, m03) = self.x_axis.into();
        let (m10, m11, m12, m13) = self.y_axis.into();
        let (m20, m21, m22, m23) = self.z_axis.into();
        let (m30, m31, m32, m33) = self.w_axis.into();

        Self {
            x_axis: Vec4::new(m00, m10, m20, m30),
            y_axis: Vec4::new(m01, m11, m21, m31),
            z_axis: Vec4::new(m02, m12, m22, m32),
            w_axis: Vec4::new(m03, m13, m23, m33),
        }
    }

    /// Returns the determinant of `self`.
    #[inline]
    pub fn determinant(&self) -> f32 {
        let (m00, m01, m02, m03) = self.x_axis.into();
        let (m10, m11, m12, m13) = self.y_axis.into();
        let (m20, m21, m22, m23) = self.z_axis.into();
        let (m30, m31, m32, m33) = self.w_axis.into();

        let a2323 = m22 * m33 - m23 * m32;
        let a1323 = m21 * m33 - m23 * m31;
        let a1223 = m21 * m32 - m22 * m31;
        let a0323 = m20 * m33 - m23 * m30;
        let a0223 = m20 * m32 - m22 * m30;
        let a0123 = m20 * m31 - m21 * m30;

        m00 * (m11 * a2323 - m12 * a1323 + m13 * a1223)
            - m01 * (m10 * a2323 - m12 * a0323 + m13 * a0223)
            + m02 * (m10 * a1323 - m11 * a0323 + m13 * a0123)
            - m03 * (m10 * a1223 - m11 * a0223 + m12 * a0123)
    }

    /// Returns the inverse of `self`.
    ///
    /// If the matrix is not invertible the returned matrix will be invalid.
    pub fn inverse(&self) -> Self {
        let (m00, m01, m02, m03) = self.x_axis.into();
        let (m10, m11, m12, m13) = self.y_axis.into();
        let (m20, m21, m22, m23) = self.z_axis.into();
        let (m30, m31, m32, m33) = self.w_axis.into();

        let coef00 = m22 * m33 - m32 * m23;
        let coef02 = m12 * m33 - m32 * m13;
        let coef03 = m12 * m23 - m22 * m13;

        let coef04 = m21 * m33 - m31 * m23;
        let coef06 = m11 * m33 - m31 * m13;
        let coef07 = m11 * m23 - m21 * m13;

        let coef08 = m21 * m32 - m31 * m22;
        let coef10 = m11 * m32 - m31 * m12;
        let coef11 = m11 * m22 - m21 * m12;

        let coef12 = m20 * m33 - m30 * m23;
        let coef14 = m10 * m33 - m30 * m13;
        let coef15 = m10 * m23 - m20 * m13;

        let coef16 = m20 * m32 - m30 * m22;
        let coef18 = m10 * m32 - m30 * m12;
        let coef19 = m10 * m22 - m20 * m12;

        let coef20 = m20 * m31 - m30 * m21;
        let coef22 = m10 * m31 - m30 * m11;
        let coef23 = m10 * m21 - m20 * m11;

        let fac0 = Vec4::new(coef00, coef00, coef02, coef03);
        let fac1 = Vec4::new(coef04, coef04, coef06, coef07);
        let fac2 = Vec4::new(coef08, coef08, coef10, coef11);
        let fac3 = Vec4::new(coef12, coef12, coef14, coef15);
        let fac4 = Vec4::new(coef16, coef16, coef18, coef19);
        let fac5 = Vec4::new(coef20, coef20, coef22, coef23);

        let vec0 = Vec4::new(m10, m00, m00, m00);
        let vec1 = Vec4::new(m11, m01, m01, m01);
        let vec2 = Vec4::new(m12, m02, m02, m02);
        let vec3 = Vec4::new(m13, m03, m03, m03);

        let inv0 = vec1 * fac0 - vec2 * fac1 + vec3 * fac2;
        let inv1 = vec0 * fac0 - vec2 * fac3 + vec3 * fac4;
        let inv2 = vec0 * fac1 - vec1 * fac3 + vec3 * fac5;
        let inv3 = vec0 * fac2 - vec1 * fac4 + vec2 * fac5;

        let sign_a = Vec4::new(1.0, -1.0, 1.0, -1.0);
        let sign_b = Vec4::new(-1.0, 1.0, -1.0, 1.0);

        let inverse = Self {
            x_axis: inv0 * sign_a,
            y_axis: inv1 * sign_b,
            z_axis: inv2 * sign_a,
            w_axis: inv3 * sign_b,
        };

        let col0 = Vec4::new(
            inverse.x_axis.x(),
            inverse.y_axis.x(),
            inverse.z_axis.x(),
            inverse.w_axis.x(),
        );

        let dot0 = self.x_axis * col0;
        let dot1 = dot0.x() + dot0.y() + dot0.z() + dot0.w();

        let rcp_det = 1.0 / dot1;
        inverse * rcp_det
    }

    /// Transforms a 4D vector.
    #[inline]
    pub fn mul_vec4(&self, other: Vec4) -> Vec4 {
        let mut res = self.x_axis * other.dup_x();
        res = self.y_axis.mul_add(other.dup_y(), res);
        res = self.z_axis.mul_add(other.dup_z(), res);
        res = self.w_axis.mul_add(other.dup_w(), res);
        res
    }

    /// Multiplies two 4x4 matrices.
    #[inline]
    pub fn mul_mat4(&self, other: &Self) -> Self {
        Self {
            x_axis: self.mul_vec4(other.x_axis),
            y_axis: self.mul_vec4(other.y_axis),
            z_axis: self.mul_vec4(other.z_axis),
            w_axis: self.mul_vec4(other.w_axis),
        }
    }

    /// Adds two 4x4 matrices.
    #[inline]
    pub fn add_mat4(&self, other: &Self) -> Self {
        Self {
            x_axis: self.x_axis + other.x_axis,
            y_axis: self.y_axis + other.y_axis,
            z_axis: self.z_axis + other.z_axis,
            w_axis: self.w_axis + other.w_axis,
        }
    }

    /// Subtracts two 4x4 matrices.
    #[inline]
    pub fn sub_mat4(&self, other: &Self) -> Self {
        Self {
            x_axis: self.x_axis - other.x_axis,
            y_axis: self.y_axis - other.y_axis,
            z_axis: self.z_axis - other.z_axis,
            w_axis: self.w_axis - other.w_axis,
        }
    }

    /// Multiplies this matrix by a scalar value.
    #[inline]
    pub fn mul_scalar(&self, other: f32) -> Self {
        let s = Vec4::splat(other);
        Self {
            x_axis: self.x_axis * s,
            y_axis: self.y_axis * s,
            z_axis: self.z_axis * s,
            w_axis: self.w_axis * s,
        }
    }
}

impl AsRef<[f32; 16]> for Mat4 {
    #[inline]
    fn as_ref(&self) -> &[f32; 16] {
        unsafe { &*(self as *const Self as *const [f32; 16]) }
    }
}

impl AsMut<[f32; 16]> for Mat4 {
    #[inline]
    fn as_mut(&mut self) -> &mut [f32; 16] {
        unsafe { &mut *(self as *mut Self as *mut [f32; 16]) }
    }
}

impl Add<Mat4> for Mat4 {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        self.add_mat4(&other)
    }
}

impl Sub<Mat4> for Mat4 {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        self.sub_mat4(&other)
    }
}

impl Mul<Mat4> for Mat4 {
    type Output = Self;
    #[inline]
    fn mul(self, other: Self) -> Self {
        self.mul_mat4(&other)
    }
}

impl Mul<Vec4> for Mat4 {
    type Output = Vec4;
    #[inline]
    fn mul(self, other: Vec4) -> Vec4 {
        self.mul_vec4(other)
    }
}

impl Mul<Mat4> for f32 {
    type Output = Mat4;
    #[inline]
    fn mul(self, other: Mat4) -> Mat4 {
        other.mul_scalar(self)
    }
}

impl Mul<f32> for Mat4 {
    type Output = Self;
    #[inline]
    fn mul(self, other: f32) -> Self {
        self.mul_scalar(other)
    }
}
