use super::Vec3;
use core::ops::{Add, Mul, Sub};

/// Creates a `Mat3` from three column vectors.
#[inline]
pub fn mat3(x_axis: Vec3, y_axis: Vec3, z_axis: Vec3) -> Mat3 {
    Mat3 {
        x_axis,
        y_axis,
        z_axis,
    }
}

/// A 3x3 column major matrix.
#[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
pub struct Mat3 {
    pub x_axis: Vec3,
    pub y_axis: Vec3,
    pub z_axis: Vec3,
}

impl Default for Mat3 {
    #[inline]
    fn default() -> Self {
        Self::identity()
    }
}

impl Mat3 {
    /// Creates a 3x3 matrix with all elements set to `0.0`.
    #[inline]
    pub const fn zero() -> Self {
        Self {
            x_axis: Vec3::zero(),
            y_axis: Vec3::zero(),
            z_axis: Vec3::zero(),
        }
    }

    /// Creates a 3x3 identity matrix.
    #[inline]
    pub const fn identity() -> Self {
        Self {
            x_axis: Vec3::new(1.0, 0.0, 0.0),
            y_axis: Vec3::new(0.0, 1.0, 0.0),
            z_axis: Vec3::new(0.0, 0.0, 1.0),
        }
    }

    /// Creates a 3x3 matrix from three column vectors.
    #[inline]
    pub fn from_cols(x_axis: Vec3, y_axis: Vec3, z_axis: Vec3) -> Self {
        Self {
            x_axis,
            y_axis,
            z_axis,
        }
    }

    /// Creates a 3x3 matrix from a `[f32; 9]` stored in column major order.
    /// If your data is stored in row major you will need to `transpose` the
    /// returned matrix.
    #[inline]
    pub fn from_cols_array(m: &[f32; 9]) -> Self {
        Self {
            x_axis: Vec3::new(m[0], m[1], m[2]),
            y_axis: Vec3::new(m[3], m[4], m[5]),
            z_axis: Vec3::new(m[6], m[7], m[8]),
        }
    }

    /// Creates a `[f32; 9]` storing data in column major order.
    /// If you require data in row major order `transpose` the matrix first.
    #[inline]
    pub fn to_cols_array(&self) -> [f32; 9] {
        let (m00, m01, m02) = self.x_axis.into();
        let (m10, m11, m12) = self.y_axis.into();
        let (m20, m21, m22) = self.z_axis.into();
        [m00, m01, m02, m10, m11, m12, m20, m21, m22]
    }

    /// Creates a 3x3 matrix from a `[[f32; 3]; 3]` stored in column major order.
    /// If your data is in row major order you will need to `transpose` the
    /// returned matrix.
    #[inline]
    pub fn from_cols_array_2d(m: &[[f32; 3]; 3]) -> Self {
        Self {
            x_axis: m[0].into(),
            y_axis: m[1].into(),
            z_axis: m[2].into(),
        }
    }

    /// Creates a `[[f32; 3]; 3]` storing data in column major order.
    /// If you require data in row major order `transpose` the matrix first.
    #[inline]
    pub fn to_cols_array_2d(&self) -> [[f32; 3]; 3] {
        [self.x_axis.into(), self.y_axis.into(), self.z_axis.into()]
    }

    /// Creates a 3x3 non-uniform scale matrix.
    #[inline]
    pub fn from_scale(scale: Vec3) -> Self {
        // TODO: should have a affine 2D scale and a 3d scale?
        // Do not panic as long as any component is non-zero
        let (x, y, z) = scale.into();
        Self {
            x_axis: Vec3::new(x, 0.0, 0.0),
            y_axis: Vec3::new(0.0, y, 0.0),
            z_axis: Vec3::new(0.0, 0.0, z),
        }
    }

    /// Sets the first column, the `x` axis.
    #[inline]
    pub fn set_x_axis(&mut self, x: Vec3) {
        self.x_axis = x;
    }

    /// Sets the second column, the `y` axis.
    #[inline]
    pub fn set_y_axis(&mut self, y: Vec3) {
        self.y_axis = y;
    }

    /// Sets the third column, the `z` axis.
    #[inline]
    pub fn set_z_axis(&mut self, z: Vec3) {
        self.z_axis = z;
    }

    /// Returns the first column, the `x` axis.
    #[inline]
    pub fn x_axis(&self) -> Vec3 {
        self.x_axis
    }

    /// Returns the second column, the `y` axis.
    #[inline]
    pub fn y_axis(&self) -> Vec3 {
        self.y_axis
    }

    /// Returns the third column, the `z` axis.
    #[inline]
    pub fn z_axis(&self) -> Vec3 {
        self.z_axis
    }

    /// Returns a mutable reference to the first column, the `x` axis.
    #[inline]
    pub fn x_axis_mut(&mut self) -> &mut Vec3 {
        &mut self.x_axis
    }

    /// Returns a mutable reference to the second column, the `y` axis.
    #[inline]
    pub fn y_axis_mut(&mut self) -> &mut Vec3 {
        &mut self.y_axis
    }

    /// Returns a mutable reference to the third column, the `z` axis.
    #[inline]
    pub fn z_axis_mut(&mut self) -> &mut Vec3 {
        &mut self.z_axis
    }

    // #[inline]
    // pub(crate) fn col(&self, index: usize) -> Vec3 {
    //     match index {
    //         0 => self.x_axis,
    //         1 => self.y_axis,
    //         2 => self.z_axis,
    //         _ => panic!(
    //             "index out of bounds: the len is 3 but the index is {}",
    //             index
    //         ),
    //     }
    // }

    // #[inline]
    // pub(crate) fn col_mut(&mut self, index: usize) -> &mut Vec3 {
    //     match index {
    //         0 => &mut self.x_axis,
    //         1 => &mut self.y_axis,
    //         2 => &mut self.z_axis,
    //         _ => panic!(
    //             "index out of bounds: the len is 3 but the index is {}",
    //             index
    //         ),
    //     }
    // }

    /// Returns the transpose of `self`.
    #[inline]
    pub fn transpose(&self) -> Self {
        Self {
            x_axis: Vec3::new(self.x_axis.0, self.y_axis.0, self.z_axis.0),
            y_axis: Vec3::new(self.x_axis.1, self.y_axis.1, self.z_axis.1),
            z_axis: Vec3::new(self.x_axis.2, self.y_axis.2, self.z_axis.2),
        }
    }

    /// Returns the determinant of `self`.
    #[inline]
    pub fn determinant(&self) -> f32 {
        self.z_axis.dot(self.x_axis.cross(self.y_axis))
    }

    /// Returns the inverse of `self`.
    ///
    /// If the matrix is not invertible the returned matrix will be invalid.
    pub fn inverse(&self) -> Self {
        let tmp0 = self.y_axis.cross(self.z_axis);
        let tmp1 = self.z_axis.cross(self.x_axis);
        let tmp2 = self.x_axis.cross(self.y_axis);
        let det = self.z_axis.dot_as_vec3(tmp2);
        let inv_det = det.recip();
        // TODO: Work out if it's possible to get rid of the transpose
        Self::from_cols(tmp0 * inv_det, tmp1 * inv_det, tmp2 * inv_det).transpose()
    }

    /// Multiplies two 3x3 matrices.
    #[inline]
    pub fn mul_mat3(&self, other: &Self) -> Self {
        Self {
            x_axis: self.mul_vec3(other.x_axis),
            y_axis: self.mul_vec3(other.y_axis),
            z_axis: self.mul_vec3(other.z_axis),
        }
    }

    /// Adds two 3x3 matrices.
    #[inline]
    pub fn add_mat3(&self, other: &Self) -> Self {
        Self {
            x_axis: self.x_axis + other.x_axis,
            y_axis: self.y_axis + other.y_axis,
            z_axis: self.z_axis + other.z_axis,
        }
    }

    /// Subtracts two 3x3 matrices.
    #[inline]
    pub fn sub_mat3(&self, other: &Self) -> Self {
        Self {
            x_axis: self.x_axis - other.x_axis,
            y_axis: self.y_axis - other.y_axis,
            z_axis: self.z_axis - other.z_axis,
        }
    }

    /// Transforms a `Vec3`.
    #[inline]
    pub fn mul_vec3(&self, other: Vec3) -> Vec3 {
        let mut res = self.x_axis * Vec3::splat(other.x());
        res = self.y_axis.mul_add(Vec3::splat(other.y()), res);
        res = self.z_axis.mul_add(Vec3::splat(other.z()), res);
        res
    }

    #[inline]
    /// Multiplies a 3x3 matrix by a scalar.
    pub fn mul_scalar(&self, other: f32) -> Self {
        let s = Vec3::splat(other);
        Self {
            x_axis: self.x_axis * s,
            y_axis: self.y_axis * s,
            z_axis: self.z_axis * s,
        }
    }
}

impl Add<Mat3> for Mat3 {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        self.add_mat3(&other)
    }
}

impl Sub<Mat3> for Mat3 {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        self.sub_mat3(&other)
    }
}

impl Mul<Mat3> for Mat3 {
    type Output = Self;
    #[inline]
    fn mul(self, other: Self) -> Self {
        self.mul_mat3(&other)
    }
}

impl Mul<Vec3> for Mat3 {
    type Output = Vec3;
    #[inline]
    fn mul(self, other: Vec3) -> Vec3 {
        self.mul_vec3(other)
    }
}

impl Mul<Mat3> for f32 {
    type Output = Mat3;
    #[inline]
    fn mul(self, other: Mat3) -> Mat3 {
        other.mul_scalar(self)
    }
}

impl Mul<f32> for Mat3 {
    type Output = Self;
    #[inline]
    fn mul(self, other: f32) -> Self {
        self.mul_scalar(other)
    }
}
