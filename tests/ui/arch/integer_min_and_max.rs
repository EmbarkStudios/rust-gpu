// build-pass

use spirv_std::arch::{signed_max, signed_min, unsigned_max, unsigned_min};

#[spirv(fragment)]
pub fn main() {
    assert!(unsigned_min(39_u8, 13) == 13);
    assert!(unsigned_min(39_u16, 13) == 13);
    assert!(unsigned_min(39_u32, 13) == 13);
    assert!(unsigned_min(39_u64, 13) == 13);

    assert!(unsigned_max(39_u8, 13) == 39);
    assert!(unsigned_max(39_u16, 13) == 39);
    assert!(unsigned_max(39_u32, 13) == 39);
    assert!(unsigned_max(39_u64, 13) == 39);

    assert!(signed_min(-112_i8, -45) == -112);
    assert!(signed_min(-112_i16, -45) == -112);
    assert!(signed_min(-112_i32, -45) == -112);
    assert!(signed_min(-112_i64, -45) == -112);

    assert!(signed_max(-112_i8, -45) == -45);
    assert!(signed_max(-112_i16, -45) == -45);
    assert!(signed_max(-112_i32, -45) == -45);
    assert!(signed_max(-112_i64, -45) == -45);
}
