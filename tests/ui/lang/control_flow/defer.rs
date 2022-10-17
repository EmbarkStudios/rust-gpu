// build-pass

use spirv_std::spirv;

#[spirv(fragment)]
pub fn main(#[spirv(flat)] i: i32) {
    while i < 32 {
        let current_position = 0;
        if i < current_position {
            break;
        }
        if i < current_position {
            break;
        }
    }
}
