// build-pass

use spirv_std::storage_class::Input;

#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    while *i < 10 {
        if *i == 0 {
            break;
        }
        if *i == 1 {
            break;
        }
    }
}
