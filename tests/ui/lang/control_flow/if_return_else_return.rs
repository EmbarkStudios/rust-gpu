// build-pass

use spirv_std::storage_class::Input;

#[spirv(fragment)]
pub fn main(i: Input<i32>) {
    if *i < 10 {
        return;
    } else {
        return;
    }
}
