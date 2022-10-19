// NOTE(eddyb) this tests `for` loop desugaring (with its call to `Iterator::next`
// and matching on the resulting `Option`), without relying on a `Range` iterator.
// More precisely, `Range` used to not compile, due to it using `mem::replace`,
// which, before https://github.com/rust-lang/rust/pull/83022, used to just call
// `mem::swap` (which has a block-wise optimization that can't work on SPIR-V).

// build-pass

use core::ops::Range;
use spirv_std::num_traits::Num;
use spirv_std::spirv;

struct RangeIter<T>(Range<T>);

impl<T: Num + Ord + Copy> Iterator for RangeIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        let x = self.0.start;
        if x >= self.0.end {
            None
        } else {
            self.0.start = x + T::one();
            Some(x)
        }
    }
}

#[spirv(fragment)]
pub fn main(#[spirv(flat)] i: i32) {
    for _ in RangeIter(0..i) {}
}
