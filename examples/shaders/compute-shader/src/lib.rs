#![cfg_attr(
    target_arch = "spirv",
    no_std,
    feature(register_attr),
    register_attr(spirv)
)]

extern crate spirv_std;

#[cfg(not(target_arch = "spirv"))]
#[macro_use]
pub extern crate spirv_std_macros;

use spirv_std::storage_class::{Input, StorageBuffer};

// The Collatz Conjecture states that for any integer n:
// If n is even, n = n/2
// If n is odd, n = 3n+1
// And repeat this process for each new n, you will always eventually reach 1.
// Though the conjecture has not been proven, no counterexample has ever been found.
// This function returns how many times this recurrence needs to be applied to reach 1.
pub fn collatz_iterations(mut n: i32) -> i32 {
    let mut i = 0;
    while n != 1 {
        if n.rem_euclid(2) == 0 {
            n /= 2;
        } else {
            n = 3 * n + 1;
        }
        i += 1;
    }
    i
}

#[allow(unused_attributes)]
#[spirv(gl_compute(local_size_x = 1))]
pub fn main_cs(
    #[spirv(global_invocation_id)] gid: Input<i32>,
    #[spirv(storage_buffer)] mut storage: StorageBuffer<u32>,
) {
    let gid = gid.load();
    let result = collatz_iterations(gid);
    storage.store(result as u32)
}
