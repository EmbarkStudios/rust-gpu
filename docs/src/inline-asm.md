# Inline Assembly
Rust-GPU has support for inline SPIR-V assembly. In addition the backend
provides several conveniences for writing inline assembly that are documented
below. For more information on specific instruction behaviour and syntax, please
refer to the [SPIR-V specification][spec].

[spec]: https://www.khronos.org/registry/spir-v/specs/unified1/SPIRV.html

### Basic syntax & usage.
You can write inline assembly using the new `asm!` macro available with the
`asm` feature on nightly. Refer to the [Rust unstable book][asm-docs] for more
information on how to use the macro.

[asm-docs]: https://doc.rust-lang.org/unstable-book/library-features/asm.html

Non-ID arguments are written as-is, e.g.

```rust
asm! {
    "OpCapability DerivativeControl"
}
```

ID based arguments are prefixed with `%` and their name. `Result<id>`s accessed
with a `=` and a ID on the left hand side of the expression. E.g.

```rust
let vector = spirv_std::glam::Vec2::new(1.0, 0.0);
let mut result = f32::default();

asm! {
    "%vector = OpLoad _ {vector}",
    "%element = OpVectorExtractDynamic _ %vector {index}",
    "OpStore {element} %element",
    vector = in(reg) &vector,
    index = in(reg) index,
    element = in(reg) &mut result
}
```

`asm!` only accepts integers, floats, SIMD vectors, pointers and
function pointers as input variables. However you can have the pointer point
to a generic variable, so you can write generic assembly code like so.

```rust
use spirv_std::{scalar::Scalar, vector::Vector};

// This fn is available as `spirv_std::arch::vector_extract_dynamic`
pub unsafe fn vector_extract_dynamic<T: Scalar, V: Vector<T>>(vector: V, index: usize) -> T {
    let mut result = T::default();

    asm! {
        "%vector = OpLoad _ {vector}",
        "%element = OpVectorExtractDynamic _ %vector {index}",
        "OpStore {element} %element",
        vector = in(reg) &vector,
        index = in(reg) index,
        element = in(reg) &mut result
    }

    result
}
```

### Additional syntax

| Syntax | Description |
| ------ | ----------- |
| `%<name>` | Used to refer to an abstract ID, every unique `<name>` use generates a new ID. |
| `typeof{<variable>}` | Returns the type of `variable` |
| `_` (underscore) | Equivalent to `typeof{<variable>}`, but uses inference to determine the variable |
