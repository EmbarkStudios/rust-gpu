# Inline Assembly
Rust-GPU has support for inline SPIR-V assembly. In addition the backend
provides several conveniences for writing inline assembly that are documented
below. For more information on specific instruction behaviour and syntax, please
refer to the [SPIR-V specification][spec].

[spec]: https://www.khronos.org/registry/spir-v/specs/unified1/SPIRV.html

### Additional syntax

| Syntax | Description |
| ------ | ----------- |
| `%name` | Used to refer to an abstract ID of a type. |
| `typeof{<variable>}` | Returns the type of `variable` |
| `_` (underscore) | Equivalent to `typeof{<variable>}`, but uses inference to determine the variable |
