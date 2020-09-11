
# Summary

Provide safe access to a (limited) set of LDS memory without race conditions and without violating SPIR-V rules.

# Explanation


```rust
struct LdsWriter;

impl LdsWriter {
    fn write_thread_idx(&mut self, value: T);
    fn barrier(self) -> LdsReader;
}

struct LdsReader;

impl LdsReader {
    fn read(&self, idx: usize) -> T;
    fn barrier(self) -> LdsWriter;
}
```
## Barriers in non-uniform control flow

```rust
// sample 1:
let lds = LdsWriter::<u32>::new();

lds.write_thread_idx(0);

// barrier should be in uniform control flow
let value = if something {
    lds.barrier().read(12)
} else {
    lds.barrier().read(23)
};
```

## Multiple barriers with write in one branch
```rust
// sample 2:

let lds = LdsWriter::<u32>::new();

lds.write_thread_idx(0);

let value = if something {
    lds.barrier().read(12)
} else {
    let rdr = lds.barrier();
    let value = rdr.read(23);
    
    rdr.barrier().write_thread_idx(12);
    value
};
```

## Multiple consecutive writes to same location
```rust
// sample 3:
let lds = LdsWriter::<u32>::new();

lds.write_thread_idx(0);
lds.write_thread_idx(666); // race?
```

## Multiple writes to same memory location after a read
```rust
// sample 4:
let lds = LdsWriter::<u32>::new();

lds.write_thread_idx(0);

let (value, rdr) = if something {
    let rdr = lds.barrier();
    let value = rdr.read(12)
    (value, rdr)
} else {
    let rdr = lds.barrier();
    let value = rdr.read(12)
    (value, rdr)
};

let wrt = rdr.barrier();
wrt.write_thread_idx(12234);
```

# Drawbacks

 * Limited amount of indexing for write operations

# Alternatives

@Tobski proposed making join operations explict and potentially passing in ranges in, to partition the data
```rust
struct BufferWriter;

impl BufferWriter {
    fn write_thread_idx(&mut self, value: T);
}

struct BufferReader;


impl BufferReader {
    fn read(&self, idx: usize) -> T;
    fn partition_readers(self, ranges: [(usize, usize)]) -> [BufferReader];
    fn partition_writers(self, ranges: [(usize, usize)]) -> [BufferWriter];
}

fn join_writers(a: BufferWriter, b: BufferWriter) -> BufferReader;
fn join_readers(a: BufferReader, b: BufferReader) -> BufferReader;
```

@Jasper-Bekkers proposed making thread index a type with limited safe operations to extend the writer types with some more flexibilty. One concern here is also that this should be done in uniform controlflow most likely.

```rust
struct ThreadIdx;

impl ThreadIdx {
    /// XOR shuffle with a constant
    fn butterfly_shuffle(&self, i: u32) -> ThreadIdx;

    /// Shuffle all lanes up / down by a constant
    fn up(&self, i: u32) -> ThreadIdx;
    fn down(&self, i: u32) -> ThreadIdx;

    /// Selecting arbitrary threads is unsafe
    unsafe fn new(idx: u32) -> ThreadIdx;
}
```

# Prior art

 * Regular shading languages only provide unsafe/unsound access