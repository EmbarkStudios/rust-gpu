
# Summary

Provide safe access to a (limited) set of LDS memory without race conditions and without violating SPIR-V rules.

# Explanation


```

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

```
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
```
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
```
// sample 3:
let lds = LdsWriter::<u32>::new();

lds.write_thread_idx(0);
lds.write_thread_idx(666); // race?
```

## Multiple writes to same memory location after a read
```
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

 * No known alternatives

# Prior art

 * Regular shading languages only provide unsafe/unsound access