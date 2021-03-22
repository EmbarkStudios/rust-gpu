//! Types for handling memory ordering constraints for concurrent memory access.

#[derive(Debug, PartialEq, Eq)]
pub enum Scope {
    /// Crosses multiple devices.
    CrossDevice = 0,

    /// The current device.
    Device = 1,

    /// The current workgroup.
    Workgroup = 2,

    /// The current subgroup.
    Subgroup = 3,

    /// The current invocation.
    Invocation = 4,

    /// The current queue family.
    QueueFamily = 5,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Semantics {
    /// No memory semantics.
    None = 0,

    /// On an atomic instruction, orders memory operations provided in program
    /// order after this atomic instruction against this atomic instruction. On
    /// a barrier, orders memory operations provided in program order after this
    /// barrier against atomic instructions before this barrier.
    Acquire = 0x2,

    /// On an atomic instruction, orders memory operations provided in program
    /// order before this atomic instruction against this atomic instruction. On
    /// a barrier, orders memory operations provided in program order before
    /// this barrier against atomic instructions after this barrier.
    Release = 0x4,

    /// Has the properties of both [`Self::Acquire`] and [`Self::Release`] semantics. It
    /// is used for read-modify-write operations.
    AcquireRelease = 0x8,

    /// All observers see this memory access in the same order with respect to
    /// other sequentially-consistent memory accesses from this invocation.
    /// If the declared memory model is `vulkan`, `SequentiallyConsistent` must
    /// not be used.
    SequentiallyConsistent = 0x10,

    /// Apply the memory-ordering constraints to
    /// [`crate::storage_class::StorageBuffer`],
    /// [`crate::storage_class::PhysicalStorageBuffer`], or
    /// [`crate::storage_class::Uniform`] Storage Class memory.
    UniformMemory = 0x40,

    /// Apply the memory-ordering constraints to subgroup memory.
    SubgroupMemory = 0x80,

    /// Apply the memory-ordering constraints to
    /// [`crate::storage_class::Workgroup`] Storage Class memory.
    WorkgroupMemory = 0x100,

    /// Apply the memory-ordering constraints to
    /// [`crate::storage_class::CrossWorkgroup`] Storage Class memory.
    CrossWorkgroupMemory = 0x200,

    /// Apply the memory-ordering constraints to
    /// [`crate::storage_class::AtomicCounter`] Storage Class memory.
    AtomicCounterMemory = 0x400,

    /// Apply the memory-ordering constraints to image contents (types declared
    /// by `OpTypeImage`), or to accesses done through pointers to the
    /// [`crate::storage_class::Image`] Storage Class.
    ImageMemory = 0x800,

    /// Apply the memory-ordering constraints to the
    /// [`crate::storage_class::Output`] Storage Class memory.
    OutputMemory = 0x1000,

    /// Perform an availability operation on all references in the selected
    /// storage classes.
    MakeAvailable = 0x2000,

    /// Perform a visibility operation on all references in the selected
    /// storage classes.
    MakeVisible = 0x4000,

    /// This access cannot be eliminated, duplicated, or combined with
    /// other accesses.
    Volatile = 0x8000,
}
