#[cfg(target_arch = "spirv")]
use crate::arch::barrier;
use crate::float::Float;
use crate::integer::{Integer, SignedInteger, UnsignedInteger};
#[cfg(target_arch = "spirv")]
use crate::memory::{Scope, Semantics};
use crate::scalar::VectorOrScalar;
#[cfg(target_arch = "spirv")]
use core::arch::asm;

#[cfg(target_arch = "spirv")]
const SUBGROUP: u32 = Scope::Subgroup as u32;

/// `SubgroupMask` is a [`glam::UVec4`] representing a bitmask of all invocations within a subgroup.
/// Mostly used in group ballot operations.
#[derive(Copy, Clone, Default, Eq, PartialEq)]
pub struct SubgroupMask(pub glam::UVec4);

/// Defines the class of group operation.
#[non_exhaustive]
#[derive(Debug, PartialEq, Eq)]
pub enum GroupOperation {
    /// A reduction operation for all values of a specific value X specified by invocations within a workgroup.
    Reduce = 0,
    /// A binary operation with an identity I and n (where n is the size of the workgroup)
    /// elements[a0, a1, … an-1] resulting in [a0, (a0 op a1), …(a0 op a1 op … op an-1)]
    InclusiveScan = 1,
    /// A binary operation with an identity I and n (where n is the size of the workgroup)
    /// elements[a0, a1, … an-1] resulting in [I, a0, (a0 op a1), … (a0 op a1 op … op an-2)].
    ExclusiveScan = 2,
    // /// See [`GROUP_OPERATION_CLUSTERED_REDUCE`]
    // ClusteredReduce = 3,
    /// Reserved.
    ///
    /// Requires Capability `GroupNonUniformPartitionedNV`.
    PartitionedReduceNV = 6,
    /// Reserved.
    ///
    /// Requires Capability `GroupNonUniformPartitionedNV`.
    PartitionedInclusiveScanNV = 7,
    /// Reserved.
    ///
    /// Requires Capability `GroupNonUniformPartitionedNV`.
    PartitionedExclusiveScanNV = 8,
}

/// The [`GroupOperation`] `ClusteredReduce`.
///
/// All instructions with a [`GroupOperation`] require an additional `ClusterSize` parameter when [`GroupOperation`] is
/// `ClusteredReduce`. To map this requirement into rust, all function have a base version accepting [`GroupOperation`]
/// as a const generic, and a `_clustered` variant that is fixed to `ClusteredReduce` and takes the additional
/// `ClusterSize` parameter as a const generic. To not accidentally use a `ClusteredReduce` in the base variant of the
/// function, it was removed from the [`GroupOperation`] enum and instead resides individually.
pub const GROUP_OPERATION_CLUSTERED_REDUCE: u32 = 3;

/// Only usable if the extension GL_KHR_shader_subgroup_basic is enabled.
///
/// The function subgroupBarrier() enforces that all active invocations within a
/// subgroup must execute this function before any are allowed to continue their
/// execution, and the results of any memory stores performed using coherent
/// variables performed prior to the call will be visible to any future
/// coherent access to the same memory performed by any other shader invocation
/// within the same subgroup.
///
/// Requires Capability `GroupNonUniform`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "subgroupBarrier")]
#[inline]
pub unsafe fn subgroup_barrier() {
    unsafe {
        barrier::control_barrier::<
            SUBGROUP,
            SUBGROUP,
            {
                Semantics::ACQUIRE_RELEASE.bits()
                    | Semantics::UNIFORM_MEMORY.bits()
                    | Semantics::WORKGROUP_MEMORY.bits()
                    | Semantics::IMAGE_MEMORY.bits()
            },
        >();
    }
}

/// Only usable if the extension GL_KHR_shader_subgroup_basic is enabled.
///
/// The function subgroupMemoryBarrier() enforces the ordering of all memory
/// transactions issued within a single shader invocation, as viewed by other
/// invocations in the same subgroup.
///
/// Requires Capability `GroupNonUniform`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "subgroupMemoryBarrier")]
#[inline]
pub unsafe fn subgroup_memory_barrier() {
    unsafe {
        barrier::memory_barrier::<
            SUBGROUP,
            {
                Semantics::ACQUIRE_RELEASE.bits()
                    | Semantics::UNIFORM_MEMORY.bits()
                    | Semantics::WORKGROUP_MEMORY.bits()
                    | Semantics::IMAGE_MEMORY.bits()
            },
        >();
    }
}

/// Only usable if the extension GL_KHR_shader_subgroup_basic is enabled.
///
/// The function subgroupMemoryBarrierBuffer() enforces the ordering of all
/// memory transactions to buffer variables issued within a single shader
/// invocation, as viewed by other invocations in the same subgroup.
///
/// Requires Capability `GroupNonUniform`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "subgroupMemoryBarrierBuffer")]
#[inline]
pub unsafe fn subgroup_memory_barrier_buffer() {
    unsafe {
        barrier::memory_barrier::<
            SUBGROUP,
            { Semantics::ACQUIRE_RELEASE.bits() | Semantics::UNIFORM_MEMORY.bits() },
        >();
    }
}

/// Only usable if the extension GL_KHR_shader_subgroup_basic is enabled.
///
/// The function subgroupMemoryBarrierShared() enforces the ordering of all
/// memory transactions to shared variables issued within a single shader
/// invocation, as viewed by other invocations in the same subgroup.
///
/// Only available in compute shaders.
///
/// Requires Capability `GroupNonUniform`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "subgroupMemoryBarrierShared")]
#[inline]
pub unsafe fn subgroup_memory_barrier_shared() {
    unsafe {
        barrier::memory_barrier::<
            SUBGROUP,
            { Semantics::ACQUIRE_RELEASE.bits() | Semantics::WORKGROUP_MEMORY.bits() },
        >();
    }
}

/// Only usable if the extension GL_KHR_shader_subgroup_basic is enabled.
///
/// The function subgroupMemoryBarrierImage() enforces the ordering of all
/// memory transactions to images issued within a single shader invocation, as
/// viewed by other invocations in the same subgroup.
///
/// Requires Capability `GroupNonUniform`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "subgroupMemoryBarrierImage")]
#[inline]
pub unsafe fn subgroup_memory_barrier_image() {
    unsafe {
        barrier::memory_barrier::<
            SUBGROUP,
            { Semantics::ACQUIRE_RELEASE.bits() | Semantics::IMAGE_MEMORY.bits() },
        >();
    }
}

/// Result is true only in the active invocation with the lowest id in the group, otherwise result is false.
///
/// Result Type must be a Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// Requires Capability `GroupNonUniform`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformElect")]
#[inline]
pub unsafe fn subgroup_non_uniform_elect() -> bool {
    let mut result = false;

    unsafe {
        asm! {
            "%bool = OpTypeBool",
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%result = OpGroupNonUniformElect %bool %subgroup",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Evaluates a predicate for all active invocations in the group, resulting in true if predicate evaluates to true for all active invocations in the group, otherwise the result is false.
///
/// Result Type must be a Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// Predicate must be a Boolean type.
///
/// Requires Capability `GroupNonUniformVote`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformAll")]
#[inline]
pub unsafe fn subgroup_non_uniform_all(predicate: bool) -> bool {
    let mut result = false;

    unsafe {
        asm! {
            "%bool = OpTypeBool",
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%predicate = OpLoad _ {predicate}",
            "%result = OpGroupNonUniformAll %bool %subgroup %predicate",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            predicate = in(reg) &predicate,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Evaluates a predicate for all active invocations in the group, resulting in true if predicate evaluates to true for any active invocation in the group, otherwise the result is false.
///
/// Result Type must be a Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// Predicate must be a Boolean type.
///
/// Requires Capability `GroupNonUniformVote`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformAny")]
#[inline]
pub unsafe fn subgroup_non_uniform_any(predicate: bool) -> bool {
    let mut result = false;

    unsafe {
        asm! {
            "%bool = OpTypeBool",
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%predicate = OpLoad _ {predicate}",
            "%result = OpGroupNonUniformAny %bool %subgroup %predicate",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            predicate = in(reg) &predicate,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Evaluates a value for all active invocations in the group. The result is true if Value is equal for all active invocations in the group. Otherwise, the result is false.
///
/// Result Type must be a Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// Value must be a scalar or vector of floating-point type, integer type, or Boolean type. The compare operation is based on this type, and if it is a floating-point type, an ordered-and-equal compare is used.
///
/// Requires Capability `GroupNonUniformVote`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformAllEqual")]
#[inline]
pub unsafe fn subgroup_non_uniform_all_equal<T: VectorOrScalar>(value: T) -> bool {
    let mut result = false;

    unsafe {
        asm! {
            "%bool = OpTypeBool",
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformAllEqual %bool %subgroup %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Result is the Value of the invocation identified by the id Id to all active invocations in the group.
///
/// Result Type must be a scalar or vector of floating-point type, integer type, or Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The type of Value must be the same as Result Type.
///
/// Id must be a scalar of integer type, whose Signedness operand is 0.
///
/// Before version 1.5, Id must come from a constant instruction. Starting with version 1.5, this restriction is lifted. However, behavior is undefined when Id is not dynamically uniform.
///
/// The resulting value is undefined if Id is an inactive invocation, or is greater than or equal to the size of the group.
///
/// Requires Capability `GroupNonUniformBallot`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBroadcast")]
#[inline]
pub unsafe fn subgroup_non_uniform_broadcast<T: VectorOrScalar>(value: T, id: u32) -> T {
    let mut result = T::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%id = OpLoad _ {id}",
            "%result = OpGroupNonUniformBroadcast _ %subgroup %value %id",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            id = in(reg) &id,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Result is the Value of the invocation from the active invocation with the lowest id in the group to all active invocations in the group.
///
/// Result Type must be a scalar or vector of floating-point type, integer type, or Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The type of Value must be the same as Result Type.
///
/// Requires Capability `GroupNonUniformBallot`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBroadcastFirst")]
#[inline]
pub unsafe fn subgroup_non_uniform_broadcast_first<T: VectorOrScalar>(value: T) -> T {
    let mut result = T::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformBroadcastFirst _ %subgroup %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Result is a bitfield value combining the Predicate value from all invocations in the group that execute the same dynamic instance of this instruction. The bit is set to one if the corresponding invocation is active and the Predicate for that invocation evaluated to true; otherwise, it is set to zero.
///
/// Result Type must be a vector of four components of integer type scalar, whose Width operand is 32 and whose Signedness operand is 0.
///
/// Result is a set of bitfields where the first invocation is represented in the lowest bit of the first vector component and the last (up to the size of the group) is the higher bit number of the last bitmask needed to represent all bits of the group invocations.
///
/// Execution is a Scope that identifies the group of invocations affected by this command.
///
/// Predicate must be a Boolean type.
///
/// Requires Capability `GroupNonUniformBallot`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBallot")]
#[inline]
pub unsafe fn subgroup_non_uniform_ballot(predicate: bool) -> SubgroupMask {
    let mut result = SubgroupMask::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%groupmask = OpTypeVector %u32 4",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%predicate = OpLoad _ {predicate}",
            "%result = OpGroupNonUniformBallot %groupmask %subgroup %predicate",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            predicate = in(reg) &predicate,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Evaluates a value for all active invocations in the group, resulting in true if the bit in Value for the corresponding invocation is set to one, otherwise the result is false.
///
/// Result Type must be a Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// Value must be a vector of four components of integer type scalar, whose Width operand is 32 and whose Signedness operand is 0.
///
/// Behavior is undefined unless Value is the same for all invocations that execute the same dynamic instance of this instruction.
///
/// Value is a set of bitfields where the first invocation is represented in the lowest bit of the first vector component and the last (up to the size of the group) is the higher bit number of the last bitmask needed to represent all bits of the group invocations.
///
/// Requires Capability `GroupNonUniformBallot`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformInverseBallot")]
#[inline]
pub unsafe fn subgroup_non_uniform_inverse_ballot(subgroup_mask: SubgroupMask) -> bool {
    let mut result = false;

    unsafe {
        asm! {
            "%bool = OpTypeBool",
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%subgroup_mask = OpLoad _ {subgroup_mask}",
            "%result = OpGroupNonUniformInverseBallot %bool %subgroup %subgroup_mask",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            subgroup_mask = in(reg) &subgroup_mask,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Evaluates a value for all active invocations in the group, resulting in true if the bit in Value that corresponds to Index is set to one, otherwise the result is false.
///
/// Result Type must be a Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// Value must be a vector of four components of integer type scalar, whose Width operand is 32 and whose Signedness operand is 0.
///
/// Value is a set of bitfields where the first invocation is represented in the lowest bit of the first vector component and the last (up to the size of the group) is the higher bit number of the last bitmask needed to represent all bits of the group invocations.
///
/// Index must be a scalar of integer type, whose Signedness operand is 0.
///
/// The resulting value is undefined if Index is greater than or equal to the size of the group.
///
/// Requires Capability `GroupNonUniformBallot`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBallotBitExtract")]
#[inline]
pub unsafe fn subgroup_non_uniform_ballot_bit_extract(
    subgroup_mask: SubgroupMask,
    id: u32,
) -> bool {
    let mut result = false;

    unsafe {
        asm! {
            "%bool = OpTypeBool",
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%subgroup_mask = OpLoad _ {subgroup_mask}",
            "%id = OpLoad _ {id}",
            "%result = OpGroupNonUniformBallotBitExtract %bool %subgroup %subgroup_mask %id",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            subgroup_mask = in(reg) &subgroup_mask,
            id = in(reg) &id,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Result is the number of bits that are set to 1 in Value, considering only the bits in Value required to represent all bits of the group's invocations.
///
/// Result Type must be a scalar of integer type, whose Signedness operand is 0.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 0.
///
/// Value must be a vector of four components of integer type scalar, whose Width operand is 32 and whose Signedness operand is 0.
///
/// Value is a set of bitfields where the first invocation is represented in the lowest bit of the first vector component and the last (up to the size of the group) is the higher bit number of the last bitmask needed to represent all bits of the group invocations.
///
/// Requires Capability `GroupNonUniformBallot`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBallotBitCount")]
#[inline]
pub unsafe fn subgroup_non_uniform_ballot_bit_count<const GROUP_OP: u32>(
    subgroup_mask: SubgroupMask,
) -> u32 {
    let mut result = 0;

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%subgroup_mask = OpLoad _ {subgroup_mask}",
            "%result = OpGroupNonUniformBallotBitCount %u32 %subgroup {groupop} %subgroup_mask",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OP,
            subgroup_mask = in(reg) &subgroup_mask,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Find the least significant bit set to 1 in Value, considering only the bits in Value required to represent all bits of the group's invocations. If none of the considered bits is set to 1, the resulting value is undefined.
///
/// Result Type must be a scalar of integer type, whose Signedness operand is 0.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// Value must be a vector of four components of integer type scalar, whose Width operand is 32 and whose Signedness operand is 0.
///
/// Value is a set of bitfields where the first invocation is represented in the lowest bit of the first vector component and the last (up to the size of the group) is the higher bit number of the last bitmask needed to represent all bits of the group invocations.
///
/// Requires Capability `GroupNonUniformBallot`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBallotFindLSB")]
#[inline]
pub unsafe fn subgroup_non_uniform_ballot_find_lsb(subgroup_mask: SubgroupMask) -> u32 {
    let mut result = 0;

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%subgroup_mask = OpLoad _ {subgroup_mask}",
            "%result = OpGroupNonUniformBallotFindLSB %u32 %subgroup %subgroup_mask",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            subgroup_mask = in(reg) &subgroup_mask,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Find the most significant bit set to 1 in Value, considering only the bits in Value required to represent all bits of the group's invocations. If none of the considered bits is set to 1, the resulting value is undefined.
///
/// Result Type must be a scalar of integer type, whose Signedness operand is 0.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// Value must be a vector of four components of integer type scalar, whose Width operand is 32 and whose Signedness operand is 0.
///
/// Value is a set of bitfields where the first invocation is represented in the lowest bit of the first vector component and the last (up to the size of the group) is the higher bit number of the last bitmask needed to represent all bits of the group invocations.
///
/// Requires Capability `GroupNonUniformBallot`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBallotFindMSB")]
#[inline]
pub unsafe fn subgroup_non_uniform_ballot_find_msb(subgroup_mask: SubgroupMask) -> u32 {
    let mut result = 0;

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%subgroup_mask = OpLoad _ {subgroup_mask}",
            "%result = OpGroupNonUniformBallotFindMSB %u32 %subgroup %subgroup_mask",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            subgroup_mask = in(reg) &subgroup_mask,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Result is the Value of the invocation identified by the id Id.
///
/// Result Type must be a scalar or vector of floating-point type, integer type, or Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command.
///
/// The type of Value must be the same as Result Type.
///
/// Id must be a scalar of integer type, whose Signedness operand is 0.
///
/// The resulting value is undefined if Id is an inactive invocation, or is greater than or equal to the size of the group.
///
/// Requires Capability `GroupNonUniformShuffle`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformShuffle")]
#[inline]
pub unsafe fn subgroup_non_uniform_shuffle<T: VectorOrScalar>(value: T, id: u32) -> T {
    let mut result = T::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%id = OpLoad _ {id}",
            "%result = OpGroupNonUniformShuffle _ %subgroup %value %id",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            id = in(reg) &id,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Result is the Value of the invocation identified by the current invocation’s id within the group xor’ed with Mask.
///
/// Result Type must be a scalar or vector of floating-point type, integer type, or Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The type of Value must be the same as Result Type.
///
/// Mask must be a scalar of integer type, whose Signedness operand is 0.
///
/// The resulting value is undefined if current invocation’s id within the group xor’ed with Mask is an inactive invocation, or is greater than or equal to the size of the group.
///
/// Requires Capability `GroupNonUniformShuffle`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformShuffleXor")]
#[inline]
pub unsafe fn subgroup_non_uniform_shuffle_xor<T: VectorOrScalar>(value: T, mask: u32) -> T {
    let mut result = T::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%mask = OpLoad _ {mask}",
            "%result = OpGroupNonUniformShuffleXor _ %subgroup %value %mask",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            mask = in(reg) &mask,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Result is the Value of the invocation identified by the current invocation’s id within the group - Delta.
///
/// Result Type must be a scalar or vector of floating-point type, integer type, or Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The type of Value must be the same as Result Type.
///
/// Delta must be a scalar of integer type, whose Signedness operand is 0.
///
/// Delta is treated as unsigned and the resulting value is undefined if Delta is greater than the current invocation’s id within the group or if the selected lane is inactive.
///
/// Requires Capability `GroupNonUniformShuffleRelative`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformShuffleUp")]
#[inline]
pub unsafe fn subgroup_non_uniform_shuffle_up<T: VectorOrScalar>(value: T, delta: u32) -> T {
    let mut result = T::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%delta = OpLoad _ {delta}",
            "%result = OpGroupNonUniformShuffleUp _ %subgroup %value %delta",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            delta = in(reg) &delta,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Result is the Value of the invocation identified by the current invocation’s id within the group + Delta.
///
/// Result Type must be a scalar or vector of floating-point type, integer type, or Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The type of Value must be the same as Result Type.
///
/// Delta must be a scalar of integer type, whose Signedness operand is 0.
///
/// Delta is treated as unsigned and the resulting value is undefined if Delta is greater than or equal to the size of the group, or if the current invocation’s id within the group + Delta is either an inactive invocation or greater than or equal to the size of the group.
///
/// Requires Capability `GroupNonUniformShuffleRelative`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformShuffleDown")]
#[inline]
pub unsafe fn subgroup_non_uniform_shuffle_down<T: VectorOrScalar>(value: T, delta: u32) -> T {
    let mut result = T::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%delta = OpLoad _ {delta}",
            "%result = OpGroupNonUniformShuffleDown _ %subgroup %value %delta",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            delta = in(reg) &delta,
            result = in(reg) &mut result,
        }
    }

    result
}

/// An integer add group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 0.
///
/// The type of Value must be the same as Result Type.
///
/// Requires Capability `GroupNonUniformArithmetic`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformIAdd")]
#[inline]
pub unsafe fn subgroup_non_uniform_i_add<
    const GROUP_OP: u32,
    I: VectorOrScalar<Scalar = impl Integer>,
>(
    value: I,
) -> I {
    let mut result = I::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformIAdd _ %subgroup {groupop} %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// An integer add group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 0. If Operation is ClusteredReduce, ClusterSize must be present.
///
/// The type of Value must be the same as Result Type.
///
/// ClusterSize is the size of cluster to use. ClusterSize must be a scalar of integer type, whose Signedness operand is 0. ClusterSize must come from a constant instruction. Behavior is undefined unless ClusterSize is at least 1 and a power of 2. If ClusterSize is greater than the size of the group, executing this instruction results in undefined behavior.
///
/// Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformIAdd")]
#[inline]
pub unsafe fn subgroup_non_uniform_i_add_clustered<
    const CLUSTER_SIZE: u32,
    I: VectorOrScalar<Scalar = impl Integer>,
>(
    value: I,
) -> I {
    let mut result = I::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%clustersize = OpConstant %u32 {clustersize}",
            "%result = OpGroupNonUniformIAdd _ %subgroup {groupop} %value %clustersize",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OPERATION_CLUSTERED_REDUCE,
            clustersize = const CLUSTER_SIZE,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A floating point add group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of floating-point type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 0.
///
/// The type of Value must be the same as Result Type. The method used to perform the group operation on the contributed Value(s) from active invocations is implementation defined.
///
/// Requires Capability `GroupNonUniformArithmetic`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformFAdd")]
#[inline]
pub unsafe fn subgroup_non_uniform_f_add<
    const GROUP_OP: u32,
    F: VectorOrScalar<Scalar = impl Float>,
>(
    value: F,
) -> F {
    let mut result = F::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformFAdd _ %subgroup {groupop} %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A floating point add group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of floating-point type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 0. If Operation is ClusteredReduce, ClusterSize must be present.
///
/// The type of Value must be the same as Result Type. The method used to perform the group operation on the contributed Value(s) from active invocations is implementation defined.
///
/// ClusterSize is the size of cluster to use. ClusterSize must be a scalar of integer type, whose Signedness operand is 0. ClusterSize must come from a constant instruction. Behavior is undefined unless ClusterSize is at least 1 and a power of 2. If ClusterSize is greater than the size of the group, executing this instruction results in undefined behavior.
///
/// Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformFAdd")]
#[inline]
pub unsafe fn subgroup_non_uniform_f_add_clustered<
    const CLUSTER_SIZE: u32,
    F: VectorOrScalar<Scalar = impl Float>,
>(
    value: F,
) -> F {
    let mut result = F::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%clustersize = OpConstant %u32 {clustersize}",
            "%result = OpGroupNonUniformFAdd _ %subgroup {groupop} %value %clustersize",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OPERATION_CLUSTERED_REDUCE,
            clustersize = const CLUSTER_SIZE,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// An integer multiply group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 1.
///
/// The type of Value must be the same as Result Type.
///
/// Requires Capability `GroupNonUniformArithmetic`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformIMul")]
#[inline]
pub unsafe fn subgroup_non_uniform_i_mul<
    const GROUP_OP: u32,
    I: VectorOrScalar<Scalar = impl Integer>,
>(
    value: I,
) -> I {
    let mut result = I::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformIMul _ %subgroup {groupop} %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// An integer multiply group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 1. If Operation is ClusteredReduce, ClusterSize must be present.
///
/// The type of Value must be the same as Result Type.
///
/// ClusterSize is the size of cluster to use. ClusterSize must be a scalar of integer type, whose Signedness operand is 0. ClusterSize must come from a constant instruction. Behavior is undefined unless ClusterSize is at least 1 and a power of 2. If ClusterSize is greater than the size of the group, executing this instruction results in undefined behavior.
///
/// Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformIMul")]
#[inline]
pub unsafe fn subgroup_non_uniform_i_mul_clustered<
    const CLUSTER_SIZE: u32,
    I: VectorOrScalar<Scalar = impl Integer>,
>(
    value: I,
) -> I {
    let mut result = I::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%clustersize = OpConstant %u32 {clustersize}",
            "%result = OpGroupNonUniformIMul _ %subgroup {groupop} %value %clustersize",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OPERATION_CLUSTERED_REDUCE,
            clustersize = const CLUSTER_SIZE,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A floating point multiply group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of floating-point type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 1.
///
/// The type of Value must be the same as Result Type. The method used to perform the group operation on the contributed Value(s) from active invocations is implementation defined.
///
/// Requires Capability `GroupNonUniformArithmetic`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformFMul")]
#[inline]
pub unsafe fn subgroup_non_uniform_f_mul<
    const GROUP_OP: u32,
    F: VectorOrScalar<Scalar = impl Float>,
>(
    value: F,
) -> F {
    let mut result = F::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformFMul _ %subgroup {groupop} %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A floating point multiply group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of floating-point type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 1. If Operation is ClusteredReduce, ClusterSize must be present.
///
/// The type of Value must be the same as Result Type. The method used to perform the group operation on the contributed Value(s) from active invocations is implementation defined.
///
/// ClusterSize is the size of cluster to use. ClusterSize must be a scalar of integer type, whose Signedness operand is 0. ClusterSize must come from a constant instruction. Behavior is undefined unless ClusterSize is at least 1 and a power of 2. If ClusterSize is greater than the size of the group, executing this instruction results in undefined behavior.
///
/// Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformFMul")]
#[inline]
pub unsafe fn subgroup_non_uniform_f_mul_clustered<
    const CLUSTER_SIZE: u32,
    F: VectorOrScalar<Scalar = impl Float>,
>(
    value: F,
) -> F {
    let mut result = F::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%clustersize = OpConstant %u32 {clustersize}",
            "%result = OpGroupNonUniformFMul _ %subgroup {groupop} %value %clustersize",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OPERATION_CLUSTERED_REDUCE,
            clustersize = const CLUSTER_SIZE,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A signed integer minimum group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is INT_MAX.
///
/// The type of Value must be the same as Result Type.
///
/// Requires Capability `GroupNonUniformArithmetic`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformSMin")]
#[inline]
pub unsafe fn subgroup_non_uniform_s_min<
    const GROUP_OP: u32,
    S: VectorOrScalar<Scalar = impl SignedInteger>,
>(
    value: S,
) -> S {
    let mut result = S::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformSMin _ %subgroup {groupop} %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A signed integer minimum group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is INT_MAX. If Operation is ClusteredReduce, ClusterSize must be present.
///
/// The type of Value must be the same as Result Type.
///
/// ClusterSize is the size of cluster to use. ClusterSize must be a scalar of integer type, whose Signedness operand is 0. ClusterSize must come from a constant instruction. Behavior is undefined unless ClusterSize is at least 1 and a power of 2. If ClusterSize is greater than the size of the group, executing this instruction results in undefined behavior.
///
/// Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformSMin")]
#[inline]
pub unsafe fn subgroup_non_uniform_s_min_clustered<
    const CLUSTER_SIZE: u32,
    S: VectorOrScalar<Scalar = impl SignedInteger>,
>(
    value: S,
) -> S {
    let mut result = S::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%clustersize = OpConstant %u32 {clustersize}",
            "%result = OpGroupNonUniformSMin _ %subgroup {groupop} %value %clustersize",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OPERATION_CLUSTERED_REDUCE,
            clustersize = const CLUSTER_SIZE,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// An unsigned integer minimum group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type, whose Signedness operand is 0.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is UINT_MAX.
///
/// The type of Value must be the same as Result Type.
///
/// Requires Capability `GroupNonUniformArithmetic`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformUMin")]
#[inline]
pub unsafe fn subgroup_non_uniform_u_min<
    const GROUP_OP: u32,
    U: VectorOrScalar<Scalar = impl UnsignedInteger>,
>(
    value: U,
) -> U {
    let mut result = U::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformUMin _ %subgroup {groupop} %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// An unsigned integer minimum group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type, whose Signedness operand is 0.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is UINT_MAX. If Operation is ClusteredReduce, ClusterSize must be present.
///
/// The type of Value must be the same as Result Type.
///
/// ClusterSize is the size of cluster to use. ClusterSize must be a scalar of integer type, whose Signedness operand is 0. ClusterSize must come from a constant instruction. Behavior is undefined unless ClusterSize is at least 1 and a power of 2. If ClusterSize is greater than the size of the group, executing this instruction results in undefined behavior.
///
/// Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformUMin")]
#[inline]
pub unsafe fn subgroup_non_uniform_u_min_clustered<
    const CLUSTER_SIZE: u32,
    U: VectorOrScalar<Scalar = impl UnsignedInteger>,
>(
    value: U,
) -> U {
    let mut result = U::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%clustersize = OpConstant %u32 {clustersize}",
            "%result = OpGroupNonUniformUMin _ %subgroup {groupop} %value %clustersize",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OPERATION_CLUSTERED_REDUCE,
            clustersize = const CLUSTER_SIZE,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A floating point minimum group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of floating-point type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is +INF.
///
/// The type of Value must be the same as Result Type. The method used to perform the group operation on the contributed Value(s) from active invocations is implementation defined. From the set of Value(s) provided by active invocations within a subgroup, if for any two Values one of them is a NaN, the other is chosen. If all Value(s) that are used by the current invocation are NaN, then the result is an undefined value.
///
/// Requires Capability `GroupNonUniformArithmetic`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformFMin")]
#[inline]
pub unsafe fn subgroup_non_uniform_f_min<
    const GROUP_OP: u32,
    F: VectorOrScalar<Scalar = impl Float>,
>(
    value: F,
) -> F {
    let mut result = F::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformFMin _ %subgroup {groupop} %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A floating point minimum group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of floating-point type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is +INF. If Operation is ClusteredReduce, ClusterSize must be present.
///
/// The type of Value must be the same as Result Type. The method used to perform the group operation on the contributed Value(s) from active invocations is implementation defined. From the set of Value(s) provided by active invocations within a subgroup, if for any two Values one of them is a NaN, the other is chosen. If all Value(s) that are used by the current invocation are NaN, then the result is an undefined value.
///
/// ClusterSize is the size of cluster to use. ClusterSize must be a scalar of integer type, whose Signedness operand is 0. ClusterSize must come from a constant instruction. Behavior is undefined unless ClusterSize is at least 1 and a power of 2. If ClusterSize is greater than the size of the group, executing this instruction results in undefined behavior.
///
/// Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformFMin")]
#[inline]
pub unsafe fn subgroup_non_uniform_f_min_clustered<
    const CLUSTER_SIZE: u32,
    F: VectorOrScalar<Scalar = impl Float>,
>(
    value: F,
) -> F {
    let mut result = F::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%clustersize = OpConstant %u32 {clustersize}",
            "%result = OpGroupNonUniformFMin _ %subgroup {groupop} %value %clustersize",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OPERATION_CLUSTERED_REDUCE,
            clustersize = const CLUSTER_SIZE,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A signed integer maximum group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is INT_MIN.
///
/// The type of Value must be the same as Result Type.
///
/// Requires Capability `GroupNonUniformArithmetic`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformSMax")]
#[inline]
pub unsafe fn subgroup_non_uniform_s_max<
    const GROUP_OP: u32,
    S: VectorOrScalar<Scalar = impl SignedInteger>,
>(
    value: S,
) -> S {
    let mut result = S::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformSMax _ %subgroup {groupop} %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A signed integer maximum group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is INT_MIN. If Operation is ClusteredReduce, ClusterSize must be present.
///
/// The type of Value must be the same as Result Type.
///
/// ClusterSize is the size of cluster to use. ClusterSize must be a scalar of integer type, whose Signedness operand is 0. ClusterSize must come from a constant instruction. Behavior is undefined unless ClusterSize is at least 1 and a power of 2. If ClusterSize is greater than the size of the group, executing this instruction results in undefined behavior.
///
/// Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformSMax")]
#[inline]
pub unsafe fn subgroup_non_uniform_s_max_clustered<
    const CLUSTER_SIZE: u32,
    S: VectorOrScalar<Scalar = impl SignedInteger>,
>(
    value: S,
) -> S {
    let mut result = S::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%clustersize = OpConstant %u32 {clustersize}",
            "%result = OpGroupNonUniformSMax _ %subgroup {groupop} %value %clustersize",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OPERATION_CLUSTERED_REDUCE,
            clustersize = const CLUSTER_SIZE,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// An unsigned integer maximum group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type, whose Signedness operand is 0.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 0.
///
/// The type of Value must be the same as Result Type.
///
/// Requires Capability `GroupNonUniformArithmetic`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformUMax")]
#[inline]
pub unsafe fn subgroup_non_uniform_u_max<
    const GROUP_OP: u32,
    U: VectorOrScalar<Scalar = impl UnsignedInteger>,
>(
    value: U,
) -> U {
    let mut result = U::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformUMax _ %subgroup {groupop} %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// An unsigned integer maximum group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type, whose Signedness operand is 0.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 0. If Operation is ClusteredReduce, ClusterSize must be present.
///
/// The type of Value must be the same as Result Type.
///
/// ClusterSize is the size of cluster to use. ClusterSize must be a scalar of integer type, whose Signedness operand is 0. ClusterSize must come from a constant instruction. Behavior is undefined unless ClusterSize is at least 1 and a power of 2. If ClusterSize is greater than the size of the group, executing this instruction results in undefined behavior.
///
/// Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformUMax")]
#[inline]
pub unsafe fn subgroup_non_uniform_u_max_clustered<
    const CLUSTER_SIZE: u32,
    U: VectorOrScalar<Scalar = impl UnsignedInteger>,
>(
    value: U,
) -> U {
    let mut result = U::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%clustersize = OpConstant %u32 {clustersize}",
            "%result = OpGroupNonUniformUMax _ %subgroup {groupop} %value %clustersize",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OPERATION_CLUSTERED_REDUCE,
            clustersize = const CLUSTER_SIZE,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A floating point maximum group operation of all Value operands contributed by active invocations in by group.
///
/// Result Type must be a scalar or vector of floating-point type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is -INF.
///
/// The type of Value must be the same as Result Type. The method used to perform the group operation on the contributed Value(s) from active invocations is implementation defined. From the set of Value(s) provided by active invocations within a subgroup, if for any two Values one of them is a NaN, the other is chosen. If all Value(s) that are used by the current invocation are NaN, then the result is an undefined value.
///
/// Requires Capability `GroupNonUniformArithmetic`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformFMax")]
#[inline]
pub unsafe fn subgroup_non_uniform_f_max<
    const GROUP_OP: u32,
    F: VectorOrScalar<Scalar = impl Float>,
>(
    value: F,
) -> F {
    let mut result = F::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformFMax _ %subgroup {groupop} %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A floating point maximum group operation of all Value operands contributed by active invocations in by group.
///
/// Result Type must be a scalar or vector of floating-point type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is -INF.
///
/// The type of Value must be the same as Result Type. The method used to perform the group operation on the contributed Value(s) from active invocations is implementation defined. From the set of Value(s) provided by active invocations within a subgroup, if for any two Values one of them is a NaN, the other is chosen. If all Value(s) that are used by the current invocation are NaN, then the result is an undefined value.
///
/// Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformFMax")]
#[inline]
pub unsafe fn subgroup_non_uniform_f_max_clustered<
    const CLUSTER_SIZE: u32,
    F: VectorOrScalar<Scalar = impl Float>,
>(
    value: F,
) -> F {
    let mut result = F::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%clustersize = OpConstant %u32 {clustersize}",
            "%result = OpGroupNonUniformFMax _ %subgroup {groupop} %value %clustersize",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OPERATION_CLUSTERED_REDUCE,
            clustersize = const CLUSTER_SIZE,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A bitwise and group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is ~0.
///
/// The type of Value must be the same as Result Type.
///
/// Requires Capability `GroupNonUniformArithmetic`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBitwiseAnd")]
#[inline]
pub unsafe fn subgroup_non_uniform_bitwise_and<
    const GROUP_OP: u32,
    I: VectorOrScalar<Scalar = impl Integer>,
>(
    value: I,
) -> I {
    let mut result = I::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformBitwiseAnd _ %subgroup {groupop} %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A bitwise and group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is ~0. If Operation is ClusteredReduce, ClusterSize must be present.
///
/// The type of Value must be the same as Result Type.
///
/// ClusterSize is the size of cluster to use. ClusterSize must be a scalar of integer type, whose Signedness operand is 0. ClusterSize must come from a constant instruction. Behavior is undefined unless ClusterSize is at least 1 and a power of 2. If ClusterSize is greater than the size of the group, executing this instruction results in undefined behavior.
///
/// Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBitwiseAnd")]
#[inline]
pub unsafe fn subgroup_non_uniform_bitwise_and_clustered<
    const CLUSTER_SIZE: u32,
    I: VectorOrScalar<Scalar = impl Integer>,
>(
    value: I,
) -> I {
    let mut result = I::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%clustersize = OpConstant %u32 {clustersize}",
            "%result = OpGroupNonUniformBitwiseAnd _ %subgroup {groupop} %value %clustersize",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OPERATION_CLUSTERED_REDUCE,
            clustersize = const CLUSTER_SIZE,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A bitwise or group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 0.
///
/// The type of Value must be the same as Result Type.
///
/// Requires Capability `GroupNonUniformArithmetic`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBitwiseOr")]
#[inline]
pub unsafe fn subgroup_non_uniform_bitwise_or<
    const GROUP_OP: u32,
    I: VectorOrScalar<Scalar = impl Integer>,
>(
    value: I,
) -> I {
    let mut result = I::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformBitwiseOr _ %subgroup {groupop} %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A bitwise or group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 0. If Operation is ClusteredReduce, ClusterSize must be present.
///
/// The type of Value must be the same as Result Type.
///
/// ClusterSize is the size of cluster to use. ClusterSize must be a scalar of integer type, whose Signedness operand is 0. ClusterSize must come from a constant instruction. Behavior is undefined unless ClusterSize is at least 1 and a power of 2. If ClusterSize is greater than the size of the group, executing this instruction results in undefined behavior.
///
/// Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBitwiseOr")]
#[inline]
pub unsafe fn subgroup_non_uniform_bitwise_or_clustered<
    const CLUSTER_SIZE: u32,
    I: VectorOrScalar<Scalar = impl Integer>,
>(
    value: I,
) -> I {
    let mut result = I::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%clustersize = OpConstant %u32 {clustersize}",
            "%result = OpGroupNonUniformBitwiseOr _ %subgroup {groupop} %value %clustersize",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OPERATION_CLUSTERED_REDUCE,
            clustersize = const CLUSTER_SIZE,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A bitwise xor group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 0.
///
/// The type of Value must be the same as Result Type.
///
/// Requires Capability `GroupNonUniformArithmetic`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBitwiseXor")]
#[inline]
pub unsafe fn subgroup_non_uniform_bitwise_xor<
    const GROUP_OP: u32,
    I: VectorOrScalar<Scalar = impl Integer>,
>(
    value: I,
) -> I {
    let mut result = I::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformBitwiseXor _ %subgroup {groupop} %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A bitwise xor group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of integer type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 0. If Operation is ClusteredReduce, ClusterSize must be present.
///
/// The type of Value must be the same as Result Type.
///
/// ClusterSize is the size of cluster to use. ClusterSize must be a scalar of integer type, whose Signedness operand is 0. ClusterSize must come from a constant instruction. Behavior is undefined unless ClusterSize is at least 1 and a power of 2. If ClusterSize is greater than the size of the group, executing this instruction results in undefined behavior.
///
/// Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformBitwiseXor")]
#[inline]
pub unsafe fn subgroup_non_uniform_bitwise_xor_clustered<
    const CLUSTER_SIZE: u32,
    I: VectorOrScalar<Scalar = impl Integer>,
>(
    value: I,
) -> I {
    let mut result = I::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%clustersize = OpConstant %u32 {clustersize}",
            "%result = OpGroupNonUniformBitwiseXor _ %subgroup {groupop} %value %clustersize",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OPERATION_CLUSTERED_REDUCE,
            clustersize = const CLUSTER_SIZE,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A logical and group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is ~0.
///
/// The type of Value must be the same as Result Type.
///
/// Requires Capability `GroupNonUniformArithmetic`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformLogicalAnd")]
#[inline]
pub unsafe fn subgroup_non_uniform_logical_and<
    const GROUP_OP: u32,
    I: VectorOrScalar<Scalar = bool>,
>(
    value: I,
) -> I {
    let mut result = I::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformLogicalAnd _ %subgroup {groupop} %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A logical and group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is ~0. If Operation is ClusteredReduce, ClusterSize must be present.
///
/// The type of Value must be the same as Result Type.
///
/// ClusterSize is the size of cluster to use. ClusterSize must be a scalar of integer type, whose Signedness operand is 0. ClusterSize must come from a constant instruction. Behavior is undefined unless ClusterSize is at least 1 and a power of 2. If ClusterSize is greater than the size of the group, executing this instruction results in undefined behavior.
///
/// Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformLogicalAnd")]
#[inline]
pub unsafe fn subgroup_non_uniform_logical_and_clustered<
    const CLUSTER_SIZE: u32,
    I: VectorOrScalar<Scalar = bool>,
>(
    value: I,
) -> I {
    let mut result = I::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%clustersize = OpConstant %u32 {clustersize}",
            "%result = OpGroupNonUniformLogicalAnd _ %subgroup {groupop} %value %clustersize",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OPERATION_CLUSTERED_REDUCE,
            clustersize = const CLUSTER_SIZE,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A logical or group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 0.
///
/// The type of Value must be the same as Result Type.
///
/// Requires Capability `GroupNonUniformArithmetic`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformLogicalOr")]
#[inline]
pub unsafe fn subgroup_non_uniform_logical_or<
    const GROUP_OP: u32,
    I: VectorOrScalar<Scalar = bool>,
>(
    value: I,
) -> I {
    let mut result = I::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformLogicalOr _ %subgroup {groupop} %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A logical or group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 0. If Operation is ClusteredReduce, ClusterSize must be present.
///
/// The type of Value must be the same as Result Type.
///
/// ClusterSize is the size of cluster to use. ClusterSize must be a scalar of integer type, whose Signedness operand is 0. ClusterSize must come from a constant instruction. Behavior is undefined unless ClusterSize is at least 1 and a power of 2. If ClusterSize is greater than the size of the group, executing this instruction results in undefined behavior.
///
/// Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformLogicalOr")]
#[inline]
pub unsafe fn subgroup_non_uniform_logical_or_clustered<
    const CLUSTER_SIZE: u32,
    I: VectorOrScalar<Scalar = bool>,
>(
    value: I,
) -> I {
    let mut result = I::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%clustersize = OpConstant %u32 {clustersize}",
            "%result = OpGroupNonUniformLogicalOr _ %subgroup {groupop} %value %clustersize",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OPERATION_CLUSTERED_REDUCE,
            clustersize = const CLUSTER_SIZE,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A logical xor group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 0.
///
/// The type of Value must be the same as Result Type.
///
/// Requires Capability `GroupNonUniformArithmetic`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformLogicalXor")]
#[inline]
pub unsafe fn subgroup_non_uniform_logical_xor<
    const GROUP_OP: u32,
    I: VectorOrScalar<Scalar = bool>,
>(
    value: I,
) -> I {
    let mut result = I::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformLogicalXor _ %subgroup {groupop} %value",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OP,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// A logical xor group operation of all Value operands contributed by active invocations in the group.
///
/// Result Type must be a scalar or vector of Boolean type.
///
/// Execution is a Scope that identifies the group of invocations affected by this command. It must be Subgroup.
///
/// The identity I for Operation is 0. If Operation is ClusteredReduce, ClusterSize must be present.
///
/// The type of Value must be the same as Result Type.
///
/// ClusterSize is the size of cluster to use. ClusterSize must be a scalar of integer type, whose Signedness operand is 0. ClusterSize must come from a constant instruction. Behavior is undefined unless ClusterSize is at least 1 and a power of 2. If ClusterSize is greater than the size of the group, executing this instruction results in undefined behavior.
///
/// Requires Capability `GroupNonUniformArithmetic` and `GroupNonUniformClustered`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformLogicalXor")]
#[inline]
pub unsafe fn subgroup_non_uniform_logical_xor_clustered<
    const CLUSTER_SIZE: u32,
    I: VectorOrScalar<Scalar = bool>,
>(
    value: I,
) -> I {
    let mut result = I::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%clustersize = OpConstant %u32 {clustersize}",
            "%result = OpGroupNonUniformLogicalXor _ %subgroup {groupop} %value %clustersize",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            groupop = const GROUP_OPERATION_CLUSTERED_REDUCE,
            clustersize = const CLUSTER_SIZE,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Result is the Value of the invocation within the quad with a quad index equal to Index.
///
/// Result Type must be a scalar or vector of floating-point type, integer type, or Boolean type.
///
/// Execution is a Scope, but has no effect on the behavior of this instruction. It must be Subgroup.
///
/// The type of Value must be the same as Result Type.
///
/// Index must be a scalar of integer type, whose Signedness operand is 0.
///
/// Before version 1.5, Index must come from a constant instruction. Starting with version 1.5, Index must be dynamically uniform.
///
/// If the value of Index is greater than or equal to 4, or refers to an inactive invocation, the resulting value is undefined.
///
/// Requires Capability `GroupNonUniformQuad`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformQuadBroadcast")]
#[inline]
pub unsafe fn subgroup_non_uniform_quad_broadcast<T: VectorOrScalar>(value: T, id: u32) -> T {
    let mut result = T::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%value = OpLoad _ {value}",
            "%id = OpLoad _ {id}",
            "%result = OpGroupNonUniformQuadBroadcast _ %subgroup %value %id",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            value = in(reg) &value,
            id = in(reg) &id,
            result = in(reg) &mut result,
        }
    }

    result
}

/// Direction is the kind of swap to perform.
///
/// Direction must be a scalar of integer type, whose Signedness operand is 0.
///
/// Direction must come from a constant instruction.
///
/// The value returned in Result is the value provided to Value by another invocation in the same quad scope instance. The invocation providing this value is determined according to Direction.
///
/// Requires Capability `GroupNonUniformQuad`.
pub enum QuadDirection {
    /// A Direction of 0 indicates a horizontal swap;
    /// - Invocations with quad indices of 0 and 1 swap values
    /// - Invocations with quad indices of 2 and 3 swap values
    Horizontal = 0,
    /// A Direction of 1 indicates a vertical swap;
    /// - Invocations with quad indices of 0 and 2 swap values
    /// - Invocations with quad indices of 1 and 3 swap values
    Vertical = 1,
    /// A Direction of 2 indicates a diagonal swap;
    /// - Invocations with quad indices of 0 and 3 swap values
    /// - Invocations with quad indices of 1 and 2 swap values
    Diagonal = 2,
}

/// Swap the Value of the invocation within the quad with another invocation in the quad using Direction.
///
/// Result Type must be a scalar or vector of floating-point type, integer type, or Boolean type.
///
/// Execution is a Scope, but has no effect on the behavior of this instruction. It must be Subgroup.
///
/// The type of Value must be the same as Result Type.
///
/// Direction is the kind of swap to perform.
///
/// Direction must be a scalar of integer type, whose Signedness operand is 0.
///
/// Direction must come from a constant instruction.
///
/// The value returned in Result is the value provided to Value by another invocation in the same quad scope instance. The invocation providing this value is determined according to Direction.
///
/// A Direction of 0 indicates a horizontal swap;
/// - Invocations with quad indices of 0 and 1 swap values
/// - Invocations with quad indices of 2 and 3 swap values
/// A Direction of 1 indicates a vertical swap;
/// - Invocations with quad indices of 0 and 2 swap values
/// - Invocations with quad indices of 1 and 3 swap values
/// A Direction of 2 indicates a diagonal swap;
/// - Invocations with quad indices of 0 and 3 swap values
/// - Invocations with quad indices of 1 and 2 swap values
///
/// Direction must be one of the above values.
///
/// If an active invocation reads Value from an inactive invocation, the resulting value is undefined.
///
/// Requires Capability `GroupNonUniformQuad`.
#[spirv_std_macros::gpu_only]
#[doc(alias = "OpGroupNonUniformQuadSwap")]
#[inline]
pub unsafe fn subgroup_non_uniform_quad_swap<const DIRECTION: u32, T: VectorOrScalar>(
    value: T,
) -> T {
    let mut result = T::default();

    unsafe {
        asm! {
            "%u32 = OpTypeInt 32 0",
            "%subgroup = OpConstant %u32 {subgroup}",
            "%direction = OpConstant %u32 {direction}",
            "%value = OpLoad _ {value}",
            "%result = OpGroupNonUniformQuadSwap _ %subgroup %value %direction",
            "OpStore {result} %result",
            subgroup = const SUBGROUP,
            direction = const DIRECTION,
            value = in(reg) &value,
            result = in(reg) &mut result,
        }
    }

    result
}
