#[repr(C)]
pub struct Optimizer {
    _unused: [u8; 0],
}

#[repr(C)]
pub struct OptimizerOptions {
    _unused: [u8; 0],
}

#[derive(Copy, Clone, Debug)]
#[repr(C)]
pub enum Passes {
    // Creates a null pass.
    // A null pass does nothing to the SPIR-V module to be optimized.
    Null,
    // Creates a strip-atomic-counter-memory pass.
    // A strip-atomic-counter-memory pass removes all usages of the
    // AtomicCounterMemory bit in Memory Semantics bitmasks. This bit is a no-op in
    // Vulkan, so isn't needed in that env. And the related capability is not
    // allowed in WebGPU, so it is not allowed in that env.
    StripAtomicCounterMemory,
    // Creates a strip-debug-info pass.
    // A strip-debug-info pass removes all debug instructions (as documented in
    // Section 3.32.2 of the SPIR-V spec) of the SPIR-V module to be optimized.
    StripDebugInfo,
    // Creates a strip-reflect-info pass.
    // A strip-reflect-info pass removes all reflections instructions.
    // For now, this is limited to removing decorations defined in
    // SPV_GOOGLE_hlsl_functionality1.  The coverage may expand in
    // the future.
    StripReflectInfo,
    // Creates an eliminate-dead-functions pass.
    // An eliminate-dead-functions pass will remove all functions that are not in
    // the call trees rooted at entry points and exported functions.  These
    // functions are not needed because they will never be called.
    EliminateDeadFunctions,
    // Creates an eliminate-dead-members pass.
    // An eliminate-dead-members pass will remove all unused members of structures.
    // This will not affect the data layout of the remaining members.
    EliminateDeadMembers,
    // Creates a flatten-decoration pass.
    // A flatten-decoration pass replaces grouped decorations with equivalent
    // ungrouped decorations.  That is, it replaces each OpDecorationGroup
    // instruction and associated OpGroupDecorate and OpGroupMemberDecorate
    // instructions with equivalent OpDecorate and OpMemberDecorate instructions.
    // The pass does not attempt to preserve debug information for instructions
    // it removes.
    FlattenDecoration,
    // Creates a freeze-spec-constant-value pass.
    // A freeze-spec-constant pass specializes the value of spec constants to
    // their default values. This pass only processes the spec constants that have
    // SpecId decorations (defined by OpSpecConstant, OpSpecConstantTrue, or
    // OpSpecConstantFalse instructions) and replaces them with their normal
    // counterparts (OpConstant, OpConstantTrue, or OpConstantFalse). The
    // corresponding SpecId annotation instructions will also be removed. This
    // pass does not fold the newly added normal constants and does not process
    // other spec constants defined by OpSpecConstantComposite or
    // OpSpecConstantOp.
    FreezeSpecConstantValue,
    // Creates a fold-spec-constant-op-and-composite pass.
    // A fold-spec-constant-op-and-composite pass folds spec constants defined by
    // OpSpecConstantOp or OpSpecConstantComposite instruction, to normal Constants
    // defined by OpConstantTrue, OpConstantFalse, OpConstant, OpConstantNull, or
    // OpConstantComposite instructions. Note that spec constants defined with
    // OpSpecConstant, OpSpecConstantTrue, or OpSpecConstantFalse instructions are
    // not handled, as these instructions indicate their value are not determined
    // and can be changed in future. A spec constant is foldable if all of its
    // value(s) can be determined from the module. E.g., an integer spec constant
    // defined with OpSpecConstantOp instruction can be folded if its value won't
    // change later. This pass will replace the original OpSpecContantOp instruction
    // with an OpConstant instruction. When folding composite spec constants,
    // new instructions may be inserted to define the components of the composite
    // constant first, then the original spec constants will be replaced by
    // OpConstantComposite instructions.
    //
    // There are some operations not supported yet:
    //   OpSConvert, OpFConvert, OpQuantizeToF16 and
    //   all the operations under Kernel capability.
    // TODO(qining): Add support for the operations listed above.
    FoldSpecConstantOpAndComposite,
    // Creates a unify-constant pass.
    // A unify-constant pass de-duplicates the constants. Constants with the exact
    // same value and identical form will be unified and only one constant will
    // be kept for each unique pair of type and value.
    // There are several cases not handled by this pass:
    //  1) Constants defined by OpConstantNull instructions (null constants) and
    //  constants defined by OpConstantFalse, OpConstant or OpConstantComposite
    //  with value 0 (zero-valued normal constants) are not considered equivalent.
    //  So null constants won't be used to replace zero-valued normal constants,
    //  vice versa.
    //  2) Whenever there are decorations to the constant's result id id, the
    //  constant won't be handled, which means, it won't be used to replace any
    //  other constants, neither can other constants replace it.
    //  3) NaN in float point format with different bit patterns are not unified.
    UnifyConstant,
    // Creates a eliminate-dead-constant pass.
    // A eliminate-dead-constant pass removes dead constants, including normal
    // contants defined by OpConstant, OpConstantComposite, OpConstantTrue, or
    // OpConstantFalse and spec constants defined by OpSpecConstant,
    // OpSpecConstantComposite, OpSpecConstantTrue, OpSpecConstantFalse or
    // OpSpecConstantOp.
    EliminateDeadConstant,
    // Creates a strength-reduction pass.
    // A strength-reduction pass will look for opportunities to replace an
    // instruction with an equivalent and less expensive one.  For example,
    // multiplying by a power of 2 can be replaced by a bit shift.
    StrengthReduction,
    // Creates a block merge pass.
    // This pass searches for blocks with a single Branch to a block with no
    // other predecessors and merges the blocks into a single block. Continue
    // blocks and Merge blocks are not candidates for the second block.
    //
    // The pass is most useful after Dead Branch Elimination, which can leave
    // such sequences of blocks. Merging them makes subsequent passes more
    // effective, such as single block local store-load elimination.
    //
    // While this pass reduces the number of occurrences of this sequence, at
    // this time it does not guarantee all such sequences are eliminated.
    //
    // Presence of phi instructions can inhibit this optimization. Handling
    // these is left for future improvements.
    BlockMerge,
    // Creates an exhaustive inline pass.
    // An exhaustive inline pass attempts to exhaustively inline all function
    // calls in all functions in an entry point call tree. The intent is to enable,
    // albeit through brute force, analysis and optimization across function
    // calls by subsequent optimization passes. As the inlining is exhaustive,
    // there is no attempt to optimize for size or runtime performance. Functions
    // that are not in the call tree of an entry point are not changed.
    InlineExhaustive,
    // Creates an opaque inline pass.
    // An opaque inline pass inlines all function calls in all functions in all
    // entry point call trees where the called function contains an opaque type
    // in either its parameter types or return type. An opaque type is currently
    // defined as Image, Sampler or SampledImage. The intent is to enable, albeit
    // through brute force, analysis and optimization across these function calls
    // by subsequent passes in order to remove the storing of opaque types which is
    // not legal in Vulkan. Functions that are not in the call tree of an entry
    // point are not changed.
    InlineOpaque,
    // Creates a single-block local variable load/store elimination pass.
    // For every entry point function, do single block memory optimization of
    // function variables referenced only with non-access-chain loads and stores.
    // For each targeted variable load, if previous store to that variable in the
    // block, replace the load's result id with the value id of the store.
    // If previous load within the block, replace the current load's result id
    // with the previous load's result id. In either case, delete the current
    // load. Finally, check if any remaining stores are useless, and delete store
    // and variable if possible.
    //
    // The presence of access chain references and function calls can inhibit
    // the above optimization.
    //
    // Only modules with relaxed logical addressing (see opt/instruction.h) are
    // currently processed.
    //
    // This pass is most effective if preceeded by Inlining and
    // LocalAccessChainConvert. This pass will reduce the work needed to be done
    // by LocalSingleStoreElim and LocalMultiStoreElim.
    //
    // Only functions in the call tree of an entry point are processed.
    LocalSingleBlockLoadStoreElim,
    // Create dead branch elimination pass.
    // For each entry point function, this pass will look for SelectionMerge
    // BranchConditionals with constant condition and convert to a Branch to
    // the indicated label. It will delete resulting dead blocks.
    //
    // For all phi functions in merge block, replace all uses with the id
    // corresponding to the living predecessor.
    //
    // Note that some branches and blocks may be left to avoid creating invalid
    // control flow. Improving this is left to future work.
    //
    // This pass is most effective when preceeded by passes which eliminate
    // local loads and stores, effectively propagating constant values where
    // possible.
    DeadBranchElim,
    // Creates an SSA local variable load/store elimination pass.
    // For every entry point function, eliminate all loads and stores of function
    // scope variables only referenced with non-access-chain loads and stores.
    // Eliminate the variables as well.
    //
    // The presence of access chain references and function calls can inhibit
    // the above optimization.
    //
    // Only shader modules with relaxed logical addressing (see opt/instruction.h)
    // are currently processed. Currently modules with any extensions enabled are
    // not processed. This is left for future work.
    //
    // This pass is most effective if preceeded by Inlining and
    // LocalAccessChainConvert. LocalSingleStoreElim and LocalSingleBlockElim
    // will reduce the work that this pass has to do.
    LocalMultiStoreElim,
    // Creates a local access chain conversion pass.
    // A local access chain conversion pass identifies all function scope
    // variables which are accessed only with loads, stores and access chains
    // with constant indices. It then converts all loads and stores of such
    // variables into equivalent sequences of loads, stores, extracts and inserts.
    //
    // This pass only processes entry point functions. It currently only converts
    // non-nested, non-ptr access chains. It does not process modules with
    // non-32-bit integer types present. Optional memory access options on loads
    // and stores are ignored as we are only processing function scope variables.
    //
    // This pass unifies access to these variables to a single mode and simplifies
    // subsequent analysis and elimination of these variables along with their
    // loads and stores allowing values to propagate to their points of use where
    // possible.
    LocalAccessChainConvert,
    // Creates a local single store elimination pass.
    // For each entry point function, this pass eliminates loads and stores for
    // function scope variable that are stored to only once, where possible. Only
    // whole variable loads and stores are eliminated; access-chain references are
    // not optimized. Replace all loads of such variables with the value that is
    // stored and eliminate any resulting dead code.
    //
    // Currently, the presence of access chains and function calls can inhibit this
    // pass, however the Inlining and LocalAccessChainConvert passes can make it
    // more effective. In additional, many non-load/store memory operations are
    // not supported and will prohibit optimization of a function. Support of
    // these operations are future work.
    //
    // Only shader modules with relaxed logical addressing (see opt/instruction.h)
    // are currently processed.
    //
    // This pass will reduce the work needed to be done by LocalSingleBlockElim
    // and LocalMultiStoreElim and can improve the effectiveness of other passes
    // such as DeadBranchElimination which depend on values for their analysis.
    LocalSingleStoreElim,
    // Creates an insert/extract elimination pass.
    // This pass processes each entry point function in the module, searching for
    // extracts on a sequence of inserts. It further searches the sequence for an
    // insert with indices identical to the extract. If such an insert can be
    // found before hitting a conflicting insert, the extract's result id is
    // replaced with the id of the values from the insert.
    //
    // Besides removing extracts this pass enables subsequent dead code elimination
    // passes to delete the inserts. This pass performs best after access chains are
    // converted to inserts and extracts and local loads and stores are eliminated.
    InsertExtractElim,
    // Creates a dead insert elimination pass.
    // This pass processes each entry point function in the module, searching for
    // unreferenced inserts into composite types. These are most often unused
    // stores to vector components. They are unused because they are never
    // referenced, or because there is another insert to the same component between
    // the insert and the reference. After removing the inserts, dead code
    // elimination is attempted on the inserted values.
    //
    // This pass performs best after access chains are converted to inserts and
    // extracts and local loads and stores are eliminated. While executing this
    // pass can be advantageous on its own, it is also advantageous to execute
    // this pass after CreateInsertExtractPass() as it will remove any unused
    // inserts created by that pass.
    DeadInsertElim,
    // Create aggressive dead code elimination pass
    // This pass eliminates unused code from the module. In addition,
    // it detects and eliminates code which may have spurious uses but which do
    // not contribute to the output of the function. The most common cause of
    // such code sequences is summations in loops whose result is no longer used
    // due to dead code elimination. This optimization has additional compile
    // time cost over standard dead code elimination.
    //
    // This pass only processes entry point functions. It also only processes
    // shaders with relaxed logical addressing (see opt/instruction.h). It
    // currently will not process functions with function calls. Unreachable
    // functions are deleted.
    //
    // This pass will be made more effective by first running passes that remove
    // dead control flow and inlines function calls.
    //
    // This pass can be especially useful after running Local Access Chain
    // Conversion, which tends to cause cycles of dead code to be left after
    // Store/Load elimination passes are completed. These cycles cannot be
    // eliminated with standard dead code elimination.
    AggressiveDCE,
    // Create line propagation pass
    // This pass propagates line information based on the rules for OpLine and
    // OpNoline and clones an appropriate line instruction into every instruction
    // which does not already have debug line instructions.
    //
    // This pass is intended to maximize preservation of source line information
    // through passes which delete, move and clone instructions. Ideally it should
    // be run before any such pass. It is a bookend pass with EliminateDeadLines
    // which can be used to remove redundant line instructions at the end of a
    // run of such passes and reduce final output file size.
    PropagateLineInfo,
    // Create dead line elimination pass
    // This pass eliminates redundant line instructions based on the rules for
    // OpLine and OpNoline. Its main purpose is to reduce the size of the file
    // need to store the SPIR-V without losing line information.
    //
    // This is a bookend pass with PropagateLines which attaches line instructions
    // to every instruction to preserve line information during passes which
    // delete, move and clone instructions. DeadLineElim should be run after
    // PropagateLines and all such subsequent passes. Normally it would be one
    // of the last passes to be run.
    RedundantLineInfoElim,
    // Creates a compact ids pass.
    // The pass remaps result ids to a compact and gapless range starting from %1.
    CompactIds,
    // Creates a remove duplicate pass.
    // This pass removes various duplicates:
    // * duplicate capabilities;
    // * duplicate extended instruction imports;
    // * duplicate types;
    // * duplicate decorations.
    RemoveDuplicates,
    // Creates a CFG cleanup pass.
    // This pass removes cruft from the control flow graph of functions that are
    // reachable from entry points and exported functions. It currently includes the
    // following functionality:
    //
    // - Removal of unreachable basic blocks.
    CFGCleanup,
    // Create dead variable elimination pass.
    // This pass will delete module scope variables, along with their decorations,
    // that are not referenced.
    DeadVariableElimination,
    // create merge return pass.
    // changes functions that have multiple return statements so they have a single
    // return statement.
    //
    // for structured control flow it is assumed that the only unreachable blocks in
    // the function are trivial merge and continue blocks.
    //
    // a trivial merge block contains the label and an opunreachable instructions,
    // nothing else.  a trivial continue block contain a label and an opbranch to
    // the header, nothing else.
    //
    // these conditions are guaranteed to be met after running dead-branch
    // elimination.
    MergeReturn,
    // Create value numbering pass.
    // This pass will look for instructions in the same basic block that compute the
    // same value, and remove the redundant ones.
    LocalRedundancyElimination,
    // Create LICM pass.
    // This pass will look for invariant instructions inside loops and hoist them to
    // the loops preheader.
    LoopInvariantCodeMotion,
    // Creates a loop peeling pass.
    // This pass will look for conditions inside a loop that are true or false only
    // for the N first or last iteration. For loop with such condition, those N
    // iterations of the loop will be executed outside of the main loop.
    // To limit code size explosion, the loop peeling can only happen if the code
    // size growth for each loop is under |code_growth_threshold|.
    LoopPeeling,
    // Creates a loop unswitch pass.
    // This pass will look for loop independent branch conditions and move the
    // condition out of the loop and version the loop based on the taken branch.
    // Works best after LICM and local multi store elimination pass.
    LoopUnswitch,
    // Create global value numbering pass.
    // This pass will look for instructions where the same value is computed on all
    // paths leading to the instruction.  Those instructions are deleted.
    RedundancyElimination,
    // Create a private to local pass.
    // This pass looks for variables delcared in the private storage class that are
    // used in only one function.  Those variables are moved to the function storage
    // class in the function that they are used.
    PrivateToLocal,
    // Creates a conditional constant propagation (CCP) pass.
    // This pass implements the SSA-CCP algorithm in
    //
    //      Constant propagation with conditional branches,
    //      Wegman and Zadeck, ACM TOPLAS 13(2):181-210.
    //
    // Constant values in expressions and conditional jumps are folded and
    // simplified. This may reduce code size by removing never executed jump targets
    // and computations with constant operands.
    ConditionalConstantPropagation,
    // Creates a workaround driver bugs pass.  This pass attempts to work around
    // a known driver bug (issue #1209) by identifying the bad code sequences and
    // rewriting them.
    //
    // Current workaround: Avoid OpUnreachable instructions in loops.
    Workaround1209,
    // Creates a pass that converts if-then-else like assignments into OpSelect.
    IfConversion,
    // Creates a pass that will replace instructions that are not valid for the
    // current shader stage by constants.  Has no effect on non-shader modules.
    ReplaceInvalidOpcode,
    // Creates a pass that simplifies instructions using the instruction folder.
    Simplification,
    // Create the SSA rewrite pass.
    // This pass converts load/store operations on function local variables into
    // operations on SSA IDs.  This allows SSA optimizers to act on these variables.
    // Only variables that are local to the function and of supported types are
    // processed (see IsSSATargetVar for details).
    SSARewrite,
    // Create pass to convert relaxed precision instructions to half precision.
    // This pass converts as many relaxed float32 arithmetic operations to half as
    // possible. It converts any float32 operands to half if needed. It converts
    // any resulting half precision values back to float32 as needed. No variables
    // are changed. No image operations are changed.
    //
    // Best if run after function scope store/load and composite operation
    // eliminations are run. Also best if followed by instruction simplification,
    // redundancy elimination and DCE.
    ConvertRelaxedToHalf,
    // Create relax float ops pass.
    // This pass decorates all float32 result instructions with RelaxedPrecision
    // if not already so decorated.
    RelaxFloatOps,
    // Create copy propagate arrays pass.
    // This pass looks to copy propagate memory references for arrays.  It looks
    // for specific code patterns to recognize array copies.
    CopyPropagateArrays,
    // Create a vector dce pass.
    // This pass looks for components of vectors that are unused, and removes them
    // from the vector.  Note this would still leave around lots of dead code that
    // a pass of ADCE will be able to remove.
    VectorDCE,
    // Create a pass to reduce the size of loads.
    // This pass looks for loads of structures where only a few of its members are
    // used.  It replaces the loads feeding an OpExtract with an OpAccessChain and
    // a load of the specific elements.
    ReduceLoadSize,
    // Create a pass to combine chained access chains.
    // This pass looks for access chains fed by other access chains and combines
    // them into a single instruction where possible.
    CombineAccessChains,
    // Create a pass to upgrade to the VulkanKHR memory model.
    // This pass upgrades the Logical GLSL450 memory model to Logical VulkanKHR.
    // Additionally, it modifies memory, image, atomic and barrier operations to
    // conform to that model's requirements.
    UpgradeMemoryModel,
    // Create a pass to do code sinking.  Code sinking is a transformation
    // where an instruction is moved into a more deeply nested construct.
    CodeSinking,
    // Create a pass to adds initializers for OpVariable calls that require them
    // in WebGPU. Currently this pass naively initializes variables that are
    // missing an initializer with a null value. In the future it may initialize
    // variables to the first value stored in them, if that is a constant.
    GenerateWebGPUInitializers,
    // Create a pass to fix incorrect storage classes.  In order to make code
    // generation simpler, DXC may generate code where the storage classes do not
    // match up correctly.  This pass will fix the errors that it can.
    FixStorageClass,
    // Create a pass to legalize OpVectorShuffle operands going into WebGPU. WebGPU
    // forbids using 0xFFFFFFFF, which indicates an undefined result, so this pass
    // converts those literals to 0.
    LegalizeVectorShuffle,
    // Create a pass to decompose initialized variables into a seperate variable
    // declaration and an initial store.
    DecomposeInitializedVariables,
    // Create a pass to attempt to split up invalid unreachable merge-blocks and
    // continue-targets to legalize for WebGPU.
    SplitInvalidUnreachable,
    // Creates a graphics robust access pass.
    //
    // This pass injects code to clamp indexed accesses to buffers and internal
    // arrays, providing guarantees satisfying Vulkan's robustBufferAccess rules.
    //
    // TODO(dneto): Clamps coordinates and sample index for pointer calculations
    // into storage images (OpImageTexelPointer).  For an cube array image, it
    // assumes the maximum layer count times 6 is at most 0xffffffff.
    //
    // NOTE: This pass will fail with a message if:
    // - The module is not a Shader module.
    // - The module declares VariablePointers, VariablePointersStorageBuffer, or
    //   RuntimeDescriptorArrayEXT capabilities.
    // - The module uses an addressing model other than Logical
    // - Access chain indices are wider than 64 bits.
    // - Access chain index for a struct is not an OpConstant integer or is out
    //   of range. (The module is already invalid if that is the case.)
    // - TODO(dneto): The OpImageTexelPointer coordinate component is not 32-bits
    // wide.
    //
    // NOTE: Access chain indices are always treated as signed integers.  So
    //   if an array has a fixed size of more than 2^31 elements, then elements
    //   from 2^31 and above are never accessible with a 32-bit index,
    //   signed or unsigned.  For this case, this pass will clamp the index
    //   between 0 and at 2^31-1, inclusive.
    //   Similarly, if an array has more then 2^15 element and is accessed with
    //   a 16-bit index, then elements from 2^15 and above are not accessible.
    //   In this case, the pass will clamp the index between 0 and 2^15-1
    //   inclusive.
    GraphicsRobustAccess,
    // Create descriptor scalar replacement pass.
    // This pass replaces every array variable |desc| that has a DescriptorSet and
    // Binding decorations with a new variable for each element of the array.
    // Suppose |desc| was bound at binding |b|.  Then the variable corresponding to
    // |desc[i]| will have binding |b+i|.  The descriptor set will be the same.  It
    // is assumed that no other variable already has a binding that will used by one
    // of the new variables.  If not, the pass will generate invalid Spir-V.  All
    // accesses to |desc| must be OpAccessChain instructions with a literal index
    // for the first index.
    DescriptorScalarReplacement,
    // Create a pass to replace each OpKill instruction with a function call to a
    // function that has a single OpKill.  Also replace each OpTerminateInvocation
    // instruction  with a function call to a function that has a single
    // OpTerminateInvocation.  This allows more code to be inlined.
    WrapOpKill,
    // Replaces the extensions VK_AMD_shader_ballot,VK_AMD_gcn_shader, and
    // VK_AMD_shader_trinary_minmax with equivalent code using core instructions and
    // capabilities.
    AmdExtToKhr,
}

extern "C" {
    pub fn optimizer_create(env: crate::shared::TargetEnv) -> *mut Optimizer;
    pub fn optimizer_destroy(opt: *mut Optimizer);

    pub fn optimizer_run(
        opt: *const Optimizer,
        input_ptr: *const u32,
        input_size: usize,
        binary: *mut *mut crate::shared::Binary,
        message_callback: crate::diagnostics::MessageCallback,
        message_ctx: *mut std::ffi::c_void,
        options: *const OptimizerOptions,
    ) -> crate::shared::SpirvResult;

    /// Creates an optimizer options object with default options. Returns a valid
    /// options object. The object remains valid until it is passed into
    /// |spvOptimizerOptionsDestroy|.
    #[link_name = "spvOptimizerOptionsCreate"]
    pub fn optimizer_options_create() -> *mut OptimizerOptions;

    /// Destroys the given optimizer options object.
    #[link_name = "spvOptimizerOptionsDestroy"]
    pub fn optimizer_options_destroy(options: *mut OptimizerOptions);

    /// Records whether or not the optimizer should run the validator before
    /// optimizing.  If |val| is true, the validator will be run.
    #[link_name = "spvOptimizerOptionsSetRunValidator"]
    pub fn optimizer_options_run_validator(options: *mut OptimizerOptions, run: bool);

    /// Records the validator options that should be passed to the validator if it is
    /// run.
    #[link_name = "spvOptimizerOptionsSetValidatorOptions"]
    pub fn optimizer_options_set_validator_options(
        options: *mut OptimizerOptions,
        validator_opts: *mut crate::val::ValidatorOptions,
    );

    /// Records the maximum possible value for the id bound.
    #[link_name = "spvOptimizerOptionsSetMaxIdBound"]
    pub fn optimizer_options_set_max_id_bound(options: *mut OptimizerOptions, max: u32);

    /// Records whether all bindings within the module should be preserved.
    #[link_name = "spvOptimizerOptionsSetPreserveBindings"]
    pub fn optimizer_options_preserve_bindings(options: *mut OptimizerOptions, preserve: bool);

    /// Records whether all specialization constants within the module
    /// should be preserved.
    #[link_name = "spvOptimizerOptionsSetPreserveSpecConstants"]
    pub fn optimizer_options_preserve_spec_constants(
        options: *mut OptimizerOptions,
        preserve: bool,
    );

    pub fn optimizer_register_pass(opt: *mut Optimizer, which: Passes);

    /// Registers passes that attempt to improve performance of generated code.
    /// This sequence of passes is subject to constant review and will change
    /// from time to time.
    pub fn optimizer_register_performance_passes(opt: *mut Optimizer);

    /// Registers passes that attempt to improve the size of generated code.
    /// This sequence of passes is subject to constant review and will change
    /// from time to time.
    pub fn optimizer_register_size_passes(opt: *mut Optimizer);

    /// Registers passes that have been prescribed for converting from Vulkan to
    /// WebGPU. This sequence of passes is subject to constant review and will
    /// change from time to time.
    pub fn optimizer_register_vulkan_to_webgpu_passes(opt: *mut Optimizer);

    /// Registers passes that have been prescribed for converting from WebGPU to
    /// Vulkan. This sequence of passes is subject to constant review and will
    /// change from time to time.
    pub fn optimizer_register_webgpu_to_vulkan_passes(opt: *mut Optimizer);

    /// Registers passes that attempt to legalize the generated code.
    ///
    /// Note: this recipe is specially designed for legalizing SPIR-V. It should be
    /// used by compilers after translating HLSL source code literally. It should
    /// *not* be used by general workloads for performance or size improvement.
    ///
    /// This sequence of passes is subject to constant review and will change
    /// from time to time.
    pub fn optimizer_register_legalization_passes(opt: *mut Optimizer);

// Some passes take arguments, so we create those separately on a
// case-by-case basis

// #[repr(C)]
// pub struct SpecConstantDefault {
//     pub id: u32,
//     pub value_ptr: *const c_char,
//     pub value_len: usize,
// }

// Creates a set-spec-constant-default-value pass from a mapping from spec-ids
// to the default values in the form of string.
// A set-spec-constant-default-value pass sets the default values for the
// spec constants that have SpecId decorations (i.e., those defined by
// OpSpecConstant{|True|False} instructions).
// SetSpecConstantDefaultValuePass(
//     const std::unordered_map<uint32_t, std::string>& id_value_map);

// Create a pass to instrument OpDebugPrintf instructions.
// This pass replaces all OpDebugPrintf instructions with instructions to write
// a record containing the string id and the all specified values into a special
// printf output buffer (if space allows). This pass is designed to support
// the printf validation in the Vulkan validation layers.
//
// The instrumentation will write buffers in debug descriptor set |desc_set|.
// It will write |shader_id| in each output record to identify the shader
// module which generated the record.
// InstDebugPrintfPass(uint32_t desc_set,
//     uint32_t shader_id);

// Create a pass to instrument bindless descriptor checking
// This pass instruments all bindless references to check that descriptor
// array indices are inbounds, and if the descriptor indexing extension is
// enabled, that the descriptor has been initialized. If the reference is
// invalid, a record is written to the debug output buffer (if space allows)
// and a null value is returned. This pass is designed to support bindless
// validation in the Vulkan validation layers.
//
// TODO(greg-lunarg): Add support for buffer references. Currently only does
// checking for image references.
//
// Dead code elimination should be run after this pass as the original,
// potentially invalid code is not removed and could cause undefined behavior,
// including crashes. It may also be beneficial to run Simplification
// (ie Constant Propagation), DeadBranchElim and BlockMerge after this pass to
// optimize instrument code involving the testing of compile-time constants.
// It is also generally recommended that this pass (and all
// instrumentation passes) be run after any legalization and optimization
// passes. This will give better analysis for the instrumentation and avoid
// potentially de-optimizing the instrument code, for example, inlining
// the debug record output function throughout the module.
//
// The instrumentation will read and write buffers in debug
// descriptor set |desc_set|. It will write |shader_id| in each output record
// to identify the shader module which generated the record.
// |input_length_enable| controls instrumentation of runtime descriptor array
// references, and |input_init_enable| controls instrumentation of descriptor
// initialization checking, both of which require input buffer support.
// InstBindlessCheckPass(
//     uint32_t desc_set, uint32_t shader_id, bool input_length_enable = false,
//     bool input_init_enable = false, bool input_buff_oob_enable = false);

// // Create a pass to instrument physical buffer address checking
// // This pass instruments all physical buffer address references to check that
// // all referenced bytes fall in a valid buffer. If the reference is
// // invalid, a record is written to the debug output buffer (if space allows)
// // and a null value is returned. This pass is designed to support buffer
// // address validation in the Vulkan validation layers.
// //
// // Dead code elimination should be run after this pass as the original,
// // potentially invalid code is not removed and could cause undefined behavior,
// // including crashes. Instruction simplification would likely also be
// // beneficial. It is also generally recommended that this pass (and all
// // instrumentation passes) be run after any legalization and optimization
// // passes. This will give better analysis for the instrumentation and avoid
// // potentially de-optimizing the instrument code, for example, inlining
// // the debug record output function throughout the module.
// //
// // The instrumentation will read and write buffers in debug
// // descriptor set |desc_set|. It will write |shader_id| in each output record
// // to identify the shader module which generated the record.
// InstBuffAddrCheckPass(uint32_t desc_set,
//                                                  uint32_t shader_id);

// Create loop unroller pass.
// Creates a pass to unroll loops which have the "Unroll" loop control
// mask set. The loops must meet a specific criteria in order to be unrolled
// safely this criteria is checked before doing the unroll by the
// LoopUtils::CanPerformUnroll method. Any loop that does not meet the criteria
// won't be unrolled. See CanPerformUnroll LoopUtils.h for more information.
//LoopUnrollPass(bool fully_unroll, int factor = 0);

// Create scalar replacement pass.
// This pass replaces composite function scope variables with variables for each
// element if those elements are accessed individually.  The parameter is a
// limit on the number of members in the composite variable that the pass will
// consider replacing.
//ScalarReplacementPass(uint32_t size_limit = 100);

// Creates a loop fission pass.
// This pass will split all top level loops whose register pressure exceedes the
// given |threshold|.
//LoopFissionPass(size_t threshold);

// Creates a loop fusion pass.
// This pass will look for adjacent loops that are compatible and legal to be
// fused. The fuse all such loops as long as the register usage for the fused
// loop stays under the threshold defined by |max_registers_per_loop|.
//LoopFusionPass(size_t max_registers_per_loop);
}
