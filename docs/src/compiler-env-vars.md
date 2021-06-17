# Debug environment variables that the Rust-GPU compiler reads

Please keep in mind that all of these variables are for internal development, and may break output
unexpectedly and generally muck things up. Please only use these if you know what you're doing.

Help is also appreciated keeping this document up to date, environment variables may be
added/removed on an ad-hoc basis without much thought, as they're internal development tools, not a
public API - this documentation is only here because these variables may be helpful diagnosing
problems for others.

It's recommended that environment variables that take paths to files or directories are set to full
paths, as the working directory of the compiler might be something wonky and unexpected, and it's
easier to set the full path.

## DUMP_MIR

Takes: path to file

Dumps the MIR of every function rust-gpu encounters to a file. Yes, rustc does have options to do
this by default, but I always forget the syntax, and plumbing through the option to spirv-builder
is annoying, so this is handy to just hack an output.

## DUMP_MODULE_ON_PANIC

Takes: path to file

If codegen panics, then write the (partially) emitted module to a file. Note that this only exists
for codegen, if the linker panics, this option does nothing, sadly.

## DUMP_PRE_LINK

Takes: path to directory

Dumps all input modules to the linker, before the linker touches them at all.

## DUMP_POST_MERGE

Takes: path to file

Dumps the merged module immediately after merging, but before the linker has done anything else
(including, well, linking the methods - LinkageAttributes will still exist, etc.). This is very
similar to DUMP_PRE_LINK, except it outputs only a single file, which might make grepping through
for stuff easier.

## DUMP_POST_SPLIT

Takes: path to directory

Dumps the modules immediately after multimodule splitting, but before final cleanup passes (e.g.
DCE to remove the other entry points).

## DUMP_POST_LINK

Takes: path to directory

Dumps all output modules from the linker. This may be multiple files due to the multimodule/module
splitting option, hence it takes a directory instead of a file path. This is the final output
binary before spirv-opt is executed, so it may be useful to output this to check if an issue is in
rust-gpu, or in spirv-opt.

## SPECIALIZER_DEBUG

Takes: presence or absence (e.g. set to `1`)

Prints to rustc stdout some debug information about the specializer.

## SPECIALIZER_DUMP_INSTANCES

Takes: path to file

Prints to file some... stuff, idk, ask eddyb (useful for debugging specializer)

## PRINT_ZOMBIE

Takes: presence or absence (e.g. set to `1`)

Prints to rustc stdout which functions were removed due to being zombies, and why.

## PRINT_ALL_ZOMBIE

Takes: presence or absence (e.g. set to `1`)

Prints to rustc stdout *everything* that was removed due to being zombies, why, and if it was an
original zombie or if it was infected. (prints a lot!)

## NO_SPIRV_VAL

Takes: presence or absence (e.g. set to `1`)

Disables running spirv-val on the final output. Spooky scary option, can cause invalid modules!

## NO_SPIRV_OPT

Takes: presence or absence (e.g. set to `1`)

Forcibly disables running spirv-opt on the final output, even if optimizations are enabled.

## NO_DCE

Takes: presence or absence (e.g. set to `1`)

Disables running dead code elimination. Can and probably will generate invalid modules or crash the
linker, hasn't been tested for a while.

## NO_COMPACT_IDS

Takes: presence or absence (e.g. set to `1`)

Disables compaction of SPIR-V IDs at the end of linking. Causes absolutely ginormous IDs to be
emitted. Useful if you're println debugging IDs in the linker (although spirv-opt will compact them
anyway, be careful).

## NO_STRUCTURIZE

Takes: presence or absence (e.g. set to `1`)

Disables structurization. Probably results in invalid modules.
