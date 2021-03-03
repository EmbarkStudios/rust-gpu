use super::CodegenCx;
use crate::abi::ConvSpirvType;
use crate::builder_spirv::SpirvValue;
use crate::builder_spirv::SpirvValueExt;
use crate::codegen_cx::BindlessDescriptorSets;
use crate::spirv_type::SpirvType;
use crate::symbols::{parse_attrs, Entry, SpirvAttribute};
use rspirv::dr::Operand;
use rspirv::spirv::{Decoration, ExecutionModel, FunctionControl, StorageClass, Word};
use rustc_hir as hir;
use rustc_middle::ty::layout::TyAndLayout;
use rustc_middle::ty::{Instance, Ty};
use rustc_span::Span;
use rustc_target::abi::call::{FnAbi, PassMode};
use rustc_target::abi::{Align, Size};
use std::collections::HashMap;

impl<'tcx> CodegenCx<'tcx> {
    // Entry points declare their "interface" (all uniforms, inputs, outputs, etc.) as parameters.
    // spir-v uses globals to declare the interface. So, we need to generate a lil stub for the
    // "real" main that collects all those global variables and calls the user-defined main
    // function.
    pub fn entry_stub(
        &self,
        instance: &Instance<'_>,
        fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
        entry_func: SpirvValue,
        name: String,
        entry: Entry,
    ) {
        let local_id = match instance.def_id().as_local() {
            Some(id) => id,
            None => {
                self.tcx
                    .sess
                    .err(&format!("Cannot declare {} as an entry point", name));
                return;
            }
        };
        let fn_hir_id = self.tcx.hir().local_def_id_to_hir_id(local_id);
        let body = self.tcx.hir().body(self.tcx.hir().body_owned_by(fn_hir_id));
        for (abi, arg) in fn_abi.args.iter().zip(body.params) {
            if let PassMode::Direct(_) = abi.mode {
            } else {
                self.tcx.sess.span_err(
                    arg.span,
                    &format!("PassMode {:?} invalid for entry point parameter", abi.mode),
                )
            }
        }
        if let PassMode::Ignore = fn_abi.ret.mode {
        } else {
            self.tcx.sess.span_err(
                self.tcx.hir().span(fn_hir_id),
                &format!(
                    "PassMode {:?} invalid for entry point return type",
                    fn_abi.ret.mode
                ),
            )
        }
        let execution_model = entry.execution_model;
        let fn_id = if execution_model == ExecutionModel::Kernel {
            self.kernel_entry_stub(entry_func, name, execution_model)
        } else {
            self.shader_entry_stub(
                self.tcx.def_span(instance.def_id()),
                entry_func,
                fn_abi,
                body.params,
                name,
                execution_model,
            )
        };
        let mut emit = self.emit_global();
        entry
            .execution_modes
            .iter()
            .for_each(|(execution_mode, execution_mode_extra)| {
                emit.execution_mode(fn_id, *execution_mode, execution_mode_extra);
            });
    }

    pub fn bindless_mode(&self) -> bool {
        self.tcx
            .sess
            .contains_name(self.tcx.hir().krate_attrs(), self.sym.spirv_bindless)
    }

    pub fn lazy_add_bindless_descriptor_sets(&self) {
        self.bindless_descriptor_sets
            .replace(Some(BindlessDescriptorSets {
                // all storage buffers are compatible and go in set 0
                buffers: self.buffer_descriptor_set(0),

                // sampled images are all compatible in vulkan, so we can overlap them
                sampled_image_1d: self.texture_bindless_descriptor_set(
                    1,
                    rspirv::spirv::Dim::Dim1D,
                    true,
                ),
                sampled_image_2d: self.texture_bindless_descriptor_set(
                    1,
                    rspirv::spirv::Dim::Dim2D,
                    true,
                ),
                sampled_image_3d: self.texture_bindless_descriptor_set(
                    1,
                    rspirv::spirv::Dim::Dim3D,
                    true,
                ),
                // jb-todo: storage images are all compatible so they can live in the same descriptor set too
            }));
    }

    fn buffer_descriptor_set(&self, descriptor_set: u32) -> Word {
        let uint_ty = SpirvType::Integer(32, false).def(rustc_span::DUMMY_SP, self);

        let runtime_array_uint =
            SpirvType::RuntimeArray { element: uint_ty }.def(rustc_span::DUMMY_SP, self);

        let buffer_struct = SpirvType::Adt {
            def_id: None,
            size: Some(Size::from_bytes(4)),
            align: Align::from_bytes(4).unwrap(),
            field_types: vec![runtime_array_uint],
            field_offsets: vec![],
            field_names: None,
            is_block: false,
        }
        .def(rustc_span::DUMMY_SP, self);

        let runtime_array_struct = SpirvType::RuntimeArray {
            element: buffer_struct,
        }
        .def(rustc_span::DUMMY_SP, self);

        let uniform_ptr_runtime_array = SpirvType::Pointer {
            pointee: runtime_array_struct,
        }
        .def(rustc_span::DUMMY_SP, self);

        let uniform_uint_ptr =
            SpirvType::Pointer { pointee: uint_ty }.def(rustc_span::DUMMY_SP, self);

        let mut emit_global = self.emit_global();
        let buffer = emit_global
            .variable(uniform_ptr_runtime_array, None, StorageClass::Uniform, None)
            .with_type(uniform_ptr_runtime_array)
            .def_cx(self);

        emit_global.decorate(
            buffer,
            rspirv::spirv::Decoration::DescriptorSet,
            std::iter::once(Operand::LiteralInt32(descriptor_set)),
        );
        emit_global.decorate(
            buffer,
            rspirv::spirv::Decoration::Binding,
            std::iter::once(Operand::LiteralInt32(0)),
        );

        emit_global.decorate(
            buffer_struct,
            rspirv::spirv::Decoration::BufferBlock,
            std::iter::empty(),
        );

        emit_global.decorate(
            runtime_array_uint,
            rspirv::spirv::Decoration::ArrayStride,
            std::iter::once(Operand::LiteralInt32(4)),
        );

        emit_global.member_decorate(
            buffer_struct,
            0,
            rspirv::spirv::Decoration::Offset,
            std::iter::once(Operand::LiteralInt32(0)),
        );

        buffer
    }

    fn texture_bindless_descriptor_set(
        &self,
        descriptor_set: u32,
        dim: rspirv::spirv::Dim,
        sampled: bool,
    ) -> Word {
        let uint_ty = SpirvType::Integer(32, false).def(rustc_span::DUMMY_SP, self);
        let float_ty = SpirvType::Float(32).def(rustc_span::DUMMY_SP, self);

        let image_2d = SpirvType::Image {
            sampled_type: float_ty,
            dim,
            depth: 0,
            arrayed: 0,
            multisampled: 0,
            sampled: if sampled { 1 } else { 0 },
            image_format: rspirv::spirv::ImageFormat::Unknown,
            access_qualifier: None,
        }
        .def(rustc_span::DUMMY_SP, self);

        let sampled_image_2d = SpirvType::SampledImage {
            image_type: image_2d,
        }
        .def(rustc_span::DUMMY_SP, self);

        let runtime_array_image = SpirvType::RuntimeArray {
            element: sampled_image_2d,
        }
        .def(rustc_span::DUMMY_SP, self);

        let uniform_ptr_runtime_array = SpirvType::Pointer {
            pointee: runtime_array_image,
        }
        .def(rustc_span::DUMMY_SP, self);

        let mut emit_global = self.emit_global();
        let image_array = emit_global
            .variable(uniform_ptr_runtime_array, None, StorageClass::Uniform, None)
            .with_type(uniform_ptr_runtime_array)
            .def_cx(self);

        emit_global.decorate(
            image_array,
            rspirv::spirv::Decoration::DescriptorSet,
            std::iter::once(Operand::LiteralInt32(descriptor_set)),
        );
        emit_global.decorate(
            image_array,
            rspirv::spirv::Decoration::Binding,
            std::iter::once(Operand::LiteralInt32(0)),
        );

        image_array
    }

    fn shader_entry_stub(
        &self,
        span: Span,
        entry_func: SpirvValue,
        entry_fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
        hir_params: &[hir::Param<'tcx>],
        name: String,
        execution_model: ExecutionModel,
    ) -> Word {
        let void = SpirvType::Void.def(span, self);
        let fn_void_void = SpirvType::Function {
            return_type: void,
            arguments: vec![],
        }
        .def(span, self);
        let entry_func_return_type = match self.lookup_type(entry_func.ty) {
            SpirvType::Function {
                return_type,
                arguments: _,
            } => return_type,
            other => self.tcx.sess.fatal(&format!(
                "Invalid entry_stub type: {}",
                other.debug(entry_func.ty, self)
            )),
        };
        let mut decoration_locations = HashMap::new();
        // Create OpVariables before OpFunction so they're global instead of local vars.
        let arguments = entry_fn_abi
            .args
            .iter()
            .zip(hir_params)
            .map(|(entry_fn_arg, hir_param)| {
                self.declare_parameter(entry_fn_arg.layout, hir_param, &mut decoration_locations)
            })
            .collect::<Vec<_>>();
        let mut emit = self.emit_global();
        let fn_id = emit
            .begin_function(void, None, FunctionControl::NONE, fn_void_void)
            .unwrap();
        emit.begin_block(None).unwrap();
        emit.function_call(
            entry_func_return_type,
            None,
            entry_func.def_cx(self),
            arguments.iter().map(|&(a, _)| a),
        )
        .unwrap();
        emit.ret().unwrap();
        emit.end_function().unwrap();

        let interface: Vec<_> = if emit.version().unwrap() > (1, 3) {
            // SPIR-V >= v1.4 includes all OpVariables in the interface.
            arguments.into_iter().map(|(a, _)| a).collect()
        } else {
            // SPIR-V <= v1.3 only includes Input and Output in the interface.
            arguments
                .into_iter()
                .filter(|&(_, s)| s == StorageClass::Input || s == StorageClass::Output)
                .map(|(a, _)| a)
                .collect()
        };
        emit.entry_point(execution_model, fn_id, name, interface);
        fn_id
    }

    fn declare_parameter(
        &self,
        layout: TyAndLayout<'tcx>,
        hir_param: &hir::Param<'tcx>,
        decoration_locations: &mut HashMap<StorageClass, u32>,
    ) -> (Word, StorageClass) {
        let storage_class = crate::abi::get_storage_class(self, layout).unwrap_or_else(|| {
            self.tcx.sess.span_fatal(
                hir_param.span,
                &format!("invalid entry param type `{}`", layout.ty),
            );
        });
        let mut has_location = matches!(
            storage_class,
            StorageClass::Input | StorageClass::Output | StorageClass::UniformConstant
        );
        // Note: this *declares* the variable too.
        let spirv_type = layout.spirv_type(hir_param.span, self);
        let variable = self
            .emit_global()
            .variable(spirv_type, None, storage_class, None);
        if let hir::PatKind::Binding(_, _, ident, _) = &hir_param.pat.kind {
            self.emit_global().name(variable, ident.to_string());
        }
        for attr in parse_attrs(self, hir_param.attrs) {
            match attr {
                SpirvAttribute::Builtin(builtin) => {
                    self.emit_global().decorate(
                        variable,
                        Decoration::BuiltIn,
                        std::iter::once(Operand::BuiltIn(builtin)),
                    );
                    has_location = false;
                }
                SpirvAttribute::DescriptorSet(index) => {
                    self.emit_global().decorate(
                        variable,
                        Decoration::DescriptorSet,
                        std::iter::once(Operand::LiteralInt32(index)),
                    );
                    has_location = false;
                }
                SpirvAttribute::Binding(index) => {
                    self.emit_global().decorate(
                        variable,
                        Decoration::Binding,
                        std::iter::once(Operand::LiteralInt32(index)),
                    );
                    has_location = false;
                }
                SpirvAttribute::Flat => {
                    self.emit_global()
                        .decorate(variable, Decoration::Flat, std::iter::empty());
                }
                _ => {}
            }
        }
        // Assign locations from left to right, incrementing each storage class
        // individually.
        // TODO: Is this right for UniformConstant? Do they share locations with
        // input/outpus?
        if has_location {
            let location = decoration_locations
                .entry(storage_class)
                .or_insert_with(|| 0);
            self.emit_global().decorate(
                variable,
                Decoration::Location,
                std::iter::once(Operand::LiteralInt32(*location)),
            );
            *location += 1;
        }
        (variable, storage_class)
    }

    // Kernel mode takes its interface as function parameters(??)
    // OpEntryPoints cannot be OpLinkage, so write out a stub to call through.
    fn kernel_entry_stub(
        &self,
        entry_func: SpirvValue,
        name: String,
        execution_model: ExecutionModel,
    ) -> Word {
        let (entry_func_return, entry_func_args) = match self.lookup_type(entry_func.ty) {
            SpirvType::Function {
                return_type,
                arguments,
            } => (return_type, arguments),
            other => self.tcx.sess.fatal(&format!(
                "Invalid kernel_entry_stub type: {}",
                other.debug(entry_func.ty, self)
            )),
        };
        let mut emit = self.emit_global();
        let fn_id = emit
            .begin_function(
                entry_func_return,
                None,
                FunctionControl::NONE,
                entry_func.ty,
            )
            .unwrap();
        let arguments = entry_func_args
            .iter()
            .map(|&ty| emit.function_parameter(ty).unwrap())
            .collect::<Vec<_>>();
        emit.begin_block(None).unwrap();
        let call_result = emit
            .function_call(entry_func_return, None, entry_func.def_cx(self), arguments)
            .unwrap();
        if self.lookup_type(entry_func_return) == SpirvType::Void {
            emit.ret().unwrap();
        } else {
            emit.ret_value(call_result).unwrap();
        }
        emit.end_function().unwrap();

        emit.entry_point(execution_model, fn_id, name, &[]);
        fn_id
    }
}
