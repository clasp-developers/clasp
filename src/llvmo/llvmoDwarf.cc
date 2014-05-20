#define DEBUG_LEVEL_FULL


#include "llvm/Support/Dwarf.h"
#include "core/foundation.h"
#include "core/object.h"
#include "core/lisp.h"
#include "core/symbolToEnumConverter.h"
#include "llvmo/llvmoPackage.h"
#include "llvmoDwarf.h"
#include "llvmo/symbolTable.h"
#include "core/wrappers.h"
namespace llvmo
{


    
#define ARGS_af_dwTag "(tagsym &optional (debug-version llvm-sys:llvmdebug-version8))"
#define DECL_af_dwTag ""
#define DOCS_af_dwTag "Convert a DW_TAG and version to an integer"
    uint af_dwTag(core::Symbol_sp tagsym, uint debugVersion )
    {_G();
	core::SymbolToEnumConverter_sp converter = _sym_STARdwarfConstantsSTAR->symbolValue().as<core::SymbolToEnumConverter_O>();
	uint itag = converter->enumIndexForSymbol(tagsym);
	uint dwtag = itag + debugVersion;
	return dwtag;
    }
    
    

    void initialize_dwarf_constants()
    {_G();
	SYMBOL_EXPORT_SC_(LlvmoPkg,dwTag);
	core::af_def(LlvmoPkg,"dwTag",&af_dwTag,ARGS_af_dwTag,DECL_af_dwTag,DOCS_af_dwTag);

	SYMBOL_EXPORT_SC_(LlvmoPkg,LLVMDebugVersion11);
	SYMBOL_EXPORT_SC_(LlvmoPkg,LLVMDebugVersion10);
	SYMBOL_EXPORT_SC_(LlvmoPkg,LLVMDebugVersion9);
	SYMBOL_EXPORT_SC_(LlvmoPkg,LLVMDebugVersion8);
	SYMBOL_EXPORT_SC_(LlvmoPkg,LLVMDebugVersion7);
	SYMBOL_EXPORT_SC_(LlvmoPkg,LLVMDebugVersion6);
	SYMBOL_EXPORT_SC_(LlvmoPkg,LLVMDebugVersion5);
	SYMBOL_EXPORT_SC_(LlvmoPkg,LLVMDebugVersion4);

	_sym_LLVMDebugVersion11->defconstant(core::Fixnum_O::create(llvm::LLVMDebugVersion11));
	_sym_LLVMDebugVersion10->defconstant(core::Fixnum_O::create(llvm::LLVMDebugVersion10));
	_sym_LLVMDebugVersion9->defconstant(core::Fixnum_O::create(llvm::LLVMDebugVersion9));
	_sym_LLVMDebugVersion8->defconstant(core::Fixnum_O::create(llvm::LLVMDebugVersion8));
	_sym_LLVMDebugVersion7->defconstant(core::Fixnum_O::create(llvm::LLVMDebugVersion7));
	_sym_LLVMDebugVersion6->defconstant(core::Fixnum_O::create(llvm::LLVMDebugVersion6));
	_sym_LLVMDebugVersion5->defconstant(core::Fixnum_O::create(llvm::LLVMDebugVersion5));
	_sym_LLVMDebugVersion4->defconstant(core::Fixnum_O::create(llvm::LLVMDebugVersion4));


//===----------------------------------------------------------------------===//
// Dwarf constants as gleaned from the DWARF Debugging Information Format V.3
// reference manual http://dwarf.freestandards.org .
//


	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_array_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_class_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_entry_point);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_enumeration_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_formal_parameter);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_imported_declaration);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_label);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_lexical_block);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_member);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_pointer_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_reference_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_compile_unit);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_string_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_structure_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_subroutine_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_typedef);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_union_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_unspecified_parameters);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_variant);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_common_block);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_common_inclusion);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_inheritance);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_inlined_subroutine);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_module);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_ptr_to_member_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_set_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_subrange_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_with_stmt);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_access_declaration);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_base_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_catch_block);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_const_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_constant);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_enumerator);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_file_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_friend);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_namelist);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_namelist_item);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_packed_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_subprogram);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_template_type_parameter);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_template_value_parameter);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_thrown_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_try_block);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_variant_part);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_variable);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_volatile_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_dwarf_procedure);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_restrict_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_interface_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_namespace);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_imported_module);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_unspecified_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_partial_unit);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_imported_unit);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_condition);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_shared_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_type_unit);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_rvalue_reference_type);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_template_alias);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_MIPS_loop);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_format_label);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_function_template);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_class_template);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_GNU_template_template_param);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_GNU_template_parameter_pack);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_GNU_formal_parameter_pack);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_lo_user);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_APPLE_property);
	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_TAG_hi_user);

	// Tags defined in llvm->dwarf.h

	SYMBOL_EXPORT_SC_(LlvmoPkg,STARdwarfConstantsSTAR);
 	core::enum_<llvm::dwarf::Tag>(_sym_STARdwarfConstantsSTAR,"llvm::dwarf::Constants")
	    .value(_sym_DW_TAG_array_type,llvm::dwarf::DW_TAG_array_type)
	    .value(_sym_DW_TAG_class_type,llvm::dwarf::DW_TAG_class_type)
	    .value(_sym_DW_TAG_entry_point,llvm::dwarf::DW_TAG_entry_point)
	    .value(_sym_DW_TAG_enumeration_type,llvm::dwarf::DW_TAG_enumeration_type)
	    .value(_sym_DW_TAG_formal_parameter,llvm::dwarf::DW_TAG_formal_parameter)
	    .value(_sym_DW_TAG_imported_declaration,llvm::dwarf::DW_TAG_imported_declaration)
	    .value(_sym_DW_TAG_label,llvm::dwarf::DW_TAG_label)
	    .value(_sym_DW_TAG_lexical_block,llvm::dwarf::DW_TAG_lexical_block)
	    .value(_sym_DW_TAG_member,llvm::dwarf::DW_TAG_member)
	    .value(_sym_DW_TAG_pointer_type,llvm::dwarf::DW_TAG_pointer_type)
	    .value(_sym_DW_TAG_reference_type,llvm::dwarf::DW_TAG_reference_type)
	    .value(_sym_DW_TAG_compile_unit,llvm::dwarf::DW_TAG_compile_unit)
	    .value(_sym_DW_TAG_string_type,llvm::dwarf::DW_TAG_string_type)
	    .value(_sym_DW_TAG_structure_type,llvm::dwarf::DW_TAG_structure_type)
	    .value(_sym_DW_TAG_subroutine_type,llvm::dwarf::DW_TAG_subroutine_type)
	    .value(_sym_DW_TAG_typedef,llvm::dwarf::DW_TAG_typedef)
	    .value(_sym_DW_TAG_union_type,llvm::dwarf::DW_TAG_union_type)
	    .value(_sym_DW_TAG_unspecified_parameters,llvm::dwarf::DW_TAG_unspecified_parameters)
	    .value(_sym_DW_TAG_variant,llvm::dwarf::DW_TAG_variant)
	    .value(_sym_DW_TAG_common_block,llvm::dwarf::DW_TAG_common_block)
	    .value(_sym_DW_TAG_common_inclusion,llvm::dwarf::DW_TAG_common_inclusion)
	    .value(_sym_DW_TAG_inheritance,llvm::dwarf::DW_TAG_inheritance)
	    .value(_sym_DW_TAG_inlined_subroutine,llvm::dwarf::DW_TAG_inlined_subroutine)
	    .value(_sym_DW_TAG_module,llvm::dwarf::DW_TAG_module)
	    .value(_sym_DW_TAG_ptr_to_member_type,llvm::dwarf::DW_TAG_ptr_to_member_type)
	    .value(_sym_DW_TAG_set_type,llvm::dwarf::DW_TAG_set_type)
	    .value(_sym_DW_TAG_subrange_type,llvm::dwarf::DW_TAG_subrange_type)
	    .value(_sym_DW_TAG_with_stmt,llvm::dwarf::DW_TAG_with_stmt)
	    .value(_sym_DW_TAG_access_declaration,llvm::dwarf::DW_TAG_access_declaration)
	    .value(_sym_DW_TAG_base_type,llvm::dwarf::DW_TAG_base_type)
	    .value(_sym_DW_TAG_catch_block,llvm::dwarf::DW_TAG_catch_block)
	    .value(_sym_DW_TAG_const_type,llvm::dwarf::DW_TAG_const_type)
	    .value(_sym_DW_TAG_constant,llvm::dwarf::DW_TAG_constant)
	    .value(_sym_DW_TAG_enumerator,llvm::dwarf::DW_TAG_enumerator)
	    .value(_sym_DW_TAG_file_type,llvm::dwarf::DW_TAG_file_type)
	    .value(_sym_DW_TAG_friend,llvm::dwarf::DW_TAG_friend)
	    .value(_sym_DW_TAG_namelist,llvm::dwarf::DW_TAG_namelist)
	    .value(_sym_DW_TAG_namelist_item,llvm::dwarf::DW_TAG_namelist_item)
	    .value(_sym_DW_TAG_packed_type,llvm::dwarf::DW_TAG_packed_type)
	    .value(_sym_DW_TAG_subprogram,llvm::dwarf::DW_TAG_subprogram)
	    .value(_sym_DW_TAG_template_type_parameter,llvm::dwarf::DW_TAG_template_type_parameter)
	    .value(_sym_DW_TAG_template_value_parameter,llvm::dwarf::DW_TAG_template_value_parameter)
	    .value(_sym_DW_TAG_thrown_type,llvm::dwarf::DW_TAG_thrown_type)
	    .value(_sym_DW_TAG_try_block,llvm::dwarf::DW_TAG_try_block)
	    .value(_sym_DW_TAG_variant_part,llvm::dwarf::DW_TAG_variant_part)
	    .value(_sym_DW_TAG_variable,llvm::dwarf::DW_TAG_variable)
	    .value(_sym_DW_TAG_volatile_type,llvm::dwarf::DW_TAG_volatile_type)
	    .value(_sym_DW_TAG_dwarf_procedure,llvm::dwarf::DW_TAG_dwarf_procedure)
	    .value(_sym_DW_TAG_restrict_type,llvm::dwarf::DW_TAG_restrict_type)
	    .value(_sym_DW_TAG_interface_type,llvm::dwarf::DW_TAG_interface_type)
	    .value(_sym_DW_TAG_namespace,llvm::dwarf::DW_TAG_namespace)
	    .value(_sym_DW_TAG_imported_module,llvm::dwarf::DW_TAG_imported_module)
	    .value(_sym_DW_TAG_unspecified_type,llvm::dwarf::DW_TAG_unspecified_type)
	    .value(_sym_DW_TAG_partial_unit,llvm::dwarf::DW_TAG_partial_unit)
	    .value(_sym_DW_TAG_imported_unit,llvm::dwarf::DW_TAG_imported_unit)
	    .value(_sym_DW_TAG_condition,llvm::dwarf::DW_TAG_condition)
	    .value(_sym_DW_TAG_shared_type,llvm::dwarf::DW_TAG_shared_type)
	    .value(_sym_DW_TAG_type_unit,llvm::dwarf::DW_TAG_type_unit)
	    .value(_sym_DW_TAG_rvalue_reference_type,llvm::dwarf::DW_TAG_rvalue_reference_type)
	    .value(_sym_DW_TAG_template_alias,llvm::dwarf::DW_TAG_template_alias)
	    .value(_sym_DW_TAG_MIPS_loop,llvm::dwarf::DW_TAG_MIPS_loop)
	    .value(_sym_DW_TAG_format_label,llvm::dwarf::DW_TAG_format_label)
	    .value(_sym_DW_TAG_function_template,llvm::dwarf::DW_TAG_function_template)
	    .value(_sym_DW_TAG_class_template,llvm::dwarf::DW_TAG_class_template)
	    .value(_sym_DW_TAG_GNU_template_template_param,llvm::dwarf::DW_TAG_GNU_template_template_param)
	    .value(_sym_DW_TAG_GNU_template_parameter_pack,llvm::dwarf::DW_TAG_GNU_template_parameter_pack)
	    .value(_sym_DW_TAG_GNU_formal_parameter_pack,llvm::dwarf::DW_TAG_GNU_formal_parameter_pack)
	    .value(_sym_DW_TAG_lo_user,llvm::dwarf::DW_TAG_lo_user)
	    .value(_sym_DW_TAG_APPLE_property,llvm::dwarf::DW_TAG_APPLE_property)
	    .value(_sym_DW_TAG_hi_user,llvm::dwarf::DW_TAG_hi_user)
	    ;

	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_LANG_COMMON_LISP);
	_sym_DW_LANG_COMMON_LISP->defconstant(core::Fixnum_O::create(llvm::dwarf::DW_LANG_lo_user));

	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_LANG_C);
	_sym_DW_LANG_C->defconstant(core::Fixnum_O::create(llvm::dwarf::DW_LANG_C));

	SYMBOL_EXPORT_SC_(LlvmoPkg,DW_LANG_C_plus_plus);
	_sym_DW_LANG_C_plus_plus->defconstant(core::Fixnum_O::create(llvm::dwarf::DW_LANG_C_plus_plus));

	SYMBOL_EXPORT_SC_(LlvmoPkg,_PLUS_DW_ATE_SIGNED_FIXED_PLUS_);
	_sym__PLUS_DW_ATE_SIGNED_FIXED_PLUS_->defconstant(core::Fixnum_O::create(llvm::dwarf::DW_ATE_signed_fixed));

    };

};
