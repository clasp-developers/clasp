/*
    File: llvmoDwarf.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
//#define DEBUG_LEVEL_FULL
#include <clasp/core/foundation.h>

#include <llvm/IR/Metadata.h>
#include <llvm/BinaryFormat/Dwarf.h>

#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/llvmo/llvmoPackage.h>
#include <clasp/llvmo/llvmoDwarf.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/wrappers.h>
namespace llvmo {

CL_LAMBDA(tagsym &optional (debug-version (quote llvm-sys:llvmdebug-version8)));
CL_DOCSTRING("Convert a DW_TAG and version to an integer");
CL_NAME(dw-tag);
CL_DEFUN uint llvm_sys__dwTag(core::Symbol_sp tagsym, uint debugVersion) {
  core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(_sym_STARdwarfConstantsSTAR->symbolValue());
  uint itag = converter->enumIndexForSymbol(tagsym);
  uint dwtag = itag + debugVersion;
  return dwtag;
}

void initialize_dwarf_constants() {
  SYMBOL_EXPORT_SC_(LlvmoPkg, dwTag);
//  core::af_def(LlvmoPkg, "dwTag", &af_dwTag, ARGS_af_dwTag, DECL_af_dwTag, DOCS_af_dwTag);

  SYMBOL_EXPORT_SC_(LlvmoPkg, LLVMDebugVersion11);
  SYMBOL_EXPORT_SC_(LlvmoPkg, LLVMDebugVersion10);
  SYMBOL_EXPORT_SC_(LlvmoPkg, LLVMDebugVersion9);
  SYMBOL_EXPORT_SC_(LlvmoPkg, LLVMDebugVersion8);
  SYMBOL_EXPORT_SC_(LlvmoPkg, LLVMDebugVersion7);
  SYMBOL_EXPORT_SC_(LlvmoPkg, LLVMDebugVersion6);
  SYMBOL_EXPORT_SC_(LlvmoPkg, LLVMDebugVersion5);
  SYMBOL_EXPORT_SC_(LlvmoPkg, LLVMDebugVersion4);
  SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_DebugMetadataVersion_PLUS_);
  //	_sym_LLVMDebugVersion11->defconstant(core::make_fixnum(llvm::LLVMDebugVersion11));
  //_sym_LLVMDebugVersion10->defconstant(core::make_fixnum(llvm::LLVMDebugVersion10));
  // _sym_LLVMDebugVersion9->defconstant(core::make_fixnum(llvm::LLVMDebugVersion9));
  // _sym_LLVMDebugVersion8->defconstant(core::make_fixnum(llvm::LLVMDebugVersion8));
  // _sym_LLVMDebugVersion7->defconstant(core::make_fixnum(llvm::LLVMDebugVersion7));
  // _sym_LLVMDebugVersion6->defconstant(core::make_fixnum(llvm::LLVMDebugVersion6));
  // _sym_LLVMDebugVersion5->defconstant(core::make_fixnum(llvm::LLVMDebugVersion5));
  // _sym_LLVMDebugVersion4->defconstant(core::make_fixnum(llvm::LLVMDebugVersion4));
  _sym__PLUS_DebugMetadataVersion_PLUS_->defconstant(core::make_fixnum(llvm::DEBUG_METADATA_VERSION));

  //===----------------------------------------------------------------------===//
  // Dwarf constants as gleaned from the DWARF Debugging Information Format V.3
  // reference manual http://dwarf.freestandards.org .
  //

  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_array_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_class_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_entry_point);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_enumeration_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_formal_parameter);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_imported_declaration);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_label);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_lexical_block);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_member);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_pointer_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_reference_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_compile_unit);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_string_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_structure_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_subroutine_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_typedef);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_union_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_unspecified_parameters);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_variant);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_common_block);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_common_inclusion);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_inheritance);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_inlined_subroutine);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_module);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_ptr_to_member_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_set_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_subrange_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_with_stmt);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_access_declaration);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_base_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_catch_block);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_const_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_constant);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_enumerator);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_file_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_friend);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_namelist);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_namelist_item);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_packed_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_subprogram);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_template_type_parameter);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_template_value_parameter);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_thrown_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_try_block);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_variant_part);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_variable);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_volatile_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_dwarf_procedure);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_restrict_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_interface_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_namespace);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_imported_module);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_unspecified_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_partial_unit);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_imported_unit);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_condition);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_shared_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_type_unit);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_rvalue_reference_type);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_template_alias);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_MIPS_loop);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_format_label);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_function_template);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_class_template);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_GNU_template_template_param);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_GNU_template_parameter_pack);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_GNU_formal_parameter_pack);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_lo_user);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_APPLE_property);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_TAG_hi_user);

  // Tags defined in llvm->dwarf.h

  SYMBOL_EXPORT_SC_(LlvmoPkg, STARdwarfConstantsSTAR);
  CL_BEGIN_ENUM(llvm::dwarf::Tag,_sym_STARdwarfConstantsSTAR, "llvm::dwarf::Constants");
  CL_VALUE_ENUM(_sym_DW_TAG_array_type, llvm::dwarf::DW_TAG_array_type);
  CL_VALUE_ENUM(_sym_DW_TAG_class_type, llvm::dwarf::DW_TAG_class_type);
  CL_VALUE_ENUM(_sym_DW_TAG_entry_point, llvm::dwarf::DW_TAG_entry_point);
  CL_VALUE_ENUM(_sym_DW_TAG_enumeration_type, llvm::dwarf::DW_TAG_enumeration_type);
  CL_VALUE_ENUM(_sym_DW_TAG_formal_parameter, llvm::dwarf::DW_TAG_formal_parameter);
  CL_VALUE_ENUM(_sym_DW_TAG_imported_declaration, llvm::dwarf::DW_TAG_imported_declaration);
  CL_VALUE_ENUM(_sym_DW_TAG_label, llvm::dwarf::DW_TAG_label);
  CL_VALUE_ENUM(_sym_DW_TAG_lexical_block, llvm::dwarf::DW_TAG_lexical_block);
  CL_VALUE_ENUM(_sym_DW_TAG_member, llvm::dwarf::DW_TAG_member);
  CL_VALUE_ENUM(_sym_DW_TAG_pointer_type, llvm::dwarf::DW_TAG_pointer_type);
  CL_VALUE_ENUM(_sym_DW_TAG_reference_type, llvm::dwarf::DW_TAG_reference_type);
  CL_VALUE_ENUM(_sym_DW_TAG_compile_unit, llvm::dwarf::DW_TAG_compile_unit);
  CL_VALUE_ENUM(_sym_DW_TAG_string_type, llvm::dwarf::DW_TAG_string_type);
  CL_VALUE_ENUM(_sym_DW_TAG_structure_type, llvm::dwarf::DW_TAG_structure_type);
  CL_VALUE_ENUM(_sym_DW_TAG_subroutine_type, llvm::dwarf::DW_TAG_subroutine_type);
  CL_VALUE_ENUM(_sym_DW_TAG_typedef, llvm::dwarf::DW_TAG_typedef);
  CL_VALUE_ENUM(_sym_DW_TAG_union_type, llvm::dwarf::DW_TAG_union_type);
  CL_VALUE_ENUM(_sym_DW_TAG_unspecified_parameters, llvm::dwarf::DW_TAG_unspecified_parameters);
  CL_VALUE_ENUM(_sym_DW_TAG_variant, llvm::dwarf::DW_TAG_variant);
  CL_VALUE_ENUM(_sym_DW_TAG_common_block, llvm::dwarf::DW_TAG_common_block);
  CL_VALUE_ENUM(_sym_DW_TAG_common_inclusion, llvm::dwarf::DW_TAG_common_inclusion);
  CL_VALUE_ENUM(_sym_DW_TAG_inheritance, llvm::dwarf::DW_TAG_inheritance);
  CL_VALUE_ENUM(_sym_DW_TAG_inlined_subroutine, llvm::dwarf::DW_TAG_inlined_subroutine);
  CL_VALUE_ENUM(_sym_DW_TAG_module, llvm::dwarf::DW_TAG_module);
  CL_VALUE_ENUM(_sym_DW_TAG_ptr_to_member_type, llvm::dwarf::DW_TAG_ptr_to_member_type);
  CL_VALUE_ENUM(_sym_DW_TAG_set_type, llvm::dwarf::DW_TAG_set_type);
  CL_VALUE_ENUM(_sym_DW_TAG_subrange_type, llvm::dwarf::DW_TAG_subrange_type);
  CL_VALUE_ENUM(_sym_DW_TAG_with_stmt, llvm::dwarf::DW_TAG_with_stmt);
  CL_VALUE_ENUM(_sym_DW_TAG_access_declaration, llvm::dwarf::DW_TAG_access_declaration);
  CL_VALUE_ENUM(_sym_DW_TAG_base_type, llvm::dwarf::DW_TAG_base_type);
  CL_VALUE_ENUM(_sym_DW_TAG_catch_block, llvm::dwarf::DW_TAG_catch_block);
  CL_VALUE_ENUM(_sym_DW_TAG_const_type, llvm::dwarf::DW_TAG_const_type);
  CL_VALUE_ENUM(_sym_DW_TAG_constant, llvm::dwarf::DW_TAG_constant);
  CL_VALUE_ENUM(_sym_DW_TAG_enumerator, llvm::dwarf::DW_TAG_enumerator);
  CL_VALUE_ENUM(_sym_DW_TAG_file_type, llvm::dwarf::DW_TAG_file_type);
  CL_VALUE_ENUM(_sym_DW_TAG_friend, llvm::dwarf::DW_TAG_friend);
  CL_VALUE_ENUM(_sym_DW_TAG_namelist, llvm::dwarf::DW_TAG_namelist);
  CL_VALUE_ENUM(_sym_DW_TAG_namelist_item, llvm::dwarf::DW_TAG_namelist_item);
  CL_VALUE_ENUM(_sym_DW_TAG_packed_type, llvm::dwarf::DW_TAG_packed_type);
  CL_VALUE_ENUM(_sym_DW_TAG_subprogram, llvm::dwarf::DW_TAG_subprogram);
  CL_VALUE_ENUM(_sym_DW_TAG_template_type_parameter, llvm::dwarf::DW_TAG_template_type_parameter);
  CL_VALUE_ENUM(_sym_DW_TAG_template_value_parameter, llvm::dwarf::DW_TAG_template_value_parameter);
  CL_VALUE_ENUM(_sym_DW_TAG_thrown_type, llvm::dwarf::DW_TAG_thrown_type);
  CL_VALUE_ENUM(_sym_DW_TAG_try_block, llvm::dwarf::DW_TAG_try_block);
  CL_VALUE_ENUM(_sym_DW_TAG_variant_part, llvm::dwarf::DW_TAG_variant_part);
  CL_VALUE_ENUM(_sym_DW_TAG_variable, llvm::dwarf::DW_TAG_variable);
  CL_VALUE_ENUM(_sym_DW_TAG_volatile_type, llvm::dwarf::DW_TAG_volatile_type);
  CL_VALUE_ENUM(_sym_DW_TAG_dwarf_procedure, llvm::dwarf::DW_TAG_dwarf_procedure);
  CL_VALUE_ENUM(_sym_DW_TAG_restrict_type, llvm::dwarf::DW_TAG_restrict_type);
  CL_VALUE_ENUM(_sym_DW_TAG_interface_type, llvm::dwarf::DW_TAG_interface_type);
  CL_VALUE_ENUM(_sym_DW_TAG_namespace, llvm::dwarf::DW_TAG_namespace);
  CL_VALUE_ENUM(_sym_DW_TAG_imported_module, llvm::dwarf::DW_TAG_imported_module);
  CL_VALUE_ENUM(_sym_DW_TAG_unspecified_type, llvm::dwarf::DW_TAG_unspecified_type);
  CL_VALUE_ENUM(_sym_DW_TAG_partial_unit, llvm::dwarf::DW_TAG_partial_unit);
  CL_VALUE_ENUM(_sym_DW_TAG_imported_unit, llvm::dwarf::DW_TAG_imported_unit);
  CL_VALUE_ENUM(_sym_DW_TAG_condition, llvm::dwarf::DW_TAG_condition);
  CL_VALUE_ENUM(_sym_DW_TAG_shared_type, llvm::dwarf::DW_TAG_shared_type);
  CL_VALUE_ENUM(_sym_DW_TAG_type_unit, llvm::dwarf::DW_TAG_type_unit);
  CL_VALUE_ENUM(_sym_DW_TAG_rvalue_reference_type, llvm::dwarf::DW_TAG_rvalue_reference_type);
  CL_VALUE_ENUM(_sym_DW_TAG_template_alias, llvm::dwarf::DW_TAG_template_alias);
  CL_VALUE_ENUM(_sym_DW_TAG_MIPS_loop, llvm::dwarf::DW_TAG_MIPS_loop);
  CL_VALUE_ENUM(_sym_DW_TAG_format_label, llvm::dwarf::DW_TAG_format_label);
  CL_VALUE_ENUM(_sym_DW_TAG_function_template, llvm::dwarf::DW_TAG_function_template);
  CL_VALUE_ENUM(_sym_DW_TAG_class_template, llvm::dwarf::DW_TAG_class_template);
  CL_VALUE_ENUM(_sym_DW_TAG_GNU_template_template_param, llvm::dwarf::DW_TAG_GNU_template_template_param);
  CL_VALUE_ENUM(_sym_DW_TAG_GNU_template_parameter_pack, llvm::dwarf::DW_TAG_GNU_template_parameter_pack);
  CL_VALUE_ENUM(_sym_DW_TAG_GNU_formal_parameter_pack, llvm::dwarf::DW_TAG_GNU_formal_parameter_pack);
  CL_VALUE_ENUM(_sym_DW_TAG_lo_user, llvm::dwarf::DW_TAG_lo_user);
  CL_VALUE_ENUM(_sym_DW_TAG_APPLE_property, llvm::dwarf::DW_TAG_APPLE_property);
  CL_VALUE_ENUM(_sym_DW_TAG_hi_user, llvm::dwarf::DW_TAG_hi_user);;
  CL_END_ENUM(_sym_STARdwarfConstantsSTAR);
  
  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_LANG_COMMON_LISP);
  _sym_DW_LANG_COMMON_LISP->defconstant(core::make_fixnum(llvm::dwarf::DW_LANG_lo_user));

  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_LANG_C);
  _sym_DW_LANG_C->defconstant(core::make_fixnum(llvm::dwarf::DW_LANG_C));

  SYMBOL_EXPORT_SC_(LlvmoPkg, DW_LANG_C_plus_plus);
  _sym_DW_LANG_C_plus_plus->defconstant(core::make_fixnum(llvm::dwarf::DW_LANG_C_plus_plus));

  SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_DW_ATE_SIGNED_FIXED_PLUS_);
  _sym__PLUS_DW_ATE_SIGNED_FIXED_PLUS_->defconstant(core::make_fixnum(llvm::dwarf::DW_ATE_signed_fixed));
  SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_DW_ATE_ADDRESS_PLUS_);
  _sym__PLUS_DW_ATE_ADDRESS_PLUS_->defconstant(core::make_fixnum(llvm::dwarf::DW_ATE_address));
  SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_DW_ATE_boolean_PLUS_);
  _sym__PLUS_DW_ATE_boolean_PLUS_->defconstant(core::make_fixnum(llvm::dwarf::DW_ATE_boolean));
  SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_DW_ATE_complex_float_PLUS_);
  _sym__PLUS_DW_ATE_complex_float_PLUS_->defconstant(core::make_fixnum(llvm::dwarf::DW_ATE_complex_float));
  SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_DW_ATE_float_PLUS_);
  _sym__PLUS_DW_ATE_float_PLUS_->defconstant(core::make_fixnum(llvm::dwarf::DW_ATE_float));
  SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_DW_ATE_signed_PLUS_);
  _sym__PLUS_DW_ATE_signed_PLUS_->defconstant(core::make_fixnum(llvm::dwarf::DW_ATE_signed));
  SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_DW_ATE_signed_char_PLUS_);
  _sym__PLUS_DW_ATE_signed_char_PLUS_->defconstant(core::make_fixnum(llvm::dwarf::DW_ATE_signed_char));
  SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_DW_ATE_unsigned_PLUS_);
  _sym__PLUS_DW_ATE_unsigned_PLUS_->defconstant(core::make_fixnum(llvm::dwarf::DW_ATE_unsigned));
  SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_DW_ATE_unsigned_char_PLUS_);
  _sym__PLUS_DW_ATE_unsigned_char_PLUS_->defconstant(core::make_fixnum(llvm::dwarf::DW_ATE_unsigned_char));
  SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_DW_ATE_imaginary_float_PLUS_);
  _sym__PLUS_DW_ATE_imaginary_float_PLUS_->defconstant(core::make_fixnum(llvm::dwarf::DW_ATE_imaginary_float));
  SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_DW_ATE_packed_decimal_PLUS_);
  _sym__PLUS_DW_ATE_packed_decimal_PLUS_->defconstant(core::make_fixnum(llvm::dwarf::DW_ATE_packed_decimal));
  SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_DW_ATE_numeric_string_PLUS_);
  _sym__PLUS_DW_ATE_numeric_string_PLUS_->defconstant(core::make_fixnum(llvm::dwarf::DW_ATE_numeric_string));
  SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_DW_ATE_edited_PLUS_);
  _sym__PLUS_DW_ATE_edited_PLUS_->defconstant(core::make_fixnum(llvm::dwarf::DW_ATE_edited));
  SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_DW_ATE_unsigned_fixed_PLUS_);
  _sym__PLUS_DW_ATE_unsigned_fixed_PLUS_->defconstant(core::make_fixnum(llvm::dwarf::DW_ATE_unsigned_fixed));
  SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_DW_ATE_decimal_float_PLUS_);
  _sym__PLUS_DW_ATE_decimal_float_PLUS_->defconstant(core::make_fixnum(llvm::dwarf::DW_ATE_decimal_float));
  SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_DW_ATE_UTF_PLUS_);
  _sym__PLUS_DW_ATE_UTF_PLUS_->defconstant(core::make_fixnum(llvm::dwarf::DW_ATE_UTF));
    
};
};
