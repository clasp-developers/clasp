/*
    File: link_intrinsics.cc
    Small functions used by the runtime that should NOT be inlined.
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
#define DEBUG_LANDING_PAD 1

//#define DEBUG_LEVEL_FULL
#ifdef USE_MPS
extern "C" {
#include <clasp/mps/code/mps.h>
};
#endif
#include <dlfcn.h>
#include <typeinfo>
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/commandLineOptions.h>
#include <clasp/core/bignum.h>
#include <clasp/core/functor.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/core/character.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/array.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/lispList.h>
#include <clasp/core/bformat.h>
#include <clasp/core/instance.h>
#include <clasp/core/arguments.h>
#include <clasp/core/designators.h>
#include <clasp/core/compPackage.h>
#include <clasp/core/package.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/stacks.h>
#include <clasp/core/compiler.h>
#include <clasp/core/debugger.h>
#include <clasp/core/random.h>
#include <clasp/core/primitives.h>
#include <clasp/core/posixTime.h>
#include <clasp/core/numbers.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/symbolTable.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/intrinsics.h>
#include <clasp/gctools/gc_interface.fwd.h>
#include <clasp/core/exceptions.h>

#if defined(_TARGET_OS_DARWIN)
#include <mach-o/ldsyms.h>
#include <mach-o/getsect.h>
#endif
#if defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_FREEBSD)
#include <link.h>
#endif

#pragma GCC visibility push(default)

using namespace core;
namespace llvmo {

core::T_sp functionNameOrNilFromFunctionDescription(core::FunctionDescription* functionDescription)
{
  if (functionDescription==NULL) {
    return _Nil<core::T_O>();
  }
  core::Cons_sp sourcePosition_functionName((gctools::Tagged)functionDescription->gcrootsInModule->getTaggedIndex(LITERAL_TAG_CHAR,functionDescription->sourcePathname_functionName_Index));
  return CONS_CDR(sourcePosition_functionName);
}
  

[[noreturn]] __attribute__((optnone)) void not_function_designator_error(core::T_sp arg) {
  TYPE_ERROR(arg,core::Cons_O::createList(::cl::_sym_or,::cl::_sym_function,::cl::_sym_symbol));
}

[[noreturn]] __attribute__((optnone))  void intrinsic_error(ErrorCode err, core::T_sp arg0, core::T_sp arg1, core::T_sp arg2) {
  switch (err) {
  case noFunctionBoundToSymbol:
    {
      core::Symbol_sp sym(gc::As<core::Symbol_sp>(arg0));
      ERROR_UNDEFINED_FUNCTION(sym); 
    }
    break;
  case couldNotCoerceToClosure:
      SIMPLE_ERROR(BF(" symbol %s") % _rep_(arg0));
  case destinationMustBeActivationFrame:
      SIMPLE_ERROR(BF("Destination must be ActivationFrame"));
  case invalidIndexForFunctionFrame:
      SIMPLE_ERROR(BF("Invalid index[%d] for FunctionFrame(size=%d)") % _rep_(arg0) % _rep_(arg1));
  case unboundSymbolValue:
    {
      core::Symbol_sp sym = gc::As<core::Symbol_sp>(arg0);
      UNBOUND_VARIABLE_ERROR(sym);
    };
  case unboundSymbolFunction:
    {
      core::Symbol_sp sym = gc::As<core::Symbol_sp>(arg0);
      ERROR_UNDEFINED_FUNCTION(sym);
    }
  case unboundSymbolSetfFunction:
    {
      core::Symbol_sp sym = gc::As<core::Symbol_sp>(arg0);
      SIMPLE_ERROR(BF("The symbol %s has no setf function bound to it") % sym->fullName() );
    }
  case badCell:
    {
      SIMPLE_ERROR(BF("The object with pointer %p is not a cell") % arg0.raw_());
    }
  case slot_reader_problem: {
    SIMPLE_ERROR(BF("TRAPPED SLOT READER PROBLEM!!!   The slot accessor efm returned %s and the direct slot read at %d returned %s") % _rep_(arg0) % _rep_(arg1) % _rep_(arg2));
  }
  case slot_writer_problem: {
    SIMPLE_ERROR(BF("TRAPPED SLOT WRITER PROBLEM!!!   The expected slot at %d value %s was unchanged!  The value should be %s") % _rep_(arg0) % _rep_(arg1) % _rep_(arg2) );
  }
  default:
      SIMPLE_ERROR(BF("An intrinsicError %d was signaled and there needs to be a more descriptive error message for it in gctools::intrinsic_error arg0: %s arg1: %s arg2: %s") % err % _rep_(arg0) % _rep_(arg1) % _rep_(arg2));
  };
};

};

namespace llvmo {
core::T_sp intrinsic_slot_unbound(core::T_sp slot_info, core::T_sp instance ) {
  return core::eval::funcall(clos::_sym_fastgf_slot_unbound,slot_info,instance);
};
};



extern "C" {

void cc_initialize_gcroots_in_module(gctools::GCRootsInModule* holder,
                                     core::T_O** root_address,
                                     size_t num_roots,
                                     gctools::Tagged initial_data,
                                     SimpleVector_O** transientAlloca,
                                     size_t transient_entries,
                                     size_t function_pointer_count,
                                     fnLispCallingConvention* fptrs,
                                     void** fdescs )
{NO_UNWIND_BEGIN();
  initialize_gcroots_in_module(holder,root_address,num_roots,initial_data,transientAlloca,transient_entries, function_pointer_count, (void**)fptrs, fdescs );
  NO_UNWIND_END();
}

void cc_finish_gcroots_in_module(gctools::GCRootsInModule* holder)
{NO_UNWIND_BEGIN();
  holder->_TransientAlloca = NULL;
  NO_UNWIND_END();
}

void cc_remove_gcroots_in_module(gctools::GCRootsInModule* holder)
{NO_UNWIND_BEGIN();
  shutdown_gcroots_in_module(holder);
  NO_UNWIND_END();
}


void ltvc_assign_source_file_info_handle(const char *moduleName, const char *sourceDebugPathname, size_t sourceDebugOffset, int useLineno, int *sourceFileInfoHandleP) {
  NO_UNWIND_BEGIN();
  //	printf("%s:%d assignSourceFileInfoHandle %s\n", __FILE__, __LINE__, moduleName );
  core::SimpleBaseString_sp mname = core::SimpleBaseString_O::make(moduleName);
  core::SimpleBaseString_sp struename = core::SimpleBaseString_O::make(sourceDebugPathname);
  T_mv sfi_mv = core::core__source_file_info(mname, struename, sourceDebugOffset, useLineno ? true : false);
  SourceFileInfo_sp sfi = gc::As<SourceFileInfo_sp>(sfi_mv);
  int sfindex = unbox_fixnum(gc::As<core::Fixnum_sp>(sfi_mv.valueGet_(1)));
  *sourceFileInfoHandleP = sfindex;
  NO_UNWIND_END();
}

// Define what ltvc_xxxx functions return - this must match what is
//  in cmpintrinsics.lsp
typedef void LtvcReturn;
#define LTVCRETURN /* Nothing return for void */

LtvcReturn ltvc_make_nil(gctools::GCRootsInModule* holder, char tag, size_t index)
{
  NO_UNWIND_BEGIN();
  core::T_sp val = _Nil<core::T_O>();
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_t(gctools::GCRootsInModule* holder, char tag, size_t index)
{
  NO_UNWIND_BEGIN();
  core::T_sp val = _lisp->_true();
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
  NO_UNWIND_END();
}


T_O* ltvc_lookup_transient(gctools::GCRootsInModule* holder, char tag, size_t index)
{
  NO_UNWIND_BEGIN();
  return (T_O*)holder->getTransient(index);
  NO_UNWIND_END();
}


LtvcReturn ltvc_make_ratio(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* num, core::T_O* denom )
{NO_UNWIND_BEGIN();
  Integer_sp inum((gc::Tagged)num);
  Integer_sp idenom((gc::Tagged)denom);
  core::T_sp val = core::Ratio_O::create(inum,idenom);
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_complex(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* real, core::T_O* imag)
{NO_UNWIND_BEGIN();
  core::Real_sp nreal((gctools::Tagged)real);
  core::Real_sp nimag((gctools::Tagged)imag);
  // Do not convert nreal and nimag to double, can be all types of Real_sp
  core::T_sp val = core::Complex_O::create(nreal,nimag);
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
  NO_UNWIND_END();
}


LtvcReturn ltvc_make_cons(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* car, core::T_O* cdr)
{NO_UNWIND_BEGIN();
  core::T_sp val = core::Cons_O::create(core::T_sp(car),core::T_sp(cdr));
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_nconc(gctools::GCRootsInModule* holder, char tag, size_t index,
                               core::T_O* front, core::T_O* back)
{NO_UNWIND_BEGIN();
  core::T_sp val = core::clasp_nconc(core::T_sp(front),core::T_sp(back));
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
  NO_UNWIND_END();
}

NOINLINE LtvcReturn ltvc_make_list(gctools::GCRootsInModule* holder, char tag, size_t index, size_t num, ... )
{NO_UNWIND_BEGIN();
  core::T_sp first;
  core::T_sp* cur = &first;
  va_list va;
  va_start(va,num);
  for (; num; --num) {
    gctools::Tagged p = va_arg(va, gctools::Tagged);
    Cons_sp one = Cons_O::create(core::T_sp(p),_Nil<core::T_O>());
    *cur = one;
    cur = &one->_Cdr;
  }
  va_end(va);
  core::T_sp val = first;
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_array(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* telement_type, core::T_O* tdimensions )
{NO_UNWIND_BEGIN();
  core::T_sp element_type(telement_type);
  core::List_sp dimensions((gctools::Tagged)tdimensions);
  core::T_sp val;
  if (core::cl__length(dimensions) == 1) // vector
  {
    val = core::core__make_vector(element_type, oCar(dimensions).unsafe_fixnum());
  } else {
    val = core::core__make_mdarray(dimensions, element_type);
  }
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
  NO_UNWIND_END();
}

void ltvc_setf_row_major_aref(gctools::GCRootsInModule* holder, core::T_O* array_t, size_t row_major_index, core::T_O* value_t )
{NO_UNWIND_BEGIN();
  core::T_sp tarray((gctools::Tagged)array_t);
  core::Array_sp array = gc::As<core::Array_sp>(tarray);
  array->rowMajorAset(row_major_index,core::T_sp(value_t));
  NO_UNWIND_END();
}
  
LtvcReturn ltvc_make_hash_table(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* test_t )
{NO_UNWIND_BEGIN();
  LTVCRETURN holder->setTaggedIndex(tag,index,core::HashTable_O::create(core::T_sp(test_t)).tagged_());
  NO_UNWIND_END();
}

void ltvc_setf_gethash(gctools::GCRootsInModule* holder, core::T_O* hash_table_t, core::T_O* key_index_t, core::T_O* value_index_t )
{NO_UNWIND_BEGIN();
  core::HashTable_sp hash_table = gctools::As<HashTable_sp>(core::T_sp(hash_table_t));
  core::T_sp key(key_index_t);
  core::T_sp value(value_index_t);
  hash_table->hash_table_setf_gethash(key, value);
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_fixnum(gctools::GCRootsInModule* holder, char tag, size_t index, int64_t val)
{NO_UNWIND_BEGIN();
  core::T_sp v = clasp_make_fixnum(val);
  LTVCRETURN holder->setTaggedIndex(tag,index,v.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_bignum(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* bignum_string_t)
{NO_UNWIND_BEGIN();
  core::SimpleBaseString_sp bignum_string = gctools::As<core::SimpleBaseString_sp>(core::T_sp(bignum_string_t));
  core::T_sp val = core::Bignum_O::make(bignum_string->get());
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_bitvector(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* bitvector_string_t)
{NO_UNWIND_BEGIN();
  core::SimpleBaseString_sp bitvector_string = gctools::As<core::SimpleBaseString_sp>(core::T_sp(bitvector_string_t));
  core::T_sp val = core::SimpleBitVector_O::make(bitvector_string->get());
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_symbol(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* name_t, core::T_O* package_t )
{NO_UNWIND_BEGIN();
  core::T_sp package((gctools::Tagged)package_t);
  core::SimpleString_sp symbol_name((gctools::Tagged)name_t);
  core::Symbol_sp sym;
  if (package.notnilp()) {
    sym = gctools::As<Package_sp>(package)->intern(symbol_name);
  } else {
    sym = core::Symbol_O::create(symbol_name);
  }
  core::T_sp val = sym;
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_character(gctools::GCRootsInModule* holder, char tag, size_t index, uintptr_clasp_t val)
{NO_UNWIND_BEGIN();
  core::T_sp v = clasp_make_character(val);
  LTVCRETURN holder->setTaggedIndex(tag,index,v.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_base_string(gctools::GCRootsInModule* holder, char tag, size_t index, const char* str) \
{NO_UNWIND_BEGIN();
  core::T_sp v = core::SimpleBaseString_O::make(str);
  LTVCRETURN holder->setTaggedIndex(tag,index,v.tagged_());
  NO_UNWIND_END();
}
};

extern "C" {


LtvcReturn ltvc_make_pathname(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* host_t, core::T_O* device_t, core::T_O* directory_t, core::T_O* name_t, core::T_O* type_t, core::T_O* version_t )
{NO_UNWIND_BEGIN();
  core::T_sp val = core::Pathname_O::makePathname(core::T_sp(host_t),
                                        core::T_sp(device_t),
                                        core::T_sp(directory_t),
                                        core::T_sp(name_t),
                                        core::T_sp(type_t),
                                        core::T_sp(version_t),
                                        kw::_sym_local,
                                              core::T_sp(host_t).notnilp());
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_package(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* package_name_t )
{
  NO_UNWIND_BEGIN();
  core::SimpleBaseString_sp package_name((gctools::Tagged)package_name_t);
  core::T_sp tpkg = _lisp->findPackage(package_name->get(),false);
  if ( tpkg.nilp() ) {
    // If we don't find the package - just make it
    // a more comprehensive defpackage should be coming
    tpkg = _lisp->makePackage(package_name->get(),std::list<std::string>(), std::list<std::string>());
  }
  core::T_sp val = tpkg;
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_random_state(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* random_state_string_t)
{NO_UNWIND_BEGIN();
  core::SimpleBaseString_sp random_state_string((gctools::Tagged)random_state_string_t);
  core::RandomState_sp rs = core::RandomState_O::create();
  rs->random_state_set(random_state_string->get());
  core::T_sp val = rs;
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
  NO_UNWIND_END();
}


LtvcReturn ltvc_find_class(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* class_name_t )
{
  core::Symbol_sp class_name((gctools::Tagged)class_name_t);
  core::T_sp cl = core::cl__find_class(class_name, true, _Nil<core::T_O>());
  LTVCRETURN holder->setTaggedIndex(tag,index,cl.tagged_());
}


LtvcReturn ltvc_make_float(gctools::GCRootsInModule* holder, char tag, size_t index, float f)
{NO_UNWIND_BEGIN();
  core::T_sp val = clasp_make_single_float(f);
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_double(gctools::GCRootsInModule* holder, char tag, size_t index, double f)
{NO_UNWIND_BEGIN();
  core::T_sp val = clasp_make_double_float(f);
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
  NO_UNWIND_END();
}

gctools::Tagged ltvc_lookup_literal( gctools::GCRootsInModule* holder, size_t index) {
  return holder->getTaggedIndex(LITERAL_TAG_CHAR,index);
}

LtvcReturn ltvc_enclose(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* lambdaName, size_t function_index)
{NO_UNWIND_BEGIN();
  core::T_sp tlambdaName = gctools::smart_ptr<core::T_O>((gc::Tagged)lambdaName);
  fnLispCallingConvention llvm_func = (fnLispCallingConvention)holder->lookup_function(function_index);
  void* functionDescription = holder->lookup_function_description(function_index);
  gctools::smart_ptr<core::ClosureWithSlots_O> functoid =
    gctools::GC<core::ClosureWithSlots_O>::allocate_container(false,0,
                                                              llvm_func,
                                                              (core::FunctionDescription*)functionDescription,
                                                              core::ClosureWithSlots_O::cclaspClosure);
  LTVCRETURN holder->setTaggedIndex(tag,index, functoid.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_allocate_instance(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* klass) {
  core::T_sp myklass((gctools::Tagged)klass);
  core::T_sp object = core::eval::funcall(cl::_sym_allocate_instance, myklass);
  LTVCRETURN holder->setTaggedIndex(tag,index, object.tagged_());
}

LtvcReturn ltvc_set_mlf_creator_funcall(gctools::GCRootsInModule* holder, char tag, size_t index, size_t fptr_index, const char* name) {
  fnLispCallingConvention fptr = (fnLispCallingConvention)holder->lookup_function(fptr_index);
  core::T_O *lcc_arglist = _Nil<core::T_O>().raw_();
  Symbol_sp sname = Symbol_O::create_from_string(std::string(name));
  core::ClosureWithSlots_sp toplevel_closure = core::ClosureWithSlots_O::make_bclasp_closure(sname, fptr, kw::_sym_function, _Nil<core::T_O>(),  _Nil<core::T_O>() );
  LCC_RETURN ret = fptr(LCC_PASS_ARGS0_VA_LIST(toplevel_closure.raw_()));
  core::T_sp res((gctools::Tagged)ret.ret0[0]);
  core::T_sp val = res;
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
}

LtvcReturn ltvc_mlf_init_funcall(gctools::GCRootsInModule* holder, size_t fptr_index, const char* name) {
  fnLispCallingConvention fptr = (fnLispCallingConvention)holder->lookup_function(fptr_index);
  core::T_O *lcc_arglist = _Nil<core::T_O>().raw_();
  Symbol_sp sname = Symbol_O::create_from_string(std::string(name));
  core::ClosureWithSlots_sp toplevel_closure = core::ClosureWithSlots_O::make_bclasp_closure(sname, fptr, kw::_sym_function, _Nil<core::T_O>(), _Nil<core::T_O>());
  LCC_RETURN ret = fptr(LCC_PASS_ARGS0_VA_LIST(toplevel_closure.raw_()));
//  LTVCRETURN reinterpret_cast<gctools::Tagged>(ret.ret0[0]);
}

// This is exactly like the one above - is it necessary?
LtvcReturn ltvc_set_ltv_funcall(gctools::GCRootsInModule* holder, char tag, size_t index, size_t fptr_index, const char* name) {\
  fnLispCallingConvention fptr = (fnLispCallingConvention)holder->lookup_function(fptr_index);
  core::T_O *lcc_arglist = _Nil<core::T_O>().raw_();
  Symbol_sp sname = Symbol_O::create_from_string(std::string(name));
  core::ClosureWithSlots_sp toplevel_closure = core::ClosureWithSlots_O::make_bclasp_closure(sname, fptr, kw::_sym_function, _Nil<core::T_O>(), _Nil<core::T_O>());
  LCC_RETURN ret = fptr(LCC_PASS_ARGS0_VA_LIST(toplevel_closure.raw_()));
  core::T_sp res((gctools::Tagged)ret.ret0[0]);
  core::T_sp val = res;
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
}

LtvcReturn ltvc_set_ltv_funcall_cleavir(gctools::GCRootsInModule* holder, char tag, size_t index, size_t fptr_index , const char* name) {
  // I created this function just in case cleavir returns a function that when evaluated returns a function that
  // would return the ltv value - that appears not to be the case.
  // FIXME: Remove this function and use the ltvc_set_ltv_funcall instead
  fnLispCallingConvention fptr = (fnLispCallingConvention)holder->lookup_function(fptr_index);
  core::T_O *lcc_arglist = _Nil<core::T_O>().raw_();
  Symbol_sp sname = Symbol_O::create_from_string(std::string(name));
  core::ClosureWithSlots_sp toplevel_closure = core::ClosureWithSlots_O::make_bclasp_closure(sname, fptr, kw::_sym_function, _Nil<core::T_O>(), _Nil<core::T_O>());
  LCC_RETURN ret = fptr(LCC_PASS_ARGS0_VA_LIST(toplevel_closure.raw_()));
  core::T_sp tret((gctools::Tagged)ret.ret0);
//  printf("%s:%d:%s     ret -> %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(tret).c_str());
  core::T_sp res((gctools::Tagged)ret.ret0[0]);
  core::T_sp val = res;
  LTVCRETURN holder->setTaggedIndex(tag,index,val.tagged_());
}



LtvcReturn ltvc_toplevel_funcall(gctools::GCRootsInModule* holder, size_t fptr_index, const char* name) {
  fnLispCallingConvention fptr = (fnLispCallingConvention)holder->lookup_function(fptr_index);  
#ifdef DEBUG_SLOW
  MaybeDebugStartup startup((void*)fptr,name);
#endif
  core::T_O *lcc_arglist = _Nil<core::T_O>().raw_();
  Symbol_sp sname = Symbol_O::create_from_string(std::string(name));
  core::ClosureWithSlots_sp toplevel_closure = core::ClosureWithSlots_O::make_bclasp_closure(sname, fptr, kw::_sym_function, _Nil<core::T_O>(), _Nil<core::T_O>());
  LCC_RETURN ret = fptr(LCC_PASS_ARGS0_VA_LIST(toplevel_closure.raw_()));
//  LTVCRETURN reinterpret_cast<gctools::Tagged>(ret.ret0[0]);
}

};

extern "C" {

LispCallingConventionPtr lccGlobalFunction(core::Symbol_sp sym) {
  printf("%s:%d lccSymbolFunction for %s returning NULL for now\n", __FILE__, __LINE__, _rep_(sym).c_str());
  return NULL;
}
};

extern "C" {

const std::type_info &typeidCoreCatchThrow = typeid(core::CatchThrow);
const std::type_info &typeidCoreLexicalGo = typeid(core::LexicalGo);
const std::type_info &typeidCoreDynamicGo = typeid(core::DynamicGo);
const std::type_info &typeidCoreReturnFrom = typeid(core::ReturnFrom);
const std::type_info &typeidCoreUnwind = typeid(core::Unwind);

#define LOW_LEVEL_TRACE_QUEUE_SIZE 1024
uint _LLVMLowLevelTraceQueueIn = 0;
uint _LLVMLowLevelTraceQueueWrapped = false;
uint _LLVMLowLevelTraceQueue[LOW_LEVEL_TRACE_QUEUE_SIZE];

NOINLINE void lowLevelTrace(uint traceid)
{NO_UNWIND_BEGIN();
  if (comp::_sym_STARlowLevelTracePrintSTAR->symbolValue().isTrue()) {
    printf("+++ lowLevelTrace[%d]\n", traceid);
    if (traceid == 1000115396) {
      printf("%s:%d Set a breakpoint here\n", __FILE__, __LINE__);
    }
  }
  _LLVMLowLevelTraceQueue[_LLVMLowLevelTraceQueueIn] = traceid;
  ++_LLVMLowLevelTraceQueueIn;
  if (_LLVMLowLevelTraceQueueIn >= LOW_LEVEL_TRACE_QUEUE_SIZE) {
    _LLVMLowLevelTraceQueueIn = 0;
    _LLVMLowLevelTraceQueueWrapped = true;
  }
  NO_UNWIND_END();
}

void unreachableError()
{NO_UNWIND_BEGIN();
  printf("%s:%d In unreachableError -  Hit an unreachable block\n",
         __FILE__, __LINE__);
  NO_UNWIND_END();
}


void dumpLowLevelTrace(int numLowLevels) {
  int cur = _LLVMLowLevelTraceQueueIn;
  for (int i = 0; i < numLowLevels; i++) {
    --cur;
    if (cur < 0) {
      if (_LLVMLowLevelTraceQueueWrapped) {
        cur = LOW_LEVEL_TRACE_QUEUE_SIZE - 1;
      } else {
        printf("-----Ran out of block trace entries----\n");
        break;
      }
    }
    printf("LowLevel-trace#%d -> %u\n", -i, _LLVMLowLevelTraceQueue[cur]);
  }
}
};

extern "C" {

NOINLINE void va_tooManyArgumentsException(const char *funcName, std::size_t givenNumberOfArguments, std::size_t requiredNumberOfArguments) {
  SIMPLE_ERROR(BF("Too many arguments for %s - got %d and expected %d") % funcName % givenNumberOfArguments % requiredNumberOfArguments);
}

NOINLINE void va_notEnoughArgumentsException(const char *funcName, std::size_t givenNumberOfArguments, std::size_t requiredNumberOfArguments) {
  SIMPLE_ERROR(BF("Too few arguments for %s - got %d and expected %d") % funcName % givenNumberOfArguments % requiredNumberOfArguments);
}

NOINLINE extern void va_ifExcessKeywordArgumentsException(char *fnName, std::size_t nargs, Vaslist *varglist, size_t argIdx) {
  if (argIdx >= nargs)
    return;
  Vaslist *vl = reinterpret_cast<Vaslist *>(gc::untag_vaslist((void *)varglist));
  va_list vrest;
  va_copy(vrest, vl->_Args);
  stringstream ss;
  for (int i(argIdx); i < nargs; ++i) {
    T_sp obj(va_arg(vrest, T_O *));
    ss << _rep_(obj).c_str() << " ";
  }
  SIMPLE_ERROR(BF("va_ifExcessKeywordArgumentsException>> Excess keyword arguments fnName: %s nargs: %d argIdx: %d  args: %s") % fnName % nargs % argIdx % ss.str());
  //        core::throwUnrecognizedKeywordArgumentError(argArray[argIdx]);
}



ALWAYS_INLINE T_O *va_lexicalFunction(size_t depth, size_t index, core::T_O* evaluateFrameP)
{NO_UNWIND_BEGIN();
  core::ActivationFrame_sp af((gctools::Tagged)evaluateFrameP);
  core::Function_sp func = gc::As_unsafe<core::Function_sp>(core::function_frame_lookup(af, depth, index));
  return func.raw_();
  NO_UNWIND_END();
}

/* Conses up a &rest argument from the passed valist.
 * Used in cmp/arguments.lsp for the general case of functions with a &rest in their lambda list. */
__attribute__((visibility("default"))) core::T_O *cc_gatherRestArguments(va_list vargs, std::size_t nargs)
{NO_UNWIND_BEGIN();
  va_list rargs;
  va_copy(rargs, vargs); // the original valist is needed for &key processing elsewhere.
  core::List_sp result = _Nil<core::T_O>();
  core::Cons_sp *cur = reinterpret_cast<Cons_sp *>(&result);
  for (int i = 0; i<nargs; ++i ) {
    core::T_O* tagged_obj = ENSURE_VALID_OBJECT(va_arg(rargs,core::T_O*));
    *cur = core::Cons_O::create(gc::smart_ptr<core::T_O>((gc::Tagged)tagged_obj), _Nil<core::T_O>());
    cur = reinterpret_cast<Cons_sp *>(&(*cur)->_Cdr);
  }
  va_end(rargs);
  return result.raw_();
  NO_UNWIND_END();
}

/* Like cc_gatherRestArguments, but uses a vector of conses provided by the caller-
 * intended to be stack space, for &rest parameters declared dynamic-extent. */
__attribute__((visibility("default"))) core::T_O *cc_gatherDynamicExtentRestArguments(va_list vargs, std::size_t nargs, core::Cons_O* cur)
{NO_UNWIND_BEGIN();
  va_list rargs;
  va_copy(rargs, vargs);
  core::List_sp result = Cons_sp((gctools::Tagged)gctools::tag_cons((core::Cons_O*)cur));
  if (nargs) {
    for (int i = 0; i<nargs-1; ++i ) {
      core::T_O* tagged_obj = ENSURE_VALID_OBJECT(va_arg(rargs,core::T_O*));
      Cons_O* next = cur+1;
      new (cur) Cons_O(T_sp((gctools::Tagged)tagged_obj),T_sp((gctools::Tagged)gctools::tag_cons((core::Cons_O*)next)));
      cur = next;
    }
    core::T_O* tagged_obj = ENSURE_VALID_OBJECT(va_arg(rargs,core::T_O*));
    new (cur) Cons_O(T_sp((gctools::Tagged)tagged_obj),_Nil<T_O>());
    va_end(rargs);
    return result.raw_();
  }
  va_end(rargs);
  return _Nil<T_O>().raw_();
  NO_UNWIND_END();
}

void badKeywordArgumentError(core::T_sp keyword, core::FunctionDescription* functionDescription)
{
  core::T_sp functionName = llvmo::functionNameOrNilFromFunctionDescription(functionDescription);
  if (functionName.nilp()) {
    SIMPLE_ERROR(BF("When calling an unnamed function the bad keyword argument %s was passed") % _rep_(keyword) );
  }
  SIMPLE_ERROR(BF("When calling %s the bad keyword argument %s was passed") % _rep_(functionName) % _rep_(keyword) );
}

void cc_ifBadKeywordArgumentException(core::T_O *allowOtherKeys, core::T_O *kw,
                                      core::FunctionDescription* functionDescription) {
  if (gctools::tagged_nilp(allowOtherKeys))
    badKeywordArgumentError(core::T_sp((gc::Tagged)kw), functionDescription);
}

extern void mv_copyTmv(core::T_mv *destP, core::T_mv *sourceP)
{NO_UNWIND_BEGIN();
  ASSERT(sourceP != NULL);
  ASSERT(destP != NULL);
  *destP = *sourceP;
  NO_UNWIND_END();
}

/* This function slices a T_mv down to a T_sp */
extern void sp_copyTmv(core::T_sp *destP, core::T_mv *sourceP)
{NO_UNWIND_BEGIN();
  ASSERT(sourceP != NULL);
  ASSERT(destP != NULL);
  *destP = *sourceP;
  NO_UNWIND_END();
}
};

extern "C" {


DONT_OPTIMIZE_WHEN_DEBUG_RELEASE core::T_O* makeCompiledFunction(fnLispCallingConvention funcPtr,
                                                                 void* functionDescription,
                                                                 core::T_O* frameP
                                                                 )
{NO_UNWIND_BEGIN();
  // TODO: If a pointer to an integer was passed here we could write the sourceName SourceFileInfo_sp index into it for source line debugging
  core::T_sp frame((gctools::Tagged)frameP);
  core::ClosureWithSlots_sp toplevel_closure =
    gctools::GC<core::ClosureWithSlots_O>::allocate_container(false, BCLASP_CLOSURE_SLOTS,
                                                              funcPtr,
                                                              (core::FunctionDescription*)functionDescription,
                                                              core::ClosureWithSlots_O::bclaspClosure);
  (*toplevel_closure)[BCLASP_CLOSURE_ENVIRONMENT_SLOT] = frame;
  return toplevel_closure.raw_();
  NO_UNWIND_END();
};
};

extern "C" {
#if 0
void invokeTopLevelFunction(core::T_mv *resultP,
                            fnLispCallingConvention fptr,
                            char *cpname,
                            int *sourceFileInfoHandleP,
                            size_t filePos,
                            size_t lineno,
                            size_t column,
                            core::LoadTimeValues_O **ltvPP) {
  ASSERT(ltvPP != NULL);
  core::SimpleBaseString_sp name = core::SimpleBaseString_O::make(cpname);
  Closure_sp tc = Closure_O::create(fptr,name, kw::_sym_function, *sourceFileInfoHandleP, filePos, lineno, column);
#define TIME_TOP_LEVEL_FUNCTIONS
#ifdef TIME_TOP_LEVEL_FUNCTIONS
  core::Number_sp startTime;
  if (core::_sym_STARdebugStartupSTAR->symbolValue().notnilp()) {
    startTime = gc::As<core::Number_sp>(core::cl__get_internal_real_time());
  }
#endif
  // Evaluate the function
  MAKE_STACK_FRAME( onearg, tc.raw_(), 1);
  (*onearg)[0] = *ltvPP; // Leave the tag on
  core::Vaslist onearg_valist_s(onearg);
  LCC_SPILL_CLOSURE_TO_VA_LIST(onearg_valist_s,tc.raw_());
  core::InvocationHistoryFrame invFrame(onearg_valist_s._Args,onearg_valist_s.remaining_nargs());
  *resultP = fptr(LCC_PASS_ARGS0_VA_LIST(tc.raw_()));
#ifdef TIME_TOP_LEVEL_FUNCTIONS
  if (core::_sym_STARdebugStartupSTAR->symbolValue().notnilp()) {
    core::Number_sp endTime = gc::As<core::Number_sp>(core::cl__get_internal_real_time());
    core::Number_sp diff = core::contagen_sub(endTime, startTime);
    core::Number_sp seconds = core::contagen_div(diff, gc::As<Number_sp>(cl::_sym_internalTimeUnitsPerSecond->symbolValue()));
    double dseconds = clasp_to_double(seconds);
    core::SourceFileInfo_sp sfi = core::core__source_file_info(core::make_fixnum(*sourceFileInfoHandleP));
    printf("TOP-LEVEL-FUNCTION-TIME %lf %s %zu\n", dseconds, sfi->namestring().c_str(), lineno);
  }
#endif
  ASSERTNOTNULL(*resultP);
};
#endif


/*! Invoke the main functions from the main function array.
If isNullTerminatedArray is 1 then there is a NULL terminated array of functions to call.
Otherwise there is just one. */
void cc_register_startup_function(size_t index, fnStartUp fptr) {
  register_startup_function(index,fptr);
}
/*! Call this with an alloca pointer to keep the alloca from 
being optimized away */
__attribute__((optnone,noinline)) void cc_protect_alloca(char* ptr)
{
  (void)ptr;
}


void cc_invoke_sub_run_all_function(fnStartUp fptr) {
  fptr();
}

void cc_invoke_startup_functions() {
  startup_functions_invoke();
};

void cc_invoke_byte_code_interpreter(gctools::GCRootsInModule* roots, char* byte_code, size_t bytes) {
//  printf("%s:%d byte_code: %p\n", __FILE__, __LINE__, byte_code);
  core::SimpleBaseString_sp str = core::SimpleBaseString_O::make(bytes,'\0',false,bytes,(const unsigned char*)byte_code);
  core::T_sp fin = core::cl__make_string_input_stream(str,0,_Nil<core::T_O>());
  bool log = false;
  if (core::global_debug_byte_code) {
    log = true;
  }
  byte_code_interpreter(roots,fin,log);
}



};

extern "C" {

void gdb() {
  printf("%s:%d Set a breakpoint here to invoke gdb\n", __FILE__, __LINE__);
}

void debugInspectTPtr(core::T_O *tP)
{NO_UNWIND_BEGIN();
  core::T_sp obj = gctools::smart_ptr<core::T_O>((gc::Tagged)tP);
  printf("debugInspectTPtr@%p\n", tP);
  printf("debugInspectTPtr obj.px_ref()=%p: %s\n", obj.raw_(), _rep_(obj).c_str());
  printf("%s:%d Insert breakpoint here if you want to inspect object\n", __FILE__, __LINE__);
  NO_UNWIND_END();
}

void debugInspect_i8STAR(void *p) {
  printf("debugInspect_i8STAR@%p\n", p);
}

void debugInspectTPtr_detailed(core::T_O *tP) {
  core::T_sp obj = gctools::smart_ptr<core::T_O>((gc::Tagged)tP);
  printf("debugInspectTPtr@%p  obj.px_ref()=%p: %s\n", (void *)tP, obj.raw_(), _rep_(obj).c_str());
  printf("%s:%d Insert breakpoint here if you want to inspect object\n", __FILE__, __LINE__);
  core::T_sp val((gctools::Tagged)tP);
  if (core::HashTable_sp htval = val.asOrNull<core::HashTable_O>() ) {
    htval->hash_table_dump(core::make_fixnum(0),_Nil<core::T_O>());
  }
}

void debugInspectT_mv(core::T_mv *objP)
{NO_UNWIND_BEGIN();
  MultipleValues &mv = lisp_multipleValues();
  size_t size = mv.getSize();
  printf("debugInspect_return_type T_mv.val0@%p  T_mv.nvals=%d mvarray.size=%zu\n", (*objP).raw_(), (*objP).number_of_values(), size);
  size = std::max(size, (size_t)(*objP).number_of_values());
  for (size_t i(0); i < size; ++i) {
    printf("[%zu]->%p : %s\n", i, mv.valueGet(i, size).raw_(), _rep_(core::T_sp((gc::Tagged)mv.valueGet(i,size).raw_())).c_str());
  }
  printf("\n");
  printf("%s:%d Insert breakpoint here if you want to inspect object\n", __FILE__, __LINE__);
  printf("debugInspectT_mv@%p  #val=%d\n", (void *)objP, objP->number_of_values());
  printf("   debugInspectT_mv  obj= %s\n", _rep_(*objP).c_str());
  printf("%s:%d Insert breakpoint here if you want to inspect object\n", __FILE__, __LINE__);
  NO_UNWIND_END();
}

void debugInspect_return_type(gctools::return_type rt)
{NO_UNWIND_BEGIN();
  MultipleValues &mv = lisp_multipleValues();
  size_t size = mv.getSize();
  printf("debugInspect_return_type rt.ret0@%p  rt.nvals=%zu mvarray.size=%zu\n", rt.ret0[0], rt.nvals, size);
  size = std::max(size, rt.nvals);
  for (size_t i(0); i < size; ++i) {
    printf("[%zu]->%p : %s\n", i, mv.valueGet(i, size).raw_(), _rep_(core::T_sp((gc::Tagged)mv.valueGet(i,size).raw_())).c_str());
  }
  printf("\n");
  printf("%s:%d Insert breakpoint here if you want to inspect object\n", __FILE__, __LINE__);
  NO_UNWIND_END();
}

void debugMessage(const char *msg)
{NO_UNWIND_BEGIN();
  printf("++++++ debug-message: %s3", msg);
  NO_UNWIND_END();
}

uintptr_clasp_t debug_match_two_uintptr_t(uintptr_clasp_t x, uintptr_clasp_t y)
{NO_UNWIND_BEGIN();
  if ( x == y ) return x;
  printf("%s:%d !!!!! in debug_match_two_uintptr_clasp_t the two pointers %p and %p don't match\n", __FILE__, __LINE__, (void*)x, (void*)y);
  gdb();
  UNREACHABLE();
  NO_UNWIND_END();
}

void debugPointer(const unsigned char *ptr)
{NO_UNWIND_BEGIN();
  printf("++++++ debugPointer: %p \n", ptr);
  NO_UNWIND_END();
}

void debug_vaslistPtr(Vaslist *vargs)
{NO_UNWIND_BEGIN();
  Vaslist *args = reinterpret_cast<Vaslist *>(gc::untag_vaslist((void *)vargs));
  printf("++++++ debug_va_list: reg_save_area @%p \n", args->_Args[0].reg_save_area);
  printf("++++++ debug_va_list: gp_offset %d \n", args->_Args[0].gp_offset);
  printf("++++++      next reg arg: %p\n", (void *)(((uintptr_clasp_t *)((char *)args->_Args[0].reg_save_area + args->_Args[0].gp_offset))[0]));
  printf("++++++ debug_va_list: overflow_arg_area @%p \n", args->_Args[0].overflow_arg_area);
  printf("++++++      next overflow arg: %p\n", (void *)(((uintptr_clasp_t *)((char *)args->_Args[0].overflow_arg_area))[0]));
  NO_UNWIND_END();
}

void debug_va_list(va_list vargs)
{NO_UNWIND_BEGIN();
  printf("++++++ debug_va_list:          gp_offset@%p -> %x \n", &vargs[0].gp_offset, vargs[0].gp_offset);
  printf("++++++ debug_va_list:          fp_offset@%p -> %x \n", &vargs[0].fp_offset, vargs[0].fp_offset);
  printf("++++++ debug_va_list: overflow_arg_area @%p -> %p \n", &vargs[0].overflow_arg_area, vargs[0].overflow_arg_area);
  printf("++++++ debug_va_list:     reg_save_area @%p -> %p \n", &vargs[0].reg_save_area, vargs[0].reg_save_area);
  printf("++++++      next reg arg: %p\n", (void *)(((uintptr_clasp_t *)((char *)vargs[0].reg_save_area + vargs[0].gp_offset))[0]));
  printf("++++++      next overflow arg: %p\n", (void *)(((uintptr_clasp_t *)((char *)vargs[0].overflow_arg_area))[0]));
  NO_UNWIND_END();
}

void debugSymbolPointer(core::Symbol_sp *ptr) {
  printf("++++++ debugSymbolPointer: %s\n", _rep_(*ptr).c_str());
}

void debugSymbolValue(core::Symbol_sp sym) {
  printf("+++++ debugSymbolValue: %s\n", _rep_(core::cl__symbol_value(sym)).c_str());
}


void debugPrintI32(int i32)
{NO_UNWIND_BEGIN();
  printf("+++DBG-I32[%d]\n", i32);
  fflush(stdout);
  NO_UNWIND_END();
}

void debugPrint_blockFrame(core::T_O* handle)
{NO_UNWIND_BEGIN();
#if defined(DEBUG_FLOW_TRACKER)
  Cons_sp frameHandleCons((gc::Tagged)handle);
  Fixnum frameFlowCounter = CONS_CAR(frameHandleCons).unsafe_fixnum();
  printf("%s:%d debugPrint_blockFrame handle %p   FlowCounter %" PFixnum "\n", __FILE__, __LINE__, handle, frameFlowCounter );
#else
  printf("%s:%d debugPrint_blockFrame handle %p\n", __FILE__, __LINE__, handle );
#endif
  fflush(stdout);
  NO_UNWIND_END();
}

void debugPrint_blockHandleReturnFrom(unsigned char *exceptionP, core::T_O* handle)
{NO_UNWIND_BEGIN();
  core::ReturnFrom &returnFrom = (core::ReturnFrom &)*((core::ReturnFrom *)(exceptionP));
  printf("%s:%d debugPrint_blockHandleReturnFrom return-from handle %p    block handle %p\n", __FILE__, __LINE__, returnFrom.getHandle(), handle );
  if (returnFrom.getHandle() == handle) {
    printf("%s:%d debugPrint_blockHandleReturnFrom handles match!\n", __FILE__, __LINE__ );
    fflush(stdout);
    return;
  }
#if defined(DEBUG_FLOW_TRACKER)
  Cons_sp throwHandleCons((gc::Tagged)returnFrom.getHandle());
  Cons_sp frameHandleCons((gc::Tagged)handle);
  Fixnum throwFlowCounter = CONS_CAR(throwHandleCons).unsafe_fixnum();
  Fixnum frameFlowCounter = CONS_CAR(frameHandleCons).unsafe_fixnum();
  printf("%s:%d  continued  debugPrint_blockHandleReturnFrom return-from is looking for FlowCounter %" PFixnum " and it reached FlowCounter %" PFixnum "\n", __FILE__, __LINE__, throwFlowCounter, frameFlowCounter );
  fflush(stdout);
#endif
  NO_UNWIND_END();
}

void debugPrint_size_t(size_t v)
{NO_UNWIND_BEGIN();
  printf("+++DBG-size_t[%lu/%lx]\n", v, v);
  NO_UNWIND_END();
}

DONT_OPTIMIZE_WHEN_DEBUG_RELEASE void throwReturnFrom(size_t depth, core::ActivationFrame_O* frameP) {
#ifdef DEBUG_TRACK_UNWINDS
  global_ReturnFrom_count++;
#endif
  core::ActivationFrame_sp af((gctools::Tagged)(frameP));
  core::T_sp handle = *const_cast<core::T_sp *>(&core::value_frame_lookup_reference(af, depth, 0));
  core::ReturnFrom returnFrom(handle.raw_());
#if defined(DEBUG_FLOW_TRACKER)
  flow_tracker_about_to_throw(CONS_CAR(handle).unsafe_fixnum());
#endif
  throw returnFrom;
}
};

extern "C" {

DONT_OPTIMIZE_WHEN_DEBUG_RELEASE gctools::return_type blockHandleReturnFrom_or_rethrow(unsigned char *exceptionP, core::T_O* handle) {
  core::ReturnFrom &returnFrom = (core::ReturnFrom &)*((core::ReturnFrom *)(exceptionP));
  if (returnFrom.getHandle() == handle) {
    core::MultipleValues &mv = core::lisp_multipleValues();
    gctools::return_type result(mv.operator[](0),mv.getSize());
    return result;
  }
#if defined(DEBUG_FLOW_TRACKER)
  if (handle) {
    Cons_sp throwHandleCons((gc::Tagged)returnFrom.getHandle());
    Cons_sp frameHandleCons((gc::Tagged)handle);
    if (!throwHandleCons.consp()) printf("%s:%d The throwHandleCons is not a CONS -> %p\n", __FILE__, __LINE__, (void*)throwHandleCons.raw_() );
    if (!frameHandleCons.consp()) printf("%s:%d The frameHandleCons is not a CONS -> %p\n", __FILE__, __LINE__, (void*)frameHandleCons.raw_() );
    Fixnum throwFlowCounter = CONS_CAR(throwHandleCons).unsafe_fixnum();
    Fixnum frameFlowCounter = CONS_CAR(frameHandleCons).unsafe_fixnum();
    if (throwFlowCounter > frameFlowCounter) {
      printf("%s:%d A return-from has missed its target frame - the return-from is looking for FlowCounter %" PFixnum " and it reached FlowCounter %" PFixnum "\n", __FILE__, __LINE__, throwFlowCounter, frameFlowCounter );
      flow_tracker_last_throw_backtrace_dump();
      abort();
    }
  }
#endif
#if defined(DEBUG_FLOW_TRACKER)
  Cons_sp throwHandleCons((gc::Tagged)returnFrom.getHandle());
  if (!throwHandleCons.consp()) printf("%s:%d The throwHandleCons is not a CONS -> %p\n", __FILE__, __LINE__, (void*)throwHandleCons.raw_() );
  Fixnum throwFlowCounter = CONS_CAR(throwHandleCons).unsafe_fixnum();
  flow_tracker_about_to_throw(throwFlowCounter);
#endif
  throw; // throw returnFrom;
}

};

extern "C" {

DONT_OPTIMIZE_WHEN_DEBUG_RELEASE core::T_O* initializeBlockClosure(core::T_O** afP)
{NO_UNWIND_BEGIN();
  ValueFrame_sp vf = ValueFrame_sp((gc::Tagged)*reinterpret_cast<ValueFrame_O**>(afP));
#ifdef DEBUG_FLOW_TRACKER
  Fixnum counter = next_flow_tracker_counter();
  Cons_sp unique = Cons_O::create(make_fixnum(counter),_Nil<T_O>());
#else
  Cons_sp unique = Cons_O::create(_Nil<T_O>(),_Nil<T_O>());
#endif
  vf->operator[](0) = unique;
  return unique.raw_();
  NO_UNWIND_END();
}

core::T_O* initializeTagbodyClosure(core::T_O *afP)
{NO_UNWIND_BEGIN();
  core::T_sp tagbodyId((gctools::Tagged)afP);
  ValueFrame_sp vf = ValueFrame_sp((gc::Tagged)*reinterpret_cast<ValueFrame_O**>(afP));
#ifdef DEBUG_FLOW_TRACKER
  Fixnum counter = next_flow_tracker_counter();
  Cons_sp unique = Cons_O::create(make_fixnum(counter),_Nil<T_O>());
#else
  Cons_sp unique = Cons_O::create(_Nil<T_O>(),_Nil<T_O>());
#endif
  vf->operator[](0) = unique;
  return unique.raw_();
  NO_UNWIND_END();
}
};

core::T_mv proto_ifCatchFrameMatchesStoreResultElseRethrow(size_t catchFrame, unsigned char *exceptionP) {
  core::CatchThrow *ctExceptionP = reinterpret_cast<core::CatchThrow *>(exceptionP);
  if (catchFrame == ctExceptionP->getFrame()) {
    return gctools::multiple_values<core::T_O>::createFromValues(); // ctExceptionP->getReturnedObject();
  }
// rethrow the exception
  throw * ctExceptionP;
}

extern "C" {
void sp_ifCatchFrameMatchesStoreResultElseRethrow(core::T_sp *resultP, size_t catchFrame, unsigned char *exceptionP) {
  (*resultP) = proto_ifCatchFrameMatchesStoreResultElseRethrow(catchFrame, exceptionP);
  ASSERTNOTNULL(*resultP);
}
void mv_ifCatchFrameMatchesStoreResultElseRethrow(core::T_mv *resultP, size_t catchFrame, unsigned char *exceptionP) {
  (*resultP) = proto_ifCatchFrameMatchesStoreResultElseRethrow(catchFrame, exceptionP);
  ASSERTNOTNULL(*resultP);
}
};

extern "C" {
void exceptionStackUnwind(size_t frame)
{NO_UNWIND_BEGIN();
  my_thread->exceptionStack().unwind(frame);
  NO_UNWIND_END();
}

void throwIllegalSwitchValue(size_t val, size_t max) {
  SIMPLE_ERROR(BF("Illegal switch value %d - max value is %d") % val % max);
}


void throwDynamicGo(size_t depth, size_t index, core::T_O *afP) {
#ifdef DEBUG_TRACK_UNWINDS
  global_DynamicGo_count++;
#endif
  T_sp af((gctools::Tagged)afP);
  ValueFrame_sp tagbody = gc::As<ValueFrame_sp>(core::tagbody_frame_lookup(gc::As_unsafe<ValueFrame_sp>(af),depth,index));
  T_O* handle = tagbody->operator[](0).raw_();
  core::DynamicGo dgo(handle, index);
#if defined(DEBUG_FLOW_TRACKER)
  Cons_sp throwHandleCons((gc::Tagged)dgo.getHandle());
  if (!throwHandleCons.consp()) printf("%s:%d The throwHandleCons is not a CONS -> %p\n", __FILE__, __LINE__, (void*)throwHandleCons.raw_() );
  Fixnum throwFlowCounter = CONS_CAR(throwHandleCons).unsafe_fixnum();
  flow_tracker_about_to_throw(throwFlowCounter);
#endif
  throw dgo;
}

size_t tagbodyHandleDynamicGoIndex_or_rethrow(char *exceptionP, T_O* handle) {
  core::DynamicGo& goException = *reinterpret_cast<core::DynamicGo *>(exceptionP);
  if (goException.getHandle() == handle) {
    return goException.index();
  }
  throw;
}


void debugSourceFileInfoHandle(int *sourceFileInfoHandleP)
{NO_UNWIND_BEGIN();
  int sfindex = *sourceFileInfoHandleP;
  core::Fixnum_sp fn = core::make_fixnum(sfindex);
  SourceFileInfo_sp sfi = gc::As<SourceFileInfo_sp>(core::core__source_file_info(fn));
  printf("%s:%d debugSourceFileInfoHandle[%d] --> %s\n", __FILE__, __LINE__, sfindex, _rep_(sfi).c_str());
  NO_UNWIND_END();
}
};


extern "C" {
void saveToMultipleValue0(core::T_mv *mvP)
{NO_UNWIND_BEGIN();
  mvP->saveToMultipleValue0();
  NO_UNWIND_END();
}

gctools::return_type restoreFromMultipleValue0()
{NO_UNWIND_BEGIN();
  core::MultipleValues &mv = core::lisp_multipleValues();
  gctools::return_type result(mv.operator[](0),mv.getSize());
  return result;
  NO_UNWIND_END();
}

void mv_restoreFromMultipleValue0(core::T_mv *resultP)
{NO_UNWIND_BEGIN();
  resultP->readFromMultipleValue0();
  NO_UNWIND_END();
}

#if 0
/*! Copy the current MultipleValues in _lisp->values() into a SimpleVector */
extern core::T_O* saveValues(core::T_mv *mvP)
{NO_UNWIND_BEGIN();
  ASSERT(mvP != NULL);
  int numValues = (*mvP).number_of_values();
  core::SimpleVector_sp vo = core::SimpleVector_O::make(numValues,_Nil<core::T_O>());
  //	printf("intrinsics.cc saveValues numValues = %d\n", numValues );
  if (numValues > 0) {
    vo->rowMajorAset(0, (*mvP));
  }
  core::MultipleValues& mv = core::lisp_multipleValues();
  for (int i(1); i < (*mvP).number_of_values(); ++i) {
    core::T_sp val((gctools::Tagged)mv._Values[i]);
    vo->rowMajorAset(i, val );
  }
  return vo.raw_();
  NO_UNWIND_END();
}

/*! Copy the current MultipleValues in _lisp->values() into a ComplexVector_T */
extern void loadValues(core::T_mv *resultP, core::T_O* simpleVectorP)
{NO_UNWIND_BEGIN();
  ASSERT(resultP != NULL);
  // I'm not sure the following can ever happen
  if (!simpleVectorP) {
    // If there was a non-local exit then *vectorObjectP will be NULL
    // check for that here and if so set the result to gctools::multiple_values<core::T_O>()
    (*resultP) = gctools::multiple_values<core::T_O>();
    return;
  }
  SimpleVector_sp vo((gctools::Tagged)simpleVectorP);
  //	printf("intrinsics.cc loadValues vo->length() = %d\n", vo->length() );
  if (vo->length() == 0) {
    (*resultP) = gctools::multiple_values<core::T_O>();
    return;
  }
  (*resultP) = gctools::multiple_values<core::T_O>(vo->rowMajorAref(0), vo->length());
  core::MultipleValues& mv = core::lisp_multipleValues();
  for (int i(1); i < vo->length(); ++i) {
    mv._Values[i] = vo->rowMajorAref(i).raw_();
  }
  NO_UNWIND_END();
}
#endif


size_t cc_trackFirstUnexpectedKeyword(size_t badKwIdx, size_t newBadKwIdx)
{NO_UNWIND_BEGIN();
  // 65536 is the magic number for badKwIdx has not been assigned yet
  if (badKwIdx != 65536)
    return badKwIdx;
  return newBadKwIdx;
  NO_UNWIND_END();
}
};

extern "C" {

void pushDynamicBinding(core::T_O *tsymbolP)
{NO_UNWIND_BEGIN();
  core::Symbol_sp sym((gctools::Tagged)tsymbolP);
  my_thread->bindings().push_with_value_coming(sym,&sym->_GlobalValue);
  NO_UNWIND_END();
}

void popDynamicBinding(core::T_O *tsymbolP)
{NO_UNWIND_BEGIN();
  core::Symbol_sp sym((gctools::Tagged)tsymbolP);
  core::Symbol_sp top = my_thread->bindings().topSymbol();
  if (sym != my_thread->bindings().topSymbol()) {
    stringstream ss;
    ss << __FILE__ << ":" << __LINE__;
    ss << " About  to DynamicBindingStack::pop_binding" << my_thread->bindings().top();
    ss << " of " << sym->formattedName(true) << std::endl;
    ss << "  mismatch with top of dynamic binding stack: " << top->formattedName(true) << std::endl;
    ss << "  dumping stack: " << std::endl;
    core::core__dynamic_binding_stack_dump(ss);
    SIMPLE_ERROR(BF("Mismatch in popDynamicBinding:\n%s") % ss.str());
  }
  my_thread->bindings().pop_binding();
  NO_UNWIND_END();
}
};

extern "C" {

void setFrameUniqueId(size_t id, core::ActivationFrame_O* frameP) {
#ifdef DEBUG_LEXICAL_DEPTH
  ActivationFrame_sp src((gctools::Tagged)frameP);
  src->_UniqueId = id;
#endif
}

void ignore_setFrameUniqueId() {
  // Do nothing
}

void ensureFrameUniqueId(size_t id, size_t depth, core::ActivationFrame_O* frameP) {
#ifdef DEBUG_LEXICAL_DEPTH
  ActivationFrame_sp src((gctools::Tagged)frameP);
  ActivationFrame_sp dest = value_frame_lookup(src,depth);
  if (dest->_UniqueId != id) {
    printf("%s:%d Mismatch in frame UniqueId - expecting %lu - found %lu\n", __FILE__, __LINE__, id, dest->_UniqueId );
    abort();
  }
#endif
}

void ignore_ensureFrameUniqueId() {
  // Do nothing
}

};

// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------
//
//  Intrinsics for Cleavir

extern "C" {

//#define DEBUG_CC

void cc_setTmvToNil(core::T_mv *sharedP)
{NO_UNWIND_BEGIN();
  *sharedP = Values(_Nil<core::T_O>());
  NO_UNWIND_END();
}


#define PROTO_cc_setSymbolValue "void (t* t*)"
#define CATCH_cc_setSymbolValue false
void cc_setSymbolValue(core::T_O *sym, core::T_O *val)
{NO_UNWIND_BEGIN();
  //	core::Symbol_sp s = gctools::smart_ptr<core::Symbol_O>(reinterpret_cast<core::Symbol_O*>(sym));
  core::Symbol_sp s = gctools::smart_ptr<core::Symbol_O>((gc::Tagged)sym);
  s->setf_symbolValue(gctools::smart_ptr<core::T_O>((gc::Tagged)val));
  NO_UNWIND_END();
}

core::T_O *cc_enclose(fnLispCallingConvention llvm_func,
                      void* functionDescription,
                      std::size_t numCells, ...)
{
  gctools::smart_ptr<core::ClosureWithSlots_O> functoid =
    gctools::GC<core::ClosureWithSlots_O>::allocate_container( false, numCells
                                                              , llvm_func
                                                               , (core::FunctionDescription*)functionDescription,
                                                               core::ClosureWithSlots_O::cclaspClosure);
  core::T_O *p;
  va_list argp;
  va_start(argp, numCells);
  int idx = 0;
  for (; numCells; --numCells) {
    p = ENSURE_VALID_OBJECT(va_arg(argp, core::T_O *));
    (*functoid)[idx] = gctools::smart_ptr<core::T_O>((gc::Tagged)p);
    ++idx;
  }
  va_end(argp);
  return functoid.raw_();
}


/*! Take the multiple-value inputs from the thread local MultipleValues and call tfunc with them.
     This function looks exactly like the cc_invoke_multipleValueOneFormCall intrinsic but
    in cmpintrinsics.lsp it is set not to require a landing pad */
//    void cc_call_multipleValueOneFormCall(core::T_mv* result, core::T_O* tfunc )

LCC_RETURN cc_call_multipleValueOneFormCall(core::Function_O *tfunc) {
  ASSERTF(gctools::tagged_generalp(tfunc), BF("The argument %p does not have a general tag!") % (void*)tfunc);
  core::MultipleValues &mvThreadLocal = core::lisp_multipleValues();
  size_t lcc_nargs = mvThreadLocal.getSize();
  MAKE_STACK_FRAME( mvargs, tfunc, lcc_nargs);
  for (size_t i(0); i < lcc_nargs; ++i) (*mvargs)[i] = ENSURE_VALID_OBJECT(mvThreadLocal[i]);
#ifdef DEBUG_VALUES
  if (_sym_STARdebug_valuesSTAR &&
        _sym_STARdebug_valuesSTAR->boundP() &&
        _sym_STARdebug_valuesSTAR->symbolValue().notnilp()) {
    for (size_t i(0); i < lcc_nargs; ++i) {
      core::T_sp mvobj((gctools::Tagged)(*mvargs)[i]);
      printf("%s:%d  ....  cc_call_multipleValueOneFormCall[%lu] -> %s\n", __FILE__, __LINE__, i, _rep_(mvobj).c_str());
    }
  }
#endif
  core::Function_sp func((gctools::Tagged)tfunc);
  return core::funcall_frame(func,mvargs);
}

LCC_RETURN cc_call_multipleValueOneFormCallWithRet0(core::Function_O *tfunc, gctools::return_type ret0 ) {
  ASSERTF(gctools::tagged_generalp(tfunc), BF("The argument %p does not have a general tag!") % (void*)tfunc);
  MAKE_STACK_FRAME( mvargs, tfunc, ret0.nvals);
  FILL_FRAME_WITH_RETURN_REGISTERS(mvargs,ret0);
  if (ret0.nvals>LCC_RETURN_VALUES_IN_REGISTERS) {
    core::MultipleValues &mvThreadLocal = core::lisp_multipleValues();
    for (size_t i(LCC_RETURN_VALUES_IN_REGISTERS); i < ret0.nvals; ++i) (*mvargs)[i] = ENSURE_VALID_OBJECT(mvThreadLocal[i]);
  }
#ifdef DEBUG_VALUES
  if (_sym_STARdebug_valuesSTAR &&
        _sym_STARdebug_valuesSTAR->boundP() &&
        _sym_STARdebug_valuesSTAR->symbolValue().notnilp()) {
    for (size_t i(0); i < lcc_nargs; ++i) {
      core::T_sp mvobj((gctools::Tagged)(*mvargs)[i]);
      printf("%s:%d  ....  cc_call_multipleValueOneFormCall[%lu] -> %s\n", __FILE__, __LINE__, i, _rep_(mvobj).c_str());
    }
  }
#endif
  core::Function_sp func((gctools::Tagged)tfunc);
  return core::funcall_frame(func,mvargs);
}

void cc_saveThreadLocalMultipleValues(core::T_mv *result, core::MultipleValues *mv)
{NO_UNWIND_BEGIN();
  core::MultipleValues &mvThread = core::lisp_multipleValues();
  (*mv)._Size = result->number_of_values();
  (*mv)[0] = ENSURE_VALID_OBJECT((*result).raw_());
  for (size_t i = 1; i < (*mv)._Size; ++i) (*mv)[i] = ENSURE_VALID_OBJECT(mvThread[i]);
  NO_UNWIND_END();
}

void cc_loadThreadLocalMultipleValues(core::T_mv *result, core::MultipleValues *mv)
{NO_UNWIND_BEGIN();
  core::MultipleValues &mvThread = core::lisp_multipleValues();
  *result = gctools::multiple_values<core::T_O>(gctools::smart_ptr<core::T_O>((gc::Tagged)ENSURE_VALID_OBJECT((*mv)[0])), (*mv)._Size);
  for (size_t i = 1; i < (*mv)._Size; ++i) {
    mvThread[i] = ENSURE_VALID_OBJECT((*mv)[i]);
  }
  NO_UNWIND_END();
}

void cc_oddKeywordException(core::FunctionDescription* functionDescription) {
  T_sp functionName = llvmo::functionNameOrNilFromFunctionDescription(functionDescription);
  if (functionName.nilp())
    SIMPLE_ERROR(BF("Odd number of keyword arguments"));
  else
    SIMPLE_ERROR(BF("In call to %s got odd number of keyword arguments") % _rep_(functionName));
}

T_O **cc_multipleValuesArrayAddress()
{NO_UNWIND_BEGIN();
  return &lisp_multipleValues().callingArgsStart()[0];
  NO_UNWIND_END();
}

void cc_unwind(T_O *targetFrame, size_t index) {
#ifdef DEBUG_TRACK_UNWINDS
  global_unwind_count++;
#endif
  core::Unwind unwind(targetFrame, index);
    throw unwind;
}

void cc_saveMultipleValue0(core::T_mv *result)
{NO_UNWIND_BEGIN();
  (*result).saveToMultipleValue0();
  NO_UNWIND_END();
}

void cc_restoreMultipleValue0(core::T_mv *result)
{NO_UNWIND_BEGIN();
  (*result).readFromMultipleValue0();
  NO_UNWIND_END();
}

T_O *cc_pushLandingPadFrame()
{NO_UNWIND_BEGIN();
#ifdef DEBUG_FLOW_TRACKER
  Cons_sp unique = Cons_O::create(make_fixnum(next_flow_tracker_counter()),_Nil<T_O>());
#else
  Cons_sp unique = Cons_O::create(_Nil<T_O>(),_Nil<T_O>());
#endif
  return unique.raw_();
  NO_UNWIND_END();
}

size_t cc_landingpadUnwindMatchFrameElseRethrow(char *exceptionP, core::T_O *thisFrame) {
  ASSERT(gctools::tagged_fixnump(thisFrame));
  core::Unwind *unwindP = reinterpret_cast<core::Unwind *>(exceptionP);
  if (unwindP->getFrame() == thisFrame) {
    return unwindP->index();
  }
  // throw * unwindP;
  throw;
}

#if 0 // Bring this back online when we convert to new calling convention
T_mv cc_multiple_value_funcall(core::T_mv* result, T_O* funcDesignator, std::size_t numFuns, ...) {
  MultipleValues mvAccumulate;
  mvAccumulate._Size = 0;
  size_t idx = 0;
  va_list argp;
  va_start(argp,numFuns);
  for (; numFuns; --numFuns) {
    core::T_O* tfunc = va_arg(argp,core::T_O*);
#error "Check the return value of tfunc.asOrNull"
    core::Function_sp func = tfunc.asOrNull<core::Function_O>();
    ASSERT(func);
    auto closure = func->closure;
    T_mv result;
    closure->invoke(&result,LCC_PASS_ARGS0());
    mvAccumulate[idx] = result.raw_();
    ++idx;
    for (size_t i = 1, iEnd(result.number_of_values()); i < iEnd; ++i) {
      mvAccumulate[idx] = result.valueGet(i).raw_();
      ++idx;
    }
  }
  va_end(argp);
  ASSERT(idx < MultipleValues::MultipleValuesLimit);
  mvAccumulate._Size = idx;
#error "Check the return value of tfunc.asOrNull"
  core::Function_sp func = funcDesignator.asOrNull<core::Function_O>();
  ASSERT(func);
  auto closure = gc::untag_general<core::Function_O *>(func)->closure.as<core::Closure>();
  core::T_O** a = &mvAccumulate[0];
  MultipleValues &mvThreadLocal = lisp_multipleValues();
  for (size_t i = LCC_FIXED_ARGS, iEnd(mvAccumulate._Size); i < iEnd; ++i) {
    mvThreadLocal[i] = mvAccumulate[i];
  }
  closure->invoke(result, mvAccumulate._Size, mvAccumulate[0], mvAccumulate[1], mvAccumulate[2], mvAccumulate[3], mvAccumulate[4]);
}

T_mv cc_multiple_value_prog1_function(core::T_mv* result, core::T_O* tfunc1, core::T_O* tfunc2) {
  MultipleValues mvFunc1;
#error "Check the return value of tfunc.asOrNull"
  core::Function_sp func1 = tfunc1.asOrNull<core::Function_O>();
  ASSERT(func1);
  core::Closure_sp closure1 = func1->closure;
  closure1->invoke(result,LCC_PASS_ARGS0());
  mvFunc1._Size = result->number_of_values();
  mvFunc1[0] = result->raw_();
  MultipleValues &mvThreadLocal = lisp_multipleValues();
  for (size_t i(1), iEnd(mvFunc1._Size); i < iEnd; ++i) mvFunc1[i] = mvThreadLocal[i];
  T_mv resultTemp;
#error "Check the return value of tfunc.asOrNull"
  core::Function_sp func2 = tfunc2.asOrNull<core::Function_O>();
  ASSERT(func2);
  core::Closure_sp closure2 = func2->closure;
  closure2->invoke(&resultTemp,LCC_PASS_ARGS0());
  for (size_t i(1), iEnd(mvFunc1._Size); i < iEnd; ++i) mvThreadLocal[i] = mvFunc1[i];
}
#endif

void clasp_terminate()
{
  printf("Terminating clasp from clasp_terminate\n");
  abort();
}
};

#pragma GCC visibility pop

namespace llvmo {
// We must link one symbol to the executable or none of this file will be inserted

void initialize_link_intrinsics() {
//	PRIMITIVE(cc_setSymbolValue);
//printf("%s:%d  Initializing intrinsics.cc\n", __FILE__, __LINE__ );
}
};


extern "C" {

core::T_O* cc_dispatch_slot_reader_index_debug(core::T_O* toptimized_slot_reader, size_t index, core::T_O* tvargs) {
  core::SimpleVector_sp optimized_slot_reader((gctools::Tagged)toptimized_slot_reader);
  core::VaList_sp vargs((gctools::Tagged)tvargs);
  va_list vat;
  va_copy(vat,vargs->_Args);
  core::Instance_sp instance((gctools::Tagged)va_arg(vat,core::T_O*));
  va_end(vat);
  core::T_sp result = core::eval::funcall(clos::_sym_dispatch_slot_reader_index_debug,
                                          optimized_slot_reader,instance,vargs);
  return reinterpret_cast<T_O*>(result.raw_());
}

core::T_O* cc_dispatch_slot_writer_index_debug(core::T_O* toptimized_slot_writer, size_t index, core::T_O* tvargs) {
  core::SimpleVector_sp optimized_slot_writer((gctools::Tagged)toptimized_slot_writer);
  core::VaList_sp vargs((gctools::Tagged)tvargs);
  va_list vat;
  va_copy(vat,vargs->_Args);
  core::T_sp value((gctools::Tagged)va_arg(vat,core::T_O*));
  core::Instance_sp instance((gctools::Tagged)va_arg(vat,core::T_O*));
  va_end(vat);
  core::T_sp result = core::eval::funcall(clos::_sym_dispatch_slot_writer_index_debug,
                                          optimized_slot_writer,value,instance,vargs);
  return reinterpret_cast<T_O*>(result.raw_());
}
};


extern "C" {

// The following are here instead of fastgf.cc because they are apparently
// too big to be inlined.
#if 1

// set to zero and enable this function in fastgf.cc to reproduce a problem
// where clasp locks up when trying to compile the first discriminating function
// The problem crops up when cc_dispatch_miss is inlined
gctools::return_type cc_dispatch_miss(core::T_O* tgf, core::T_O* tgf_vaslist)
{
  core::FuncallableInstance_sp gf((gctools::Tagged)tgf);
  core::VaList_sp gf_vaslist((gctools::Tagged)tgf_vaslist);
  core::T_mv result = core::eval::funcall(clos::_sym_dispatch_miss,gf,gf_vaslist);
#ifdef DEBUG_GFDISPATCH
  printf("%s:%d  Returning from cc_dispatch_miss\n", __FILE__, __LINE__ );
#endif
  return result.as_return_type();
}
#endif

void cc_dispatch_debug(int msg_id, uintptr_clasp_t val)
{
  // The msg_id switch values correspond to values passed from cmpgf.lsp
  //   The values mean:
  //         0 - print the argument as an integer step index
  //         1 - Print the value as a integer
  //         2 - print the value as a tag
  //         3 - print the value as a tagged pointer to a Vaslist object
  //         4 - print the value as a pointer
  //         5 - print the contents of the va_list pointed to by the value
  //         6 - print the value as a stamp
  //         7 - print the value as a pointer to a dispatch function
  switch (msg_id) {
  case 0:
      core::write_bf_stream(BF("Step %d\n") % val);
//      printf("%s:%d    cc_dispatch_debug step %d\n", __FILE__, __LINE__, val );
      break;
  case 1:
      core::write_bf_stream(BF("Arg val[%d]") % val);
//      printf("%s:%d    cc_dispatch_debug arg val[%d]\n", __FILE__, __LINE__, val );
      break;
  case 2:
      core::write_bf_stream(BF(" tag = %d\n") % val); 
//      printf("%s:%d    cc_dispatch_debug tag [%d]\n", __FILE__, __LINE__, val );
     break;
  case 3: {
    core::VaList_sp vls((gc::Tagged)val);
//    printf("%s:%d    vaList_sp.raw_() = %p\n", __FILE__, __LINE__, vls.raw_());
    core::write_bf_stream(BF("Arg VaList_sp.raw_() = %p list -> %s\n") % (void*)vls.raw_() % _rep_(vls) );
    dump_Vaslist_ptr(&*vls);
    break;
  }
  case 4: {
//      printf("%s:%d     ptr: %p\n", __FILE__, __LINE__, (void*)val);
      core::write_bf_stream(BF("Ptr: %p\n") % (void*)val );
  }
      break;
  case 5: {
//      printf("%s:%d     ptr: %p\n", __FILE__, __LINE__, (void*)val);
      core::write_bf_stream(BF("va_list: %p\n") % (void*)val );
      void* dump_va_list_voidSTAR = (void*)&core::dump_va_list;
      typedef void (*fptr)(uintptr_t);
      fptr my_fptr = reinterpret_cast<fptr>(dump_va_list_voidSTAR);
      my_fptr(val);
      break;
  }
  case 6:
      core::write_bf_stream(BF("Argument stamp: %lu\n") % val);
      break;
  case 7:
      core::write_bf_stream(BF("Dispatch to: %p\n") % val);
      break;
  }
  fflush(stdout);
}


};
