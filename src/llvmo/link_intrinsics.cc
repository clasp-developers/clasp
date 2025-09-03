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
// #define DEBUG_LEVEL_FULL
#ifdef USE_MPS
extern "C" {
#include <clasp/mps/code/mps.h>
};
#endif
#include <dlfcn.h>
#include <cstdarg>
#include <typeinfo>
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/commandLineOptions.h>
#include <clasp/core/bignum.h>
#include <clasp/core/function.h>
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
#include <clasp/core/compiler.h>
#include <clasp/core/debugger.h>
#include <clasp/core/backtrace.h>
#include <clasp/llvmo/debugInfoExpose.h>
#include <clasp/core/random.h>
#include <clasp/core/primitives.h>
#include <clasp/core/numbers.h>
#include <clasp/core/symbolTable.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/intrinsics.h>
#include <clasp/llvmo/code.h>
#include <clasp/gctools/gc_interface.fwd.h>
#include <clasp/core/exceptions.h>
#include <clasp/core/unwind.h>
#include <clasp/gctools/interrupt.h>

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

[[noreturn]] NEVER_OPTIMIZE void not_function_designator_error(core::T_sp arg) {
  TYPE_ERROR(arg, core::Cons_O::createList(::cl::_sym_or, ::cl::_sym_function, ::cl::_sym_symbol));
}

[[noreturn]] NEVER_OPTIMIZE void intrinsic_error(ErrorCode err, core::T_sp arg0, core::T_sp arg1, core::T_sp arg2) {
  switch (err) {
  case noFunctionBoundToSymbol: {
    core::Symbol_sp sym(gc::As<core::Symbol_sp>(arg0));
    ERROR_UNDEFINED_FUNCTION(sym);
  } break;
  case couldNotCoerceToClosure:
    SIMPLE_ERROR(" symbol {}", _rep_(arg0));
  case destinationMustBeActivationFrame:
    SIMPLE_ERROR("Destination must be ActivationFrame");
  case invalidIndexForFunctionFrame:
    SIMPLE_ERROR("Invalid index[{}] for FunctionFrame(size={})", _rep_(arg0), _rep_(arg1));
  case badCell: {
    SIMPLE_ERROR("The object with pointer {} is not a cell", (void*)arg0.raw_());
  }
  default:
    SIMPLE_ERROR("An intrinsicError {} was signaled and there needs to be a more descriptive error message for it in "
                 "gctools::intrinsic_error arg0: {} arg1: {} arg2: {}",
                 (int)err, arg0, arg1, arg2);
  };
};

}; // namespace llvmo

extern "C" {

void cc_initialize_gcroots_in_module(gctools::GCRootsInModule* holder, core::T_O** root_address, size_t num_roots,
                                     gctools::Tagged initial_data, SimpleVector_O** transientAlloca, size_t transient_entries,
                                     size_t function_pointer_count, ClaspXepAnonymousFunction* fptrs) {
  NO_UNWIND_BEGIN();
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s GCRootsInModule@%p  root_address@%p  num_roots %lu initial_data = %p\n", __FILE__, __LINE__,
                            __FUNCTION__, (void*)holder, (void*)root_address, num_roots, (void*)initial_data));
  initialize_gcroots_in_module(holder, root_address, num_roots, initial_data, transientAlloca, transient_entries,
                               function_pointer_count, (void**)fptrs);
  NO_UNWIND_END();
}

void cc_finish_gcroots_in_module(gctools::GCRootsInModule* holder) {
  NO_UNWIND_BEGIN();
  llvmo::JITDataReadWriteMaybeExecute();
  holder->_TransientAlloca = NULL;
  llvmo::JITDataReadExecute();
  NO_UNWIND_END();
}

void cc_remove_gcroots_in_module(gctools::GCRootsInModule* holder) {
  NO_UNWIND_BEGIN();
  shutdown_gcroots_in_module(holder);
  NO_UNWIND_END();
}

// Define what ltvc_xxxx functions return - this must match what is
//  in cmpintrinsics.lisp
typedef void LtvcReturnVoid;
#define LTVCRETURN /* Nothing return for void */

LtvcReturnVoid ltvc_make_nil(gctools::GCRootsInModule* holder, char tag, size_t index) {
  NO_UNWIND_BEGIN();
  core::T_sp val = nil<core::T_O>();
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_t(gctools::GCRootsInModule* holder, char tag, size_t index) {
  NO_UNWIND_BEGIN();
  core::T_sp val = _lisp->_true();
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

T_O* ltvc_lookup_transient(gctools::GCRootsInModule* holder, char tag, size_t index) {
  NO_UNWIND_BEGIN();
  return (T_O*)holder->getTransient(index);
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_ratio(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* num, core::T_O* denom) {
  NO_UNWIND_BEGIN();
  Integer_sp inum((gc::Tagged)num);
  Integer_sp idenom((gc::Tagged)denom);
  core::T_sp val = core::Ratio_O::create(inum, idenom);
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_complex(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* real, core::T_O* imag) {
  NO_UNWIND_BEGIN();
  core::Real_sp nreal((gctools::Tagged)real);
  core::Real_sp nimag((gctools::Tagged)imag);
  // Do not convert nreal and nimag to double, can be all types of Real_sp
  core::T_sp val = core::Complex_O::create(nreal, nimag);
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_cons(gctools::GCRootsInModule* holder, char tag, size_t index) {
  NO_UNWIND_BEGIN();
  core::T_sp val = core::Cons_O::create(nil<core::T_O>(), nil<core::T_O>());
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_rplaca(gctools::GCRootsInModule* holder, core::T_O* cons_t, core::T_O* car_t) {
  NO_UNWIND_BEGIN();
  core::T_sp tcons((gctools::Tagged)cons_t);
  core::Cons_sp cons = gc::As<core::Cons_sp>(tcons);
  cons->rplaca(core::T_sp(car_t));
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_rplacd(gctools::GCRootsInModule* holder, core::T_O* cons_t, core::T_O* cdr_t) {
  NO_UNWIND_BEGIN();
  core::T_sp tcons((gctools::Tagged)cons_t);
  core::Cons_sp cons = gc::As<core::Cons_sp>(tcons);
  cons->rplacd(core::T_sp(cdr_t));
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_list(gctools::GCRootsInModule* holder, char tag, size_t index, size_t len) {
  NO_UNWIND_BEGIN();
  // Makes a list of length LEN where all elements are NIL.
  // (ltvc_fill_list will be immediately after, so they could be undefined just as well.)
  ql::list result;
  for (; len != 0; --len)
    result << nil<core::T_O>();
  LTVCRETURN holder->setTaggedIndex(tag, index, result.result().tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_fill_list(gctools::GCRootsInModule* holder, core::T_O* list, size_t len, ...) {
  NO_UNWIND_BEGIN();
  core::T_sp cur((gctools::Tagged)list);
  va_list va;
  va_start(va, len);
  for (; len != 0; --len) {
    core::Cons_sp cons = gc::As<core::Cons_sp>(cur);
    cons->rplaca(core::T_sp(va_arg(va, T_O*)));
    cur = cons->cdr();
  }
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_array(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* telement_type,
                               core::T_O* tdimensions) {
  NO_UNWIND_BEGIN();
  core::T_sp element_type(telement_type);
  core::List_sp dimensions((gctools::Tagged)tdimensions);
  core::T_sp val;
  if (core::cl__length(dimensions) == 1) // vector
  {
    val = core::core__make_vector(element_type, oCar(dimensions).unsafe_fixnum());
  } else {
    val = core::core__make_mdarray(dimensions, element_type);
  }
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

void ltvc_setf_row_major_aref(gctools::GCRootsInModule* holder, core::T_O* array_t, size_t row_major_index, core::T_O* value_t) {
  NO_UNWIND_BEGIN();
  core::T_sp tarray((gctools::Tagged)array_t);
  core::Array_sp array = gc::As<core::Array_sp>(tarray);
  array->rowMajorAset(row_major_index, core::T_sp(value_t));
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_hash_table(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* test_t) {
  NO_UNWIND_BEGIN();
  LTVCRETURN holder->setTaggedIndex(tag, index, core::HashTable_O::create(core::T_sp(test_t)).tagged_());
  NO_UNWIND_END();
}

void ltvc_setf_gethash(gctools::GCRootsInModule* holder, core::T_O* hash_table_t, core::T_O* key_index_t,
                       core::T_O* value_index_t) {
  NO_UNWIND_BEGIN();
  core::HashTable_sp hash_table = gctools::As<HashTable_sp>(core::T_sp(hash_table_t));
  core::T_sp key(key_index_t);
  core::T_sp value(value_index_t);
  hash_table->hash_table_setf_gethash(key, value);
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_fixnum(gctools::GCRootsInModule* holder, char tag, size_t index, int64_t val) {
  NO_UNWIND_BEGIN();
  core::T_sp v = clasp_make_fixnum(val);
  LTVCRETURN holder->setTaggedIndex(tag, index, v.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_next_bignum(gctools::GCRootsInModule* holder, char tag, size_t index, T_O* bignum) {
  NO_UNWIND_BEGIN();
  core::T_sp val = core::T_sp(bignum);
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_bitvector(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* bitvector_string_t) {
  NO_UNWIND_BEGIN();
  core::SimpleBaseString_sp bitvector_string = gctools::As<core::SimpleBaseString_sp>(core::T_sp(bitvector_string_t));
  core::T_sp val = core::SimpleBitVector_O::make(bitvector_string->get_std_string());
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_symbol(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* name_t, core::T_O* package_t) {
  NO_UNWIND_BEGIN();
  core::T_sp package((gctools::Tagged)package_t);
  core::SimpleString_sp symbol_name((gctools::Tagged)name_t);
  core::Symbol_sp sym;
  if (package.notnilp()) {
    sym = gctools::As<Package_sp>(package)->intern(symbol_name);
  } else {
    sym = core::Symbol_O::create(symbol_name);
  }
  core::T_sp val = sym;
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_character(gctools::GCRootsInModule* holder, char tag, size_t index, uintptr_t val) {
  NO_UNWIND_BEGIN();
  core::T_sp v = clasp_make_character(val);
  LTVCRETURN holder->setTaggedIndex(tag, index, v.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_base_string(gctools::GCRootsInModule* holder, char tag, size_t index, const char* str) {
  NO_UNWIND_BEGIN();
  core::T_sp v = core::SimpleBaseString_O::make(str);
  LTVCRETURN holder->setTaggedIndex(tag, index, v.tagged_());
  NO_UNWIND_END();
}
};

extern "C" {

LtvcReturnVoid ltvc_make_pathname(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* host_t, core::T_O* device_t,
                                  core::T_O* directory_t, core::T_O* name_t, core::T_O* type_t, core::T_O* version_t) {
  NO_UNWIND_BEGIN();
  core::T_sp val =
      core::Pathname_O::makePathname(core::T_sp(host_t), core::T_sp(device_t), core::T_sp(directory_t), core::T_sp(name_t),
                                     core::T_sp(type_t), core::T_sp(version_t), kw::_sym_local, core::T_sp(host_t).notnilp());
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_function_description(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* sourcePathname_t,
                                              core::T_O* functionName_t, core::T_O* lambdaList_t, core::T_O* docstring_t,
                                              core::T_O* declares_t, size_t lineno, size_t column, size_t filepos) {
  NO_UNWIND_BEGIN();
  core::FunctionDescription_sp val =
      core::makeFunctionDescription(core::T_sp(functionName_t), core::T_sp(lambdaList_t), core::T_sp(docstring_t),
                                    core::T_sp(declares_t), core::T_sp(sourcePathname_t), lineno, column, filepos);
  //  printf("%s:%d:%s Created FunctionDescription_sp @%p entry_point = %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)val.raw_(),
  //  (void*)llvm_func);
  if (!gc::IsA<core::FunctionDescription_sp>(val)) {
    SIMPLE_ERROR("The object is not a FunctionDescription {}", core::_rep_(val));
  }
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_local_entry_point(gctools::GCRootsInModule* holder, char tag, size_t index, size_t functionIndex,
                                           core::T_O* functionDescription_t) {
  NO_UNWIND_BEGIN();
  ClaspCoreFunction llvm_func = (ClaspCoreFunction)holder->lookup_function(functionIndex);
  core::FunctionDescription_sp fdesc((gctools::Tagged)functionDescription_t);
  core::CoreFun_sp simpleFun = core::makeCoreFun(fdesc, llvm_func);
  LTVCRETURN holder->setTaggedIndex(tag, index, simpleFun.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_global_entry_point(gctools::GCRootsInModule* holder, char tag, size_t index, size_t functionIndex0,
                                            core::T_O* functionDescription_t, core::T_O* core_t) {
  NO_UNWIND_BEGIN();
  core::CoreFun_sp localEntryPoint((gctools::Tagged)core_t);
  core::FunctionDescription_sp fdesc((gctools::Tagged)functionDescription_t);
  core::ClaspXepTemplate xep;
  for (size_t ii = 0; ii < core::ClaspXepFunction::Entries; ++ii) {
    xep._EntryPoints[ii] = (ClaspXepAnonymousFunction)holder->lookup_function(functionIndex0 + ii);
  }
  core::SimpleCoreFun_sp simpleFun = core::makeSimpleCoreFun(fdesc, xep, localEntryPoint);
  LTVCRETURN holder->setTaggedIndex(tag, index, simpleFun.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_ensure_fcell(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* fname) {
  NO_UNWIND_BEGIN();
  T_sp tfname((gctools::Tagged)fname);
  FunctionCell_sp fcell = core__ensure_function_cell(tfname);
  LTVCRETURN holder->setTaggedIndex(tag, index, fcell.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_ensure_vcell(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* vname) {
  NO_UNWIND_BEGIN();
  T_sp tvname((gctools::Tagged)vname);
  VariableCell_sp vcell = gc::As_assert<Symbol_sp>(tvname)->ensureVariableCell();
  LTVCRETURN holder->setTaggedIndex(tag, index, vcell.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_package(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* package_name_t) {
  NO_UNWIND_BEGIN();
  core::SimpleString_sp package_name((gctools::Tagged)package_name_t);
  core::T_sp tpkg = _lisp->findPackageGlobal(package_name, false);
  if (tpkg.nilp()) {
    // If we don't find the package - just make it
    // a more comprehensive defpackage should be coming
    tpkg = _lisp->makePackage(package_name->get_std_string(), std::list<std::string>(), std::list<std::string>());
  }
  core::T_sp val = tpkg;
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_random_state(gctools::GCRootsInModule* holder, char tag, size_t index, core::T_O* random_state_string_t) {
  NO_UNWIND_BEGIN();
  core::SimpleBaseString_sp random_state_string((gctools::Tagged)random_state_string_t);
  core::RandomState_sp rs = core::RandomState_O::create();
  rs->random_state_set(random_state_string->get_std_string());
  core::T_sp val = rs;
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_binary16(gctools::GCRootsInModule* holder, char tag, size_t index, core::short_float_t f) {
  NO_UNWIND_BEGIN();
  core::T_sp val = clasp_make_single_float(f);
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_binary32(gctools::GCRootsInModule* holder, char tag, size_t index, core::single_float_t f) {
  NO_UNWIND_BEGIN();
  core::T_sp val = clasp_make_single_float(f);
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_binary64(gctools::GCRootsInModule* holder, char tag, size_t index, core::double_float_t f) {
  NO_UNWIND_BEGIN();
  core::T_sp val = DoubleFloat_O::create(f);
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_binary80(gctools::GCRootsInModule* holder, char tag, size_t index, core::long_float_t f) {
  NO_UNWIND_BEGIN();
  core::T_sp val = LongFloat_O::create(f);
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

LtvcReturnVoid ltvc_make_binary128(gctools::GCRootsInModule* holder, char tag, size_t index, core::long_float_t f) {
  NO_UNWIND_BEGIN();
  core::T_sp val = LongFloat_O::create(f);
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
  NO_UNWIND_END();
}

gctools::Tagged ltvc_lookup_literal(gctools::GCRootsInModule* holder, size_t index) {
  return holder->getTaggedIndex(LITERAL_TAG_CHAR, index);
}

LtvcReturnVoid ltvc_set_mlf_creator_funcall(gctools::GCRootsInModule* holder, char tag, size_t index, size_t simpleFunIndex,
                                            const char* name) {
  return ltvc_set_ltv_funcall(holder, tag, index, simpleFunIndex, name);
}

LtvcReturnVoid ltvc_mlf_init_funcall(gctools::GCRootsInModule* holder, size_t simpleFunIndex, const char* name) {
  //  printf("%s:%d:%s make entry-point-index got simpleFunIndex %lu name: %s\n", __FILE__, __LINE__, __FUNCTION__, simpleFunIndex,
  //  name );
  core::Function_sp ep((gctools::Tagged)holder->getLiteral(simpleFunIndex));
  [[maybe_unused]] auto ret = ep->funcall();
}

// Similar to the above, but puts value in the table.
LtvcReturnVoid ltvc_set_ltv_funcall(gctools::GCRootsInModule* holder, char tag, size_t index, size_t simpleFunIndex,
                                    const char* name) {
  core::SimpleFun_sp ep((gctools::Tagged)holder->getLiteral(simpleFunIndex));
#ifdef DEBUG_SLOW
  MaybeDebugStartup startup((void*)ep->_EntryPoints[1], name);
#endif
  LCC_RETURN ret = ep->funcall();
  core::T_sp res((gctools::Tagged)ret.ret0[0]);
  core::T_sp val = res;
  LTVCRETURN holder->setTaggedIndex(tag, index, val.tagged_());
}

LtvcReturnVoid ltvc_toplevel_funcall(gctools::GCRootsInModule* holder, size_t simpleFunIndex, const char* name) {
  core::SimpleFun_sp ep((gctools::Tagged)holder->getLiteral(simpleFunIndex));
#ifdef DEBUG_SLOW
  MaybeDebugStartup startup((void*)ep->_EntryPoints[1], name);
#endif
  [[maybe_unused]] LCC_RETURN ret = ep->funcall();
}
};

extern "C" {

const std::type_info& typeidCoreCatchThrow = typeid(core::CatchThrow);
const std::type_info& typeidCoreUnwind = typeid(core::Unwind);

#define LOW_LEVEL_TRACE_QUEUE_SIZE 1024
uint _LLVMLowLevelTraceQueueIn = 0;
uint _LLVMLowLevelTraceQueueWrapped = false;
uint _LLVMLowLevelTraceQueue[LOW_LEVEL_TRACE_QUEUE_SIZE];

NOINLINE void lowLevelTrace(uint traceid) {
  NO_UNWIND_BEGIN();
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

void cc_set_breakstep() { my_thread->_Breakstep = true; }
void cc_unset_breakstep() { my_thread->_Breakstep = false; }

// RAII thing to toggle breakstep while respecting nonlocal exit.
struct BreakstepToggle {
  ThreadLocalState* mthread;
  bool old_breakstep;
  BreakstepToggle(ThreadLocalState* thread, bool new_breakstep) {
    mthread = thread;
    old_breakstep = thread->_Breakstep;
    thread->_Breakstep = new_breakstep;
  }
  ~BreakstepToggle() { mthread->_Breakstep = old_breakstep; }
};

NOINLINE void cc_breakstep(core::T_O* source, void* frame) {
  unlikely_if(my_thread->_Breakstep) {
    void* bframe = my_thread->_BreakstepFrame;
    // If bframe is NULL, we are doing step-into.
    // Otherwise, we are doing step-over, and we need to check
    // if we've returned yet. bframe is the frame step-over was initiated
    // from, and lframe/frame is the caller frame.
    // We have to check here because a function being stepped over may
    // nonlocally exit past the caller, and in that situation we want to
    // resume stepping.
    // FIXME: We assume stack growth direction here.
    if (!bframe || (frame >= bframe)) {
      // Make sure we don't invoke the stepper recursively,
      // but can do so again once we're out of the Lisp interaction.
      BreakstepToggle tog(my_thread, false);
      T_sp res = core::eval::funcall(core::_sym_breakstep, T_sp((gctools::Tagged)source));
      if (res.fixnump()) {
        switch (res.unsafe_fixnum()) {
        case 0:
          goto stop_stepping;
        case 1:
          my_thread->_BreakstepFrame = NULL;
          return;
        case 2:
          my_thread->_BreakstepFrame = frame;
          return;
        }
      }
      SIMPLE_ERROR("BUG: Unknown return value from {}: {}", _rep_(core::_sym_breakstep), _rep_(res));
    } else
      return;
  stop_stepping: // outside the scope of tog
    my_thread->_Breakstep = false;
    return;
  }
}

NOINLINE void cc_breakstep_after(void* frame) {
  unlikely_if(my_thread->_Breakstep) {
    void* bframe = my_thread->_BreakstepFrame;
    // If we just stepped over, and are back after the call, switch back
    // into step-into mode. Otherwise do nothing.
    if (bframe && (frame >= bframe))
      my_thread->_BreakstepFrame = NULL;
  }
}

// FIXME: This should be [[noreturn]], but I'm not sure how to communicate to clang that the
// error calls won't return, so it complains if that's declared.
NOINLINE void cc_wrong_number_of_arguments(core::T_O* tfunction, std::size_t nargs, std::size_t min, std::size_t max) {
  Function_sp function((gctools::Tagged)tfunction);
  /* This is kind of a KLUDGE, but we use a smaller max to indicate there is no
   * limit on the number of arguments (i.e., &rest or &key).
   * Check how calls to this function are generated in cmp/arguments.lisp. */
  if (max < min)
    core::eval::funcall(cl::_sym_error, core::_sym_wrongNumberOfArguments, kw::_sym_calledFunction, function, kw::_sym_givenNargs,
                        core::make_fixnum(nargs), kw::_sym_minNargs, core::make_fixnum(min));
  else
    core::eval::funcall(cl::_sym_error, core::_sym_wrongNumberOfArguments, kw::_sym_calledFunction, function, kw::_sym_givenNargs,
                        core::make_fixnum(nargs), kw::_sym_minNargs, core::make_fixnum(min), kw::_sym_maxNargs,
                        core::make_fixnum(max));
}

/* Used in cclasp local calls. */
T_O* cc_list(size_t nargs, ...) {
  va_list args;
  va_start(args, nargs);
  ql::list result;
  for (int i = 0; i < nargs; ++i) {
    T_O* tagged_obj = ENSURE_VALID_OBJECT(va_arg(args, T_O*));
    result << gc::smart_ptr<T_O>((gc::Tagged)tagged_obj);
  }
  va_end(args);
  return result.result().raw_();
}

/* Conses up a &rest argument from the passed valist.
 * Used in cmp/arguments.lisp for the general case of functions with a &rest in their lambda list. */
__attribute__((visibility("default"))) core::T_O* cc_gatherRestArguments(Vaslist* vaslist, std::size_t nargs) {
  NO_UNWIND_BEGIN();
  ql::list result;
  for (int i = 0; i < nargs; ++i) {
    core::T_O* tagged_obj = ENSURE_VALID_OBJECT((*vaslist)[i]);
    result << gc::smart_ptr<core::T_O>((gc::Tagged)tagged_obj);
  }
  MAYBE_VERIFY_ALIGNMENT(&*(result.result()));
  return result.result().raw_();
  NO_UNWIND_END();
}

/* Like cc_gatherRestArguments, but uses a vector of conses provided by the caller-
 * intended to be stack space, for &rest parameters declared dynamic-extent. */
__attribute__((visibility("default"))) core::T_O* cc_gatherDynamicExtentRestArguments(Vaslist* vaslist, std::size_t nargs,
                                                                                      core::Cons_O* cur) {
  NO_UNWIND_BEGIN();
  core::List_sp result = Cons_sp((gctools::Tagged)gctools::tag_cons((core::Cons_O*)cur));
  if (nargs) {
    for (int i = 0; i < nargs - 1; ++i) {
      core::T_O* tagged_obj = ENSURE_VALID_OBJECT((*vaslist)[i]);
      Cons_O* next = cur + 1;
      new (cur) Cons_O(T_sp((gctools::Tagged)tagged_obj), T_sp((gctools::Tagged)gctools::tag_cons((core::Cons_O*)next)));
      cur = next;
    }
    core::T_O* tagged_obj = ENSURE_VALID_OBJECT((*vaslist)[nargs - 1]);
    new (cur) Cons_O(T_sp((gctools::Tagged)tagged_obj), nil<T_O>());
    return result.raw_();
  }
  return nil<core::T_O>().raw_();
  NO_UNWIND_END();
}

void badKeywordArgumentError(core::T_sp keyword, core::T_sp functionName, core::T_sp lambdaList) {
  if (functionName.nilp()) {
    SIMPLE_ERROR("When calling an unnamed function with the lambda list {} the bad keyword argument {} was passed",
                 _rep_(lambdaList), _rep_(keyword));
  }
  SIMPLE_ERROR("When calling {} with the lambda-list {} the bad keyword argument {} was passed", _rep_(functionName),
               _rep_(lambdaList), _rep_(keyword));
}

void cc_ifBadKeywordArgumentException(core::T_O* allowOtherKeys, core::T_O* kw, core::T_O* tclosure) {
  core::Function_sp closure((gc::Tagged)tclosure);
  if (gctools::tagged_nilp(allowOtherKeys))
    badKeywordArgumentError(core::T_sp((gc::Tagged)kw), closure->functionName(), closure->lambdaList());
}
};

extern "C" {

/*! Invoke the main functions from the main function array.
If isNullTerminatedArray is 1 then there is a NULL terminated array of functions to call.
Otherwise there is just one. */
void cc_register_startup_function(size_t index, T_OStartUp fptr) {
  core::StartUp su(core::StartUp::T_O_function, index, (void*)fptr);
  register_startup_function(su);
}
/*! Call this with an alloca pointer to keep the alloca from
being optimized away */
__attribute__((optnone, noinline)) void cc_protect_alloca(char* ptr) { (void)ptr; }

void cc_invoke_start_code_interpreter(gctools::GCRootsInModule* roots, char* start_code, size_t bytes, void* caller) {
  bool log = false;
  if (core::global_debug_start_code) {
    log = true;
    llvmo::ObjectFile_sp objectFile;
    bool found = lookupObjectFileFromEntryPoint((uintptr_t)caller, objectFile);
    if (found) {
      llvmo::SectionedAddress_sp sa = object_file_sectioned_address(caller, objectFile, false);
      llvmo::DWARFContext_sp dcontext = llvmo::DWARFContext_O::createDWARFContext(objectFile);
      const char* functionName = getFunctionNameForAddress(dcontext, sa);
      printf("%s:%d:%s start-code interpreter from caller %p -------------------------\n -------- Running start-code for %s\n",
             __FILE__, __LINE__, __FUNCTION__, caller, functionName);
    } else {
      printf("%s:%d:%s for caller %p - could not match to ObjectFile_O object\n", __FILE__, __LINE__, __FUNCTION__, caller);
    }
  }
  start_code_interpreter(roots, start_code, bytes, log);
}
};

extern "C" {

void gdb() { printf("%s:%d Set a breakpoint here to invoke gdb\n", __FILE__, __LINE__); }

void debugInspectTPtr(core::T_O* tP) {
  NO_UNWIND_BEGIN();
  core::T_sp obj = gctools::smart_ptr<core::T_O>((gc::Tagged)tP);
  printf("debugInspectTPtr@%p\n", tP);
  core::T_sp header = gctools::core__instance_stamp(obj);
  printf("debugInspectTPtr instance_stamp -> %ld\n", (long)header.unsafe_fixnum());
  printf("debugInspectTPtr obj.px_ref()=%p: %s\n", obj.raw_(), _rep_(obj).c_str());
  printf("%s:%d Insert breakpoint here if you want to inspect object\n", __FILE__, __LINE__);
  NO_UNWIND_END();
}

void debugInspect_i8STAR(void* p) { printf("debugInspect_i8STAR@%p\n", p); }

void debugInspectTPtr_detailed(core::T_O* tP) {
  core::T_sp obj = gctools::smart_ptr<core::T_O>((gc::Tagged)tP);
  printf("debugInspectTPtr@%p  obj.px_ref()=%p: %s\n", (void*)tP, obj.raw_(), _rep_(obj).c_str());
  printf("%s:%d Insert breakpoint here if you want to inspect object\n", __FILE__, __LINE__);
  core::T_sp val((gctools::Tagged)tP);
  if (core::HashTable_sp htval = val.asOrNull<core::HashTable_O>()) {
    htval->hash_table_dump();
  }
}

void debugInspectT_mv(core::T_mv obj) {
  NO_UNWIND_BEGIN();
  MultipleValues& mv = lisp_multipleValues();
  size_t size = mv.getSize();
  printf("debugInspect_return_type T_mv.val0@%p  T_mv.nvals=%zu mvarray.size=%zu\n", (obj).raw_(), (obj).number_of_values(), size);
  size = std::max(size, (size_t)(obj).number_of_values());
  for (size_t i(0); i < size; ++i) {
    printf("[%zu]->%p : %s\n", i, mv.valueGet(i, size).raw_(), _rep_(core::T_sp((gc::Tagged)mv.valueGet(i, size).raw_())).c_str());
  }
  printf("\n");
  printf("%s:%d Insert breakpoint here if you want to inspect object\n", __FILE__, __LINE__);
  printf("   debugInspectT_mv  obj= %s\n", _rep_(obj).c_str());
  printf("%s:%d Insert breakpoint here if you want to inspect object\n", __FILE__, __LINE__);
  NO_UNWIND_END();
}

void debugInspect_return_type(gctools::return_type rt) {
  NO_UNWIND_BEGIN();
  MultipleValues& mv = lisp_multipleValues();
  size_t size = mv.getSize();
  printf("debugInspect_return_type rt.ret0@%p  rt.nvals=%zu mvarray.size=%zu\n", rt.ret0[0], rt.nvals, size);
  size = std::max(size, rt.nvals);
  printf("multipleValue[0] address: %p    size address: %p\n", &my_thread->_MultipleValues, &my_thread->_MultipleValues._Size);
  for (size_t i(0); i < size; ++i) {
    printf("[%zu]->%p : %s\n", i, mv.valueGet(i, size).raw_(), _rep_(core::T_sp((gc::Tagged)mv.valueGet(i, size).raw_())).c_str());
  }
  printf("\n");
  printf("%s:%d Insert breakpoint here if you want to inspect object\n", __FILE__, __LINE__);
  NO_UNWIND_END();
}

void debugMessage(const char* msg) {
  NO_UNWIND_BEGIN();
  printf("++++++ debug-message: %s3", msg);
  NO_UNWIND_END();
}

uintptr_t debug_match_two_uintptr_t(uintptr_t x, uintptr_t y) {
  NO_UNWIND_BEGIN();
  if (x == y)
    return x;
  printf("%s:%d !!!!! in debug_match_two_uintptr_t the two pointers %p and %p don't match\n", __FILE__, __LINE__, (void*)x,
         (void*)y);
  gdb();
  UNREACHABLE();
  NO_UNWIND_END();
}

void debugPointer(const unsigned char* ptr) {
  NO_UNWIND_BEGIN();
  printf("++++++ debugPointer: %p \n", ptr);
  NO_UNWIND_END();
}

void debug_vaslistPtr(Vaslist* vargs) {
  NO_UNWIND_BEGIN();
  Vaslist* args = reinterpret_cast<Vaslist*>(gc::untag_vaslist((void*)vargs));
  printf("++++++ debug_vaslist: _args @%p \n", args->args());
  printf("++++++ debug_vaslist: _nargs %lu\n", args->nargs());
  NO_UNWIND_END();
}

void debugSymbolPointer(core::Symbol_sp* ptr) { printf("++++++ debugSymbolPointer: %s\n", _rep_(*ptr).c_str()); }

void debugSymbolValue(core::Symbol_sp sym) { printf("+++++ debugSymbolValue: %s\n", _rep_(core::cl__symbol_value(sym)).c_str()); }

void debugPrintI32(int i32) {
  NO_UNWIND_BEGIN();
  // printf("+++DBG-I32[%d]\n", i32);
  // fflush(stdout);
  NO_UNWIND_END();
}

void debugPrint_blockFrame(core::T_O* handle) {
  NO_UNWIND_BEGIN();
  printf("%s:%d debugPrint_blockFrame handle %p\n", __FILE__, __LINE__, handle);
  fflush(stdout);
  NO_UNWIND_END();
}

void debugPrint_blockHandleReturnFrom(unsigned char* exceptionP, void* handle) {
  NO_UNWIND_BEGIN();
  core::Unwind& returnFrom = *reinterpret_cast<core::Unwind*>(exceptionP);
  printf("%s:%d debugPrint_blockHandleReturnFrom return-from handle %p    block handle %p\n", __FILE__, __LINE__,
         returnFrom.getFrame(), handle);
  if (returnFrom.getFrame() == handle) {
    printf("%s:%d debugPrint_blockHandleReturnFrom handles match!\n", __FILE__, __LINE__);
    fflush(stdout);
    return;
  }
  NO_UNWIND_END();
}

void debugPrint_size_t(size_t v) {
  NO_UNWIND_BEGIN();
  printf("+++DBG-size_t[%lu/%lx]\n", v, v);
  NO_UNWIND_END();
}

void cc_verify_tag(size_t uid, core::T_O* ptr, size_t tag) {
  if (((uintptr_t)ptr & 0x7) != tag) {
    printf("%s:%d:%s id: %lu - the ptr %p does not have the necessary tag 0x%lx - aborting\n", __FILE__, __LINE__, __FUNCTION__,
           uid, ptr, tag);
    abort();
  }
}

void debug_memory(size_t num, core::T_O** vector) {
  NO_UNWIND_BEGIN();
  printf("+++%s num: %lu\n", __FUNCTION__, num);
  for (size_t ii = 0; ii < num; ii++) {
    core::T_sp vobj((gctools::Tagged)(vector[ii]));
    printf("...  vector[%lu]@%p -> %p %s\n", ii, (void*)&vector[ii], vobj.raw_(), _rep_(vobj).c_str());
  }
  NO_UNWIND_END();
}
};

template <typename Ty_O, class... ARGS> gctools::smart_ptr<Ty_O> InitObject(void* space, ARGS&&... args) {
  ASSERT(((uintptr_t)(space)&0x7) == 0); // copied from cc_stack_enclose
  gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(space);
  const gctools::Header_s::BadgeStampWtagMtag c_header = gctools::Header_s::BadgeStampWtagMtag::make<Ty_O>();
#ifdef DEBUG_GUARD
  size_t size = gctools::sizeof_with_header<Ty_O>();
  new (header)(typename gctools::GCHeader<Ty_O>::HeaderType)(c_header, size, 0, size);
#else
  new (header)(typename gctools::GCHeader<Ty_O>::HeaderType)(c_header);
#endif
  auto obj = gctools::HeaderPtrToGeneralPtr<typename gctools::smart_ptr<Ty_O>::Type>(space);
  new (obj)(typename gctools::smart_ptr<Ty_O>::Type)(std::forward<ARGS>(args)...);
  return gctools::smart_ptr<Ty_O>(obj);
}

extern "C" {

core::T_O* cc_createAndPushBlockDynenv(void* cspace, void* frame, jmp_buf* target) {
  core::BlockDynEnv_sp block = BlockDynEnv_O::create(frame, target);
  auto newstack = InitObject<core::Cons_O>(cspace, block, my_thread->dynEnvStackGet());
  my_thread->dynEnvStackSet(newstack);
  return block.raw_();
}

core::T_O* cc_createAndPushTagbodyDynenv(void* cspace, void* frame, jmp_buf* target) {
  core::TagbodyDynEnv_sp tb = TagbodyDynEnv_O::create(frame, target);
  auto newstack = InitObject<core::Cons_O>(cspace, tb, my_thread->dynEnvStackGet());
  my_thread->dynEnvStackSet(newstack);
  return tb.raw_();
}

core::T_O* cc_initializeAndPushCleanupDynenv(void* space, void* cspace, jmp_buf* target) {
  NO_UNWIND_BEGIN();
  auto newde = InitObject<core::UnwindProtectDynEnv_O>(space, target);
  auto newstack = InitObject<core::Cons_O>(cspace, newde, my_thread->dynEnvStackGet());
  my_thread->dynEnvStackSet(newstack);
  return newde.raw_();
  NO_UNWIND_END();
}

core::T_O* cc_initializeAndPushBindingDynenv(void* space, void* cspace, core::T_O* cell, core::T_O* old) {
  NO_UNWIND_BEGIN();
  core::T_sp told((gc::Tagged)old);
  core::T_sp tcell((gc::Tagged)cell);
  core::VariableCell_sp rcell = gc::As_assert<VariableCell_sp>(tcell);
  auto newde = InitObject<core::BindingDynEnv_O>(space, rcell, told);
  auto newstack = InitObject<core::Cons_O>(cspace, newde, my_thread->dynEnvStackGet());
  my_thread->dynEnvStackSet(newstack);
  return newde.raw_();
  NO_UNWIND_END();
}

T_O* cc_get_dynenv_stack() {
  NO_UNWIND_BEGIN();
  return my_thread->dynEnvStackGet().raw_();
  NO_UNWIND_END();
}

void cc_set_dynenv_stack(T_O* dynenv_stack) {
  NO_UNWIND_BEGIN();
  T_sp destack((gctools::Tagged)dynenv_stack);
  my_thread->dynEnvStackSet(destack);
  NO_UNWIND_END();
}

void* cc_dynenv_frame(T_O* dynenv) {
  NO_UNWIND_BEGIN();
  T_sp tde((gctools::Tagged)dynenv);
  LexDynEnv_sp dde = gc::As_unsafe<LexDynEnv_sp>(tde);
  return dde->frame;
  NO_UNWIND_END();
}

[[noreturn]] void cc_sjlj_unwind(T_O* dde, size_t index) {
  T_sp tde((gctools::Tagged)dde);
  sjlj_unwind(gc::As<LexDynEnv_sp>(tde), index);
}

[[noreturn]] void cc_throw(T_O* tagg) {
  T_sp tag((gctools::Tagged)tagg);
  sjlj_throw(tag);
}

T_O* cc_get_unwind_dest() {
  NO_UNWIND_BEGIN();
  return my_thread->_UnwindDest.raw_();
  NO_UNWIND_END();
}

void cc_set_unwind_dest(T_O* dest) {
  NO_UNWIND_BEGIN();
  T_sp tdest((gctools::Tagged)dest);
  my_thread->_UnwindDest = tdest;
  NO_UNWIND_END();
}

size_t cc_get_unwind_dest_index() {
  NO_UNWIND_BEGIN();
  return my_thread->_UnwindDestIndex;
  NO_UNWIND_END();
}

void cc_set_unwind_dest_index(size_t ind) {
  NO_UNWIND_BEGIN();
  my_thread->_UnwindDestIndex = ind;
  NO_UNWIND_END();
}

[[noreturn]] void cc_sjlj_continue_unwinding() { sjlj_continue_unwinding(); }

void cc_error_bugged_come_from(size_t id) { SIMPLE_ERROR("BUG: Nonlocal entry frame could not match go-index {}", id); }

void debugFileScopeHandle(int* sourceFileInfoHandleP) {
  NO_UNWIND_BEGIN();
  int sfindex = *sourceFileInfoHandleP;
  core::Fixnum_sp fn = core::make_fixnum(sfindex);
  FileScope_sp sfi = gc::As<FileScope_sp>(core::core__file_scope(fn));
  printf("%s:%d debugFileScopeHandle[%d] --> %s\n", __FILE__, __LINE__, sfindex, _rep_(sfi).c_str());
  NO_UNWIND_END();
}

void cc_signal_interrupts() {
  gctools::handle_all_queued_interrupts();
}
};

extern "C" {
void saveToMultipleValue0(core::T_mv* mvP) {
  NO_UNWIND_BEGIN();
  MultipleValues& mv = lisp_multipleValues();
  mv.saveToMultipleValue0(*mvP);
  NO_UNWIND_END();
}

gctools::return_type restoreFromMultipleValue0() {
  NO_UNWIND_BEGIN();
  core::MultipleValues& mv = core::lisp_multipleValues();
  gctools::return_type result(mv.operator[](0), mv.getSize());
  return result;
  NO_UNWIND_END();
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

// #define DEBUG_CC

core::T_O* cc_overflowed_signed_bignum(int64_t add_over) {
  mp_limb_t limb;
  mp_size_t len;
  if (add_over < 0) { // positive
    len = 1;
    limb = ((uint64_t)add_over) >> 2;
  } else if (add_over > 0) { // negative
    len = -1;
    limb = ((uint64_t)((~add_over) + 1)) >> 2;
  } else { // add_over == 0
    len = -1;
    limb = 0x4000000000000000;
  }
  return core::Bignum_O::create_from_limbs(len, limb, true).raw_();
}

core::T_O* cc_variableCellValue(core::T_O* cell) {
  core::VariableCell_sp tcell = gctools::smart_ptr<core::VariableCell_O>((gc::Tagged)cell);
  return tcell->value().raw_();
}

void cc_set_variableCellValue(core::T_O* cell, core::T_O* val) {
  NO_UNWIND_BEGIN();
  core::VariableCell_sp tcell = gctools::smart_ptr<core::VariableCell_O>((gc::Tagged)cell);
  core::T_sp tval((gc::Tagged)val);
  tcell->set_value(tval);
  NO_UNWIND_END();
}

uint32_t cc_getCellTLIndex(core::T_O* sym) {
  NO_UNWIND_BEGIN();
  core::VariableCell_sp s = gctools::smart_ptr<core::VariableCell_O>((gc::Tagged)sym);
  return s->ensureBindingIndex();
  NO_UNWIND_END();
}

// Set the thread local value to be the new value, and return the old value
// for replacement later. The old value may be an unboundedness marker, but
// tlindex must be a real index.
core::T_O* cc_specialBind(uint32_t tlindex, core::T_O* val) {
  NO_UNWIND_BEGIN();
  core::T_sp v = gctools::smart_ptr<core::T_O>((gc::Tagged)val);
  auto& bindings = my_thread->_Bindings;
  T_sp old = bindings.thread_local_value(tlindex);
  bindings.set_thread_local_value(v, tlindex);
  return old.raw_();
  NO_UNWIND_END();
}

// Reset the thread local value to whatever it was.
void cc_specialUnbind(uint32_t tlindex, core::T_O* old) {
  NO_UNWIND_BEGIN();
  core::T_sp o = gctools::smart_ptr<core::T_O>((gc::Tagged)old);
  auto& bindings = my_thread->_Bindings;
  bindings.set_thread_local_value(o, tlindex);
  NO_UNWIND_END();
}

SYMBOL_EXPORT_SC_(KeywordPkg, datum);
SYMBOL_EXPORT_SC_(KeywordPkg, expected_type);
void cc_error_type_error(T_O* datum, T_O* expected_type) {
  T_sp tdatum((gctools::Tagged)datum);
  T_sp texpected_type((gctools::Tagged)expected_type);
  core::eval::funcall(cl::_sym_error, cl::_sym_type_error, kw::_sym_datum, tdatum, kw::_sym_expected_type, texpected_type);
}

void cc_error_array_out_of_bounds(T_O* index, T_O* expected_type, T_O* array) {
  core::T_sp tindex((gctools::Tagged)index);
  core::T_sp texpected_type((gctools::Tagged)expected_type);
  core::T_sp tarray((gctools::Tagged)array);
  core::eval::funcall(cl::_sym_error, core::_sym_array_out_of_bounds, kw::_sym_datum, tindex, kw::_sym_expected_type,
                      texpected_type, kw::_sym_array, tarray);
}

SYMBOL_EXPORT_SC_(CorePkg, case_failure);
SYMBOL_EXPORT_SC_(KeywordPkg, possibilities);
NEVER_OPTIMIZE void cc_etypecase_error(T_O* datum, T_O* possibilities) {
  core::T_sp tdatum((gctools::Tagged)datum);
  core::T_sp tpossibilities((gctools::Tagged)possibilities);
  core::T_sp expected = core::Cons_O::create(cl::_sym_or, tpossibilities);
  core::eval::funcall(cl::_sym_error, core::_sym_case_failure,
                      kw::_sym_datum, tdatum,
                      kw::_sym_expected_type, expected,
                      kw::_sym_name, cl::_sym_etypecase,
                      kw::_sym_possibilities, tpossibilities);
}

core::T_O* cc_enclose(core::T_O* simpleFunInfo, std::size_t numCells) {
  core::T_sp tsimpleFun((gctools::Tagged)simpleFunInfo);
  core::SimpleFun_sp simpleFun = gc::As<SimpleFun_sp>(tsimpleFun);
  gctools::smart_ptr<core::Closure_O> functoid =
      gctools::GC<core::Closure_O>::allocate_container<gctools::RuntimeStage>(false, numCells, simpleFun);
  return functoid.raw_();
}

void cc_initialize_closure(core::T_O* functoid, std::size_t numCells, ...) {
  core::T_O* p;
  va_list argp;
  va_start(argp, numCells);
  int idx = 0;
  Closure_sp closure((gctools::Tagged)functoid);
  for (; numCells; --numCells) {
    p = ENSURE_VALID_OBJECT(va_arg(argp, core::T_O*));
    (*closure)[idx] = gctools::smart_ptr<core::T_O>((gc::Tagged)p);
    ++idx;
  }
  va_end(argp);
}

LCC_RETURN cc_call_multipleValueOneFormCallWithRet0(core::Function_O* tfunc, gctools::return_type ret0) {
  ASSERTF(gctools::tagged_generalp(tfunc), "The argument {} does not have a general tag!", (void*)tfunc);
  MAKE_STACK_FRAME(callargs, ret0.nvals);
  size_t idx(0);
  gctools::fill_frame_multiple_value_return(callargs, idx, ret0);
#ifdef DEBUG_VALUES
  if (_sym_STARdebug_valuesSTAR && _sym_STARdebug_valuesSTAR->boundP() && _sym_STARdebug_valuesSTAR->symbolValue().notnilp()) {
    for (size_t i(0); i < ret0.nvals; ++i) {
      core::T_sp mvobj((gctools::Tagged)(*callargs)[i]);
      printf("%s:%d  ....  cc_call_multipleValueOneFormCall[%lu] -> %s\n", __FILE__, __LINE__, i, _rep_(mvobj).c_str());
    }
  }
#endif
  core::Function_sp func((gctools::Tagged)tfunc);
  return func->apply_raw(ret0.nvals, callargs->arguments(0));
}

T_O* cc_mvcGatherRest(size_t nret, T_O* ret0, size_t nstart) {
  MultipleValues& mv = core::lisp_multipleValues();
  ql::list result;
  if (nret == 0)
    return nil<T_O>().raw_();
  else {
    if (nstart == 0) {
      result << gc::smart_ptr<T_O>((gc::Tagged)ret0);
      nstart = 1;
    }
    for (size_t i = nstart; i < nret; ++i) {
      T_O* tagged_obj = ENSURE_VALID_OBJECT(mv[i]);
      result << gc::smart_ptr<T_O>((gc::Tagged)tagged_obj);
    }
    return result.result().raw_();
  }
}

T_O* cc_mvcGatherRest2(T_O** values, size_t nvalues) {
  if (nvalues == 0)
    return nil<T_O>().raw_();
  else {
    ql::list result;
    for (size_t i = 0; i < nvalues; ++i) {
      T_O* tagged_obj = ENSURE_VALID_OBJECT(values[i]);
      result << gc::smart_ptr<T_O>((gc::Tagged)tagged_obj);
    }
    return result.result().raw_();
  }
}

void cc_oddKeywordException(core::T_O* tclosure) {
  core::Function_sp closure((gc::Tagged)tclosure);
  throwOddKeywordsError(closure);
}

T_O** cc_multipleValuesArrayAddress() {
  NO_UNWIND_BEGIN();
  return lisp_multipleValues().returnValues(0);
  NO_UNWIND_END();
}

void cc_saveMultipleValue0(core::T_mv result) {
  NO_UNWIND_BEGIN();
  MultipleValues& mv = lisp_multipleValues();
  mv.saveToMultipleValue0(result);
  NO_UNWIND_END();
}

gctools::return_type cc_restoreMultipleValue0() {
  NO_UNWIND_BEGIN();
  MultipleValues& mv = lisp_multipleValues();
  size_t nret = mv.getSize();
  return gctools::return_type((nret == 0) ? nil<T_O>().raw_() : mv[0], nret);
  NO_UNWIND_END();
}

// cc_{save,load}_values are intended for code that does something,
// then some other things, then returns values from the first thing.
// e.g. multiple-value-prog1, unwind-protect without nonlocal exit
void cc_save_values(size_t nvals, T_O* primary, T_O** vector) {
  NO_UNWIND_BEGIN();
  returnTypeSaveToTemp(nvals, primary, vector);
#ifdef DEBUG_VALUES
  if (_sym_STARdebug_valuesSTAR && _sym_STARdebug_valuesSTAR->boundP() && _sym_STARdebug_valuesSTAR->symbolValue().notnilp()) {
    printf("%s:%d:%s nvals = %lu\n", __FILE__, __LINE__, __FUNCTION__, nvals);
    for (size_t i(0); i < nvals; ++i) {
      core::T_sp mvobj((gctools::Tagged)(vector[i]));
      printf("%s:%d  ....  vector[%lu] -> %s\n", __FILE__, __LINE__, i, _rep_(mvobj).c_str());
    }
  }
#endif
  NO_UNWIND_END();
}

gctools::return_type cc_load_values(size_t nvals, T_O** vector) {
  NO_UNWIND_BEGIN();
  return returnTypeLoadFromTemp(nvals, vector);
  NO_UNWIND_END();
}

// cc_nvalues and cc_{save,load}_all_values are for unwind protect cleanup.
// See analogous C++ code in evaluator.cc: sp_unwindProtect.
size_t cc_nvalues() {
  NO_UNWIND_BEGIN();
  MultipleValues& mv = lisp_multipleValues();
  return mv.getSize();
  NO_UNWIND_END();
}

void cc_save_all_values(size_t nvals, T_O** vector) {
  NO_UNWIND_BEGIN();
  lisp_multipleValues().saveToTemp(nvals, vector);
  NO_UNWIND_END();
}

void cc_load_all_values(size_t nvals, T_O** vector) {
  NO_UNWIND_BEGIN();
  lisp_multipleValues().loadFromTemp(nvals, vector);
  NO_UNWIND_END();
}

size_t cc_landingpadUnwindMatchFrameElseRethrow(char* exceptionP, void* thisFrame) {
  core::Unwind* unwindP = reinterpret_cast<core::Unwind*>(exceptionP);
  if (unwindP->getFrame() == thisFrame) {
    std::chrono::time_point<std::chrono::high_resolution_clock> now = std::chrono::high_resolution_clock::now();
    my_thread_low_level->_unwind_time += (now - my_thread_low_level->_start_unwind);
    return unwindP->index();
  }
  if ((uintptr_t)unwindP->getFrame() < (uintptr_t)thisFrame) {
    printf("%s:%d:%s You blew past the frame unwindP->getFrame()->%p  thisFrame->%p\n", __FILE__, __LINE__, __FUNCTION__,
           (void*)unwindP->getFrame(), (void*)thisFrame);
    abort();
  }
  // throw * unwindP;
  throw;
}

// These entry point redirect functions are used as the XEP entries for
// compiled functions that don't do anything interesting for a given arity
// (like, they just signal an error). See generate-function-for-arity-p in
// cmpir.lisp, and the redirect functionality in cmpliteral.lisp.
// All they do is call the general entry point (via apply_raw).

LCC_RETURN_RAW general_entry_point_redirect_0(core::T_O* closure) {
  return gctools::untag_general<core::Function_O*>((core::Function_O*)closure)->apply_raw(0, NULL);
}

LCC_RETURN_RAW general_entry_point_redirect_1(core::T_O* closure, core::T_O* farg0) {
  MAKE_STACK_FRAME(frame, 1);
  gctools::fill_frame_one_indexed(frame, 0, farg0);
  return gctools::untag_general<core::Function_O*>((core::Function_O*)closure)->apply_raw(1, frame->arguments(0));
}

LCC_RETURN_RAW general_entry_point_redirect_2(core::T_O* closure, core::T_O* farg0, core::T_O* farg1) {
  MAKE_STACK_FRAME(frame, 2);
  gctools::fill_frame_one_indexed(frame, 0, farg0);
  gctools::fill_frame_one_indexed(frame, 1, farg1);
  return gctools::untag_general<core::Function_O*>((core::Function_O*)closure)->apply_raw(2, frame->arguments(0));
}

LCC_RETURN_RAW general_entry_point_redirect_3(core::T_O* closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2) {
  MAKE_STACK_FRAME(frame, 3);
  gctools::fill_frame_one_indexed(frame, 0, farg0);
  gctools::fill_frame_one_indexed(frame, 1, farg1);
  gctools::fill_frame_one_indexed(frame, 2, farg2);
  return gctools::untag_general<core::Function_O*>((core::Function_O*)closure)->apply_raw(3, frame->arguments(0));
}

LCC_RETURN_RAW general_entry_point_redirect_4(core::T_O* closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2,
                                              core::T_O* farg3) {
  MAKE_STACK_FRAME(frame, 4);
  gctools::fill_frame_one_indexed(frame, 0, farg0);
  gctools::fill_frame_one_indexed(frame, 1, farg1);
  gctools::fill_frame_one_indexed(frame, 2, farg2);
  gctools::fill_frame_one_indexed(frame, 3, farg3);
  return gctools::untag_general<core::Function_O*>((core::Function_O*)closure)->apply_raw(4, frame->arguments(0));
}

LCC_RETURN_RAW general_entry_point_redirect_5(core::T_O* closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2,
                                              core::T_O* farg3, core::T_O* farg4) {
  MAKE_STACK_FRAME(frame, 5);
  gctools::fill_frame_one_indexed(frame, 0, farg0);
  gctools::fill_frame_one_indexed(frame, 1, farg1);
  gctools::fill_frame_one_indexed(frame, 2, farg2);
  gctools::fill_frame_one_indexed(frame, 3, farg3);
  gctools::fill_frame_one_indexed(frame, 4, farg4);
  return gctools::untag_general<core::Function_O*>((core::Function_O*)closure)->apply_raw(5, frame->arguments(0));
}

LCC_RETURN_RAW general_entry_point_redirect_6(core::T_O* closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2,
                                              core::T_O* farg3, core::T_O* farg4, core::T_O* farg5) {
  MAKE_STACK_FRAME(frame, 6);
  gctools::fill_frame_one_indexed(frame, 0, farg0);
  gctools::fill_frame_one_indexed(frame, 1, farg1);
  gctools::fill_frame_one_indexed(frame, 2, farg2);
  gctools::fill_frame_one_indexed(frame, 3, farg3);
  gctools::fill_frame_one_indexed(frame, 4, farg4);
  gctools::fill_frame_one_indexed(frame, 5, farg5);
  return gctools::untag_general<core::Function_O*>((core::Function_O*)closure)->apply_raw(6, frame->arguments(0));
}

LCC_RETURN_RAW general_entry_point_redirect_7(core::T_O* closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2,
                                              core::T_O* farg3, core::T_O* farg4, core::T_O* farg5, core::T_O* farg6) {
  MAKE_STACK_FRAME(frame, 7);
  gctools::fill_frame_one_indexed(frame, 0, farg0);
  gctools::fill_frame_one_indexed(frame, 1, farg1);
  gctools::fill_frame_one_indexed(frame, 2, farg2);
  gctools::fill_frame_one_indexed(frame, 3, farg3);
  gctools::fill_frame_one_indexed(frame, 4, farg4);
  gctools::fill_frame_one_indexed(frame, 5, farg5);
  gctools::fill_frame_one_indexed(frame, 6, farg6);
  return gctools::untag_general<core::Function_O*>((core::Function_O*)closure)->apply_raw(7, frame->arguments(0));
}
};

#pragma GCC visibility pop
