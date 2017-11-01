/*
    File: intrinsics.cc
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
#include <typeinfo>
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/bignum.h>
#include <clasp/core/functor.h>
#include <clasp/core/character.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/array.h>
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
#include <clasp/core/random.h>
#include <clasp/core/primitives.h>
#include <clasp/core/posixTime.h>
#include <clasp/core/numbers.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/symbolTable.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/intrinsics.h>
#include <clasp/gctools/gc_interface.fwd.h>

using namespace core;

#pragma GCC visibility push(default)

namespace llvmo {
[[noreturn]] __attribute__((optnone))  void intrinsic_error(ErrorCode err, core::T_sp arg0, core::T_sp arg1, core::T_sp arg2) {
  switch (err) {
  case noFunctionBoundToSymbol:
    {
      core::Symbol_sp sym(gc::As<core::Symbol_sp>(arg0));
      SIMPLE_ERROR(BF("There is no function bound to the symbol %s") % sym->fullName());
    }
    break;
  case badKeywordArgument:
      SIMPLE_ERROR(BF("Bad keyword argument %s") % _rep_(arg0));
  case couldNotCoerceToClosure:
      SIMPLE_ERROR(BF(" symbol %s") % _rep_(arg0));
  case destinationMustBeActivationFrame:
      SIMPLE_ERROR(BF("Destination must be ActivationFrame"));
  case invalidIndexForFunctionFrame:
      SIMPLE_ERROR(BF("Invalid index[%d] for FunctionFrame(size=%d)") % _rep_(arg0) % _rep_(arg1));
  case unboundSymbolValue:
    {
      core::Symbol_sp sym = gc::As<core::Symbol_sp>(arg0);
      SIMPLE_ERROR(BF("The symbol %s is unbound") % sym->fullName() );
    };
  case unboundSymbolFunction:
    {
      core::Symbol_sp sym = gc::As<core::Symbol_sp>(arg0);
      SIMPLE_ERROR(BF("The symbol %s has no function bound to it") % sym->fullName() );
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

void cc_initialize_gcroots_in_module(gctools::GCRootsInModule* holder, core::T_O** root_address, size_t num_roots, gctools::Tagged initial_data )
{NO_UNWIND_BEGIN();
  initialize_gcroots_in_module(holder,root_address,num_roots,initial_data);
  NO_UNWIND_END();
}

void cc_shutdown_gcroots_in_module(gctools::GCRootsInModule* holder)
{NO_UNWIND_BEGIN();
  shutdown_gcroots_in_module(holder);
  NO_UNWIND_END();
}


void ltvc_assign_source_file_info_handle(const char *moduleName, const char *sourceDebugPathname, size_t sourceDebugOffset, int useLineno, int *sourceFileInfoHandleP) {
  NO_UNWIND_BEGIN();
  //	printf("%s:%d assignSourceFileInfoHandle %s\n", __FILE__, __LINE__, moduleName );
  core::SimpleBaseString_sp mname = core::SimpleBaseString_O::make(moduleName);
  core::SimpleBaseString_sp struename = core::SimpleBaseString_O::make(sourceDebugPathname);
  SourceFileInfo_mv sfi_mv = core::core__source_file_info(mname, struename, sourceDebugOffset, useLineno ? true : false);
  int sfindex = unbox_fixnum(gc::As<core::Fixnum_sp>(sfi_mv.valueGet_(1)));
#if 0
  if ( sfindex == 0 ) {
    printf("%s:%d Could not get a SourceFileInfoHandle for %s\n", __FILE__, __LINE__, moduleName );
  } else {
    printf("%s:%d Assigning SourceFileInfoHandle %d for %s  at sourceFileInfoHandleP@%p\n", __FILE__, __LINE__, sfindex, moduleName, sourceFileInfoHandleP );
  }
#endif
  *sourceFileInfoHandleP = sfindex;
  NO_UNWIND_END();
}

// Define what ltvc_xxxx functions return - this must match what is
//  in cmpintrinsics.lsp
typedef void LtvcReturn;
#define LTVCRETURN /* Nothing return for void */

LtvcReturn ltvc_make_nil(gctools::GCRootsInModule* holder, size_t index)
{
  NO_UNWIND_BEGIN();
  core::T_sp val = _Nil<core::T_O>();
  LTVCRETURN holder->set(index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_t(gctools::GCRootsInModule* holder, size_t index)
{
  NO_UNWIND_BEGIN();
  core::T_sp val = _lisp->_true();
  LTVCRETURN holder->set(index,val.tagged_());
  NO_UNWIND_END();
}


LtvcReturn ltvc_make_ratio(gctools::GCRootsInModule* holder, size_t index, gctools::Tagged num, gctools::Tagged denom )
{NO_UNWIND_BEGIN();
  core::T_sp val = core::Ratio_O::create(core::T_sp(num),core::T_sp(denom));
  LTVCRETURN holder->set(index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_complex(gctools::GCRootsInModule* holder, size_t index, gctools::Tagged real, gctools::Tagged imag)
{NO_UNWIND_BEGIN();
  core::Number_sp nreal((gctools::Tagged)real);
  core::Number_sp nimag((gctools::Tagged)imag);
  core::T_sp val = core::Complex_O::create(clasp_to_double(nreal),clasp_to_double(nimag));
  LTVCRETURN holder->set(index,val.tagged_());
  NO_UNWIND_END();
}


LtvcReturn ltvc_make_cons(gctools::GCRootsInModule* holder, size_t index, gctools::Tagged car, gctools::Tagged cdr)
{NO_UNWIND_BEGIN();
  core::T_sp val = core::Cons_O::create(core::T_sp(car),core::T_sp(cdr));
  LTVCRETURN holder->set(index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_nconc(gctools::GCRootsInModule* holder, size_t index,
                               gctools::Tagged front, gctools::Tagged back)
{NO_UNWIND_BEGIN();
  core::T_sp val = core::clasp_nconc(core::T_sp(front),core::T_sp(back));
  LTVCRETURN holder->set(index,val.tagged_());
  NO_UNWIND_END();
}

NOINLINE LtvcReturn ltvc_make_list(gctools::GCRootsInModule* holder, size_t index, size_t num, ... )
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
  LTVCRETURN holder->set(index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_array(gctools::GCRootsInModule* holder, size_t index,
                                gctools::Tagged telement_type,
                                gctools::Tagged tdimensions )
{NO_UNWIND_BEGIN();
  core::T_sp element_type(telement_type);
  core::List_sp dimensions(tdimensions);
  core::T_sp val;
  if (core::cl__length(dimensions) == 1) // vector
  {
    val = core::core__make_vector(element_type, oCar(dimensions).unsafe_fixnum());
  } else {
    val = core::core__make_mdarray(dimensions, element_type);
  }
  LTVCRETURN holder->set(index,val.tagged_());
  NO_UNWIND_END();
}

void ltvc_setf_row_major_aref(gctools::Tagged array_t,
                              size_t row_major_index,
                              gctools::Tagged value_t )
{NO_UNWIND_BEGIN();
  core::Array_sp array = gc::As<core::Array_sp>(core::T_sp(array_t));
  array->rowMajorAset(row_major_index,core::T_sp(value_t));
  NO_UNWIND_END();
}
  
LtvcReturn ltvc_make_hash_table(gctools::GCRootsInModule* holder, size_t index,
                                gctools::Tagged test_t )
{NO_UNWIND_BEGIN();
  LTVCRETURN holder->set(index,core::HashTable_O::create(core::T_sp(test_t)).tagged_());
  NO_UNWIND_END();
}

void ltvc_setf_gethash(gctools::Tagged hash_table_t,
                       gctools::Tagged key_index_t,
                       gctools::Tagged value_index_t )
{NO_UNWIND_BEGIN();
  core::HashTable_sp hash_table = gctools::As<HashTable_sp>(core::T_sp(hash_table_t));
  core::T_sp key(key_index_t);
  core::T_sp value(value_index_t);
  hash_table->hash_table_setf_gethash(key, value);
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_fixnum(gctools::GCRootsInModule* holder, size_t index, int64_t val)
{NO_UNWIND_BEGIN();
  core::T_sp v = clasp_make_fixnum(val);
  LTVCRETURN holder->set(index,v.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_bignum(gctools::GCRootsInModule* holder, size_t index, gctools::Tagged bignum_string_t)
{NO_UNWIND_BEGIN();
  core::SimpleBaseString_sp bignum_string = gctools::As<core::SimpleBaseString_sp>(core::T_sp(bignum_string_t));
  core::T_sp val = core::Bignum_O::make(bignum_string->get());
  LTVCRETURN holder->set(index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_bitvector(gctools::GCRootsInModule* holder, size_t index, gctools::Tagged bitvector_string_t)
{NO_UNWIND_BEGIN();
  core::SimpleBaseString_sp bitvector_string = gctools::As<core::SimpleBaseString_sp>(core::T_sp(bitvector_string_t));
  core::T_sp val = core::SimpleBitVector_O::make(bitvector_string->get());
  LTVCRETURN holder->set(index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_symbol(gctools::GCRootsInModule* holder, size_t index, gctools::Tagged name_t,
                                 gctools::Tagged package_t )
{NO_UNWIND_BEGIN();
  core::T_sp package(package_t);
  core::SimpleString_sp symbol_name(name_t);
  core::Symbol_sp sym;
  if (package.notnilp()) {
    sym = gctools::As<Package_sp>(package)->intern(symbol_name);
  } else {
    sym = core::Symbol_O::create(symbol_name);
  }
  core::T_sp val = sym;
  LTVCRETURN holder->set(index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_character(gctools::GCRootsInModule* holder, size_t index, uintptr_clasp_t val)
{NO_UNWIND_BEGIN();
  core::T_sp v = clasp_make_character(val);
  LTVCRETURN holder->set(index,v.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_base_string(gctools::GCRootsInModule* holder, size_t index, const char* str) \
{NO_UNWIND_BEGIN();
  core::T_sp v = core::SimpleBaseString_O::make(str);
  LTVCRETURN holder->set(index,v.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_pathname(gctools::GCRootsInModule* holder, size_t index, gctools::Tagged host_t,
                                   gctools::Tagged device_t,
                                   gctools::Tagged directory_t,
                                   gctools::Tagged name_t,
                                   gctools::Tagged type_t,
                                   gctools::Tagged version_t )
{NO_UNWIND_BEGIN();
  core::T_sp val = core::Pathname_O::makePathname(core::T_sp(host_t),
                                        core::T_sp(device_t),
                                        core::T_sp(directory_t),
                                        core::T_sp(name_t),
                                        core::T_sp(type_t),
                                        core::T_sp(version_t),
                                        kw::_sym_local,
                                              core::T_sp(host_t).notnilp());
  LTVCRETURN holder->set(index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_package(gctools::GCRootsInModule* holder, size_t index, gctools::Tagged package_name_t )
{
  NO_UNWIND_BEGIN();
  core::SimpleBaseString_sp package_name(package_name_t);
  core::T_sp tpkg = _lisp->findPackage(package_name->get(),false);
  if ( tpkg.nilp() ) {
    // If we don't find the package - just make it
    // a more comprehensive defpackage should be coming
    tpkg = _lisp->makePackage(package_name->get(),std::list<std::string>(), std::list<std::string>());
  }
  core::T_sp val = tpkg;
  LTVCRETURN holder->set(index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_random_state(gctools::GCRootsInModule* holder, size_t index, gctools::Tagged random_state_string_t)
{NO_UNWIND_BEGIN();
  core::SimpleBaseString_sp random_state_string(random_state_string_t);
  core::RandomState_sp rs = core::RandomState_O::create();
  rs->random_state_set(random_state_string->get());
  core::T_sp val = rs;
  LTVCRETURN holder->set(index,val.tagged_());
  NO_UNWIND_END();
}


LtvcReturn ltvc_make_built_in_class(gctools::GCRootsInModule* holder, size_t index, gctools::Tagged class_name_t )
{NO_UNWIND_BEGIN();
  core::Symbol_sp class_name(class_name_t);
  core::T_sp cl = core::cl__find_class(class_name, true, _Nil<core::T_O>());
  if ( cl.nilp() ) {
    SIMPLE_ERROR(BF("Could not find class %s") % class_name );
  } else {
    core::T_sp val = cl;
    LTVCRETURN holder->set(index,val.tagged_());
  }
  NO_UNWIND_END();
}


LtvcReturn ltvc_make_float(gctools::GCRootsInModule* holder, size_t index, float f)
{NO_UNWIND_BEGIN();
  core::T_sp val = clasp_make_single_float(f);
  LTVCRETURN holder->set(index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_make_double(gctools::GCRootsInModule* holder, size_t index, double f)
{NO_UNWIND_BEGIN();
  core::T_sp val = clasp_make_double_float(f);
  LTVCRETURN holder->set(index,val.tagged_());
  NO_UNWIND_END();
}

LtvcReturn ltvc_set_mlf_creator_funcall(gctools::GCRootsInModule* holder, size_t index, fnLispCallingConvention fptr, const char* name) {
  core::T_O *lcc_arglist = _Nil<core::T_O>().raw_();
  Symbol_sp sname = Symbol_O::create_from_string(std::string(name));
  GC_ALLOCATE_VARIADIC(CompiledClosure_O, toplevel_closure, fptr, sname, kw::_sym_function, _Nil<T_O>(), _Nil<T_O>(), 0, 0, 0, 0 );
  LCC_RETURN ret = fptr(LCC_PASS_ARGS0_VA_LIST(toplevel_closure.raw_()));
  core::T_sp res((gctools::Tagged)ret.ret0[0]);
  core::T_sp val = res;
  LTVCRETURN holder->set(index,val.tagged_());
}

LtvcReturn ltvc_mlf_init_funcall(fnLispCallingConvention fptr, const char* name) {
  core::T_O *lcc_arglist = _Nil<core::T_O>().raw_();
  Symbol_sp sname = Symbol_O::create_from_string(std::string(name));
  GC_ALLOCATE_VARIADIC(CompiledClosure_O, toplevel_closure, fptr, sname, kw::_sym_function, _Nil<T_O>(), _Nil<T_O>(), 0, 0, 0, 0 );
  LCC_RETURN ret = fptr(LCC_PASS_ARGS0_VA_LIST(toplevel_closure.raw_()));
//  LTVCRETURN reinterpret_cast<gctools::Tagged>(ret.ret0[0]);
}

// This is exactly like the one above - is it necessary?
LtvcReturn ltvc_set_ltv_funcall(gctools::GCRootsInModule* holder, size_t index, fnLispCallingConvention fptr, const char* name) {
  core::T_O *lcc_arglist = _Nil<core::T_O>().raw_();
  Symbol_sp sname = Symbol_O::create_from_string(std::string(name));
  GC_ALLOCATE_VARIADIC(CompiledClosure_O, toplevel_closure, fptr, sname, kw::_sym_function, _Nil<T_O>(), _Nil<T_O>(), 0, 0, 0, 0 );
  LCC_RETURN ret = fptr(LCC_PASS_ARGS0_VA_LIST(toplevel_closure.raw_()));
  core::T_sp res((gctools::Tagged)ret.ret0[0]);
  core::T_sp val = res;
  LTVCRETURN holder->set(index,val.tagged_());
}

LtvcReturn ltvc_set_ltv_funcall_cleavir(gctools::GCRootsInModule* holder, size_t index, fnLispCallingConvention fptr, const char* name) {
  // I created this function just in case cleavir returns a function that when evaluated returns a function that
  // would return the ltv value - that appears not to be the case.
  // FIXME: Remove this function and use the ltvc_set_ltv_funcall instead
  core::T_O *lcc_arglist = _Nil<core::T_O>().raw_();
  Symbol_sp sname = Symbol_O::create_from_string(std::string(name));
  GC_ALLOCATE_VARIADIC(CompiledClosure_O, toplevel_closure, fptr, sname, kw::_sym_function, _Nil<T_O>(), _Nil<T_O>(), 0, 0, 0, 0 );
  LCC_RETURN ret = fptr(LCC_PASS_ARGS0_VA_LIST(toplevel_closure.raw_()));
  core::T_sp tret((gctools::Tagged)ret.ret0);
//  printf("%s:%d:%s     ret -> %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(tret).c_str());
  core::T_sp res((gctools::Tagged)ret.ret0[0]);
  core::T_sp val = res;
  LTVCRETURN holder->set(index,val.tagged_());
}


LtvcReturn ltvc_toplevel_funcall(fnLispCallingConvention fptr, const char* name) {
  core::T_O *lcc_arglist = _Nil<core::T_O>().raw_();
  Symbol_sp sname = Symbol_O::create_from_string(std::string(name));
  GC_ALLOCATE_VARIADIC(CompiledClosure_O, toplevel_closure, fptr, sname, kw::_sym_function, _Nil<T_O>(), _Nil<T_O>(), 0, 0, 0, 0 );
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
  core::Function_sp func = core::function_frame_lookup(af, depth, index);
  return func.raw_();
  NO_UNWIND_END();
}


__attribute__((visibility("default"))) core::T_O *cc_gatherRestArguments(va_list vargs, std::size_t* remainingP)
{NO_UNWIND_BEGIN();
  va_list rargs;
  va_copy(rargs, vargs);
  core::List_sp result = _Nil<core::T_O>();
  core::Cons_sp *cur = reinterpret_cast<Cons_sp *>(&result);
  size_t nargs = *remainingP;
  for (int i = 0; i<nargs; ++i ) {
    core::T_O* tagged_obj = ENSURE_VALID_OBJECT(va_arg(rargs,core::T_O*));
    *cur = core::Cons_O::create(gc::smart_ptr<core::T_O>((gc::Tagged)tagged_obj), _Nil<core::T_O>());
    cur = reinterpret_cast<Cons_sp *>(&(*cur)->_Cdr);
  }
  return result.raw_();
  NO_UNWIND_END();
}

void cc_ifBadKeywordArgumentException(size_t allowOtherKeys, std::size_t badKwIdx, core::T_O *kw)
{
  if (allowOtherKeys == 2) {
    return;
  }
  if (badKwIdx != 65536)
    intrinsic_error(llvmo::badKeywordArgument, core::T_sp((gc::Tagged)kw));
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


core::T_O* makeCompiledFunction(fnLispCallingConvention funcPtr,
                                int *sourceFileInfoHandleP,
                                size_t filePos,
                                size_t lineno,
                                size_t column,
                                core::T_O* functionNameP,
                                core::T_O* frameP,
                                core::T_O* lambdaListP)
{NO_UNWIND_BEGIN();
  // TODO: If a pointer to an integer was passed here we could write the sourceName SourceFileInfo_sp index into it for source line debugging
  core::T_sp functionName((gctools::Tagged)functionNameP);
  core::T_sp frame((gctools::Tagged)frameP);
  core::T_sp lambdaList((gctools::Tagged)lambdaListP);
  core::Closure_sp closure = gctools::GC<core::CompiledClosure_O>::allocate(funcPtr, functionName, kw::_sym_function, frame, lambdaList, *sourceFileInfoHandleP, filePos, lineno, column);
  return closure.raw_();
  NO_UNWIND_END();
};
};

extern "C" {
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
  FunctionClosure_sp tc = FunctionClosure_O::create(fptr,name, kw::_sym_function, *sourceFileInfoHandleP, filePos, lineno, column);
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

/*! Invoke the main functions from the main function array.
If isNullTerminatedArray is 1 then there is a NULL terminated array of functions to call.
Otherwise there is just one. */
void cc_register_startup_function(fnStartUp fptr) {
  register_startup_function(fptr);
}

void cc_invoke_sub_run_all_function(fnStartUp fptr) {
  fptr();
}

void cc_invoke_startup_functions() {
  startup_functions_invoke();
};



};


extern "C" {


#if 0
extern void attachDebuggingInfoToValueFrame(core::ActivationFrame_sp *resultP,
                                            core::T_sp *debuggingInfoP) {
  ASSERT(resultP != NULL);
  ASSERT(debuggingInfoP != NULL);
  ASSERT((*resultP));
  ASSERT((*resultP).isA<ValueFrame_O>());
  core::ValueFrame_sp vf = gc::reinterpret_cast_smart_ptr<ValueFrame_O, T_O>((*resultP));
  core::VectorObjects_sp vo = gc::reinterpret_cast_smart_ptr<core::VectorObjects_O, T_O>((*debuggingInfoP));
  vf->attachDebuggingInfo(vo);
}
#endif



/*! Look for the :allow-other-keywords XX keyword argument and
      calculate (or (*ampAllowOtherKeywordsP) XX) return 1 if result is true otherwise 0 */
extern int checkForAllowOtherKeywords(int ampAllowOtherKeywords, core::ActivationFrame_sp *frameP, int argIdx) {
  ASSERT(frameP != NULL);
  ASSERT(frameP->objectp());
  ASSERT(argIdx >= 0);
  core::ValueFrame_sp vf = gc::As<core::ValueFrame_sp>((*frameP));
  if (argIdx >= vf->length())
    return 0;
  int argsLeft = vf->length() - argIdx;
  if ((argsLeft % 2) != 0) {
    stringstream serr;
    serr << "There must be an even number of keyword arguments - you passed: ";
    for (int ei = argIdx; ei < vf->length(); ei++) {
      serr << _rep_(vf->entry(ei)) << " ";
    }
    SIMPLE_ERROR(BF("%s") % serr.str());
  }
  if (ampAllowOtherKeywords)
    return 1;
  for (int ii = argIdx; ii < vf->length(); ii += 2) {
    if (vf->entry(ii) == kw::_sym_allow_other_keys) {
      core::T_sp val = vf->entry(ii + 1);
      if (val.isTrue())
        return 1;
      // TODO: Handle :allow-other-keys nil :allow-other-keys t
      // In safe mode this should throw an exceptions
      // (see 3.4.1.4.1.1 Examples of Suppressing Keyword Argument Checking)
    }
  }
  return 0;
}

/*! Look for the keyword in (*frameP) after argIdx.
 */
extern void throwIfExcessKeywordArguments(char *fnName, core::ActivationFrame_sp *frameP, int argIdx) {
  ASSERT(frameP != NULL);
  ASSERT(frameP->objectp());
  ASSERT(argIdx >= 0);
  core::ValueFrame_sp vframe = gctools::As_unsafe<core::ValueFrame_sp>(*frameP);
  if (argIdx >= vframe->length()) return;
  stringstream ss;
  for (int i(0); i < (vframe)->length(); ++i) {
    ss << _rep_(vframe->entry(i)) << " ";
  }
  SIMPLE_ERROR(BF("Excess keyword arguments fnName: %s argIdx: %d  args: %s") % fnName % argIdx % ss.str());
}

extern void throwIfExcessArguments(core::T_sp *frameP, int argIdx) {
  ASSERT(frameP != NULL);
  ASSERT(frameP->objectp());
  ASSERT(argIdx >= 0);
  core::ValueFrame_sp frame = gc::As_unsafe<core::ValueFrame_sp>((*frameP));
  if (argIdx < frame->length()) {
    stringstream serr;
    for (int i = argIdx; i < frame->length(); i++) {
      serr << _rep_(frame->entry(i)) << " ";
    }
    SIMPLE_ERROR(BF("extraneous arguments: %s") % serr.str());
  }
}
};

inline core::T_sp prependMultipleValues(core::T_mv *multipleValuesP) {
  core::List_sp result = _Nil<core::T_O>();
  core::T_mv &mv = (*multipleValuesP);
  if (mv.number_of_values() > 0) {
    result = core::Cons_O::create(mv, result);
    for (int i = 1; i < mv.number_of_values(); i++) {
      result = core::Cons_O::create(mv.valueGet_(i), result);
    }
  }
  return result;
}

extern "C" {
void sp_prependMultipleValues(core::T_sp *resultP, core::T_mv *multipleValuesP) {
  (*resultP) = prependMultipleValues(multipleValuesP);
  ASSERTNOTNULL(*resultP);
}
void mv_prependMultipleValues(core::T_mv *resultP, core::T_mv *multipleValuesP) {
  (*resultP) = Values(prependMultipleValues(multipleValuesP));
  ASSERTNOTNULL(*resultP);
}
};

extern "C" {

/*! Invoke a symbol function with the given arguments and put the result in (*resultP) */
core::T_O* symbolFunctionRead(const core::T_O *tsymP)
{NO_UNWIND_BEGIN();
  const core::Symbol_sp sym((gc::Tagged)tsymP);
  ASSERTF((sym)->fboundp(), BF("There is no function bound to symbol[%s]") % _rep_(sym));
  return sym->symbolFunction().raw_();
  NO_UNWIND_END();
}

/*! Invoke a symbol function with the given arguments and put the result in (*resultP) */
extern core::T_O* setfSymbolFunctionRead(const core::T_O *tsymP)
{NO_UNWIND_BEGIN();
 const core::Symbol_sp sym((gctools::Tagged)tsymP);
 core::Function_sp setfFunc = sym->getSetfFdefinition(); //_lisp->get_setfDefinition(*symP);
 ASSERTF(setfFunc, BF("There is no setf function bound to symbol[%s]") % _rep_(sym));
 return setfFunc.raw_();
  NO_UNWIND_END();
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
  printf("++++++ debug-message: %s", msg);
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
  NO_UNWIND_END();
}

void debugPrint_size_t(size_t v)
{NO_UNWIND_BEGIN();
  printf("+++DBG-size_t[%lu/%lx]\n", v, v);
  NO_UNWIND_END();
}


#if 0
void throwCatchThrow(core::T_sp *tagP) {
  ASSERT(tagP != NULL);
  core::T_sp tag = *tagP;
  int frame = my_thread->exceptionStack().findKey(CatchFrame, tag);
  if (frame < 0) {
    CONTROL_ERROR();
  } else {
    core::CatchThrow catchThrow(frame);
#ifdef DEBUG_FLOW_CONTROL
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
      printf("%s:%d Throwing core::CatchThrow exception tag[%s] frame: %d\n", __FILE__, __LINE__, _rep_(*tagP).c_str(), frame);
      if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
        printf("   %s\n", my_thread->exceptionStack().summary().c_str());
    }
#endif
    throw catchThrow;
  }
  SIMPLE_ERROR(BF("This should never happen"));
}
#endif

void throwReturnFrom(core::T_O* blockSymbolP) {
  ASSERT(blockSymbolP != NULL);
  core::T_sp blockSymbol((gctools::Tagged)blockSymbolP);
  int frame = my_thread->exceptionStack().findKey(BlockFrame, blockSymbol);
  if (frame < 0) {
    CONTROL_ERROR();
  }
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("Throwing core::ReturnFrom exception frame[%d]\n", frame);
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
      printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  core::ReturnFrom returnFrom(frame);
  throw returnFrom;
}
};

extern "C" {

gctools::return_type blockHandleReturnFrom(unsigned char *exceptionP, size_t frame) {
  core::ReturnFrom &returnFrom = (core::ReturnFrom &)*((core::ReturnFrom *)(exceptionP));
  if (returnFrom.getFrame() == frame) {
    core::MultipleValues &mv = core::lisp_multipleValues();
    gctools::return_type result(mv.operator[](0),mv.getSize());
    return result;
  }
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("Re-throwing core::ReturnFrom exception frame[%d]\n", returnFrom.getFrame());
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
      printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  throw returnFrom;
}

};

extern "C" {

// Return 1 if exception depth is zero				- blockTestDepth0
// Set the result from the ReturnFrom exception return value   	- blockStoreResult
//

#if 0
size_t pushCatchFrame(core::T_O *tagP)
{NO_UNWIND_BEGIN();
  T_sp tag((gctools::Tagged)tagP);
  size_t result = my_thread->exceptionStack().push(CatchFrame, tag);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("Pushed CatchThrow frame[%zu]\n", result);
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
      printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  return result;
  NO_UNWIND_END();
}
#endif

size_t pushBlockFrame(core::T_O* tagP)
{NO_UNWIND_BEGIN();
  T_sp tag((gctools::Tagged)tagP);
  size_t result = my_thread->exceptionStack().push(BlockFrame, tag);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("Pushed Block frame[%zu]\n", result);
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
      printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  return result;
  NO_UNWIND_END();
}

size_t pushTagbodyFrame(core::T_O *afP)
{NO_UNWIND_BEGIN();
  ASSERT(afP != NULL);
  core::T_sp tagbodyId((gctools::Tagged)afP);
  size_t result = my_thread->exceptionStack().push(TagbodyFrame, tagbodyId);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("Pushed Tagbody frame[%zu]\n", result);
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
      printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  return result;
  NO_UNWIND_END();
}
};

core::T_mv proto_ifCatchFrameMatchesStoreResultElseRethrow(size_t catchFrame, unsigned char *exceptionP) {
  core::CatchThrow *ctExceptionP = reinterpret_cast<core::CatchThrow *>(exceptionP);
  if (catchFrame == ctExceptionP->getFrame()) {
    return gctools::multiple_values<core::T_O>::createFromValues(); // ctExceptionP->getReturnedObject();
  }
// rethrow the exception
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("Re-throwing CatchThrow frame[%d]\n", ctExceptionP->getFrame());
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
      printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
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
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("Unwinding to exception frame[%lu]\n", frame);
  }
#endif
  my_thread->exceptionStack().unwind(frame);
  NO_UNWIND_END();
}

void throwIllegalSwitchValue(size_t val, size_t max) {
  SIMPLE_ERROR(BF("Illegal switch value %d - max value is %d") % val % max);
}

void throw_LexicalGo(int depth, int index) {
  core::LexicalGo lgo(depth, index);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("Throwing core::Go depth[%d] index[%d]\n", depth, index);
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
      printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  throw lgo;
}

void throwDynamicGo(size_t depth, size_t index, core::T_O *afP) {
  T_sp af((gctools::Tagged)afP);
  T_sp tagbody = core::tagbody_frame_lookup(af,depth,index);
  int frame = my_thread->exceptionStack().findKey(TagbodyFrame, tagbody);
  if (frame < 0) {
    CONTROL_ERROR();
  }
  core::DynamicGo dgo((size_t)frame, index);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("Throwing core::DynamicGo tagbodyIdP[%zu] index[%zu]\n", dgo.getFrame(), dgo.index());
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
      printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  throw dgo;
}

int tagbodyLexicalGoIndexElseRethrow(char *exceptionP) {
  IMPLEMENT_MEF("Update me");
#if 0
	core::LexicalGo* goExceptionP = (core::LexicalGo*)(exceptionP);
	if ( goExceptionP->depth() == 0 )
	{
	    return goExceptionP->index();
	}
	goExceptionP->decrementDepth();
#ifdef DEBUG_FLOW_CONTROL
	printf("Re-throwing core::Go depth[%d] index[%d]\n", goExceptionP->depth(), goExceptionP->index());
        if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
          printf("   %s\n", my_thread->exceptionStack().summary().c_str());
#endif
	throw *goExceptionP;
#endif
}

size_t tagbodyDynamicGoIndexElseRethrow(char *exceptionP, size_t frame) {
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("%s:%d tagbodyDynamicGoIndexElseRethrow  frame: %lu\n", __FILE__, __LINE__, frame);
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
      printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  core::DynamicGo *goExceptionP = reinterpret_cast<core::DynamicGo *>(exceptionP);
  if (goExceptionP->getFrame() == frame) {
#ifdef DEBUG_FLOW_CONTROL
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
      printf("%s:%d Matched DynamicGo  frame: %llu  index: %lu\n", __FILE__, __LINE__, goExceptionP->getFrame(), goExceptionP->index());
      if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
        printf("   %s\n", my_thread->exceptionStack().summary().c_str());
    }
#endif
    return goExceptionP->index();
  }
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("Re-throwing core::DynamicGo frame[%zu] index[%zu]\n", goExceptionP->getFrame(), goExceptionP->index());
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
      printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  throw * goExceptionP;
}



void debugSourceFileInfoHandle(int *sourceFileInfoHandleP)
{NO_UNWIND_BEGIN();
  int sfindex = *sourceFileInfoHandleP;
  core::Fixnum_sp fn = core::make_fixnum(sfindex);
  SourceFileInfo_sp sfi = core::core__source_file_info(fn);
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

/*! Copy the current MultipleValues in _lisp->values() into a VectorObjects */
extern void loadValues(core::T_mv *resultP, core::T_O* simpleVectorP)
{NO_UNWIND_BEGIN();
  ASSERT(resultP != NULL);
  ASSERTF(*simpleVectorP, BF("*simpleVectorP is UNDEFINED"));
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

/*! If saw_aok > 0 then return that.
      Otherwise check the following argument - if true then return 2 --> :a-o-k t
      Otherwise return 1 --> :a-o-k nil
    */
#if 0
int kw_allowOtherKeywords(int saw_aok, core::ActivationFrame_sp *afP, int argIdx) {
  if (saw_aok)
    return saw_aok;
  ASSERTNOTNULL(*afP);
  core::ValueFrame_sp valueFrame = gc::As<core::ValueFrame_sp>((*afP));
  bool aokTrue = valueFrame->entryReference(argIdx + 1).isTrue();
  return aokTrue ? 2 : 1;
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

void progvSaveSpecials(void **saveSpecialsP, core::T_O *symbolsP, core::T_O *valuesP)
{NO_UNWIND_BEGIN();
  core::DynamicScopeManager *managerP = new core::DynamicScopeManager();
  (*saveSpecialsP) = (void *)managerP;
  core::List_sp symbols((gctools::Tagged)symbolsP);
  core::List_sp values((gctools::Tagged)valuesP);
  for (; symbols.notnilp(); symbols = oCdr(symbols), values = oCdr(values)) {
    core::Symbol_sp symbol = gc::As<Symbol_sp>(oCar(symbols));
    core::T_sp value = oCar(values);
    managerP->pushSpecialVariableAndSet(symbol, value);
  }
  NO_UNWIND_END();
}

void progvRestoreSpecials(void **saveSpecialsP)
{NO_UNWIND_BEGIN();
  core::DynamicScopeManager *managerP = (core::DynamicScopeManager *)(*saveSpecialsP);
  delete (managerP);
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

#if 0
void trace_setLineNumberColumnForIHSTop(char *sourceFileName, int *sourceFileInfoHandleP, size_t fileOffset, int ln, int col) {
  if (comp::_sym_STARlowLevelTracePrintSTAR->symbolValue().isTrue()) {
    if (*sourceFileInfoHandleP == 0) {
      printf("%s:%d trace_setLineNumberColumnForIHSTop has *sourceFileInfoHandleP@%p == 0 soureFileName: %s\n", __FILE__, __LINE__, sourceFileInfoHandleP, sourceFileName);
    }
  }
  my_thread->invocationHistoryStack().setSourcePosForTop(*sourceFileInfoHandleP, fileOffset, ln, col);
}
#endif

#if 0
void trace_setActivationFrameForIHSTop(core::T_sp *afP) {
  my_thread->invocationHistoryStack().setActivationFrameForTop(*afP);
}
#endif

extern size_t matchKeywordOnce(core::T_O *xP, core::T_O *yP, unsigned char *sawKeyAlreadyP)
{NO_UNWIND_BEGIN();
  if (xP != yP) return 0;
  if (*sawKeyAlreadyP) return 2;
  return 1;
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

core::T_O *cc_enclose(core::T_O *lambdaName,
                      fnLispCallingConvention llvm_func,
                      int *sourceFileInfoHandleP,
                      size_t filePos,
                      size_t lineno,
                      size_t column,
                      std::size_t numCells, ...)
{
  core::T_sp tlambdaName = gctools::smart_ptr<core::T_O>((gc::Tagged)lambdaName);
  gctools::smart_ptr<core::ClosureWithSlots_O> functoid =
    gctools::GC<core::ClosureWithSlots_O>::allocate_container(numCells
                                                              , numCells
                                                              , llvm_func
                                                              , tlambdaName
                                                              , kw::_sym_function
                                                              , _Nil<T_O>() // lambdaList
                                                              , *sourceFileInfoHandleP, filePos, lineno, column);
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



void cc_ifNotKeywordException(core::T_O *obj, size_t argIdx, va_list valist) {
  T_sp vobj((gc::Tagged)obj);
  if (!cl__symbolp(vobj)) {
    SIMPLE_ERROR(BF("Expected keyword argument at argument %d got %s") % argIdx % _rep_(gctools::smart_ptr<core::T_O>((gc::Tagged)obj)));
  }
}

T_O **cc_multipleValuesArrayAddress()
{NO_UNWIND_BEGIN();
  return &lisp_multipleValues().callingArgsStart()[0];
  NO_UNWIND_END();
}

void cc_unwind(T_O *targetFrame, size_t index) {
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("%s:%d In cc_unwind targetFrame: %ld  index: %" PRu "\n", __FILE__, __LINE__, gc::untag_fixnum(targetFrame), index);
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
      printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  core::Unwind unwind(targetFrame, index);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("- - - - in cc_unwind throwing unwind to reach targetFrame: %ld   unwind=@%p\n", gc::untag_fixnum(targetFrame), &unwind);
  }
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("- - - -  unwind event address = %p\n", &unwind);
  }
#endif
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
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("%s:%d In cc_restoreMultipleValue0\n", __FILE__, __LINE__);
  }
#endif
  NO_UNWIND_END();
}

T_O *cc_pushLandingPadFrame()
{NO_UNWIND_BEGIN();
  core::T_sp ptr = _Nil<core::T_O>();
#define DEBUG_UNWIND 1
#ifdef DEBUG_UNWIND
  ptr = core::Pointer_O::create((void *)&typeid(core::Unwind));
#endif
  size_t index = my_thread->exceptionStack().push(LandingPadFrame, ptr);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("%s:%d pushLandingPadFrame pushed frame: %" PRu "  core::Unwind typeinfo@%p\n", __FILE__, __LINE__, index, (void *)&typeid(core::Unwind));
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
      printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  return gctools::tag_fixnum<core::T_O *>(index);
  NO_UNWIND_END();
}

void cc_popLandingPadFrame(T_O *frameFixnum)
{NO_UNWIND_BEGIN();
  ASSERT(gctools::tagged_fixnump(frameFixnum));
  size_t frameIndex = gctools::untag_fixnum(frameFixnum);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("%s:%d  popLandingPadFrame   About to unwind exceptionStack to frame: %" PRu "\n", __FILE__, __LINE__, frameIndex);
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
      printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  my_thread->exceptionStack().unwind(frameIndex);
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
      printf("    After unwind of exceptionStack:  %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  NO_UNWIND_END();
}

size_t cc_landingpadUnwindMatchFrameElseRethrow(char *exceptionP, core::T_O *thisFrame) {
  ASSERT(gctools::tagged_fixnump(thisFrame));
  core::Unwind *unwindP = reinterpret_cast<core::Unwind *>(exceptionP);
#ifdef DEBUG_FLOW_CONTROL
  size_t frameIndex = gctools::untag_fixnum(thisFrame);
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("%s:%d landingpadUnwindMatchFrameElseRethrow targetFrame: %" PRu " thisFrame: %lu  unwindP=%p\n",
           __FILE__, __LINE__, gctools::untag_fixnum(unwindP->getFrame()), frameIndex, unwindP);
    if (unwindP->getFrame() > thisFrame) {
      printf("- - - - - THERE IS A SERIOUS PROBLEM - THE TARGET FRAME HAS BEEN BYPASSED\n");
    }
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue() == kw::_sym_verbose )
      printf("   %s\n", my_thread->exceptionStack().summary().c_str());
  }
#endif
  if (unwindP->getFrame() == thisFrame) {
#ifdef DEBUG_FLOW_CONTROL
    if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
      printf("- - - - - Matched Unwind to targetFrame: %" PRu "  index: %lu\n", gc::untag_fixnum(unwindP->getFrame()), unwindP->index());
    }
#endif
    return unwindP->index();
  }
#ifdef DEBUG_FLOW_CONTROL
  if (core::_sym_STARdebugFlowControlSTAR->symbolValue().notnilp()) {
    printf("- - - - - Rethrowing core::Unwind targetFrame[%" PRu "] index[%zu] (thisFrame is: %lu)\n", gc::untag_fixnum(unwindP->getFrame()), unwindP->index(), frameIndex);
  }
#endif
  throw * unwindP;
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
  core::Function_sp func1 = tfunc1.asOrNull<core::Function_O>();
  ASSERT(func1);
  core::Closure_sp closure1 = func1->closure;
  closure1->invoke(result,LCC_PASS_ARGS0());
  mvFunc1._Size = result->number_of_values();
  mvFunc1[0] = result->raw_();
  MultipleValues &mvThreadLocal = lisp_multipleValues();
  for (size_t i(1), iEnd(mvFunc1._Size); i < iEnd; ++i) mvFunc1[i] = mvThreadLocal[i];
  T_mv resultTemp;
  core::Function_sp func2 = tfunc2.asOrNull<core::Function_O>();
  ASSERT(func2);
  core::Closure_sp closure2 = func2->closure;
  closure2->invoke(&resultTemp,LCC_PASS_ARGS0());
  for (size_t i(1), iEnd(mvFunc1._Size); i < iEnd; ++i) mvThreadLocal[i] = mvFunc1[i];
}
#endif


gctools::return_type cc_dispatch_effective_method(core::T_O* teffective_method, core::T_O* tgf, core::T_O* tgf_args_vaslist) {
  core::Function_sp effective_method((gctools::Tagged)teffective_method);
  core::T_sp gf_vaslist((gctools::Tagged)tgf_args_vaslist);
  return (*effective_method).entry(LCC_PASS_ARGS2_ELLIPSIS(teffective_method,gf_vaslist.raw_(),_Nil<core::T_O>().raw_()));
}


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
      BFORMAT_T(BF("Step %d\n") % val);
//      printf("%s:%d    cc_dispatch_debug step %d\n", __FILE__, __LINE__, val );
      break;
  case 1:
      BFORMAT_T(BF("Arg val[%d]") % val);
//      printf("%s:%d    cc_dispatch_debug arg val[%d]\n", __FILE__, __LINE__, val );
      break;
  case 2:
      BFORMAT_T(BF(" tag = %d\n") % val); 
//      printf("%s:%d    cc_dispatch_debug tag [%d]\n", __FILE__, __LINE__, val );
     break;
  case 3: {
    VaList_sp vls((gc::Tagged)val);
//    printf("%s:%d    vaList_sp.raw_() = %p\n", __FILE__, __LINE__, vls.raw_());
    BFORMAT_T(BF("Arg VaList_sp.raw_() = %p list -> %s\n") % (void*)vls.raw_() % _rep_(vls) );
    dump_Vaslist_ptr(&*vls);
    break;
  }
  case 4: {
//      printf("%s:%d     ptr: %p\n", __FILE__, __LINE__, (void*)val);
      BFORMAT_T(BF("Ptr: %p\n") % (void*)val );
  }
      break;
  case 5: {
//      printf("%s:%d     ptr: %p\n", __FILE__, __LINE__, (void*)val);
      BFORMAT_T(BF("va_list: %p\n") % (void*)val );
      void* dump_va_list_voidSTAR = (void*)&dump_va_list;
      typedef void (*fptr)(uintptr_t);
      fptr my_fptr = reinterpret_cast<fptr>(dump_va_list_voidSTAR);
      my_fptr(val);
      break;
  }
  case 6:
      BFORMAT_T(BF("Argument stamp: %lu\n") % val);
      break;
  case 7:
      BFORMAT_T(BF("Dispatch to: %p\n") % val);
      break;
  }
  fflush(stdout);
}

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
#if 0
	T_mv foo = testTwoReturns();
	printf("Called testTwoReturns  foo.raw_() = %p   foo.two = %d\n", foo.raw_(), foo.number_of_values() );
#endif
}
};


extern "C" {

gctools::return_type cc_dispatch_slot_reader_index_debug(core::T_O* toptimized_slot_reader, size_t index, core::T_O* tvargs) {
  core::SimpleVector_sp optimized_slot_reader((gctools::Tagged)toptimized_slot_reader);
  core::VaList_sp vargs((gctools::Tagged)tvargs);
  va_list vat;
  va_copy(vat,vargs->_Args);
  core::Instance_sp instance((gctools::Tagged)va_arg(vat,core::T_O*));
  va_end(vat);
  core::T_sp result = core::eval::funcall(clos::_sym_dispatch_slot_reader_index_debug,optimized_slot_reader,instance,vargs);
  return result.as_return_type();
}

gctools::return_type cc_dispatch_slot_writer_index_debug(core::T_O* toptimized_slot_writer, size_t index, core::T_O* tvargs) {
  core::SimpleVector_sp optimized_slot_writer((gctools::Tagged)toptimized_slot_writer);
  core::VaList_sp vargs((gctools::Tagged)tvargs);
  va_list vat;
  va_copy(vat,vargs->_Args);
  core::T_sp value((gctools::Tagged)va_arg(vat,core::T_O*));
  core::Instance_sp instance((gctools::Tagged)va_arg(vat,core::T_O*));
  va_end(vat);
  core::T_sp result = core::eval::funcall(clos::_sym_dispatch_slot_writer_index_debug,optimized_slot_writer,value,instance,vargs);
  return result.as_return_type();
}


void cc_error_too_few_arguments(size_t nargs, size_t minargs) {
  SIMPLE_ERROR(BF("Not enough arguments - you provided %lu and %lu are required") % nargs % minargs );
}

void cc_error_too_many_arguments(size_t nargs, size_t maxargs) {
  SIMPLE_ERROR(BF("Too many arguments - you provided %lu and %lu are allowed") % nargs % maxargs );
}

};
