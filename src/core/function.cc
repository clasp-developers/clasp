/*
    File: functor.cc
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
#include <clasp/core/foundation.h>
#include <clasp/gctools/snapshotSaveLoad.h>
#include <clasp/gctools/memoryManagement.h>
#include <clasp/core/lisp.h>
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/package.h>
// #include "debugger.h"
#include <clasp/core/iterator.h>
#include <clasp/core/designators.h>
#include <clasp/core/primitives.h>
#include <clasp/core/instance.h>
#include <clasp/core/compiler.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/debugger.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/bytecode.h>
#include <clasp/core/lambdaListHandler.h>
// #i n c l u d e "environmentDependent.h"
#include <clasp/core/evaluator.h>
// to avoid Generic to_object include headers here
#include <clasp/core/wrappers.h>
#include <clasp/llvmo/code.h>
#include <clasp/llvmo/debugInfoExpose.h>

namespace core {

bytecode_trampoline_function bytecode_trampoline = bytecode_call; // default

void SimpleFun_O::fixupOneCodePointer(snapshotSaveLoad::Fixup* fixup, void** ptr) {
#ifdef USE_PRECISE_GC
  if (snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::InfoOp) {
    uintptr_t* ptrptr = (uintptr_t*)&ptr[0];
    if (fixup->_trackAddressName) {
      fixup->addAddressName((void*)*ptrptr, _rep_(this->_FunctionDescription->functionName()));
    }
  } else if (snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::SaveOp) {
    uintptr_t* ptrptr = (uintptr_t*)&ptr[0];
    snapshotSaveLoad::encodeEntryPoint(fixup, ptrptr, this->_Code, this->_FunctionDescription );
  } else if (snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::LoadOp) {
    uintptr_t* ptrptr = (uintptr_t*)&ptr[0];
    snapshotSaveLoad::decodeEntryPoint(fixup, ptrptr, this->_Code);
  } else {
    SIMPLE_ERROR("Illegal image save/load operation");
  }
#endif
}

bool SimpleFun_O::dladdrablep(std::set<void*>& uniques) {
  if (this->_Code.isA<llvmo::Library_O>()) {
    for (size_t ii = 0; ii < ClaspXepFunction::Entries; ++ii) {
      void* address = (void*)this->_EntryPoints._EntryPoints[ii].load(std::memory_order_relaxed);
      if (!uniques.contains(address)) {
        Dl_info info;
        uniques.insert(address);
        if (dladdr(address, &info) == 0)
          return false;
      }
    }
  }
  return true;
}

CL_DEFMETHOD Pointer_sp SimpleFun_O::defaultEntryAddress() const { SUBCLASS_MUST_IMPLEMENT(); }

SimpleFun_O::SimpleFun_O(FunctionDescription_sp fdesc, T_sp code, const ClaspXepTemplate& entry_point)
  : Function_O(this), _FunctionDescription(fdesc), _Code(code),
    _EntryPoints(entry_point) {
  if (code.nilp()) {
    code = llvmo::identify_code_or_library(reinterpret_cast<gctools::clasp_ptr_t>(entry_point._EntryPoints[0]));
    this->_Code = code;
  }
  if (gc::IsA<llvmo::Library_sp>(code)) {
    for (size_t ii = 0; ii < ClaspXepFunction::Entries; ii++) {
      maybe_register_symbol_using_dladdr_ep((void*)entry_point._EntryPoints[ii], sizeof(void*), "SimpleFunName", ii);
    }
  }
  llvmo::validateEntryPoint(code, _EntryPoints);
}

void SimpleFun_O::fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
  for (size_t ii = 0; ii < ClaspXepFunction::Entries; ++ii) {
    this->fixupOneCodePointer(fixup, (void**)&this->_EntryPoints._EntryPoints[ii]);
  }
}

SimpleCoreFun_O::SimpleCoreFun_O(FunctionDescription_sp fdesc, const ClaspXepTemplate& entry_point, T_sp code, CoreFun_sp lep)
  : SimpleFun_O(fdesc, code, entry_point), _localFun(lep) {};

BytecodeSimpleFun_O::BytecodeSimpleFun_O(FunctionDescription_sp fdesc, const ClaspXepTemplate& entry_point, T_sp module,
                                                     uint16_t localsFrameSize, unsigned int environmentSize, unsigned int entryPcN,
                                                     unsigned int bytecodeSize, BytecodeTrampolineFunction trampoline)
  : SimpleFun_O(fdesc, module, entry_point), _LocalsFrameSize(localsFrameSize), _EnvironmentSize(environmentSize),
    _EntryPcN(entryPcN), _BytecodeSize(bytecodeSize), _Trampoline(trampoline) {};

void BytecodeSimpleFun_O::fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
  this->fixupOneCodePointer(fixup, (void**)&this->_Trampoline);
  this->Base::fixupInternalsForSnapshotSaveLoad(fixup);
}

CL_DEFMETHOD size_t BytecodeSimpleFun_O::entryPcN() const { return this->_EntryPcN; }

// These two methods allow functions to be used uniformly as
// debug info in bytecode modules. The duck typing is a bit
// unfortunate but ought to be harmless.
CL_LISPIFY_NAME(BytecodeDebugInfo/start)
CL_DEFMETHOD T_sp BytecodeSimpleFun_O::start() const { return Integer_O::create(this->_EntryPcN); }
CL_LISPIFY_NAME(BytecodeDebugInfo/end)
CL_DEFMETHOD T_sp BytecodeSimpleFun_O::end() const { return Integer_O::create(this->_EntryPcN + this->_BytecodeSize); }

CoreFun_O::CoreFun_O(FunctionDescription_sp fdesc, T_sp code,
                       const ClaspCoreFunction& entry_point)
  : _FunctionDescription(fdesc), _Code(code), _Entry(entry_point) {
  llvmo::validateEntryPoint(code, entry_point);
}

  void BytecodeSimpleFun_O::set_trampoline(Pointer_sp trampoline) {
  this->_Trampoline = (BytecodeTrampolineFunction)trampoline->ptr();
}

Pointer_sp SimpleCoreFun_O::defaultEntryAddress() const { return Pointer_O::create((void*)this->_EntryPoints[0]); };

CL_LISPIFY_NAME("simple-core-fun/code");
CL_DEFMETHOD
llvmo::ObjectFile_sp SimpleCoreFun_O::code() const {
  llvmo::ObjectFile_sp code = gc::As<llvmo::ObjectFile_sp>(this->_Code);
  return code;
}

CL_LISPIFY_NAME("simple-core-fun-local-fun");
CL_DEFMETHOD
CoreFun_sp SimpleCoreFun_O::localFun() const { return this->_localFun; }

Pointer_sp CoreFun_O::defaultEntryAddress() const { return Pointer_O::create((void*)this->_Entry); };

Pointer_sp BytecodeSimpleFun_O::defaultEntryAddress() const { return Pointer_O::create((void*)this->_EntryPoints[0]); };

CL_LISPIFY_NAME("bytecode-simple-fun/code");
CL_DEFMETHOD
BytecodeModule_sp BytecodeSimpleFun_O::code() const {
  BytecodeModule_sp code = gc::As<BytecodeModule_sp>(this->_Code);
  return code;
}

std::string BytecodeSimpleFun_O::__repr__() const {
  stringstream ss;
  ss << "#<GLOBAL-BYTECODE-SIMPLE-FUN ";
  for (size_t ii = 0; ii < NUMBER_OF_ENTRY_POINTS; ii++) {
    if (ii == 0) {
      ss << "xep@";
    } else {
      ss << "xep" << (ii - 1) << "@";
    }
    ss << (void*)this->_EntryPoints[ii] << " ";
  }
  ss << " @" << (void*)this << ">";
  return ss.str();
}

void CoreFun_O::fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
  void** ptr = (void**)&(this->_Entry);
#ifdef USE_PRECISE_GC
  if (snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::InfoOp) {
    uintptr_t* ptrptr = (uintptr_t*)&ptr[0];
    if (fixup->_trackAddressName) {
      fixup->addAddressName((void*)*ptrptr, _rep_(this->_FunctionDescription->functionName()));
    }
  } else if (snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::SaveOp) {
    llvmo::ObjectFile_sp code = gc::As_unsafe<llvmo::ObjectFile_sp>(this->_Code);
    if ((uintptr_t)this->_Entry < code->codeStart()) {
      printf("%s:%d:%s Entrypoint %p is before the codeStart %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)this->_Entry,
             (void*)code->codeStart());
      abort();
    }
    uintptr_t* ptrptr = (uintptr_t*)&ptr[0];
    snapshotSaveLoad::encodeEntryPoint(fixup, ptrptr, this->_Code, this->_FunctionDescription );
  } else if (snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::LoadOp) {
    uintptr_t* ptrptr = (uintptr_t*)&ptr[0];
    snapshotSaveLoad::decodeEntryPoint(fixup, ptrptr, this->_Code);
  } else {
    SIMPLE_ERROR("Illegal image save/load operation");
  }
#endif
};

bool CoreFun_O::dladdrablep(std::set<void*>& uniques) {
  void* address = (void*)this->_Entry;
  if (!uniques.contains(address)) {
    Dl_info info;
    uniques.insert(address);
    if (dladdr(address, &info) == 0)
      return false;
  }
  return true;
}

CL_LAMBDA(&key function-description entry-point-functions core-fun-generator);
DOCGROUP(clasp);
CL_DEFUN SimpleCoreFunGenerator_sp core__makeSimpleCoreFunGenerator(FunctionDescription_sp fdesc, T_sp entryPointIndices,
                                                                    CoreFunGenerator_sp cfg) {
  return gctools::GC<SimpleCoreFunGenerator_O>::allocate(fdesc, entryPointIndices, cfg);
}

CL_LAMBDA(&key function-description entry-point-functions);
DOCGROUP(clasp);
CL_DEFUN CoreFunGenerator_sp core__makeCoreFunGenerator(FunctionDescription_sp fdesc, T_sp entryPointIndices) {
  return gctools::GC<CoreFunGenerator_O>::allocate(fdesc, entryPointIndices);
}

std::string CoreFunGenerator_O::__repr__() const {
  stringstream ss;
  ss << "#<LOCAL-FUN-GENERATOR ";
  ss << _rep_(this->_entry_point_indices) << " @" << (void*)this << ">";
  return ss.str();
}

std::string CoreFun_O::__repr__() const {
  stringstream ss;
  ss << "#<LOCAL-FUN ";
  ss << (void*)this->_Entry;
  ss << " @" << (void*)this << ">";
  return ss.str();
}

std::string SimpleCoreFun_O::__repr__() const {
  stringstream ss;
  ss << "#<Global-SIMPLE-FUN ";
  for (size_t ii = 0; ii < NUMBER_OF_ENTRY_POINTS; ii++) {
    if (ii == 0) {
      ss << "xep@";
    } else {
      ss << "xep" << (ii - 1) << "@";
    }
    ss << (void*)this->_EntryPoints[ii] << " ";
  }
  ss << " @" << (void*)this << ">";
  return ss.str();
}

std::string SimpleCoreFunGenerator_O::__repr__() const {
  stringstream ss;
  ss << "#<GLOBAL-SIMPLE-FUN-GENERATOR ";
  ss << _rep_(this->_entry_point_indices) << " @" << (void*)this << ">";
  return ss.str();
}

CL_LISPIFY_NAME(FunctionDescription/make);
CL_LAMBDA(&key function-name lambda-list docstring declares source-pathname (lineno 0) (column 0) (filepos 0));
DOCGROUP(clasp);
CL_DEFUN FunctionDescription_sp core__makeFunctionDescription(T_sp functionName, T_sp lambdaList, T_sp docstring, T_sp declares,
                                                              T_sp sourcePathname, int lineno, int column, int filePos) {
  return makeFunctionDescription(functionName, lambdaList, docstring, declares, sourcePathname, lineno, column, filePos);
}

FunctionDescription_sp makeFunctionDescription(T_sp functionName, T_sp lambda_list, T_sp docstring, T_sp declares,
                                               T_sp sourcePathname, int lineno, int column, int filePos) {
  auto fdesc = gctools::GC<FunctionDescription_O>::allocate();
  fdesc->_sourcePathname = sourcePathname;
  fdesc->_functionName = functionName;
  fdesc->_lambdaList = lambda_list;
  fdesc->_docstring = docstring;
  fdesc->_declares = declares;
  fdesc->lineno = lineno;
  fdesc->column = column;
  fdesc->filepos = filePos;
  //  printf("%s:%d:%s @ %p   entry_point %p fdesc->_EntryPoints[0] = %p\n" = gctools::GC<"%s:%d:%s @ %p   entry_point %p
  //  fdesc->_EntryPoints[0] = %p\n">::allocate( __LINE__, __FUNCTION__, (void*)fdesc.raw_(), (void*)entry_point,
  //  (void*)fdesc->_EntryPoints[0]);
  return fdesc;
}

CoreFun_sp makeCoreFun(FunctionDescription_sp fdesc,
                         const ClaspCoreFunction& entry_point) {
  T_sp code = unbound<llvmo::CodeBase_O>();
  if (entry_point) {
    code = llvmo::identify_code_or_library(reinterpret_cast<gctools::clasp_ptr_t>(entry_point));
    if (gc::IsA<llvmo::Library_sp>(code)) {
      maybe_register_symbol_using_dladdr_ep((void*)entry_point);
    }
  }
  return gctools::GC<CoreFun_O>::allocate(fdesc, code, entry_point);
}

SimpleFun_sp makeSimpleFun(FunctionDescription_sp tfdesc, const ClaspXepTemplate& entry_point) {
  return gctools::GC<SimpleFun_O>::allocate(tfdesc, nil<T_O>(), entry_point);
}

SimpleCoreFun_sp makeSimpleCoreFun(FunctionDescription_sp tfdesc, const ClaspXepTemplate& entry_point, CoreFun_sp lep) {
  return gctools::GC<SimpleCoreFun_O>::allocate(tfdesc, entry_point, nil<T_O>(), lep);
}

// Used by clasp-cleavir to build compiled functions from function pointers.
SimpleCoreFun_sp SimpleCoreFun_O::make(FunctionDescription_sp fdesc, ClaspCoreFunction mainptr, ClaspXepAnonymousFunction* xepptrs) {
  CoreFun_sp core = makeCoreFun(fdesc, mainptr);
  ClaspXepTemplate xep;
  // NULL pointers represent a redirect.
#define DEFXEP(n) xep._EntryPoints[(n+1)] = xepptrs[(n+1)] ? xepptrs[(n+1)] : (ClaspXepAnonymousFunction)(general_entry_point_redirect_##n)
  xep._EntryPoints[0] = xepptrs[0];
  DEFXEP(0);
  DEFXEP(1);
  DEFXEP(2);
  DEFXEP(3);
  DEFXEP(4);
  DEFXEP(5);
#undef DEFXEP
  return gctools::GC<SimpleCoreFun_O>::allocate(fdesc, xep, nil<T_O>(), core);
}

CL_DEFUN SimpleCoreFun_sp core__make_simple_core_fun(FunctionDescription_sp fdesc, Pointer_sp mainfun, Pointer_sp xept) {
  return SimpleCoreFun_O::make(fdesc, (ClaspCoreFunction)(mainfun->ptr()), (ClaspXepAnonymousFunction*)(xept->ptr()));
}

SYMBOL_EXPORT_SC_(CorePkg, bytecode_call);

struct BytecodeClosureEntryPoint {
  static inline LCC_RETURN bytecode_enter(BytecodeSimpleFun_sp entryPoint, T_O* lcc_closure, size_t lcc_nargs, T_O** lcc_args) {
    core::BytecodeModule_sp module = gctools::As_assert<core::BytecodeModule_sp>(entryPoint->_Code);
    unsigned char* pc = (unsigned char*)&(module->bytecode()->_Data[0]) + entryPoint->_EntryPcN;
    return (entryPoint->_Trampoline)(pc, lcc_closure, lcc_nargs, lcc_args);
  }

  static inline LCC_RETURN LISP_CALLING_CONVENTION() {
    Closure_O* closure = gctools::untag_general<Closure_O*>((Closure_O*)lcc_closure);
    core::BytecodeSimpleFun_sp entryPoint = gctools::As_assert<core::BytecodeSimpleFun_sp>(closure->entryPoint());
    // Check if this bytecode SF's SF has been replaced,
    // as by autocompilation. If it has,
    // swap out the entry point (function pointer) to go directly to
    // the compiled code next time.
    SimpleFun_sp eep = entryPoint->entryPoint();
    if (entryPoint != eep) [[unlikely]] {
      entryPoint->_EntryPoints.store(0, eep->_EntryPoints[0]);
      // Call the new function.
      return eep->_EntryPoints.invoke_n(lcc_closure, lcc_nargs, lcc_args);
    } else // normal bytecode call.
      return bytecode_enter(entryPoint, lcc_closure, lcc_nargs, lcc_args);
  }

  template <typename... Ts>
  static inline LCC_RETURN entry_point_fixed(T_O* lcc_closure,
                                             Ts... args) {
    Closure_O* closure = gctools::untag_general<Closure_O*>((Closure_O*)lcc_closure);
    BytecodeSimpleFun_sp entryPoint = gctools::As_assert<core::BytecodeSimpleFun_sp>(closure->entryPoint());
    SimpleFun_sp eep = entryPoint->entryPoint();
    if (entryPoint != eep) [[unlikely]] {
      // the +1 is due to the general entry at 0.
      constexpr size_t index = sizeof...(Ts) + 1;
      entryPoint->_EntryPoints.store(index, eep->_EntryPoints[index]);
      // Use this instead of funcall_raw to ensure the closure is correct
      return eep->_EntryPoints.call(lcc_closure, args...);
    } else {
      constexpr size_t lcc_nargs = sizeof...(Ts);
      T_O* lcc_args[lcc_nargs] = {args...};
      return bytecode_enter(entryPoint, lcc_closure, lcc_nargs, lcc_args);
    }
  }
};

CL_LISPIFY_NAME(BytecodeSimpleFun/make);
CL_DEFUN
BytecodeSimpleFun_sp core__makeBytecodeSimpleFun(FunctionDescription_sp fdesc, BytecodeModule_sp module,
                                                             size_t localsFrameSize, size_t environmentSize, size_t pcIndex,
                                                             size_t bytecodeSize, Pointer_sp trampoline) {
  auto entryPoint = gctools::GC<BytecodeSimpleFun_O>::allocate(fdesc, XepStereotype<BytecodeClosureEntryPoint>(), module, localsFrameSize, environmentSize, pcIndex,
                                                                     bytecodeSize, (BytecodeTrampolineFunction)trampoline->ptr());
  return entryPoint;
}

CL_LISPIFY_NAME("core-fun-generator/generate");
CL_DEFMETHOD
CoreFun_sp CoreFunGenerator_O::generate(void** entry_points) const {
  if (!_entry_point_indices.consp()) {
    SIMPLE_ERROR("The CoreFunGenerator {} does not have entry-points", _rep_(this->asSmartPtr()));
  }
  T_sp firstEntryPoint = CONS_CAR(_entry_point_indices);
  if (!firstEntryPoint.fixnump()) {
    SIMPLE_ERROR("The CoreFunGenerator {} does not have entry-points indices", _rep_(this->asSmartPtr()));
  }
  size_t entryPointIndex = firstEntryPoint.unsafe_fixnum();
  ClaspCoreFunction entry_point = (ClaspCoreFunction)(entry_points[entryPointIndex]);
  T_sp code = unbound<T_O>();
  if (entry_point) {
    code = llvmo::identify_code_or_library(reinterpret_cast<gctools::clasp_ptr_t>(entry_point));
  }
  auto entryPoint = gctools::GC<CoreFun_O>::allocate(_FunctionDescription, code, entry_point);
  return entryPoint;
}

CL_LISPIFY_NAME("simple-core-fun-generator/generate");
CL_DEFMETHOD
SimpleCoreFun_sp SimpleCoreFunGenerator_O::generate(CoreFun_sp core,
                                                    void** entry_points) const {
  if (!_entry_point_indices.consp()) {
    SIMPLE_ERROR("The SimpleCoreFunGenerator {} does not have entry-points", _rep_(this->asSmartPtr()));
  }
  List_sp epIndices = gc::As<List_sp>(_entry_point_indices);
  size_t num = cl__length(epIndices);
  if (num != ClaspXepFunction::Entries) {
    SIMPLE_ERROR("{} is not enough entry_points for a ClaspXepFunction expected {}\n", num,
                 ClaspXepFunction::Entries);
  }
  ClaspXepTemplate xepFunction;
  size_t cur = 0;
  for (auto entry : epIndices) {
    T_sp oneEntryPointIndex = CONS_CAR(entry);
    if (!oneEntryPointIndex.fixnump()) {
      SIMPLE_ERROR("The SimpleCoreFunGenerator {} does not have entry-points indices", _rep_(this->asSmartPtr()));
    }
    size_t entryPointIndex = oneEntryPointIndex.unsafe_fixnum();
    ClaspXepAnonymousFunction entry_point = (ClaspXepAnonymousFunction)(entry_points[entryPointIndex]);
    xepFunction._EntryPoints[cur] = entry_point;
    cur++;
  }
  T_sp code = unbound<T_O>();
  code = llvmo::identify_code_or_library(reinterpret_cast<gctools::clasp_ptr_t>(xepFunction._EntryPoints[0]));
  return gctools::GC<SimpleCoreFun_O>::allocate(_FunctionDescription, xepFunction, code, core);
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__FunctionDescription_sourcePathname(FunctionDescription_sp fdesc) { return fdesc->_sourcePathname; }

DOCGROUP(clasp);
CL_DEFUN T_sp core__FunctionDescription_functionName(FunctionDescription_sp fdesc) { return fdesc->_functionName; }

DOCGROUP(clasp);
CL_DEFUN T_sp core__FunctionDescription_lambdaList(FunctionDescription_sp fdesc) { return fdesc->_lambdaList; }

DOCGROUP(clasp);
CL_DEFUN T_sp core__FunctionDescription_docstring(FunctionDescription_sp fdesc) { return fdesc->_docstring; }
DOCGROUP(clasp);
CL_DEFUN T_sp core__FunctionDescription_declares(FunctionDescription_sp fdesc) { return fdesc->_declares; }

DOCGROUP(clasp);
CL_DEFUN size_t core__FunctionDescription_lineno(FunctionDescription_sp fdesc) { return fdesc->lineno; }
DOCGROUP(clasp);
CL_DEFUN size_t core__FunctionDescription_column(FunctionDescription_sp fdesc) { return fdesc->column; }
DOCGROUP(clasp);
CL_DEFUN size_t core__FunctionDescription_filepos(FunctionDescription_sp fdesc) { return fdesc->filepos; }

DOCGROUP(clasp);
CL_DEFUN T_sp core__SimpleFun_function_description(SimpleFun_sp entryPoint) { return entryPoint->_FunctionDescription; }

DOCGROUP(clasp);
CL_DEFUN T_sp core__SimpleCoreFunGenerator_entry_point_indices(SimpleCoreFunGenerator_sp fdesc) {
  return fdesc->_entry_point_indices;
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__CoreFunGenerator_entry_point_indices(CoreFunGenerator_sp fdesc) {
  return fdesc->_entry_point_indices;
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__set_SimpleCoreFun_entry_point_indices(SimpleCoreFunGenerator_sp fdesc, T_sp entry_point_indices) {
  return fdesc->_entry_point_indices = entry_point_indices;
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__set_CoreFun_entry_point_indices(CoreFunGenerator_sp fdesc, T_sp entry_point_indices) {
  return fdesc->_entry_point_indices = entry_point_indices;
}

CL_LISPIFY_NAME(function-description-equal);
CL_DEFMETHOD bool FunctionDescription_O::function_description_equal(T_sp other) const {
  if (gc::IsA<FunctionDescription_sp>(other)) {
    FunctionDescription_sp fdother = gc::As_unsafe<FunctionDescription_sp>(other);
    return this->filepos == fdother->filepos && this->lineno == fdother->lineno && this->column == fdother->column &&
           cl__equal(this->_sourcePathname, fdother->_sourcePathname) && cl__equal(this->_functionName, fdother->_functionName) &&
           cl__equal(this->_docstring, fdother->_docstring) && cl__equal(this->_declares, fdother->_declares) &&
           cl__equal(this->_lambdaList, fdother->_lambdaList);
  }
  return false;
}

std::string FunctionDescription_O::__repr__() const {
  stringstream ss;
  ss << "#<FUNCTION-DESCRIPTION " << _rep_(this->_functionName) << ">";
  return ss.str();
}

CL_LISPIFY_NAME(function-description-sxhash-equal);
CL_DEFMETHOD Fixnum FunctionDescription_O::function_description_sxhash_equal() const {
  HashGenerator hg;
  hg.addValue(this->filepos);
  hg.addValue(this->lineno);
  hg.addValue(this->column);
  clasp_sxhash(this->_sourcePathname, hg);
  clasp_sxhash(this->_functionName, hg);
  clasp_sxhash(this->_docstring, hg);
  clasp_sxhash(this->_declares, hg);
  clasp_sxhash(this->_lambdaList, hg);
  return hg.rawhash();
}

T_sp FunctionDescription_O::sourcePathname() const { return this->_sourcePathname; }

void FunctionDescription_O::setf_sourcePathname(T_sp sourceFileName) { this->_sourcePathname = sourceFileName; }

T_sp FunctionDescription_O::functionName() const { return this->_functionName; }

void FunctionDescription_O::setf_functionName(T_sp name) { this->_functionName = name; }

T_sp FunctionDescription_O::lambdaList() const { return this->_lambdaList; }

void FunctionDescription_O::setf_lambdaList(T_sp lambda_list) { this->_lambdaList = lambda_list; }

T_sp FunctionDescription_O::docstring() const { return this->_docstring; }

T_sp FunctionDescription_O::declares() const { return this->_declares; }

void FunctionDescription_O::setf_docstring(T_sp x) { this->_docstring = x; }

}; // namespace core

extern "C" void dumpFunctionDescription(core::FunctionDescription_sp fdesc) {
  core::clasp_write_string(fmt::format("FunctionDescription@{}\nsourcePathname = {}\nfunctionName = {}\nlambdaList = {}\n"
                                       "docstring = {}\ndeclares = {}\nlineno = {}\ncolumn = {}\nfilepos = {}\n",
                                       (void*)fdesc.raw_(), _rep_(fdesc->sourcePathname()), _rep_(fdesc->functionName()),
                                       _rep_(fdesc->lambdaList()), _rep_(fdesc->docstring()), _rep_(fdesc->declares()),
                                       fdesc->lineno, fdesc->column, fdesc->filepos));
};

namespace core {

DOCGROUP(clasp);
CL_DEFUN void core__dumpFunctionDescription(T_sp func) {
  if (gc::IsA<Function_sp>(func)) {
    FunctionDescription_sp fdesc = gc::As<Function_sp>(func)->fdesc();
    dumpFunctionDescription(fdesc);
  } else if (gc::IsA<FunctionDescription_sp>(func)) {
    dumpFunctionDescription(gc::As<FunctionDescription_sp>(func));
  } else {
    SIMPLE_ERROR("You can only dump function-descriptions from functions or function-descriptions");
  }
}

FunctionDescription_sp Function_O::fdesc() const { return this->entryPoint()->_FunctionDescription; };

CL_LISPIFY_NAME("core:functionSourcePos");
CL_DEFMETHOD T_mv Function_O::functionSourcePos() const {
  T_sp sfi = this->sourcePathname();
  return Values(sfi, make_fixnum(this->filePos()), make_fixnum(this->lineno()), make_fixnum(this->column()));
}

DOCGROUP(clasp);
CL_DOCSTRING("Return the documentation string associated with the function.");
CL_DEFUN T_sp core__function_docstring(Function_sp func) { return func->docstring(); }

CL_LISPIFY_NAME("core:function-docstring");
CL_DOCSTRING("Set the documentation string associated with the function.");
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp setf_function_docstring(T_sp doc, Function_sp func) {
  func->setf_docstring(doc);
  return doc;
}

SYMBOL_SC_(CompPkg, vtable);
SYMBOL_SC_(CompPkg, entry);
SYMBOL_EXPORT_SC_(CorePkg, entry_point);
SYMBOL_SC_(CompPkg, closure_type);
SYMBOL_SC_(CompPkg, data_length);
SYMBOL_SC_(CompPkg, data0);

DOCGROUP(clasp);
CL_DEFUN void core__verify_closure(T_sp alist) {
  expect_offset(core::_sym_entry_point, alist, offsetof(Closure_O, _TheSimpleFun) - gctools::general_tag);
  expect_offset(comp::_sym_data_length, alist, offsetof(Closure_O, _Slots._MaybeSignedLength) - gctools::general_tag);
  expect_offset(comp::_sym_data0, alist, offsetof(Closure_O, _Slots._Data) - gctools::general_tag);
}

SYMBOL_EXPORT_SC_(KeywordPkg, function_description);
SYMBOL_EXPORT_SC_(KeywordPkg, code);
SYMBOL_EXPORT_SC_(KeywordPkg, entry_points);
SYMBOL_EXPORT_SC_(KeywordPkg, required_args);

DOCGROUP(clasp);
CL_DEFUN void core__verify_global_entry_point(T_sp alist) {
  expect_offset(kw::_sym_function_description, alist, offsetof(SimpleCoreFun_O, _FunctionDescription) - gctools::general_tag);
  expect_offset(kw::_sym_code, alist, offsetof(SimpleCoreFun_O, _Code) - gctools::general_tag);
  expect_offset(kw::_sym_entry_points, alist, offsetof(SimpleCoreFun_O, _EntryPoints._EntryPoints) - gctools::general_tag);
}

CL_LISPIFY_NAME(bytecode_closure/make);
CL_DEF_CLASS_METHOD
Closure_sp Closure_O::make_bytecode_closure(BytecodeSimpleFun_sp entryPoint, size_t closedOverSlots) {
  Closure_sp closure = gctools::GC<core::Closure_O>::allocate_container<gctools::RuntimeStage>(false, closedOverSlots, entryPoint);
  return closure;
}

CL_LAMBDA(sfun core:&va-rest closed);
CL_DEFUN Closure_sp core__make_closure(SimpleFun_sp sfun, Vaslist_sp closed) {
  size_t nclosed = closed->nargs();
  Closure_sp closure = gctools::GC<core::Closure_O>::allocate_container<gctools::RuntimeStage>(false, nclosed, sfun);
  for (size_t idx = 0; idx < nclosed; ++idx)
    closure[idx] = closed->next_arg();
  return closure;
}

/* This function is used (currently exclusively) in funcallableInstance.cc.
 * It returns true if the function doesn't refer to any closure slots,
 * i.e., if the entry point ignores its first argument. */
bool Closure_O::openP() { return (this->_Slots.length() == 0); }

bool Function_O::compiledP() const { return !gc::IsA<BytecodeSimpleFun_sp>(this->entryPoint()); }

CL_DEFMETHOD T_sp Function_O::setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column) {
  T_mv sfi_mv = core__file_scope(sourceFile);
  FileScope_sp sfi = gc::As<FileScope_sp>(sfi_mv);
  this->setf_sourcePathname(sfi->pathname());
  this->setf_filePos(filePos);
  this->setf_lineno(lineno);
  this->setf_column(column);
  SourcePosInfo_sp spi = SourcePosInfo_O::create(sfi->fileHandle(), filePos, lineno, column);
  return spi;
}

CL_DEFMETHOD Pointer_sp Function_O::function_pointer() const { return Pointer_O::create((void*)this->entryPoint()->_EntryPoints[0]); }

SYMBOL_EXPORT_SC_(KeywordPkg, general_entry);
SYMBOL_EXPORT_SC_(KeywordPkg, local_function);
SYMBOL_EXPORT_SC_(KeywordPkg, trampoline);
CL_DOCSTRING("Return an alist of (cons entry-label pointer-or-nil )");
CL_DEFUN T_sp core__function_pointer_alist(Function_sp func) {
  SimpleFun_sp gep = func->entryPoint();
  ql::list res;
  res << Cons_O::create(kw::_sym_general_entry, Pointer_O::create((void*)gep->_EntryPoints[0]));
  for (size_t ii = 1; ii < NUMBER_OF_ENTRY_POINTS; ++ii) {
    void* ep = (void*)gep->_EntryPoints[ii];
    if (llvmo::general_entry_point_redirect_p(ep)) {
      res << Cons_O::create(make_fixnum(ii - 1 + ENTRY_POINT_ARITY_BEGIN), nil<T_O>());
    } else {
      res << Cons_O::create(make_fixnum(ii - 1 + ENTRY_POINT_ARITY_BEGIN), Pointer_O::create((void*)ep));
    }
  }
  if (gc::IsA<SimpleCoreFun_sp>(func->entryPoint())) {
    SimpleCoreFun_sp gsf = gc::As_unsafe<SimpleCoreFun_sp>(func->entryPoint());
    CoreFun_sp lep = gsf->localFun();
    res << Cons_O::create(kw::_sym_local_function, Pointer_O::create((void*)lep->_Entry));
  }
  return res.cons();
};

string Function_O::__repr__() const {
  T_sp name = this->functionName();
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString();
  ss << " " << _rep_(name);
#ifdef NON_MOVING_GC
  ss << "@" << (void*)this << " ";
#endif
  ss << ">";
  return ss.str();
}
}; // namespace core

namespace core {
char* global_dump_functions = NULL;

DOCGROUP(clasp);
CL_DEFUN size_t core__closure_size(size_t number_of_slots) {
  size_t result = gctools::sizeof_container_with_header<Closure_O>(number_of_slots);
  return result;
}

DOCGROUP(clasp);
CL_DEFUN size_t core__closure_length(Function_sp tclosure) {
  Closure_sp closure = gc::As_assert<Closure_sp>(tclosure);
  return closure->_Slots.length();
}

string Closure_O::__repr__() const {
  T_sp name = this->functionName();
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString();
#ifdef NON_MOVING_GC
  ss << "@" << (void*)this << " ";
#endif
  ss << " " << _rep_(name);
  ss << " lambda-list: " << _rep_(this->lambdaList());
  if (!this->entryPoint().unboundp()) {
    ss << " :fptr " << reinterpret_cast<void*>(this->entryPoint()->_EntryPoints[0]);
  }

  ss << ">";
  return ss.str();
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__closure_ref(Function_sp tclosure, size_t index) {
  if (Closure_sp closure = tclosure.asOrNull<Closure_O>()) {
    if (index >= closure->_Slots.length()) {
      SIMPLE_ERROR("Out of bounds closure reference - there are only {} slots", closure->_Slots.length());
    }
    return closure->_Slots[index];
  }
  SIMPLE_ERROR("Out of bounds closure reference - there are no slots");
}

DOCGROUP(clasp);
CL_DEFUN void core__closure_slots_dump(Function_sp closure) {
  size_t nslots = core__closure_length(closure);
  printf("Closure has %zu slots\n", nslots);
  for (int i = 0; i < nslots; ++i) {
    printf("    Slot[%d] --> %s\n", i, _rep_(core__closure_ref(closure, i)).c_str());
  }
}

struct UnboundCellFunctionEntryPoint {
  static inline LCC_RETURN LISP_CALLING_CONVENTION() {
    Closure_O* closure = gctools::untag_general<Closure_O*>((Closure_O*)lcc_closure);
    ERROR_UNDEFINED_FUNCTION((*closure)[0]);
  }
  template <typename... Ts>
  static inline LCC_RETURN entry_point_fixed(T_O* lcc_closure, Ts... args) {
    Closure_O* closure = gctools::untag_general<Closure_O*>((Closure_O*)lcc_closure);
    ERROR_UNDEFINED_FUNCTION((*closure)[0]);
  }
};

FunctionCell_sp FunctionCell_O::make(T_sp name, Function_sp fun) {
  SimpleFun_sp entryPoint = makeSimpleFunAndFunctionDescription<FunctionCell_O>(name);
  return gctools::GC<FunctionCell_O>::allocate(entryPoint, fun);
}

// grab the unbound cell entry point if it exists, otherwise make & cache.
// FIXME: thread safety?
SimpleFun_sp FunctionCell_O::cachedUnboundSimpleFun(T_sp name) {
  if (_lisp->_Roots._UnboundCellFunctionEntryPoint.unboundp())
    _lisp->_Roots._UnboundCellFunctionEntryPoint =
        makeSimpleFunAndFunctionDescription<UnboundCellFunctionEntryPoint>(name);
  return _lisp->_Roots._UnboundCellFunctionEntryPoint;
}

FunctionCell_sp FunctionCell_O::make(T_sp name) {
  Closure_sp cf = gctools::GC<core::Closure_O>::allocate_container<gctools::RuntimeStage>(false, 1, cachedUnboundSimpleFun(name));
  cf[0] = name;
  return FunctionCell_O::make(name, cf);
}

CL_LISPIFY_NAME(FunctionCell/make);
CL_LAMBDA(name &optional (initial nil initialp));
CL_DEFUN FunctionCell_sp core__make_function_cell(T_sp name,
                                                  T_sp initial, T_sp initialp) {
  if (initialp.nilp())
    return FunctionCell_O::make(name);
  else
    return FunctionCell_O::make(name, initial.as<Function_O>());
}

// KLUDGE: We have no CL_DEFMETHOD_SETF, so we do this
CL_LISPIFY_NAME(FunctionCell/function);
CL_DEFUN_SETF Function_sp core__function_cell_set_function(Function_sp nfun,
                                                           FunctionCell_sp cell) {
  cell->real_function_set(nfun);
  return nfun;
}

void FunctionCell_O::fmakunbound(T_sp name) {
  Closure_sp cf = gctools::GC<core::Closure_O>::allocate_container<gctools::RuntimeStage>(false, 1, cachedUnboundSimpleFun(name));
  cf[0] = name;
  real_function_set(cf);
}

bool FunctionCell_O::fboundp() const { return real_function()->entryPoint()->_EntryPoints[0] != (ClaspXepAnonymousFunction)UnboundCellFunctionEntryPoint::entry_point_n; }

Function_sp FunctionCell_O::fdefinition() const {
  // We don't use fboundp since we want to only load the real function
  // once, in order to avoid strange race condition nonsense.
  Function_sp rf = real_function();
  if (rf->entryPoint()->_EntryPoints[0] != (ClaspXepAnonymousFunction)UnboundCellFunctionEntryPoint::entry_point_n)
    return rf;
  else {
    // It's an unbound cell closure, so this will signal an
    // appropriate error.
    Closure_sp closure = gc::As_assert<Closure_sp>(rf);
    ERROR_UNDEFINED_FUNCTION((*closure)[0]);
  }
}

}; // namespace core
