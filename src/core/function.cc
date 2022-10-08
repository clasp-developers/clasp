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
//#define DEBUG_LEVEL_FULL
#include <clasp/core/foundation.h>
#include <clasp/gctools/snapshotSaveLoad.h>
#include <clasp/core/lisp.h>
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/package.h>
//#include "debugger.h"
#include <clasp/core/iterator.h>
#include <clasp/core/designators.h>
#include <clasp/core/primitives.h>
#include <clasp/core/instance.h>
#include <clasp/core/compiler.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/debugger.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/bytecode.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/lambdaListHandler.h>
//#i n c l u d e "environmentDependent.h"
#include <clasp/core/environment.h>
#include <clasp/core/evaluator.h>
// to avoid Generic to_object include headers here
#include <clasp/core/wrappers.h>
#include <clasp/llvmo/code.h>
#include <clasp/llvmo/debugInfoExpose.h>



namespace core {

bytecode_trampoline_function bytecode_trampoline = bytecode_call; // default

void CodeEntryPoint_O::fixupOneCodePointer( snapshotSaveLoad::Fixup* fixup, void** ptr) {
#ifdef USE_PRECISE_GC
  if ( snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::InfoOp) {
    uintptr_t* ptrptr = (uintptr_t*)&ptr[0];
    if (fixup->_trackAddressName) {
      fixup->addAddressName( (void*)*ptrptr, _rep_(this->_FunctionDescription->functionName()) );
    }
  } else if ( snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::SaveOp) {
    uintptr_t* ptrptr = (uintptr_t*)&ptr[0];
    snapshotSaveLoad::encodeEntryPoint(fixup, ptrptr, this->_Code);
  } else if ( snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::LoadOp) {
    uintptr_t* ptrptr = (uintptr_t*)&ptr[0];
    snapshotSaveLoad::decodeEntryPoint(fixup,ptrptr,this->_Code);
  } else {
    SIMPLE_ERROR(("Illegal image save/load operation"));
  }
#endif
}

CL_DEFMETHOD Pointer_sp EntryPoint_O::defaultEntryAddress() const {
  SUBCLASS_MUST_IMPLEMENT();
}

GlobalEntryPointBase_O::GlobalEntryPointBase_O(FunctionDescription_sp fdesc, const ClaspXepFunction& entry_point, T_sp code) :
    CodeEntryPoint_O(fdesc,code)
    , _EntryPoints(entry_point) {
  if (code.nilp()) {
    code = llvmo::identify_code_or_library(reinterpret_cast<gctools::clasp_ptr_t>(entry_point._EntryPoints[0]));
    this->_Code = code;
    if (gc::IsA<llvmo::Library_sp>(code)) {
      for ( size_t ii=0; ii<ClaspXepFunction::Entries; ii++ ) {
        maybe_register_symbol_using_dladdr_ep((void*)entry_point._EntryPoints[ii],sizeof(void*),"GlobalEntryPointBaseName",ii);
      }
    }
  }
}

void GlobalEntryPointBase_O::fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup )
{
  for ( size_t ii=0; ii<ClaspXepFunction::Entries; ++ii ) {
    this->fixupOneCodePointer( fixup,(void**)&this->_EntryPoints._EntryPoints[ii]);
  }
}

GlobalEntryPoint_O::GlobalEntryPoint_O(FunctionDescription_sp fdesc, const ClaspXepFunction& entry_point, T_sp code, T_sp lep ) : GlobalEntryPointBase_O(fdesc, entry_point, code), _localEntryPoint(lep) {
  llvmo::validateEntryPoint( code, entry_point );
};

GlobalBytecodeEntryPoint_O::GlobalBytecodeEntryPoint_O(FunctionDescription_sp fdesc,
                                                       const ClaspXepFunction& entry_point,
                                                       T_sp module,
                                                       uint16_t localsFrameSize,
                                                       unsigned int environmentSize,
                                                       unsigned int entryPcN )
: GlobalEntryPointBase_O(fdesc, entry_point, module),
  _LocalsFrameSize(localsFrameSize),
  _EnvironmentSize(environmentSize),
  _EntryPcN(entryPcN)
{
  llvmo::validateEntryPoint( module, entry_point );
};


CL_DEFMETHOD size_t GlobalBytecodeEntryPoint_O::entryPcN() const {
  return this->_EntryPcN;
}


LocalEntryPoint_O::LocalEntryPoint_O(FunctionDescription_sp fdesc, const ClaspLocalFunction& entry_point, T_sp code ) : CodeEntryPoint_O(fdesc,code), _Entry(entry_point) {
  llvmo::validateEntryPoint( code, entry_point );
};

Pointer_sp GlobalEntryPoint_O::defaultEntryAddress() const {
  return Pointer_O::create((void*)this->_EntryPoints[0]);
};

CL_LISPIFY_NAME("global-entry-point/code");
CL_DEFMETHOD
llvmo::ObjectFile_sp GlobalEntryPoint_O::code() const {
  llvmo::ObjectFile_sp code = gc::As<llvmo::ObjectFile_sp>(this->_Code);
  return code;
}

CL_LISPIFY_NAME("global-entry-point-local-entry-point");
CL_DEFMETHOD
T_sp GlobalEntryPoint_O::localEntryPoint() const {
  return this->_localEntryPoint;
}

CL_DEFMETHOD
T_mv GlobalEntryPoint_O::sectionedEntryInfo() const {
  char* address = (char*)this->_EntryPoints[0];
  llvmo::ObjectFile_sp code = gc::As<llvmo::ObjectFile_sp>(this->_Code);
  char* textStart = (char*)code->_TextSectionStart;
  uintptr_t sectionId = code->_TextSectionId;
  uintptr_t sectionAddress = (uintptr_t)(address-textStart);
  llvmo::SectionedAddress_sp sa = llvmo::SectionedAddress_O::create(sectionId, sectionAddress );
  return Values( sa, code );
}

CL_DEFMETHOD
T_sp GlobalEntryPoint_O::lineTable() const {
  T_mv saCode = this->sectionedEntryInfo();
  llvmo::SectionedAddress_sp sa = gc::As<llvmo::SectionedAddress_sp>(saCode);
  MultipleValues& mvn = core::lisp_multipleValues();
  llvmo::ObjectFile_sp of = gc::As<llvmo::ObjectFile_sp>(mvn.second(saCode.number_of_values()));
  llvmo::DWARFContext_sp dwarfContext = llvmo::DWARFContext_O::createDWARFContext(of);
  auto addressRanges = getAddressRangesForAddressInner(dwarfContext, sa );
  if (addressRanges) {
    for ( auto range : addressRanges.get () ) {
      printf("%s:%d:%s Got address range %p %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)range.LowPC, (void*)range.HighPC );
      llvm::object::SectionedAddress sa;
      sa.Address = range.LowPC;
      sa.SectionIndex = of->_TextSectionId;
      uintptr_t size = (uintptr_t)range.HighPC - (uintptr_t)range.LowPC;
      auto lineTable = (*dwarfContext).wrappedPtr()->getLineInfoForAddressRange(sa, size );
      printf("%s:%d:%s Number of entries: %lu\n", __FILE__, __LINE__, __FUNCTION__, lineTable.size());
      for ( auto ii : lineTable ) {
        printf("%s:%d:%s    first %p  second %u\n", __FILE__, __LINE__, __FUNCTION__, (void*)ii.first, ii.second.Line );
      }
    }
  }
  return nil<T_O>();
}


Pointer_sp LocalEntryPoint_O::defaultEntryAddress() const {
  return Pointer_O::create((void*)this->_Entry);
};

Pointer_sp GlobalBytecodeEntryPoint_O::defaultEntryAddress() const {
  return Pointer_O::create((void*)this->_EntryPoints[0]);
};

CL_LISPIFY_NAME("global-bytecode-entry-point/code");
CL_DEFMETHOD
BytecodeModule_sp GlobalBytecodeEntryPoint_O::code() const {
  BytecodeModule_sp code = gc::As<BytecodeModule_sp>(this->_Code);
  return code;
}

std::string GlobalBytecodeEntryPoint_O::__repr__() const {
  stringstream ss;
  ss << "#<GLOBAL-BYTECODE-ENTRY-POINT ";
  for ( size_t ii = 0; ii<NUMBER_OF_ENTRY_POINTS; ii++ ) {
    if (ii==0) {
      ss << "xep@";
    } else {
      ss << "xep" << (ii-1) << "@";
    }
    ss << (void*)this->_EntryPoints._EntryPoints[ii] << " ";
  }
  ss << " @" << (void*)this << ">";
  return ss.str();
}

void LocalEntryPoint_O::fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup) {
  if (snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::SaveOp) {
    llvmo::ObjectFile_sp code = gc::As_unsafe<llvmo::ObjectFile_sp>(this->_Code);
    if ((uintptr_t)this->_Entry<code->codeStart()) {
      printf("%s:%d:%s Entrypoint %p is before the codeStart %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)this->_Entry, (void*)code->codeStart() );
      abort();
    }
  }
  this->fixupOneCodePointer( fixup,(void**)&this->_Entry);
};



CL_LAMBDA(&key function-description entry-point-functions local-entry-point-index)
DOCGROUP(clasp)
CL_DEFUN GlobalEntryPointGenerator_sp core__makeGlobalEntryPointGenerator(FunctionDescription_sp fdesc,
                                                                          T_sp entryPointIndices,
                                                                          size_t localEntryPointIndex ) {
  auto entryPoint = gctools::GC<GlobalEntryPointGenerator_O>::allocate(fdesc,entryPointIndices,localEntryPointIndex);
//  printf("%s:%d:%s  entryPoint-> %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)entryPoint.raw_());
  return entryPoint;
}

CL_LAMBDA(&key function-description entry-point-functions)
DOCGROUP(clasp)
CL_DEFUN LocalEntryPointGenerator_sp core__makeLocalEntryPointGenerator(FunctionDescription_sp fdesc,
                                                                        T_sp entryPointIndices) {
  auto entryPoint = gctools::GC<LocalEntryPointGenerator_O>::allocate(fdesc,entryPointIndices);
//  printf("%s:%d:%s  entryPoint-> %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)entryPoint.raw_());
  return entryPoint;
}


std::string LocalEntryPointGenerator_O::__repr__() const {
  stringstream ss;
  ss << "#<LOCAL-ENTRY-POINT-GENERATOR ";
  ss << _rep_(this->_entry_point_indices) << " @" << (void*)this << ">";
  return ss.str();
}

std::string LocalEntryPoint_O::__repr__() const {
  stringstream ss;
  ss << "#<LOCAL-ENTRY-POINT ";
  ss << (void*)this->_Entry;
  ss << " @" << (void*)this << ">";
  return ss.str();
}

std::string GlobalEntryPoint_O::__repr__() const {
  stringstream ss;
  ss << "#<Global-ENTRY-POINT ";
  for ( size_t ii = 0; ii<NUMBER_OF_ENTRY_POINTS; ii++ ) {
    if (ii==0) {
      ss << "xep@";
    } else {
      ss << "xep" << (ii-1) << "@";
    }
    ss << (void*)this->_EntryPoints._EntryPoints[ii] << " ";
  }
  ss << " @" << (void*)this << ">";
  return ss.str();
}

CL_LISPIFY_NAME("global-entry-point-generator-local-entry-point-index");
CL_DEFMETHOD
size_t GlobalEntryPointGenerator_O::localEntryPointIndex() const {
  return this->_localEntryPointIndex;
}

  
std::string GlobalEntryPointGenerator_O::__repr__() const {
  stringstream ss;
  ss << "#<GLOBAL-ENTRY-POINT-GENERATOR ";
  ss << _rep_(this->_entry_point_indices) << " @" << (void*)this << ">";
  return ss.str();
}

CL_LISPIFY_NAME(FunctionDescription/make);
CL_LAMBDA(&key function-name lambda-list docstring declares source-pathname (lineno 0) (column 0) (filepos 0))
DOCGROUP(clasp)
CL_DEFUN FunctionDescription_sp core__makeFunctionDescription(T_sp functionName,
                              T_sp lambdaList,
                              T_sp docstring,
                              T_sp declares,
                              T_sp sourcePathname,
                              int lineno,
                              int column,
                              int filePos) {
  return makeFunctionDescription(functionName,
                                 lambdaList,
                                 docstring,
                                 declares,
                                 sourcePathname,
                                 lineno,
                                 column,
                                 filePos);
}

#if 0
GlobalEntryPoint_sp makeGlobalEntryPointAndFunctionDescription(T_sp functionName,
                                                               const ClaspXepFunction& entryPoint,
                                                               T_sp lambda_list,
                                                               T_sp docstring,
                                                               T_sp declares,
                                                               T_sp sourcePathname,
                                                               int lineno,
                                                               int column,
                                                               int filePos) {
  FunctionDescription_sp fdesc = makeFunctionDescription(functionName,
                                                         lambda_list,
                                                         docstring,
                                                         declares,
                                                         sourcePathname,
                                                         lineno,
                                                         column,
                                                         filePos );
  return makeGlobalEntryPoint(fdesc,entryPoint);
}
#endif

FunctionDescription_sp makeFunctionDescription(T_sp functionName,
                                                     T_sp lambda_list,
                                                     T_sp docstring,
                                                     T_sp declares,
                                                     T_sp sourcePathname,
                                                     int lineno,
                                                     int column,
                                                     int filePos) {
  auto fdesc = gctools::GC<FunctionDescription_O>::allocate();
  fdesc->_sourcePathname = sourcePathname;
  fdesc->_functionName = functionName;
  fdesc->_lambdaList = lambda_list;
  fdesc->_docstring = docstring;
  fdesc->_declares = declares;
  fdesc->lineno = lineno;
  fdesc->column = column;
  fdesc->filepos = filePos;
//  printf("%s:%d:%s @ %p   entry_point %p fdesc->_EntryPoints[0] = %p\n" = gctools::GC<"%s:%d:%s @ %p   entry_point %p fdesc->_EntryPoints[0] = %p\n">::allocate( __LINE__, __FUNCTION__, (void*)fdesc.raw_(), (void*)entry_point, (void*)fdesc->_EntryPoints[0]);
  return fdesc;
}

LocalEntryPoint_sp makeLocalEntryPoint(FunctionDescription_sp fdesc,
                                       const ClaspLocalFunction& entry_point) {
  T_sp code = unbound<llvmo::CodeBase_O>();
  if (entry_point) {
    code = llvmo::identify_code_or_library(reinterpret_cast<gctools::clasp_ptr_t>(entry_point));
    if (gc::IsA<llvmo::Library_sp>(code)) {
      maybe_register_symbol_using_dladdr_ep((void*)entry_point);
    }
  }
  auto ep = gctools::GC<LocalEntryPoint_O>::allocate( fdesc, entry_point, code );
  return ep;
}



GlobalEntryPoint_sp makeGlobalEntryPoint(FunctionDescription_sp tfdesc,
                                         const ClaspXepFunction& entry_point,
                                         T_sp lep) {
  T_sp code = unbound<T_O>();
  if (entry_point._Defined) {
    std::string name = "unkfunc";
    if (gc::IsA<FunctionDescription_sp>(tfdesc)) {
      FunctionDescription_sp fdesc = gc::As_unsafe<FunctionDescription_sp>(tfdesc);
//      printf("%s:%d:%s FunctionDescription is defined\n", __FILE__, __LINE__, __FUNCTION__ );
      name = "namenil";
      if (gc::IsA<Symbol_sp>(fdesc->functionName())) {
//        printf("%s:%d:%s FunctionDescription name is defined\n", __FILE__, __LINE__, __FUNCTION__ );
        Symbol_sp sname = gc::As_unsafe<Symbol_sp>(fdesc->functionName());
        name = sname->safeFormattedName();
      }
    }
    code = llvmo::identify_code_or_library(reinterpret_cast<gctools::clasp_ptr_t>(entry_point._EntryPoints[0]));
    if (gc::IsA<llvmo::Library_sp>(code)) {
      for ( size_t ii=0; ii<ClaspXepFunction::Entries; ii++ ) {
        maybe_register_symbol_using_dladdr_ep((void*)entry_point._EntryPoints[ii],sizeof(void*),name,ii);
      }
    }
  }
  auto  ep = gctools::GC<GlobalEntryPoint_O>::allocate( tfdesc, entry_point, code, lep );
  return ep;
}


GlobalEntryPoint_sp makeGlobalEntryPointCopy(GlobalEntryPoint_sp entryPoint,
                                             const ClaspXepFunction& entry_point) {
  T_sp code = unbound<T_O>();
  if (entry_point._Defined) {
    code = llvmo::identify_code_or_library(reinterpret_cast<gctools::clasp_ptr_t>(entry_point._EntryPoints[0]));
  }
  auto ep = gctools::GC<GlobalEntryPoint_O>::allocate( entryPoint->_FunctionDescription, entry_point, code, entryPoint->_localEntryPoint );
  return ep;
}

SYMBOL_EXPORT_SC_(CorePkg,bytecode_call);

struct BytecodeClosureEntryPoint {
  static inline LCC_RETURN bytecode_enter(T_O* lcc_closure, size_t lcc_nargs, T_O** lcc_args ) {
    Closure_O* closure = gctools::untag_general<Closure_O*>((Closure_O*)lcc_closure);
    core::GlobalBytecodeEntryPoint_sp entryPoint = gctools::As_unsafe<core::GlobalBytecodeEntryPoint_sp>(closure->entryPoint());
    ASSERT(gc::IsA<core::BytecodeModule_sp>(entryPoint->_Code));
    core::BytecodeModule_sp module = gctools::As_unsafe<core::BytecodeModule_sp>(entryPoint->_Code);
    int entryPcOffset = entryPoint->_EntryPcN;
    ASSERT(gc::IsA<Array_sp>(module->_Bytecode));
    unsigned char* pc =  (unsigned char*)&(gc::As_unsafe<SimpleVector_byte8_t_sp>(module->_Bytecode)->_Data[0]) + entryPcOffset;
    return (bytecode_trampoline)( pc, lcc_closure, lcc_nargs, lcc_args );
  }

  static inline LCC_RETURN LISP_CALLING_CONVENTION() {
    return bytecode_enter( lcc_closure, lcc_nargs, lcc_args );
  }

  static inline LISP_ENTRY_0() {
    return bytecode_enter( lcc_closure, 0, NULL );
  }
  static inline LISP_ENTRY_1() {
    core::T_O* args[1] = {lcc_farg0};
    return bytecode_enter( lcc_closure, 1, args );
  }
  static inline LISP_ENTRY_2() {
    core::T_O* args[2] = {lcc_farg0,lcc_farg1};
    return bytecode_enter( lcc_closure, 2, args );
  }
  static inline LISP_ENTRY_3() {
    core::T_O* args[3] = {lcc_farg0,lcc_farg1,lcc_farg2};
    return bytecode_enter( lcc_closure, 3, args );
  }
  static inline LISP_ENTRY_4() {
    core::T_O* args[4] = {lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3};
    return bytecode_enter( lcc_closure, 4, args );
  }
  static inline LISP_ENTRY_5() {
    core::T_O* args[5] = {lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3,lcc_farg4};
    return bytecode_enter( lcc_closure, 5, args );
  }
};


CL_LISPIFY_NAME(GlobalBytecodeEntryPoint/make);
CL_DEFUN
GlobalBytecodeEntryPoint_sp core__makeGlobalBytecodeEntryPoint(FunctionDescription_sp fdesc,
                                                               BytecodeModule_sp module,
                                                               size_t localsFrameSize,
                                                               size_t environmentSize,
                                                               size_t pcIndex )
{
  size_t idx = 0;
  ClaspXepFunction xep;
  xep.setup<BytecodeClosureEntryPoint>();
  auto entryPoint = gctools::GC<GlobalBytecodeEntryPoint_O>::allocate( fdesc,
                                                                       xep,
                                                                       module,
                                                                       localsFrameSize,
                                                                       environmentSize,
                                                                       pcIndex);
  return entryPoint;
}

LocalEntryPoint_sp makeLocalEntryPointFromGenerator(LocalEntryPointGenerator_sp original, void** entry_points) {
  if (!original->_entry_point_indices.consp()){
    SIMPLE_ERROR(("The LocalEntryPoint %s does not have entry-points") , _rep_(original));
  }
  T_sp firstEntryPoint = CONS_CAR(original->_entry_point_indices);
  if (!firstEntryPoint.fixnump()) {
    SIMPLE_ERROR(("The FunctionDescriptionGenerator %s does not have entry-points indices") , _rep_(original));
  }
  size_t entryPointIndex = firstEntryPoint.unsafe_fixnum();
  ClaspLocalFunction entry_point = (ClaspLocalFunction)(entry_points[entryPointIndex]);
  T_sp code = unbound<T_O>();
  if (entry_point) {
    code = llvmo::identify_code_or_library(reinterpret_cast<gctools::clasp_ptr_t>(entry_point));
  }
  auto entryPoint = gctools::GC< LocalEntryPoint_O>::allocate( original->_FunctionDescription, entry_point, code);
  return entryPoint;
}


GlobalEntryPoint_sp makeGlobalEntryPointFromGenerator(GlobalEntryPointGenerator_sp original, gctools::GCRootsInModule* roots, void** entry_points ) {
  if (!original->_entry_point_indices.consp()){
    SIMPLE_ERROR(("The GlobalEntryPoint %s does not have entry-points") , _rep_(original));
  }
  List_sp epIndices = gc::As<List_sp>(original->_entry_point_indices);
  size_t num = cl__length(epIndices);
  if ( num != ClaspXepFunction::Entries) {
    printf("%s:%d:%s %lu is not enough entry_points for a ClaspXepFunction expected %d\n", __FILE__, __LINE__, __FUNCTION__,
           num, ClaspXepFunction::Entries );
    abort();
  }
  ClaspXepFunction xepFunction((XepFilling())); // Need extra paranetheses to disambiguate
  size_t cur = 0;
  for ( auto entry : epIndices ) {
    T_sp oneEntryPointIndex = CONS_CAR(entry);
    if (!oneEntryPointIndex.fixnump()) {
      SIMPLE_ERROR(("The FunctionDescriptionGenerator %s does not have entry-points indices") , _rep_(original));
    }
    size_t entryPointIndex = oneEntryPointIndex.unsafe_fixnum();
    ClaspXepAnonymousFunction entry_point = (ClaspXepAnonymousFunction)(entry_points[entryPointIndex]);
    xepFunction._EntryPoints[cur] = entry_point;
    cur++;
  }
  T_sp localEntryPoint((gctools::Tagged)roots->getLiteral(original->_localEntryPointIndex));
  T_sp code = unbound<T_O>();
  code = llvmo::identify_code_or_library(reinterpret_cast<gctools::clasp_ptr_t>(xepFunction[0]));
  auto globalEntryPoint = gctools::GC< GlobalEntryPoint_O>::allocate( original->_FunctionDescription, xepFunction, code, localEntryPoint ); 
  return globalEntryPoint;
}



DOCGROUP(clasp)
CL_DEFUN T_sp core__FunctionDescription_sourcePathname(FunctionDescription_sp fdesc) {
  return fdesc->_sourcePathname;
}

DOCGROUP(clasp)
CL_DEFUN T_sp core__FunctionDescription_functionName(FunctionDescription_sp fdesc) {
  return fdesc->_functionName;
}

DOCGROUP(clasp)
CL_DEFUN T_sp core__FunctionDescription_lambdaList(FunctionDescription_sp fdesc) {
  return fdesc->_lambdaList;
}

DOCGROUP(clasp)
CL_DEFUN T_sp core__FunctionDescription_docstring(FunctionDescription_sp fdesc) {
  return fdesc->_docstring;
}
DOCGROUP(clasp)
CL_DEFUN T_sp core__FunctionDescription_declares(FunctionDescription_sp fdesc) {
  return fdesc->_declares;
}

DOCGROUP(clasp)
CL_DEFUN size_t core__FunctionDescription_lineno(FunctionDescription_sp fdesc) {
  return fdesc->lineno;
}
DOCGROUP(clasp)
CL_DEFUN size_t core__FunctionDescription_column(FunctionDescription_sp fdesc) {
  return fdesc->column;
}
DOCGROUP(clasp)
CL_DEFUN size_t core__FunctionDescription_filepos(FunctionDescription_sp fdesc) {
  return fdesc->filepos;
}

DOCGROUP(clasp)
CL_DEFUN T_sp core__EntryPoint_function_description(EntryPoint_sp entryPoint) {
  return entryPoint->_FunctionDescription;
}

DOCGROUP(clasp)
CL_DEFUN T_sp core__GlobalEntryPointGenerator_entry_point_indices(GlobalEntryPointGenerator_sp fdesc) {
  return fdesc->_entry_point_indices;
}

DOCGROUP(clasp)
CL_DEFUN T_sp core__LocalEntryPointGenerator_entry_point_indices(LocalEntryPointGenerator_sp fdesc) {
  return fdesc->_entry_point_indices;
}

DOCGROUP(clasp)
CL_DEFUN T_sp core__set_GlobalEntryPoint_entry_point_indices(GlobalEntryPointGenerator_sp fdesc, T_sp entry_point_indices) {
  return fdesc->_entry_point_indices = entry_point_indices;
}

DOCGROUP(clasp)
CL_DEFUN T_sp core__set_LocalEntryPoint_entry_point_indices(LocalEntryPointGenerator_sp fdesc, T_sp entry_point_indices) {
  return fdesc->_entry_point_indices = entry_point_indices;
}

DOCGROUP(clasp)
CL_DEFUN T_sp core__LocalEntryPoint_relptr(LocalEntryPoint_sp lep) {
  uintptr_t start = llvmo::codeStart(lep);
  uintptr_t abs = (uintptr_t)(lep->_Entry);
  return Integer_O::create(abs - start);
}

DOCGROUP(clasp)
CL_DEFUN T_sp core__GlobalEntryPoint_relptr(GlobalEntryPoint_sp gep) {
  uintptr_t start = llvmo::codeStart(gep);
  uintptr_t abs = (uintptr_t)(gep->_EntryPoints[0]);
  return Integer_O::create(abs - start);
}

CL_LISPIFY_NAME(function-description-equal);
CL_DEFMETHOD bool FunctionDescription_O::function_description_equal(T_sp other) const {
  if (gc::IsA<FunctionDescription_sp>(other)) {
    FunctionDescription_sp fdother = gc::As_unsafe<FunctionDescription_sp>(other);
    return this->filepos == fdother->filepos
      && this->lineno == fdother->lineno
      && this->column == fdother->column
      && cl__equal(this->_sourcePathname, fdother->_sourcePathname)
      && cl__equal(this->_functionName, fdother->_functionName)
      && cl__equal(this->_docstring, fdother->_docstring)
      && cl__equal(this->_declares, fdother->_declares)
      && cl__equal(this->_lambdaList, fdother->_lambdaList);
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
  clasp_sxhash(this->_sourcePathname,hg);
  clasp_sxhash(this->_functionName,hg);
  clasp_sxhash(this->_docstring,hg);
  clasp_sxhash(this->_declares,hg);
  clasp_sxhash(this->_lambdaList,hg);
  return hg.rawhash();
}

T_sp FunctionDescription_O::sourcePathname() const {
  return this->_sourcePathname;
}


void FunctionDescription_O::setf_sourcePathname(T_sp sourceFileName) {
  this->_sourcePathname = sourceFileName;
}

T_sp FunctionDescription_O::functionName() const {
  return this->_functionName;
}

void FunctionDescription_O::setf_functionName(T_sp name) {
  this->_functionName = name;
}

T_sp FunctionDescription_O::lambdaList() const {
  return this->_lambdaList;
}

void FunctionDescription_O::setf_lambdaList(T_sp lambda_list) {
  this->_lambdaList = lambda_list;
}

T_sp FunctionDescription_O::docstring() const {
  return this->_docstring;
}

T_sp FunctionDescription_O::declares() const {
  return this->_declares;
}

void FunctionDescription_O::setf_docstring(T_sp x) {
  this->_docstring = x;
}

};

extern "C" void dumpFunctionDescription(core::FunctionDescription_sp fdesc)
{
  core::write_bf_stream(fmt::sprintf("FunctionDescription@%p\n" , (void*)fdesc.raw_()));
  core::write_bf_stream(fmt::sprintf("sourcePathname = %s\n" , _rep_(fdesc->sourcePathname()) ));
  core::write_bf_stream(fmt::sprintf("functionName = %s\n" , _rep_(fdesc->functionName())));
  core::write_bf_stream(fmt::sprintf("lambdaList = %s\n" , _rep_(fdesc->lambdaList())));
  core::write_bf_stream(fmt::sprintf("docstring = %s\n" , _rep_(fdesc->docstring())));
  core::write_bf_stream(fmt::sprintf("declares = %s\n" , _rep_(fdesc->declares())));
  core::write_bf_stream(fmt::sprintf("lineno = %lu\n" , fdesc->lineno));
  core::write_bf_stream(fmt::sprintf("column = %lu\n" , fdesc->column));
  core::write_bf_stream(fmt::sprintf("filepos = %lu\n" , fdesc->filepos));
} ;

namespace core {

DOCGROUP(clasp)
CL_DEFUN void core__dumpFunctionDescription(T_sp func)
{
  if (gc::IsA<Function_sp>(func)) {
    FunctionDescription_sp fdesc = gc::As<Function_sp>(func)->fdesc();
    dumpFunctionDescription(fdesc);
  } else if (gc::IsA<FunctionDescription_sp>(func)) {
    dumpFunctionDescription(gc::As<FunctionDescription_sp>(func));
  } else {
    SIMPLE_ERROR(("You can only dump function-descriptions from functions or function-descriptions"));
  }
}


ClaspXepGeneralFunction Function_O::entry() const { return (ClaspXepGeneralFunction)(gc::As_unsafe<GlobalEntryPoint_sp>(this->entryPoint())->_EntryPoints[0]); }
ClaspXep0Function Function_O::entry_0() const { return (ClaspXep0Function)(gc::As_unsafe<GlobalEntryPoint_sp>(this->entryPoint())->_EntryPoints[1]); }
ClaspXep1Function Function_O::entry_1() const { return (ClaspXep1Function)(gc::As_unsafe<GlobalEntryPoint_sp>(this->entryPoint())->_EntryPoints[2]); }
ClaspXep2Function Function_O::entry_2() const { return (ClaspXep2Function)(gc::As_unsafe<GlobalEntryPoint_sp>(this->entryPoint())->_EntryPoints[3]); }
ClaspXep3Function Function_O::entry_3() const { return (ClaspXep3Function)(gc::As_unsafe<GlobalEntryPoint_sp>(this->entryPoint())->_EntryPoints[4]); }
ClaspXep4Function Function_O::entry_4() const { return (ClaspXep4Function)(gc::As_unsafe<GlobalEntryPoint_sp>(this->entryPoint())->_EntryPoints[5]); }
ClaspXep5Function Function_O::entry_5() const { return (ClaspXep5Function)(gc::As_unsafe<GlobalEntryPoint_sp>(this->entryPoint())->_EntryPoints[6]); }
FunctionDescription_sp Function_O::fdesc() const { return this->entryPoint()->_FunctionDescription; };

CL_LISPIFY_NAME("core:functionSourcePos");
CL_DEFMETHOD T_mv Function_O::functionSourcePos() const {
  T_sp sfi = this->sourcePathname();
  return Values(sfi, make_fixnum(this->filePos()),
                make_fixnum(this->lineno()), make_fixnum(this->column()));
}

DOCGROUP(clasp)
CL_DOCSTRING("Return the documentation string associated with the function.")
CL_DEFUN T_sp core__function_docstring(Function_sp func) {
  return func->docstring();
}

CL_LISPIFY_NAME("core:function-docstring");
CL_DOCSTRING("Set the documentation string associated with the function.")
DOCGROUP(clasp)
CL_DEFUN_SETF T_sp setf_function_docstring(T_sp doc, Function_sp func) {
  func->setf_docstring(doc);
  return doc;
}

SYMBOL_SC_(CompPkg,vtable);
SYMBOL_SC_(CompPkg,entry);
SYMBOL_EXPORT_SC_(CorePkg,entry_point);
SYMBOL_SC_(CompPkg,closure_type);
SYMBOL_SC_(CompPkg,data_length);
SYMBOL_SC_(CompPkg,data0);


DOCGROUP(clasp)
CL_DEFUN void core__verify_closure(T_sp alist)
{
  expect_offset(core::_sym_entry_point,alist,offsetof(Closure_O,_TheEntryPoint)-gctools::general_tag);
  expect_offset(comp::_sym_closure_type,alist,offsetof(Closure_O,closureType)-gctools::general_tag);
  expect_offset(comp::_sym_data_length,alist,offsetof(Closure_O,_Slots._MaybeSignedLength)-gctools::general_tag);
  expect_offset(comp::_sym_data0,alist,offsetof(Closure_O,_Slots._Data)-gctools::general_tag);
}

SYMBOL_EXPORT_SC_(KeywordPkg,function_description);
SYMBOL_EXPORT_SC_(KeywordPkg,code);
SYMBOL_EXPORT_SC_(KeywordPkg,entry_points);
SYMBOL_EXPORT_SC_(KeywordPkg,defined);

DOCGROUP(clasp)
CL_DEFUN void core__verify_global_entry_point(T_sp alist)
{
  expect_offset(kw::_sym_function_description,alist,offsetof(GlobalEntryPoint_O,_FunctionDescription)-gctools::general_tag);
  expect_offset(kw::_sym_code,alist,offsetof(GlobalEntryPoint_O,_Code)-gctools::general_tag);
  expect_offset(kw::_sym_entry_points,alist,offsetof(GlobalEntryPoint_O,_EntryPoints._EntryPoints)-gctools::general_tag);
  expect_offset(kw::_sym_defined,alist,offsetof(GlobalEntryPoint_O,_EntryPoints._Defined)-gctools::general_tag);
}


CL_LISPIFY_NAME(bytecode_closure/make);
CL_DEF_CLASS_METHOD
Closure_sp Closure_O::make_bytecode_closure(GlobalBytecodeEntryPoint_sp entryPoint, size_t closedOverSlots ) {
  Closure_sp closure =
      gctools::GC<core::Closure_O>::allocate_container<gctools::RuntimeStage>(false,
                                                                              closedOverSlots,
                                                                              entryPoint,
                                                                              Closure_O::bytecodeClosure);
  return closure;
}

Closure_sp Closure_O::make_cclasp_closure(T_sp name, const ClaspXepFunction& fn, T_sp type, T_sp lambda_list, T_sp localEntryPoint, core::Fixnum sourceFileInfoHandle, core::Fixnum filePos, core::Fixnum lineno, core::Fixnum column ) {
  printf("%s:%d:%s What are you going to do with an unbound Code_O object\n", __FILE__, __LINE__, __FUNCTION__ );
  FunctionDescription_sp fdesc = makeFunctionDescription(name,lambda_list,nil<T_O>(),nil<T_O>(),nil<T_O>(),lineno,column);
  core::GlobalEntryPoint_sp entryPoint = makeGlobalEntryPoint(fdesc,fn,localEntryPoint);
  Closure_sp closure = 
      gctools::GC<core::Closure_O>::allocate_container<gctools::RuntimeStage>(false,
                                                                                       0,
                                                                                       entryPoint,
                                                                                       Closure_O::cclaspClosure);
  closure->setf_lambdaList(lambda_list);
  closure->setf_docstring(nil<T_O>());
  return closure;
}

/* This function is used (currently exclusively) in funcallableInstance.cc.
 * It returns true if the function doesn't refer to any closure slots,
 * i.e., if the entry point ignores its first argument. */
bool Closure_O::openP() {
  switch (this->closureType) {
  case cclaspClosure: return (this->_Slots.length() == 0);
  default: return false;
  }
}

#ifdef DEBUG_FUNCTION_CALL_COUNTER

DOCGROUP(clasp)
CL_DEFUN size_t core__function_call_counter(Function_sp f)
{
  return f->_TimesCalled;
}
#endif

bool Function_O::compiledP() const {
  return !gc::IsA<GlobalBytecodeEntryPoint_sp>(this->entryPoint());
}
  
CL_DEFMETHOD T_sp Function_O::setSourcePosInfo(T_sp sourceFile,
                                               size_t filePos, int lineno, int column) {
  T_mv sfi_mv = core__file_scope(sourceFile);
  FileScope_sp sfi = gc::As<FileScope_sp>(sfi_mv);
  this->setf_sourcePathname(sfi->pathname());
  this->setf_filePos(filePos);
  this->setf_lineno(lineno);
  this->setf_column(column);
  SourcePosInfo_sp spi = SourcePosInfo_O::create(sfi->fileHandle(), filePos, lineno, column);
  return spi;
}

CL_DEFMETHOD Pointer_sp Function_O::function_pointer() const {
  return Pointer_O::create((void*)this->entry());
};

SYMBOL_EXPORT_SC_(KeywordPkg, general_entry);
SYMBOL_EXPORT_SC_(KeywordPkg, local_function);
SYMBOL_EXPORT_SC_(KeywordPkg, trampoline);
CL_DOCSTRING("Return an alist of (cons entry-label pointer-or-nil )");
CL_DEFUN T_sp core__function_pointer_alist(Function_sp func) {
  ASSERT(gc::IsA<GlobalEntryPoint_sp>(func->entryPoint()));
  GlobalEntryPoint_sp gep = gc::As_unsafe<GlobalEntryPoint_sp>(func->entryPoint());
  ql::list res;
  res << Cons_O::create( kw::_sym_general_entry, Pointer_O::create((void*)gep->_EntryPoints._EntryPoints[0]) );
  for ( size_t ii=1; ii< NUMBER_OF_ENTRY_POINTS; ++ii ) {
    void* ep = (void*)gep->_EntryPoints._EntryPoints[ii];
    if (llvmo::general_entry_point_redirect_p(ep)) {
      res << Cons_O::create(make_fixnum(ii-1+ENTRY_POINT_ARITY_BEGIN),nil<T_O>());
    } else {
      res << Cons_O::create(make_fixnum(ii-1+ENTRY_POINT_ARITY_BEGIN),Pointer_O::create((void*)ep));
    }
  }
  T_sp tlep = gep->localEntryPoint();
  if (tlep.notnilp()) {
    LocalEntryPoint_sp lep = gc::As<LocalEntryPoint_sp>(tlep);
    res << Cons_O::create(kw::_sym_local_function,Pointer_O::create((void*)lep->_Entry));
  }
  return res.cons();
};

string Function_O::__repr__() const {
  T_sp name = this->functionName();
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString();
#if 1
  ss << " " << _rep_(name);
#else
#ifdef NON_MOVING_GC
  ss << "@" << (void*)this << " ";
#endif
  ss << " " << _rep_(name);
  ss << " lambda-list: " << _rep_(this->lambdaList());
  if ( this->entry != NULL ) {
    ss << " :fptr " << reinterpret_cast<void*>(this->entry());
  }
#endif
  ss << ">";
  return ss.str();
}
};


namespace core {
char* global_dump_functions = NULL;
#if 0
void Closure_O::describeFunction() const {
  if (global_dump_functions) {
    printf("%s:%d  Closure_O %s entry@%p\n", __FILE__, __LINE__, _rep_(this->functionName()).c_str(), (void*)this->entry());
  }
}
#endif

DOCGROUP(clasp)
CL_DEFUN size_t core__closure_size(size_t number_of_slots)
{
  size_t result = gctools::sizeof_container_with_header<Closure_O>(number_of_slots);
  return result;
}

DOCGROUP(clasp)
CL_DEFUN size_t core__closure_length(Function_sp tclosure)
{
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
  ss << " :type ";
  switch (this->closureType) {
  case bytecodeClosure:
      ss << "bytecode ";
      break;
  case cclaspClosure:
      ss << "cclasp ";
      break;
  }
  ss << " lambda-list: " << _rep_(this->lambdaList());
  if ( !this->entryPoint().unboundp() ) {
    ss << " :fptr " << reinterpret_cast<void*>(this->entry());
  }
  
  ss << ">";
  return ss.str();
}



DOCGROUP(clasp)
CL_DEFUN T_sp core__closure_ref(Function_sp tclosure, size_t index)
{
  if ( Closure_sp closure = tclosure.asOrNull<Closure_O>() ) {
    switch (closure->closureType) {
    case Closure_O::bytecodeClosure:
        printf("%s:%d:%s Add support for looking up slot %lu in bytecodeClosure\n", __FILE__, __LINE__, __FUNCTION__, index );
        return nil<core::T_O>();
        break;
    case Closure_O::cclaspClosure:
        if ( index >= closure->_Slots.length() ) {
          SIMPLE_ERROR(("Out of bounds closure reference - there are only %d slots") , closure->_Slots.length() );
        }
        return closure->_Slots[index];
    }
  }
  SIMPLE_ERROR(("Out of bounds closure reference - there are no slots"));
}

DOCGROUP(clasp)
CL_DEFUN void core__closure_slots_dump(Function_sp closure) {
  size_t nslots = core__closure_length(closure);
  printf("Closure has %zu slots\n", nslots);
  for ( int i=0; i<nslots; ++i ) {
    printf("    Slot[%d] --> %s\n", i, _rep_(core__closure_ref(closure,i)).c_str());
  }
}
};

