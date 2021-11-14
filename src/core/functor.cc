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

// Keep track of how many interpreted closure calls there are
std::atomic<uint64_t> global_interpreted_closure_calls;

void CodeEntryPoint_O::fixupOneCodePointer( snapshotSaveLoad::Fixup* fixup, void** ptr) {
#ifdef USE_PRECISE_GC
  if ( snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::SaveOp) {
    uintptr_t* ptrptr = (uintptr_t*)&ptr[0];
    snapshotSaveLoad::encodeEntryPoint(fixup, ptrptr, this->_Code);
  } else if ( snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::LoadOp) {
    uintptr_t* ptrptr = (uintptr_t*)&ptr[0];
    snapshotSaveLoad::decodeEntryPoint(fixup,ptrptr,this->_Code);
  } else {
    SIMPLE_ERROR(BF("Illegal image save/load operation"));
  }
#endif
}

CL_DEFMETHOD Pointer_sp EntryPointBase_O::defaultEntryAddress() const {
  SUBCLASS_MUST_IMPLEMENT();
}


GlobalEntryPoint_O::GlobalEntryPoint_O(FunctionDescription_sp fdesc, const ClaspXepFunction& entry_point, llvmo::CodeBase_sp code, T_sp lep ) : CodeEntryPoint_O(fdesc, code), _EntryPoints(entry_point), _localEntryPoint(lep) {
    code->validateEntryPoint(entry_point);
  };
  
  LocalEntryPoint_O::LocalEntryPoint_O(FunctionDescription_sp fdesc, const ClaspLocalFunction& entry_point, llvmo::CodeBase_sp code ) : CodeEntryPoint_O(fdesc,code), _EntryPoint(entry_point) {
    code->validateEntryPoint(entry_point);
  };
  
  Pointer_sp GlobalEntryPoint_O::defaultEntryAddress() const {
    return Pointer_O::create((void*)this->_EntryPoints[0]);
  };

CL_LISPIFY_NAME("global-entry-point-code");
CL_DEFMETHOD
llvmo::Code_sp GlobalEntryPoint_O::code() const {
  llvmo::Code_sp code = gc::As<llvmo::Code_sp>(this->_Code);
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
  llvmo::Code_sp code = gc::As<llvmo::Code_sp>(this->_Code);
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
  llvmo::Code_sp code = gc::As<llvmo::Code_sp>(saCode.second());
  llvmo::ObjectFile_sp of = code->_ObjectFile;
  llvmo::DWARFContext_sp dwarfContext = llvmo::DWARFContext_O::createDWARFContext(of);
  auto addressRanges = getAddressRangesForAddressInner(dwarfContext, sa );
  if (addressRanges) {
    for ( auto range : addressRanges.get () ) {
      printf("%s:%d:%s Got address range %p %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)range.LowPC, (void*)range.HighPC );
      llvm::object::SectionedAddress sa;
      sa.Address = range.LowPC;
      sa.SectionIndex = code->_TextSectionId;
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
  return Pointer_O::create((void*)this->_EntryPoint);
};

void GlobalEntryPoint_O::fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup ) {
  if (snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::SaveOp) {
    llvmo::Code_sp code = gc::As_unsafe<llvmo::Code_sp>(this->_Code);
    if ((uintptr_t)this->_EntryPoints[0]<code->codeStart()) {
      printf("%s:%d:%s Entrypoint %p is before the codeStart %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)this->_EntryPoints[0], (void*)code->codeStart() );
      abort();
    }
  }
  for ( size_t ii=0; ii<ClaspXepFunction::Entries; ++ii ) {
    this->fixupOneCodePointer( fixup,(void**)&this->_EntryPoints._EntryPoints[ii]);
  }
};


void LocalEntryPoint_O::fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup) {
  if (snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::SaveOp) {
    llvmo::Code_sp code = gc::As_unsafe<llvmo::Code_sp>(this->_Code);
    if ((uintptr_t)this->_EntryPoint<code->codeStart()) {
      printf("%s:%d:%s Entrypoint %p is before the codeStart %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)this->_EntryPoint, (void*)code->codeStart() );
      abort();
    }
  }
  this->fixupOneCodePointer( fixup,(void**)&this->_EntryPoint);
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
  ss << (void*)this->_EntryPoint;
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
  llvmo::CodeBase_sp code = unbound<llvmo::CodeBase_O>();
  if (entry_point) {
    code = llvmo::identify_code_or_library(reinterpret_cast<gctools::clasp_ptr_t>(entry_point));
    if (gc::IsA<llvmo::Library_sp>(code)) {
      maybe_register_symbol_using_dladdr_ep((void*)entry_point);
    }
  }
  auto  ep = gctools::GC<LocalEntryPoint_O>::allocate( fdesc, entry_point, code );
  return ep;
}
GlobalEntryPoint_sp makeGlobalEntryPoint(FunctionDescription_sp fdesc,
                                         const ClaspXepFunction& entry_point,
                                         T_sp lep) {
  llvmo::CodeBase_sp code = unbound<llvmo::CodeBase_O>();
  if (entry_point._Defined) {
    code = llvmo::identify_code_or_library(reinterpret_cast<gctools::clasp_ptr_t>(entry_point._EntryPoints[0]));
    if (gc::IsA<llvmo::Library_sp>(code)) {
      for ( size_t ii=0; ii<ClaspXepFunction::Entries; ii++ ) {
        maybe_register_symbol_using_dladdr_ep((void*)entry_point._EntryPoints[ii]);
      }
    }
  }
  auto  ep = gctools::GC<GlobalEntryPoint_O>::allocate( fdesc, entry_point, code, lep );
  return ep;
}


GlobalEntryPoint_sp makeGlobalEntryPointCopy(GlobalEntryPoint_sp entryPoint,
                                             const ClaspXepFunction& entry_point) {
  llvmo::CodeBase_sp code = unbound<llvmo::CodeBase_O>();
  if (entry_point._Defined) {
    code = llvmo::identify_code_or_library(reinterpret_cast<gctools::clasp_ptr_t>(entry_point._EntryPoints[0]));
  }
  auto  ep = gctools::GC<GlobalEntryPoint_O>::allocate( entryPoint->_FunctionDescription, entry_point, code, entryPoint->_localEntryPoint );
  return ep;
}

LocalEntryPoint_sp makeLocalEntryPointFromGenerator(LocalEntryPointGenerator_sp original, void** entry_points) {
  if (!original->_entry_point_indices.consp()){
    SIMPLE_ERROR(BF("The LocalEntryPoint %s does not have entry-points") % _rep_(original));
  }
  T_sp firstEntryPoint = CONS_CAR(original->_entry_point_indices);
  if (!firstEntryPoint.fixnump()) {
    SIMPLE_ERROR(BF("The FunctionDescriptionGenerator %s does not have entry-points indices") % _rep_(original));
  }
  size_t entryPointIndex = firstEntryPoint.unsafe_fixnum();
  ClaspLocalFunction entry_point = (ClaspLocalFunction)(entry_points[entryPointIndex]);
  llvmo::CodeBase_sp code = unbound<llvmo::CodeBase_O>();
  if (entry_point) {
    code = llvmo::identify_code_or_library(reinterpret_cast<gctools::clasp_ptr_t>(entry_point));
  }
  auto entryPoint = gctools::GC< LocalEntryPoint_O>::allocate( original->_FunctionDescription, entry_point, code);
  return entryPoint;
}


GlobalEntryPoint_sp makeGlobalEntryPointFromGenerator(GlobalEntryPointGenerator_sp original, gctools::GCRootsInModule* roots, void** entry_points ) {
  if (!original->_entry_point_indices.consp()){
    SIMPLE_ERROR(BF("The GlobalEntryPoint %s does not have entry-points") % _rep_(original));
  }
  List_sp epIndices = gc::As<List_sp>(original->_entry_point_indices);
  size_t num = cl__length(epIndices);
  if ( num != ClaspXepFunction::Entries) {
    printf("%s:%d:%s %lu is not enough entry_points for a ClaspXepFunction expected %d\n", __FILE__, __LINE__, __FUNCTION__,
           num, ClaspXepFunction::Entries );
    abort();
  }
  ClaspXepFunction xepFunction((XepFilling()));
  size_t cur = 0;
  for ( auto entry : epIndices ) {
    T_sp oneEntryPointIndex = CONS_CAR(entry);
    if (!oneEntryPointIndex.fixnump()) {
      SIMPLE_ERROR(BF("The FunctionDescriptionGenerator %s does not have entry-points indices") % _rep_(original));
    }
    size_t entryPointIndex = oneEntryPointIndex.unsafe_fixnum();
    ClaspXepAnonymousFunction entry_point = (ClaspXepAnonymousFunction)(entry_points[entryPointIndex]);
    xepFunction._EntryPoints[cur] = entry_point;
    cur++;
  }
  T_sp localEntryPoint((gctools::Tagged)roots->getLiteral(original->_localEntryPointIndex));
  llvmo::CodeBase_sp code = unbound<llvmo::CodeBase_O>();
  code = llvmo::identify_code_or_library(reinterpret_cast<gctools::clasp_ptr_t>(xepFunction[0]));
  auto globalEntryPoint = gctools::GC< GlobalEntryPoint_O>::allocate( original->_FunctionDescription, xepFunction, code, localEntryPoint ); 
  return globalEntryPoint;
}

#if 0
FunctionDescription_sp makeFunctionDescriptionFromFunctionInfo(T_sp information,
                                                               claspFunction entry_point)
{
#define POP(rrr,lll) T_sp rrr = oCar(lll); lll = oCdr(lll);
  // THE order of entries MUST match cmpir.lsp function-info
  POP(function_info_name, information);
  POP(functionName, information);
  POP(lambdaList, information);
  POP(docstring, information);
  POP(declares, information);
  POP(sourcePathname,information);
  POP(tlineno,information);
  POP(tcolumn,information);
  POP(tfilepos,information);
  POP(form,information);
  int lineno = clasp_to_fixnum(tlineno);
  int column = clasp_to_fixnum(tcolumn);
  int filepos = clasp_to_fixnum(tfilepos);
  return makeFunctionDescription(functionName, entry_point,
                                 lambdaList, docstring,
                                 sourcePathname,
                                 lineno,
                                 column,
                                 filepos);
}
#endif


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
CL_DEFUN T_sp core__EntryPointBase_function_description(EntryPointBase_sp entryPoint) {
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
  uintptr_t start = lep->_Code->codeStart();
  uintptr_t abs = (uintptr_t)(lep->_EntryPoint);
  return Integer_O::create(abs - start);
}

DOCGROUP(clasp)
CL_DEFUN T_sp core__GlobalEntryPoint_relptr(GlobalEntryPoint_sp gep) {
  uintptr_t start = gep->_Code->codeStart();
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

void validateFunctionDescription(const char* filename, size_t lineno, Function_sp func) {
  T_sp functionName = func->functionName();
  if (functionName.unboundp()) {
    printf("FunctionDescription defined at %s:%zu  is missing functionName\n", filename, lineno);
    abort();
  }
  T_sp sourcePathname = func->sourcePathname();
  if (sourcePathname.unboundp()) {
    printf("FunctionDescription for function %s defined at %s:%zu  is missing sourcePathname\n", _rep_(functionName).c_str(), filename, lineno);
  }
  T_sp lambdaList = func->lambdaList();
  if (lambdaList.unboundp()) {
    printf("FunctionDescription for function %s defined at %s:%zu  is missing lambdaList\n", _rep_(functionName).c_str(), filename, lineno);
  }
  T_sp docstring = func->docstring();
  if (docstring.unboundp()) {
    printf("FunctionDescription for function %s defined at %s:%zu  is missing docstring\n", _rep_(functionName).c_str(), filename, lineno);
  }
}
};

extern "C" void dumpFunctionDescription(core::FunctionDescription_sp fdesc)
{
  core::write_bf_stream(BF("FunctionDescription@%p\n") % (void*)fdesc.raw_());
  core::write_bf_stream(BF("sourcePathname = %s\n") % _rep_(fdesc->sourcePathname()) );
  core::write_bf_stream(BF("functionName = %s\n") % _rep_(fdesc->functionName()));
  core::write_bf_stream(BF("lambdaList = %s\n") % _rep_(fdesc->lambdaList()));
  core::write_bf_stream(BF("docstring = %s\n") % _rep_(fdesc->docstring()));
  core::write_bf_stream(BF("declares = %s\n") % _rep_(fdesc->declares()));
  core::write_bf_stream(BF("lineno = %lu\n") % fdesc->lineno);
  core::write_bf_stream(BF("column = %lu\n") % fdesc->column);
  core::write_bf_stream(BF("filepos = %lu\n") % fdesc->filepos);
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
    SIMPLE_ERROR(BF("You can only dump function-descriptions from functions or function-descriptions"));
  }
}

CL_LISPIFY_NAME("core:functionSourcePos");
CL_DEFMETHOD T_mv Function_O::functionSourcePos() const {
  T_sp sfi = this->sourcePathname();
  return Values(sfi, make_fixnum(this->filePos()),
                make_fixnum(this->lineno()), make_fixnum(this->column()));
}

DOCGROUP(clasp)
CL_DEFUN T_sp core__function_docstring(Function_sp func) {
  return func->docstring();
}

CL_LISPIFY_NAME("core:function-docstring");
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
CL_DEFUN void core__verify_closure_with_slots(T_sp alist)
{
  expect_offset(core::_sym_entry_point,alist,offsetof(ClosureWithSlots_O,_EntryPoint)-gctools::general_tag);
  expect_offset(comp::_sym_closure_type,alist,offsetof(ClosureWithSlots_O,closureType)-gctools::general_tag);
  expect_offset(comp::_sym_data_length,alist,offsetof(ClosureWithSlots_O,_Slots._MaybeSignedLength)-gctools::general_tag);
  expect_offset(comp::_sym_data0,alist,offsetof(ClosureWithSlots_O,_Slots._Data)-gctools::general_tag);
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

struct InterpretedClosureEntryPoint {
  static inline LCC_RETURN LISP_CALLING_CONVENTION() {
    ClosureWithSlots_O* closure = gctools::untag_general<ClosureWithSlots_O*>((ClosureWithSlots_O*)lcc_closure);
    //  printf("%s:%d    closure name -> %s\n", __FILE__, __LINE__, _rep_(closure->functionName()).c_str());
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    ++global_interpreted_closure_calls;
    ValueEnvironment_sp newValueEnvironment = ValueEnvironment_O::createForLambdaListHandler(gc::As<LambdaListHandler_sp>((*closure)[INTERPRETED_CLOSURE_LAMBDA_LIST_HANDLER_SLOT]), (*closure)[INTERPRETED_CLOSURE_ENVIRONMENT_SLOT]);
    //  printf("%s:%d ValueEnvironment_O:createForLambdaListHandler llh: %s\n", __FILE__, __LINE__, _rep_(this->_lambdaListHandler).c_str());
    //  newValueEnvironment->dump();
    LambdaListHandler_sp llh = gc::As_unsafe<LambdaListHandler_sp>((*closure)[INTERPRETED_CLOSURE_LAMBDA_LIST_HANDLER_SLOT]);
    MAKE_SPECIAL_BINDINGS_HOLDER(numSpecials,specialsVLA,lisp_lambdaListHandlerNumberOfSpecialVariables(llh));
    ValueEnvironmentDynamicScopeManager scope(numSpecials,specialsVLA,newValueEnvironment);
    lambdaListHandler_createBindings(closure->asSmartPtr(), llh, &scope, lcc_nargs, lcc_args );
    //  printf("%s:%d     after lambdaListHandler_createbindings\n", __FILE__, __LINE__);
    //  newValueEnvironment->dump();
    ValueFrame_sp newActivationFrame = gc::As<ValueFrame_sp>(newValueEnvironment->getActivationFrame());
    return eval::sp_progn((*closure)[INTERPRETED_CLOSURE_FORM_SLOT], newValueEnvironment).as_return_type();
  };
    static inline LISP_ENTRY_0() {
    return entry_point_n(lcc_closure,0,NULL);
  }
  static inline LISP_ENTRY_1() {
    core::T_O* args[1] = {lcc_farg0};
    return entry_point_n(lcc_closure,1,args);
  }
  static inline LISP_ENTRY_2() {
    core::T_O* args[2] = {lcc_farg0,lcc_farg1};
    return entry_point_n(lcc_closure,2,args);
  }
  static inline LISP_ENTRY_3() {
    core::T_O* args[3] = {lcc_farg0,lcc_farg1,lcc_farg2};
    return entry_point_n(lcc_closure,3,args);
  }
  static inline LISP_ENTRY_4() {
    core::T_O* args[4] = {lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3};
    return entry_point_n(lcc_closure,4,args);
  }
  static inline LISP_ENTRY_5() {
    core::T_O* args[5] = {lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3,lcc_farg4};
    return entry_point_n(lcc_closure,5,args);
  }

};

    
ClosureWithSlots_sp ClosureWithSlots_O::make_interpreted_closure(T_sp name, T_sp type, T_sp lambda_list, LambdaListHandler_sp lambda_list_handler, T_sp declares, T_sp docstring, T_sp form, T_sp environment, core::Fixnum sourceFileInfoHandle, core::Fixnum filePos, core::Fixnum lineno, core::Fixnum column ) {
  FileScope_sp sfi = gc::As<FileScope_sp>(core__file_scope(core::make_fixnum(sourceFileInfoHandle)));
  GlobalEntryPoint_sp entryPoint = makeGlobalEntryPointAndFunctionDescription<InterpretedClosureEntryPoint>(name,nil<T_O>(),lambda_list,docstring,declares,sfi,lineno,column,filePos );
  ClosureWithSlots_sp closure =
    gctools::GC<core::ClosureWithSlots_O>::allocate_container(false,
                                                              INTERPRETED_CLOSURE_SLOTS,
                                                              entryPoint,
                                                              ClosureWithSlots_O::interpretedClosure);
  (*closure)[INTERPRETED_CLOSURE_FORM_SLOT] = form;
  (*closure)[INTERPRETED_CLOSURE_ENVIRONMENT_SLOT] = environment;
  if (lambda_list_handler.nilp()) {
    printf("%s:%d  A NIL lambda-list-handler was passed for %s lambdalist: %s\n", __FILE__, __LINE__, _rep_(name).c_str(), _rep_(lambda_list).c_str());
    abort();
  }
  (*closure)[INTERPRETED_CLOSURE_LAMBDA_LIST_HANDLER_SLOT] = lambda_list_handler;
  closure->setf_lambdaList(lambda_list_handler->lambdaList());
  closure->setf_docstring(docstring);
  validateFunctionDescription(__FILE__,__LINE__,closure);
  return closure;
}
ClosureWithSlots_sp ClosureWithSlots_O::make_bclasp_closure(T_sp name, const ClaspXepFunction& fn, T_sp type, T_sp lambda_list, T_sp environment, T_sp localEntryPoint ) {
  FunctionDescription_sp fdesc = makeFunctionDescription(name,lambda_list);
  core::GlobalEntryPoint_sp entryPoint = makeGlobalEntryPoint(fdesc,fn,localEntryPoint);
  ClosureWithSlots_sp closure = 
    gctools::GC<core::ClosureWithSlots_O>::allocate_container(false,
                                                              BCLASP_CLOSURE_SLOTS,
                                                              entryPoint,
                                                              ClosureWithSlots_O::bclaspClosure);
  (*closure)[BCLASP_CLOSURE_ENVIRONMENT_SLOT] = environment;
  closure->setf_sourcePathname(nil<T_O>());
  closure->setf_lambdaList(lambda_list);
  closure->setf_docstring(nil<T_O>());
  validateFunctionDescription(__FILE__,__LINE__,closure);
  return closure;
}

ClosureWithSlots_sp ClosureWithSlots_O::make_cclasp_closure(T_sp name, const ClaspXepFunction& fn, T_sp type, T_sp lambda_list, T_sp localEntryPoint, core::Fixnum sourceFileInfoHandle, core::Fixnum filePos, core::Fixnum lineno, core::Fixnum column ) {
  printf("%s:%d:%s What are you going to do with an unbound Code_O object\n", __FILE__, __LINE__, __FUNCTION__ );
  FunctionDescription_sp fdesc = makeFunctionDescription(name,lambda_list,nil<T_O>(),nil<T_O>(),nil<T_O>(),lineno,column);
  core::GlobalEntryPoint_sp entryPoint = makeGlobalEntryPoint(fdesc,fn,localEntryPoint);
  ClosureWithSlots_sp closure = 
    gctools::GC<core::ClosureWithSlots_O>::allocate_container(false,
                                                              0,
                                                              entryPoint,
                                                              ClosureWithSlots_O::cclaspClosure);
  closure->setf_lambdaList(lambda_list);
  closure->setf_docstring(nil<T_O>());
  validateFunctionDescription(__FILE__,__LINE__,closure);
  return closure;
}

T_sp ClosureWithSlots_O::interpretedSourceCode() {
  if (this->closureType==interpretedClosure) {
    return (*this)[INTERPRETED_CLOSURE_FORM_SLOT];
  };
  SIMPLE_ERROR(BF("Source code is only available for interpreted functions"));
}

/* This function is used (currently exclusively) in funcallableInstance.cc.
 * It returns true if the function doesn't refer to any closure slots,
 * i.e., if the entry point ignores its first argument. */
bool ClosureWithSlots_O::openP() {
  switch (this->closureType) {
  case bclaspClosure: return this->closedEnvironment().nilp();
  case cclaspClosure: return (this->_Slots.length() == 0);
  default: return false;
  }
}

DOCGROUP(clasp)
CL_DEFUN T_mv core__interpreted_closure_form(ClosureWithSlots_sp func) {
  return Values(func->interpretedSourceCode(),func->closedEnvironment());
}

DOCGROUP(clasp)
CL_DEFUN Integer_sp core__interpreted_closure_calls() {
  return Integer_O::create((Fixnum)global_interpreted_closure_calls);
}

#ifdef DEBUG_FUNCTION_CALL_COUNTER

DOCGROUP(clasp)
CL_DEFUN size_t core__function_call_counter(Function_sp f)
{
  return f->_TimesCalled;
}
#endif

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
  GlobalEntryPoint_sp gep = func->_EntryPoint.load();
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
    res << Cons_O::create(kw::_sym_local_function,Pointer_O::create((void*)lep->_EntryPoint));
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
void Closure_O::describeFunction() const {
  if (global_dump_functions) {
    printf("%s:%d  Closure_O %s entry@%p\n", __FILE__, __LINE__, _rep_(this->functionName()).c_str(), (void*)this->entry());
  }
}

DOCGROUP(clasp)
CL_DEFUN size_t core__closure_with_slots_size(size_t number_of_slots)
{
  size_t result = gctools::sizeof_container_with_header<ClosureWithSlots_O>(number_of_slots);
  return result;
}

DOCGROUP(clasp)
CL_DEFUN size_t core__closure_length(Closure_sp tclosure)
{
  ASSERT(gc::IsA<ClosureWithSlots_sp>(tclosure));
  ClosureWithSlots_sp closure = gc::As_unsafe<ClosureWithSlots_sp>(tclosure);
  if (closure->closureType == ClosureWithSlots_O::cclaspClosure) {
    return closure->_Slots.length();
  }
  if (tclosure->closedEnvironment().notnilp()) {
    return gc::As<ValueFrame_sp>(tclosure->closedEnvironment())->length();
  }
  return 0;
}

T_sp ClosureWithSlots_O::code() const {
  if (this->interpretedP()) {
    return (*this)[INTERPRETED_CLOSURE_FORM_SLOT];
  }
  SIMPLE_ERROR(BF("Tried to get code for a non interpreted closure"));
}


string ClosureWithSlots_O::__repr__() const {
  T_sp name = this->functionName();
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString();
#ifdef NON_MOVING_GC
  ss << "@" << (void*)this << " ";
#endif
  ss << " " << _rep_(name);
  ss << " :type ";
  switch (this->closureType) {
  case interpretedClosure:
      ss << "interpreted ";
      break;
  case bclaspClosure:
      ss << "bclasp ";
      break;
  case cclaspClosure:
      ss << "cclasp ";
      break;
  }
  ss << " lambda-list: " << _rep_(this->lambdaList());
  if ( !this->_EntryPoint.load().unboundp() ) {
    ss << " :fptr " << reinterpret_cast<void*>(this->entry());
  }
  
  ss << ">";
  return ss.str();
}



DOCGROUP(clasp)
CL_DEFUN T_sp core__closure_ref(Closure_sp tclosure, size_t index)
{
  if ( ClosureWithSlots_sp closure = tclosure.asOrNull<ClosureWithSlots_O>() ) {
    switch (closure->closureType) {
    case ClosureWithSlots_O::interpretedClosure:
    case ClosureWithSlots_O::bclaspClosure:
      {
        T_sp env = closure->closedEnvironment();
        if ( ValueEnvironment_sp ve = env.asOrNull<ValueEnvironment_O>() ) {
          env = ve->getActivationFrame();
        }
        if ( ValueFrame_sp tvf = env.asOrNull<ValueFrame_O>() ) {
          if ( index >= tvf->length() ) {
            SIMPLE_ERROR(BF("Out of bounds closure reference - there are only %d slots") % tvf->length() );
          }
          return (*tvf)[index];
        }
        SIMPLE_ERROR(BF("Out of bounds closure reference - there are no slots"));
      }
      break;
    case ClosureWithSlots_O::cclaspClosure:
        if ( index >= closure->_Slots.length() ) {
          SIMPLE_ERROR(BF("Out of bounds closure reference - there are only %d slots") % closure->_Slots.length() );
        }
        return closure->_Slots[index];
    }
  }
  SIMPLE_ERROR(BF("Out of bounds closure reference - there are no slots"));
}

DOCGROUP(clasp)
CL_DEFUN void core__closure_slots_dump(Closure_sp closure) {
  size_t nslots = core__closure_length(closure);
  printf("Closure has %zu slots\n", nslots);
  for ( int i=0; i<nslots; ++i ) {
    printf("    Slot[%d] --> %s\n", i, _rep_(core__closure_ref(closure,i)).c_str());
  }
  if (!closure->interpretedP()) {
    SourcePosInfo_sp spi = gc::As<SourcePosInfo_sp>(closure->sourcePosInfo());
    T_mv tsfi = core__file_scope(spi);
    if ( FileScope_sp sfi = tsfi.asOrNull<FileScope_O>() ) {
      printf("Closure source: %s:%d\n", sfi->namestring().c_str(), spi->lineno() );
    }
  }
}



void BuiltinClosure_O::fixupOneCodePointer( snapshotSaveLoad::Fixup* fixup, void** funcPtr, size_t sizeofFuncPtr ) {
#ifdef USE_PRECISE_GC
    // Virtual method pointers look different from function pointers - they are small integers
    //  here we assume a virtual method is always < 1024
  if ( snapshotSaveLoad::operation(fixup)==snapshotSaveLoad::SaveOp) {
    if ((uintptr_t)funcPtr[0] > 1024) {
      uintptr_t* ptrptr = (uintptr_t*)&funcPtr[0];
      snapshotSaveLoad::encodeEntryPointInLibrary(fixup,ptrptr);
    }
  } else if ( snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::LoadOp) {
    if ((uintptr_t)funcPtr[0] > 1024) {
      snapshotSaveLoad::decodeEntryPointInLibrary(fixup,(uintptr_t*)&funcPtr[0]);
    }
  } else {
    SIMPLE_ERROR(BF("Illegal image save/load operation"));
  }
#endif
}


//  printf("%s:%d    closure name -> %s\n", __FILE__, __LINE__, _rep_(closure->functionName()).c_str());



};

