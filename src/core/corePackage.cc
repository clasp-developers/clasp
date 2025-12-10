/*
    File: corePackage.cc
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
#include <limits.h>
#include <float.h>
#include <stdio.h>
#include <sys/wait.h>
#include <cfenv>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/array.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/allClSymbols.h>
#include <clasp/core/commonLispPackage.h>
#include <clasp/core/keywordPackage.h>
#include <clasp/core/extensionPackage.h>
#include <clasp/core/lightProfiler.h>
#include <clasp/core/package.h>
#include <clasp/core/compPackage.h>
#include <clasp/core/grayPackage.h>
#include <clasp/core/closPackage.h>
#include <clasp/core/cleavirPrimopsPackage.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/random.h>
#include <clasp/core/ql.h>
#include <clasp/core/readtable.h>
#include <clasp/core/commonLispUserPackage.h>
#include <clasp/core/lispReader.h>
#include <clasp/core/numerics.h>
#include <clasp/core/bootStrapCoreSymbolMap.h>

// ------------------- include all headers for corePackage here

#include <clasp/core/object.h>
#include <clasp/clbind/class_registry.h>
#include <clasp/core/character.h>
#include <clasp/core/cons.h>
#include <clasp/core/cxxObject.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/externalObject.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/primitives.h>
#include <clasp/core/iterator.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/userData.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/numbers.h>
#include <clasp/core/package.h>
#include <clasp/core/pathname.h>
#include <clasp/core/pointer.h>
#include <clasp/core/random.h>
#include <clasp/core/readtable.h>
#include <clasp/core/record.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/core/smallMap.h>
#include <clasp/core/smallMultimap.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/core/symbol.h>
#include <clasp/core/lispList.h>
#include <clasp/core/weakPointer.h>
#include <clasp/core/wrappedPointer.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/debugInfoExpose.h>
#include <clasp/llvmo/debugLoc.h>
#include <clasp/llvmo/insertPoint.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/hashTableEqualp.h>
#include <clasp/core/instance.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/null.h>
#include <clasp/core/singleDispatchGenericFunction.h>
#include <clasp/core/metaClass.h>
#include <clasp/core/bignum.h>
#include <clasp/clbind/class_rep.h>

//
// Load the gctools::GcInfo<core-classes>::Kind specializers
//
#define NAMESPACE_core
#include <clasp/gctools/gc_interface.h>
#undef NAMESPACE_core

namespace core {
const char* CorePkg_nicknames[] = {
    "SYSTEM", "sys", "SYS", "si", "SI", "" /*guard*/
};

SYMBOL_EXPORT_SC_(CorePkg, STARallCxxClassesSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARallow_with_interruptsSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARbacktraceSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARbuild_cppflagsSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARbuild_libSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARbuild_linkflagsSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARbuild_stlibSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARbuiltin_function_namesSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARbuiltin_macro_function_namesSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARbuiltin_setf_function_namesSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARbuiltin_single_dispatch_method_namesSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARcache_macroexpandSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARcircleCounterSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARcircle_counterSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARcircle_stackSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARcommandLineLoadSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARcore_startup_functionSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARcurrentSourcePosInfoSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARvariableSourceInfosSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARcurrent_dlopen_handleSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARcxxDocumentationSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugByteCodeSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugConditionSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugEvalSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugFlowControlSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugInterpretedClosureSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugInterpretedFunctionsSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugLoadTimeValuesSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugReaderSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugSourcePosInfoSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugStartupSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugVaslistSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebug_accessorsSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebug_dtree_interpreterSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebug_fastgfSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebug_hash_tableSTAR)
SYMBOL_EXPORT_SC_(CorePkg, STARdebug_symbol_lookupSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebug_threadsSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebug_valuesSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdrag_native_callsSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARenablePrintPrettySTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARenvironment_debugSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARexit_backtraceSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARextension_systemsSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARfunctions_to_inlineSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARfunctions_to_notinlineSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARihs_baseSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARihs_currentSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARihs_modeSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARihs_topSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARinitialize_hooksSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARinterrupts_enabledSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARllvmFunctionNameHookSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARllvmVersionSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARloadHooksSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARloadSearchListSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARmpi_rankSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARmpi_sizeSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARnumber_of_entry_pointsSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARpollTicksPerGcSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARprint_denseSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARquasiquoteSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARreader_cst_resultSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARreader_generate_cstSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARsave_hookSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARserializerArchiveSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARterminate_hooksSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARtopLevelCommandHookSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARuseBuildForkRedirectSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARuseParallelBuildSTAR);
SYMBOL_EXPORT_SC_(CorePkg, _BANG_unbound_BANG_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_WNOHANG_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_application_name_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_bitcode_name_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_clasp_ctor_function_name_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_class_name_to_lisp_name_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_executable_name_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_gcroots_in_module_name_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_io_syntax_progv_list_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_known_typep_predicates_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_literals_name_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_numberOfFixedArguments_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_run_all_function_name_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_standardReadtable_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_type_header_value_map_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_variant_name_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_fe_divbyzero_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_fe_inexact_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_fe_invalid_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_fe_underflow_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_fe_overflow_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, arguments);
SYMBOL_EXPORT_SC_(CorePkg, array_out_of_bounds);
SYMBOL_EXPORT_SC_(CorePkg, breakstep);
SYMBOL_EXPORT_SC_(CorePkg, c_local);
SYMBOL_EXPORT_SC_(CorePkg, circle_subst);
SYMBOL_EXPORT_SC_(CorePkg, class_source_location)
SYMBOL_EXPORT_SC_(CorePkg, clbind_cxx_class);
SYMBOL_EXPORT_SC_(CorePkg, closedStream);
SYMBOL_EXPORT_SC_(CorePkg, coerce_fdesignator);
SYMBOL_EXPORT_SC_(CorePkg, creator);
SYMBOL_EXPORT_SC_(CorePkg, cxx_class);
SYMBOL_EXPORT_SC_(CorePkg, cxx_method_source_location);
SYMBOL_EXPORT_SC_(CorePkg, debug_message);
SYMBOL_EXPORT_SC_(CorePkg, defcallback);
SYMBOL_EXPORT_SC_(CorePkg, derivable_cxx_class);
SYMBOL_EXPORT_SC_(CorePkg, derivable_cxx_object);
SYMBOL_EXPORT_SC_(CorePkg, dump_module);
SYMBOL_EXPORT_SC_(CorePkg, dynamicGo);
SYMBOL_EXPORT_SC_(CorePkg, every_list);
SYMBOL_EXPORT_SC_(CorePkg, fileDoesNotExist);
SYMBOL_EXPORT_SC_(CorePkg, fileExists);
SYMBOL_EXPORT_SC_(CorePkg, fillArrayWithElt);
SYMBOL_EXPORT_SC_(CorePkg, fillPointerSet);
SYMBOL_EXPORT_SC_(CorePkg, fixnump);
SYMBOL_EXPORT_SC_(CorePkg, float_infinity_string);
SYMBOL_EXPORT_SC_(CorePkg, foreign_call);
SYMBOL_EXPORT_SC_(CorePkg, foreign_call_pointer);
SYMBOL_EXPORT_SC_(CorePkg, index);
SYMBOL_EXPORT_SC_(CorePkg, instance);
SYMBOL_EXPORT_SC_(CorePkg, lambdaList);
SYMBOL_EXPORT_SC_(CorePkg, lambdaName);
SYMBOL_EXPORT_SC_(CorePkg, loadSource);
SYMBOL_EXPORT_SC_(CorePkg, load_binary);
SYMBOL_EXPORT_SC_(CorePkg, load_bytecode);
SYMBOL_EXPORT_SC_(CorePkg, load_faso);
SYMBOL_EXPORT_SC_(CorePkg, load_fasobc);
SYMBOL_EXPORT_SC_(CorePkg, load_fasoll);
SYMBOL_EXPORT_SC_(CorePkg, localGo);
SYMBOL_EXPORT_SC_(CorePkg, make_source_pos_info);
SYMBOL_EXPORT_SC_(CorePkg, multiple_value_foreign_call);
SYMBOL_EXPORT_SC_(CorePkg, noCatchTag);
SYMBOL_EXPORT_SC_(CorePkg, oddKeywords);
SYMBOL_EXPORT_SC_(CorePkg, outOfExtentUnwind);
SYMBOL_EXPORT_SC_(CorePkg, replaceArray);
SYMBOL_EXPORT_SC_(CorePkg, row_major_out_of_bounds);
SYMBOL_EXPORT_SC_(CorePkg, scharSet);
SYMBOL_EXPORT_SC_(CorePkg, sequence_out_of_bounds);
SYMBOL_EXPORT_SC_(CorePkg, setf_documentation);
SYMBOL_EXPORT_SC_(CorePkg, setf_subseq);
SYMBOL_EXPORT_SC_(CorePkg, sharp_a_reader);
SYMBOL_EXPORT_SC_(CorePkg, sharp_s_reader);
SYMBOL_EXPORT_SC_(CorePkg, sharpmacros_lisp_redefine);
SYMBOL_EXPORT_SC_(CorePkg, signalSimpleError);
SYMBOL_EXPORT_SC_(CorePkg, signal_servicing);
SYMBOL_EXPORT_SC_(CorePkg, simpleControlError);
SYMBOL_EXPORT_SC_(CorePkg, simpleFileError);
SYMBOL_EXPORT_SC_(CorePkg, simplePackageError);
SYMBOL_EXPORT_SC_(CorePkg, simpleParseError);
SYMBOL_EXPORT_SC_(CorePkg, simpleProgramError);
SYMBOL_EXPORT_SC_(CorePkg, simpleReaderError);
SYMBOL_EXPORT_SC_(CorePkg, simpleStreamError);
SYMBOL_EXPORT_SC_(CorePkg, single_dispatch_method);
SYMBOL_EXPORT_SC_(CorePkg, single_float_p);
SYMBOL_EXPORT_SC_(CorePkg, size_t);
SYMBOL_EXPORT_SC_(CorePkg, some_list);
SYMBOL_EXPORT_SC_(CorePkg, syntax_type);
SYMBOL_EXPORT_SC_(CorePkg, threadInfo);
SYMBOL_EXPORT_SC_(CorePkg, tmv);
SYMBOL_EXPORT_SC_(CorePkg, topLevel);
SYMBOL_EXPORT_SC_(CorePkg, tsp);
SYMBOL_EXPORT_SC_(CorePkg, type_assertions);
SYMBOL_EXPORT_SC_(CorePkg, unquote);
SYMBOL_EXPORT_SC_(CorePkg, unquote_nsplice);
SYMBOL_EXPORT_SC_(CorePkg, unquote_splice);
SYMBOL_EXPORT_SC_(CorePkg, valist);
SYMBOL_EXPORT_SC_(CorePkg, wrongNumberOfArguments);
SYMBOL_SC_(CorePkg, DOT);
SYMBOL_SC_(CorePkg, STARPATHSTAR);
SYMBOL_SC_(CorePkg, STARargsSTAR);
SYMBOL_SC_(CorePkg, STARbackquote_expand_hookSTAR);
SYMBOL_SC_(CorePkg, STARdebugMacroexpandSTAR);
SYMBOL_SC_(CorePkg, STARdebugMonitorSTAR);
SYMBOL_SC_(CorePkg, STARdocumentation_databaseSTAR);
SYMBOL_SC_(CorePkg, STARdocumentation_poolSTAR);
SYMBOL_SC_(CorePkg, STARechoReplReadSTAR);
SYMBOL_SC_(CorePkg, STARenvironmentPrintingTabIncrementSTAR);
SYMBOL_SC_(CorePkg, STARenvironmentPrintingTabSTAR);
SYMBOL_SC_(CorePkg, STARnestedErrorDepthSTAR);
SYMBOL_SC_(CorePkg, STARprintPackageSTAR);
SYMBOL_SC_(CorePkg, STARsharpEqContextSTAR);
SYMBOL_SC_(CorePkg, STARsystem_defsetf_update_functionsSTAR);
SYMBOL_SC_(CorePkg, __init__);
SYMBOL_SC_(CorePkg, adjustable);
SYMBOL_SC_(CorePkg, alist);
SYMBOL_SC_(CorePkg, all_keys);
SYMBOL_SC_(CorePkg, anonymous);
SYMBOL_SC_(CorePkg, backquote);
SYMBOL_SC_(CorePkg, color);
SYMBOL_SC_(CorePkg, create);
SYMBOL_SC_(CorePkg, declaredSpecial);
SYMBOL_SC_(CorePkg, default);
SYMBOL_SC_(CorePkg, dimensions);
SYMBOL_SC_(CorePkg, dot);
SYMBOL_SC_(CorePkg, double_backquote);
SYMBOL_SC_(CorePkg, element_type);
SYMBOL_SC_(CorePkg, end);
SYMBOL_SC_(CorePkg, eof_error_p);
SYMBOL_SC_(CorePkg, eof_value);
SYMBOL_SC_(CorePkg, foreach);
SYMBOL_SC_(CorePkg, generic);
SYMBOL_SC_(CorePkg, globalFunction);
SYMBOL_SC_(CorePkg, globalSetfFunction);
SYMBOL_SC_(CorePkg, ifDoesNotExist);
SYMBOL_SC_(CorePkg, initial_element);
SYMBOL_SC_(CorePkg, input_stream);
SYMBOL_SC_(CorePkg, input_stream_designator);
SYMBOL_SC_(CorePkg, invalidKeywordArgumentError);
SYMBOL_SC_(CorePkg, invokeInternalDebugger);
SYMBOL_SC_(CorePkg, io);
SYMBOL_SC_(CorePkg, item);
SYMBOL_SC_(CorePkg, key);
SYMBOL_SC_(CorePkg, lambda_with_handler);
SYMBOL_SC_(CorePkg, lexical);
SYMBOL_SC_(CorePkg, lexicalFunction);
SYMBOL_SC_(CorePkg, macro);
SYMBOL_SC_(CorePkg, monitorReader);
SYMBOL_SC_(CorePkg, newVersion);
SYMBOL_SC_(CorePkg, object);
SYMBOL_SC_(CorePkg, okey);
SYMBOL_SC_(CorePkg, overwrite);
SYMBOL_SC_(CorePkg, preserve_whitespace);
SYMBOL_SC_(CorePkg, printf);
SYMBOL_SC_(CorePkg, probe);
SYMBOL_SC_(CorePkg, recursive_p);
SYMBOL_SC_(CorePkg, renameAndDelete);
SYMBOL_SC_(CorePkg, setThrowPosition);
SYMBOL_SC_(CorePkg, single_dispatch_on);
SYMBOL_SC_(CorePkg, slot);
SYMBOL_SC_(CorePkg, start);
SYMBOL_SC_(CorePkg, supersede);
SYMBOL_SC_(CorePkg, swapElements);
SYMBOL_SC_(CorePkg, symbolMacroletLambda);
SYMBOL_SC_(CorePkg, test_not);
SYMBOL_SC_(CorePkg, universalErrorHandler);
SYMBOL_SC_(CorePkg, unrecognizedKeywordArgumentError);

void setNilable(gc::Nilable<String_sp>& val, bool s) {
  if (!s) {
    val = nil<String_O>();
  } else {
    val = gc::As_unsafe<String_sp>(SimpleBaseString_O::make("Yahoo"));
  }
}

gc::Nilable<String_sp> getNilable(bool s) {
  gc::Nilable<String_sp> val;
  if (!s) {
    val = nil<String_O>();
  } else {
    val = gc::As_unsafe<String_sp>(SimpleBaseString_O::make("Yahoo"));
  }
  return val;
}

void testNilable() {
  gc::Nilable<String_sp> foo;
  printf("%s:%d initialized foo = %s\n", __FILE__, __LINE__, _rep_(static_cast<T_sp>(foo)).c_str());
  foo = gc::As_unsafe<String_sp>(SimpleBaseString_O::make("This is a test"));
  printf("%s:%d assigned foo = %s  nilp=%d\n", __FILE__, __LINE__, _rep_(foo).c_str(), foo.nilp());
  foo = nil<T_O>();
  printf("%s:%d nil'd foo = %s  nilp=%d\n", __FILE__, __LINE__, _rep_(foo).c_str(), foo.nilp());
  setNilable(foo, false);
  printf("%s:%d set false foo = %s  nilp=%d\n", __FILE__, __LINE__, _rep_(foo).c_str(), foo.nilp());
  setNilable(foo, true);
  printf("%s:%d set true foo = %s  nilp=%d\n", __FILE__, __LINE__, _rep_(foo).c_str(), foo.nilp());
  foo = getNilable(false);
  printf("%s:%d get false foo = %s  nilp=%d\n", __FILE__, __LINE__, _rep_(foo).c_str(), foo.nilp());
  foo = getNilable(true);
  printf("%s:%d get true foo = %s  nilp=%d\n", __FILE__, __LINE__, _rep_(foo).c_str(), foo.nilp());
}

void testFeatures() {
  testNilable();
}

CoreExposer_O::CoreExposer_O(LispPtr lisp) : Exposer_O(lisp, CorePkg){};

__attribute((optnone)) void CoreExposer_O::expose(core::LispPtr lisp, WhatToExpose what) const {
  switch (what) {
  case candoClasses:
    break;
  case candoFunctions:
    exposeCando_Numerics();
    exposeCore_lisp_reader();
    {
      Readtable_sp readtable = Readtable_O::create_standard_readtable();
      cl::_sym_STARreadtableSTAR->defparameter(readtable);
      _sym__PLUS_standardReadtable_PLUS_->defconstant(Readtable_O::create_standard_readtable());
    }
    break;
  case candoGlobals: {
    // expose the CorePkg constants here
    //----------- symbols are created in lisp.cc::startupLispEnvironment ----------
    // #define SYMBOLS_CREATE
    // #i n c l u d e SYMBOLS_SCRAPED_INC_H
    //-----------------------------------------------------------------------------
  }

  break;
  case pythonClasses: {
    IMPLEMENT_MEF("Handle other packages");
  } break;
  case pythonFunctions:
    break;
  case pythonGlobals:
    // expose globals here
    break;
  }
}

void CoreExposer_O::define_essential_globals(LispPtr lisp) {
  {
    Package_sp package = gc::As<Package_sp>(_lisp->findPackage(this->packageName()));
    package->usePackage(gc::As<Package_sp>(_lisp->findPackage("CL", true)));
#define CorePkg_EXPORT
#define DO_SYMBOL(ns, cname, idx, pkgName, lispName, export) cname->exportYourself(export);
#ifndef SCRAPING
#include SYMBOLS_SCRAPED_INC_H
#endif
#undef DO_SYMBOL
#undef CorePkg_EXPORT
  };
  /* Set the values of some essential global symbols */
  cl::_sym_nil = gctools::smart_ptr<core::Symbol_O>((gctools::Tagged)gctools::global_tagged_Symbol_OP_nil); //->initialize();
  cl::_sym_nil->setf_name(SimpleBaseString_O::make("NIL"));
  //        printf("%s:%d About to add NIL to the COMMON-LISP package - is it defined at this point\n", __FILE__, __LINE__ );
  //	_lisp->_Roots._CommonLispPackage->add_symbol_to_package("NIL"cl::_sym_nil);
  cl::_sym_nil->setPackage(_lisp->_Roots._CommonLispPackage);
  cl::_sym_nil->setf_symbolValue(nil<T_O>());
  cl::_sym_nil->makeSpecial();
  cl::_sym_nil->exportYourself();
  cl::_sym_nil->setReadOnly(true);
  _lisp->commonLispPackage()->add_symbol_to_package(SimpleBaseString_O::make("NIL"), nil<Symbol_O>(), true);
  _lisp->_Roots._TrueObject = cl::_sym_T_O;
  _lisp->_Roots._NilObject = nil<core::T_O>();
  cl::_sym_T_O->exportYourself()->defparameter(_lisp->_Roots._TrueObject);
  cl::_sym_T_O->setReadOnly(true);
  cl::_sym_STARload_printSTAR->exportYourself()->defparameter(_lisp->_false());
  cl::_sym_STARload_verboseSTAR->exportYourself()->defparameter(_lisp->_false());
  cl::_sym_STARread_suppressSTAR->exportYourself()->defparameter(_lisp->_false());
  cl::_sym_STARpackageSTAR->exportYourself()->defparameter(lisp->_Roots._CorePackage);
  _sym_STARpreserve_whitespace_pSTAR->defparameter(_lisp->_false());
  _sym_STARechoReplReadSTAR->exportYourself()->defparameter(_lisp->_false());
  _sym_STARbackquote_levelSTAR->defparameter(make_fixnum(0));
  cl::_sym_STARmodulesSTAR->defparameter(nil<T_O>());
  cl::_sym_STARread_evalSTAR->defparameter(_lisp->_true());
  _sym_STARenvironmentPrintingTabSTAR->defparameter(make_fixnum(0));
  _sym_STARenvironmentPrintingTabIncrementSTAR->defparameter(make_fixnum(6));
  _sym_STARenvironment_debugSTAR->defparameter(nil<T_O>());
  SYMBOL_EXPORT_SC_(ClPkg, most_negative_fixnum);
  cl::_sym_most_negative_fixnum->defconstant(make_fixnum(MOST_NEGATIVE_FIXNUM));
  SYMBOL_EXPORT_SC_(ClPkg, most_positive_fixnum);
  cl::_sym_most_positive_fixnum->defconstant(make_fixnum(MOST_POSITIVE_FIXNUM));

  cl::_sym_STARread_baseSTAR->defparameter(make_fixnum(10));
  SYMBOL_EXPORT_SC_(CorePkg, cl_fixnum_bits);
  _sym_cl_fixnum_bits->defconstant(make_fixnum(gc::fixnum_bits));
  SYMBOL_EXPORT_SC_(ClPkg, array_rank_limit);
  cl::_sym_array_rank_limit->defconstant(make_fixnum(CLASP_ARRAY_RANK_LIMIT));
  SYMBOL_EXPORT_SC_(ClPkg, char_code_limit);
  cl::_sym_char_code_limit->defconstant(make_fixnum(CHAR_CODE_LIMIT));
  cl::_sym_STARgensym_counterSTAR->defparameter(make_fixnum(0));
  cl::_sym_STARdefaultPathnameDefaultsSTAR->defparameter(Pathname_O::create());
  cl::_sym_STARprint_arraySTAR->defparameter(_lisp->_true());
  cl::_sym_STARprint_baseSTAR->defparameter(make_fixnum(10));
  cl::_sym_STARprint_caseSTAR->defparameter(kw::_sym_upcase);
  cl::_sym_STARprint_circleSTAR->defparameter(nil<T_O>());
  cl::_sym_STARprint_escapeSTAR->defparameter(_lisp->_true());
  cl::_sym_STARprint_gensymSTAR->defparameter(_lisp->_true());
  cl::_sym_STARprint_lengthSTAR->defparameter(nil<T_O>());
  cl::_sym_STARprint_levelSTAR->defparameter(nil<T_O>());
  cl::_sym_STARprint_linesSTAR->defparameter(nil<T_O>());
  cl::_sym_STARprint_miser_widthSTAR->defparameter(nil<T_O>());
  cl::_sym_STARprint_pprint_dispatchSTAR->defparameter(nil<T_O>());
  _sym_STARenablePrintPrettySTAR->defparameter(nil<T_O>()); // _lisp->_true()); // Just for debugging *print-pretty*
  cl::_sym_STARprint_prettySTAR->defparameter(nil<T_O>());
  cl::_sym_STARprint_radixSTAR->defparameter(nil<T_O>());
  cl::_sym_STARprint_readablySTAR->defparameter(nil<T_O>());
  core::_sym_STARprint_denseSTAR->defparameter(nil<T_O>());
  cl::_sym_STARprint_right_marginSTAR->defparameter(nil<T_O>());

  //        testPointers();

  T_sp stdin_stream = CFileStream_O::make(str_create("*STDIN*"), stdin, StreamDirection::input);
  T_sp stdout_stream = CFileStream_O::make(str_create("*STDOUT*"), stdout, StreamDirection::output);
  T_sp stderr_stream = CFileStream_O::make(str_create("*STDERR*"), stderr, StreamDirection::output);
  TwoWayStream_sp terminal_stream = TwoWayStream_O::make(stdin_stream, stdout_stream);
  terminal_stream->echo_p() = true;

  {
    HashTable_sp vsis = HashTable_O::createEqWeakKey();
    vsis->setupThreadSafeHashTable();
    _sym_STARvariableSourceInfosSTAR->defparameter(vsis);
  }

  ext::_sym__PLUS_processStandardInput_PLUS_->defparameter(stdin_stream);
  ext::_sym__PLUS_processStandardOutput_PLUS_->defparameter(stdout_stream);
  ext::_sym__PLUS_processErrorOutput_PLUS_->defparameter(stderr_stream);
  ext::_sym__PLUS_process_terminal_io_PLUS_->defparameter(terminal_stream);
  _sym_STARcurrentSourcePosInfoSTAR->defparameter(nil<T_O>());
  cl::_sym_STARstandard_inputSTAR->defparameter(SynonymStream_O::make(ext::_sym__PLUS_processStandardInput_PLUS_));
  cl::_sym_STARstandard_outputSTAR->defparameter(SynonymStream_O::make(ext::_sym__PLUS_processStandardOutput_PLUS_));
  cl::_sym_STARerror_outputSTAR->defparameter(SynonymStream_O::make(ext::_sym__PLUS_processErrorOutput_PLUS_));
  cl::_sym_STARtrace_outputSTAR->defparameter(SynonymStream_O::make(ext::_sym__PLUS_processErrorOutput_PLUS_));
  T_sp debug_stream = SynonymStream_O::make(ext::_sym__PLUS_process_terminal_io_PLUS_);
  cl::_sym_STARdebug_ioSTAR->defparameter(debug_stream);
  cl::_sym_STARquery_ioSTAR->defparameter(debug_stream);
  T_sp terminal_syn = SynonymStream_O::make(ext::_sym__PLUS_process_terminal_io_PLUS_);
  _lisp->_Roots._TerminalIO = terminal_syn;
  cl::_sym_STARterminal_ioSTAR->defparameter(terminal_syn);
  _sym_STARdocumentation_poolSTAR->defparameter(
      Cons_O::createList(HashTable_O::createEql(), SimpleBaseString_O::make("help_file.dat")));
  _sym_STARdocumentation_poolSTAR->exportYourself();
  _sym_STARsystem_defsetf_update_functionsSTAR->defparameter(nil<T_O>());
  cl::_sym_STARmacroexpand_hookSTAR->defparameter(cl::_sym_funcall);
  _sym_STARsharp_equal_final_tableSTAR->defparameter(nil<T_O>());
  _sym__PLUS_variant_name_PLUS_->defconstant(SimpleBaseString_O::make(VARIANT_NAME));
  _sym__PLUS_bitcode_name_PLUS_->defconstant(SimpleBaseString_O::make(BITCODE_NAME));
  _sym__PLUS_executable_name_PLUS_->defconstant(SimpleBaseString_O::make(EXECUTABLE_NAME));
  _sym__PLUS_application_name_PLUS_->defconstant(SimpleBaseString_O::make(APP_NAME));
  _sym__PLUS_fe_divbyzero_PLUS_->defconstant(clasp_make_fixnum(FE_DIVBYZERO));
  _sym__PLUS_fe_inexact_PLUS_->defconstant(clasp_make_fixnum(FE_INEXACT));
  _sym__PLUS_fe_invalid_PLUS_->defconstant(clasp_make_fixnum(FE_INVALID));
  _sym__PLUS_fe_underflow_PLUS_->defconstant(clasp_make_fixnum(FE_UNDERFLOW));
  _sym__PLUS_fe_overflow_PLUS_->defconstant(clasp_make_fixnum(FE_OVERFLOW));
  _sym_STARbuild_libSTAR->defconstant(SimpleBaseString_O::make(BUILD_LIB));
  _sym_STARbuild_stlibSTAR->defconstant(SimpleBaseString_O::make(BUILD_STLIB));
  _sym_STARbuild_linkflagsSTAR->defconstant(SimpleBaseString_O::make(BUILD_LINKFLAGS));
  _sym_STARbuild_cppflagsSTAR->defconstant(SimpleBaseString_O::make(BUILD_CPPFLAGS));
  _sym__PLUS_run_all_function_name_PLUS_->defconstant(SimpleBaseString_O::make(RUN_ALL_FUNCTION_NAME));
  _sym__PLUS_clasp_ctor_function_name_PLUS_->defconstant(SimpleBaseString_O::make(CLASP_CTOR_FUNCTION_NAME));
  SYMBOL_SC_(CorePkg, cArgumentsLimit);
  _sym_cArgumentsLimit->defconstant(make_fixnum(Lisp::MaxFunctionArguments));
  _sym_STARdebugMacroexpandSTAR->defparameter(nil<T_O>());
  _lisp->_Roots._ClassTable =
      HashTable_O::create_thread_safe(cl::_sym_eq, SimpleBaseString_O::make("CLTBLRD"), SimpleBaseString_O::make("CLTBLWR"));
  _sym_STARsharpEqContextSTAR->defparameter(nil<T_O>());
  cl::_sym_STARreadDefaultFloatFormatSTAR->defparameter(cl::_sym_single_float);
  _sym_STARnestedErrorDepthSTAR->defparameter(make_fixnum(0));
  cl::_sym_STARbreakOnSignalsSTAR->defparameter(nil<T_O>());
  cl::_sym_STARdebuggerHookSTAR->defparameter(nil<T_O>());
  cl::_sym_internalTimeUnitsPerSecond->defconstant(make_fixnum(CLASP_INTERNAL_TIME_UNITS_PER_SECOND));
  cl::_sym_MultipleValuesLimit->defconstant(make_fixnum(MultipleValues::MultipleValuesLimit));
  _sym_STARprintPackageSTAR->defparameter(nil<T_O>());
  _sym_STARcircle_counterSTAR->defparameter(nil<T_O>());
  _sym_STARcircle_stackSTAR->defparameter(nil<T_O>());
  // *quasiquote* is currently a dynamically bound plist of stream to nil/t values. It indicates whether or not the current stream
  // in inside a quasiquote during printing. This is used so that unquote forms print correctly inside of a quasiquote but print as
  // normal forms outside of a quasiquote.
  _sym_STARquasiquoteSTAR->defparameter(nil<T_O>());
  _sym_STARdebugReaderSTAR->defparameter(nil<T_O>());
  _sym__PLUS_known_typep_predicates_PLUS_->defparameter(nil<T_O>());
  cl::_sym_STARloadPathnameSTAR->defparameter(nil<T_O>());
  cl::_sym_STARloadTruenameSTAR->defparameter(nil<T_O>());
  cl::_sym_callArgumentsLimit->defconstant(make_fixnum(CALL_ARGUMENTS_LIMIT));
  cl::_sym_lambdaParametersLimit->defconstant(make_fixnum(CALL_ARGUMENTS_LIMIT));
  cl::_sym_arrayDimensionLimit->defconstant(make_fixnum(MOST_POSITIVE_FIXNUM));
  cl::_sym_arrayTotalSizeLimit->defconstant(make_fixnum(MOST_POSITIVE_FIXNUM));
  core::_sym_STARpollTicksPerGcSTAR->defparameter(make_fixnum(POLL_TICKS_PER_GC));
  comp::_sym_STARlowLevelTraceSTAR->defparameter(nil<T_O>());
  comp::_sym_STARlowLevelTracePrintSTAR->defparameter(nil<T_O>());
  comp::_sym_STARsave_module_for_disassembleSTAR->defparameter(nil<core::T_O>());
  comp::_sym_STARsaved_module_from_clasp_jitSTAR->defparameter(nil<core::T_O>());
  comp::_sym_STARdebug_jitSTAR->defparameter(nil<core::T_O>());
  _sym_STARallCxxClassesSTAR->defparameter(nil<T_O>());
  _sym_STARtopLevelCommandHookSTAR->defparameter(nil<T_O>());
  _sym_STARllvmFunctionNameHookSTAR->defparameter(nil<T_O>());
  _sym_STARihs_currentSTAR->defparameter(nil<T_O>());
  _sym_STARihs_topSTAR->defparameter(nil<T_O>());
  _sym_STARihs_baseSTAR->defparameter(nil<T_O>());
  _sym_STARihs_modeSTAR->defparameter(nil<T_O>());
  _sym_STARserializerArchiveSTAR->defparameter(nil<T_O>());
  _sym_STARcommandLineLoadSTAR->defparameter(nil<T_O>());
  _sym_STARdebugMonitorSTAR->defparameter(nil<T_O>());
  _sym_STARdebugLoadTimeValuesSTAR->defparameter(nil<T_O>());
  _sym_STARdebugEvalSTAR->defparameter(nil<T_O>());
  _sym_STARdebugStartupSTAR->defparameter(nil<T_O>());
  _sym_STARdebugInterpretedFunctionsSTAR->defparameter(nil<T_O>());
  _sym_STARcxxDocumentationSTAR->defparameter(nil<T_O>());
  _sym__PLUS_class_name_to_lisp_name_PLUS_->defparameter(nil<T_O>());
  _sym__PLUS_type_header_value_map_PLUS_->defparameter(nil<T_O>());
  initialize_typeq_map();
  _sym_STARllvmVersionSTAR->defparameter(SimpleBaseString_O::make(LLVM_VERSION_STRING));
#ifdef USE_PARALLEL_BUILD
  _sym_STARuseParallelBuildSTAR->defparameter(_lisp->_true());
#else
  _sym_STARuseParallelBuildSTAR->defparameter(nil<T_O>());
#endif
#ifdef USE_BUILD_FORK_REDIRECT_OUTPUT
  _sym_STARuseBuildForkRedirectSTAR->defparameter(_lisp->_true());
#else
  _sym_STARuseBuildForkRedirectSTAR->defparameter(nil<T_O>());
#endif
  _sym__PLUS_numberOfFixedArguments_PLUS_->defconstant(make_fixnum(LCC_ARGS_IN_REGISTERS));
  _sym__PLUS_WNOHANG_PLUS_->defconstant(make_fixnum(WNOHANG));
  cl::_sym_STARrandom_stateSTAR->defparameter(RandomState_O::create());
  // Set up a hash table to save JIT info
  HashTable_sp jit_save = HashTable_O::createEqual();
#ifdef CLASP_THREADS
  // When threading - make *jit-saved-symbol-info* a thread safe hash table
  SimpleBaseString_sp sbsr = SimpleBaseString_O::make("JITSAVR");
  SimpleBaseString_sp sbsw = SimpleBaseString_O::make("JITSAVW");
  jit_save->_Mutex = mp::SharedMutex_O::make_shared_mutex(sbsr, sbsw);
#endif
  comp::_sym_STARjit_saved_symbol_infoSTAR->defparameter(jit_save);
  //
  comp::_sym_STARthread_safe_contextSTAR->defparameter(llvmo::ThreadSafeContext_O::create_thread_safe_context());
  comp::_sym_STARload_time_value_holder_nameSTAR->defparameter(core::SimpleBaseString_O::make("[VALUES-TABLE]"));
  List_sp hooks = nil<T_O>();
  hooks = Cons_O::create(Cons_O::create(clasp_make_fixnum(FASL_MAGIC_NUMBER), _sym_load_bytecode), hooks);
  // This not ideal, but ANSI tests uses FASL as a generic pathname type so dispatching in LOAD via
  // *LOAD-HOOKS* will end up sending a FASO with an extension of FASL to the FASL loader. We
  // could sniff the magic number before we dispatch on pathname type, but this is inefficient since
  // it results in two file opens. If we had only one FASL format this wouldn't be an issue.
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("fasl"), _sym_load_bytecode), hooks);
  hooks = Cons_O::create(Cons_O::create(clasp_make_fixnum(FASO_MAGIC_NUMBER), _sym_load_faso), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("faso"), _sym_load_faso), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("fasoll"), _sym_load_fasoll), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("fasobc"), _sym_load_fasobc), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("l"), _sym_loadSource), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("L"), _sym_loadSource), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("lsp"), _sym_loadSource), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("asd"), _sym_loadSource), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("lisp"), _sym_loadSource), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("cl"), _sym_loadSource), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("clasprc"), _sym_loadSource), hooks);
  _sym_STARloadHooksSTAR->defparameter(hooks);
  ext::_sym_STARdefault_external_formatSTAR->defparameter(_lisp->_true());
  ext::_sym_STARinspectorHookSTAR->defparameter(nil<T_O>());
  core::_sym_STARsave_hookSTAR->defparameter(nil<T_O>());
  ext::_sym_STARclasp_clang_pathSTAR->defparameter(SimpleBaseString_O::make(CLASP_CLANG_PATH));
  ext::_sym_STARtoplevel_hookSTAR->defparameter(nil<T_O>());
  _sym_STARloadSearchListSTAR->defparameter(nil<T_O>());
  _sym_STARcurrent_dlopen_handleSTAR->defparameter(nil<T_O>());
  _sym_STARdebugInterpretedClosureSTAR->defparameter(nil<T_O>());
  _sym_STARdebugFlowControlSTAR->defparameter(nil<T_O>());
  _sym_STARbacktraceSTAR->defparameter(nil<T_O>());
  _sym_STARfunctions_to_inlineSTAR->defparameter(HashTable_O::createEqual());
  _sym_STARfunctions_to_notinlineSTAR->defparameter(HashTable_O::createEqual());
  _sym_STARextension_systemsSTAR->defparameter(EXTENSION_SYSTEMS);
  _sym_STARinitialize_hooksSTAR->defparameter(nil<T_O>());
  _sym_STARterminate_hooksSTAR->defparameter(nil<T_O>());
  SimpleBaseString_sp sbsr1 = SimpleBaseString_O::make("SYSPMNR");
  SimpleBaseString_sp sbsw1 = SimpleBaseString_O::make("SYSPMNW");
  _lisp->_Roots._Finalizers = HashTable_O::createEqWeakKey();
  _lisp->_Roots._Sysprop = HashTable_O::create_thread_safe(cl::_sym_eql, sbsr1, sbsw1);
  _sym_STARdebug_accessorsSTAR->defparameter(nil<T_O>());
  std::list<string> nicknames;
  std::list<string> use_packages;
  _sym_STARdebug_fsetSTAR->defparameter(nil<core::T_O>());

  ext::_sym_STARinvoke_debugger_hookSTAR->defparameter(nil<core::T_O>());
  _sym_STARinterrupts_enabledSTAR->defparameter(_lisp->_true());
  _sym_STARallow_with_interruptsSTAR->defparameter(_lisp->_true());
  _sym_STARexit_backtraceSTAR->defparameter(nil<core::T_O>());
  clos::_sym__PLUS_the_standard_class_PLUS_->defparameter(_lisp->_Roots._TheStandardClass);
  core::_sym__PLUS_gcroots_in_module_name_PLUS_->defparameter(SimpleBaseString_O::make(GCROOTS_IN_MODULE_NAME));
  core::_sym__PLUS_literals_name_PLUS_->defparameter(SimpleBaseString_O::make(LITERALS_NAME));
  _sym_STARdebug_threadsSTAR->defparameter(nil<core::T_O>());
  _sym_STARdebug_fastgfSTAR->defparameter(nil<core::T_O>());
#ifdef DEBUG_DRAG_NATIVE_CALLS
  _sym_STARdrag_native_callsSTAR->defparameter(_lisp->_true());
#else
  _sym_STARdrag_native_callsSTAR->defparameter(nil<core::T_O>());
#endif
  _sym_STARbuiltin_function_namesSTAR->defparameter(nil<core::T_O>());
  _sym_STARbuiltin_single_dispatch_method_namesSTAR->defparameter(nil<core::T_O>());
  _sym_STARbuiltin_macro_function_namesSTAR->defparameter(nil<core::T_O>());
  _sym_STARbuiltin_setf_function_namesSTAR->defparameter(nil<core::T_O>());
  _sym_STARdebug_valuesSTAR->defparameter(nil<core::T_O>());
  _sym_STARdebug_hash_tableSTAR->defparameter(nil<core::T_O>());
  _sym_STARforeign_data_reader_callbackSTAR->defparameter(nil<core::T_O>());
  _sym_STARnumber_of_entry_pointsSTAR->defparameter(make_fixnum(NUMBER_OF_ENTRY_POINTS));
  _sym_STARcore_startup_functionSTAR->defparameter(nil<core::T_O>());
  comp::_sym_STARcompile_file_parallelSTAR->defparameter(nil<core::T_O>());
  /*
#ifdef DEFAULT_OUTPUT_TYPE_FASO
  comp::_sym_STARdefault_output_typeSTAR->defparameter(kw::_sym_faso);
#endif
*/
#ifdef DEFAULT_OUTPUT_TYPE_FASOLL
  comp::_sym_STARdefault_output_typeSTAR->defparameter(kw::_sym_fasoll);
#endif
#ifdef DEFAULT_OUTPUT_TYPE_FASOBC
  comp::_sym_STARdefault_output_typeSTAR->defparameter(kw::_sym_fasobc);
#endif
#if defined(DEFAULT_OUTPUT_TYPE_BYTECODE) || defined(DEFAULT_OUTPUT_TYPE_FASO)
  comp::_sym_STARdefault_output_typeSTAR->defparameter(kw::_sym_bytecode);
#endif
  comp::_sym_STARforce_startup_external_linkageSTAR->defparameter(nil<core::T_O>());
  gctools::_sym_STARdebug_gcrootsSTAR->defparameter(nil<core::T_O>());
#ifdef DEBUG_LLVM_OPTIMIZATION_LEVEL_0
  int optimization_level = 0;
#else
  int optimization_level = 3;
#endif
  const char* optLevel = getenv("CLASP_OPTIMIZATION_LEVEL");
  if (optLevel) {
    optimization_level = strtol(optLevel, NULL, 10);
    if (optimization_level < 0)
      optimization_level = 0;
    if (optimization_level > 3)
      optimization_level = 3;
    printf("%s:%d Due to CLASP_OPTIMIZATION_LEVEL environment variable --> cmp:*optimization-level* = %d\n", __FILE__, __LINE__,
           optimization_level);
  }
  comp::_sym_STARoptimization_levelSTAR->defparameter(core::make_fixnum(optimization_level));
  _sym_STARreader_generate_cstSTAR->defparameter(nil<core::T_O>());
  _sym_STARreader_cst_resultSTAR->defparameter(nil<core::T_O>());
  _sym_STARcache_macroexpandSTAR->defparameter(nil<core::T_O>());
  _sym_STARdebugByteCodeSTAR->defparameter(nil<core::T_O>());
  _sym_STARdebugSourcePosInfoSTAR->defparameter(nil<core::T_O>());
  _sym_STARdebugVaslistSTAR->defparameter(nil<core::T_O>());
  _sym_STARdebug_dtree_interpreterSTAR->defparameter(nil<core::T_O>());
  _sym_STARdebug_symbol_lookupSTAR->defparameter(nil<core::T_O>());

  List_sp features = nil<core::T_O>();
  features = Cons_O::create(_lisp->internKeyword("CLASP"), features);
  features = Cons_O::create(_lisp->internKeyword("COMMON-LISP"), features);
  features = Cons_O::create(_lisp->internKeyword("ANSI-CL"), features);
  features = Cons_O::create(_lisp->internKeyword("IEEE-FLOATING-POINT"), features);
  features = Cons_O::create(_lisp->internKeyword("64-BIT"), features);
#ifdef _TARGET_OS_DARWIN
  features = Cons_O::create(_lisp->internKeyword("UNIX"), features);
  features = Cons_O::create(_lisp->internKeyword("OS-UNIX"), features);
  features = Cons_O::create(_lisp->internKeyword("BSD"), features);
  features = Cons_O::create(_lisp->internKeyword("DARWIN"), features);
#endif
#ifdef _TARGET_OS_LINUX
  features = Cons_O::create(_lisp->internKeyword("UNIX"), features);
  features = Cons_O::create(_lisp->internKeyword("OS-UNIX"), features);
  features = Cons_O::create(_lisp->internKeyword("LINUX"), features);
#endif
#ifdef _TARGET_OS_FREEBSD
  features = Cons_O::create(_lisp->internKeyword("UNIX"), features);
  features = Cons_O::create(_lisp->internKeyword("OS-UNIX"), features);
  features = Cons_O::create(_lisp->internKeyword("BSD"), features);
  features = Cons_O::create(_lisp->internKeyword("FREEBSD"), features);
#endif
#ifdef __x86_64__
  features = Cons_O::create(_lisp->internKeyword("X86-64"), features);
#endif
#ifdef __aarch64__
  features = Cons_O::create(_lisp->internKeyword("ARM64"), features);
#endif
#ifdef CLASP_UNICODE
  features = Cons_O::create(_lisp->internKeyword("UNICODE"), features);
#endif
  features = Cons_O::create(_lisp->internKeyword("LLVM" CXX_MACRO_STRING(__clang_major__)), features);
#ifdef VARARGS
  features = Cons_O::create(_lisp->internKeyword("VARARGS"), features);
#endif
#ifdef POLYMORPHIC_SMART_PTR
  features = Cons_O::create(_lisp->internKeyword("POLYMORPHIC-SMART-PTR"), features);
#endif
#ifdef _DEBUG_BUILD
  features = Cons_O::create(_lisp->internKeyword("DEBUG-BUILD"), features);
#else // _RELEASE_BUILD
  features = Cons_O::create(_lisp->internKeyword("RELEASE-BUILD"), features);
#endif
#ifdef USE_MPI
  features = Cons_O::create(_lisp->internKeyword("USE-MPI"), features);
#endif
#if defined(USE_BOEHM)
  features = Cons_O::create(_lisp->internKeyword("USE-BOEHM"), features);
#elif defined(USE_MPS)
  features = Cons_O::create(_lisp->internKeyword("USE-MPS"), features);
#elif defined(USE_MMTK)
  features = Cons_O::create(_lisp->internKeyword("USE-MMTK"), features);
#endif
#ifdef USE_PRECISE_GC
  // Informs CL that precise GC is being used
  features = Cons_O::create(_lisp->internKeyword("USE-PRECISE-GC"), features);
#endif
#ifdef CLASP_THREADS
  features = Cons_O::create(_lisp->internKeyword("THREADS"), features);
#endif
#if TAG_BITS == 4
  features = Cons_O::create(_lisp->internKeyword("TAG-BITS4"), features);
#endif
#ifdef CLASP_EXTENSIONS
  features = Cons_O::create(_lisp->internKeyword("EXTENSIONS"), features);
#endif
#ifdef DEFAULT_OUTPUT_TYPE_BYTECODE
  features = Cons_O::create(_lisp->internKeyword("BYTECODE"), features);
#endif
#ifdef CLASP_SHORT_FLOAT
  features = Cons_O::create(_lisp->internKeyword("SHORT-FLOAT"), features);
#endif
#ifdef CLASP_SHORT_FLOAT_BINARY16
  features = Cons_O::create(_lisp->internKeyword("SHORT-FLOAT/BINARY16"), features);
#endif
#ifdef CLASP_LONG_FLOAT
  features = Cons_O::create(_lisp->internKeyword("LONG-FLOAT"), features);
#endif
#ifdef CLASP_LONG_FLOAT_BINARY80
  features = Cons_O::create(_lisp->internKeyword("LONG-FLOAT/BINARY80"), features);
#endif
#ifdef CLASP_LONG_FLOAT_BINARY128
  features = Cons_O::create(_lisp->internKeyword("LONG-FLOAT/BINARY128"), features);
#endif
  cl::_sym_STARfeaturesSTAR->exportYourself()->defparameter(features);
}

void add_defsetf_access_update(Symbol_sp access_fn, Symbol_sp update_fn) {
  Cons_sp pair = Cons_O::create(access_fn, update_fn);
  List_sp list = _sym_STARsystem_defsetf_update_functionsSTAR->symbolValue();
  _sym_STARsystem_defsetf_update_functionsSTAR->defparameter(Cons_O::create(pair, list));
}
}; // namespace core

#define EXPAND_CLASS_MACROS
#define _CLASS_MACRO(_T_) STATIC_CLASS_INFO(_T_);
// #include <clasp/core/initClasses.h>
#undef _CLASS_MACRO
#undef EXPAND_CLASS_MACROS
