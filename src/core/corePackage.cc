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
//#define DEBUG_LEVEL_FULL
#include <limits.h>
#include <float.h>
#include <stdio.h>
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
#include <clasp/core/cleavirEnvPackage.fwd.h>
#include <clasp/core/cleavirIrPackage.fwd.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/posixTime.h>
#include <clasp/core/random.h>
#include <clasp/core/ql.h>
#include <clasp/core/readtable.h>
#include <clasp/core/commonLispUserPackage.h>
#include <clasp/core/lispReader.h>
#include <clasp/core/numerics.h>
#include <clasp/core/bootStrapCoreSymbolMap.h>

// ------------------- include all headers for corePackage here

#include <clasp/core/object.h>
#include <clasp/asttooling/astVisitor.h>
#include <clasp/clbind/class_registry.h>
#include <clasp/core/serialize.h>
#include <clasp/core/character.h>
#include <clasp/core/cons.h>
#include <clasp/core/cxxObject.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/environment.h>
#include <clasp/core/externalObject.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/intArray.h>
#include <clasp/core/primitives.h>
#include <clasp/core/iterator.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/userData.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/multiStringBuffer.h>
#include <clasp/core/numbers.h>
#include <clasp/core/package.h>
#include <clasp/core/pathname.h>
#include <clasp/core/pointer.h>
#include <clasp/core/posixTime.h>
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
#include <clasp/core/weakHashTable.h>
#include <clasp/core/weakKeyMapping.h>
#include <clasp/core/weakPointer.h>
#include <clasp/core/wrappedPointer.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/debugInfoExpose.h>
#include <clasp/llvmo/debugLoc.h>
#include <clasp/llvmo/insertPoint.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/hashTableEqualp.h>
#include <clasp/core/instance.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/null.h>
#include <clasp/core/singleDispatchGenericFunction.h>
#include <clasp/core/specialForm.h>
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
const char *CorePkg_nicknames[] = {
    "SYSTEM", "sys", "SYS", "si", "SI", "" /*guard*/
};
SYMBOL_EXPORT_SC_(CorePkg, STARuse_cleavir_compilerSTAR);  // nil (clasp) or T (cleavir)
SYMBOL_EXPORT_SC_(CorePkg, STARstack_top_hintSTAR);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_contab_name_PLUS_);
SYMBOL_EXPORT_SC_(KeywordPkg,object);
SYMBOL_EXPORT_SC_(KeywordPkg,fasl);
SYMBOL_EXPORT_SC_(KeywordPkg,bitcode);
SYMBOL_EXPORT_SC_(KeywordPkg,linkage);
SYMBOL_EXPORT_SC_(KeywordPkg, verbose);
SYMBOL_EXPORT_SC_(KeywordPkg, pause_pid);
SYMBOL_EXPORT_SC_(KeywordPkg, exit_backtrace);
SYMBOL_EXPORT_SC_(CorePkg, STARcurrent_dlopen_handleSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARuseParallelBuildSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARuseBuildForkRedirectSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARreader_generate_cstSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARreader_cst_resultSTAR);
SYMBOL_EXPORT_SC_(CorePkg, arguments );
SYMBOL_EXPORT_SC_(CorePkg, STARdebug_valuesSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebug_accessorsSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebug_dispatchSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARexit_backtraceSTAR);
SYMBOL_EXPORT_SC_(CorePkg, make_source_pos_info);
SYMBOL_EXPORT_SC_(ExtPkg, STARclasp_clang_pathSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARinterrupts_enabledSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebug_threadsSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebug_dtree_interpreterSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARallow_with_interruptsSTAR);
SYMBOL_EXPORT_SC_(CorePkg, signal_servicing);
SYMBOL_EXPORT_SC_(CorePkg, handle_signal);
SYMBOL_EXPORT_SC_(CorePkg, every_list);
SYMBOL_EXPORT_SC_(CorePkg, some_list);
SYMBOL_EXPORT_SC_(CorePkg, clbind_cxx_class);
SYMBOL_EXPORT_SC_(CorePkg, derivable_cxx_class);
SYMBOL_EXPORT_SC_(CorePkg, derivable_cxx_object);
SYMBOL_EXPORT_SC_(CorePkg, stack_closure);
SYMBOL_EXPORT_SC_(CorePkg, make_source_pos_info);
SYMBOL_EXPORT_SC_(CorePkg, STARloadHooksSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARcache_macroexpandSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARmodule_startup_function_nameSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARmodule_shutdown_function_nameSTAR);
SYMBOL_EXPORT_SC_(ExtPkg, STARinvoke_debugger_hookSTAR);
SYMBOL_EXPORT_SC_(CorePkg,variable_source_location)
SYMBOL_EXPORT_SC_(CorePkg,class_source_location)
SYMBOL_EXPORT_SC_(CorePkg,STARdebug_hash_tableSTAR)
SYMBOL_EXPORT_SC_(CorePkg,bt)
SYMBOL_EXPORT_SC_(CorePkg,btcl)
SYMBOL_EXPORT_SC_(CorePkg,STARdebug_fastgfSTAR);
SYMBOL_EXPORT_SC_(CorePkg,cxx_method_source_location);
SYMBOL_EXPORT_SC_(CompPkg, STARllvm_contextSTAR);
SYMBOL_EXPORT_SC_(CompPkg, STARdebug_jitSTAR );
SYMBOL_EXPORT_SC_(CompPkg, STARload_time_value_holder_nameSTAR);
SYMBOL_EXPORT_SC_(CompPkg, STARoptimization_levelSTAR);
SYMBOL_EXPORT_SC_(CorePkg,c_local);
SYMBOL_EXPORT_SC_(CorePkg,_PLUS_known_typep_predicates_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg,_PLUS_class_name_to_lisp_name_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg,_PLUS_type_header_value_map_PLUS_);
SYMBOL_EXPORT_SC_(ExtPkg,check_arguments_type);
SYMBOL_EXPORT_SC_(ExtPkg,array_index);
SYMBOL_EXPORT_SC_(CorePkg,index);
SYMBOL_EXPORT_SC_(CorePkg,cxx_class);
SYMBOL_EXPORT_SC_(KeywordPkg,read_only);
SYMBOL_EXPORT_SC_(KeywordPkg,interactive);
SYMBOL_EXPORT_SC_(CorePkg,function_boundary);
SYMBOL_EXPORT_SC_(CorePkg,type_assertions);
SYMBOL_EXPORT_SC_(ExtPkg,assume_no_errors);
SYMBOL_EXPORT_SC_(ExtPkg,ignore_signal);
SYMBOL_EXPORT_SC_(ExtPkg,unix_signal_received);
SYMBOL_EXPORT_SC_(KeywordPkg,process);
SYMBOL_EXPORT_SC_(KeywordPkg,code);
SYMBOL_EXPORT_SC_(KeywordPkg,handler);
SYMBOL_EXPORT_SC_(CorePkg,STARinformation_callbackSTAR);


SYMBOL_EXPORT_SC_(CorePkg, STARmpi_rankSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARmpi_sizeSTAR);

SYMBOL_EXPORT_SC_(ClPkg, upgraded_array_element_type);
SYMBOL_EXPORT_SC_(ClPkg, member);
SYMBOL_EXPORT_SC_(ClPkg, class_name);
SYMBOL_EXPORT_SC_(ClPkg, every);
SYMBOL_EXPORT_SC_(ClPkg, some);
SYMBOL_EXPORT_SC_(ClPkg, float);
SYMBOL_EXPORT_SC_(ClPkg, double_float);
SYMBOL_EXPORT_SC_(ClPkg, single_float);
SYMBOL_EXPORT_SC_(ClPkg, parse_namestring);
SYMBOL_EXPORT_SC_(ClPkg, make_instance);
SYMBOL_EXPORT_SC_(ClPkg, class);
SYMBOL_EXPORT_SC_(ClPkg, printNotReadableObject);
SYMBOL_EXPORT_SC_(ClPkg, read_char);
SYMBOL_EXPORT_SC_(ClPkg, simple_base_string);
SYMBOL_EXPORT_SC_(ClPkg, base_string);
SYMBOL_EXPORT_SC_(ClPkg, provide);
SYMBOL_EXPORT_SC_(ClPkg, condition);
SYMBOL_EXPORT_SC_(ClPkg, seriousCondition);
SYMBOL_EXPORT_SC_(ClPkg, error);
SYMBOL_EXPORT_SC_(ClPkg, warn);
SYMBOL_EXPORT_SC_(ClPkg, programError);
SYMBOL_EXPORT_SC_(ClPkg, cellError);
SYMBOL_EXPORT_SC_(ClPkg, unboundVariable);
SYMBOL_EXPORT_SC_(ClPkg, unboundSlot);
SYMBOL_EXPORT_SC_(ClPkg, undefinedFunction);
SYMBOL_EXPORT_SC_(ClPkg, package_error);
SYMBOL_EXPORT_SC_(ClPkg, arithmeticError);
SYMBOL_EXPORT_SC_(ClPkg, floatingPointOverflow);
SYMBOL_EXPORT_SC_(ClPkg, floatingPointInexact);
SYMBOL_EXPORT_SC_(ClPkg, floatingPointInvalidOperation);
SYMBOL_EXPORT_SC_(ClPkg, floatingPointUnderflow);
SYMBOL_EXPORT_SC_(ClPkg, divisionByZero);
SYMBOL_EXPORT_SC_(ClPkg, printNotReadable);
SYMBOL_EXPORT_SC_(ClPkg, fileError);
SYMBOL_EXPORT_SC_(ClPkg, streamError);
SYMBOL_EXPORT_SC_(ClPkg, endOfFile);
SYMBOL_EXPORT_SC_(ClPkg, parseError);
SYMBOL_EXPORT_SC_(ClPkg, readerError);
SYMBOL_EXPORT_SC_(ClPkg, STARrandom_stateSTAR);
SYMBOL_EXPORT_SC_(ClPkg, controlError);
SYMBOL_EXPORT_SC_(ClPkg, type_error);
SYMBOL_EXPORT_SC_(ClPkg, simple_type_error);
SYMBOL_EXPORT_SC_(ClPkg, simpleError);
SYMBOL_EXPORT_SC_(ClPkg, storageCondition);
SYMBOL_EXPORT_SC_(ClPkg, simpleCondition);
SYMBOL_EXPORT_SC_(ClPkg, simpleWarning);
SYMBOL_EXPORT_SC_(ClPkg, warning);
SYMBOL_EXPORT_SC_(ClPkg, styleWarning);
SYMBOL_EXPORT_SC_(ClPkg, asin);
SYMBOL_EXPORT_SC_(ClPkg, acos);
SYMBOL_EXPORT_SC_(ClPkg, asinh);
SYMBOL_EXPORT_SC_(ClPkg, acosh);
SYMBOL_EXPORT_SC_(ClPkg, atanh);
SYMBOL_EXPORT_SC_(ClPkg, dynamic_extent);
SYMBOL_EXPORT_SC_(ClPkg, ftype);
SYMBOL_EXPORT_SC_(ClPkg, boole);
SYMBOL_EXPORT_SC_(ClPkg, ignorable);
SYMBOL_EXPORT_SC_(ClPkg, notinline );
SYMBOL_EXPORT_SC_(ClPkg, callArgumentsLimit);
SYMBOL_EXPORT_SC_(ClPkg, arrayDimensionLimit);
SYMBOL_EXPORT_SC_(ClPkg, arrayTotalSizeLimit);
SYMBOL_EXPORT_SC_(ClPkg, lambdaParametersLimit);
SYMBOL_EXPORT_SC_(ClPkg, schar);
SYMBOL_EXPORT_SC_(ClPkg, fixnum);
SYMBOL_EXPORT_SC_(ClPkg, bit);
SYMBOL_EXPORT_SC_(ClPkg, documentation);
SYMBOL_EXPORT_SC_(ClPkg, substitute);
SYMBOL_EXPORT_SC_(ClPkg, subtypep);
SYMBOL_EXPORT_SC_(ClPkg, subseq);

SYMBOL_EXPORT_SC_(CorePkg, make_atom_cst);
SYMBOL_EXPORT_SC_(CorePkg, make_cons_cst);
SYMBOL_EXPORT_SC_(CorePkg, STARbits_in_bit_array_wordSTAR);
SYMBOL_EXPORT_SC_(CorePkg, setf_subseq);
SYMBOL_EXPORT_SC_(CorePkg,STARextension_startup_loadsSTAR);
SYMBOL_EXPORT_SC_(CorePkg, multiple_value_foreign_call);
SYMBOL_EXPORT_SC_(CorePkg, foreign_call);
SYMBOL_EXPORT_SC_(CorePkg, foreign_call_pointer);
SYMBOL_EXPORT_SC_(CorePkg, STARclasp_packageSTAR );
SYMBOL_EXPORT_SC_(CorePkg, single_dispatch_method);
SYMBOL_EXPORT_SC_(CorePkg, setf_documentation);
SYMBOL_EXPORT_SC_(CorePkg, debug_message);
SYMBOL_EXPORT_SC_(CorePkg, STARcxxDocumentationSTAR);
SYMBOL_EXPORT_SC_(CorePkg, topLevel);
SYMBOL_EXPORT_SC_(CorePkg, scharSet);
SYMBOL_EXPORT_SC_(CorePkg, STARuseInterpreterForEvalSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARllvmVersionSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugInterpretedClosureSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugFlowControlSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugStartupSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugByteCodeSTAR);
SYMBOL_EXPORT_SC_(CorePkg, _BANG_unbound_BANG_);
SYMBOL_EXPORT_SC_(CorePkg, bitArrayOp);
SYMBOL_EXPORT_SC_(CorePkg, lambdaName);
SYMBOL_EXPORT_SC_(CorePkg, dump_module);
SYMBOL_EXPORT_SC_(CorePkg, STARfunctions_to_inlineSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARfunctions_to_notinlineSTAR);
SYMBOL_SC_(CorePkg, printf);

//SYMBOL_EXPORT_SC_(CorePkg, asin);
//SYMBOL_EXPORT_SC_(CorePkg, asinh);
//SYMBOL_EXPORT_SC_(CorePkg, acos);
//SYMBOL_EXPORT_SC_(CorePkg, acosh);
//SYMBOL_EXPORT_SC_(CorePkg, atanh);
SYMBOL_EXPORT_SC_(CompPkg, bclasp_compile);

SYMBOL_EXPORT_SC_(ClPkg, nil);
SYMBOL_EXPORT_SC_(CorePkg, STARpollTicksPerGcSTAR);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_standardReadtable_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, syntax_type);
SYMBOL_EXPORT_SC_(CorePkg, sicl_readtable_case);
SYMBOL_EXPORT_SC_(CorePkg, sicl_syntax_type);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_WNOHANG_PLUS_);
SYMBOL_EXPORT_SC_(KeywordPkg, create);
SYMBOL_EXPORT_SC_(KeywordPkg, append);
SYMBOL_EXPORT_SC_(KeywordPkg, debugStartup);
SYMBOL_EXPORT_SC_(KeywordPkg, debugStartupVerbose);
SYMBOL_EXPORT_SC_(KeywordPkg, cclasp);
SYMBOL_EXPORT_SC_(KeywordPkg, bclasp);
SYMBOL_EXPORT_SC_(KeywordPkg, load);
SYMBOL_EXPORT_SC_(KeywordPkg, eval);
SYMBOL_EXPORT_SC_(KeywordPkg, clasp_min);
SYMBOL_EXPORT_SC_(KeywordPkg, use_mps);
SYMBOL_EXPORT_SC_(KeywordPkg, use_boehmdc);
SYMBOL_EXPORT_SC_(KeywordPkg, use_boehm);
SYMBOL_EXPORT_SC_(KeywordPkg, lf);
SYMBOL_EXPORT_SC_(KeywordPkg, cr);
SYMBOL_EXPORT_SC_(KeywordPkg, lf);
SYMBOL_EXPORT_SC_(KeywordPkg, littleEndian);
SYMBOL_EXPORT_SC_(KeywordPkg, bigEndian);
SYMBOL_EXPORT_SC_(KeywordPkg, crlf);
SYMBOL_EXPORT_SC_(KeywordPkg, latin_1);
SYMBOL_EXPORT_SC_(KeywordPkg, passThrough);
SYMBOL_EXPORT_SC_(KeywordPkg, ucs_4);
SYMBOL_EXPORT_SC_(KeywordPkg, passThrough);
SYMBOL_EXPORT_SC_(KeywordPkg, if_does_not_exist);
SYMBOL_EXPORT_SC_(KeywordPkg, new_version);
SYMBOL_EXPORT_SC_(KeywordPkg, rename);
SYMBOL_EXPORT_SC_(KeywordPkg, rename_and_delete);
SYMBOL_EXPORT_SC_(KeywordPkg, overwrite);
SYMBOL_EXPORT_SC_(KeywordPkg, if_exists);
SYMBOL_EXPORT_SC_(KeywordPkg, io);
SYMBOL_EXPORT_SC_(KeywordPkg, probe);

SYMBOL_EXPORT_SC_(ClPkg, stream_element_type);
SYMBOL_EXPORT_SC_(ClPkg, stream_external_format);
SYMBOL_EXPORT_SC_(ClPkg, open_stream_p);
SYMBOL_EXPORT_SC_(ClPkg, write_sequence);
SYMBOL_EXPORT_SC_(ClPkg, read_sequence);
SYMBOL_EXPORT_SC_(ClPkg, file_string_length);
SYMBOL_EXPORT_SC_(ClPkg, echo_stream_input_stream);
SYMBOL_EXPORT_SC_(ClPkg, echo_stream_output_stream);
SYMBOL_EXPORT_SC_(ClPkg, concatenated_stream_streams);
SYMBOL_EXPORT_SC_(ClPkg, synonym_stream_symbol);

SYMBOL_EXPORT_SC_(ClPkg, broadcast_stream_streams);
SYMBOL_EXPORT_SC_(ClPkg, getOutputStreamString);
SYMBOL_EXPORT_SC_(CorePkg, closedStream);
SYMBOL_EXPORT_SC_(CorePkg, creator);
SYMBOL_EXPORT_SC_(ClPkg, elt);
SYMBOL_EXPORT_SC_(ClPkg, null);
SYMBOL_EXPORT_SC_(ClPkg, sequence);
SYMBOL_EXPORT_SC_(ClPkg, list);
SYMBOL_EXPORT_SC_(ClPkg, callNextMethod);
SYMBOL_EXPORT_SC_(ClPkg, nextMethodP);
SYMBOL_EXPORT_SC_(ExtPkg, STARinspectorHookSTAR);
SYMBOL_EXPORT_SC_(ExtPkg, float_nan_string);
SYMBOL_EXPORT_SC_(ExtPkg, float_infinity_string);
SYMBOL_EXPORT_SC_(ExtPkg, STARdefault_external_formatSTAR);
SYMBOL_EXPORT_SC_(ExtPkg, specialVar);
SYMBOL_EXPORT_SC_(ExtPkg, registerVar);
SYMBOL_EXPORT_SC_(ExtPkg, lexicalVar);
SYMBOL_EXPORT_SC_(ExtPkg, stackVar);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_numberOfFixedArguments_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, STARinterpreterTraceSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugLoadTimeValuesSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugEvalSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugInterpretedFunctionsSTAR);
SYMBOL_EXPORT_SC_(KeywordPkg, FullDebug);
SYMBOL_EXPORT_SC_(KeywordPkg, LineTablesOnly);

SYMBOL_EXPORT_SC_(CorePkg, debug_message);
SYMBOL_SC_(CorePkg, STARdebugMonitorSTAR);
SYMBOL_SC_(CorePkg, monitorReader);
SYMBOL_EXPORT_SC_(CorePkg, tsp);
SYMBOL_EXPORT_SC_(CorePkg, tmv);
SYMBOL_EXPORT_SC_(CorePkg, invocationHistoryFrame);
SYMBOL_EXPORT_SC_(CorePkg, size_t);
SYMBOL_EXPORT_SC_(CorePkg, threadInfo);

SYMBOL_EXPORT_SC_(CorePkg, circle_subst);
SYMBOL_EXPORT_SC_(CorePkg, STARcurrentSourcePosInfoSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARstartRunTimeSTAR);
SYMBOL_EXPORT_SC_(ClPkg, internalTimeUnitsPerSecond);
SYMBOL_EXPORT_SC_(ClPkg, getInternalRealTime);
SYMBOL_EXPORT_SC_(ClPkg, getInternalRunTime);
SYMBOL_EXPORT_SC_(CorePkg, STARcommandLineLoadSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARserializerArchiveSTAR);
SYMBOL_EXPORT_SC_(KeywordPkg, _uid);
SYMBOL_EXPORT_SC_(KeywordPkg, function);
SYMBOL_EXPORT_SC_(CorePkg, STARihsCurrentSTAR);
SYMBOL_EXPORT_SC_(ClPkg, logicalPathnameTranslations);
SYMBOL_EXPORT_SC_(ClPkg, set);
SYMBOL_EXPORT_SC_(ClPkg, restartName);
SYMBOL_EXPORT_SC_(ClPkg, position);
SYMBOL_EXPORT_SC_(ClPkg, compileFile);
SYMBOL_EXPORT_SC_(ClPkg, compiler_macro);
SYMBOL_EXPORT_SC_(ClPkg, inline);
SYMBOL_EXPORT_SC_(ClPkg, compilation_speed);
SYMBOL_EXPORT_SC_(ClPkg, first);
SYMBOL_EXPORT_SC_(ClPkg, float);
SYMBOL_EXPORT_SC_(ClPkg, logical_pathname);
SYMBOL_EXPORT_SC_(ClPkg, pathname);
SYMBOL_EXPORT_SC_(CorePkg, STARllvmFunctionNameHookSTAR);
SYMBOL_EXPORT_SC_(ClPkg, pathnamep);
SYMBOL_EXPORT_SC_(CorePkg, STARtopLevelCommandHookSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARloadSearchListSTAR);
SYMBOL_EXPORT_SC_(LlvmoPkg, load_bitcode);
SYMBOL_EXPORT_SC_(LlvmoPkg, load_bitcode_ll);
SYMBOL_EXPORT_SC_(CorePkg, loadSource);
SYMBOL_EXPORT_SC_(CorePkg, load_binary);
SYMBOL_EXPORT_SC_(ClPkg, STARloadPathnameSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARloadTruenameSTAR);
SYMBOL_EXPORT_SC_(KeywordPkg, none);
SYMBOL_EXPORT_SC_(KeywordPkg, line);
SYMBOL_EXPORT_SC_(KeywordPkg, full);
SYMBOL_EXPORT_SC_(KeywordPkg, message);
SYMBOL_EXPORT_SC_(KeywordPkg, line_buffered);
SYMBOL_EXPORT_SC_(KeywordPkg, fully_buffered);

SYMBOL_EXPORT_SC_(ClPkg, array);
SYMBOL_EXPORT_SC_(KeywordPkg, array);
SYMBOL_EXPORT_SC_(ClPkg, makeArray);
SYMBOL_EXPORT_SC_(ClPkg, makePathname);
SYMBOL_EXPORT_SC_(CorePkg, STARallCxxClassesSTAR);
SYMBOL_EXPORT_SC_(ClPkg, mismatch);
SYMBOL_EXPORT_SC_(ClPkg, SignedByte);
SYMBOL_EXPORT_SC_(ClPkg, UnsignedByte);
SYMBOL_EXPORT_SC_(KeywordPkg, UnsignedByte);
SYMBOL_EXPORT_SC_(ClPkg, Bit);
SYMBOL_EXPORT_SC_(ClPkg, parseNamestring);
SYMBOL_EXPORT_SC_(KeywordPkg, start);
SYMBOL_EXPORT_SC_(KeywordPkg, end);
SYMBOL_EXPORT_SC_(ClPkg, or );
SYMBOL_EXPORT_SC_(KeywordPkg, test);
SYMBOL_EXPORT_SC_(KeywordPkg, junkAllowed);
SYMBOL_EXPORT_SC_(ClPkg, STARdefaultPathnameDefaultsSTAR);
SYMBOL_EXPORT_SC_(KeywordPkg, absolute);
SYMBOL_EXPORT_SC_(KeywordPkg, relative);
SYMBOL_EXPORT_SC_(KeywordPkg, back);
SYMBOL_EXPORT_SC_(CorePkg, simpleProgramError);
SYMBOL_EXPORT_SC_(CorePkg, simpleControlError);
SYMBOL_EXPORT_SC_(CorePkg, simplePackageError);
SYMBOL_EXPORT_SC_(CorePkg, simpleParseError);
SYMBOL_EXPORT_SC_(CorePkg, simpleReaderError);
SYMBOL_EXPORT_SC_(CorePkg, simpleFileError);
SYMBOL_EXPORT_SC_(CorePkg, simpleStreamError);
SYMBOL_EXPORT_SC_(ClPkg, simpleTypeError);
SYMBOL_EXPORT_SC_(ClPkg, MultipleValuesLimit);
SYMBOL_EXPORT_SC_(CorePkg, STARdebugReaderSTAR);
SYMBOL_EXPORT_SC_(ClPkg, keywordp);
SYMBOL_EXPORT_SC_(ClPkg, null);
SYMBOL_EXPORT_SC_(ClPkg, endp);
SYMBOL_EXPORT_SC_(ClPkg, symbolp);
SYMBOL_EXPORT_SC_(ClPkg, atom);
SYMBOL_EXPORT_SC_(ClPkg, consp);
SYMBOL_EXPORT_SC_(ClPkg, listp);
SYMBOL_EXPORT_SC_(ClPkg, numberp);
SYMBOL_EXPORT_SC_(ClPkg, integerp);
SYMBOL_EXPORT_SC_(ClPkg, rationalp);
SYMBOL_EXPORT_SC_(ClPkg, floatp);
SYMBOL_EXPORT_SC_(ClPkg, realp);
SYMBOL_EXPORT_SC_(ClPkg, complexp);
SYMBOL_EXPORT_SC_(ClPkg, character);
SYMBOL_EXPORT_SC_(ClPkg, base_char);
SYMBOL_EXPORT_SC_(ClPkg, single_float);
SYMBOL_EXPORT_SC_(ClPkg, characterp);
SYMBOL_EXPORT_SC_(ClPkg, stringp);
SYMBOL_EXPORT_SC_(ClPkg, bit_vector_p);
SYMBOL_EXPORT_SC_(ClPkg, vectorp);
SYMBOL_EXPORT_SC_(ClPkg, simple_vector_p);
SYMBOL_EXPORT_SC_(ClPkg, simple_string_p);
SYMBOL_EXPORT_SC_(ClPkg, simple_bit_vector_p);
SYMBOL_EXPORT_SC_(ClPkg, arrayp);
SYMBOL_EXPORT_SC_(ClPkg, packagep);
SYMBOL_EXPORT_SC_(ClPkg, functionp);
SYMBOL_EXPORT_SC_(ClPkg, compiled_function_p);
SYMBOL_EXPORT_SC_(ClPkg, hash_table_p);

SYMBOL_EXPORT_SC_(CorePkg, STARenablePrintPrettySTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARcircle_counterSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARcircle_stackSTAR);
SYMBOL_EXPORT_SC_(CorePkg, dynamicGo);
SYMBOL_EXPORT_SC_(CorePkg, localGo);
SYMBOL_EXPORT_SC_(ClPkg, _DIVIDE_);
SYMBOL_EXPORT_SC_(KeywordPkg, operation);
SYMBOL_EXPORT_SC_(KeywordPkg, operands);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_ecl_syntax_progv_list_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_io_syntax_progv_list_PLUS_);
SYMBOL_SC_(CorePkg, STARprintPackageSTAR);
SYMBOL_SC_(CorePkg, STARsharpEqContextSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARcircleCounterSTAR);

SYMBOL_EXPORT_SC_(ClPkg, typep);
SYMBOL_EXPORT_SC_(ClPkg, type);
SYMBOL_EXPORT_SC_(ClPkg, step);
SYMBOL_EXPORT_SC_(ClPkg, speed);
SYMBOL_EXPORT_SC_(ClPkg, space);
SYMBOL_EXPORT_SC_(ClPkg, sin);
SYMBOL_EXPORT_SC_(ClPkg, safety);
SYMBOL_EXPORT_SC_(ClPkg, restart_bind);
SYMBOL_EXPORT_SC_(ClPkg, restart);
SYMBOL_EXPORT_SC_(ClPkg, describe);

SYMBOL_EXPORT_SC_(ClPkg, disassemble);
SYMBOL_EXPORT_SC_(ClPkg, rename_file);
SYMBOL_EXPORT_SC_(ClPkg, random);
SYMBOL_EXPORT_SC_(ClPkg, optimize);
SYMBOL_EXPORT_SC_(ClPkg, two_way_stream_input_stream);
SYMBOL_EXPORT_SC_(ClPkg, two_way_stream_output_stream);
SYMBOL_EXPORT_SC_(ClPkg, two_way_stream);
SYMBOL_EXPORT_SC_(ClPkg, make_two_way_stream);
SYMBOL_EXPORT_SC_(ClPkg, make_synonym_stream);
SYMBOL_EXPORT_SC_(ClPkg, make_string_input_stream);
SYMBOL_EXPORT_SC_(ClPkg, invoke_restart);
SYMBOL_EXPORT_SC_(ClPkg, get);
SYMBOL_EXPORT_SC_(ClPkg, find_restart);
SYMBOL_EXPORT_SC_(ClPkg, fill_pointer);
SYMBOL_EXPORT_SC_(ClPkg, directory);
SYMBOL_EXPORT_SC_(ClPkg, defvar);
SYMBOL_EXPORT_SC_(ClPkg, defun);
SYMBOL_EXPORT_SC_(ClPkg, defparameter);
SYMBOL_EXPORT_SC_(ClPkg, defconstant);
SYMBOL_EXPORT_SC_(ClPkg, debug);
SYMBOL_EXPORT_SC_(ClPkg, count);
SYMBOL_EXPORT_SC_(ClPkg, compute_restarts);
SYMBOL_EXPORT_SC_(ClPkg, char);

SYMBOL_EXPORT_SC_(KeywordPkg, fill_pointer);
SYMBOL_EXPORT_SC_(KeywordPkg, escape);
SYMBOL_EXPORT_SC_(ClPkg, write);
SYMBOL_EXPORT_SC_(KeywordPkg, capitalize);
SYMBOL_EXPORT_SC_(ClPkg, STARreadDefaultFloatFormatSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARprint_escapeSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARprint_baseSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARprint_levelSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARprint_lengthSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARprint_radixSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARprint_caseSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARprint_gensymSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARprint_arraySTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARprint_readablySTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARprint_escapeSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARprint_circleSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARprint_linesSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARprint_right_marginSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARprint_prettySTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARprint_miser_widthSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARprint_pprint_dispatchSTAR);

SYMBOL_EXPORT_SC_(CorePkg, signalSimpleError);
SYMBOL_SC_(CorePkg, invokeInternalDebugger);
SYMBOL_EXPORT_SC_(ClPkg, STARdebuggerHookSTAR);

SYMBOL_EXPORT_SC_(ClPkg, break);
SYMBOL_EXPORT_SC_(ClPkg, STARbreakOnSignalsSTAR);
SYMBOL_SC_(CorePkg, STARnestedErrorDepthSTAR);
SYMBOL_SC_(CorePkg, universalErrorHandler);
SYMBOL_EXPORT_SC_(KeywordPkg, type_error);
SYMBOL_EXPORT_SC_(KeywordPkg, datum);
SYMBOL_EXPORT_SC_(KeywordPkg, expected_type);
SYMBOL_EXPORT_SC_(ClPkg, type_error);
SYMBOL_EXPORT_SC_(CorePkg, STARliteral_print_objectSTAR);
SYMBOL_EXPORT_SC_(ClPkg, printObject);
SYMBOL_EXPORT_SC_(ClPkg, makeCondition);
SYMBOL_EXPORT_SC_(ClPkg, controlError);
SYMBOL_EXPORT_SC_(KeywordPkg, print);
SYMBOL_EXPORT_SC_(KeywordPkg, pathname);
SYMBOL_SC_(CorePkg, setThrowPosition);
SYMBOL_EXPORT_SC_(CorePkg, tooFewArgumentsError);
SYMBOL_EXPORT_SC_(CorePkg, tooManyArgumentsError);
SYMBOL_EXPORT_SC_(KeywordPkg, object);
SYMBOL_EXPORT_SC_(KeywordPkg, format_control);
SYMBOL_EXPORT_SC_(KeywordPkg, format_arguments);
SYMBOL_EXPORT_SC_(KeywordPkg, name);
SYMBOL_EXPORT_SC_(KeywordPkg, stream);
SYMBOL_EXPORT_SC_(KeywordPkg, package);
SYMBOL_SC_(CorePkg, unrecognizedKeywordArgumentError);
SYMBOL_SC_(CorePkg, invalidKeywordArgumentError);
SYMBOL_EXPORT_SC_(ClPkg, satisfies);
SYMBOL_EXPORT_SC_(ClPkg, fillPointer);
SYMBOL_EXPORT_SC_(ClPkg, arrayDimension);
SYMBOL_EXPORT_SC_(ClPkg, array_has_fill_pointer_p);
SYMBOL_EXPORT_SC_(ClPkg, simple_bit_vector);
SYMBOL_EXPORT_SC_(ClPkg, bit_vector);


SYMBOL_EXPORT_SC_(CorePkg, array_out_of_bounds);
SYMBOL_EXPORT_SC_(KeywordPkg, array);
SYMBOL_EXPORT_SC_(CorePkg, replaceArray);
SYMBOL_EXPORT_SC_(CorePkg, fillArrayWithElt );
SYMBOL_EXPORT_SC_(CorePkg, fillPointerSet );
SYMBOL_EXPORT_SC_(CorePkg, swapElements );
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_llvmTargetTriple_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_variant_name_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_bitcode_name_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_executable_name_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_application_name_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, STARbuild_libSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARbuild_stlibSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARbuild_linkflagsSTAR);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_run_all_function_name_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, _PLUS_clasp_ctor_function_name_PLUS_);
SYMBOL_EXPORT_SC_(CorePkg, STARcodeWalkerSTAR);
SYMBOL_SC_(CorePkg, STARdebugMacroexpandSTAR);
SYMBOL_EXPORT_SC_(ClPkg, T);
SYMBOL_EXPORT_SC_(ClPkg, method);
SYMBOL_EXPORT_SC_(ClPkg, generic_function);
SYMBOL_SC_(CorePkg, STARenvironmentPrintingTabSTAR);
SYMBOL_SC_(CorePkg, STARenvironmentPrintingTabIncrementSTAR);
SYMBOL_EXPORT_SC_(CorePkg, STARenvironment_debugSTAR);
SYMBOL_SC_(CorePkg, _PLUS_activationFrameNil_PLUS_);
SYMBOL_EXPORT_SC_(ClPkg, cond);
SYMBOL_SC_(CorePkg, globalFunction);
SYMBOL_SC_(CorePkg, globalSetfFunction);
SYMBOL_SC_(CorePkg, lexicalFunction);
SYMBOL_SC_(CorePkg, declaredSpecial);
SYMBOL_SC_(CorePkg, lexical);
SYMBOL_EXPORT_SC_(ClPkg, stream);
SYMBOL_EXPORT_SC_(ClPkg, boolean);
SYMBOL_EXPORT_SC_(ClPkg, keyword);
SYMBOL_EXPORT_SC_(ClPkg, array);
SYMBOL_EXPORT_SC_(ClPkg, simple_array);
SYMBOL_EXPORT_SC_(ClPkg, vector);
SYMBOL_EXPORT_SC_(ClPkg, simple_vector);
SYMBOL_EXPORT_SC_(ClPkg, input_stream_p);
SYMBOL_EXPORT_SC_(ClPkg, output_stream_p);

SYMBOL_SC_(CorePkg, STARsystem_defsetf_update_functionsSTAR);
SYMBOL_EXPORT_SC_(ExtPkg, _PLUS_processStandardInput_PLUS_);
SYMBOL_EXPORT_SC_(ExtPkg, _PLUS_processStandardOutput_PLUS_);
SYMBOL_EXPORT_SC_(ExtPkg, _PLUS_processErrorOutput_PLUS_);
SYMBOL_EXPORT_SC_(ClPkg, STARstandard_inputSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARstandard_outputSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARerror_outputSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARtrace_outputSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARdebug_ioSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARquery_ioSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARterminal_ioSTAR);

SYMBOL_EXPORT_SC_(ClPkg, STARgensym_counterSTAR);
SYMBOL_EXPORT_SC_(ClPkg, standard_char);
SYMBOL_EXPORT_SC_(ClPkg, extended_char);
SYMBOL_EXPORT_SC_(ClPkg, special);
SYMBOL_EXPORT_SC_(ClPkg, nconc);
SYMBOL_EXPORT_SC_(ClPkg, cons);
SYMBOL_EXPORT_SC_(ClPkg, cadr);
SYMBOL_SC_(CorePkg, STARbackquote_expand_hookSTAR);
SYMBOL_SC_(CorePkg, single_dispatch_on);
SYMBOL_EXPORT_SC_(ClPkg, STARmacroexpand_hookSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARread_baseSTAR);
SYMBOL_EXPORT_SC_(ClPkg, compile);
SYMBOL_EXPORT_SC_(ClPkg, load);
SYMBOL_EXPORT_SC_(ClPkg, eval);
SYMBOL_EXPORT_SC_(KeywordPkg, compile_toplevel);
SYMBOL_EXPORT_SC_(KeywordPkg, load_toplevel);
SYMBOL_EXPORT_SC_(KeywordPkg, execute);

SYMBOL_EXPORT_SC_(ClPkg, STARread_evalSTAR);
SYMBOL_SC_(CorePkg, STARdocumentation_poolSTAR);
SYMBOL_EXPORT_SC_(ClPkg, define_modify_macro);
SYMBOL_EXPORT_SC_(ClPkg, destructuring_bind);
SYMBOL_EXPORT_SC_(ClPkg, deftype);
SYMBOL_EXPORT_SC_(ClPkg, define_method_combination);
SYMBOL_SC_(CorePkg, generic);
SYMBOL_EXPORT_SC_(ClPkg, defsetf);
SYMBOL_EXPORT_SC_(KeywordPkg, allow_other_keys);
SYMBOL_SC_(CorePkg, DOT);
SYMBOL_EXPORT_SC_(ClPkg, AMPwhole);
SYMBOL_EXPORT_SC_(ClPkg, AMPenvironment);
SYMBOL_EXPORT_SC_(ClPkg, AMPoptional);
SYMBOL_EXPORT_SC_(ClPkg, AMPkey);
SYMBOL_EXPORT_SC_(ClPkg, AMPallow_other_keys);
SYMBOL_EXPORT_SC_(ClPkg, AMPaux);
SYMBOL_EXPORT_SC_(ClPkg, AMPrest);
SYMBOL_EXPORT_SC_(CorePkg, AMPva_rest);
SYMBOL_EXPORT_SC_(CorePkg, valist );
SYMBOL_EXPORT_SC_(ClPkg, AMPbody);
SYMBOL_EXPORT_SC_(ClPkg, integer);
SYMBOL_EXPORT_SC_(ClPkg, sequence);
SYMBOL_SC_(CorePkg, anonymous);
SYMBOL_EXPORT_SC_(ClPkg, declare);
SYMBOL_EXPORT_SC_(KeywordPkg, macro);
SYMBOL_EXPORT_SC_(KeywordPkg, function);
SYMBOL_EXPORT_SC_(KeywordPkg, dispatch_function);
SYMBOL_SC_(CorePkg, macro);
SYMBOL_EXPORT_SC_(ClPkg, function);
SYMBOL_EXPORT_SC_(ClPkg, variable);
SYMBOL_SC_(CorePkg, STARdocumentation_databaseSTAR);
SYMBOL_EXPORT_SC_(ClPkg, bit);
SYMBOL_SC_(CorePkg, __init__);
SYMBOL_EXPORT_SC_(ClPkg, STARreadtableSTAR);
SYMBOL_SC_(CorePkg, input_stream_designator);
/*! Set to true if you want the repl to print what was read */
SYMBOL_SC_(CorePkg, STARechoReplReadSTAR);
SYMBOL_EXPORT_SC_(KeywordPkg, brcl);
SYMBOL_EXPORT_SC_(KeywordPkg, not);
SYMBOL_EXPORT_SC_(KeywordPkg, and);
SYMBOL_EXPORT_SC_(KeywordPkg, or );
SYMBOL_EXPORT_SC_(ClPkg, and);
SYMBOL_EXPORT_SC_(ClPkg, or );
SYMBOL_EXPORT_SC_(ClPkg, car);
SYMBOL_EXPORT_SC_(ClPkg, cdr);
SYMBOL_EXPORT_SC_(ClPkg, dotimes);
SYMBOL_EXPORT_SC_(ClPkg, dolist);
SYMBOL_EXPORT_SC_(ClPkg, do);
SYMBOL_EXPORT_SC_(ClPkg, package);
SYMBOL_EXPORT_SC_(ClPkg, string);
SYMBOL_SC_(CorePkg, eof_error_p);
SYMBOL_SC_(CorePkg, eof_value);
SYMBOL_SC_(CorePkg, start);
SYMBOL_SC_(CorePkg, end);
SYMBOL_SC_(CorePkg, preserve_whitespace);
SYMBOL_EXPORT_SC_(ClPkg, aref);
SYMBOL_EXPORT_SC_(ClPkg, nth);
SYMBOL_SC_(CorePkg, io);
SYMBOL_SC_(CorePkg, probe);
SYMBOL_EXPORT_SC_(ClPkg, error);
SYMBOL_SC_(CorePkg, newVersion);
SYMBOL_SC_(CorePkg, renameAndDelete);
SYMBOL_SC_(CorePkg, overwrite);
SYMBOL_EXPORT_SC_(ClPkg, append);
SYMBOL_SC_(CorePkg, supersede);
SYMBOL_SC_(CorePkg, create);
SYMBOL_SC_(CorePkg, input_stream);
SYMBOL_SC_(CorePkg, recursive_p);
SYMBOL_SC_(CorePkg, dimensions);
SYMBOL_SC_(CorePkg, element_type);
SYMBOL_SC_(CorePkg, initial_element);
SYMBOL_SC_(CorePkg, adjustable);

SYMBOL_EXPORT_SC_(ClPkg, adjust_array);
SYMBOL_EXPORT_SC_(ClPkg, gethash);
SYMBOL_SC_(CorePkg, object);
SYMBOL_EXPORT_SC_(ClPkg, eq);
SYMBOL_EXPORT_SC_(ClPkg, eql);
SYMBOL_EXPORT_SC_(ClPkg, equal);
SYMBOL_EXPORT_SC_(ClPkg, equalp);
SYMBOL_SC_(CorePkg, okey);
SYMBOL_EXPORT_SC_(ClPkg, hash_table);
SYMBOL_SC_(CorePkg, default);

SYMBOL_EXPORT_SC_(KeywordPkg, class);
SYMBOL_EXPORT_SC_(KeywordPkg, instance);

SYMBOL_EXPORT_SC_(KeywordPkg, output);
SYMBOL_EXPORT_SC_(KeywordPkg, input);
SYMBOL_EXPORT_SC_(KeywordPkg, io);
SYMBOL_EXPORT_SC_(KeywordPkg, default);
SYMBOL_EXPORT_SC_(KeywordPkg, internal);
SYMBOL_EXPORT_SC_(KeywordPkg, external);
SYMBOL_EXPORT_SC_(KeywordPkg, inherited);
SYMBOL_EXPORT_SC_(KeywordPkg, changed);
SYMBOL_SC_(CorePkg, dot);
SYMBOL_EXPORT_SC_(ClPkg, STARfeaturesSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARload_printSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARload_verboseSTAR);
SYMBOL_SC_(CorePkg, ifDoesNotExist);
SYMBOL_EXPORT_SC_(KeywordPkg, debug);
SYMBOL_EXPORT_SC_(KeywordPkg, direct_super_classes);
SYMBOL_EXPORT_SC_(ClPkg, lambda);
SYMBOL_EXPORT_SC_(CorePkg, symbolMacroletLambda);
SYMBOL_EXPORT_SC_(ExtPkg, lambda_block);
SYMBOL_SC_(CorePkg, lambda_with_handler);
SYMBOL_EXPORT_SC_(ClPkg, symbol);
SYMBOL_SC_(CorePkg, color);
SYMBOL_SC_(CorePkg, foreach);
SYMBOL_SC_(CorePkg, STARPATHSTAR);
SYMBOL_SC_(CorePkg, STARargsSTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARpackageSTAR);
//SYMBOL_SC_(CorePkg, STARcurrent_working_directorySTAR);
SYMBOL_EXPORT_SC_(ClPkg, STARmodulesSTAR);
SYMBOL_EXPORT_SC_(ClPkg, progn);
SYMBOL_SC_(CorePkg, backquote);
SYMBOL_SC_(CorePkg, double_backquote);
SYMBOL_SC_(CorePkg, unquote);
// was S Y M B O L _SC_(CorePkg,comma);
SYMBOL_SC_(CorePkg, unquote_splice);
// was S Y M B O L _SC_(CorePkg,comma_atsign);
SYMBOL_SC_(CorePkg, unquote_nsplice);
// was S Y M B O L _SC_(CorePkg,comma_dot);
SYMBOL_EXPORT_SC_(ClPkg, quote);
SYMBOL_EXPORT_SC_(ClPkg, function);
SYMBOL_SC_(CorePkg, slot);
SYMBOL_EXPORT_SC_(ClPkg, slot_value);
SYMBOL_EXPORT_SC_(ClPkg, slot_unbound);
SYMBOL_EXPORT_SC_(ClPkg, values);
SYMBOL_SC_(CorePkg, item);
SYMBOL_SC_(CorePkg, alist);
SYMBOL_EXPORT_SC_(ClPkg, list);
SYMBOL_SC_(CorePkg, key);
SYMBOL_SC_(CorePkg, test_not);

SYMBOL_EXPORT_SC_(ClPkg, standard_class);
SYMBOL_EXPORT_SC_(ClPkg, rest);

SYMBOL_EXPORT_SC_(CorePkg, instance);
SYMBOL_SC_(CorePkg, all_keys);

SYMBOL_EXPORT_SC_(KeywordPkg, changed);
SYMBOL_EXPORT_SC_(CorePkg,STARstack_top_hintSTAR);

SYMBOL_EXPORT_SC_(ClPkg, structure_object);
SYMBOL_EXPORT_SC_(ClPkg, standard_object);
SYMBOL_EXPORT_SC_(ClPkg, copy_structure);

SYMBOL_EXPORT_SC_(CorePkg, defcallback);
SYMBOL_EXPORT_SC_(CorePkg, sharp_a_reader);
SYMBOL_EXPORT_SC_(CorePkg, sharp_s_reader);
SYMBOL_EXPORT_SC_(CorePkg, sharpmacros_lisp_redefine);


void testConses() {
  printf("%s:%d Testing Conses and iterators\n", __FILE__, __LINE__);
  List_sp cur = _Nil<T_O>();
  for (int i = 1; i < 1000; ++i) {
    cur = Cons_O::create(make_fixnum(i), cur);
  }
  List_sp l = coerce_to_list(cur);
  for (int trials = 0; trials < 3; ++trials) {
    int times = 0xdead;
    long long fastCount = 0;
    LightTimer fastTimer;
    fastTimer.reset();
    fastTimer.start();
    for (int i = 0; i < times; ++i) {
      for (auto c : l.full()) {
        T_sp t = c->_Car;
        fastCount += unbox_fixnum(gc::As<Fixnum_sp>(t));
      }
    }
    fastTimer.stop();
    printf("%s:%d Fast list traversal time: %lf counted %lld elements\n", __FILE__, __LINE__, fastTimer.getAccumulatedTime(), fastCount);
    long long normalCount = 0;
    LightTimer normalTimer;
    normalTimer.reset();
    normalTimer.start();
    for (int i = 0; i < times; ++i) {
      for (auto c : l) {
        T_sp t = c->_Car;
        normalCount += unbox_fixnum(gc::As<Fixnum_sp>(t));
      }
    }
    normalTimer.stop();
    printf("%s:%d Normal list traversal time: %lf counted %lld elements\n", __FILE__, __LINE__, normalTimer.getAccumulatedTime(), normalCount);
  }
};

void setNilable(gc::Nilable<String_sp> &val, bool s) {
  if (!s) {
    val = _Nil<String_O>();
  } else {
    val = gc::As_unsafe<String_sp>(SimpleBaseString_O::make("Yahoo"));
  }
}

gc::Nilable<String_sp> getNilable(bool s) {
  gc::Nilable<String_sp> val;
  if (!s) {
    val = _Nil<String_O>();
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
  foo = _Nil<T_O>();
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
  testConses();
}

CoreExposer_O::CoreExposer_O(Lisp_sp lisp) : Exposer_O(lisp, CorePkg) {
};

__attribute((optnone))
void CoreExposer_O::expose(core::Lisp_sp lisp, WhatToExpose what) const {
  switch (what) {
  case candoClasses:
    break;
  case candoFunctions:
    exposeCando_Numerics();
    exposeCore_lisp_reader();
    {
      ReadTable_sp readtable = ReadTable_O::create_standard_readtable();
      if (!readtable->_SyntaxTypes) {
        printf("%s:%d The readtable->_SyntaxTypes is NULL\n", __FILE__, __LINE__ );
        abort();
      }
      cl::_sym_STARreadtableSTAR->defparameter(readtable);
    }
    break;
  case candoGlobals: {
    // expose the CorePkg constants here
    //----------- symbols are created in lisp.cc::startupLispEnvironment ----------
    //#define SYMBOLS_CREATE
    //#i n c l u d e SYMBOLS_SCRAPED_INC_H
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

void CoreExposer_O::define_essential_globals(Lisp_sp lisp) {
  {
    this->package()->usePackage(gc::As<Package_sp>(_lisp->findPackage("CL", true)));
    _BLOCK_TRACEF(BF("Exporting symbols in lisp"));
#define CorePkg_EXPORT
#define DO_SYMBOL( ns, cname, idx, pkgName, lispName, export) cname->exportYourself(export);
#ifndef SCRAPING
#include SYMBOLS_SCRAPED_INC_H
#endif
#undef DO_SYMBOL
#undef CorePkg_EXPORT
  };
  /* Set the values of some essential global symbols */
  cl::_sym_nil = gctools::smart_ptr<core::Symbol_O>((gctools::Tagged)gctools::global_tagged_Symbol_OP_nil); //->initialize();
  cl::_sym_nil->_Name = SimpleBaseString_O::make("NIL");
  //        printf("%s:%d About to add NIL to the COMMON-LISP package - is it defined at this point\n", __FILE__, __LINE__ );
  //	_lisp->_Roots._CommonLispPackage->add_symbol_to_package("NIL"cl::_sym_nil);
  cl::_sym_nil->_HomePackage = _lisp->_Roots._CommonLispPackage;
  cl::_sym_nil->setf_symbolValue(_Nil<T_O>());
  cl::_sym_nil->makeSpecial();
  cl::_sym_nil->exportYourself();
  cl::_sym_nil->setReadOnly(true);
  _lisp->commonLispPackage()->add_symbol_to_package(SimpleBaseString_O::make("NIL"), _Nil<Symbol_O>(), true);
  _lisp->_Roots._TrueObject = cl::_sym_T_O;
  cl::_sym_T_O->exportYourself()->defparameter(_lisp->_Roots._TrueObject);
  cl::_sym_T_O->setReadOnly(true);
  cl::_sym_STARload_printSTAR->exportYourself()->defparameter(_lisp->_false());
  cl::_sym_STARload_verboseSTAR->exportYourself()->defparameter(_lisp->_false());
  cl::_sym_STARread_suppressSTAR->exportYourself()->defparameter(_lisp->_false());
  cl::_sym_STARpackageSTAR->exportYourself()->defparameter(lisp->_Roots._CorePackage);
  _sym_STARpreserve_whitespace_pSTAR->defparameter(_lisp->_false());
  _sym_STARechoReplReadSTAR->exportYourself()->defparameter(_lisp->_false());
  _sym_STARbackquote_levelSTAR->defparameter(make_fixnum(0));
  cl::_sym_STARmodulesSTAR->defparameter(_Nil<T_O>());
  cl::_sym_STARread_evalSTAR->defparameter(_lisp->_true());
  _sym_STARenvironmentPrintingTabSTAR->defparameter(make_fixnum(0));
  _sym_STARenvironmentPrintingTabIncrementSTAR->defparameter(make_fixnum(6));
  _sym_STARenvironment_debugSTAR->defparameter(_Nil<T_O>());
  SYMBOL_EXPORT_SC_(ClPkg, most_negative_fixnum);
  cl::_sym_most_negative_fixnum->defconstant(make_fixnum(MOST_NEGATIVE_FIXNUM));
  SYMBOL_EXPORT_SC_(ClPkg, most_positive_fixnum);
  cl::_sym_most_positive_fixnum->defconstant(make_fixnum(MOST_POSITIVE_FIXNUM));

  // SYMBOL_EXPORT_SC_(ClPkg,most_negative_double_float);
  // cl::_sym_most_negative_double_float->defconstant(DoubleFloat_O::create(DBL_MIN));
  // SYMBOL_EXPORT_SC_(ClPkg,most_negative_long_float);
  // cl::_sym_most_negative_long_float->defconstant(DoubleFloat_O::create(DBL_MIN));
  // SYMBOL_EXPORT_SC_(ClPkg,most_negative_short_float);
  // cl::_sym_most_negative_short_float->defconstant(DoubleFloat_O::create(DBL_MIN));
  // SYMBOL_EXPORT_SC_(ClPkg,most_negative_single_float);
  // cl::_sym_most_negative_single_float->defconstant(DoubleFloat_O::create(DBL_MIN));
  // SYMBOL_EXPORT_SC_(ClPkg,most_positive_double_float);
  // cl::_sym_most_positive_double_float->defconstant(DoubleFloat_O::create(DBL_MAX));
  // SYMBOL_EXPORT_SC_(ClPkg,least_negative_normalized_long_float);
  // cl::_sym_least_negative_normalized_long_float->defconstant(DoubleFloat_O::create(-std::numeric_limits<LongFloat>::denorm_min()));
  // SYMBOL_EXPORT_SC_(ClPkg,least_positive_normalized_long_float);
  // cl::_sym_least_positive_normalized_long_float->defconstant(DoubleFloat_O::create(std::numeric_limits<LongFloat>::denorm_min()));

  // SYMBOL_EXPORT_SC_(ClPkg,most_positive_long_float);
  // cl::_sym_most_positive_long_float->defconstant(DoubleFloat_O::create(DBL_MAX));
  // SYMBOL_EXPORT_SC_(ClPkg,most_positive_short_float);
  // cl::_sym_most_positive_short_float->defconstant(DoubleFloat_O::create(DBL_MAX));
  // SYMBOL_EXPORT_SC_(ClPkg,most_positive_single_float);
  // cl::_sym_most_positive_single_float->defconstant(DoubleFloat_O::create(DBL_MAX));

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
  cl::_sym_STARprint_circleSTAR->defparameter(_Nil<T_O>());
  cl::_sym_STARprint_escapeSTAR->defparameter(_lisp->_true());
  cl::_sym_STARprint_gensymSTAR->defparameter(_lisp->_true());
  cl::_sym_STARprint_lengthSTAR->defparameter(_Nil<T_O>());
  cl::_sym_STARprint_levelSTAR->defparameter(_Nil<T_O>());
  cl::_sym_STARprint_linesSTAR->defparameter(_Nil<T_O>());
  cl::_sym_STARprint_miser_widthSTAR->defparameter(_Nil<T_O>());
  cl::_sym_STARprint_pprint_dispatchSTAR->defparameter(_Nil<T_O>());
  _sym_STARenablePrintPrettySTAR->defparameter(_Nil<T_O>()); // _lisp->_true()); // Just for debugging *print-pretty*
  cl::_sym_STARprint_prettySTAR->defparameter(_Nil<T_O>());
  cl::_sym_STARprint_radixSTAR->defparameter(_Nil<T_O>());
  cl::_sym_STARprint_readablySTAR->defparameter(_Nil<T_O>());
  cl::_sym_STARprint_right_marginSTAR->defparameter(_Nil<T_O>());

  //        testPointers();

  T_sp stdin_stream = IOStreamStream_O::makeInput("*STDIN*", stdin);
  T_sp stdout_stream = IOStreamStream_O::makeOutput("*STDOUT*", stdout);
  T_sp stderr_stream = IOStreamStream_O::makeOutput("*STDERR*", stderr);
  ext::_sym__PLUS_processStandardInput_PLUS_->defparameter(stdin_stream);
  ext::_sym__PLUS_processStandardOutput_PLUS_->defparameter(stdout_stream);
  ext::_sym__PLUS_processErrorOutput_PLUS_->defparameter(stderr_stream);
  _sym_STARcurrentSourcePosInfoSTAR->defparameter(_Nil<T_O>());
  cl::_sym_STARstandard_inputSTAR->defparameter(SynonymStream_O::make(ext::_sym__PLUS_processStandardInput_PLUS_));
  cl::_sym_STARstandard_outputSTAR->defparameter(SynonymStream_O::make(ext::_sym__PLUS_processStandardOutput_PLUS_));
  cl::_sym_STARerror_outputSTAR->defparameter(SynonymStream_O::make(ext::_sym__PLUS_processErrorOutput_PLUS_));
  cl::_sym_STARtrace_outputSTAR->defparameter(SynonymStream_O::make(ext::_sym__PLUS_processErrorOutput_PLUS_));
  cl::_sym_STARdebug_ioSTAR->defparameter(TwoWayStream_O::make(stdin_stream, stdout_stream));
  cl::_sym_STARquery_ioSTAR->defparameter(TwoWayStream_O::make(stdin_stream, stdout_stream));
  core::_sym_STARstack_top_hintSTAR->defparameter(_Nil<T_O>());
  _sym_STARdocumentation_poolSTAR->defparameter(Cons_O::createList(HashTableEql_O::create_default(), SimpleBaseString_O::make("help_file.dat")));
  _sym_STARdocumentation_poolSTAR->exportYourself();
  TwoWayStream_sp terminal = gc::As_unsafe<TwoWayStream_sp>(TwoWayStream_O::make(stdin_stream, stdout_stream));
  _lisp->_Roots._TerminalIO = terminal;
  cl::_sym_STARterminal_ioSTAR->defparameter(terminal);
  _sym_STARsystem_defsetf_update_functionsSTAR->defparameter(_Nil<T_O>());
  cl::_sym_STARmacroexpand_hookSTAR->defparameter(cl::_sym_funcall);
  _sym_STARsharp_equal_final_tableSTAR->defparameter(_Nil<T_O>());
  _sym__PLUS_activationFrameNil_PLUS_->defconstant(_Nil<T_O>());
  _sym__PLUS_variant_name_PLUS_->defconstant(SimpleBaseString_O::make(VARIANT_NAME));
  _sym__PLUS_bitcode_name_PLUS_->defconstant(SimpleBaseString_O::make(BITCODE_NAME));
  _sym__PLUS_executable_name_PLUS_->defconstant(SimpleBaseString_O::make(EXECUTABLE_NAME));
  _sym__PLUS_application_name_PLUS_->defconstant(SimpleBaseString_O::make(APP_NAME));
  _sym_STARbuild_libSTAR->defconstant(SimpleBaseString_O::make(BUILD_LIB));
  _sym_STARbuild_stlibSTAR->defconstant(SimpleBaseString_O::make(BUILD_STLIB));
  _sym_STARbuild_linkflagsSTAR->defconstant(SimpleBaseString_O::make(BUILD_LINKFLAGS));
  _sym__PLUS_run_all_function_name_PLUS_->defconstant(SimpleBaseString_O::make(RUN_ALL_FUNCTION_NAME));
  _sym__PLUS_clasp_ctor_function_name_PLUS_->defconstant(SimpleBaseString_O::make(CLASP_CTOR_FUNCTION_NAME));
  SYMBOL_SC_(CorePkg, cArgumentsLimit);
  _sym_cArgumentsLimit->defconstant(make_fixnum(Lisp_O::MaxFunctionArguments));
  _sym_STARdebugMacroexpandSTAR->defparameter(_Nil<T_O>());
  _lisp->_Roots._ClassTable = HashTable_O::create_thread_safe(cl::_sym_eq,SimpleBaseString_O::make("CLTBLRD"),SimpleBaseString_O::make("CLTBLWR"));
  _sym_STARcodeWalkerSTAR->defparameter(_Nil<T_O>());
  _sym_STARsharpEqContextSTAR->defparameter(_Nil<T_O>());
  cl::_sym_STARreadDefaultFloatFormatSTAR->defparameter(cl::_sym_single_float);
  _sym_STARnestedErrorDepthSTAR->defparameter(make_fixnum(0));
  cl::_sym_STARbreakOnSignalsSTAR->defparameter(_Nil<T_O>());
  cl::_sym_STARdebuggerHookSTAR->defparameter(_Nil<T_O>());
  cl::_sym_internalTimeUnitsPerSecond->defconstant(make_fixnum(CLASP_INTERNAL_TIME_UNITS_PER_SECOND));
  _sym_STARstartRunTimeSTAR->defparameter(PosixTime_O::createNow());
  cl::_sym_MultipleValuesLimit->defconstant(make_fixnum(MultipleValues::MultipleValuesLimit));
  _sym_STARprint_structureSTAR->defparameter(_Nil<T_O>());
  _sym_STARprintPackageSTAR->defparameter(_Nil<T_O>());
  _sym_STARcircle_counterSTAR->defparameter(_Nil<T_O>());
  _sym_STARcircle_stackSTAR->defparameter(_Nil<T_O>());
  _sym_STARdebugReaderSTAR->defparameter(_Nil<T_O>());
  _sym__PLUS_known_typep_predicates_PLUS_->defparameter(_Nil<T_O>());
  cl::_sym_STARloadPathnameSTAR->defparameter(_Nil<T_O>());
  cl::_sym_STARloadTruenameSTAR->defparameter(_Nil<T_O>());
  cl::_sym_callArgumentsLimit->defconstant(make_fixnum(CALL_ARGUMENTS_LIMIT));
  cl::_sym_lambdaParametersLimit->defconstant(make_fixnum(CALL_ARGUMENTS_LIMIT));
  cl::_sym_arrayDimensionLimit->defconstant(make_fixnum(MOST_POSITIVE_FIXNUM));
  cl::_sym_arrayTotalSizeLimit->defconstant(make_fixnum(MOST_POSITIVE_FIXNUM));
  core::_sym__PLUS_standardReadtable_PLUS_->defparameter(_Nil<T_O>());
  core::_sym_STARpollTicksPerGcSTAR->defparameter(make_fixnum(POLL_TICKS_PER_GC));
  comp::_sym_STARlowLevelTraceSTAR->defparameter(_Nil<T_O>());
  comp::_sym_STARlowLevelTracePrintSTAR->defparameter(_Nil<T_O>());
  comp::_sym_STARsave_module_for_disassembleSTAR->defparameter(_Nil<core::T_O>());
  comp::_sym_STARsaved_module_from_clasp_jitSTAR->defparameter(_Nil<core::T_O>());
  comp::_sym_STARdebug_jitSTAR->defparameter(_Nil<core::T_O>());
  _sym_STARallCxxClassesSTAR->defparameter(_Nil<T_O>());
  _sym_STARtopLevelCommandHookSTAR->defparameter(_Nil<T_O>());
  _sym_STARllvmFunctionNameHookSTAR->defparameter(_Nil<T_O>());
  _sym_STARihsCurrentSTAR->defparameter(_Nil<T_O>());
  _sym_STARserializerArchiveSTAR->defparameter(_Nil<T_O>());
  _sym_STARcommandLineLoadSTAR->defparameter(_Nil<T_O>());
  _sym_STARdebugMonitorSTAR->defparameter(_Nil<T_O>());
  _sym_STARwatchDynamicBindingStackSTAR->defparameter(_Nil<T_O>());
  _sym_STARdebugLoadTimeValuesSTAR->defparameter(_Nil<T_O>());
  _sym_STARdebugEvalSTAR->defparameter(_Nil<T_O>());
  _sym_STARdebugStartupSTAR->defparameter(_Nil<T_O>());
  _sym_STARliteral_print_objectSTAR->defparameter(_Nil<T_O>());
  _sym_STARdebugInterpretedFunctionsSTAR->defparameter(_Nil<T_O>());
  _sym_STARuseInterpreterForEvalSTAR->defparameter(_Nil<T_O>()); // _lisp->_true());
  _sym_STARcxxDocumentationSTAR->defparameter(_Nil<T_O>());
  _sym_STARinterpreterTraceSTAR->defparameter(_Nil<T_O>());
  _sym__PLUS_class_name_to_lisp_name_PLUS_->defparameter(_Nil<T_O>());
  _sym__PLUS_type_header_value_map_PLUS_->defparameter(_Nil<T_O>());
  initialize_typeq_map();
  gctools::_sym_STARfinalizersSTAR->defparameter(WeakKeyHashTable_O::create());
  _sym_STARllvmVersionSTAR->defparameter(SimpleBaseString_O::make(LLVM_VERSION));
#ifdef USE_PARALLEL_BUILD
  _sym_STARuseParallelBuildSTAR->defparameter(_lisp->_true());
#else
  _sym_STARuseParallelBuildSTAR->defparameter(_Nil<T_O>());
#endif
#ifdef USE_BUILD_FORK_REDIRECT_OUTPUT
  _sym_STARuseBuildForkRedirectSTAR->defparameter(_lisp->_true());
#else
  _sym_STARuseBuildForkRedirectSTAR->defparameter(_Nil<T_O>());
#endif
  _sym__PLUS_numberOfFixedArguments_PLUS_->defconstant(make_fixnum(LCC_ARGS_IN_REGISTERS));
  _sym__PLUS_WNOHANG_PLUS_->defconstant(make_fixnum(WNOHANG));
  cl::_sym_STARrandom_stateSTAR->defparameter(RandomState_O::create());
  // Set up a hash table to save JIT info
  HashTableEqual_sp jit_save = HashTableEqual_O::create_default();
#ifdef CLASP_THREADS
  // When threading - make *jit-saved-symbol-info* a thread safe hash table
  SimpleBaseString_sp sbsr = SimpleBaseString_O::make("JITSAVR");
  SimpleBaseString_sp sbsw = SimpleBaseString_O::make("JITSAVW");
  jit_save->_Mutex = mp::SharedMutex_O::make_shared_mutex(sbsr,sbsw);
#endif
  comp::_sym_STARjit_saved_symbol_infoSTAR->defparameter(jit_save);
  //
  comp::_sym_STARllvm_contextSTAR->defparameter(llvmo::LLVMContext_O::create_llvm_context());
  comp::_sym_STARload_time_value_holder_nameSTAR->defparameter(core::SimpleBaseString_O::make("[VALUES-TABLE]"));
  List_sp hooks = _Nil<T_O>();
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("lfasl"), _sym_loadSource), hooks); // List of load commands
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("fasl"), _sym_load_binary), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("fasb"), _sym_load_binary), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("bundle"), _sym_load_binary), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("dylib"), _sym_load_binary), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("so"), _sym_load_binary), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("ll"), llvmo::_sym_load_bitcode_ll), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("bc"), llvmo::_sym_load_bitcode), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("lbc"), llvmo::_sym_load_bitcode), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("l"), _sym_loadSource), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("L"), _sym_loadSource), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("lsp"), _sym_loadSource), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("LSP"), _sym_loadSource), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("asd"), _sym_loadSource), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("ASD"), _sym_loadSource), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("lisp"), _sym_loadSource), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("LISP"), _sym_loadSource), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("cl"), _sym_loadSource), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("CL"), _sym_loadSource), hooks);
  hooks = Cons_O::create(Cons_O::create(SimpleBaseString_O::make("clasprc"), _sym_loadSource), hooks);
  _sym_STARloadHooksSTAR->defparameter(hooks);
  ext::_sym_STARdefault_external_formatSTAR->defparameter(_lisp->_true());
  ext::_sym_STARinspectorHookSTAR->defparameter(_Nil<T_O>());
  ext::_sym_STARclasp_clang_pathSTAR->defparameter(SimpleBaseString_O::make(CLASP_CLANG_PATH));
  _sym_STARloadSearchListSTAR->defparameter(_Nil<T_O>());
  _sym_STARcurrent_dlopen_handleSTAR->defparameter(_Nil<T_O>());
  _sym_STARdebugInterpretedClosureSTAR->defparameter(_Nil<T_O>());
  _sym_STARdebugFlowControlSTAR->defparameter(_Nil<T_O>());
  _sym_STARbacktraceFrameSelectorHookSTAR->defparameter(_Nil<T_O>());
  _sym_STARfunctions_to_inlineSTAR->defparameter(HashTableEqual_O::create_default());
  _sym_STARfunctions_to_notinlineSTAR->defparameter(HashTableEqual_O::create_default());
  _sym_STARextension_startup_loadsSTAR->defparameter(_Nil<T_O>());
  SimpleBaseString_sp sbsr1 = SimpleBaseString_O::make("SYSPMNR");
  SimpleBaseString_sp sbsw1 = SimpleBaseString_O::make("SYSPMNW");
  _lisp->_Roots._Sysprop = gc::As<HashTableEql_sp>(HashTable_O::create_thread_safe(cl::_sym_eql,sbsr1,sbsw1));
  _sym_STARdebug_accessorsSTAR->defparameter(_Nil<T_O>());
  _sym_STARmodule_startup_function_nameSTAR->defparameter(SimpleBaseString_O::make(std::string(MODULE_STARTUP_FUNCTION_NAME)));
  _sym_STARmodule_shutdown_function_nameSTAR->defparameter(SimpleBaseString_O::make(std::string(MODULE_SHUTDOWN_FUNCTION_NAME)));
  std::list<string> nicknames;
  std::list<string> use_packages;
  _sym_STARclasp_packageSTAR->defparameter(_lisp->makePackage("CLASP!",nicknames,use_packages));
  _sym_STARdebug_fsetSTAR->defparameter(_Nil<core::T_O>());
  ext::_sym_ignore_signal->defparameter(SimpleBaseString_O::make("Ignore signal"));
  ext::_sym_STARinvoke_debugger_hookSTAR->defparameter(_Nil<core::T_O>());
  _sym_STARuse_cleavir_compilerSTAR->defparameter(_Nil<core::T_O>());
  _sym_STARinterrupts_enabledSTAR->defparameter(_lisp->_true());
  _sym_STARallow_with_interruptsSTAR->defparameter(_lisp->_true());
  _sym_STARexit_backtraceSTAR->defparameter(_Nil<core::T_O>());
  clos::_sym__PLUS_the_standard_class_PLUS_->defparameter(_lisp->_Roots._TheStandardClass);
  core::_sym__PLUS_contab_name_PLUS_->defparameter(SimpleBaseString_O::make(CONTAB_NAME));
  _sym_STARdebug_threadsSTAR->defparameter(_Nil<core::T_O>());
  _sym_STARdebug_fastgfSTAR->defparameter(_Nil<core::T_O>());
  _sym_STARdebug_dispatchSTAR->defparameter(_Nil<core::T_O>());
  _sym_STARdebug_valuesSTAR->defparameter(_Nil<core::T_O>());
  _sym_STARdebug_hash_tableSTAR->defparameter(_Nil<core::T_O>());
  _sym_STARforeign_data_reader_callbackSTAR->defparameter(_Nil<core::T_O>());
  _sym_STARinformation_callbackSTAR->defparameter(_Nil<core::T_O>());
  gctools::_sym_STARdebug_gcrootsSTAR->defparameter(_Nil<core::T_O>());
  int optimization_level = 3;
  const char* optLevel = getenv("CLASP_OPTIMIZATION_LEVEL");
  if (optLevel) {
    optimization_level = strtol(optLevel,NULL,10);
    if (optimization_level < 0) optimization_level = 0;
    if (optimization_level > 3) optimization_level = 3;
    printf("%s:%d Due to CLASP_OPTIMIZATION_LEVEL environment variable --> cmp:*optimization-level* = %d\n", __FILE__, __LINE__, optimization_level);
  } else {
    optimization_level = 3;
#ifdef DEBUG_LLVM_OPTIMIZATION_LEVEL_0
    optimization_level = 0;
    printf("%s:%d Due to DEBUG_LLVM_OPTIMIZATION_LEVEL_0 --> cmp:*optimization-level* = %d\n", __FILE__, __LINE__, optimization_level);
#else
    optimization_level = 3;
#endif
  }
  comp::_sym_STARoptimization_levelSTAR->defparameter(core::make_fixnum(optimization_level));
  _sym_STARbits_in_bit_array_wordSTAR->defparameter(core::clasp_make_fixnum(BIT_ARRAY_BYTE_SIZE));
  _sym_STARreader_generate_cstSTAR->defparameter(_Nil<core::T_O>());
  _sym_STARreader_cst_resultSTAR->defparameter(_Nil<core::T_O>());
  _sym_STARcache_macroexpandSTAR->defparameter(_Nil<core::T_O>());
  _sym_STARdebugByteCodeSTAR->defparameter(_Nil<core::T_O>());
  _sym_STARdebug_dtree_interpreterSTAR->defparameter(_Nil<core::T_O>());
#if defined(__x86_64__)
  SYMBOL_EXPORT_SC_(KeywordPkg, address_model_64);
  Symbol_sp address_model = kw::_sym_address_model_64;

#if defined(__APPLE__) && defined(__MACH__)
#include <TargetConditionals.h>

#if TARGET_OS_IPHONE == 1
#error Currently iPhone simulator and iOS are not supported
#elif TARGET_OS_MAC == 1

  SYMBOL_EXPORT_SC_(KeywordPkg, target_os_darwin);
  Symbol_sp target_os = kw::_sym_target_os_darwin;

#else
#error Your TargetConditionals.h file says you are not a Mac or iPhone?????
#endif

#elif defined(__linux__)

  SYMBOL_EXPORT_SC_(KeywordPkg, target_os_linux);
  Symbol_sp target_os = kw::_sym_target_os_linux;

#elif defined(__FreeBSD__)

  SYMBOL_EXPORT_SC_(KeywordPkg, target_os_freebsd);
  Symbol_sp target_os = kw::_sym_target_os_freebsd;

#else
#error Currently only MacOSX, linux and FreeBSD are supported for x86_64
#endif

#elif defined(__i386__)

  SYMBOL_EXPORT_SC_(KeywordPkg, address_model_32);
  Symbol_sp address_model = kw::_sym_address_model_32;

#if defined(__linux__)

  SYMBOL_EXPORT_SC_(KeywordPkg, target_os_linux);
  Symbol_sp target_os = kw::_sym_target_os_linux;

#else
#error Currently only linux is supported for i386
#endif

#else
#error Currently only x86_64 and i386 is supported
#endif

  ql::list features;
  features << target_os;
  features << address_model;

  // Now add other standard features
  // features << kw::_sym_brcl;

  cl::_sym_STARfeaturesSTAR->exportYourself()->defparameter(features.cons());
}

void add_defsetf_access_update(Symbol_sp access_fn, Symbol_sp update_fn) {
  Cons_sp pair = Cons_O::create(access_fn, update_fn);
  List_sp list = _sym_STARsystem_defsetf_update_functionsSTAR->symbolValue();
  _sym_STARsystem_defsetf_update_functionsSTAR->defparameter(Cons_O::create(pair, list));
}
};

#define EXPAND_CLASS_MACROS
#define _CLASS_MACRO(_T_) STATIC_CLASS_INFO(_T_);
//#include <clasp/core/initClasses.h>
#undef _CLASS_MACRO
#undef EXPAND_CLASS_MACROS
