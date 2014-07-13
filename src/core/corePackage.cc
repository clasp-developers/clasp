#define DEBUG_LEVEL_FULL
#include <limits.h>
#include <float.h>
#include <stdio.h>
#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "symbolTable.h"
#include "str.h"
#include "lispStream.h"
#include "core/corePackage.h"
#include "commonLispPackage.h"
#include "keywordPackage.h"
#include "extensionPackage.h"
#include "package.h"
#include "compPackage.h"
#include "grayPackage.h"
#include "closPackage.h"
#include "hashTable.h"
#include "posixTime.h"
#include "ql.h"
#include "readtable.h"
#include "commonLispUserPackage.h"
#include "lispReader.h"
#include "numerics.h"
#include "bootStrapCoreSymbolMap.h"

// ------------------- include all headers for corePackage here

#define HEADER_INCLUDES
#include "core_initClasses_inc.h"

//
// Load the gctools::GcInfo<core-classes>::Kind specializers
//
#define NAMESPACE_core
#include "main/gc_interface.h"
#undef NAMESPACE_core


namespace core
{
#define EXPOSE_TO_CANDO
#define Use_CorePkg
#define Use_ClPkg
#define Use_ExtPkg
#define EXTERN_REGISTER
#include "core_initClasses_inc.h"
#undef EXTERN_REGISTER
#undef Use_ExtPkg
#undef Use_ClPkg
#undef Use_CorePkg
#undef EXPOSE_TO_CANDO
};



namespace core
{
    const char* CorePkg_nicknames[] = { "SYSTEM", "sys", "SYS", "si", "SI", "" /*guard*/ };

    SYMBOL_EXPORT_SC_(ClPkg,callNextMethod);
    SYMBOL_EXPORT_SC_(ClPkg,nextMethodP);
    SYMBOL_EXPORT_SC_(ExtPkg,truly_the);
    SYMBOL_EXPORT_SC_(ExtPkg,specialVar);
    SYMBOL_EXPORT_SC_(ExtPkg,lexicalVar);
    SYMBOL_EXPORT_SC_(CorePkg,STARdebugLoadTimeValuesSTAR);
    SYMBOL_EXPORT_SC_(CorePkg,STARdebugInterpretedFunctionsSTAR);
SYMBOL_EXPORT_SC_(KeywordPkg,FullDebug);
SYMBOL_EXPORT_SC_(KeywordPkg,LineTablesOnly);

    SYMBOL_SC_(CorePkg,STARdebugMonitorSTAR);
    SYMBOL_SC_(CorePkg,monitorReader);
    SYMBOL_EXPORT_SC_(CorePkg,STARsourceDatabaseSTAR);
    SYMBOL_EXPORT_SC_(CorePkg,STARstartRunTimeSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,internalTimeUnitsPerSecond);
    SYMBOL_EXPORT_SC_(ClPkg,getInternalRealTime);
    SYMBOL_EXPORT_SC_(ClPkg,getInternalRunTime);
    SYMBOL_EXPORT_SC_(CorePkg,STARcommandLineLoadSTAR);
    SYMBOL_EXPORT_SC_(CorePkg,STARserializerArchiveSTAR);
    SYMBOL_EXPORT_SC_(KeywordPkg,_uid);
    SYMBOL_EXPORT_SC_(KeywordPkg,function);
    SYMBOL_EXPORT_SC_(CorePkg,STARihsCurrentSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,logicalPathnameTranslations);
    SYMBOL_EXPORT_SC_(ClPkg,set);
    SYMBOL_EXPORT_SC_(ClPkg,restartName);
    SYMBOL_EXPORT_SC_(ClPkg,position);
    SYMBOL_EXPORT_SC_(ClPkg,compileFile);
    SYMBOL_EXPORT_SC_(ClPkg,first);
    SYMBOL_EXPORT_SC_(ClPkg,float);
    SYMBOL_EXPORT_SC_(CorePkg,STARllvmFunctionNameHookSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,pathnamep);
    SYMBOL_EXPORT_SC_(CorePkg,STARtopLevelCommandHookSTAR);
    SYMBOL_EXPORT_SC_(CorePkg,STARloadSearchListSTAR);
    SYMBOL_EXPORT_SC_(CorePkg,loadBitcode);
    SYMBOL_EXPORT_SC_(CorePkg,loadSource);
    SYMBOL_EXPORT_SC_(CorePkg,loadBundle);
    SYMBOL_EXPORT_SC_(ClPkg,STARloadPathnameSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARloadTruenameSTAR);
    SYMBOL_EXPORT_SC_(KeywordPkg,none);
    SYMBOL_EXPORT_SC_(KeywordPkg,line);
    SYMBOL_EXPORT_SC_(KeywordPkg,full);
    SYMBOL_EXPORT_SC_(KeywordPkg,line_buffered);
    SYMBOL_EXPORT_SC_(KeywordPkg,fully_buffered);

    SYMBOL_EXPORT_SC_(ClPkg,array);
    SYMBOL_EXPORT_SC_(KeywordPkg,array);
    SYMBOL_EXPORT_SC_(ClPkg,makeArray);
    SYMBOL_EXPORT_SC_(CorePkg,STARallCxxClassesSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,mismatch);
    SYMBOL_EXPORT_SC_(ClPkg,SignedByte);
    SYMBOL_EXPORT_SC_(ClPkg,UnsignedByte);
    SYMBOL_EXPORT_SC_(KeywordPkg,UnsignedByte);
    SYMBOL_EXPORT_SC_(ClPkg,Bit);
    SYMBOL_EXPORT_SC_(ClPkg,parseNamestring);
SYMBOL_EXPORT_SC_(KeywordPkg,start);
SYMBOL_EXPORT_SC_(KeywordPkg,end);
    SYMBOL_EXPORT_SC_(ClPkg,or);
    SYMBOL_EXPORT_SC_(KeywordPkg,test);
    SYMBOL_EXPORT_SC_(KeywordPkg,junkAllowed);
    SYMBOL_EXPORT_SC_(ClPkg,STARdefaultPathnameDefaultsSTAR);
    SYMBOL_EXPORT_SC_(KeywordPkg,absolute);
    SYMBOL_EXPORT_SC_(KeywordPkg,relative);
    SYMBOL_EXPORT_SC_(KeywordPkg,back);
    SYMBOL_EXPORT_SC_(CorePkg,simpleProgramError);
    SYMBOL_EXPORT_SC_(ClPkg,simpleTypeError);
    SYMBOL_EXPORT_SC_(ClPkg,MultipleValuesLimit);
    SYMBOL_EXPORT_SC_(CorePkg,STARdebugReaderSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,keywordp);
    SYMBOL_EXPORT_SC_(ClPkg,null);
    SYMBOL_EXPORT_SC_(ClPkg,endp);
    SYMBOL_EXPORT_SC_(ClPkg,symbolp);
    SYMBOL_EXPORT_SC_(ClPkg,atom);
    SYMBOL_EXPORT_SC_(ClPkg,consp);
    SYMBOL_EXPORT_SC_(ClPkg,listp);
    SYMBOL_EXPORT_SC_(ClPkg,numberp);
    SYMBOL_EXPORT_SC_(ClPkg,integerp);
    SYMBOL_EXPORT_SC_(ClPkg,rationalp);
    SYMBOL_EXPORT_SC_(ClPkg,floatp);
    SYMBOL_EXPORT_SC_(ClPkg,realp);
    SYMBOL_EXPORT_SC_(ClPkg,complexp);
    SYMBOL_EXPORT_SC_(ClPkg,characterp);
    SYMBOL_EXPORT_SC_(ClPkg,stringp);
    SYMBOL_EXPORT_SC_(ClPkg,bit_vector_p);
    SYMBOL_EXPORT_SC_(ClPkg,vectorp);
    SYMBOL_EXPORT_SC_(ClPkg,simple_vector_p);
    SYMBOL_EXPORT_SC_(ClPkg,simple_string_p);
    SYMBOL_EXPORT_SC_(ClPkg,simple_bit_vector_p);
    SYMBOL_EXPORT_SC_(ClPkg,arrayp);
    SYMBOL_EXPORT_SC_(ClPkg,packagep);
    SYMBOL_EXPORT_SC_(ClPkg,functionp);
    SYMBOL_EXPORT_SC_(ClPkg,compiled_function_p);
    SYMBOL_EXPORT_SC_(ClPkg,hash_table_p);

    SYMBOL_EXPORT_SC_(CorePkg,STARenablePrintPrettySTAR);
    SYMBOL_EXPORT_SC_(CorePkg,STARcircle_counterSTAR);
    SYMBOL_EXPORT_SC_(CorePkg,STARcircle_stackSTAR);
    SYMBOL_EXPORT_SC_(CorePkg,dynamicGo);
    SYMBOL_EXPORT_SC_(CorePkg,localGo);
    SYMBOL_EXPORT_SC_(CorePkg,functionName);
    SYMBOL_EXPORT_SC_(ClPkg,_DIVIDE_);
    SYMBOL_EXPORT_SC_(KeywordPkg,operation);
    SYMBOL_EXPORT_SC_(KeywordPkg,operands);
    SYMBOL_EXPORT_SC_(CorePkg,_PLUS_ecl_syntax_progv_list_PLUS_);
    SYMBOL_EXPORT_SC_(CorePkg,_PLUS_io_syntax_progv_list_PLUS_);
    SYMBOL_SC_(CorePkg,STARprintPackageSTAR);
    SYMBOL_SC_(CorePkg,STARprintStructureSTAR);
    SYMBOL_SC_(CorePkg,STARsharpEqContextSTAR);
    SYMBOL_SC_(CorePkg,STARcircleCounterSTAR);

    SYMBOL_EXPORT_SC_(ClPkg,typep);
    SYMBOL_EXPORT_SC_(ClPkg,type);
    SYMBOL_EXPORT_SC_(ClPkg,step);
    SYMBOL_EXPORT_SC_(ClPkg,speed);
    SYMBOL_EXPORT_SC_(ClPkg,space);
    SYMBOL_EXPORT_SC_(ClPkg,sin);
    SYMBOL_EXPORT_SC_(ClPkg,safety);
    SYMBOL_EXPORT_SC_(ClPkg,restart_bind);
    SYMBOL_EXPORT_SC_(ClPkg,restart);
    SYMBOL_EXPORT_SC_(ClPkg,describe);

    SYMBOL_EXPORT_SC_(ClPkg,disassemble);
    SYMBOL_EXPORT_SC_(ClPkg,rename_file);
    SYMBOL_EXPORT_SC_(ClPkg,random);
    SYMBOL_EXPORT_SC_(ClPkg,optimize);
    SYMBOL_EXPORT_SC_(ClPkg,make_two_way_stream);
    SYMBOL_EXPORT_SC_(ClPkg,make_synonym_stream);
    SYMBOL_EXPORT_SC_(ClPkg,invoke_restart);
    SYMBOL_EXPORT_SC_(ClPkg,get);
    SYMBOL_EXPORT_SC_(ClPkg,find_restart);
    SYMBOL_EXPORT_SC_(ClPkg,fill_pointer);
    SYMBOL_EXPORT_SC_(ClPkg,directory);
    SYMBOL_EXPORT_SC_(ClPkg,defvar);
    SYMBOL_EXPORT_SC_(ClPkg,defun);
    SYMBOL_EXPORT_SC_(ClPkg,defparameter);
    SYMBOL_EXPORT_SC_(ClPkg,defconstant);
    SYMBOL_EXPORT_SC_(ClPkg,debug);
    SYMBOL_EXPORT_SC_(ClPkg,count);
    SYMBOL_EXPORT_SC_(ClPkg,compute_restarts);
    SYMBOL_EXPORT_SC_(ClPkg,char);


    SYMBOL_EXPORT_SC_(KeywordPkg,escape);
    SYMBOL_EXPORT_SC_(ClPkg,write);
    SYMBOL_SC_(KeywordPkg,capitalize);
    SYMBOL_EXPORT_SC_(ClPkg,STARreadDefaultFloatFormatSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARprint_escapeSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARprint_baseSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARprint_levelSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARprint_lengthSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARprint_radixSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARprint_caseSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARprint_gensymSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARprint_arraySTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARprint_readablySTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARprint_escapeSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARprint_circleSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARprint_linesSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARprint_right_marginSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARprint_prettySTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARprint_miser_widthSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARprint_pprint_dispatchSTAR);

    
    SYMBOL_EXPORT_SC_(CorePkg,signalSimpleError);
    SYMBOL_SC_(CorePkg,invokeInternalDebugger);
    SYMBOL_EXPORT_SC_(ClPkg,STARdebuggerHookSTAR);

    SYMBOL_EXPORT_SC_(ClPkg,break);
    SYMBOL_EXPORT_SC_(ClPkg,STARbreakOnSignalsSTAR);
    SYMBOL_SC_(CorePkg,STARnestedErrorDepthSTAR);
    SYMBOL_SC_(CorePkg,universalErrorHandler);
    SYMBOL_SC_(KeywordPkg,typeError);
    SYMBOL_SC_(KeywordPkg,datum);
    SYMBOL_SC_(KeywordPkg,expectedType);
    SYMBOL_EXPORT_SC_(ClPkg,typeError);
    SYMBOL_EXPORT_SC_(ClPkg,printObject);
    SYMBOL_EXPORT_SC_(ClPkg,makeCondition);
    SYMBOL_EXPORT_SC_(ClPkg,controlError);
    SYMBOL_SC_(KeywordPkg,print);
    SYMBOL_SC_(KeywordPkg,pathname);
    SYMBOL_SC_(CorePkg,setThrowPosition);
    SYMBOL_EXPORT_SC_(CorePkg,tooFewArgumentsError);
    SYMBOL_EXPORT_SC_(CorePkg,tooManyArgumentsError);
    SYMBOL_EXPORT_SC_(ClPkg,readerError);
    SYMBOL_EXPORT_SC_(ClPkg,streamError);
    SYMBOL_EXPORT_SC_(ClPkg,endOfFile);
    SYMBOL_EXPORT_SC_(ClPkg,packageError);
    SYMBOL_EXPORT_SC_(ClPkg,parseError);
    SYMBOL_EXPORT_SC_(ClPkg,printNotReadable);
    SYMBOL_EXPORT_SC_(ClPkg,printNotReadableObject);
    SYMBOL_EXPORT_SC_(KeywordPkg,object);
    SYMBOL_EXPORT_SC_(ClPkg,cellError);
    SYMBOL_EXPORT_SC_(ClPkg,simpleError);
    SYMBOL_SC_(KeywordPkg,formatControl);
    SYMBOL_SC_(KeywordPkg,formatArguments);
    SYMBOL_SC_(KeywordPkg,name);
    SYMBOL_SC_(KeywordPkg,stream);
    SYMBOL_SC_(KeywordPkg,package);
    SYMBOL_SC_(CorePkg,unrecognizedKeywordArgumentError);
    SYMBOL_SC_(CorePkg,invalidKeywordArgumentError);
    SYMBOL_EXPORT_SC_(ClPkg,fileError);
    SYMBOL_SC_(CorePkg,_PLUS_llvmTargetTriple_PLUS_);
    SYMBOL_SC_(CorePkg,_PLUS_executableName_PLUS_);
    SYMBOL_EXPORT_SC_(CorePkg,STARcodeWalkerSTAR);
    SYMBOL_EXPORT_SC_(CorePkg,STARsourcePathNameSTAR);
    SYMBOL_EXPORT_SC_(CorePkg,STARcurrentLineNumberSTAR);
    SYMBOL_EXPORT_SC_(CorePkg,STARcurrentColumnSTAR);
    SYMBOL_SC_(CorePkg,STARdebugMacroexpandSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,T);
    SYMBOL_EXPORT_SC_(ClPkg,method);
    SYMBOL_EXPORT_SC_(ClPkg,generic_function);
    SYMBOL_SC_(CorePkg,STARenvironmentPrintingTabSTAR);
    SYMBOL_SC_(CorePkg,STARenvironmentPrintingTabIncrementSTAR);
    SYMBOL_SC_(CorePkg,_PLUS_activationFrameNil_PLUS_);
    SYMBOL_EXPORT_SC_(ClPkg,cond);
    SYMBOL_SC_(CompPkg,parse_macro);
    SYMBOL_SC_(CorePkg,globalFunction);
    SYMBOL_SC_(CorePkg,globalSetfFunction);
    SYMBOL_SC_(CorePkg,lexicalFunction);
    SYMBOL_SC_(CorePkg,declaredSpecial);
    SYMBOL_SC_(CorePkg,lexical);
    SYMBOL_EXPORT_SC_(ClPkg,stream);
    SYMBOL_EXPORT_SC_(ClPkg,pathname);
    SYMBOL_EXPORT_SC_(ClPkg,boolean);
    SYMBOL_EXPORT_SC_(ClPkg,keyword);
    SYMBOL_EXPORT_SC_(ClPkg,array);
    SYMBOL_EXPORT_SC_(ClPkg,simple_array);
    SYMBOL_EXPORT_SC_(ClPkg,base_char);
    SYMBOL_EXPORT_SC_(ClPkg,vector);
    SYMBOL_EXPORT_SC_(ClPkg,simple_vector);

    SYMBOL_SC_(CorePkg,STARsystem_defsetf_update_functionsSTAR);
    SYMBOL_EXPORT_SC_(ExtPkg,_PLUS_processStandardInput_PLUS_);
    SYMBOL_EXPORT_SC_(ExtPkg,_PLUS_processStandardOutput_PLUS_);
    SYMBOL_EXPORT_SC_(ExtPkg,_PLUS_processErrorOutput_PLUS_);
    SYMBOL_EXPORT_SC_(ClPkg,STARstandard_inputSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARstandard_outputSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARerror_outputSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARtrace_outputSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARdebug_ioSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARquery_ioSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARterminal_ioSTAR);

    SYMBOL_EXPORT_SC_(ClPkg,STARgensym_counterSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,standard_char);
    SYMBOL_EXPORT_SC_(ClPkg,special);
    SYMBOL_EXPORT_SC_(ClPkg,nconc);
    SYMBOL_EXPORT_SC_(ClPkg,cons);
    SYMBOL_EXPORT_SC_(ClPkg,cadr);
    SYMBOL_SC_(CorePkg,STARbackquote_expand_hookSTAR);
    SYMBOL_SC_(CorePkg,single_dispatch_on);
    SYMBOL_EXPORT_SC_(ClPkg,STARmacroexpand_hookSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARread_baseSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,compile);
    SYMBOL_EXPORT_SC_(ClPkg,load);
    SYMBOL_EXPORT_SC_(ClPkg,eval);
    SYMBOL_SC_(KeywordPkg,compile_toplevel);
    SYMBOL_SC_(KeywordPkg,load_toplevel);
    SYMBOL_SC_(KeywordPkg,execute);

    SYMBOL_EXPORT_SC_(ClPkg,array_dimension_limit);

    SYMBOL_EXPORT_SC_(ClPkg,STARread_evalSTAR);
    SYMBOL_SC_(CorePkg,STARdocumentation_poolSTAR);
    SYMBOL_SC_(CorePkg,STARexecutable_nameSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,define_modify_macro);
    SYMBOL_EXPORT_SC_(ClPkg,destructuring_bind);
    SYMBOL_EXPORT_SC_(ClPkg,deftype);
    SYMBOL_EXPORT_SC_(ClPkg,define_method_combination);
    SYMBOL_SC_(CorePkg,generic);
    SYMBOL_EXPORT_SC_(ClPkg,defsetf);
    SYMBOL_SC_(KeywordPkg,allow_other_keys);
    SYMBOL_SC_(CorePkg,DOT);
    SYMBOL_EXPORT_SC_(ClPkg,AMPwhole);
    SYMBOL_EXPORT_SC_(ClPkg,AMPenvironment);
    SYMBOL_EXPORT_SC_(ClPkg,AMPoptional);
    SYMBOL_EXPORT_SC_(ClPkg,AMPkey);
    SYMBOL_EXPORT_SC_(ClPkg,AMPallow_other_keys);
    SYMBOL_EXPORT_SC_(ClPkg,AMPaux);
    SYMBOL_EXPORT_SC_(ClPkg,AMPrest);
    SYMBOL_EXPORT_SC_(ClPkg,AMPbody);
    SYMBOL_EXPORT_SC_(ClPkg,integer);
    SYMBOL_EXPORT_SC_(ClPkg,sequence);
    SYMBOL_SC_(CorePkg,anonymous);
    SYMBOL_EXPORT_SC_(ClPkg,declare);
    SYMBOL_SC_(KeywordPkg,macro);
    SYMBOL_SC_(KeywordPkg,function);
    SYMBOL_SC_(CorePkg,macro);
    SYMBOL_EXPORT_SC_(ClPkg,function);
    SYMBOL_EXPORT_SC_(ClPkg,variable);
    SYMBOL_SC_(CorePkg,STARdocumentation_databaseSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,bit);
    SYMBOL_SC_(CorePkg,__init__);
    SYMBOL_EXPORT_SC_(ClPkg,STARreadtableSTAR);
    SYMBOL_SC_(CorePkg,input_stream_designator);
    /*! Set to true if you want SourceCodeCons to print line no info */
    SYMBOL_SC_(CorePkg,STARprint_source_code_consSTAR);
    /*! Set to true if you want the repl to print what was read */
    SYMBOL_SC_(CorePkg,STARechoReplReadSTAR);
    SYMBOL_SC_(KeywordPkg,invalid_character);
    SYMBOL_SC_(KeywordPkg,brcl);
    SYMBOL_SC_(KeywordPkg,not);
    SYMBOL_SC_(KeywordPkg,and);
    SYMBOL_SC_(KeywordPkg,or);
    SYMBOL_EXPORT_SC_(ClPkg,and);
    SYMBOL_EXPORT_SC_(ClPkg,or);
    SYMBOL_EXPORT_SC_(ClPkg,car);
    SYMBOL_EXPORT_SC_(ClPkg,cdr);
    SYMBOL_EXPORT_SC_(ClPkg,dotimes);
    SYMBOL_EXPORT_SC_(ClPkg,dolist);
    SYMBOL_EXPORT_SC_(ClPkg,do);
    SYMBOL_EXPORT_SC_(ClPkg,package);
    SYMBOL_EXPORT_SC_(ClPkg,string);
    SYMBOL_SC_(CorePkg,eof_error_p);
    SYMBOL_SC_(CorePkg,eof_value);
    SYMBOL_SC_(CorePkg,start);
    SYMBOL_SC_(CorePkg,end);
    SYMBOL_SC_(CorePkg,preserve_whitespace);
    SYMBOL_EXPORT_SC_(ClPkg,aref);
    SYMBOL_EXPORT_SC_(ClPkg,nth);
    SYMBOL_SC_(CorePkg,io);
    SYMBOL_SC_(CorePkg,probe);
    SYMBOL_EXPORT_SC_(ClPkg,error);
    SYMBOL_SC_(CorePkg,newVersion);
    SYMBOL_SC_(CorePkg,renameAndDelete);
    SYMBOL_SC_(CorePkg,overwrite);
    SYMBOL_EXPORT_SC_(ClPkg,append);
    SYMBOL_SC_(CorePkg,supersede);
    SYMBOL_SC_(CorePkg,create);
    SYMBOL_SC_(CorePkg,input_stream);
    SYMBOL_SC_(CorePkg,recursive_p);
    SYMBOL_SC_(CorePkg,dimensions);
    SYMBOL_SC_(CorePkg,element_type);
    SYMBOL_SC_(CorePkg,initial_element);
    SYMBOL_SC_(CorePkg,adjustable);

    SYMBOL_EXPORT_SC_(ClPkg,gethash);
    SYMBOL_SC_(CorePkg,object);
    SYMBOL_EXPORT_SC_(ClPkg,eq);
    SYMBOL_EXPORT_SC_(ClPkg,eql);
    SYMBOL_EXPORT_SC_(ClPkg,equal);
    SYMBOL_EXPORT_SC_(ClPkg,equalp);
    SYMBOL_SC_(CorePkg,okey);
    SYMBOL_EXPORT_SC_(ClPkg,hash_table);
    SYMBOL_SC_(CorePkg,default);

    SYMBOL_SC_(KeywordPkg,class);
    SYMBOL_SC_(KeywordPkg,instance);

    SYMBOL_SC_(KeywordPkg,output);
    SYMBOL_SC_(KeywordPkg,input);
    SYMBOL_SC_(KeywordPkg,io);
    SYMBOL_SC_(KeywordPkg,default);
    SYMBOL_SC_(KeywordPkg,internal);
    SYMBOL_SC_(KeywordPkg,external);
    SYMBOL_SC_(KeywordPkg,inherited);
    SYMBOL_SC_(KeywordPkg,changed);
    SYMBOL_SC_(CorePkg,dot);
    SYMBOL_EXPORT_SC_(ClPkg,STARfeaturesSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARload_printSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARload_verboseSTAR);
    SYMBOL_SC_(CorePkg,ifDoesNotExist);
    SYMBOL_SC_(KeywordPkg,debug);
    SYMBOL_SC_(KeywordPkg,direct_super_classes);
    SYMBOL_EXPORT_SC_(ClPkg,lambda);
    SYMBOL_EXPORT_SC_(ExtPkg,lambda_block);
    SYMBOL_SC_(CorePkg,lambda_with_handler);
    SYMBOL_EXPORT_SC_(ClPkg,symbol);
    SYMBOL_SC_(CorePkg,color);
    SYMBOL_SC_(CorePkg,foreach);
    SYMBOL_SC_(CorePkg,STARPATHSTAR);
    SYMBOL_SC_(CorePkg,STARARGSSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARpackageSTAR);
    SYMBOL_SC_(CorePkg,STARcurrent_working_directorySTAR);
    SYMBOL_EXPORT_SC_(ClPkg,STARmodulesSTAR);
    SYMBOL_EXPORT_SC_(ClPkg,progn);
    SYMBOL_SC_(CorePkg,backquote);
    SYMBOL_SC_(CorePkg,double_backquote);
    SYMBOL_SC_(CorePkg,unquote); 
    // was S Y M B O L _SC_(CorePkg,comma);
    SYMBOL_SC_(CorePkg,unquote_splice);
    // was S Y M B O L _SC_(CorePkg,comma_atsign);
    SYMBOL_SC_(CorePkg,unquote_nsplice);
    // was S Y M B O L _SC_(CorePkg,comma_dot);
    SYMBOL_EXPORT_SC_(ClPkg,quote);
    SYMBOL_EXPORT_SC_(ClPkg,function);
    SYMBOL_SC_(CorePkg,slot);
    SYMBOL_EXPORT_SC_(ClPkg,slot_value);
    SYMBOL_EXPORT_SC_(ClPkg,values);
    SYMBOL_SC_(CorePkg,item);
    SYMBOL_SC_(CorePkg,alist);
    SYMBOL_EXPORT_SC_(ClPkg,list);
    SYMBOL_SC_(CorePkg,key);
    SYMBOL_SC_(CorePkg,test_not);

    SYMBOL_SC_(KeywordPkg,name);
    SYMBOL_SC_(CorePkg,forward_referenced_class);
    SYMBOL_EXPORT_SC_(ClPkg,standard_class);
    SYMBOL_EXPORT_SC_(ClPkg,rest);

    SYMBOL_SC_(CorePkg,instance);
    SYMBOL_SC_(CorePkg,all_keys);

    SYMBOL_SC_(KeywordPkg,changed);



#pragma GCC visibility push(default)
#define CorePkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkgName,lispName,export) Symbol_sp cname = UNDEFINED_SYMBOL;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef CorePkg_SYMBOLS
#pragma GCC visibility pop


#pragma GCC visibility push(default)

#pragma GCC visibility pop


    CoreExposer::CoreExposer(Lisp_sp lisp) : Exposer(lisp,CorePkg,CorePkg_nicknames)
    {
	this->package()->usePackage(_lisp->findPackage("CL"));
    };



    void CoreExposer::expose(core::Lisp_sp lisp, WhatToExpose what) const
    {_G();
	switch (what)
	{
	case candoClasses:
#define EXPOSE_TO_CANDO
#define Use_CorePkg
#define Use_ClPkg
#define Use_ExtPkg
#define INVOKE_REGISTER
#include "core_initClasses_inc.h"
#undef INVOKE_REGISTER
#undef Use_ExtPkg
#undef Use_ClPkg
#undef Use_CorePkg
	    break;
	case candoFunctions:
	    exposeCando_Numerics();
	    exposeCore_lisp_reader();
	    {
		ReadTable_sp readtable = ReadTable_O::create_standard_readtable(_lisp);
		cl::_sym_STARreadtableSTAR->defparameter(readtable);
	    }
	    break;
	case candoGlobals:
	{
	    // expose the CorePkg constants here
//----------- symbols are created in lisp.cc::startupLispEnvironment ----------
//#define SYMBOLS_CREATE
//#i n c l u d e "symbols_scraped_inc.h"
//-----------------------------------------------------------------------------
	}
	
	    break;
	case pythonClasses:
	{
	    IMPLEMENT_MEF(BF("Handle other packages"));
#define _DBG(x)
#define EXPOSE_TO_PYTHON
#define Use_CorePkg
#include "core_initClasses_inc.h"
#undef Use_CorePkg
#undef EXPOSE_TO_PYTHON
#undef _DBG
	}
	    break;
	case pythonFunctions:
#ifdef USEBOOSTPYTHON	    
	    exposePython_Numerics();
#endif
	    break;
	case pythonGlobals:
	    // expose globals here
	    break;
	}
    }






    CoreExposer* CoreExposer::create_core_packages_and_classes()
    { _G();
	LOG(BF("Initialize core classes by hand"));
	BootStrapCoreSymbolMap bootStrapSymbolMap;
	Symbol_sp startSymbol;
	// initialize classSymbol's only if they have not been done before
	if ( IS_SYMBOL_UNDEFINED(T_O::static_classSymbol()) )
	{ _BLOCK_TRACE("Setting static classSymbols for core classes");

	    //
	    // The following will set the static_ClassSymbol to the Symbol created
	    // by BootStrapCoreSymbolMap for each core class
	    //
#define SET_SYMBOL
	    {_BLOCK_TRACEF(BF("LOOKUP Symbol"));
#undef LOOKUP_SYMBOL
#define LOOKUP_SYMBOL(pkgName,symName) bootStrapSymbolMap.lookupSymbol(pkgName,symName)
#include "core_initClasses_inc.h"
#undef LOOKUP_SYMBOL
	    }
	} else
	{
	    THROW_HARD_ERROR(BF("You cannot initializes classes twice"));
	}
    //
    // The following will create each class as an instance of
    // BuiltInClass or whatever was specified as its LISP_METACLASS,
    // it will also set its _WeakThis and _WeakLisp pointers
    // and set its InstanceClassSymbol variable
    // It will also put the Class instance into the Lisp class table

#define CREATE_CLASS
#include "core_initClasses_inc.h"
	// Put core::Null_O::___staticClass into a global variable so that every
	// class can access it from the _class() virtual function
	// See object.h definition of Null_O___staticClass
//	Null_O___staticClass = core::Null_O::___staticClass;
	{_BLOCK_TRACEF(BF("Dump info on classes"));
#define DUMP_INFO_CLASS
#include "core_initClasses_inc.h"
	}


	Package_sp commonLispPackage = cl::initialize_commonLispPackage();
	Package_sp keywordPackage = kw::initialize_keywordPackage();
	ext::initialize_extensionPackage();
	comp::initialize_compPackage();
	clos::initialize_closPackage();
	gray::initialize_grayPackage();
	cluser::initialize_commonLispUserPackage();


//	ASSERT_lt(classesHandInitialized, get_nextGlobalClassSymbol())
	//
	// Define the base class for every hand initialized class
	//
//	classObject->_DirectSuperClasses.clear();
	{_BLOCK_TRACEF(BF("Define base classes"));
#define DEFINE_BASE_CLASSES
#include "core_initClasses_inc.h"
	}
        CoreExposer* coreExposerPtr = gctools::ClassAllocator<CoreExposer>::allocateClass(_lisp);
	Package_sp corePackage = coreExposerPtr->package();
	_lisp->_Roots._CorePackage = corePackage;
	_lisp->_Roots._KeywordPackage = keywordPackage;
//	_lisp->_PackageNameIndexMap[KeywordPkg] = _lisp->_Roots._Packages.size();
	_lisp->_Roots._CommonLispPackage = commonLispPackage;
	commonLispPackage->usePackage(corePackage);
	{_BLOCK_TRACEF(BF("Setup instance base classes for T_O"));
	    T_O::___staticClass->setInstanceBaseClasses(_Nil<Cons_O>());
	}
	{_BLOCK_TRACEF(BF("Define class names"));
#define DEFINE_CLASS_NAMES
	string NSPkg = CorePkg;
#include "core_initClasses_inc.h"
	};


#if 0
        // 
        // Test hashtables
        //
        printf("%s:%d Testing hashtables\n", __FILE__, __LINE__ );
        HashTableEq_sp ht = HashTableEq_O::create(16,Fixnum_O::create(256),1.5);
        printf("Empty ht = %s\n", ht->hash_table_dump().c_str() );
        ht->hash_table_setf_gethash(cl::_sym_first, Fixnum_O::create(1));
        ht->hash_table_setf_gethash(cl::_sym_car, _Nil<T_O>());
        ht->hash_table_setf_gethash(cl::_sym_method, Fixnum_O::create(2));
        printf("after add ht = \n");
        printf("%s\n", ht->hash_table_dump().c_str() );
        T_sp find = ht->gethash(cl::_sym_first,_Nil<T_O>());
        printf(" find = @%p\n", find.px_ref());
        printf("Testing done\n");
        __builtin_trap();
#endif
	//
	// Finish setting up the symbols
	//
	bootStrapSymbolMap.finish_setup_of_symbols();

	return coreExposerPtr;
    }



    void CoreExposer::define_essential_globals(Lisp_sp lisp)
    {_G();


	{_BLOCK_TRACEF(BF("Exporting symbols in lisp"));
#define CorePkg_EXPORT
#define DO_SYMBOL(cname,idx,pkgName,lispName,export) cname->exportYourself(export);
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef CorePkg_EXPORT
	};
	/* Set the values of some essential global symbols */
#if 0
	_sym_nil->initialize();
	_sym_nil->_Name = "NIL";
	_lisp->_CorePackage->_add_symbol_to_package(_sym_nil);
	_sym_nil->_WeakPackage = _lisp->_CorePackage;
	_sym_nil->setf_symbolValue(_Nil<T_O>());
	_sym_nil->makeSpecial();
	_sym_nil->exportYourself();
	_sym_t = cl::_sym_T_O;
#endif
        _lisp->commonLispPackage()->add_symbol_to_package("NIL",_Nil<Symbol_O>(),true);
	_lisp->_Roots._TrueObject = cl::_sym_T_O;
	cl::_sym_T_O->exportYourself()->defparameter(_lisp->_Roots._TrueObject);
	cl::_sym_STARload_printSTAR->exportYourself()->defparameter(_lisp->_false());
	cl::_sym_STARload_verboseSTAR->exportYourself()->defparameter(_lisp->_false());
	cl::_sym_STARread_suppressSTAR->exportYourself()->defparameter(_lisp->_false());
	cl::_sym_STARpackageSTAR->exportYourself()->defparameter(lisp->_Roots._CorePackage);
	_sym_STARpreserve_whitespace_pSTAR->defparameter(_lisp->_false());
	_sym_STARechoReplReadSTAR->exportYourself()->defparameter(_lisp->_false());
	_sym_STARprint_source_code_consSTAR->exportYourself()->defparameter(_lisp->_false());
	_sym_STARbackquote_levelSTAR->defparameter(Fixnum_O::create(0));
	cl::_sym_STARmodulesSTAR->defparameter(_Nil<T_O>());
	_sym_STARexecutable_nameSTAR->defparameter(Str_O::create(_lisp->_FunctionName));
	cl::_sym_STARread_evalSTAR->defparameter(_lisp->_true());
	_sym_STARenvironmentPrintingTabSTAR->defparameter(Fixnum_O::create(0));
	_sym_STARenvironmentPrintingTabIncrementSTAR->defparameter(Fixnum_O::create(6));
	cl::_sym_array_dimension_limit->defconstant(Fixnum_O::create(65535));


	SYMBOL_EXPORT_SC_(ClPkg,most_negative_double_float);
	cl::_sym_most_negative_double_float->defconstant(DoubleFloat_O::create(DBL_MIN));
	SYMBOL_EXPORT_SC_(ClPkg,most_negative_fixnum);
	cl::_sym_most_negative_fixnum->defconstant(Fixnum_O::create(MOST_NEGATIVE_FIXNUM));
	SYMBOL_EXPORT_SC_(ClPkg,most_negative_long_float);
	cl::_sym_most_negative_long_float->defconstant(DoubleFloat_O::create(DBL_MIN));
	SYMBOL_EXPORT_SC_(ClPkg,most_negative_short_float);
	cl::_sym_most_negative_short_float->defconstant(DoubleFloat_O::create(DBL_MIN));
	SYMBOL_EXPORT_SC_(ClPkg,most_negative_single_float);
	cl::_sym_most_negative_single_float->defconstant(DoubleFloat_O::create(DBL_MIN));
	SYMBOL_EXPORT_SC_(ClPkg,most_positive_double_float);
	cl::_sym_most_positive_double_float->defconstant(DoubleFloat_O::create(DBL_MAX));

	SYMBOL_EXPORT_SC_(ClPkg,least_negative_normalized_long_float);
	cl::_sym_least_negative_normalized_long_float->defconstant(LongFloat_O::create(-std::numeric_limits<LongFloat>::denorm_min()));

	SYMBOL_EXPORT_SC_(ClPkg,least_positive_normalized_long_float);
	cl::_sym_least_positive_normalized_long_float->defconstant(LongFloat_O::create(std::numeric_limits<LongFloat>::denorm_min()));



	SYMBOL_EXPORT_SC_(ClPkg,most_positive_fixnum);
	cl::_sym_most_positive_fixnum->defconstant(Fixnum_O::create(MOST_POSITIVE_FIXNUM));
	SYMBOL_EXPORT_SC_(ClPkg,most_positive_long_float);
	cl::_sym_most_positive_long_float->defconstant(DoubleFloat_O::create(DBL_MAX));
	SYMBOL_EXPORT_SC_(ClPkg,most_positive_short_float);
	cl::_sym_most_positive_short_float->defconstant(DoubleFloat_O::create(DBL_MAX));
	SYMBOL_EXPORT_SC_(ClPkg,most_positive_single_float);
	cl::_sym_most_positive_single_float->defconstant(DoubleFloat_O::create(DBL_MAX));
	cl::_sym_STARread_baseSTAR->defparameter(Fixnum_O::create(10));
	SYMBOL_SC_(CorePkg,cl_fixnum_bits);
	_sym_cl_fixnum_bits->defconstant(Fixnum_O::create((int)(sizeof(int)/8)));
	SYMBOL_EXPORT_SC_(ClPkg,array_rank_limit);
	cl::_sym_array_rank_limit->defconstant(Fixnum_O::create(8));
	SYMBOL_EXPORT_SC_(ClPkg,char_code_limit);
	cl::_sym_char_code_limit->defconstant(Fixnum_O::create(128));
	cl::_sym_STARgensym_counterSTAR->defparameter(Fixnum_O::create(0));
	cl::_sym_STARdefaultPathnameDefaultsSTAR->defparameter(_Nil<T_O>());
	cl::_sym_STARprint_arraySTAR->defparameter(_Nil<T_O>());
	cl::_sym_STARprint_baseSTAR->defparameter(Fixnum_O::create(10));
	cl::_sym_STARprint_caseSTAR->defparameter(kw::_sym_upcase);
	cl::_sym_STARprint_circleSTAR->defparameter(_Nil<T_O>());
	cl::_sym_STARprint_escapeSTAR->defparameter(_lisp->_true());
	cl::_sym_STARprint_gensymSTAR->defparameter(_lisp->_true());
	cl::_sym_STARprint_lengthSTAR->defparameter(_Nil<T_O>());
	cl::_sym_STARprint_levelSTAR->defparameter(_Nil<T_O>());
	cl::_sym_STARprint_linesSTAR->defparameter(_Nil<T_O>());
	cl::_sym_STARprint_miser_widthSTAR->defparameter(_Nil<T_O>());
	cl::_sym_STARprint_pprint_dispatchSTAR->defparameter(_Nil<T_O>());
	cl::_sym_STARprint_prettySTAR->defparameter(_Nil<T_O>());
	cl::_sym_STARprint_radixSTAR->defparameter(_Nil<T_O>());
	cl::_sym_STARprint_readablySTAR->defparameter(_Nil<T_O>());
	cl::_sym_STARprint_right_marginSTAR->defparameter(_Nil<T_O>());

//        testPointers();

	FDInStream_sp stdin_stream = FDInStream_O::create(stdin,"*STDIN*",false);
	Stream_sp stdout_stream = FDOutStream_O::create(stdout,"*STDOUT*",false);
	Stream_sp stderr_stream = FDOutStream_O::create(stderr,"*STDERR*",false);
	_sym_STARenablePrintPrettySTAR->defparameter(_Nil<T_O>()); // Just for debugging *print-pretty*
        ext::_sym__PLUS_processStandardInput_PLUS_->defparameter(stdin_stream);
        ext::_sym__PLUS_processStandardOutput_PLUS_->defparameter(stdout_stream);
        ext::_sym__PLUS_processErrorOutput_PLUS_->defparameter(stderr_stream);
        _sym_STARsourceDatabaseSTAR->defparameter(_Nil<T_O>());
	cl::_sym_STARstandard_inputSTAR->defparameter(SynonymStream_O::make(ext::_sym__PLUS_processStandardInput_PLUS_));
	cl::_sym_STARstandard_outputSTAR->defparameter(SynonymStream_O::make(ext::_sym__PLUS_processStandardOutput_PLUS_));
	cl::_sym_STARerror_outputSTAR->defparameter(SynonymStream_O::make(ext::_sym__PLUS_processErrorOutput_PLUS_));
	cl::_sym_STARtrace_outputSTAR->defparameter(SynonymStream_O::make(ext::_sym__PLUS_processErrorOutput_PLUS_));
	cl::_sym_STARdebug_ioSTAR->defparameter(TwoWayStream_O::make(stdin_stream,stdout_stream));
	cl::_sym_STARquery_ioSTAR->defparameter(TwoWayStream_O::make(stdin_stream,stdout_stream));
        TwoWayStream_sp terminal = TwoWayStream_O::make(stdin_stream,stdout_stream);
        terminal->setInteractive(true);
	cl::_sym_STARterminal_ioSTAR->defparameter(terminal);
	_sym_STARsystem_defsetf_update_functionsSTAR->defparameter(_Nil<Cons_O>());
	cl::_sym_STARmacroexpand_hookSTAR->defparameter(_sym_macroexpand_default);
#ifndef USE_SHARP_EQUAL_HASH_TABLES
	_sym_STARsharp_equal_alistSTAR->defparameter(_Nil<Cons_O>());
	_sym_STARsharp_sharp_alistSTAR->defparameter(_Nil<Cons_O>());
#else
	_sym_STARsharp_equal_final_tableSTAR->defparameter(HashTable_O::create(cl::_sym_eq));
	_sym_STARsharp_equal_temp_tableSTAR->defparameter(HashTable_O::create(cl::_sym_eq));
	_sym_STARsharp_equal_repl_tableSTAR->defparameter(HashTable_O::create(cl::_sym_eq));
#endif
	_sym__PLUS_activationFrameNil_PLUS_->defconstant(_Nil<ActivationFrame_O>());
	_sym__PLUS_executableName_PLUS_->defconstant(Str_O::create(EXECUTABLE_NAME));
	SYMBOL_SC_(CorePkg,cArgumentsLimit);
	_sym_cArgumentsLimit->defconstant(Fixnum_O::create(Lisp_O::MaxFunctionArguments));
	_sym_STARdebugMacroexpandSTAR->defparameter(_Nil<T_O>());
	_sym_STARclassNameHashTableSTAR->defparameter(HashTable_O::create(cl::_sym_eq));
	_sym_STARsourcePathNameSTAR->defparameter(Str_O::create("no-source-file"));
	_sym_STARcurrentLineNumberSTAR->defparameter(Fixnum_O::create(0));
	_sym_STARcurrentColumnSTAR->defparameter(Fixnum_O::create(0));
	_sym_STARcodeWalkerSTAR->defparameter(_Nil<T_O>());
	_sym_STARsharpEqContextSTAR->defparameter(_Nil<T_O>());
	cl::_sym_STARreadDefaultFloatFormatSTAR->defparameter(cl::_sym_SingleFloat_O);
	_sym_STARnestedErrorDepthSTAR->defparameter(Fixnum_O::create(0));
	cl::_sym_STARbreakOnSignalsSTAR->defparameter(_Nil<T_O>());
	cl::_sym_STARdebuggerHookSTAR->defparameter(_Nil<T_O>());
        cl::_sym_internalTimeUnitsPerSecond->defparameter(Fixnum_O::create(BRCL_INTERNAL_TIME_UNITS_PER_SECOND));
        _sym_STARstartRunTimeSTAR->defparameter(PosixTime_O::createNow());
	cl::_sym_MultipleValuesLimit->defconstant(Fixnum_O::create(MultipleValues::MultipleValuesLimit));
	_sym_STARprintStructureSTAR->defparameter(_Nil<T_O>());
	_sym_STARprintPackageSTAR->defparameter(_Nil<T_O>());
	_sym_STARcircle_counterSTAR->defparameter(_Nil<T_O>());
	_sym_STARcircle_stackSTAR->defparameter(_Nil<T_O>());
	_sym_STARdebugReaderSTAR->defparameter(_Nil<T_O>());
	cl::_sym_STARloadPathnameSTAR->defparameter(_Nil<T_O>());
	cl::_sym_STARloadTruenameSTAR->defparameter(_Nil<T_O>());
	comp::_sym_STARlowLevelTraceSTAR->defparameter(_Nil<core::T_O>());
	comp::_sym_STARlowLevelTracePrintSTAR->defparameter(_Nil<core::T_O>());
	_sym_STARallCxxClassesSTAR->defparameter(_Nil<T_O>());
	_sym_STARtopLevelCommandHookSTAR->defparameter(_Nil<T_O>());
	_sym_STARllvmFunctionNameHookSTAR->defparameter(_Nil<T_O>());
	_sym_STARihsCurrentSTAR->defparameter(_Nil<T_O>());
	_sym_STARserializerArchiveSTAR->defparameter(_Nil<T_O>());
	_sym_STARcommandLineLoadSTAR->defparameter(_Nil<T_O>());
	_sym_STARdebugMonitorSTAR->defparameter(_Nil<T_O>());
        _sym_STARwatchDynamicBindingStackSTAR->defparameter(_Nil<T_O>());
        _sym_STARdebugLoadTimeValuesSTAR->defparameter(_Nil<T_O>());
        _sym_STARdebugInterpretedFunctionsSTAR->defparameter(_Nil<T_O>());
	Cons_sp hooks = Cons_O::createList(
	    Cons_O::create(Str_O::create("l"),_sym_loadSource),
	    Cons_O::create(Str_O::create("L"),_sym_loadSource),
	    Cons_O::create(Str_O::create("lsp"),_sym_loadSource),
	    Cons_O::create(Str_O::create("LSP"),_sym_loadSource),
	    Cons_O::create(Str_O::create("lisp"),_sym_loadSource),
	    Cons_O::create(Str_O::create("LISP"),_sym_loadSource),
	    Cons_O::create(Str_O::create("bc"),_sym_loadBitcode),
	    Cons_O::create(Str_O::create("bundle"),_sym_loadBundle)
	    );
	hooks = Cons_O::create(Cons_O::create(Str_O::create("brclrc"),_sym_loadSource),hooks);
	ext::_sym_STARloadHooksSTAR->defparameter(hooks);
	_sym_STARloadSearchListSTAR->defparameter(_Nil<T_O>());
#if 0

	_sym_STARbq_simplifySTAR->defparameter(_lisp->_true());
	_sym_STARbackquote_expand_hookSTAR->defparameter(_sym_backquote_completely_process->symbolFunction());
#endif


#if 0 //Old system checking
	/*! Set up the features based on _TARGET_OS_xxxx and _ADDRESS_MODEL_ */
#if defined(_TARGET_OS_DARWIN)
	SYMBOL_SC_(KeywordPkg,target_os_darwin);
	Symbol_sp target_os = kw::_sym_target_os_darwin;
#elif defined(_TARGET_OS_LINUX)
	SYMBOL_SC_(KeywordPkg,target_os_linux);
	Symbol_sp target_os = kw::_sym_target_os_linux;
#endif

#endif //End old system checking

#if defined(__x86_64__)

	SYMBOL_SC_(KeywordPkg,address_model_64);
	Symbol_sp address_model = kw::_sym_address_model_64;

#if defined(__APPLE__) && defined(__MACH__)
#include <TargetConditionals.h>

#if TARGET_OS_IPHONE == 1
#error Currently iPhone simulator and iOS are not supported
#elif TARGET_OS_MAC == 1

	SYMBOL_SC_(KeywordPkg,target_os_darwin);
	Symbol_sp target_os = kw::_sym_target_os_darwin;

#else
#error Your TargetConditionals.h file says you are not a Mac or iPhone?????
#endif

#elif defined(__linux__)

	SYMBOL_SC_(KeywordPkg,target_os_linux);
	Symbol_sp target_os = kw::_sym_target_os_linux;

#else
#error Currently only MacOSX and linux are supported for x86_64
#endif

#elif defined(__i386__)

	SYMBOL_SC_(KeywordPkg,address_model_32);
	Symbol_sp address_model = kw::_sym_address_model_32;

#if defined(__linux__)

	SYMBOL_SC_(KeywordPkg,target_os_linux);
	Symbol_sp target_os = kw::_sym_target_os_linux;

#else
#error Currently only linux is supported for i386
#endif

#else
#error Currently only x86_64 and i386 is supported
#endif

	ql::list features(_lisp);
	//
	// The following will fail at compile time if a _TARGET_OS_xxxx wasn't defined 
	// Check src/Jamfile.jam to add definitions for other <target-os> types
	//
	features << target_os;

#if 0 //Old System checking
	/*! Set up the features based on _TARGET_OS_xxxx and _ADDRESS_MODEL_ */
#if defined(_ADDRESS_MODEL_64)
	SYMBOL_SC_(KeywordPkg,address_model_64);
	Symbol_sp address_model = kw::_sym_address_model_64;
#elif defined(_ADDRESS_MODEL_32)
	SYMBOL_SC_(KeywordPkg,address_model_32);
	Symbol_sp address_model = kw::_sym_address_model_32;
#endif

#endif //End old system checking
	//
	// The following will fail at compile time if a _TARGET_OS_xxxx wasn't defined 
	// Check src/Jamfile.jam to add definitions for other <target-os> types
	//
	features << address_model;
	
	// Now add other standard features
	features << kw::_sym_brcl;

	cl::_sym_STARfeaturesSTAR->exportYourself()->defparameter(features.cons());

    }




    void add_defsetf_access_update(Symbol_sp access_fn, Symbol_sp update_fn )
    {_G();
	Cons_sp pair = Cons_O::create(access_fn,update_fn);
	Cons_sp list = _sym_STARsystem_defsetf_update_functionsSTAR->symbolValue().as_or_nil<Cons_O>();
	_sym_STARsystem_defsetf_update_functionsSTAR->defparameter(Cons_O::create(pair,list));
        _sym_STARmonitorRegisterSourceInfoSTAR->defparameter(_Nil<T_O>());
    }

};


#define EXPAND_CLASS_MACROS
#define _CLASS_MACRO(_T_)   STATIC_CLASS_INFO(_T_);			
#include "core_initClasses_inc.h"
#undef _CLASS_MACRO
#undef EXPAND_CLASS_MACROS

#if defined(USE_REFCOUNT)
#define EXPAND_CLASS_MACROS
#define _CLASS_MACRO(_T_)    INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(_T_);
#include "core_initClasses_inc.h"
#undef _CLASS_MACRO
#undef EXPAND_CLASS_MACROS
#endif
