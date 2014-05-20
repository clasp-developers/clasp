
#include <stdint.h>

#include "core/foundation.h"
#include "core/object.h"
#include "core/lisp.h"
#include "core/builtInClass.h"
#include "core/fileSystem.h"
#include "llvmo/llvmoPackage.h"
//#include "llvmoExpose.generated.h"
#include "llvmoExpose.h"
#include "insertPoint.h"
#include "debugLoc.h"
#include "llvmoDwarf.h"
#include "clbindLlvmExpose.h"
#include "debugInfoExpose.h"
#include "intrinsics.h"
#include "core/environment.h"
#include "core/str.h"
#include "core/wrappers.h"




using namespace core;



namespace kw {
#pragma GCC visibility push(default)
#define KeywordPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkgName,lispName,export) core::Symbol_sp cname = UNDEFINED_SYMBOL;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef KeywordPkg_SYMBOLS
#pragma GCC visibility pop
};

namespace llvmo {


#pragma GCC visibility push(default)
#define LlvmoPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkgName,lispName,export) core::Symbol_sp cname = UNDEFINED_SYMBOL;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef LlvmoPkg_SYMBOLS
#pragma GCC visibility pop
};



namespace llvmo
{
#define EXPOSE_TO_CANDO
#define Use_LlvmoPkg
#define EXTERN_REGISTER
#include "llvmo_initClasses_inc.h"
#undef EXTERN_REGISTER
#undef Use_LlvmoPkg
#undef EXPOSE_TO_CANDO
};



//
// Load the gctools::GcInfo<core-classes>::Kind specializers
//
#define NAMESPACE_llvmo
#include "main/gc_interface.h"
#undef NAMESPACE_llvmo






namespace llvmo
{

    SYMBOL_EXPORT_SC_(LlvmoPkg,STARrunTimeExecutionEngineSTAR);


    void redirect_llvm_interface_addSymbol()
    {
//	llvm_interface::addSymbol = &addSymbolAsGlobal;
    }

    
    
#define ARGS_af_mangleSymbolName "(arg)"
#define DECL_af_mangleSymbolName ""
#define DOCS_af_mangleSymbolName "Mangle the LLVM symbol name so that it will be a legal symbol for ld"
    Str_sp af_mangleSymbolName(Str_sp name)
    {_G();
	stringstream sout;
	const char* cur = name->get().c_str();
	bool first = true;
	while (*cur) {
	    if ( ((*cur) >= 'a' && (*cur) <= 'z')
		 || ((*cur) >= 'A' && (*cur) <= 'Z')
		 || ((*cur) == '_')
		 || (!first && ((*cur) >= '0' && (*cur) <= '9'))) {
		sout << (*cur);
	    } else {
		sout << "_";
		sout << std::hex << std::uppercase << (int)(*cur) << std::dec;
		sout << "_";
	    }
	    first=false;
	    ++cur;
	}
	return Str_O::create(sout.str());
    };


    
    
#define ARGS_af_throwIfMismatchedStructureSizes "(&key tsp tmv lisp-compiled-function-ihf)"
#define DECL_af_throwIfMismatchedStructureSizes ""
#define DOCS_af_throwIfMismatchedStructureSizes "throwIfMismatchedStructureSizes"
    void af_throwIfMismatchedStructureSizes(Fixnum_sp tspSize, Fixnum_sp tmvSize, Fixnum_sp lispCompiledFunctionIHFSize)
    {_G();
	if ( tspSize.nilp() ) SIMPLE_ERROR(BF("You must provide a tspSize"));
	if ( tmvSize.nilp() ) SIMPLE_ERROR(BF("You must provide a tmvSize"));
	int T_sp_size = sizeof(core::T_sp);
	if ( tspSize->get() != T_sp_size )
	{
	    SIMPLE_ERROR(BF("Mismatch between tsp size[%d] and core::T_sp size[%d]") % tspSize->get() % T_sp_size );
	}
	int T_mv_size = sizeof(core::T_mv);
	if ( tmvSize->get() != T_mv_size )
	{
	    SIMPLE_ERROR(BF("Mismatch between tmv size[%d] and core::T_mv size[%d]") % tmvSize->get() % T_mv_size );
	}
	if ( !lispCompiledFunctionIHFSize.nilp() )
	{
	    int LispCompiledFunctionIHF_size = sizeof(core::LispCompiledFunctionIHF);
	    if ( lispCompiledFunctionIHFSize->get() != LispCompiledFunctionIHF_size )
	    {
		SIMPLE_ERROR(BF("Mismatch between IR lisp-compiled-function-ihf size[%d]"
				" and sizeof(LispCompiledFunctionIHF)=[%d]")
			     % lispCompiledFunctionIHFSize % LispCompiledFunctionIHF_size );
	    }
	}
    };






#if 0    
#define ARGS_af_memoryLockedSymbolForLlvm "(symbol)"
#define DECL_af_memoryLockedSymbolForLlvm ""
#define DOCS_af_memoryLockedSymbolForLlvm "Lookup or create a boost::shared_ptr<Symbol_O> for a Symbol and return the pointer to it"
    core::Symbol_sp* getOrCreateMemoryLockedSymbolForLlvm(core::Symbol_sp sym)
    {_G();
	STATIC_ROOT_FRAME_BEGIN(MemoryLockedSymbols) {
	    map<string,core::Symbol_sp*>	_Map;
	    MemoryLockedSymbols() {this->attachToGCRoot();} // attach to MPS
	    GC_SCANNER() {
		GC_SCANNER_BEGIN() {
		for ( map<string,core::Symbol_sp*>::iterator it=this->_Map.begin(); it!=this->_Map.end(); ++it )
		{
		    SMART_PTR_FIX(*(it->second));
		}
		} GC_SCANNER_END();
		return GC_RES_OK;
	    }
	} STATIC_ROOT_FRAME_END(MemoryLockedSymbols,static_lockedSymbols);

	string symbolFullName = sym->fullName();
	map<string,core::Symbol_sp*>::iterator it = static_lockedSymbols->_Map.find(symbolFullName);
	core::Symbol_sp* address = NULL;
	if (it == static_lockedSymbols->_Map.end() )
	{
	    address = new core::Symbol_sp(sym);
	    static_lockedSymbols->_Map[symbolFullName] = address;
	} else
	{
	    address = it->second;
	}
	return address;
    };
#endif



#define ARGS_af_getOrCreateExternalGlobal "(symbol resname shared-ptr-type)"
#define DECL_af_getOrCreateExternalGlobal ""
#define DOCS_af_getOrCreateExternalGlobal "getOrCreateExternalGlobal"
    llvmo::GlobalVariable_sp af_getOrCreateExternalGlobal( llvmo::Module_sp module, const string& name, llvmo::Type_sp data_type )
    {_G();
	llvm::Module* llvm_module = module->wrappedPtr();
	llvm::Type* llvm_data_type = data_type->wrappedPtr();
	ASSERT(llvm_module!=NULL);
	ASSERT(llvm_data_type!=NULL);
	llvm::GlobalVariable* global = NULL;
	global = llvm_module->getNamedGlobal(name);
	if ( global == NULL )
	{
	    global = new llvm::GlobalVariable(*llvm_module,
					      llvm_data_type,
					      true, // isConstant
					      llvm::GlobalValue::ExternalLinkage,
					      0, // Initializer
					      name );
	}
	GlobalVariable_sp gv = RP_Create_wrapped<GlobalVariable_O,llvm::GlobalVariable*>(global);
	return gv;
    }





    
    




    void dump_funcs(core::CompiledFunction_sp compiledFunction)
    {
	core::CompiledBody_sp cb = compiledFunction->getBody();
	core::T_sp funcs = cb->compiledFuncs();
	if ( af_consP(funcs) )
	{
	    core::Cons_sp cfuncs = funcs.as<core::Cons_O>();
	    for ( core::Cons_sp cur = cfuncs; cur.notnilp(); cur=cCdr(cur) )
	    {
		core::T_sp func = oCar(cur);
		if ( llvmo::Function_sp f = func.as<llvmo::Function_O>() )
		{
		    f->dump();
		} else
		{
		    printf("af_disassemble -> %s\n", _rep_(func).c_str() );
		}
	    }
	    return;
	}
	printf("af_disassemble -> %s\n", _rep_(funcs).c_str() );
    }
    
    
#define ARGS_af_disassembleSTAR "(fn)"
#define DECL_af_disassembleSTAR ""
#define DOCS_af_disassembleSTAR "disassembleSTAR"
    void af_disassembleSTAR(core::CompiledFunction_sp cf)
    {_G();
	if (cf.pointerp()) {
	    dump_funcs(cf);
	} else {
	    SIMPLE_ERROR(BF("Could not disassemble %s") % _rep_(cf));
	}
    }





    void LlvmoExposer::expose(core::Lisp_sp lisp,core::PackageExposer::WhatToExpose what) const
    {_G();
	//
	// Initialize the intrinsic functions in intrinsics.cc
	//
	
	switch (what)
	{
	case candoClasses:
	{
#define LlvmoPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,exportp) {cname = _lisp->internWithPackageName(pkg,lispname); cname->exportYourself(exportp);}
#include "llvmo/symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef LlvmoPkg_SYMBOLS

#define ALL_STAGES
#define Use_LlvmoPkg
#define INVOKE_REGISTER
#define LOOKUP_SYMBOL(pkg,name) _lisp->internWithPackageName(pkg,name)
#include "llvmo_initClasses_inc.h"
#undef LOOKUP_SYMBOL
#undef INVOKE_REGISTER
#undef Use_LlvmoPkg
#undef ALL_STAGES
	}	
	break;
	case candoFunctions:
	{
	    SYMBOL_EXPORT_SC_(LlvmoPkg,getOrCreateExternalGlobal);
	    Defun(getOrCreateExternalGlobal);
	    SYMBOL_EXPORT_SC_(LlvmoPkg,disassembleSTAR);
	    Defun(disassembleSTAR);
	    SYMBOL_EXPORT_SC_(LlvmoPkg,throwIfMismatchedStructureSizes);
	    Defun(throwIfMismatchedStructureSizes);
	    Defun(mangleSymbolName);
	    //nothing
	};
	break;
	case candoGlobals:
	{
            initialize_intrinsics();
	    initialize_llvmo_expose();
            initialize_clbind_llvm_expose();
	    initialize_dwarf_constants();

//	initializeLlvmConstants(_lisp);
		};
	break;
	case pythonClasses:
	case pythonFunctions:
	case pythonGlobals:
	{
	    IMPLEMENT_ME();
	}
	break;
	}
    }









	
};




#ifdef USE_MPS
//
// Include the Kinds
//
#define NAMESPACE_llvmo
#include GARBAGE_COLLECTION_INCLUDE
#undef NAMESPACE_llvmo
#endif



#if USE_INTRUSIVE_SMART_PTR==1
#define EXPAND_CLASS_MACROS
#if defined(USE_MPS) // MPS doesn't need INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS
#define _CLASS_MACRO(_U_)				\
    STATIC_CLASS_INFO(_U_);			
#else
#define _CLASS_MACRO(_U_)				\
    STATIC_CLASS_INFO(_U_);			\
    INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(_U_)
#endif
#include "llvmo_initClasses_inc.h"
#undef _CLASS_MACRO
#undef EXPAND_CLASS_MACROS
#endif




