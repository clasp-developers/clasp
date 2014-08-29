
#define	DEBUG_LEVEL_FULL
#include "dlfcn.h"
#include "core/common.h"
#include "core/environment.h"
#include "fileSystem.h"
#include "designators.h"
#include "evaluator.h"
#include "symbolTable.h"
#include "core/str.h"
#include "compiler.h"
#include "pathname.h"
#include "unixfsys.h"
#include "lambdaListHandler.h"
#include "multipleValues.h"
#include "pointer.h"
#include "environment.h"
#include "core/wrappers.h"

#ifdef _TARGET_OS_DARWIN
#import <mach-o/dyld.h>
#endif



namespace core
{



    typedef void (*InitFnPtr)();
    



    T_sp varArgsList(int n_args, ... )
    {
	va_list ap;
	va_start(ap, n_args);
	Cons_O::CdrType_sp first = _Nil<Cons_O::CdrType_O>();
        Cons_O::CdrType_sp* curP = &first; // gctools::StackRootedPointerToSmartPtr<Cons_O::CdrType_O> cur(&first);
	for(int i = 1; i <= n_args; ++i) {
	    T_sp obj = *(va_arg(ap, const T_sp*));
	    Cons_sp one = Cons_O::create(obj);
	    *curP = one; // cur.setPointee(one); // *cur = one;
	    curP = one->cdrPtr(); // cur.setPointer(one->cdrPtr()); // cur = one->cdrPtr();
	}
	va_end(ap);
	return first;
    }


    
    
#define ARGS_af_loadBundle "(name &optional init-fn-name)"
#define DECL_af_loadBundle ""
#define DOCS_af_loadBundle "loadBundle"
    T_mv af_loadBundle(T_sp pathDesig, Str_sp oinitFnName)
    {_G();
	DynamicScopeManager dynScopeManager1(cl::_sym_STARreadtableSTAR,cl::_sym_STARreadtableSTAR->symbolValue());
	DynamicScopeManager dynScopeManager2(cl::_sym_STARpackageSTAR,cl::_sym_STARpackageSTAR->symbolValue());

	Pathname_sp path = cl_pathname(pathDesig);
	if ( af_probe_file(path).notnilp() ) goto LOAD;
	path->_Type = Str_O::create("bundle");
	if ( af_probe_file(path).notnilp() ) goto LOAD;
	path->_Type = Str_O::create("dylib");
	if ( af_probe_file(path).notnilp() ) goto LOAD;
	path->_Type = Str_O::create("so");
	if ( af_probe_file(path).notnilp() ) goto LOAD;
	SIMPLE_ERROR(BF("Could not find bundle %s") % _rep_(pathDesig) );
    LOAD:
	Str_sp nameStr = af_namestring(af_probe_file(path));
	string name = nameStr->get();

	/* Look up the initialization function. */
	string stem = af_string_downcase(path->_Name.as<Str_O>())->get();
	size_t dsp = 0;
	if ( (dsp = stem.find("_dbg")) != string::npos) stem = stem.substr(0,dsp);
	else if ( (dsp = stem.find("_opt")) != string::npos) stem = stem.substr(0,dsp);
	else if ( (dsp = stem.find("_d")) != string::npos) stem = stem.substr(0,dsp);
	else if ( (dsp = stem.find("_o")) != string::npos) stem = stem.substr(0,dsp);

	int mode = RTLD_NOW | RTLD_GLOBAL; // | RTLD_FIRST;
	void* handle = dlopen(name.c_str(),mode);
	if ( handle == NULL )
	{
	    string error = dlerror();
	    return(Values(_Nil<T_O>(),Str_O::create(error)));
	}
	if ( _sym_STARllvmFunctionNameHookSTAR->symbolValue().nilp() )
	{
	    SIMPLE_ERROR(BF("Cannot generate llvm function name for %s because *llvm-function-name-hook* is not defined") % name );
	}
	string mainName = "";
	if ( oinitFnName.notnilp() )
	{
	    mainName = oinitFnName->get();
	} else {
            mainName = eval::funcall(_sym_STARllvmFunctionNameHookSTAR->symbolValue(), path).as<Str_O>()->get();
        }
	InitFnPtr fnP = (InitFnPtr)dlsym(handle,mainName.c_str());
	if ( fnP == NULL )
	{
	    SIMPLE_ERROR(BF("Could not find initialization function %s") % mainName );
	}
//	printf("Found function %s at address %p\n", mainName.c_str(), fnP);
	(*fnP)();
	return(Values(Pointer_O::create(handle),_Nil<T_O>()));
    };



    
#define ARGS_af_dlload "(pathDesig)"
#define DECL_af_dlload ""
#define DOCS_af_dlload "dlload - Open a dynamic library and evaluate the 'init_XXXX' extern C function. Returns (values returned-value error-message(or nil if no error))"
    T_mv af_dlload(T_sp pathDesig)
    {_G();
	string lib_extension = ".dylib";
#ifdef	_TARGET_OS_DARWIN
	lib_extension = ".dylib";
#endif
#ifdef	_TARGET_OS_LINUX
	lib_extension = ".so";
#endif
	int mode = RTLD_NOW | RTLD_GLOBAL;
	Path_sp path = coerce::pathDesignator(pathDesig);
	Path_sp pathWithProperExtension = path->replaceExtension(lib_extension);
	string ts = pathWithProperExtension->asString();
	void* handle = dlopen(ts.c_str(),mode);
	if ( handle == NULL )
	{
	    string error = dlerror();
	    return(Values(_Nil<T_O>(),Str_O::create(error)));
	}
	string stem = path->stem();
	size_t dsp = 0;
	if ( (dsp = stem.find("_d")) != string::npos)
	{
	    stem = stem.substr(0,dsp);
	}
	stringstream ss;
	ss << "___kernel_" << stem;
	string initName;
	string kernelInitName = ss.str();
	initName = kernelInitName;
	InitFnPtr fnP = (InitFnPtr)dlsym(handle,kernelInitName.c_str());
	if ( fnP == NULL )
	{
	    ss.str("");
	    ss << "___user_" << stem;
	    string userInitName = ss.str();
	    initName = userInitName;
	    fnP = (InitFnPtr)dlsym(handle,userInitName.c_str());
	    if ( fnP == NULL )
	    {
		SIMPLE_ERROR(BF("Could not find initialization function %s or %s") % kernelInitName % userInitName );
	    }
	}
//	printf("Found function %s at address %p\n", initName.c_str(), fnP);
	T_mv result;
	ActivationFrame_sp frame = _Nil<ActivationFrame_O>();
	(*fnP)();
	return(Values(Pointer_O::create(handle),_Nil<T_O>()));
    }




    
#define ARGS_af_dlopen "(pathDesig)"
#define DECL_af_dlopen ""
#define DOCS_af_dlopen "dlopen - Open a dynamic library and evaluate the 'init_XXXX' extern C function. Returns (values returned-value error-message(or nil if no error))"
    T_mv af_dlopen(T_sp pathDesig)
    {_G();
	string lib_extension = ".dylib";
#ifdef	_TARGET_OS_DARWIN
	lib_extension = ".dylib";
#endif
#ifdef	_TARGET_OS_LINUX
	lib_extension = ".so";
#endif
	int mode = RTLD_NOW | RTLD_GLOBAL;
	Path_sp path = coerce::pathDesignator(pathDesig);
	string ts0 = path->asString();
	void* handle = dlopen(ts0.c_str(),mode);
	if ( !handle ) {
	  Path_sp pathWithProperExtension = path->replaceExtension(lib_extension);
	  string ts = pathWithProperExtension->asString();
	  handle = dlopen(ts.c_str(),mode);
	  if ( !handle )
	    {
	      string error = dlerror();
	      return(Values(_Nil<T_O>(),Str_O::create(error)));
	    }
	}
	return(Values(Pointer_O::create(handle),_Nil<T_O>()));
    }






    
    
#define ARGS_af_dlsym "(handle name)"
#define DECL_af_dlsym ""
#define DOCS_af_dlsym "(dlsym handle name) handle is from dlopen or :rtld-next, :rtld-self, :rtld-default or :rtld-main-only (see dlsym man page) returns ptr or nil if not found."
    T_sp af_dlsym(T_sp ohandle, Str_sp name)
    {_G();
	void* handle = NULL;
	if ( ohandle.nilp() ) {
	    SIMPLE_ERROR(BF("Invalid ohandle passed -> nil"));
	} else if ( Pointer_sp phandle = ohandle.asOrNull<Pointer_O>() )
	{
	    handle = phandle->ptr();
	} else if (ohandle.isA<Symbol_O>() )
	{
	    Symbol_sp sym = ohandle.asOrNull<Symbol_O>();
	    SYMBOL_SC_(KeywordPkg,rtld_default);
	    SYMBOL_SC_(KeywordPkg,rtld_next);	
	    SYMBOL_SC_(KeywordPkg,rtld_self);
	    SYMBOL_SC_(KeywordPkg,rtld_main_only);
	    if ( sym == kw::_sym_rtld_default )
	    {
		handle = RTLD_DEFAULT;
	    } else if ( sym == kw::_sym_rtld_next )
	    {
		handle = RTLD_NEXT;
#ifndef _TARGET_OS_LINUX
            } else if ( sym == kw::_sym_rtld_self ) //NOT PORTABLE TO LINUX
	    {
		handle = RTLD_SELF;
	    } else if ( sym == kw::_sym_rtld_main_only )
	    {
		handle = RTLD_MAIN_ONLY;
#endif
            } else
	    {
		SIMPLE_ERROR(BF("Illegal keyword[%s] for dlsym - only :rtld-next :rtld-self :rtld-default :rtld-main-only are allowed") % _rep_(sym));
	    }
	}
	string ts = name->get();
	void* ptr = dlsym(handle,ts.c_str());
	if ( ptr == NULL )
	{
	    return _Nil<T_O>();
	}
	return Pointer_O::create(ptr);
    }




    
#define ARGS_af_dladdr "(addr)"
#define DECL_af_dladdr ""
#define DOCS_af_dladdr "(call dladdr with the address and return nil if not found or the contents of the Dl_info structure as multiple values)"
    T_mv af_dladdr(Integer_sp addr)
    {_G();
	uint64_t val = addr->as_uint64();
	void* ptr = (void*)val;
	Dl_info info;
	int ret = dladdr(ptr,&info);
	if ( !ret )
	{
	    return Values(_Nil<T_O>());
	} else
	{
	    return Values(Str_O::create(info.dli_fname),
			  Pointer_O::create(info.dli_fbase),
			  Str_O::create(info.dli_sname),
			  Pointer_O::create(info.dli_saddr));
	}
    }





    
    
    
#define ARGS_af_implicit_compile_hook_default "(form &optional environment)"
#define DECL_af_implicit_compile_hook_default ""
#define DOCS_af_implicit_compile_hook_default "implicit_compile_hook_default"
    T_mv af_implicit_compile_hook_default(T_sp form, Environment_sp env)
    {_G();
	// Convert the form into a thunk and return like COMPILE does
	LambdaListHandler_sp llh = LambdaListHandler_O::create(0);
	Cons_sp code = Cons_O::create(form,_Nil<Cons_O>());
        SourceManager_sp db = _lisp->sourceDatabase();
        SourcePosInfo_sp info = lisp_registerSourceInfo(code
                                                       , core::_sym_STARloadCurrentSourceFileInfoSTAR->symbolValue().as<SourceFileInfo_O>()
                                                       , core::_sym_STARloadCurrentLinenumberSTAR->symbolValue().as<Fixnum_O>()->get()
                                                       , core::_sym_STARloadCurrentColumnSTAR->symbolValue().as<Fixnum_O>()->get() );
        stringstream ss;
        ss << "repl"<<_lisp->nextReplCounter();
        Symbol_sp name = _lisp->intern(ss.str());
        InterpretedClosure* ic = gctools::ClassAllocator<InterpretedClosure>::allocateClass(name
                                                                                            ,info
                                                                                            , kw::_sym_function
                                                                                            , llh
                                                                                            , _Nil<Cons_O>()
                                                                                            , _Nil<Str_O>()
                                                                                            , env
                                                                                            , code );
        Function_sp thunk = Function_O::make(ic);
	return(Values(thunk,_Nil<T_O>(),_Nil<T_O>()));
    };





    
    
#define ARGS_core_applysPerSecond "(fn &rest args)"
#define DECL_core_applysPerSecond ""
#define DOCS_core_applysPerSecond "applysPerSecond"
    T_sp core_applysPerSecond(T_sp fn, T_sp args)
    {_G();
        LightTimer timer;
        int nargs = cl_length(args);
        ALLOC_STACK_VALUE_FRAME(frameImpl,frame,nargs);
        for ( int pow=0; pow<16; ++pow ) {
            int times = 1 << pow*2;
            timer.reset();
            timer.start();
            T_sp cur = args;
            // Fill frame here
            for ( int i(0); i<times; ++i ) {
                eval::apply(fn,args);
            }
            timer.stop();
            if ( timer.getAccumulatedTime() > 0.5 ) {
                return DoubleFloat_O::create(((double)times)/timer.getAccumulatedTime());
            }
        }
        printf("%s:%d The function %s is too fast\n", __FILE__, __LINE__, _rep_(fn).c_str());
        return _Nil<T_O>();
    }




    void initialize_compiler_primitives(Lisp_sp lisp)
    {_G();
//	SYMBOL_SC_(CorePkg,processDeclarations);
//	Defun(processDeclarations);
	SYMBOL_EXPORT_SC_(CorePkg,STARimplicit_compile_hookSTAR);
	SYMBOL_EXPORT_SC_(CorePkg,implicit_compile_hook_default);
	Defun(implicit_compile_hook_default);
	_sym_STARimplicit_compile_hookSTAR->defparameter(_sym_implicit_compile_hook_default->symbolFunction());
	SYMBOL_SC_(CorePkg,dlload);
	Defun(dlload);
	
	SYMBOL_SC_(CorePkg,dlopen);
	Defun(dlopen);
	
	SYMBOL_SC_(CorePkg,dlsym);
	Defun(dlsym);
	
	SYMBOL_SC_(CorePkg,dladdr);
	Defun(dladdr);
	
	SYMBOL_SC_(CorePkg,loadBundle);
	Defun(loadBundle);

        CoreDefun(applysPerSecond);

    }


}; /* namespace */
