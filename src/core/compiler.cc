/*
    File: compiler.cc
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



// #define EXPOSE_DLOPEN
// #define EXPOSE_DLLOAD
#define	DEBUG_LEVEL_FULL
#include <dlfcn.h>
#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/lightProfiler.h>
#include <clasp/core/designators.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/str.h>
#include <clasp/core/compiler.h>
#include <clasp/core/sequence.h>
#include <clasp/core/pathname.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/cleavirPrimopsPackage.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/pointer.h>
#include <clasp/core/environment.h>
#include <clasp/core/wrappers.h>

#ifdef _TARGET_OS_DARWIN
#import <mach-o/dyld.h>
#endif



namespace core
{



    typedef void (*InitFnPtr)(LCC_RETURN, LCC_CLOSED_ENVIRONMENT, LCC_ARGS_BASE );




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






    #define ARGS_core_mangleName "(object &optional is_function)"
#define DECL_core_mangleName ""
#define DOCS_core_mangleName "mangleName"
    T_mv core_mangleName(Symbol_sp sym, bool is_function)
    {
	Str_sp name;
	if ( !is_function ) {
	    if ( sym.nilp() ) name = Str_O::create("CLASP_NIL");
	    else if ( sym == _lisp->_true() ) name = Str_O::create("CLASP_T");
	    else {
		stringstream ss;
		ss << "SYM(" << sym->symbolName()->get() << ")";
		name = Str_O::create(ss.str());
	    }
	    return Values(_Nil<T_O>(),name,Fixnum_O::create(0),Fixnum_O::create(CALL_ARGUMENTS_LIMIT));
	}
	Function_sp fsym = coerce::functionDesignator(sym);
	gctools::tagged_functor<Closure> closure = fsym->closure;
	if ( gctools::tagged_functor<BuiltinClosure> bcc = closure.asOrNull<BuiltinClosure>() ) {
	    return Values(_lisp->_true(),Str_O::create("Provide-c-func-name"), Fixnum_O::create(0),Fixnum_O::create(CALL_ARGUMENTS_LIMIT));
	}
	return Values(_Nil<T_O>(),Str_O::create("Provide-func-name"),Fixnum_O::create(0),Fixnum_O::create(CALL_ARGUMENTS_LIMIT));
    }
    



#define ARGS_core_startupImagePathname "()"
#define DECL_core_startupImagePathname ""
#define DOCS_core_startupImagePathname "startupImagePathname - returns one of min-boehm, full-boehm, min-mps, full-mps, cleavir-boehm, cleavir-mps based on *features* :ECL-MIN, :USE-MPS, :CLEAVIR"
    T_sp core_startupImagePathname()
    {_G();
	Cons_sp features = cl::_sym_STARfeaturesSTAR->symbolValue().as<Cons_O>();
        List_sp min = features->memberEq(kw::_sym_ecl_min);
        List_sp mps = features->memberEq(kw::_sym_use_mps);
	List_sp cleavir = features->memberEq(kw::_sym_cleavir);
        string strStage = "min";
        if ( min.nilp() ) {
	    if ( cleavir.nilp() ) {
		strStage = "full";
	    } else {
		strStage = "cleavir";
	    }
	}
        string strGc = "boehm";
        if ( mps.notnilp() ) strGc = "mps";
        stringstream ss;
        ss << strStage << "-" << strGc;
        ss << ":image.fasl";
        Str_sp spath = Str_O::create(ss.str());
        Pathname_sp pn = cl_pathname(spath);
        return pn;
    };



#define ARGS_core_loadBundle "(name &optional verbose print external-format)"
#define DECL_core_loadBundle ""
#define DOCS_core_loadBundle "loadBundle"
    T_mv core_loadBundle(T_sp pathDesig, T_sp verbose, T_sp print, T_sp external_format )
    {_G();
        /* Define the source file */
        SourceFileInfo_sp sfi = core_sourceFileInfo(pathDesig);
	DynamicScopeManager scope(_sym_STARcurrentSourceFileInfoSTAR,sfi);
	scope.pushSpecialVariableAndSet(_sym_STARsourceDatabaseSTAR,SourceManager_O::create());
	scope.pushSpecialVariableAndSet(_sym_STARcurrentSourcePosInfoSTAR,SourcePosInfo_O::create(0,0,0,0));
	scope.pushSpecialVariableAndSet(cl::_sym_STARreadtableSTAR,cl::_sym_STARreadtableSTAR->symbolValue());
	scope.pushSpecialVariableAndSet(cl::_sym_STARpackageSTAR,cl::_sym_STARpackageSTAR->symbolValue());
	Pathname_sp path = cl_pathname(pathDesig);
	if ( af_probe_file(path).notnilp() ) goto LOAD;
	path->_Type = Str_O::create("bundle");
	if ( af_probe_file(path).notnilp() ) goto LOAD;
	path->_Type = Str_O::create("fasl");
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
	string stem = cl_string_downcase(path->_Name.as<Str_O>())->get();
	size_t dsp = 0;
	if ( (dsp = stem.find("_dbg")) != string::npos) stem = stem.substr(0,dsp);
	else if ( (dsp = stem.find("_opt")) != string::npos) stem = stem.substr(0,dsp);
	else if ( (dsp = stem.find("_d")) != string::npos) stem = stem.substr(0,dsp);
	else if ( (dsp = stem.find("_o")) != string::npos) stem = stem.substr(0,dsp);

	int mode = RTLD_NOW | RTLD_LOCAL; // | RTLD_FIRST;
	// Check if we already have this dynamic library loaded
	map<string,void*>::iterator handleIt = _lisp->openDynamicLibraryHandles().find(name);
	if (handleIt != _lisp->openDynamicLibraryHandles().end() ) {
	    dlclose(handleIt->second);
	    //	    printf("%s:%d Closing the existing dynamic library %s\n", __FILE__, __LINE__, name.c_str());
	    _lisp->openDynamicLibraryHandles().erase(handleIt);
	}
	//	printf("%s:%d Loading dynamic library: %s\n", __FILE__, __LINE__, name.c_str());
	void* handle = dlopen(name.c_str(),mode);
	if ( handle == NULL )
	{
	    string error = dlerror();
	    return(Values(_Nil<T_O>(),Str_O::create(error)));
	}
	_lisp->openDynamicLibraryHandles()[name] = handle;
	string mainName = CLASP_MAIN_FUNCTION_NAME;
	InitFnPtr mainFunctionPointer = (InitFnPtr)dlsym(handle,mainName.c_str());
	if ( mainFunctionPointer == NULL )
	{
	    SIMPLE_ERROR(BF("Could not find initialization function %s") % mainName );
	}
	//	printf("%s:%d Found initialization function %s at address %p\n", __FILE__, __LINE__, mainName.c_str(), mainFunctionPointer);
	T_mv result;
	(*mainFunctionPointer)(&result,NULL,LCC_PASS_ARGS0());
	return(Values(Pointer_O::create(handle),_Nil<T_O>()));
    };



#ifdef EXPOSE_DLLOAD
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
	int mode = RTLD_NOW | RTLD_LOCAL;
	Path_sp path = coerce::pathDesignator(pathDesig);
	Path_sp pathWithProperExtension = path->replaceExtension(lib_extension);
	string ts = pathWithProperExtension->asString();
	printf("%s:%d Loading with af_dlload %s\n", __FILE__, __LINE__, ts.c_str());
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
	InitFnPtr mainFunctionPointer = (InitFnPtr)dlsym(handle,kernelInitName.c_str());
	if ( mainFunctionPointer == NULL )
	{
	    ss.str("");
	    ss << "___user_" << stem;
	    string userInitName = ss.str();
	    initName = userInitName;
	    mainFunctionPointer = (InitFnPtr)dlsym(handle,userInitName.c_str());
	    if ( mainFunctionPointer == NULL )
	    {
		SIMPLE_ERROR(BF("Could not find initialization function %s or %s") % kernelInitName % userInitName );
	    }
	}
//	printf("Found function %s at address %p\n", initName.c_str(), mainFunctionPointer);
	T_mv result;
	ActivationFrame_sp frame = _Nil<ActivationFrame_O>();
	(*mainFunctionPointer)();
	return(Values(Pointer_O::create(handle),_Nil<T_O>()));
    }
#endif




#define ARGS_af_dlopen "(pathDesig)"
#define DECL_af_dlopen ""
#define DOCS_af_dlopen "dlopen - Open a dynamic library and return the handle. Returns (values returned-value error-message(or nil if no error))"
    T_mv af_dlopen(T_sp pathDesig)
    {_G();
	string lib_extension = ".dylib";
	int mode = RTLD_NOW | RTLD_LOCAL;
	Path_sp path = coerce::pathDesignator(pathDesig);
	string ts0 = path->asString();
	void* handle = dlopen(ts0.c_str(),mode);
	if ( !handle ) {
            printf("%s:%d Could not open %s  error: %s\n", __FILE__, __LINE__, ts0.c_str(), dlerror());
            string error = dlerror();
            return(Values(_Nil<T_O>(),Str_O::create(error)));
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

#define ARGS_core_callDlMainFunction "(addr)"
#define DECL_core_callDlMainFunction ""
#define DOCS_core_callDlMainFunction "(call dladdr with the address and return nil if not found or the contents of the Dl_info structure as multiple values)"
    void core_callDlMainFunction(Pointer_sp addr)
    {
	InitFnPtr mainFunctionPointer = (InitFnPtr)addr->ptr();
	T_mv result;
	(*mainFunctionPointer)(&result,NULL,LCC_PASS_ARGS0());
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
    T_mv af_implicit_compile_hook_default(T_sp form, T_sp env)
    {_G();
	// Convert the form into a thunk and return like COMPILE does
	LambdaListHandler_sp llh = LambdaListHandler_O::create(0);
	Cons_sp code = Cons_O::create(form,_Nil<T_O>());
        SourceManager_sp db = _lisp->sourceDatabase();
	T_sp sourcePosInfo = db->duplicateSourcePosInfo(form,code);
        stringstream ss;
        ss << "repl"<<_lisp->nextReplCounter();
        Symbol_sp name = _lisp->intern(ss.str());
	gctools::tagged_functor<InterpretedClosure> ic = 
	    gctools::tagged_functor<InterpretedClosure>(gctools::ClassAllocator<InterpretedClosure>::allocateClass(
								       name
								       , sourcePosInfo
								       , kw::_sym_function
								       , llh
								       , _Nil<T_O>()
								       , _Nil<T_O>()
								       , env
								       , code ));
        Function_sp thunk = Function_O::make(ic);
	return(Values(thunk,_Nil<T_O>(),_Nil<T_O>()));
    };







#define ARGS_core_applysPerSecond "(fn &rest args)"
#define DECL_core_applysPerSecond ""
#define DOCS_core_applysPerSecond "applysPerSecond"
    T_sp core_applysPerSecond(T_sp fn, List_sp args)
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
                eval::applyLastArgsPLUSFirst(fn,args);
            }
            timer.stop();
            if ( timer.getAccumulatedTime() > 0.1 ) {
                return DoubleFloat_O::create(((double)times)/timer.getAccumulatedTime());
            }
        }
        printf("%s:%d The function %s is too fast\n", __FILE__, __LINE__, _rep_(fn).c_str());
        return _Nil<T_O>();
    }

};

extern "C" {
    __attribute__ ((noinline)) int callByValue(core::T_sp v1, core::T_sp v2, core::T_sp v3, core::T_sp v4)
    {
	ASSERT(v1.fixnump());
	ASSERT(v2.fixnump());
	ASSERT(v3.fixnump());
	ASSERT(v4.fixnump());
	int f1 = v1.unsafe_fixnum();
	int f2 = v2.unsafe_fixnum();
	int f3 = v3.unsafe_fixnum();
	int f4 = v4.unsafe_fixnum();
	int res = f1+f2+f3+f4;
	return res;
    }


    __attribute__ ((noinline)) int callByPointer( core::T_O* v1,  core::T_O* v2, core::T_O* v3, core::T_O* v4)
    {
	ASSERT(gctools::tagged_fixnump(v1));
	ASSERT(gctools::tagged_fixnump(v2));
	ASSERT(gctools::tagged_fixnump(v3));
	ASSERT(gctools::tagged_fixnump(v4));
	int f1 = gctools::untag_fixnum(v1);
	int f2 = gctools::untag_fixnum(v2);
	int f3 = gctools::untag_fixnum(v3);
	int f4 = gctools::untag_fixnum(v4);
	int res = f1+f2+f3+f4;
	return res;
    }


    __attribute__ ((noinline)) int callByConstRef(const core::T_sp& v1, const core::T_sp& v2, const core::T_sp& v3, const core::T_sp& v4)
    {
	ASSERT(v1.fixnump());
	ASSERT(v2.fixnump());
	ASSERT(v3.fixnump());
	ASSERT(v4.fixnump());
	int f1 = v1.unsafe_fixnum();
	int f2 = v2.unsafe_fixnum();
	int f3 = v3.unsafe_fixnum();
	int f4 = v4.unsafe_fixnum();
	int res = f1+f2+f3+f4;
	return res;
    }

};

namespace core {

#if 0
#define ARGS_core_globalFuncallCyclesPerSecond "(stage fn &rest args)"
#define DECL_core_globalFuncallCyclesPerSecond ""
#define DOCS_core_globalFuncallCyclesPerSecond "globalFuncallCyclesPerSecond"
    T_sp core_globalFuncallCyclesPerSecond(int stage, Symbol_sp fn, List_sp args )
    {_G();
        LightTimer timer;
        int nargs = cl_length(args);
        T_sp v1, v2, v3;
        T_O* rawArgs[64];
        int nargs = af_length(args);
        Cons_sp cur = args;
        for ( int i=0; i<nargs; ++i ) {
            rawArgs[i] = oCar(cur).raw_();
            cur=cCdr(cur);
        }
        ALLOC_STACK_VALUE_FRAME(frameImpl,frame,nargs);
        for ( int pow=0; pow<16; ++pow ) {
            int times = 1 << pow*2;  // the number of times to run the inner loop
            timer.reset();
            timer.start();   // Wrap a timer around the repeated inner loop
            T_sp cur = args;
            // Fill frame here
            for ( int i(0); i<times; ++i ) {
                // Compare to call by value
                callByValue(v1,v2,v3,v4);
                if ( stage>=1 ) {
                    closure = va_symbolFunction(&fn);
                    if ( stage>=2 ) {
                        switch (nargs) {
                        case 0:
                            mv_FUNCALL(&result_mv,closure,NULL,NULL,NULL);
                            break;
                        case 1:
                            mv_FUNCALL(&result_mv,closure,rawArg[0],NULL,NULL);
                            break;
                        case 2:
                            mv_FUNCALL(&result_mv,closure,rawArg[0],rawArg[1],NULL);
                            break;
                        case 3:
                            mv_FUNCALL(&result_mv,closure,rawArg[0],rawArg[1],rawArg[2]);
                            break;

                        mv_FUNCALL(&result_mv,closure,
                        int nargs = cl_length(args);
                        if ( stage>=3 ) { // This is expensive
                            ValueFrame_sp frame(ValueFrame_O::create_fill_numExtraArgs(nargs,_Nil<ActivationFrame_O>()));
                            if ( stage>=4 ) {
                                Cons_sp cur = args;
                                for ( int i=nargs; i<nargs; ++i ) {
                                    frame->operator[](i) = oCar(cur);
                                    cur=cCdr(cur);
                                }
                                if ( stage >= 5) {
                                    Closure* closureP = func->closure;
                                    ASSERTF(closureP,BF("In applyToActivationFrame the closure for %s is NULL") % _rep_(fn));
                                    eval::applyClosureToActivationFrame(closureP,frame);
                                }
                            }
                        }
                    }
                }
            }
            timer.stop();
            if ( timer.getAccumulatedTime() > 0.1 ) {
                return DoubleFloat_O::create(((double)times)/timer.getAccumulatedTime());
            }
        }
        printf("%s:%d The function %s is too fast\n", __FILE__, __LINE__, _rep_(fn).c_str());
        return _Nil<T_O>();
    }
#endif



#define ARGS_core_partialApplysPerSecond "(stage fn args)"
#define DECL_core_partialApplysPerSecond ""
#define DOCS_core_partialApplysPerSecond "partialApplysPerSecond"
    T_sp core_partialApplysPerSecond(int stage, T_sp fn, List_sp args)
    {_G();
        LightTimer timer;
        int nargs = cl_length(args);
        T_sp v1, v2, v3, v4;
        ALLOC_STACK_VALUE_FRAME(frameImpl,frame,nargs);
        for ( int pow=0; pow<16; ++pow ) {
            int times = 1 << pow*2;  // the number of times to run the inner loop
            timer.reset();
            timer.start();   // Wrap a timer around the repeated inner loop
            T_sp cur = args;
            // Fill frame here
            for ( int i(0); i<times; ++i ) {
                // Compare to call by value
                callByValue(v1,v2,v3,v4);
                if ( stage>=1 ) {
#if 0
                    Function_O* func = reinterpret_cast<Function_O*>(fn.px_ref());
#else

                    Function_sp func;
                    func = eval::lookupFunction(fn,_Nil<T_O>());
#endif
                    if ( stage>=2 ) {
#if 0  // This is the fastest alternative I can think of relative to cl_length()
                        int nargs;
                        if ( args.nilp() ) {
                            nargs = 0;
                        } else {
                            nargs = args->fastUnsafeLength();
                        }
#else
                        int nargs = cl_length(args);
#endif
                        if ( stage>=3 ) { // This is expensive
#if 1 // heap based frame
                            ValueFrame_sp frame(ValueFrame_O::create_fill_numExtraArgs(nargs,_Nil<ActivationFrame_O>()));
                            if ( stage>=4 ) {
                                List_sp cur = args;
                                for ( int i=0; i<nargs; ++i ) {
                                    frame->operator[](i) = oCar(cur);
                                    cur=oCdr(cur);
                                }
#else
                            // ALLOC_STACK_VALUE_FRAME(frameImpl,frob,nargs);   // I cant do this in a loop!!!
                            // frame::SetParentFrame(frob,_Nil<T_O>());
                            // if ( stage>=4 ) {
                            //     Cons_sp cur = args;
                            //     T_O** values = frame::ValuesArray(frob);
                            //     for ( int i=0; i<nargs; ++i ) {
                            //         values[i] = oCar(cur).raw_();
                            //         cur = cCdr(cur);
                            //     }
#endif
                                if ( stage >= 5) {
				    gctools::tagged_functor<Closure> closureP = func->closure;
                                    ASSERTF(closureP,BF("In applyToActivationFrame the closure for %s is NULL") % _rep_(fn));
                                    eval::applyClosureToActivationFrame(closureP,frame);
                                }
                            }
                        }
                    }
                }
            }
            timer.stop();
            if ( timer.getAccumulatedTime() > 0.1 ) {
                return DoubleFloat_O::create(((double)times)/timer.getAccumulatedTime());
            }
        }
        printf("%s:%d The function %s is too fast\n", __FILE__, __LINE__, _rep_(fn).c_str());
        return _Nil<T_O>();
    }

    T_sp allocFixnum()
    {
        Fixnum_sp fn = Fixnum_O::create(3);
        return fn;
    }

    void dynamicCastArg(T_sp a)
    {
        Cons_sp c = a.as<Cons_O>();
    }

    void allocateValueFrame5()
    {
        ValueFrame_sp v = ValueFrame_O::create(5,_Nil<ActivationFrame_O>());
    }

    void allocateStackFrame5()
    {
        ALLOC_STACK_VALUE_FRAME(frameImpl,frame,5);
    }

    Cons_sp consList5()
    {
        T_sp val = _Nil<T_O>();
        return Cons_O::createList(val,val,val,val,val);
    };

    T_sp bitOLogicWithObjects()
    {
        T_sp val = _lisp->_true();
        T_sp val2 = _Nil<T_O>();
        if (val2.nilp()) {
            return val;
        }
        return val2;
    };

    T_sp allocCons()
    {
        Cons_sp fn = Cons_O::create();
        return fn;
    }

    T_sp lexicalFrameLookup(T_sp fr, int depth, int index)
    {
        T_sp val = Environment_O::clasp_lookupValue(fr,depth,index);
        return val;
    }


#define ARGS_core_operationsPerSecond "(op &optional arg)"
#define DECL_core_operationsPerSecond ""
#define DOCS_core_operationsPerSecond "operationsPerSecond"
    T_mv core_operationsPerSecond(int op, T_sp arg)
    {_G();
        ALLOC_STACK_VALUE_FRAME(frameImpl1,frame1,5);
        frame::SetParentFrame(frame1,_Nil<T_O>());
        T_O** values1 = frame::ValuesArray(frame1);
        int val=0;
        for (int i=0; i<5; ++i ) values1[i] = Fixnum_O::create(++val).raw_();
        ALLOC_STACK_VALUE_FRAME(frameImpl2,frame2,5);
        frame::SetParentFrame(frame2,frame1);
        T_O** values2 = frame::ValuesArray(frame1);
        for (int i=0; i<5; ++i ) values2[i] = Fixnum_O::create(++val).raw_();
        ALLOC_STACK_VALUE_FRAME(frameImpl3,frame3,5);
        frame::SetParentFrame(frame3,frame2);
        T_O** values3 = frame::ValuesArray(frame1);
        for (int i=0; i<5; ++i ) values3[i] = Fixnum_O::create(++val).raw_();
        LightTimer timer;
        T_sp v1, v2, v3, v4;
        T_sp ocons = Cons_O::create(_Nil<T_O>(),_Nil<T_O>());
        int times = 0;
        for ( int pow=5; pow<16; pow = pow+2 ) {
            times = 1 << pow*2;  // the number of times to run the inner loop
            timer.reset();
            timer.start();   // Wrap a timer around the repeated inner loop
            // Fill frame here
            for ( int i(0); i<times; ++i ) {
                // Compare to call by value
                switch (op) {
                case 0: break;
                case 1: {
                    callByValue(v1,v2,v3,v4);
                    break;
                }
                case 2: {
                    allocFixnum();
                    break;
                }
                case 3: {
                    dynamicCastArg(ocons);
                    break;
                }
                case 4: {
                    allocateValueFrame5();
                    break;
                }
                case 5: {
                    allocateStackFrame5();
                    break;
                }
                case 6: {
                    consList5();
                    break;
                }
                case 7: {
                    bitOLogicWithObjects();
                    break;
                }
                case 8: {
                    allocCons();
                    break;
                }
                case 9: {
                    lexicalFrameLookup(frame3,0,0);
                    break;
                }
                case 10: {
                    lexicalFrameLookup(frame3,2,0);
                    break;
                }
                default:
                    break;
                }
            }
            timer.stop();
            if ( timer.getAccumulatedTime() > 0.5 ) break;
        }
        string name;
        switch (op) {
        case 0: name = "nothing"; break;
        case 1: name = "callByValue-3args"; break;
        case 2: name = "allocate fixnum on heap"; break;
        case 3: name = "dynamic cast down"; break;
        case 4: name = "alloc value frame on heap, 5 elements"; break;
        case 5: name = "alloc stack frame, 5 elements"; break;
        case 6: name = "cons list 5 elements"; break;
        case 7: name = "logic with T and nil"; break;
        case 8: name = "alloc/abandon cons on heap"; break;
        case 9: name = "lexicalFrameLookup(frame3,0,0)"; break;
        case 10: name = "lexicalFrameLookup(frame3,2,3)"; break;
        default:
            return Values(_Nil<T_O>());
        }
        return Values(DoubleFloat_O::create(((double)times)/timer.getAccumulatedTime()),Str_O::create(name));
    }







#define ARGS_core_callsByValuePerSecond "()"
#define DECL_core_callsByValuePerSecond ""
#define DOCS_core_callsByValuePerSecond "callsByValuePerSecond"
    T_sp core_callsByValuePerSecond()
    {_G();
        LightTimer timer;
        T_sp v1 = T_sp::make_tagged_fixnum(1);
        T_sp v2 = T_sp::make_tagged_fixnum(2);
        T_sp v3 = T_sp::make_tagged_fixnum(3);
        T_sp v4 = T_sp::make_tagged_fixnum(4);
        printf("%s:%d Starting %s\n", __FILE__, __LINE__, __FUNCTION__);
	int pow;
	int res;
        for ( pow=0; pow<32; ++pow ) {
            size_t times = 1 << pow*2;
            timer.reset();
            timer.start();
	    res = 0;
            for ( size_t i=0; i<times; ++i ) {
		v1 = T_sp::make_tagged_fixnum(i);
                res += callByValue(v1,v2,v3,v4);
            }
            timer.stop();
            if ( timer.getAccumulatedTime() > 0.5 ) {
		printf("%s:%d return %s pow = %d res=%d \n", __FILE__, __LINE__, __FUNCTION__, pow, res);
                return DoubleFloat_O::create(((double)times)/timer.getAccumulatedTime());
            }
        }
        printf("%s:%d Fell through %s pow = %d res= %d \n", __FILE__, __LINE__, __FUNCTION__, pow, res);
        return _Nil<T_O>();
    }




#define ARGS_core_callsByConstantReferencePerSecond "()"
#define DECL_core_callsByConstantReferencePerSecond ""
#define DOCS_core_callsByConstantReferencePerSecond "callsByConstantReferencePerSecond"
    T_sp core_callsByConstantReferencePerSecond()
    {_G();
        LightTimer timer;
        T_sp v1 = T_sp::make_tagged_fixnum(1);
        T_sp v2 = T_sp::make_tagged_fixnum(2);
        T_sp v3 = T_sp::make_tagged_fixnum(3);
        T_sp v4 = T_sp::make_tagged_fixnum(4);
        printf("%s:%d Starting %s\n", __FILE__, __LINE__, __FUNCTION__);
	int pow;
	int res;
        for ( pow=0; pow<32; ++pow ) {
            size_t times = 1 << pow*2;
            timer.reset();
            timer.start();
	    res = 0;
            for ( size_t i=0; i<times; ++i ) {
		v1 = T_sp::make_tagged_fixnum(i);
                res += callByConstRef(v1,v2,v3,v4);
            }
            timer.stop();
            if ( timer.getAccumulatedTime() > 0.5 ) {
		printf("%s:%d return %s pow = %d res=%d \n", __FILE__, __LINE__, __FUNCTION__, pow, res);
                return DoubleFloat_O::create(((double)times)/timer.getAccumulatedTime());
            }
        }
        printf("%s:%d Fell through %s pow = %d res = %d\n", __FILE__, __LINE__, __FUNCTION__, pow, res);
        return _Nil<T_O>();
    }



#define ARGS_core_callsByPointerPerSecond "()"
#define DECL_core_callsByPointerPerSecond ""
#define DOCS_core_callsByPointerPerSecond "callsByPointerPerSecond"
    T_sp core_callsByPointerPerSecond()
    {_G();
        LightTimer timer;
        T_sp v1 = T_sp::make_tagged_fixnum(1);
        T_sp v2 = T_sp::make_tagged_fixnum(2);
        T_sp v3 = T_sp::make_tagged_fixnum(3);
        T_sp v4 = T_sp::make_tagged_fixnum(4);
        printf("%s:%d Starting %s\n", __FILE__, __LINE__, __FUNCTION__);
	int pow, res;
        for ( pow=0; pow<32; ++pow ) {
            size_t times = 1 << pow*2;
            timer.reset();
            timer.start();
	    res = 0;
            for ( size_t i=0; i<times; ++i ) {
		v1 = T_sp::make_tagged_fixnum(i);
                res += callByPointer(v1.raw_(),v2.raw_(),v3.raw_(),v4.raw_());
            }
            timer.stop();
            if ( timer.getAccumulatedTime() > 0.5 ) {
		printf("%s:%d return %s pow = %d res=%d \n", __FILE__, __LINE__, __FUNCTION__, pow, res);
                return DoubleFloat_O::create(((double)times)/timer.getAccumulatedTime());
            }
        }
        printf("%s:%d Fell through %s pow = %d res=%d \n", __FILE__, __LINE__, __FUNCTION__, pow, res);
        return _Nil<T_O>();
    }




#define ARGS_core_callWithVariableBound "(sym val thunk)"
#define DECL_core_callWithVariableBound ""
#define DOCS_core_callWithVariableBound "callWithVariableBound"
    T_mv core_callWithVariableBound(Symbol_sp sym, T_sp val, T_sp thunk)
    {
	T_sp oldVal = sym->symbolValueUnsafe();
	DynamicScopeManager scope(sym,val);
	return eval::funcall(thunk);
	// Don't put anything in here - don't mess up the MV return
    }


#define ARGS_core_multipleValueFuncall "(function-designator &rest functions)"
#define DECL_core_multipleValueFuncall ""
#define DOCS_core_multipleValueFuncall "multipleValueFuncall"
    T_mv core_multipleValueFuncall(T_sp funcDesignator, List_sp functions)
    {
	MultipleValues mvAccumulate;
	mvAccumulate._Size = 0;
	size_t idx = 0;
	for ( auto cur : functions ) {
	    Function_sp func = oCar(cur).as<Function_O>();
	    T_mv result = eval::funcall(func);
	    mvAccumulate[idx] = result.raw_();
	    ++idx;
	    for ( size_t i=1,iEnd(result.number_of_values()); i<iEnd; ++i ) {
		mvAccumulate[idx] = result.valueGet(i).raw_();
		++idx;
	    }
	}
	ASSERT(idx<MultipleValues::MultipleValuesLimit);
	mvAccumulate._Size = idx;
	Function_sp fmv = coerce::functionDesignator(funcDesignator);
	MultipleValues& mvThreadLocal = lisp_multipleValues();
	for ( size_t i=LCC_FIXED_ARGS, iEnd(mvAccumulate._Size); i<iEnd; ++i ) {
	    mvThreadLocal[i] = mvAccumulate[i];
	}
	T_mv result;
	fmv->closure->invoke(&result,mvAccumulate._Size
			     ,mvAccumulate[0]
			     ,mvAccumulate[1]
			     ,mvAccumulate[2]
			     ,mvAccumulate[3]
			     ,mvAccumulate[4]);
	return result;
    }



#define ARGS_core_multipleValueProg1_Function "(func1 func2)"
#define DECL_core_multipleValueProg1_Function ""
#define DOCS_core_multipleValueProg1_Function "multipleValueProg1_Function - evaluate func1, save the multiple values and then evaluate func2 and restore the multiple values"
    T_mv core_multipleValueProg1_Function(Function_sp func1, Function_sp func2)
    {
	MultipleValues mvFunc1;
	T_mv result;
	ASSERT(func1.notnilp()&&func1->closure);
	result = eval::funcall(func1);
	mvFunc1._Size = result.number_of_values();
	mvFunc1[0] = result.raw_();
	MultipleValues& mvThreadLocal = lisp_multipleValues();
	for ( size_t i(1),iEnd(mvFunc1._Size); i<iEnd; ++i ) {
	    mvFunc1[i] = mvThreadLocal[i];
	}
	T_mv resultTemp;
	eval::funcall(func2);
	for ( size_t i(1),iEnd(mvFunc1._Size); i<iEnd; ++i ) {
	    mvThreadLocal[i] = mvFunc1[i];
	}
	return result;
    }


#define ARGS_core_catchFunction "(tag func)"
#define DECL_core_catchFunction ""
#define DOCS_core_catchFunction "catchFunction"
    T_mv core_catchFunction(T_sp tag, Function_sp func)
    {
	int frame = _lisp->exceptionStack().push(CatchFrame,tag);
	//	printf("%s:%d Entered catchFunction my-frame: %d CatchThrow type_info@%p\n", __FILE__, __LINE__, frame, (void*)&typeid(core::CatchThrow) );
	T_mv result;
	try {
	    //		printf("%s:%d In catchFunction - evaluating function\n", __FILE__, __LINE__ );
	    result = eval::funcall(func);
	    //		printf("%s:%d In catchFunction - returned from function\n", __FILE__, __LINE__ );
	} catch (CatchThrow& catchThrow) {
	    //		printf("%s:%d Caught CatchThrow exception frame: %d  my frame: %d\n", __FILE__, __LINE__, catchThrow.getFrame(), frame );
	    if (catchThrow.getFrame() != frame) {
		throw catchThrow;
	    }
	    result = gctools::multiple_values<T_O>::createFromValues();
	}
	_lisp->exceptionStack().unwind(frame);
	//	printf("%s:%d Leaving catchFunction my-frame: %d CatchThrow type_info@%p\n", __FILE__, __LINE__, frame, (void*)&typeid(core::CatchThrow) );
	return result;
    }

#define ARGS_core_throwFunction "(tag result)"
#define DECL_core_throwFunction ""
#define DOCS_core_throwFunction "throwFunction TODO: The semantics are not followed here - only the first return value is returned!!!!!!!!"
    void core_throwFunction(T_sp tag, T_sp res)
    {
	int frame = _lisp->exceptionStack().findKey(CatchFrame,tag);
	if (frame < 0 ) {
	    CONTROL_ERROR();
	}
	//	printf("%s:%d throwFunction throwing to frame: %d  core::CatchThrow type_info@%p\n", __FILE__, __LINE__, frame, (void*)&typeid(core::CatchThrow));
	T_mv result(res,1);  // THIS IS WRONG!!!! WE MUST HANDLE MULTIPLE VALUES!!!
	result.saveToMultipleValue0();
	throw CatchThrow(frame);
    }

#define ARGS_core_progvFunction "(symbols values func)"
#define DECL_core_progvFunction ""
#define DOCS_core_progvFunction "progvFunction"
    T_mv core_progvFunction(List_sp symbols, List_sp values, Function_sp func)
    {
	DynamicScopeManager manager;
	for ( auto curSym : symbols ) {
	    Symbol_sp symbol = oCar(curSym).as<Symbol_O>();
	    T_sp value = oCar(values);
	    manager.pushSpecialVariableAndSet(symbol,value);
	    values = oCdr(values);
	}
	T_mv result = eval::funcall(func);
	return result;
    }





    
    void initialize_compiler_primitives(Lisp_sp lisp)
    {_G();
//	SYMBOL_SC_(CorePkg,processDeclarations);
//	Defun(processDeclarations);
	SYMBOL_EXPORT_SC_(CorePkg,STARimplicit_compile_hookSTAR);
	SYMBOL_EXPORT_SC_(CorePkg,implicit_compile_hook_default);
	Defun(implicit_compile_hook_default);
	_sym_STARimplicit_compile_hookSTAR->defparameter(_sym_implicit_compile_hook_default->symbolFunction());

#ifdef EXPOSE_DLLOAD
	SYMBOL_SC_(CorePkg,dlload);
	Defun(dlload);
#endif

	SYMBOL_SC_(CorePkg,dlopen);
	Defun(dlopen);

	SYMBOL_SC_(CorePkg,dlsym);
	Defun(dlsym);

	SYMBOL_SC_(CorePkg,dladdr);
	Defun(dladdr);
        CoreDefun(callDlMainFunction);

	SYMBOL_SC_(CorePkg,loadBundle);
	CoreDefun(loadBundle);

        CoreDefun(applysPerSecond);
//        CoreDefun(globalFuncallCyclesPerSecond);
        CoreDefun(partialApplysPerSecond);
        CoreDefun(operationsPerSecond);
        CoreDefun(callsByValuePerSecond);
        CoreDefun(callsByConstantReferencePerSecond);
        CoreDefun(callsByPointerPerSecond);
        CoreDefun(startupImagePathname);
	CoreDefun(mangleName);
	SYMBOL_EXPORT_SC_(CorePkg,callWithVariableBound);
	CoreDefun(callWithVariableBound);
	cleavirPrimops::_sym_callWithVariableBound->setf_symbolFunction(_sym_callWithVariableBound->symbolFunction());
	
	CoreDefun(multipleValueFuncall);
	CoreDefun(multipleValueProg1_Function);
	CoreDefun(catchFunction);
	CoreDefun(throwFunction);
	CoreDefun(progvFunction);
    }


}; /* namespace */
