#define	DEBUG_LEVEL_FULL
       
//
// (C) 2004 Christian E. Schafmeister
//

#include <csignal>


#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "symbolToEnumConverter.h"
#include "symbolTable.h"
#include "builtInClass.h"
#include "stdClass.h"
#include "standardClass.h"
#include "structureClass.h"
#include "core/corePackage.h"
#include "ql.h"
#include "lispStream.h"
//#i n c l u d e "genericFunction.h"
#include "core/multipleValues.h"
#include "core/lambdaListHandler.h"
#include "core/reader.h"
#include "core/pointer.h"
#include "singleDispatchGenericFunction.h"
#include "singleDispatchMethod.h"
#include "evaluator.h"
#include "write_object.h"
#include "designators.h"
#include "instance.h"
#include "structureClass.h"
#include "structureObject.h"
#include "str.h"
#include "binder.h"
#include "pointer.h"
#include "wrappedPointer.h"
#include "debugger.h"
//#i n c l u d e "setfExpander.h"
#include "environment.h"
#include "primitives.h"
#include "conditions.h"

#ifdef	darwin
#include <stdint.h>
#include <mach/mach_time.h>
#else
#include <time.h>
#endif

#include "object.h"
#include "package.h"
#include "null.h"
#include "wrappers.h"


namespace reg {


    class_id allocate_class_id(type_id const& cls)
    {
        typedef std::map<type_id, class_id> map_type;
        static map_type registered;
        static class_id id = 0;

        std::pair<map_type::iterator, bool> inserted = registered.insert(
            std::make_pair(cls, id));

        if (inserted.second) {
//            printf("%s:%d allocate_class_id for %40s %ld\n", __FILE__, __LINE__, cls.name(), id );
            ++id;
        }

        return inserted.first->second;
    }



    void lisp_associateClassIdWithClassSymbol(class_id cid, core::Symbol_sp sym)
    {
        ASSERT(_lisp);
        if ( cid >= _lisp->classSymbolsHolder().size() ) {
            _lisp->classSymbolsHolder().resize(cid+1,core::lisp_symbolNil());
        }
        _lisp->classSymbolsHolder()[cid] = sym;
    }


    core::Symbol_sp lisp_classSymbolFromClassId(class_id cid)
    {
        core::Symbol_sp sym = _lisp->classSymbolsHolder()[cid];
        if (sym.nilp()) {
            return _Unbound<core::Symbol_O>();
        }
        return sym;
    }
};


    void lisp_errorDereferencedNil()
    {
        SIMPLE_ERROR(BF("Tried to dereference nil"));
    }

    void lisp_errorDereferencedUnbound()
    {
        SIMPLE_ERROR(BF("Tried to dereference unbound"));
    }

void lisp_errorUnexpectedType(class_id expectedTyp, class_id givenTyp, core::T_O* objP)
{
    ASSERT(_lisp);
    if ( expectedTyp >= _lisp->classSymbolsHolder().size() ) {
        core::lisp_error_simple(__FUNCTION__,__FILE__,__LINE__,boost::format("expected class_id %lu out of range max[%lu]") % expectedTyp % _lisp->classSymbolsHolder().size() );
    }
    core::Symbol_sp expectedSym = _lisp->classSymbolsHolder()[expectedTyp];
    if ( expectedSym.nilp() ) {
        core::lisp_error_simple(__FUNCTION__,__FILE__,__LINE__,boost::format("expected class_id %lu symbol was not defined") % expectedTyp );
    }

    if ( givenTyp >= _lisp->classSymbolsHolder().size() ) {
        core::lisp_error_simple(__FUNCTION__,__FILE__,__LINE__,boost::format("given class_id %lu out of range max[%lu]") % givenTyp % _lisp->classSymbolsHolder().size() );
    }
    core::Symbol_sp givenSym = _lisp->classSymbolsHolder()[givenTyp];
    if ( givenSym.nilp() ) {
        core::lisp_error_simple(__FUNCTION__,__FILE__,__LINE__,boost::format("given class_id %lu symbol was not defined") % givenTyp );
    }

    gctools::smart_ptr<core::T_O> obj(objP);
    TYPE_ERROR(obj,expectedSym);
}


#if defined(USE_MPS)
namespace gctools
{
    mps_ap_t	_global_automatic_mostly_copying_allocation_point;
    mps_ap_t	_global_automatic_mark_sweep_allocation_point;
};
#endif //!defined(USE_MPS)

extern "C"
{
/*
 * Store the global lisp object
 */
core::Lisp_sp _lisp;

    bool debug_mps = true;
};


void brcl_mps_debug_allocation(const char* poolName, void* base, void* client, int size, int kind)
{
    if (!debug_mps) return;
    if ( kind == 0 ) {
        printf("%s:%d brcl_mps_debug_allocation kind == 0  ----------------- !!!!\n", __FILE__, __LINE__ );
        __builtin_debugtrap();
    };

#if defined(USE_MPS)  && !defined(RUNNING_GC_BUILDER)
    const char* kindName = obj_name((gctools::GCKindEnum)(kind));
    printf("%s:%d brcl_mps_allocation poolName: %s  base: %p  client: %p  size: %3d  kind[%d/%s]  \n",
           __FILE__, __LINE__,
           poolName,
           base, client, size,
           kind, kindName);
#endif
}

void brcl_mps_debug_fix1_before(void* base, void* smartAddr)
{
#if defined(USE_MPS)
    if (!debug_mps) return;
    printf("brcl_mps_debug_fix1_before  base: %p  smartAddr: %p\n", base, smartAddr );
#endif
}

    void brcl_mps_debug_fix_before(void* pbase, void* px, int offset)
    {
#if defined(USE_MPS)
#if defined(DEBUG_LOG_MPS_KINDS)
    if (!debug_mps) return;
	gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(pbase);
	const char* kindName = gctools::obj_name((gctools::GCKindEnum)(header->kind._Kind));
	printf("brcl_mps_debug_fix_before   pbase: %p  px: %p   kind: %s\n", pbase, px, kindName);
#else
	printf("brcl_mps_debug_fix_before   pbase: %p  px: %p\n", pbase, px);
#endif
#endif
    }

void brcl_mps_debug_fix_after(void* pbase, void* px)
    {
#if defined(USE_MPS)
    if (!debug_mps) return;
	printf("brcl_mps_debug_fix_after    pbase: %p  px: %p\n", pbase, px);
#endif
    }

void brcl_mps_debug_scan_object(gctools::GCObject* dobj)
{
#if defined(USE_MPS)
    if (!debug_mps) return;
    if ( core::T_O* tobj_gc_safe = dynamic_cast<core::T_O*>(dobj) )
    {
        gctools::smart_ptr<core::T_O> tsp(tobj_gc_safe);
	printf("brcl_mps_debug_scan_object   T_O*: %p\n", tobj_gc_safe );
    }
#endif
}

void brcl_mps_debug_container(const char* ctype, const char* name, int size)
{
#if defined(USE_MPS)
    if (!debug_mps) return;
    printf("brcl_mps_debug_container   type: %s   name: %s    size: %d\n",
	   ctype, name, size );
#endif
};



namespace boost
{
    using namespace core;
    void assertion_failed(char const* expr, char const* function, char const* file, long line )
    {
	THROW_HARD_ERROR(BF("A BOOST assertion failed"));
    }
};


namespace llvm_interface
{

    ::llvm_interface::llvmAddSymbolCallbackType addSymbol = NULL;

};



/*! A Symbol that is always undefined */
//core::Symbol_sp	_global_undefined_symbol;


    void dbg_hook(const char* error)
    {
	// Do nothing
	// set a break point here to catch every error
	//
	printf("\n\n%s\n%s:%d dbg_hook(...) was called\n",error,__FILE__,__LINE__);

	af_invokeInternalDebugger(_Nil<core::T_O>());
    }



namespace core
{

    int _global_signalTrap = 0;

    
    void lisp_processSignal(int signo)
    {
        SET_SIGNAL(0);
        if (signo == SIGINT) {
            printf("You pressed Ctrl+C\n");
            try {
                core::eval::funcall(cl::_sym_break,core::Str_O::create("Break on Ctrl+C"));
            } catch (...) {
                throw;
            }
//    af_invokeInternalDebugger(_Nil<core::T_O>());
            printf("Resuming after Ctrl+C\n");
        } else if ( signo == SIGCHLD ) {
//            printf("A child terminated\n");
        } else if ( signo == SIGABRT ) {
            printf("ABORT was called!!!!!!!!!!!!\n");
            af_invokeInternalDebugger(_Nil<core::T_O>());
//    core:eval::funcall(cl::_sym_break,core::Str_O::create("ABORT was called"));
        }
    }


    void lisp_pushClassSymbolOntoSTARallCxxClassesSTAR(Symbol_sp classSymbol) {
        if ( _sym_STARallCxxClassesSTAR->symbolValueUnsafe() ) {
            _sym_STARallCxxClassesSTAR->setf_symbolValue(Cons_O::create(classSymbol,_sym_STARallCxxClassesSTAR->symbolValue()));
        }
    };

    void lisp_symbolSetSymbolValue(Symbol_sp sym, T_sp val)
    {
        sym->setf_symbolValue(val);
    }



    Symbol_sp lisp_symbolNil()
    {
        return _Nil<Symbol_O>();
    }


    bool lisp_search(T_sp seq, T_sp obj, int& index)
    {
        if ( seq.nilp() ) {
            return false;
        } else if ( Vector_sp vec = seq.asOrNull<Vector_O>() ) {
            for ( int i(0), iEnd(vec->dimension()); i<iEnd; ++i ) {
                if ( vec->elt(i) == obj ) {
                    index = i;
                    return true;
                }
            }
            return false;
        }
        SIMPLE_ERROR(BF("Add support for lisp_search to search %s") % _rep_(seq));
    }



#if 0
    extern "C"
    {
	int	__mb_cur_max;
	const unsigned short* _pctype;
	int errno;
	double	_HUGE;
	int	_crtDbgFlag;
    }
#endif



    
    
#define ARGS_af_lispifyName "(name)"
#define DECL_af_lispifyName ""
#define DOCS_af_lispifyName "lispifyName"
    Str_sp af_lispifyName(Str_sp name)
    {_G();
        if ( name.nilp() ) return _Nil<Str_O>();
        string lispified = lispify_symbol_name(name->get());
        return Str_O::create(lispified);
    };


    MultipleValues* lisp_multipleValues()
    {
	return &(_lisp->multipleValues());
    }




    void errorFormatted(boost::format fmt)
    {
	TRY_BOOST_FORMAT_STRING(fmt,fmt_str);
	dbg_hook(fmt_str.c_str());
    }

    void errorFormatted(const string& msg)
    {
	dbg_hook(msg.c_str());
    }

    void errorFormatted(const char* msg)
    {
	dbg_hook(msg);
    }

    string lisp_currentPackageName()
    {
        string pkg = _lisp->getCurrentPackage()->packageName();
        return pkg;
    }

    Symbol_sp lispify_intern_keyword(string const& name)
    {
	string lispName = lispify_symbol_name(name);
	return _lisp->internKeyword(lispName);
    }

#if 0
    Symbol_sp lispify_intern(string const& name, string const& packageName)
    {
	string lispName = lispify_symbol_name(name);
	return _lisp->intern(lispName,packageName);
    }
    Symbol_sp lispify_intern_export(string const& name, string const& packageName)
    {
        Symbol_sp sym = lispify_intern(name,packageName);
        sym->exportYourself();
        return sym;
    }
#endif
    Symbol_sp lisp_upcase_intern(string const& name, string const& packageName)
    {
	string lispName = stringUpper(name);
	return _lisp->intern(lispName,packageName);
    }

    Symbol_sp lisp_upcase_intern_export(string const& name, string const& packageName)
    {
        Symbol_sp sym = lisp_upcase_intern(name,packageName);
        sym->exportYourself();
        return sym;
    }

    bool lispify_match(const char*& cur, const char* match )
    {
	const char* ccur = cur;
	while ( *match )
	{
	    if ( *ccur == '\0' ) return false;
	    if ( *ccur != *match ) return false;
	    ++ccur;
	    ++match;
	}
	cur = ccur;
	return true;
    }


    string lispify_symbol_name(const string& s)
    {_G();
	LOG(BF("lispify_symbol_name pass1 source[%s]") % s );
	stringstream stream_pass1;
	const char* start_pass1 = s.c_str();
	const char* cur = start_pass1;
	while (*cur)
	{
	    if ( lispify_match(cur,"_SHARP_")) { stream_pass1 << "#"; continue;}
	    if ( lispify_match(cur,"_BANG_")) { stream_pass1 << "!"; continue;}
	    if ( lispify_match(cur,"_ATSIGN_")) { stream_pass1 << "@"; continue;}
	    if ( lispify_match(cur,"_COMMA_")) { stream_pass1 << ","; continue;}
	    if ( lispify_match(cur,"_DIVIDE_")) { stream_pass1 << "/"; continue;}
	    if ( lispify_match(cur,"_MINUS_")) { stream_pass1 << "-"; continue;}
	    if ( lispify_match(cur,"_TIMES_")) { stream_pass1 << "*"; continue;}
	    if ( lispify_match(cur,"_SLASH_")) { stream_pass1 << "/"; continue;}
	    if ( lispify_match(cur,"PERCENT")) {stream_pass1 << "%"; continue;}
	    if ( lispify_match(cur,"_PLUS_")) { stream_pass1 << "+"; continue;}
	    if ( lispify_match(cur,"_EQ_")) {stream_pass1 << "="; continue;} 
	    if ( lispify_match(cur,"_NE_")) {stream_pass1 << "/="; continue;}
	    if ( lispify_match(cur,"_LT_")) {stream_pass1 << "<"; continue;}
	    if ( lispify_match(cur,"_GT_")) {stream_pass1 << ">"; continue;}
	    if ( lispify_match(cur,"_LE_")) {stream_pass1 << "<="; continue;}
	    if ( lispify_match(cur,"_GE_")) {stream_pass1 << ">="; continue;}
	    if ( lispify_match(cur,"STAR")) {stream_pass1 << "*"; continue;}
	    if ( lispify_match(cur,"AMP")) {stream_pass1 << "&"; continue;}
	    if ( lispify_match(cur,"_")) {stream_pass1 << "-"; continue;}
	    stream_pass1 << *cur;
	    ++cur;
	}
	stringstream stream_pass2;
	string str_pass2 = stream_pass1.str();
	LOG(BF("lispify_symbol_name pass2 source[%s]") % str_pass2 );
	const char* start_pass2 = str_pass2.c_str();
	cur = start_pass2;
	while (*cur)
	{
	    if ( islower(*cur) && isalpha(*(cur+1)) && isupper(*(cur+1)))
	    {
		char lcur = *cur;
		char ucur = toupper(lcur);
		stream_pass2 << ucur << "-";
		++cur;
		continue;
	    }
	    stream_pass2 << (char)(toupper(*cur));
	    ++cur;
	}
	LOG(BF("lispify_symbol_name output[%s]") % stream_pass2.str() );
	return stream_pass2.str();
    }


    string symbol_symbolName(Symbol_sp sym)
    {
	return sym->symbolNameAsString();
    }

    string symbol_packageName(Symbol_sp sym)
    {
        Package_sp p = sym->homePackage();
        if ( p.nilp() ) {
            return "";
        }
        return p->packageName();
    }

    string symbol_repr(Symbol_sp sym)
    {
	return _rep_(sym);
    }


    /* If o is an instance of Instance_O or Class_O then it returns o->_instanceClass()
       Otherwise it returns lisp_static_class(o)
    */
    Class_sp lisp_instance_class(T_sp o)
    {
	if (o.nilp())
	{
	    return core::Null_O::___staticClass;
	} else if (!o)
	{
	    SIMPLE_ERROR(BF("You cannot get the class of NULL"));
	} else if (o.unboundp())
	{
	    SIMPLE_ERROR(BF("You cannot get the class of UNBOUND"));
	} else if (o._NULLp())
	{
	    SIMPLE_ERROR(BF("You cannot get the class of _NULL"));
	} else if (Instance_sp iobj = o.asOrNull<Instance_O>() )
	{
	    return iobj->_instanceClass();
        } else if (WrappedPointer_sp exobj = o.asOrNull<WrappedPointer_O>() )
        {
            return exobj->_instanceClass();
//#ifndef CLOS
	} else if ( StructureObject_sp sobj = o.asOrNull<StructureObject_O>() )
	{
	    IMPLEMENT_MEF(BF("structureType returns a T_sp but I need a Class_sp - What do I return here????"));
//	    return sobj->structureType();
//#endif
	} else if ( Class_sp cobj = o.asOrNull<Class_O>() )
	{
	    return cobj->_instanceClass();
	}
	return lisp_static_class(o);
    }


    Class_sp lisp_static_class(T_sp o)
    {
	if (o.nilp()) return core::Null_O::___staticClass;
	if (o.unboundp())
	{
	    SIMPLE_ERROR(BF("You cannot get the class of UNBOUND"));
	} else if (o._NULLp())
	{
	    SIMPLE_ERROR(BF("You cannot get the class of _NULL"));
	} else if (!o)
	{
	    SIMPLE_ERROR(BF("You cannot get the class of (0)"));
	}
	return o->__class();
    }
	

    bool lisp_fixnumP(T_sp o)
    {
        if (o.base_fixnump()) return true;
	return af_fixnumP(o);
    }

    Fixnum lisp_asFixnum(T_sp o)
    {
        if (o.base_fixnump()) return o.fixnum();
        if (af_fixnumP(o)) return o.as<Fixnum_O>()->get();
        SIMPLE_ERROR(BF("Not fixnum %s") % _rep_(o));
    }

    bool lisp_characterP(T_sp o)
    {
	return af_characterP(o);
    }


    T_sp lisp_apply(T_sp funcDesig, ActivationFrame_sp frame )
    {_G();
	return eval::applyToActivationFrame(funcDesig,frame);
    }


#if 0
    string lisp_convertCNameToLispName(string const& cname, bool convertUnderscoreToDash)
    {_G();
	if ( convertUnderscoreToDash )
	{
	    string lispName = searchAndReplaceString(cname,"_","-",_lisp);
	    return lispName;
	}
	return cname;
    }
#endif

    string _rep_(T_sp obj)
    {
#define USE_WRITE_OBJECT
#if defined(USE_WRITE_OBJECT)
	StringOutStream_sp sout = StringOutStream_O::make();
	write_object(obj,sout);
	return sout->str();
#else
	if ( obj.nilp() )
	{
	    return "nil";
	} else if ( obj.unboundp() )
	{
	    return "!UNBOUND!";
	} else if ( obj._NULLp() )
	{
	    return "!NULL!";
        } else if ( obj.fixnump() ) {
            stringstream ss;
            ss << obj.fixnum();
            return ss.str();
        } else if ( obj.framep() ) {
            stringstream ss;
            ss << "Frame@" << (void*)(obj.frame());
	} else if ( !obj )
	{
	    return "!!!UNDEFINED!!!";
	}
	return obj->__repr__();   // This is the only place where obj->__repr__() is allowed
#endif
    }


    void lisp_throwUnexpectedType(T_sp offendingObject, Symbol_sp expectedTypeId )
    {_G();
	Symbol_sp offendingTypeId = offendingObject->_instanceClass()->className();
	SIMPLE_ERROR(BF("Expected %s of class[%s] to be subclass of class[%s]") % _rep_(offendingObject) % _rep_(offendingTypeId) % _rep_(expectedTypeId));
    }


    string lisp_classNameAsString(Class_sp c)
    {
	return c->classNameAsString();
    }

    void lisp_throwLispError( const string& str)
    {_G()
	    SIMPLE_ERROR(BF("%s") % str );
    }

    void lisp_throwLispError( const boost::format& fmt)
    {_G();
	TRY_BOOST_FORMAT_STRING(fmt,fmt_str);
	SIMPLE_ERROR(BF(fmt_str));
    }

    bool lisp_debugIsOn(const char* fileName,uint debugFlags)
    {
	bool ret = _lisp->debugLog().isOn(fileName,debugFlags);
	return ret;
    }

    DebugStream* lisp_debugLog()
    {
	return &(_lisp->debugLog());
    }


    uint lisp_hash(uintptr_t x) 
    {
        HashGenerator hg;
        hg.addPart(x);
        return hg.hash();
    }

    T_sp lisp_true()
    {
	return _lisp->_true();
    }


    T_sp lisp_false()
    {
	return _lisp->_false();
    }


    T_sp lisp_ocar(Cons_sp args)
    {_G();
	return oCar(args);
    }

    T_sp lisp_ocadr(Cons_sp args)
    {_G();
	return oCadr(args);
    }

    T_sp lisp_ocaddr(Cons_sp args)
    {_G();
	return oCaddr(args);
    }


    bool lisp_CoreBuiltInClassesInitialized()
    {_G();
	return _lisp->CoreBuiltInClassesInitialized();
    }

    bool lisp_BuiltInClassesInitialized()
    {_G();
	return _lisp->BuiltInClassesInitialized();
    }

#if 0
    bool lisp_NilsCreated()
    {_G();
	return lisp->NilsCreated();
    }


    bool internal_isTrue(const void* T_spPtr)
    {
	T_sp o = *(T_sp*)(T_spPtr);
	if ( o.nilp() ) return false;
	return o.isTrue();
    }
#endif



    void	lisp_exposeClass(const string& className, ExposeCandoFunction exposeCandoFunction, ExposePythonFunction exposePythonFunction)
    {_G();
	DEPRECIATED();
//    ASSERTP(lisp.notnilp(),"In lisp_exposeClass env can not be nil");
	bool exposed = false;
	{_BLOCK_TRACE("Invoking exposer static method");
	    if ( exposeCandoFunction != NULL )
	    {
		(exposeCandoFunction)(_lisp);
		exposed = true;
	    }
	}
    }


    Class_sp lisp_boot_findClassBySymbolOrNil(Symbol_sp classSymbol)
    {_G();
	Class_sp mc = eval::funcall(cl::_sym_findClass,_lisp->_true()).as<Class_O>();
	return mc;
    }


// void lisp_defineInitializationArgumentsForClassSymbol(Lisp_sp lisp, const string& argumentString, uint classSymbol)
// {_G();
//     Class_sp mc = lisp->classFromClassSymbol(classSymbol);
//     mc->__setLambdaListHandlerString(argumentString);
// }


    void lisp_addClass(Symbol_sp classSymbol,
		       Creator* cb,
		       Symbol_sp base1ClassSymbol,
		       Symbol_sp base2ClassSymbol,
		       Symbol_sp base3ClassSymbol)
    {_G();
	_lisp->addClass(classSymbol,cb,base1ClassSymbol,base2ClassSymbol);
    }

    void lisp_addClass(Symbol_sp classSymbol )
    {_G();
        DEPRECIATED();
//	_lisp->addClass(classSymbol);
    }


    void lisp_addClassAndInitialize(Symbol_sp classSymbol,
                                    Creator* cb,
		       Symbol_sp base1ClassSymbol,
		       Symbol_sp base2ClassSymbol,
		       Symbol_sp base3ClassSymbol)
    {_G();
	_lisp->addClass(classSymbol,cb,base1ClassSymbol,base2ClassSymbol);
    }

    Cons_sp lisp_parse_arguments(const string& packageName, const string& args)
    {_G();
	if ( args == "" ) return _Nil<Cons_O>();
	Package_sp pkg = _lisp->findPackage(packageName);
	ChangePackage changePackage(pkg);
	Stream_sp str = StringInputStream_O::create(args);
	Reader_sp reader = Reader_O::create(str);
	T_sp osscons = reader->primitive_read(true,_Nil<T_O>(),false);
	Cons_sp sscons = osscons.as_or_nil<Cons_O>();
	return sscons;
    }


    Cons_sp lisp_parse_declares(const string& packageName, const string& declarestring)
    {_G();
	if ( declarestring == "" ) return _Nil<Cons_O>();
	Package_sp pkg = _lisp->findPackage(packageName);
	ChangePackage changePackage(pkg);
	Stream_sp str = StringInputStream_O::create(declarestring);
	Reader_sp reader = Reader_O::create(str);
	Cons_sp sscons = reader->primitive_read(true,_Nil<T_O>(),false).as_or_nil<Cons_O>();
	return sscons;
    }


    LambdaListHandler_sp lisp_function_lambda_list_handler(Cons_sp lambda_list, Cons_sp declares, std::set<int> pureOutValues )
    {_G();
	LambdaListHandler_sp llh = LambdaListHandler_O::create(lambda_list,declares,cl::_sym_function, pureOutValues);
	return llh;
    }



    void lisp_defineSingleDispatchMethod(Symbol_sp sym,
					 Symbol_sp classSymbol,
					 BuiltinClosure* methoid,
                                         int TemplateDispatchOn,
					 const string& arguments,
					 const string& declares,
					 const string& docstring,
					 bool autoExport,
					 int number_of_required_arguments,
                                         const std::set<int> pureOutIndices )
    {_G();
	Class_sp receiver_class = eval::funcall(cl::_sym_findClass,classSymbol,_lisp->_true()).as<Class_O>();
	Symbol_sp className = receiver_class->name();
#if 1
	if ( sym->symbolName()->get().find("JSONDATABASE-LOAD-FROM-FILE") != string::npos )
	{
            printf("%s:%d - Caught lisp_defineSingleDispatchMethod for %s\n", __FILE__, __LINE__, sym->symbolName()->get().c_str() );
	}
#endif
	Cons_sp ldeclares = lisp_parse_declares(className->getPackage()->getName(),declares);
	// NOTE: We are compiling the llhandler in the package of the class - not the package of the
	// method name  -- sometimes the method name will belong to another class (ie: core:--init--)
	LambdaListHandler_sp llhandler;
	if ( arguments == "" && number_of_required_arguments >= 0 )
	{
	    // If the arguments string is empty and number_of_required_arguments is >= 0 then create
	    // a LambdaListHandler that supports that number of required arguments
	    llhandler = LambdaListHandler_O::create(number_of_required_arguments,pureOutIndices);
	} else if ( arguments != "" )
	{
	    Cons_sp llraw = lisp_parse_arguments(className->getPackage()->getName(),arguments);
	    Cons_sp llproc;
	    /*-----*/
	    MULTIPLE_VALUES_CONTEXT();
	    Cons_mv mv_llprocessed = LambdaListHandler_O::process_single_dispatch_lambda_list(llraw,true);
	    llproc = mv_llprocessed; // slice
	    Symbol_sp sd_symbol = mv_llprocessed.valueGet(1).as<Symbol_O>();
	    Symbol_sp sd_class_symbol = mv_llprocessed.valueGet(2).as<Symbol_O>();
	    /*-----*/

	    if ( sd_class_symbol.notnilp() && sd_class_symbol != classSymbol )
	    {
		SIMPLE_ERROR(BF("Mismatch between hard coded class[%s] and"
                                " lambda-list single-dispatch argument class[%s] in argument list: %s")
                             % _rep_(classSymbol) % _rep_(sd_class_symbol) % _rep_(llraw) );
	    }
	    llhandler = lisp_function_lambda_list_handler(llproc,ldeclares,pureOutIndices);
	    if ( sd_symbol.notnilp() )
	    {
		int single_dispatch_argument_index = llhandler->single_dispatch_on_argument(sd_symbol);
                if (single_dispatch_argument_index != 0 ) {
                    SIMPLE_ERROR(BF("There is no support for dispatching on anything but the first argument -"
                                    " wrap this virtual function in a regular function and do the dispatch yourself  %s::%s") % _rep_(className) % _rep_(sym) );
                }
	    }
	} else
	{
	    SIMPLE_ERROR(BF("No arguments were provided and number_of_required_arguments is %d") % number_of_required_arguments);
	}
        if ( TemplateDispatchOn != 0 ) {
            SIMPLE_ERROR(BF("Mismatch between single_dispatch_argument_index[0] from lambda_list and TemplateDispatchOn[%d] for class %s  method: %s  lambda_list: %s")
                         % TemplateDispatchOn
                         % _rep_(classSymbol) % _rep_(sym) % arguments );
        }
	if ( autoExport) sym->exportYourself();
	LOG(BF("Interned method in class[%s]@%p with symbol[%s] arguments[%s] - autoexport[%d]")
	    % receiver_class->instanceClassName() % (receiver_class.get()) % sym->fullName() % arguments % autoExport );
	Str_sp docStr = Str_O::create(docstring);
	SingleDispatchGenericFunction_sp gfn = af_ensureSingleDispatchGenericFunction(sym,llhandler);
	SYMBOL_SC_(KeywordPkg,body);
	SYMBOL_SC_(KeywordPkg,lambda_list_handler);
	SYMBOL_SC_(KeywordPkg,docstring);
	LOG(BF("Attaching single_dispatch_method symbol[%s] receiver_class[%s]  methoid@%p")
	    % _rep_(sym) % _rep_(receiver_class) %  ((void*)(methoid)) );
        methoid->finishSetup(llhandler,kw::_sym_function);
        Function_sp fn = Function_O::make(methoid);
	af_ensureSingleDispatchMethod(sym,receiver_class,llhandler,ldeclares,docStr,fn);
    }



    void	lisp_throwIfBuiltInClassesNotInitialized()
    {
	_lisp->throwIfBuiltInClassesNotInitialized();
    }

    string	lisp_classNameFromClassSymbol( Symbol_sp classSymbol )
    {_G();
	return _lisp->classNameFromClassSymbol(classSymbol);
    }

    Class_sp lisp_classFromClassSymbol(Symbol_sp classSymbol)
    {_G();
	return eval::funcall(cl::_sym_findClass,classSymbol,_lisp->_true()).as<Class_O>();
    }



    /*! If the name has the structure XXX:YYY or XXX::YYY then intern YYY in package XXX either
      exported or not respectively.   If there is no package prefix then use the defaultPackageName */
    Symbol_sp lispify_intern( const string& name, const string& defaultPackageName, bool exportSymbol)
    {
	string lispName = lispify_symbol_name(name);
#if 0
	// Trap the definition of specific functions here
	// sometimes I accidentally define things more than once and
	// we can't have that - so this will trap the first definition of 
	// a particular signal
	if ( lispName == "CREATE-COMPILE-UNIT")
	{
	    printf("%s:%d defining %s - break here to trap\n", __FILE__,__LINE__, lispName.c_str() );
	}
#endif
	Symbol_sp sym = _lisp->internWithDefaultPackageName(defaultPackageName,lispName);
        if (exportSymbol) {
            sym->exportYourself();
        }
        return sym;
    }

    void lisp_defun(Symbol_sp sym,
                    const string& packageName,
		    BuiltinClosure* fc,
		    const string& arguments,
		    const string& declarestring,
		    const string& docstring,
                    const string& sourceFile,
                    int lineNumber,
		    bool autoExport,
		    int number_of_required_arguments,
                    const std::set<int>& skipIndices )
    {_G();
	if ( sym->getReadOnlyFunction() )
	{
	    printf( "%s:%d - The symbol[%s] has already been assigned a function and will not be redefined\n", __FILE__, __LINE__, _rep_(sym).c_str() );
	    return;
	}
	Cons_sp ldeclares = lisp_parse_declares(packageName,declarestring);
	LambdaListHandler_sp llh;
	if ( (arguments == "" || arguments=="()") && number_of_required_arguments >= 0)
	{
	    llh = LambdaListHandler_O::create(number_of_required_arguments, skipIndices);
            if ( skipIndices.size() > 0 ) {
                stringstream ss;
                for ( auto i: skipIndices) { ss << i << " "; }
                printf("%s:%d number_of_required_arguments=%d  skip_indices = %s    llh = %s\n",
                       __FILE__, __LINE__, number_of_required_arguments, ss.str().c_str(), _rep_(llh).c_str() );
            }
	} else
	{
	    Cons_sp ll = lisp_parse_arguments(packageName,arguments);
	    llh = lisp_function_lambda_list_handler(ll,_Nil<Cons_O>(), skipIndices);
	}
        fc->finishSetup(llh,kw::_sym_function);
        fc->setSourcePosInfo(Str_O::create(sourceFile),lineNumber,0);
	Function_sp func = Function_O::make(fc);
	sym->setf_symbolFunction(func);
	if ( autoExport ) sym->exportYourself();
	else sym->setReadOnlyFunction(false);
    }



    void lisp_defmacro( Symbol_sp sym,
                        const string& packageName,
			BuiltinClosure* f,
			const string& arguments,
			const string& declarestring,
			const string& docstring,
			bool autoExport )
    {_G();
	LOG(BF("Adding form[%s] with arguments[%s]") % name % arguments );
	if ( sym->getReadOnlyFunction() )
	{
	    printf( "%s:%d - The symbol[%s] has already been assigned a function and will not be redefined\n", __FILE__, __LINE__, _rep_(sym).c_str() );
	    return;
	}
	Cons_sp ll = lisp_parse_arguments(packageName,arguments);
	Cons_sp ldeclares = lisp_parse_declares(packageName,declarestring);
	LambdaListHandler_sp llh = lisp_function_lambda_list_handler(ll,_Nil<Cons_O>());
        f->finishSetup(llh,kw::_sym_macro);
	Function_sp func = Function_O::make(f);
//    Package_sp package = lisp->getPackage(packageName);
//    package->addFunctionForLambdaListHandlerCreation(func);
	sym->setf_symbolFunction(func);
	if ( autoExport ) sym->exportYourself();
    }


    void lisp_defgeneric(const string& packageName,
			 const string& cname,
			 Functoid* f,
			 const string& arguments,
			 const string& docstring,
			 bool autoExport )
    {_G();
	// Remember to lock the function name
	IMPLEMENT_MEF(BF("implement-defgeneric"));
	string name = lispify_symbol_name(cname);
#if 0
	LOG(BF("Adding generic-function[%s:%s] with arguments[%s]") %packageName % name % arguments );
	string lispName = lisp_convertCNameToLispName(name,true);
	Symbol_sp sym = lisp->internWithPackageName(packageName,lispName);
	lisp->createPredefinedSymbol(symSymbol,sym);
	IMPLEMENT_MEF(BF("Switch to CompiledBody"));
	FunctionPrimitive_sp func = FunctionPrimitive_O::create(sym,f,arguments,""/*Docstring*/,lisp);
	sym->setf_symbolFunction(func);
	if ( autoExport ) sym->exportYourself();
#endif
    }




    Symbol_sp lisp_internKeyword(const string& name)
    {
	if ( name == "" ) return _Nil<Symbol_O>();
	return _lisp->internKeyword(name);
    }

    Symbol_sp lisp_intern(const string& name)
    {
	if ( name == "" ) return _Nil<Symbol_O>();
	return _lisp->intern(name);
    }

    Symbol_sp lisp_intern(const string& pkg, const string& name)
    {
	if ( name == "" ) return _Nil<Symbol_O>();
	return _lisp->internWithPackageName(pkg,name);
    }

#if 0
    Symbol_sp lisp_lookupSymbol(Lisp_sp lisp, Symbol_sp name)
    {
	return lisp->lookupPredefinedSymbol(name);
    }
#endif

    string symbol_fullName(Symbol_sp s)
    {
	return s->fullName();
    }



    core::SourcePosInfo_sp lisp_registerSourceInfo(T_sp obj
                                                   , SourceFileInfo_sp sfo
                                                   , int lineno
                                                   , int column )
    {
        SourceManager_sp db = _lisp->sourceDatabase();
        if ( db.notnilp() ) {
            return db->registerSourceInfo(obj,sfo,lineno,column);
        }
        return _Nil<SourcePosInfo_O>();
    }

    core::SourcePosInfo_sp lisp_registerSourceInfoFromStream(T_sp obj
                                                             , Stream_sp stream)
    {
        SourceManager_sp db = _lisp->sourceDatabase();
        if ( db.notnilp() ) {
            return db->registerSourceInfoFromStream(obj,stream);
        }
        return _Nil<SourcePosInfo_O>();
    }






    void lisp_installGlobalInitializationCallback(InitializationCallback initGlobals)
    {
	_lisp->installGlobalInitializationCallback(initGlobals);
    }

#if 0
    T_sp lisp_hiddenBinderLookup(Lisp_sp lisp, Symbol_sp sym)
    {_G();
	T_sp obj = lisp->hiddenBinder()->lookup(sym);
	return obj;
    }
#endif

    int lisp_lookupEnumForSymbol(Symbol_sp predefSymId, T_sp symbol )
    {
	SymbolToEnumConverter_sp converter = predefSymId->symbolValue().as<SymbolToEnumConverter_O>();
	return converter->enumIndexForSymbol(symbol.as<Symbol_O>());
    }


    Symbol_sp lisp_lookupSymbolForEnum(Symbol_sp predefSymId, int enumVal )
    {
	SymbolToEnumConverter_sp converter = predefSymId->symbolValue().as<SymbolToEnumConverter_O>();
	return converter->symbolForEnumIndex(enumVal);
    }


#if 0
    void lisp_hiddenBinderExtend(Lisp_sp lisp, Symbol_sp sym, T_sp obj)
    {
	lisp->hiddenBinder()->extend(sym,obj);
    }
#endif


    void lisp_extendSymbolToEnumConverter(SymbolToEnumConverter_sp conv, Symbol_sp const& name, Symbol_sp const& archiveName, int value )
    {
	conv->addSymbolEnumPair(name,archiveName,value);
    }




    bool lisp_isGlobalInitializationAllowed(Lisp_sp lisp)
    {
	return lisp->isGlobalInitializationAllowed();
    }

    void lisp_debugLogWrite( char const* fileName, const char* functionName, uint lineNumber, uint col, const boost::format& fmt, uint debugFlags)
    {
	TRY_BOOST_FORMAT_STRING(fmt,fmt_str);
	if ( debugFlags == DEBUG_SCRIPT )
	{
	    _lisp->debugLog().beginNode(DEBUG_TOPLEVEL,fileName,functionName,lineNumber,0,fmt_str);
	    _lisp->debugLog().endNode(DEBUG_TOPLEVEL);
	    return;
	}
	if ( debugFlags == DEBUG_SHOUT )
	{
	    _lisp->debugLog().beginNode(DEBUG_SHOUT,fileName,functionName,lineNumber,0,fmt_str);
	    _lisp->debugLog().endNode(DEBUG_SHOUT);
	    return;
	}
	_lisp->debugLog().beginNode(DEBUG_LOG,fileName,functionName,lineNumber,0,fmt_str);
	_lisp->debugLog().endNode(DEBUG_LOG);
    }


    void lisp_logException( const char* file, const char* fn, int line, const char* structure, T_sp condition)
    {
	string message;
	if ( CandoException_sp ce=condition.asOrNull<CandoException_O>() )
	{
	    message = ce->message();
	} else
	{
	    message = _rep_(condition);
	}
	_lisp->debugLog().beginNode(DEBUG_EXCEPTION,file,fn,line,0,message);
	_lisp->debugLog().endNode(DEBUG_EXCEPTION);
    }























#if 0
    void printv_prompt()
    {
	_lisp->print(BF("> ") );
    }


    void foundation_printv_write(const char* outputBuffer)
    {
	_lisp->outputStream() << outputBuffer;
    }

    void foundation_printv_writeChar(char outputChar)
    {
	_lisp->outputStream() << outputChar;
    }

    void foundation_printv_flush()
    {
	_lisp->outputStream().flush();
    }


    static vector<string>	printfPrefixStack;
    void printvPushPrefix(const string& prefix)
    {
	_lisp->printfPrefixStack().push_back(prefix);
    }



    void printvPopPrefix( )
    {
	_lisp->printfPrefixStack().pop_back();
    }


    void printvShowPrefix()
    {
	for ( vector<string>::iterator si=printfPrefixStack.begin();
	      si!=printfPrefixStack.end(); si++ )
	{
	    _lisp->printvWrite((*si).c_str());
	}
    }


    static bool printv_dangling_newline = true;

    void	printv( const char* fmt, ...) 
    {_G();
// # p r a g m a omp critical ( printv )
	{
            IMPLEMENT_MEF(BF("Make sure malloc works\n"));
	    va_list	arg_ptr;
	    char	*outBuffer;
	    char	*newOutBuffer;
	    char	*cp;
	    int	outBufferSize = 1024, n;
	    stringstream	serr;
	    if ( (outBuffer = (char*)malloc(outBufferSize)) == NULL ) {
		printf("Could not malloc buffer for printv\n");
		SIMPLE_ERROR(BF("Could not malloc buffer for printv"));
	    }
	    while ( 1 ) {
		va_start( arg_ptr, fmt );
		n = vsnprintf( outBuffer, outBufferSize-1, fmt, arg_ptr );
		va_end(arg_ptr);
		if ( n>-1 && n<outBufferSize ) break;
		if ( n >=0 ) {
		    outBufferSize = n + 1;
		} else {
		    outBufferSize *= 2;
		}
		if ( (newOutBuffer = (char*)realloc(outBuffer,outBufferSize) )==NULL ) {
		    free(outBuffer);
		    printf( "Could not realloc printv buffer\n");
		    SIMPLE_ERROR(BF("Could not realloc printv buffer"));
		}
		outBuffer = newOutBuffer;
	    }
	    int numChars = n;
	    int i=0;
	    cp = outBuffer;
	    for ( ; i<numChars; cp++,i++ )
	    {
		if ( printv_dangling_newline )
		{
		    printvShowPrefix();
		    printv_dangling_newline = false;
		}
		if ( *cp=='\n' )
		{
		    printv_dangling_newline = true;
		}
		_lisp->printvWriteChar(*cp);
	    }
	    _lisp->printvFlush();
	    free(outBuffer);
	}
    }


    void print( const string& str)
    {
	_lisp->print(BF("%s")%str);
    }


    void println( const string& str)
    {
	_lisp->print(BF("%s") %str.c_str());
    }

#endif


    string concatenateVectorStrings( VectorStrings strs )
    {
	string			conc;
	VectorStrings::iterator	vs;
	conc = "";
	for ( vs=strs.begin(); vs!=strs.end(); vs++ ) {
	    conc = conc + (*vs) + " ";
	}
	return conc;
    }

    void	appendVectorStrings( VectorStrings& app, VectorStrings strs )
    {
	VectorStrings::iterator	vs;
	for ( vs=strs.begin(); vs!=strs.end(); vs++ ) {
	    app.push_back(*vs);
	}
    }


    string	stripCharacters(const string& orig, const string& stripSet)
    {
	stringstream sout;
	stringstream sin;
	sin.str(orig);
	char c;
	while ( sin.good() )
	{
	    sin.get(c);
	    if ( stripSet.find(c) != string::npos ) continue;
	    if ( !sin.good() ) continue;
	    sout << c;
	}
	return sout.str();
    }



    string escapeWhiteSpace(const string& inp)
    {
	stringstream sout;
	stringstream sin(inp);
	char c;
	while ( 1 )
	{
	    sin.get(c);
	    if ( !sin.good()) break;
	    switch (c)
	    {
	    case ' ':
		sout << "\\s";
		break;
	    case '\n':
		sout << "\\n";
		break;
	    case '\t':
		sout << "\\t";
		break;
	    case '!':
		sout << "\\\\";
		break;
	    default:
		sout << c;
	    }
	};
	return sout.str();
    }


    string unEscapeWhiteSpace(string const& inp)
    {
	stringstream sout;
	stringstream sin(inp);
	char c;
	while (1)
	{
	    sin.get(c);
	    if ( !sin.good() ) break;
	    if ( c == '\\')
	    {
		sin.get(c);
		switch (c)
		{
		case 's':
		    sout << " ";
		    break;
		case 'n':
		    sout << std::endl;
		    break;
		case 't':
		    sout << "\t";
		    break;
		case '!':
		    sout << "\\";
		    break;
		default:
		    THROW_HARD_ERROR(BF("Illegal escaped char[%s]") % c );
		    break;
		}
	    } else
	    {
		sout << c;
	    }
	}
	return sout.str();
    }
	

    // Trim Both leading and trailing spaces
    string trimWhiteSpace( const string& str)
    {
	// Find the first character position after excluding leading blank spaces
	size_t startpos = str.find_first_not_of(" \t"); 
	// Find the first character position from reverse af
	size_t endpos = str.find_last_not_of(" \t"); 
	// if all spaces or empty return an empty string
	if(( string::npos == startpos ) || ( string::npos == endpos))
	{
	    return "";
	}
	return str.substr( startpos, endpos-startpos+1 );
    }


    Function_sp lisp_symbolFunction(Symbol_sp sym)
    {
	return sym->symbolFunction();
    }

    string lisp_symbolNameAsString(Symbol_sp sym)
    {
        if ( sym.nilp() ) return "NIL";
	return sym->symbolNameAsString();
    }

    T_sp lisp_createStr(const string& s)
    {_G();
	return Str_O::create(s);
    }

    T_sp lisp_createFixnum(int fn)
    {_G();
	return Fixnum_O::create(fn);
    }

    SourcePosInfo_sp lisp_createSourcePosInfo(const string& fileName, int lineno )
    {
        Str_sp fn = Str_O::create(fileName);
        SourceFileInfo_mv sfi_mv = af_sourceFileInfo(fn);
        int sfindex = sfi_mv.valueGet(1).as<Fixnum_O>()->get();
        return SourcePosInfo_O::create(sfindex,lineno,0);
    }

    T_sp lisp_createList(T_sp a1) {return Cons_O::create(a1,_Nil<T_O>());}
    T_sp lisp_createList(T_sp a1, T_sp a2) {return Cons_O::createList(a1,a2);};
    T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3) {return Cons_O::createList(a1,a2,a3);};
    T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4) {return Cons_O::createList(a1,a2,a3,a4);};
    T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4, T_sp a5) {return Cons_O::createList(a1,a2,a3,a4,a5);};
    T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4, T_sp a5, T_sp a6) {return Cons_O::createList(a1,a2,a3,a4,a5,a6);};
    T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4, T_sp a5, T_sp a6, T_sp a7) {return Cons_O::createList(a1,a2,a3,a4,a5,a6,a7);}
    T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4, T_sp a5, T_sp a6, T_sp a7, T_sp a8) {return Cons_O::createList(a1,a2,a3,a4,a5,a6,a7,a8);}

    void lisp_errorExpectedTypeSymbol(Symbol_sp typeSym, T_sp datum)
    {
	TYPE_ERROR(datum,typeSym);
    }



    void lisp_error_simple(const char* functionName, const char* fileName, int lineNumber, const boost::format& fmt)
    {
	stringstream ss;
	ss << "In " << functionName << " " << fileName << " line " << lineNumber << std::endl;
	ss << fmt.str();
	if ( !_sym_signalSimpleError->fboundp() )
	{
	    printf("%s:%d %s\n", __FILE__, __LINE__, ss.str().c_str() );
	    LispDebugger dbg;
	    dbg.invoke();
//	    af_error(CandoException_O::create(ss.str()),_Nil<Cons_O>());
	}
	SYMBOL_EXPORT_SC_(ClPkg,programError);
	eval::funcall(_sym_signalSimpleError,
		    cl::_sym_programError, 		//arg0
		    _Nil<T_O>(),		// arg1
		    Str_O::create(fmt.str()),	// arg2
		    _Nil<Cons_O>());
    }

    void lisp_error_condition(const char* functionName, const char* fileName, int lineNumber, T_sp baseCondition, T_sp initializers )
    {
	stringstream ss;
	ss << "In " << functionName << " " << fileName << " line " << lineNumber << std::endl << _rep_(baseCondition) << " :initializers " << _rep_(initializers) << std::endl;
	if ( !_sym_signalSimpleError->fboundp() )
	{
	    ss << "Error " << _rep_(baseCondition) << " initializers: " << _rep_(initializers)<< std::endl;
	    printf("%s:%d lisp_error_condition--->\n %s\n", __FILE__, __LINE__, ss.str().c_str() );
	    LispDebugger dbg;
	    dbg.invoke();
//	    af_error(CandoException_O::create(ss.str()),_Nil<Cons_O>());
	}
	eval::apply(_sym_signalSimpleError,
			baseCondition,
			_Nil<T_O>(),
			Str_O::create(ss.str()),
		    _Nil<Cons_O>(),
			initializers.as_or_nil<Cons_O>()
	    );
    }




    void lisp_error(T_sp datum, T_sp arguments )
    {
	if ( !cl::_sym_error->fboundp() )
	{
            stringstream ss;
	    ss << "Error " << _rep_(datum) << " initializers: " << _rep_(arguments)<< std::endl;
	    printf("%s:%d lisp_error_condition--->\n %s\n", __FILE__, __LINE__, ss.str().c_str() );
	    LispDebugger dbg;
	    dbg.invoke();
//	    af_error(CandoException_O::create(ss.str()),_Nil<Cons_O>());
	}
        Cons_sp cargs = arguments.as<Cons_O>();
	eval::apply(cl::_sym_error, datum, cargs );
    }


    string stringUpper(const string& s)
    {_G();
	LOG(BF("Converting string(%s) to uppercase")% s);
	stringstream ss;
	for ( uint si=0; si<s.length(); si++ )
	{
	    ss << (char)(toupper(s[si]));
	}
	LOG(BF("Returning stringUpper(%s)")% ss.str() );
	return ss.str();
    }

    string stringUpper(const char* s)
    {_G();
	LOG(BF("Converting const char*(%s) to uppercase")% s);
	stringstream ss;
	for ( ; *s; s++ )
	{
	    ss << (char)(toupper(*s));
	}
	LOG(BF("Returning stringUpper(%s)")% ss.str() );
	return ss.str();
    }



    T_sp lisp_ArgArrayToCons(int nargs, ArgArray args)
    {
	Cons_O::CdrType_sp first = _Nil<Cons_O::CdrType_O>();
        Cons_O::CdrType_sp* curP = &first;
//        gctools::StackRootedPointerToSmartPtr<Cons_O::CdrType_O> cur(&first);
	for ( int i(0); i<nargs; ++i ) {
	    Cons_sp one = Cons_O::create(args[i]);
            *curP = one; // cur.setPointee(one); //*cur = one;
	    curP = one->cdrPtr(); // cur.setPointer(one->cdrPtr()); // cur = one->cdrPtr();
	}
	return first;
    }


    vector<string> split(const string& str, const string& delimiters)
    {
	vector<string>	parts;
	tokenize(str,parts,delimiters);
	return parts;
    }



/*! DONT PUT DEBUGGING CODE IN TOKENIZE!!!!  IT IS USED BY DebugStream
 */
    void tokenize(const string& str,
		  vector<string>& tokens,
		  const string& delimiters)
    {
	tokens.erase(tokens.begin(),tokens.end());
	// Skip delimiters at beginning.
	string::size_type lastPos = str.find_first_not_of(delimiters, 0);
	// Find first "non-delimiter".
	string::size_type pos     = str.find_first_of(delimiters, lastPos);
	while (string::npos != pos || string::npos != lastPos) {
	    // Found a token, add it to the vector.
	    tokens.push_back(str.substr(lastPos, pos - lastPos));
	    // Skip delimiters.  Note the "not_of"
	    lastPos = str.find_first_not_of(delimiters, pos);
	    // Find next "non-delimiter"
	    pos = str.find_first_of(delimiters, lastPos);
	}
    }


    string	searchAndReplaceString( const string& str, const string& search, const string& replace, Lisp_sp lisp )
    {_G();
	string			result;
	string::size_type	pos = 0;
	result = str;
	while ( (pos = result.find(search,pos))!= string::npos )
	{
	    result.replace( pos, search.size(), replace );
	    pos++;
	}
	return result;
    }



    void queueSplitString(const string& str,
			  std::queue<string>& tokens,
			  const string& delimiters)
    {
//    LOG(BF("foundation.cc::tokenize-- Entered") );
	// Skip delimiters at beginning.
	string::size_type lastPos = str.find_first_not_of(delimiters, 0);
	// Find first "non-delimiter".
	string::size_type pos     = str.find_first_of(delimiters, lastPos);
	while (string::npos != pos || string::npos != lastPos) {
	    // Found a token, add it to the vector.
	    tokens.push(str.substr(lastPos, pos - lastPos));
	    // Skip delimiters.  Note the "not_of"
	    lastPos = str.find_first_not_of(delimiters, pos);
	    // Find next "non-delimiter"
	    pos = str.find_first_of(delimiters, lastPos);
	}
    }

//
//	Compare a wildcard containing string to a regular string
//
//	Wildcard strings can contain '*' or '?'
//
//	Return true if it matches and false if it doesn't
//
    bool	wildcmp(string const&	sWild,
			string const&	sRegular )
    {
	const char	*wild, *mp, *cp;
	const char	*regular;

	cp = NULL;
	mp = NULL;
	wild = sWild.c_str();
	regular = sRegular.c_str();
	while ((*regular) && (*wild != '*')) {
	    if ((*wild != *regular) && (*wild != '?')) return false;
	    wild++;
	    regular++;
	}

	while (*regular) {
	    if (*wild == '*') {
		if (!*++wild) return true;
		mp = wild;
		cp = regular+1;
	    } else if ((*wild == *regular) || (*wild == '?')) {
		wild++;
		regular++;
	    } else {
		wild = mp;
		regular = cp++;
	    }

	}

	while (*wild == '*') {
	    wild++;
	}
	return !*wild;
    }




    void	StringStack::pop()
    {
	HARD_ASSERT(this->parts.size()>0);
	this->parts.pop_back();
    };

    string	StringStack::all(const string& separator)
    {
	stringstream	ss;
	ss.str("");
	if ( this->parts.size() > 0 )
	{
	    ss << this->parts[0];
	}
	for (uint i=1;i<this->parts.size();i++){
	    ss<<separator;
	    ss<<this->parts[i];
	}
	return ss.str();
    };



    const char* trimSourceFilePathName(const char* longName)
    {
	if ( longName == NULL ) return NULL;
	const char* cp = longName+strlen(longName);
	while (cp > longName && *cp != '/' ) --cp;
	if ( *cp == '/' ) ++cp;
	return cp;
    }








    bool _ClassesAreInitialized = false;

    void throwIfClassesNotInitialized(const Lisp_sp& lisp)
    {
	if (!_ClassesAreInitialized)
	{
	    lisp->print(BF("Debug information was being written when classes have not yet been initialized"));
	    lisp->print(BF("This should never happen."));
	    lisp->print(BF( "Run with the debugger and use the following commands:" ));
	    lisp->print(BF( "l foundation.cc:1" ));
	    lisp->print(BF( "search throwIfClassesNotInitialized" ));
	    lisp->print(BF( "--> set a breakpoint in the if block" ));
	    lisp->print(BF( "Then backtrace to find the offending initialization routine." ));
	    exit(1);
	}
    }


#if 0
    void old_initializeCandoAndPython(Lisp_sp env)
    {_errorF();
#include "core_initScripting_inc.h"
    }
#ifdef	USEBOOSTPYTHON
#undef	USEBOOSTPYTHON
    void old_initializeCandoNoPython(Lisp_sp env)
    {_errorF();
#include "core_initScripting_inc.h"
    }
#define USEBOOSTPYTHON
#else
    void old_initializeCandoNoPython(Lisp_sp env)
    {_errorF();
#include "core_initScripting_inc.h"
    }
#endif
#endif

#if 0
    void initializeExposeClasses(bool exposeCando, bool exposePython )
    {_errorF();
	ClassManager::iterator	it;
	int			passes = 0;
	bool			exposedOne = true;
	while ( exposedOne )
	{
	    LOG(BF("initializeExposeClasses passes=%d") % passes  );
	    exposedOne = false;
	    for ( it=rootClassManager().begin(); it!=rootClassManager().end(); it++ )
	    {
		// If we have an Exposer defined then check if our BaseClass has
		// been exposed
		exposedOne = it->exposeYourself(exposeCando,exposePython);
	    }
	    passes++;
	    ASSERTP(passes<10, "There were more than 10 passes carried out when exposing classes");
	}
    }
#endif


    void initializeCandoScript(Lisp_sp lisp)
    {_G();
	DEPRECIATED();
    }

    void initializePythonScript(Lisp_sp lisp)
    {_G();
	DEPRECIATED();
//    initializeExposeClasses(false,true);
    }


#ifdef	USEBOOSTPYTHON
    __INITIALIZE_PYTHON(InitPython_Foundation)
    void InitPython_Foundation()
    {
	boost::python::def("print",&print);
	boost::python::def("println",&println);
	boost::python::def("printvPushPrefix",&printvPushPrefix);
	boost::python::def("printvPopPrefix",&printvPopPrefix);

    }
#endif


    void initialize_foundation()
    {

        Defun(lispifyName);
    };
    

};

