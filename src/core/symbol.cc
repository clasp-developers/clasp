/*
    File: symbol.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
#define	DEBUG_LEVEL_FULL

#include "useBoostPython.h"
#include "common.h"
#include "symbol.h"
#include "str.h"
#include "symbolTable.h"
#include "hashTable.h"
#include "numbers.h"
#include "lispList.h"
#include "package.h"
#include "lisp.h"
#include "wrappers.h"

// Print more information about the symbol
#define	VERBOSE_SYMBOLS	0

namespace core 
{

//    Symbol_sp 	_sym_nil;	// equivalent to _Nil<T_O>()
//    Symbol_sp 	_sym_t;		// equivalent to _lisp->_true()



    
    
#define ARGS_af_boundp "(arg)"
#define DECL_af_boundp ""
#define DOCS_af_boundp "boundp"
    bool af_boundp(Symbol_sp arg)
    {_G();
	if ( arg.nilp() ) return _lisp->_true();
	return arg->boundP();
    };



    
    
#define ARGS_af_symbolPackage "(arg)"
#define DECL_af_symbolPackage ""
#define DOCS_af_symbolPackage "symbolPackage"
    Package_sp af_symbolPackage(Symbol_sp arg)
    {_G();
	if ( arg.nilp() ) return _lisp->commonLispPackage();
	return arg->homePackage();
    };



    
    
#define ARGS_af_symbolFunction "(arg)"
#define DECL_af_symbolFunction ""
#define DOCS_af_symbolFunction "symbolFunction"
    Function_sp af_symbolFunction(Symbol_sp sym)
    {_G();
	if ( sym.nilp() ) return _Nil<Function_O>();
	return sym->symbolFunction();
    };



    
    
#define ARGS_af_symbolName "(arg)"
#define DECL_af_symbolName ""
#define DOCS_af_symbolName "symbolName"
    Str_sp af_symbolName(Symbol_sp arg)
    {_G();
	if ( arg.nilp() )
	{
	    return Str_O::create("NIL");
	}
	return arg->symbolName();
    }
    


    
    
#define ARGS_af_symbolValue "(arg)"
#define DECL_af_symbolValue ""
#define DOCS_af_symbolValue "symbolValue"
    T_sp af_symbolValue(const Symbol_sp arg)
    {_G();
	if ( arg.nilp() )
	{
	    return _Nil<T_O>();
	} else if ( arg.unboundp() )
	{
	    SIMPLE_ERROR(BF("Symbol %s is unbound") % _rep_(arg));
	}
	return arg->symbolValue();
    };
    

#define DOCS_af_make_symbol "make_symbol"
#define LOCK_af_make_symbol 1
#define ARGS_af_make_symbol "(name)"
#define DECL_af_make_symbol ""
    Symbol_mv af_make_symbol(Str_sp name)
    {_G();
	Symbol_sp sym = Symbol_O::create(name->get());
	return(Values(sym));
    };




    
    

#if 0
    Symbol_sp Symbol_O::create_classless_packageless(string const& name)
    {// no guard
	DEPRECIATED();
	ASSERTF(name[0] != '(',BF("Symbol names cannot start with paren[%s]") % name);
        GC_ALLOCATE(Symbol_O,sym);
	sym->_Name = Str_O::create(name);
	printf("%s:%d  Allocating packageless symbol @ %p name: %s\n", __FILE__, __LINE__, sym.px_ref(),name.c_str());
//	Symbol_sp sym = gctools::smart_ptr<Symbol_O>(new Symbol_O(name));
	return sym;
    }
#endif

    /*! Construct a symbol that is incomplete, it has no Class or Package */
    Symbol_O::Symbol_O(string const& name) : T_O() ,
					     _HomePackage(_Nil<Package_O>()),
					     _Value(_Unbound<T_O>()),
					     _Function(_Unbound<Function_O>()),
					     _IsSpecial(false),
					     _IsConstant(false),
					     _ReadOnlyFunction(false),
					     _PropertyList(_Nil<Cons_O>())
    {//no guard
	this->_Name = Str_O::create(name);
    }


    Symbol_O::Symbol_O() : Base(),
			   _Name(_Nil<Str_O>()),
			   _HomePackage(_Nil<Package_O>()),
			   _Value(_Unbound<T_O>()),
			   _Function(_Unbound<Function_O>()),
			   _IsSpecial(false),
			   _IsConstant(false),
			   _ReadOnlyFunction(false),
			   _PropertyList(_Nil<Cons_O>())
    {
	// nothing
    };


    Symbol_O::~Symbol_O()
    {
    };


    void Symbol_O::finish_setup(Package_sp pkg,bool exportp)
    {
	ASSERTF(pkg,BF("The package is UNDEFINED"));
	this->_HomePackage = pkg;
	pkg->add_symbol_to_package(this->symbolName()->get().c_str(),this->sharedThis<Symbol_O>(),exportp);
	this->_PropertyList = _Nil<Cons_O>();
	this->_Function = _Nil<Function_O>();
    }


    Symbol_sp	Symbol_O::create(const string& nm)
    {_G();
        // This is used to allocate roots that are pointed
        // to by global variable _sym_XXX  and will never be collected
        Symbol_sp n = gctools::GCObjectAllocator<Symbol_O>::rootAllocate();
	ASSERTF(nm!="",BF("You cannot create a symbol without a name"));
#if VERBOSE_SYMBOLS
	if ( nm.find("/dyn") != string::npos)
	{
	    THROW_HARD_ERROR(BF("Illegal name for symbol[%s]") % nm );
	}
#endif
	n->_Name = Str_O::create(nm);
	return n;
    };

    bool Symbol_O::boundP() const
    {
	return !this->_Value.unboundp();
    }

    void Symbol_O::makunbound()
    {
	this->_Value = _Unbound<T_O>();
    }


#if 0
    T_sp Symbol_O::evaluate(Cons_sp exp, Lisp_sp env)
    {_G();
	LOG(BF("Evaluating symbol: %s") % this->_Value.c_str()  );
	T_sp res = env->environment()->oget(this->sharedThis<Symbol_O>());
	// oget throws if variable is not found
	return res;
    };
#endif


    void Symbol_O::sxhash(HashGenerator& hg) const
    {_OF();
	Bignum bn = Str_O::stringToBignum(this->fullName().c_str());
	hg.addPart(bn);
    }



#define ARGS_Symbol_O_copy_symbol "(symbol &optional copy-properties)"
#define DECL_Symbol_O_copy_symbol ""
#define DOCS_Symbol_O_copy_symbol "copy_symbol"
    Symbol_sp Symbol_O::copy_symbol(T_sp copy_properties) const
    {_G();
	Symbol_sp new_symbol = Symbol_O::create(this->_Name->get());
	if ( copy_properties.isTrue() )
	{
	    ASSERT(this->_Value);
	    ASSERT(this->_Function);
	    new_symbol->_Value = this->_Value;
	    new_symbol->_Function = this->_Function;
	    new_symbol->_IsConstant = this->_IsConstant;
	    new_symbol->_ReadOnlyFunction = this->_ReadOnlyFunction;
	    new_symbol->_PropertyList = cl_copyList(this->_PropertyList).as_or_nil<Cons_O>();
	}
	return new_symbol;
    };



    bool Symbol_O::isKeywordSymbol()
    { 
	if ( !this->_HomePackage.pointerp() ) return false;
	return this->_HomePackage->isKeywordPackage();
    };


    bool Symbol_O::amp_symbol_p() const
    {
	return ( this->_Name->get()[0] == '&' );
    }


#if defined(OLD_SERIALIZE)
    void Symbol_O::serialize(serialize::SNode node)
    {_OF();
	if ( node->loading() )
	{
	    SIMPLE_ERROR(BF("You can't load symbols with serialize!!"));
	} else
	{
	    SIMPLE_ERROR(BF("You can't save symbols with serialize!!!"));
	}
    }
#endif

#if defined(XML_ARCHIVE)
    void	Symbol_O::archiveBase(ArchiveP node)
    {_G();
	if ( node->loading() )
	{
	    SIMPLE_ERROR(BF("You can't load symbols with archiveBase!! See Dumb_Node::createYourSymbol"));
	} else
	{
	    string name = this->formattedName(true);
	    node->attribute("_sym",name);
	}
    }
#endif // defined(XML_ARCHIVE)

    Symbol_sp Symbol_O::asKeywordSymbol()
    {_OF();
	if ( this->symbolNameAsString().find(":") != string::npos )
	{
	    SIMPLE_ERROR(BF("You are trying to convert the string[%s] into a keyword symbol and it contains colons") % this->symbolNameAsString());
	}
	if ( !this->_HomePackage.nilp() )
	{
	    if ( this->_HomePackage->isKeywordPackage() ) return this->sharedThis<Symbol_O>();
	}
	Symbol_sp kwSymbol = _lisp->internKeyword(this->symbolNameAsString());
	return kwSymbol;
    };





    T_sp Symbol_O::setf_symbolValue(T_sp val)
    {_OF();
	ASSERT(!this->_IsConstant);
#if 0 // I used this to test *print-pretty*
	// trap a change in a dynamic variable
	if ( this->_Name.as<Str_O>()->get() == "*PRINT-PRETTY*")
	{
            if ( val.notnilp() && _sym_STARenablePrintPrettySTAR->symbolValue().nilp() ) {
                // force *print-pretty* to nil if *enable-print-pretty* == nil 
                val = _Nil<T_O>();
                printf("forcing setf_symbolValue of *print-pretty* to nil because !core::*enable-print-pretty*\n" );
            }
	}
#endif
	this->_Value = val;
	ASSERTF(this->_Value,BF("In Symbol_O::setf_symbolValue symbol[%s] to illegal value: %s") % _rep_(this->sharedThis<Symbol_O>()) % _rep_(this->_Value));
	return val;
    }


    void Symbol_O::makeSpecial()
    {
	this->_IsSpecial = true;
    }


    void Symbol_O::makeConstant(T_sp val)
    {_G();
	this->_Value = val;
	ASSERT(this->_Value);
	this->_IsSpecial = true;
	this->_IsConstant = true;
    }

    T_sp Symbol_O::defconstant(T_sp val)
    {_OF();
	T_sp result = this->setf_symbolValue(val);
	this->_IsSpecial = true;
	this->setReadOnly(true);
	return result;
    }



    T_sp Symbol_O::defparameter(T_sp val)
    {_OF();
	T_sp result = this->setf_symbolValue(val);
	this->_IsSpecial = true;
	return result;
    }


    void Symbol_O::setf_symbolValueReadOnlyOverRide(T_sp val)
    {_OF();
	this->_IsConstant = true;
	this->_Value = val;
	ASSERT(this->_Value);
    }

    T_sp Symbol_O::symbolValue() const
    {
	ASSERTF(this->_Value,BF("NULL symbol-value for %s") % this->_Name->c_str() );
	if ( this->_Value.unboundp() )
	{
	    SIMPLE_ERROR(BF("Unbound symbol-value for %s") % this->_Name->c_str() );
	}
	return this->_Value;
    }

    T_sp Symbol_O::symbolValueUnsafe() const
    {
	return this->_Value;
    }





    Function_sp Symbol_O::symbolFunction()
    {_OF();
	return this->_Function;
    }

    void Symbol_O::setf_symbolFunction(Function_sp exec)
    {_OF();
	this->_Function = exec;
    }

    bool Symbol_O::fboundp() const
    {_OF();
	return this->_Function.pointerp();
    }


    string Symbol_O::symbolNameAsString() const
    {
	return this->_Name->get();
    }


    string Symbol_O::formattedName(bool prefixAlways) const
    {//no guard
	stringstream ss;
	if ( this->_HomePackage.nilp() )
	{
	    ss << "#:";
	    if ( this->_Name.pointerp() ) {
		ss << this->_Name->get();
	    } else
	    {
		ss << "---no-name---";
	    }
	}
	else
	{
	    Package_sp myPackage = this->_HomePackage;
	    if ( myPackage->isKeywordPackage() )
	    {
		ss << ":" << this->_Name->get();
	    } else
	    {
		Package_sp currentPackage = _lisp->getCurrentPackage();
		if ( (currentPackage.get() == myPackage.get()) && !prefixAlways )
		{
		    ss << this->_Name->get();
		} else
		{
		    if ( myPackage->isExported(this->const_sharedThis<Symbol_O>()) )
		    {
			ss << myPackage->getName() << ":" << this->_Name->get();
		    } else
		    {
			ss << myPackage->getName() << "::" << this->_Name->get();
		    }
		}
	    }
	}
// Sometimes its useful to add the address of the symbol to
// the name for debugging - uncomment the following line if you want that
#if 0
        ss << "@" << (void*)(this);
#endif

#if	VERBOSE_SYMBOLS
	if ( this->specialP() )
	{
	    ss << "/special";
	} else
	{
	    ss << "/lexical";
	}
#endif

	return ss.str();
    };

    bool Symbol_O::isExported()
    {
	Package_sp myPackage = this->getPackage();
	if ( myPackage.nilp() ) return false;
	return myPackage->isExported(this->sharedThis<Symbol_O>());
    }



    Symbol_sp Symbol_O::exportYourself(bool doit)
    {_G();
	if ( doit )
	{
	    if ( !this->isExported() )
	    {
		if ( this->_HomePackage.nilp() )
		{
		    SIMPLE_ERROR(BF("Cannot export - no package"));
		}
		Package_sp pkg = this->getPackage();
		if ( !pkg->isKeywordPackage())
		{   
		    if ( this->_Name->get() == "NIL" )
		    {
			// Debugging nil symbol - we could take this out now
			Symbol_sp nil_sym = this->sharedThis<Symbol_O>();
			Cons_sp list = Cons_O::create(nil_sym);
			pkg->_export(list);
		    } else
		    {
			pkg->_export(Cons_O::create(this->sharedThis<Symbol_O>()));
		    }
		}
	    }
	}
	return this->sharedThis<Symbol_O>();
    }



    T_sp Symbol_O::apply()
    {_G();
	ValueFrame_sp frame = ValueFrame_O::create(0,_Nil<ActivationFrame_O>());
	T_sp result = lisp_apply(this->sharedThis<Symbol_O>(),frame);
	return result;
    }
    T_sp Symbol_O::funcall()
    {_OF();
	ValueFrame_sp frame(ValueFrame_O::create(0,_Nil<ActivationFrame_O>()));
	T_sp result = lisp_apply(this->sharedThis<Symbol_O>(),frame);
	return result;
    }


    string Symbol_O::__repr__() const
    {
	return this->formattedName(false);

    };

    string Symbol_O::currentName() const
    {
	string formattedName = this->formattedName(false);
	return formattedName;
    }


    string Symbol_O::fullName() const
    {
	string formattedName = this->formattedName(true);
	return formattedName;
    }



    Package_sp Symbol_O::getPackage() const
    {_G();
	if ( !this->_HomePackage ) return _Nil<Package_O>();
	return this->_HomePackage;
    } 

    void Symbol_O::setPackage(Package_sp p)
    {_G();
	ASSERTF(p,BF("The package is UNDEFINED"));
	this->_HomePackage = p;
    }





    void Symbol_O::exposeCando(Lisp_sp lisp)
    {_G();
	// TODO: By default these symbols like SPECIALP are being dumped into the COMMON-LISP package - don't do that.
	class_<Symbol_O>()
	    .def("core:specialp",&Symbol_O::specialP)
	    .def("core:STARmakeSpecial",&Symbol_O::makeSpecial)
	    .def("core:STARmakeConstant",&Symbol_O::makeConstant)
	    .def("core:fullName",&Symbol_O::fullName )
	    .def("core:asKeywordSymbol",&Symbol_O::asKeywordSymbol)
	    .def("core:setf_symbolFunction",&Symbol_O::setf_symbolFunction)
	    .def("makunbound",&Symbol_O::makunbound)
	    .DEF(Symbol_O,copy_symbol)
	    ;
	SYMBOL_EXPORT_SC_(ClPkg,make_symbol);
	Defun(make_symbol);
	SYMBOL_EXPORT_SC_(ClPkg,symbolName);
	Defun(symbolName);
	SYMBOL_EXPORT_SC_(ClPkg,symbolValue);
	Defun(symbolValue);
	SYMBOL_EXPORT_SC_(ClPkg,symbolPackage);
	Defun(symbolPackage);
	SYMBOL_EXPORT_SC_(ClPkg,symbolFunction);
	Defun(symbolFunction);
	SYMBOL_EXPORT_SC_(ClPkg,boundp);
	Defun(boundp);
    }

    void Symbol_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Symbol,"","",_lisp)
	    .def("fullName",&Symbol_O::fullName )
//	    .def("namestring",&Symbol_O::fullName)
	    .def("asKeywordSymbol",&Symbol_O::asKeywordSymbol)
	    ;
#endif
    }




    void Symbol_O::dump()
    {_G();
	stringstream ss;
	ss << "Symbol @" << (void*)this << " --->" << std::endl;
	{
	    ss << "Name: " << this->_Name->get() <<std::endl;
	    if ( !this->_HomePackage )
	    {
		ss << "Package: UNDEFINED" << std::endl;
	    } else
	    {
		ss << "Package: ";
		ss << _rep_(this->_HomePackage) << std::endl;
	    }
	    if ( !this->_Value )
	    {
		ss << "VALUE: NULL" << std::endl;
	    } else 	if ( this->_Value.unboundp() )
	    {
		ss << "Value: UNBOUND" << std::endl;
	    } else if ( this->_Value.nilp() )
	    {
		ss << "Value: nil" << std::endl;
	    } else
	    {
		ss << "Value: " << _rep_(this->_Value) << std::endl;
	    }
	    if ( !this->_Function )
	    {
		ss << "Function: NULL" << std::endl;
	    } else if ( this->_Function.unboundp() )
	    {
		ss << "Function: UNBOUND" << std::endl;
	    } else if ( this->_Function.nilp() )
	    {
		ss << "Function: nil" << std::endl;
	    } else
	    {
		ss << "Function: " << _rep_(this->_Function) << std::endl;
	    }
	    ss << "IsSpecial: " << this->_IsSpecial << std::endl;
	    ss << "IsConstant: " << this->_IsConstant << std::endl;
	    ss << "ReadOnlyFunction: " << this->_ReadOnlyFunction << std::endl;
	    ss << "PropertyList: ";
	    if (this->_PropertyList)
	    {
		ss << _rep_(this->_PropertyList) << std::endl;
	    } else
	    {
		ss << "UNDEFINED" << std::endl;
	    }
	}
	printf("%s", ss.str().c_str() );
    }


    EXPOSE_CLASS(core,Symbol_O);

};
