#ifndef asttooling_translators_H
#define asttooling_translators_H

#include <core/foundation.h>
#include <core/symbolTable.h>
#include <core/vectorObjects.h>
#include <core/vectorObjectsWithFillPtr.h>
#include "clbind/clbind.h"

#include <clang/Tooling/JSONCompilationDatabase.h>
#include "clang/AST/Stmt.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/AST/declBase.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"

namespace translate {


    template <>
    struct	from_object<int&,std::true_type>
    {
	typedef	int DeclareType;
	DeclareType _v;
	from_object(core::T_sp o) : _v(0)
	{_G();
	    if ( core::Fixnum_sp fn = o.asOrNull<core::Fixnum_O>() )
	    {
		this->_v = (int)(fn->get());
		return;
	    }
	    SIMPLE_ERROR(BF("Add support to convert other types to int"));
	}
    };




    template<>
    struct to_object<int&>
    {
	static core::T_sp convert(const int& val)
	{
	    return (core::Integer_O::create(val));
	}
    };


#if 0
    template<>
    struct to_object<clang::CXXRecordDecl*>
    {
	static core::T_sp convert(clang::CXXRecordDecl* ptr)
	{
            IMPLEMENT_MEF(BF("Handle more complex wrappers"));
//	    return (clbind::Wrapper<clang::CXXRecordDecl>::create(ptr));
	}
    };

#endif



    template<>
    struct to_object<std::vector<std::string>,translate::adopt_pointer >
    {
        static core::T_sp convert(std::vector<std::string> strings)
        {
            core::VectorObjects_sp vo = core::VectorObjects_O::create(_Nil<core::T_O>(),strings.size(),cl::_sym_T_O);
            int i(0);
            for (auto ai=strings.begin(); ai!=strings.end(); ai++ ) {
                vo->setf_elt(i++,core::lisp_createStr(*ai));
            }
            return vo;
        }
    };

    template<>
    struct to_object<std::vector<std::string>,translate::dont_adopt_pointer >
    {
        static core::T_sp convert(std::vector<std::string> strings)
        {
            core::VectorObjects_sp vo = core::VectorObjects_O::create(_Nil<core::T_O>(),strings.size(),cl::_sym_T_O);
            int i(0);
            for (auto ai=strings.begin(); ai!=strings.end(); ai++ ) {
                vo->setf_elt(i++,core::lisp_createStr(*ai));
            }
            return vo;
        }
    };

    template <>
    struct	from_object<const vector<string>&>
    {
	typedef	vector<string> DeclareType;
	DeclareType _v;
	from_object(core::T_sp o)
	{_G();
            if ( o.nilp() ) {
                _v.clear();
                return;
            } else if ( core::VectorObjects_sp vo = o.asOrNull<core::VectorObjects_O>() ) {
                _v.resize(vo->length());
                for ( int i(0), iEnd(vo->length()); i<iEnd; ++i ) {
                    _v[i] = vo->elt(i).as<core::Str_O>()->get();
                }
                return;
            } else if ( core::Cons_sp lo = o.asOrNull<core::Cons_O>() ) {
                _v.resize(lo->length());
                int i=0;
                for ( core::Cons_sp cur = lo; cur.notnilp(); cur=cCdr(cur) ) {
                    _v[i] = oCar(cur).as<core::Str_O>()->get();
                    ++i;
                }
                return;
            }
	    SIMPLE_ERROR(BF("Add support to convert other types to vector<string>"));
	}
    };
    
#if 0 // You will need the following from_object and to_object to wrap ClangTool::buildASTs
    // You will also need to make clbind::Wrappers do the right thing with std::unique_ptrs
    //
    template <>
    struct from_object<std::vector<std::unique_ptr<clang::ASTUnit>>&>
    {
        typedef std::vector<std::unique_ptr<clang::ASTUnit>>&   DeclareType;
        std::vector<std::unique_ptr<clang::ASTUnit>>    _Temp;
        DeclareType     _v;
        from_object(core::T_sp o) : _v(_Temp)
        {
            if ( o.nilp() ) {
                this->_v.clear();
                return;
            } else if ( core::VectorObjects_sp vo = o.asOrNull<core::VectorObjects_O>() ) {
                this->_v.clear();
                this->_v.resize(vo->length());
                for ( int i(0), iEnd(vo->length()); i<iEnd; i++ ) {
                    this->_v[i] = vo->elt(i).as<clbind::Wrapper<clang::ASTUnit,std::unique_ptr<clang::ASTUnit> > >();
                }
                return;
            }
            SIMPLE_ERROR(BF("Could not convert argument %s into std::vector<clang::ASTUnit*>") % _rep_(o));
        }
    };

    template <>
    struct to_object<std::vector<std::unique_ptr<clang::ASTUnit>>&>
    {
        typedef std::vector<std::unique_ptr<clang::ASTUnit>> GivenType;
        static core::T_sp convert(GivenType vals)
        {
            core::VectorObjectsWithFillPtr_sp vo = core::VectorObjectsWithFillPtr_O::make(_Nil<core::T_O>(),_Nil<core::Cons_O>(),vals.size(),0,true);
            for ( int i(0),iEnd(vals.size()); i<iEnd; ++i ) {
                vo->vectorPushExtend(clbind::Wrapper<clang::ASTUnit,std::unique_ptr<clang::ASTUnit> >::create(vals[i],reg::registered_class<clang::ASTUnit>::id));
            }
            return vo;
        }
    };
#endif


    template <>
    struct to_object<std::vector<clang::tooling::CompileCommand>>
    {
        typedef std::vector<clang::tooling::CompileCommand> GivenType;
        static core::T_sp convert(GivenType vals)
        {
            core::VectorObjectsWithFillPtr_sp vo = core::VectorObjectsWithFillPtr_O::make(_Nil<core::T_O>(),_Nil<core::Cons_O>(),vals.size(),0,true);
            for ( int i(0),iEnd(vals.size()); i<iEnd; ++i ) {
                vo->vectorPushExtend(clbind::Wrapper<clang::tooling::CompileCommand,std::unique_ptr<clang::tooling::CompileCommand> >::create(vals[i],reg::registered_class<clang::tooling::CompileCommand>::id));
            }
            return vo;
        }
    };


};

#endif
