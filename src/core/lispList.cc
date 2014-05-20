#define	DEBUG_LEVEL_FULL

#include "core/common.h"
#include "core/environment.h"
#include "core/ql.h"
#include "lispList.h"
#include "symbolTable.h"
#include "core/wrappers.h"
namespace core
{

// ----------------------------------------------------------------------
//


#define ARGS_af_nth "(idx arg)"
#define DECL_af_nth ""
#define DOCS_af_nth "See CLHS nth"
    T_sp af_nth(int idx, T_sp arg)
    {_G();
	if (arg.nilp()) return 0;
	if ( List_sp list = arg.asOrNull<List_O>() )
	{
	    return list->onth(idx);
	}
	TYPE_ERROR(arg,cl::_sym_List_O);
    };


#define ARGS_af_nthcdr "(idx arg)"
#define DECL_af_nthcdr ""
#define DOCS_af_nthcdr "See CLHS nth"
    T_sp af_nthcdr(int idx, T_sp arg)
    {_G();
	if (arg.nilp()) return arg;
	if ( List_sp list = arg.asOrNull<List_O>() )
	{
	    return list->onthcdr(idx);
	}
	TYPE_ERROR(arg,cl::_sym_List_O);
    };




    
    
#define ARGS_af_copyList "(arg)"
#define DECL_af_copyList ""
#define DOCS_af_copyList "copyList"
    T_sp af_copyList(T_sp arg)
    {_G();
	if ( arg.nilp() ) return arg;
	if ( List_sp l = arg.asOrNull<List_O>() )
	{
	    return l->copyList();
	}
	TYPE_ERROR(arg,cl::_sym_List_O);
    };


    
    
#define ARGS_af_butlast "(list &optional (n 1))"
#define DECL_af_butlast ""
#define DOCS_af_butlast "butlast"
    T_mv af_butlast(List_sp list, Integer_sp n)
    {_G();
	int ni = n->as_int();
	int keepi = af_length(list)-ni;
	if ( keepi <= 0 ) return(Values(_Nil<Cons_O>()));
	ql::list res;
	Cons_sp cur = list.as_or_nil<Cons_O>();
	for ( int i=0; i<keepi; i++ )
	{
	    res << oCar(cur);
	    cur = cCdr(cur);
	}
	return(Values(res.cons()));
    };



#define ARGS_af_nbutlast "(list &optional (n 1))"
#define DECL_af_nbutlast ""
#define DOCS_af_nbutlast "butlast"
    List_sp af_nbutlast(List_sp list, Integer_sp n)
    {_G();
	int ni = n->as_int();
	int keepi = af_length(list)-ni;
	if ( keepi <= 0 ) return (_Nil<List_O>());
	Cons_sp cur = list.as<Cons_O>();
	Cons_sp prev = _Nil<Cons_O>();;
	for ( int i=0; i<keepi; i++ )
	{
	    prev = cur;
	    cur = cCdr(cur);
	}
	prev->setCdr(_Nil<Cons_O>());
	return list;
    };


    
    
#define ARGS_af_list "(&rest objects)"
#define DECL_af_list ""
#define DOCS_af_list "See CLHS: list"
    T_sp af_list(List_sp objects)
    {_G();
	return objects;
    };




    
    

#define DOCS_af_listSTAR "list* see CLHS"
#define LOCK_af_listSTAR 0
#define ARGS_af_listSTAR "(&rest objects)"
#define DECL_af_listSTAR ""
    T_sp af_listSTAR(T_sp tobjects)
    {_G();
	T_sp objects = tobjects;
	if ( objects.nilp() ) return(Values(_Nil<Cons_O>()));
	if (cCdr(objects).nilp() ) return(oCar(objects));
	Cons_sp cur;
	ql::list result(_lisp);
	for ( ; oCdr(objects).notnilp(); objects=oCdr(objects))
	{
//	    printf("Adding %s\n", _rep_(oCar(objects)).c_str());
	    result << oCar(objects);
	}
//	printf("dotting %s\n", _rep_(objects).c_str());
	result.dot(oCar(objects));
	return result.cons();
    }


#define DOCS_af_last "last - see CLHS"
#define LOCK_af_last 1
#define ARGS_af_last "(list &optional (on 1))"
#define DECL_af_last ""    
    T_sp af_last(List_sp list, int n)
    {_G();
	if ( list.nilp() ) return list;
	if ( n < 0 )
	{
	    CELL_ERROR(Fixnum_O::create(n));
	}
	return list->last(n);
    };











#define DOCS_af_revappend "revappend"
#define LOCK_af_revappend 1
#define ARGS_af_revappend "(list tail)"
#define DECL_af_revappend ""
    T_mv af_revappend(List_sp list, T_sp tail)
    {_G();
	if ( list.nilp() ) return(Values(tail));
	return(Values(list->revappend(tail)));
    };



#define DOCS_af_nreconc "nreconc"
#define LOCK_af_nreconc 1
#define ARGS_af_nreconc "(list tail)"
#define DECL_af_nreconc ""
    T_mv af_nreconc(List_sp list, T_sp tail)
    {_G();
	if ( list.nilp() ) return(Values(tail));
	return(Values(list->nreconc(tail)));
    };




    EXPOSE_CLASS(core,List_O);

    void List_O::exposeCando(Lisp_sp lisp)
    {_G();
	class_<List_O>()
	    //	    .def("revappend",&List_O::revappend)    I need to wrap this
	    //	    .def("nreconc",&List_O::nreconc)	I need to wrap this
	    //	.initArgs("(self)")
	    ;
	SYMBOL_EXPORT_SC_(ClPkg,revappend);
 	Defun(revappend);
	SYMBOL_EXPORT_SC_(ClPkg,nreconc);
	Defun(nreconc);
	SYMBOL_EXPORT_SC_(ClPkg,list);
	Defun(list);
	SYMBOL_EXPORT_SC_(ClPkg,listSTAR);
	Defun(listSTAR);
	SYMBOL_EXPORT_SC_(ClPkg,butlast);
	Defun(butlast);
	SYMBOL_EXPORT_SC_(ClPkg,nbutlast);
	Defun(nbutlast);
	SYMBOL_EXPORT_SC_(ClPkg,nth);
	Defun(nth);
	SYMBOL_EXPORT_SC_(ClPkg,nthcdr);
	Defun(nthcdr);
	SYMBOL_EXPORT_SC_(ClPkg,copyList);
	Defun(copyList); 
	SYMBOL_EXPORT_SC_(ClPkg,last);
	Defun(last);
   }

    void List_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(Pkg(),List,"","",_LISP)
	    //	.initArgs("(self)")
	    ;
#endif
    }



#if 0
    void List_O::serialize(::serialize::SNodeP node)
    {
	IMPLEMENT_ME();
	this->Bases::serialize(node);
	// Archive other instance variables here
    }
#endif

#if 0
    void List_O::archiveBase(::core::ArchiveP node)
    {
	IMPLEMENT_ME();
	this->Base1::archiveBase(node);
	// Archive other instance variables here
    }
#endif


}; /* core */
