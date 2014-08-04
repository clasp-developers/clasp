#define	DEBUG_LEVEL_FULL

#include "lisp.h"
#include "specialForm.h"
#include "lisp.h"
//#include "debugger.h"
// last include is wrappers.h
#include "wrappers.h"


namespace core {


void SpecialForm_O::exposeCando(Lisp_sp lisp)
{
    class_<SpecialForm_O>()
    ;
}

    void SpecialForm_O::exposePython(Lisp_sp lisp)
    {_G();
#if	0 // USEBOOSTPYTHON //[
	PYTHON_CLASS(CorePkg,SpecialForm,"","",_lisp)
    ;
#endif //]
}


SpecialForm_sp SpecialForm_O::create(Symbol_sp symbol, SpecialFormCallback fptr )
{
    SpecialForm_sp sf = SpecialForm_O::create();
    sf->_SpecialSymbol = symbol;
    sf->_fptr = fptr;
    return sf;
}


    T_mv SpecialForm_O::evaluate( Cons_sp args, T_sp environment )
    {_OF();
	ASSERTP(this->_fptr!=NULL,"Functoid can not be NULL");
	return (this->_fptr)(args,environment);
    }



void	SpecialForm_O::initialize()
{
    this->Base::initialize();

}

#if defined(XML_ARCHIVE)
void	SpecialForm_O::archiveBase(ArchiveP node)
{
    this->Base::archiveBase(node);
    IMPLEMENT_ME();
}
#endif // defined(XML_ARCHIVE)


string SpecialForm_O::__repr__() const
{
    return this->_SpecialSymbol->fullName();
}

    EXPOSE_CLASS(core,SpecialForm_O);

};




