#ifndef	Special_H //[
#define Special_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "executables.h"
#include "object.h"


namespace core {



    SMART(SpecialForm );
    class SpecialForm_O : public Function_O
    {
	LISP_BASE1(Function_O);
	LISP_CLASS(core,CorePkg,SpecialForm_O,"SpecialForm");
	DECLARE_INIT();
public: // virtual functions inherited from Object
	void	initialize();
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
//	string	__repr__() const;

private: // instance variables
	Symbol_sp		_SpecialSymbol;
	SpecialFormCallback	_fptr;
public:
    static SpecialForm_sp create(Symbol_sp symbol, SpecialFormCallback cb);
public:	// initialize

	virtual bool isSpecialForm() { return true;};

	string __repr__() const;
	T_mv evaluate( Cons_sp args, T_sp environment);


	SpecialForm_O( const SpecialForm_O& ss ); //!< Copy constructor

	DEFAULT_CTOR_DTOR(SpecialForm_O);
    };


};
TRANSLATE(core::SpecialForm_O);
#endif //]
