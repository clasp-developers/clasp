       
       
//
// (C) 2004 Christian E. Schafmeister
//


#ifndef SymbolList_H
#define SymbolList_H
#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"



namespace core {


SMART(Residue);
SMART(SymbolSet);
SMART(SymbolList);

/*!
	A class that stores a set of strings
*/
SMART(SymbolList);
class SymbolList_O : public T_O
{
    LISP_BASE1(T_O);
    LISP_CLASS(core,CorePkg,SymbolList_O,"SymbolList");
public:
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
private:
    gctools::Vec0<Symbol_sp>	_Contents;
public:
    typedef	gctools::Vec0<Symbol_sp>::iterator	iterator;
    typedef	gctools::Vec0<Symbol_sp>::const_iterator	const_iterator;

    iterator begin() { return this->_Contents.begin();};
    iterator end() { return this->_Contents.end();};
    const_iterator begin() const { return this->_Contents.begin();};
    const_iterator end() const { return this->_Contents.end();};

	int	size() { return this->_Contents.size(); };
	void	prepend(Symbol_sp s);
	void	append(Symbol_sp s);
	void	appendSymbolList(SymbolList_sp strings);
	void	appendConsOfStrings(Cons_sp strings);

	bool	contains(Symbol_sp nm);
	int	indexOf(Symbol_sp nm);
	Symbol_sp get(uint i) {_G(); ASSERT_lt(i,this->_Contents.size()); return this->_Contents[i];};
	void	clear();

	void	setFromVectorStrings( VectorStrings vs);
	string	asString();

	SymbolSet_sp asStringSet();


	void map(std::function<void(Symbol_sp)> const& fn);

	SymbolList_O( const SymbolList_O& ss ); //!< Copy constructor

	DEFAULT_CTOR_DTOR(SymbolList_O);
};





};


TRANSLATE(core::SymbolList_O);
#endif


