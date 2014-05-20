       
       
//
// (C) 2004 Christian E. Schafmeister
//


#ifndef StringList_H
#define StringList_H
#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"



namespace core
{

    SMART(StringList);

/*!
  A class that stores a set of strings
*/
    SMART(StringList);
    class StringList_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,CorePkg,StringList_O,"StringList");
    public:
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    private:
	vector<string>	_Contents;
    public:
	typedef	vector<string>::iterator	iterator;

	iterator begin() { return this->_Contents.begin(); };
	iterator end() { return this->_Contents.end(); };

	string	first() { return *(this->_Contents.begin());};
	int	size() { return this->_Contents.size(); };
	void	prepend(const string& s);
	void	append(const string& s);
	void	appendStringList(StringList_sp strings);
	void	appendConsOfStrings(Cons_sp strings);

	bool	contains(const string& nm);
	int	indexOf(const string& nm);
	string get(uint i) {_G(); ASSERT_lt(i,this->_Contents.size()); return this->_Contents[i];};
	void	clear();

	void	setFromVectorStrings( VectorStrings vs);
	void	setFromString(const string& s);
	string	asString();

	StringSet_sp asStringSet();

#ifdef	USEBOOSTPYTHON
	void		python_setFromList(boost::python::list res);
	boost::python::list	python_asList();
#endif

	DEFAULT_CTOR_DTOR(StringList_O);
    };

};


TRANSLATE(core::StringList_O);
#endif


