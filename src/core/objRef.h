#ifndef	ObjRef_H //[
#define ObjRef_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"

namespace core {


    SMART(ObjRef );
    class ObjRef_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,CorePkg,ObjRef_O,"ObjRef");
public:
#if defined(XML_ARCHIVE)
	void	archive(ArchiveP node);
#endif // defined(XML_ARCHIVE)
	void	initialize();
private:
	string		_Selector;
	string		_Name;
	ObjRef_sp	_SubRef;
public:
    static ObjRef_sp create(Lisp_sp e,const string& asString );
    static ObjRef_sp create2(Lisp_sp e,const string& asString ) { return ObjRef_O::create(e,asString);};
public:
    string	getSelector() { return this->_Selector;};
    void	setSelector(const string& t) { this->_Selector = t;};
    string	getName() { return this->_Name;};
    void	setName(const string& t) { this->_Name = t;};
    ObjRef_sp	getSubRef() { return this->_SubRef; };
    void	setSubRef(ObjRef_sp o) { this->_SubRef = o;};

    string	asString();


	/*! Follow the reference relative to the given object
	 */
    T_sp	relativeTo(T_sp o);

    DEFAULT_CTOR_DTOR(ObjRef_O);
    };



};
TRANSLATE(core::ObjRef_O);
#endif //]
