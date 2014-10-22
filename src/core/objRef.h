/*
    File: objRef.h
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
GCPRIVATE:
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
