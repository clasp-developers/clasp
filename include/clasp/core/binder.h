/*
    File: binder.h
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
#ifndef	_core_binder_H //[
#define _core_binder_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/symbol.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/holder.h>
//#include "stringSet.fwd.h"
#include <clasp/core/environment.fwd.h>
#include <clasp/core/activationFrame.fwd.h>
#include <clasp/core/cons.h>
#include <clasp/core/binder.fwd.h>

namespace core
{

    SMART(ObjectDictionary);
    SMART(Name);



    SMART(Binder);
    class Binder_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,CorePkg,Binder_O,"Binder");
	void initialize();

#if defined(OLD_SERIALIZE)
	DECLARE_SERIALIZE();
#endif // defined(OLD_SERIALIZE)
    public:
	void archiveBase(ArchiveP node);
    GCPRIVATE:
        HashTableEq_sp          _Bindings;
        VectorObjects_sp        _Values;
    public:

	uint size() { return this->_Values->length();};

	T_sp indexed_value(int idx) const { return this->_Values->operator[](idx); };
	void erase();
	T_sp	extend(Symbol_sp sym, T_sp val);
//	void		update(Symbol_sp sym, T_sp val);
	T_sp	lookup(Symbol_sp sym) const;
	T_sp	lookupSymbol(Symbol_sp sym) const {return this->lookup(sym);};
	T_sp	lookup(const string& package, const string& symStr) const;

	bool		contains(Symbol_sp sym) const;
        //	const_iterator	find(Symbol_sp sym) const;
	// iterator	find(Symbol_sp sym);

	string		summaryOfContents() const;

#if 0
	bool	canRender() { return true; };
	Render_sp rendered(List_sp options);
#endif
	/*! Return true if this binder contains the symbol
	 * that you get when you search for the kw
	 */
	bool		containsSymbolFromString(const string& kw);
	T_sp	valueSymbolFromString(const string& kw);

	/*! Return the value for the symbol/string 
	 * or the default
	 */
	T_sp valueOrDefault(const string& kw, T_sp defVal);
	bool boolValueOrDefault(const string& kw, bool defVal);
	int  intValueOrDefault(const string& kw, int defVal);
	int  intValueOrDefault(Symbol_sp, int defVal);
	string stringValueOrDefault(const string& kw, const string& defVal);
	/* Set the value if provided otherwise do nothing.
	 * Return true if the value was provided
	 */
	bool setBoolValueIfAvailable(bool& dest, const string& kw);
	bool setIntValueIfAvailable(int& dest, const string& kw);
	bool setStringValueIfAvailable(string& dest, const string& kw);



	DEFAULT_CTOR_DTOR(Binder_O);
    };

};
    

TRANSLATE(core::Binder_O);

#endif //]
