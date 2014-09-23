/*
    File: vectorSymbolsWithFillPtr.h
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
#ifndef	_core_VectorSymbolsWithFillPtr_H
#define _core_VectorSymbolsWithFillPtr_H
#include "core/foundation.h"
#include "core/object.h"
#include "core/vectorWithFillPtrTemplate.h"
#include "corePackage.fwd.h"

namespace core
{

    FORWARD(VectorSymbolsWithFillPtr);
    class VectorSymbolsWithFillPtr_O : public VectorWithFillPtr_template_O<Symbol_O>
    {
	LISP_TEMPLATE_BASE1(VectorWithFillPtr_template_O<Symbol_O>);
	LISP_CLASS(CorePkg,VectorSymbolsWithFillPtr_O,"VectorSymbolsWithFillPtr");
    public:
	VectorSymbolsWithFillPtr_O() : T_O(), Base() {};
	virtual ~VectorSymbolsWithFillPtr_O() {};
    private: // instance variables here
	int 		_FillPtr;
    public:
	static VectorSymbolsWithFillPtr_sp make(Symbol_sp initial_element, Cons_sp initial_values, int dimension, int fillPtr );

    public: // Functions here
	bool arraySymbolsP() const { return this->isObject();};
	bool vectorSymbolsP() const { return this->isObject();};
	virtual bool arrayHasFillPointerP() const { return true;};
	T_sp elementType() const;
	
	uint length() const { return this->_FillPtr;};


	virtual Symbol_sp& symbolElement(uint index)
	{
	    ASSERT(index<this->_FillPtr);
	    return this->_Values[index];
	}

    };

}; /* core */

TRANSLATE(core::VectorSymbolsWithFillPtr_O);

#endif /* _core_VectorSymbolsWithFillPtr_H */
