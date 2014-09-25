/*
    File: vectorSymbols.h
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
#ifndef	_core_VectorSymbols_H
#define _core_VectorSymbols_H

#include "core/object.h"
#include "core/lispVector.h"
#include "core/vectorTemplate.h"
#include "corePackage.fwd.h"

namespace core
{

    FORWARD(VectorSymbols);
    class VectorSymbols_O : public Vector_template_O<Symbol_O>
    {
	LISP_TEMPLATE_BASE1(Vector_template_O<Symbol_O>);
	LISP_CLASS(CorePkg,VectorSymbols_O,"VectorSymbols");
    public:
	VectorSymbols_O() : T_O(), Vector_template_O<Symbol_O>() {};
	virtual ~VectorSymbols_O() {};
    public:
	static VectorSymbols_sp make(Symbol_sp initial_element, Cons_sp initial_values, int dimension );

    public: // Functions here
	bool arraySymbolsP() const { return this->isObject();};
	bool vectorSymbolsP() const { return this->isObject();};
	T_sp elementType() const;

    };

}; /* core */

TRANSLATE(core::VectorSymbols_O);

#endif /* _core_VectorSymbols_H */
