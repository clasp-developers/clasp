/*
    File: newVectorObjectsWithFillPtr.h
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
#ifndef	_core_VectorObjectsWithFillPtr_H
#define _core_VectorObjectsWithFillPtr_H
#include "core/foundation.h"
#include "core/object.h"
#include "core/vectorObjects.h"
#include "corePackage.fwd.h"

namespace core
{

    FORWARD(VectorObjectsWithFillPtr);
    class VectorObjectsWithFillPtr_O : public VectorObjects_O
    {
	LISP_BASE1(VectorObjects_O);
	LISP_CLASS(core,CorePkg,VectorObjectsWithFillPtr_O,"VectorObjectsWithFillPtr");
	void archiveBase(SNode_sp node);
    public:
	VectorObjectsWithFillPtr_O();
	virtual ~VectorObjectsWithFillPtr_O() {};
    public:
	void initialize();
    private: // instance variables here
	int 		_FillPtr;
    public:
	static VectorObjectsWithFillPtr_sp make(T_sp initial_element, T_sp initial_values, int dimension, int fillPtr, bool adjustable );

    public: // Functions here

	uint length() const { return this->_FillPtr;};

	virtual bool arrayHasFillPointerP() const { return true;};
	virtual T_sp& operator[](uint index);

	virtual T_sp elt(int index) const;
	virtual T_sp setf_elt(int index, T_sp value);

	string __repr__() const;

	int fillPointer() const { return this->_FillPtr;};
	void setf_fillPointer(int fp);

	Fixnum_sp vectorPush(T_sp newElement);
	Fixnum_sp vectorPushExtend(T_sp newElement, int extension=16);

    };

}; /* core */

TRANSLATE(core::VectorObjectsWithFillPtr_O);

#endif /* _core_VectorObjectsWithFillPtr_H */
