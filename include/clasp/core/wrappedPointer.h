/*
    File: wrappedPointer.h
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
#ifndef core_wrappedPointer_H
#define core_wrappedPointer_H

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/instance.h>
#include <clasp/core/lisp.h>


namespace core {

// set this class up by hand
    SMART(WrappedPointer);
    class WrappedPointer_O : public core::T_O
    {
        FRIEND_GC_SCANNER();
        LISP_BASE1(core::T_O);
	LISP_CLASS(core,CorePkg,WrappedPointer_O,"WrappedPointer");
    GCPROTECTED:
        core::Class_sp _Class;
    public:
        virtual core::Class_sp _instanceClass() const { return this->_Class;};
        virtual T_sp instanceClassSet(Class_sp mc);

        void setInstanceClassUsingSymbol(core::Symbol_sp classSymbol);
    public:
        virtual bool validp() const {SUBIMP();};
        virtual size_t templatedSizeof() const {SUBIMP();};
	virtual bool eql(core::T_sp obj) const;
	virtual void* mostDerivedPointer() const {_OF(); SUBCLASS_MUST_IMPLEMENT();};
        Pointer_sp  address() const;
        virtual void* castTo(class_id cid) const {SUBIMP();};
        virtual class_id classId() const {SUBIMP();};
        template <typename T> T* castOrNull() const {
            return static_cast<T*>(this->castTo(reg::registered_class<T>::id));
        }
        template <typename T> T* cast() const {
            T* result = this->castOrNull<T>();
            if (!result) {
                SIMPLE_ERROR(BF("Is inheritance defined correctly? Could not cast WrappedPointer of class %s to %s class_id/from=%d/%s class_id/to=%d/%s")
                             % _rep_(this->_instanceClass())
                             % _rep_(reg::lisp_classSymbol<T>())
                             % this->classId()
                             % _rep_(reg::lisp_classSymbolFromClassId(this->classId()))
                             % reg::registered_class<T>::id
                             % _rep_(reg::lisp_classSymbolFromClassId(reg::registered_class<T>::id)) );
            }
            return result;
        }
        virtual void* pointerRelease() {SUBIMP();};
        virtual void pointerDelete() {SUBIMP();};
    public:
	explicit WrappedPointer_O() : Base()
                                    , _Class(_Nil<core::Class_O>())
        {};
	virtual ~WrappedPointer_O() {};
    };


};


#endif
