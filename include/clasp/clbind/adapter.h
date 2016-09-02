/*
    File: adapter.h
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
#ifndef clbind_adapter_H
#define clbind_adapter_H

#include <clasp/clbind/config.h>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/instance.h>
#include <clasp/core/numbers.h>
#include <clasp/clbind/adapter.fwd.h>
#include <clasp/clbind/clbind_wrappers.h>
#include <clasp/clbind/clbindPackage.h>

#if 0
namespace clbind { 


    template <typename T>
    struct WeakWrapperPtr {
        typedef T       WrapperType;
        WrapperType*    ptr_gc_ignore;
        GC_RESULT onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE) {
            // do nothing
            return GC_RES_OK;
        }
        WeakWrapperPtr() : ptr_gc_ignore(NULL) {};
        void set(WrapperType* z) { this->ptr_gc_ignore = z; };
        WrapperType* get() const { return this->ptr_gc_ignore; };
    };

    template <class T, class HolderType=T*>
    class Adapter 
    {
    public:
        typedef int enable_derivable;
        typedef T* enable_wrapper_from_this;
        typedef T* enable_slots;
        typedef Wrapper<T,HolderType>   WrapperType;
        typedef gctools::smart_ptr<WrapperType> SmartPtrType;
    private:
        string                          _Name;
        gctools::Vec0<core::T_sp>          _Slots;
        core::T_sp                      _Sig;
        WeakWrapperPtr<WrapperType>     _WeakWrapper;
    public:
        Adapter(const string& name) { this->_Name = name;};
        virtual ~Adapter() {};
    public:
        bool cxxAdapterClassP() const { return true;};

        SmartPtrType wrapper_from_this() const { return gctools::smart_ptr<WrapperType>(this->_WeakWrapper.get()); };//gctools::smart_ptr<WrapperType>(this->_wrapper);};
        void _internal_accept_wrapper(gctools::smart_ptr<WrapperType>& wrapper) { this->_WeakWrapper.set(wrapper.pxget());};

        void* address() const { return const_cast<void*>(static_cast<const void*>(this)); };
        void initializeSlots(int numberOfSlots)
        {
//            printf("%s:%d initializeSlots\n", __FILE__, __LINE__ );
            this->_Slots.resize(numberOfSlots,_Unbound<core::T_O>());
            for ( int i(0); i<numberOfSlots; ++i ) {
//                printf("%s:%d Adaptor::initializeSlots  slot[%d]@%p = %p\n", __FILE__,__LINE__, i, &(this->_Slots[i]), this->_Slots[i].pxget());
            }
//            printf("%s:%d done initializeSlots\n", __FILE__, __LINE__ );
        }

        core::T_sp instanceSigSet() {
            SmartPtrType me = this->wrapper_from_this();
            core::Class_sp mc = me->_instanceClass();
            core::T_sp classSlots = mc->slots();
            this->_Sig = classSlots;
            return classSlots;
        }


        core::T_sp instanceSig() const
        {
            return this->_Sig;
        };


        core::T_sp instanceRef(int idx) const
        {
            return this->_Slots[idx];
        }

        core::T_sp instanceSet(int idx, core::T_sp val)
        {
            this->_Slots[idx] = val;
//            printf("%s:%d adaptor::instanceSet[%d]@%p = %p\n", __FILE__,__LINE__,idx,&(this->_Slots[idx]),val.pxget());
            return val;
        }

    };





    template <class T, class HolderType>
    void support_enable_wrapper_from_this(gctools::smart_ptr<Wrapper<T,HolderType> >& wrapper, T* ptr,typename T::enable_wrapper_from_this)
    {
        ptr->_internal_accept_wrapper(wrapper);
    }

    template <class T, class HolderType>
    void support_enable_wrapper_from_this(gctools::smart_ptr<Wrapper<T,HolderType> >& wrapper, T* ptr, ...) {}

 


};


//#include <clbind/detail/overload_rep_impl.hpp>

#endif //if 0

#endif // CLBIND_CLASS_REP_HPP_INCLUDED
