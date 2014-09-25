/*
    File: managedStatic.h
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
#ifndef gctools_managedStatic_H
#define gctools_managedStatic_H

namespace gctools {

    // This stuff is copied and modified from LLVM
    //===-- llvm/Support/ManagedStatic.h - Static Global wrapper ----*- C++ -*-===//
    //
    //                     The LLVM Compiler Infrastructure
    //
    // This file is distributed under the University of Illinois Open Source
    // License. See LICENSE.TXT for details.
    //
    //===----------------------------------------------------------------------===//
    //
    // This file defines the ManagedStatic class and the llvm_shutdown() function.
    //
    //===----------------------------------------------------------------------===//

    /// object_creator - Helper method for ManagedStatic.
    template<class C>
    void* object_creator() {
        return RootClassAllocator<C>::allocate();
    }
 
    /// object_deleter - Helper method for ManagedStatic.
    ///
    template<typename T> struct object_deleter {
        static void call(void * Ptr) { /*delete (T*)Ptr;*/ }
    };
 
    /// ManagedStaticBase - Common base class for ManagedStatic instances.
    template <typename CC>
    class ManagedStaticBase {
    protected:
        // This should only be used as a static variable, which guarantees that this
        // will be zero initialized.
        mutable CC *Ptr;
        void RegisterManagedStatic(void *(*Creator)() )
        {
//            ASSERT(Creator!=NULL);
#if 0
            if (llvm_is_multithreaded()) {
                llvm_acquire_global_lock();
 
                if (!Ptr) {
                    void* tmp = Creator();
 
                    TsanHappensBefore(this);
                    sys::MemoryFence();
 
                    // This write is racy against the first read in the ManagedStatic
                    // accessors. The race is benign because it does a second read after a
                    // memory fence, at which point it isn't possible to get a partial value.
                    TsanIgnoreWritesBegin();
                    Ptr = tmp;
                    TsanIgnoreWritesEnd();
                    DeleterFn = Deleter;
       
                    // Add to list of managed statics.
                    Next = StaticList;
                    StaticList = this;
                }
 
                llvm_release_global_lock();
            } else {
#endif
//                ASSERTF(!Ptr && !Next, BF("Partially initialized ManagedStatic!?"));
                this->Ptr = reinterpret_cast<CC*>(Creator());
                //DeleterFn = Deleter;
                
                // Add to list of managed statics.
//                Next = StaticList;
//                ManagedStaticList = this;
//            }
        }
 
    public:
        /// isConstructed - Return true if this object has not been created yet.
        bool isConstructed() const { return Ptr != nullptr; }
 
        void destroy() const;
    };
 
    template<class C>
    class ManagedStatic : public ManagedStaticBase<C> {
    public:
 
        // Accessors.
        C &operator*() {
            C* tmp = this->Ptr;
//            if (llvm_is_multithreaded()) sys::MemoryFence();
            if (!tmp) RegisterManagedStatic(object_creator<C> /*, object_deleter<C>::call*/);
            return *static_cast<C*>(this->Ptr);
        }
        C *operator->() {
            C* tmp = this->Ptr;
//            if (llvm_is_multithreaded()) sys::MemoryFence();
            if (!tmp) this->RegisterManagedStatic(object_creator<C> /*, object_deleter<C>::call*/ );
            return static_cast<C*>(this->Ptr);
        }
        const C &operator*() const {
            C* tmp = this->Ptr;
//            if (llvm_is_multithreaded()) sys::MemoryFence();
            if (!tmp) RegisterManagedStatic(object_creator<C> /*, object_deleter<C>::call*/);
            return *static_cast<C*>(this->Ptr);
        }
        const C *operator->() const {
            C* tmp = this->Ptr;
//            if (llvm_is_multithreaded()) sys::MemoryFence();
            if (!tmp) RegisterManagedStatic(object_creator<C> /*, object_deleter<C>::call*/);
            return static_cast<C*>(this->Ptr);
        }
    };

};

#endif // gctools_managedStatic_H
