/*
    File: derivable.h
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
#ifndef clbind_derivable_H
#define clbind_derivable_H

#include <clasp/core/derivableCxxObject.h>

namespace clbind {
  template <class Alien>
    class Derivable;
};


/*! Every Derivable class needs to have the unmanaged GCInfo<T>::Policy
*/
template <class T>
struct gctools::GCInfo<clbind::Derivable<T>> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = unmanaged;
};

namespace clbind {
/*! Derivables are template classes that inherit from 
    core::DerivableCxxObject_O and wrap Alien classes.
I NEED to use inheritance here - so Derivable<T> inherits from T
so that the Derivable<T> class can modify the vtable of the Alien to
redirect its virtual functions to the Derivable<T> functions. */
  template <class Alien>
    class Derivable : public core::DerivableCxxObject_O, public Alien  {
  public:
// All classes derived from Derivable must be put in the non-moving pool
    struct metadata_gc_do_not_move {};
    typedef Derivable<Alien> DerivableType;
    typedef Alien AlienType;
  public:
  // Used by template program to indicate that CommonLisp classes
  // can be derived from this class
    typedef int enable_derivable;
  public:
    template <typename... Args>
      static gctools::smart_ptr<DerivableType> create(Args &&... args) {
      GC_ALLOCATE_VARIADIC(DerivableType, obj, std::forward<Args>(args)...);
      return obj;
    }
    explicit Derivable(){};
    /*! Define a pointerToAlienWithin() virtual function that returns
        a void* pointer to the Derivable<Alien> Alien object.
        This will be used by translators. */
    virtual void *pointerToAlienWithin() { return reinterpret_cast<void*>(static_cast<Alien*>(this)); };
    bool cxxAdapterClassP() const { return true; };
    virtual Fixnum get_stamp_() const { return this->stamp(); };
    virtual Instance_O* get_Instance_O_address_() { Instance_O* inst = this; return inst; };
    virtual size_t get_size_() const { return sizeof(*this); };
    void describe() {
      printf("#<Derivable>@%p\n", this);
      printf("typeid(this) --> %p\n", &typeid(this));
      printf("dynamic_cast to void* --> %p\n", dynamic_cast<void *>(this));
      printf("dynamic_cast to T_O* -->  %p\n", dynamic_cast<core::T_O *>(this));
      printf("dynamic_cast to Derivable<Alien>* --> %p\n", dynamic_cast<Derivable<Alien> *>(this));

      printf("alien pointer = %p\n", this->pointerToAlienWithin());
      printf("_Class: %s\n", _rep_(this->_Class).c_str());
      for (int i(0); i < this->_Slots.size(); ++i) {
        printf("_Slots[%d]: %s\n", i, _rep_(this->_Slots[i]).c_str());
      }
    }
  };
};

#endif
