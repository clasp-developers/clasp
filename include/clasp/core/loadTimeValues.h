/*
    File: loadTimeValues.h
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
#ifndef _core_loadTimeValues_H_
#define _core_loadTimeValues_H_

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/vectorObjectsWithFillPtr.h>
#include <clasp/core/loadTimeValues.fwd.h>

namespace core {
class LoadTimeValues_O : public T_O {
  friend void(::sp_copyLoadTimeValue(T_sp *resultP, LoadTimeValues_O **ltvPP, int index));
  LISP_BASE1(T_O);
  LISP_CLASS(core, CorePkg, LoadTimeValues_O, "LoadTimeValues");

public: // Simple default ctor/dtor
  LoadTimeValues_O() : Base(){};
  virtual ~LoadTimeValues_O(){};

public: // ctor/dtor for classes with shared virtual base
        //    explicit LoadTimeValues_O(core::Class_sp const& mc) : T_O(mc), VectorObjectsWithFillPtr(mc) {};
        //    virtual ~LoadTimeValues_O() {};

GCPRIVATE: // instance variables here
  gctools::Vec0<T_sp> _Objects;
  gctools::Vec0<Symbol_sp> _Symbols;

public: // Functions here
  static LoadTimeValues_sp make(int dataDimension, int symbolDimension);

  int numberOfValues() const { return this->_Objects.size(); };
  int numberOfSymbols() const { return this->_Symbols.size(); };

  void dumpValues(vector<gctools::Fixnum> &indices);
  void dumpSymbols(vector<gctools::Fixnum> &indices);

  // -------- Regular data storage

  ALWAYS_INLINE T_sp &data_element(uint i) { return this->_Objects[i]; };
  int data_vectorPushExtend(T_sp val, int extension);

  // -------- Symbols storage

  void symbols_setFillPointer(uint i);
  ALWAYS_INLINE Symbol_sp &symbols_element(uint i) { return this->_Symbols[i]; };
  int symbols_vectorPushExtend(Symbol_sp val, int extension);

}; // LoadTimeValues class
};
TRANSLATE(core::LoadTimeValues_O);
template <>
struct gctools::GCInfo<core::LoadTimeValues_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = unmanaged;
};

#if 0
namespace core {
    /*! This class will be allocated in a non-moving pool and store a smart_ptr
      to a LoadTimeValuesPointer_sp to which pointers can be taken and guaranteed
      that they will not become stale */
    FORWARD(MemoryLockedLoadTimeValuesPointer);
    c l a s s MemoryLockedLoadTimeValuesPointer_O : public T_O
    {
        struct metadata_gc_do_not_move {};   // put this in a non-moveable pool
    private:
	L I S P _BASE1(T_O);
	L I S P _CLASS(core,CorePkg,MemoryLockedLoadTimeValuesPointer_O,"MemoryLockedLoadTimeValuePointer");
    public: // Simple default ctor/dtor
	MemoryLockedLoadTimeValuesPointer_O() : Base(), _LoadTimeValues(_Nil<LoadTimeValues_O>()) {
            printf("%s:%d  MemoryLockedLoadTimeValuesPointer is DEPRECIATED()\n",__FILE__,__LINE__);
        };
	virtual ~MemoryLockedLoadTimeValuesPointer_O() {};
    private: // instance variables here
        LoadTimeValues_sp               _LoadTimeValues;
    public: // Functions here
	static MemoryLockedLoadTimeValuesPointer_sp make(LoadTimeValues_sp ltv) {
            GC_ALLOCATE_UNCOLLECTABLE(MemoryLockedLoadTimeValuesPointer_O,obj);
            obj->_LoadTimeValues = ltv;
            return obj;
        }

        LoadTimeValues_sp& ref() { return this->_LoadTimeValues; };
        LoadTimeValues_sp* ptr() { return &this->_LoadTimeValues; };

    }; //  class
    
}; // core namespace
TRANSLATE(core::MemoryLockedLoadTimeValuesPointer_O);



template<> struct gctools::GCInfo<core::MemoryLockedLoadTimeValuesPointer_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static GCInfo_policy constexpr Policy = unmanaged;
};

#endif

#endif /* _core_loadTimeValues_H_ */
