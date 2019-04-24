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

#include <clasp/core/object.h>
#include <clasp/core/loadTimeValues.fwd.h>

namespace core {
class LoadTimeValues_O : public General_O {
//  friend void(::sp_copyLoadTimeValue(T_sp *resultP, LoadTimeValues_O **ltvPP, size_t index));
  LISP_CLASS(core, CorePkg, LoadTimeValues_O, "LoadTimeValues",General_O);

public: // Simple default ctor/dtor
  LoadTimeValues_O() : Base(){};
  virtual ~LoadTimeValues_O(){};

public: // ctor/dtor for classes with shared virtual base
        //    explicit LoadTimeValues_O(core::Instance_sp const& mc) : T_O(mc), ComplexVector_TWithFillPtr(mc) {};
        //    virtual ~LoadTimeValues_O() {};

GCPRIVATE: // instance variables here
  gctools::Vec0<T_sp> _Objects;
//  gctools::Vec0<Symbol_sp> _Symbols;

public: // Functions here
  static LoadTimeValues_sp make(size_t dataDimension);


  T_sp &operator[](size_t index) { BOUNDS_ASSERT(index<this->_Objects.size()); return this->_Objects[index]; }

  int numberOfValues() const { return this->_Objects.size(); };
//  int numberOfSymbols() const { return this->_Symbols.size(); };

  void dumpValues(vector<gctools::Fixnum> &indices);
//  void dumpSymbols(vector<gctools::Fixnum> &indices);

  // -------- Regular data storage

  inline T_sp &data_element(size_t i) { return this->_Objects[i]; };
  size_t data_vectorPushExtend(T_sp val, size_t extension);
CL_DEFMETHOD  void load_time_value_array_setf(size_t index, T_sp object ) { this->_Objects[index] = object;};
  // -------- Symbols storage

//  void symbols_setFillPointer(uint i);
//  ALWAYS_INLINE Symbol_sp &symbols_element(uint i) { return this->_Symbols[i]; };
//  int symbols_vectorPushExtend(Symbol_sp val, int extension);

}; // LoadTimeValues class
};
template <>
struct gctools::GCInfo<core::LoadTimeValues_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = unmanaged;
};

#endif /* _core_loadTimeValues_H_ */
