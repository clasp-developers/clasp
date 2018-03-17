/*
    File: methodptr.h
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
#ifndef methodptr_H
#define methodptr_H

#include <clasp/core/object.h>

namespace core {

#if 0
template <typename OT>
class OldStyleMethoid : public SingleDispatchMethoid
{
private:
	T_sp (OT::*fptr)(Function_sp,List_sp, Environment_sp, Lisp_sp );
public:
    virtual string describe() const {return "OldStyleMethoid";};
    OldStyleMethoid( const string& name, T_sp (OT::*fp)(Function_sp,List_sp, Environment_sp, Lisp_sp ) ) : SingleDispatchMethoid("OldStyleMethoid->"+name)
    {
	this->fptr = fp;
    };
};
#endif

/*! Methods that don't need to have argument/result translation are
 * stored as Methoid objects
 */
template <typename OT>
class Methoid : public SingleDispatchMethoid {
private:
  T_mv (OT::*fptr)(ActivationFrame_sp frame, int singleDispatchArgumentIndex);

public:
  Methoid(T_mv (OT::*fp)(ActivationFrame_sp)) {
    this->fptr = fp;
  };
  virtual string describe() const { return "Methoid"; };
  T_mv activate(ActivationFrame_sp closedEnv, const_ActivationFrame_spREF frame) {
    T_sp receiver = frame->entry(this->_SingleDispatchArgumentIndex);
    gctools::smart_ptr<OT> obj = receiver.as<OT>();
    T_mv result = ((obj.get())->*fptr)(frame, this->_SingleDispatchArgumentIndex);
    return result;
  }
};
};
#endif // methodptr_H
