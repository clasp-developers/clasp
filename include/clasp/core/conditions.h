/*
    File: conditions.h
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

#ifndef Conditions_H //[
#define Conditions_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/lispStream.fwd.h>
#include <clasp/core/conditions.fwd.h>
#include <clasp/core/object.h>

namespace core {
class CandoException_O : public General_O {
  LISP_CLASS(core, CorePkg, CandoException_O, "CandoException",General_O);
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(CandoException_O);

public: // ctor/dtor for classes with shared virtual base
        //    explicit CandoException_O(core::MetaClass_sp const& mc) : T_O(mc), T(mc) {};
        //    virtual ~CandoException_O() {};
public:
  static CandoException_sp create(const string &msg);
//  static CandoException_sp create(const boost::format &fmt);

GCPRIVATE: // instance variables here
  SimpleString_sp _message;

public: // Functions here
  void setMessage(const string &msg);
  string message() const;

}; // CandoException class

}; // core namespace

namespace core {

class Condition {
private:
  T_sp _ConditionObject;

public:
  T_sp conditionObject() const;
  void setConditionObject(T_sp co);
  string message() const;

  Condition(T_sp cond);
  Condition(const Condition &c);
  virtual ~Condition() throw();
};
};

namespace core {

T_sp cl__make_condition(T_sp datum, List_sp initializers);

string core__condition_message(T_sp condition);

}

#endif //]
