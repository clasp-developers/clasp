/*
    File: ast.h
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
#ifndef core_ast_H
#define core_ast_H

#include <clasp/core/foundation.h>
#include <clasp/core/ql.h>
#include <clasp/core/evaluator.fwd.h>
#include <clasp/core/executables.h>
#include <clasp/core/clc.h>
#include <clasp/core/instance.h>
#include <clasp/core/activationFrame.h>

#define LOG_NODE_CREATION() printf("%s:%d %s\n", __FILE__, __LINE__, __FUNCTION__ )

namespace clc {

  FORWARD(AstData);
  class AstNode_O : public core::Instance_O {
    LISP_CLASS(clc, ClcPkg, AstNode_O, "AstNode", core::Instance_O);
  private:
  public:
    virtual int number_of_slots() const {SUBIMP();};
    virtual core::List_sp children() const {SUBIMP();};
    AstNode_O() {};
    virtual ~AstNode_O() {};
  };

  FORWARD(LoadTimeValueAst);
  class LoadTimeValueAst_O : public AstNode_O {
    LISP_CLASS(clc, ClcPkg, LoadTimeValueAst_O, "LoadTimeValueAst", AstNode_O);
  private:
  private:
    static const int Slot_form = 0;
    static const int Slot_read_only_p = 1;
    static const int NumSlots = 2;
  public:
    virtual int number_of_slots() const override {return NumSlots;};
    core::T_sp form() const { return this->instanceRef(Slot_form); };
    core::T_sp setf_form(core::T_sp val) { return this->instanceSet(Slot_form,val); }; 
    core::T_sp read_only_p() const { return this->instanceRef(Slot_read_only_p); };
    core::T_sp setf_read_only_p(core::T_sp val) { return this->instanceSet(Slot_read_only_p,val); };
    core::List_sp children() const override { return core::Cons_O::createList(this->form());};
 public:
    void reinitialize(core::T_sp form, core::T_sp read_only_p) {
      this->instanceClassSet(LoadTimeValueAst_O::__class());
      this->initializeSlots(NumSlots);
      this->setf_form(form);
      this->setf_read_only_p(read_only_p);
    }
    static LoadTimeValueAst_sp make(core::T_sp form, core::T_sp read_only_p) {
      LOG_NODE_CREATION();
      GC_ALLOCATE(LoadTimeValueAst_O, node);
      node->reinitialize(form,read_only_p);
      return node;
    }
    LoadTimeValueAst_O() {};
    virtual ~LoadTimeValueAst_O() {};
  };

  FORWARD(ImmediateAst);
  class ImmediateAst_O : public AstNode_O {
    LISP_CLASS(clc, ClcPkg, ImmediateAst_O, "ImmediateAst", AstNode_O);
  private:
    static const int Slot_value = 0;
    static const int NumSlots = 1;
  public:
    virtual int number_of_slots() const override {return NumSlots;};
    core::T_sp value() const { return this->instanceRef(Slot_value); };
    core::T_sp setf_value(core::T_sp val) { return this->instanceSet(Slot_value,val); };
    core::List_sp children() const override { return core::Cons_O::createList(this->value());};
  public:
    void reinitialize(core::T_sp value) {
      this->instanceClassSet(ImmediateAst_O::__class());
      this->initializeSlots(NumSlots);
      this->setf_value(value);
    }
    static ImmediateAst_sp make(core::T_sp value) {
      LOG_NODE_CREATION();
      GC_ALLOCATE(ImmediateAst_O, node);
      node->reinitialize(value);
      return node;
    }
    ImmediateAst_O() {};
    virtual ~ImmediateAst_O() {};
  };
};


#endif
