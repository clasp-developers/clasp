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
    const size_t slot_origin = 0;
    const size_t num_slots = 1;
  public:
    CL_DEFMETHOD core::T_sp origin() const { return this->_Slots[slot_origin];};
    CL_DEFMETHOD void setf_origin(core::T_sp v) { this->_Slots[slot_origin] = v; };

    virtual int number_of_slots() const {return num_slots};
    virtual core::List_sp children() const {SUBIMP();};
  };

  FORWARD(LoadTimeValueAst);
  class LoadTimeValueAst_O : public AstNode_O {
    LISP_CLASS(clc, ClcPkg, LoadTimeValueAst_O, "LoadTimeValueAst", AstNode_O);
  private:
  private:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_form = first_slot+0;
    static const size_t slot_read_only_p = first_slot+1;
    static const size_t num_slots = first_slot+2;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp form() const { return this->_Slots[slot_form]; };
    void setf_form(core::T_sp val) { return this->_Slots[slot_form] = val; }; 
    core::T_sp read_only_p() const { return this->_Slots[slot_read_only_p]; };
    void setf_read_only_p(core::T_sp val) { return this->_Slots[slot_read_only_p] = val; };
    core::List_sp children() const override { return core::Cons_O::createList(this->form());};
 public:
    void reinitialize(core::T_sp form, core::T_sp read_only_p) {
      this->instanceClassSet(LoadTimeValueAst_O::__class());
      this->initializeSlots(num_slots);
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
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_value = first_slot+0;
    static const size_t num_slots = 1;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp value() const { return this->_Slots[slot_value]; };
    void setf_value(core::T_sp val) { return this->_Slots[slot_value] = val; };
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

  FORWARD(IfAst);
  class IfAst_O : public Ast_O {
    LISP_CLASS(comp,CompPkg,IfAst_O,"IfAst",Ast_O);
  public:
    const size_t first_slot = Base::num_fields;
    const size_t slot_test = first_slot+0;
    const size_t slot_then = first_slot+1;
    const size_t slot_else = first_slot+2;
    const size_t num_fields = first_slot+3;
  public:

    CL_DEFUN static IfAst_sp make_if_ast(core::T_sp test, core::T_sp then, core::T_sp else_) {
      GC_ALLOCATE(IfAst_O,ast);
      ast->setf_test(test);
      ast->setf_then(then);
      ast->setf_else(else_);
      return ast;
    }
  public:
    CL_DEFMETHOD core::T_sp test() const { return this->_Slots[slot_test];};
    CL_DEFMETHOD void setf_test(core::T_sp v) { this->_Slots[slot_test] = v; };
    CL_DEFMETHOD core::T_sp then() const { return this->_Slots[slot_then];};
    CL_DEFMETHOD void setf_then(core::T_sp v) { this->_Slots[slot_then] = v; };
    CL_LISPIFY_NAME("cmp::else");
    CL_DEFMETHOD core::T_sp else_() const { return this->_Slots[slot_else];};
    CL_DEFMETHOD void setf_else(core::T_sp v) { this->_Slots[slot_else] = v; };
  };



  
};


#endif
