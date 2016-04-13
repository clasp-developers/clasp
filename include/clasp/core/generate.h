#ifndef generate_H
#define generate_H

#include <clasp/core/foundation.h>
#include <clasp/core/compPackage.fwd.h>
#include <clasp/core/instance.h>


namespace comp {
  FORWARD(Ast);
  class Ast_O : public core::Instance_O {
    LISP_CLASS(comp,CompPkg,Ast_O,"AST",core::Instance_O);
  public:
    const size_t origin_field_idx = 0;
    const size_t num_fields = 1;
  public:
    CL_DEFMETHOD core::T_sp origin() const { return this->_Slots[origin_field_idx];};
    CL_DEFMETHOD void setf_origin(core::T_sp v) { this->_Slots[origin_field_idx] = v; };
  };
};


namespace comp {
  FORWARD(IfAst);
  class IfAst_O : public Ast_O {
    LISP_CLASS(comp,CompPkg,IfAst_O,"IF-AST",Ast_O);
  public:
    const size_t test_field_idx = Base::num_fields+0;
    const size_t then_field_idx = Base::num_fields+1;
    const size_t else_field_idx = Base::num_fields+2;
    const size_t num_fields = Base::num_fields+3;
  public:

    CL_DEFUN static IfAst_sp make_if_ast(core::T_sp test, core::T_sp then, core::T_sp else_=_Nil<core::T_O>()) {
      GC_ALLOCATE(IfAst_O,ast);
      ast->setf_test(test);
      ast->setf_then(then);
      ast->setf_else(else_);
      return ast;
    }
  public:
    CL_DEFMETHOD core::T_sp test() const { return this->_Slots[test_field_idx];};
    CL_DEFMETHOD void setf_test(core::T_sp v) { this->_Slots[test_field_idx] = v; };
    CL_DEFMETHOD core::T_sp then() const { return this->_Slots[then_field_idx];};
    CL_DEFMETHOD void setf_then(core::T_sp v) { this->_Slots[then_field_idx] = v; };
    CL_LISPIFY_NAME("cmp::else");
    CL_DEFMETHOD core::T_sp else_() const { return this->_Slots[else_field_idx];};
    CL_DEFMETHOD void setf_else(core::T_sp v) { this->_Slots[else_field_idx] = v; };
  };
};


#endif
