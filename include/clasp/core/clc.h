/*
    File: clc.h
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
#ifndef _core_clc_H_
#define _core_clc_H_

#include <clasp/core/object.h>
#include <clasp/core/instance.h>


NAMESPACE_PACKAGE_ASSOCIATION(clasp_cleavir,ClaspCleavirPkg,"CLASP-CLEAVIR");
NAMESPACE_PACKAGE_ASSOCIATION(clc,ClcPkg,"CLC");


// Typical arguments for convert functions
#define PASS_rest dummy_
#define PASS_env_rest env, PASS_rest
#define PASS_form_env_rest form, PASS_env_rest
#define ARGS_env_rest core::T_sp env, int dummy_
#define ARGS_form_env_rest core::T_sp form, core::T_sp env, int dummy_


#define LOG_NODE_CREATION() printf("%s:%d %s\n", __FILE__, __LINE__, __FUNCTION__ )
#define SETUP_SOURCE_INFO(s) /* See macro db */

namespace clc {

  FORWARD(Ast);
  class Ast_O : public core::Instance_O {
    LISP_CLASS(clc, ClcPkg, Ast_O, "AstNode", core::Instance_O);
  protected:
    static const size_t first_slot = 0;
    static const size_t slot_origin = first_slot+0;
    static const size_t num_slots = first_slot+1;
  public:
    CL_DEFMETHOD core::T_sp origin() const { return this->_Slots[slot_origin];};
    CL_DEFMETHOD void setf_origin(core::T_sp v) { this->_Slots[slot_origin] = v; };
    virtual int number_of_slots() const {return num_slots;};
    virtual core::List_sp children() {SUBIMP();};
    virtual bool boolean_ast_mixin_p() const { return false; };
    virtual bool no_value_ast_mixin_p() const { return false; };
    virtual bool one_value_ast_mixin_p() const { return false; };
    virtual bool side_effect_free_ast_mixin_p() const { return false; };
  public:
    core::T_sp id();
    std::string id_string();
    virtual std::string label() const {
      return lisp_classNameAsString(core::instance_class(this->asSmartPtr()));
    }
    virtual void draw_ast(core::T_sp stream);
  };


  FORWARD(ImmediateAst);
  class ImmediateAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, ImmediateAst_O, "ImmediateAst", Ast_O);
  public:
    virtual bool one_value_ast_mixin_p() const override {return true;};
    virtual bool side_effect_free_ast_mixin_p() const override {return true;};
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_value = first_slot+0;
    static const size_t num_slots = first_slot+1;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp value() const { return this->_Slots[slot_value]; };
    void setf_value(core::T_sp val) { this->_Slots[slot_value] = val; };
    core::List_sp children() override { return _Nil<core::T_O>(); };
  public:
    void reinitialize(core::T_sp value) {
      this->instanceClassSet(ImmediateAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_value(value);
    }
    ImmediateAst_O() {};
    virtual ~ImmediateAst_O() {};
  };
  CL_DEFUN inline ImmediateAst_sp make_ImmediateAst(core::T_sp value) {
    LOG_NODE_CREATION();
    GC_ALLOCATE(ImmediateAst_O, node);
    node->reinitialize(value);
    return node;
  }


  FORWARD(ConstantAst);
  class ConstantAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, ConstantAst_O, "ConstantAst", Ast_O);
  public:
    virtual bool one_value_ast_mixin_p() const override {return true;};
    virtual bool side_effect_free_ast_mixin_p() const override {return true;};
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_value = first_slot+0;
    static const size_t num_slots = first_slot+1;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp value() const { return this->_Slots[slot_value]; };
    void setf_value(core::T_sp val) { this->_Slots[slot_value] = val; };
    core::List_sp children() override { return _Nil<core::T_O>(); };
  public:
    void reinitialize(core::T_sp value) {
      this->instanceClassSet(ConstantAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_value(value);
    }
    ConstantAst_O() {};
    virtual ~ConstantAst_O() {};
  };
  CL_DEFUN inline ConstantAst_sp make_ConstantAst(core::T_sp value) {
    LOG_NODE_CREATION();
    GC_ALLOCATE(ConstantAst_O, node);
    node->reinitialize(value);
    return node;
  }

  FORWARD(LexicalAst);
  class LexicalAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, LexicalAst_O, "LexicalAst", Ast_O);
  public:
    virtual bool one_value_ast_mixin_p() const override {return true;};
    virtual bool side_effect_free_ast_mixin_p() const override {return true;};
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_name = first_slot+0;
    static const size_t num_slots = first_slot+1;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp name() const { return this->_Slots[slot_name]; };
    void setf_name(core::T_sp val) { this->_Slots[slot_name] = val; };
    core::List_sp children() override { return _Nil<core::T_O>(); };
  public:
    void reinitialize(core::T_sp name) {
      this->instanceClassSet(LexicalAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_name(name);
    }
    LexicalAst_O() {};
    virtual ~LexicalAst_O() {};
  };
  CL_DEFUN inline LexicalAst_sp make_LexicalAst(core::T_sp name) {
    LOG_NODE_CREATION();
    GC_ALLOCATE(LexicalAst_O, node);
    node->reinitialize(name);
    return node;
  }


  FORWARD(SymbolValueAst);
  class SymbolValueAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, SymbolValueAst_O, "SymbolValueAst", Ast_O);
  public:
    virtual bool one_value_ast_mixin_p() const override {return true;};
    virtual bool side_effect_free_ast_mixin_p() const override {return true;};
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_symbol_ast = first_slot+0;
    static const size_t num_slots = first_slot+1;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp symbol_ast() const { return this->_Slots[slot_symbol_ast]; };
    void setf_symbol_ast(core::T_sp val) { this->_Slots[slot_symbol_ast] = val; };
    core::List_sp children() override { return core::Cons_O::createList(this->symbol_ast()); };
  public:
    void reinitialize(core::T_sp symbol_ast) {
      this->instanceClassSet(SymbolValueAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_symbol_ast(symbol_ast);
    }
    SymbolValueAst_O() {};
    virtual ~SymbolValueAst_O() {};
  };
  CL_DEFUN inline SymbolValueAst_sp make_SymbolValueAst(core::T_sp symbol_ast) {
    LOG_NODE_CREATION();
    GC_ALLOCATE(SymbolValueAst_O, node);
    node->reinitialize(symbol_ast);
    return node;
  }

  FORWARD(SetSymbolValueAst);
  class SetSymbolValueAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, SetSymbolValueAst_O, "SetSymbolValueAst", Ast_O);
  public:
    virtual bool one_value_ast_mixin_p() const override {return true;};
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_symbol_ast = first_slot+0;
    static const size_t slot_value_ast = first_slot+1;
    static const size_t num_slots = first_slot+2;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp symbol_ast() const { return this->_Slots[slot_symbol_ast]; };
    void setf_symbol_ast(core::T_sp val) { this->_Slots[slot_symbol_ast] = val; };
    core::T_sp value_ast() const { return this->_Slots[slot_value_ast]; };
    void setf_value_ast(core::T_sp val) { this->_Slots[slot_value_ast] = val; };
    core::List_sp children() override {
      return core::Cons_O::createList(this->symbol_ast(),
                                      this->value_ast()); };
  public:
    void reinitialize(core::T_sp symbol_ast,core::T_sp value_ast) {
      this->instanceClassSet(SetSymbolValueAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_symbol_ast(symbol_ast);
      this->setf_value_ast(value_ast);
    }
    SetSymbolValueAst_O() {};
    virtual ~SetSymbolValueAst_O() {};
  };
  CL_DEFUN inline SetSymbolValueAst_sp make_SetSymbolValueAst(core::T_sp symbol_ast,core::T_sp value_ast) {
    LOG_NODE_CREATION();
    GC_ALLOCATE(SetSymbolValueAst_O, node);
    node->reinitialize(symbol_ast,value_ast);
    return node;
  }

  
  FORWARD(FDefinitionAst);
  class FDefinitionAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, FDefinitionAst_O, "FDefinitionAst", Ast_O);
  public:
    virtual bool one_value_ast_mixin_p() const override {return true;};
    virtual bool side_effect_free_ast_mixin_p() const override {return true;};
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_name_ast = first_slot+0;
    static const size_t slot_info = first_slot+1;
    static const size_t num_slots = first_slot+2;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp name_ast() const { return this->_Slots[slot_name_ast]; };
    void setf_name_ast(core::T_sp val) { this->_Slots[slot_name_ast] = val; };
    core::T_sp info() const { return this->_Slots[slot_info]; };
    void setf_info(core::T_sp val) { this->_Slots[slot_info] = val; };
    core::List_sp children() override { return core::Cons_O::createList(this->name_ast()); };
  public:
    void reinitialize(core::T_sp name_ast,core::T_sp info) {
      this->instanceClassSet(FDefinitionAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_name_ast(name_ast);
      this->setf_info(info);
    }
    FDefinitionAst_O() {};
    virtual ~FDefinitionAst_O() {};
  };
  CL_DEFUN inline FDefinitionAst_sp make_FDefinitionAst(core::T_sp name_ast,core::T_sp info) {
    LOG_NODE_CREATION();
    GC_ALLOCATE(FDefinitionAst_O, node);
    node->reinitialize(name_ast,info);
    return node;
  }

  FORWARD(CallAst);
  class CallAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, CallAst_O, "CallAst", Ast_O);
  public:
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_callee_ast = first_slot+0;
    static const size_t slot_argument_asts = first_slot+1;
    static const size_t num_slots = first_slot+2;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp callee_ast() const { return this->_Slots[slot_callee_ast]; };
    void setf_callee_ast(core::T_sp val) { this->_Slots[slot_callee_ast] = val; };
    core::T_sp argument_asts() const { return this->_Slots[slot_argument_asts]; };
    void setf_argument_asts(core::T_sp val) { this->_Slots[slot_argument_asts] = val; };
    core::List_sp children() override {
      return core::Cons_O::createList(this->callee_ast(),
                                      this->argument_asts()); };
  public:
    void reinitialize(core::T_sp callee_ast,core::T_sp argument_asts) {
      this->instanceClassSet(CallAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_callee_ast(callee_ast);
      this->setf_argument_asts(argument_asts);
    }
    CallAst_O() {};
    virtual ~CallAst_O() {};
  };
  CL_DEFUN inline CallAst_sp make_CallAst(core::T_sp callee_ast,core::T_sp argument_asts) {
    LOG_NODE_CREATION();
    GC_ALLOCATE(CallAst_O, node);
    node->reinitialize(callee_ast,argument_asts);
    return node;
  }

  FORWARD(FunctionAst);
  class FunctionAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, FunctionAst_O, "FunctionAst", Ast_O);
  public:
    virtual bool one_value_ast_mixin_p() const override {return true;};
    virtual bool side_effect_free_ast_mixin_p() const override {return true;};
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_lambda_list = first_slot+0;
    static const size_t slot_body_ast = first_slot+1;
    static const size_t num_slots = first_slot+2;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp lambda_list() const { return this->_Slots[slot_lambda_list]; };
    void setf_lambda_list(core::T_sp val) { this->_Slots[slot_lambda_list] = val; };
    core::T_sp body_ast() const { return this->_Slots[slot_body_ast]; };
    void setf_body_ast(core::T_sp val) { this->_Slots[slot_body_ast] = val; };
    core::List_sp children() override; 
  public:
    void reinitialize(core::T_sp lambda_list,core::T_sp body_ast) {
      this->instanceClassSet(FunctionAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_lambda_list(lambda_list);
      this->setf_body_ast(body_ast);
    }
    FunctionAst_O() {};
    virtual ~FunctionAst_O() {};
  };
  CL_DEFUN inline FunctionAst_sp make_FunctionAst(core::T_sp lambda_list,core::T_sp body_ast) {
    LOG_NODE_CREATION();
    GC_ALLOCATE(FunctionAst_O, node);
    node->reinitialize(lambda_list,body_ast);
    return node;
  }


  FORWARD(TopLevelFunctionAst);
  class TopLevelFunctionAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, TopLevelFunctionAst_O, "TopLevelFunctionAst", Ast_O);
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_forms = first_slot+0;
    static const size_t num_slots = first_slot+1;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp forms() const { return this->_Slots[slot_forms]; };
    void setf_forms(core::T_sp val) { this->_Slots[slot_forms] = val; };
  public:
    void reinitialize(core::T_sp forms) {
      this->instanceClassSet(TopLevelFunctionAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_forms(forms);
    }
    TopLevelFunctionAst_O() {};
    virtual ~TopLevelFunctionAst_O() {};
  };
  CL_DEFUN inline TopLevelFunctionAst_sp make_TopLevelFunctionAst(core::T_sp forms) {
    LOG_NODE_CREATION();
    GC_ALLOCATE(TopLevelFunctionAst_O, node);
    node->reinitialize(forms);
    return node;
  }

  FORWARD(PrognAst);
  class PrognAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, PrognAst_O, "PrognAst", Ast_O);
  protected:
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_form_asts = first_slot+0;
    static const size_t num_slots = first_slot+1;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp form_asts() const { return this->_Slots[slot_form_asts]; };
    void setf_form_asts(core::T_sp val) { this->_Slots[slot_form_asts] = val; }; 
    core::List_sp children() override { return this->form_asts();};
  public:
    void reinitialize(core::T_sp form_asts) {
      this->instanceClassSet(PrognAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_form_asts(form_asts);
    }
    PrognAst_O() {};
    virtual ~PrognAst_O() {};
  };
  CL_DEFUN inline PrognAst_sp make_PrognAst(core::T_sp form_asts) {
      LOG_NODE_CREATION();
      GC_ALLOCATE(PrognAst_O, node);
      node->reinitialize(form_asts);
      return node;
    }

  FORWARD(BlockAst);
  class BlockAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, BlockAst_O, "BlockAst", Ast_O);
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_body_ast = first_slot+0;
    static const size_t num_slots = first_slot+1;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp body_ast() const { return this->_Slots[slot_body_ast]; };
    void setf_body_ast(core::T_sp val) { this->_Slots[slot_body_ast] = val; }; 
    core::List_sp children() override { return core::Cons_O::create(this->body_ast());};
  public:
    void reinitialize(core::T_sp body_ast) {
      this->instanceClassSet(BlockAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_body_ast(body_ast);
    }
    BlockAst_O() {};
    virtual ~BlockAst_O() {};
  };
  CL_DEFUN inline BlockAst_sp make_BlockAst(core::T_sp body_ast) {
      LOG_NODE_CREATION();
      GC_ALLOCATE(BlockAst_O, node);
      node->reinitialize(body_ast);
      return node;
    }

  FORWARD(ReturnFromAst);
  class ReturnFromAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, ReturnFromAst_O, "ReturnFromAst", Ast_O);
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_block_ast = first_slot+0;
    static const size_t slot_form_ast = first_slot+1;
    static const size_t num_slots = first_slot+2;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp block_ast() const { return this->_Slots[slot_block_ast]; };
    void setf_block_ast(core::T_sp val) { this->_Slots[slot_block_ast] = val; };
    core::T_sp form_ast() const { return this->_Slots[slot_form_ast]; };
    void setf_form_ast(core::T_sp val) { this->_Slots[slot_form_ast] = val; };
    core::List_sp children() override { return core::Cons_O::createList(this->block_ast(),this->form_ast());}; 
  public:
    void reinitialize(core::T_sp block_ast,core::T_sp form_ast) {
      this->instanceClassSet(ReturnFromAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_block_ast(block_ast);
      this->setf_form_ast(form_ast);
    }
    ReturnFromAst_O() {};
    virtual ~ReturnFromAst_O() {};
  };
  CL_DEFUN inline ReturnFromAst_sp make_ReturnFromAst(core::T_sp block_ast,core::T_sp form_ast) {
    LOG_NODE_CREATION();
    GC_ALLOCATE(ReturnFromAst_O, node);
    node->reinitialize(block_ast,form_ast);
    return node;
  }


  FORWARD(SetqAst);
  class SetqAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, SetqAst_O, "SetqAst", Ast_O);
  public:
    virtual bool no_value_ast_mixin_p() const { return true; };
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_lhs_ast = first_slot+0;
    static const size_t slot_value_ast = first_slot+1;
    static const size_t num_slots = first_slot+2;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp lhs_ast() const { return this->_Slots[slot_lhs_ast]; };
    void setf_lhs_ast(core::T_sp val) { this->_Slots[slot_lhs_ast] = val; };
    core::T_sp value_ast() const { return this->_Slots[slot_value_ast]; };
    void setf_value_ast(core::T_sp val) { this->_Slots[slot_value_ast] = val; };
    core::List_sp children() override { return core::Cons_O::createList(this->lhs_ast(),this->value_ast());}; 
  public:
    void reinitialize(core::T_sp lhs_ast,core::T_sp value_ast) {
      this->instanceClassSet(SetqAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_lhs_ast(lhs_ast);
      this->setf_value_ast(value_ast);
    }
    SetqAst_O() {};
    virtual ~SetqAst_O() {};
  };
  CL_DEFUN inline SetqAst_sp make_SetqAst(core::T_sp lhs_ast,core::T_sp value_ast) {
    LOG_NODE_CREATION();
    GC_ALLOCATE(SetqAst_O, node);
    node->reinitialize(lhs_ast,value_ast);
    return node;
  }

  
  FORWARD(MultipleValueSetqAst);
  class MultipleValueSetqAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, MultipleValueSetqAst_O, "MultipleValueSetqAst", Ast_O);
  public:
    virtual bool no_value_ast_mixin_p() const { return true; };
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_lhs_asts = first_slot+0;
    static const size_t slot_form_ast = first_slot+1;
    static const size_t num_slots = first_slot+2;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp lhs_asts() const { return this->_Slots[slot_lhs_asts]; };
    void setf_lhs_asts(core::T_sp val) { this->_Slots[slot_lhs_asts] = val; };
    core::T_sp form_ast() const { return this->_Slots[slot_form_ast]; };
    void setf_form_ast(core::T_sp val) { this->_Slots[slot_form_ast] = val; };
    core::List_sp children() override { return core::Cons_O::create(this->form_ast(),this->lhs_asts());}; 
  public:
    void reinitialize(core::T_sp lhs_asts,core::T_sp form_ast) {
      this->instanceClassSet(MultipleValueSetqAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_lhs_asts(lhs_asts);
      this->setf_form_ast(form_ast);
    }
    MultipleValueSetqAst_O() {};
    virtual ~MultipleValueSetqAst_O() {};
  };
  CL_DEFUN inline MultipleValueSetqAst_sp make_MultipleValueSetqAst(core::T_sp lhs_asts,core::T_sp form_ast) {
    LOG_NODE_CREATION();
    GC_ALLOCATE(MultipleValueSetqAst_O, node);
    node->reinitialize(lhs_asts,form_ast);
    return node;
  }


  FORWARD(TagAst);
  class TagAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, TagAst_O, "TagAst", Ast_O);
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_name = first_slot+0;
    static const size_t num_slots = first_slot+1;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp name() const { return this->_Slots[slot_name]; };
    void setf_name(core::T_sp val) { this->_Slots[slot_name] = val; }; 
    core::List_sp children() override { return _Nil<core::T_O>(); };
  public:
    void reinitialize(core::T_sp name) {
      this->instanceClassSet(TagAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_name(name);
    }
    TagAst_O() {};
    virtual ~TagAst_O() {};
  };
  CL_DEFUN inline TagAst_sp make_TagAst(core::T_sp name) {
      LOG_NODE_CREATION();
      GC_ALLOCATE(TagAst_O, node);
      node->reinitialize(name);
      return node;
    }

  FORWARD(TagbodyAst);
  class TagbodyAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, TagbodyAst_O, "TagbodyAst", Ast_O);
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_item_asts = first_slot+0;
    static const size_t num_slots = first_slot+1;
  public:
    virtual bool no_value_ast_mixin_p() const { return true; };
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp item_asts() const { return this->_Slots[slot_item_asts]; };
    void setf_item_asts(core::T_sp val) { this->_Slots[slot_item_asts] = val; }; 
    core::List_sp children() override { return this->item_asts(); };
  public:
    void reinitialize(core::T_sp item_asts) {
      this->instanceClassSet(TagbodyAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_item_asts(item_asts);
    }
    TagbodyAst_O() {};
    virtual ~TagbodyAst_O() {};
  };
  CL_DEFUN inline TagbodyAst_sp make_TagbodyAst(core::T_sp item_asts) {
      LOG_NODE_CREATION();
      GC_ALLOCATE(TagbodyAst_O, node);
      node->reinitialize(item_asts);
      return node;
  }

  FORWARD(GoAst);
  class GoAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, GoAst_O, "GoAst", Ast_O);
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_tag_ast = first_slot+0;
    static const size_t num_slots = first_slot+1;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp tag_ast() const { return this->_Slots[slot_tag_ast]; };
    void setf_tag_ast(core::T_sp val) { this->_Slots[slot_tag_ast] = val; }; 
    core::List_sp children() override { return core::Cons_O::createList(this->tag_ast()); };
  public:
    void reinitialize(core::T_sp tag_ast) {
      this->instanceClassSet(GoAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_tag_ast(tag_ast);
    }
    GoAst_O() {};
    virtual ~GoAst_O() {};
  };
  CL_DEFUN inline GoAst_sp make_GoAst(core::T_sp tag_ast) {
    LOG_NODE_CREATION();
    GC_ALLOCATE(GoAst_O, node);
    node->reinitialize(tag_ast);
    return node;
  }

  FORWARD(TheAst);
  class TheAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, TheAst_O, "TheAst", Ast_O);
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_form_ast = first_slot+0;
    static const size_t slot_type_specifiers = first_slot+1;
    static const size_t num_slots = first_slot+2;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp form_ast() const { return this->_Slots[slot_form_ast]; };
    void setf_form_ast(core::T_sp val) { this->_Slots[slot_form_ast] = val; };
    core::T_sp type_specifiers() const { return this->_Slots[slot_type_specifiers]; };
    void setf_type_specifiers(core::T_sp val) { this->_Slots[slot_type_specifiers] = val; };
    core::List_sp children() override { return core::Cons_O::createList(this->form_ast());}; 
  public:
    void reinitialize(core::T_sp form_ast,core::T_sp type_specifiers) {
      this->instanceClassSet(TheAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_form_ast(form_ast);
      this->setf_type_specifiers(type_specifiers);
    }
    TheAst_O() {};
    virtual ~TheAst_O() {};
  };
  CL_DEFUN inline TheAst_sp make_TheAst(core::T_sp form_ast,core::T_sp type_specifiers) {
    LOG_NODE_CREATION();
    GC_ALLOCATE(TheAst_O, node);
    node->reinitialize(form_ast,type_specifiers);
    return node;
  }

    FORWARD(TypeqAst);
  class TypeqAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, TypeqAst_O, "TypeqAst", Ast_O);
  public:
    bool boolean_ast_mixin_p() const { return true; };
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_type_specifier = first_slot+0;
    static const size_t slot_type_specifier_ast = first_slot+1;
    static const size_t slot_form_ast = first_slot+2;
    static const size_t num_slots = first_slot+3;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp type_specifier() const { return this->_Slots[slot_type_specifier]; };
    void setf_type_specifier(core::T_sp val) { this->_Slots[slot_type_specifier] = val; };
    core::T_sp type_specifier_ast_inner() const { return this->_Slots[slot_type_specifier_ast]; };
    core::T_sp type_specifier_ast();
    void setf_type_specifier_ast(core::T_sp val) { this->_Slots[slot_type_specifier_ast] = val; };
    core::T_sp form_ast() const { return this->_Slots[slot_form_ast]; };
    void setf_form_ast(core::T_sp val) { this->_Slots[slot_form_ast] = val; };
    core::List_sp children() override { return core::Cons_O::createList(this->form_ast(),this->type_specifier_ast());}; 
  public:
    void reinitialize(core::T_sp type_specifier,core::T_sp type_specifier_ast, core::T_sp form_ast) {
      this->instanceClassSet(TypeqAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_type_specifier(type_specifier);
      this->setf_type_specifier_ast(type_specifier_ast);
      this->setf_form_ast(form_ast);
    }
    TypeqAst_O() {};
    virtual ~TypeqAst_O() {};
  };
  CL_DEFUN inline TypeqAst_sp make_TypeqAst(core::T_sp type_specifier,core::T_sp type_specifier_ast,core::T_sp form_ast) {
    LOG_NODE_CREATION();
    GC_ALLOCATE(TypeqAst_O, node);
    node->reinitialize(type_specifier,type_specifier_ast,form_ast);
    return node;
  }

  FORWARD(LoadTimeValueAst);
  class LoadTimeValueAst_O : public Ast_O {
    LISP_CLASS(clc, ClcPkg, LoadTimeValueAst_O, "LoadTimeValueAst", Ast_O);
  public:
    bool one_value_ast_mixin_p() const {return true; };
  protected:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_form = first_slot+0;
    static const size_t slot_read_only_p = first_slot+1;
    static const size_t num_slots = first_slot+2;
  public:
    virtual int number_of_slots() const override {return num_slots;};
    core::T_sp form() const { return this->_Slots[slot_form]; };
    void setf_form(core::T_sp val) { this->_Slots[slot_form] = val; }; 
    core::T_sp read_only_p() const { return this->_Slots[slot_read_only_p]; };
    void setf_read_only_p(core::T_sp val) { this->_Slots[slot_read_only_p] = val; };
    core::List_sp children() override { return _Nil<core::T_O>(); };
  public:
    void reinitialize(core::T_sp form, core::T_sp read_only_p) {
      this->instanceClassSet(LoadTimeValueAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_form(form);
      this->setf_read_only_p(read_only_p);
    }
    LoadTimeValueAst_O() {};
    virtual ~LoadTimeValueAst_O() {};
  };
  CL_DEFUN inline LoadTimeValueAst_sp make_LoadTimeValueAst(core::T_sp form, core::T_sp read_only_p) {
      LOG_NODE_CREATION();
      GC_ALLOCATE(LoadTimeValueAst_O, node);
      node->reinitialize(form,read_only_p);
      return node;
  }


  FORWARD(IfAst);
  class IfAst_O : public Ast_O {
    LISP_CLASS(clc,ClcPkg,IfAst_O,"IfAst",Ast_O);
  public:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_test_ast = first_slot+0;
    static const size_t slot_then_ast = first_slot+1;
    static const size_t slot_else_ast = first_slot+2;
    static const size_t num_slots = first_slot+3;
  public:
    CL_DEFMETHOD core::T_sp test_ast() const { return this->_Slots[slot_test_ast];};
    CL_DEFMETHOD void setf_test_ast(core::T_sp v) { this->_Slots[slot_test_ast] = v; };
    CL_DEFMETHOD core::T_sp then_ast() const { return this->_Slots[slot_then_ast];};
    CL_DEFMETHOD void setf_then_ast(core::T_sp v) { this->_Slots[slot_then_ast] = v; };
    CL_LISPIFY_NAME("cmp::else_ast");
    CL_DEFMETHOD core::T_sp else_ast() const { return this->_Slots[slot_else_ast];};
    CL_DEFMETHOD void setf_else_ast(core::T_sp v) { this->_Slots[slot_else_ast] = v; };
    core::List_sp children() override { return core::Cons_O::createList(this->test_ast(),this->then_ast(),this->else_ast()); };
  public:
    void reinitialize(core::T_sp test_ast, core::T_sp then_ast, core::T_sp else_ast) {
      this->instanceClassSet(IfAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_test_ast(test_ast);
      this->setf_then_ast(then_ast);
      this->setf_else_ast(else_ast);
    }
    IfAst_O() {};
    virtual ~IfAst_O() {};
  };
  CL_DEFUN inline IfAst_sp make_IfAst(core::T_sp test_ast, core::T_sp then_ast, core::T_sp else_ast) {
    GC_ALLOCATE(IfAst_O,ast);
    ast->reinitialize(test_ast,then_ast,else_ast);
    return ast;
  }

FORWARD(MultipleValueCallAst);
class MultipleValueCallAst_O : public Ast_O {
  LISP_CLASS(clc, ClcPkg, MultipleValueCallAst_O, "MultipleValueCallAst", Ast_O);
 protected:
  static const size_t first_slot = Base::num_slots;
  static const size_t slot_function_form_ast = first_slot+0;
  static const size_t slot_form_asts = first_slot+1;
  static const size_t num_slots = first_slot+2;
 public:
  virtual int number_of_slots() const override {return num_slots;};
  core::T_sp function_form_ast() const { return this->_Slots[slot_function_form_ast]; };
  void setf_function_form_ast(core::T_sp val) { this->_Slots[slot_function_form_ast] = val; }; 
  core::T_sp form_asts() const { return this->_Slots[slot_form_asts]; };
  void setf_form_asts(core::T_sp val) { this->_Slots[slot_form_asts] = val; };
  core::List_sp children() override { return core::Cons_O::create(this->function_form_ast(),this->form_asts());};
 public:
  void reinitialize(core::T_sp function_form_ast, core::T_sp form_asts) {
    this->instanceClassSet(MultipleValueCallAst_O::__class());
    this->initializeSlots(num_slots);
    this->setf_function_form_ast(function_form_ast);
    this->setf_form_asts(form_asts);
  }
  MultipleValueCallAst_O() {};
  virtual ~MultipleValueCallAst_O() {};
};
CL_DEFUN inline MultipleValueCallAst_sp make_MultipleValueCallAst(core::T_sp function_form_ast, core::T_sp form_asts) {
  LOG_NODE_CREATION();
  GC_ALLOCATE(MultipleValueCallAst_O, node);
  node->reinitialize(function_form_ast,form_asts);
  return node;
}

FORWARD(MultipleValueProg1Ast);
class MultipleValueProg1Ast_O : public Ast_O {
  LISP_CLASS(clc, ClcPkg, MultipleValueProg1Ast_O, "MultipleValueProg1Ast", Ast_O);
 protected:
  static const size_t first_slot = Base::num_slots;
  static const size_t slot_first_form_ast = first_slot+0;
  static const size_t slot_form_asts = first_slot+1;
  static const size_t num_slots = first_slot+2;
 public:
  virtual int number_of_slots() const override {return num_slots;};
  core::T_sp first_form_ast() const { return this->_Slots[slot_first_form_ast]; };
  void setf_first_form_ast(core::T_sp val) { this->_Slots[slot_first_form_ast] = val; }; 
  core::T_sp form_asts() const { return this->_Slots[slot_form_asts]; };
  void setf_form_asts(core::T_sp val) { this->_Slots[slot_form_asts] = val; };
  core::List_sp children() override { return core::Cons_O::create(this->first_form_ast(),this->form_asts());};
 public:
  void reinitialize(core::T_sp first_form_ast, core::T_sp form_asts) {
    this->instanceClassSet(MultipleValueProg1Ast_O::__class());
    this->initializeSlots(num_slots);
    this->setf_first_form_ast(first_form_ast);
    this->setf_form_asts(form_asts);
  }
  MultipleValueProg1Ast_O() {};
  virtual ~MultipleValueProg1Ast_O() {};
};
CL_DEFUN inline MultipleValueProg1Ast_sp make_MultipleValueProg1Ast(core::T_sp first_form_ast, core::T_sp form_asts) {
  LOG_NODE_CREATION();
  GC_ALLOCATE(MultipleValueProg1Ast_O, node);
  node->reinitialize(first_form_ast,form_asts);
  return node;
}

  FORWARD(BindAst);
  class BindAst_O : public Ast_O {
    LISP_CLASS(clc,ClcPkg,BindAst_O,"BindAst",Ast_O);
  public:
    static const size_t first_slot = Base::num_slots;
    static const size_t slot_symbol = first_slot+0;
    static const size_t slot_value_ast = first_slot+1;
    static const size_t slot_body_ast = first_slot+2;
    static const size_t num_slots = first_slot+3;
  public:
    CL_DEFMETHOD core::T_sp symbol() const { return this->_Slots[slot_symbol];};
    CL_DEFMETHOD void setf_symbol(core::T_sp v) { this->_Slots[slot_symbol] = v; };
    CL_DEFMETHOD core::T_sp value_ast() const { return this->_Slots[slot_value_ast];};
    CL_DEFMETHOD void setf_value_ast(core::T_sp v) { this->_Slots[slot_value_ast] = v; };
    CL_LISPIFY_NAME("cmp::body_ast");
    CL_DEFMETHOD core::T_sp body_ast() const { return this->_Slots[slot_body_ast];};
    CL_DEFMETHOD void setf_body_ast(core::T_sp v) { this->_Slots[slot_body_ast] = v; };
    core::List_sp children() override { return core::Cons_O::createList(this->value_ast(),this->body_ast()); };
  public:
    void reinitialize(core::T_sp symbol, core::T_sp value_ast, core::T_sp body_ast) {
      this->instanceClassSet(BindAst_O::__class());
      this->initializeSlots(num_slots);
      this->setf_symbol(symbol);
      this->setf_value_ast(value_ast);
      this->setf_body_ast(body_ast);
    }
    BindAst_O() {};
    virtual ~BindAst_O() {};
  };
  CL_DEFUN inline BindAst_sp make_BindAst(core::T_sp symbol, core::T_sp value_ast, core::T_sp body_ast) {
    GC_ALLOCATE(BindAst_O,ast);
    ast->reinitialize(symbol,value_ast,body_ast);
    return ast;
  }

FORWARD(EqAst);
class EqAst_O : public Ast_O {
  LISP_CLASS(clc, ClcPkg, EqAst_O, "EqAst", Ast_O);
 protected:
  static const size_t first_slot = Base::num_slots;
  static const size_t slot_arg1_ast = first_slot+0;
  static const size_t slot_arg2_ast = first_slot+1;
  static const size_t num_slots = first_slot+2;
 public:
  virtual int number_of_slots() const override {return num_slots;};
  core::T_sp arg1_ast() const { return this->_Slots[slot_arg1_ast]; };
  void setf_arg1_ast(core::T_sp val) { this->_Slots[slot_arg1_ast] = val; }; 
  core::T_sp arg2_ast() const { return this->_Slots[slot_arg2_ast]; };
  void setf_arg2_ast(core::T_sp val) { this->_Slots[slot_arg2_ast] = val; };
  core::List_sp children() override { return core::Cons_O::createList(this->arg1_ast(),this->arg2_ast());};
 public:
  void reinitialize(core::T_sp arg1_ast, core::T_sp arg2_ast) {
    this->instanceClassSet(EqAst_O::__class());
    this->initializeSlots(num_slots);
    this->setf_arg1_ast(arg1_ast);
    this->setf_arg2_ast(arg2_ast);
  }
  EqAst_O() {};
  virtual ~EqAst_O() {};
};
CL_DEFUN inline EqAst_sp make_EqAst(core::T_sp arg1_ast, core::T_sp arg2_ast) {
  LOG_NODE_CREATION();
  GC_ALLOCATE(EqAst_O, node);
  node->reinitialize(arg1_ast,arg2_ast);
  return node;
}
  
};


namespace clc {

  Ast_sp convert_special(core::T_sp head, ARGS_form_env_rest );
  Ast_sp convert(ARGS_form_env_rest);
  Ast_sp convert_constant( ARGS_form_env_rest );


  
};
#endif

