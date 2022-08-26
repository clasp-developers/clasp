#ifndef bytecode_compiler_H
#define bytecode_compiler_H

#include <clasp/core/common.h>

namespace core {

// Structures for the bytecode compiler.
class BytecodeCmpVarInfo_O : public General_O {
  LISP_ABSTRACT_CLASS(core, CorePkg, BytecodeCmpVarInfo_O, "BytecodeCmpVarInfo", General_O);
public:
  BytecodeCmpVarInfo_O() {};
};

FORWARD(BytecodeCmpLexicalVarInfo);
class BytecodeCmpLexicalVarInfo_O : public BytecodeCmpVarInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeCmpLexicalVarInfo_O, "BytecodeCmpLexicalVarInfo", BytecodeCmpVarInfo_O);
public:
  size_t frame_index;
  T_sp _funct;
  bool closed_over_p = false;
  bool set_p = false;
public:
  BytecodeCmpLexicalVarInfo_O(size_t ind, T_sp nfunct)
    : BytecodeCmpVarInfo_O(), frame_index(ind), _funct(nfunct) {};
  CL_LISPIFY_NAME(BytecodeCmpLexicalVarInfo/make)
  CL_DEF_CLASS_METHOD
  static BytecodeCmpLexicalVarInfo_sp make(size_t frame_index, T_sp funct) {
    return gctools::GC<BytecodeCmpLexicalVarInfo_O>::allocate<gctools::RuntimeStage>(frame_index, funct);
  }
  CL_DEFMETHOD size_t frameIndex() const { return this->frame_index; }
  CL_LISPIFY_NAME(BytecodeCmpLexicalVarInfo/function)
  CL_DEFMETHOD T_sp funct() const { return this->_funct; }
  CL_DEFMETHOD bool closedOverP() const { return this->closed_over_p; }
  CL_LISPIFY_NAME(BytecodeCmpLexicalVarInfo/setf-closed-over-p)
  CL_DEFMETHOD bool setClosedOverP(bool n) {
    this->closed_over_p = n;
    return n;
  }
  CL_DEFMETHOD bool setP() const { return this->set_p; }
  CL_LISPIFY_NAME(BytecodeCmpLexicalVarInfo/setf-set-p)
  CL_DEFMETHOD bool setSetP(bool n) {
    this->set_p = n;
    return n;
  }
};

FORWARD(BytecodeCmpSpecialVarInfo);
class BytecodeCmpSpecialVarInfo_O : public BytecodeCmpVarInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeCmpSpecialVarInfo_O, "BytecodeCmpSpecialVarInfo", BytecodeCmpVarInfo_O);
public:
  BytecodeCmpSpecialVarInfo_O() : BytecodeCmpVarInfo_O() {};
  CL_LISPIFY_NAME(BytecodeCmpSpecialVarInfo/make)
  CL_DEF_CLASS_METHOD
  static BytecodeCmpSpecialVarInfo_sp make() {
    BytecodeCmpSpecialVarInfo_sp info = gctools::GC<BytecodeCmpSpecialVarInfo_O>::allocate<gctools::RuntimeStage>();
    return info;
  }
};

FORWARD(BytecodeCmpSymbolMacroVarInfo);
class BytecodeCmpSymbolMacroVarInfo_O : public BytecodeCmpVarInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeCmpSymbolMacroVarInfo_O, "BytecodeCmpSymbolMacroVarInfo", BytecodeCmpVarInfo_O);
public:
  Function_sp _expander;
public:
  BytecodeCmpSymbolMacroVarInfo_O(T_sp n_expander)
    : BytecodeCmpVarInfo_O(), _expander(n_expander) {};
  CL_LISPIFY_NAME(BytecodeCmpSymbolMacroVarInfo/make)
  CL_DEF_CLASS_METHOD
  static BytecodeCmpSymbolMacroVarInfo_sp make(T_sp expander) {
    BytecodeCmpSymbolMacroVarInfo_sp info = gctools::GC<BytecodeCmpSymbolMacroVarInfo_O>::allocate<gctools::RuntimeStage>(expander);
    return info;
  }
  CL_DEFMETHOD T_sp expander() const { return this->_expander; }
};

FORWARD(BytecodeCmpConstantVarInfo);
class BytecodeCmpConstantVarInfo_O : public BytecodeCmpVarInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeCmpConstantVarInfo_O, "BytecodeCmpConstantVarInfo", BytecodeCmpVarInfo_O);
public:
  T_sp _value;
public:
  BytecodeCmpConstantVarInfo_O(T_sp nvalue)
    : BytecodeCmpVarInfo_O(), _value(nvalue) {};
  CL_LISPIFY_NAME(BytecodeCmpConstantVarInfo/make)
  CL_DEF_CLASS_METHOD
  static BytecodeCmpConstantVarInfo_sp make(T_sp value) {
    return gctools::GC<BytecodeCmpConstantVarInfo_O>::allocate<gctools::RuntimeStage>(value);
  }
  CL_DEFMETHOD T_sp value() const { return this->_value; }
};

FORWARD(BytecodeCmpFunInfo);
class BytecodeCmpFunInfo_O : public General_O {
  LISP_ABSTRACT_CLASS(core, CorePkg, BytecodeCmpFunInfo_O, "BytecodeCmpFunInfo", General_O);
public:
  BytecodeCmpFunInfo_O() {};
};

FORWARD(BytecodeCmpGlobalFunInfo);
class BytecodeCmpGlobalFunInfo_O : public BytecodeCmpFunInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeCmpGlobalFunInfo_O, "BytecodeCmpGlobalFunInfo", BytecodeCmpFunInfo_O);
public:
  BytecodeCmpGlobalFunInfo_O() : BytecodeCmpFunInfo_O() {};
  CL_LISPIFY_NAME(BytecodeCmpGlobalFunInfo/make)
  CL_DEF_CLASS_METHOD
  static BytecodeCmpGlobalFunInfo_sp make() {
    BytecodeCmpGlobalFunInfo_sp info = gctools::GC<BytecodeCmpGlobalFunInfo_O>::allocate<gctools::RuntimeStage>();
    return info;
  }
};

FORWARD(BytecodeCmpLocalFunInfo);
class BytecodeCmpLocalFunInfo_O : public BytecodeCmpFunInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeCmpLocalFunInfo_O, "BytecodeCmpLocalFunInfo", BytecodeCmpFunInfo_O);
public:
  T_sp fun_var;
public:
  BytecodeCmpLocalFunInfo_O(T_sp nfun_var)
    : BytecodeCmpFunInfo_O(), fun_var(nfun_var) {};
  CL_LISPIFY_NAME(BytecodeCmpLocalFunInfo/make)
  CL_DEF_CLASS_METHOD
  static BytecodeCmpLocalFunInfo_sp make(T_sp value) {
    return gctools::GC<BytecodeCmpLocalFunInfo_O>::allocate<gctools::RuntimeStage>(value);
  }
  CL_DEFMETHOD T_sp funVar() const { return this->fun_var; }
};

// We have separate global and local macro classes because it is sometimes
// important to know that a binding is local - for example, a local binding
// shadows a global setf expander.
FORWARD(BytecodeCmpGlobalMacroInfo);
class BytecodeCmpGlobalMacroInfo_O : public BytecodeCmpFunInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeCmpGlobalMacroInfo_O, "BytecodeCmpGlobalMacroInfo", BytecodeCmpFunInfo_O);
public:
  Function_sp _expander;
public:
  BytecodeCmpGlobalMacroInfo_O(Function_sp nexpander)
    : BytecodeCmpFunInfo_O(), _expander(nexpander) {};
  CL_LISPIFY_NAME(BytecodeCmpGlobalMacroInfo/make)
  CL_DEF_CLASS_METHOD
  static BytecodeCmpGlobalMacroInfo_sp make(Function_sp expander) {
    return gctools::GC<BytecodeCmpGlobalMacroInfo_O>::allocate<gctools::RuntimeStage>(expander);
  }
  CL_DEFMETHOD Function_sp expander() const { return this->_expander; }
};

FORWARD(BytecodeCmpLocalMacroInfo);
class BytecodeCmpLocalMacroInfo_O : public BytecodeCmpFunInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeCmpLocalMacroInfo_O, "BytecodeCmpLocalMacroInfo", BytecodeCmpFunInfo_O);
public:
  Function_sp _expander;
public:
  BytecodeCmpLocalMacroInfo_O(Function_sp nexpander)
    : BytecodeCmpFunInfo_O(), _expander(nexpander) {};
  CL_LISPIFY_NAME(BytecodeCmpLocalMacroInfo/make)
  CL_DEF_CLASS_METHOD
  static BytecodeCmpLocalMacroInfo_sp make(Function_sp expander) {
    return gctools::GC<BytecodeCmpLocalMacroInfo_O>::allocate<gctools::RuntimeStage>(expander);
  }
  CL_DEFMETHOD Function_sp expander() const { return this->_expander; }
};

FORWARD(BytecodeCmpEnv);
class BytecodeCmpEnv_O : public General_O {
  LISP_CLASS(core, CorePkg, BytecodeCmpEnv_O, "BytecodeCmpEnv", General_O);
public:
  T_sp _vars;
  T_sp _tags;
  T_sp _blocks;
  T_sp _funs;
  Integer_sp frame_end;
public:
  BytecodeCmpEnv_O(T_sp nvars, T_sp ntags, T_sp nblocks, T_sp nfuns,
                   Integer_sp nframe_end)
    : _vars(nvars), _tags(ntags), _blocks(nblocks), _funs(nfuns),
      frame_end(nframe_end) {};
  CL_LISPIFY_NAME(BytecodeCmpEnv/make)
  CL_DEF_CLASS_METHOD
  static BytecodeCmpEnv_sp make(T_sp vars, T_sp tags, T_sp blocks, T_sp funs,
                                Integer_sp frame_end) {
    return gctools::GC<BytecodeCmpEnv_O>::allocate<gctools::RuntimeStage>(vars, tags, blocks, funs, frame_end);
  }
  CL_DEFMETHOD T_sp vars() const { return this->_vars; }
  CL_DEFMETHOD T_sp tags() const { return this->_tags; }
  CL_DEFMETHOD T_sp blocks() const { return this->_blocks; }
  CL_DEFMETHOD T_sp funs() const { return this->_funs; }
  CL_DEFMETHOD Integer_sp frameEnd() const { return this->frame_end; }
public:
  T_sp variableInfo(T_sp varname);
  T_sp lookupSymbolMacro(T_sp sname);
  T_sp functionInfo(T_sp fname);
  T_sp lookupMacro(T_sp mname);
  /*
  T_sp blockInfo(T_sp bname);
  T_sp tagInfo(T_sp tname);
*/
};

}; // namespace core

#endif /* guard */
