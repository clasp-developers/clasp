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

// Context contains information about what the current form needs
// to know about what it is enclosed by.
 FORWARD(BytecodeCmpContext);
 class BytecodeCmpContext_O : public General_O {
   LISP_CLASS(core, CorePkg, BytecodeCmpContext_O, "BytecodeCmpContext", General_O);
 public:
   T_sp _receiving;
   T_sp _cfunction;
 public:
   BytecodeCmpContext_O(T_sp receiving, T_sp cfunction)
     : _receiving(receiving), _cfunction(cfunction) {}
   CL_LISPIFY_NAME(BytecodeCmpContext/make)
     CL_DEF_CLASS_METHOD
     static BytecodeCmpContext_sp make(T_sp receiving, T_sp cfunction) {
     return gctools::GC<BytecodeCmpContext_O>::allocate<gctools::RuntimeStage>(receiving, cfunction);
   }
   CL_DEFMETHOD T_sp receiving() { return this->_receiving; }
   CL_DEFMETHOD T_sp cfunction() { return this->_cfunction; }
 };

 FORWARD(BytecodeCmpAnnotation);
 class BytecodeCmpAnnotation_O : public General_O {
   LISP_ABSTRACT_CLASS(core, CorePkg, BytecodeCmpAnnotation_O, "BytecodeCmpAnnotation", General_O);
 public:
   // The cfunction containing this annotation.
   T_sp _function;
   // The index of this annotation in its function's annotations.
   size_t _index;
   // The current (optimistic) position of this annotation in this function.
   size_t _position;
   // The initial position of this annotation in this function.
   size_t _initial_position;
 public:
   BytecodeCmpAnnotation_O() : _function(unbound<T_O>()) {}
   CL_DEFMETHOD T_sp function() { return this->_function; }
   CL_LISPIFY_NAME(BytecodeCmpAnnotation/setf-function)
     CL_DEFMETHOD T_sp setFunction(T_sp nfun) {
     this->_function = nfun;
     return nfun;
   }
   // Calling these "index" or "position" directly
   // seems to cause inscrutable issues in exposeClasses that I
   // do not want to deal with.
   CL_LISPIFY_NAME(BytecodeCmpAnnotation/index)
   CL_DEFMETHOD size_t iindex() { return this->_index; }
   CL_LISPIFY_NAME(BytecodeCmpAnnotation/setf-index)
     CL_DEFMETHOD size_t setIndex(size_t nind) {
     this->_index = nind;
     return nind;
   }
   CL_LISPIFY_NAME(BytecodeCmpAnnotation/position)
   CL_DEFMETHOD size_t pposition() { return this->_position; }
   CL_LISPIFY_NAME(BytecodeCmpAnnotation/setf-position)
     CL_DEFMETHOD size_t setPosition(size_t npos) {
     this->_position = npos;
     return npos;
   }
   CL_DEFMETHOD size_t initial_position() { return this->_initial_position; }
   CL_LISPIFY_NAME(BytecodeCmpAnnotation/setf-initial-position)
     CL_DEFMETHOD size_t setInitialPosition(size_t npos) {
     this->_initial_position = npos;
     return npos;
   }
 };

 FORWARD(BytecodeCmpLabel);
 class BytecodeCmpLabel_O : public BytecodeCmpAnnotation_O {
   LISP_CLASS(core, CorePkg, BytecodeCmpLabel_O, "BytecodeCmpLabel", BytecodeCmpAnnotation_O);
 public:
 BytecodeCmpLabel_O() : BytecodeCmpAnnotation_O() {}
   CL_LISPIFY_NAME(BytecodeCmpLabel/make)
     CL_DEF_CLASS_METHOD
     static BytecodeCmpLabel_sp make() {
     return gctools::GC<BytecodeCmpLabel_O>::allocate<gctools::RuntimeStage>();
   }
 };

 FORWARD(BytecodeCmpFixup);
 class BytecodeCmpFixup_O : public BytecodeCmpAnnotation_O {
   LISP_CLASS(core, CorePkg, BytecodeCmpFixup_O, "BytecodeCmpFixup", BytecodeCmpAnnotation_O);
 public:
   // The label this fixup references.
   // i think the bytecode compiler puts lexical infos here sometimes?
   T_sp _label;
   // The current (optimistic) size of this fixup in bytes.
   size_t _size;
   // The initial size of this fixup in bytes.
   size_t _initial_size;
   // How to emit this fixup once sizes are resolved.
   // These two are functions, but that will have to change in the C++ port
   // probably, since we don't have closures.
   T_sp _emitter;
   // How to resize this fixup. Returns the new size.
   T_sp _resizer;
 public:
   BytecodeCmpFixup_O(T_sp label, size_t initial_size,
                      T_sp emitter, T_sp resizer)
     : BytecodeCmpAnnotation_O(), _label(label), _size(initial_size),
     _initial_size(initial_size), _emitter(emitter), _resizer(resizer) {}
   CL_LISPIFY_NAME(BytecodeCmpFixup/make)
     CL_DEF_CLASS_METHOD
     static BytecodeCmpFixup_sp make(T_sp label, size_t initial_size,
                                     T_sp emitter, T_sp resizer) {
     return gctools::GC<BytecodeCmpFixup_O>::allocate<gctools::RuntimeStage>(label, initial_size, emitter, resizer);
   }
   CL_DEFMETHOD T_sp label() { return this->_label; }
   CL_DEFMETHOD size_t size() { return this->_size; }
   CL_LISPIFY_NAME(BytecodeCmpFixup/setf-size)
     CL_DEFMETHOD size_t setSize(size_t nsize) {
     this->_size = nsize;
     return nsize;
   }
   CL_DEFMETHOD size_t initial_size() { return this->_initial_size; }
   CL_DEFMETHOD T_sp emitter() { return this->_emitter; }
   CL_DEFMETHOD T_sp resizer() { return this->_resizer; }
 };

FORWARD(BytecodeCmpModule);
 class BytecodeCmpModule_O : public General_O {
   LISP_CLASS(core, CorePkg, BytecodeCmpModule_O, "BytecodeCmpModule", General_O);
 public:
   ComplexVector_T_sp _cfunctions;
   ComplexVector_T_sp _literals;
 public:
   BytecodeCmpModule_O()
     : _cfunctions(ComplexVector_T_O::make(1, nil<T_O>(), clasp_make_fixnum(0))),
     _literals(ComplexVector_T_O::make(0, nil<T_O>(), clasp_make_fixnum(0))) {}
   CL_LISPIFY_NAME(BytecodeCmpModule/make)
     CL_DEF_CLASS_METHOD
     static BytecodeCmpModule_sp make() {
     return gctools::GC<BytecodeCmpModule_O>::allocate<gctools::RuntimeStage>();
   }
   CL_DEFMETHOD ComplexVector_T_sp cfunctions() { return this->_cfunctions; }
   CL_DEFMETHOD ComplexVector_T_sp literals() { return this->_literals; }
 };

 FORWARD(BytecodeCmpFunction);
 class BytecodeCmpFunction_O : public General_O {
   LISP_CLASS(core, CorePkg, BytecodeCmpFunction_O, "BytecodeCmpFunction", General_O);
 public:
   BytecodeCmpModule_sp _module;
   // Bytecode vector for this function.
   ComplexVector_byte8_t_sp _bytecode;
   // An ordered vector of annotations emitted in this function.
   ComplexVector_T_sp _annotations;
   size_t _nlocals;
   ComplexVector_T_sp _closed;
   BytecodeCmpLabel_sp _entry_point;
   // The position of the start of this function in this module (optimistic).
   size_t _position;
   // How much to add to the bytecode vector length for increased fixup
   // sizes for the true length.
   size_t _extra;
   // The index of this function in the containing module's function vector.
   size_t _index;
   // The runtime function, used during link.
   GlobalBytecodeEntryPoint_sp _info;
   // Stuff for the function description.
   T_sp _name;
   T_sp _doc;
 public:
 BytecodeCmpFunction_O(BytecodeCmpModule_sp module, T_sp name, T_sp doc)
   : _module(module),
     // A zero-length adjustable vector with fill pointer.
     _bytecode(ComplexVector_byte8_t_O::make_vector(0, 0,
                                                    clasp_make_fixnum(0),
                                                    nil<T_O>(), false,
                                                    clasp_make_fixnum(0))),
     _annotations(ComplexVector_T_O::make(0, nil<T_O>(), clasp_make_fixnum(0))),
     _closed(ComplexVector_T_O::make(0, nil<T_O>(), clasp_make_fixnum(0))),
     _entry_point(BytecodeCmpLabel_O::make()),
     // not sure this has to be initialized, but just in case
     _info(unbound<GlobalBytecodeEntryPoint_O>()),
     _name(name), _doc(doc)
   {}
   CL_LISPIFY_NAME(BytecodeCmpFunction/make)
     CL_DEF_CLASS_METHOD
     static BytecodeCmpFunction_sp make(BytecodeCmpModule_sp module,
                                        T_sp name, T_sp doc) {
     return gctools::GC<BytecodeCmpFunction_O>::allocate<gctools::RuntimeStage>(module, name, doc);
   }
   CL_DEFMETHOD BytecodeCmpModule_sp module() { return _module; }
   CL_DEFMETHOD ComplexVector_byte8_t_sp bytecode() { return _bytecode; }
   CL_DEFMETHOD ComplexVector_T_sp annotations() { return _annotations; }
   CL_DEFMETHOD size_t nlocals() { return _nlocals; }
   CL_LISPIFY_NAME(BytecodeCmpFunction/setf-nlocals)
     CL_DEFMETHOD size_t setNlocals(size_t new_nlocals) {
     this->_nlocals = new_nlocals;
     return new_nlocals;
   }
   CL_DEFMETHOD ComplexVector_T_sp closed() { return _closed; }
   CL_DEFMETHOD T_sp entry_point() { return _entry_point; }
   CL_LISPIFY_NAME(BytecodeCmpFunction/position)
     CL_DEFMETHOD size_t pposition() { return _position; }
   CL_LISPIFY_NAME(BytecodeCmpFunction/setf-position)
   CL_DEFMETHOD size_t setPosition(size_t npos) {
     this->_position = npos;
     return npos;
   }
   CL_DEFMETHOD size_t extra() { return _extra; }
   CL_LISPIFY_NAME(BytecodeCmpFunction/setf-extra)
   CL_DEFMETHOD size_t setExtra(size_t next) {
     this->_extra = next;
     return next;
   }
   CL_LISPIFY_NAME(BytecodeCmpFunction/index)
   CL_DEFMETHOD size_t iindex() { return _index; }
   CL_LISPIFY_NAME(BytecodeCmpFunction/setf-index)
     CL_DEFMETHOD size_t setIndex(size_t nindex) {
     this->_index = nindex;
     return nindex;
   }
   CL_DEFMETHOD GlobalBytecodeEntryPoint_sp info() { return _info; }
   CL_LISPIFY_NAME(BytecodeCmpFunction/setf-info)
     CL_DEFMETHOD GlobalBytecodeEntryPoint_sp setInfo(GlobalBytecodeEntryPoint_sp gbep) {
     this->_info = gbep;
     return gbep;
   }
   CL_LISPIFY_NAME(BytecodeCmpFunction/name)
     CL_DEFMETHOD T_sp nname() { return _name; }
   CL_DEFMETHOD T_sp doc() { return _doc; }
 };

}; // namespace core

#endif /* guard */
