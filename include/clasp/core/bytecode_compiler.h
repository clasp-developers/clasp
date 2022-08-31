#ifndef bytecode_compiler_H
#define bytecode_compiler_H

#include <clasp/core/common.h>
#include <clasp/core/compPackage.fwd.h>

namespace comp {

  using namespace core;

// Structures for the bytecode compiler.
class VarInfo_O : public General_O {
  LISP_ABSTRACT_CLASS(comp, CompPkg, VarInfo_O, "VarInfo", General_O);
public:
  VarInfo_O() {};
};

FORWARD(LexicalVarInfo);
class LexicalVarInfo_O : public VarInfo_O {
  LISP_CLASS(comp, CompPkg, LexicalVarInfo_O, "LexicalVarInfo", VarInfo_O);
public:
  size_t frame_index;
  T_sp _funct;
  bool closed_over_p = false;
  bool set_p = false;
public:
  LexicalVarInfo_O(size_t ind, T_sp nfunct)
    : VarInfo_O(), frame_index(ind), _funct(nfunct) {};
  CL_LISPIFY_NAME(LexicalVarInfo/make)
  CL_DEF_CLASS_METHOD
  static LexicalVarInfo_sp make(size_t frame_index, T_sp funct) {
    return gctools::GC<LexicalVarInfo_O>::allocate<gctools::RuntimeStage>(frame_index, funct);
  }
  CL_DEFMETHOD size_t frameIndex() const { return this->frame_index; }
  CL_LISPIFY_NAME(LexicalVarInfo/cfunction)
  CL_DEFMETHOD T_sp funct() const { return this->_funct; }
  CL_DEFMETHOD bool closedOverP() const { return this->closed_over_p; }
  CL_LISPIFY_NAME(LexicalVarInfo/setf-closed-over-p)
  CL_DEFMETHOD bool setClosedOverP(bool n) {
    this->closed_over_p = n;
    return n;
  }
  CL_DEFMETHOD bool setP() const { return this->set_p; }
  CL_LISPIFY_NAME(LexicalVarInfo/setf-set-p)
  CL_DEFMETHOD bool setSetP(bool n) {
    this->set_p = n;
    return n;
  }
};

FORWARD(SpecialVarInfo);
class SpecialVarInfo_O : public VarInfo_O {
  LISP_CLASS(comp, CompPkg, SpecialVarInfo_O, "SpecialVarInfo", VarInfo_O);
public:
  SpecialVarInfo_O() : VarInfo_O() {};
  CL_LISPIFY_NAME(SpecialVarInfo/make)
  CL_DEF_CLASS_METHOD
  static SpecialVarInfo_sp make() {
    SpecialVarInfo_sp info = gctools::GC<SpecialVarInfo_O>::allocate<gctools::RuntimeStage>();
    return info;
  }
};

FORWARD(SymbolMacroVarInfo);
class SymbolMacroVarInfo_O : public VarInfo_O {
  LISP_CLASS(comp, CompPkg, SymbolMacroVarInfo_O, "SymbolMacroVarInfo", VarInfo_O);
public:
  Function_sp _expander;
public:
 SymbolMacroVarInfo_O(Function_sp n_expander)
    : VarInfo_O(), _expander(n_expander) {};
  CL_LISPIFY_NAME(SymbolMacroVarInfo/make)
    CL_DEF_CLASS_METHOD
    static SymbolMacroVarInfo_sp make(Function_sp expander) {
    SymbolMacroVarInfo_sp info = gctools::GC<SymbolMacroVarInfo_O>::allocate<gctools::RuntimeStage>(expander);
    return info;
  }
  CL_DEFMETHOD Function_sp expander() const { return this->_expander; }
};

FORWARD(ConstantVarInfo);
class ConstantVarInfo_O : public VarInfo_O {
  LISP_CLASS(comp, CompPkg, ConstantVarInfo_O, "ConstantVarInfo", VarInfo_O);
public:
  T_sp _value;
public:
  ConstantVarInfo_O(T_sp nvalue)
    : VarInfo_O(), _value(nvalue) {};
  CL_LISPIFY_NAME(ConstantVarInfo/make)
  CL_DEF_CLASS_METHOD
  static ConstantVarInfo_sp make(T_sp value) {
    return gctools::GC<ConstantVarInfo_O>::allocate<gctools::RuntimeStage>(value);
  }
  CL_DEFMETHOD T_sp value() const { return this->_value; }
};

FORWARD(FunInfo);
class FunInfo_O : public General_O {
  LISP_ABSTRACT_CLASS(comp, CompPkg, FunInfo_O, "FunInfo", General_O);
public:
  FunInfo_O() {};
};

FORWARD(GlobalFunInfo);
class GlobalFunInfo_O : public FunInfo_O {
  LISP_CLASS(comp, CompPkg, GlobalFunInfo_O, "GlobalFunInfo", FunInfo_O);
public:
  GlobalFunInfo_O() : FunInfo_O() {};
  CL_LISPIFY_NAME(GlobalFunInfo/make)
  CL_DEF_CLASS_METHOD
  static GlobalFunInfo_sp make() {
    GlobalFunInfo_sp info = gctools::GC<GlobalFunInfo_O>::allocate<gctools::RuntimeStage>();
    return info;
  }
};

FORWARD(LocalFunInfo);
class LocalFunInfo_O : public FunInfo_O {
  LISP_CLASS(comp, CompPkg, LocalFunInfo_O, "LocalFunInfo", FunInfo_O);
public:
  T_sp fun_var;
public:
  LocalFunInfo_O(T_sp nfun_var)
    : FunInfo_O(), fun_var(nfun_var) {};
  CL_LISPIFY_NAME(LocalFunInfo/make)
  CL_DEF_CLASS_METHOD
  static LocalFunInfo_sp make(T_sp value) {
    return gctools::GC<LocalFunInfo_O>::allocate<gctools::RuntimeStage>(value);
  }
  CL_DEFMETHOD T_sp funVar() const { return this->fun_var; }
};

// We have separate global and local macro classes because it is sometimes
// important to know that a binding is local - for example, a local binding
// shadows a global setf expander.
FORWARD(GlobalMacroInfo);
class GlobalMacroInfo_O : public FunInfo_O {
  LISP_CLASS(comp, CompPkg, GlobalMacroInfo_O, "GlobalMacroInfo", FunInfo_O);
public:
  Function_sp _expander;
public:
 GlobalMacroInfo_O(Function_sp nexpander)
   : FunInfo_O(), _expander(nexpander) {};
  CL_LISPIFY_NAME(GlobalMacroInfo/make)
    CL_DEF_CLASS_METHOD
    static GlobalMacroInfo_sp make(Function_sp expander) {
    return gctools::GC<GlobalMacroInfo_O>::allocate<gctools::RuntimeStage>(expander);
  }
  CL_DEFMETHOD Function_sp expander() const { return this->_expander; }
};

FORWARD(LocalMacroInfo);
class LocalMacroInfo_O : public FunInfo_O {
  LISP_CLASS(comp, CompPkg, LocalMacroInfo_O, "LocalMacroInfo", FunInfo_O);
public:
  Function_sp _expander;
public:
 LocalMacroInfo_O(Function_sp nexpander)
   : FunInfo_O(), _expander(nexpander) {};
  CL_LISPIFY_NAME(LocalMacroInfo/make)
    CL_DEF_CLASS_METHOD
    static LocalMacroInfo_sp make(Function_sp expander) {
    return gctools::GC<LocalMacroInfo_O>::allocate<gctools::RuntimeStage>(expander);
  }
  CL_DEFMETHOD Function_sp expander() const { return this->_expander; }
};

FORWARD(Lexenv);
class Lexenv_O : public General_O {
  LISP_CLASS(comp, CompPkg, Lexenv_O, "Lexenv", General_O);
public:
  T_sp _vars;
  T_sp _tags;
  T_sp _blocks;
  T_sp _funs;
  Integer_sp frame_end;
public:
  Lexenv_O(T_sp nvars, T_sp ntags, T_sp nblocks, T_sp nfuns,
                   Integer_sp nframe_end)
    : _vars(nvars), _tags(ntags), _blocks(nblocks), _funs(nfuns),
      frame_end(nframe_end) {};
  CL_LISPIFY_NAME(lexenv/make)
  CL_DEF_CLASS_METHOD
  static Lexenv_sp make(T_sp vars, T_sp tags, T_sp blocks, T_sp funs,
                                Integer_sp frame_end) {
    return gctools::GC<Lexenv_O>::allocate<gctools::RuntimeStage>(vars, tags, blocks, funs, frame_end);
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
 FORWARD(Context);
 class Context_O : public General_O {
   LISP_CLASS(comp, CompPkg, Context_O, "Context", General_O);
 public:
   T_sp _receiving;
   T_sp _cfunction;
 public:
   Context_O(T_sp receiving, T_sp cfunction)
     : _receiving(receiving), _cfunction(cfunction) {}
   CL_LISPIFY_NAME(Context/make)
     CL_DEF_CLASS_METHOD
     static Context_sp make(T_sp receiving, T_sp cfunction) {
     return gctools::GC<Context_O>::allocate<gctools::RuntimeStage>(receiving, cfunction);
   }
   CL_DEFMETHOD T_sp receiving() { return this->_receiving; }
   CL_DEFMETHOD T_sp cfunction() { return this->_cfunction; }
 };

 FORWARD(Annotation);
 class Annotation_O : public General_O {
   LISP_ABSTRACT_CLASS(comp, CompPkg, Annotation_O, "Annotation", General_O);
 public:
   // The cfunction containing this annotation.
   T_sp _cfunction;
   // The index of this annotation in its cfunction's annotations.
   size_t _index;
   // The current (optimistic) position of this annotation in this cfunction.
   size_t _position;
   // The initial position of this annotation in this cfunction.
   size_t _initial_position;
 public:
   Annotation_O() : _cfunction(unbound<T_O>()) {}
   CL_DEFMETHOD T_sp cfunction() { return this->_cfunction; }
   CL_LISPIFY_NAME(Annotation/setf-cfunction)
     CL_DEFMETHOD T_sp setCfunction(T_sp nfun) {
     this->_cfunction = nfun;
     return nfun;
   }
   // Calling these "index" or "position" directly
   // seems to cause inscrutable issues in exposeClasses that I
   // do not want to deal with.
   CL_LISPIFY_NAME(Annotation/index)
   CL_DEFMETHOD size_t iindex() { return this->_index; }
   CL_LISPIFY_NAME(Annotation/setf-index)
     CL_DEFMETHOD size_t setIndex(size_t nind) {
     this->_index = nind;
     return nind;
   }
   CL_LISPIFY_NAME(Annotation/position)
   CL_DEFMETHOD size_t pposition() { return this->_position; }
   CL_LISPIFY_NAME(Annotation/setf-position)
     CL_DEFMETHOD size_t setPosition(size_t npos) {
     this->_position = npos;
     return npos;
   }
   CL_DEFMETHOD size_t initial_position() { return this->_initial_position; }
   CL_LISPIFY_NAME(Annotation/setf-initial-position)
     CL_DEFMETHOD size_t setInitialPosition(size_t npos) {
     this->_initial_position = npos;
     return npos;
   }
 };

 FORWARD(Label);
 class Label_O : public Annotation_O {
   LISP_CLASS(comp, CompPkg, Label_O, "Label", Annotation_O);
 public:
 Label_O() : Annotation_O() {}
   CL_LISPIFY_NAME(Label/make)
     CL_DEF_CLASS_METHOD
     static Label_sp make() {
     return gctools::GC<Label_O>::allocate<gctools::RuntimeStage>();
   }
 };

 FORWARD(Fixup);
 class Fixup_O : public Annotation_O {
   LISP_CLASS(comp, CompPkg, Fixup_O, "Fixup", Annotation_O);
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
   Fixup_O(T_sp label, size_t initial_size,
                      T_sp emitter, T_sp resizer)
     : Annotation_O(), _label(label), _size(initial_size),
     _initial_size(initial_size), _emitter(emitter), _resizer(resizer) {}
   CL_LISPIFY_NAME(Fixup/make)
     CL_DEF_CLASS_METHOD
     static Fixup_sp make(T_sp label, size_t initial_size,
                                     T_sp emitter, T_sp resizer) {
     return gctools::GC<Fixup_O>::allocate<gctools::RuntimeStage>(label, initial_size, emitter, resizer);
   }
   CL_DEFMETHOD T_sp label() { return this->_label; }
   CL_DEFMETHOD size_t size() { return this->_size; }
   CL_LISPIFY_NAME(Fixup/setf-size)
     CL_DEFMETHOD size_t setSize(size_t nsize) {
     this->_size = nsize;
     return nsize;
   }
   CL_DEFMETHOD size_t initial_size() { return this->_initial_size; }
   CL_DEFMETHOD T_sp emitter() { return this->_emitter; }
   CL_DEFMETHOD T_sp resizer() { return this->_resizer; }
 };

FORWARD(Module);
 class Module_O : public General_O {
   LISP_CLASS(comp, CompPkg, Module_O, "Module", General_O);
 public:
   ComplexVector_T_sp _cfunctions;
   ComplexVector_T_sp _literals;
 public:
   Module_O()
     : _cfunctions(ComplexVector_T_O::make(1, nil<T_O>(), clasp_make_fixnum(0))),
     _literals(ComplexVector_T_O::make(0, nil<T_O>(), clasp_make_fixnum(0))) {}
   CL_LISPIFY_NAME(Module/make)
     CL_DEF_CLASS_METHOD
     static Module_sp make() {
     return gctools::GC<Module_O>::allocate<gctools::RuntimeStage>();
   }
   CL_DEFMETHOD ComplexVector_T_sp cfunctions() { return this->_cfunctions; }
   CL_LISPIFY_NAME(module/literals) // avoid defining cmp::literals
     CL_DEFMETHOD ComplexVector_T_sp literals() { return this->_literals; }
 };

 FORWARD(Cfunction);
 class Cfunction_O : public General_O {
   LISP_CLASS(comp, CompPkg, Cfunction_O, "Cfunction", General_O);
 public:
   Module_sp _module;
   // Bytecode vector for this cfunction.
   ComplexVector_byte8_t_sp _bytecode;
   // An ordered vector of annotations emitted in this cfunction.
   ComplexVector_T_sp _annotations;
   size_t _nlocals;
   ComplexVector_T_sp _closed;
   Label_sp _entry_point;
   // The position of the start of this cfunction in this module (optimistic).
   size_t _position;
   // How much to add to the bytecode vector length for increased fixup
   // sizes for the true length.
   size_t _extra;
   // The index of this cfunction in the containing module's cfunction vector.
   size_t _index;
   // The runtime function, used during link.
   GlobalBytecodeEntryPoint_sp _info;
   // Stuff for the function description.
   T_sp _name;
   T_sp _doc;
 public:
 Cfunction_O(Module_sp module, T_sp name, T_sp doc)
   : _module(module),
     // A zero-length adjustable vector with fill pointer.
     _bytecode(ComplexVector_byte8_t_O::make_vector(0, 0,
                                                    clasp_make_fixnum(0),
                                                    nil<T_O>(), false,
                                                    clasp_make_fixnum(0))),
     _annotations(ComplexVector_T_O::make(0, nil<T_O>(), clasp_make_fixnum(0))),
     _closed(ComplexVector_T_O::make(0, nil<T_O>(), clasp_make_fixnum(0))),
     _entry_point(Label_O::make()),
     // not sure this has to be initialized, but just in case
     _info(unbound<GlobalBytecodeEntryPoint_O>()),
     _name(name), _doc(doc)
   {}
   CL_LISPIFY_NAME(Cfunction/make)
     CL_DEF_CLASS_METHOD
     static Cfunction_sp make(Module_sp module, T_sp name, T_sp doc) {
     return gctools::GC<Cfunction_O>::allocate<gctools::RuntimeStage>(module, name, doc);
   }
   CL_DEFMETHOD Module_sp module() { return _module; }
   CL_LISPIFY_NAME(cfunction/bytecode)
     CL_DEFMETHOD ComplexVector_byte8_t_sp bytecode() { return _bytecode; }
   CL_DEFMETHOD ComplexVector_T_sp annotations() { return _annotations; }
   CL_DEFMETHOD size_t nlocals() { return _nlocals; }
   CL_LISPIFY_NAME(Cfunction/setf-nlocals)
     CL_DEFMETHOD size_t setNlocals(size_t new_nlocals) {
     this->_nlocals = new_nlocals;
     return new_nlocals;
   }
   CL_DEFMETHOD ComplexVector_T_sp closed() { return _closed; }
   CL_LISPIFY_NAME(Cfunction/entry-point)
     CL_DEFMETHOD T_sp entry_point() { return _entry_point; }
   CL_LISPIFY_NAME(Cfunction/position)
     CL_DEFMETHOD size_t pposition() { return _position; }
   CL_LISPIFY_NAME(Cfunction/setf-position)
   CL_DEFMETHOD size_t setPosition(size_t npos) {
     this->_position = npos;
     return npos;
   }
   CL_DEFMETHOD size_t extra() { return _extra; }
   CL_LISPIFY_NAME(Cfunction/setf-extra)
     CL_LAMBDA(cfunction new) // avoid name conflict with next
     CL_DEFMETHOD size_t setExtra(size_t next) {
     this->_extra = next;
     return next;
   }
   CL_LISPIFY_NAME(Cfunction/index)
   CL_DEFMETHOD size_t iindex() { return _index; }
   CL_LISPIFY_NAME(Cfunction/setf-index)
     CL_DEFMETHOD size_t setIndex(size_t nindex) {
     this->_index = nindex;
     return nindex;
   }
   CL_DEFMETHOD GlobalBytecodeEntryPoint_sp info() { return _info; }
   CL_LISPIFY_NAME(Cfunction/setf-info)
     CL_DEFMETHOD GlobalBytecodeEntryPoint_sp setInfo(GlobalBytecodeEntryPoint_sp gbep) {
     this->_info = gbep;
     return gbep;
   }
   CL_LISPIFY_NAME(Cfunction/name)
     CL_DEFMETHOD T_sp nname() { return _name; }
   CL_DEFMETHOD T_sp doc() { return _doc; }
 };

}; // namespace comp

#endif /* guard */
