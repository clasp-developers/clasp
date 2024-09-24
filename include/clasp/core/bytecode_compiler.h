#pragma once

#include <variant>
#include <clasp/core/common.h>
#include <clasp/core/compPackage.fwd.h>

#define VM_CODES
#include <virtualMachine.h>
#undef VM_CODES

namespace comp {

using namespace core;

// Structures for the bytecode compiler.

// Information for something lexical, like a variable, function, tag, whatever.
FORWARD(Cfunction);
FORWARD(LexicalInfo);
class LexicalInfo_O : public General_O {
  LISP_CLASS(comp, CompPkg, LexicalInfo_O, "LexicalInfo", General_O);

public:
  uint16_t _frame_index;
  Cfunction_sp _cfunction;
  bool _closed_over_p = false;
  // only used for lexical variables, but it's convenient for fixups and debug
  // infos to have it generally present.
  bool _set_p = false;
  // Used for IGNORE tracking, which is used for local variables and functions
  // but irrelevant for blocks and tags.
  // To make things easier on the compiler, we default this to NOIGNORE, and then
  // set it afterwards, so that you don't have to know the ignore status any time
  // you make a variable.
  enum class IgnoreStatus { NOIGNORE, IGNORE, IGNORABLE };
  IgnoreStatus _ignore = IgnoreStatus::NOIGNORE;
  bool _read_p = false;
  // Other declarations.
  List_sp _decls = nil<T_O>();

public:
  LexicalInfo_O(uint16_t ind, Cfunction_sp funct) : _frame_index(ind), _cfunction(funct){};
  CL_LISPIFY_NAME(LexicalInfo/make)
  CL_DEF_CLASS_METHOD
  static LexicalInfo_sp make(uint16_t frame_index, Cfunction_sp cfunction) {
    return gctools::GC<LexicalInfo_O>::allocate<gctools::RuntimeStage>(frame_index, cfunction);
  }
  CL_DEFMETHOD uint16_t frameIndex() const { return this->_frame_index; }
  CL_DEFMETHOD Cfunction_sp cfunction() const { return this->_cfunction; }
  CL_DEFMETHOD bool closedOverP() const { return this->_closed_over_p; }
  void setClosedOverP(bool n) { this->_closed_over_p = n; }
  CL_DEFMETHOD bool setP() const { return this->_set_p; }
  void setSetP(bool n) { this->_set_p = n; }
  // Is a cell required?
  bool indirectLexicalP() const { return closedOverP() && setP(); }
  IgnoreStatus ignore() const { return this->_ignore; }
  void setIgnore(IgnoreStatus n) { this->_ignore = n; }
  bool readP() const { return this->_read_p; }
  void setReadP(bool n) { this->_read_p = n; }
  List_sp decls() const { return this->_decls; }
  void setDecls(List_sp ndecls) { this->_decls = ndecls; }
};

class VarInfo_O : public General_O {
  LISP_ABSTRACT_CLASS(comp, CompPkg, VarInfo_O, "VarInfo", General_O);

public:
  VarInfo_O(){};
};

FORWARD(LexicalVarInfo);
class LexicalVarInfo_O : public VarInfo_O {
  LISP_CLASS(comp, CompPkg, LexicalVarInfo_O, "LexicalVarInfo", VarInfo_O);

public:
  LexicalInfo_sp _lex;

public:
  LexicalVarInfo_O(LexicalInfo_sp nlex) : VarInfo_O(), _lex(nlex){};
  CL_LISPIFY_NAME(LexicalVarInfo/make)
  CL_DEF_CLASS_METHOD
  static LexicalVarInfo_sp make(size_t frame_index, Cfunction_sp funct) {
    LexicalInfo_sp nlex = LexicalInfo_O::make(frame_index, funct);
    return gctools::GC<LexicalVarInfo_O>::allocate<gctools::RuntimeStage>(nlex);
  }
  LexicalInfo_sp lex() const { return this->_lex; }
  CL_DEFMETHOD size_t frameIndex() const { return this->lex()->frameIndex(); }
  CL_LISPIFY_NAME(LexicalVarInfo/cfunction)
  CL_DEFMETHOD Cfunction_sp funct() const { return this->lex()->cfunction(); }
  CL_DEFMETHOD bool closedOverP() const { return this->lex()->closedOverP(); }
  CL_LISPIFY_NAME(LexicalVarInfo/setf-closed-over-p)
  CL_DEFMETHOD bool setClosedOverP(bool n) {
    this->lex()->setClosedOverP(n);
    return n;
  }
  CL_DEFMETHOD bool setP() const { return this->lex()->setP(); }
  CL_LISPIFY_NAME(LexicalVarInfo/setf-set-p)
  CL_DEFMETHOD bool setSetP(bool n) {
    this->lex()->setSetP(n);
    return n;
  }
  // Does the variable need a cell?
  bool indirectLexicalP() const { return this->lex()->indirectLexicalP(); }
};

struct LexicalVarInfoV {
  LexicalVarInfoV(LexicalVarInfo_sp info) : _info(info){};
  // Because we mutate infos, and store them in lexenvs, we have to
  // allocate them. This wrapper is just for var_info_v purposes.
  LexicalVarInfo_sp _info;
  LexicalVarInfo_sp info() const { return _info; }
};

FORWARD(SpecialVarInfo);
class SpecialVarInfo_O : public VarInfo_O {
  LISP_CLASS(comp, CompPkg, SpecialVarInfo_O, "SpecialVarInfo", VarInfo_O);

public:
  // global specials are different in that they apply to inner bindings.
  // e.g., if x is globally special than (let ((x ...)) ...) is a special binding,
  // but that's not the case if x is only special outside the let because of
  // a local declaration.
  bool _globalp;

public:
  SpecialVarInfo_O(bool globalp) : VarInfo_O(), _globalp(globalp){};
  CL_LISPIFY_NAME(SpecialVarInfo/make)
  CL_DEF_CLASS_METHOD
  static SpecialVarInfo_sp make(bool globalp) {
    SpecialVarInfo_sp info = gctools::GC<SpecialVarInfo_O>::allocate<gctools::RuntimeStage>(globalp);
    return info;
  }
  CL_DEFMETHOD bool globalp() const { return this->_globalp; }
};

struct SpecialVarInfoV {
  SpecialVarInfoV(bool globalp) : _globalp(globalp){};
  SpecialVarInfoV(SpecialVarInfo_sp info) : _globalp(info->globalp()){};
  bool _globalp;
  bool globalp() const { return _globalp; }
};

FORWARD(SymbolMacroVarInfo);
class SymbolMacroVarInfo_O : public VarInfo_O {
  LISP_CLASS(comp, CompPkg, SymbolMacroVarInfo_O, "SymbolMacroVarInfo", VarInfo_O);

public:
  Function_sp _expander;

public:
  SymbolMacroVarInfo_O(Function_sp n_expander) : VarInfo_O(), _expander(n_expander){};
  CL_LISPIFY_NAME(SymbolMacroVarInfo/make)
  CL_DEF_CLASS_METHOD
  static SymbolMacroVarInfo_sp make(Function_sp expander) {
    SymbolMacroVarInfo_sp info = gctools::GC<SymbolMacroVarInfo_O>::allocate<gctools::RuntimeStage>(expander);
    return info;
  }
  CL_DEFMETHOD Function_sp expander() const { return this->_expander; }
};

struct SymbolMacroVarInfoV {
  SymbolMacroVarInfoV(Function_sp expander) : _expander(expander){};
  SymbolMacroVarInfoV(SymbolMacroVarInfo_sp info) : _expander(info->expander()){};
  Function_sp _expander;
  Function_sp expander() const { return _expander; }
};

FORWARD(ConstantVarInfo);
class ConstantVarInfo_O : public VarInfo_O {
  LISP_CLASS(comp, CompPkg, ConstantVarInfo_O, "ConstantVarInfo", VarInfo_O);

public:
  T_sp _value;

public:
  ConstantVarInfo_O(T_sp nvalue) : VarInfo_O(), _value(nvalue){};
  CL_LISPIFY_NAME(ConstantVarInfo/make)
  CL_DEF_CLASS_METHOD
  static ConstantVarInfo_sp make(T_sp value) { return gctools::GC<ConstantVarInfo_O>::allocate<gctools::RuntimeStage>(value); }
  CL_DEFMETHOD T_sp value() const { return this->_value; }
};

struct ConstantVarInfoV {
  ConstantVarInfoV(T_sp value) : _value(value){};
  ConstantVarInfoV(ConstantVarInfo_sp info) : _value(info->value()){};
  T_sp _value;
  T_sp value() const { return _value; }
};

// non-heaped infos, to avoid consing. see var_info_v.
// We use this thing instead of monostate for clarity.
struct NoVarInfoV {};

typedef std::variant<NoVarInfoV, LexicalVarInfoV, SpecialVarInfoV, SymbolMacroVarInfoV, ConstantVarInfoV> VarInfoV;

FORWARD(FunInfo);
class FunInfo_O : public General_O {
  LISP_ABSTRACT_CLASS(comp, CompPkg, FunInfo_O, "FunInfo", General_O);

public:
  FunInfo_O(){};
};

FORWARD(GlobalFunInfo);
class GlobalFunInfo_O : public FunInfo_O {
  LISP_CLASS(comp, CompPkg, GlobalFunInfo_O, "GlobalFunInfo", FunInfo_O);

public:
  T_sp _cmexpander = nil<T_O>(); // compiler macro expander
public:
  GlobalFunInfo_O() : FunInfo_O() {}
  GlobalFunInfo_O(T_sp expander) : FunInfo_O(), _cmexpander(expander) {}
  CL_LISPIFY_NAME(GlobalFunInfo/make)
  CL_DEF_CLASS_METHOD
  static GlobalFunInfo_sp make(T_sp expander) {
    GlobalFunInfo_sp info = gctools::GC<GlobalFunInfo_O>::allocate<gctools::RuntimeStage>(expander);
    return info;
  }
  CL_DEFMETHOD T_sp cmexpander() const { return this->_cmexpander; }
};

struct GlobalFunInfoV {
  GlobalFunInfoV(T_sp cmexpander) : _cmexpander(cmexpander){};
  GlobalFunInfoV(GlobalFunInfo_sp info) : _cmexpander(info->cmexpander()){};
  T_sp _cmexpander;
  T_sp cmexpander() const { return _cmexpander; }
};

FORWARD(LocalFunInfo);
class LocalFunInfo_O : public FunInfo_O {
  LISP_CLASS(comp, CompPkg, LocalFunInfo_O, "LocalFunInfo", FunInfo_O);

public:
  LexicalInfo_sp _lex;

public:
  LocalFunInfo_O(LexicalInfo_sp nlex) : FunInfo_O(), _lex(nlex){};
  CL_LISPIFY_NAME(LocalFunInfo/make)
  CL_DEF_CLASS_METHOD
  static LocalFunInfo_sp make(size_t frame_index, Cfunction_sp funct) {
    LexicalInfo_sp nlex = LexicalInfo_O::make(frame_index, funct);
    return gctools::GC<LocalFunInfo_O>::allocate<gctools::RuntimeStage>(nlex);
  }
  CL_LISPIFY_NAME(LocalFunInfo/lex)
  CL_DEFMETHOD LexicalInfo_sp lex() const { return this->_lex; }
  size_t frameIndex() const { return this->lex()->frameIndex(); }
  Cfunction_sp funct() const { return this->lex()->cfunction(); }
};

struct LocalFunInfoV {
  LocalFunInfoV(LocalFunInfo_sp info) : _info(info){};
  LocalFunInfo_sp _info;
  LocalFunInfo_sp info() const { return _info; }
};

// We have separate global and local macro classes because it is sometimes
// important to know that a binding is local - for example, a local binding
// shadows a global setf expander.
// FIXME: We could add a slot for a compiler macro function.
FORWARD(GlobalMacroInfo);
class GlobalMacroInfo_O : public FunInfo_O {
  LISP_CLASS(comp, CompPkg, GlobalMacroInfo_O, "GlobalMacroInfo", FunInfo_O);

public:
  Function_sp _expander;

public:
  GlobalMacroInfo_O(Function_sp nexpander) : FunInfo_O(), _expander(nexpander){};
  CL_LISPIFY_NAME(GlobalMacroInfo/make)
  CL_DEF_CLASS_METHOD
  static GlobalMacroInfo_sp make(Function_sp expander) {
    return gctools::GC<GlobalMacroInfo_O>::allocate<gctools::RuntimeStage>(expander);
  }
  CL_DEFMETHOD Function_sp expander() const { return this->_expander; }
};

struct GlobalMacroInfoV {
  GlobalMacroInfoV(Function_sp expander) : _expander(expander){};
  GlobalMacroInfoV(GlobalMacroInfo_sp info) : _expander(info->expander()){};
  Function_sp _expander;
  Function_sp expander() const { return _expander; }
};

FORWARD(LocalMacroInfo);
class LocalMacroInfo_O : public FunInfo_O {
  LISP_CLASS(comp, CompPkg, LocalMacroInfo_O, "LocalMacroInfo", FunInfo_O);

public:
  Function_sp _expander;

public:
  LocalMacroInfo_O(Function_sp nexpander) : FunInfo_O(), _expander(nexpander){};
  CL_LISPIFY_NAME(LocalMacroInfo/make)
  CL_DEF_CLASS_METHOD
  static LocalMacroInfo_sp make(Function_sp expander) {
    return gctools::GC<LocalMacroInfo_O>::allocate<gctools::RuntimeStage>(expander);
  }
  CL_DEFMETHOD Function_sp expander() const { return this->_expander; }
};

struct LocalMacroInfoV {
  LocalMacroInfoV(Function_sp expander) : _expander(expander){};
  LocalMacroInfoV(LocalMacroInfo_sp info) : _expander(info->expander()){};
  Function_sp _expander;
  Function_sp expander() const { return _expander; }
};

struct NoFunInfoV {};

typedef std::variant<NoFunInfoV, GlobalFunInfoV, LocalFunInfoV, GlobalMacroInfoV, LocalMacroInfoV> FunInfoV;

FORWARD(BlockInfo);
FORWARD(Label);
class BlockInfo_O : public General_O {
  LISP_CLASS(comp, CompPkg, BlockInfo_O, "BlockInfo", General_O);

public:
  LexicalInfo_sp _lex;
  Label_sp _exit;
  int _receiving;

public:
  BlockInfo_O(LexicalInfo_sp lex, Label_sp exit, int receiving) : _lex(lex), _exit(exit), _receiving(receiving) {}
  static BlockInfo_sp make(size_t frame_index, Cfunction_sp funct, Label_sp exit, int receiving) {
    LexicalInfo_sp lex = LexicalInfo_O::make(frame_index, funct);
    return gctools::GC<BlockInfo_O>::allocate<gctools::RuntimeStage>(lex, exit, receiving);
  }
  LexicalInfo_sp lex() const { return this->_lex; }
  Label_sp exit() const { return this->_exit; }
  int receiving() const { return this->_receiving; }
};

FORWARD(TagInfo);
class TagInfo_O : public General_O {
  LISP_CLASS(comp, CompPkg, TagInfo_O, "TagInfo", General_O);

public:
  LexicalInfo_sp _lex;
  Label_sp _exit;

public:
  TagInfo_O(LexicalInfo_sp lex, Label_sp exit) : _lex(lex), _exit(exit) {}
  static TagInfo_sp make(LexicalInfo_sp lex, Label_sp exit) {
    return gctools::GC<TagInfo_O>::allocate<gctools::RuntimeStage>(lex, exit);
  }
  LexicalInfo_sp lex() const { return this->_lex; }
  Label_sp exit() const { return this->_exit; }
};

FORWARD(Lexenv);
class Context; // forward decl
class Lexenv_O : public General_O {
  LISP_CLASS(comp, CompPkg, Lexenv_O, "Lexenv", General_O);

public:
  T_sp _vars;
  T_sp _tags;
  T_sp _blocks;
  T_sp _funs;
  List_sp _decls;
  size_t frame_end;

public:
  Lexenv_O(T_sp nvars, T_sp ntags, T_sp nblocks, T_sp nfuns, List_sp ndecls, size_t nframe_end)
      : _vars(nvars), _tags(ntags), _blocks(nblocks), _funs(nfuns), _decls(ndecls), frame_end(nframe_end){};
  CL_LISPIFY_NAME(lexenv/make)
  CL_DEF_CLASS_METHOD
  static Lexenv_sp make(T_sp vars, T_sp tags, T_sp blocks, T_sp funs, List_sp decls, size_t frame_end) {
    return gctools::GC<Lexenv_O>::allocate<gctools::RuntimeStage>(vars, tags, blocks, funs, decls, frame_end);
  }
  static Lexenv_sp make_top_level() {
    return make(nil<core::T_O>(), nil<core::T_O>(), nil<core::T_O>(), nil<core::T_O>(), nil<core::T_O>(), 0);
  }
  CL_DEFMETHOD List_sp vars() const { return this->_vars; }
  CL_DEFMETHOD List_sp tags() const { return this->_tags; }
  CL_DEFMETHOD List_sp blocks() const { return this->_blocks; }
  CL_DEFMETHOD List_sp funs() const { return this->_funs; }
  CL_DEFMETHOD List_sp decls() const { return this->_decls; }
  CL_DEFMETHOD size_t frameEnd() const { return this->frame_end; }

public:
  inline Lexenv_sp sub_vars(List_sp vars, size_t frame_end) const {
    return make(vars, tags(), blocks(), funs(), decls(), frame_end);
  }
  inline Lexenv_sp sub_funs(List_sp funs) const { return make(vars(), tags(), blocks(), funs, decls(), frameEnd()); }
  inline Lexenv_sp sub_funs(List_sp funs, size_t frame_end) const {
    return make(vars(), tags(), blocks(), funs, decls(), frame_end);
  }
  inline Lexenv_sp sub_tags(List_sp tags) const { return make(vars(), tags, blocks(), funs(), decls(), frameEnd() + 1); }
  inline Lexenv_sp sub_block(List_sp blocks) const { return make(vars(), tags(), blocks, funs(), decls(), frameEnd() + 1); }
  inline Lexenv_sp sub_decls(List_sp decls) const { return make(vars(), tags(), blocks(), funs(), decls, frameEnd()); }

public:
  /* Bind each variable to a stack location, returning a new lexical
   * environment. The max local count in the current function is also
   * updated.
   */
  Lexenv_sp bind_vars(List_sp vars, const Context context);
  // Ditto but with just one.
  Lexenv_sp bind1var(Symbol_sp var, const Context context);
  // Like bind_vars but for function bindings.
  Lexenv_sp bind_funs(List_sp funs, const Context context);
  // And blocks and tags.
  Lexenv_sp bind_block(T_sp name, Label_sp exit, const Context context);
  Lexenv_sp bind_tags(List_sp tags, LexicalInfo_sp dynenv, const Context context);
  // Add VARS as special in ENV.
  CL_DEFMETHOD Lexenv_sp add_specials(List_sp vars);
  // Add new declarations.
  CL_DEFMETHOD Lexenv_sp add_decls(List_sp decls);
  /* Macrolet expanders need to be compiled in the local compilation environment,
   * so that e.g. their bodies can use macros defined in outer macrolets.
   * At the same time, they obviously do not have access to any runtime
   * environment. Taking out all runtime information is one way to do this but
   * it's slightly not-nice in that if someone writes a macroexpander that does
   * try to use local runtime information may fail silently by using global info
   * instead. So: KLUDGE.
   */
  CL_DEFMETHOD Lexenv_sp macroexpansion_environment();
  T_sp variableInfo(T_sp varname);
  T_sp lookupSymbolMacro(T_sp sname);
  T_sp functionInfo(T_sp fname);
  T_sp lookupMacro(Symbol_sp mname);
  bool notinlinep(T_sp fname);
  T_sp blockInfo(T_sp bname);
  T_sp tagInfo(T_sp tname);
};

// Context contains information about what the current form needs
// to know about what it is enclosed by.
// Because they are only ever passed "down" and used within the
// compiler, they are POD rather than Lisp objects, to save some time
// on consing and GC.
// (Profiling has shown that the compiler spends a lot of time consing.)
FORWARD(Label);
FORWARD(Module);
class Context {
public:
  int _receiving;
  List_sp _dynenv;
  Cfunction_sp _cfunction;
  T_sp _source_info;

public:
  Context(int receiving, T_sp dynenv, Cfunction_sp cfunction, T_sp source_info)
      : _receiving(receiving), _dynenv(dynenv), _cfunction(cfunction), _source_info(source_info) {}
  // Sub constructors
  inline Context sub_receiving(int receiving) const { return Context(receiving, dynenv(), cfunction(), source_info()); }
  inline Context sub_de(T_sp de) const {
    // Plop on the front.
    return Context(receiving(), Cons_O::create(de, dynenv()), cfunction(), source_info());
  }
  inline Context sub_source(T_sp source_info) const { return Context(receiving(), dynenv(), cfunction(), source_info); }
  // Access
  int receiving() const { return this->_receiving; }
  List_sp dynenv() const { return this->_dynenv; }
  Cfunction_sp cfunction() const { return this->_cfunction; }
  T_sp source_info() const { return this->_source_info; }
  Module_sp module() const;

public:
  size_t literal_index(T_sp literal) const;
  size_t new_literal_index(T_sp literal) const;
  size_t ltv_index(T_sp form, bool read_only_p) const;
  size_t cfunction_index(Cfunction_sp fun) const;
  size_t fcell_index(T_sp name) const;
  size_t vcell_index(Symbol_sp name) const;
  size_t env_index() const;
  size_t closure_index(T_sp info) const;
  void push_debug_info(T_sp info) const;
  void assemble0(vm_code opcode) const;
  void assemble1(vm_code opcode, size_t operand1) const;
  void assemble2(vm_code opcode, size_t operand1, size_t operand2) const;
  void emit_control_label(Label_sp, vm_code opcode8, vm_code opcode16, vm_code opcode24) const;
  void emit_jump(Label_sp label) const;
  void emit_jump_if(Label_sp label) const;
  void emit_entry_or_save_sp(LexicalInfo_sp info) const;
  void emit_ref_or_restore_sp(LexicalInfo_sp info) const;
  void emit_exit(Label_sp label) const;
  void emit_exit_or_jump(LexicalInfo_sp info, Label_sp label) const;
  void maybe_emit_entry_close(LexicalInfo_sp info) const;
  void emit_catch(Label_sp label) const;
  void emit_jump_if_supplied(Label_sp label, size_t indx) const;
  void reference_lexical_info(LexicalInfo_sp info) const;
  void maybe_emit_make_cell(LexicalVarInfo_sp info) const;
  void maybe_emit_cell_ref(LexicalVarInfo_sp info) const;
  void maybe_emit_encage(LexicalVarInfo_sp info) const;
  void emit_lexical_set(LexicalVarInfo_sp info) const;
  void emit_parse_key_args(size_t max_count, size_t key_count, size_t key_start, size_t indx, bool aokp) const;
  void emit_bind(size_t count, size_t offset) const;
  void emit_call(size_t argcount) const;
  void emit_mv_call() const;
  void emit_special_bind(Symbol_sp sym) const;
  void emit_unbind(size_t count) const;
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

public:
  // Optimistic positioning of this annotation in its module.
  CL_DEFMETHOD size_t module_position();
};

class Label_O : public Annotation_O {
  LISP_CLASS(comp, CompPkg, Label_O, "Label", Annotation_O);

public:
  Label_O() : Annotation_O() {}
  CL_LISPIFY_NAME(Label/make)
  CL_DEF_CLASS_METHOD
  static Label_sp make() { return gctools::GC<Label_O>::allocate<gctools::RuntimeStage>(); }

public:
  void contextualize(const Context context);
};

FORWARD(Fixup);
class Fixup_O : public Annotation_O {
  LISP_ABSTRACT_CLASS(comp, CompPkg, Fixup_O, "Fixup", Annotation_O);

public:
  // The current (optimistic) size of this fixup in bytes.
  size_t _size;
  // The initial size of this fixup in bytes.
  size_t _initial_size;

public:
  Fixup_O() : Annotation_O() {} // default constructor for clasp
  Fixup_O(size_t initial_size) : Annotation_O(), _size(initial_size), _initial_size(initial_size) {}
  CL_DEFMETHOD size_t size() { return this->_size; }
  CL_LISPIFY_NAME(Fixup/setf-size)
  CL_DEFMETHOD size_t setSize(size_t nsize) {
    this->_size = nsize;
    return nsize;
  }
  CL_DEFMETHOD size_t initial_size() { return this->_initial_size; }

public:
  // Mark the fixup in the instruction stream during assembly.
  // FIXME: This name sucks, but emit_fixup seems wayy too confusing.
  void contextualize(const Context context);
  // Compute the final size (in bytes) for the fixed up code.
  CL_DEFMETHOD virtual size_t resize() = 0;
  // Emit the final code into the bytecode vector.
  CL_DEFMETHOD virtual void emit(size_t position, SimpleVector_byte8_t_sp code) = 0;
  /* Update the positions of all affected functions and annotations
   * from the effect of increasing the size of FIXUP by INCREASE. The
   * resizer has already updated the size of the the fixup.
   */
  void update_positions(size_t increase);
};

// A fixup for a jump, i.e. that is relative to a label.
FORWARD(LabelFixup);
class LabelFixup_O : public Fixup_O {
  LISP_ABSTRACT_CLASS(comp, CompPkg, LabelFixup_O, "LabelFixup", Fixup_O);

public:
  Label_sp _label;

public:
  LabelFixup_O() : Fixup_O() {}
  LabelFixup_O(Label_sp label, size_t initial_size) : Fixup_O(initial_size), _label(label) {}

public:
  CL_DEFMETHOD Label_sp label() { return this->_label; }
  // The (module) displacement from this fixup to its label.
  ptrdiff_t delta();
};

FORWARD(ControlLabelFixup);
class ControlLabelFixup_O : public LabelFixup_O {
  LISP_CLASS(comp, CompPkg, ControlLabelFixup_O, "ControlLabelFixup", LabelFixup_O);

public:
  vm_code _opcode8;
  vm_code _opcode16;
  vm_code _opcode24;

public:
  ControlLabelFixup_O(Label_sp label, vm_code opcode8, vm_code opcode16, vm_code opcode24)
    : LabelFixup_O(label, 2), _opcode8(opcode8), _opcode16(opcode16), _opcode24(opcode24) {}
  CL_LISPIFY_NAME(ControlLabelFixup/make)
  CL_DEF_CLASS_METHOD
  static ControlLabelFixup_sp make(Label_sp label, vm_code opcode8, vm_code opcode16, vm_code opcode24) {
    return gctools::GC<ControlLabelFixup_O>::allocate<gctools::RuntimeStage>(label, opcode8, opcode16, opcode24);
  }

public:
  virtual void emit(size_t position, SimpleVector_byte8_t_sp code);
  virtual size_t resize();
};

FORWARD(JumpIfSuppliedFixup);
class JumpIfSuppliedFixup_O : public LabelFixup_O {
  LISP_CLASS(comp, CompPkg, JumpIfSuppliedFixup_O, "JumpIfSuppliedFixup", LabelFixup_O);

public:
  uint16_t _index;

public:
  JumpIfSuppliedFixup_O(Label_sp label, uint16_t index) : LabelFixup_O(label, 3), _index(index) {}
  CL_LISPIFY_NAME(JumpIfSuppliedFixup/make)
  CL_DEF_CLASS_METHOD
  static JumpIfSuppliedFixup_sp make(Label_sp label, uint16_t nindex) {
    return gctools::GC<JumpIfSuppliedFixup_O>::allocate<gctools::RuntimeStage>(label, nindex);
  }

public:
  uint16_t iindex() { return this->_index; }
  virtual void emit(size_t position, SimpleVector_byte8_t_sp code);
  virtual size_t resize();
};

// A fixup defined in relation to some lexical variable.
FORWARD(LexFixup);
class LexFixup_O : public Fixup_O {
  LISP_ABSTRACT_CLASS(comp, CompPkg, LexFixup_O, "LexFixup", Fixup_O);

public:
  LexicalInfo_sp _lex;

public:
  LexFixup_O() : Fixup_O() {}
  LexFixup_O(LexicalInfo_sp lex, size_t initial_size) : Fixup_O(initial_size), _lex(lex) {}

public:
  LexicalInfo_sp lex() { return _lex; }
};

// Used to add make-cell or cell-ref where needed.
FORWARD(LexRefFixup);
class LexRefFixup_O : public LexFixup_O {
  LISP_CLASS(comp, CompPkg, LexRefFixup_O, "LexRefFixup", LexFixup_O);

public:
  vm_code _opcode;

public:
  LexRefFixup_O() : LexFixup_O() {}
  LexRefFixup_O(LexicalInfo_sp lex, vm_code opcode) : LexFixup_O(lex, 0), _opcode(opcode) {}
  CL_LISPIFY_NAME(LexRefFixup/make)
  CL_DEF_CLASS_METHOD
  static LexRefFixup_sp make(LexicalInfo_sp lex, vm_code opcode) {
    return gctools::GC<LexRefFixup_O>::allocate<gctools::RuntimeStage>(lex, opcode);
  }

public:
  vm_code opcode() { return this->_opcode; }
  virtual void emit(size_t position, SimpleVector_byte8_t_sp code);
  virtual size_t resize();
};

FORWARD(EncageFixup);
class EncageFixup_O : public LexFixup_O {
  LISP_CLASS(comp, CompPkg, EncageFixup_O, "EncageFixup", LexFixup_O);

public:
  EncageFixup_O() : LexFixup_O() {}
  EncageFixup_O(LexicalInfo_sp lex) : LexFixup_O(lex, 0) {}
  CL_LISPIFY_NAME(EncageFixup/make)
  CL_DEF_CLASS_METHOD
  static EncageFixup_sp make(LexicalInfo_sp lex) { return gctools::GC<EncageFixup_O>::allocate<gctools::RuntimeStage>(lex); }

public:
  virtual void emit(size_t position, SimpleVector_byte8_t_sp code);
  virtual size_t resize();
};

FORWARD(LexSetFixup);
class LexSetFixup_O : public LexFixup_O {
  LISP_CLASS(comp, CompPkg, LexSetFixup_O, "LexSetFixup", LexFixup_O);

public:
  LexSetFixup_O() : LexFixup_O() {}
  LexSetFixup_O(LexicalInfo_sp lex) : LexFixup_O(lex, 0) {}
  CL_LISPIFY_NAME(LexSetFixup/make)
  CL_DEF_CLASS_METHOD
  static LexSetFixup_sp make(LexicalInfo_sp lex) { return gctools::GC<LexSetFixup_O>::allocate<gctools::RuntimeStage>(lex); }

public:
  virtual void emit(size_t position, SimpleVector_byte8_t_sp code);
  virtual size_t resize();
};

// Used to add a vm_entry at nonlocal entrance points.
// The LexicalVarInfo is that of the dynenv.
FORWARD(EntryFixup);
class EntryFixup_O : public LexFixup_O {
  LISP_CLASS(comp, CompPkg, EntryFixup_O, "EntryFixup", LexFixup_O);

public:
  EntryFixup_O() : LexFixup_O() {}
  EntryFixup_O(LexicalInfo_sp lex) : LexFixup_O(lex, 0) {}
  CL_LISPIFY_NAME(EntryFixup/make)
  CL_DEF_CLASS_METHOD
  static EntryFixup_sp make(LexicalInfo_sp lex) { return gctools::GC<EntryFixup_O>::allocate<gctools::RuntimeStage>(lex); }

public:
  virtual void emit(size_t position, SimpleVector_byte8_t_sp code);
  virtual size_t resize();
};

// Generate a ref or restore_sp for a (possibly nonlocal) exit.
FORWARD(RestoreSPFixup);
class RestoreSPFixup_O : public LexFixup_O {
  LISP_CLASS(comp, CompPkg, RestoreSPFixup_O, "RestoreSPFixup", LexFixup_O);

public:
  RestoreSPFixup_O() : LexFixup_O() {}
  RestoreSPFixup_O(LexicalInfo_sp lex) : LexFixup_O(lex, 0) {}
  CL_LISPIFY_NAME(RestoreSPFixup/make)
  CL_DEF_CLASS_METHOD
  static RestoreSPFixup_sp make(LexicalInfo_sp lex) { return gctools::GC<RestoreSPFixup_O>::allocate<gctools::RuntimeStage>(lex); }

public:
  virtual void emit(size_t position, SimpleVector_byte8_t_sp code);
  virtual size_t resize();
};

// Generate an exit or jump.
FORWARD(ExitFixup);
class ExitFixup_O : public LabelFixup_O {
  LISP_CLASS(comp, CompPkg, ExitFixup_O, "ExitFixup", LabelFixup_O);

public:
  // Clasp doesn't like C++ multiple inheritance.
  LexicalInfo_sp _lex;

public:
  ExitFixup_O() : LabelFixup_O() {}
  ExitFixup_O(LexicalInfo_sp lex, Label_sp label) : LabelFixup_O(label, 2), _lex(lex) {}
  CL_LISPIFY_NAME(ExitFixup/make)
  CL_DEF_CLASS_METHOD
  static ExitFixup_sp make(LexicalInfo_sp lex, Label_sp label) {
    return gctools::GC<ExitFixup_O>::allocate<gctools::RuntimeStage>(lex, label);
  }

public:
  virtual void emit(size_t position, SimpleVector_byte8_t_sp code);
  virtual size_t resize();
  LexicalInfo_sp lex() { return this->_lex; }
};

// Similar to EntryFixup, adds a vm_entry_close.
FORWARD(EntryCloseFixup);
class EntryCloseFixup_O : public LexFixup_O {
  LISP_CLASS(comp, CompPkg, EntryCloseFixup_O, "EntryCloseFixup", LexFixup_O);

public:
  EntryCloseFixup_O() : LexFixup_O() {}
  EntryCloseFixup_O(LexicalInfo_sp lex) : LexFixup_O(lex, 0) {}
  CL_LISPIFY_NAME(EntryCloseFixup/make)
  CL_DEF_CLASS_METHOD
  static EntryCloseFixup_sp make(LexicalInfo_sp lex) {
    return gctools::GC<EntryCloseFixup_O>::allocate<gctools::RuntimeStage>(lex);
  }

public:
  virtual void emit(size_t position, SimpleVector_byte8_t_sp code);
  virtual size_t resize();
};

// Object that lives in the compiler's literals vector to represent a
// LOAD-TIME-VALUE form.
FORWARD(LoadTimeValueInfo);
class LoadTimeValueInfo_O : public General_O {
  LISP_CLASS(comp, CompPkg, LoadTimeValueInfo_O, "LoadTimeValueInfo", General_O);

public:
  LoadTimeValueInfo_O(T_sp form, bool read_only_p) : _form(form), _read_only_p(read_only_p) {}

public:
  T_sp _form; // the form that will produce the value
  // Whether the load-time-value was marked read-only.
  // This is currently unused, but it's included in case we want it later.
  bool _read_only_p;

public:
  CL_LISPIFY_NAME(LoadTimeValueInfo/make)
  CL_DEF_CLASS_METHOD
  static LoadTimeValueInfo_sp make(T_sp form, bool read_only_p) {
    return gctools::GC<LoadTimeValueInfo_O>::allocate<gctools::RuntimeStage>(form, read_only_p);
  }

public:
  CL_LISPIFY_NAME(LoadTimeValueInfo/form)
  CL_DEFMETHOD T_sp form() { return this->_form; }
  CL_LISPIFY_NAME(LoadTimeValueInfo/ReadOnlyP)
  CL_DEFMETHOD bool read_only_p() { return this->_read_only_p; }
  // Evaluate the load time value form.
  CL_LISPIFY_NAME(LoadTimeValueInfo/eval)
  CL_DEFMETHOD T_sp eval();
};

// Wrapper for compiler constant literals. Having this is necessary to
// avoid any weird problems from using any of the other wrappers as
// constants... which isn't terribly likely, but better safe than sorry.
FORWARD(ConstantInfo)
class ConstantInfo_O : public General_O {
  LISP_CLASS(comp, CompPkg, ConstantInfo_O, "ConstantInfo", General_O);

public:
  ConstantInfo_O(T_sp value) : _value(value) {}

public:
  T_sp _value; // the value of the constant.
public:
  CL_LISPIFY_NAME(ConstantInfo/make)
  CL_DEF_CLASS_METHOD
  static ConstantInfo_sp make(T_sp value) { return gctools::GC<ConstantInfo_O>::allocate<gctools::RuntimeStage>(value); }

public:
  CL_LISPIFY_NAME(ConstantInfo/value)
  CL_DEFMETHOD T_sp value() { return this->_value; }
};

// Compiler literals marker representing the load-time lookup of a
// function cell. (Clasp doesn't currently have function cells, so the
// "cell" will just be the function name.)
FORWARD(FunctionCellInfo)
class FunctionCellInfo_O : public General_O {
  LISP_CLASS(comp, CompPkg, FunctionCellInfo_O, "FunctionCellInfo", General_O);

public:
  FunctionCellInfo_O(T_sp fname) : _fname(fname) {}

public:
  T_sp _fname; // Name of the function being looked up.
public:
  CL_LISPIFY_NAME(FunctionCellInfo/make)
  CL_DEF_CLASS_METHOD
  static FunctionCellInfo_sp make(T_sp fname) { return gctools::GC<FunctionCellInfo_O>::allocate<gctools::RuntimeStage>(fname); }

public:
  CL_LISPIFY_NAME(FunctionCellInfo/fname)
  CL_DEFMETHOD T_sp fname() { return this->_fname; }
};

// Ditto, but for variables.
FORWARD(VariableCellInfo)
class VariableCellInfo_O : public General_O {
  LISP_CLASS(comp, CompPkg, VariableCellInfo_O, "VariableCellInfo", General_O);

public:
  VariableCellInfo_O(Symbol_sp vname) : _vname(vname) {}

public:
  Symbol_sp _vname; // Name of the variable being looked up.
public:
  CL_LISPIFY_NAME(VariableCellInfo/make)
  CL_DEF_CLASS_METHOD
  static VariableCellInfo_sp make(Symbol_sp vname) {
    return gctools::GC<VariableCellInfo_O>::allocate<gctools::RuntimeStage>(vname);
  }

public:
  CL_LISPIFY_NAME(VariableCellInfo/vname)
  CL_DEFMETHOD Symbol_sp vname() { return this->_vname; }
};

// And the environment, which is useless within Clasp itself but
// needed for portable VM stuff.
FORWARD(EnvInfo)
class EnvInfo_O : public General_O {
  LISP_CLASS(comp, CompPkg, EnvInfo_O, "EnvInfo", General_O);
public:
  EnvInfo_O() {}
public:
  CL_LISPIFY_NAME(EnvInfo/make)
  CL_DEF_CLASS_METHOD
  static EnvInfo_sp make() {
    return gctools::GC<EnvInfo_O>::allocate<gctools::RuntimeStage>();
  }
};

class Module_O : public General_O {
  LISP_CLASS(comp, CompPkg, Module_O, "Module", General_O);

public:
  ComplexVector_T_sp _cfunctions;
  ComplexVector_T_sp _literals;

public:
  Module_O()
      : _cfunctions(ComplexVector_T_O::make(1, nil<T_O>(), clasp_make_fixnum(0))),
        _literals(ComplexVector_T_O::make(0, nil<T_O>(), clasp_make_fixnum(0))) {}
  Module_O(ComplexVector_T_sp literals)
      : _cfunctions(ComplexVector_T_O::make(1, nil<T_O>(), clasp_make_fixnum(0))), _literals(literals) {}
  CL_LISPIFY_NAME(Module/make)
  CL_DEF_CLASS_METHOD
  static Module_sp make() { return gctools::GC<Module_O>::allocate<gctools::RuntimeStage>(); }
  // Ugly. Can we do keyword parameters w/defaults instead?
  CL_LISPIFY_NAME(Module/make_with_literals)
  CL_DEF_CLASS_METHOD
  static Module_sp make_with_literals(ComplexVector_T_sp nliterals) {
    return gctools::GC<Module_O>::allocate<gctools::RuntimeStage>(nliterals);
  }
  CL_DEFMETHOD ComplexVector_T_sp cfunctions() { return this->_cfunctions; }
  CL_LISPIFY_NAME(module/literals) // avoid defining cmp::literals
  CL_DEFMETHOD ComplexVector_T_sp literals() { return this->_literals; }

public:
  // Use the optimistic bytecode vector sizes to initialize the optimistic
  // cfunction position.
  CL_DEFMETHOD void initialize_cfunction_positions();
  // With all functions and annotations initialized with optimistic
  // sizes, resize fixups until no more expansion is needed.
  CL_DEFMETHOD void resolve_fixup_sizes();
  // The size of the module bytecode vector.
  CL_DEFMETHOD size_t bytecode_size();
  // Fix up the debug infos with resolved labels.
  void resolve_debug_infos();
  // Finalize cfunction positions and fixup sizes.
  CL_DEFMETHOD void link();
  // Create the bytecode module vector. We scan over the fixups in the
  // module and copy segments of bytecode between fixup positions.
  // Can only be run after link.
  CL_DEFMETHOD SimpleVector_byte8_t_sp create_bytecode();
  // Create the debug info vector. Can only be run after link.
  CL_DEFMETHOD SimpleVector_sp create_debug_info();
  // Link, then create actual run-time function objects and a bytecode module.
  // Suitable for cl:compile.
  CL_DEFMETHOD void link_load();
};

class Cfunction_O : public General_O {
  LISP_CLASS(comp, CompPkg, Cfunction_O, "Cfunction", General_O);

public:
  Module_sp _module;
  // Bytecode vector for this cfunction.
  ComplexVector_byte8_t_sp _bytecode;
  // An ordered vector of annotations emitted in this cfunction.
  ComplexVector_T_sp _annotations;
  // An ordered vector of debug infos emitted for this cfunction.
  ComplexVector_T_sp _debug_info;
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
  BytecodeSimpleFun_sp _info;
  // Stuff for the function description.
  T_sp _name;
  T_sp _doc;
  T_sp _lambda_list;
  T_sp _source_pos_info;

public:
  Cfunction_O(Module_sp module, T_sp name, T_sp doc, T_sp lambda_list, T_sp source_pos_info)
      : _module(module),
        // A zero-length adjustable vector with fill pointer.
        _bytecode(ComplexVector_byte8_t_O::make_vector(0, 0, clasp_make_fixnum(0), nil<T_O>(), false, clasp_make_fixnum(0))),
        _annotations(ComplexVector_T_O::make(0, nil<T_O>(), clasp_make_fixnum(0))),
        _debug_info(ComplexVector_T_O::make(0, nil<T_O>(), clasp_make_fixnum(0))),
        _closed(ComplexVector_T_O::make(0, nil<T_O>(), clasp_make_fixnum(0))), _entry_point(Label_O::make()),
        // not sure this has to be initialized, but just in case
        _info(unbound<BytecodeSimpleFun_O>()), _name(name), _doc(doc), _lambda_list(lambda_list),
        _source_pos_info(source_pos_info) {}
  CL_LISPIFY_NAME(Cfunction/make)
  CL_DEF_CLASS_METHOD
  static Cfunction_sp make(Module_sp module, T_sp name, T_sp doc, T_sp lambda_list, T_sp source_pos_info) {
    return gctools::GC<Cfunction_O>::allocate<gctools::RuntimeStage>(module, name, doc, lambda_list, source_pos_info);
  }
  CL_DEFMETHOD Module_sp module() { return _module; }
  CL_LISPIFY_NAME(cfunction/bytecode)
  CL_DEFMETHOD ComplexVector_byte8_t_sp bytecode() const { return _bytecode; }
  CL_DEFMETHOD ComplexVector_T_sp annotations() const { return _annotations; }
  CL_LISPIFY_NAME(cfunction/debug-info)
  CL_DEFMETHOD ComplexVector_T_sp debug_info() const { return _debug_info; }
  CL_DEFMETHOD size_t nlocals() const { return _nlocals; }
  CL_LISPIFY_NAME(Cfunction/setf-nlocals)
  CL_DEFMETHOD size_t setNlocals(size_t new_nlocals) {
    this->_nlocals = new_nlocals;
    return new_nlocals;
  }
  CL_DEFMETHOD ComplexVector_T_sp closed() const { return _closed; }
  CL_LISPIFY_NAME(Cfunction/entry-point)
  CL_DEFMETHOD Label_sp entry_point() const { return _entry_point; }
  CL_LISPIFY_NAME(Cfunction/position)
  CL_DEFMETHOD size_t pposition() const { return _position; }
  CL_LISPIFY_NAME(Cfunction/setf-position)
  CL_DEFMETHOD size_t setPosition(size_t npos) {
    this->_position = npos;
    return npos;
  }
  CL_DEFMETHOD size_t extra() const { return _extra; }
  CL_LISPIFY_NAME(Cfunction/setf-extra)
  CL_LAMBDA(cfunction new) // avoid name conflict with next
  CL_DEFMETHOD size_t setExtra(size_t next) {
    this->_extra = next;
    return next;
  }
  CL_LISPIFY_NAME(Cfunction/final_size)
  CL_DEFMETHOD size_t final_size() const { return this->bytecode()->length() + this->extra(); }
  CL_LISPIFY_NAME(Cfunction/index)
  CL_DEFMETHOD size_t iindex() const { return _index; }
  CL_LISPIFY_NAME(Cfunction/setf-index)
  CL_DEFMETHOD size_t setIndex(size_t nindex) {
    this->_index = nindex;
    return nindex;
  }
  CL_DEFMETHOD BytecodeSimpleFun_sp info() const { return _info; }
  CL_LISPIFY_NAME(Cfunction/setf-info)
  CL_DEFMETHOD BytecodeSimpleFun_sp setInfo(BytecodeSimpleFun_sp gbep) {
    this->_info = gbep;
    return gbep;
  }
  CL_LISPIFY_NAME(Cfunction/name)
  CL_DEFMETHOD T_sp nname() const { return _name; }
  CL_DEFMETHOD T_sp doc() const { return _doc; }
  CL_LISPIFY_NAME(Cfunction/lambda-list)
  CL_DEFMETHOD T_sp lambda_list() const { return _lambda_list; }
  CL_LISPIFY_NAME(Cfunction/source-pos-info)
  CL_DEFMETHOD T_sp sourcePosInfo() const { return _source_pos_info; }

public:
  // Convenience method to link the module and return the new bytecode function
  // corresponding to this cfunction. Good for cl:compile.
  CL_DEFMETHOD Function_sp link_function();
public:
  // For use as a BytecodeDebugInfo.
  T_sp start() const;
  T_sp end() const;
};

// Main entry point
Function_sp bytecompile(T_sp, Lexenv_sp);
// main entry point for using the evaluator
T_mv cmp__bytecode_implicit_compile_form(T_sp, T_sp);
T_mv bytecode_toplevel_eval(T_sp, T_sp);
// Used in loader
bool btb_bcfun_p(BytecodeSimpleFun_sp, SimpleVector_sp);

}; // namespace comp
