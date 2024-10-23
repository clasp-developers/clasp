#include <clasp/core/bytecode_compiler.h>
#include <clasp/core/evaluator.h>         // extract_decl...
#include <clasp/core/sysprop.h>           // core__get_sysprop
#include <clasp/core/lambdaListHandler.h> // lambda list parsing
#include <clasp/core/designators.h>       // calledFunctionDesignator
#include <clasp/core/primitives.h>        // gensym, function_block_name
#include <clasp/core/sourceFileInfo.h>    // source info stuff
#include <clasp/llvmo/llvmoPackage.h>
#include <clasp/core/bytecode.h>
#include <algorithm> // max

namespace comp {

using namespace core;

T_sp Lexenv_O::variableInfo(T_sp varname) {
  T_sp vars = this->vars();
  if (vars.nilp())
    return vars;
  else {
    T_sp pair = core__alist_assoc_eq(gc::As_assert<Cons_sp>(vars), varname);
    if (pair.nilp())
      return pair;
    else
      return oCdr(pair);
  }
}

T_sp Lexenv_O::lookupSymbolMacro(T_sp sname) {
  T_sp info = this->variableInfo(sname);
  if (gc::IsA<SymbolMacroVarInfo_sp>(info))
    return gc::As_unsafe<SymbolMacroVarInfo_sp>(info)->expander();
  else
    return nil<T_O>();
}

T_sp Lexenv_O::functionInfo(T_sp funname) {
  T_sp funs = this->funs();
  if (funs.nilp())
    return funs;
  else {
    T_sp pair = core__alist_assoc_equal(gc::As_assert<Cons_sp>(funs), funname);
    if (pair.nilp())
      return pair;
    else
      return oCdr(pair);
  }
}

T_sp Lexenv_O::lookupMacro(Symbol_sp macroname) {
  T_sp info = this->functionInfo(macroname);
  if (gc::IsA<LocalMacroInfo_sp>(info))
    return gc::As_unsafe<LocalMacroInfo_sp>(info)->expander();
  else if (gc::IsA<LocalFunInfo_sp>(info))
    return nil<T_O>(); // not a macro, i.e. shadowed
  // no local info: check global
  else if (macroname->fboundp() && macroname->macroP())
    return macroname->symbolFunction();
  else // nothing
    return nil<T_O>();
}

T_sp Lexenv_O::blockInfo(T_sp blockname) {
  T_sp blocks = this->blocks();
  if (blocks.nilp())
    return blocks;
  else {
    T_sp pair = core__alist_assoc_equal(gc::As_assert<Cons_sp>(blocks), blockname);
    if (pair.nilp())
      return pair;
    else
      return oCdr(pair);
  }
}

T_sp Lexenv_O::tagInfo(T_sp tagname) {
  T_sp tags = this->tags();
  if (tags.nilp())
    return tags;
  else {
    T_sp pair = core__alist_assoc_equal(gc::As_assert<Cons_sp>(tags), tagname);
    if (pair.nilp())
      return pair;
    else
      return oCdr(pair);
  }
}

Lexenv_sp Lexenv_O::bind_vars(List_sp vars, const Context ctxt) {
  if (vars.nilp())
    return this->asSmartPtr();
  size_t frame_start = this->frameEnd();
  size_t frame_end = frame_start + vars.unsafe_cons()->length();
  Cfunction_sp cf = ctxt.cfunction();

  cf->setNlocals(std::max(frame_end, cf->nlocals()));
  size_t idx = frame_start;
  List_sp new_vars = this->vars();
  for (auto cur : vars) {
    Symbol_sp var = gc::As<Symbol_sp>(oCar(cur));
    if (var->getReadOnly())
      SIMPLE_ERROR("Cannot bind constant value {}!", _rep_(var));
    auto info = LexicalVarInfo_O::make(idx++, cf);
    Cons_sp pair = Cons_O::create(var, info);
    new_vars = Cons_O::create(pair, new_vars);
  }
  return this->sub_vars(new_vars, frame_end);
}

// Save a cons and append call in a common case.
Lexenv_sp Lexenv_O::bind1var(Symbol_sp var, const Context ctxt) {
  size_t frame_start = this->frameEnd();
  size_t frame_end = frame_start + 1;
  Cfunction_sp cf = ctxt.cfunction();
  cf->setNlocals(std::max(frame_end, cf->nlocals()));
  auto info = LexicalVarInfo_O::make(frame_start, cf);
  Cons_sp pair = Cons_O::create(var, info);
  Cons_sp new_vars = Cons_O::create(pair, this->vars());
  return this->sub_vars(new_vars, frame_end);
}

Lexenv_sp Lexenv_O::bind_funs(List_sp funs, const Context ctxt) {
  if (funs.nilp())
    return this->asSmartPtr();
  size_t frame_start = this->frameEnd();
  size_t frame_end = frame_start + funs.unsafe_cons()->length();
  Cfunction_sp cf = ctxt.cfunction();

  cf->setNlocals(std::max(frame_end, cf->nlocals()));
  size_t idx = frame_start;
  List_sp new_funs = this->funs();
  for (auto cur : funs) {
    T_sp name = oCar(cur);
    auto info = LocalFunInfo_O::make(idx++, cf);
    Cons_sp pair = Cons_O::create(name, info);
    new_funs = Cons_O::create(pair, new_funs);
  }
  return this->sub_funs(new_funs, frame_end);
}

Lexenv_sp Lexenv_O::bind_block(T_sp name, Label_sp exit, const Context ctxt) {
  size_t frame_start = this->frameEnd();
  Cfunction_sp cf = ctxt.cfunction();

  cf->setNlocals(std::max(frame_start + 1, cf->nlocals()));
  BlockInfo_sp binfo = BlockInfo_O::make(frame_start, ctxt.cfunction(), exit, ctxt.receiving());
  return this->sub_block(Cons_O::create(Cons_O::create(name, binfo), this->blocks()));
}

Lexenv_sp Lexenv_O::bind_tags(List_sp tags, LexicalInfo_sp dynenv, const Context ctxt) {
  Cfunction_sp cf = ctxt.cfunction();
  cf->setNlocals(std::max(this->frameEnd() + 1, cf->nlocals()));
  if (tags.nilp())
    return this->asSmartPtr();
  List_sp new_tags = this->tags();
  for (auto cur : tags) {
    T_sp tag = oCar(cur);
    TagInfo_sp info = TagInfo_O::make(dynenv, Label_O::make());
    Cons_sp pair = Cons_O::create(tag, info);
    new_tags = Cons_O::create(pair, new_tags);
  }
  return this->sub_tags(new_tags);
}

Lexenv_sp Lexenv_O::add_specials(List_sp vars) {
  if (vars.nilp())
    return this->asSmartPtr();
  List_sp new_vars = this->vars();
  for (auto cur : vars) {
    Symbol_sp var = gc::As<Symbol_sp>(oCar(cur));
    if (this->lookupSymbolMacro(var).notnilp())
      SIMPLE_PROGRAM_ERROR("A symbol macro was declared SPECIAL:~%~s", var);
    auto info = SpecialVarInfo_O::make(var->specialP());
    Cons_sp pair = Cons_O::create(var, info);
    new_vars = Cons_O::create(pair, new_vars);
  }
  return this->sub_vars(new_vars, this->frameEnd());
}

static List_sp scrub_decls(List_sp decls) {
  // Remove SPECIAL declarations since those are already handled.
  ql::list r;
  for (auto cur : decls) {
    T_sp decl = oCar(cur);
    if (!gc::IsA<Cons_sp>(decl) || oCar(decl) != cl::_sym_special)
      r << decl;
  }
  return r.cons();
}

Lexenv_sp Lexenv_O::add_decls(List_sp decls) {
  decls = scrub_decls(decls);
  if (decls.nilp())
    return this->asSmartPtr();
  else
    return this->sub_decls(Cons_O::append(decls, this->decls()));
}

Lexenv_sp Lexenv_O::macroexpansion_environment() {
  ql::list new_vars;
  for (auto cur : (List_sp)(this->vars())) {
    T_sp pair = oCar(cur);
    T_sp info = oCdr(pair);
    if (gc::IsA<ConstantVarInfo_sp>(info) || gc::IsA<SymbolMacroVarInfo_sp>(info))
      new_vars << pair;
  }
  ql::list new_funs;
  for (auto cur : (List_sp)(this->funs())) {
    T_sp pair = oCar(cur);
    T_sp info = oCdr(pair);
    if (gc::IsA<GlobalMacroInfo_sp>(info) || gc::IsA<LocalMacroInfo_sp>(info))
      new_funs << pair;
  }
  return Lexenv_O::make(new_vars.cons(), nil<T_O>(), nil<T_O>(), new_funs.cons(), this->decls(), 0);
}

CL_DEFUN Lexenv_sp make_null_lexical_environment() {
  return Lexenv_O::make(nil<T_O>(), nil<T_O>(), nil<T_O>(), nil<T_O>(), nil<T_O>(), 0);
}

void assemble(const Context context, uint8_t opcode, List_sp operands) {
  Cfunction_sp func = context.cfunction();
  ComplexVector_byte8_t_sp bytecode = func->bytecode();
  bytecode->vectorPushExtend(opcode);
  for (auto cur : operands) {
    bytecode->vectorPushExtend(clasp_to_integral<uint8_t>(oCar(cur)));
  }
}

template <typename T>
[[deprecated]] inline constexpr void print_type(T&& t, const char* msg=nullptr) {}

CL_LAMBDA(code position &rest values);
CL_DEFUN void assemble_into(SimpleVector_byte8_t_sp code, size_t position, List_sp values) {
  for (auto cur : values)
    code[position++] = clasp_to_integral<uint8_t>(oCar(cur));
}

void assemble_maybe_long(const Context context, uint8_t opcode, List_sp operands) {
  Cfunction_sp func = context.cfunction();
  ComplexVector_byte8_t_sp bytecode = func->bytecode();
  // Check for long operands. Also signal an error if something''s over 16 bits.
  bool longp = false;
  for (auto cur : operands) {
    if (clasp_to_integral<uint16_t>(oCar(cur)) > 255) {
      longp = true;
      break;
    }
  }
  if (longp) {
    bytecode->vectorPushExtend((uint8_t)vm_code::_long);
    bytecode->vectorPushExtend(opcode);
    for (auto cur : operands) {
      uint16_t operand = clasp_to_integral<uint16_t>(oCar(cur));
      bytecode->vectorPushExtend(operand & 0xff); // low
      bytecode->vectorPushExtend(operand >> 8);   // high
    }
  } else { // normal/short
    bytecode->vectorPushExtend(opcode);
    for (auto cur : operands)
      bytecode->vectorPushExtend(clasp_to_integral<uint8_t>(oCar(cur)));
  }
}

CL_DEFUN T_sp var_info(Symbol_sp sym, Lexenv_sp env) {
  // Local?
  T_sp info = env->variableInfo(sym);
  if (info.notnilp())
    return info;
  // Constant?
  // (Constants are also specialP, so we have to check constancy first.)
  if (cl__keywordp(sym) || sym->getReadOnly())
    return ConstantVarInfo_O::make(sym->symbolValue());
  // Globally special?
  if (sym->specialP())
    return SpecialVarInfo_O::make(true);
  // Global symbol macro?
  T_mv symmac = core__get_sysprop(sym, ext::_sym_symbolMacro);
  MultipleValues& mvn = core::lisp_multipleValues();
  if (gc::As_unsafe<T_sp>(mvn.valueGet(1, symmac.number_of_values())).notnilp()) {
    T_sp symmac0 = symmac;
    Function_sp fsymmac = gc::As_assert<Function_sp>(symmac0);
    return SymbolMacroVarInfo_O::make(fsymmac);
  }
  // Unknown.
  return nil<T_O>();
}

// Like the above, but returns a std::variant. Good when you don't need
// to cons up info objects.
VarInfoV var_info_v(Symbol_sp sym, Lexenv_sp env) {
  T_sp info = env->variableInfo(sym);
  if (gc::IsA<LexicalVarInfo_sp>(info)) // in_place_type_t?
    return VarInfoV(LexicalVarInfoV(gc::As_unsafe<LexicalVarInfo_sp>(info)));
  else if (gc::IsA<SpecialVarInfo_sp>(info))
    return VarInfoV(SpecialVarInfoV(gc::As_unsafe<SpecialVarInfo_sp>(info)));
  else if (gc::IsA<SymbolMacroVarInfo_sp>(info))
    return VarInfoV(SymbolMacroVarInfoV(gc::As_unsafe<SymbolMacroVarInfo_sp>(info)));
  ASSERT(info.nilp());
  // Constant?
  if (cl__keywordp(sym) || sym->getReadOnly())
    return VarInfoV(ConstantVarInfoV(sym->symbolValue()));
  // Globally special?
  if (sym->specialP())
    return VarInfoV(SpecialVarInfoV(true));
  // Global symbol macro?
  T_mv symmac = core__get_sysprop(sym, ext::_sym_symbolMacro);
  MultipleValues& mvn = core::lisp_multipleValues();
  if (gc::As_unsafe<T_sp>(mvn.valueGet(1, symmac.number_of_values())).notnilp()) {
    T_sp symmac0 = symmac;
    Function_sp fsymmac = gc::As_assert<Function_sp>(symmac0);
    return VarInfoV(SymbolMacroVarInfoV(fsymmac));
  }
  // Unknown.
  return VarInfoV(NoVarInfoV());
}

CL_DEFUN T_sp fun_info(T_sp name, Lexenv_sp env) {
  // Local?
  T_sp info = env->functionInfo(name);
  if (info.notnilp())
    return info;
  // Split into setf and not versions.
  if (name.consp()) {
    List_sp cname = name;
    if (oCar(cname) == cl::_sym_setf) {
      // take care of (setf . bar) or (setf bar foo) or (setf bar .foo)
      // so don't go directly for the cadr
      T_sp dname = oCdr(cname);
      if (dname.consp()) {
        T_sp sss = CONS_CAR(dname);
        Symbol_sp fname = gc::As<Symbol_sp>(sss);
        if (fname.notnilp() && oCdr(dname).nilp()) {
          if (!fname->fboundp_setf())
            return nil<T_O>();
          if (fname->macroP())
            return GlobalMacroInfo_O::make(fname->getSetfFdefinition());
          else {
            if (cl::_sym_compiler_macro_function->fboundp()) {
              T_sp cmexpander = eval::funcall(cl::_sym_compiler_macro_function, name);
              return GlobalFunInfo_O::make(cmexpander);
            } else
              return GlobalFunInfo_O::make(nil<T_O>());
          }
        }
      }
    }
    // Bad function name.
    return nil<T_O>();
  } else {
    Symbol_sp fname = gc::As<Symbol_sp>(name);
    if (!fname->fboundp())
      return nil<T_O>();
    else if (fname->macroP())
      return GlobalMacroInfo_O::make(fname->symbolFunction());
    else {
      // Look for a compiler macro expander.
      if (cl::_sym_compiler_macro_function->fboundp()) {
        T_sp cmexpander = eval::funcall(cl::_sym_compiler_macro_function, fname);
        return GlobalFunInfo_O::make(cmexpander);
      } else
        return GlobalFunInfo_O::make(nil<T_O>());
    }
  }
}

FunInfoV fun_info_v(T_sp name, Lexenv_sp env) {
  // Local?
  T_sp info = env->functionInfo(name);
  if (gc::IsA<LocalFunInfo_sp>(info))
    return FunInfoV(LocalFunInfoV(gc::As_unsafe<LocalFunInfo_sp>(info)));
  else if (gc::IsA<LocalMacroInfo_sp>(info))
    return FunInfoV(LocalMacroInfoV(gc::As_unsafe<LocalMacroInfo_sp>(info)));
  ASSERT(info.nilp());
  // Split into setf and not versions.
  if (name.consp()) {
    List_sp cname = name;
    T_sp dname = oCdr(cname);
    if (oCar(cname) != cl::_sym_setf || !dname.consp() || oCdr(dname).notnilp())
      return FunInfoV(NoFunInfoV()); // TODO: error?
    T_sp sss = CONS_CAR(dname);
    Symbol_sp fname = gc::As<Symbol_sp>(sss);
    if (!fname->fboundp_setf())
      return FunInfoV(NoFunInfoV());
    if (fname->macroP())
      return FunInfoV(GlobalMacroInfoV(fname->getSetfFdefinition()));
    else if (cl::_sym_compiler_macro_function->fboundp()) {
      T_sp cmexpander = eval::funcall(cl::_sym_compiler_macro_function, name);
      return FunInfoV(GlobalFunInfoV(cmexpander));
    } else
      return FunInfoV(GlobalFunInfoV(nil<T_O>()));
  } else {
    Symbol_sp fname = gc::As<Symbol_sp>(name);
    if (!fname->fboundp())
      return FunInfoV(NoFunInfoV());
    else if (fname->macroP())
      return FunInfoV(GlobalMacroInfoV(fname->symbolFunction()));
    else if (cl::_sym_compiler_macro_function->fboundp()) {
      T_sp cmexpander = eval::funcall(cl::_sym_compiler_macro_function, fname);
      return FunInfoV(GlobalFunInfoV(cmexpander));
    } else
      return FunInfoV(GlobalFunInfoV(nil<T_O>()));
  }
}

bool Lexenv_O::notinlinep(T_sp fname) {
  for (auto cur : this->decls()) {
    T_sp decl = oCar(cur);
    if (gc::IsA<Cons_sp>(decl) && oCar(decl) == cl::_sym_notinline && gc::IsA<Cons_sp>(oCdr(decl)) &&
        gc::As_unsafe<Cons_sp>(oCdr(decl))->memberEq(fname).notnilp())
      return true;
  }
  return false;
}

// defined out of line for circularity reasons
Module_sp Context::module() const { return this->cfunction()->module(); }

size_t Context::literal_index(T_sp literal) const {
  ComplexVector_T_sp literals = this->cfunction()->module()->literals();
  // FIXME: Smarter POSITION
  for (size_t i = 0; i < literals->length(); ++i) {
    T_sp slit = literals[i];
    if (gc::IsA<ConstantInfo_sp>(slit) && gc::As_unsafe<ConstantInfo_sp>(slit)->value() == literal)
      return i;
  }
  Fixnum_sp nind = literals->vectorPushExtend(ConstantInfo_O::make(literal));
  return nind.unsafe_fixnum();
}

// Like literal-index, but forces insertion. This is used when generating
// a keyword argument parser, since the keywords must be sequential even if
// they've previously appeared in the literals vector.
// This is also used by LTV processing to put in a placeholder.
size_t Context::new_literal_index(T_sp literal) const {
  Fixnum_sp nind = this->cfunction()->module()->literals()->vectorPushExtend(ConstantInfo_O::make(literal));
  return nind.unsafe_fixnum();
}

// We never coalesce LTVs at the moment. Hypothetically we could, but
// it seems like a pretty marginal thing.
size_t Context::ltv_index(T_sp form, bool read_only_p) const {
  LoadTimeValueInfo_sp ltvi = LoadTimeValueInfo_O::make(form, read_only_p);
  Fixnum_sp nind = this->cfunction()->module()->literals()->vectorPushExtend(ltvi);
  return nind.unsafe_fixnum();
}

size_t Context::cfunction_index(Cfunction_sp fun) const {
  ComplexVector_T_sp literals = this->cfunction()->module()->literals();
  // FIXME: Smarter POSITION
  for (size_t i = 0; i < literals->length(); ++i) {
    T_sp slit = literals[i];
    if (gc::IsA<Cfunction_sp>(slit) && slit == fun)
      return i;
  }
  Fixnum_sp nind = literals->vectorPushExtend(fun);
  return nind.unsafe_fixnum();
}

size_t Context::fcell_index(T_sp name) const {
  ComplexVector_T_sp literals = this->cfunction()->module()->literals();
  // FIXME: Smarter POSITION
  for (size_t i = 0; i < literals->length(); ++i) {
    T_sp slit = literals[i];
    if (gc::IsA<FunctionCellInfo_sp>(slit) && gc::As_unsafe<FunctionCellInfo_sp>(slit)->fname() == name)
      return i;
  }
  Fixnum_sp nind = literals->vectorPushExtend(FunctionCellInfo_O::make(name));
  return nind.unsafe_fixnum();
}

size_t Context::vcell_index(Symbol_sp name) const {
  ComplexVector_T_sp literals = this->cfunction()->module()->literals();
  // FIXME: Smarter POSITION
  for (size_t i = 0; i < literals->length(); ++i) {
    T_sp slit = literals[i];
    if (gc::IsA<VariableCellInfo_sp>(slit) && gc::As_unsafe<VariableCellInfo_sp>(slit)->vname() == name)
      return i;
  }
  Fixnum_sp nind = literals->vectorPushExtend(VariableCellInfo_O::make(name));
  return nind.unsafe_fixnum();
}

size_t Context::env_index() const {
  ComplexVector_T_sp literals = this->cfunction()->module()->literals();
  for (size_t i = 0; i < literals->length(); ++i) {
    T_sp slit = literals[i];
    if (gc::IsA<EnvInfo_sp>(slit))
      return i;
  }
  Fixnum_sp nind = literals->vectorPushExtend(EnvInfo_O::make());
  return nind.unsafe_fixnum();
}

size_t Context::closure_index(T_sp info) const {
  ComplexVector_T_sp closed = this->cfunction()->closed();
  for (size_t i = 0; i < closed->length(); ++i)
    if (closed[i] == info)
      return i;
  Fixnum_sp nind = closed->vectorPushExtend(info);
  return nind.unsafe_fixnum();
}

void Context::push_debug_info(T_sp info) const { this->cfunction()->debug_info()->vectorPushExtend(info); }

void Context::emit_jump(Label_sp label) const {
  ControlLabelFixup_O::make(label, vm_code::jump_8, vm_code::jump_16, vm_code::jump_24)->contextualize(*this);
}

void Context::emit_jump_if(Label_sp label) const {
  ControlLabelFixup_O::make(label, vm_code::jump_if_8, vm_code::jump_if_16, vm_code::jump_if_24)->contextualize(*this);
}

void Context::emit_entry_or_save_sp(LexicalInfo_sp dynenv) const { EntryFixup_O::make(dynenv)->contextualize(*this); }

void Context::emit_ref_or_restore_sp(LexicalInfo_sp dynenv) const { RestoreSPFixup_O::make(dynenv)->contextualize(*this); }

void Context::emit_exit(Label_sp label) const {
  ControlLabelFixup_O::make(label, vm_code::exit_8, vm_code::exit_16, vm_code::exit_24)->contextualize(*this);
}

void Context::emit_exit_or_jump(LexicalInfo_sp dynenv, Label_sp label) const {
  ExitFixup_O::make(dynenv, label)->contextualize(*this);
}

void Context::maybe_emit_entry_close(LexicalInfo_sp dynenv) const { EntryCloseFixup_O::make(dynenv)->contextualize(*this); }

void Context::emit_catch(Label_sp label) const {
  ControlLabelFixup_O::make(label, vm_code::catch_8, vm_code::catch_16, vm_code::catch_16)->contextualize(*this);
}

void Context::emit_jump_if_supplied(Label_sp label, size_t ind) const {
  JumpIfSuppliedFixup_O::make(label, ind)->contextualize(*this);
}

// Push the immutable value or cell of lexical in CONTEXT.
void Context::reference_lexical_info(LexicalInfo_sp info) const {
  if (info->cfunction() == this->cfunction())
    this->assemble1(vm_code::ref, info->frameIndex());
  else
    this->assemble1(vm_code::closure, this->closure_index(info));
}

void Context::maybe_emit_make_cell(LexicalVarInfo_sp info) const {
  LexRefFixup_O::make(info->lex(), vm_code::make_cell)->contextualize(*this);
}

void Context::maybe_emit_cell_ref(LexicalVarInfo_sp info) const {
  LexRefFixup_O::make(info->lex(), vm_code::cell_ref)->contextualize(*this);
}

void Context::maybe_emit_encage(LexicalVarInfo_sp info) const { EncageFixup_O::make(info->lex())->contextualize(*this); }

void Context::emit_lexical_set(LexicalVarInfo_sp info) const { LexSetFixup_O::make(info->lex())->contextualize(*this); }

void Context::emit_parse_key_args(size_t max_count, size_t key_count, size_t keystart, size_t indx, bool aokp) const {
  ComplexVector_byte8_t_sp bytecode = this->cfunction()->bytecode();
  if ((max_count < (1 << 8)) && (key_count < (1 << 8)) && (keystart < (1 << 8)) && (indx < (1 << 8))) {
    bytecode->vectorPushExtend((uint8_t)vm_code::parse_key_args);
    bytecode->vectorPushExtend(max_count);
    bytecode->vectorPushExtend(key_count | (aokp ? 0x80 : 0));
    bytecode->vectorPushExtend(keystart);
    bytecode->vectorPushExtend(indx);
  } else if ((max_count < (1 << 16)) && (key_count < (1 << 16)) && (keystart < (1 << 16)) && (indx < (1 << 16))) {
    bytecode->vectorPushExtend((uint8_t)vm_code::_long);
    bytecode->vectorPushExtend((uint8_t)vm_code::parse_key_args);
    bytecode->vectorPushExtend(max_count & 0xff);
    bytecode->vectorPushExtend(max_count >> 8);
    bytecode->vectorPushExtend(key_count & 0xff);
    bytecode->vectorPushExtend((key_count >> 8) | (aokp ? 0x80 : 0));
    bytecode->vectorPushExtend(keystart & 0xff);
    bytecode->vectorPushExtend(keystart >> 8);
    bytecode->vectorPushExtend(indx & 0xff);
    bytecode->vectorPushExtend(indx >> 8);
  } else
    SIMPLE_ERROR("Bytecode compiler limit reached: keyarg indices too large: %zu %zu %zu %zu", max_count, key_count, keystart,
                 indx);
}

void Context::assemble0(vm_code opcode) const { this->cfunction()->bytecode()->vectorPushExtend((uint8_t)opcode); }

void Context::assemble1(vm_code opcode, size_t operand) const {
  ComplexVector_byte8_t_sp bytecode = this->cfunction()->bytecode();
  if (operand < (1 << 8)) {
    bytecode->vectorPushExtend((uint8_t)opcode);
    bytecode->vectorPushExtend(operand);
  } else if (operand < (1 << 16)) {
    bytecode->vectorPushExtend((uint8_t)vm_code::_long);
    bytecode->vectorPushExtend((uint8_t)opcode);
    bytecode->vectorPushExtend(operand & 0xff);
    bytecode->vectorPushExtend(operand >> 8);
  } else
    SIMPLE_ERROR("Bytecode compiler limit reached: operand %zu too large", operand);
}

void Context::assemble2(vm_code opcode, size_t operand1, size_t operand2) const {
  ComplexVector_byte8_t_sp bytecode = this->cfunction()->bytecode();
  if ((operand1 < (1 << 8)) && (operand2 < (1 << 8))) {
    bytecode->vectorPushExtend((uint8_t)opcode);
    bytecode->vectorPushExtend(operand1);
    bytecode->vectorPushExtend(operand2);
  } else if ((operand1 < (1 << 16)) && (operand2 < (1 << 16))) {
    bytecode->vectorPushExtend((uint8_t)vm_code::_long);
    bytecode->vectorPushExtend((uint8_t)opcode);
    bytecode->vectorPushExtend(operand1 & 0xff);
    bytecode->vectorPushExtend(operand1 >> 8);
    bytecode->vectorPushExtend(operand2 & 0xff);
    bytecode->vectorPushExtend(operand2 >> 8);
  } else
    SIMPLE_ERROR("Bytecode compiler limit reached: operands %zu %zu too large", operand1, operand2);
}

void Context::emit_bind(size_t count, size_t offset) const {
  switch (count) {
  case 1:
    this->assemble1(vm_code::set, offset);
    break;
  case 0:
    break;
  default:
    this->assemble2(vm_code::bind, count, offset);
    break;
  }
}

void Context::emit_call(size_t argcount) const {
  switch (this->receiving()) {
  case 1:
    this->assemble1(vm_code::call_receive_one, argcount);
    break;
  case -1:
    this->assemble1(vm_code::call, argcount);
    break;
  default:
    this->assemble2(vm_code::call_receive_fixed, argcount, this->receiving());
    break;
  }
}

void Context::emit_mv_call() const {
  switch (this->receiving()) {
  case 1:
    this->assemble0(vm_code::mv_call_receive_one);
    break;
  case -1:
    this->assemble0(vm_code::mv_call);
    break;
  default:
    this->assemble1(vm_code::mv_call_receive_fixed, this->receiving());
    break;
  }
}

void Context::emit_special_bind(Symbol_sp sym) const { this->assemble1(vm_code::special_bind, this->vcell_index(sym)); }

void Context::emit_unbind(size_t count) const {
  for (size_t i = 0; i < count; ++i)
    this->assemble0(vm_code::unbind);
}

size_t Annotation_O::module_position() { return this->pposition() + gc::As_assert<Cfunction_sp>(this->cfunction())->pposition(); }

void Label_O::contextualize(const Context ctxt) {
  Cfunction_sp cfunction = ctxt.cfunction();
  this->setPosition(cfunction->bytecode()->length());
  this->setCfunction(cfunction);
  Fixnum_sp nind = cfunction->annotations()->vectorPushExtend(this->asSmartPtr());
  this->setIndex(nind.unsafe_fixnum());
}

void Fixup_O::contextualize(const Context ctxt) {
  Cfunction_sp cfunction = ctxt.cfunction();
  ComplexVector_byte8_t_sp assembly = cfunction->bytecode();
  size_t position = assembly->length();
  this->setCfunction(cfunction);
  this->setInitialPosition(position);
  this->setPosition(position);
  Fixnum_sp nind = cfunction->annotations()->vectorPushExtend(this->asSmartPtr());
  this->setIndex(nind.unsafe_fixnum());
  for (size_t i = 0; i < this->initial_size(); ++i)
    assembly->vectorPushExtend(0);
}

ptrdiff_t LabelFixup_O::delta() { return this->label()->module_position() - this->module_position(); }

static void emit_control_label_fixup(size_t size, size_t offset, size_t position, SimpleVector_byte8_t_sp code, vm_code opcode8,
                                     vm_code opcode16, vm_code opcode24) {
  // Offset is a size_t so it's a positive integer i.e. dumpable.
  switch (size) {
  case 2:
      code[position] = (uint8_t)opcode8;
    break;
  case 3:
    code[position] = (uint8_t)opcode16;
    break;
  case 4:
    code[position] = (uint8_t)opcode24;
    break;
  default:
    SIMPLE_ERROR("Assembler bug: Impossible size %zu", size);
  }
  for (size_t i = 0; i < size - 1; ++i) {
    // Write the offset one byte at a time, starting with the LSB.
    code[position + i + 1] = offset & 0xff;
    offset >>= 8;
  }
}

void ControlLabelFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  emit_control_label_fixup(this->size(), this->delta(), position, code, this->_opcode8, this->_opcode16, this->_opcode24);
}

size_t resize_control_label_fixup(ptrdiff_t delta) {
  if ((-(1 << 7) <= delta) && (delta <= (1 << 7) - 1))
    return 2;
  if ((-(1 << 15) <= delta) && (delta <= (1 << 15) - 1))
    return 3;
  if ((-(1 << 23) <= delta) && (delta <= (1 << 23) - 1))
    return 4;
  else
    SIMPLE_ERROR("Bytecode compiler limit reached: Fixup delta too large");
}

size_t ControlLabelFixup_O::resize() { return resize_control_label_fixup(this->delta()); }

void JumpIfSuppliedFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  uint16_t index = this->iindex();
  if (index > 0xff)
    code[position++] = (uint8_t)vm_code::_long;
  size_t size = this->size();
  bool s16 = false;
  switch (size) {
  case 3:
    code[position++] = (uint8_t)vm_code::jump_if_supplied_8;
    break;
  case 4:
    s16 = true;
    code[position++] = (uint8_t)vm_code::jump_if_supplied_16;
    break;
  case 5:
    code[position++] = (uint8_t)vm_code::jump_if_supplied_8;
    break;
  case 6:
    s16 = true;
    code[position] = (uint8_t)vm_code::jump_if_supplied_16;
    break;
  default:
    SIMPLE_ERROR("Assembler bug: Impossible size %zu", size);
  }
  code[position++] = index & 0xff;
  if (index > 0xff)
    code[position++] = index >> 8;
  size_t offset = this->delta();
  code[position++] = offset & 0xff;
  if (s16)
    code[position] = offset >> 8;
}

size_t JumpIfSuppliedFixup_O::resize() {
  ptrdiff_t delta = this->delta();
  uint16_t index = this->iindex();
  if ((-(1 << 7) <= delta) && (delta <= (1 << 7) - 1)) {
    if (index > 0xff)
      return 5;
    else
      return 3;
  }
  if ((-(1 << 15) <= delta) && (delta <= (1 << 15) - 1)) {
    if (index > 0xff)
      return 6;
    else
      return 4;
  } else
    SIMPLE_ERROR("Bytecode compiler limit reached: Fixup delta too large");
}

void LexRefFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  size_t size = this->size();
  switch (size) {
  case 1:
    code[position] = (uint8_t)this->opcode();
    break;
  default:
    UNREACHABLE();
  }
}

size_t LexRefFixup_O::resize() { return this->lex()->indirectLexicalP() ? 1 : 0; }

void EncageFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  size_t size = this->size();
  size_t index = this->lex()->frameIndex();
  switch (size) {
  case 2: // FIXME: Use assemble_into?
    code[position] = (uint8_t)vm_code::encell;
    code[position + 1] = index;
    break;
  case 4:
    code[position] = (uint8_t)vm_code::_long;
    code[position + 1] = (uint8_t)vm_code::encell;
    code[position + 2] = index & 0xff;
    code[position + 3] = index >> 8;
    break;
  default:
    UNREACHABLE();
  }
}

size_t EncageFixup_O::resize() {
  size_t index = this->lex()->frameIndex();
  if (!this->lex()->indirectLexicalP())
    return 0;
  else if (index < 1 << 8)
    return 2;
  else if (index < 1 << 16)
    return 4;
  else
    SIMPLE_ERROR("Bytecode compiler limit reached: Fixup delta too large");
}

void LexSetFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  size_t size = this->size();
  size_t index = this->lex()->frameIndex();
  switch (size) {
  case 2:
    code[position] = (uint8_t)vm_code::set;
    code[position + 1] = index;
    break;
  case 3:
    code[position] = (uint8_t)vm_code::ref;
    code[position + 1] = index;
    code[position + 2] = (uint8_t)vm_code::cell_set;
    break;
  case 4:
    code[position] = (uint8_t)vm_code::_long;
    code[position + 1] = (uint8_t)vm_code::set;
    code[position + 2] = index & 0xff;
    code[position + 3] = index >> 8;
    break;
  case 5:
    code[position] = (uint8_t)vm_code::_long;
    code[position + 1] = (uint8_t)vm_code::ref;
    code[position + 2] = index & 0xff;
    code[position + 3] = index >> 8;
    code[position + 4] = (uint8_t)vm_code::cell_set;
    break;
  default:
    UNREACHABLE();
  }
}

size_t LexSetFixup_O::resize() {
  bool indirectp = this->lex()->indirectLexicalP();
  size_t index = this->lex()->frameIndex();
  if (index < 1 << 8)
    if (indirectp)
      return 3;
    else
      return 2;
  else if (index < 1 << 16)
    if (indirectp)
      return 5;
    else
      return 4;
  else
    SIMPLE_ERROR("Bytecode compiler limit reached: Fixup delta too large");
}

void EntryFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  size_t index = this->lex()->frameIndex();
  if (index >= 1 << 8)
    code[position++] = (uint8_t)vm_code::_long;
  if (this->lex()->closedOverP())
    code[position] = (uint8_t)vm_code::entry;
  else
    code[position] = (uint8_t)vm_code::save_sp;
  if (index < 1 << 8)
    code[position + 1] = index;
  else {
    code[position + 1] = index & 0xff;
    code[position + 2] = index >> 8;
  }
}

size_t EntryFixup_O::resize() { return (this->lex()->frameIndex() < 1 << 8) ? 2 : 4; }

void RestoreSPFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  size_t index = this->lex()->frameIndex();
  if (index >= 1 << 8)
    code[position++] = (uint8_t)vm_code::_long;
  if (this->lex()->closedOverP())
    code[position] = (uint8_t)vm_code::ref;
  else
    code[position] = (uint8_t)vm_code::restore_sp;
  if (index < 1 << 8)
    code[position + 1] = index;
  else {
    code[position + 1] = index & 0xff;
    code[position + 2] = index >> 8;
  }
}

size_t RestoreSPFixup_O::resize() { return (this->lex()->frameIndex() < 1 << 8) ? 2 : 4; }

void ExitFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  if (this->lex()->closedOverP())
    emit_control_label_fixup(this->size(), this->delta(), position, code, vm_code::exit_8, vm_code::exit_16, vm_code::exit_24);
  else
    emit_control_label_fixup(this->size(), this->delta(), position, code, vm_code::jump_8, vm_code::jump_16, vm_code::jump_24);
}

size_t ExitFixup_O::resize() { return resize_control_label_fixup(this->delta()); }

void EntryCloseFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  switch (this->size()) {
  case 1:
    code[position] = (uint8_t)vm_code::entry_close;
    break;
  default:
    UNREACHABLE();
  }
}

size_t EntryCloseFixup_O::resize() { return (this->lex()->closedOverP()) ? 1 : 0; }

void Module_O::initialize_cfunction_positions() {
  size_t position = 0;
  ComplexVector_T_sp cfunctions = this->cfunctions();
  for (T_sp tfunction : *cfunctions) {
    Cfunction_sp cfunction = gc::As_assert<Cfunction_sp>(tfunction);
    cfunction->setPosition(position);
    position += cfunction->bytecode()->length();
  }
}

void Fixup_O::update_positions(size_t increase) {
  Cfunction_sp funct = gc::As_assert<Cfunction_sp>(this->cfunction());
  ComplexVector_T_sp annotations = funct->annotations();
  size_t nannot = annotations->length();
  for (size_t idx = this->iindex() + 1; idx < nannot; ++idx) {
    gc::As_assert<Annotation_sp>(annotations[idx])->_position += increase;
  }
  funct->_extra += increase;
  ComplexVector_T_sp functions = funct->module()->cfunctions();
  size_t nfuns = functions->length();
  for (size_t idx = funct->iindex() + 1; idx < nfuns; ++idx) {
    gc::As_assert<Cfunction_sp>(functions[idx])->_position += increase;
  }
}

void Module_O::resolve_fixup_sizes() {
  bool changedp;
  ComplexVector_T_sp cfunctions = this->cfunctions();
  do {
    changedp = false;
    for (T_sp tfunction : *cfunctions) {
      ComplexVector_T_sp annotations = gc::As_assert<Cfunction_sp>(tfunction)->annotations();
      for (T_sp tannot : *annotations) {
        if (gc::IsA<Fixup_sp>(tannot)) {
          Fixup_sp fixup = gc::As_unsafe<Fixup_sp>(tannot);
          size_t old_size = fixup->size();
          size_t new_size = fixup->resize();
          if (old_size != new_size) {
            ASSERT(new_size >= old_size);
            fixup->setSize(new_size);
            fixup->update_positions(new_size - old_size);
            changedp = true;
          }
        }
      }
    }
  } while (changedp);
}

size_t Module_O::bytecode_size() {
  ComplexVector_T_sp cfunctions = this->cfunctions();
  T_sp tlast_cfunction = cfunctions[cfunctions->length() - 1];
  Cfunction_sp last_cfunction = gc::As_assert<Cfunction_sp>(tlast_cfunction);
  return last_cfunction->pposition() + last_cfunction->final_size();
}

// Resolve start and end but leave the rest alone.
static void resolve_debug_info(BytecodeDebugInfo_sp info) {
  T_sp open_label = info->start();
  if (gc::IsA<Label_sp>(open_label))
    info->setStart(clasp_make_fixnum(gc::As_unsafe<Label_sp>(open_label)->module_position()));
  else // compiler screwed up, but this is just debug info, don't raise stink
    info->setStart(clasp_make_fixnum(0));
  T_sp close_label = info->end();
  if (gc::IsA<Label_sp>(close_label))
    info->setEnd(clasp_make_fixnum(gc::As_unsafe<Label_sp>(close_label)->module_position()));
  else
    info->setEnd(clasp_make_fixnum(0));
}

// Resolve the labels to fixnums, and LVInfos to frame locations.
// If a variable is stored in a cell, we indicate this by wrapping its
// frame location in a cons.
static void resolve_debug_vars(BytecodeDebugVars_sp info) {
  resolve_debug_info(info);
  for (Cons_sp cur : info->bindings()) {
    T_sp tentry = cur->car();
    if (gc::IsA<Cons_sp>(tentry)) {
      Cons_sp entry = gc::As_unsafe<Cons_sp>(tentry);
      T_sp tlvinfo = entry->cdr();
      if (gc::IsA<LexicalInfo_sp>(tlvinfo)) {
        T_sp name = entry->car();
        LexicalInfo_sp lvinfo = gc::As_unsafe<LexicalInfo_sp>(tlvinfo);
        auto bdv = BytecodeDebugVar_O::make(name, lvinfo->frameIndex(), lvinfo->indirectLexicalP(), lvinfo->decls());
        cur->setCar(bdv);
      }
    }
  }
}

static void resolve_ast_tagbody(BytecodeAstTagbody_sp info) {
  resolve_debug_info(info);
  for (Cons_sp cur : info->tags()) {
    T_sp tentry = cur->car();
    if (gc::IsA<Cons_sp>(tentry)) {
      Cons_sp centry = gc::As_unsafe<Cons_sp>(tentry);
      if (gc::IsA<Label_sp>(centry->cdr()))
        centry->setCdr(clasp_make_fixnum(gc::As_unsafe<Label_sp>(centry->cdr())->module_position()));
    }
  }
}

SimpleVector_sp Module_O::create_debug_info() {
  ComplexVector_T_sp cfunctions = this->cfunctions();
  // Add up the sizes of the cfunction debug infos to get the total size.
  // We add one to each since we put in the functions as well.
  // Replace all labels.
  size_t ndebugs = 0;
  for (T_sp tfunction : *cfunctions) {
    Cfunction_sp function = gc::As_assert<Cfunction_sp>(tfunction);
    ndebugs += 1 + function->debug_info()->length();
  }
  SimpleVector_sp debuginfos = SimpleVector_O::make(ndebugs);
  // For each cfunction, put the cfunction in the debug infos,
  // and then the cfunction's infos.
  size_t ind = 0;
  for (T_sp tfunction : *cfunctions) {
    Cfunction_sp function = gc::As_assert<Cfunction_sp>(tfunction);
    debuginfos[ind++] = function;
    for (T_sp info : *(function->debug_info())) {
      debuginfos[ind++] = info;
      // Resolve labels, etc.
      if (gc::IsA<BytecodeDebugVars_sp>(info))
        resolve_debug_vars(gc::As_unsafe<BytecodeDebugVars_sp>(info));
      else if (gc::IsA<BytecodeAstTagbody_sp>(info))
        resolve_ast_tagbody(gc::As_unsafe<BytecodeAstTagbody_sp>(info));
      else if (gc::IsA<BytecodeDebugInfo_sp>(info))
        resolve_debug_info(gc::As_unsafe<BytecodeDebugInfo_sp>(info));
    }
  }
  return debuginfos;
}

// Replacement for CL:REPLACE, which isn't available here.
static void replace_bytecode(SimpleVector_byte8_t_sp dest, ComplexVector_byte8_t_sp src, size_t start1, size_t start2,
                             size_t end2) {
  size_t index1, index2;
  for (index1 = start1, index2 = start2; index2 < end2; ++index1, ++index2) {
    dest[index1] = src[index2];
  }
}

SimpleVector_byte8_t_sp Module_O::create_bytecode() {
  SimpleVector_byte8_t_sp bytecode = SimpleVector_byte8_t_O::make(this->bytecode_size());
  size_t index = 0;
  ComplexVector_T_sp cfunctions = this->cfunctions();
  for (T_sp tfunction : *cfunctions) {
    Cfunction_sp function = gc::As_assert<Cfunction_sp>(tfunction);
    ComplexVector_byte8_t_sp cfunction_bytecode = function->bytecode();
    size_t position = 0;
    ComplexVector_T_sp annotations = function->annotations();
    for (T_sp tannot : *annotations) {
      if (gc::IsA<Fixup_sp>(tannot)) {
        Fixup_sp annotation = gc::As_unsafe<Fixup_sp>(tannot);
        if (annotation->size() != 0) {
          ASSERT(annotation->size() == annotation->resize());
          // Copy bytes in this segment.
          size_t end = annotation->initial_position();
          replace_bytecode(bytecode, cfunction_bytecode, index, position, end);
          index += end - position;
          position = end;
          ASSERT(index == annotation->module_position());
          // Emit fixup.
          annotation->emit(index, bytecode);
          position += annotation->initial_size();
          index += annotation->size();
        }
      }
    }
    // Copy any remaining bytes from this function to the module.
    size_t end = cfunction_bytecode->length();
    replace_bytecode(bytecode, cfunction_bytecode, index, position, end);
    index += end - position;
  }
  return bytecode;
}

CL_DEFUN T_sp lambda_list_for_name(T_sp raw_lambda_list) { return core::lambda_list_for_name(raw_lambda_list); }

Function_sp Cfunction_O::link_function() {
  this->module()->link_load();
  // Linking installed the GBEP in this cfunction's info. Return that.
  return this->info();
}

// For using a cfunction as a debug info (in BTB).
// These only work after the module has been linked.
CL_LISPIFY_NAME("core:bytecode-debug-info/start")
CL_DEFMETHOD T_sp Cfunction_O::start() const { return clasp_make_fixnum(this->entry_point()->module_position()); }
CL_LISPIFY_NAME("core:bytecode-debug-info/end")
CL_DEFMETHOD T_sp Cfunction_O::end() const {
  return clasp_make_fixnum(this->entry_point()->module_position() + this->final_size());
}

// Should we BTB compile this new bytecode function?
// We say yes if there's a (speed 3) declaration anywhere in it.
bool btb_bcfun_p(BytecodeSimpleFun_sp fun, SimpleVector_sp debug_info) {
  size_t start = fun->entryPcN();
  size_t end = start + fun->bytecodeSize();
  for (size_t i = 0; i < debug_info->length(); ++i) {
    T_sp tinfo = debug_info[i];
    if (gc::IsA<BytecodeAstDecls_sp>(tinfo)) {
      BytecodeAstDecls_sp info = gc::As_unsafe<BytecodeAstDecls_sp>(tinfo);
      size_t infostart = info->start().unsafe_fixnum();
      size_t infoend = info->start().unsafe_fixnum();
      if (infostart > end)
        return false; // overshot.
      if (infostart < start || infoend > end)
        continue;
      // Look for an optimize speed 3.
      for (auto cur : info->decls()) {
        T_sp decl = oCar(cur);
        if (gc::IsA<Cons_sp>(decl) && oCar(decl) == cl::_sym_optimize)
          for (auto copt : gc::As<List_sp>(oCdr(decl))) {
            T_sp opt = oCar(copt);
            if (opt == cl::_sym_speed || (gc::IsA<Cons_sp>(opt) && oCar(opt) == cl::_sym_speed && gc::IsA<Cons_sp>(oCdr(opt)) &&
                                          gc::IsA<Fixnum_sp>(oCadr(opt)) && oCadr(opt).unsafe_fixnum() == 3))
              return true;
          }
      }
    }
  }
  return false;
}

void Module_O::link() {
  Module_sp cmodule = this->asSmartPtr();
  cmodule->initialize_cfunction_positions();
  cmodule->resolve_fixup_sizes();
}

void Module_O::link_load() {
  Module_sp cmodule = this->asSmartPtr();
  cmodule->link();
  SimpleVector_byte8_t_sp bytecode = cmodule->create_bytecode();
  ComplexVector_T_sp cmodule_literals = cmodule->literals();
  size_t literal_length = cmodule_literals->length();
  SimpleVector_sp literals = SimpleVector_O::make(literal_length);
  SimpleVector_sp debug_info = cmodule->create_debug_info();
  BytecodeModule_sp bytecode_module = BytecodeModule_O::make(bytecode);
  ComplexVector_T_sp cfunctions = cmodule->cfunctions();
  // Create the real function objects.
  for (T_sp tfun : *cfunctions) {
    Cfunction_sp cfunction = gc::As_assert<Cfunction_sp>(tfun);
    T_sp sourcePathname = nil<T_O>();
    int lineno = -1;
    int column = -1;
    int filepos = -1;
    if (cfunction->sourcePosInfo().notnilp()) {
      SourcePosInfo_sp spi = gc::As_assert<SourcePosInfo_sp>(cfunction->sourcePosInfo());
      // FIXME: This is ridiculous.
      sourcePathname = gc::As_assert<FileScope_sp>(core__file_scope(clasp_make_fixnum(spi->fileHandle())))->pathname();
      lineno = spi->lineno();
      column = spi->column();
      filepos = spi->filepos();
    }
    FunctionDescription_sp fdesc = makeFunctionDescription(cfunction->nname(), cfunction->lambda_list(), cfunction->doc(),
                                                           nil<T_O>(), // declares
                                                           sourcePathname, lineno, column, filepos);
    Fixnum_sp ep = clasp_make_fixnum(cfunction->entry_point()->module_position());
    Pointer_sp trampoline = llvmo::cmp__compile_trampoline(cfunction->nname());
    BytecodeSimpleFun_sp func =
        core__makeBytecodeSimpleFun(fdesc, bytecode_module, cfunction->nlocals(), cfunction->closed()->length(), ep.unsafe_fixnum(),
                                    cfunction->final_size(), trampoline);
    cfunction->setInfo(func);
  }
  // Replace the cfunctions in the cmodule literal vector with
  // real bytecode functions in the module vector.
  // Also replace load-time-value infos with the evaluated forms,
  // and resolve cells.
  // Also also record mutable LTVs.
  ql::list mutableLTVs;
  for (size_t i = 0; i < literal_length; ++i) {
    T_sp lit = cmodule_literals[i];
    if (gc::IsA<Cfunction_sp>(lit))
      literals[i] = gc::As_unsafe<Cfunction_sp>(lit)->info();
    else if (gc::IsA<LoadTimeValueInfo_sp>(lit)) {
      LoadTimeValueInfo_sp ltvinfo = gc::As_unsafe<LoadTimeValueInfo_sp>(lit);
      literals[i] = ltvinfo->eval();
      if (!ltvinfo->read_only_p())
        mutableLTVs << Integer_O::create(i);
    } else if (gc::IsA<ConstantInfo_sp>(lit))
      literals[i] = gc::As_unsafe<ConstantInfo_sp>(lit)->value();
    else if (gc::IsA<FunctionCellInfo_sp>(lit))
      literals[i] = core__ensure_function_cell(gc::As_unsafe<FunctionCellInfo_sp>(lit)->fname());
    else if (gc::IsA<VariableCellInfo_sp>(lit))
      literals[i] = gc::As_unsafe<VariableCellInfo_sp>(lit)->vname()->ensureVariableCell();
    else if (gc::IsA<EnvInfo_sp>(lit))
      literals[i] = nil<T_O>(); // the only environment we have
    else
      SIMPLE_ERROR("BUG: Weird thing in compiler literals vector: {}", _rep_(lit));
  }
  // Also replace the cfunctions in the debug info.
  // We just modify the vector rather than cons a new one since create_debug_info
  // already created a fresh vector for us.
  for (size_t i = 0; i < debug_info->length(); ++i) {
    T_sp info = debug_info[i];
    if (gc::IsA<Cfunction_sp>(info))
      debug_info[i] = gc::As_unsafe<Cfunction_sp>(info)->info();
  }
  // Now just install the bytecode and Bob's your uncle.
  bytecode_module->setf_literals(literals);
  bytecode_module->setf_debugInfo(debug_info);
  bytecode_module->setf_mutableLiterals(mutableLTVs.cons());
  // Native-compile anything that really seems like it should be,
  // and install the resulting simple funs.
  // We can only do native compilations after the module is
  // fully realized above.
  if (_sym_STARautocompile_hookSTAR->boundP() && _sym_STARautocompile_hookSTAR->symbolValue().notnilp()) {
    for (T_sp tfun : *cfunctions) {
      Cfunction_sp cfun = gc::As_assert<Cfunction_sp>(tfun);
      BytecodeSimpleFun_sp fun = cfun->info();
      if (btb_bcfun_p(fun, debug_info)) {
        T_sp nat = eval::funcall(_sym_STARautocompile_hookSTAR->symbolValue(), fun, nil<T_O>());
        fun->setSimpleFun(gc::As_assert<SimpleFun_sp>(nat));
      }
    }
  }
}

void compile_literal(T_sp literal, Lexenv_sp env, const Context context) {
  (void)env;
  switch (context.receiving()) {
  case 0:
    return; // No value required, so do nothing
  case 1:
    if (literal.nilp())
      context.assemble0(vm_code::nil);
    else
      context.assemble1(vm_code::_const, context.literal_index(literal));
    return;
  case -1: // values
    if (literal.nilp())
      context.assemble0(vm_code::nil);
    else
      context.assemble1(vm_code::_const, context.literal_index(literal));
    context.assemble0(vm_code::pop);
    return;
  default:
    // FIXME: Just need to pad in some NILs.
    SIMPLE_ERROR("BUG: Don't know how to compile literal returning %" PFixnum " values", context.receiving());
  }
}

void compile_form(T_sp, Lexenv_sp, const Context);

static T_sp expand_macro(Function_sp expander, T_sp form, Lexenv_sp env) {
  // This is copied from cl__macroexpand. I guess eval::funcall doesn't do the
  // coercion itself?
  T_sp macroexpandHook = cl::_sym_STARmacroexpand_hookSTAR->symbolValue();
  Function_sp hook = coerce::calledFunctionDesignator(macroexpandHook);
  return eval::funcall(hook, expander, form, env);
}

// Redefined in compiler-conditions.lisp.
SYMBOL_EXPORT_SC_(CompPkg, expand_compiler_macro_safely);
CL_DEFUN T_sp cmp__expand_compiler_macro_safely(Function_sp expander, T_sp form, Lexenv_sp env, T_sp source_info) {
  (void)source_info;
  return expand_macro(expander, form, env);
}

static T_sp expand_compiler_macro(Function_sp expander, T_sp form, Lexenv_sp env, T_sp source_info) {
  // Go through symbolFunction to make it sensitive to redefinition.
  // Also, slower! Too bad.
  return eval::funcall(_sym_expand_compiler_macro_safely->symbolFunction(), expander, form, env, source_info);
}

SYMBOL_EXPORT_SC_(CompPkg, warn_undefined_global_variable);

inline static bool code_walking_p() {
  return _sym_STARcodeWalkerSTAR->boundP() && _sym_STARcodeWalkerSTAR->symbolValue().notnilp();
}

SYMBOL_EXPORT_SC_(CompPkg, warn_used_ignored_variable);
CL_DEFUN void cmp__warn_used_ignored_variable(T_sp name, T_sp sourceloc) {
  (void)name;
  (void)sourceloc;
}

// Function is called whenever a lexical is referenced, to issue a
// warning with appropriate source location.
static void maybe_warn_used(T_sp name, LexicalInfo_sp lex, T_sp sloc, bool funp) {
  if (lex->ignore() == LexicalInfo_O::IgnoreStatus::IGNORE) {
    T_sp rname = funp ? Cons_O::createList(cl::_sym_Function_O, name) : name;
    eval::funcall(_sym_warn_used_ignored_variable, rname, sloc);
  }
}

void compile_symbol(Symbol_sp sym, Lexenv_sp env, const Context context) {
  VarInfoV info = var_info_v(sym, env);
  if (std::holds_alternative<SymbolMacroVarInfoV>(info)) {
    Function_sp expander = std::get<SymbolMacroVarInfoV>(info).expander();
    T_sp expansion = expand_macro(expander, sym, env);
    compile_form(expansion, env, context);
    return;
  } else if (context.receiving() == 0) {
    // A symbol macro could expand into something with arbitrary side effects
    // so we always have to compile that, but otherwise, if no values are
    // wanted, we want to not compile anything.
    // But we do want to note any lexical variable as used.
    if (std::holds_alternative<LexicalVarInfoV>(info)) {
      LexicalInfo_sp lex = std::get<LexicalVarInfoV>(info).info()->lex();
      lex->setReadP(true);
      maybe_warn_used(sym, lex, context.source_info(), false);
    }
    return;
  } else {
    if (std::holds_alternative<LexicalVarInfoV>(info)) {
      LexicalVarInfo_sp lvinfo = std::get<LexicalVarInfoV>(info).info();
      if (lvinfo->funct() == context.cfunction())
        // Local variable, just read it.
        context.assemble1(vm_code::ref, lvinfo->frameIndex());
      else { // closed over
        lvinfo->setClosedOverP(true);
        context.assemble1(vm_code::closure, context.closure_index(lvinfo->lex()));
      }
      context.maybe_emit_cell_ref(lvinfo);
      lvinfo->lex()->setReadP(true);
      maybe_warn_used(sym, lvinfo->lex(), context.source_info(), false);
    } else if (std::holds_alternative<SpecialVarInfoV>(info))
      context.assemble1(vm_code::symbol_value, context.vcell_index(sym));
    else if (std::holds_alternative<ConstantVarInfoV>(info)) {
      compile_literal(std::get<ConstantVarInfoV>(info).value(), env, context);
      // Avoid the pop code below - compile-literal handles it.
      return;
    } else if (std::holds_alternative<NoVarInfoV>(info)) {
      if (_sym_warn_undefined_global_variable->fboundp() && !code_walking_p())
        eval::funcall(_sym_warn_undefined_global_variable, context.source_info(), sym);
      context.assemble1(vm_code::symbol_value, context.vcell_index(sym));
    }
    if (context.receiving() == -1)
      // Values return - put value in mv vector.
      context.assemble0(vm_code::pop);
  }
}

void compile_progn(List_sp forms, Lexenv_sp env, const Context ctxt) {
  if (forms.nilp())
    compile_literal(nil<T_O>(), env, ctxt);
  else
    for (auto cur : forms) {
      if (oCdr(cur).notnilp()) // compile for effect
        compile_form(oCar(cur), env, ctxt.sub_receiving(0));
      else // compile for value
        compile_form(oCar(cur), env, ctxt);
    }
}

void compile_locally(List_sp body, Lexenv_sp env, const Context ctxt) {
  List_sp declares = nil<T_O>();
  gc::Nilable<String_sp> docstring;
  List_sp code;
  List_sp specials;
  eval::extract_declares_docstring_code_specials(body, declares, false, docstring, code, specials);
  declares = scrub_decls(declares);
  env = env->add_specials(specials)->add_decls(declares);
  Label_sp begin_label = Label_O::make(), end_label = Label_O::make();
  if (declares.notnilp()) {
    begin_label->contextualize(ctxt);
    ctxt.push_debug_info(BytecodeAstDecls_O::make(begin_label, end_label, declares));
  }
  compile_progn(code, env, ctxt);
  if (declares.notnilp())
    end_label->contextualize(ctxt);
}

bool special_binding_p(Symbol_sp sym, List_sp specials, Lexenv_sp env) {
  if (specials.notnilp() && specials.unsafe_cons()->memberEq(sym).notnilp())
    return true;
  else {
    VarInfoV info = var_info_v(sym, env);
    if (std::holds_alternative<SpecialVarInfoV>(info))
      return std::get<SpecialVarInfoV>(info).globalp();
    else
      return false;
  }
}

// From a list of declarations, determine the ignore status of the variable
// or #'function. The latter is why we use cl:equal.
LexicalInfo_O::IgnoreStatus binding_ignore(T_sp name, List_sp decls) {
  for (auto cur : decls) {
    T_sp decl = oCar(cur);
    if (!gc::IsA<Cons_sp>(decl) || !gc::IsA<Cons_sp>(oCdr(decl)))
      continue;
    if (oCar(decl) == cl::_sym_ignore) {
      for (auto cv : gc::As_unsafe<List_sp>(oCdr(decl))) {
        if (cl__equal(oCar(cv), name))
          return LexicalInfo_O::IgnoreStatus::IGNORE;
      }
    } else if (oCar(decl) == cl::_sym_ignorable) {
      for (auto cv : gc::As_unsafe<List_sp>(oCdr(decl))) {
        if (cl__equal(oCar(cv), name))
          return LexicalInfo_O::IgnoreStatus::IGNORABLE;
      }
    }
  }
  return LexicalInfo_O::IgnoreStatus::NOIGNORE; // ain't nothin
}

// These will be redefined in compiler-conditions.lisp,
// once the condition system is really up.
SYMBOL_EXPORT_SC_(CompPkg, warn_unused_variable);
SYMBOL_EXPORT_SC_(CompPkg, warn_set_unused_variable);
CL_DEFUN void cmp__warn_unused_variable(T_sp name, T_sp sourceloc) {
  (void)name;
  (void)sourceloc;
}
CL_DEFUN void cmp__warn_set_unused_variable(T_sp name, T_sp sourceloc) {
  (void)name;
  (void)sourceloc;
}

// Emit warnings for unused variables etc.
// Bindings is an alist of (name . lexical-info).
static void warn_ignorance(List_sp bindings) {
  if (code_walking_p())
    return;
  for (auto cur : bindings) {
    LexicalInfo_sp lex = gc::As_assert<LexicalInfo_sp>(oCadar(cur));
    if (lex->ignore() == LexicalInfo_O::IgnoreStatus::NOIGNORE && !lex->readP()) {
      T_sp name = oCaar(cur);
      T_sp sloc = oCaddar(cur);
      if (lex->setP())
        eval::funcall(_sym_warn_set_unused_variable, name, sloc);
      else
        eval::funcall(_sym_warn_unused_variable, name, sloc);
    }
  }
}

static T_sp source_location_for(T_sp form, T_sp fallback) {
  if (_sym_STARsourceLocationsSTAR->boundP()) {
    T_sp table = _sym_STARsourceLocationsSTAR->symbolValue();
    if (gc::IsA<HashTableBase_sp>(table))
      return gc::As_unsafe<HashTableBase_sp>(table)->gethash(form, fallback);
  }
  return fallback;
}

static List_sp decls_for_var(T_sp varname, List_sp decls) {
  // FIXME: Should be extensible.
  // Also ideally in Lisp. This is pretty grody as-is.
  ql::list result;
  for (auto cur : decls) {
    T_sp decl = oCar(cur);
    if (gc::IsA<Cons_sp>(decl) && gc::IsA<Cons_sp>(oCdr(decl))) {
      T_sp spec = oCar(decl);
      Cons_sp rest = gc::As_unsafe<Cons_sp>(oCdr(decl));
      // SPECIAL declarations are ignored, since we handle that
      // directly rather than needing to put it in annotations.
      if (spec == cl::_sym_dynamic_extent || spec == cl::_sym_ignore || spec == cl::_sym_ignorable) {
        if (rest->memberEq(varname).notnilp())
          result << spec;
      } else if (spec == cl::_sym_type && gc::IsA<Cons_sp>(oCdr(rest))) {
        Cons_sp names = gc::As_unsafe<Cons_sp>(oCdr(rest));
        if (names->memberEq(varname).notnilp())
          result << Cons_O::createList(spec, oCar(rest));
      } else if (!(spec == cl::_sym_ftype || spec == cl::_sym_inline || spec == cl::_sym_notinline || spec == cl::_sym_optimize ||
                   spec == cl::_sym_special || spec == core::_sym_lambdaName || spec == core::_sym_lambdaList)) {
        // Presumed type decl
        if (rest->memberEq(varname).notnilp())
          result << Cons_O::createList(cl::_sym_type, spec);
      }
    }
  }
  return result.cons();
}

static List_sp decls_for_fun(T_sp funname, List_sp decls) {
  // FIXME: Should be extensible.
  // Also ideally in Lisp. This is pretty grody as-is.
  ql::list result;
  for (auto cur : decls) {
    T_sp decl = oCar(cur);
    if (gc::IsA<Cons_sp>(decl) && gc::IsA<Cons_sp>(oCdr(decl))) {
      T_sp spec = oCar(decl);
      Cons_sp rest = gc::As_unsafe<Cons_sp>(oCdr(decl));
      if (spec == cl::_sym_dynamic_extent || spec == cl::_sym_ignore || spec == cl::_sym_ignorable) {
        for (auto ncur : (List_sp)rest) {
          T_sp ncurn = oCar(ncur);
          if (gc::IsA<Cons_sp>(ncurn) && oCar(ncurn) == cl::_sym_Function_O && oCadr(ncurn) == funname && oCddr(ncurn).nilp()) {
            result << spec;
            break;
          }
        }
      } else if (spec == cl::_sym_inline || spec == cl::_sym_notinline) {
        if (rest->memberEq(funname).notnilp())
          result << spec;
      } else if (spec == cl::_sym_ftype && gc::IsA<Cons_sp>(oCdr(rest))) {
        Cons_sp names = gc::As_unsafe<Cons_sp>(oCdr(rest));
        if (names->memberEq(funname).notnilp())
          result << Cons_O::createList(spec, oCar(rest));
      }
    }
  }
  return result.cons();
}

void compile_let(List_sp bindings, List_sp body, Lexenv_sp env, const Context ctxt) {
  List_sp declares = nil<T_O>();
  gc::Nilable<String_sp> docstring;
  List_sp code;
  List_sp specials;
  eval::extract_declares_docstring_code_specials(body, declares, false, docstring, code, specials);
  size_t lexical_binding_count = 0;
  size_t special_binding_count = 0;
  Lexenv_sp post_binding_env = env->add_specials(specials);
  // debug info
  Label_sp begin_label = Label_O::make();
  Label_sp end_label = Label_O::make();
  ql::list debug_bindings; // alist (name . LexicalInfo)
  ql::list ibindings;      // (name lex source). FIXME merge w/ above.
  // now get processing
  for (auto cur : bindings) {
    T_sp binding = oCar(cur);
    Symbol_sp var;
    T_sp valf;
    if (binding.consp()) {
      var = gc::As<Symbol_sp>(oCar(binding));
      valf = oCadr(binding);
    } else {
      var = gc::As<Symbol_sp>(binding);
      valf = nil<T_O>();
    }
    compile_form(valf, env, ctxt.sub_receiving(1));
    if (special_binding_p(var, specials, env)) {
      ++special_binding_count;
      ctxt.emit_special_bind(var);
    } else {
      // FIXME: We don't need to cons actual lexenvs here.
      post_binding_env = post_binding_env->bind1var(var, ctxt);
      ++lexical_binding_count;
      LexicalVarInfo_sp lvinfo = gc::As_assert<LexicalVarInfo_sp>(post_binding_env->variableInfo(var));
      debug_bindings << Cons_O::create(var, lvinfo->lex());
      ibindings << Cons_O::createList(var, lvinfo->lex(), source_location_for(binding, ctxt.source_info()));
      lvinfo->lex()->setIgnore(binding_ignore(var, declares));
      lvinfo->lex()->setDecls(decls_for_var(var, declares));
      ctxt.maybe_emit_make_cell(lvinfo);
    }
  }
  ctxt.emit_bind(lexical_binding_count, env->frameEnd());
  post_binding_env = post_binding_env->add_decls(declares);
  begin_label->contextualize(ctxt);
  // Output debug info before the progn to ensure sorting.
  T_sp dbindings = debug_bindings.cons();
  if (dbindings.notnilp())
    ctxt.push_debug_info(BytecodeDebugVars_O::make(begin_label, end_label, dbindings));
  if (declares.notnilp())
    ctxt.push_debug_info(BytecodeAstDecls_O::make(begin_label, end_label, declares));
  compile_progn(code, post_binding_env, ctxt.sub_de(Integer_O::create(special_binding_count)));
  ctxt.emit_unbind(special_binding_count);
  end_label->contextualize(ctxt);
  // Warn about unused variables.
  warn_ignorance(ibindings.cons());
}

void compile_letSTAR(List_sp bindings, List_sp body, Lexenv_sp env, const Context ectxt) {
  List_sp declares = nil<T_O>();
  gc::Nilable<String_sp> docstring;
  List_sp code;
  List_sp specials;
  eval::extract_declares_docstring_code_specials(body, declares, false, docstring, code, specials);
  size_t special_binding_count = 0;
  Lexenv_sp new_env = env;
  Context ctxt = ectxt;
  Label_sp end_label = Label_O::make();
  ql::list debug_bindings;
  ql::list ibindings;
  for (auto cur : bindings) {
    T_sp binding = oCar(cur);
    Symbol_sp var;
    T_sp valf;
    if (binding.consp()) {
      var = gc::As<Symbol_sp>(oCar(binding));
      valf = oCadr(binding);
    } else {
      var = gc::As<Symbol_sp>(binding);
      valf = nil<T_O>();
    }
    compile_form(valf, new_env, ctxt.sub_receiving(1));
    if (special_binding_p(var, specials, env)) {
      ++special_binding_count;
      new_env = new_env->add_specials(Cons_O::createList(var));
      ctxt.emit_special_bind(var);
      ctxt = ctxt.sub_de(Integer_O::create(1));
    } else {
      Label_sp begin_label = Label_O::make();
      size_t frame_start = new_env->frameEnd();
      new_env = new_env->bind1var(var, ctxt);
      LexicalVarInfo_sp lvinfo = gc::As_assert<LexicalVarInfo_sp>(new_env->variableInfo(var));
      ctxt.maybe_emit_make_cell(lvinfo);
      ctxt.assemble1(vm_code::set, frame_start);
      lvinfo->lex()->setIgnore(binding_ignore(var, declares));
      lvinfo->lex()->setDecls(decls_for_var(var, declares));
      // Set up debug info
      begin_label->contextualize(ctxt);
      Cons_sp dpair = Cons_O::create(var, lvinfo->lex());
      ctxt.push_debug_info(BytecodeDebugVars_O::make(begin_label, end_label, Cons_O::createList(dpair)));
      debug_bindings << dpair;
      ibindings << Cons_O::createList(var, lvinfo->lex(), source_location_for(binding, ctxt.source_info()));
    }
  }
  new_env = new_env->add_decls(declares);
  // We make a new environment to make sure free special declarations get
  // through even if this let* doesn't bind them.
  // This creates duplicate alist entries for anything that _is_ bound
  // here, but that's not a big deal.
  compile_progn(code, new_env->add_specials(specials), ctxt);
  ctxt.emit_unbind(special_binding_count);
  end_label->contextualize(ctxt);
  warn_ignorance(ibindings.cons());
}

// copied from evaluator.cc (on the premise that that will be deleted or
// heavily rewritten once the bytecode compiler is ported)
static T_sp extract_lambda_name_from_declares(List_sp declares) {
  for (auto cur : declares) {
    List_sp decl = gc::As<List_sp>(oCar(cur));
    if (oCar(decl) == core::_sym_lambdaName)
      return oCadr(decl);
  }
  return nil<T_O>();
}

static T_sp extract_lambda_list_from_declares(List_sp declares, T_sp defaultll) {
  for (auto cur : declares) {
    List_sp decl = gc::As<List_sp>(oCar(cur));
    if (oCar(decl) == core::_sym_lambdaList)
      return oCdr(decl);
  }
  return defaultll;
}

Lexenv_sp compile_optional_or_key_item(Symbol_sp var, T_sp defaulting_form, LexicalVarInfo_sp varinfo, Symbol_sp supplied_var,
                                       bool var_specialp, bool supplied_specialp, const Context context, Lexenv_sp env) {
  Label_sp supplied_label = Label_O::make();
  Label_sp next_label = Label_O::make();
  context.emit_jump_if_supplied(supplied_label, varinfo->frameIndex());
  // Emit an annotation for the if.
  Label_sp else_label = Label_O::make();
  else_label->contextualize(context);
  context.push_debug_info(BytecodeAstIf_O::make(else_label, next_label, supplied_var.notnilp() ? 2 : 1));
  // Emit code for the case of the variable not being supplied:
  // Bind the var to the default, and the suppliedvar to NIL if applicable.
  // We push the suppliedp value first because it's bound second.
  if (supplied_var.notnilp())
    context.assemble0(vm_code::nil);
  compile_form(defaulting_form, env, context.sub_receiving(1));
  // And actually set the variable, if we're lexical.
  context.emit_jump(next_label);
  // Set up the new environment. Make sure this is AFTER compiling the default form,
  // as the default form does not have the variable or suppliedp bound.
  if (var_specialp)
    env = env->add_specials(Cons_O::createList(var));
  else
    // import the existing info.
    env = env->sub_vars(Cons_O::create(Cons_O::create(var, varinfo), env->vars()), env->frameEnd());
  if (supplied_var.notnilp()) {
    if (supplied_specialp)
      env = env->add_specials(Cons_O::createList(supplied_var));
    else
      env = env->bind1var(supplied_var, context);
  }
  // Now for when the variable is supplied.
  supplied_label->contextualize(context);
  if (supplied_var.notnilp())
    compile_literal(cl::_sym_T_O, env, context.sub_receiving(1));
  context.assemble1(vm_code::ref, varinfo->frameIndex());
  next_label->contextualize(context);
  // Bind the main variable if it's special.
  // We emit this one special bind after the branch for the same reason as with the
  // suppliedp var below. (In the lexical case it's considered bound by the bind-optional.)
  if (var_specialp)
    context.emit_special_bind(var);
  else {
    context.maybe_emit_make_cell(varinfo);
    context.assemble1(vm_code::set, varinfo->frameIndex());
  }
  // The suppliedp value was pushed most recently, so bind that first.
  // We do it this way after the branch so that the suppliedp var has a dominating bind
  // instruction, which is nice for verification and further compilation.
  if (supplied_var.notnilp()) {
    if (supplied_specialp)
      context.emit_special_bind(supplied_var);
    else {
      LexicalVarInfo_sp lsinfo = gc::As_assert<LexicalVarInfo_sp>(env->variableInfo(supplied_var));
      context.maybe_emit_make_cell(lsinfo);
      // This is a separate set because the variable and its optionalp usually
      // don't have contiguous indices.
      context.assemble1(vm_code::set, lsinfo->frameIndex());
    }
  }
  // That's it for code generation. Now return the new environment.
  return env;
}

// Generate BytecodeDebug whatsits for optional/key variables.
// Also set the ignore and other declarations.
void annotate_optional_or_key_item(Symbol_sp key_var, Symbol_sp supplied_var, List_sp decls, Label_sp end, const Context ctxt,
                                   Lexenv_sp env) {
  ql::list dvars;
  VarInfoV kinfo = var_info_v(key_var, env);
  if (std::holds_alternative<LexicalVarInfoV>(kinfo)) {
    LexicalVarInfo_sp lvinfo = std::get<LexicalVarInfoV>(kinfo).info();
    LexicalInfo_sp lex = lvinfo->lex();
    lex->setIgnore(binding_ignore(key_var, decls));
    lex->setDecls(decls_for_var(key_var, decls));
    dvars << Cons_O::create(key_var, lex);
  }
  if (supplied_var.notnilp()) {
    VarInfoV sinfo = var_info_v(supplied_var, env);
    if (std::holds_alternative<LexicalVarInfoV>(sinfo)) {
      LexicalVarInfo_sp lvinfo = std::get<LexicalVarInfoV>(sinfo).info();
      LexicalInfo_sp lex = lvinfo->lex();
      lex->setIgnore(binding_ignore(supplied_var, decls));
      lex->setDecls(decls_for_var(supplied_var, decls));
      dvars << Cons_O::create(supplied_var, lex);
    }
  }
  T_sp dbinds = dvars.cons();
  if (dbinds.notnilp()) {
    Label_sp dlabel = Label_O::make();
    dlabel->contextualize(ctxt);
    ctxt.push_debug_info(BytecodeDebugVars_O::make(dlabel, end, dbinds));
  }
}

void compile_with_lambda_list(T_sp lambda_list, List_sp body, Lexenv_sp env, const Context context) {
  List_sp declares = nil<T_O>();
  gc::Nilable<String_sp> docstring;
  List_sp code;
  List_sp specials;
  eval::extract_declares_docstring_code_specials(body, declares, true, docstring, code, specials);
  // docstring and declares ignored
  gctools::Vec0<RequiredArgument> reqs;
  gctools::Vec0<OptionalArgument> optionals;
  gctools::Vec0<KeywordArgument> keys;
  gctools::Vec0<AuxArgument> auxs;
  RestArgument restarg;
  T_sp key_flag;
  T_sp aokp;
  parse_lambda_list(lambda_list, cl::_sym_Function_O, reqs, optionals, restarg, key_flag, keys, aokp, auxs);
  Cfunction_sp function = context.cfunction();
  Label_sp entry_point = function->entry_point();
  size_t min_count = reqs.size();
  size_t optional_count = optionals.size();
  size_t max_count = min_count + optional_count;
  bool morep = restarg._ArgTarget.notnilp() || key_flag.notnilp();
  Label_sp end_label = Label_O::make(); // for debug info
  ql::list ibindings;                   // for ignore. &optional/&key not included FIXME
  ql::list lreqs;
  for (auto& it : reqs)
    lreqs << it._ArgTarget;
  Lexenv_sp new_env = env->bind_vars(lreqs.cons(), context);
  // This environment is only used for assigning indices to opt/key variables.
  size_t special_binding_count = 0;

  entry_point->contextualize(context);
  // Generate argument count check.
  if ((min_count > 0) && (min_count == max_count) && !morep)
    context.assemble1(vm_code::check_arg_count_EQ, min_count);
  else {
    if (min_count > 0)
      context.assemble1(vm_code::check_arg_count_GE, min_count);
    if (!morep)
      context.assemble1(vm_code::check_arg_count_LE, max_count);
  }
  if (min_count > 0) {
    Label_sp begin_label = Label_O::make();
    // Bind the required arguments.
    context.assemble1(vm_code::bind_required_args, min_count);
    ql::list debugbindings;
    ql::list debugdecls;
    ql::list sreqs; // required parameters that are special
    for (auto& it : reqs) {
      // We account for special declarations in outer environments/globally
      // by checking the original environment - not our new one - for info.
      T_sp var = it._ArgTarget;
      auto lvinfo = gc::As_assert<LexicalVarInfo_sp>(new_env->variableInfo(var));
      if (special_binding_p(var, specials, env)) {
        sreqs << var;
        context.assemble1(vm_code::ref, lvinfo->frameIndex());
        context.emit_special_bind(var);
        ++special_binding_count; // not in lisp - bug?
      } else {
        context.maybe_emit_encage(lvinfo);
        T_sp dpair = Cons_O::create(var, lvinfo->lex());
        debugbindings << dpair;
        lvinfo->lex()->setIgnore(binding_ignore(var, declares));
        lvinfo->lex()->setDecls(decls_for_var(var, declares));
        ibindings << Cons_O::createList(var, lvinfo->lex(), context.source_info());
      }
    }
    new_env = new_env->add_specials(sreqs.cons());
    begin_label->contextualize(context); // after encages
    context.push_debug_info(BytecodeDebugVars_O::make(begin_label, end_label, debugbindings.cons()));
  }
  Lexenv_sp optkey_env = new_env;
  if (optional_count > 0) {
    // Generate code to bind the provided optional args, unprovided args will
    // be initialized with the unbound marker.
    context.assemble2(vm_code::bind_optional_args, min_count, optional_count);
    // Mark the locations of each optional. Note that we do this even if
    // the variable will be specially bound, to match the placement by
    // bind_optional_args.
    ql::list opts;
    for (auto& it : optionals)
      opts << it._ArgTarget;
    List_sp lopts = opts.cons();
    optkey_env = optkey_env->bind_vars(lopts, context);
    // new_env has enough space for the optional arguments, but without the
    // variables actually bound, so that default forms can be compiled correctly
    new_env = Lexenv_O::make(new_env->vars(), optkey_env->tags(), optkey_env->blocks(), optkey_env->funs(), optkey_env->decls(),
                             optkey_env->frameEnd());
  }
  // &rest
  if (restarg._ArgTarget.notnilp()) {
    Symbol_sp rest = restarg._ArgTarget;
    bool varestp = restarg.VaRest;
    if (varestp) {
      context.assemble1(vm_code::vaslistify_rest_args, max_count);
    } else {
      context.assemble1(vm_code::listify_rest_args, max_count);
    }
    new_env = new_env->bind1var(rest, context);
    optkey_env = optkey_env->bind1var(rest, context);
    auto lvinfo = gc::As_assert<LexicalVarInfo_sp>(new_env->variableInfo(rest));
    if (special_binding_p(rest, specials, env)) {
      context.assemble1(vm_code::ref, lvinfo->frameIndex());
      context.emit_special_bind(rest);
      ++special_binding_count;
      new_env = new_env->add_specials(Cons_O::createList(rest));
    } else {
      context.maybe_emit_encage(lvinfo);
      Label_sp begin_label = Label_O::make();
      begin_label->contextualize(context);
      T_sp dpair = Cons_O::create(rest, lvinfo->lex());
      context.push_debug_info(BytecodeDebugVars_O::make(begin_label, end_label, Cons_O::createList(dpair)));
      lvinfo->lex()->setIgnore(binding_ignore(rest, declares));
      lvinfo->lex()->setDecls(decls_for_var(rest, declares));
      ibindings << Cons_O::createList(rest, lvinfo->lex(), context.source_info());
    }
  }
  if (key_flag.notnilp()) {
    // Generate code to parse the key args. As with optionals, we don't do
    // defaulting yet.
    ql::list keynames;
    // Give each key a literal index. This is always a new one, to ensure that
    // they are consecutive even if the keyword appeared earlier in the literals.
    size_t first_key_index = 0;
    bool set_first_key_index = false;
    for (auto& it : keys) {
      if (!set_first_key_index) {
        first_key_index = context.new_literal_index(it._Keyword);
        set_first_key_index = true;
      } else
        context.new_literal_index(it._Keyword);
    }
    // now the actual instruction
    context.emit_parse_key_args(max_count, keys.size(), first_key_index, optkey_env->frameEnd(), aokp.notnilp());
    ql::list keyvars;
    for (auto& it : keys)
      keyvars << it._ArgTarget;
    List_sp lkeyvars = keyvars.cons();
    optkey_env = optkey_env->bind_vars(lkeyvars, context);
    new_env = Lexenv_O::make(new_env->vars(), optkey_env->tags(), optkey_env->blocks(), optkey_env->funs(), optkey_env->decls(),
                             optkey_env->frameEnd());
  }
  // Generate defaulting code for optional args, and bind them properly.
  if (optional_count > 0) {
    for (auto& it : optionals) {
      T_sp optional_var = it._ArgTarget;
      T_sp defaulting_form = it._Default;
      T_sp supplied_var = it._Sensor._ArgTarget;
      bool optional_special_p = special_binding_p(optional_var, specials, env);
      auto varinfo = gc::As_assert<LexicalVarInfo_sp>(var_info(optional_var, optkey_env));
      bool supplied_special_p = supplied_var.notnilp() && special_binding_p(supplied_var, specials, env);
      new_env = compile_optional_or_key_item(optional_var, defaulting_form, varinfo, supplied_var, optional_special_p,
                                             supplied_special_p, context, new_env);
      annotate_optional_or_key_item(optional_var, supplied_var, declares, end_label, context, new_env);
      if (optional_special_p)
        ++special_binding_count;
      else
        ibindings << Cons_O::createList(optional_var, varinfo->lex(), context.source_info());
      if (supplied_special_p)
        ++special_binding_count;
      else if (supplied_var.notnilp()) {
        T_sp lexvarinfo = var_info(supplied_var, new_env);
        ibindings << Cons_O::createList(supplied_var, gc::As_assert<LexicalVarInfo_sp>(lexvarinfo)->lex(), context.source_info());
      }
    }
  }
  // Generate defaulting code for key args, and special-bind them if necessary
  if (key_flag.notnilp()) {
    for (auto& it : keys) {
      T_sp key_var = it._ArgTarget;
      T_sp defaulting_form = it._Default;
      T_sp supplied_var = it._Sensor._ArgTarget;
      bool key_special_p = special_binding_p(key_var, specials, env);
      auto varinfo = gc::As_assert<LexicalVarInfo_sp>(var_info(key_var, optkey_env));
      bool supplied_special_p = supplied_var.notnilp() && special_binding_p(supplied_var, specials, env);
      new_env = compile_optional_or_key_item(key_var, defaulting_form, varinfo, supplied_var, key_special_p, supplied_special_p,
                                             context, new_env);
      annotate_optional_or_key_item(key_var, supplied_var, declares, end_label, context, new_env);
      if (key_special_p)
        ++special_binding_count;
      else
        ibindings << Cons_O::createList(key_var, varinfo->lex(), context.source_info());
      if (supplied_special_p)
        ++special_binding_count;
      else if (supplied_var.notnilp())
        ibindings << Cons_O::createList(supplied_var, gc::As_assert<LexicalVarInfo_sp>(var_info(supplied_var, new_env))->lex(),
                                        context.source_info());
    }
  }
  // Generate aux and the body as a let*.
  // We repeat the declarations so that let* will know the auxs are
  // special, and so that any free special declarations are processed.
  // Similarly for notinline and other free declarations.
  ql::list auxbinds;
  for (auto& it : auxs)
    auxbinds << Cons_O::createList(it._ArgTarget, it._Expression);
  T_sp declexpr = Cons_O::create(cl::_sym_declare, declares);
  T_sp lbody = Cons_O::create(declexpr, code);
  compile_letSTAR(auxbinds.cons(), lbody, new_env, context);
  // Finally, clean up any special bindings.
  context.emit_unbind(special_binding_count);
  end_label->contextualize(context);
  warn_ignorance(ibindings.cons());
}

// Compile the lambda expression in MODULE, returning the resulting CFUNCTION.
CL_DEFUN Cfunction_sp compile_lambda(T_sp lambda_list, List_sp body, Lexenv_sp env, Module_sp module, T_sp source_info) {
  List_sp declares = nil<T_O>();
  Label_sp begin = Label_O::make(), end = Label_O::make();
  gc::Nilable<String_sp> docstring;
  List_sp code;
  List_sp specials;
  eval::extract_declares_docstring_code_specials(body, declares, true, docstring, code, specials);
  List_sp all_declares = Cons_O::append(declares, env->decls());
  // Get a declared debug display lambda list if it exists.
  // If not declared, use the actual lambda list.
  // (This is useful for e.g. macros.)
  T_sp oll = extract_lambda_list_from_declares(declares, lambda_list);
  // Get a declared debug display name if it exists.
  // If it doesn't, use (lambda lambda-list...)
  T_sp name = extract_lambda_name_from_declares(declares);
  if (name.nilp())
    name = Cons_O::createList(cl::_sym_lambda, comp::lambda_list_for_name(oll));
  Cfunction_sp function = Cfunction_O::make(module, name, docstring, oll, source_info);
  Context context(-1, nil<T_O>(), function, source_info);
  Lexenv_sp lenv = Lexenv_O::make(env->vars(), env->tags(), env->blocks(), env->funs(), env->decls(), 0);
  Fixnum_sp ind = module->cfunctions()->vectorPushExtend(function);
  function->setIndex(ind.unsafe_fixnum());
  if (all_declares.notnilp() || source_info.notnilp()) {
    begin->contextualize(context);
    if (all_declares.notnilp())
      context.push_debug_info(BytecodeAstDecls_O::make(begin, end, all_declares));
    if (source_info.notnilp())
      context.push_debug_info(BytecodeDebugLocation_O::make(begin, end, source_info));
  }
  // We pass the original body w/declarations to compile-with-lambda-list
  // so that it can do its own handling of specials, etc.
  compile_with_lambda_list(lambda_list, body, lenv, context);
  context.assemble0(vm_code::_return);
  if (all_declares.notnilp() || source_info.notnilp())
    end->contextualize(context);
  return function;
}

SYMBOL_EXPORT_SC_(CompPkg, register_global_function_ref);

void compile_function(T_sp fnameoid, Lexenv_sp env, const Context ctxt) {
  bool mvp;
  switch (ctxt.receiving()) {
  case -1:
    mvp = true;
    break;
  case 0:
    return;
  default:
    mvp = false;
    break;
  }
  if (gc::IsA<Cons_sp>(fnameoid) && oCar(fnameoid) == cl::_sym_lambda) {
    Cfunction_sp fun =
        compile_lambda(oCadr(fnameoid), oCddr(fnameoid), env, ctxt.module(), source_location_for(fnameoid, ctxt.source_info()));
    ComplexVector_T_sp closed = fun->closed();
    for (size_t i = 0; i < closed->length(); ++i) {
      ctxt.reference_lexical_info(gc::As_assert<LexicalInfo_sp>(closed[i]));
    }
    if (closed->length() == 0) // don't need to actually close
      ctxt.assemble1(vm_code::_const, ctxt.cfunction_index(fun));
    else
      ctxt.assemble1(vm_code::make_closure, ctxt.cfunction_index(fun));
  } else { // ought to be a function name
    FunInfoV info = fun_info_v(fnameoid, env);
    if (std::holds_alternative<GlobalFunInfoV>(info) || std::holds_alternative<NoFunInfoV>(info)) {
      if (std::holds_alternative<NoFunInfoV>(info) // Warn
          && _sym_register_global_function_ref->fboundp() && !code_walking_p())
        eval::funcall(_sym_register_global_function_ref, fnameoid, ctxt.source_info());
      ctxt.assemble1(vm_code::fdefinition, ctxt.fcell_index(fnameoid));
    } else if (std::holds_alternative<LocalFunInfoV>(info)) {
      LocalFunInfo_sp lfinfo = std::get<LocalFunInfoV>(info).info();
      lfinfo->lex()->setReadP(true);
      maybe_warn_used(fnameoid, lfinfo->lex(), ctxt.source_info(), true);
      ctxt.reference_lexical_info(lfinfo->lex());
    } else
      // FIXME: e.g. #'with-open-file. needs better error.
      SIMPLE_ERROR("{} does not name a function", _rep_(fnameoid));
  }
  // Coerce to values if necessary.
  if (mvp)
    ctxt.assemble0(vm_code::pop);
}

// Compile a function designator knowing that it will be immediately
// called. We ignore ctxt's actual receiving and return one value,
// and we can skip some runtime checks.
void compile_called_function(T_sp fnameoid, Lexenv_sp env, const Context ctxt) {
  if (gc::IsA<Cons_sp>(fnameoid) && oCar(fnameoid) == cl::_sym_lambda) {
    Cfunction_sp fun =
        compile_lambda(oCadr(fnameoid), oCddr(fnameoid), env, ctxt.module(), source_location_for(fnameoid, ctxt.source_info()));
    ComplexVector_T_sp closed = fun->closed();
    for (size_t i = 0; i < closed->length(); ++i) {
      ctxt.reference_lexical_info(gc::As_assert<LexicalInfo_sp>(closed[i]));
    }
    if (closed->length() == 0) // don't need to actually close
      ctxt.assemble1(vm_code::_const, ctxt.cfunction_index(fun));
    else
      ctxt.assemble1(vm_code::make_closure, ctxt.cfunction_index(fun));
  } else { // ought to be a function name
    FunInfoV info = fun_info_v(fnameoid, env);
    if (std::holds_alternative<GlobalFunInfoV>(info) || std::holds_alternative<NoFunInfoV>(info)) {
      if (std::holds_alternative<NoFunInfoV>(info) // Warn
          && _sym_register_global_function_ref->fboundp() && !code_walking_p())
        eval::funcall(_sym_register_global_function_ref, fnameoid, ctxt.source_info());
      ctxt.assemble1(vm_code::called_fdefinition, ctxt.fcell_index(fnameoid));
    } else if (std::holds_alternative<LocalFunInfoV>(info)) {
      LocalFunInfo_sp lfinfo = std::get<LocalFunInfoV>(info).info();
      lfinfo->lex()->setReadP(true);
      maybe_warn_used(fnameoid, lfinfo->lex(), ctxt.source_info(), true);
      ctxt.reference_lexical_info(lfinfo->lex());
    } else
      // FIXME: e.g. #'with-open-file. needs better error.
      SIMPLE_ERROR("{} does not name a function", _rep_(fnameoid));
  }
}

// Compile fform, which needs to return one value (regardless of
//  ctxt.receiving) and evaluates to an fdesignator that needs coercion
//  to a function which will only be called immediately.
void compile_fdesignator(T_sp fform, Lexenv_sp env, const Context ctxt) {
  // If we get (function ...) or (lambda ...), which is quite common
  // e.g. in (funcall #'(setf...) ...)
  // and (multiple-value-call (lambda ...mv-bind code ...) form)
  // we don't need to emit a vm_code::fdesignator instruction or anything.
  // TODO: We could do something smarter if given 'foo or a constant,
  // but those are more marginal.
  // This function basically bypasses compile_form. As such we need to
  // call the walker specially, if that's what we're doing, and if
  // we don't end up just calling compile_form. We ignore the rewriting
  // aspect and anyway we don't actually use that, so hey.
  if (fform.consp()) {
    if (oCar(fform) == cl::_sym_Function_O) {
      if (code_walking_p())
        eval::funcall(_sym_STARcodeWalkerSTAR->symbolValue(), fform, env);
      compile_called_function(oCadr(fform), env, ctxt);
      return;
    } else if (oCar(fform) == cl::_sym_lambda) {
      if (code_walking_p())
        eval::funcall(_sym_STARcodeWalkerSTAR->symbolValue(), fform, env);
      compile_called_function(fform, env, ctxt);
      return;
    }
  }
  // default
  compile_form(fform, env, ctxt.sub_receiving(1));
  ctxt.assemble1(vm_code::fdesignator, ctxt.env_index());
}

void compile_flet(List_sp definitions, List_sp body, Lexenv_sp env, const Context ctxt) {
  gc::Nilable<String_sp> docstring;
  List_sp code, specials, declares = nil<T_O>();
  eval::extract_declares_docstring_code_specials(body, declares, false, docstring, code, specials);
  ql::list fun_vars;
  size_t fun_count = 0;
  for (auto cur : definitions) {
    Cons_sp definition = gc::As<Cons_sp>(oCar(cur));
    T_sp name = oCar(definition);
    List_sp declares = nil<T_O>();
    gc::Nilable<String_sp> docstring;
    List_sp code;
    List_sp specials;
    eval::extract_declares_docstring_code_specials(oCddr(definition), declares, false, docstring, code, specials);
    // If the function does not have a declared name, name it (flet whatever).
    if (extract_lambda_name_from_declares(declares).nilp())
      declares = Cons_O::create(Cons_O::createList(core::_sym_lambdaName, Cons_O::createList(cl::_sym_flet, name)), declares);
    // Compile the function.
    T_sp block = Cons_O::create(cl::_sym_block, Cons_O::create(core__function_block_name(name), code));
    T_sp lambda = Cons_O::createList(cl::_sym_lambda, oCadr(definition), Cons_O::create(cl::_sym_declare, declares), block);
    compile_function(lambda, env, ctxt.sub_receiving(1));
    fun_vars << name;
    ++fun_count;
  }
  ctxt.emit_bind(fun_count, env->frameEnd());
  Lexenv_sp new_env = env->bind_funs(fun_vars.cons(), ctxt);
  // Generate debug info
  Label_sp begin_label = Label_O::make(), end_label = Label_O::make();
  if (definitions.notnilp())
    begin_label->contextualize(ctxt);
  ql::list debugbindings, ibindings;
  for (auto cur : definitions) {
    T_sp name = oCaar(cur);
    T_sp fname = Cons_O::createList(cl::_sym_Function_O, name);
    auto info = gc::As_assert<LocalFunInfo_sp>(new_env->functionInfo(name));
    LexicalInfo_sp lex = info->lex();
    lex->setIgnore(binding_ignore(fname, declares));
    lex->setDecls(decls_for_fun(name, declares));
    debugbindings << Cons_O::create(fname, lex);
    ibindings << Cons_O::createList(fname, lex, source_location_for(oCar(cur), ctxt.source_info()));
  }
  T_sp dbindings = debugbindings.cons();
  if (dbindings.notnilp())
    ctxt.push_debug_info(BytecodeDebugVars_O::make(begin_label, end_label, dbindings));
  // Compile body
  compile_locally(body, new_env, ctxt);
  if (dbindings.notnilp())
    end_label->contextualize(ctxt);
  warn_ignorance(ibindings.cons());
}

void compile_labels(List_sp definitions, List_sp body, Lexenv_sp env, const Context ctxt) {
  gc::Nilable<String_sp> docstring;
  List_sp code, specials, body_declares = nil<T_O>();
  eval::extract_declares_docstring_code_specials(body, body_declares, false, docstring, code, specials);
  size_t fun_count = 0;
  ql::list fun_vars;
  ql::list closures;
  ql::list debugbindings, ibindings;
  Label_sp begin_label = Label_O::make(), end_label = Label_O::make();
  for (auto cur : definitions) {
    Cons_sp definition = gc::As<Cons_sp>(oCar(cur));
    T_sp name = oCar(definition);
    fun_vars << name;
    ++fun_count;
  }
  Lexenv_sp new_env = env->bind_funs(fun_vars.cons(), ctxt);
  for (auto cur : definitions) {
    Cons_sp definition = gc::As_unsafe<Cons_sp>(oCar(cur));
    T_sp name = oCar(definition);
    List_sp declares = nil<T_O>();
    gc::Nilable<String_sp> docstring;
    List_sp code;
    List_sp specials;
    eval::extract_declares_docstring_code_specials(oCddr(definition), declares, false, docstring, code, specials);
    if (extract_lambda_name_from_declares(declares).nilp())
      declares = Cons_O::create(Cons_O::createList(core::_sym_lambdaName, Cons_O::createList(cl::_sym_labels, name)), declares);
    T_sp block = Cons_O::create(cl::_sym_block, Cons_O::create(core__function_block_name(name), code));
    T_sp fun_body = Cons_O::createList(Cons_O::create(cl::_sym_declare, declares), block);
    Cfunction_sp fun =
        compile_lambda(oCadr(definition), fun_body, new_env, ctxt.module(), source_location_for(definition, ctxt.source_info()));
    size_t literal_index = ctxt.cfunction_index(fun);
    LocalFunInfo_sp lfi = gc::As_assert<LocalFunInfo_sp>(fun_info(name, new_env));
    if (fun->closed()->length() == 0) // not a closure- easy
      ctxt.assemble1(vm_code::_const, literal_index);
    else {
      closures << Cons_O::create(fun, clasp_make_fixnum(lfi->frameIndex()));
      ctxt.assemble1(vm_code::make_uninitialized_closure, literal_index);
    }
    T_sp fname = Cons_O::createList(cl::_sym_Function_O, name);
    LexicalInfo_sp lex = lfi->lex();
    lex->setIgnore(binding_ignore(fname, body_declares));
    lex->setDecls(decls_for_fun(name, body_declares));
    debugbindings << Cons_O::create(fname, lex);
    ibindings << Cons_O::createList(fname, lex, source_location_for(definition, ctxt.source_info()));
  }
  ctxt.emit_bind(fun_count, env->frameEnd());
  T_sp dbindings = debugbindings.cons();
  if (dbindings.notnilp())
    ctxt.push_debug_info(BytecodeDebugVars_O::make(begin_label, end_label, dbindings));
  begin_label->contextualize(ctxt);
  // Make the closures
  for (auto cur : gc::As_assert<List_sp>(closures.cons())) {
    Cfunction_sp cf = gc::As_unsafe<Cfunction_sp>(oCaar(cur));
    ComplexVector_T_sp closed = cf->closed();
    for (size_t i = 0; i < closed->length(); ++i) {
      LexicalInfo_sp info = gc::As_assert<LexicalInfo_sp>(closed[i]);
      ctxt.reference_lexical_info(info);
    }
    ctxt.assemble1(vm_code::initialize_closure, oCdar(cur).unsafe_fixnum());
  }
  compile_locally(body, new_env, ctxt);
  end_label->contextualize(ctxt);
  warn_ignorance(ibindings.cons());
}

static void compile_setq_1(Symbol_sp var, T_sp valf, Lexenv_sp env, const Context ctxt) {
  VarInfoV info = var_info_v(var, env);
  if (std::holds_alternative<SymbolMacroVarInfoV>(info)) {
    Function_sp expander = std::get<SymbolMacroVarInfoV>(info).expander();
    T_sp expansion = expand_macro(expander, var, env);
    T_sp setform = Cons_O::createList(cl::_sym_setf, expansion, valf);
    compile_form(setform, env, ctxt);
  } else if (std::holds_alternative<NoVarInfoV>(info) || std::holds_alternative<SpecialVarInfoV>(info)) {
    if (std::holds_alternative<NoVarInfoV>(info) && _sym_warn_undefined_global_variable->fboundp() && !code_walking_p())
      eval::funcall(_sym_warn_undefined_global_variable, ctxt.source_info(), var);
    compile_form(valf, env, ctxt.sub_receiving(1));
    // If we need to return the new value, duplicate it on the stack.
    // We can't just read from the special, since some other thread may
    // alter it.
    // but if we're not returning a value we don't actually have to do that crap.
    if (ctxt.receiving() != 0) {
      ctxt.assemble0(vm_code::dup);
    }
    ctxt.assemble1(vm_code::symbol_value_set, ctxt.vcell_index(var));
    if (ctxt.receiving() == -1) // need values
      ctxt.assemble0(vm_code::pop);
  } else if (std::holds_alternative<LexicalVarInfoV>(info)) {
    LexicalVarInfo_sp lvinfo = std::get<LexicalVarInfoV>(info).info();
    bool localp = (lvinfo->funct() == ctxt.cfunction());
    if (!localp)
      lvinfo->setClosedOverP(true);
    lvinfo->setSetP(true);
    compile_form(valf, env, ctxt.sub_receiving(1));
    // Similar concerns to specials above (for closure variables)
    if (ctxt.receiving() != 0) {
      ctxt.assemble0(vm_code::dup);
    }
    if (localp)
      ctxt.emit_lexical_set(lvinfo);
    else { // we already know we need a cell, so don't bother w/ a fixup.
      ctxt.assemble1(vm_code::closure, ctxt.closure_index(lvinfo->lex()));
      ctxt.assemble0(vm_code::cell_set);
    }
    if (ctxt.receiving() == -1)
      ctxt.assemble0(vm_code::pop);
  } else if (std::holds_alternative<ConstantVarInfoV>(info)) {
    // FIXME: Better error (warning?)
    SIMPLE_ERROR("Cannot modify constant {}", var->__repr__());
  } else
    UNREACHABLE();
}

void compile_setq(List_sp pairs, Lexenv_sp env, const Context ctxt) {
  if (pairs.nilp()) {
    // degenerate case
    if (ctxt.receiving() != 0) {
      ctxt.assemble0(vm_code::nil);
      if (ctxt.receiving() == -1)
        ctxt.assemble0(vm_code::pop);
    }
  } else {
    do {
      Symbol_sp var = gc::As<Symbol_sp>(oCar(pairs));
      T_sp valf = oCadr(pairs);
      pairs = gc::As<List_sp>(oCddr(pairs));
      compile_setq_1(var, valf, env, pairs.notnilp() ? ctxt.sub_receiving(0) : ctxt);
    } while (pairs.notnilp());
  }
}

static bool eval_when_execp(List_sp situations) {
  for (auto cur : situations) {
    T_sp situation = oCar(cur);
    if ((situation == cl::_sym_eval) || (situation == kw::_sym_execute))
      return true;
  }
  return false;
}

void compile_eval_when(List_sp situations, List_sp body, Lexenv_sp env, const Context ctxt) {
  if (eval_when_execp(situations))
    compile_progn(body, env, ctxt);
  else
    compile_literal(nil<T_O>(), env, ctxt);
}

void compile_the(T_sp type, T_sp form, Lexenv_sp env, const Context ctxt) {
  // Bytecode ignores type declarations, but we save as an annotation for later.
  // The annotation goes AFTER the computation that produces it, so that for example
  // receiving=1 means "the most recently pushed datum at this IP is of this type".
  Label_sp lab = Label_O::make();
  compile_form(form, env, ctxt);
  lab->contextualize(ctxt);
  ctxt.push_debug_info(BytecodeAstThe_O::make(lab, lab, type, ctxt.receiving()));
}

void compile_if(T_sp cond, T_sp thn, T_sp els, Lexenv_sp env, const Context ctxt) {
  compile_form(cond, env, ctxt.sub_receiving(1));
  Label_sp then_label = Label_O::make(), else_label = Label_O::make();
  Label_sp done_label = Label_O::make();
  ctxt.emit_jump_if(then_label);
  else_label->contextualize(ctxt);
  ctxt.push_debug_info(BytecodeAstIf_O::make(else_label, done_label, ctxt.receiving()));
  compile_form(els, env, ctxt);
  ctxt.emit_jump(done_label);
  then_label->contextualize(ctxt);
  compile_form(thn, env, ctxt);
  done_label->contextualize(ctxt);
}

static bool go_tag_p(T_sp object) { return object.fixnump() || gc::IsA<Integer_sp>(object) || gc::IsA<Symbol_sp>(object); }

void compile_tagbody(List_sp statements, Lexenv_sp env, const Context ctxt) {
  Label_sp start = Label_O::make(), end = Label_O::make();
  ql::list tags;
  ql::list dtags; // for debug info
  for (auto cur : statements) {
    T_sp statement = oCar(cur);
    if (go_tag_p(statement)) {
      tags << statement;
      dtags << Cons_O::create(statement, nil<T_O>());
    }
  }
  List_sp ltags = tags.cons();
  List_sp ldtags = dtags.cons();
  if (ltags.nilp()) { // degenerate case
    Context stmt_ctxt = ctxt.sub_receiving(0);
    for (auto cur : statements) {
      T_sp statement = oCar(cur);
      compile_form(statement, env, stmt_ctxt);
    }
  } else { // actual dynenv+tags case
    LexicalInfo_sp dynenv = LexicalInfo_O::make(env->frameEnd(), ctxt.cfunction());
    Lexenv_sp nenv = env->bind_tags(tags.cons(), dynenv, ctxt);
    // Install labels in the debug tags list.
    for (Cons_sp cur : ldtags) {
      Cons_sp ccur = gc::As_assert<Cons_sp>(cur->car());
      ccur->setCdr(gc::As<TagInfo_sp>(nenv->tagInfo(ccur->car()))->exit());
    }
    // Bind the dynamic environment (or just save the stack pointer).
    ctxt.emit_entry_or_save_sp(dynenv);
    start->contextualize(ctxt);
    ctxt.push_debug_info(BytecodeAstTagbody_O::make(start, end, ldtags));
    // Compile the body, emitting the tag destination labels.
    Context stmt_ctxt = ctxt.sub_de(dynenv).sub_receiving(0);
    for (auto cur : statements) {
      T_sp statement = oCar(cur);
      if (go_tag_p(statement)) {
        TagInfo_sp tinfo = gc::As<TagInfo_sp>(nenv->tagInfo(statement));
        Label_sp lab = tinfo->exit();
        lab->contextualize(stmt_ctxt);
      } else
        compile_form(statement, nenv, stmt_ctxt);
    }
    end->contextualize(ctxt);
    stmt_ctxt.maybe_emit_entry_close(dynenv);
  }
  // return nil if we really have to
  if (ctxt.receiving() != 0) {
    ctxt.assemble0(vm_code::nil);
    if (ctxt.receiving() == -1)
      ctxt.assemble0(vm_code::pop);
  }
}

static void compile_exit(LexicalInfo_sp exit_de, Label_sp exit, const Context context) {
  if (exit_de->cfunction() == context.cfunction()) { // local return
    // Unwind interposed dynenvs.
    for (auto cur : context.dynenv()) {
      T_sp interde = oCar(cur);
      if (interde == exit_de)
        break;
      if (gc::IsA<LexicalInfo_sp>(interde))
        context.maybe_emit_entry_close(gc::As_unsafe<LexicalInfo_sp>(interde));
      else if (interde == cl::_sym_catch)
        context.assemble0(vm_code::catch_close);
      else if (interde == cl::_sym_unwind_protect)
        context.assemble0(vm_code::cleanup);
      else // must be a count of specials
        context.emit_unbind(interde.unsafe_fixnum());
    }
    // Actually exit.
    context.emit_ref_or_restore_sp(exit_de);
    context.emit_exit_or_jump(exit_de, exit);
  } else { // nonlocal
    exit_de->setClosedOverP(true);
    context.reference_lexical_info(exit_de);
    context.emit_exit(exit);
  }
}

void compile_go(T_sp tag, Lexenv_sp env, const Context ctxt) {
  T_sp tinfo = env->tagInfo(tag);
  if (gc::IsA<TagInfo_sp>(tinfo)) {
    Label_sp start = Label_O::make(), end = Label_O::make();
    start->contextualize(ctxt);
    TagInfo_sp info = gc::As_unsafe<TagInfo_sp>(tinfo);
    compile_exit(info->lex(), info->exit(), ctxt);
    end->contextualize(ctxt);
  } else
    SIMPLE_ERROR("The GO tag {} does not exist.", _rep_(tag));
}

void compile_block(Symbol_sp name, List_sp body, Lexenv_sp env, const Context ctxt) {
  Label_sp label = Label_O::make();
  Label_sp normal_label = Label_O::make();
  Label_sp start = Label_O::make();
  Lexenv_sp nenv = env->bind_block(name, label, ctxt);
  BlockInfo_sp binfo = gc::As<BlockInfo_sp>(nenv->blockInfo(name));
  LexicalInfo_sp blex = binfo->lex();
  // Bind the dynamic environment or save SP.
  ctxt.emit_entry_or_save_sp(blex);
  start->contextualize(ctxt);
  ctxt.push_debug_info(BytecodeAstBlock_O::make(start, label, name, ctxt.receiving()));
  // We force single values into multiple so that we can uniformly PUSH afterward.
  // Specifically: if we're returning 0 values, there's no problem anyway.
  // If we're returning multiple values, the local and nonlocal returns just
  // store into the multiple values, so no problem there.
  // If we're returning exactly one value, the local just pushes one, and
  // the nonlocal stores into the MV which is then vm_code::push'd to the stack.
  bool r1p = ctxt.receiving() == 1;
  compile_progn(body, nenv, ctxt.sub_de(blex));
  if (r1p)
    ctxt.emit_jump(normal_label);
  label->contextualize(ctxt);
  // When we need 1 value, we have to make sure that the
  // "exceptional" case pushes a single value onto the stack.
  if (r1p) {
    ctxt.assemble0(vm_code::push);
    normal_label->contextualize(ctxt);
  }
  ctxt.maybe_emit_entry_close(blex);
}

void compile_return_from(T_sp name, T_sp valuef, Lexenv_sp env, const Context ctxt) {
  T_sp tbinfo = env->blockInfo(name);
  if (gc::IsA<BlockInfo_sp>(tbinfo)) {
    Label_sp start = Label_O::make(), end = Label_O::make();
    start->contextualize(ctxt);
    BlockInfo_sp binfo = gc::As<BlockInfo_sp>(tbinfo);
    int breceiving = binfo->receiving();
    compile_form(valuef, env, ctxt.sub_receiving(breceiving == 0 ? 0 : -1));
    compile_exit(binfo->lex(), binfo->exit(), ctxt);
    end->contextualize(ctxt);
  } else
    SIMPLE_ERROR("The block {} does not exist.", _rep_(name));
}

// catch, throw, and progv are actually handled by macros right now,
// so these aren't used, but maybe will be in the fture.
void compile_catch(T_sp tag, List_sp body, Lexenv_sp env, const Context ctxt) {
  compile_form(tag, env, ctxt.sub_receiving(1));
  Label_sp target = Label_O::make();
  Label_sp normal_label = Label_O::make();
  Label_sp start = Label_O::make();
  // We force multiple values, as with block.
  bool r1p = ctxt.receiving() == 1;
  ctxt.emit_catch(target);
  start->contextualize(ctxt);
  ctxt.push_debug_info(BytecodeAstBlock_O::make(start, target, cl::_sym_catch, ctxt.receiving()));
  compile_progn(body, env, ctxt.sub_de(cl::_sym_catch));
  ctxt.assemble0(vm_code::catch_close);
  if (r1p)
    ctxt.emit_jump(normal_label);
  target->contextualize(ctxt);
  if (r1p) {
    ctxt.assemble0(vm_code::push);
    normal_label->contextualize(ctxt);
  }
}

void compile_throw(T_sp tag, T_sp rform, Lexenv_sp env, const Context ctxt) {
  compile_form(tag, env, ctxt.sub_receiving(1));
  compile_form(rform, env, ctxt.sub_receiving(-1));
  ctxt.assemble0(vm_code::_throw);
}

void compile_unwind_protect(T_sp protect, List_sp cleanup, Lexenv_sp env, const Context ctxt) {
  if (cleanup.nilp()) { // trivial
    compile_form(protect, env, ctxt);
  } else {
    // Make the cleanup closure.
    // Duplicates a bit of code from compile_function.
    Cfunction_sp cleanupt = compile_lambda(nil<T_O>(), Cons_O::createList(Cons_O::create(cl::_sym_progn, cleanup)), env,
                                           ctxt.module(), ctxt.source_info());
    ComplexVector_T_sp closed = cleanupt->closed();
    for (size_t i = 0; i < closed->length(); ++i)
      ctxt.reference_lexical_info(closed[i].as_assert<LexicalInfo_O>());
    // Actual protect instruction
    ctxt.assemble1(vm_code::protect, ctxt.cfunction_index(cleanupt));
    // and the body...
    compile_form(protect, env, ctxt.sub_de(cl::_sym_unwind_protect));
    ctxt.assemble0(vm_code::cleanup);
  }
}

void compile_progv(T_sp syms, T_sp vals, List_sp body, Lexenv_sp env, const Context ctxt) {
  compile_form(syms, env, ctxt.sub_receiving(1));
  compile_form(vals, env, ctxt.sub_receiving(1));
  ctxt.assemble1(vm_code::progv, ctxt.env_index());
  compile_progn(body, env, ctxt.sub_de(clasp_make_fixnum(1)));
  ctxt.emit_unbind(1);
}

void compile_multiple_value_call(T_sp fform, List_sp aforms, Lexenv_sp env, const Context ctxt) {
  compile_fdesignator(fform, env, ctxt);
  if (aforms.nilp()) {
    ctxt.emit_call(0);
  } else {
    // Compile the arguments
    T_sp first = oCar(aforms);
    List_sp rest = gc::As<List_sp>(oCdr(aforms));
    compile_form(first, env, ctxt.sub_receiving(-1));
    ctxt.assemble0(vm_code::push_values);
    if (rest.notnilp()) {
      for (auto cur : rest) {
        compile_form(oCar(cur), env, ctxt.sub_receiving(-1));
        ctxt.assemble0(vm_code::append_values);
      }
    }
    ctxt.emit_mv_call();
  }
}

void compile_multiple_value_prog1(T_sp fform, List_sp forms, Lexenv_sp env, const Context ctxt) {
  compile_form(fform, env, ctxt);
  // We only need to actually save anything with all-values returns.
  if (ctxt.receiving() == -1)
    ctxt.assemble0(vm_code::push_values);
  for (auto cur : forms)
    compile_form(oCar(cur), env, ctxt.sub_receiving(0));
  if (ctxt.receiving() == -1)
    ctxt.assemble0(vm_code::pop_values);
}

// Compile a call, where the function is already on the stack.
static void compile_call(T_sp args, Lexenv_sp env, const Context context) {
  // Compile the arguments.
  size_t argcount = 0;
  for (auto cur : gc::As<List_sp>(args)) {
    ++argcount;
    compile_form(oCar(cur), env, context.sub_receiving(1));
  }
  // generate the call
  context.emit_call(argcount);
}

void compile_load_time_value(T_sp form, T_sp tread_only_p, Lexenv_sp env, const Context context) {
  // load-time-value forms are compiled by putting their information into
  // a slot in the cmodule. This is so that (this part of) the compiler can
  // be used uniformly for eval, compile, or compile-file. It is slightly
  // inefficient for the former two cases, compared to evaluating forms
  // immediately, but load-time-value is not exactly heavily used.
  // The standard specifies the behavior when read-only-p is a literal t or
  // nil, and nothing else.
  bool read_only_p;
  if (tread_only_p.nilp())
    read_only_p = false;
  else if (tread_only_p == cl::_sym_T_O)
    read_only_p = true;
  // FIXME: Better error
  else
    SIMPLE_ERROR("load-time-value read-only-p is not T or NIL: {}", _rep_(tread_only_p));

  // Add the LTV to the cmodule.
  size_t ind = context.ltv_index(form, read_only_p);
  // With that done, we basically just need to compile a literal load.
  // (Note that we do always need to register the LTV, since it may have
  //  some weird side effect. We could hypothetically save some space by
  //  not allocating a spot in the constants if the value isn't actually
  //  used, but that's a very marginal case.)
  switch (context.receiving()) {
  case 0:
    break; // no value required, so compile nothing
  case 1:
    context.assemble1(vm_code::_const, ind);
    break;
  case -1: // all values
    context.assemble1(vm_code::_const, ind);
    context.assemble0(vm_code::pop);
    break;
  default:
    SIMPLE_ERROR("BUG: Don't know how to compile LTV returning %" PFixnum " values", context.receiving());
  }
}

static T_sp symbol_macrolet_bindings(Lexenv_sp menv, List_sp bindings, T_sp vars) {
  for (auto cur : bindings) {
    T_sp binding = oCar(cur);
    Symbol_sp name = gc::As<Symbol_sp>(oCar(binding));
    if (name->getReadOnly())
      SIMPLE_PROGRAM_ERROR("The symbol bound by SYMBOL-MACROLET must not be a constant variable: ~s", name);
    if (name->specialP())
      SIMPLE_PROGRAM_ERROR("The symbol bound by SYMBOL-MACROLET must not be a special variable: ~s", name);
    T_sp expansion = oCadr(binding);
    // FIXME: Compiling a new function for the expander is overkill
    T_sp formv = cl__gensym(SimpleBaseString_O::make("FORM"));
    T_sp envv = cl__gensym(SimpleBaseString_O::make("ENV"));
    T_sp lexpr = Cons_O::createList(cl::_sym_lambda, Cons_O::createList(formv, envv),
                                    Cons_O::createList(cl::_sym_declare, Cons_O::createList(cl::_sym_ignore, formv, envv)),
                                    Cons_O::createList(cl::_sym_quote, expansion));
    Function_sp expander = bytecompile(lexpr, menv);
    SymbolMacroVarInfo_sp info = SymbolMacroVarInfo_O::make(expander);
    vars = Cons_O::create(Cons_O::create(name, info), vars);
  }
  return vars;
}

void compile_symbol_macrolet(List_sp bindings, List_sp body, Lexenv_sp env, const Context context) {
  T_sp vars = symbol_macrolet_bindings(env->macroexpansion_environment(), bindings, env->vars());
  compile_locally(body, env->sub_vars(vars, env->frameEnd()), context);
}

// Given a macroexpansion environment, a alist of macrolet bindings, and the
// funs() of a lexenv, return new funs() with macro infos prepended.
static List_sp macrolet_bindings(Lexenv_sp menv, List_sp bindings, List_sp funs) {
  for (auto cur : bindings) {
    T_sp binding = oCar(cur);
    T_sp name = oCar(binding);
    T_sp lambda_list = oCadr(binding);
    T_sp body = oCddr(binding);
    T_sp eform = eval::funcall(ext::_sym_parse_macro, name, lambda_list, body, menv);
    Function_sp expander = bytecompile(eform, menv);
    LocalMacroInfo_sp info = LocalMacroInfo_O::make(expander);
    funs = Cons_O::create(Cons_O::create(name, info), funs);
  }
  return funs;
}

void compile_macrolet(List_sp bindings, List_sp body, Lexenv_sp env, const Context context) {
  List_sp funs = macrolet_bindings(env->macroexpansion_environment(), bindings, env->funs());
  Lexenv_sp nenv = env->sub_funs(funs);
  compile_locally(body, nenv, context);
}

void compile_funcall(T_sp fform, List_sp args, Lexenv_sp env, const Context context) {
  // Expand compiler macros when fform = #'foo or #'(setf foo).
  if (fform.consp() && oCar(fform) == cl::_sym_Function_O && oCdr(fform).consp() && oCddr(fform).nilp() &&
      (gc::IsA<Symbol_sp>(oCadr(fform)) || (oCadr(fform).consp() && oCaadr(fform) != cl::_sym_lambda))) {
    T_sp fname = oCadr(fform);
    FunInfoV info = fun_info_v(fname, env);
    if (std::holds_alternative<GlobalFunInfoV>(info)) {
      T_sp cmexpander = std::get<GlobalFunInfoV>(info).cmexpander();
      // We don't skip typep/case here since we don't actually use them
      // in a dangerously recursive way with funcall.
      if (cmexpander.notnilp() && !env->notinlinep(fname)) {
        T_sp form = Cons_O::create(cl::_sym_funcall, Cons_O::create(fform, args));
        T_sp expansion = expand_compiler_macro(gc::As<Function_sp>(cmexpander), form, env, context.source_info());
        if (expansion != form) {
          compile_form(expansion, env, context);
          return;
        }
      }
    }
  }
  // No compiler macro, but we can avoid actually calling FUNCALL.
  compile_fdesignator(fform, env, context);
  compile_call(args, env, context);
}

void compile_primop_funcall(T_sp callee, List_sp args, Lexenv_sp env, const Context context) {
  compile_form(callee, env, context.sub_receiving(1));
  compile_call(args, env, context);
}

void compile_combination(T_sp head, T_sp rest, Lexenv_sp env, const Context context) {
  if (head == cl::_sym_progn)
    compile_progn(rest, env, context);
  else if (head == cl::_sym_let)
    compile_let(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_letSTAR)
    compile_letSTAR(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_flet)
    compile_flet(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_labels)
    compile_labels(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_setq)
    compile_setq(rest, env, context);
  else if (head == cl::_sym_if)
    compile_if(oCar(rest), oCadr(rest), oCaddr(rest), env, context);
  else if (head == cl::_sym_Function_O)
    compile_function(oCar(rest), env, context);
  else if (head == cl::_sym_tagbody)
    compile_tagbody(rest, env, context);
  else if (head == cl::_sym_go)
    compile_go(oCar(rest), env, context);
  else if (head == cl::_sym_block)
    compile_block(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_return_from)
    compile_return_from(oCar(rest), oCadr(rest), env, context);
  else if (head == cl::_sym_quote)
    compile_literal(oCar(rest), env, context);
  else if (head == cl::_sym_load_time_value)
    compile_load_time_value(oCar(rest), oCadr(rest), env, context);
  else if (head == cl::_sym_macrolet)
    compile_macrolet(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_symbol_macrolet)
    compile_symbol_macrolet(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_multiple_value_call)
    compile_multiple_value_call(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_multiple_value_prog1)
    compile_multiple_value_prog1(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_locally)
    compile_locally(rest, env, context);
  else if (head == cl::_sym_eval_when)
    compile_eval_when(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_the)
    compile_the(oCar(rest), oCadr(rest), env, context);
  else if (head == cl::_sym_catch)
    compile_catch(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_throw)
    compile_throw(oCar(rest), oCadr(rest), env, context);
  else if (head == cl::_sym_unwind_protect)
    compile_unwind_protect(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_progv)
    compile_progv(oCar(rest), oCadr(rest), oCddr(rest), env, context);
  // basic optimization
  else if (head == cl::_sym_funcall
           // Do a basic syntax check so that (funcall) fails properly.
           && rest.consp())
    compile_funcall(oCar(rest), oCdr(rest), env, context);
  // extension
  else if (head == cleavirPrimop::_sym_funcall)
    compile_primop_funcall(oCar(rest), oCdr(rest), env, context);
  else if (head == cleavirPrimop::_sym_eq) {
    // KLUDGE: Compile a call to EQ.
    // Better would be to use the EQ opcode. Better than that would be
    // eliminating the special operator entirely and working with the
    // function instead.
    compile_called_function(cl::_sym_eq, env, context);
    compile_call(rest, env, context);
  }
  // not a special form
  else {
    if (gc::IsA<Symbol_sp>(head)) {
      FunInfoV info = fun_info_v(head, env);
      if (std::holds_alternative<GlobalMacroInfoV>(info)) {
        Function_sp expander = std::get<GlobalMacroInfoV>(info).expander();
        T_sp expansion = expand_macro(expander, Cons_O::create(head, rest), env);
        Label_sp begin_label = Label_O::make(), end_label = Label_O::make();
        begin_label->contextualize(context);
        context.push_debug_info(BytecodeDebugMacroexpansion_O::make(begin_label, end_label, head));
        compile_form(expansion, env, context);
        end_label->contextualize(context);
      } else if (std::holds_alternative<LocalMacroInfoV>(info)) {
        Function_sp expander = std::get<LocalMacroInfoV>(info).expander();
        T_sp expansion = expand_macro(expander, Cons_O::create(head, rest), env);
        compile_form(expansion, env, context);
      } else if (std::holds_alternative<GlobalFunInfoV>(info)) {
        T_sp cmexpander = std::get<GlobalFunInfoV>(info).cmexpander();
        if (cmexpander.notnilp() &&
            !env->notinlinep(head)
            // KLUDGE: CASE expands into PRIMOP:CASE.
            && (head != cl::_sym_case)) {
          // Compiler macroexpand
          T_sp form = Cons_O::create(head, rest);
          T_sp expansion = expand_compiler_macro(gc::As<Function_sp>(cmexpander), form, env, context.source_info());
          if (expansion != form) {
            compile_form(expansion, env, context);
            return;
          }
        } // no compiler macro, or expansion declined: call
        compile_called_function(head, env, context);
        compile_call(rest, env, context);
      } else if (std::holds_alternative<LocalFunInfoV>(info) || std::holds_alternative<NoFunInfoV>(info)) {
        // unknown function warning handled by compile-function (eventually)
        // note we do a double lookup of the fun info,
        // which is inefficient in the compiler (doesn't affect generated code)
        compile_called_function(head, env, context);
        compile_call(rest, env, context);
      } else
        UNREACHABLE();
    } else if (gc::IsA<Cons_sp>(head) && (oCar(head) == cl::_sym_lambda)) {
      // Lambda form
      compile_called_function(head, env, context);
      compile_call(rest, env, context);
    } else
      SIMPLE_ERROR("Illegal combination head: {} rest: {}", _rep_(head), _rep_(rest));
  }
}

void compile_form(T_sp form, Lexenv_sp env, const Context context) {
  // Code walk if we're doing that
  if (code_walking_p())
    form = eval::funcall(_sym_STARcodeWalkerSTAR->symbolValue(), form, env);
  // Record source location if we have it.
  T_sp source_location = source_location_for(form, nil<T_O>());
  Label_sp begin_label = Label_O::make();
  Label_sp end_label = Label_O::make();
  Context ncontext = context;
  if (source_location.notnilp()) {
    ncontext = context.sub_source(source_location);
    begin_label->contextualize(ncontext);
    // We push the info BEFORE compiling the form so that the infos
    // are naturally sorted by their start position.
    context.push_debug_info(BytecodeDebugLocation_O::make(begin_label, end_label, source_location));
  }
  // Compile
  if (gc::IsA<Symbol_sp>(form))
    compile_symbol(gc::As_unsafe<Symbol_sp>(form), env, ncontext);
  else if (form.consp())
    compile_combination(oCar(form), oCdr(form), env, ncontext);
  else
    compile_literal(form, env, ncontext);
  // And finish off the source info.
  if (source_location.notnilp()) {
    end_label->contextualize(ncontext);
  }
}

CL_LAMBDA(module lambda-expression &optional (env (cmp::make-null-lexical-environment)));
CL_DOCSTRING(R"dx(Compile the given lambda-expression into an existing module. Return a handle to it.)dx");
CL_DEFUN Cfunction_sp bytecompile_into(Module_sp module, T_sp lambda_expression, Lexenv_sp env) {
  if (!gc::IsA<Cons_sp>(lambda_expression) || (oCar(lambda_expression) != cl::_sym_lambda))
    SIMPLE_ERROR("bytecompiler passed a non-lambda-expression: {}", _rep_(lambda_expression));
  T_sp lambda_list = oCadr(lambda_expression);
  T_sp body = oCddr(lambda_expression);
  return compile_lambda(lambda_list, body, env, module,
                        source_location_for(lambda_expression, core::_sym_STARcurrentSourcePosInfoSTAR->symbolValue()));
}

CL_LAMBDA(lambda-expression &optional (env (cmp::make-null-lexical-environment)));
CL_DEFUN Function_sp bytecompile(T_sp lambda_expression, Lexenv_sp env) {
  Module_sp module = Module_O::make();
  Cfunction_sp cf = bytecompile_into(module, lambda_expression, env);
  return cf->link_function();
}

static Lexenv_sp coerce_lexenv_desig(T_sp env) {
  if (env.nilp())
    return make_null_lexical_environment();
  else
    return gc::As<Lexenv_sp>(env);
}

SYMBOL_EXPORT_SC_(CompPkg, bytecode_implicit_compile_form);

CL_LAMBDA(form &optional env);
CL_DEFUN T_mv cmp__bytecode_implicit_compile_form(T_sp form, T_sp env) {
  T_sp lexpr = Cons_O::createList(cl::_sym_lambda, nil<T_O>(), Cons_O::createList(cl::_sym_declare),
                                  Cons_O::createList(cl::_sym_progn, form));
  //  printf("%s:%d:%s lexpr = %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(lexpr).c_str());
  Function_sp thunk = bytecompile(lexpr, coerce_lexenv_desig(env));
  return eval::funcall(thunk);
}

T_sp LoadTimeValueInfo_O::eval() { return cmp__bytecode_implicit_compile_form(this->form(), make_null_lexical_environment()); }

T_mv bytecode_toplevel_eval(T_sp, T_sp);

CL_DEFUN T_mv bytecode_toplevel_progn(List_sp forms, Lexenv_sp env) {
  for (auto cur : forms)
    if (oCdr(cur).nilp()) // done
      return bytecode_toplevel_eval(oCar(cur), env);
    else
      bytecode_toplevel_eval(oCar(cur), env);
  // If there are no forms, return NIL.
  return Values(nil<T_O>());
}

CL_DEFUN T_mv bytecode_toplevel_eval_when(List_sp situations, List_sp forms, Lexenv_sp env) {
  if (eval_when_execp(situations))
    return bytecode_toplevel_progn(forms, env);
  else
    return nil<T_O>();
}

CL_DEFUN T_mv bytecode_toplevel_locally(List_sp body, Lexenv_sp env) {
  List_sp declares = nil<T_O>();
  gc::Nilable<String_sp> docstring;
  List_sp code;
  List_sp specials;
  eval::extract_declares_docstring_code_specials(body, declares, false, docstring, code, specials);
  env = env->add_specials(specials)->add_decls(declares);
  return bytecode_toplevel_progn(code, env);
}

CL_DEFUN T_mv bytecode_toplevel_macrolet(List_sp bindings, List_sp body, Lexenv_sp env) {
  // FIXME: We can maybe skip macroexpansion_environment,
  // assuming bytecode_toplevel_eval was originally actually called
  // with an empty lexenv as it ought to be.
  List_sp funs = macrolet_bindings(env->macroexpansion_environment(), bindings, env->funs());
  return bytecode_toplevel_locally(body, env->sub_funs(funs));
}

CL_DEFUN T_mv bytecode_toplevel_symbol_macrolet(List_sp bindings, List_sp body, Lexenv_sp env) {
  T_sp vars = symbol_macrolet_bindings(env->macroexpansion_environment(), bindings, env->vars());
  return bytecode_toplevel_locally(body, env->sub_vars(vars, env->frameEnd()));
}

SYMBOL_EXPORT_SC_(CompPkg, bytecode_toplevel_eval);

CL_LAMBDA(form &optional env);
CL_DEFUN T_mv bytecode_toplevel_eval(T_sp form, T_sp tenv) {
  Lexenv_sp env = coerce_lexenv_desig(tenv);
  T_sp eform = cl__macroexpand(form, env);
  if (gc::IsA<Cons_sp>(eform)) {
    T_sp head = oCar(eform);
    if (head == cl::_sym_progn)
      return bytecode_toplevel_progn(oCdr(eform), env);
    else if (head == cl::_sym_eval_when)
      return bytecode_toplevel_eval_when(oCadr(eform), oCddr(eform), env);
    else if (head == cl::_sym_locally)
      return bytecode_toplevel_locally(oCdr(eform), env);
    else if (head == cl::_sym_macrolet)
      return bytecode_toplevel_macrolet(oCadr(eform), oCddr(eform), env);
    else if (head == cl::_sym_symbol_macrolet)
      return bytecode_toplevel_symbol_macrolet(oCadr(eform), oCddr(eform), env);
    else
      return cmp__bytecode_implicit_compile_form(eform, env);
  } else
    return cmp__bytecode_implicit_compile_form(eform, env);
}

}; // namespace comp
