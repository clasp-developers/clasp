#include <clasp/core/bytecode_compiler.h>
#include <clasp/core/evaluator.h>         // extract_decl...
#include <clasp/core/sysprop.h>           // core__get_sysprop
#include <clasp/core/lambdaListHandler.h> // lambda list parsing
#include <clasp/core/designators.h>       // functionDesignator
#include <clasp/core/primitives.h>        // gensym, function_block_name
#include <clasp/core/sourceFileInfo.h>    // source info stuff
#include <clasp/llvmo/llvmoPackage.h>
#include <clasp/core/bytecode.h>
#include <algorithm> // max

#define VM_CODES
#include <virtualMachine.h>
#undef VM_CODES

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
  else return nil<T_O>();
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

T_sp Lexenv_O::lookupMacro(T_sp macroname) {
  T_sp info = this->functionInfo(macroname);
  if (gc::IsA<GlobalMacroInfo_sp>(info))
    return gc::As_unsafe<GlobalMacroInfo_sp>(info)->expander();
  else if (gc::IsA<LocalMacroInfo_sp>(info))
    return gc::As_unsafe<LocalMacroInfo_sp>(info)->expander();
  // no info
  else return info;
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
    Symbol_sp var = oCar(cur);
    if (var->getReadOnly())
      SIMPLE_ERROR("Cannot bind constant value %s!", _rep_(var));
    auto info = LexicalVarInfo_O::make(idx++, cf);
    Cons_sp pair = Cons_O::create(var, info);
    new_vars = Cons_O::create(pair, new_vars);
  }
  return Lexenv_O::make(new_vars, this->tags(), this->blocks(), this->funs(), this->notinlines(), frame_end);
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
  return Lexenv_O::make(new_vars, this->tags(), this->blocks(), this->funs(), this->notinlines(), frame_end);
}

Lexenv_sp Lexenv_O::add_specials(List_sp vars) {
  if (vars.nilp())
    return this->asSmartPtr();
  List_sp new_vars = this->vars();
  for (auto cur : vars) {
    Symbol_sp var = oCar(cur);
    auto info = SpecialVarInfo_O::make(var->specialP());
    Cons_sp pair = Cons_O::create(var, info);
    new_vars = Cons_O::create(pair, new_vars);
  }
  return Lexenv_O::make(new_vars, this->tags(), this->blocks(), this->funs(), this->notinlines(), this->frameEnd());
}

Lexenv_sp Lexenv_O::add_notinlines(List_sp fnames) {
  if (fnames.nilp())
    return this->asSmartPtr();
  else
    return Lexenv_O::make(this->vars(), this->tags(), this->blocks(), this->funs(), Cons_O::append(fnames, this->notinlines()),
                          this->frameEnd());
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
  return Lexenv_O::make(new_vars.cons(), nil<T_O>(), nil<T_O>(), new_funs.cons(), this->notinlines(), 0);
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

CL_LAMBDA(code position &rest values);
CL_DEFUN void assemble_into(SimpleVector_byte8_t_sp code, size_t position, List_sp values) {
  for (auto cur : values)
    (*code)[position++] = clasp_to_integral<uint8_t>(oCar(cur));
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
    bytecode->vectorPushExtend(vm_long);
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
  MultipleValues &mvn = core::lisp_multipleValues();
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
  MultipleValues &mvn = core::lisp_multipleValues();
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
    if (oCar(cname) != cl::_sym_setf || !dname.consp()
        || oCdr(dname).notnilp())
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
    } else return FunInfoV(GlobalFunInfoV(nil<T_O>()));    
  } else {
    Symbol_sp fname = gc::As<Symbol_sp>(name);
    if (!fname->fboundp()) return FunInfoV(NoFunInfoV());
    else if (fname->macroP())
      return FunInfoV(GlobalMacroInfoV(fname->symbolFunction()));
    else if (cl::_sym_compiler_macro_function->fboundp()) {
      T_sp cmexpander = eval::funcall(cl::_sym_compiler_macro_function, fname);
      return FunInfoV(GlobalFunInfoV(cmexpander));
    } else return FunInfoV(GlobalFunInfoV(nil<T_O>()));
  }
}
  
bool Lexenv_O::notinlinep(T_sp fname) {
  for (auto cur : this->notinlines())
    if (oCar(cur) == fname)
      return true;
  return false;
}

// defined out of line for circularity reasons
Module_sp Context::module() const { return this->cfunction()->module(); }

size_t Context::literal_index(T_sp literal) const {
  ComplexVector_T_sp literals = this->cfunction()->module()->literals();
  // FIXME: Smarter POSITION
  for (size_t i = 0; i < literals->length(); ++i)
    if ((*literals)[i] == literal)
      return i;
  Fixnum_sp nind = literals->vectorPushExtend(literal);
  return nind.unsafe_fixnum();
}

// Like literal-index, but forces insertion. This is used when generating
// a keyword argument parser, since the keywords must be sequential even if
// they've previously appeared in the literals vector.
// This is also used by LTV processing to put in a placeholder.
size_t Context::new_literal_index(T_sp literal) const {
  Fixnum_sp nind = this->cfunction()->module()->literals()->vectorPushExtend(literal);
  return nind.unsafe_fixnum();
}

size_t Context::closure_index(T_sp info) const {
  ComplexVector_T_sp closed = this->cfunction()->closed();
  for (size_t i = 0; i < closed->length(); ++i)
    if ((*closed)[i] == info)
      return i;
  Fixnum_sp nind = closed->vectorPushExtend(info);
  return nind.unsafe_fixnum();
}

void Context::push_debug_info(T_sp info) const {
  this->cfunction()->module()->push_debug_info(info);
}

void Context::emit_jump(Label_sp label) const {
  ControlLabelFixup_O::make(label, vm_jump_8, vm_jump_16, vm_jump_24)->contextualize(*this);
}

void Context::emit_jump_if(Label_sp label) const {
  ControlLabelFixup_O::make(label, vm_jump_if_8, vm_jump_if_16, vm_jump_if_24)->contextualize(*this);
}

void Context::emit_entry_or_save_sp(LexicalVarInfo_sp dynenv) const {
  EntryFixup_O::make(dynenv)->contextualize(*this);
}

void Context::emit_ref_or_restore_sp(LexicalVarInfo_sp dynenv) const {
  RestoreSPFixup_O::make(dynenv)->contextualize(*this);
}

void Context::emit_exit(Label_sp label) const {
  ControlLabelFixup_O::make(label, vm_exit_8, vm_exit_16, vm_exit_24)->contextualize(*this);
}

void Context::emit_exit_or_jump(LexicalVarInfo_sp dynenv, Label_sp label) const {
  ExitFixup_O::make(dynenv, label)->contextualize(*this);
}

void Context::maybe_emit_entry_close(LexicalVarInfo_sp dynenv) const {
  EntryCloseFixup_O::make(dynenv)->contextualize(*this);
}

void Context::emit_catch(Label_sp label) const {
  ControlLabelFixup_O::make(label, vm_catch_8, vm_catch_16, 0)->contextualize(*this);
}

void Context::emit_jump_if_supplied(Label_sp label, size_t ind) const {
  JumpIfSuppliedFixup_O::make(label, ind)->contextualize(*this);
}

// Push the immutable value or cell of lexical in CONTEXT.
void Context::reference_lexical_info(LexicalVarInfo_sp info) const {
  if (info->funct() == this->cfunction())
    this->assemble1(vm_ref, info->frameIndex());
  else
    this->assemble1(vm_closure, this->closure_index(info));
}

void Context::maybe_emit_make_cell(LexicalVarInfo_sp info) const {
  LexRefFixup_O::make(info, vm_make_cell)->contextualize(*this);
}

void Context::maybe_emit_cell_ref(LexicalVarInfo_sp info) const {
  LexRefFixup_O::make(info, vm_cell_ref)->contextualize(*this);
}

// FIXME: This is probably a good candidate for a specialized
// instruction.
void Context::maybe_emit_encage(LexicalVarInfo_sp info) const { EncageFixup_O::make(info)->contextualize(*this); }

void Context::emit_lexical_set(LexicalVarInfo_sp info) const { LexSetFixup_O::make(info)->contextualize(*this); }

void Context::emit_parse_key_args(size_t max_count, size_t key_count, size_t keystart, size_t indx, bool aokp) const {
  ComplexVector_byte8_t_sp bytecode = this->cfunction()->bytecode();
  if ((max_count < (1 << 8)) && (key_count < (1 << 8)) && (keystart < (1 << 8)) && (indx < (1 << 8))) {
    bytecode->vectorPushExtend(vm_parse_key_args);
    bytecode->vectorPushExtend(max_count);
    bytecode->vectorPushExtend(key_count | (aokp ? 0x80 : 0));
    bytecode->vectorPushExtend(keystart);
    bytecode->vectorPushExtend(indx);
  } else if ((max_count < (1 << 16)) && (key_count < (1 << 16)) && (keystart < (1 << 16)) && (indx < (1 << 16))) {
    bytecode->vectorPushExtend(clasp_make_fixnum(vm_long));
    bytecode->vectorPushExtend(clasp_make_fixnum(vm_parse_key_args));
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

void Context::assemble0(uint8_t opcode) const { this->cfunction()->bytecode()->vectorPushExtend(opcode); }

void Context::assemble1(uint8_t opcode, size_t operand) const {
  ComplexVector_byte8_t_sp bytecode = this->cfunction()->bytecode();
  if (operand < (1 << 8)) {
    bytecode->vectorPushExtend(opcode);
    bytecode->vectorPushExtend(operand);
  } else if (operand < (1 << 16)) {
    bytecode->vectorPushExtend(vm_long);
    bytecode->vectorPushExtend(opcode);
    bytecode->vectorPushExtend(operand & 0xff);
    bytecode->vectorPushExtend(operand >> 8);
  } else
    SIMPLE_ERROR("Bytecode compiler limit reached: operand %zu too large", operand);
}

void Context::assemble2(uint8_t opcode, size_t operand1, size_t operand2) const {
  ComplexVector_byte8_t_sp bytecode = this->cfunction()->bytecode();
  if ((operand1 < (1 << 8)) && (operand2 < (1 << 8))) {
    bytecode->vectorPushExtend(opcode);
    bytecode->vectorPushExtend(operand1);
    bytecode->vectorPushExtend(operand2);
  } else if ((operand1 < (1 << 16)) && (operand2 < (1 << 16))) {
    bytecode->vectorPushExtend(vm_long);
    bytecode->vectorPushExtend(opcode);
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
    this->assemble1(vm_set, offset);
    break;
  case 0:
    break;
  default:
    this->assemble2(vm_bind, count, offset);
    break;
  }
}

void Context::emit_call(size_t argcount) const {
  switch (this->receiving()) {
  case 1:
      this->assemble1(vm_call_receive_one, argcount);
      break;
  case -1:
  case 0: // should be receive_fixed 0?
      this->assemble1(vm_call, argcount);
      break;
  default:
      this->assemble2(vm_call_receive_fixed, argcount, this->receiving());
      break;
  }
}

void Context::emit_mv_call() const {
  switch (this->receiving()) {
  case 1:
      this->assemble0(vm_mv_call_receive_one);
      break;
  case -1:
  case 0: // should be receive_fixed 0?
      this->assemble0(vm_mv_call);
      break;
  default:
      this->assemble1(vm_mv_call_receive_fixed, this->receiving());
      break;
  }
}

void Context::emit_special_bind(Symbol_sp sym) const { this->assemble1(vm_special_bind, this->literal_index(sym)); }

void Context::emit_unbind(size_t count) const {
  for (size_t i = 0; i < count; ++i)
    this->assemble0(vm_unbind);
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

static void emit_control_label_fixup(size_t size, size_t offset, size_t position,
                                     SimpleVector_byte8_t_sp code,
                                     uint8_t opcode8, uint8_t opcode16,
                                     uint8_t opcode24) {
  // Offset is a size_t so it's a positive integer i.e. dumpable.
  switch (size) {
  case 2:
    (*code)[position] = opcode8;
    break;
  case 3:
    (*code)[position] = opcode16;
    break;
  case 4:
    (*code)[position] = opcode24;
    break;
  default:
    SIMPLE_ERROR("Assembler bug: Impossible size %zu", size);
  }
  for (size_t i = 0; i < size - 1; ++i) {
    // Write the offset one byte at a time, starting with the LSB.
    (*code)[position + i + 1] = offset & 0xff;
    offset >>= 8;
  }
}

void ControlLabelFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  emit_control_label_fixup(this->size(), this->delta(), position, code,
                           this->_opcode8, this->_opcode16, this->_opcode24);
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

size_t ControlLabelFixup_O::resize() {
  return resize_control_label_fixup(this->delta());
}

void JumpIfSuppliedFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  size_t size = this->size();
  switch (size) {
  case 3:
    (*code)[position] = vm_jump_if_supplied_8;
    break;
  case 4:
    (*code)[position] = vm_jump_if_supplied_16;
    break;
  default:
    SIMPLE_ERROR("Assembler bug: Impossible size %zu", size);
  }
  (*code)[position + 1] = this->iindex();
  size_t offset = this->delta();
  for (size_t i = 0; i < size - 2; ++i) {
    (*code)[position + i + 2] = offset & 0xff;
    offset >>= 8;
  }
}

size_t JumpIfSuppliedFixup_O::resize() {
  ptrdiff_t delta = this->delta();
  if ((-(1 << 7) <= delta) && (delta <= (1 << 7) - 1))
    return 3;
  if ((-(1 << 15) <= delta) && (delta <= (1 << 15) - 1))
    return 4;
  else
    SIMPLE_ERROR("Bytecode compiler limit reached: Fixup delta too large");
}

void LexRefFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  size_t size = this->size();
  switch (size) {
  case 1:
    (*code)[position] = this->opcode();
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
  case 5: // FIXME: Use assemble_into?
    (*code)[position] = vm_ref;
    (*code)[position + 1] = index;
    (*code)[position + 2] = vm_make_cell;
    (*code)[position + 3] = vm_set;
    (*code)[position + 4] = index;
    break;
  case 9:
    (*code)[position] = vm_long;
    (*code)[position + 1] = vm_ref;
    (*code)[position + 2] = index & 0xff;
    (*code)[position + 3] = index >> 8;
    (*code)[position + 4] = vm_make_cell;
    (*code)[position + 5] = vm_long;
    (*code)[position + 6] = vm_set;
    (*code)[position + 7] = index & 0xff;
    (*code)[position + 8] = index >> 8;
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
    return 5;
  else if (index < 1 << 16)
    return 9;
  else
    SIMPLE_ERROR("Bytecode compiler limit reached: Fixup delta too large");
}

void LexSetFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  size_t size = this->size();
  size_t index = this->lex()->frameIndex();
  switch (size) {
  case 2:
    (*code)[position] = vm_set;
    (*code)[position + 1] = index;
    break;
  case 3:
    (*code)[position] = vm_ref;
    (*code)[position + 1] = index;
    (*code)[position + 2] = vm_cell_set;
    break;
  case 4:
    (*code)[position] = vm_long;
    (*code)[position + 1] = vm_set;
    (*code)[position + 2] = index & 0xff;
    (*code)[position + 3] = index >> 8;
    break;
  case 5:
    (*code)[position] = vm_long;
    (*code)[position + 1] = vm_ref;
    (*code)[position + 2] = index & 0xff;
    (*code)[position + 3] = index >> 8;
    (*code)[position + 4] = vm_cell_set;
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
    (*code)[position++] = vm_long;
  if (this->lex()->closedOverP())
    (*code)[position] = vm_entry;
  else
    (*code)[position] = vm_save_sp;
  if (index < 1 << 8)
    (*code)[position + 1] = index;
  else {
    (*code)[position + 1] = index & 0xff;
    (*code)[position + 2] = index >> 8;
  }
}

size_t EntryFixup_O::resize() {
  return (this->lex()->frameIndex() < 1 << 8) ? 2 : 4;
}

void RestoreSPFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  size_t index = this->lex()->frameIndex();
  if (index >= 1 << 8)
    (*code)[position++] = vm_long;
  if (this->lex()->closedOverP())
    (*code)[position] = vm_ref;
  else
    (*code)[position] = vm_restore_sp;
  if (index < 1 << 8)
    (*code)[position + 1] = index;
  else {
    (*code)[position + 1] = index & 0xff;
    (*code)[position + 2] = index >> 8;
  }
}

size_t RestoreSPFixup_O::resize() {
  return (this->lex()->frameIndex() < 1 << 8) ? 2 : 4;
}

void ExitFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  if (this->lex()->closedOverP())
    emit_control_label_fixup(this->size(), this->delta(), position, code,
                             vm_exit_8, vm_exit_16, vm_exit_24);
  else
    emit_control_label_fixup(this->size(), this->delta(), position, code,
                             vm_jump_8, vm_jump_16, vm_jump_24);
}

size_t ExitFixup_O::resize() {
  return resize_control_label_fixup(this->delta());
}

void EntryCloseFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  switch (this->size()) {
  case 1:
    (*code)[position] = vm_entry_close;
    break;
  default:
    UNREACHABLE();
  }
}

size_t EntryCloseFixup_O::resize() {
  return (this->lex()->closedOverP()) ? 1 : 0;
}

void Module_O::push_debug_info(T_sp info) {
  this->_debugInfo->vectorPushExtend(info);
}

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
    gc::As_assert<Annotation_sp>((*annotations)[idx])->_position += increase;
  }
  funct->_extra += increase;
  ComplexVector_T_sp functions = funct->module()->cfunctions();
  size_t nfuns = functions->length();
  for (size_t idx = funct->iindex() + 1; idx < nfuns; ++idx) {
    gc::As_assert<Cfunction_sp>((*functions)[idx])->_position += increase;
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
  T_sp tlast_cfunction = (*cfunctions)[cfunctions->length() - 1];
  Cfunction_sp last_cfunction = gc::As_assert<Cfunction_sp>(tlast_cfunction);
  return last_cfunction->pposition() + last_cfunction->final_size();
}


// Resolve the labels to fixnums, and LVInfos to frame locations.
// If a variable is stored in a cell, we indicate this by wrapping its
// frame location in a cons.
static void resolve_debug_vars(BytecodeDebugVars_sp info) {
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
  for (Cons_sp cur : info->bindings()) {
    T_sp tentry = cur->ocar();
    if (gc::IsA<Cons_sp>(tentry)) {
      Cons_sp entry = gc::As_unsafe<Cons_sp>(tentry);
      T_sp tlvinfo = entry->cdr();
      if (gc::IsA<LexicalVarInfo_sp>(tlvinfo)) {
        LexicalVarInfo_sp lvinfo = gc::As_unsafe<LexicalVarInfo_sp>(tlvinfo);
        T_sp index = clasp_make_fixnum(lvinfo->frameIndex());
        if (lvinfo->indirectLexicalP())
          entry->setCdr(Cons_O::createList(index));
        else
          entry->setCdr(index);
      }
    }
  }
}


void Module_O::resolve_debug_info() {
  // Replace all labels.
  for (T_sp info : *(this->debugInfo())) {
    if (gc::IsA<BytecodeDebugVars_sp>(info))
      resolve_debug_vars(gc::As_unsafe<BytecodeDebugVars_sp>(info));
  }
}

// Replacement for CL:REPLACE, which isn't available here.
static void replace_bytecode(SimpleVector_byte8_t_sp dest, ComplexVector_byte8_t_sp src, size_t start1, size_t start2,
                             size_t end2) {
  size_t index1, index2;
  for (index1 = start1, index2 = start2; index2 < end2; ++index1, ++index2) {
    (*dest)[index1] = (*src)[index2];
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

GlobalBytecodeSimpleFun_sp Cfunction_O::link_function(T_sp compile_info) {
  this->module()->link_load(compile_info);
  // Linking installed the GBEP in this cfunction's info. Return that.
  return this->info();
}

SimpleVector_byte8_t_sp Module_O::link() {
  Module_sp cmodule = this->asSmartPtr();
  cmodule->initialize_cfunction_positions();
  cmodule->resolve_fixup_sizes();
  cmodule->resolve_debug_info();
  return cmodule->create_bytecode();
}

void Module_O::link_load(T_sp compile_info) {
  Module_sp cmodule = this->asSmartPtr();
  SimpleVector_byte8_t_sp bytecode = cmodule->link();
  ComplexVector_T_sp cmodule_literals = cmodule->literals();
  size_t literal_length = cmodule_literals->length();
  SimpleVector_sp literals = SimpleVector_O::make(literal_length);
  ComplexVector_T_sp cmodule_debug_info = cmodule->debugInfo();
  size_t debug_info_length = cmodule_debug_info->length();
  SimpleVector_sp debug_info = SimpleVector_O::make(debug_info_length);
  BytecodeModule_sp bytecode_module = BytecodeModule_O::make();
  ComplexVector_T_sp cfunctions = cmodule->cfunctions();
  size_t function_index = 0;
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
    GlobalBytecodeSimpleFun_sp func = core__makeGlobalBytecodeSimpleFun(fdesc, bytecode_module, cfunction->nlocals(), cfunction->closed()->length(), ep.unsafe_fixnum(), cfunction->final_size(), trampoline);
    cfunction->setInfo(func);
  }
  // Replace the cfunctions in the cmodule literal vector with
  // real bytecode functions in the module vector.
  // Also replace load-time-value infos with the evaluated forms.
  for (size_t i = 0; i < literal_length; ++i) {
    T_sp lit = (*cmodule_literals)[i];
    if (gc::IsA<Cfunction_sp>(lit))
      (*literals)[i] = gc::As_unsafe<Cfunction_sp>(lit)->info();
    else if (gc::IsA<LoadTimeValueInfo_sp>(lit))
      (*literals)[i] = gc::As_unsafe<LoadTimeValueInfo_sp>(lit)->eval();
    else
      (*literals)[i] = lit;
  }
  // Copy the debug info.
  for (size_t i = 0; i < debug_info_length; ++i) {
    T_sp info = (*cmodule_debug_info)[i];
    if (gc::IsA<Cfunction_sp>(info))
      (*debug_info)[i] = gc::As_unsafe<Cfunction_sp>(info)->info();
    else
      (*debug_info)[i] = info;
  }
  // Now just install the bytecode and Bob's your uncle.
  bytecode_module->setf_literals(literals);
  bytecode_module->setf_bytecode(bytecode);
  bytecode_module->setf_debugInfo(debug_info);
  bytecode_module->setf_compileInfo(compile_info);
}

void compile_literal(T_sp literal, Lexenv_sp env, const Context context) {
  (void)env;
  switch (context.receiving()) {
  case 0:
      return; // No value required, so do nothing
  case 1:
      if (literal.nilp())
        context.assemble0(vm_nil);
      else
        context.assemble1(vm_const, context.literal_index(literal));
      return;
  case -1: // values
      if (literal.nilp())
        context.assemble0(vm_nil);
      else
        context.assemble1(vm_const, context.literal_index(literal));
      context.assemble0(vm_pop);
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
  Function_sp hook = coerce::functionDesignator(macroexpandHook);
  return eval::funcall(hook, expander, form, env);
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
    return;
  } else {
    if (std::holds_alternative<LexicalVarInfoV>(info)) {
      LexicalVarInfo_sp lvinfo = std::get<LexicalVarInfoV>(info).info();
      if (lvinfo->funct() == context.cfunction())
        // Local variable, just read it.
        context.assemble1(vm_ref, lvinfo->frameIndex());
      else { // closed over
        lvinfo->setClosedOverP(true);
        context.assemble1(vm_closure, context.closure_index(lvinfo));
      }
      context.maybe_emit_cell_ref(lvinfo);
    } else if (std::holds_alternative<SpecialVarInfoV>(info))
      context.assemble1(vm_symbol_value, context.literal_index(sym));
    else if (std::holds_alternative<ConstantVarInfoV>(info)) {
      compile_literal(std::get<ConstantVarInfoV>(info).value(), env, context);
      // Avoid the pop code below - compile-literal handles it.
      return;
    } else if (std::holds_alternative<NoVarInfoV>(info))
      // FIXME: Warn that the variable is unknown and we're assuming special.
      context.assemble1(vm_symbol_value, context.literal_index(sym));
    if (context.receiving() == -1)
      // Values return - put value in mv vector.
      context.assemble0(vm_pop);
  }
}

void compile_progn(List_sp forms, Lexenv_sp env, const Context ctxt) {
  if (forms.nilp())
    compile_literal(nil<T_O>(), env, ctxt);
  else
    for (auto cur : forms) {
      if (oCdr(cur).notnilp()) // compile for effect
        compile_form(oCar(cur), env, Context(ctxt, 0));
      else // compile for value
        compile_form(oCar(cur), env, ctxt);
    }
}

// Given a list of declaration specifiers as returned by extract_declares_etc,
// return a list of symbols declared notinline.
static List_sp decl_notinlines(List_sp declares) {
  ql::list result;
  for (auto cur : declares) {
    T_sp spec = oCar(cur);
    if (gc::IsA<Cons_sp>(spec) && (CONS_CAR(spec) == cl::_sym_notinline)) {
      for (auto dc : gc::As<List_sp>(CONS_CDR(spec)))
        result << oCar(dc);
    }
  }
  return result.cons();
}

void compile_locally(List_sp body, Lexenv_sp env, const Context ctxt) {
  List_sp declares = nil<T_O>();
  gc::Nilable<String_sp> docstring;
  List_sp code;
  List_sp specials;
  eval::extract_declares_docstring_code_specials(body, declares, false, docstring, code, specials);
  Lexenv_sp inner1 = env->add_specials(specials);
  Lexenv_sp inner2 = env->add_notinlines(decl_notinlines(declares));
  compile_progn(code, inner2, ctxt);
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
  ql::list debug_bindings; // alist (name . stack-location)
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
    compile_form(valf, env, Context(ctxt, 1));
    if (special_binding_p(var, specials, env)) {
      ++special_binding_count;
      ctxt.emit_special_bind(var);
    } else {
      // FIXME: We don't need to cons actual lexenvs here.
      post_binding_env = post_binding_env->bind1var(var, ctxt);
      ++lexical_binding_count;
     LexicalVarInfo_sp lvinfo
        = gc::As_assert<LexicalVarInfo_sp>(post_binding_env->variableInfo(var));
     debug_bindings << Cons_O::create(var, lvinfo);
     ctxt.maybe_emit_make_cell(lvinfo);
    }
  }
  ctxt.emit_bind(lexical_binding_count, env->frameEnd());
  post_binding_env = post_binding_env->add_notinlines(decl_notinlines(declares));
  begin_label->contextualize(ctxt);
  compile_progn(code, post_binding_env, Context(ctxt, Integer_O::create(special_binding_count)));
  ctxt.emit_unbind(special_binding_count);
  end_label->contextualize(ctxt);
  // Add the debug info
  ctxt.push_debug_info(BytecodeDebugVars_O::make(begin_label, end_label, debug_bindings.cons()));
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
    compile_form(valf, new_env, Context(ctxt, 1));
    if (special_binding_p(var, specials, env)) {
      ++special_binding_count;
      new_env = new_env->add_specials(Cons_O::createList(var));
      ctxt.emit_special_bind(var);
      ctxt = Context(ctxt, Integer_O::create(1));
    } else {
      Label_sp begin_label = Label_O::make();
      size_t frame_start = new_env->frameEnd();
      new_env = new_env->bind1var(var, ctxt);
      LexicalVarInfo_sp lvinfo = gc::As_assert<LexicalVarInfo_sp>(new_env->variableInfo(var));
      ctxt.maybe_emit_make_cell(lvinfo);
      ctxt.assemble1(vm_set, frame_start);
      // Set up debug info
      begin_label->contextualize(ctxt);
      ctxt.push_debug_info(BytecodeDebugVars_O::make(begin_label, end_label,
                                                     Cons_O::createList(Cons_O::create(var, lvinfo))));
    }
  }
  new_env = new_env->add_notinlines(decl_notinlines(declares));
  // We make a new environment to make sure free special declarations get
  // through even if this let* doesn't bind them.
  // This creates duplicate alist entries for anything that _is_ bound
  // here, but that's not a big deal.
  compile_progn(code, new_env->add_specials(specials), ctxt);
  ctxt.emit_unbind(special_binding_count);
  end_label->contextualize(ctxt);
}

// copied from evaluator.cc (on the premise that that will be deleted or
// heavily rewritten once the bytecode compiler is ported)
static T_sp extract_lambda_name_from_declares(List_sp declares) {
  for (auto cur : declares) {
    List_sp decl = oCar(cur);
    if (oCar(decl) == core::_sym_lambdaName)
      return oCadr(decl);
  }
  return nil<T_O>();
}

static T_sp extract_lambda_list_from_declares(List_sp declares, T_sp defaultll) {
  for (auto cur : declares) {
    List_sp decl = oCar(cur);
    if (oCar(decl) == core::_sym_lambdaList)
      return oCdr(decl);
  }
  return defaultll;
}

Lexenv_sp compile_optional_or_key_item(Symbol_sp var, T_sp defaulting_form, LexicalVarInfo_sp varinfo, Symbol_sp supplied_var,
                                                Label_sp next_label, bool var_specialp, bool supplied_specialp, const Context context,
                                                Lexenv_sp env) {
  Label_sp supplied_label = Label_O::make();
  T_sp supinfo = nil<T_O>();
  context.emit_jump_if_supplied(supplied_label, varinfo->frameIndex());
  // Emit code for the case of the variable not being supplied:
  // Bind the var to the default, and the suppliedvar to NIL if applicable.
  compile_form(defaulting_form, env, Context(context, 1));
  // Now that the default form is compiled, bind variables (for later vars)
  if (var_specialp)
    env = env->add_specials(Cons_O::createList(var));
  else
    // import the existing info.
    env = Lexenv_O::make(Cons_O::create(Cons_O::create(var, varinfo),
                                        env->vars()),
                         env->tags(), env->blocks(), env->funs(),
                         env->notinlines(), env->frameEnd());
  if (supplied_var.notnilp()) {
    if (supplied_specialp)
      env = env->add_specials(Cons_O::createList(supplied_var));
    else
      env = env->bind1var(supplied_var, context);
    supinfo = env->variableInfo(supplied_var);
  }
  // Actually generate the unsupplied case.
  if (var_specialp)
    context.emit_special_bind(var);
  else {
    context.maybe_emit_make_cell(varinfo);
    context.assemble1(vm_set, varinfo->frameIndex());
  }
  if (supplied_var.notnilp()) { // bind supplied_var to NIL
    context.assemble0(vm_nil);
    if (supplied_specialp)
      context.emit_special_bind(supplied_var);
    else {
      LexicalVarInfo_sp lsinfo = gc::As_assert<LexicalVarInfo_sp>(supinfo);
      context.maybe_emit_make_cell(lsinfo);
      context.assemble1(vm_set, lsinfo->frameIndex());
    }
  }
  context.emit_jump(next_label);
  // Now for when the variable is supplied.
  supplied_label->contextualize(context);
  if (var_specialp) { // we have it in a reg, so rebind
    context.assemble1(vm_ref, varinfo->frameIndex());
    context.emit_special_bind(var);
  } else { // in the reg already, but maybe needs a cell
    context.maybe_emit_encage(varinfo);
  }
  if (supplied_var.notnilp()) {
    compile_literal(cl::_sym_T_O, env, Context(context, 1));
    if (supplied_specialp)
      context.emit_special_bind(supplied_var);
    else {
      LexicalVarInfo_sp lsinfo = gc::As_assert<LexicalVarInfo_sp>(supinfo);
      context.maybe_emit_make_cell(lsinfo);
      context.assemble1(vm_set, lsinfo->frameIndex());
    }
  }
  // That's it for code generation. Now return the new environment.
  return env;
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
  ql::list lreqs;
  for (auto &it : reqs)
    lreqs << it._ArgTarget;
  Lexenv_sp new_env = env->bind_vars(lreqs.cons(), context);
  // This environment is only used for assigning indices to opt/key variables.
  size_t special_binding_count = 0;

  entry_point->contextualize(context);
  // Generate argument count check.
  if ((min_count > 0) && (min_count == max_count) && !morep)
    context.assemble1(vm_check_arg_count_EQ, min_count);
  else {
    if (min_count > 0)
      context.assemble1(vm_check_arg_count_GE, min_count);
    if (!morep)
      context.assemble1(vm_check_arg_count_LE, max_count);
  }
  if (min_count > 0) {
    Label_sp begin_label = Label_O::make();
    // Bind the required arguments.
    context.assemble1(vm_bind_required_args, min_count);
    ql::list debugbindings;
    ql::list sreqs; // required parameters that are special
    for (auto &it : reqs) {
      // We account for special declarations in outer environments/globally
      // by checking the original environment - not our new one - for info.
      T_sp var = it._ArgTarget;
      auto lvinfo = gc::As_assert<LexicalVarInfo_sp>(new_env->variableInfo(var));
      if (special_binding_p(var, specials, env)) {
        sreqs << var;
        context.assemble1(vm_ref, lvinfo->frameIndex());
        context.emit_special_bind(var);
        ++special_binding_count; // not in lisp - bug?
      } else {
        context.maybe_emit_encage(lvinfo);
        debugbindings << Cons_O::create(var, lvinfo);
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
    context.assemble2(vm_bind_optional_args, min_count, optional_count);
    // Mark the locations of each optional. Note that we do this even if
    // the variable will be specially bound, to match the placement by
    // bind_optional_args.
    ql::list opts;
    for (auto &it : optionals)
      opts << it._ArgTarget;
    optkey_env = optkey_env->bind_vars(opts.cons(), context);
    // new_env has enough space for the optional arguments, but without the
    // variables actually bound, so that default forms can be compiled correctly
    new_env = Lexenv_O::make(new_env->vars(), optkey_env->tags(),
                             optkey_env->blocks(), optkey_env->funs(),
                             optkey_env->notinlines(), optkey_env->frameEnd());
  }
  if (key_flag.notnilp()) {
    // Generate code to parse the key args. As with optionals, we don't do
    // defaulting yet.
    ql::list keynames;
    // Give each key a literal index. This is always a new one, to ensure that
    // they are consecutive even if the keyword appeared earlier in the literals.
    size_t first_key_index = 0;
    bool set_first_key_index = false;
    for (auto &it : keys) {
      if (!set_first_key_index) {
        first_key_index = context.new_literal_index(it._Keyword);
        set_first_key_index = true;
      } else
        context.new_literal_index(it._Keyword);
    }
    // now the actual instruction
    context.emit_parse_key_args(max_count, keys.size(), first_key_index, optkey_env->frameEnd(), aokp.notnilp());
    ql::list keyvars;
    for (auto &it : keys)
      keyvars << it._ArgTarget;
    optkey_env = optkey_env->bind_vars(keyvars.cons(), context);
    new_env = Lexenv_O::make(new_env->vars(), optkey_env->tags(),
                             optkey_env->blocks(), optkey_env->funs(),
                             optkey_env->notinlines(), optkey_env->frameEnd());
  }
  // Generate defaulting code for optional args, and bind them properly.
  if (optional_count > 0) {
    Label_sp optional_label = Label_O::make();
    Label_sp next_optional_label = Label_O::make();
    for (auto &it : optionals) {
      optional_label->contextualize(context);
      T_sp optional_var = it._ArgTarget;
      T_sp defaulting_form = it._Default;
      T_sp supplied_var = it._Sensor._ArgTarget;
      bool optional_special_p = special_binding_p(optional_var, specials, env);
      auto varinfo = gc::As_assert<LexicalVarInfo_sp>(var_info(optional_var, optkey_env));
      bool supplied_special_p = supplied_var.notnilp() && special_binding_p(supplied_var, specials, env);
      new_env = compile_optional_or_key_item(optional_var, defaulting_form, varinfo, supplied_var, next_optional_label,
                                             optional_special_p, supplied_special_p, context, new_env);
      ql::list debugbindings;
      if (optional_special_p)
        ++special_binding_count;
      else
        debugbindings << Cons_O::create(optional_var, varinfo);
      if (supplied_special_p)
        ++special_binding_count;
      else if (supplied_var.notnilp()) {
        auto svarinfo = gc::As_assert<LexicalVarInfo_sp>(var_info(supplied_var, new_env));
        debugbindings << Cons_O::create(supplied_var, svarinfo);
      }
      context.push_debug_info(BytecodeDebugVars_O::make(next_optional_label, end_label, debugbindings.cons()));
      optional_label = next_optional_label;
      next_optional_label = Label_O::make();
    }
    optional_label->contextualize(context);
  }
  // &rest
  if (restarg._ArgTarget.notnilp()) {
    Symbol_sp rest = restarg._ArgTarget;
    bool varestp = restarg.VaRest;
    if (varestp) {
      context.assemble1(vm_vaslistify_rest_args, max_count);
    } else {
      context.assemble1(vm_listify_rest_args, max_count);
    }
    context.assemble1(vm_set, new_env->frameEnd());
    new_env = new_env->bind1var(rest, context);
    auto lvinfo = gc::As_assert<LexicalVarInfo_sp>(new_env->variableInfo(rest));
    if (special_binding_p(rest, specials, env)) {
      context.assemble1(vm_ref, lvinfo->frameIndex());
      context.emit_special_bind(rest);
      ++special_binding_count;
      new_env = new_env->add_specials(Cons_O::createList(rest));
    } else {
      context.maybe_emit_encage(lvinfo);
      Label_sp begin_label = Label_O::make();
      begin_label->contextualize(context);
      context.push_debug_info(BytecodeDebugVars_O::make(begin_label, end_label, Cons_O::createList(Cons_O::create(rest, lvinfo))));
    }
  }
  // Generate defaulting code for key args, and special-bind them if necessary
  if (key_flag.notnilp()) {
    Label_sp key_label = Label_O::make();
    Label_sp next_key_label = Label_O::make();
    for (auto &it : keys) {
      key_label->contextualize(context);
      T_sp key_var = it._ArgTarget;
      T_sp defaulting_form = it._Default;
      T_sp supplied_var = it._Sensor._ArgTarget;
      bool key_special_p = special_binding_p(key_var, specials, env);
      auto varinfo = gc::As_assert<LexicalVarInfo_sp>(var_info(key_var, optkey_env));
      bool supplied_special_p = supplied_var.notnilp() && special_binding_p(supplied_var, specials, env);
      new_env = compile_optional_or_key_item(key_var, defaulting_form, varinfo, supplied_var, next_key_label, key_special_p,
                                             supplied_special_p, context, new_env);
      ql::list debugbindings;
      if (key_special_p)
        ++special_binding_count;
      else
        debugbindings << Cons_O::create(key_var, varinfo);
      if (supplied_special_p)
        ++special_binding_count;
      else if (supplied_var.notnilp()) {
        auto svarinfo = gc::As_assert<LexicalVarInfo_sp>(var_info(supplied_var, new_env));
        debugbindings << Cons_O::create(supplied_var, svarinfo);
      }
      context.push_debug_info(BytecodeDebugVars_O::make(next_key_label, end_label, debugbindings.cons()));
      key_label = next_key_label;
      next_key_label = Label_O::make();
    }
    key_label->contextualize(context);
  }
  // Generate aux and the body as a let*.
  // We repeat the special declarations so that let* will know the auxs are
  // special, and so that any free special declarations are processed.
  // Similarly for notinline.
  ql::list auxbinds;
  for (auto &it : auxs)
    auxbinds << Cons_O::createList(it._ArgTarget, it._Expression);
  T_sp specialdecl = Cons_O::create(cl::_sym_special, specials);
  T_sp notinlinedecl = Cons_O::create(cl::_sym_notinline, decl_notinlines(declares));
  T_sp declexpr = Cons_O::createList(cl::_sym_declare, specialdecl, notinlinedecl);
  T_sp lbody = Cons_O::create(declexpr, code);
  compile_letSTAR(auxbinds.cons(), lbody, new_env, context);
  // Finally, clean up any special bindings.
  context.emit_unbind(special_binding_count);
  end_label->contextualize(context);
}

// Compile the lambda expression in MODULE, returning the resulting CFUNCTION.
CL_DEFUN Cfunction_sp compile_lambda(T_sp lambda_list, List_sp body, Lexenv_sp env, Module_sp module) {
  List_sp declares = nil<T_O>();
  gc::Nilable<String_sp> docstring;
  List_sp code;
  List_sp specials;
  eval::extract_declares_docstring_code_specials(body, declares, true, docstring, code, specials);
  // Get a declared debug display lambda list if it exists.
  // If not declared, use the actual lambda list.
  // (This is useful for e.g. macros.)
  T_sp oll = extract_lambda_list_from_declares(declares, lambda_list);
  // Get a declared debug display name if it exists.
  // If it doesn't, use (lambda lambda-list...)
  T_sp name = extract_lambda_name_from_declares(declares);
  if (name.nilp())
    name = Cons_O::createList(cl::_sym_lambda, comp::lambda_list_for_name(oll));
  Cfunction_sp function =
      Cfunction_O::make(module, name, docstring, oll, core::_sym_STARcurrentSourcePosInfoSTAR->symbolValue());
  Context context(-1, nil<T_O>(), function);
  Lexenv_sp lenv = Lexenv_O::make(env->vars(), env->tags(), env->blocks(), env->funs(), env->notinlines(), 0);
  Fixnum_sp ind = module->cfunctions()->vectorPushExtend(function);
  function->setIndex(ind.unsafe_fixnum());
  // Stick the new function into the debug info.
  module->push_debug_info(function);
  // We pass the original body w/declarations to compile-with-lambda-list
  // so that it can do its own handling of specials, etc.
  compile_with_lambda_list(lambda_list, body, lenv, context);
  context.assemble0(vm_return);
  return function;
}

void compile_function(T_sp fnameoid, Lexenv_sp env, const Context ctxt) {
  bool mvp;
  switch (ctxt.receiving()) {
  case -1: mvp = true; break;
  case 0: return;
  default: mvp = false; break;
  }
  if (gc::IsA<Cons_sp>(fnameoid) && oCar(fnameoid) == cl::_sym_lambda) {
    Cfunction_sp fun = compile_lambda(oCadr(fnameoid), oCddr(fnameoid), env, ctxt.module());
    ComplexVector_T_sp closed = fun->closed();
    for (size_t i = 0; i < closed->length(); ++i) {
      ctxt.reference_lexical_info(gc::As_assert<LexicalVarInfo_sp>((*closed)[i]));
    }
    if (closed->length() == 0) // don't need to actually close
      ctxt.assemble1(vm_const, ctxt.literal_index(fun));
    else
      ctxt.assemble1(vm_make_closure, ctxt.literal_index(fun));
  } else { // ought to be a function name
    FunInfoV info = fun_info_v(fnameoid, env);
    if (std::holds_alternative<GlobalFunInfoV>(info)
        || std::holds_alternative<NoFunInfoV>(info)) {
      // TODO: Warn on unknown (nil)
      ctxt.assemble1(vm_fdefinition, ctxt.literal_index(fnameoid));
    } else if (std::holds_alternative<LocalFunInfoV>(info)) {
      LocalFunInfo_sp lfinfo = std::get<LocalFunInfoV>(info).info();
      LexicalVarInfo_sp lvinfo = gc::As_assert<LexicalVarInfo_sp>(lfinfo->funVar());
      ctxt.reference_lexical_info(lvinfo);
    } else
      // FIXME: e.g. #'with-open-file. needs better error.
      SIMPLE_ERROR("%s does not name a function", _rep_(fnameoid));
  }
  // Coerce to values if necessary.
  if (mvp)
    ctxt.assemble0(vm_pop);
}

void compile_flet(List_sp definitions, List_sp body, Lexenv_sp env, const Context ctxt) {
  ql::list fun_vars;
  ql::list funs;
  size_t fun_count = 0;
  size_t frame_slot = env->frameEnd(); // HACK FIXME
  for (auto cur : definitions) {
    Cons_sp definition = gc::As<Cons_sp>(oCar(cur));
    T_sp name = oCar(definition);
    Symbol_sp fun_var = cl__gensym(SimpleBaseString_O::make("FLET-FUN"));
    // Build up a lambda expression for the function.
    // FIXME: Probably need to parse declarations so they can refer
    // to the parameters.
    T_sp locally = Cons_O::create(cl::_sym_locally, oCddr(definition));
    T_sp block = Cons_O::createList(cl::_sym_block, core__function_block_name(name), locally);
    T_sp lambda = Cons_O::createList(cl::_sym_lambda, oCadr(definition), block);
    compile_function(lambda, env, Context(ctxt, 1));
    fun_vars << fun_var;
    funs << Cons_O::create(name, LocalFunInfo_O::make(LexicalVarInfo_O::make(frame_slot++, ctxt.cfunction())));
    ++fun_count;
  }
  ctxt.emit_bind(fun_count, env->frameEnd());
  // KLUDGEy - we could do this in one new environment
  Lexenv_sp new_env1 = env->bind_vars(fun_vars.cons(), ctxt);
  Lexenv_sp new_env2 = Lexenv_O::make(new_env1->vars(), new_env1->tags(), new_env1->blocks(),
                                      funs.dot(new_env1->funs()).cons(), new_env1->notinlines(), new_env1->frameEnd());
  compile_locally(body, new_env2, ctxt);
}

void compile_labels(List_sp definitions, List_sp body, Lexenv_sp env, const Context ctxt) {
  size_t fun_count = 0;
  ql::list funs;
  ql::list fun_vars;
  ql::list closures;
  size_t frame_start = env->frameEnd();
  size_t frame_slot = env->frameEnd();
  for (auto cur : definitions) {
    Cons_sp definition = gc::As<Cons_sp>(oCar(cur));
    T_sp name = oCar(definition);
    T_sp fun_var = cl__gensym(SimpleBaseString_O::make("LABELS-FUN"));
    fun_vars << fun_var;
    funs << Cons_O::create(name, LocalFunInfo_O::make(LexicalVarInfo_O::make(frame_slot++, ctxt.cfunction())));
    ++fun_count;
  }
  frame_slot = frame_start;
  Lexenv_sp new_env1 = env->bind_vars(fun_vars.cons(), ctxt);
  Lexenv_sp new_env2 = Lexenv_O::make(new_env1->vars(), new_env1->tags(), new_env1->blocks(),
                                      funs.dot(new_env1->funs()).cons(), new_env1->notinlines(), new_env1->frameEnd());
  for (auto cur : definitions) {
    Cons_sp definition = gc::As_unsafe<Cons_sp>(oCar(cur));
    T_sp name = oCar(definition);
    T_sp locally = Cons_O::create(cl::_sym_locally, oCddr(definition));
    T_sp block = Cons_O::createList(cl::_sym_block, core__function_block_name(name), locally);
    Cfunction_sp fun = compile_lambda(oCadr(definition), Cons_O::createList(block), new_env2, ctxt.module());
    size_t literal_index = ctxt.literal_index(fun);
    if (fun->closed()->length() == 0) // not a closure- easy
      ctxt.assemble1(vm_const, literal_index);
    else {
      closures << Cons_O::create(fun, clasp_make_fixnum(frame_slot));
      ctxt.assemble1(vm_make_uninitialized_closure, literal_index);
    }
    ++frame_slot;
  }
  ctxt.emit_bind(fun_count, frame_start);
  // Make the closures
  for (auto cur : gc::As_assert<List_sp>(closures.cons())) {
    Cfunction_sp cf = gc::As_unsafe<Cfunction_sp>(oCaar(cur));
    ComplexVector_T_sp closed = cf->closed();
    for (size_t i = 0; i < closed->length(); ++i) {
      LexicalVarInfo_sp info = gc::As_assert<LexicalVarInfo_sp>((*closed)[i]);
      ctxt.reference_lexical_info(info);
    }
    ctxt.assemble1(vm_initialize_closure, oCdar(cur).unsafe_fixnum());
  }
  compile_locally(body, new_env2, ctxt);
}

static void compile_setq_1(Symbol_sp var, T_sp valf, Lexenv_sp env, const Context ctxt) {
  VarInfoV info = var_info_v(var, env);
  if (std::holds_alternative<SymbolMacroVarInfoV>(info)) {
    Function_sp expander = std::get<SymbolMacroVarInfoV>(info).expander();
    T_sp expansion = expand_macro(expander, var, env);
    T_sp setform = Cons_O::createList(cl::_sym_setf, expansion, valf);
    compile_form(setform, env, ctxt);
  } else if (std::holds_alternative<NoVarInfoV>(info)
             || std::holds_alternative<SpecialVarInfoV>(info)) {
    // TODO: Warn on unknown variable
    compile_form(valf, env, Context(ctxt, 1));
    // If we need to return the new value, stick it into a new local
    // variable, do the set, then return the lexical variable.
    // We can't just read from the special, since some other thread may
    // alter it.
    size_t index = env->frameEnd();
    // but if we're not returning a value we don't actually have to do that crap.
    if (ctxt.receiving() != 0) {
      ctxt.assemble1(vm_set, index);
      ctxt.assemble1(vm_ref, index);
      // called for effect, i.e. to keep frame size correct
      // FIXME: This is super kludgey.
      env->bind1var(var, ctxt);
    }
    ctxt.assemble1(vm_symbol_value_set, ctxt.literal_index(var));
    if (ctxt.receiving() != 0) {
      ctxt.assemble1(vm_ref, index);
      if (ctxt.receiving() == -1) // need values
        ctxt.assemble0(vm_pop);
    }
  } else if (std::holds_alternative<LexicalVarInfoV>(info)) {
    LexicalVarInfo_sp lvinfo = std::get<LexicalVarInfoV>(info).info();
    bool localp = (lvinfo->funct() == ctxt.cfunction());
    size_t index = env->frameEnd();
    if (!localp)
      lvinfo->setClosedOverP(true);
    lvinfo->setSetP(true);
    compile_form(valf, env, Context(ctxt, 1));
    // Similar concerns to specials above (for closure variables)
    if (ctxt.receiving() != 0) {
      ctxt.assemble1(vm_set, index);
      ctxt.assemble1(vm_ref, index);
      env->bind1var(var, ctxt);
    }
    if (localp)
      ctxt.emit_lexical_set(lvinfo);
    else { // we already know we need a cell, so don't bother w/ a fixup.
      ctxt.assemble1(vm_closure, ctxt.closure_index(lvinfo));
      ctxt.assemble0(vm_cell_set);
    }
    if (ctxt.receiving() != 0) {
      ctxt.assemble1(vm_ref, index);
      if (ctxt.receiving() == -1)
        ctxt.assemble0(vm_pop);
    }
  } else UNREACHABLE();
}

void compile_setq(List_sp pairs, Lexenv_sp env, const Context ctxt) {
  if (pairs.nilp()) {
    // degenerate case
    if (ctxt.receiving() != 0)
      ctxt.assemble0(vm_nil);
  } else {
    do {
      Symbol_sp var = gc::As<Symbol_sp>(oCar(pairs));
      T_sp valf = oCadr(pairs);
      pairs = gc::As<List_sp>(oCddr(pairs));
      compile_setq_1(var, valf, env,
                     pairs.notnilp() ? Context(ctxt, 0) : ctxt);
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

void compile_if(T_sp cond, T_sp thn, T_sp els, Lexenv_sp env, const Context ctxt) {
  compile_form(cond, env, Context(ctxt, 1));
  Label_sp then_label = Label_O::make();
  Label_sp done_label = Label_O::make();
  ctxt.emit_jump_if(then_label);
  compile_form(els, env, ctxt);
  ctxt.emit_jump(done_label);
  then_label->contextualize(ctxt);
  compile_form(thn, env, ctxt);
  done_label->contextualize(ctxt);
}

static bool go_tag_p(T_sp object) { return object.fixnump() || gc::IsA<Integer_sp>(object) || gc::IsA<Symbol_sp>(object); }

void compile_tagbody(List_sp statements, Lexenv_sp env, const Context ctxt) {
  List_sp new_tags = gc::As_assert<List_sp>(env->tags());
  Symbol_sp tagbody_dynenv = cl__gensym(SimpleBaseString_O::make("TAG-DYNENV"));
  Lexenv_sp nenv = env->bind1var(tagbody_dynenv, ctxt);
  auto dynenv_info = gc::As_assert<LexicalVarInfo_sp>(nenv->variableInfo(tagbody_dynenv));
  Context stmt_ctxt(ctxt, dynenv_info);
  for (auto cur : statements) {
    T_sp statement = oCar(cur);
    if (go_tag_p(statement))
      new_tags = Cons_O::create(Cons_O::create(statement, Cons_O::create(dynenv_info, Label_O::make())), new_tags);
  }
  Lexenv_sp nnenv = Lexenv_O::make(nenv->vars(), new_tags, nenv->blocks(), nenv->funs(), nenv->notinlines(), nenv->frameEnd());
  // Bind the dynamic environment (or just save the stack pointer).
  ctxt.emit_entry_or_save_sp(dynenv_info);
  // Compile the body, emitting the tag destination labels.
  for (auto cur : statements) {
    T_sp statement = oCar(cur);
    if (go_tag_p(statement)) {
      T_sp info = core__alist_assoc_eql(gc::As<Cons_sp>(nnenv->tags()), statement);
      Label_sp lab = gc::As_assert<Label_sp>(oCddr(info));
      lab->contextualize(ctxt);
    } else
      compile_form(statement, nnenv, Context(stmt_ctxt, 0));
  }
  ctxt.maybe_emit_entry_close(dynenv_info);
  // return nil if we really have to
  if (ctxt.receiving() != 0) {
    ctxt.assemble0(vm_nil);
    if (ctxt.receiving() == -1)
      ctxt.assemble0(vm_pop);
  }
}

static void compile_exit(LexicalVarInfo_sp exit_de, Label_sp exit,
                         const Context context) {
  if (exit_de->funct() == context.cfunction()) { // local return
    // Unwind interposed dynenvs.
    for (auto cur : context.dynenv()) {
      T_sp interde = oCar(cur);
      if (interde == exit_de) break;
      if (gc::IsA<LexicalVarInfo_sp>(interde))
        context.maybe_emit_entry_close(gc::As_unsafe<LexicalVarInfo_sp>(interde));
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
  T_sp tags = env->tags();
  if (tags.consp()) {
    // tags must be a cons now
    T_sp pair = core__alist_assoc_eql(gc::As<Cons_sp>(tags), tag);
    if (pair.consp()) {
      Cons_sp rpair = gc::As_assert<Cons_sp>(CONS_CDR(pair));
      compile_exit(gc::As_assert<LexicalVarInfo_sp>(CONS_CAR(rpair)),
                   gc::As_assert<Label_sp>(CONS_CDR(rpair)),
                   ctxt);
      return;
    }
  }
  SIMPLE_ERROR("The GO tag %s does not exist.", _rep_(tag));
}

void compile_block(Symbol_sp name, List_sp body, Lexenv_sp env, const Context ctxt) {
  Symbol_sp block_dynenv = cl__gensym(SimpleBaseString_O::make("BLOCK-DYNENV"));
  Lexenv_sp nenv = env->bind1var(block_dynenv, ctxt);
  auto dynenv_info = gc::As_assert<LexicalVarInfo_sp>(nenv->variableInfo(block_dynenv));
  Label_sp label = Label_O::make();
  Label_sp normal_label = Label_O::make();
  // Bind the dynamic environment or save SP.
  ctxt.emit_entry_or_save_sp(dynenv_info);
  Cons_sp new_pair = Cons_O::create(name, Cons_O::create(dynenv_info, label));
  Lexenv_sp nnenv = Lexenv_O::make(nenv->vars(), nenv->tags(), Cons_O::create(new_pair, nenv->blocks()), nenv->funs(),
                                   nenv->notinlines(), nenv->frameEnd());
  // We force single values into multiple so that we can uniformly PUSH afterward.
  // Specifically: if we're returning 0 values, there's no problem anyway.
  // If we're returning multiple values, the local and nonlocal returns just
  // store into the multiple values, so no problem there.
  // If we're returning exactly one value, the nonlocal just pushes one, and
  // the nonlocal stores into the MV which is then vm_push'd to the stack.
  compile_progn(body, nnenv, Context(ctxt, dynenv_info));
  bool r1p = ctxt.receiving() == 1;
  if (r1p)
    ctxt.emit_jump(normal_label);
  label->contextualize(ctxt);
  // When we need 1 value, we have to make sure that the
  // "exceptional" case pushes a single value onto the stack.
  if (r1p) {
    ctxt.assemble0(vm_push);
    normal_label->contextualize(ctxt);
  }
  ctxt.maybe_emit_entry_close(dynenv_info);
}

void compile_return_from(T_sp name, T_sp valuef, Lexenv_sp env, const Context ctxt) {
  compile_form(valuef, env, Context(ctxt, -1));
  T_sp blocks = env->blocks();
  if (blocks.consp()) {
    // blocks must be a cons now
    T_sp pair = core__alist_assoc_eq(gc::As_unsafe<Cons_sp>(blocks), name);
    if (pair.consp()) {
      Cons_sp rpair = gc::As_assert<Cons_sp>(CONS_CDR(pair));
      compile_exit(gc::As_assert<LexicalVarInfo_sp>(CONS_CAR(rpair)),
                   gc::As_assert<Label_sp>(CONS_CDR(rpair)),
                   ctxt);
      return;
    }
  }
  SIMPLE_ERROR("The block %s does not exist.", _rep_(name));
}

// catch, throw, and progv are actually handled by macros right now,
// so these aren't used, but maybe will be in the fture.
void compile_catch(T_sp tag, List_sp body, Lexenv_sp env, const Context ctxt) {
  compile_form(tag, env, Context(ctxt, 1));
  Label_sp target = Label_O::make();
  ctxt.emit_catch(target);
  // FIXME: maybe should be a T context to match throw
  compile_progn(body, env, ctxt);
  ctxt.assemble0(vm_catch_close);
  target->contextualize(ctxt);
}

void compile_throw(T_sp tag, T_sp rform, Lexenv_sp env, const Context ctxt) {
  compile_form(tag, env, Context(ctxt, 1));
  compile_form(rform, env, Context(ctxt, -1));
  ctxt.assemble0(vm_throw);
}

void compile_progv(T_sp syms, T_sp vals, List_sp body, Lexenv_sp env, const Context ctxt) {
  compile_form(syms, env, Context(ctxt, 1));
  compile_form(vals, env, Context(ctxt, 1));
  ctxt.assemble0(vm_progv);
  compile_progn(body, env, ctxt);
  ctxt.emit_unbind(1);
}

void compile_multiple_value_call(T_sp fform, List_sp aforms, Lexenv_sp env, const Context ctxt) {
  // Compile the function. Coerce it as a designator.
  // TODO: When the fform is a #'foo form we could skip coercion.
  compile_function(core::_sym_coerce_fdesignator, env, Context(ctxt, 1));
  compile_form(fform, env, Context(ctxt, 1));
  Context(ctxt, 1).emit_call(1);
  // Compile the arguments
  T_sp first = oCar(aforms);
  List_sp rest = gc::As<List_sp>(oCdr(aforms));
  compile_form(first, env, Context(ctxt, -1));
  if (rest.notnilp()) {
    ctxt.assemble0(vm_push_values);
    for (auto cur : rest) {
      compile_form(oCar(cur), env, Context(ctxt, -1));
      ctxt.assemble0(vm_append_values);
    }
    ctxt.assemble0(vm_pop_values);
  }
  ctxt.emit_mv_call();
}

void compile_multiple_value_prog1(T_sp fform, List_sp forms, Lexenv_sp env, const Context ctxt) {
  compile_form(fform, env, ctxt);
  // We only need to actually save anything with all-values returns.
  if (ctxt.receiving() == -1)
    ctxt.assemble0(vm_push_values);
  for (auto cur : forms)
    compile_form(oCar(cur), env, Context(ctxt, 0));
  if (ctxt.receiving() == -1)
    ctxt.assemble0(vm_pop_values);
}

// Compile a call, where the function is already on the stack.
static void compile_call(T_sp args, Lexenv_sp env, const Context context) {
  // Compile the arguments.
  size_t argcount = 0;
  for (auto cur : gc::As<List_sp>(args)) {
    ++argcount;
    compile_form(oCar(cur), env, Context(context, 1));
  }
  // generate the call
  context.emit_call(argcount);
}

void compile_load_time_value(T_sp form, T_sp tread_only_p,
                             Lexenv_sp env, const Context context) {
  // load-time-value forms are compiled by putting their information into
  // a slot in the cmodule. This is so that (this part of) the compiler can
  // be used uniformly for eval, compile, or compile-file. It is slightly
  // inefficient for the former two cases, compared to evaluating forms
  // immediately, but load-time-value is not exactly heavily used.
  // The standard specifies the behavior when read-only-p is a literal t or
  // nil, and nothing else.
  bool read_only_p;
  if (tread_only_p.nilp()) read_only_p = false;
  else if (tread_only_p == cl::_sym_T_O) read_only_p = true;
  // FIXME: Better error
  else SIMPLE_ERROR("load-time-value read-only-p is not T or NIL: %s"
                    , _rep_(tread_only_p));
  
  auto ltv = LoadTimeValueInfo_O::make(form, read_only_p);
  // Add the LTV to the cmodule.
  size_t ind = context.new_literal_index(ltv);
  // With that done, we basically just need to compile a literal load.
  // (Note that we do always need to register the LTV, since it may have
  //  some weird side effect. We could hypothetically save some space by
  //  not allocating a spot in the constants if the value isn't actually
  //  used, but that's a very marginal case.)
  switch (context.receiving()) {
  case 0: break; // no value required, so compile nothing
  case 1: context.assemble1(vm_const, ind); break;
  case -1: // all values
      context.assemble1(vm_const, ind);
      context.assemble0(vm_pop);
      break;
  default:
      SIMPLE_ERROR("BUG: Don't know how to compile LTV returning %" PFixnum " values", context.receiving());
  }
}

static T_sp symbol_macrolet_bindings(Lexenv_sp menv, List_sp bindings, T_sp vars) {
  for (auto cur : bindings) {
    T_sp binding = oCar(cur);
    Symbol_sp name = gc::As<Symbol_sp>(oCar(binding));
    T_sp expansion = oCadr(binding);
    // FIXME: Compiling a new function for the expander is overkill
    T_sp formv = cl__gensym(SimpleBaseString_O::make("FORM"));
    T_sp envv = cl__gensym(SimpleBaseString_O::make("ENV"));
    T_sp lexpr = Cons_O::createList(cl::_sym_lambda, Cons_O::createList(formv, envv),
                                    Cons_O::createList(cl::_sym_declare, Cons_O::createList(cl::_sym_ignore, formv, envv)),
                                    Cons_O::createList(cl::_sym_quote, expansion));
    GlobalBytecodeSimpleFun_sp expander = bytecompile(lexpr, menv);
    SymbolMacroVarInfo_sp info = SymbolMacroVarInfo_O::make(expander);
    vars = Cons_O::create(Cons_O::create(name, info), vars);
  }
  return vars;
}

void compile_symbol_macrolet(List_sp bindings, List_sp body, Lexenv_sp env, const Context context) {
  T_sp vars = symbol_macrolet_bindings(env->macroexpansion_environment(), bindings, env->vars());
  Lexenv_sp nenv = Lexenv_O::make(vars, env->tags(), env->blocks(), env->funs(), env->notinlines(), env->frameEnd());
  compile_locally(body, nenv, context);
}

// Given a macroexpansion environment, a alist of macrolet bindings, and the
// funs() of a lexenv, return new funs() with macro infos prepended.
static T_sp macrolet_bindings(Lexenv_sp menv, List_sp bindings, T_sp funs) {
  for (auto cur : bindings) {
    T_sp binding = oCar(cur);
    T_sp name = oCar(binding);
    T_sp lambda_list = oCadr(binding);
    T_sp body = oCddr(binding);
    T_sp eform = eval::funcall(ext::_sym_parse_macro, name, lambda_list, body, menv);
    GlobalBytecodeSimpleFun_sp expander = bytecompile(eform, menv);
    LocalMacroInfo_sp info = LocalMacroInfo_O::make(expander);
    funs = Cons_O::create(Cons_O::create(name, info), funs);
  }
  return funs;
}

void compile_macrolet(List_sp bindings, List_sp body, Lexenv_sp env, const Context context) {
  T_sp funs = macrolet_bindings(env->macroexpansion_environment(), bindings, env->funs());
  Lexenv_sp nenv = Lexenv_O::make(env->vars(), env->tags(), env->blocks(), funs, env->notinlines(), env->frameEnd());
  compile_locally(body, nenv, context);
}

void compile_funcall(T_sp callee, List_sp args, Lexenv_sp env, const Context context) {
  compile_form(callee, env, Context(context, 1));
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
  else if (head == cl::_sym_the) // skip
    compile_form(oCadr(rest), env, context);
  // extension
  else if (head == cleavirPrimop::_sym_funcall)
    compile_funcall(oCar(rest), oCdr(rest), env, context);
  else if (head == cleavirPrimop::_sym_eq) {
    // KLUDGE: Compile a call to EQ.
    // Better would be to use the EQ opcode. Better than that would be
    // eliminating the special operator entirely and working with the
    // function instead.
    compile_function(cl::_sym_eq, env, Context(context, 1));
    compile_call(rest, env, context);
  } else if (head == cleavirPrimop::_sym_typeq) {
    // KLUDGE: call to typep.
    T_sp type = oCadr(rest);
    if (type == cl::_sym_cons) {
      compile_function(cl::_sym_consp, env, Context(context, 1));
      compile_form(oCar(rest), env, Context(context, 1));
      context.emit_call(1);
    } else if (type == cl::_sym_symbol) {
      compile_function(cl::_sym_symbolp, env, Context(context, 1));
      compile_form(oCar(rest), env, Context(context, 1));
      context.emit_call(1);
    } else {
      compile_function(cl::_sym_typep, env, Context(context, 1));
      compile_form(oCar(rest), env, Context(context, 1));
      compile_literal(oCadr(rest), env, Context(context, 1));
      context.emit_call(2);
    }
  }
  // not a special form
  else {
    if (gc::IsA<Symbol_sp>(head)) {
      Symbol_sp shead = gc::As_unsafe<Symbol_sp>(head);
      FunInfoV info = fun_info_v(head, env);
      if (std::holds_alternative<GlobalMacroInfoV>(info)) {
        Function_sp expander = std::get<GlobalMacroInfoV>(info).expander();
        T_sp expansion = expand_macro(expander, Cons_O::create(head, rest), env);
        compile_form(expansion, env, context);
      } else if (std::holds_alternative<LocalMacroInfoV>(info)) {
        Function_sp expander = std::get<LocalMacroInfoV>(info).expander();
        T_sp expansion = expand_macro(expander, Cons_O::create(head, rest), env);
        compile_form(expansion, env, context);
      } else if (std::holds_alternative<GlobalFunInfoV>(info)) {
        T_sp cmexpander = std::get<GlobalFunInfoV>(info).cmexpander();
        if (cmexpander.notnilp() && !env->notinlinep(head)
            // KLUDGE: The TYPEP compiler macro expands into TYPEQ, which
            // causes infinite recursion as we implement TYPEQ as TYPEP.
            // Better solution would be to have TYPEP expand into
            // simpler TYPEPs, I'm thinking.
            // CASE, similarly, expands into PRIMOP:TYPEQ.
            && (head != cl::_sym_typep) && (head != cl::_sym_case)) {
          // Compiler macroexpand
          T_sp form = Cons_O::create(head, rest);
          T_sp expansion = expand_macro(gc::As<Function_sp>(cmexpander), form, env);
          if (expansion != form) {
            compile_form(expansion, env, context);
            return;
          }
        } // no compiler macro, or expansion declined: call
        compile_function(head, env, Context(context, 1));
        compile_call(rest, env, context);
      } else if (std::holds_alternative<LocalFunInfoV>(info)
                 || std::holds_alternative<NoFunInfoV>(info)) {
        // unknown function warning handled by compile-function (eventually)
        // note we do a double lookup of the fun info,
        // which is inefficient in the compiler (doesn't affect generated code)
        compile_function(head, env, Context(context, 1));
        compile_call(rest, env, context);
      } else UNREACHABLE();
    } else if (gc::IsA<Cons_sp>(head) && (oCar(head) == cl::_sym_lambda)) {
      // Lambda form
      compile_function(head, env, Context(context, 1));
      compile_call(rest, env, context);
    } else
      SIMPLE_ERROR("Illegal combination head: %s rest: %s", _rep_(head), _rep_(rest));
  }
}

void compile_form(T_sp form, Lexenv_sp env, const Context context) {
  // Code walk if we're doing that
  if (_sym_STARcodeWalkerSTAR->boundP() && _sym_STARcodeWalkerSTAR->symbolValue().notnilp())
    form = eval::funcall(_sym_STARcodeWalkerSTAR->symbolValue(), form, env);
  // Compile
  if (gc::IsA<Symbol_sp>(form))
    compile_symbol(gc::As_unsafe<Symbol_sp>(form), env, context);
  else if (form.consp())
    compile_combination(oCar(form), oCdr(form), env, context);
  else
    compile_literal(form, env, context);
}

CL_LAMBDA(module lambda-expression &optional (env (cmp::make-null-lexical-environment)));
CL_DOCSTRING(R"dx(Compile the given lambda-expression into an existing module. Return a handle to it.)dx");
CL_DEFUN Cfunction_sp bytecompile_into(Module_sp module, T_sp lambda_expression,
                                       Lexenv_sp env) {
  if (!gc::IsA<Cons_sp>(lambda_expression) || (oCar(lambda_expression) != cl::_sym_lambda))
    SIMPLE_ERROR("bytecompiler passed a non-lambda-expression: %s", _rep_(lambda_expression));
  T_sp lambda_list = oCadr(lambda_expression);
  T_sp body = oCddr(lambda_expression);
  return compile_lambda(lambda_list, body, env, module);
}

CL_LAMBDA(lambda-expression &optional (env (cmp::make-null-lexical-environment)));
CL_DEFUN GlobalBytecodeSimpleFun_sp bytecompile(T_sp lambda_expression, Lexenv_sp env) {
  Module_sp module = Module_O::make();
  Cfunction_sp cf = bytecompile_into(module, lambda_expression, env);
  return cf->link_function(Cons_O::create(lambda_expression, env));
}

static Lexenv_sp coerce_lexenv_desig(T_sp env) {
  if (env.nilp())
    return Lexenv_O::make(nil<T_O>(), nil<T_O>(), nil<T_O>(), nil<T_O>(), nil<T_O>(), 0);
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

T_sp LoadTimeValueInfo_O::eval() {
  return cmp__bytecode_implicit_compile_form(this->form(),
                                             make_null_lexical_environment());
}

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
  Lexenv_sp inner1 = env->add_specials(specials);
  Lexenv_sp inner2 = env->add_notinlines(decl_notinlines(declares));
  return bytecode_toplevel_progn(code, inner2);
}

CL_DEFUN T_mv bytecode_toplevel_macrolet(List_sp bindings, List_sp body, Lexenv_sp env) {
  // FIXME: We can maybe skip macroexpansion_environment,
  // assuming bytecode_toplevel_eval was originally actually called
  // with an empty lexenv as it ought to be.
  T_sp funs = macrolet_bindings(env->macroexpansion_environment(), bindings, env->funs());
  Lexenv_sp nenv = Lexenv_O::make(env->vars(), env->tags(), env->blocks(), funs, env->notinlines(), env->frameEnd());
  return bytecode_toplevel_locally(body, nenv);
}

CL_DEFUN T_mv bytecode_toplevel_symbol_macrolet(List_sp bindings, List_sp body, Lexenv_sp env) {
  T_sp vars = symbol_macrolet_bindings(env->macroexpansion_environment(), bindings, env->vars());
  Lexenv_sp nenv = Lexenv_O::make(vars, env->tags(), env->blocks(), env->funs(), env->notinlines(), env->frameEnd());
  return bytecode_toplevel_locally(body, nenv);
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
