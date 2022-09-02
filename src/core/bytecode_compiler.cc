#include <clasp/core/bytecode_compiler.h>
#include <clasp/core/virtualMachine.h>
#include <clasp/core/evaluator.h> // af_interpreter_lookup_macro
#include <clasp/core/sysprop.h> // core__get_sysprop
#include <algorithm> // max

namespace comp {

using namespace core;

T_sp Lexenv_O::variableInfo(T_sp varname) {
  T_sp vars = this->vars();
  if (vars.nilp()) return vars;
  else {
    T_sp pair = gc::As<Cons_sp>(vars)->assoc(varname, nil<T_O>(),
                                             cl::_sym_eq, nil<T_O>());
    if (pair.nilp()) return pair;
    else return oCdr(pair);
  }
}

T_sp Lexenv_O::lookupSymbolMacro(T_sp sname) {
  T_sp info = this->variableInfo(sname);
  if (gc::IsA<SymbolMacroVarInfo_sp>(info))
    return gc::As_unsafe<SymbolMacroVarInfo_sp>(info)->expander();
  else if (info.notnilp()) { // global?
    T_mv result = core__get_sysprop(sname, ext::_sym_symbolMacro);
    if (gc::As<T_sp>(result.valueGet_(1)).notnilp()) {
      return result;
    } else return nil<T_O>();
  } else return nil<T_O>();
}

T_sp Lexenv_O::functionInfo(T_sp funname) {
  T_sp funs = this->funs();
  if (funs.nilp()) return funs;
  else {
    T_sp pair = gc::As<Cons_sp>(funs)->assoc(funname, nil<T_O>(),
                                             cl::_sym_equal, nil<T_O>());
    if (pair.nilp()) return pair;
    else return oCdr(pair);
  }
}

T_sp Lexenv_O::lookupMacro(T_sp macroname) {
  T_sp info = this->functionInfo(macroname);
  if (gc::IsA<GlobalMacroInfo_sp>(info))
    return gc::As_unsafe<GlobalMacroInfo_sp>(info)->expander();
  else if (gc::IsA<LocalMacroInfo_sp>(info))
    return gc::As_unsafe<LocalMacroInfo_sp>(info)->expander();
  else if (info.nilp()) // could be global
    return af_interpreter_lookup_macro(macroname, nil<T_O>());
  else return nil<T_O>();
}

Lexenv_sp Lexenv_O::bind_vars(List_sp vars, Context_sp ctxt) {
  size_t frame_start = this->frameEnd();
  size_t frame_end = frame_start
    + (vars.nilp() ? 0 : vars.unsafe_cons()->length());
  Cfunction_sp cf = ctxt->cfunction();

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
  return Lexenv_O::make(new_vars, this->tags(), this->blocks(),
                        this->funs(), frame_end);
}

Lexenv_sp Lexenv_O::add_specials(List_sp vars) {
  List_sp new_vars = this->vars();
  for (auto cur : vars) {
    Symbol_sp var = oCar(cur);
    auto info = SpecialVarInfo_O::make();
    Cons_sp pair = Cons_O::create(var, info);
    new_vars = Cons_O::create(pair, new_vars);
  }
  return Lexenv_O::make(new_vars, this->tags(), this->blocks(),
                        this->funs(), this->frameEnd());
}

Lexenv_sp Lexenv_O::macroexpansion_environment() {
  ql::list new_vars;
  for (auto cur : (List_sp)(this->vars())) {
    T_sp pair = oCar(cur);
    T_sp info = oCdr(pair);
    if (gc::IsA<ConstantVarInfo_sp>(info)
        || gc::IsA<SymbolMacroVarInfo_sp>(info))
      new_vars << pair;
  }
  ql::list new_funs;
  for (auto cur : (List_sp)(this->funs())) {
    T_sp pair = oCar(cur);
    T_sp info = oCdr(pair);
    if (gc::IsA<GlobalMacroInfo_sp>(info)
        || gc::IsA<LocalMacroInfo_sp>(info))
      new_funs << pair;
  }
  return Lexenv_O::make(new_vars.cons(), nil<T_O>(), nil<T_O>(),
                        new_funs.cons(), 0);
}

CL_LAMBDA(context opcode &rest operands)
CL_DEFUN void cmp__assemble(Context_sp context,
                            uint8_t opcode, List_sp operands) {
  Cfunction_sp func = context->cfunction();
  ComplexVector_byte8_t_sp bytecode = func->bytecode();
  bytecode->vectorPushExtend(opcode);
  for (auto cur : operands) {
    bytecode->vectorPushExtend(clasp_to_integral<uint8_t>(oCar(cur)));
  }
}

CL_LAMBDA(code position &rest values)
CL_DEFUN void cmp__assemble_into(SimpleVector_byte8_t_sp code,
                                 size_t position, List_sp values) {
  for (auto cur : values)
    (*code)[position++] = clasp_to_integral<uint8_t>(oCar(cur));
}

CL_LAMBDA(context opcode &rest operands)
CL_DEFUN void cmp__assemble_maybe_long(Context_sp context,
                                       uint8_t opcode, List_sp operands) {
  Cfunction_sp func = context->cfunction();
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
      bytecode->vectorPushExtend(operand >> 8); // high
    }
  } else { // normal/short
    bytecode->vectorPushExtend(opcode);
    for (auto cur : operands)
      bytecode->vectorPushExtend(clasp_to_integral<uint8_t>(oCar(cur)));
  }
}

CL_DEFUN T_sp cmp__var_info(Symbol_sp sym, Lexenv_sp env) {
  // Local?
  T_sp info = env->variableInfo(sym);
  if (info.notnilp()) return info;
  // Constant?
  // Constants are also specialP, so we have to check this first.
  if (cl__keywordp(sym) || sym->getReadOnly())
    return ConstantVarInfo_O::make(sym->symbolValue());
  // Globally special?
  if (sym->specialP())
    return SpecialVarInfo_O::make();
  // Global symbol macro?
  T_mv symmac = core__get_sysprop(sym, ext::_sym_symbolMacro);
  if (gc::As<T_sp>(symmac.valueGet_(1)).notnilp())
    return SymbolMacroVarInfo_O::make(symmac);
  // Unknown.
  return nil<T_O>();
}

CL_DEFUN T_sp cmp__fun_info(T_sp name, Lexenv_sp env) {
  // Local?
  T_sp info = env->functionInfo(name);
  if (info.notnilp()) return info;
  // Split into setf and not versions.
  if (name.consp()) {
    List_sp cname = name;
    if (oCar(cname) == cl::_sym_setf) {
      // take care of (setf . bar) or (setf bar foo) or (setf bar .foo)
      // so don't go directly for the cadr
      T_sp dname = oCdr(cname);
      if (dname.consp()) {
        Symbol_sp fname = gc::As<Symbol_sp>(oCar(dname));
        if (fname.notnilp() && oCdr(dname).nilp()) {
          if (!fname->fboundp_setf())
            return nil<T_O>();
          if (fname->macroP())
            return GlobalMacroInfo_O::make(fname->getSetfFdefinition());
          else
            return GlobalFunInfo_O::make();
        }
      }
    }
    // Bad function name.
    return nil<T_O>();
  } else {
    Symbol_sp fname = gc::As<Symbol_sp>(name);
    if (!fname->fboundp()) return nil<T_O>();
    else if (fname->macroP())
      return GlobalMacroInfo_O::make(fname->symbolFunction());
    else
      return GlobalFunInfo_O::make();
  }
}

size_t Context_O::literal_index(T_sp literal) {
  ComplexVector_T_sp literals = this->cfunction()->module()->literals();
  // FIXME: Smarter POSITION
  for (size_t i = 0; i < literals->length(); ++i)
    if ((*literals)[i] == literal)
      return i;
  Fixnum_sp nind = literals->vectorPushExtend(literal);
  return nind.unsafe_fixnum();
}

size_t Context_O::closure_index(T_sp info) {
  ComplexVector_T_sp closed = this->cfunction()->closed();
  for (size_t i = 0; i < closed->length(); ++i)
    if ((*closed)[i] == info)
      return i;
  Fixnum_sp nind = closed->vectorPushExtend(info);
  return nind.unsafe_fixnum();
}

void Context_O::emit_jump(Label_sp label) {
  ControlLabelFixup_O::make(label, vm_jump_8, vm_jump_16, vm_jump_24)->contextualize(this->asSmartPtr());
}

void Context_O::emit_jump_if(Label_sp label) {
  ControlLabelFixup_O::make(label, vm_jump_if_8, vm_jump_if_16, vm_jump_if_24)->contextualize(this->asSmartPtr());
}

void Context_O::emit_exit(Label_sp label) {
  ControlLabelFixup_O::make(label, vm_exit_8, vm_exit_16, vm_exit_24)->contextualize(this->asSmartPtr());
}

void Context_O::emit_catch(Label_sp label) {
  ControlLabelFixup_O::make(label, vm_catch_8, vm_catch_16, 0)->contextualize(this->asSmartPtr());
}

void Context_O::emit_jump_if_supplied(Label_sp label, size_t ind) {
  JumpIfSuppliedFixup_O::make(label, ind)->contextualize(this->asSmartPtr());
}

void Context_O::maybe_emit_make_cell(LexicalVarInfo_sp info) {
  LexRefFixup_O::make(info, vm_make_cell)->contextualize(this->asSmartPtr());
}

void Context_O::maybe_emit_cell_ref(LexicalVarInfo_sp info) {
  LexRefFixup_O::make(info, vm_cell_ref)->contextualize(this->asSmartPtr());
}

// FIXME: This is probably a good candidate for a specialized
// instruction.
void Context_O::maybe_emit_encage(LexicalVarInfo_sp info) {
  EncageFixup_O::make(info)->contextualize(this->asSmartPtr());
}

void Context_O::emit_lexical_set(LexicalVarInfo_sp info) {
  LexSetFixup_O::make(info)->contextualize(this->asSmartPtr());
}

void Context_O::emit_parse_key_args(size_t max_count, size_t key_count,
                                    size_t keystart, size_t indx, bool aokp) {
  ComplexVector_byte8_t_sp bytecode = this->cfunction()->bytecode();
  if ((max_count < (1<<8)) && (key_count < (1<<8))
      && (keystart < (1<<8)) && (indx < (1<<8))) {
    bytecode->vectorPushExtend(vm_parse_key_args);
    bytecode->vectorPushExtend(max_count);
    bytecode->vectorPushExtend(key_count | (aokp ? 0x80 : 0));
    bytecode->vectorPushExtend(keystart);
    bytecode->vectorPushExtend(indx);
  } else if ((max_count < (1<<16)) && (key_count < (1<<16))
             && (keystart < (1<<16)) && (indx < (1<<16))) {
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
  } else SIMPLE_ERROR("Bytecode compiler limit reached: keyarg indices too large: %zu %zu %zu %zu", max_count, key_count, keystart, indx);
}

void Context_O::assemble0(uint8_t opcode) {
  this->cfunction()->bytecode()->vectorPushExtend(opcode);
}

void Context_O::assemble1(uint8_t opcode, size_t operand) {
  ComplexVector_byte8_t_sp bytecode = this->cfunction()->bytecode();
  if (operand < (1<<8)) {
    bytecode->vectorPushExtend(opcode);
    bytecode->vectorPushExtend(operand);
  } else if (operand < (1<<16)) {
    bytecode->vectorPushExtend(vm_long);
    bytecode->vectorPushExtend(opcode);
    bytecode->vectorPushExtend(operand & 0xff);
    bytecode->vectorPushExtend(operand >> 8);
  } else SIMPLE_ERROR("Bytecode compiler limit reached: operand %zu too large", operand);
}

void Context_O::assemble2(uint8_t opcode, size_t operand1, size_t operand2) {
  ComplexVector_byte8_t_sp bytecode = this->cfunction()->bytecode();
  if ((operand1 < (1<<8)) && (operand2 < (1<<8))) {
    bytecode->vectorPushExtend(opcode);
    bytecode->vectorPushExtend(operand1);
    bytecode->vectorPushExtend(operand2);
  } else if ((operand1 < (1<<16)) && (operand2 < (1<<16))) {
    bytecode->vectorPushExtend(vm_long);
    bytecode->vectorPushExtend(opcode);
    bytecode->vectorPushExtend(operand1 & 0xff);
    bytecode->vectorPushExtend(operand1 >> 8);
    bytecode->vectorPushExtend(operand2 & 0xff);
    bytecode->vectorPushExtend(operand2 >> 8);
    } else SIMPLE_ERROR("Bytecode compiler limit reached: operands %zu %zu too large", operand1, operand2);
}

void Context_O::emit_bind(size_t count, size_t offset) {
  switch (count) {
  case 1: this->assemble1(vm_set, offset); break;
  case 0: break;
  default: this->assemble2(vm_bind, count, offset); break;
  }
}

void Context_O::emit_call(size_t argcount) {
  T_sp receiving = this->receiving();
  if (receiving.fixnump()) {
    gc::Fixnum freceiving = receiving.unsafe_fixnum();
    switch (freceiving) {
    case 1: this->assemble1(vm_call_receive_one, argcount); break;
    case 0: this->assemble1(vm_call, argcount); break;
    default:
        this->assemble2(vm_call_receive_fixed, argcount, freceiving);
        break;
    }
  } else this->assemble1(vm_call, argcount); // must be T
}

void Context_O::emit_mv_call() {
  T_sp receiving = this->receiving();
  if (receiving.fixnump()) {
    gc::Fixnum freceiving = receiving.unsafe_fixnum();
    switch (freceiving) {
    case 1: this->assemble0(vm_mv_call_receive_one); break;
    case 0: this->assemble0(vm_mv_call); break;
    default:
        this->assemble1(vm_mv_call_receive_fixed, freceiving);
        break;
    }
  } else this->assemble0(vm_mv_call); // must be T
}

void Context_O::emit_special_bind(Symbol_sp sym) {
  this->assemble1(vm_special_bind, this->literal_index(sym));
}

void Context_O::emit_unbind(size_t count) {
  for (size_t i = 0; i < count; ++i)
    this->assemble0(vm_unbind);
}

size_t Annotation_O::module_position() {
  return this->pposition() + gc::As<Cfunction_sp>(this->cfunction())->pposition();
}

void Label_O::contextualize(Context_sp ctxt) {
  Cfunction_sp cfunction = ctxt->cfunction();
  this->setPosition(cfunction->bytecode()->length());
  this->setCfunction(cfunction);
  Fixnum_sp nind = cfunction->annotations()->vectorPushExtend(this->asSmartPtr());
  this->setIndex(nind.unsafe_fixnum());
}

void Fixup_O::contextualize(Context_sp ctxt) {
  Cfunction_sp cfunction = ctxt->cfunction();
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

ptrdiff_t LabelFixup_O::delta() {
  return this->label()->module_position() - this->module_position();
}

void ControlLabelFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  size_t size = this->size();
  switch (size) {
  case 2: (*code)[position] = this->_opcode8; break;
  case 3: (*code)[position] = this->_opcode16; break;
  case 4: (*code)[position] = this->_opcode24; break;
  default: SIMPLE_ERROR("Assembler bug: Impossible size %zu", size);
  }
  size_t offset = this->delta(); // size_t to get a positive integer
  for(size_t i = 0; i < size - 1; ++i) {
    // Write the offset one byte at a time, starting with the LSB.
    (*code)[position+i+1] = offset & 0xff;
    offset >>= 8;
  }
}

size_t ControlLabelFixup_O::resize() {
  ptrdiff_t delta = this->delta();
  if ((-(1 <<  7) <= delta) && (delta <= (1 <<  7) - 1)) return 2;
  if ((-(1 << 15) <= delta) && (delta <= (1 << 15) - 1)) return 3;
  if ((-(1 << 23) <= delta) && (delta <= (1 << 23) - 1)) return 4;
  else SIMPLE_ERROR("Bytecode compiler limit reached: Fixup delta too large");
}

void JumpIfSuppliedFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  size_t size = this->size();
  switch (size) {
  case 3: (*code)[position] = vm_jump_if_supplied_8; break;
  case 4: (*code)[position] = vm_jump_if_supplied_16; break;
  default: SIMPLE_ERROR("Assembler bug: Impossible size %zu", size);
  }
  (*code)[position+1] = this->iindex();
  size_t offset = this->delta();
  for (size_t i = 0; i < size - 2; ++i) {
    (*code)[position+i+2] = offset & 0xff;
    offset >>= 8;
  }
}

size_t JumpIfSuppliedFixup_O::resize() {
  ptrdiff_t delta = this->delta();
  if ((-(1 <<  7) <= delta) && (delta <= (1 <<  7) - 1)) return 3;
  if ((-(1 << 15) <= delta) && (delta <= (1 << 15) - 1)) return 4;
  else SIMPLE_ERROR("Bytecode compiler limit reached: Fixup delta too large");
}

void LexRefFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  size_t size = this->size();
  switch (size) {
  case 1: (*code)[position] = this->opcode(); break;
  default: UNREACHABLE();
  }
}

size_t LexRefFixup_O::resize() {
  return this->lex()->indirectLexicalP() ? 1 : 0;
}

void EncageFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  size_t size = this->size();
  size_t index = this->lex()->frameIndex();
  switch (size) {
  case 5: // FIXME: Use assemble_into?
      (*code)[position  ] = vm_ref;
      (*code)[position+1] = index;
      (*code)[position+2] = vm_make_cell;
      (*code)[position+3] = vm_set;
      (*code)[position+4] = index;
      break;
  case 9:
      (*code)[position  ] = vm_long;
      (*code)[position+1] = vm_ref;
      (*code)[position+2] = index & 0xff;
      (*code)[position+3] = index >> 8;
      (*code)[position+4] = vm_make_cell;
      (*code)[position+5] = vm_long;
      (*code)[position+6] = vm_set;
      (*code)[position+7] = index & 0xff;
      (*code)[position+8] = index >> 8;
      break;
  default: UNREACHABLE();
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
  else SIMPLE_ERROR("Bytecode compiler limit reached: Fixup delta too large");
}

void LexSetFixup_O::emit(size_t position, SimpleVector_byte8_t_sp code) {
  size_t size = this->size();
  size_t index = this->lex()->frameIndex();
  switch (size) {
  case 2:
      (*code)[position  ] = vm_set;
      (*code)[position+1] = index;
      break;
  case 3:
      (*code)[position  ] = vm_ref;
      (*code)[position+1] = index;
      (*code)[position+2] = vm_cell_set;
      break;
  case 4:
      (*code)[position  ] = vm_long;
      (*code)[position+1] = vm_set;
      (*code)[position+2] = index & 0xff;
      (*code)[position+3] = index >> 8;
      break;
  case 5:
      (*code)[position  ] = vm_long;
      (*code)[position+1] = vm_ref;
      (*code)[position+2] = index & 0xff;
      (*code)[position+3] = index >> 8;
      (*code)[position+4] = vm_cell_set;
      break;
  default: UNREACHABLE();
  }
}

size_t LexSetFixup_O::resize() {
  bool indirectp = this->lex()->indirectLexicalP();
  size_t index = this->lex()->frameIndex();
  if (index < 1 << 8)
    if (indirectp) return 3;
    else return 2;
  else if (index < 1 << 16)
    if (indirectp) return 5;
    else return 4;
  else SIMPLE_ERROR("Bytecode compiler limit reached: Fixup delta too large");
}

}; //namespace comp
