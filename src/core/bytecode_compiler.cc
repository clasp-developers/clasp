#include <clasp/core/bytecode_compiler.h>
#include <clasp/core/virtualMachine.h>
#include <clasp/core/evaluator.h> // af_interpreter_lookup_macro
#include <clasp/core/sysprop.h> // core__get_sysprop

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

size_t Annotation_O::module_position() {
  return this->pposition() + gc::As<Cfunction_sp>(this->cfunction())->pposition();
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
      (*code)[position+3] = index & 0xff00;
      (*code)[position+4] = vm_make_cell;
      (*code)[position+5] = vm_long;
      (*code)[position+6] = vm_set;
      (*code)[position+7] = index & 0xff;
      (*code)[position+8] = index & 0xff00;
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
      (*code)[position+3] = index & 0xff00;
      break;
  case 5:
      (*code)[position  ] = vm_long;
      (*code)[position+1] = vm_ref;
      (*code)[position+2] = index & 0xff;
      (*code)[position+3] = index & 0xff00;
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
