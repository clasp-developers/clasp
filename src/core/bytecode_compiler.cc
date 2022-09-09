#include <clasp/core/bytecode_compiler.h>
#include <clasp/core/virtualMachine.h>
#include <clasp/core/evaluator.h> // af_interpreter_lookup_macro, extract_decl...
#include <clasp/core/sysprop.h> // core__get_sysprop
#include <clasp/core/lambdaListHandler.h> // lambda_list_for_name
#include <clasp/core/designators.h> // functionDesignator
#include <clasp/core/primitives.h> // gensym, function_block_name
#include <clasp/core/bytecode.h>
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
  if (vars.nilp()) return this->asSmartPtr();
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
  if (vars.nilp()) return this->asSmartPtr();
  List_sp new_vars = this->vars();
  for (auto cur : vars) {
    Symbol_sp var = oCar(cur);
    auto info = SpecialVarInfo_O::make(var->specialP());
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
CL_DEFUN void assemble(Context_sp context,
                            uint8_t opcode, List_sp operands) {
  Cfunction_sp func = context->cfunction();
  ComplexVector_byte8_t_sp bytecode = func->bytecode();
  bytecode->vectorPushExtend(opcode);
  for (auto cur : operands) {
    bytecode->vectorPushExtend(clasp_to_integral<uint8_t>(oCar(cur)));
  }
}

CL_LAMBDA(code position &rest values)
CL_DEFUN void assemble_into(SimpleVector_byte8_t_sp code,
                                 size_t position, List_sp values) {
  for (auto cur : values)
    (*code)[position++] = clasp_to_integral<uint8_t>(oCar(cur));
}

CL_LAMBDA(context opcode &rest operands)
CL_DEFUN void assemble_maybe_long(Context_sp context,
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

CL_DEFUN T_sp var_info(Symbol_sp sym, Lexenv_sp env) {
  // Local?
  T_sp info = env->variableInfo(sym);
  if (info.notnilp()) return info;
  // Constant?
  // Constants are also specialP, so we have to check this first.
  if (cl__keywordp(sym) || sym->getReadOnly())
    return ConstantVarInfo_O::make(sym->symbolValue());
  // Globally special?
  if (sym->specialP())
    return SpecialVarInfo_O::make(true);
  // Global symbol macro?
  T_mv symmac = core__get_sysprop(sym, ext::_sym_symbolMacro);
  if (gc::As<T_sp>(symmac.valueGet_(1)).notnilp())
    return SymbolMacroVarInfo_O::make(symmac);
  // Unknown.
  return nil<T_O>();
}

CL_DEFUN T_sp fun_info(T_sp name, Lexenv_sp env) {
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

// declared out of line for circularity reasons
Module_sp Context_O::module() { return this->cfunction()->module(); }

size_t Context_O::literal_index(T_sp literal) {
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
size_t Context_O::new_literal_index(T_sp literal) {
  Fixnum_sp nind = this->cfunction()->module()->literals()->vectorPushExtend(literal);
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

// Push the immutable value or cell of lexical in CONTEXT.
void Context_O::reference_lexical_info(LexicalVarInfo_sp info) {
  if (info->funct() == this->cfunction())
    this->assemble1(vm_ref, info->frameIndex());
  else
    this->assemble1(vm_closure, this->closure_index(info));
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

void Module_O::initialize_cfunction_positions() {
  size_t position = 0;
  ComplexVector_T_sp cfunctions = this->cfunctions();
  for (T_sp tfunction : *cfunctions) {
    Cfunction_sp cfunction = gc::As<Cfunction_sp>(tfunction);
    cfunction->setPosition(position);
    position += cfunction->bytecode()->length();
  }
}

void Fixup_O::update_positions(size_t increase) {
  Cfunction_sp funct = this->cfunction();
  ComplexVector_T_sp annotations = funct->annotations();
  size_t nannot = annotations->length();
  for (size_t idx = this->iindex() + 1; idx < nannot; ++idx)
    gc::As<Annotation_sp>((*annotations)[idx])->_position += increase;
  funct->_extra += increase;
  ComplexVector_T_sp functions = funct->module()->cfunctions();
  size_t nfuns = functions->length();
  for (size_t idx = funct->iindex() + 1; idx < nfuns; ++idx)
    gc::As<Cfunction_sp>((*functions)[idx])->_position += increase;
}

void Module_O::resolve_fixup_sizes() {
  bool changedp;
  ComplexVector_T_sp cfunctions = this->cfunctions();
  do {
    changedp = false;
    for (T_sp tfunction : *cfunctions) {
      ComplexVector_T_sp annotations = gc::As<Cfunction_sp>(tfunction)->annotations();
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
  Cfunction_sp last_cfunction = gc::As<Cfunction_sp>(tlast_cfunction);
  return last_cfunction->pposition()
    + last_cfunction->bytecode()->length()
    + last_cfunction->extra();
}

// Replacement for CL:REPLACE, which isn't available here.
static void replace_bytecode(SimpleVector_byte8_t_sp dest,
                             ComplexVector_byte8_t_sp src,
                             size_t start1, size_t start2, size_t end2) {
  size_t index1, index2;
  for (index1 = start1, index2 = start2;
       index2 < end2;
       ++index1, ++index2) {
    (*dest)[index1] = (*src)[index2];
  }
}

SimpleVector_byte8_t_sp Module_O::create_bytecode() {
  SimpleVector_byte8_t_sp bytecode = SimpleVector_byte8_t_O::make(this->bytecode_size());
  size_t index = 0;
  ComplexVector_T_sp cfunctions = this->cfunctions();
  for (T_sp tfunction : *cfunctions) {
    Cfunction_sp function = gc::As<Cfunction_sp>(tfunction);
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
          replace_bytecode(bytecode, cfunction_bytecode,
                           index, position, end);
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
    replace_bytecode(bytecode, cfunction_bytecode,
                     index, position, end);
    index += end - position;
  }
  return bytecode;
}

CL_DEFUN T_sp lambda_list_for_name(T_sp raw_lambda_list) {
  return core::lambda_list_for_name(raw_lambda_list);
}

GlobalBytecodeEntryPoint_sp Cfunction_O::link_function(T_sp compile_info) {
  Module_sp cmodule = this->module();
  cmodule->initialize_cfunction_positions();
  cmodule->resolve_fixup_sizes();
  ComplexVector_T_sp cmodule_literals = cmodule->literals();
  size_t literal_length = cmodule_literals->length();
  SimpleVector_sp literals = SimpleVector_O::make(literal_length);
  SimpleVector_byte8_t_sp bytecode = cmodule->create_bytecode();
  BytecodeModule_sp bytecode_module = BytecodeModule_O::make();
  ComplexVector_T_sp cfunctions = cmodule->cfunctions();
  // Create the real function objects.
  for (T_sp tfun : *cfunctions) {
    Cfunction_sp cfunction = gc::As<Cfunction_sp>(tfun);
    FunctionDescription_sp fdesc
      = makeFunctionDescription(cfunction->nname(),
                                cfunction->lambda_list(),
                                cfunction->doc());
    Fixnum_sp ep = clasp_make_fixnum(cfunction->entry_point()->module_position());
    GlobalBytecodeEntryPoint_sp func
      = core__makeGlobalBytecodeEntryPoint(fdesc, bytecode_module,
                                           cfunction->nlocals(),
                                          // FIXME: remove
                                           0, 0, 0, 0, nil<T_O>(), 0,
                                           cfunction->closed()->length(),
                                          // FIXME: remove
                                           cl__make_list(clasp_make_fixnum(7),
                                                         ep));
    cfunction->setInfo(func);
  }
  // Now replace the cfunctions in the cmodule literal vector with
  // real bytecode functions in the module vector.
  for (size_t i = 0; i < literal_length; ++i) {
    T_sp cfunc_lit = (*cmodule_literals)[i];
    if (gc::IsA<Cfunction_sp>(cfunc_lit))
      (*literals)[i] = gc::As_unsafe<Cfunction_sp>(cfunc_lit)->info();
    else
      (*literals)[i] = cfunc_lit;
  }
  // Now just install the bytecode and Bob's your uncle.
  bytecode_module->setf_literals(literals);
  bytecode_module->setf_bytecode(bytecode);
  bytecode_module->setf_compileInfo(compile_info);
  // Finally, return the GBEP for the main function.
  return this->info();
}

CL_DEFUN void compile_literal(T_sp literal, Lexenv_sp env,
                                   Context_sp context) {
  (void)env;
  T_sp rec = context->receiving();
  if (rec.fixnump()) {
    gc::Fixnum frec = rec.unsafe_fixnum();
    switch (frec) {
    case 0: return; // No value required, so do nothing
    case 1:
        if (literal.nilp()) context->assemble0(vm_nil);
        else context->assemble1(vm_const, context->literal_index(literal));
        return;
    default: SIMPLE_ERROR("BUG: Don't know how to compile literal returning %" PFixnum " values", frec);
    }
  } else { // must be T, i.e. values
    if (literal.nilp()) context->assemble0(vm_nil);
    else context->assemble1(vm_const, context->literal_index(literal));
    context->assemble0(vm_pop);
  }
}

void compile_form(T_sp, Lexenv_sp, Context_sp);

static T_sp expand_macro(Function_sp expander, T_sp form, Lexenv_sp env) {
  // This is copied from cl__macroexpand. I guess eval::funcall doesn't do the
  // coercion itself?
  T_sp macroexpandHook = cl::_sym_STARmacroexpand_hookSTAR->symbolValue();
  Function_sp hook = coerce::functionDesignator(macroexpandHook);
  return eval::funcall(hook, expander, form, env);
}

CL_DEFUN void compile_symbol(Symbol_sp sym,
                                  Lexenv_sp env, Context_sp context) {
  T_sp info = var_info(sym, env);
  if (gc::IsA<SymbolMacroVarInfo_sp>(info)) {
    Function_sp expander = gc::As_unsafe<SymbolMacroVarInfo_sp>(info)->expander();
    T_sp expansion = expand_macro(expander, sym, env);
    compile_form(expansion, env, context);
    return;
  } else if (context->receiving().fixnump()
             && (context->receiving().unsafe_fixnum() == 0)) {
    // A symbol macro could expand into something with arbitrary side effects
    // so we always have to compile that, but otherwise, if no values are
    // wanted, we want to not compile anything.
    return;
  } else {
    if (gc::IsA<LexicalVarInfo_sp>(info)) {
      LexicalVarInfo_sp lvinfo = gc::As_unsafe<LexicalVarInfo_sp>(info);
      if (lvinfo->funct() == context->cfunction())
        // Local variable, just read it.
        context->assemble1(vm_ref, lvinfo->frameIndex());
      else { // closed over
        lvinfo->setClosedOverP(true);
        context->assemble1(vm_closure, context->closure_index(lvinfo));
      }
      context->maybe_emit_cell_ref(lvinfo);
    } else if (gc::IsA<SpecialVarInfo_sp>(info))
      context->assemble1(vm_symbol_value, context->literal_index(sym));
    else if (gc::IsA<ConstantVarInfo_sp>(info)) {
      compile_literal(gc::As_unsafe<ConstantVarInfo_sp>(info)->value(),
                           env, context);
      // Avoid the pop code below - compile-literal handles it.
      return;
    } else if (info.nilp()) {
      // FIXME: Warn that the variable is unknown and we're assuming special.
      context->assemble1(vm_symbol_value, context->literal_index(sym));
    }
    if (!(context->receiving().fixnump()))
      // Not a fixnum, so must be T - put value in mv vector.
      context->assemble0(vm_pop);
  }
}

CL_DEFUN void compile_progn(List_sp forms, Lexenv_sp env, Context_sp ctxt) {
  if (forms.nilp())
    compile_literal(nil<T_O>(), env, ctxt);
  else
    for (auto cur : forms) {
      if (oCdr(cur).notnilp()) // compile for effect
        compile_form(oCar(cur), env, ctxt->sub(clasp_make_fixnum(0)));
      else // compile for value
        compile_form(oCar(cur), env, ctxt);
    }
}

CL_DEFUN void compile_locally(List_sp body, Lexenv_sp env, Context_sp ctxt) {
  List_sp declares = nil<T_O>();
  gc::Nilable<String_sp> docstring;
  List_sp code;
  List_sp specials;
  eval::extract_declares_docstring_code_specials(body, declares,
                                                 false, docstring, code, specials);
  Lexenv_sp inner = env->add_specials(specials);
  compile_progn(code, inner, ctxt);
}

CL_DEFUN bool special_binding_p(Symbol_sp sym, List_sp specials,
                                     Lexenv_sp env) {
  if (specials.notnilp()
      && specials.unsafe_cons()->memberEq(sym).notnilp())
    return true;
  else {
    T_sp info = var_info(sym, env);
    if (gc::IsA<SpecialVarInfo_sp>(info))
      return gc::As_unsafe<SpecialVarInfo_sp>(info)->globalp();
    else return false;
  }
}

CL_DEFUN void compile_let(List_sp bindings, List_sp body,
                               Lexenv_sp env, Context_sp ctxt) {
  List_sp declares = nil<T_O>();
  gc::Nilable<String_sp> docstring;
  List_sp code;
  List_sp specials;
  eval::extract_declares_docstring_code_specials(body, declares,
                                                 false, docstring, code, specials);
  size_t lexical_binding_count = 0;
  size_t special_binding_count = 0;
  Lexenv_sp post_binding_env = env->add_specials(specials);
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
    compile_form(valf, env, ctxt->sub(clasp_make_fixnum(1)));
    if (special_binding_p(var, specials, env)) {
      ++special_binding_count;
      ctxt->emit_special_bind(var);
    } else {
      post_binding_env = post_binding_env->bind_vars(Cons_O::createList(var),
                                                     ctxt);
      ++lexical_binding_count;
      ctxt->maybe_emit_make_cell(var_info(var, post_binding_env));
    }
  }
  ctxt->emit_bind(lexical_binding_count, env->frameEnd());
  compile_progn(code, post_binding_env, ctxt);
  ctxt->emit_unbind(special_binding_count);
}

CL_DEFUN void compile_letSTAR(List_sp bindings, List_sp body,
                                   Lexenv_sp env, Context_sp ctxt) {
  List_sp declares = nil<T_O>();
  gc::Nilable<String_sp> docstring;
  List_sp code;
  List_sp specials;
  eval::extract_declares_docstring_code_specials(body, declares,
                                                 false, docstring, code, specials);
  size_t special_binding_count = 0;
  Lexenv_sp new_env = env;
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
    compile_form(valf, new_env, ctxt->sub(clasp_make_fixnum(1)));
    if (special_binding_p(var, specials, env)) {
      ++special_binding_count;
      new_env = new_env->add_specials(Cons_O::createList(var));
      ctxt->emit_special_bind(var);
    } else {
      size_t frame_start = new_env->frameEnd();
      new_env = new_env->bind_vars(Cons_O::createList(var), ctxt);
      ctxt->maybe_emit_make_cell(var_info(var, new_env));
      ctxt->assemble1(vm_set, frame_start);
    }
  }
  // We make a new environment to make sure free special declarations get
  // through even if this let* doesn't bind them.
  // This creates duplicate alist entries for anything that _is_ bound
  // here, but that's not a big deal.
  compile_progn(code, new_env->add_specials(specials), ctxt);
  ctxt->emit_unbind(special_binding_count);
}

SYMBOL_EXPORT_SC_(CompPkg, compile_lambda);
CL_DEFUN void compile_function(T_sp fnameoid, Lexenv_sp env, Context_sp ctxt) {
  bool mvp;
  if (!(ctxt->receiving().fixnump())) mvp = true;
  else if (ctxt->receiving().unsafe_fixnum() == 0) return;
  else mvp = false;
  if (gc::IsA<Cons_sp>(fnameoid) && oCar(fnameoid) == cl::_sym_lambda) {
    T_sp tfun = eval::funcall(_sym_compile_lambda,
                              oCadr(fnameoid), oCddr(fnameoid),
                              env, ctxt->module());
    Cfunction_sp fun = gc::As<Cfunction_sp>(tfun);
    ComplexVector_T_sp closed = fun->closed();
    for (size_t i = 0; i < closed->length(); ++i)
      ctxt->reference_lexical_info(gc::As<LexicalVarInfo_sp>((*closed)[i]));
    if (closed->length() == 0) // don't need to actually close
      ctxt->assemble1(vm_const, ctxt->literal_index(fun));
    else
      ctxt->assemble1(vm_make_closure, ctxt->literal_index(fun));
  } else { // ought to be a function name
    T_sp info = fun_info(fnameoid, env);
    if (gc::IsA<GlobalFunInfo_sp>(info) || info.nilp()) {
      // TODO: Warn on unknown (nil)
      ctxt->assemble1(vm_fdefinition, ctxt->literal_index(fnameoid));
    } else if (gc::IsA<LocalFunInfo_sp>(info)) {
      LocalFunInfo_sp lfinfo = gc::As_unsafe<LocalFunInfo_sp>(info);
      LexicalVarInfo_sp lvinfo = gc::As<LexicalVarInfo_sp>(lfinfo->funVar());
      ctxt->reference_lexical_info(lvinfo);
    } else SIMPLE_ERROR("BUG: Unknown fun info %s", _rep_(info));
  }
  // Coerce to values if necessary.
  if (mvp) ctxt->assemble0(vm_pop);
}

CL_DEFUN void compile_flet(List_sp definitions, List_sp body,
                           Lexenv_sp env, Context_sp ctxt) {
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
    T_sp block = Cons_O::createList(cl::_sym_block,
                                    core__function_block_name(name),
                                    locally);
    T_sp lambda = Cons_O::createList(cl::_sym_lambda,
                                     oCadr(definition),
                                     block);
    compile_function(lambda, env, ctxt->sub(clasp_make_fixnum(1)));
    fun_vars << fun_var;
    funs << Cons_O::create(name,
                           LocalFunInfo_O::make(LexicalVarInfo_O::make(frame_slot++,
                                                                       ctxt->cfunction())));
    ++fun_count;
  }
  ctxt->emit_bind(fun_count, env->frameEnd());
  // KLUDGEy - we could do this in one new environment
  Lexenv_sp new_env1 = env->bind_vars(fun_vars.cons(), ctxt);
  Lexenv_sp new_env2 = Lexenv_O::make(new_env1->vars(),
                                      new_env1->tags(),
                                      new_env1->blocks(),
                                      Cons_O::append(funs.cons(),
                                                     new_env1->funs()),
                                      new_env1->frameEnd());
  compile_locally(body, new_env2, ctxt);
}

CL_DEFUN void compile_labels(List_sp definitions, List_sp body,
                                  Lexenv_sp env, Context_sp ctxt) {
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
    funs << Cons_O::create(name,
                           LocalFunInfo_O::make(LexicalVarInfo_O::make(frame_slot++,
                                                                       ctxt->cfunction())));
    ++fun_count;
  }
  frame_slot = frame_start;
  Lexenv_sp new_env1 = env->bind_vars(fun_vars.cons(), ctxt);
  Lexenv_sp new_env2 = Lexenv_O::make(new_env1->vars(),
                                      new_env1->tags(),
                                      new_env1->blocks(),
                                      Cons_O::append(funs.cons(),
                                                     new_env1->funs()),
                                      new_env1->frameEnd());
  for (auto cur : definitions) {
    Cons_sp definition = gc::As_unsafe<Cons_sp>(oCar(cur));
    T_sp name = oCar(definition);
    T_sp locally = Cons_O::create(cl::_sym_locally, oCddr(definition));
    T_sp block = Cons_O::createList(cl::_sym_block,
                                    core__function_block_name(name),
                                    locally);
    T_sp tfun = eval::funcall(_sym_compile_lambda,
                              oCadr(definition),
                              Cons_O::createList(block),
                              new_env2, ctxt->module());
    size_t literal_index = ctxt->literal_index(tfun);
    Cfunction_sp fun = gc::As<Cfunction_sp>(tfun);
    if (fun->closed()->length() == 0) // not a closure- easy
      ctxt->assemble1(vm_const, literal_index);
    else {
      closures << Cons_O::create(fun, clasp_make_fixnum(frame_slot));
      ctxt->assemble1(vm_make_uninitialized_closure, literal_index);
    }
    ++frame_slot;
  }
  ctxt->emit_bind(fun_count, frame_start);
  // Make the closures
  for (auto cur : gc::As<List_sp>(closures.cons())) {
    Cfunction_sp cf = gc::As_unsafe<Cfunction_sp>(oCaar(cur));
    ComplexVector_T_sp closed = cf->closed();
    for (size_t i = 0; i < closed->length(); ++i) {
      LexicalVarInfo_sp info = gc::As<LexicalVarInfo_sp>((*closed)[i]);
      ctxt->reference_lexical_info(info);
    }
    ctxt->assemble1(vm_initialize_closure, oCdar(cur).unsafe_fixnum());
  }
  compile_locally(body, new_env2, ctxt);
}

static void compile_setq_1(Symbol_sp var, T_sp valf,
                           Lexenv_sp env, Context_sp ctxt) {
  T_sp info = var_info(var, env);
  if (gc::IsA<SymbolMacroVarInfo_sp>(info)) {
    Function_sp expander = gc::As_unsafe<SymbolMacroVarInfo_sp>(info)->expander();
    T_sp expansion = expand_macro(expander, var, env);
    T_sp setform = Cons_O::createList(cl::_sym_setf, expansion, valf);
    compile_form(setform, env, ctxt);
  } else if (info.nilp() || gc::IsA<SpecialVarInfo_sp>(info)) {
    // TODO: Warn on unknown variable
    compile_form(valf, env, ctxt->sub(clasp_make_fixnum(1)));
    // If we need to return the new value, stick it into a new local
    // variable, do the set, then return the lexical variable.
    // We can't just read from the special, since some other thread may
    // alter it.
    size_t index = env->frameEnd();
    // but if we're not returning a value we don't actually have to do that crap.
    if (!(ctxt->receiving().fixnump()
          && (ctxt->receiving().unsafe_fixnum() == 0))) {
      ctxt->assemble1(vm_set, index);
      ctxt->assemble1(vm_ref, index);
      // called for effect, i.e. to keep frame size correct
      // FIXME: This is super kludgey.
      env->bind_vars(Cons_O::createList(var), ctxt);
    }
    ctxt->assemble1(vm_symbol_value_set, ctxt->literal_index(var));
    if (!(ctxt->receiving().fixnump()
          && (ctxt->receiving().unsafe_fixnum() == 0))) {
      ctxt->assemble1(vm_ref, index);
      if (!(ctxt->receiving().fixnump())) // need values
        ctxt->assemble0(vm_pop);
    }
  } else if (gc::IsA<LexicalVarInfo_sp>(info)) {
    LexicalVarInfo_sp lvinfo = gc::As_unsafe<LexicalVarInfo_sp>(info);
    bool localp = (lvinfo->funct() == ctxt->cfunction());
    size_t index = env->frameEnd();
    if (!localp) lvinfo->setClosedOverP(true);
    lvinfo->setSetP(true);
    compile_form(valf, env, ctxt->sub(clasp_make_fixnum(1)));
    // Similar concerns to specials above (for closure variables)
    if (!(ctxt->receiving().fixnump()
          && (ctxt->receiving().unsafe_fixnum() == 0))) {
      ctxt->assemble1(vm_set, index);
      ctxt->assemble1(vm_ref, index);
      env->bind_vars(Cons_O::createList(var), ctxt);
    }
    if (localp)
      ctxt->emit_lexical_set(lvinfo);
    else { // we already know we need a cell, so don't bother w/ a fixup.
      ctxt->assemble1(vm_closure, ctxt->closure_index(lvinfo));
      ctxt->assemble0(vm_cell_set);
    }
    if (!(ctxt->receiving().fixnump()
          && (ctxt->receiving().unsafe_fixnum() == 0))) {
      ctxt->assemble1(vm_ref, index);
      if (!(ctxt->receiving().fixnump()))
        ctxt->assemble0(vm_pop);
    }
  } else SIMPLE_ERROR("BUG: Unknown info %s", _rep_(info));
}

CL_DEFUN void compile_setq(List_sp pairs, Lexenv_sp env, Context_sp ctxt) {
  if (pairs.nilp()) {
    // degenerate case
    if (!(ctxt->receiving().fixnump()
          && (ctxt->receiving().unsafe_fixnum() == 0)))
      ctxt->assemble0(vm_nil);
  } else {
    do {
      Symbol_sp var = gc::As<Symbol_sp>(oCar(pairs));
      T_sp valf = oCadr(pairs);
      pairs = gc::As<List_sp>(oCddr(pairs));
      compile_setq_1(var, valf, env,
                     pairs.notnilp() ? ctxt->sub(clasp_make_fixnum(0)) : ctxt);
    } while (pairs.notnilp());
  }
}

CL_DEFUN void compile_eval_when(List_sp situations, List_sp body,
                                     Lexenv_sp env, Context_sp ctxt) {
  for (auto cur : situations) {
    T_sp situation = oCar(cur);
    if ((situation == cl::_sym_eval) || (situation == kw::_sym_execute)) {
      compile_progn(body, env, ctxt);
      return;
    }
  }
  // no eval or execute, so
  compile_literal(nil<T_O>(), env, ctxt);
}

CL_DEFUN void compile_if(T_sp cond, T_sp thn, T_sp els,
                              Lexenv_sp env, Context_sp ctxt) {
  compile_form(cond, env, ctxt->sub(clasp_make_fixnum(1)));
  Label_sp then_label = Label_O::make();
  Label_sp done_label = Label_O::make();
  ctxt->emit_jump_if(then_label);
  compile_form(els, env, ctxt);
  ctxt->emit_jump(done_label);
  then_label->contextualize(ctxt);
  compile_form(thn, env, ctxt);
  done_label->contextualize(ctxt);
}

static bool go_tag_p(T_sp object) {
  return object.fixnump() || gc::IsA<Integer_sp>(object)
    || gc::IsA<Symbol_sp>(object);
}

CL_DEFUN void compile_tagbody(List_sp statements,
                                   Lexenv_sp env, Context_sp ctxt) {
  List_sp new_tags = gc::As<List_sp>(env->tags());
  Symbol_sp tagbody_dynenv = cl__gensym(SimpleBaseString_O::make("TAG-DYNENV"));
  Lexenv_sp nenv = env->bind_vars(Cons_O::createList(tagbody_dynenv), ctxt);
  LexicalVarInfo_sp dynenv_info
    = gc::As<LexicalVarInfo_sp>(var_info(tagbody_dynenv, nenv));
  for (auto cur : statements) {
    T_sp statement = oCar(cur);
    if (go_tag_p(statement))
      new_tags = Cons_O::create(Cons_O::create(statement,
                                               Cons_O::create(dynenv_info,
                                                              Label_O::make())),
                                new_tags);
  }
  Lexenv_sp nnenv = Lexenv_O::make(nenv->vars(), new_tags, nenv->blocks(),
                                   nenv->funs(), nenv->frameEnd());
  // Bind the dynamic environment.
  ctxt->assemble1(vm_entry, dynenv_info->frameIndex());
  // Compile the body, emitting the tag destination labels.
  for (auto cur : statements) {
    T_sp statement = oCar(cur);
    if (go_tag_p(statement)) {
      T_sp info = nnenv->tags().unsafe_cons()->assoc(statement, nil<T_O>(),
                                                     cl::_sym_eql, nil<T_O>());
      Label_sp lab = gc::As<Label_sp>(oCddr(info));
      lab->contextualize(ctxt);
    } else
      compile_form(statement, nnenv, ctxt->sub(clasp_make_fixnum(0)));
  }
  ctxt->assemble0(vm_entry_close);
  // return nil if we really have to
  if (!(ctxt->receiving().fixnump() && (ctxt->receiving().unsafe_fixnum() == 0))) {
    ctxt->assemble0(vm_nil);
    if (!(ctxt->receiving().fixnump()))
      ctxt->assemble0(vm_pop);
  }
}

CL_DEFUN void compile_go(T_sp tag, Lexenv_sp env, Context_sp ctxt) {
  T_sp tags = env->tags();
  if (!(tags.nilp())) {
    // tags must be a cons now
    T_sp pair = tags.unsafe_cons()->assoc(tag, nil<T_O>(),
                                          cl::_sym_eql, nil<T_O>());
    if (!(pair.nilp())) {
      Cons_sp rpair = gc::As<Cons_sp>(oCdr(pair));
      ctxt->reference_lexical_info(gc::As<LexicalVarInfo_sp>(oCar(rpair)));
      ctxt->emit_exit(gc::As<Label_sp>(oCdr(rpair)));
      return;
    }
  }
  SIMPLE_ERROR("The GO tag %s does not exist.", _rep_(tag));
}

CL_DEFUN void compile_block(Symbol_sp name, List_sp body,
                                 Lexenv_sp env, Context_sp ctxt) {
  Symbol_sp block_dynenv = cl__gensym(SimpleBaseString_O::make("BLOCK-DYNENV"));
  Lexenv_sp nenv = env->bind_vars(Cons_O::createList(block_dynenv), ctxt);
  LexicalVarInfo_sp dynenv_info
    = gc::As<LexicalVarInfo_sp>(var_info(block_dynenv, nenv));
  Label_sp label = Label_O::make();
  Label_sp normal_label = Label_O::make();
  // Bind the dynamic environment.
  ctxt->assemble1(vm_entry, dynenv_info->frameIndex());
  Cons_sp new_pair = Cons_O::create(name, Cons_O::create(dynenv_info, label));
  Lexenv_sp nnenv = Lexenv_O::make(nenv->vars(), nenv->tags(),
                                   Cons_O::create(new_pair, nenv->blocks()),
                                   nenv->funs(), nenv->frameEnd());
  // We force single values into multiple so that we can uniformly PUSH afterward.
  // Specifically: if we're returning 0 values, there's no problem anyway.
  // If we're returning multiple values, the local and nonlocal returns just
  // store into the multiple values, so no problem there.
  // If we're returning exactly one value, the nonlocal just pushes one, and
  // the nonlocal stores into the MV which is then vm_push'd to the stack.
  compile_progn(body, nnenv, ctxt);
  bool r1p = ctxt->receiving().fixnump()
    && (ctxt->receiving().unsafe_fixnum() == 1);
  if (r1p) ctxt->emit_jump(normal_label);
  label->contextualize(ctxt);
  // When we need 1 value, we have to make sure that the
  // "exceptional" case pushes a single value onto the stack.
  if (r1p) {
    ctxt->assemble0(vm_push);
    normal_label->contextualize(ctxt);
  }
  ctxt->assemble0(vm_entry_close);
}

CL_DEFUN void compile_return_from(T_sp name, T_sp valuef,
                                       Lexenv_sp env, Context_sp ctxt) {
  compile_form(valuef, env, ctxt->sub(cl::_sym_T_O));
  T_sp blocks = env->blocks();
  if (!(blocks.nilp())) {
    // blocks must be a cons now
    T_sp pair = blocks.unsafe_cons()->assoc(name, nil<T_O>(),
                                            cl::_sym_eq, nil<T_O>());
    if (!(pair.nilp())) {
      Cons_sp rpair = gc::As<Cons_sp>(oCdr(pair));
      ctxt->reference_lexical_info(gc::As<LexicalVarInfo_sp>(oCar(rpair)));
      ctxt->emit_exit(gc::As<Label_sp>(oCdr(rpair)));
      return;
    }
  }
  SIMPLE_ERROR("The block %s does not exist.", _rep_(name));
}

// catch, throw, and progv are actually handled by macros right now,
// so these aren't used, but maybe will be in the fture.
CL_DEFUN void compile_catch(T_sp tag, List_sp body,
                                 Lexenv_sp env, Context_sp ctxt) {
  compile_form(tag, env, ctxt->sub(clasp_make_fixnum(1)));
  Label_sp target = Label_O::make();
  ctxt->emit_catch(target);
  // FIXME: maybe should be a T context to match throw
  compile_progn(body, env, ctxt);
  ctxt->assemble0(vm_catch_close);
  target->contextualize(ctxt);
}

CL_DEFUN void compile_throw(T_sp tag, T_sp rform,
                                 Lexenv_sp env, Context_sp ctxt) {
  compile_form(tag, env, ctxt->sub(clasp_make_fixnum(1)));
  compile_form(rform, env, ctxt->sub(cl::_sym_T_O));
  ctxt->assemble0(vm_throw);
}

CL_DEFUN void compile_progv(T_sp syms, T_sp vals, List_sp body,
                                 Lexenv_sp env, Context_sp ctxt) {
  compile_form(syms, env, ctxt->sub(clasp_make_fixnum(1)));
  compile_form(vals, env, ctxt->sub(clasp_make_fixnum(1)));
  ctxt->assemble0(vm_progv);
  compile_progn(body, env, ctxt);
  ctxt->emit_unbind(1);
}

CL_DEFUN void compile_multiple_value_call(T_sp fform, List_sp aforms,
                                               Lexenv_sp env, Context_sp ctxt) {
  compile_form(fform, env, ctxt->sub(clasp_make_fixnum(1)));
  T_sp first = oCar(aforms);
  List_sp rest = gc::As<List_sp>(oCdr(aforms));
  compile_form(first, env, ctxt->sub(cl::_sym_T_O));
  if (rest.notnilp()) {
    ctxt->assemble0(vm_push_values);
    for (auto cur : rest) {
      compile_form(oCar(cur), env, ctxt->sub(cl::_sym_T_O));
      ctxt->assemble0(vm_append_values);
    }
    ctxt->assemble0(vm_pop_values);
  }
  ctxt->emit_mv_call();
}

CL_DEFUN void compile_multiple_value_prog1(T_sp fform, List_sp forms,
                                                Lexenv_sp env, Context_sp ctxt) {
  compile_form(fform, env, ctxt);
  // Don't need to save anything with fixed value returns
  if (!ctxt->receiving().fixnump())
    ctxt->assemble0(vm_push_values);
  for (auto cur : forms)
    compile_form(oCar(cur), env, ctxt->sub(clasp_make_fixnum(0)));
  if (!ctxt->receiving().fixnump())
    ctxt->assemble0(vm_pop_values);
}

// Compile a call, where the function is already on the stack.
static void compile_call(T_sp args, Lexenv_sp env, Context_sp context) {
  // Compile the arguments.
  size_t argcount = 0;
  for (auto cur : gc::As<List_sp>(args)) {
    ++argcount;
    compile_form(oCar(cur), env, context->sub(clasp_make_fixnum(1)));
  }
  // generate the call
  context->emit_call(argcount);
}

CL_DEFUN Lexenv_sp make_null_lexical_environment() {
  return Lexenv_O::make(nil<T_O>(), nil<T_O>(), nil<T_O>(), nil<T_O>(), 0);
}

CL_DEFUN void compile_load_time_value(T_sp form, Lexenv_sp env, Context_sp ctxt) {
  // TODO: compile-file semantics?
  // Here we just use funcall of bytecompile to do eval. In the future we might
  // want to just call eval, which may or may not go through bytecompilation.
  Lexenv_sp nenv = make_null_lexical_environment();
  T_sp lexpr = Cons_O::createList(cl::_sym_lambda, nil<T_O>(), form);
  GlobalBytecodeEntryPoint_sp thunk = bytecompile(lexpr, nenv);
  T_sp value = eval::funcall(thunk);
  compile_literal(value, env, ctxt);
}

CL_DEFUN void compile_symbol_macrolet(List_sp bindings, List_sp body,
                                      Lexenv_sp env, Context_sp context) {
  T_sp vars = env->vars();
  Lexenv_sp menv = env->macroexpansion_environment();
  for (auto cur : bindings) {
    T_sp binding = oCar(cur);
    Symbol_sp name = gc::As<Symbol_sp>(oCar(binding));
    T_sp expansion = oCadr(binding);
    // FIXME: Compiling a new function for the expander is overkill
    T_sp formv = cl__gensym(SimpleBaseString_O::make("FORM"));
    T_sp envv = cl__gensym(SimpleBaseString_O::make("ENV"));
    T_sp lexpr = Cons_O::createList(cl::_sym_lambda,
                                    Cons_O::createList(formv, envv),
                                    Cons_O::createList(cl::_sym_declare,
                                                       Cons_O::createList(cl::_sym_ignore, formv, envv)),
                                    Cons_O::createList(cl::_sym_quote, expansion));
    GlobalBytecodeEntryPoint_sp expander = bytecompile(lexpr, menv);
    SymbolMacroVarInfo_sp info = SymbolMacroVarInfo_O::make(expander);
    vars = Cons_O::create(Cons_O::create(name, info), vars);
  }
  Lexenv_sp nenv = Lexenv_O::make(vars, env->tags(), env->blocks(), env->funs(),
                                  env->frameEnd());
  compile_locally(body, nenv, context);
}

CL_DEFUN void compile_macrolet(List_sp bindings, List_sp body,
                               Lexenv_sp env, Context_sp context) {
  T_sp funs = env->funs();
  Lexenv_sp menv = env->macroexpansion_environment();
  for (auto cur : bindings) {
    T_sp binding = oCar(cur);
    T_sp name = oCar(binding);
    T_sp lambda_list = oCadr(binding);
    T_sp body = oCddr(binding);
    T_sp eform = eval::funcall(ext::_sym_parse_macro,
                               name, lambda_list, body, menv);
    GlobalBytecodeEntryPoint_sp expander = bytecompile(eform, menv);
    LocalMacroInfo_sp info = LocalMacroInfo_O::make(expander);
    funs = Cons_O::create(Cons_O::create(name, info), funs);
  }
  Lexenv_sp nenv = Lexenv_O::make(env->vars(), env->tags(), env->blocks(),
                                  funs, env->frameEnd());
  compile_locally(body, nenv, context);
}

CL_DEFUN void compile_combination(T_sp head, T_sp rest,
                                  Lexenv_sp env, Context_sp context) {
  if (head == cl::_sym_progn) compile_progn(rest, env, context);
  else if (head == cl::_sym_let)
    compile_let(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_letSTAR)
    compile_letSTAR(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_flet)
    compile_flet(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_labels)
    compile_labels(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_setq) compile_setq(rest, env, context);
  else if (head == cl::_sym_if)
    compile_if(oCar(rest), oCadr(rest), oCaddr(rest), env, context);
  else if (head == cl::_sym_Function_O)
    compile_function(oCar(rest), env, context);
  else if (head == cl::_sym_tagbody) compile_tagbody(rest, env, context);
  else if (head == cl::_sym_go) compile_go(oCar(rest), env, context);
  else if (head == cl::_sym_block)
    compile_block(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_return_from)
    compile_return_from(oCar(rest), oCadr(rest), env, context);
  else if (head == cl::_sym_quote) compile_literal(oCar(rest), env, context);
  else if (head == cl::_sym_load_time_value)
    compile_load_time_value(oCar(rest), env, context);
  else if (head == cl::_sym_macrolet)
    compile_macrolet(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_symbol_macrolet)
    compile_symbol_macrolet(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_multiple_value_call)
    compile_multiple_value_call(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_multiple_value_prog1)
    compile_multiple_value_prog1(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_locally) compile_locally(rest, env, context);
  else if (head == cl::_sym_eval_when)
    compile_eval_when(oCar(rest), oCdr(rest), env, context);
  else if (head == cl::_sym_the) // skip
    compile_form(oCadr(rest), env, context);
  else {
    if (gc::IsA<Symbol_sp>(head)) {
      Symbol_sp shead = gc::As_unsafe<Symbol_sp>(head);
      T_sp info = fun_info(head, env);
      if (gc::IsA<GlobalMacroInfo_sp>(info)) {
        GlobalMacroInfo_sp minfo = gc::As_unsafe<GlobalMacroInfo_sp>(info);
        T_sp expansion = expand_macro(minfo->expander(),
                                      Cons_O::create(head, rest), env);
        compile_form(expansion, env, context);
      } else if (gc::IsA<LocalMacroInfo_sp>(info)) {
        LocalMacroInfo_sp minfo = gc::As_unsafe<LocalMacroInfo_sp>(info);
        T_sp expansion = expand_macro(minfo->expander(),
                                      Cons_O::create(head, rest), env);
        compile_form(expansion, env, context);
      } else if (gc::IsA<GlobalFunInfo_sp>(info)
                 || gc::IsA<LocalFunInfo_sp>(info)
                 || info.nilp()) {
        // unknown function warning handled by compile-function (eventually)
        // note we do a double lookup of the fun info,
        // which is inefficient in the compiler (doesn't affect generated code)
        compile_function(head, env, context->sub(clasp_make_fixnum(1)));
        compile_call(rest, env, context);
      } else SIMPLE_ERROR("BUG: Unknown info %s", _rep_(info));
    } else if (gc::IsA<Cons_sp>(head) && (oCar(head) == cl::_sym_lambda)) {
      // Lambda form
      compile_function(head, env, context->sub(clasp_make_fixnum(1)));
      compile_call(rest, env, context);
    } else SIMPLE_ERROR("Illegal combination %s %s", _rep_(head), _rep_(rest));
  }
}

CL_DEFUN void compile_form(T_sp form, Lexenv_sp env, Context_sp context) {
  // Code walk if we're doing that
  if (_sym_STARcodeWalkerSTAR->boundP()
      && _sym_STARcodeWalkerSTAR->symbolValue().notnilp())
    form = eval::funcall(_sym_STARcodeWalkerSTAR->symbolValue(), form, env);
  // Compile
  if (gc::IsA<Symbol_sp>(form))
    compile_symbol(gc::As_unsafe<Symbol_sp>(form), env, context);
  else if (form.consp())
    compile_combination(oCar(form), oCdr(form), env, context);
  else compile_literal(form, env, context);
}

CL_LAMBDA(lambda-expression &optional (env (make-null-lexical-environment)))
CL_DEFUN GlobalBytecodeEntryPoint_sp bytecompile(T_sp lambda_expression,
                                                 Lexenv_sp env) {
  if (!gc::IsA<Cons_sp>(lambda_expression)
      || (oCar(lambda_expression) != cl::_sym_lambda))
    SIMPLE_ERROR("bytecompile passed a non-lambda-expression: %s",
                 _rep_(lambda_expression));
  Module_sp module = Module_O::make();
  T_sp lambda_list = oCadr(lambda_expression);
  T_sp body = oCddr(lambda_expression);
  Cfunction_sp cf = eval::funcall(_sym_compile_lambda,
                                  lambda_list, body, env, module);
  return cf->link_function(Cons_O::create(lambda_expression, env));
}

}; //namespace comp
