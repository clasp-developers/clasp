#include <clasp/core/bytecode_compiler.h>
#include <clasp/core/evaluator.h> // af_interpreter_lookup_macro, extract_decl...
#include <clasp/core/sysprop.h> // core__get_sysprop
#include <clasp/core/lambdaListHandler.h> // lambda list parsing
#include <clasp/core/designators.h> // functionDesignator
#include <clasp/core/primitives.h> // gensym, function_block_name
#include <clasp/core/bytecode.h>
#include <algorithm> // max


#define VM_CODES
#include <clasp/core/virtualMachine.h>
#undef VM_CODES

namespace comp {

using namespace core;

T_sp Lexenv_O::variableInfo(T_sp varname) {
  T_sp vars = this->vars();
  if (vars.nilp()) return vars;
  else {
    ASSERT(gc::IsA<Cons_sp>(vars));
    T_sp pair = core__alist_assoc_eq( gc::As_unsafe<Cons_sp>(vars), varname );
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
    MultipleValues& mvn = core::lisp_multipleValues();
    if (gc::As_unsafe<T_sp>(mvn.valueGet(1,result.number_of_values())).notnilp()) {
      return result;
    } else return nil<T_O>();
  } else return nil<T_O>();
}

T_sp Lexenv_O::functionInfo(T_sp funname) {
  T_sp funs = this->funs();
  if (funs.nilp()) return funs;
  else {
    ASSERT(gc::IsA<Cons_sp>(funs));
    T_sp pair = core__alist_assoc_equal(gc::As_unsafe<Cons_sp>(funs),funname);
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
                        this->funs(), this->notinlines(), frame_end);
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
                        this->funs(), this->notinlines(), this->frameEnd());
}

Lexenv_sp Lexenv_O::add_notinlines(List_sp fnames) {
  if (fnames.nilp()) return this->asSmartPtr();
  else return Lexenv_O::make(this->vars(), this->tags(), this->blocks(),
                             this->funs(),
                             Cons_O::append(fnames, this->notinlines()),
                             this->frameEnd());
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
                        new_funs.cons(), this->notinlines(), 0);
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
  MultipleValues& mvn = core::lisp_multipleValues();
  if (gc::As_unsafe<T_sp>(mvn.valueGet(1,symmac.number_of_values())).notnilp())
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
        T_sp sss = CONS_CAR(dname);
        ASSERT(gc::IsA<Symbol_sp>(sss));
        Symbol_sp fname = gc::As_unsafe<Symbol_sp>(sss);
        if (fname.notnilp() && oCdr(dname).nilp()) {
          if (!fname->fboundp_setf())
            return nil<T_O>();
          if (fname->macroP())
            return GlobalMacroInfo_O::make(fname->getSetfFdefinition());
          else {
            if (cl::_sym_compiler_macro_function->fboundp()) {
              T_sp cmexpander = eval::funcall(cl::_sym_compiler_macro_function,
                                              name);
              return GlobalFunInfo_O::make(cmexpander);
            } else return GlobalFunInfo_O::make(nil<T_O>());
          }
        }
      }
    }
    // Bad function name.
    return nil<T_O>();
  } else {
    ASSERT(gc::IsA<Symbol_sp>(name));
    Symbol_sp fname = gc::As_unsafe<Symbol_sp>(name);
    if (!fname->fboundp()) return nil<T_O>();
    else if (fname->macroP())
      return GlobalMacroInfo_O::make(fname->symbolFunction());
    else {
      // Look for a compiler macro expander.
      if (cl::_sym_compiler_macro_function->fboundp()) {
        T_sp cmexpander = eval::funcall(cl::_sym_compiler_macro_function, fname);
        return GlobalFunInfo_O::make(cmexpander);
      } else return GlobalFunInfo_O::make(nil<T_O>());
    }
  }
}

bool Lexenv_O::notinlinep(T_sp fname) {
  for (auto cur : this->notinlines())
    if (oCar(cur) == fname) return true;
  return false;
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
  ASSERT(gc::IsA<Cfunction_sp>(this->cfunction()));
  return this->pposition() + gc::As_unsafe<Cfunction_sp>(this->cfunction())->pposition();
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
    ASSERT(gc::IsA<Cfunction_sp>(tfunction));
    Cfunction_sp cfunction = gc::As_unsafe<Cfunction_sp>(tfunction);
    cfunction->setPosition(position);
    position += cfunction->bytecode()->length();
  }
}

void Fixup_O::update_positions(size_t increase) {
  Cfunction_sp funct = this->cfunction();
  ComplexVector_T_sp annotations = funct->annotations();
  size_t nannot = annotations->length();
  for (size_t idx = this->iindex() + 1; idx < nannot; ++idx) {
    ASSERT(gc::IsA<Annotation_sp>((*annotations)[idx]));
    gc::As_unsafe<Annotation_sp>((*annotations)[idx])->_position += increase;
  }
  funct->_extra += increase;
  ComplexVector_T_sp functions = funct->module()->cfunctions();
  size_t nfuns = functions->length();
  for (size_t idx = funct->iindex() + 1; idx < nfuns; ++idx) {
    ASSERT(gc::IsA<Cfunction_sp>((*functions)[idx]));
    gc::As_unsafe<Cfunction_sp>((*functions)[idx])->_position += increase;
  }
}

void Module_O::resolve_fixup_sizes() {
  bool changedp;
  ComplexVector_T_sp cfunctions = this->cfunctions();
  do {
    changedp = false;
    for (T_sp tfunction : *cfunctions) {
      ASSERT(gc::IsA<Cfunction_sp>(tfunction));
      ComplexVector_T_sp annotations = gc::As_unsafe<Cfunction_sp>(tfunction)->annotations();
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
  ASSERT(gc::IsA<Cfunction_sp>(tlast_cfunction));
  Cfunction_sp last_cfunction = gc::As_unsafe<Cfunction_sp>(tlast_cfunction);
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
    ASSERT(gc::IsA<Cfunction_sp>(tfunction));
    Cfunction_sp function = gc::As_unsafe<Cfunction_sp>(tfunction);
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
    ASSERT(gc::IsA<Cfunction_sp>(tfun));
    Cfunction_sp cfunction = gc::As_unsafe<Cfunction_sp>(tfun);
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

// Given a list of declaration specifiers as returned by extract_declares_etc,
// return a list of symbols declared notinline.
static List_sp decl_notinlines(List_sp declares) {
  ql::list result;
  for (auto cur : declares) {
    T_sp spec = oCar(cur);
    if (gc::IsA<Cons_sp>(spec)
        && (CONS_CAR(spec) == cl::_sym_notinline)) {
      ASSERT(gc::IsA<List_sp>(CONS_CDR(spec)));
      for (auto dc : gc::As_unsafe<List_sp>(CONS_CDR(spec))) result << oCar(dc);
    }
  }
  return result.cons();
}

CL_DEFUN void compile_locally(List_sp body, Lexenv_sp env, Context_sp ctxt) {
  List_sp declares = nil<T_O>();
  gc::Nilable<String_sp> docstring;
  List_sp code;
  List_sp specials;
  eval::extract_declares_docstring_code_specials(body, declares,
                                                 false, docstring, code, specials);
  Lexenv_sp inner1 = env->add_specials(specials);
  Lexenv_sp inner2 = env->add_notinlines(decl_notinlines(declares));
  compile_progn(code, inner2, ctxt);
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
      ASSERT(gc::IsA<Symbol_sp>(oCar(binding)));
      var = gc::As_unsafe<Symbol_sp>(oCar(binding));
      valf = oCadr(binding);
    } else {
      ASSERT(gc::IsA<Symbol_sp>(binding));
      var = gc::As_unsafe<Symbol_sp>(binding);
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
  post_binding_env = post_binding_env->add_notinlines(decl_notinlines(declares));
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
      ASSERT(gc::IsA<Symbol_sp>(oCar(binding)));
      var = gc::As_unsafe<Symbol_sp>(oCar(binding));
      valf = oCadr(binding);
    } else {
      ASSERT(gc::IsA<Symbol_sp>(binding));
      var = gc::As_unsafe<Symbol_sp>(binding);
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
  new_env = new_env->add_notinlines(decl_notinlines(declares));
  // We make a new environment to make sure free special declarations get
  // through even if this let* doesn't bind them.
  // This creates duplicate alist entries for anything that _is_ bound
  // here, but that's not a big deal.
  compile_progn(code, new_env->add_specials(specials), ctxt);
  ctxt->emit_unbind(special_binding_count);
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

CL_DEFUN Lexenv_sp compile_optional_or_key_item(Symbol_sp var,
                                                T_sp defaulting_form,
                                                size_t var_index,
                                                Symbol_sp supplied_var,
                                                Label_sp next_label,
                                                bool var_specialp,
                                                bool supplied_specialp,
                                                Context_sp context,
                                                Lexenv_sp env) {
  Label_sp supplied_label = Label_O::make();
  T_sp varinfo = var_info(var, env);
  T_sp supinfo = nil<T_O>();
  if (supplied_var.notnilp()) {
    env = env->bind_vars(Cons_O::createList(supplied_var), context);
    supinfo = var_info(supplied_var, env);
  }
  context->emit_jump_if_supplied(supplied_label, var_index);
  // Emit code for the case of the variable not being supplied:
  // Bind the var to the default, and the suppliedvar to NIL if applicable.
  compile_form(defaulting_form, env, context->sub(clasp_make_fixnum(1)));
  if (var_specialp)
    context->emit_special_bind(var);
  else {
    ASSERT(gc::IsA<LexicalVarInfo_sp>(varinfo));
    context->maybe_emit_make_cell(gc::As_unsafe<LexicalVarInfo_sp>(varinfo));
    context->assemble1(vm_set, var_index);
  }
  if (supplied_var.notnilp()) { // bind supplied_var to NIL
    context->assemble0(vm_nil);
    if (supplied_specialp)
      context->emit_special_bind(supplied_var);
    else {
      ASSERT(gc::IsA<LexicalVarInfo_sp>(supinfo));
      LexicalVarInfo_sp lsinfo = gc::As_unsafe<LexicalVarInfo_sp>(supinfo);
      context->maybe_emit_make_cell(lsinfo);
      context->assemble1(vm_set, lsinfo->frameIndex());
    }
  }
  context->emit_jump(next_label);
  // Now for when the variable is supplied.
  supplied_label->contextualize(context);
  if (var_specialp) { // we have it in a reg, so rebind
    context->assemble1(vm_ref, var_index);
    context->emit_special_bind(var);
  } else { // in the reg already, but maybe needs a cell 
    ASSERT(gc::IsA<LexicalVarInfo_sp>(varinfo));
    context->maybe_emit_encage(gc::As_unsafe<LexicalVarInfo_sp>(varinfo));
  }
  if (supplied_var.notnilp()) {
    compile_literal(cl::_sym_T_O, env, context->sub(clasp_make_fixnum(1)));
    if (supplied_specialp)
      context->emit_special_bind(supplied_var);
    else {
      ASSERT(gc::IsA<LexicalVarInfo_sp>(supinfo));
      LexicalVarInfo_sp lsinfo = gc::As_unsafe<LexicalVarInfo_sp>(supinfo);
      context->maybe_emit_make_cell(lsinfo);
      context->assemble1(vm_set, lsinfo->frameIndex());
    }
  }
  // That's it for code generation. Now return the new environment.
  if (var_specialp) env = env->add_specials(Cons_O::createList(var));
  if (supplied_specialp) env = env->add_specials(Cons_O::createList(supplied_var));
  return env;
}

CL_DEFUN void compile_with_lambda_list(T_sp lambda_list, List_sp body,
                                       Lexenv_sp env, Context_sp context) {
  List_sp declares = nil<T_O>();
  gc::Nilable<String_sp> docstring;
  List_sp code;
  List_sp specials;
  eval::extract_declares_docstring_code_specials(body, declares,
                                                 true, docstring, code, specials);
  // docstring and declares ignored
  gctools::Vec0<RequiredArgument> reqs;
  gctools::Vec0<OptionalArgument> optionals;
  gctools::Vec0<KeywordArgument> keys;
  gctools::Vec0<AuxArgument> auxs;
  RestArgument restarg;
  T_sp key_flag;
  T_sp aokp;
  parse_lambda_list(lambda_list, cl::_sym_Function_O,
                    reqs, optionals, restarg,
                    key_flag, keys, aokp, auxs);
  Cfunction_sp function = context->cfunction();
  Label_sp entry_point = function->entry_point();
  size_t min_count = reqs.size();
  size_t optional_count = optionals.size();
  size_t max_count = min_count + optional_count;
  bool morep = restarg._ArgTarget.notnilp() || key_flag.notnilp();
  ql::list lreqs;
  for (auto &it : reqs) lreqs << it._ArgTarget;
  Lexenv_sp new_env = env->bind_vars(lreqs.cons(), context);
  size_t special_binding_count = 0;
  // An alist from optional and key variables to their local indices.
  // This is needed so that we can properly mark any that are special as
  // such while leaving them temporarily "lexically" bound during
  // argument parsing.
  List_sp opt_key_indices = nil<T_O>();

  entry_point->contextualize(context);
  // Generate argument count check.
  if ((min_count > 0) && (min_count == max_count) && !morep)
    context->assemble1(vm_check_arg_count_EQ, min_count);
  else {
    if (min_count > 0)
      context->assemble1(vm_check_arg_count_GE, min_count);
    if (!morep)
      context->assemble1(vm_check_arg_count_LE, max_count);
  }
  if (min_count > 0) {
    // Bind the required arguments.
    context->assemble1(vm_bind_required_args, min_count);
    ql::list sreqs; // required parameters that are special
    for (auto &it : reqs) {
      // We account for special declarations in outer environments/globally
      // by checking the original environment - not our new one - for info.
      T_sp var = it._ArgTarget;
      ASSERT(gc::IsA<LexicalVarInfo_sp>(var_info(var, new_env)));
      LexicalVarInfo_sp lvinfo = gc::As_unsafe<LexicalVarInfo_sp>(var_info(var, new_env));
      if (special_binding_p(var, specials, env)) {
        sreqs << var;
        context->assemble1(vm_ref, lvinfo->frameIndex());
        context->emit_special_bind(var);
        ++special_binding_count; // not in lisp - bug?
      } else {
        ASSERT(gc::IsA<LexicalVarInfo_sp>(lvinfo));
        context->maybe_emit_encage(gc::As_unsafe<LexicalVarInfo_sp>(lvinfo));
      }
    }
    new_env = new_env->add_specials(sreqs.cons());
  }
  if (optional_count > 0) {
    // Generate code to bind the provided optional args, unprovided args will
    // be initialized with the unbound marker.
    context->assemble2(vm_bind_optional_args, min_count, optional_count);
    // Mark the locations of each optional. Note that we do this even if
    // the variable will be specially bound.
    ql::list opts;
    for (auto &it : optionals) opts << it._ArgTarget;
    new_env = new_env->bind_vars(opts.cons(), context);
    // Add everything to opt-key-indices.
    for (auto &it : optionals) {
      T_sp var = it._ArgTarget;
      ASSERT(gc::IsA<LexicalVarInfo_sp>(var_info(var, new_env)));
      LexicalVarInfo_sp lvinfo = gc::As_unsafe<LexicalVarInfo_sp>(var_info(var, new_env));
      opt_key_indices = Cons_O::create(Cons_O::create(var,
                                                      clasp_make_fixnum(lvinfo->frameIndex())),
                                       opt_key_indices);
    }
    // Re-mark anything that's special in the outer context as such, so that
    // default initforms properly treat them as special.
    ql::list sopts;
    for (auto &it : optionals)
      if (gc::IsA<SpecialVarInfo_sp>(var_info(it._ArgTarget, env)))
        sopts << it._ArgTarget;
    new_env = new_env->add_specials(sopts.cons());
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
        first_key_index = context->new_literal_index(it._Keyword);
        set_first_key_index = true;
      } else context->new_literal_index(it._Keyword);
    }
    // now the actual instruction
    context->emit_parse_key_args(max_count, keys.size(),
                                 first_key_index,
                                 new_env->frameEnd(),
                                 aokp.notnilp());
    ql::list keyvars;
    ql::list skeys;
    for (auto &it : keys) {
      keyvars << it._ArgTarget;
      if (gc::IsA<SpecialVarInfo_sp>(var_info(it._ArgTarget, env)))
        skeys << it._ArgTarget;
    }
    new_env = new_env->bind_vars(keyvars.cons(), context);
    for (auto &it : keys) {
      T_sp var = it._ArgTarget;
      ASSERT(gc::IsA<LexicalVarInfo_sp>(var_info(var, new_env)));
      LexicalVarInfo_sp lvinfo = gc::As_unsafe<LexicalVarInfo_sp>(var_info(var, new_env));
      opt_key_indices = Cons_O::create(Cons_O::create(var,
                                                      clasp_make_fixnum(lvinfo->frameIndex())),
                                       opt_key_indices);
    }
    new_env = new_env->add_specials(skeys.cons());
  }
  // Generate defaulting code for optional args, and special-bind them
  // if necessary.
  if (optional_count > 0) {
    Label_sp optional_label = Label_O::make();
    Label_sp next_optional_label = Label_O::make();
    for (auto &it : optionals) {
      optional_label->contextualize(context);
      T_sp optional_var = it._ArgTarget;
      T_sp defaulting_form = it._Default;
      T_sp supplied_var = it._Sensor._ArgTarget;
      bool optional_special_p = special_binding_p(optional_var, specials, env);
      ASSERT(gc::IsA<Cons_sp>(opt_key_indices));
      T_sp pair = core__alist_assoc_eq(gc::As_unsafe<Cons_sp>(opt_key_indices),optional_var);
      size_t index = oCdr(pair).unsafe_fixnum();
      bool supplied_special_p
        = supplied_var.notnilp() && special_binding_p(supplied_var, specials, env);
      new_env = compile_optional_or_key_item(optional_var, defaulting_form, index,
                                             supplied_var, next_optional_label,
                                             optional_special_p, supplied_special_p,
                                             context, new_env);
      if (optional_special_p) ++special_binding_count;
      if (supplied_special_p) ++special_binding_count;
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
      context->assemble1(vm_vaslistify_rest_args, max_count);
    } else {
      context->assemble1(vm_listify_rest_args, max_count);
    }
    context->assemble1(vm_set, new_env->frameEnd());
    new_env = new_env->bind_vars(Cons_O::createList(rest), context);
    ASSERT(gc::IsA<LexicalVarInfo_sp>(var_info(rest, new_env)));
    LexicalVarInfo_sp lvinfo = gc::As_unsafe<LexicalVarInfo_sp>(var_info(rest, new_env));
    if (special_binding_p(rest, specials, env)) {
      context->assemble1(vm_ref, lvinfo->frameIndex());
      context->emit_special_bind(rest);
      ++special_binding_count;
      new_env = new_env->add_specials(Cons_O::createList(rest));
    } else
      context->maybe_emit_encage(lvinfo);
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
      T_sp pair = core__alist_assoc_eq( gc::As<Cons_sp>(opt_key_indices), key_var );
      size_t index = oCdr(pair).unsafe_fixnum();
      bool supplied_special_p
        = supplied_var.notnilp() && special_binding_p(supplied_var, specials, env);
      new_env = compile_optional_or_key_item(key_var, defaulting_form, index,
                                             supplied_var, next_key_label,
                                             key_special_p, supplied_special_p,
                                             context, new_env);
      if (key_special_p) ++special_binding_count;
      if (supplied_special_p) ++special_binding_count;
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
  T_sp notinlinedecl = Cons_O::create(cl::_sym_notinline,
                                      decl_notinlines(declares));
  T_sp declexpr = Cons_O::createList(cl::_sym_declare,
                                     specialdecl, notinlinedecl);
  T_sp lbody = Cons_O::create(declexpr, code);
  compile_letSTAR(auxbinds.cons(), lbody, new_env, context);
  // Finally, clean up any special bindings.
  context->emit_unbind(special_binding_count);
}

// Compile the lambda expression in MODULE, returning the resulting CFUNCTION.
CL_DEFUN Cfunction_sp compile_lambda(T_sp lambda_list, List_sp body,
                                     Lexenv_sp env, Module_sp module) {
  List_sp declares = nil<T_O>();
  gc::Nilable<String_sp> docstring;
  List_sp code;
  List_sp specials;
  eval::extract_declares_docstring_code_specials(body, declares,
                                                 true, docstring, code, specials);
  // We pass the original body w/declarations to compile-with-lambda-list
  // so that it can do its own handling of specials, etc.
  T_sp name = extract_lambda_name_from_declares(declares);
  if (name.nilp())
    name = Cons_O::createList(cl::_sym_lambda,
                              comp::lambda_list_for_name(lambda_list));
  Cfunction_sp function = Cfunction_O::make(module, name, docstring, lambda_list);
  Context_sp context = Context_O::make(cl::_sym_T_O, function);
  Lexenv_sp lenv = Lexenv_O::make(env->vars(), env->tags(),
                                  env->blocks(), env->funs(),
                                  env->notinlines(), 0);
  Fixnum_sp ind = module->cfunctions()->vectorPushExtend(function);
  function->setIndex(ind.unsafe_fixnum());
  compile_with_lambda_list(lambda_list, body, lenv, context);
  context->assemble0(vm_return);
  return function;
}

CL_DEFUN void compile_function(T_sp fnameoid, Lexenv_sp env, Context_sp ctxt) {
  bool mvp;
  if (!(ctxt->receiving().fixnump())) mvp = true;
  else if (ctxt->receiving().unsafe_fixnum() == 0) return;
  else mvp = false;
  if (gc::IsA<Cons_sp>(fnameoid) && oCar(fnameoid) == cl::_sym_lambda) {
    Cfunction_sp fun = compile_lambda(oCadr(fnameoid), oCddr(fnameoid),
                                      env, ctxt->module());
    ComplexVector_T_sp closed = fun->closed();
    for (size_t i = 0; i < closed->length(); ++i) {
      ASSERT(gc::IsA<LexicalVarInfo_sp>((*closed)[i]));
      ctxt->reference_lexical_info(gc::As_unsafe<LexicalVarInfo_sp>((*closed)[i]));
    }
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
      ASSERT(gc::IsA<LexicalVarInfo_sp>(lfinfo->funVar()));
      LexicalVarInfo_sp lvinfo = gc::As_unsafe<LexicalVarInfo_sp>(lfinfo->funVar());
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
    ASSERT(gc::IsA<Cons_sp>(oCar(cur)));
    Cons_sp definition = gc::As_unsafe<Cons_sp>(oCar(cur));
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
                                      new_env1->notinlines(),
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
    ASSERT(gc::IsA<Cons_sp>(oCar(cur)));
    Cons_sp definition = gc::As_unsafe<Cons_sp>(oCar(cur));
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
                                      new_env1->notinlines(),
                                      new_env1->frameEnd());
  for (auto cur : definitions) {
    Cons_sp definition = gc::As_unsafe<Cons_sp>(oCar(cur));
    T_sp name = oCar(definition);
    T_sp locally = Cons_O::create(cl::_sym_locally, oCddr(definition));
    T_sp block = Cons_O::createList(cl::_sym_block,
                                    core__function_block_name(name),
                                    locally);
    Cfunction_sp fun = compile_lambda(oCadr(definition),
                                      Cons_O::createList(block),
                                      new_env2, ctxt->module());
    size_t literal_index = ctxt->literal_index(fun);
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
  ASSERT(gc::IsA<List_sp>(closures.cons()));
  for (auto cur : gc::As_unsafe<List_sp>(closures.cons())) {
    Cfunction_sp cf = gc::As_unsafe<Cfunction_sp>(oCaar(cur));
    ComplexVector_T_sp closed = cf->closed();
    for (size_t i = 0; i < closed->length(); ++i) {
      ASSERT(gc::IsA<LexicalVarInfo_sp>((*closed)[i]));
      LexicalVarInfo_sp info = gc::As_unsafe<LexicalVarInfo_sp>((*closed)[i]);
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
      ASSERT(gc::IsA<Symbol_sp>(oCar(pairs)));
      Symbol_sp var = gc::As_unsafe<Symbol_sp>(oCar(pairs));
      T_sp valf = oCadr(pairs);
      ASSERT(gc::IsA<List_sp>(oCddr(pairs)));
      pairs = gc::As_unsafe<List_sp>(oCddr(pairs));
      compile_setq_1(var, valf, env,
                     pairs.notnilp() ? ctxt->sub(clasp_make_fixnum(0)) : ctxt);
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

CL_DEFUN void compile_eval_when(List_sp situations, List_sp body,
                                Lexenv_sp env, Context_sp ctxt) {
  if (eval_when_execp(situations))
    compile_progn(body, env, ctxt);
  else compile_literal(nil<T_O>(), env, ctxt);
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
  ASSERT(gc::IsA<List_sp>(env->tags()));
  List_sp new_tags = gc::As_unsafe<List_sp>(env->tags());
  Symbol_sp tagbody_dynenv = cl__gensym(SimpleBaseString_O::make("TAG-DYNENV"));
  Lexenv_sp nenv = env->bind_vars(Cons_O::createList(tagbody_dynenv), ctxt);
  ASSERT(gc::IsA<LexicalVarInfo_sp>(var_info(tagbody_dynenv, nenv)));
  LexicalVarInfo_sp dynenv_info = gc::As_unsafe<LexicalVarInfo_sp>(var_info(tagbody_dynenv, nenv));
  for (auto cur : statements) {
    T_sp statement = oCar(cur);
    if (go_tag_p(statement))
      new_tags = Cons_O::create(Cons_O::create(statement,
                                               Cons_O::create(dynenv_info,
                                                              Label_O::make())),
                                new_tags);
  }
  Lexenv_sp nnenv = Lexenv_O::make(nenv->vars(), new_tags, nenv->blocks(),
                                   nenv->funs(), nenv->notinlines(),
                                   nenv->frameEnd());
  // Bind the dynamic environment.
  ctxt->assemble1(vm_entry, dynenv_info->frameIndex());
  // Compile the body, emitting the tag destination labels.
  for (auto cur : statements) {
    T_sp statement = oCar(cur);
    if (go_tag_p(statement)) {
      T_sp info = core__alist_assoc_eql(gc::As<Cons_sp>(nnenv->tags()), statement);
      ASSERT(gc::IsA<Label_sp>(oCddr(info)));
      Label_sp lab = gc::As_unsafe<Label_sp>(oCddr(info));
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
  if (tags.consp()) {
    // tags must be a cons now
    T_sp pair = core__alist_assoc_eql( gc::As<Cons_sp>(tags), tag );
    if (pair.consp()) {
      ASSERT(gc::IsA<Cons_sp>(CONS_CDR(pair)));
      Cons_sp rpair = gc::As_unsafe<Cons_sp>(CONS_CDR(pair));
      ASSERT(gc::IsA<Cons_sp>(rpair));
      ASSERT(gc::IsA<LexicalVarInfo_sp>(CONS_CAR(rpair)));
      ctxt->reference_lexical_info(gc::As_unsafe<LexicalVarInfo_sp>(CONS_CAR(rpair)));
      ASSERT(gc::IsA<Label_sp>(CONS_CDR(rpair)));
      ctxt->emit_exit(gc::As_unsafe<Label_sp>(CONS_CDR(rpair)));
      return;
    }
  }
  SIMPLE_ERROR("The GO tag %s does not exist.", _rep_(tag));
}

CL_DEFUN void compile_block(Symbol_sp name, List_sp body,
                                 Lexenv_sp env, Context_sp ctxt) {
  Symbol_sp block_dynenv = cl__gensym(SimpleBaseString_O::make("BLOCK-DYNENV"));
  Lexenv_sp nenv = env->bind_vars(Cons_O::createList(block_dynenv), ctxt);
  ASSERT(gc::IsA<LexicalVarInfo_sp>(var_info(block_dynenv, nenv)));
  LexicalVarInfo_sp dynenv_info = gc::As_unsafe<LexicalVarInfo_sp>(var_info(block_dynenv, nenv));
  Label_sp label = Label_O::make();
  Label_sp normal_label = Label_O::make();
  // Bind the dynamic environment.
  ctxt->assemble1(vm_entry, dynenv_info->frameIndex());
  Cons_sp new_pair = Cons_O::create(name, Cons_O::create(dynenv_info, label));
  Lexenv_sp nnenv = Lexenv_O::make(nenv->vars(), nenv->tags(),
                                   Cons_O::create(new_pair, nenv->blocks()),
                                   nenv->funs(), nenv->notinlines(),
                                   nenv->frameEnd());
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
  if (blocks.consp()) {
    // blocks must be a cons now
    T_sp pair = core__alist_assoc_eq( gc::As_unsafe<Cons_sp>(blocks), name );
    if (pair.consp()) {
      ASSERT(gc::IsA<Cons_sp>(CONS_CDR(pair)));
      Cons_sp rpair = gc::As_unsafe<Cons_sp>(CONS_CDR(pair));
      ASSERT(gc::IsA<Cons_sp>(rpair));
      ASSERT(gc::IsA<LexicalVarInfo_sp>(CONS_CAR(rpair)));
      ctxt->reference_lexical_info(gc::As_unsafe<LexicalVarInfo_sp>(CONS_CAR(rpair)));
      ASSERT(gc::IsA<Label_sp>(CONS_CDR(rpair)));
      ctxt->emit_exit(gc::As_unsafe<Label_sp>(CONS_CDR(rpair)));
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
  ASSERT(gc::IsA<List_sp>(oCdr(aforms)));
  List_sp rest = gc::As_unsafe<List_sp>(oCdr(aforms));
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
  ASSERT(gc::IsA<List_sp>(args));
  for (auto cur : gc::As_unsafe<List_sp>(args)) {
    ++argcount;
    compile_form(oCar(cur), env, context->sub(clasp_make_fixnum(1)));
  }
  // generate the call
  context->emit_call(argcount);
}

CL_DEFUN Lexenv_sp make_null_lexical_environment() {
  return Lexenv_O::make(nil<T_O>(), nil<T_O>(), nil<T_O>(), nil<T_O>(),
                        nil<T_O>(), 0);
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

static T_sp symbol_macrolet_bindings(Lexenv_sp menv, List_sp bindings,
                                     T_sp vars) {
  for (auto cur : bindings) {
    T_sp binding = oCar(cur);
    ASSERT(gc::IsA<Symbol_sp>(oCar(binding)));
    Symbol_sp name = gc::As_unsafe<Symbol_sp>(oCar(binding));
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
  return vars;
}

CL_DEFUN void compile_symbol_macrolet(List_sp bindings, List_sp body,
                                      Lexenv_sp env, Context_sp context) {
  T_sp vars = symbol_macrolet_bindings(env->macroexpansion_environment(),
                                       bindings, env->vars());
  Lexenv_sp nenv = Lexenv_O::make(vars, env->tags(), env->blocks(), env->funs(),
                                  env->notinlines(), env->frameEnd());
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
    T_sp eform = eval::funcall(ext::_sym_parse_macro,
                               name, lambda_list, body, menv);
    GlobalBytecodeEntryPoint_sp expander = bytecompile(eform, menv);
    LocalMacroInfo_sp info = LocalMacroInfo_O::make(expander);
    funs = Cons_O::create(Cons_O::create(name, info), funs);
  }
  return funs;
}

CL_DEFUN void compile_macrolet(List_sp bindings, List_sp body,
                               Lexenv_sp env, Context_sp context) {
  T_sp funs = macrolet_bindings(env->macroexpansion_environment(),
                                bindings, env->funs());
  Lexenv_sp nenv = Lexenv_O::make(env->vars(), env->tags(), env->blocks(),
                                  funs, env->notinlines(), env->frameEnd());
  compile_locally(body, nenv, context);
}

CL_DEFUN void compile_funcall(T_sp callee, List_sp args,
                              Lexenv_sp env, Context_sp context) {
  compile_form(callee, env, context->sub(clasp_make_fixnum(1)));
  compile_call(args, env, context);
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
  // extension
  else if (head == cleavirPrimop::_sym_funcall)
    compile_funcall(oCar(rest), oCdr(rest), env, context);
  // not a special form
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
      } else if (gc::IsA<GlobalFunInfo_sp>(info)) {
        GlobalFunInfo_sp gfinfo = gc::As_unsafe<GlobalFunInfo_sp>(info);
        T_sp cmexpander = gfinfo->cmexpander();
        if (cmexpander.notnilp() && !env->notinlinep(head)) {
          // Compiler macroexpand
          T_sp form = Cons_O::create(head, rest);
          ASSERT(gc::IsA<Function_sp>(cmexpander));
          T_sp expansion = expand_macro(gc::As_unsafe<Function_sp>(cmexpander),
                                        form, env);
          if (expansion != form) {
            compile_form(expansion, env, context);
            return;
          }
        } // no compiler macro, or expansion declined: call
        compile_function(head, env, context->sub(clasp_make_fixnum(1)));
        compile_call(rest, env, context);
      } else if (gc::IsA<LocalFunInfo_sp>(info) || info.nilp()) {
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
    } else SIMPLE_ERROR("Illegal combination head: %s rest: %s", _rep_(head), _rep_(rest));
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

CL_LAMBDA(lambda-expression &optional (env (cmp::make-null-lexical-environment)))
CL_DEFUN GlobalBytecodeEntryPoint_sp bytecompile(T_sp lambda_expression,
                                                 Lexenv_sp env) {
  if (!gc::IsA<Cons_sp>(lambda_expression)
      || (oCar(lambda_expression) != cl::_sym_lambda))
    SIMPLE_ERROR("bytecompile passed a non-lambda-expression: %s",
                 _rep_(lambda_expression));
  Module_sp module = Module_O::make();
  T_sp lambda_list = oCadr(lambda_expression);
  T_sp body = oCddr(lambda_expression);
  Cfunction_sp cf = compile_lambda(lambda_list, body, env, module);
  return cf->link_function(Cons_O::create(lambda_expression, env));
}

static Lexenv_sp coerce_lexenv_desig(T_sp env) {
  if (env.nilp())
    return Lexenv_O::make(nil<T_O>(), nil<T_O>(), nil<T_O>(), nil<T_O>(), nil<T_O>(), 0);
  else return gc::As<Lexenv_sp>(env);
}

SYMBOL_EXPORT_SC_(CompPkg, bytecode_implicit_compile_form);

CL_LAMBDA(form &optional env)
CL_DEFUN T_mv cmp__bytecode_implicit_compile_form(T_sp form, T_sp env) {
  T_sp lexpr = Cons_O::createList(cl::_sym_lambda, nil<T_O>(),
                                  Cons_O::createList(cl::_sym_declare),
                                  Cons_O::createList(cl::_sym_progn, form));
//  printf("%s:%d:%s lexpr = %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(lexpr).c_str());
  Function_sp thunk = bytecompile(lexpr, coerce_lexenv_desig(env));
  return eval::funcall(thunk);
}

T_mv bytecode_toplevel_eval(T_sp, T_sp);

CL_DEFUN T_mv bytecode_toplevel_progn(List_sp forms, Lexenv_sp env) {
  for (auto cur : forms)
    if (oCdr(cur).nilp()) // done
      return bytecode_toplevel_eval(oCar(cur), env);
    else bytecode_toplevel_eval(oCar(cur), env);
  // If there are no forms, return NIL.
  return Values(nil<T_O>());
}

CL_DEFUN T_mv bytecode_toplevel_eval_when(List_sp situations, List_sp forms,
                                          Lexenv_sp env) {
  if (eval_when_execp(situations))
    return bytecode_toplevel_progn(forms, env);
  else return nil<T_O>();
}

CL_DEFUN T_mv bytecode_toplevel_locally(List_sp body, Lexenv_sp env) {
  List_sp declares = nil<T_O>();
  gc::Nilable<String_sp> docstring;
  List_sp code;
  List_sp specials;
  eval::extract_declares_docstring_code_specials(body, declares,
                                                 false, docstring, code, specials);
  Lexenv_sp inner1 = env->add_specials(specials);
  Lexenv_sp inner2 = env->add_notinlines(decl_notinlines(declares));
  return bytecode_toplevel_progn(code, inner2);
}

CL_DEFUN T_mv bytecode_toplevel_macrolet(List_sp bindings, List_sp body,
                                         Lexenv_sp env) {
  // FIXME: We can maybe skip macroexpansion_environment,
  // assuming bytecode_toplevel_eval was originally actually called
  // with an empty lexenv as it ought to be.
  T_sp funs = macrolet_bindings(env->macroexpansion_environment(),
                                bindings, env->funs());
  Lexenv_sp nenv = Lexenv_O::make(env->vars(), env->tags(), env->blocks(),
                                  funs, env->notinlines(), env->frameEnd());
  return bytecode_toplevel_locally(body, nenv);
}

CL_DEFUN T_mv bytecode_toplevel_symbol_macrolet(List_sp bindings, List_sp body,
                                                Lexenv_sp env) {
  T_sp vars = symbol_macrolet_bindings(env->macroexpansion_environment(),
                                       bindings, env->vars());
  Lexenv_sp nenv = Lexenv_O::make(vars, env->tags(), env->blocks(), env->funs(),
                                  env->notinlines(), env->frameEnd());
  return bytecode_toplevel_locally(body, nenv);
}

SYMBOL_EXPORT_SC_(CompPkg, bytecode_toplevel_eval);

CL_LAMBDA(form &optional env)
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
    else return cmp__bytecode_implicit_compile_form(eform, env);
  } else return cmp__bytecode_implicit_compile_form(eform, env);
}

}; //namespace comp
