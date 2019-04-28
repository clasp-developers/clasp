/*
    File: cleavirIrPackage.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbol.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/primitives.h>
#include <clasp/core/cleavirEnvPackage.fwd.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/package.h>

namespace core {

SYMBOL_EXPORT_SC_(CleavirIrPkg,touched);
SYMBOL_EXPORT_SC_(CleavirIrPkg,enclose_instruction_p);
SYMBOL_EXPORT_SC_(CleavirIrPkg,enter_instruction_p);
SYMBOL_EXPORT_SC_(CleavirIrPkg,code);
SYMBOL_EXPORT_SC_(CleavirIrPkg,successors);

T_sp register_if_unvisited(T_sp visited, T_sp instruction, T_sp instructions_to_process) {
  T_sp instruction_touched = eval::funcall_function( cleavirIr::_sym_touched->symbolFunction(), instruction );
  // Ignore anything that doesn't fit into a FIXNUM
  if (instruction_touched.unsafe_fixnum() != visited.unsafe_fixnum()) {
    eval::funcall_function( cleavirIr::_sym_touched->getSetfFdefinition(), visited, instruction );
    instructions_to_process = Cons_O::create(instruction,instructions_to_process);
  }
  return instructions_to_process;
}



T_sp register_last_if_unvisited(T_sp visited, T_sp instruction, T_sp instructions_to_process) {
  T_sp instruction_touched = eval::funcall_function( cleavirIr::_sym_touched->symbolFunction(), instruction );
  // Ignore anything that doesn't fit into a FIXNUM
  if (instruction_touched.unsafe_fixnum() != visited.unsafe_fixnum()) {
    eval::funcall_function( cleavirIr::_sym_touched->getSetfFdefinition(), visited, instruction );
    if (instructions_to_process.nilp()) {
      instructions_to_process = Cons_O::create(instruction,_Nil<T_O>());
    } else {
      Cons_sp last = gc::As_unsafe<Cons_sp>(gc::As_unsafe<Cons_sp>(instructions_to_process)->last());
      last->_Cdr = Cons_O::create(instruction,_Nil<T_O>());
    }
  }
  return instructions_to_process;
}


CL_DEFUN void core__map_instructions_arbitrary_order(T_sp tfunction, T_sp initial_instruction)
{
  Function_sp function = interpreter_lookup_function_or_error(tfunction,_Nil<T_O>());
  Function_sp code_function = interpreter_lookup_function_or_error(cleavirIr::_sym_code,_Nil<T_O>());
  Function_sp successors_function = interpreter_lookup_function_or_error(cleavirIr::_sym_successors,_Nil<T_O>());
  Function_sp enclose_instruction_p_function = interpreter_lookup_function_or_error(cleavirIr::_sym_enclose_instruction_p,_Nil<T_O>());
  T_sp visited = core__next_number();
  T_sp instructions_to_process = _Nil<T_O>();
  instructions_to_process = register_if_unvisited(visited,initial_instruction,instructions_to_process);
  while (instructions_to_process.notnilp()) {
    T_sp instruction = CONS_CAR(instructions_to_process);
    instructions_to_process = CONS_CDR(instructions_to_process);
    core::eval::funcall_function(function,instruction);
    T_sp enclose_instruction_p = core::eval::funcall_function(enclose_instruction_p_function,instruction);
    if (enclose_instruction_p.notnilp()) {
      // When the instruction is an ENCLOSE-INSTRUCTION,
      // we must also account for the CODE slot of the
      // instruction, because it contains the
      // ENTER-INSTRUCTION of a nested function.
      instructions_to_process = register_if_unvisited(visited,core::eval::funcall_function(code_function,instruction),instructions_to_process);
    }
    // For each successor of the current instruction,
    // register it so that it will be processed
    // ultimately, unless, of course, it has already been
    // processed.
    List_sp successors = core::eval::funcall_function(successors_function,instruction);
    for ( auto cur : successors ) {
      T_sp cur_instruction = CONS_CAR(cur);
      instructions_to_process = register_if_unvisited(visited,cur_instruction,instructions_to_process);
    }
  }
}



CL_DEFUN void core__map_instructions(T_sp tfunction, T_sp initial_instruction)
{
  Function_sp function = interpreter_lookup_function_or_error(tfunction,_Nil<T_O>());
  Function_sp code_function = interpreter_lookup_function_or_error(cleavirIr::_sym_code,_Nil<T_O>());
  Function_sp successors_function = interpreter_lookup_function_or_error(cleavirIr::_sym_successors,_Nil<T_O>());
  Function_sp enclose_instruction_p_function = interpreter_lookup_function_or_error(cleavirIr::_sym_enclose_instruction_p,_Nil<T_O>());
  T_sp visited = core__next_number();
  T_sp instructions_to_process = _Nil<T_O>();
  instructions_to_process = register_if_unvisited(visited,initial_instruction,instructions_to_process);
  while (instructions_to_process.notnilp()) {
    T_sp instruction = CONS_CAR(instructions_to_process);
    instructions_to_process = CONS_CDR(instructions_to_process);
    core::eval::funcall_function(function,instruction);
    T_sp enclose_instruction_p = core::eval::funcall_function(enclose_instruction_p_function,instruction);
    if (enclose_instruction_p.notnilp()) {
      // When the instruction is an ENCLOSE-INSTRUCTION,
      // we must also account for the CODE slot of the
      // instruction, because it contains the
      // ENTER-INSTRUCTION of a nested function.
      instructions_to_process = register_last_if_unvisited(visited,core::eval::funcall_function(code_function,instruction),instructions_to_process);
    }
    // For each successor of the current instruction,
    // register it so that it will be processed
    // ultimately, unless, of course, it has already been
    // processed.
    List_sp successors = core::eval::funcall_function(successors_function,instruction);
    for ( auto cur : successors ) {
      T_sp cur_instruction = CONS_CAR(cur);
      instructions_to_process = register_if_unvisited(visited,cur_instruction,instructions_to_process);
    }
  }
}


CL_DEFUN void core__map_instructions_with_owner(T_sp tfunction, T_sp initial_instruction)
{
  Function_sp function = interpreter_lookup_function_or_error(tfunction,_Nil<T_O>());
  Function_sp code_function = interpreter_lookup_function_or_error(cleavirIr::_sym_code,_Nil<T_O>());
  Function_sp successors_function = interpreter_lookup_function_or_error(cleavirIr::_sym_successors,_Nil<T_O>());
  Function_sp enclose_instruction_p_function = interpreter_lookup_function_or_error(cleavirIr::_sym_enclose_instruction_p,_Nil<T_O>());
  Function_sp enter_instruction_p_function = interpreter_lookup_function_or_error(cleavirIr::_sym_enter_instruction_p,_Nil<T_O>());
  T_sp visited = core__next_number();
  T_sp instructions_to_process = _Nil<T_O>();
  T_sp current_owner = initial_instruction;
  instructions_to_process = register_if_unvisited(visited,initial_instruction,instructions_to_process);
  while (instructions_to_process.notnilp()) {
    T_sp instruction = CONS_CAR(instructions_to_process);
    instructions_to_process = CONS_CDR(instructions_to_process);
    T_sp enter_instruction_p = core::eval::funcall_function(enter_instruction_p_function,instruction);
    if (enter_instruction_p.notnilp()) {
      current_owner = instruction;
    }
    core::eval::funcall_function(function,instruction,current_owner);
    T_sp enclose_instruction_p = core::eval::funcall_function(enclose_instruction_p_function,instruction);
    if (enclose_instruction_p.notnilp()) {
      // When the instruction is an ENCLOSE-INSTRUCTION,
      // we must also account for the CODE slot of the
      // instruction, because it contains the
      // ENTER-INSTRUCTION of a nested function.
      instructions_to_process = register_last_if_unvisited(visited,core::eval::funcall_function(code_function,instruction),instructions_to_process);
    }
    // For each successor of the current instruction,
    // register it so that it will be processed
    // ultimately, unless, of course, it has already been
    // processed.
    List_sp successors = core::eval::funcall_function(successors_function,instruction);
    for ( auto cur : successors ) {
      T_sp cur_instruction = CONS_CAR(cur);
      instructions_to_process = register_if_unvisited(visited,cur_instruction,instructions_to_process);
    }
  }
}

};
