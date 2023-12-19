/*
    File: write_list.cc
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
// #define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/arguments.h>
#include <clasp/core/cons.h>
#include <clasp/core/write_ugly.h>
#include <clasp/core/print.h>

#include <clasp/core/character.h>

#include <clasp/core/wrappers.h>

namespace core {

bool Cons_O::maybe_write_quoted_form(bool tail, T_sp stream) const {
  if (car().nilp() || !cdr().consp() || oCdr(cdr()).notnilp())
    return false;

  T_sp op = car();

  if (op == cl::_sym_quote) {
    if (tail)
      return false;
    stream_write_char(stream, '\'');
    write_object(oCar(cdr()), stream);
    return true;
  }

  if (op == cl::_sym_function) {
    if (tail)
      return false;
    clasp_write_string("#'", stream);
    write_object(oCar(cdr()), stream);
    return true;
  }

  T_sp quasiquote = _sym_STARquasiquoteSTAR->symbolValue();
  T_sp iq = nil<T_O>();

  if (op == _sym_quasiquote) {
    iq = _lisp->_true();
    clasp_write_string(tail ? " . `" : "`", stream);
  } else if (cl__getf(quasiquote, stream, nil<T_O>()).nilp()) {
    return false;
  } else if (op == _sym_unquote) {
    clasp_write_string(tail ? " . ," : ",", stream);
  } else if (op == _sym_unquote_splice) {
    clasp_write_string(tail ? " . ,@" : ",@", stream);
  } else if (op == _sym_unquote_nsplice) {
    clasp_write_string(tail ? " . ,." : ",.", stream);
  } else {
    return false;
  }

  DynamicScopeManager scope(_sym_STARquasiquoteSTAR, Cons_O::create(stream, Cons_O::create(iq, quasiquote)));

  write_object(oCar(cdr()), stream);

  return true;
}

void Cons_O::__write__(T_sp stream) const {
  stream = coerce::outputStreamDesignator(stream);

  if (maybe_write_quoted_form(false, stream))
    return;

  bool circle = clasp_print_circle();
  Fixnum print_level, print_length;

  if (clasp_print_readably()) {
    print_level = MOST_POSITIVE_FIXNUM;
    print_length = MOST_POSITIVE_FIXNUM;
  } else {
    print_level = clasp_print_level();
    print_length = clasp_print_length();
  }

  if (print_level == 0) {
    stream_write_char(stream, '#');
    return;
  }

  T_sp x = this->const_sharedThis<Cons_O>();

  stream_write_char(stream, '(');

  for (_Index i = 0;; i++) {
    if (i >= print_length) {
      clasp_write_string("...", stream);
      break;
    }

    // recursion with printlevel -1
    DynamicScopeManager scope(cl::_sym_STARprint_levelSTAR, clasp_make_fixnum(print_level - 1));
    write_object(oCar(x), stream);

    x = oCdr(x);

    if (x.nilp())
      break;

    if (cl__atom(x) || (circle && will_print_as_hash(x))) {
      clasp_write_string(" . ", stream);
      write_object(x, stream);
      break;
    }

    if (x.consp() && gc::As<Cons_sp>(x)->maybe_write_quoted_form(true, stream))
      break;

    stream_write_char(stream, ' ');
  }

  stream_write_char(stream, ')');
}
}; // namespace core
