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
//#define DEBUG_LEVEL_FULL

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

void Cons_O::__write__(T_sp stream) const {
  T_sp x;
  bool circle;
  Fixnum print_level, print_length;
  _Index i;
  T_sp y;
  SYMBOL_EXPORT_SC_(CorePkg, _SHARP__BANG_);
  if (this->ocar() == _sym__SHARP__BANG_) {
    clasp_write_string("#!", stream);
    x = this->cdr();
    write_object(x, stream);
    return;
  }
  if ((this->cdr()).consp() && oCdr(this->cdr()).nilp()) {
    if (this->ocar() == cl::_sym_quote) {
      clasp_write_char('\'', stream);
      x = oCar(this->cdr());
      write_object(x, stream);
      return;
    }
    if (this->ocar() == cl::_sym_function) {
      clasp_write_char('#', stream);
      clasp_write_char('\'', stream);
      x = oCar(this->cdr());
      write_object(x, stream);
      return;
    }
    if (this->ocar() == _sym_quasiquote) {
      clasp_write_char('`', stream);
      x = oCar(this->cdr());
      write_object(x, stream);
      return;
    }
    if (this->ocar() == _sym_unquote) {
      clasp_write_char(',', stream);
      x = oCar(this->cdr());
      write_object(x, stream);
      return;
    }
    if (this->ocar() == _sym_unquote_splice) {
      clasp_write_string(",@", stream);
      x = oCar(this->cdr());
      write_object(x, stream);
      return;
    }
    if (this->ocar() == _sym_unquote_nsplice) {
      clasp_write_string(",.", stream);
      x = oCar(this->cdr());
      write_object(x, stream);
      return;
    }
  }
  circle = clasp_print_circle();
  if (clasp_print_readably()) {
    print_level = MOST_POSITIVE_FIXNUM;
    print_length = MOST_POSITIVE_FIXNUM;
  } else {
    print_level = clasp_print_level();
    print_length = clasp_print_length();
  }
  if (print_level == 0) {
    clasp_write_char('#', stream);
    return;
  }
  x = this->const_sharedThis<Cons_O>();
  clasp_write_char('(', stream);
  for (i = 0;; i++) {
    if (i >= print_length) {
      clasp_write_string("...", stream);
      break;
    }
    y = oCar(x);
    x = oCdr(x);
    // recursion with printlevel -1
    DynamicScopeManager scope(cl::_sym_STARprint_levelSTAR, clasp_make_fixnum(print_level - 1));
    write_object(y, stream);
    /* FIXME! */
    if (!x || cl__atom(x) ||
        (circle && will_print_as_hash(x))) {
      if (!x.nilp()) {
        clasp_write_char(' ', stream);
        clasp_write_string(". ", stream);
        write_object(x, stream);
      }
      break;
    }
    if (i == 0 && !y._NULLp() && cl__symbolp(y))
      clasp_write_char(' ', stream);
    else
      clasp_write_char(' ', stream);
  }
  clasp_write_char(')', stream);
#if 0
	stringstream ss;
	ss << "@" << (void*)(this) << " ";
	clasp_write_string(ss.str(),stream);
#endif
}
};
