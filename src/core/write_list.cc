/*
    File: write_list.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
#define	DEBUG_LEVEL_FULL

#include "core/foundation.h"
#include "core/object.h"
#include "core/cons.h"
#include "core/symbolTable.h"
#include "core/designators.h"
#include "core/lispStream.h"
#include "core/hashTable.h"
#include "core/arguments.h"
#include "core/cons.h"
#include "core/write_ugly.h"
#include "core/print.h"


#include "core/character.h"


#include "core/wrappers.h"

namespace core
{

    void Cons_O::__write__(T_sp stream) const
    {
	T_sp x;
	bool circle;
	Fixnum print_level, print_length;
	_Index i;
	T_sp y;
	SYMBOL_EXPORT_SC_(CorePkg,_SHARP__BANG_);
	if ( this->_Car == _sym__SHARP__BANG_) {
	    clasp_write_string("#!",stream);
	    x = this->_Cdr;
	    write_object(x,stream);
	    return;
	}
	if (af_consP(this->_Cdr) && oCdr(this->_Cdr).nilp() ) {
	    if ( this->_Car == cl::_sym_quote ) {
		clasp_write_char('\'',stream);
		x = oCar(this->_Cdr);
		write_object(x,stream);
		return;
	    }
	    if ( this->_Car == cl::_sym_function) {
		clasp_write_char('#',stream);
		clasp_write_char('\'',stream);
		x = oCar(this->_Cdr);
		write_object(x,stream);
		return;
	    }
	    if ( this->_Car == _sym_quasiquote ) {
		clasp_write_char('`',stream);
		x = oCar(this->_Cdr);
		write_object(x,stream);
		return;
	    }
	    if ( this->_Car == _sym_unquote ) {
		clasp_write_char(',',stream);
		x = oCar(this->_Cdr);
		write_object(x,stream);
		return;
	    }
	    if ( this->_Car == _sym_unquote_splice ) {
		clasp_write_string(",@",stream);
		x = oCar(this->_Cdr);
		write_object(x,stream);
		return;
	    }
	    if ( this->_Car == _sym_unquote_nsplice ) {
		clasp_write_string(",.",stream);
		x = oCar(this->_Cdr);
		write_object(x, stream);
		return;
	    }
	}
	circle = brcl_print_circle();
	if (clasp_print_readably()) {
	    print_level = MOST_POSITIVE_FIXNUM;
	    print_length = MOST_POSITIVE_FIXNUM;
	} else {
	    print_level = brcl_print_level();
	    print_length = brcl_print_length();
	}
	if (print_level == 0) {
	    clasp_write_char('#',stream);
	    return;
	}
	x = this->const_sharedThis<Cons_O>();
	clasp_write_char('(',stream);
	for (i = 0;  ;  i++) {
	    if (i >= print_length) {
		clasp_write_string("...",stream);
		break;
	    }
	    y = oCar(x);
	    x = oCdr(x);
	    write_object(y,stream);
	    /* FIXME! */
	    if (x._NULLp() || af_atom(x) || 
		(circle && will_print_as_hash(x)))
	    {
		if (!x.nilp()) {
		    clasp_write_char(' ',stream);
		    clasp_write_string(". ",stream);
		    write_object(x, stream);
		}
		break;
	    }
	    if (i == 0 && !y._NULLp() && af_symbolp(y))
		clasp_write_char(' ',stream);
	    else
		clasp_write_char(' ',stream);
	}
	clasp_write_char(')',stream);
    }



};
