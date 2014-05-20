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

    void Cons_O::__write__(Stream_sp stream) const
    {
	T_sp x;
	bool circle;
	Fixnum print_level, print_length;
	_Index i;
	T_sp y;
	SYMBOL_EXPORT_SC_(CorePkg,_SHARP__BANG_);
	if ( this->_Car == _sym__SHARP__BANG_) {
	    stream->writeStr("#!");
	    x = this->_Cdr;
	    write_object(x,stream);
	    return;
	}
	if (af_consP(this->_Cdr) && oCdr(this->_Cdr).nilp() ) {
	    if ( this->_Car == cl::_sym_quote ) {
		stream->writeChar('\'');
		x = oCar(this->_Cdr);
		write_object(x,stream);
		return;
	    }
	    if ( this->_Car == cl::_sym_function) {
		stream->writeChar('#');
		stream->writeChar('\'');
		x = oCar(this->_Cdr);
		write_object(x,stream);
		return;
	    }
	    if ( this->_Car == _sym_quasiquote ) {
		stream->writeChar('`');
		x = oCar(this->_Cdr);
		write_object(x,stream);
		return;
	    }
	    if ( this->_Car == _sym_unquote ) {
		stream->writeChar(',');
		x = oCar(this->_Cdr);
		write_object(x,stream);
		return;
	    }
	    if ( this->_Car == _sym_unquote_splice ) {
		stream->writeStr(",@");
		x = oCar(this->_Cdr);
		write_object(x,stream);
		return;
	    }
	    if ( this->_Car == _sym_unquote_nsplice ) {
		stream->writeStr(",.");
		x = oCar(this->_Cdr);
		write_object(x, stream);
		return;
	    }
	}
	circle = brcl_print_circle();
	if (brcl_print_readably()) {
	    print_level = MOST_POSITIVE_FIXNUM;
	    print_length = MOST_POSITIVE_FIXNUM;
	} else {
	    print_level = brcl_print_level();
	    print_length = brcl_print_length();
	}
	if (print_level == 0) {
	    stream->writeChar('#');
	    return;
	}
	x = this->const_sharedThis<Cons_O>();
	stream->writeChar('(');
	for (i = 0;  ;  i++) {
	    if (i >= print_length) {
		stream->writeStr("...");
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
		    stream->writeChar(' ');
		    stream->writeStr(". ");
		    write_object(x, stream);
		}
		break;
	    }
	    if (i == 0 && !y._NULLp() && af_symbolp(y))
		stream->writeChar(' ');
	    else
		stream->writeChar(' ');
	}
	stream->writeChar(')');
    }



};
