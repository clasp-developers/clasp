
#define	DEBUG_LEVEL_FULL
#include "core/common.h"
#include "core/environment.h"
#include "symbolTable.h"
#include "core/hashTableEql.h"
#include "core/evaluator.h"
#include "core/character.h"
#include "core/str.h"
#include "core/bitVector.h"
#include "core/package.h"
#include "core/predicates.h"
#include "core/multipleValues.h"
#include "core/designators.h"
#include "core/lispStream.h"
#include "core/vectorObjects.h"
#include "core/lispReader.h"
#include "core/pathname.h"
#include "core/primitives.h"
#include "core/arguments.h"
#include "readtable.h"
#include "core/wrappers.h"
namespace core
{
    
// ----------------------------------------------------------------------
//


    void extra_argument(char macro, Stream_sp sin, T_sp arg)
    {
	READER_ERROR(Str_O::create("~S is an extra argument for the #~C readmacro."),
		     Cons_O::createList(arg,Character_O::create(macro)),sin);
    }


    
#define ARGS_af_setDispatchMacroCharacter "(dispChar subChar newFunction &optional (readtable *readtable*))"
#define DECL_af_setDispatchMacroCharacter ""
#define DOCS_af_setDispatchMacroCharacter "setDispatchMacroCharacter"
    T_mv af_setDispatchMacroCharacter(Character_sp dispChar, Character_sp subChar, T_sp newFunctionDesig, ReadTable_sp readtable)
    {_G();
	return(Values(readtable->set_dispatch_macro_character(dispChar,subChar,newFunctionDesig)));
    };



    
    
#define ARGS_af_getDispatchMacroCharacter "(dispChar subChar &optional (readtable *readtable*))"
#define DECL_af_getDispatchMacroCharacter ""
#define DOCS_af_getDispatchMacroCharacter "getDispatchMacroCharacter"
    T_mv af_getDispatchMacroCharacter(Character_sp dispChar, Character_sp subChar, ReadTable_sp readtable)
    {_G();
	return(Values(readtable->get_dispatch_macro_character(dispChar,subChar)));
    };

    
    
#define ARGS_af_setMacroCharacter "(ch func_desig &optional non-terminating-p (readtable *readtable*))"
#define DECL_af_setMacroCharacter ""
#define DOCS_af_setMacroCharacter "setMacroCharacter"
    T_mv af_setMacroCharacter(Character_sp ch, T_sp func_desig, T_sp non_terminating_p, ReadTable_sp readtable)
    {_G();
	return(Values(readtable->set_macro_character(ch,func_desig,non_terminating_p)));
    };

    SYMBOL_SC_(KeywordPkg,constituent_character);
    SYMBOL_SC_(KeywordPkg,whitespace_character);
    SYMBOL_SC_(CorePkg,STARsharp_equal_alistSTAR);
    SYMBOL_SC_(CorePkg,STARsharp_sharp_alistSTAR);
    SYMBOL_SC_(CorePkg,STARconsing_dot_allowedSTAR);
    SYMBOL_SC_(CorePkg,STARconsing_dotSTAR);
    SYMBOL_SC_(CorePkg,STARpreserve_whitespace_pSTAR);
    SYMBOL_SC_(CorePkg,STARinput_streamSTAR);
    SYMBOL_SC_(CorePkg,STARbackquote_levelSTAR);
    SYMBOL_SC_(CorePkg,STARstandard_readtableSTAR);

#define LOCK_af_reader_double_quote_string 1
#define DOCS_af_reader_double_quote_string "reader_double_quote_string"
#define ARGS_af_reader_double_quote_string "(stream chr)"
#define DECL_af_reader_double_quote_string ""    
    T_mv af_reader_double_quote_string(Stream_sp stream, Character_sp ch)
    {_G();
	stringstream str;
	bool done = false;
	while (!done)
	{
	    Character_sp nc = cl_readChar(stream,_lisp->_true(),_Nil<T_O>(),_lisp->_true()).as<Character_O>();
	    char cc = nc->asChar();
	    if ( cc == '"' ) break;
	    if ( cc == '\\' )
	    {
		nc = cl_readChar(stream,_lisp->_true(),_Nil<T_O>(),_lisp->_true()).as<Character_O>();
		cc = nc->asChar();
		if ( cc == 'n' ) cc = '\n';
	    }
	    str << cc;
	}
	// Return one value in a MultipleValues object to indicate that something is being returned
	return(Values(Str_O::create(str.str())));
    };

#define LOCK_af_reader_backquoted_expression 1
#define DOCS_af_reader_backquoted_expression "reader_backquoted_expression"
#define ARGS_af_reader_backquoted_expression "(sin ch)"
#define DECL_af_reader_backquoted_expression ""    
    T_mv af_reader_backquoted_expression(Stream_sp sin, Character_sp ch)
    {_G();
	Fixnum_sp backquote_level = _sym_STARbackquote_levelSTAR->symbolValue().as<Fixnum_O>();
	Fixnum_sp new_backquote_level = Fixnum_O::create(backquote_level->get()+1);
	// DynamicScopeManager will save the dynamic value of the symbol and restore it in dtor
	DynamicScopeManager scope(_sym_STARbackquote_levelSTAR,new_backquote_level);
	T_sp quoted_object = read_lisp_object(sin,true,_Nil<T_O>(),true);
	Cons_sp result = Cons_O::createList(_sym_backquote,quoted_object);
	//HERE_scCONS_CREATE_LIST2(_sym_backquote,quoted_object);
        if ( _lisp->sourceDatabase().notnilp() ) {
            _lisp->sourceDatabase()->duplicateSourceInfo(quoted_object,result);
        }
	return(Values(result));
    };

#define LOCK_af_reader_comma_form 1
#define DOCS_af_reader_comma_form "reader_comma_form"
#define ARGS_af_reader_comma_form "(sin ch)"
#define DECL_af_reader_comma_form ""    
    T_mv af_reader_comma_form(Stream_sp sin, Character_sp ch)
    {_G();
	Fixnum_sp backquote_level = _sym_STARbackquote_levelSTAR->symbolValue().as<Fixnum_O>();
	Fixnum_sp new_backquote_level = Fixnum_O::create(backquote_level->get()-1);
	DynamicScopeManager scope(_sym_STARbackquote_levelSTAR,new_backquote_level);
	char nextc = clasp_peek_char(sin);
//	ql::source_code_list list(sin->lineNumber(),sin->column(),af_sourceFileInfo(sin));
	ql::list list;
	Symbol_sp head = _sym_unquote;
	if ( nextc == '@' )
	{
	    head = _sym_unquote_splice;
	    cl_readChar(sin,_lisp->_true(),_Nil<T_O>(),_lisp->_true()).as<Character_O>();
	} else if ( nextc == '.')
	{
	    head = _sym_unquote_nsplice;
	    cl_readChar(sin,_lisp->_true(),_Nil<T_O>(),_lisp->_true()).as<Character_O>();
	}
	T_sp comma_object = read_lisp_object(sin,true,_Nil<T_O>(),true);
        list << head << comma_object;
	return(Values(list.cons()));
    };

#define LOCK_af_reader_list_allow_consing_dot 1
#define DOCS_af_reader_list_allow_consing_dot "reader_list_allow_consing_dot"
#define ARGS_af_reader_list_allow_consing_dot "(sin ch)"
#define DECL_af_reader_list_allow_consing_dot ""    
    T_mv af_reader_list_allow_consing_dot(Stream_sp sin, Character_sp ch)
    {_G();
	Cons_sp list = read_list(sin,')',true);
	return(Values(list));
    };

#define LOCK_af_reader_error_unmatched_close_parenthesis 1
#define DOCS_af_reader_error_unmatched_close_parenthesis "reader_error_unmatched_close_parenthesis"
#define ARGS_af_reader_error_unmatched_close_parenthesis "(sin ch)"
#define DECL_af_reader_error_unmatched_close_parenthesis ""    
    T_mv af_reader_error_unmatched_close_parenthesis(Stream_sp sin, Character_sp ch)
    {_G();
	SourceFileInfo_sp info = af_sourceFileInfo(sin);
	SIMPLE_ERROR(BF("Unmatched close parenthesis in file: %s line: %s")
                     % info->fileName()
                     % clasp_input_lineno(sin) );
	return(Values(_Nil<T_O>()));
    };

#define LOCK_af_reader_quote 1
#define DOCS_af_reader_quote "reader_quote"
#define ARGS_af_reader_quote "(sin ch)"
#define DECL_af_reader_quote ""    
    T_mv af_reader_quote(Stream_sp sin, Character_sp ch)
    {_G();
//	ql::source_code_list result(sin->lineNumber(),sin->column(),af_sourceFileInfo(sin));
	ql::list result;
	T_sp quoted_object = read_lisp_object(sin,true,_Nil<T_O>(),true);
	result << cl::_sym_quote << quoted_object;
	return(Values(result.cons()));
    }


#define LOCK_af_reader_skip_semicolon_comment 1
#define DOCS_af_reader_skip_semicolon_comment "reader_skip_semicolon_comment"
#define ARGS_af_reader_skip_semicolon_comment "(sin ch)"
#define DECL_af_reader_skip_semicolon_comment ""    
    T_mv af_reader_skip_semicolon_comment(Stream_sp sin, Character_sp ch)
    {_G();
	ASSERT(clasp_input_stream_p(sin));
	stringstream str;
	bool done = false;
	while (!done)
	{
	    Character_sp nc = cl_readChar(sin,_lisp->_true(),_Nil<T_O>(),_lisp->_true()).as<Character_O>();
	    char cc = nc->asChar();
	    if ( cc == '\n' ) break;
	}
	// Return one value in a MultipleValues object to indicate that something is being returned
	return( Values0<T_O>() );
    };





#define LOCK_af_dispatch_macro_character 1
#define DOCS_af_dispatch_macro_character "dispatch_macro_character"
#define ARGS_af_dispatch_macro_character "(sin ch)"
#define DECL_af_dispatch_macro_character ""    
    T_mv af_dispatch_macro_character(Stream_sp sin, Character_sp ch)
    {_G();
	char cpeek = clasp_peek_char(sin);
	bool sawnumarg = false;
	uint numarg = 0;
	while (isdigit(cpeek))
	{
	    sawnumarg = true;
	    int cget = clasp_read_char(sin);
	    if ( cget == EOF )
	    {
		SIMPLE_ERROR(BF("Hit eof in sharp macro"));
	    }
	    numarg *= 10;
	    numarg += (cget - '0');
	    cpeek = clasp_peek_char(sin);
	}
	Fixnum_sp onumarg = _Nil<Fixnum_O>();
	if ( sawnumarg )
	{
	    onumarg = Fixnum_O::create(numarg);
	}
	Character_sp subchar = cl_readChar(sin,_lisp->_true(),_Nil<T_O>(),_lisp->_true()).as<Character_O>();
	Function_sp macro_func = cl::_sym_STARreadtableSTAR->symbolValue().as<ReadTable_O>()->get_dispatch_macro_character(ch,subchar);
	if ( macro_func.nilp() )
	{	
	    SIMPLE_ERROR(BF("Undefined reader macro for %s %s") % _rep_(ch) % _rep_(subchar));
	}
	return eval::funcall(macro_func,sin,subchar,onumarg);
    };


    /*! See SACLA reader.lisp::read-ch */
    Character_sp read_ch(Stream_sp sin)
    {_G();
	Character_sp nc = cl_readChar(sin,_Nil<T_O>(),_Nil<T_O>(),_lisp->_true()).as<Character_O>();
	return nc;
    }

    /*! See SACLA reader.lisp::read-ch-or-die */
    Character_sp read_ch_or_die(Stream_sp sin)
    {_G();
	Character_sp nc = cl_readChar(sin,_lisp->_true(),_Nil<T_O>(),_lisp->_true()).as<Character_O>();
	return nc;
    }

    /*! See SACLA reader.lisp::unread-ch */
    void unread_ch(Stream_sp sin, Character_sp c)
    {_G();
	clasp_unread_char(c->asChar(),sin);
    }




    /*! See SACLA reader.lisp::collect-escaped-lexemes */
    Cons_sp collect_escaped_lexemes(Character_sp c, Stream_sp sin)
    {_G();
	ReadTable_sp readTable = _lisp->getCurrentReadTable();
	Symbol_sp syntax_type = readTable->syntax_type(c);
	if ( syntax_type == kw::_sym_invalid_character )
	{
	    SIMPLE_ERROR(BF("invalid-character-error: %s") %_rep_(c) );
	} else if ( syntax_type == kw::_sym_multiple_escape_character )
	{
	    return _Nil<Cons_O>();
	} else if ( syntax_type == kw::_sym_single_escape_character )
	{
	    return Cons_O::create(read_ch_or_die(sin),collect_escaped_lexemes(read_ch_or_die(sin),sin));
	} else if ( syntax_type == kw::_sym_constituent_character
		    || syntax_type == kw::_sym_whitespace_character
		    || syntax_type == kw::_sym_terminating_macro_character
		    || syntax_type == kw::_sym_non_terminating_macro_character )
	{
	    return Cons_O::create(c,collect_escaped_lexemes(read_ch_or_die(sin),sin));
	}
	return _Nil<Cons_O>();
    }



    /*! See SACLA reader.lisp::collect-lexemes */
    Cons_sp collect_lexemes(Character_sp c, Stream_sp sin)
    {_G();
	if ( c.notnilp() )
	{
	    ReadTable_sp readTable = _lisp->getCurrentReadTable();
	    Symbol_sp syntax_type = readTable->syntax_type(c);
	    if ( syntax_type == kw::_sym_invalid_character )
	    {
		SIMPLE_ERROR(BF("invalid-character-error: %s") %_rep_(c) );
	    } else if ( syntax_type == kw::_sym_whitespace_character )
	    {
		if ( _sym_STARpreserve_whitespace_pSTAR->symbolValue().isTrue() )
		{
		    unread_ch(sin,c);
		}
	    } else if ( syntax_type == kw::_sym_terminating_macro_character )
	    {
		unread_ch(sin,c);
	    } else if ( syntax_type == kw::_sym_multiple_escape_character )
	    {
		return Cons_O::create(collect_escaped_lexemes(read_ch_or_die(sin),sin),
				      collect_lexemes(read_ch(sin),sin));
	    } else if ( syntax_type == kw::_sym_single_escape_character )
	    {
		return Cons_O::create(Cons_O::create(read_ch_or_die(sin)),
				      collect_lexemes(read_ch(sin),sin));
	    } else if ( syntax_type == kw::_sym_constituent_character
			|| syntax_type == kw::_sym_non_terminating_macro_character )
	    {
		return Cons_O::create(c,collect_lexemes(read_ch(sin),sin));
	    }
	}
	return _Nil<Cons_O>();
    }


    /*! Works like SACLA readtable::make-str but accumulates the characters
      into a stringstream */
    void make_str(stringstream& sout, Cons_sp cur_char, bool preserveCase=false)
    {_G();
	while ( cur_char.notnilp() )
	{
	    T_sp obj = oCar(cur_char);
	    if ( af_consP(obj) )
	    {
		make_str(sout,obj.as_or_nil<Cons_O>(),preserveCase);
	    } else if ( af_characterP(obj) )
	    {
		if ( preserveCase ) sout << obj.as<Character_O>()->asChar();
		else sout << cl::_sym_STARreadtableSTAR->symbolValue().as<ReadTable_O>()->convert_case(obj.as<Character_O>())->asChar();
	    } else
	    {
		SIMPLE_ERROR(BF("Illegal entry for make_str[%s]") % _rep_(obj) );
	    }
	    cur_char = cCdr(cur_char);
	}
    }
    

#define LOCK_af_sharp_backslash 1
#define DOCS_af_sharp_backslash "sharp_backslash"
#define ARGS_af_sharp_backslash "(stream ch num)"
#define DECL_af_sharp_backslash ""	    
    T_mv af_sharp_backslash(Stream_sp sin, Character_sp ch, T_sp num)
    {_G();
	stringstream sslexemes;
	Cons_sp lexemes = collect_lexemes(ch,sin);
	make_str(sslexemes,lexemes,true);
	if ( !cl::_sym_STARread_suppressSTAR->symbolValue().isTrue() )
	{
	    string lexeme_str = sslexemes.str();
	    if ( lexeme_str.size() == 1 )
	    {
		return(Values(Character_O::create(lexeme_str[0])));
	    } else
	    {
		Str_sp name = Str_O::create(lexeme_str);
		Character_sp ch = eval::funcall(cl::_sym_name_char,name).as<Character_O>();
		if ( ch.nilp() )
		{
		    SIMPLE_ERROR(BF("Unknown character name[%s]") % _rep_(name) );
		}
		return(Values(ch) );
	    }
	}
	return( Values0<T_O>() );
    }




#define LOCK_af_sharp_dot 1
#define DOCS_af_sharp_dot "sharp_dot"
#define ARGS_af_sharp_dot "(stream ch num)"
#define DECL_af_sharp_dot ""    
    T_mv af_sharp_dot(Stream_sp sin, Character_sp ch, T_sp num)
    {_G();
	T_sp object = read_lisp_object(sin,true,_Nil<T_O>(),true);
	if ( !cl::_sym_STARread_suppressSTAR->symbolValue().isTrue() )
	{
	    if ( !cl::_sym_STARread_evalSTAR->symbolValue().isTrue() )
	    {
		READER_ERROR(Str_O::create("Cannot evaluate the form #.~S"),
			     Cons_O::create(object),
			     sin);
	    }
	    T_sp result = eval::af_topLevelEvalWithEnv(object,_Nil<Environment_O>());
	    return(Values(result));
	}
	return( Values0<T_O>() );
    }




#define LOCK_af_sharp_single_quote 1
#define DOCS_af_sharp_single_quote "sharp_single_quote"
#define ARGS_af_sharp_single_quote "(stream ch num)"
#define DECL_af_sharp_single_quote ""    
    T_mv af_sharp_single_quote(Stream_sp sin, Character_sp ch, T_sp num)
    {_G();
	T_sp quoted_object = read_lisp_object(sin,true,_Nil<T_O>(),true);
//	ql::source_code_list result(sin->lineNumber(),sin->column(),af_sourceFileInfo(sin));
	ql::list result;
	result << cl::_sym_function << quoted_object;
	return(Values(result.cons()));
    };

#define LOCK_af_sharp_left_parenthesis 1
#define DOCS_af_sharp_left_parenthesis "sharp_left_parenthesis"
#define ARGS_af_sharp_left_parenthesis "(stream ch num)"
#define DECL_af_sharp_left_parenthesis ""    
    T_mv af_sharp_left_parenthesis(Stream_sp sin, Character_sp ch, Fixnum_sp num)
    {_G();
	Character_sp right_paren = Character_O::create(')');
	T_sp olist = af_read_delimited_list(right_paren,sin,_lisp->_true());
	Cons_sp list = olist.as_or_nil<Cons_O>();
	if ( !cl::_sym_STARread_suppressSTAR->symbolValue().isTrue() )
	{
	    int list_length = cl_length(list);
	    if ( num.notnilp() )
	    {
		if ( list_length > num->get() )
		{
		    SIMPLE_ERROR(BF("vector is longer than specified length %s: %s") % num->get() % _rep_(list) );
		}
		int need_length = num->get();
		if ( list_length < need_length )
		{
		    Cons_sp reversed = cl_nreverse(list).as_or_nil<Cons_O>();
		    T_sp fill_entry = oCar(reversed);
		    for ( int i=list_length; i<need_length; i++ )
		    {
			reversed = Cons_O::create(fill_entry,reversed);
		    }
		    list = cl_nreverse(reversed).as_or_nil<Cons_O>();
		}
	    }
	    VectorObjects_sp vec = VectorObjects_O::make(_Nil<T_O>(),list,cl_length(list),false);
	    return(Values(vec));
	}
	return(Values(_Nil<T_O>()));
    };


#define LOCK_af_sharp_asterisk 1
#define DOCS_af_sharp_asterisk "sharp_asterisk"
#define ARGS_af_sharp_asterisk "(stream ch num)"
#define DECL_af_sharp_asterisk ""    
    T_mv af_sharp_asterisk(Stream_sp sin, Character_sp ch, T_sp num)
    {_G();
	int dimcount, dim =0;
	stringstream pattern;
	ReadTable_sp rtbl = cl::_sym_STARreadtableSTAR->symbolValue().as<ReadTable_O>();
	
	if ( cl::_sym_STARread_suppressSTAR->symbolValue().isTrue() ) {
	    read_lisp_object(sin,true,_Nil<T_O>(),true);
	    return Values(_Nil<T_O>());
	}
	for (dimcount = 0 ; ; dimcount++) {
	    ch = cl_readChar(sin,_Nil<T_O>(),_Nil<T_O>(),_lisp->_true()).as<Character_O>();
	    if (ch.nilp()) break;
	    Symbol_sp syntaxType = rtbl->syntax_type(ch);
	    if (syntaxType == kw::_sym_terminating_macro_character
		|| syntaxType == kw::_sym_whitespace_character) {
		unread_ch(sin,ch);
		break;
	    }
	    unlikely_if (syntaxType == kw::_sym_single_escape_character ||
			 syntaxType == kw::_sym_multiple_escape_character ||
			 ( ch->asChar() != '0' && ch->asChar() != '1')) {
		READER_ERROR(Str_O::create("Character ~:C is not allowed after #*"),Cons_O::create(ch),sin);
	    }
	    pattern << ch->asChar();
	}
	if (Null(num)) {
	    dim = dimcount;
	} else if (num.isA<Fixnum_O>()) {
	    dim = num.as<Fixnum_O>()->get();
	    unlikely_if ( dim < 0 ||
			  (dim > BRCL_ARRAY_DIMENSION_LIMIT))
	{
		READER_ERROR(Str_O::create("Wrong vector dimension size ~D in #*."), Cons_O::create(num), sin);
	    }
	    unlikely_if (dimcount > dim)
		READER_ERROR(Str_O::create("Too many elements in #*."),_Nil<Cons_O>(),sin);
	    unlikely_if (dim && (dimcount == 0))
		READER_ERROR(Str_O::create("Cannot fill the bit-vector #*."),_Nil<Cons_O>(),sin);
	}
	string bitPattern = pattern.str();
	char last = bitPattern.size()>0 ? bitPattern[bitPattern.size()-1] : '0';
	SimpleBitVector_sp x = SimpleBitVector_O::create(dim);
	for (int i = 0; i < dim; i++) {
	    char elt = (i < dimcount) ? bitPattern[i] : last;
	    if (elt == '0')
		x->setBit(i,0);
	    else
		x->setBit(i,1);
	}
	return Values(x);
    };


#define LOCK_af_sharp_colon 1
#define DOCS_af_sharp_colon "sharp_colon"
#define ARGS_af_sharp_colon "(stream ch num)"
#define DECL_af_sharp_colon ""    
    T_mv af_sharp_colon(Stream_sp sin, Character_sp ch, T_sp num)
    {_G();
	stringstream sslexemes;
	Cons_sp lexemes = collect_lexemes(ch,sin);
	make_str(sslexemes,lexemes);
	if ( !cl::_sym_STARread_suppressSTAR->symbolValue().isTrue() )
	{
	    string lexeme_str = sslexemes.str().substr(1);
	    Symbol_sp new_symbol = Symbol_O::create(lexeme_str);
	    return(Values(new_symbol));
	}
	return(Values(_Nil<T_O>()));
    }; // af_sharp_colon



#define LOCK_af_sharp_r 1
#define DOCS_af_sharp_r "sharp_r"
#define ARGS_af_sharp_r "(stream subchar radix)"
#define DECL_af_sharp_r ""    
    T_mv af_sharp_r(Stream_sp sin, Character_sp ch, Fixnum_sp radix)
    {_G();
	if ( cl::_sym_STARread_suppressSTAR->symbolValue().isTrue() )
	{
	    T_sp object = read_lisp_object(sin,true,_Nil<T_O>(),true);
	    return(Values(_Nil<T_O>()));
	} else if (radix.nilp())
	{
	    SIMPLE_ERROR(BF("Radix missing in #R reader macro"));
	} else
	{
	    int iradix = radix->get();
	    if ( iradix < 2 || iradix > 36 )
	    {
		SIMPLE_ERROR(BF("Illegal radix for #R: %d") % iradix);
	    }
	    {
		Fixnum_sp oradix = Fixnum_O::create(iradix);
		DynamicScopeManager scope(cl::_sym_STARread_baseSTAR,oradix);
		T_sp val = read_lisp_object(sin,true,_Nil<T_O>(),true);
		if ( !val.isA<Rational_O>() )
		{
		    SIMPLE_ERROR(BF("#%s (base %d) is not a rational: %s")
				       % _rep_(ch) % iradix % _rep_(val));
		}
		return(Values(val));
	    }
	}
    }



#define LOCK_af_sharp_b 1
#define DOCS_af_sharp_b "sharp_b"
#define ARGS_af_sharp_b "(stream ch num)"
#define DECL_af_sharp_b ""    
    T_mv af_sharp_b(Stream_sp sin, Character_sp ch, Fixnum_sp num)
    {_G();
	return af_sharp_r(sin,ch,Fixnum_O::create(2));
    };


#define LOCK_af_sharp_o 1
#define DOCS_af_sharp_o "sharp_o"
#define ARGS_af_sharp_o "(stream ch num)"
#define DECL_af_sharp_o ""    
    T_mv af_sharp_o(Stream_sp sin, Character_sp ch, Fixnum_sp num)
    {_G();
	return af_sharp_r(sin,ch,Fixnum_O::create(8));
    };


#define LOCK_af_sharp_x 1
#define DOCS_af_sharp_x "sharp_x"
#define ARGS_af_sharp_x "(stream ch num)"
#define DECL_af_sharp_x ""    
    T_mv af_sharp_x(Stream_sp sin, Character_sp ch, T_sp num)
    {_G();
	return af_sharp_r(sin,ch,Fixnum_O::create(16));
    };



#define LOCK_af_sharp_c 1
#define DOCS_af_sharp_c "sharp_c"
#define ARGS_af_sharp_c "(stream ch num)"
#define DECL_af_sharp_c ""    
    T_mv af_sharp_c(Stream_sp sin, Character_sp ch, T_sp num)
    {_G();
	Character_sp right_paren = Character_O::create(')');
	T_sp olist = read_lisp_object(sin,true,_Nil<T_O>(),true);
	Cons_sp list = olist.as_or_nil<Cons_O>();
	if ( !cl::_sym_STARread_suppressSTAR->symbolValue().isTrue() )
	{
	    int list_length = cl_length(list);
	    if ( list_length != 2 )
	    {
		SIMPLE_ERROR(BF("#C complex number needs two numbers"));
	    }
	    Real_sp r = oCar(list).as<Real_O>();
	    Real_sp i = oCadr(list).as<Real_O>();
	    return(Values(Complex_O::create(r,i)));
	}
	return(Values(_Nil<T_O>()));

    }; // af_sharp_c

#define LOCK_af_sharp_a 1
#define DOCS_af_sharp_a "sharp_a"
#define ARGS_af_sharp_a "(stream ch num)"
#define DECL_af_sharp_a ""    
    T_mv af_sharp_a(Stream_sp sin, Character_sp ch, T_sp num)
    {_G();
	IMPLEMENT_MEF(BF("Implement sharp_a"));
    }; // af_sharp_a

#define LOCK_af_sharp_s 1
#define DOCS_af_sharp_s "sharp_s"
#define ARGS_af_sharp_s "(stream ch num)"
#define DECL_af_sharp_s ""    
    T_mv af_sharp_s(Stream_sp sin, Character_sp ch, T_sp num)
    {_G();
	IMPLEMENT_MEF(BF("Implement sharp_s"));
    }; // af_sharp_s

#define LOCK_af_sharp_p 1
#define DOCS_af_sharp_p "sharp_p"
#define ARGS_af_sharp_p "(stream ch num)"
#define DECL_af_sharp_p ""    
    T_mv af_sharp_p(Stream_sp sin, Character_sp ch, T_sp num)
    {_G();
	bool suppress = cl::_sym_STARread_suppressSTAR->symbolValue().isTrue();
	if (num.notnilp() && !suppress)
	    extra_argument('P', sin, num);
	T_sp d = read_lisp_object(sin,true,_Nil<T_O>(),true);
	if (suppress) {
	    d = _Nil<T_O>();
	} else {
	    d = af_parseNamestring(d);
	}
	return Values(d);
    }; // af_sharp_p




// #if 0
// #define LOCK_af_sharp_equal 1
// #define DOCS_af_sharp_equal "sharp_equal"
// #define ARGS_af_sharp_equal "(stream ch num)"
// #define DECL_af_sharp_equal ""    
//     T_mv af_sharp_equal(Stream_sp sin, Character_sp ch, Fixnum_sp num)
//     {_G();
// 	if ( cl::_sym_STARread_suppressSTAR->symbolValue().isTrue() )
// 	{
// 	    SIMPLE_ERROR(BF("af_sharp_equal should have read something before it returned nil"));
// 	    return(Values(_Nil<T_O>()));
// 	}
// 	// Associate the object with the num
// 	if ( num.nilp() ) SIMPLE_ERROR(BF("You must provide a number for #xx="));
// 	LOG(BF("num[%s]") % num->__repr__() );
// 	T_sp sharpThis = af_gensym(_Nil<T_O>());
// 	{ // (setq *sharp-sharp-aliast* (acons n sharpThis *sharp-sharp-alist*))
// 	    Cons_sp sharp_sharp_alist = _sym_STARsharp_sharp_alistSTAR->symbolValue().as_or_nil<Cons_O>();
// 	    sharp_sharp_alist = Cons_O::create(Cons_O::create(num,sharpThis),sharp_sharp_alist,_lisp);
// 	    _sym_STARsharp_sharp_alistSTAR->setf_symbolValue(sharp_sharp_alist);
// 	}
// 	T_sp object = read_lisp_object(sin,true,_Nil<T_O>(),true);
// 	Cons_sp sharp_equal_alist = _sym_STARsharp_equal_alistSTAR->symbolValue().as_or_nil<Cons_O>();
// 	if ( sharp_equal_alist->assoc(sharpThis,_Nil<T_O>(),_Nil<T_O>(),_Nil<T_O>()).notnilp() )
// 	{
// 	    SIMPLE_ERROR(BF("#%d= is already defined") % num->get() );
// 	}
// 	if ( object == sharpThis )
// 	{
// 	    SIMPLE_ERROR(BF("need to tag something more than just #%d#") % num->get());
// 	}
// 	_sym_STARsharp_equal_alistSTAR->setf_symbolValue(Cons_O::create(Cons_O::create(sharpThis,object),sharp_equal_alist,_lisp));
// 	return(Values(object));
//     }


// #define LOCK_af_sharp_sharp 1
// #define DOCS_af_sharp_sharp "sharp_sharp"
// #define ARGS_af_sharp_sharp "(stream ch num)"
// #define DECL_af_sharp_sharp ""    
//     T_mv af_sharp_sharp(Stream_sp sin, Character_sp ch, Fixnum_sp num)
//     {_G();
// 	if ( cl::_sym_STARread_suppressSTAR->symbolValue().isTrue() )
// 	{
// 	    return( Values0<T_O>() );
// 	}
// 	// Associate the object with the num
// 	if ( num.nilp() ) SIMPLE_ERROR(BF("You must provide a number for #xx#"));
// 	LOG(BF("num[%s]") % num->__repr__() );
// 	Cons_sp assoc = _sym_STARsharp_sharp_alistSTAR->symbolValue().as_or_nil<Cons_O>()->assoc(num,_Nil<T_O>(),_Nil<T_O>(),_Nil<T_O>());
// 	if ( assoc.notnilp() )
// 	{
// 	    Cons_sp sharp_equal_assoc = _sym_STARsharp_equal_alistSTAR->symbolValue().as_or_nil<Cons_O>()->assoc(assoc->ocdr(),_Nil<T_O>(),_Nil<T_O>(),_Nil<T_O>());
// 	    if ( assoc.notnilp() )
// 	    {
// 		return(Values(sharp_equal_assoc->ocdr()));
// 	    }
// 	    SIMPLE_ERROR(BF("Whoah! no object labeled %d could be identified") % num->get() );
// 	}
// 	SIMPLE_ERROR(BF("No object labeled %d is seen") % num->get() );
//     }
// #endif

#define DOCS_af_reader_feature_p "feature_p takes one argument - a feature test"
#define LOCK_af_reader_feature_p 1
#define ARGS_af_reader_feature_p "(feature-test)"
#define DECL_af_reader_feature_p ""    
    T_mv af_reader_feature_p(T_sp feature_test)
    {_G();
	if ( feature_test.nilp() ) return(Values(_Nil<T_O>()));
	else if ( af_atom(feature_test) )
	{
	    Cons_sp features_list = cl::_sym_STARfeaturesSTAR->symbolValue().as_or_nil<Cons_O>();
	    return(Values(features_list->member(feature_test.as<Symbol_O>(),
						_Nil<T_O>(),_Nil<T_O>(),_Nil<T_O>())));
	} else
	{
	    ASSERT(af_listp(feature_test));
	    Cons_sp features_cons = feature_test.as_or_nil<Cons_O>();
	    T_sp features_head = oCar(features_cons);
	    if ( features_head == kw::_sym_not)
	    {
		return(Values(_lisp->_not(eval::funcall(_sym_reader_feature_p,oSecond(features_cons)))));
	    } else if ( features_head == kw::_sym_and )
	    {
		return(Values(eval::funcall(cl::_sym_every,_sym_reader_feature_p,cCdr(features_cons))));
	    } else if ( features_head == kw::_sym_or )
	    {
		return(Values(eval::funcall(cl::_sym_some,_sym_reader_feature_p,cCdr(features_cons))));
	    }
	    SIMPLE_ERROR(BF("Illegal feature test: %s") % _rep_(features_cons) );
	}
    }
	    
	

    /*! Read a feature test in the keyword package */
    T_sp read_feature_test(Stream_sp sin)
    {_G();
	// Read the feature test in the keyword package
	DynamicScopeManager dynamicScopeManager(cl::_sym_STARpackageSTAR,_lisp->keywordPackage());
	T_sp feature = read_lisp_object(sin,true,_Nil<T_O>(),true);
	return feature;
    }
	    



#define LOCK_af_sharp_plus 1
#define DOCS_af_sharp_plus "sharp_plus"
#define ARGS_af_sharp_plus "(stream ch num)"
#define DECL_af_sharp_plus ""    
    T_mv af_sharp_plus(Stream_sp sin, Character_sp ch, T_sp num)
    {_G();
	T_sp feat = read_feature_test(sin);
	LOG(BF("feature[%s]") % _rep_(feat) );
	if ( eval::funcall(_sym_reader_feature_p,feat).isTrue() )
	{
	    LOG(BF("The feature test passed - reading lisp object"));
	    T_sp obj = read_lisp_object(sin,true,_Nil<T_O>(),true);
	    LOG(BF("Read the object[%s]") % _rep_(obj) );
	    return(Values(obj));
	} else
	{
	    LOG(BF("The feature test failed - returning nil"));
	    {_BLOCK_TRACEF(BF("Suppressing read for unsupported feature[%s]") % feat->__repr__() );
		DynamicScopeManager dynScopeManager(cl::_sym_STARread_suppressSTAR,_lisp->_true());
		read_lisp_object(sin,true,_Nil<T_O>(),true);
	    }
	    return( Values0<T_O>() );
	}
    }; // af_sharp_plus

#define LOCK_af_sharp_minus 1
#define DOCS_af_sharp_minus "sharp_minus"
#define ARGS_af_sharp_minus "(stream ch num)"
#define DECL_af_sharp_minus ""    
    T_mv af_sharp_minus(Stream_sp sin, Character_sp ch, T_sp num)
    {_G();
	T_sp feat = read_feature_test(sin);
	LOG(BF("feature[%s]") % _rep_(feat) );
	if ( !eval::funcall(_sym_reader_feature_p,feat).isTrue() )
	{
	    LOG(BF("The feature test passed - reading lisp object"));
	    T_sp obj = read_lisp_object(sin,true,_Nil<T_O>(),true);
	    LOG(BF("Read the object[%s]") % _rep_(obj) );
	    return(Values(obj));
	} else
	{
	    LOG(BF("The feature test failed - returning nil"));
	    DynamicScopeManager dynScopeManager(cl::_sym_STARread_suppressSTAR,_lisp->_true());
	    read_lisp_object(sin,true,_Nil<T_O>(),true);
	    return( Values0<T_O>() );
	}
    }; // af_sharp_minus

#define LOCK_af_sharp_vertical_bar 1
#define DOCS_af_sharp_vertical_bar "sharp_vertical_bar"
#define ARGS_af_sharp_vertical_bar "(stream ch num)"
#define DECL_af_sharp_vertical_bar ""    
    T_mv af_sharp_vertical_bar(Stream_sp sin, Character_sp ch, T_sp num)
    {_G();
	ASSERT(clasp_input_stream_p(sin));
	bool done = false;
	while (!done)
	{
	    Character_sp nc = cl_readChar(sin,_lisp->_true(),_Nil<T_O>(),_lisp->_true()).as<Character_O>();
	    char cc = nc->asChar();
	    if ( cc == '#' )
	    {
		char nextc = clasp_peek_char(sin);
		if ( nextc == '|')
		{
		    Character_sp nextsubc = cl_readChar(sin,_lisp->_true(),_Nil<T_O>(),_lisp->_true()).as<Character_O>();
		    eval::funcall(_sym_sharp_vertical_bar,sin,nextsubc,num);
		}
	    } else if ( cc == '|' )
	    {
		char nextc = clasp_peek_char(sin);
		if ( nextc == '#')
		{
		    Character_sp nextsubc = cl_readChar(sin,_lisp->_true(),_Nil<T_O>(),_lisp->_true()).as<Character_O>();
		    goto DONE;
		}
	    }
	}
	// Return nothing in MultipleValues to indicate that nothing is returned
    DONE:
	return( Values0<T_O>() );
    }; // af_sharp_vertical_bar















    
    EXPOSE_CLASS(core,ReadTable_O);
    SYMBOL_SC_(KeywordPkg,syntax);
    SYMBOL_SC_(KeywordPkg,whitespace_character);
    HashTable_sp ReadTable_O::create_standard_syntax_table(Lisp_sp lisp)
    {_G();
	HashTableEql_sp syntax = HashTableEql_O::create_default();
	Cons_sp whiteSpaceNames = (ql::list(_lisp)
				   << StandardChar_O::create_from_name("TAB")
				   << StandardChar_O::create_from_name("NEWLINE")
				   << StandardChar_O::create_from_name("LINEFEED")
				   << StandardChar_O::create_from_name("PAGE")
				   << StandardChar_O::create_from_name("RETURN")
				   << StandardChar_O::create_from_name("SPACE")
				   << StandardChar_O::create_from_name("RETURN")
	    ).cons();
	for ( Cons_sp cur=whiteSpaceNames; cur.notnilp(); cur=cCdr(cur) )
	{
	    syntax->hash_table_setf_gethash(oCar(cur).as<StandardChar_O>(),
				 (ql::list(_lisp)
				  << kw::_sym_syntax
				  << kw::_sym_whitespace_character).cons());
	}
	SYMBOL_SC_(KeywordPkg,single_escape_character);
	SYMBOL_SC_(KeywordPkg,multiple_escape_character);
	syntax->hash_table_setf_gethash(StandardChar_O::create('\\'),
			     (ql::list(_lisp)
			      << kw::_sym_syntax
			      << kw::_sym_single_escape_character).cons());
	syntax->hash_table_setf_gethash(StandardChar_O::create('|'),
			     (ql::list(_lisp)
			      << kw::_sym_syntax
			      << kw::_sym_multiple_escape_character).cons());
	return syntax;
    }


    ReadTable_sp ReadTable_O::create_standard_readtable(Lisp_sp lisp)
    {_G();
        GC_ALLOCATE(ReadTable_O,rt );
	rt->_Syntax = ReadTable_O::create_standard_syntax_table(_lisp);
	ASSERTNOTNULL(_sym_reader_backquoted_expression->symbolFunction());
	ASSERT(_sym_reader_backquoted_expression->symbolFunction().notnilp());
	rt->set_macro_character(StandardChar_O::create('`'),
				_sym_reader_backquoted_expression->symbolFunction(),
				_Nil<T_O>());
	rt->set_macro_character(StandardChar_O::create(','),
				_sym_reader_comma_form->symbolFunction(),
				_Nil<T_O>());
	SYMBOL_SC_(CorePkg,read_list_allow_consing_dot);
	rt->set_macro_character(StandardChar_O::create('('),
				_sym_reader_list_allow_consing_dot->symbolFunction(),
				_Nil<T_O>());
	SYMBOL_SC_(CorePkg,reader_error_unmatched_close_parenthesis);
	rt->set_macro_character(StandardChar_O::create(')'),
				_sym_reader_error_unmatched_close_parenthesis->symbolFunction(),
				_Nil<T_O>());
	SYMBOL_SC_(CorePkg,reader_quote);
	rt->set_macro_character(StandardChar_O::create('\''),
				_sym_reader_quote->symbolFunction(),
				_Nil<T_O>());
	SYMBOL_SC_(CorePkg,reader_skip_semicolon_comment);
	rt->set_macro_character(StandardChar_O::create(';'),
				_sym_reader_skip_semicolon_comment->symbolFunction(),
				_Nil<T_O>());
	SYMBOL_SC_(CorePkg,reader_read_double_quote_string);
	rt->set_macro_character(StandardChar_O::create('"'),
				_sym_reader_double_quote_string->symbolFunction(),
				_Nil<T_O>());
	Character_sp sharp = StandardChar_O::create('#');
	rt->make_dispatch_macro_character(sharp,_lisp->_true());
	ql::list dispatchers(_lisp);
	dispatchers << StandardChar_O::create('\\') << _sym_sharp_backslash
		    << StandardChar_O::create('\'') << _sym_sharp_single_quote
		    << StandardChar_O::create('(') << _sym_sharp_left_parenthesis
		    << StandardChar_O::create('*') << _sym_sharp_asterisk
		    << StandardChar_O::create(':') << _sym_sharp_colon
		    << StandardChar_O::create('.') << _sym_sharp_dot
		    << StandardChar_O::create('b') << _sym_sharp_b
		    << StandardChar_O::create('o') << _sym_sharp_o
		    << StandardChar_O::create('x') << _sym_sharp_x
		    << StandardChar_O::create('r') << _sym_sharp_r
		    << StandardChar_O::create('c') << _sym_sharp_c
		    << StandardChar_O::create('a') << _sym_sharp_a
		    << StandardChar_O::create('s') << _sym_sharp_s
		    << StandardChar_O::create('p') << _sym_sharp_p
//		    << StandardChar_O::create('=') << _sym_sharp_equal
//		    << StandardChar_O::create('#') << _sym_sharp_sharp
		    << StandardChar_O::create('+') << _sym_sharp_plus
		    << StandardChar_O::create('-') << _sym_sharp_minus
		    << StandardChar_O::create('|') << _sym_sharp_vertical_bar;
	for ( Cons_sp cur=dispatchers.cons(); cur.notnilp(); cur = cCdr(cCdr(cur)) )
	{
	    Character_sp ch = oCar(cur).as<Character_O>();
	    Symbol_sp sym = oCadr(cur).as<Symbol_O>();
	    rt->set_dispatch_macro_character(sharp, ch, sym );
	}
	return rt;
    }
				
			       

    
    
#if 0
#if defined(OLD_SERIALIZE)
    void ReadTable_O::serialize(::serialize::SNodeP node)
    {
	IMPLEMENT_ME();
        this->Bases::serialize(node);
	// Archive other instance variables here
    }
#endif
    
#if defined(XML_ARCHIVE)
    void ReadTable_O::archiveBase(::core::ArchiveP node)
    {
	IMPLEMENT_ME();
        this->Base1::archiveBase(node);
	// Archive other instance variables here
    }
#endif // defined(XML_ARCHIVE)
#endif
    
    
    SYMBOL_SC_(KeywordPkg,upcase);
    SYMBOL_SC_(KeywordPkg,downcase);
    SYMBOL_SC_(KeywordPkg,preserve);
    SYMBOL_SC_(KeywordPkg,invert);
    void ReadTable_O::initialize()
    {_OF();
        this->Base::initialize();
	this->_Case = kw::_sym_upcase;
	this->_Syntax = HashTableEql_O::create_default();
    }


    clasp_readtable_case ReadTable_O::getReadTableCaseAsEnum()
    {
        Symbol_sp ccase = this->_Case;
        if ( ccase == kw::_sym_upcase ) {
            return clasp_case_upcase;
        } else if ( ccase == kw::_sym_downcase ) {
            return clasp_case_downcase;
        } else if ( ccase == kw::_sym_invert ) {
            return clasp_case_invert;
        } else if ( ccase == kw::_sym_preserve ) {
            return clasp_case_preserve;
        }
        SIMPLE_ERROR(BF("Unknown readtable case: %s") % _rep_(this->_Case));
    }

    Symbol_sp ReadTable_O::setf_readtable_case( Symbol_sp newCase)
    {_OF();
	if ( (newCase == kw::_sym_upcase)
	     || (newCase == kw::_sym_downcase)
	     || (newCase == kw::_sym_preserve)
	     || (newCase == kw::_sym_invert))
	{
	    this->_Case = newCase;
	    return newCase;
	} else
	{
	    SIMPLE_ERROR(BF("Illegal newValue[%s] for (setf (readtable-case {readtable}) newValue) - it can only be :upcase, :downcase, :preserve or :invert")%_rep_(newCase) );
	}
    }


#define DOCS_ReadTable_set_macro_character "set-macro-character as in CL"
#define ARGS_ReadTable_set_macro_character "(ch func_desig &optional non-terminating-p)"
#define	DECL_ReadTable_set_macro_character ""
    SYMBOL_SC_(KeywordPkg,non_terminating_macro_character);
    SYMBOL_SC_(KeywordPkg,terminating_macro_character);
    SYMBOL_SC_(KeywordPkg,macro_function);
    T_sp ReadTable_O::set_macro_character(Character_sp ch, T_sp funcDesig, T_sp non_terminating_p)
    {_OF();
	ql::list plist(_lisp);
	if ( non_terminating_p.isTrue() )
	{
	    plist << kw::_sym_syntax << kw::_sym_non_terminating_macro_character;
	} else
	{
	    plist << kw::_sym_syntax << kw::_sym_terminating_macro_character;
	}
	Function_sp func = coerce::functionDesignator(funcDesig);
	plist << kw::_sym_macro_function << func;
	this->_Syntax->hash_table_setf_gethash(ch,plist.cons());
	return _lisp->_true();
    }

    string ReadTable_O::__repr__() const
    {
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString();
	ss << ":case " << _rep_(this->_Case) << std::endl;
	ss << ":syntax " << this->_Syntax->hash_table_dump() << std::endl;
	ss << "> ";
	return ss.str();
    }
	    


    Symbol_sp ReadTable_O::syntax_type(Character_sp ch) const
    {_OF();
	Cons_sp plist = this->_Syntax->gethash(ch,_Nil<Cons_O>()).as_or_nil<Cons_O>();
	LOG(BF("character[%s] plist: %s") % _rep_(ch) % _rep_(plist) );
	Symbol_sp result(kw::_sym_constituent_character); // should this be the default????
	if ( plist.notnilp() ) result = plist->getf(kw::_sym_syntax,kw::_sym_constituent_character).as<Symbol_O>();
	if ( result.nilp() )
	{
	    printf("%s:%d syntax_type for %s --> %s\n", __FILE__, __LINE__, _rep_(ch).c_str(), _rep_(result).c_str() );
	}
	LOG(BF("syntax_type: %s") % _rep_(result) );
	return result;
    }


    T_mv ReadTable_O::get_macro_character( Character_sp ch )
    {_OF();
	Cons_sp plist = this->_Syntax->gethash(ch,_Nil<Cons_O>()).as_or_nil<Cons_O>();
	T_sp dispatcher = plist->getf(kw::_sym_macro_function,_Nil<T_O>());
	Symbol_sp syntaxType = this->syntax_type(ch);
	if ( syntaxType == kw::_sym_terminating_macro_character )
	{
	    return(Values(dispatcher,_Nil<T_O>()));
	} else if ( syntaxType == kw::_sym_non_terminating_macro_character )
	{
	    return(Values(dispatcher,_lisp->_true()));
	}
	return(Values(_Nil<T_O>(),_Nil<T_O>()));
    }

    T_sp ReadTable_O::make_dispatch_macro_character(Character_sp ch, T_sp non_terminating_p )
    {_OF();
	this->set_macro_character(ch,_sym_dispatch_macro_character, non_terminating_p);
	HashTable_sp syntax = this->_Syntax;
	Cons_sp plist = syntax->gethash(ch,_Nil<Cons_O>()).as_or_nil<Cons_O>();
	ql::list qplist(_lisp);
	SYMBOL_SC_(KeywordPkg,dispatch_table);
	// add the :dispatch-table (make-hash-table) property 
	qplist << kw::_sym_dispatch_table
	       << HashTableEql_O::create_default()
	    & plist;
	LOG(BF("Adding :dispatch-table property to plist"));
	LOG(BF("New plist: %s") % _rep_(qplist.cons()));
	LOG(BF("Existing plist: %s") % _rep_(syntax->gethash(ch,_Nil<T_O>())) );
	syntax->hash_table_setf_gethash(ch,qplist.cons());
	LOG(BF("After setf plist: %s") % _rep_(syntax->gethash(ch,_Nil<T_O>())) );
	return _lisp->_true();
    }



    T_sp ReadTable_O::set_dispatch_macro_character(Character_sp disp_char, Character_sp sub_char,
						   T_sp new_func_desig )
    {_OF();
	if ( this->get_macro_character(disp_char)
	     != _sym_dispatch_macro_character->symbolFunction() )
	{
	    SIMPLE_ERROR(BF("%c is not a dispatch character") % _rep_(disp_char) );
	}
	HashTable_sp syntax_table = this->_Syntax;
	Cons_sp disp_char_plist;
	{MULTIPLE_VALUES_CONTEXT()
		disp_char_plist = syntax_table->gethash(disp_char,_Nil<Cons_O>()).as_or_nil<Cons_O>();
	}
	HashTable_sp dispatch_table = disp_char_plist->getf(kw::_sym_dispatch_table,_Nil<HashTable_O>() ).as<HashTable_O>();
	ASSERTF(dispatch_table.notnilp(),BF("The dispatch table for the character[%s] is nil! - this shouldn't happen") % _rep_(disp_char) );
	Character_sp upcase_sub_char = sub_char->char_upcase();
	Function_sp new_func = coerce::functionDesignator(new_func_desig);
	dispatch_table->hash_table_setf_gethash(upcase_sub_char,new_func);
	return _lisp->_true();
    }


    Function_sp ReadTable_O::get_dispatch_macro_character(Character_sp disp_char, Character_sp sub_char )
    {_OF();
	if ( this->get_macro_character(disp_char)
	     != _sym_dispatch_macro_character->symbolFunction() )
	{
	    SIMPLE_ERROR(BF("%c is not a dispatch character") % _rep_(disp_char) );
	}
	HashTable_sp syntax_table = this->_Syntax;
	Cons_sp disp_char_plist = syntax_table->gethash(disp_char,_Nil<Cons_O>()).as_or_nil<Cons_O>();
	HashTable_sp dispatch_table = disp_char_plist->getf(kw::_sym_dispatch_table,_Nil<HashTable_O>() ).as<HashTable_O>();
	ASSERTF(dispatch_table.notnilp(),BF("The dispatch table for the character[%s] is nil! - this shouldn't happen") % _rep_(disp_char) );
	Character_sp upcase_sub_char = sub_char->char_upcase();
	Function_sp func = dispatch_table->gethash(upcase_sub_char,_Nil<T_O>()).as<Function_O>();
	return func;
    }
	





    Character_sp ReadTable_O::convert_case(Character_sp cc)
    {_OF();
	if ( this->_Case == kw::_sym_upcase )
	{
	    return cc->char_upcase();
	} else if (this->_Case == kw::_sym_downcase)
	{
	    return cc->char_downcase();
	} else if (this->_Case == kw::_sym_preserve)
	{
	    return cc;
	} else if (this->_Case == kw::_sym_invert)
	{
	    SIMPLE_ERROR(BF("I can't handle invert yet, that has to be handled when the token is converted"));
	}
	SIMPLE_ERROR(BF("Bad readtable case[%s]") % _rep_(this->_Case) );
    }





    void ReadTable_O::exposeCando(::core::Lisp_sp lisp)
    {_G();
	::core::class_<ReadTable_O>()
	      //.def_accessor("readtable-case",&ReadTable_O::_Case,&ReadTable_O::setf_readtable_case)
	      //.def_readonly("readtable-syntax",&ReadTable_O::_Syntax)
	    ;
	SYMBOL_EXPORT_SC_(ClPkg,setMacroCharacter);
	Defun(setMacroCharacter);
	SYMBOL_SC_(CorePkg,reader_backquoted_expression);
	Defun(reader_backquoted_expression);
	SYMBOL_SC_(CorePkg,sharp_backslash);
	Defun(sharp_backslash);
	SYMBOL_SC_(CorePkg,sharp_single_quote);
	Defun(sharp_single_quote);
	SYMBOL_SC_(CorePkg,sharp_left_parenthesis);
	Defun(sharp_left_parenthesis);
	SYMBOL_SC_(CorePkg,sharp_asterisk);
	Defun(sharp_asterisk);
	SYMBOL_SC_(CorePkg,sharp_colon);
	Defun(sharp_colon);
	SYMBOL_SC_(CorePkg,sharp_dot);
	Defun(sharp_dot);
	SYMBOL_SC_(CorePkg,sharp_b);
	Defun(sharp_b);
	SYMBOL_SC_(CorePkg,sharp_o);
	Defun(sharp_o);
	SYMBOL_SC_(CorePkg,sharp_x);
	Defun(sharp_x);
	SYMBOL_SC_(CorePkg,sharp_r);
	Defun(sharp_r);
	SYMBOL_SC_(CorePkg,sharp_c);
	Defun(sharp_c);
	SYMBOL_SC_(CorePkg,sharp_a);
	Defun(sharp_a);
	SYMBOL_SC_(CorePkg,sharp_s);
	Defun(sharp_s);
	SYMBOL_SC_(CorePkg,sharp_p);
	Defun(sharp_p);
#if 0
	SYMBOL_SC_(CorePkg,sharp_equal);
	Defun(sharp_equal);
	SYMBOL_SC_(CorePkg,sharp_sharp);
	Defun(sharp_sharp);
#endif
	SYMBOL_SC_(CorePkg,sharp_plus);
	Defun(sharp_plus);
	SYMBOL_SC_(CorePkg,sharp_minus);
	Defun(sharp_minus);
	SYMBOL_SC_(CorePkg,sharp_vertical_bar);
	Defun(sharp_vertical_bar);
	SYMBOL_SC_(CorePkg,dispatch_macro_character);
	Defun(dispatch_macro_character);
	SYMBOL_SC_(CorePkg,reader_double_quote_string);
	Defun(reader_double_quote_string);
	SYMBOL_SC_(CorePkg,reader_comma_form);
	Defun(reader_comma_form);
	SYMBOL_SC_(CorePkg,reader_list_allow_consing_dot);
	Defun(reader_list_allow_consing_dot);
	SYMBOL_SC_(CorePkg,reader_error_unmatched_close_parenthesis);
	Defun(reader_error_unmatched_close_parenthesis);
	SYMBOL_SC_(CorePkg,reader_quote);
	Defun(reader_quote);
	SYMBOL_SC_(CorePkg,reader_skip_semicolon_comment);
	Defun(reader_skip_semicolon_comment);
	SYMBOL_SC_(CorePkg,reader_feature_p);
	Defun(reader_feature_p);
	SYMBOL_EXPORT_SC_(ClPkg,setDispatchMacroCharacter);
	Defun(setDispatchMacroCharacter);
	SYMBOL_EXPORT_SC_(ClPkg,getDispatchMacroCharacter);
	Defun(getDispatchMacroCharacter);
    }
    
    void ReadTable_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(Pkg(),ReadTable,"","",_LISP)
//	.initArgs("(self)")
	    ;
#endif
    }
    


}; /* core */
