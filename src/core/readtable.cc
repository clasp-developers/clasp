/*
    File: readtable.cc
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

#define DEBUG_LEVEL_FULL
#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/character.h>
#include <clasp/core/str.h>
#include <clasp/core/bitVector.h>
#include <clasp/core/package.h>
#include <clasp/core/predicates.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/designators.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/lispReader.h>
#include <clasp/core/pathname.h>
#include <clasp/core/primitives.h>
#include <clasp/core/arguments.h>
#include <clasp/core/readtable.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

void extra_argument(char macro, T_sp sin, T_sp arg) {
  READER_ERROR(Str_O::create("~S is an extra argument for the #~C readmacro."),
               Cons_O::createList(arg, clasp_make_character(macro)), sin);
}

CL_LAMBDA(tochar fromchar &optional (toreadtable *readtable*) (fromreadtable nil fromreadtablep));
CL_DECLARE();
CL_DOCSTRING("setSyntaxFromChar");
CL_DEFUN T_sp cl__set_syntax_from_char(Character_sp toChar, Character_sp fromChar, ReadTable_sp toReadTable, gc::Nilable<ReadTable_sp> fromReadTable, T_sp fromReadTableP) {
  if (fromReadTableP.nilp()) {
    if (core::_sym__PLUS_standardReadtable_PLUS_->symbolValue().nilp()) {
      core::_sym__PLUS_standardReadtable_PLUS_->defconstant(ReadTable_O::create_standard_readtable());
    }
    fromReadTable = core::_sym__PLUS_standardReadtable_PLUS_->symbolValue();
  }
  T_sp syntax = fromReadTable->syntax_type(fromChar);
  toReadTable->set_syntax_type(toChar, syntax);
  T_mv macro = fromReadTable->get_macro_character(fromChar);
  if (macro.notnilp()) {
    T_sp nonTerminating = macro.second();
    toReadTable->set_macro_character(toChar, macro, nonTerminating);
  }
  gc::Nilable<HashTable_sp> fromTable = fromReadTable->_DispatchMacroCharacters->gethash(fromChar);
  if (fromTable.notnilp()) {
    HashTableEql_sp toTable = HashTableEql_O::create_default();
    fromTable->maphash([&toTable](T_sp key, T_sp val) {
		    toTable->setf_gethash(key,val);
    });
    toReadTable->_DispatchMacroCharacters->setf_gethash(toChar, toTable);
  }
  return _lisp->_true();
}

CL_LAMBDA(char &optional non-terminating-p (readtable *readtable*));
CL_DECLARE();
CL_DOCSTRING("makeDispatchMacroCharacter");
CL_DEFUN T_sp cl__make_dispatch_macro_character(Character_sp ch, T_sp nonTerminatingP, ReadTable_sp readtable) {
  readtable->make_dispatch_macro_character(ch, nonTerminatingP);
  return _lisp->_true();
};

CL_LAMBDA(char &optional readtable);
CL_DECLARE();
CL_DOCSTRING("getMacroCharacter");
CL_DEFUN T_mv cl__get_macro_character(Character_sp chr, T_sp readtable) {
  if (readtable.nilp()) {
    readtable = gc::As<ReadTable_sp>(cl::_sym_STARreadtableSTAR->symbolValue());
  }
  return gc::As<ReadTable_sp>(readtable)->get_macro_character(chr);
};

CL_LAMBDA(&optional (from-readtable cl:*readtable*) to-readtable);
CL_DECLARE();
CL_DOCSTRING("clhs: copy-readtable");
CL_DEFUN T_sp cl__copy_readtable(gc::Nilable<ReadTable_sp> fromReadTable, gc::Nilable<ReadTable_sp> toReadTable) {
  if (fromReadTable.nilp()) {
    return ReadTable_O::create_standard_readtable();
  }
  return fromReadTable->copyReadTable(toReadTable);
}

CL_LAMBDA(readtable);
CL_DECLARE();
CL_DOCSTRING("clhs: readtable-case");
CL_DEFUN T_sp cl__readtable_case(ReadTable_sp readTable) {
  return readTable->getReadTableCase();
}

CL_LAMBDA(readtable mode);
CL_DECLARE();
CL_DOCSTRING("clhs: (setf readtable-case)");
CL_DEFUN void core__readtable_case_set(ReadTable_sp readTable, T_sp mode) {
  readTable->setf_readtable_case(gc::As<Symbol_sp>(mode));
}

CL_LAMBDA(dispChar subChar newFunction &optional (readtable *readtable*));
CL_DECLARE();
CL_DOCSTRING("setDispatchMacroCharacter");
CL_DEFUN T_mv cl__set_dispatch_macro_character(Character_sp dispChar, Character_sp subChar, T_sp newFunctionDesig, ReadTable_sp readtable) {
  return (Values(readtable->set_dispatch_macro_character(dispChar, subChar, newFunctionDesig)));
};

CL_LAMBDA(dispChar subChar &optional (readtable *readtable*));
CL_DECLARE();
CL_DOCSTRING("getDispatchMacroCharacter");
CL_DEFUN T_mv cl__get_dispatch_macro_character(Character_sp dispChar, Character_sp subChar, ReadTable_sp readtable) {
  return (Values(readtable->get_dispatch_macro_character(dispChar, subChar)));
};

CL_LAMBDA(ch func_desig &optional non-terminating-p (readtable *readtable*));
CL_DECLARE();
CL_DOCSTRING("setMacroCharacter");
CL_DEFUN T_mv cl__set_macro_character(Character_sp ch, T_sp func_desig, T_sp non_terminating_p, ReadTable_sp readtable) {
  return (Values(readtable->set_macro_character(ch, func_desig, non_terminating_p)));
};

SYMBOL_EXPORT_SC_(KeywordPkg, constituent_character);
SYMBOL_EXPORT_SC_(KeywordPkg, whitespace_character);
SYMBOL_SC_(CorePkg, STARconsing_dot_allowedSTAR);
SYMBOL_SC_(CorePkg, STARconsing_dotSTAR);
SYMBOL_SC_(CorePkg, STARpreserve_whitespace_pSTAR);
SYMBOL_SC_(CorePkg, STARinput_streamSTAR);
SYMBOL_SC_(CorePkg, STARbackquote_levelSTAR);
SYMBOL_SC_(CorePkg, STARstandard_readtableSTAR);

CL_LAMBDA(stream chr);
CL_DECLARE();
CL_DOCSTRING("reader_double_quote_string");
CL_DEFUN T_mv core__reader_double_quote_string(T_sp stream, Character_sp ch) {
  stringstream str;
  bool done = false;
  while (!done) {
    Character_sp nc = gc::As<Character_sp>(cl__read_char(stream, _lisp->_true(), _Nil<T_O>(), _lisp->_true()));
    char cc = clasp_as_char(nc);
    if (cc == '"')
      break;
    if (cc == '\\') {
      nc = gc::As<Character_sp>(cl__read_char(stream, _lisp->_true(), _Nil<T_O>(), _lisp->_true()));
      cc = clasp_as_char(nc);
      if (cc == 'n')
        cc = '\n';
    }
    str << cc;
  }
  // Return one value in a MultipleValues object to indicate that something is being returned
  return (Values(Str_O::create(str.str())));
};

CL_LAMBDA(sin ch);
CL_DECLARE();
CL_DOCSTRING("reader_backquoted_expression");
CL_DEFUN T_mv core__reader_backquoted_expression(T_sp sin, Character_sp ch) {
  Fixnum_sp backquote_level = gc::As<Fixnum_sp>(_sym_STARbackquote_levelSTAR->symbolValue());
  Fixnum_sp new_backquote_level = make_fixnum(unbox_fixnum(backquote_level) + 1);
  // DynamicScopeManager will save the dynamic value of the symbol and restore it in dtor
  DynamicScopeManager scope(_sym_STARbackquote_levelSTAR, new_backquote_level);
  T_sp quoted_object = cl__read(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true());
  Cons_sp result = Cons_O::createList(_sym_backquote, quoted_object);
  //HERE_scCONS_CREATE_LIST2(_sym_backquote,quoted_object);
  if (_lisp->sourceDatabase().notnilp()) {
    gc::As<SourceManager_sp>(_lisp->sourceDatabase())->duplicateSourcePosInfo(quoted_object, result);
  }
  return (Values(result));
};

CL_LAMBDA(sin ch);
CL_DECLARE();
CL_DOCSTRING("reader_comma_form");
CL_DEFUN T_sp core__reader_comma_form(T_sp sin, Character_sp ch) {
  Fixnum_sp backquote_level = gc::As<Fixnum_sp>(_sym_STARbackquote_levelSTAR->symbolValue());
  Fixnum_sp new_backquote_level = make_fixnum(unbox_fixnum(backquote_level) - 1);
  DynamicScopeManager scope(_sym_STARbackquote_levelSTAR, new_backquote_level);
  char nextc = clasp_peek_char(sin);
  //	ql::source_code_list list(sin->lineNumber(),sin->column(),core__source_file_info(sin));
  ql::list list;
  Symbol_sp head = _sym_unquote;
  if (nextc == '@') {
    head = _sym_unquote_splice;
    gc::As<Character_sp>(cl__read_char(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true()));
  } else if (nextc == '.') {
    head = _sym_unquote_nsplice;
    gc::As<Character_sp>(cl__read_char(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true()));
  }
#ifdef SOURCE_TRACKING
  SourcePosInfo_sp info = core__input_stream_source_pos_info(sin);
#endif
  T_sp comma_object = cl__read(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true());
  list << head << comma_object;
#ifdef SOURCE_TRACKING
  lisp_registerSourcePosInfo(list.cons(), info);
#endif
  return (list.cons());
};

CL_LAMBDA(sin ch);
CL_DECLARE();
CL_DOCSTRING("reader_list_allow_consing_dot");
CL_DEFUN T_sp core__reader_list_allow_consing_dot(T_sp sin, Character_sp ch) {
#ifdef SOURCE_TRACKING
  SourcePosInfo_sp info = core__input_stream_source_pos_info(sin);
#endif
  List_sp list = read_list(sin, ')', true);
#ifdef SOURCE_TRACKING
  lisp_registerSourcePosInfo(list, info);
#endif
  return list;
};

CL_LAMBDA(sin ch);
CL_DECLARE();
CL_DOCSTRING("reader_error_unmatched_close_parenthesis");
CL_DEFUN T_mv core__reader_error_unmatched_close_parenthesis(T_sp sin, Character_sp ch) {
#ifdef SOURCE_TRACKING
#error "There is nothing that uses info"
  SourceFileInfo_sp info = core__source_file_info(sin);
  SIMPLE_ERROR(BF("Unmatched close parenthesis in file: %s line: %s") % info->fileName() % clasp_input_lineno(sin));
#endif
  return (Values(_Nil<T_O>()));
};

CL_LAMBDA(sin ch);
CL_DECLARE();
CL_DOCSTRING("reader_quote");
CL_DEFUN T_sp core__reader_quote(T_sp sin, Character_sp ch) {
  //	ql::source_code_list result(sin->lineNumber(),sin->column(),core__source_file_info(sin));
  ql::list acc;
#ifdef SOURCE_TRACKING
  SourcePosInfo_sp spi = core__input_stream_source_pos_info(sin);
#endif
  T_sp quoted_object = cl__read(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true());
  acc << cl::_sym_quote << quoted_object;
  T_sp result = acc.cons();
#ifdef SOURCE_TRACKING
  lisp_registerSourcePosInfo(result, spi);
#endif
  return result;
}

CL_LAMBDA(sin ch);
CL_DECLARE();
CL_DOCSTRING("reader_skip_semicolon_comment");
CL_DEFUN T_mv core__reader_skip_semicolon_comment(T_sp sin, Character_sp ch) {
  ASSERT(clasp_input_stream_p(sin));
  stringstream str;
  bool done = false;
  while (!done) {
    Character_sp nc = gc::As<Character_sp>(cl__read_char(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true()));
    char cc = clasp_as_char(nc);
    if (cc == '\n')
      break;
  }
  // Return one value in a MultipleValues object to indicate that something is being returned
  return (Values0<T_O>());
};

CL_LAMBDA(sin ch);
CL_DECLARE();
CL_DOCSTRING("dispatch_macro_character");
CL_DEFUN T_mv core__dispatch_macro_character(T_sp sin, Character_sp ch) {
  char cpeek = clasp_peek_char(sin);
  bool sawnumarg = false;
  uint numarg = 0;
  while (isdigit(cpeek)) {
    sawnumarg = true;
    int cget = clasp_read_char(sin);
    if (cget == EOF) {
      SIMPLE_ERROR(BF("Hit eof in sharp macro"));
    }
    numarg *= 10;
    numarg += (cget - '0');
    cpeek = clasp_peek_char(sin);
  }
  T_sp onumarg(_Nil<T_O>());
  if (sawnumarg)
    onumarg = make_fixnum(numarg);
  Character_sp subchar = gc::As<Character_sp>(cl__read_char(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true()));
  T_sp macro_func = gc::As<ReadTable_sp>(cl::_sym_STARreadtableSTAR->symbolValue())->get_dispatch_macro_character(ch, subchar);
  if (macro_func.nilp())
    SIMPLE_ERROR(BF("Undefined reader macro for %s %s") % _rep_(ch) % _rep_(subchar));
  return eval::funcall(macro_func, sin, subchar, onumarg);
};

/*! See SACLA reader.lisp::read-ch */
T_sp read_ch(T_sp sin) {
  T_sp nc = cl__read_char(sin, _Nil<T_O>(), _Nil<T_O>(), _lisp->_true());
  return nc;
}

/*! See SACLA reader.lisp::read-ch-or-die */
T_sp read_ch_or_die(T_sp sin) {
  T_sp nc = cl__read_char(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true());
  return nc;
}

/*! See SACLA reader.lisp::unread-ch */
void unread_ch(T_sp sin, Character_sp c) {
  clasp_unread_char(clasp_as_char(c), sin);
}

/*! See SACLA reader.lisp::collect-escaped-lexemes */
List_sp collect_escaped_lexemes(Character_sp c, T_sp sin) {
  ReadTable_sp readTable = _lisp->getCurrentReadTable();
  Symbol_sp syntax_type = readTable->syntax_type(c);
  if (syntax_type == kw::_sym_invalid_character) {
    SIMPLE_ERROR(BF("invalid-character-error: %s") % _rep_(c));
  } else if (syntax_type == kw::_sym_multiple_escape_character) {
    return _Nil<T_O>();
  } else if (syntax_type == kw::_sym_single_escape_character) {
    return Cons_O::create(read_ch_or_die(sin), collect_escaped_lexemes(read_ch_or_die(sin), sin));
  } else if (syntax_type == kw::_sym_constituent_character || syntax_type == kw::_sym_whitespace_character || syntax_type == kw::_sym_terminating_macro_character || syntax_type == kw::_sym_non_terminating_macro_character) {
    return Cons_O::create(c, collect_escaped_lexemes(read_ch_or_die(sin), sin));
  }
  return _Nil<T_O>();
}

/*! See SACLA reader.lisp::collect-lexemes */
List_sp collect_lexemes(/*Character_sp*/ T_sp tc, T_sp sin) {
  if (tc.notnilp()) {
    Character_sp c = gc::As<Character_sp>(tc);
    ReadTable_sp readTable = _lisp->getCurrentReadTable();
    Symbol_sp syntax_type = readTable->syntax_type(c);
    if (syntax_type == kw::_sym_invalid_character) {
      SIMPLE_ERROR(BF("invalid-character-error: %s") % _rep_(c));
    } else if (syntax_type == kw::_sym_whitespace_character) {
      if (_sym_STARpreserve_whitespace_pSTAR->symbolValue().isTrue()) {
        unread_ch(sin, c);
      }
    } else if (syntax_type == kw::_sym_terminating_macro_character) {
      unread_ch(sin, c);
    } else if (syntax_type == kw::_sym_multiple_escape_character) {
      return Cons_O::create(collect_escaped_lexemes(read_ch_or_die(sin), sin),
                            collect_lexemes(read_ch(sin), sin));
    } else if (syntax_type == kw::_sym_single_escape_character) {
      return Cons_O::create(Cons_O::create(read_ch_or_die(sin)),
                            collect_lexemes(read_ch(sin), sin));
    } else if (syntax_type == kw::_sym_constituent_character || syntax_type == kw::_sym_non_terminating_macro_character) {
      return Cons_O::create(c, collect_lexemes(read_ch(sin), sin));
    }
  }
  return _Nil<T_O>();
}

/*! Works like SACLA readtable::make-str but accumulates the characters
      into a stringstream */
void make_str(stringstream &sout, List_sp cur_char, bool preserveCase = false) {
  while (cur_char.notnilp()) {
    T_sp obj = oCar(cur_char);
    if (cl__consp(obj)) {
      make_str(sout, obj, preserveCase);
    } else if (cl__characterp(obj)) {
      if (preserveCase)
        sout << clasp_as_char(gc::As<Character_sp>(obj));
      else
        sout << clasp_as_char(gc::As<ReadTable_sp>(cl::_sym_STARreadtableSTAR->symbolValue())->convert_case(gc::As<Character_sp>(obj)));
    } else {
      SIMPLE_ERROR(BF("Illegal entry for make_str[%s]") % _rep_(obj));
    }
    cur_char = oCdr(cur_char);
  }
}

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING("sharp_backslash");
CL_DEFUN T_mv core__sharp_backslash(T_sp sin, Character_sp ch, T_sp num) {
  stringstream sslexemes;
  List_sp lexemes = collect_lexemes(ch, sin);
  make_str(sslexemes, lexemes, true);
  if (!cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
    string lexeme_str = sslexemes.str();
    if (lexeme_str.size() == 1) {
      return (Values(clasp_make_character(lexeme_str[0])));
    } else {
      Str_sp name = Str_O::create(lexeme_str);
      T_sp tch = eval::funcall(cl::_sym_name_char, name);
      if (tch.nilp())
        SIMPLE_ERROR(BF("Unknown character name[%s]") % _rep_(name));
      return (Values(gc::As<Character_sp>(tch)));
    }
  }
  return Values(_Nil<T_O>());//(Values0<T_O>());
}

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING("sharp_dot");
CL_DEFUN T_sp core__sharp_dot(T_sp sin, Character_sp ch, T_sp num) {
#ifdef SOURCE_TRACKING
  SourcePosInfo_sp spi = core__input_stream_source_pos_info(sin);
#endif
  T_sp object = cl__read(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true());
  if (!cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
    if (!cl::_sym_STARread_evalSTAR->symbolValue().isTrue()) {
      READER_ERROR(Str_O::create("Cannot evaluate the form #.~S"),
                   Cons_O::create(object),
                   sin);
    }
    T_sp result = eval::funcall(core::_sym_STAReval_with_env_hookSTAR->symbolValue(), object, _Nil<T_O>());
#ifdef SOURCE_TRACKING
    if (cl__consp(result)) {
      lisp_registerSourcePosInfo(result, spi);
    }
#endif
    return result;
  }
  return (Values0<T_O>());
}

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING("sharp_single_quote");
CL_DEFUN T_sp core__sharp_single_quote(T_sp sin, Character_sp ch, T_sp num) {
#ifdef SOURCE_TRACKING
  SourcePosInfo_sp spi = core__input_stream_source_pos_info(sin);
#endif
  T_sp quoted_object = cl__read(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true());
  //	ql::source_code_list result(sin->lineNumber(),sin->column(),core__source_file_info(sin));
  ql::list result;
  result << cl::_sym_function << quoted_object;
#ifdef SOURCE_TRACKING
  lisp_registerSourcePosInfo(result.cons(), spi);
#endif
  T_sp tresult = result.cons();
  return tresult;
};

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING("sharp_left_parenthesis");
CL_DEFUN T_mv core__sharp_left_parenthesis(T_sp sin, Character_sp ch, /*Fixnum_sp*/ T_sp tnum) {
  Character_sp right_paren = clasp_make_character(')');
  T_sp olist = cl__read_delimited_list(right_paren, sin, _lisp->_true());
  List_sp list = olist;
  if (!cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
    int list_length = cl__length(list);
    if (tnum.notnilp()) {
      Fixnum_sp num = gc::As<Fixnum_sp>(tnum);
      if (list_length > unbox_fixnum(num))
        SIMPLE_ERROR(BF("vector is longer than specified length %s: %s") % unbox_fixnum(num) % _rep_(list));
      int need_length = unbox_fixnum(num);
      if (list_length < need_length) {
        List_sp reversed = cl__nreverse(list);
        T_sp fill_entry = oCar(reversed);
        for (int i = list_length; i < need_length; i++) {
          reversed = Cons_O::create(fill_entry, reversed);
        }
        list = cl__nreverse(reversed);
      }
    }
    VectorObjects_sp vec = VectorObjects_O::make(_Nil<T_O>(), list, cl__length(list), false, cl::_sym_T_O);
    return (Values(vec));
  }
  return (Values(_Nil<T_O>()));
};

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING("sharp_asterisk");
CL_DEFUN T_mv core__sharp_asterisk(T_sp sin, Character_sp ch, T_sp num) {
  int dimcount, dim = 0;
  stringstream pattern;
  ReadTable_sp rtbl = gc::As<ReadTable_sp>(cl::_sym_STARreadtableSTAR->symbolValue());

  if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
    cl__read(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true());
    return Values(_Nil<T_O>());
  }
  for (dimcount = 0;; dimcount++) {
    T_sp tch = cl__read_char(sin, _Nil<T_O>(), _Nil<T_O>(), _lisp->_true());
    if (tch.nilp())
      break;
    ch = gc::As<Character_sp>(tch);
    Symbol_sp syntaxType = rtbl->syntax_type(ch);
    if (syntaxType == kw::_sym_terminating_macro_character || syntaxType == kw::_sym_whitespace_character) {
      unread_ch(sin, ch);
      break;
    }
    unlikely_if(syntaxType == kw::_sym_single_escape_character ||
                syntaxType == kw::_sym_multiple_escape_character ||
                (clasp_as_char(ch) != '0' && clasp_as_char(ch) != '1')) {
      READER_ERROR(Str_O::create("Character ~:C is not allowed after #*"), Cons_O::create(ch), sin);
    }
    pattern << clasp_as_char(ch);
  }
  if (num.nilp()) {
    dim = dimcount;
  } else if (gc::IsA<Fixnum_sp>(num)) {
    dim = unbox_fixnum(gc::As<Fixnum_sp>(num));
    unlikely_if(dim < 0 ||
                (dim > CLASP_ARRAY_DIMENSION_LIMIT)) {
      READER_ERROR(Str_O::create("Wrong vector dimension size ~D in #*."), Cons_O::create(num), sin);
    }
    unlikely_if(dimcount > dim)
        READER_ERROR(Str_O::create("Too many elements in #*."), _Nil<T_O>(), sin);
    unlikely_if(dim && (dimcount == 0))
        READER_ERROR(Str_O::create("Cannot fill the bit-vector #*."), _Nil<T_O>(), sin);
  }
  string bitPattern = pattern.str();
  char last = bitPattern.size() > 0 ? bitPattern[bitPattern.size() - 1] : '0';
  SimpleBitVector_sp x = SimpleBitVector_O::make(dim);
  for (int i = 0; i < dim; i++) {
    char elt = (i < dimcount) ? bitPattern[i] : last;
    if (elt == '0')
      x->setBit(i, 0);
    else
      x->setBit(i, 1);
  }
  return Values(x);
};

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING("sharp_colon");
CL_DEFUN T_mv core__sharp_colon(T_sp sin, Character_sp ch, T_sp num) {
  stringstream sslexemes;
  List_sp lexemes = collect_lexemes(ch, sin);
  make_str(sslexemes, lexemes);
  if (!cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
    string lexeme_str = sslexemes.str().substr(1);
    Symbol_sp new_symbol = Symbol_O::create(lexeme_str);
    return (Values(new_symbol));
  }
  return (Values(_Nil<T_O>()));
}; // core__sharp_colon

CL_LAMBDA(stream subchar radix);
CL_DECLARE();
CL_DOCSTRING("sharp_r");
CL_DEFUN T_mv core__sharp_r(T_sp sin, Character_sp ch, gc::Nilable<Fixnum_sp> nradix) {
  if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
    T_sp object = cl__read(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true());
    (void)object; // suppress warning
    return (Values(_Nil<T_O>()));
  } else if (nradix.nilp()) {
    SIMPLE_ERROR(BF("Radix missing in #R reader macro"));
  } else {
    T_sp tradix = nradix;
    Fixnum_sp radix = gc::As<Fixnum_sp>(tradix);
    int iradix = unbox_fixnum(radix);
    if (iradix < 2 || iradix > 36) {
      SIMPLE_ERROR(BF("Illegal radix for #R: %d") % iradix);
    }
    {
      Fixnum_sp oradix = make_fixnum(iradix);
      DynamicScopeManager scope(cl::_sym_STARread_baseSTAR, oradix);
      T_sp val = cl__read(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true());
      if (!gc::IsA<Rational_sp>(val)) {
        SIMPLE_ERROR(BF("#%s (base %d) is not a rational: %s") % _rep_(ch) % iradix % _rep_(val));
      }
      return (Values(val));
    }
  }
}

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING("sharp_b");
CL_DEFUN T_mv core__sharp_b(T_sp sin, Character_sp ch, gc::Nilable<Fixnum_sp> num) {
  return core__sharp_r(sin, ch, make_fixnum(2));
};

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING("sharp_o");
CL_DEFUN T_mv core__sharp_o(T_sp sin, Character_sp ch, gc::Nilable<Fixnum_sp> num) {
  return core__sharp_r(sin, ch, make_fixnum(8));
};

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING("sharp_x");
CL_DEFUN T_mv core__sharp_x(T_sp sin, Character_sp ch, T_sp num) {
  return core__sharp_r(sin, ch, make_fixnum(16));
};

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING("sharp_c");
CL_DEFUN T_mv core__sharp_c(T_sp sin, Character_sp ch, T_sp num) {
  T_sp olist = cl__read(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true());
  List_sp list = olist;
  if (!cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
    int list_length = cl__length(list);
    if (list_length != 2) {
      SIMPLE_ERROR(BF("#C complex number needs two numbers"));
    }
    Real_sp r = gc::As<Real_sp>(oCar(list));
    Real_sp i = gc::As<Real_sp>(oCadr(list));
    return (Values(Complex_O::create(r, i)));
  }
  return (Values(_Nil<T_O>()));

}; // core__sharp_c

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING("sharp_a");
CL_DEFUN T_mv core__sharp_a(T_sp sin, Character_sp ch, T_sp num) {
  IMPLEMENT_MEF(BF("Implement sharp_a"));
}; // core__sharp_a

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING("sharp_s");
CL_DEFUN T_mv core__sharp_s(T_sp sin, Character_sp ch, T_sp num) {
  IMPLEMENT_MEF(BF("Implement sharp_s"));
}; // core__sharp_s

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING("sharp_p");
CL_DEFUN T_mv core__sharp_p(T_sp sin, Character_sp ch, T_sp num) {
  bool suppress = cl::_sym_STARread_suppressSTAR->symbolValue().isTrue();
  if (num.notnilp() && !suppress)
    extra_argument('P', sin, num);
  T_sp d = cl__read(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true());
  if (suppress) {
    d = _Nil<T_O>();
  } else {
    d = cl__parse_namestring(d);
  }
  return Values(d);
}; // core__sharp_p


CL_LAMBDA(feature-test);
CL_DECLARE();
CL_DOCSTRING("feature_p takes one argument - a feature test");
CL_DEFUN T_sp core__reader_feature_p(T_sp feature_test) {
  if (feature_test.nilp())
    return _Nil<T_O>();
  else if (cl__atom(feature_test)) {
    List_sp features_list = cl::_sym_STARfeaturesSTAR->symbolValue();
    if (features_list.nilp())
      return _Nil<T_O>();
    return features_list.asCons()->member(gc::As<Symbol_sp>(feature_test),
                                          _Nil<T_O>(), _Nil<T_O>(), _Nil<T_O>());
  } else {
    ASSERT(cl__listp(feature_test));
    List_sp features_cons = feature_test;
    T_sp features_head = oCar(features_cons);
    if (features_head == kw::_sym_not) {
      return _lisp->_not(eval::funcall(_sym_reader_feature_p, oSecond(features_cons)));
    } else if (features_head == kw::_sym_and) {
      return (eval::funcall(cl::_sym_every, _sym_reader_feature_p, oCdr(features_cons)));
    } else if (features_head == kw::_sym_or) {
      return (eval::funcall(cl::_sym_some, _sym_reader_feature_p, oCdr(features_cons)));
    }
    SIMPLE_ERROR(BF("Illegal feature test: %s") % _rep_(features_cons));
  }
}

/*! Read a feature test in the keyword package */
T_sp read_feature_test(T_sp sin) {
  // Read the feature test in the keyword package
  DynamicScopeManager dynamicScopeManager(cl::_sym_STARpackageSTAR, _lisp->keywordPackage());
  T_sp feature = cl__read(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true());
  return feature;
}

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING("sharp_plus");
CL_DEFUN T_mv core__sharp_plus(T_sp sin, Character_sp ch, T_sp num) {
  T_sp feat = read_feature_test(sin);
  LOG(BF("feature[%s]") % _rep_(feat));
  if (T_sp(eval::funcall(_sym_reader_feature_p, feat)).isTrue()) {
    LOG(BF("The feature test passed - reading lisp object"));
    T_sp obj = cl__read(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true());
    LOG(BF("Read the object[%s]") % _rep_(obj));
    return Values(obj);
  } else {
    _BLOCK_TRACEF(BF("Suppressing read for unsupported feature[%s]") % feat->__repr__());
    DynamicScopeManager dynScopeManager(cl::_sym_STARread_suppressSTAR, _lisp->_true());
    cl__read(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true());
    return Values0<T_O>();
  }
}; // core__sharp_plus

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING("sharp_minus");
CL_DEFUN T_mv core__sharp_minus(T_sp sin, Character_sp ch, T_sp num) {
  T_sp feat = read_feature_test(sin);
  LOG(BF("feature[%s]") % _rep_(feat));
  if (!T_sp(eval::funcall(_sym_reader_feature_p, feat)).isTrue()) {
    LOG(BF("The feature test passed - reading lisp object"));
    T_sp obj = cl__read(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true());
    LOG(BF("Read the object[%s]") % _rep_(obj));
    return Values(obj);
  } else {
    LOG(BF("The feature test failed - returning nil"));
    DynamicScopeManager dynScopeManager(cl::_sym_STARread_suppressSTAR, _lisp->_true());
    cl__read(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true());
    return Values0<T_O>();
  }
}; // core__sharp_minus

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING("sharp_vertical_bar");
CL_DEFUN T_mv core__sharp_vertical_bar(T_sp sin, Character_sp ch, T_sp num) {
  ASSERT(clasp_input_stream_p(sin));
  bool done = false;
  while (!done) {
    Character_sp nc = gc::As<Character_sp>(cl__read_char(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true()));
    char cc = clasp_as_char(nc);
    if (cc == '#') {
      char nextc = clasp_peek_char(sin);
      if (nextc == '|') {
        Character_sp nextsubc = gc::As<Character_sp>(cl__read_char(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true()));
        eval::funcall(_sym_sharp_vertical_bar, sin, nextsubc, num);
      }
    } else if (cc == '|') {
      char nextc = clasp_peek_char(sin);
      if (nextc == '#') {
        Character_sp nextsubc = gc::As<Character_sp>(cl__read_char(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true()));
        (void)nextsubc;
        goto DONE;
      }
    }
  }
// Return nothing in MultipleValues to indicate that nothing is returned
DONE:
  return (Values0<T_O>());
}; // core__sharp_vertical_bar


SYMBOL_EXPORT_SC_(KeywordPkg, syntax);
SYMBOL_EXPORT_SC_(KeywordPkg, whitespace_character);
HashTable_sp ReadTable_O::create_standard_syntax_table() {
  HashTableEql_sp syntax = HashTableEql_O::create_default();
  syntax->setf_gethash(clasp_character_create_from_name("TAB"), kw::_sym_whitespace_character);
  syntax->setf_gethash(clasp_character_create_from_name("NEWLINE"), kw::_sym_whitespace_character);
  syntax->setf_gethash(clasp_character_create_from_name("LINEFEED"), kw::_sym_whitespace_character);
  syntax->setf_gethash(clasp_character_create_from_name("PAGE"), kw::_sym_whitespace_character);
  syntax->setf_gethash(clasp_character_create_from_name("RETURN"), kw::_sym_whitespace_character);
  syntax->setf_gethash(clasp_character_create_from_name("SPACE"), kw::_sym_whitespace_character);
  SYMBOL_EXPORT_SC_(KeywordPkg, single_escape_character);
  SYMBOL_EXPORT_SC_(KeywordPkg, multiple_escape_character);
  syntax->hash_table_setf_gethash(clasp_make_standard_character('\\'), kw::_sym_single_escape_character);
  syntax->hash_table_setf_gethash(clasp_make_standard_character('|'), kw::_sym_multiple_escape_character);
  return syntax;
}

ReadTable_sp ReadTable_O::create_standard_readtable() {
  GC_ALLOCATE(ReadTable_O, rt);
  rt->_SyntaxTypes = ReadTable_O::create_standard_syntax_table();
  ASSERTNOTNULL(_sym_reader_backquoted_expression->symbolFunction());
  ASSERT(_sym_reader_backquoted_expression->symbolFunction().notnilp());
  rt->set_macro_character(clasp_make_standard_character('`'),
                          _sym_reader_backquoted_expression->symbolFunction(),
                          _Nil<T_O>());
  rt->set_macro_character(clasp_make_standard_character(','),
                          _sym_reader_comma_form->symbolFunction(),
                          _Nil<T_O>());
  SYMBOL_SC_(CorePkg, read_list_allow_consing_dot);
  rt->set_macro_character(clasp_make_standard_character('('),
                          _sym_reader_list_allow_consing_dot->symbolFunction(),
                          _Nil<T_O>());
  SYMBOL_SC_(CorePkg, reader_error_unmatched_close_parenthesis);
  rt->set_macro_character(clasp_make_standard_character(')'),
                          _sym_reader_error_unmatched_close_parenthesis->symbolFunction(),
                          _Nil<T_O>());
  SYMBOL_SC_(CorePkg, reader_quote);
  rt->set_macro_character(clasp_make_standard_character('\''),
                          _sym_reader_quote->symbolFunction(),
                          _Nil<T_O>());
  SYMBOL_SC_(CorePkg, reader_skip_semicolon_comment);
  rt->set_macro_character(clasp_make_standard_character(';'),
                          _sym_reader_skip_semicolon_comment->symbolFunction(),
                          _Nil<T_O>());
  SYMBOL_SC_(CorePkg, reader_read_double_quote_string);
  rt->set_macro_character(clasp_make_standard_character('"'),
                          _sym_reader_double_quote_string->symbolFunction(),
                          _Nil<T_O>());
  Character_sp sharp = clasp_make_standard_character('#');
  rt->make_dispatch_macro_character(sharp, _lisp->_true());
  ql::list dispatchers(_lisp);
  dispatchers << clasp_make_standard_character('\\') << _sym_sharp_backslash
              << clasp_make_standard_character('\'') << _sym_sharp_single_quote
              << clasp_make_standard_character('(') << _sym_sharp_left_parenthesis
              << clasp_make_standard_character('*') << _sym_sharp_asterisk
              << clasp_make_standard_character(':') << _sym_sharp_colon
              << clasp_make_standard_character('.') << _sym_sharp_dot
              << clasp_make_standard_character('b') << _sym_sharp_b
              << clasp_make_standard_character('o') << _sym_sharp_o
              << clasp_make_standard_character('x') << _sym_sharp_x
              << clasp_make_standard_character('r') << _sym_sharp_r
              << clasp_make_standard_character('c') << _sym_sharp_c
              << clasp_make_standard_character('a') << _sym_sharp_a
              << clasp_make_standard_character('s') << _sym_sharp_s
              << clasp_make_standard_character('p') << _sym_sharp_p
              //		    << clasp_make_standard_character('=') << _sym_sharp_equal
              //		    << clasp_make_standard_character('#') << _sym_sharp_sharp
              << clasp_make_standard_character('+') << _sym_sharp_plus
              << clasp_make_standard_character('-') << _sym_sharp_minus
              << clasp_make_standard_character('|') << _sym_sharp_vertical_bar;
  for (List_sp cur = dispatchers.cons(); cur.notnilp(); cur = oCdr(oCdr(cur))) {
    Character_sp ch = gc::As<Character_sp>(oCar(cur));
    Symbol_sp sym = gc::As<Symbol_sp>(oCadr(cur));
    rt->set_dispatch_macro_character(sharp, ch, sym);
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

SYMBOL_EXPORT_SC_(KeywordPkg, upcase);
SYMBOL_EXPORT_SC_(KeywordPkg, downcase);
SYMBOL_EXPORT_SC_(KeywordPkg, preserve);
SYMBOL_EXPORT_SC_(KeywordPkg, invert);
void ReadTable_O::initialize() {
  _OF();
  this->Base::initialize();
  //	printf("%s:%d Initializing readtable\n", __FILE__, __LINE__ );
  this->_Case = kw::_sym_upcase;
  this->_SyntaxTypes = HashTableEql_O::create_default();
  this->_MacroCharacters = HashTableEql_O::create_default();
  this->_DispatchMacroCharacters = HashTableEql_O::create_default();
}

clasp_readtable_case ReadTable_O::getReadTableCaseAsEnum() {
  Symbol_sp ccase = this->_Case;
  if (ccase == kw::_sym_upcase) {
    return clasp_case_upcase;
  } else if (ccase == kw::_sym_downcase) {
    return clasp_case_downcase;
  } else if (ccase == kw::_sym_invert) {
    return clasp_case_invert;
  } else if (ccase == kw::_sym_preserve) {
    return clasp_case_preserve;
  }
  SIMPLE_ERROR(BF("Unknown readtable case: %s") % _rep_(this->_Case));
}

Symbol_sp ReadTable_O::setf_readtable_case(Symbol_sp newCase) {
  _OF();
  if ((newCase == kw::_sym_upcase) || (newCase == kw::_sym_downcase) || (newCase == kw::_sym_preserve) || (newCase == kw::_sym_invert)) {
    this->_Case = newCase;
    return newCase;
  } else {
    SIMPLE_ERROR(BF("Illegal newValue[%s] for (setf (readtable-case {readtable}) newValue) - it can only be :upcase, :downcase, :preserve or :invert") % _rep_(newCase));
  }
}

T_sp ReadTable_O::set_syntax_type(Character_sp ch, T_sp syntaxType) {
  this->_SyntaxTypes->setf_gethash(ch, syntaxType);
  return _lisp->_true();
}

SYMBOL_EXPORT_SC_(KeywordPkg, non_terminating_macro_character);
SYMBOL_EXPORT_SC_(KeywordPkg, terminating_macro_character);
SYMBOL_EXPORT_SC_(KeywordPkg, macro_function);

T_sp ReadTable_O::set_macro_character(Character_sp ch, T_sp funcDesig, T_sp non_terminating_p) {
  if (non_terminating_p.isTrue()) {
    this->set_syntax_type(ch, kw::_sym_non_terminating_macro_character);
  } else {
    this->set_syntax_type(ch, kw::_sym_terminating_macro_character);
  }
  Function_sp func = coerce::functionDesignator(funcDesig);
  this->_MacroCharacters->setf_gethash(ch, func);
  return _lisp->_true();
}

string ReadTable_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString();
  ss << " :case " << _rep_(this->_Case); 
  ss << "> ";
  return ss.str();
}

Symbol_sp ReadTable_O::syntax_type(Character_sp ch) const {
  _OF();
  Symbol_sp result = this->_SyntaxTypes->gethash(ch, kw::_sym_constituent_character);
  LOG(BF("character[%s] syntax_type: %s") % _rep_(ch) % _rep_(result));
  return result;
}

T_mv ReadTable_O::get_macro_character(Character_sp ch) {
  _OF();
  T_sp dispatcher = this->_MacroCharacters->gethash(ch, _Nil<T_O>());
  Symbol_sp syntaxType = this->syntax_type(ch);
  if (syntaxType == kw::_sym_terminating_macro_character) {
    return (Values(dispatcher, _Nil<T_O>()));
  } else if (syntaxType == kw::_sym_non_terminating_macro_character) {
    return (Values(dispatcher, _lisp->_true()));
  }
  return (Values(_Nil<T_O>(), _Nil<T_O>()));
}

T_sp ReadTable_O::make_dispatch_macro_character(Character_sp ch, T_sp non_terminating_p) {
  _OF();
  this->set_macro_character(ch, _sym_dispatch_macro_character, non_terminating_p);
  this->_DispatchMacroCharacters->setf_gethash(ch, HashTableEql_O::create_default());
  return _lisp->_true();
#if 0
  HashTable_sp syntax = this->_Syntax;
	List_sp plist = syntax->gethash(ch,_Nil<T_O>());
	ql::list qplist(_lisp);
	SYMBOL_EXPORT_SC_(KeywordPkg,dispatch_table);
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
#endif
}

T_sp ReadTable_O::set_dispatch_macro_character(Character_sp disp_char, Character_sp sub_char,
                                               T_sp new_func_desig) {
  if (this->get_macro_character(disp_char) != _sym_dispatch_macro_character->symbolFunction()) {
    SIMPLE_ERROR(BF("%c is not a dispatch character") % _rep_(disp_char));
  }
  HashTable_sp dispatch_table = this->_DispatchMacroCharacters->gethash(disp_char);
  ASSERTF(dispatch_table.notnilp(), BF("The dispatch table for the character[%s] is nil! - this shouldn't happen") % _rep_(disp_char));
  Character_sp upcase_sub_char = clasp_char_upcase(sub_char);
  Function_sp new_func = coerce::functionDesignator(new_func_desig);
  dispatch_table->hash_table_setf_gethash(upcase_sub_char, new_func);
  return _lisp->_true();
#if 0
	if ( this->get_macro_character(disp_char)
	     != _sym_dispatch_macro_character->symbolFunction() )
	{
	    SIMPLE_ERROR(BF("%c is not a dispatch character") % _rep_(disp_char) );
	}
	HashTable_sp syntax_table = this->_Syntax;
	List_sp disp_char_plist = syntax_table->gethash(disp_char,_Nil<T_O>());
	HashTable_sp dispatch_table = disp_char_plist->getf(kw::_sym_dispatch_table,_Nil<HashTable_O>() ).as<HashTable_O>();
	ASSERTF(dispatch_table.notnilp(),BF("The dispatch table for the character[%s] is nil! - this shouldn't happen") % _rep_(disp_char) );
	Character_sp upcase_sub_char = clasp_char_upcase(sub_char);
	Function_sp new_func = coerce::functionDesignator(new_func_desig);
	dispatch_table->hash_table_setf_gethash(upcase_sub_char,new_func);
	return _lisp->_true();
#endif
}

T_sp ReadTable_O::get_dispatch_macro_character(Character_sp disp_char, Character_sp sub_char) {
  _OF();
  if (this->get_macro_character(disp_char) != _sym_dispatch_macro_character->symbolFunction()) {
    SIMPLE_ERROR(BF("%c is not a dispatch character") % _rep_(disp_char));
  }
  HashTable_sp dispatch_table = this->_DispatchMacroCharacters->gethash(disp_char);
  ASSERTF(dispatch_table.notnilp(), BF("The dispatch table for the character[%s] is nil! - this shouldn't happen") % _rep_(disp_char));
  Character_sp upcase_sub_char = clasp_char_upcase(sub_char);
  T_sp func = dispatch_table->gethash(upcase_sub_char, _Nil<T_O>());
  return func;
#if 0
	HashTable_sp syntax_table = this->_Syntax;
	Cons_sp disp_char_plist = syntax_table->gethash(disp_char,_Nil<T_O>());
	HashTable_sp dispatch_table = disp_char_plist->getf(kw::_sym_dispatch_table,_Nil<HashTable_O>() ).as<HashTable_O>();
	ASSERTF(dispatch_table.notnilp(),BF("The dispatch table for the character[%s] is nil! - this shouldn't happen") % _rep_(disp_char) );
	Character_sp upcase_sub_char = clasp_char_upcase(sub_char);
	Function_sp func = dispatch_table->gethash(upcase_sub_char,_Nil<T_O>()).as<Function_O>();
	return func;
#endif
}

Character_sp ReadTable_O::convert_case(Character_sp cc) {
  _OF();
  if (this->_Case == kw::_sym_upcase) {
    return clasp_char_upcase(cc);
  } else if (this->_Case == kw::_sym_downcase) {
    return clasp_char_downcase(cc);
  } else if (this->_Case == kw::_sym_preserve) {
    return cc;
  } else if (this->_Case == kw::_sym_invert) {
    SIMPLE_ERROR(BF("I can't handle invert yet, that has to be handled when the token is converted"));
  }
  SIMPLE_ERROR(BF("Bad readtable case[%s]") % _rep_(this->_Case));
}

ReadTable_sp ReadTable_O::copyReadTable(gc::Nilable<ReadTable_sp> tdest) {
  //	printf("%s:%d copy-readtable\n", __FILE__, __LINE__ );
  if (tdest.nilp()) {
    //	    printf("%s:%d allocating copy-readtable\n", __FILE__, __LINE__ );
    GC_ALLOCATE(ReadTable_O, temp);
    tdest = temp;
  }
  ReadTable_sp dest = gc::As<ReadTable_sp>(tdest);
  //	printf("%s:%d dest.nilp() == %d\n", __FILE__, __LINE__, dest.nilp());
  //	printf("%s:%d dest->_SyntaxTypes.nilp() == %d\n", __FILE__, __LINE__, dest->_SyntaxTypes.nilp());
  //	printf("%s:%d about to _SyntaxTypes->clrhash() copy-readtable\n", __FILE__, __LINE__ );
  dest->_SyntaxTypes->clrhash();
  //	printf("%s:%d about to _MacroCharacters->clrhash() copy-readtable\n", __FILE__, __LINE__ );
  dest->_MacroCharacters->clrhash();
  dest->_DispatchMacroCharacters->clrhash();
  this->_SyntaxTypes->maphash([&dest](T_sp key, T_sp val) {
		dest->_SyntaxTypes->setf_gethash(key,val);
  });
  this->_MacroCharacters->maphash([&dest](T_sp key, T_sp val) {
		dest->_MacroCharacters->setf_gethash(key,val);
  });
  this->_DispatchMacroCharacters->maphash([&dest](T_sp key, T_sp val) {
		HashTable_sp entry = gc::As<HashTable_sp>(val);
		HashTable_sp table = HashTableEql_O::create_default();
		entry->maphash( [&table] (T_sp subkey, T_sp func) {
			table->setf_gethash(subkey,func);
		    } );
		dest->_DispatchMacroCharacters->setf_gethash(key,table);
  });
  dest->_Case = this->_Case;
  return dest;
}

  SYMBOL_EXPORT_SC_(ClPkg, setMacroCharacter);
  SYMBOL_SC_(CorePkg, reader_backquoted_expression);
  SYMBOL_SC_(CorePkg, sharp_backslash);
  SYMBOL_SC_(CorePkg, sharp_single_quote);
  SYMBOL_SC_(CorePkg, sharp_left_parenthesis);
  SYMBOL_SC_(CorePkg, sharp_asterisk);
  SYMBOL_SC_(CorePkg, sharp_colon);
  SYMBOL_SC_(CorePkg, sharp_dot);
  SYMBOL_SC_(CorePkg, sharp_b);
  SYMBOL_SC_(CorePkg, sharp_o);
  SYMBOL_SC_(CorePkg, sharp_x);
  SYMBOL_SC_(CorePkg, sharp_r);
  SYMBOL_SC_(CorePkg, sharp_c);
  SYMBOL_SC_(CorePkg, sharp_a);
  SYMBOL_SC_(CorePkg, sharp_s);
  SYMBOL_SC_(CorePkg, sharp_p);
  SYMBOL_SC_(CorePkg, sharp_plus);
  SYMBOL_SC_(CorePkg, sharp_minus);
  SYMBOL_SC_(CorePkg, sharp_vertical_bar);
  SYMBOL_SC_(CorePkg, dispatch_macro_character);
  SYMBOL_SC_(CorePkg, reader_double_quote_string);
  SYMBOL_SC_(CorePkg, reader_comma_form);
  SYMBOL_SC_(CorePkg, reader_list_allow_consing_dot);
  SYMBOL_SC_(CorePkg, reader_error_unmatched_close_parenthesis);
  SYMBOL_SC_(CorePkg, reader_quote);
  SYMBOL_SC_(CorePkg, reader_skip_semicolon_comment);
  SYMBOL_SC_(CorePkg, reader_feature_p);
  SYMBOL_EXPORT_SC_(ClPkg, setDispatchMacroCharacter);
  SYMBOL_EXPORT_SC_(ClPkg, getDispatchMacroCharacter);




}; /* core */
