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

// #define DEBUG_LEVEL_FULL
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/character.h>
#include <clasp/core/array.h>
#include <clasp/core/package.h>
#include <clasp/core/predicates.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/designators.h>
#include <clasp/core/lispStream.h>
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
  READER_ERROR(SimpleBaseString_O::make("~S is an extra argument for the #~C readmacro."),
               Cons_O::createList(arg, clasp_make_character(macro)), sin);
}

CL_LAMBDA(tochar fromchar &optional (toreadtable *readtable*) (fromreadtable nil fromreadtablep));
CL_DECLARE();
CL_DOCSTRING(R"dx(setSyntaxFromChar)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__set_syntax_from_char(Character_sp toChar, Character_sp fromChar, T_sp ttoReadTable, T_sp tfromReadTable,
                                       T_sp fromReadTableP) {
  if (gc::IsA<Readtable_sp>(ttoReadTable)) {
    Readtable_sp toReadTable = gc::As_unsafe<Readtable_sp>(ttoReadTable);
    if (fromReadTableP.nilp()) {
      tfromReadTable = core::_sym__PLUS_standardReadtable_PLUS_->symbolValue();
    } else {
      if (!gc::IsA<Readtable_sp>(tfromReadTable)) {
        SIMPLE_ERROR("read-tables are not both cl:readtable");
      }
    }
    Readtable_sp fromReadTable = gc::As<Readtable_sp>(tfromReadTable);
    T_sp syntax = core__syntax_type(fromReadTable, fromChar);
    core__setf_syntax_type(syntax, toReadTable, toChar);
    T_mv macro = cl__get_macro_character(fromChar, tfromReadTable);
    if (macro.notnilp()) {
      MultipleValues& mvn = core::lisp_multipleValues();
      T_sp nonTerminating = mvn.second(macro.number_of_values());
      toReadTable->set_macro_character_(toChar, macro, nonTerminating);
    }
    gc::Nilable<HashTable_sp> fromTable = fromReadTable->DispatchMacroCharacters_->gethash(fromChar);
    if (fromTable.notnilp()) {
      HashTable_sp toTable = HashTable_O::createEql();
      fromTable->maphash([&toTable](T_sp key, T_sp val) { toTable->setf_gethash(key, val); });
      toReadTable->DispatchMacroCharacters_->setf_gethash(toChar, toTable);
    } else {
      toReadTable->DispatchMacroCharacters_->remhash(toChar);
    }
    return _lisp->_true();
  }
  return eval::funcall(eclector_readtable::_sym_set_syntax_from_char, toChar, fromChar, ttoReadTable, tfromReadTable);
}

CL_LAMBDA(char &optional non-terminating-p (readtable *readtable*));
CL_DECLARE();
CL_DOCSTRING(R"dx(makeDispatchMacroCharacter)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__make_dispatch_macro_character(Character_sp ch, T_sp nonTerminatingP, T_sp readtable) {
  if (gc::IsA<Readtable_sp>(readtable)) {
    gc::As_unsafe<Readtable_sp>(readtable)->make_dispatch_macro_character_(ch, nonTerminatingP);
    return _lisp->_true();
  }
  return eval::funcall(eclector_readtable::_sym_make_dispatch_macro_character, readtable, ch, nonTerminatingP);
};

CL_LAMBDA(char &optional (readtable *readtable*));
CL_DECLARE();
CL_DOCSTRING(R"dx(getMacroCharacter)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__get_macro_character(Character_sp chr, T_sp readtable) {
  if (gc::IsA<Readtable_sp>(readtable)) {
    return gc::As_unsafe<Readtable_sp>(readtable)->get_macro_character_(chr);
  }
  return eval::funcall(eclector_readtable::_sym_get_macro_character, readtable, chr);
};

CL_LAMBDA(&optional (from-readtable cl:*readtable*) to-readtable);
CL_DECLARE();
CL_DOCSTRING(R"dx(clhs: copy-readtable)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__copy_readtable(T_sp fromReadTable, T_sp toReadTable) {
  if (fromReadTable.nilp()) {
    return Readtable_O::create_standard_readtable();
  }
  if (gc::IsA<Readtable_sp>(fromReadTable)) {
    if (toReadTable.notnilp() && !gc::IsA<Readtable_sp>(toReadTable)) {
      SIMPLE_ERROR("Mismatch in readtable-type from-read-table {} and to-read-table {}", _rep_(fromReadTable), _rep_(toReadTable));
    }
    return gc::As_unsafe<Readtable_sp>(fromReadTable)->copyReadtable_(toReadTable);
  }
  if (toReadTable.nilp())
    return eval::funcall(eclector_readtable::_sym_copy_readtable, fromReadTable);
  else
    return eval::funcall(eclector_readtable::_sym_copy_readtable_into, fromReadTable, toReadTable);
}

CL_LAMBDA(readtable);
CL_DECLARE();
CL_DOCSTRING(R"dx(clhs: readtable-case)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__readtable_case(T_sp readtable) {
  // FIXME: Should be possible to declare readtable Readtable_sp, but isn't
  if (gc::IsA<Readtable_sp>(readtable))
    return gc::As<Readtable_sp>(readtable)->getReadtableCase_();
  return eval::funcall(eclector_readtable::_sym_readtable_case, readtable);
}

CL_LISPIFY_NAME("cl:readtable-case");
CL_LAMBDA(mode readtable);
CL_DECLARE();
CL_DOCSTRING(R"dx(clhs: (setf readtable-case))dx");
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp core__readtable_case_set(T_sp mode, T_sp readTable) {
  if (gc::IsA<Readtable_sp>(readTable))
    return gc::As_unsafe<Readtable_sp>(readTable)->setf_readtable_case_(gc::As<Symbol_sp>(mode));
  return eval::funcall(eclector_readtable::_sym_setf_readtable_case, mode, readTable);
}

CL_LAMBDA(dispChar subChar newFunction &optional (readtable *readtable*));
CL_DECLARE();
CL_DOCSTRING(R"dx(setDispatchMacroCharacter)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__set_dispatch_macro_character(Character_sp dispChar, Character_sp subChar, T_sp newFunctionDesig, T_sp readtable) {
  if (gc::IsA<Readtable_sp>(readtable)) {
    return gc::As_unsafe<Readtable_sp>(readtable)->set_dispatch_macro_character_(dispChar, subChar, newFunctionDesig);
  }
  return eval::funcall(eclector_readtable::_sym_set_dispatch_macro_character, readtable, dispChar, subChar, newFunctionDesig);
};

CL_LAMBDA(dispChar subChar &optional (readtable *readtable*));
CL_DECLARE();
CL_DOCSTRING(R"dx(getDispatchMacroCharacter)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__get_dispatch_macro_character(Character_sp dispChar, Character_sp subChar, T_sp readtable) {
  if (gc::IsA<Readtable_sp>(readtable))
    return gc::As_unsafe<Readtable_sp>(readtable)->get_dispatch_macro_character_(dispChar, subChar);
  return eval::funcall(eclector_readtable::_sym_get_dispatch_macro_character, readtable, dispChar, subChar);
};

CL_LAMBDA(ch func-desig &optional non-terminating-p (readtable *readtable*));
CL_DECLARE();
CL_DOCSTRING(R"dx(setMacroCharacter)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__set_macro_character(Character_sp ch, T_sp func_desig, T_sp non_terminating_p, T_sp readtable) {
  if (gc::IsA<Readtable_sp>(readtable))
    return gc::As_unsafe<Readtable_sp>(readtable)->set_macro_character_(ch, func_desig, non_terminating_p);
  return eval::funcall(eclector_readtable::_sym_set_macro_character, readtable, ch, func_desig, non_terminating_p);
};

CL_LAMBDA(readtable chr);
CL_DECLARE();
CL_DOCSTRING(
    R"dx(Return the syntax type of chr. Either :whitespace, :terminating-macro, :non-terminating-macro, :constituent, :single-escape, :multiple-escape, or :invalid.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__syntax_type(T_sp readtable, Character_sp chr) {
  if (gc::IsA<Readtable_sp>(readtable))
    return gc::As_unsafe<Readtable_sp>(readtable)->syntax_type_(chr);
  return eval::funcall(eclector_readtable::_sym_syntax_type, readtable, chr);
}

CL_LAMBDA(syntax-type readtable chr);
CL_DECLARE();
CL_DOCSTRING(
    R"dx(Return the syntax type of chr. Either :whitespace, :terminating-macro, :non-terminating-macro, :constituent, :single-escape, :multiple-escape, or :invalid.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__setf_syntax_type(T_sp syntax_type, T_sp readtable, Character_sp chr) {
  if (gc::IsA<Readtable_sp>(readtable))
    return gc::As_unsafe<Readtable_sp>(readtable)->set_syntax_type_(chr, syntax_type);
  return eval::funcall(eclector_readtable::_sym_setf_syntax_type, syntax_type, readtable, chr);
}

SYMBOL_EXPORT_SC_(KeywordPkg, constituent);
SYMBOL_EXPORT_SC_(KeywordPkg, whitespace);
SYMBOL_EXPORT_SC_(KeywordPkg, single_escape);
SYMBOL_EXPORT_SC_(KeywordPkg, multiple_escape);
SYMBOL_EXPORT_SC_(KeywordPkg, non_terminating_macro);
SYMBOL_EXPORT_SC_(KeywordPkg, terminating_macro);
SYMBOL_EXPORT_SC_(KeywordPkg, invalid);

SYMBOL_SC_(CorePkg, STARconsing_dot_allowedSTAR);
SYMBOL_SC_(CorePkg, STARconsing_dotSTAR);
SYMBOL_SC_(CorePkg, STARpreserve_whitespace_pSTAR);
SYMBOL_SC_(CorePkg, STARinput_streamSTAR);
SYMBOL_SC_(CorePkg, STARbackquote_levelSTAR);
SYMBOL_SC_(CorePkg, STARstandard_readtableSTAR);

CL_LAMBDA(stream chr);
CL_DECLARE();
CL_DOCSTRING(R"dx(reader_double_quote_string)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__reader_double_quote_string(T_sp stream, Character_sp ch) {
  claspCharacter delimiter = clasp_as_claspCharacter(ch);
  // Create a wide character string buffer
  SafeBufferStrWNs buffer;
  bool done = false;
  while (!done) {
    Character_sp nc = gc::As<Character_sp>(cl__read_char(stream, _lisp->_true(), nil<T_O>(), _lisp->_true()));
    claspCharacter cc = clasp_as_claspCharacter(nc);
    if (cc == delimiter)
      break;
    if (cc == '\\') {
      nc = gc::As<Character_sp>(cl__read_char(stream, _lisp->_true(), nil<T_O>(), _lisp->_true()));
      cc = clasp_as_claspCharacter(nc);
      if (cc == 'n')
        cc = '\n';
    }
    buffer.string()->vectorPushExtend(cc);
  }
  SimpleString_sp result = buffer.string()->asMinimalSimpleString();
  return result;
};

CL_LAMBDA(sin ch);
CL_DECLARE();
CL_DOCSTRING(R"dx(reader_backquoted_expression)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__reader_backquoted_expression(T_sp sin, Character_sp ch) {
  Fixnum_sp backquote_level = gc::As<Fixnum_sp>(_sym_STARbackquote_levelSTAR->symbolValue());
  Fixnum_sp new_backquote_level = make_fixnum(unbox_fixnum(backquote_level) + 1);
  // DynamicScopeManager will save the dynamic value of the symbol and restore it in dtor
  DynamicScopeManager scope(_sym_STARbackquote_levelSTAR, new_backquote_level);
  T_sp quoted_object = cl__read(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
  Cons_sp result = Cons_O::createList(_sym_quasiquote, quoted_object);
  return (Values(result));
};

CL_LAMBDA(sin ch);
CL_DECLARE();
CL_DOCSTRING(R"dx(Error signaler for when a comma (or splice) is outside a backquote.)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__reader_error_backquote_context(T_sp sin) {
  FileScope_sp info = gc::As<FileScope_sp>(core__file_scope(sin));
  // FIXME: Use a real condition class.
  // SIMPLE_ERROR("Comma outside of backquote in file: {} line: {}", info->fileName() , stream_input_line(sin));
  string fn = info->fileName();
  if (fn.compare("-no-name-") == 0) {
    READER_ERROR(SimpleBaseString_O::make("Comma outside of backquote in stream at line: ~a column ~a."),
                 Cons_O::createList(Cons_O::create(make_fixnum(stream_input_line_as_uint(sin)), nil<T_O>()),
                                    Cons_O::create(make_fixnum(stream_input_column_as_uint(sin)), nil<T_O>())),
                 sin);
  } else {
    READER_ERROR(SimpleBaseString_O::make("Comma outside of backquote in file: ~a line: ~a column ~a."),
                 Cons_O::createList(SimpleBaseString_O::make(fn),
                                    Cons_O::create(make_fixnum(stream_input_line_as_uint(sin)), nil<T_O>()),
                                    Cons_O::create(make_fixnum(stream_input_column_as_uint(sin)), nil<T_O>())),
                 sin);
  };
  return (Values(nil<T_O>()));
};

CL_LAMBDA(sin ch);
CL_DECLARE();
CL_DOCSTRING(R"dx(reader_comma_form)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__reader_comma_form(T_sp sin, Character_sp ch) {
  Fixnum_sp backquote_level = gc::As<Fixnum_sp>(_sym_STARbackquote_levelSTAR->symbolValue());
  if (backquote_level.unsafe_fixnum() == 0)
    core__reader_error_backquote_context(sin);
  Fixnum_sp new_backquote_level = make_fixnum(unbox_fixnum(backquote_level) - 1);
  DynamicScopeManager scope(_sym_STARbackquote_levelSTAR, new_backquote_level);
  char nextc = stream_peek_char(sin);
  //	ql::source_code_list list(sin->lineNumber(),sin->column(),core__file_scope(sin));
  ql::list list;
  Symbol_sp head = _sym_unquote;
  if (nextc == '@') {
    head = _sym_unquote_splice;
    gc::As<Character_sp>(cl__read_char(sin, _lisp->_true(), nil<T_O>(), _lisp->_true()));
  } else if (nextc == '.') {
    head = _sym_unquote_nsplice;
    gc::As<Character_sp>(cl__read_char(sin, _lisp->_true(), nil<T_O>(), _lisp->_true()));
  }
  T_sp comma_object = cl__read(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
  list << head << comma_object;
  return (list.cons());
};

CL_PRIORITY(1);
CL_DECLARE();
CL_DOCSTRING(R"dx(reader_list_allow_consing_dot)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__reader_list_allow_consing_dot(T_sp sin, Character_sp ch) {
  List_sp list = read_list(sin, ')', true);
  return list;
};

CL_LAMBDA(sin ch);
CL_DECLARE();
CL_DOCSTRING(R"dx(reader_error_unmatched_close_parenthesis)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__reader_error_unmatched_close_parenthesis(T_sp sin, Character_sp ch) {
  FileScope_sp info = gc::As<FileScope_sp>(core__file_scope(sin));
  string fn = info->fileName();
  if (fn.compare("-no-name-") == 0) {
    READER_ERROR(SimpleBaseString_O::make("Unmatched close parenthesis in stream at line: ~a column ~a."),
                 Cons_O::createList(Cons_O::create(make_fixnum(stream_input_line_as_uint(sin)), nil<T_O>()),
                                    Cons_O::create(make_fixnum(stream_input_column_as_uint(sin)), nil<T_O>())),
                 sin);
  } else {
    READER_ERROR(SimpleBaseString_O::make("Unmatched close parenthesis in file ~a line: ~a column ~a."),
                 Cons_O::createList(SimpleBaseString_O::make(fn),
                                    Cons_O::create(make_fixnum(stream_input_line_as_uint(sin)), nil<T_O>()),
                                    Cons_O::create(make_fixnum(stream_input_column_as_uint(sin)), nil<T_O>())),
                 sin);
  };
  return (Values(nil<T_O>()));
};

CL_LAMBDA(sin ch);
CL_DECLARE();
CL_DOCSTRING(R"dx(reader_quote)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__reader_quote(T_sp sin, Character_sp ch) {
  //	ql::source_code_list result(sin->lineNumber(),sin->column(),core__file_scope(sin));
  ql::list acc;
  T_sp quoted_object = cl__read(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
  acc << cl::_sym_quote << quoted_object;
  T_sp result = acc.cons();
  return result;
}

CL_LAMBDA(sin ch);
CL_DECLARE();
CL_DOCSTRING(R"dx(reader_skip_semicolon_comment)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__reader_skip_semicolon_comment(T_sp sin, Character_sp ch) {
  ASSERT(stream_input_p(sin));
  stringstream str;
  bool done = false;
  while (!done) {
    T_sp tc = cl__read_char(sin, nil<core::T_O>(), _sym_eof_value, _lisp->_true());
    if (tc == _sym_eof_value)
      break;
    ASSERT(tc.characterp());
    Character_sp nc = gc::As<Character_sp>(tc);
    claspCharacter cc = clasp_as_claspCharacter(nc);
    if (cc == '\n')
      break;
  }
  // Return one value in a MultipleValues object to indicate that something is being returned
  return (Values0<T_O>());
};

CL_LAMBDA(sin ch);
CL_DECLARE();
CL_DOCSTRING(R"dx(dispatch_macro_character)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__dispatch_macro_character(T_sp sin, Character_sp ch) {
  char cpeek = stream_peek_char(sin);
  bool sawnumarg = false;
  uint numarg = 0;
  while (isdigit(cpeek)) {
    sawnumarg = true;
    int cget = stream_read_char(sin);
    if (cget == EOF) {
      SIMPLE_ERROR("Hit eof in sharp macro");
    }
    numarg *= 10;
    numarg += (cget - '0');
    cpeek = stream_peek_char(sin);
  }
  T_sp onumarg(nil<T_O>());
  if (sawnumarg)
    onumarg = make_fixnum(numarg);
  Character_sp subchar = gc::As<Character_sp>(cl__read_char(sin, _lisp->_true(), nil<T_O>(), _lisp->_true()));
  T_sp macro_func = cl__get_dispatch_macro_character(ch, subchar, _lisp->getCurrentReadTable());
  if (macro_func.nilp()) {
    // SIMPLE_ERROR("Undefined reader macro for {} {}", _rep_(ch) , _rep_(subchar));
    //  Need to be a reader error
    FileScope_sp info = gc::As<FileScope_sp>(core__file_scope(sin));
    string fn = info->fileName();
    if (fn.compare("-no-name-") == 0) {
      READER_ERROR(SimpleBaseString_O::make("Undefined reader macro for char '~a' subchar '~a' in stream at line: ~a column ~a."),
                   Cons_O::createList(ch, subchar, Cons_O::create(make_fixnum(stream_input_line_as_uint(sin)), nil<T_O>()),
                                      Cons_O::create(make_fixnum(stream_input_column_as_uint(sin)), nil<T_O>())),
                   sin);
    } else {
      READER_ERROR(SimpleBaseString_O::make("Undefined reader macro for char '~a' subchar '~a' in file ~a line: ~a column ~a."),
                   Cons_O::createList(ch, subchar, SimpleBaseString_O::make(fn),
                                      Cons_O::create(make_fixnum(stream_input_line_as_uint(sin)), nil<T_O>()),
                                      Cons_O::create(make_fixnum(stream_input_column_as_uint(sin)), nil<T_O>())),
                   sin);
    };
  }
  return eval::funcall(macro_func, sin, subchar, onumarg);
};

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING(R"dx(sharp_backslash)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sharp_backslash(T_sp sin, Character_sp ch, T_sp num) {
  SafeBufferStr8Ns sslexemes;
  List_sp lexemes = collect_lexemes(ch, sin);
  make_str_preserve_case(sslexemes.string(), lexemes);
  if (!cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
    if (sslexemes.string()->length() == 1) {
      return Values(sslexemes.string()->rowMajorAref(0));
    } else {
      T_sp tch = eval::funcall(cl::_sym_name_char, sslexemes.string());
      if (tch.nilp())
        SIMPLE_ERROR("Unknown character name for [{}]", _rep_(sslexemes.string()));
      return Values(gc::As<Character_sp>(tch));
    }
  }
  return Values(nil<T_O>()); //(Values0<T_O>());
}

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING(R"dx(sharp_dot)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sharp_dot(T_sp sin, Character_sp ch, T_sp num) {
  T_sp object = cl__read(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
  if (!cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
    if (!cl::_sym_STARread_evalSTAR->symbolValue().isTrue()) {
      READER_ERROR(SimpleBaseString_O::make("Cannot evaluate the form #.~S"), Cons_O::create(object, nil<T_O>()), sin);
    }
    return cl__eval(object);
  }
  return nil<T_O>();
}

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING(R"dx(sharp_single_quote)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__sharp_single_quote(T_sp sin, Character_sp ch, T_sp num) {
  T_sp quoted_object = cl__read(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
  //	ql::source_code_list result(sin->lineNumber(),sin->column(),core__file_scope(sin));
  ql::list result;
  result << cl::_sym_function << quoted_object;
  T_sp tresult = result.cons();
  return tresult;
};

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING(R"dx(sharp_left_parenthesis)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sharp_left_parenthesis(T_sp sin, Character_sp ch, /*Fixnum_sp*/ T_sp tnum) {
  Character_sp right_paren = clasp_make_character(')');
  T_sp olist = cl__read_delimited_list(right_paren, sin, _lisp->_true());
  List_sp list = olist;
  if (!cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
    int list_length = cl__length(list);
    if (tnum.notnilp()) {
      Fixnum_sp num = gc::As<Fixnum_sp>(tnum);
      if (list_length > unbox_fixnum(num))
        SIMPLE_ERROR("vector is longer than specified length {}: {}", unbox_fixnum(num), _rep_(list));
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
    SimpleVector_sp vec = SimpleVector_O::make(cl__length(list), nil<T_O>());
    vec->fillInitialContents(list);
    return Values(vec);
  }
  return (Values(nil<T_O>()));
};

SYMBOL_EXPORT_SC_(CorePkg, STARforeign_data_reader_callbackSTAR);

CL_LAMBDA(stream ch num);
CL_DECLARE();
DOCGROUP(clasp);
CL_DEFUN T_mv core__sharp_foreign_data_reader(T_sp tsin, Character_sp ch, /*Fixnum_sp*/ T_sp tnum) {
  // ignore *read-suppress*
  T_sp loader = gc::As<T_sp>(_sym_STARforeign_data_reader_callbackSTAR->symbolValue());
  StringInputStream_sp sin = gc::As<StringInputStream_sp>(tsin);
  if (tnum.notnilp() && tnum.fixnump()) {
    size_t list_length = tnum.unsafe_fixnum();
    for (size_t i = 0; i < list_length; ++i) {
      //      printf("%s:%d reader: |%s|\n", __FILE__, __LINE__, sin->peer(50).c_str());
      T_sp object = cl__read(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
      eval::funcall(loader, sin, core::make_fixnum(i), object);
    }
    return Values(nil<T_O>());
  }
  SIMPLE_ERROR("Error in #x{{ x is {}", _rep_(tnum));
};

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING(R"dx(sharp_asterisk)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sharp_asterisk(T_sp sin, Character_sp ch, T_sp num) {
  int dimcount, dim = 0;
  stringstream pattern;
  T_sp rtbl = _lisp->getCurrentReadTable();
  if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
    cl__read(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
    return Values(nil<T_O>());
  }
  for (dimcount = 0;; dimcount++) {
    T_sp tch = cl__read_char(sin, nil<T_O>(), nil<T_O>(), _lisp->_true());
    if (tch.nilp())
      break;
    ch = gc::As<Character_sp>(tch);
    Symbol_sp syntaxType = core__syntax_type(rtbl, ch);
    if (syntaxType == kw::_sym_terminating_macro || syntaxType == kw::_sym_whitespace) {
      unread_ch(sin, ch);
      break;
    }
    unlikely_if(syntaxType == kw::_sym_single_escape || syntaxType == kw::_sym_multiple_escape ||
                (clasp_as_claspCharacter(ch) != '0' && clasp_as_claspCharacter(ch) != '1')) {
      READER_ERROR(SimpleBaseString_O::make("Character ~:C is not allowed after #*"), Cons_O::create(ch, nil<T_O>()), sin);
    }
    pattern << (char)(clasp_as_claspCharacter(ch));
  }
  if (num.nilp()) {
    dim = dimcount;
  } else if (gc::IsA<Fixnum_sp>(num)) {
    dim = unbox_fixnum(gc::As<Fixnum_sp>(num));
    unlikely_if(dim < 0 || (dim > CLASP_ARRAY_DIMENSION_LIMIT)) {
      READER_ERROR(SimpleBaseString_O::make("Wrong vector dimension size ~D in #*."), Cons_O::create(num, nil<T_O>()), sin);
    }
    unlikely_if(dimcount > dim) READER_ERROR(SimpleBaseString_O::make("Too many elements in #*."), nil<T_O>(), sin);
    unlikely_if(dim && (dimcount == 0)) READER_ERROR(SimpleBaseString_O::make("Cannot fill the bit-vector #*."), nil<T_O>(), sin);
  }
  string bitPattern = pattern.str();
  char last = bitPattern.size() > 0 ? bitPattern[bitPattern.size() - 1] : '0';
  SimpleBitVector_sp x = SimpleBitVector_O::make(dim);
  for (int i = 0; i < dim; i++) {
    char elt = (i < dimcount) ? bitPattern[i] : last;
    x->setBit(i, elt - '0');
  }
  return Values(x);
};

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING(R"dx(sharp_colon)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sharp_colon(T_sp sin, Character_sp ch, T_sp num) {
  // CHECKME
  List_sp lexemes = collect_lexemes(ch, sin);
  SafeBufferStrWNs sslexemes;
  make_str(sslexemes.string(), lexemes);
  SimpleString_sp lexeme_str = sslexemes.string()->asMinimalSimpleString();
  if (!cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
    Symbol_sp new_symbol = Symbol_O::create(gc::As<SimpleString_sp>(lexeme_str->unsafe_subseq(1, lexeme_str->length())));
    return Values(new_symbol);
  }
  return Values(nil<T_O>());
}; // core__sharp_colon

CL_LAMBDA(stream subchar radix);
CL_DECLARE();
CL_DOCSTRING(R"dx(sharp_r)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sharp_r(T_sp sin, Character_sp ch, T_sp nradix) {
  if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
    T_sp object = cl__read(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
    (void)object; // suppress warning
    return (Values(nil<T_O>()));
  } else if (nradix.nilp()) {
    SIMPLE_ERROR("Radix missing in #R reader macro");
  } else {
    Fixnum_sp radix = gc::As<Fixnum_sp>(nradix);
    int iradix = unbox_fixnum(radix);
    if (iradix < 2 || iradix > 36) {
      SIMPLE_ERROR("Illegal radix for #R: {}", iradix);
    }
    {
      Fixnum_sp oradix = make_fixnum(iradix);
      DynamicScopeManager scope(cl::_sym_STARread_baseSTAR, oradix);
      T_sp val = cl__read(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
      if (!gc::IsA<Rational_sp>(val)) {
        SIMPLE_ERROR("#{} (base {}) is not a rational: {}", _rep_(ch), iradix, _rep_(val));
      }
      return (Values(val));
    }
  }
}

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING(R"dx(sharp_b)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sharp_b(T_sp sin, Character_sp ch, T_sp num) { return core__sharp_r(sin, ch, make_fixnum(2)); };

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING(R"dx(sharp_o)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sharp_o(T_sp sin, Character_sp ch, T_sp num) { return core__sharp_r(sin, ch, make_fixnum(8)); };

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING(R"dx(sharp_x)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sharp_x(T_sp sin, Character_sp ch, T_sp num) { return core__sharp_r(sin, ch, make_fixnum(16)); };

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING(R"dx(sharp_c)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sharp_c(T_sp sin, Character_sp ch, T_sp num) {
  T_sp olist = cl__read(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
  List_sp list = olist;
  if (!cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
    int list_length = cl__length(list);
    if (list_length != 2) {
      SIMPLE_ERROR("#C complex number needs two numbers");
    }
    Real_sp r = gc::As<Real_sp>(oCar(list));
    Real_sp i = gc::As<Real_sp>(oCadr(list));
    return (Values(clasp_make_complex(r, i)));
  }
  return (Values(nil<T_O>()));

}; // core__sharp_c

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING(R"dx(sharp_a)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sharp_a(T_sp sin, Character_sp ch, T_sp num) {
  if (core::_sym_sharp_a_reader->fboundp())
    return eval::funcall(core::_sym_sharp_a_reader, sin, ch, num);
  if (num.nilp()) {
    T_sp initial_contents = core::eval::funcall(cl::_sym_read, sin, nil<T_O>(), _lisp->_true());
    if (cl::_sym_STARread_suppressSTAR->symbolValue().notnilp()) {
      return Values(nil<T_O>());
    } else if (num.nilp()) {
      // readably-pretty-printed array: #A(type dims initial-contents)
      T_sp elt_type = oCar(initial_contents);
      T_sp dims = gc::As<T_sp>(oCadr(initial_contents));
      // is either a number or a list of n elements
      List_sp linitial_contents = gc::As<List_sp>(oCaddr(initial_contents));
      if (!dims.consp() || cl__length(dims) == 1) {
        if (dims.consp())
          dims = oCar(dims);
        Vector_sp vec = core__make_vector(elt_type, dims.unsafe_fixnum(), false, nil<T_O>(), nil<T_O>(), core::make_fixnum(0),
                                          nil<T_O>(), true);
        size_t idx = 0;
        for (auto e : linitial_contents) {
          T_sp val = CONS_CAR(e);
          vec->rowMajorAset(idx, val);
          idx++;
        }
        return Values(vec);
      }
      SIMPLE_ERROR("Add support for multidimensional #a");
    }
  }
  SIMPLE_ERROR("Add support for #a with argument");
}

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING(R"dx(sharp_s)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sharp_s(T_sp sin, Character_sp ch, T_sp num) {
  if (core::_sym_sharp_s_reader->fboundp())
    return eval::funcall(core::_sym_sharp_s_reader, sin, ch, num);
  IMPLEMENT_MEF("Implement sharp_s");
}; // core__sharp_s

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING(R"dx(sharp_p)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sharp_p(T_sp sin, Character_sp ch, T_sp num) {
  bool suppress = cl::_sym_STARread_suppressSTAR->symbolValue().isTrue();
  if (num.notnilp() && !suppress)
    extra_argument('P', sin, num);
  T_sp d = cl__read(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
  if (suppress) {
    d = nil<T_O>();
  } else {
    d = cl__parse_namestring(d);
  }
  return Values(d);
}; // core__sharp_p

CL_LAMBDA(feature-test);
CL_DECLARE();
CL_DOCSTRING(R"dx(feature_p takes one argument - a feature test)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__reader_feature_p(T_sp feature_test) {
  if (feature_test.nilp())
    return nil<T_O>();
  else if (cl__atom(feature_test)) {
    List_sp features_list = cl::_sym_STARfeaturesSTAR->symbolValue();
    if (features_list.nilp())
      return nil<T_O>();
    return features_list.asCons()->member(gc::As<Symbol_sp>(feature_test), nil<T_O>(), nil<T_O>(), nil<T_O>());
  } else {
    ASSERT(cl__listp(feature_test));
    List_sp features_cons = feature_test;
    T_sp features_head = oCar(features_cons);
    if (features_head == kw::_sym_not) {
      return _lisp->_not(eval::funcall(_sym_reader_feature_p, oSecond(features_cons)));
    } else if (features_head == kw::_sym_and) {
      return (eval::funcall(core::_sym_every_list, _sym_reader_feature_p, oCdr(features_cons)));
    } else if (features_head == kw::_sym_or) {
      List_sp or_features = oCdr(features_cons);
      if (or_features.consp()) {
        return (eval::funcall(core::_sym_some_list, _sym_reader_feature_p, oCdr(features_cons)));
      }
      // Trivial case of #+(or) returns nil.
      return nil<T_O>();
    }
    SIMPLE_ERROR("Illegal feature test: {}", _rep_(features_cons));
  }
}

/*! Read a feature test in the keyword package */
T_sp read_feature_test(T_sp sin) {
  // Read the feature test in the keyword package
  DynamicScopeManager dynamicScopeManager(cl::_sym_STARpackageSTAR, _lisp->keywordPackage());
  T_sp feature = cl__read(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
  return feature;
}

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING(R"dx(sharp_plus)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sharp_plus(T_sp sin, Character_sp ch, T_sp num) {
  T_sp feat = read_feature_test(sin);
  LOG("feature[{}]", _rep_(feat));
  if (T_sp(eval::funcall(_sym_reader_feature_p, feat)).isTrue()) {
    LOG("The feature test passed - reading lisp object");
    T_sp obj = cl__read(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
    LOG("Read the object[{}]", _rep_(obj));
    return Values(obj);
  } else {
    DynamicScopeManager dynScopeManager(cl::_sym_STARread_suppressSTAR, _lisp->_true());
    cl__read(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
    return Values0<T_O>();
  }
}; // core__sharp_plus

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING(R"dx(sharp_minus)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sharp_minus(T_sp sin, Character_sp ch, T_sp num) {
  T_sp feat = read_feature_test(sin);
  LOG("feature[{}]", _rep_(feat));
  if (!T_sp(eval::funcall(_sym_reader_feature_p, feat)).isTrue()) {
    LOG("The feature test passed - reading lisp object");
    T_sp obj = cl__read(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
    LOG("Read the object[{}]", _rep_(obj));
    return Values(obj);
  } else {
    LOG("The feature test failed - returning nil");
    DynamicScopeManager dynScopeManager(cl::_sym_STARread_suppressSTAR, _lisp->_true());
    cl__read(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
    return Values0<T_O>();
  }
}; // core__sharp_minus

CL_LAMBDA(stream ch num);
CL_DECLARE();
CL_DOCSTRING(R"dx(sharp_vertical_bar)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sharp_vertical_bar(T_sp sin, Character_sp ch, T_sp num) {
  ASSERT(stream_input_p(sin));
  bool done = false;
  while (!done) {
    Character_sp nc = gc::As<Character_sp>(cl__read_char(sin, _lisp->_true(), nil<T_O>(), _lisp->_true()));
    claspCharacter cc = clasp_as_claspCharacter(nc);
    if (cc == '#') {
      claspCharacter nextc = stream_peek_char(sin);
      if (nextc == '|') {
        Character_sp nextsubc = gc::As<Character_sp>(cl__read_char(sin, _lisp->_true(), nil<T_O>(), _lisp->_true()));
        eval::funcall(_sym_sharp_vertical_bar, sin, nextsubc, num);
      }
    } else if (cc == '|') {
      claspCharacter nextc = stream_peek_char(sin);
      if (nextc == '#') {
        Character_sp nextsubc = gc::As<Character_sp>(cl__read_char(sin, _lisp->_true(), nil<T_O>(), _lisp->_true()));
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
HashTable_sp Readtable_O::create_standard_syntax_table() {
  HashTable_sp syntax = HashTable_O::createEql();
  syntax->setf_gethash(clasp_character_create_from_name("TAB"), kw::_sym_whitespace);
  syntax->setf_gethash(clasp_character_create_from_name("NEWLINE"), kw::_sym_whitespace);
  syntax->setf_gethash(clasp_character_create_from_name("LINEFEED"), kw::_sym_whitespace);
  syntax->setf_gethash(clasp_character_create_from_name("ESCAPE"), kw::_sym_whitespace);
  syntax->setf_gethash(clasp_character_create_from_name("PAGE"), kw::_sym_whitespace);
  syntax->setf_gethash(clasp_character_create_from_name("RETURN"), kw::_sym_whitespace);
  syntax->setf_gethash(clasp_character_create_from_name("SPACE"), kw::_sym_whitespace);
  syntax->hash_table_setf_gethash(clasp_make_standard_character('\\'), kw::_sym_single_escape);
  syntax->hash_table_setf_gethash(clasp_make_standard_character('|'), kw::_sym_multiple_escape);
  return syntax;
}

Readtable_sp Readtable_O::create_standard_readtable() {
  auto rt = gctools::GC<Readtable_O>::allocate_with_default_constructor();
  rt->SyntaxTypes_ = Readtable_O::create_standard_syntax_table();
  ASSERTNOTNULL(_sym_reader_backquoted_expression->symbolFunction());
  //  ASSERT(_sym_reader_backquoted_expression->symbolFunction().notnilp());
  rt->set_macro_character_(clasp_make_standard_character('`'), _sym_reader_backquoted_expression, nil<T_O>());
  rt->set_macro_character_(clasp_make_standard_character(','), _sym_reader_comma_form, nil<T_O>());
  SYMBOL_SC_(CorePkg, read_list_allow_consing_dot);
  rt->set_macro_character_(clasp_make_standard_character('('), _sym_reader_list_allow_consing_dot, nil<T_O>());
  rt->set_macro_character_(clasp_make_standard_character(')'), _sym_reader_error_unmatched_close_parenthesis, nil<T_O>());
  rt->set_macro_character_(clasp_make_standard_character('\''), _sym_reader_quote, nil<T_O>());
  rt->set_macro_character_(clasp_make_standard_character(';'), _sym_reader_skip_semicolon_comment, nil<T_O>());
  SYMBOL_SC_(CorePkg, reader_read_double_quote_string);
  rt->set_macro_character_(clasp_make_standard_character('"'), _sym_reader_double_quote_string, nil<T_O>());
  Character_sp sharp = clasp_make_standard_character('#');
  rt->make_dispatch_macro_character_(sharp, _lisp->_true());
  ql::list dispatchers;
  dispatchers << clasp_make_standard_character('\\') << _sym_sharp_backslash << clasp_make_standard_character('\'')
              << _sym_sharp_single_quote << clasp_make_standard_character('(') << _sym_sharp_left_parenthesis
              << clasp_make_standard_character('f') << _sym_sharp_foreign_data_reader << clasp_make_standard_character('*')
              << _sym_sharp_asterisk << clasp_make_standard_character(':') << _sym_sharp_colon << clasp_make_standard_character('.')
              << _sym_sharp_dot << clasp_make_standard_character('b') << _sym_sharp_b << clasp_make_standard_character('o')
              << _sym_sharp_o << clasp_make_standard_character('x') << _sym_sharp_x << clasp_make_standard_character('r')
              << _sym_sharp_r << clasp_make_standard_character('c') << _sym_sharp_c << clasp_make_standard_character('a')
              << _sym_sharp_a << clasp_make_standard_character('s') << _sym_sharp_s << clasp_make_standard_character('p')
              << _sym_sharp_p
              //		    << clasp_make_standard_character('=') << _sym_sharp_equal
              //		    << clasp_make_standard_character('#') << _sym_sharp_sharp
              << clasp_make_standard_character('+') << _sym_sharp_plus << clasp_make_standard_character('-') << _sym_sharp_minus
              << clasp_make_standard_character('|') << _sym_sharp_vertical_bar;
  for (List_sp cur = dispatchers.cons(); cur.notnilp(); cur = oCdr(oCdr(cur))) {
    Character_sp ch = gc::As<Character_sp>(oCar(cur));
    Symbol_sp sym = gc::As<Symbol_sp>(oCadr(cur));
    rt->set_dispatch_macro_character_(sharp, ch, sym);
  }
  // reinstall the things defined in lisp
  if (core::_sym_sharpmacros_lisp_redefine->fboundp())
    eval::funcall(core::_sym_sharpmacros_lisp_redefine, rt);
  return rt;
}

SYMBOL_EXPORT_SC_(KeywordPkg, upcase);
SYMBOL_EXPORT_SC_(KeywordPkg, downcase);
SYMBOL_EXPORT_SC_(KeywordPkg, preserve);
SYMBOL_EXPORT_SC_(KeywordPkg, invert);
void Readtable_O::initialize() {

  this->Base::initialize();
  //	printf("%s:%d Initializing readtable\n", __FILE__, __LINE__ );
  this->Case_ = kw::_sym_upcase;
  this->SyntaxTypes_ = HashTable_O::createEql();
  this->MacroCharacters_ = HashTable_O::createEql();
  this->DispatchMacroCharacters_ = HashTable_O::createEql();
}

clasp_readtable_case Readtable_O::getReadtableCaseAsEnum_() {
  Symbol_sp ccase = this->Case_;
  if (ccase == kw::_sym_upcase) {
    return clasp_case_upcase;
  } else if (ccase == kw::_sym_downcase) {
    return clasp_case_downcase;
  } else if (ccase == kw::_sym_invert) {
    return clasp_case_invert;
  } else if (ccase == kw::_sym_preserve) {
    return clasp_case_preserve;
  }
  SIMPLE_ERROR("Unknown readtable case: {}", _rep_(this->Case_));
}

Symbol_sp Readtable_O::setf_readtable_case_(Symbol_sp newCase) {

  if ((newCase == kw::_sym_upcase) || (newCase == kw::_sym_downcase) || (newCase == kw::_sym_preserve) ||
      (newCase == kw::_sym_invert)) {
    this->Case_ = newCase;
    return newCase;
  } else {
    TYPE_ERROR(newCase,
               Cons_O::createList(cl::_sym_member, kw::_sym_upcase, kw::_sym_downcase, kw::_sym_preserve, kw::_sym_invert));
  }
}

T_sp Readtable_O::set_syntax_type_(Character_sp ch, T_sp syntaxType) {
  this->SyntaxTypes_->setf_gethash(ch, syntaxType);
  return _lisp->_true();
}

SYMBOL_EXPORT_SC_(KeywordPkg, macro_function);

T_sp Readtable_O::set_macro_character_(Character_sp ch, T_sp funcDesig, T_sp non_terminating_p) {
  if (!(gctools::IsA<core::Symbol_sp>(funcDesig) || gctools::IsA<core::Function_sp>(funcDesig))) {
    TYPE_ERROR(funcDesig, Cons_O::createList(cl::_sym_or, cl::_sym_symbol, cl::_sym_function));
  }
  if (non_terminating_p.isTrue()) {
    this->set_syntax_type_(ch, kw::_sym_non_terminating_macro);
  } else {
    this->set_syntax_type_(ch, kw::_sym_terminating_macro);
  }
  this->MacroCharacters_->setf_gethash(ch, funcDesig);
  return _lisp->_true();
}

string Readtable_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString();
  ss << " @" << this;
  ss << " :case " << _rep_(this->Case_);
  ss << "> ";
  return ss.str();
}

Symbol_sp Readtable_O::syntax_type_(Character_sp ch) const {

  Symbol_sp result = this->SyntaxTypes_->gethash(ch, kw::_sym_constituent);
  LOG("character[{}] syntax_type: {}", _rep_(ch), _rep_(result));
  return result;
}

T_mv Readtable_O::get_macro_character_(Character_sp ch) {

  T_sp dispatcher = this->MacroCharacters_->gethash(ch, nil<T_O>());
  Symbol_sp syntaxType = this->syntax_type_(ch);
  if (syntaxType == kw::_sym_terminating_macro) {
    return (Values(dispatcher, nil<T_O>()));
  } else if (syntaxType == kw::_sym_non_terminating_macro) {
    return (Values(dispatcher, _lisp->_true()));
  }
  return (Values(nil<T_O>(), nil<T_O>()));
}

T_sp Readtable_O::make_dispatch_macro_character_(Character_sp ch, T_sp non_terminating_p) {

  this->set_macro_character_(ch, _sym_dispatch_macro_character, non_terminating_p);
  this->DispatchMacroCharacters_->setf_gethash(ch, HashTable_O::createEql());
  return _lisp->_true();
}

T_sp Readtable_O::set_dispatch_macro_character_(Character_sp disp_char, Character_sp sub_char, T_sp new_func_desig) {
  T_sp tdispatch_table = this->DispatchMacroCharacters_->gethash(disp_char);
  if (!gc::IsA<HashTable_sp>(tdispatch_table)) {
    SIMPLE_ERROR("{} is not a dispatching macro character", _rep_(disp_char));
  }
  HashTable_sp dispatch_table = gc::As_unsafe<HashTable_sp>(tdispatch_table);
  ASSERTF(dispatch_table.notnilp(), "The dispatch table for the character[{}] is nil! - this shouldn't happen", _rep_(disp_char));
  Character_sp upcase_sub_char = clasp_make_character(char_upcase(sub_char.unsafe_character()));
  if (!(gctools::IsA<core::Symbol_sp>(new_func_desig) || gctools::IsA<core::Function_sp>(new_func_desig))) {
    TYPE_ERROR(new_func_desig, Cons_O::createList(cl::_sym_or, cl::_sym_symbol, cl::_sym_function));
  }
  //  Function_sp new_func = coerce::functionDesignator(new_func_desig);
  dispatch_table->hash_table_setf_gethash(upcase_sub_char, new_func_desig);
  return _lisp->_true();
}

T_sp Readtable_O::get_dispatch_macro_character_(Character_sp disp_char, Character_sp sub_char) {
  T_sp tdispatch_table = this->DispatchMacroCharacters_->gethash(disp_char);
  if (!gc::IsA<HashTable_sp>(tdispatch_table)) {
    SIMPLE_ERROR("{} is not a dispatching macro character", _rep_(disp_char));
  }
  HashTable_sp dispatch_table = gc::As_unsafe<HashTable_sp>(tdispatch_table);
  Character_sp upcase_sub_char = clasp_make_character(char_upcase(sub_char.unsafe_character()));
  T_sp func = dispatch_table->gethash(upcase_sub_char, nil<T_O>());
  return func;
}

Character_sp Readtable_O::convert_case_(Character_sp cc) {

  if (this->Case_ == kw::_sym_upcase) {
    return clasp_make_character(char_upcase(cc.unsafe_character()));
  } else if (this->Case_ == kw::_sym_downcase) {
    return clasp_make_character(char_downcase(cc.unsafe_character()));
  } else if (this->Case_ == kw::_sym_preserve) {
    return cc;
  } else if (this->Case_ == kw::_sym_invert) {
    SIMPLE_ERROR("I can't handle invert yet, that has to be handled when the token is converted");
  }
  SIMPLE_ERROR("Bad readtable case[{}]", _rep_(this->Case_));
}

Readtable_sp Readtable_O::copyReadtable_(gc::Nilable<Readtable_sp> tdest) {
  //	printf("%s:%d copy-readtable\n", __FILE__, __LINE__ );
  if (tdest.nilp()) {
    //	    printf("%s:%d allocating copy-readtable\n", __FILE__, __LINE__ );
    auto temp = gctools::GC<Readtable_O>::allocate_with_default_constructor();
    tdest = temp;
  }
  Readtable_sp dest = gc::As<Readtable_sp>(tdest);
  //	printf("%s:%d dest.nilp() == %d\n", __FILE__, __LINE__, dest.nilp());
  //	printf("%s:%d dest->SyntaxTypes_.nilp() == %d\n", __FILE__, __LINE__, dest->SyntaxTypes_.nilp());
  //	printf("%s:%d about to SyntaxTypes_->clrhash() copy-readtable\n", __FILE__, __LINE__ );
  dest->SyntaxTypes_->clrhash();
  //	printf("%s:%d about to MacroCharacters_->clrhash() copy-readtable\n", __FILE__, __LINE__ );
  dest->MacroCharacters_->clrhash();
  dest->DispatchMacroCharacters_->clrhash();
  this->SyntaxTypes_->maphash([&dest](T_sp key, T_sp val) { dest->SyntaxTypes_->setf_gethash(key, val); });
  this->MacroCharacters_->maphash([&dest](T_sp key, T_sp val) { dest->MacroCharacters_->setf_gethash(key, val); });
  this->DispatchMacroCharacters_->maphash([&dest](T_sp key, T_sp val) {
    HashTable_sp entry = gc::As<HashTable_sp>(val);
    HashTable_sp table = HashTable_O::createEql();
    entry->maphash([&table](T_sp subkey, T_sp func) { table->setf_gethash(subkey, func); });
    dest->DispatchMacroCharacters_->setf_gethash(key, table);
  });
  dest->Case_ = this->Case_;
  return dest;
}

SYMBOL_EXPORT_SC_(ClPkg, setMacroCharacter);
SYMBOL_EXPORT_SC_(CorePkg, reader_backquoted_expression);
SYMBOL_EXPORT_SC_(CorePkg, sharp_backslash);
SYMBOL_EXPORT_SC_(CorePkg, sharp_single_quote);
SYMBOL_EXPORT_SC_(CorePkg, sharp_left_parenthesis);
SYMBOL_EXPORT_SC_(CorePkg, sharp_foreign_data_reader);
SYMBOL_EXPORT_SC_(CorePkg, sharp_asterisk);
SYMBOL_EXPORT_SC_(CorePkg, sharp_colon);
SYMBOL_EXPORT_SC_(CorePkg, sharp_dot);
SYMBOL_EXPORT_SC_(CorePkg, sharp_b);
SYMBOL_EXPORT_SC_(CorePkg, sharp_o);
SYMBOL_EXPORT_SC_(CorePkg, sharp_x);
SYMBOL_EXPORT_SC_(CorePkg, sharp_r);
SYMBOL_EXPORT_SC_(CorePkg, sharp_c);
SYMBOL_EXPORT_SC_(CorePkg, sharp_a);
SYMBOL_EXPORT_SC_(CorePkg, sharp_s);
SYMBOL_EXPORT_SC_(CorePkg, sharp_p);
SYMBOL_EXPORT_SC_(CorePkg, sharp_plus);
SYMBOL_EXPORT_SC_(CorePkg, sharp_minus);
SYMBOL_EXPORT_SC_(CorePkg, sharp_vertical_bar);
SYMBOL_EXPORT_SC_(CorePkg, dispatch_macro_character);
SYMBOL_EXPORT_SC_(CorePkg, reader_double_quote_string);
SYMBOL_EXPORT_SC_(CorePkg, reader_comma_form);
SYMBOL_EXPORT_SC_(CorePkg, reader_list_allow_consing_dot);
SYMBOL_EXPORT_SC_(CorePkg, reader_error_unmatched_close_parenthesis);
SYMBOL_EXPORT_SC_(CorePkg, reader_quote);
SYMBOL_EXPORT_SC_(CorePkg, reader_skip_semicolon_comment);
SYMBOL_EXPORT_SC_(CorePkg, reader_feature_p);
SYMBOL_EXPORT_SC_(ClPkg, setDispatchMacroCharacter);
SYMBOL_EXPORT_SC_(ClPkg, getDispatchMacroCharacter);

}; // namespace core
