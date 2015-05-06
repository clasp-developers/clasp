/*
    File: character.cc
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

#pragma clang diagnostic push
//#pragma clang diagnostic ignored "-Wunused-local-typedef"
#include <boost/algorithm/string.hpp>
#pragma clang diagnostic pop
#include <clasp/core/common.h>
#include <clasp/core/str.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/environment.h>
#include <clasp/core/designators.h>
#include <clasp/core/character.h>
#include <clasp/core/wrappers.h>

namespace core {

claspCharacter
clasp_char_upcase(claspCharacter code) {
  return toupper(code);
}

claspCharacter
clasp_char_downcase(claspCharacter code) {
  return tolower(code);
}

bool clasp_alphanumericp(claspCharacter i) {
  return isalnum(i);
}

bool clasp_invalid_character_p(int c) {
  return (c <= 32) || (c == 127);
}

int clasp_string_case(Str_sp s) {
  int upcase = 0;
  for (Str_O::iterator it = s->begin(); it != s->end(); ++it) {
    if (isupper(*it)) {
      if (upcase < 0)
        return 0;
      upcase = +1;
    } else if (islower(*it)) {
      if (upcase > 0)
        return 0;
      upcase = -1;
    }
  }
  return upcase;
}

/*! Return -1 if a<b
      0 if a == b
      +1 if a > b
    */
int char_basic_compare(int na, int nb) {
  _G();
  if (na < nb)
    return -1;
  if (na == nb)
    return 0;
  return 1;
}

T_sp monotonic(int s, int t, Cons_sp args, bool preserve_case = true) {
  _G();
  char c = oCar(args).as<Character_O>()->get();
  if (!preserve_case)
    c = toupper(c);
  char d;
  int dir;
  args = cCdr(args);
  while (args.notnilp()) {
    d = oCar(args).as<Character_O>()->get();
    if (!preserve_case)
      d = toupper(d);
    dir = s * char_basic_compare(c, d);
    if (dir < t)
      return _Nil<T_O>();
    c = d;
    args = cCdr(args);
  }
  return _lisp->_true();
};

#define ARGS_af_charUpcase "(char)"
#define DECL_af_charUpcase ""
#define DOCS_af_charUpcase "charUpcase"
char af_charUpcase(Character_sp ch) {
  _G();
  return toupper(ch->get());
};

#define ARGS_af_charDowncase "(char)"
#define DECL_af_charDowncase ""
#define DOCS_af_charDowncase "charDowncase"
char af_charDowncase(Character_sp ch) {
  _G();
  return tolower(ch->get());
};

#define DOCS_af_char_LT_ "Return true if characters are monotonically increasing"
#define LOCK_af_char_LT_ 1
#define ARGS_af_char_LT_ "(&rest args)"
#define DECL_af_char_LT_ ""
T_mv af_char_LT_(Cons_sp args) {
  _G();
  return (Values(monotonic(-1, 1, args)));
};

#define DOCS_af_char_GT_ "Return true if characters are monotonically decreasing"
#define LOCK_af_char_GT_ 1
#define ARGS_af_char_GT_ "(&rest args)"
#define DECL_af_char_GT_ ""
T_mv af_char_GT_(Cons_sp args) {
  _G();
  return (Values(monotonic(1, 1, args)));
};

#define DOCS_af_char_LE_ "Return true if characters are monotonically non-decreasing"
#define LOCK_af_char_LE_ 1
#define ARGS_af_char_LE_ "(&rest args)"
#define DECL_af_char_LE_ ""
T_mv af_char_LE_(Cons_sp args) {
  _G();
  return (Values(monotonic(-1, 0, args)));
};

#define DOCS_af_char_GE_ "Return true if characters are monotonically non-increasing"
#define LOCK_af_char_GE_ 1
#define ARGS_af_char_GE_ "(&rest args)"
#define DECL_af_char_GE_ ""
T_mv af_char_GE_(Cons_sp args) {
  _G();
  return (Values(monotonic(1, 0, args)));
};

#define DOCS_af_charLessp "Return true if characters are monotonically increasing, ignore case"
#define LOCK_af_charLessp 1
#define ARGS_af_charLessp "(&rest args)"
#define DECL_af_charLessp ""
T_mv af_charLessp(Cons_sp args) {
  _G();
  return (Values(monotonic(-1, 1, args, false)));
};

#define DOCS_af_charGreaterp "Return true if characters are monotonically decreasing, ignore case"
#define LOCK_af_charGreaterp 1
#define ARGS_af_charGreaterp "(&rest args)"
#define DECL_af_charGreaterp ""
T_mv af_charGreaterp(Cons_sp args) {
  _G();
  return (Values(monotonic(1, 1, args, false)));
};

#define DOCS_af_charNotGreaterp "Return true if characters are monotonically non-increasing, ignore case"
#define LOCK_af_charNotGreaterp 1
#define ARGS_af_charNotGreaterp "(&rest args)"
#define DECL_af_charNotGreaterp ""
T_mv af_charNotGreaterp(Cons_sp args) {
  _G();
  return (Values(monotonic(-1, 0, args, false)));
};

#define DOCS_af_charNotLessp "Return true if characters are monotonically non-decreasing, ignore case"
#define LOCK_af_charNotLessp 1
#define ARGS_af_charNotLessp "(&rest args)"
#define DECL_af_charNotLessp ""
T_mv af_charNotLessp(Cons_sp args) {
  _G();
  return (Values(monotonic(1, 0, args, false)));
};

#define DOCS_af_char_NE_ "NE_"
#define LOCK_af_char_NE_ 1
#define ARGS_af_char_NE_ "(&rest args)"
#define DECL_af_char_NE_ ""
T_mv af_char_NE_(Cons_sp args) {
  _G();
  while (args.notnilp()) {
    int a = oCar(args).as<Character_O>()->get();
    for (Cons_sp cur = cCdr(args); cur.notnilp(); cur = cCdr(cur)) {
      int b = oCar(cur).as<Character_O>()->get();
      if (a == b)
        return (Values(_Nil<T_O>()));
    }
    args = cCdr(args);
  }
  return (Values(_lisp->_true()));
}

#define DOCS_af_char_EQ_ "EQ_"
#define LOCK_af_char_EQ_ 1
#define ARGS_af_char_EQ_ "(&rest args)"
#define DECL_af_char_EQ_ ""
T_mv af_char_EQ_(Cons_sp args) {
  _G();
  if (args.nilp())
    return (Values(_lisp->_true()));
  int a = oCar(args).as<Character_O>()->get();
  args = cCdr(args);
  while (args.notnilp()) {
    int b = oCar(args).as<Character_O>()->get();
    if (a != b)
      return (Values(_Nil<T_O>()));
    args = cCdr(args);
  }
  return (Values(_lisp->_true()));
};

#define DOCS_af_charNotEqual "Like char_NE_ but ignore case"
#define LOCK_af_charNotEqual 1
#define ARGS_af_charNotEqual "(&rest args)"
#define DECL_af_charNotEqual ""
T_mv af_charNotEqual(Cons_sp args) {
  _G();
  while (args.notnilp()) {
    int a = oCar(args).as<Character_O>()->get();
    a = toupper(a);
    for (Cons_sp cur = cCdr(args); cur.notnilp(); cur = cCdr(cur)) {
      int b = oCar(cur).as<Character_O>()->get();
      b = toupper(b);
      if (a == b)
        return (Values(_Nil<T_O>()));
    }
    args = cCdr(args);
  }
  return (Values(_lisp->_true()));
}

#define DOCS_af_charEqual "Like char_EQ_, ignore case"
#define LOCK_af_charEqual 1
#define ARGS_af_charEqual "(&rest args)"
#define DECL_af_charEqual ""
bool af_charEqual(Cons_sp args) {
  _G();
  if (args.nilp())
    return true;
  int a = oCar(args).as<Character_O>()->get();
  a = toupper(a);
  args = cCdr(args);
  while (args.notnilp()) {
    int b = oCar(args).as<Character_O>()->get();
    b = toupper(b);
    if (a != b)
      return false;
    args = cCdr(args);
  }
  return true;
};
};
namespace core {

CharacterInfo::CharacterInfo() {
  this->gCharacterNames.resize(256, _Nil<Str_O>());
  this->gIndexedCharacters.resize(256, _Nil<Character_O>());
#define ADD_CHAR(name, char_index)                               \
  {                                                              \
    string upcase_name = stringUpper(name);                      \
    int fci = char_index;                                        \
    gNamesToCharacterIndex[upcase_name] = fci;                   \
    gIndexedCharacters[fci] = StandardChar_O::create((char)fci); \
    gCharacterNames[fci] = Str_O::create(upcase_name);           \
  }
  int ci = 0;
  ADD_CHAR("Nul", ci++);
  ADD_CHAR("Soh", ci++);       //1
  ADD_CHAR("Stx", ci++);       //2
  ADD_CHAR("Etx", ci++);       //3
  ADD_CHAR("Eot", ci++);       //4
  ADD_CHAR("Enq", ci++);       //5
  ADD_CHAR("Ack", ci++);       //6
  ADD_CHAR("Bel", ci++);       //7
  ADD_CHAR("Backspace", ci++); //8
  ADD_CHAR("Tab", ci++);       //9
  ADD_CHAR("Newline", ci++);   //10
  ADD_CHAR("Linefeed", 10);    //10
  ADD_CHAR("Vt", ci++);        //11
  ADD_CHAR("Page", ci++);      //12
  ADD_CHAR("Return", ci++);    //13
  ADD_CHAR("So", ci++);
  ADD_CHAR("Si", ci++);
  ADD_CHAR("Dle", ci++);
  ADD_CHAR("Dc1", ci++);
  ADD_CHAR("Dc2", ci++);
  ADD_CHAR("Dc3", ci++);
  ADD_CHAR("Dc4", ci++);
  ADD_CHAR("Nak", ci++);
  ADD_CHAR("Syn", ci++);
  ADD_CHAR("Etb", ci++);
  ADD_CHAR("Can", ci++);
  ADD_CHAR("Em", ci++);
  ADD_CHAR("Sub", ci++);
  ADD_CHAR("Esc", ci++);
  ADD_CHAR("Fs", ci++);
  ADD_CHAR("Gs", ci++);
  ADD_CHAR("Rs", ci++);
  ADD_CHAR("Us", ci++);
  ADD_CHAR("Space", ci++);
  ADD_CHAR("EXCLAMATION_MARK", ci++);
  ADD_CHAR("QUOTATION_MARK", ci++);
  ADD_CHAR("NUMBER_SIGN", ci++);
  ADD_CHAR("DOLLAR_SIGN", ci++);
  ADD_CHAR("PERCENT_SIGN", ci++);
  ADD_CHAR("AMPERSAND", ci++);
  ADD_CHAR("APOSTROPHE", ci++);
  ADD_CHAR("LEFT_PARENTHESIS", ci++);
  ADD_CHAR("RIGHT_PARENTHESIS", ci++);
  ADD_CHAR("ASTERISK", ci++);
  ADD_CHAR("PLUS_SIGN", ci++);
  ADD_CHAR("COMMA", ci++);
  ADD_CHAR("HYPHEN-MINUS", ci++);
  ADD_CHAR("FULL_STOP", ci++);
  ADD_CHAR("SOLIDUS", ci++);
  ADD_CHAR("DIGIT_ZERO", ci++);
  ADD_CHAR("DIGIT_ONE", ci++);
  ADD_CHAR("DIGIT_TWO", ci++);
  ADD_CHAR("DIGIT_THREE", ci++);
  ADD_CHAR("DIGIT_FOUR", ci++);
  ADD_CHAR("DIGIT_FIVE", ci++);
  ADD_CHAR("DIGIT_SIX", ci++);
  ADD_CHAR("DIGIT_SEVEN", ci++);
  ADD_CHAR("DIGIT_EIGHT", ci++);
  ADD_CHAR("DIGIT_NINE", ci++);
  ADD_CHAR("COLON", ci++);
  ADD_CHAR("SEMICOLON", ci++);
  ADD_CHAR("LESS-THAN_SIGN", ci++);
  ADD_CHAR("EQUALS_SIGN", ci++);
  ADD_CHAR("GREATER-THAN_SIGN", ci++);
  ADD_CHAR("QUESTION_MARK", ci++);
  ADD_CHAR("COMMERCIAL_AT", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_A", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_B", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_C", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_D", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_E", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_F", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_G", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_H", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_I", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_J", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_K", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_L", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_M", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_N", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_O", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_P", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_Q", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_R", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_S", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_T", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_U", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_V", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_W", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_X", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_Y", ci++);
  ADD_CHAR("LATIN_CAPITAL_LETTER_Z", ci++);
  ADD_CHAR("LEFT_SQUARE_BRACKET", ci++);
  ADD_CHAR("REVERSE_SOLIDUS", ci++);
  ADD_CHAR("RIGHT_SQUARE_BRACKET", ci++);
  ADD_CHAR("CIRCUMFLEX_ACCENT", ci++);
  ADD_CHAR("LOW_LINE", ci++);
  ADD_CHAR("GRAVE_ACCENT", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_A", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_B", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_C", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_D", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_E", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_F", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_G", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_H", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_I", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_J", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_K", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_L", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_M", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_N", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_O", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_P", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_Q", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_R", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_S", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_T", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_U", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_V", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_W", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_X", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_Y", ci++);
  ADD_CHAR("LATIN_SMALL_LETTER_Z", ci++);
  ADD_CHAR("LEFT_CURLY_BRACKET", ci++);
  ADD_CHAR("VERTICAL_LINE", ci++);
  ADD_CHAR("RIGHT_CURLY_BRACKET", ci++);
  ADD_CHAR("TILDE", ci++);
  ADD_CHAR("Rubout", ci++);
}

#define ARGS_af_standard_char_p "(ch)"
#define DECL_af_standard_char_p ""
#define DOCS_af_standard_char_p "See CLHS: standard_char_p"
bool af_standard_char_p(Character_sp ch) {
  _G();
  char c = ch->asChar();
  if (c == 10)
    return true; // NEWLINE
  if (c == ' ')
    return true;
  if (c >= 'a' && c <= 'z')
    return true;
  if (c >= 'A' && c <= 'Z')
    return true;
  if (c >= '0' && c <= '9')
    return true;
  if (c == '!' || c == '%' || c == '"' || c == '\'' || c == '(' //      left parenthesis, or open parenthesis
      || c == ')'                                               //      right parenthesis, or close parenthesis
      || c == ','                                               //      comma
      || c == '_'                                               //      low line, or underscore
      || c == '-'                                               //      hyphen, or minus [sign]
      || c == '.'                                               //      full stop, period, or dot
      || c == '/'                                               //      solidus, or slash
      || c == ':'                                               //      colon
      || c == ';'                                               //      semicolon
      || c == '?'                                               //      question mark
      || c == '+'                                               //      plus [sign]
      || c == '<'                                               //      less-than [sign]
      || c == '='                                               //      equals [sign]
      || c == '>'                                               //      greater-than [sign]
      || c == '#'                                               //      number sign, or sharp[sign]
      || c == '%'                                               //      percent [sign]
      || c == '&'                                               //      ampersand
      || c == '*'                                               //      asterisk, or star
      || c == '@'                                               //      commercial at, or at-sign
      || c == '['                                               //      left [square] bracket
      || c == '\\'                                              //      reverse solidus, or backslash
      || c == ']'                                               //      right [square] bracket
      || c == '{'                                               //      left curly bracket, or left brace
      || c == '|'                                               //      vertical bar
      || c == '}'                                               //      right curly bracket, or right brace
      || c == '`'                                               //      grave accent, or backquote
      || c == '^'                                               //      circumflex accent
      || c == '~'                                               //      tilde
      )
    return true;
  return false;
};

#define ARGS_af_alpha_char_p "(ch)"
#define DECL_af_alpha_char_p ""
#define DOCS_af_alpha_char_p "alpha_char_p"
bool af_alpha_char_p(Character_sp ch) {
  _G();
  return isalpha(ch->asChar());
};

Fixnum clasp_digitp(int ch, int basis) {
  if (('0' <= ch) && (ch <= '9') && (ch < '0' + basis))
    return ch - '0';
  if (('A' <= ch) && (10 < basis) && (ch < 'A' + (basis - 10)))
    return ch - 'A' + 10;
  if (('a' <= ch) && (10 < basis) && (ch < 'a' + (basis - 10)))
    return ch - 'a' + 10;
#ifdef BRCL_UNICODE
  IMPLEMENT_MEF(BF("Handle Unicode"));
#endif
  return -1;
}

#define ARGS_cl_digitCharP "(c &optional (radix 10))"
#define DECL_cl_digitCharP ""
#define DOCS_cl_digitCharP "digitCharP"
T_sp cl_digitCharP(Character_sp c, Fixnum_sp radix) {
  _G();
  Fixnum basis = radix->get();
  if (basis < 2 || basis > 36) {
    QERROR_WRONG_TYPE_NTH_ARG(2, radix, Integer_O::makeIntegerType(2, 36));
  }
  Fixnum value = clasp_digitp(c->toInt(), basis);
  return (value < 0) ? _Nil<Fixnum_O>() : Fixnum_O::create(value);
};

#define DOCS_af_name_char "name_char"
#define LOCK_af_name_char 1
#define ARGS_af_name_char "(sname)"
#define DECL_af_name_char ""
T_mv af_name_char(Str_sp sname) {
  _G();
  Str_sp name = coerce::stringDesignator(sname);
  string upname = stringUpper(name->get());
  map<string, int>::const_iterator it = _lisp->characterInfo().gNamesToCharacterIndex.find(upname);
  if (it != _lisp->characterInfo().gNamesToCharacterIndex.end()) {
    return (Values(_lisp->characterInfo().gIndexedCharacters[it->second]));
  }
  return (Values(_Nil<T_O>()));
};

#define DOCS_cl_char_name "char_name"
#define LOCK_cl_char_name 1
#define ARGS_cl_char_name "(och)"
#define DECL_cl_char_name ""
Str_sp cl_char_name(Character_sp och) {
  _G();
  char ch = och->asChar();
  return (_lisp->characterInfo().gCharacterNames[ch]);
};

#define DOCS_cl_char_code "char_code"
#define ARGS_cl_char_code "(och)"
#define DECL_cl_char_code ""
Fixnum_sp cl_char_code(Character_sp och) {
  _G();
  int ch = och->asChar();
  if (ch < CHAR_CODE_LIMIT) {
    return Fixnum_O::create((int)ch);
  }
  SIMPLE_ERROR(BF("Character is beyon CHAR_CODE_LIMIT: %d") % CHAR_CODE_LIMIT);
};

#define DOCS_cl_char_int "char_int"
#define ARGS_cl_char_int "(och)"
#define DECL_cl_char_int ""
Fixnum_sp cl_char_int(Character_sp och) {
  _G();
  char ch = och->asChar();
  return Fixnum_O::create((int)ch);
};

#define DOCS_cl_code_char "code_char"
#define ARGS_cl_code_char "(och)"
#define DECL_cl_code_char ""
Character_sp cl_code_char(Integer_sp ich) {
  _G();
  int ii = ich->as_int();
  if (ii >= 0 && ii < CHAR_CODE_LIMIT) {
    return Character_O::create(ii);
  }
  return _Nil<Character_O>();
};

claspChar clasp_charCode(T_sp c) {
  Character_sp cc = c.as<Character_O>();
  return cc->asChar();
}

// ----------------------------------------------------------------------
//

EXPOSE_CLASS(core, Character_O);

void Character_O::exposeCando(::core::Lisp_sp lisp) {
  _G();
  ::core::class_<Character_O>()
      .def("lower_case_p", &Character_O::lower_case_p)
      .def("upper_case_p", &Character_O::upper_case_p)
      .def("both_case_p", &Character_O::both_case_p)
      .def("alphanumericp", &Character_O::alphanumericp)
      .def("graphic-char-p", &Character_O::graphicCharP)

      //	.initArgs("(self)")
      ;
  SYMBOL_EXPORT_SC_(ClPkg, char_code);
  ClDefun(char_code);
  SYMBOL_EXPORT_SC_(ClPkg, code_char);
  ClDefun(code_char);
  SYMBOL_EXPORT_SC_(ClPkg, char_int);
  ClDefun(char_int);

  SYMBOL_EXPORT_SC_(ClPkg, name_char);
  Defun(name_char);
  SYMBOL_EXPORT_SC_(ClPkg, char_name);
  ClDefun(char_name);
  SYMBOL_EXPORT_SC_(ClPkg, alpha_char_p);
  Defun(alpha_char_p);
  SYMBOL_EXPORT_SC_(ClPkg, standard_char_p);
  Defun(standard_char_p);
  SYMBOL_EXPORT_SC_(ClPkg, charUpcase);
  Defun(charUpcase);
  SYMBOL_EXPORT_SC_(ClPkg, charDowncase);
  Defun(charDowncase);
  SYMBOL_EXPORT_SC_(ClPkg, char_LT_);
  Defun(char_LT_);
  SYMBOL_EXPORT_SC_(ClPkg, char_GT_);
  Defun(char_GT_);
  SYMBOL_EXPORT_SC_(ClPkg, char_LE_);
  Defun(char_LE_);
  SYMBOL_EXPORT_SC_(ClPkg, char_GE_);
  Defun(char_GE_);
  SYMBOL_EXPORT_SC_(ClPkg, char_NE_);
  Defun(char_NE_);
  SYMBOL_EXPORT_SC_(ClPkg, char_EQ_);
  Defun(char_EQ_);

  SYMBOL_EXPORT_SC_(ClPkg, charLessp);
  Defun(charLessp);
  SYMBOL_EXPORT_SC_(ClPkg, charGreaterp);
  Defun(charGreaterp);
  SYMBOL_EXPORT_SC_(ClPkg, charNotGreaterp);
  Defun(charNotGreaterp);
  SYMBOL_EXPORT_SC_(ClPkg, charNotLessp);
  Defun(charNotLessp);
  SYMBOL_EXPORT_SC_(ClPkg, charNotEqual);
  Defun(charNotEqual);
  SYMBOL_EXPORT_SC_(ClPkg, charEqual);
  Defun(charEqual);
  SYMBOL_EXPORT_SC_(ClPkg, digitCharP);
  ClDefun(digitCharP);
}

void Character_O::exposePython(::core::Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(Pkg(), Character, "", "", _LISP)
      //	.initArgs("(self)")
      ;
#endif
}

Character_sp Character_O::create(char c) {
  _G();
  StandardChar_sp sc = StandardChar_O::create(c);
  return sc;
}

Character_sp Character_O::create(int c) {
  _G();
  StandardChar_sp sc = StandardChar_O::create(BRCL_CHAR(c));
  return sc;
}

Character_sp Character_O::create(T_sp val) {
  _G();
  if (af_fixnumP(val)) {
    int v = val.as<Fixnum_O>()->get();
    return Character_O::create(v);
  }
  SIMPLE_ERROR(BF("Cannot create Character from %s") % _rep_(val));
}

Character_sp Character_O::create_from_name(string const &name) {
  _G();
  return StandardChar_O::create_from_name(name);
}

#if 0
#if defined(OLD_SERIALIZE)
    void Character_O::serialize(::serialize::SNodeP node)
    {
        this->Bases::serialize(node);
	// Archive other instance variables here
    }
#endif
#endif

#if defined(XML_ARCHIVE)
void Character_O::archiveBase(::core::ArchiveP node) {
  this->Base::archiveBase(node);
  // Archive other instance variables here
}
#endif // defined(XML_ARCHIVE)

void Character_O::initialize() {
  _OF();
  this->Base::initialize();
}

bool Character_O::equal(T_sp other) const {
  _OF();
  return this->eql(other);
}

bool Character_O::equalp(T_sp other) const {
  _OF();
  if (other.nilp())
    return false;
  if (Character_sp cother = other.as<Character_O>()) {
    return (toupper(cother->asChar()) == toupper(this->asChar()));
  }
  return false;
}

EXPOSE_CLASS(core, BaseChar_O);

void BaseChar_O::exposeCando(::core::Lisp_sp lisp) {
  ::core::class_<BaseChar_O>()
      //	.initArgs("(self)")
      ;
}

void BaseChar_O::exposePython(::core::Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(Pkg(), BaseChar, "", "", _LISP)
      //	.initArgs("(self)")
      ;
#endif
}

void BaseChar_O::initialize() {
  _OF();
  this->Base::initialize();
}

EXPOSE_CLASS(core, StandardChar_O);

#define LINE_FEED_CHAR 10
#define PAGE_CHAR 12
#define RETURN_CHAR 13

StandardChar_sp StandardChar_O::create_from_name(string const &name) {
  _G();
  StandardChar_sp ch = _Nil<StandardChar_O>();
  string ssup = boost::to_upper_copy(name);
  if (ssup == "TAB")
    ch = StandardChar_O::create(9);
  else if (ssup == "NEWLINE")
    ch = StandardChar_O::create('\n');
  else if (ssup == "LINEFEED")
    ch = StandardChar_O::create(LINE_FEED_CHAR);
  else if (ssup == "PAGE")
    ch = StandardChar_O::create(PAGE_CHAR);
  else if (ssup == "RETURN")
    ch = StandardChar_O::create(RETURN_CHAR);
  else if (ssup == "SPACE")
    ch = StandardChar_O::create(' ');
  else if (ssup == "BACKSPACE")
    ch = StandardChar_O::create(8);
  else {
    SIMPLE_ERROR(BF("Unknown character name[%s]") % ssup);
  }
  return ch;
}

void StandardChar_O::exposeCando(Lisp_sp lisp) {
  class_<StandardChar_O>();
}

void StandardChar_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, StandardChar, "", "", _lisp);
#endif
}

StandardChar_sp StandardChar_O::create(char c) {
  GC_ALLOCATE(StandardChar_O, v);
  v->set(c);
  return v;
}

#if defined(OLD_SERIALIZE)
void StandardChar_O::serialize(serialize::SNode node) {
  _G();
  if (node->saving()) {
    //	    node.setObject(this->sharedThis<StandardChar_O>());
  } else {
    IMPLEMENT_ME();
  }
}
#endif

#if defined(XML_ARCHIVE)
void StandardChar_O::archiveBase(ArchiveP node) {
  this->Base::archiveBase(node);
  node->attribute("val", this->_Value);
}
#endif // defined(XML_ARCHIVE)

string StandardChar_O::__repr__() const {
  stringstream ss;
  ss << "#\\";
  if (this->_Value >= ' ') {
    ss << this->_Value;
  } else {
    ss << cl_char_name(this->const_sharedThis<StandardChar_O>());
  }
  return ss.str();
}

void StandardChar_O::sxhash(HashGenerator &hg) const {
  hg.addPart(this->_Value);
}

T_sp StandardChar_O::deepCopy() const {
  _OF();
  return this->const_sharedThis<StandardChar_O>();
}

void StandardChar_O::setFromString(const string &strVal) {
  this->_Value = strVal[0];
}

string StandardChar_O::valueAsString() const {
  stringstream ss;
  ss << (unsigned char)(this->_Value);
  return ss.str();
}

bool StandardChar_O::operator>=(T_sp obj) const {
  _OF();
  if (af_characterP(obj)) {
    Character_sp wn = obj.as<Character_O>();
    char v = wn->asChar();
    return this->_Value >= v;
  }
  SIMPLE_ERROR(BF("Wrong format for ge with Char"));
}

bool StandardChar_O::operator<=(T_sp obj) const {
  _OF();
  if (af_characterP(obj)) {
    Character_sp wn = obj.as<Character_O>();
    char v = wn->asChar();
    return this->_Value <= v;
  }
  SIMPLE_ERROR(BF("Wrong format for le with Char"));
}

bool StandardChar_O::operator<(T_sp obj) const {
  _OF();
  if (af_characterP(obj)) {
    Character_sp wn = obj.as<Character_O>();
    char v = wn->asChar();
    return this->_Value < v;
  }
  SIMPLE_ERROR(BF("Wrong format for le with Char"));
}

bool StandardChar_O::eqn(T_sp obj) const {
  _OF();
  if (obj.nilp())
    return false;
  if (obj.get() == this)
    return true;
  if (af_characterP(obj)) {
    Character_sp wn = obj.as<Character_O>();
    char v = wn->asChar();
    return this->_Value == v;
  }
  SIMPLE_ERROR(BF("Wrong format for comparison with Char"));
}

bool StandardChar_O::eql(T_sp obj) const {
  _OF();
  if (obj.nilp())
    return false;
  if (Character_sp wn = obj.asOrNull<Character_O>()) {
    char v = wn->asChar();
    return this->_Value == v;
  }
  return false;
}

bool StandardChar_O::operator>(T_sp obj) const {
  _OF();
  if (af_characterP(obj)) {
    Character_sp wn = obj.as<Character_O>();
    char v = wn->asChar();
    return this->_Value > v;
  }
  SIMPLE_ERROR(BF("Wrong format for comparison with Char"));
}

Character_sp StandardChar_O::char_upcase() const {
  _OF();
  unsigned char uc = toupper(this->_Value);
  return StandardChar_O::create(uc);
}

Character_sp StandardChar_O::char_downcase() const {
  _OF();
  unsigned char uc = tolower(this->_Value);
  return StandardChar_O::create(uc);
}

bool StandardChar_O::upper_case_p() const {
  _OF();
  return isupper(this->_Value);
}

bool StandardChar_O::lower_case_p() const {
  _OF();
  return islower(this->_Value);
}

bool StandardChar_O::both_case_p() const {
  _OF();
  return isalpha(this->_Value);
}

bool StandardChar_O::alpha_char_p() const {
  _OF();
  return isalpha(this->_Value);
}

bool StandardChar_O::alphanumericp() const {
  _OF();
  return isalpha(this->_Value) || isdigit(this->_Value);
}

bool StandardChar_O::graphicCharP() const {
  _G();
  return this->_Value != '\n';
}

EXPOSE_CLASS(core, ExtendedChar_O);

void ExtendedChar_O::exposeCando(::core::Lisp_sp lisp) {
  ::core::class_<ExtendedChar_O>()
      //	.initArgs("(self)")
      ;
}

void ExtendedChar_O::exposePython(::core::Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(Pkg(), ExtendedChar, "", "", _LISP)
      //	.initArgs("(self)")
      ;
#endif
}

#if 0
#if defined(OLD_SERIALIZE)
    void ExtendedChar_O::serialize(::serialize::SNodeP node)
    {
        this->Base::serialize(node);
	// Archive other instance variables here
    }
#endif
#endif

#if defined(XML_ARCHIVE)
void ExtendedChar_O::archiveBase(::core::ArchiveP node) {
  this->Base::archiveBase(node);
  // Archive other instance variables here
}
#endif // defined(XML_ARCHIVE)

void ExtendedChar_O::initialize() {
  _OF();
  this->Base::initialize();
}

}; /* core */
