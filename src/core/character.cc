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

T_sp monotonic(int s, int t, List_sp args, bool preserve_case = true) {
  _G();
  char c = clasp_as_char(gc::As<Character_sp>(oCar(args)));
  if (!preserve_case)
    c = toupper(c);
  char d;
  int dir;
  args = oCdr(args);
  while (args.notnilp()) {
    d = clasp_as_char(gc::As<Character_sp>(oCar(args)));
    if (!preserve_case)
      d = toupper(d);
    dir = s * char_basic_compare(c, d);
    if (dir < t)
      return _Nil<T_O>();
    c = d;
    args = oCdr(args);
  }
  return _lisp->_true();
};

#define ARGS_cl_lower_case_p "(arg)"
#define DECL_cl_lower_case_p ""
#define DOCS_cl_lower_case_p "lower_case_p"
bool cl_lower_case_p(Character_sp c) {
  claspCharacter x = clasp_char_code(c);
  return islower(x);
};

#define ARGS_cl_upper_case_p "(arg)"
#define DECL_cl_upper_case_p ""
#define DOCS_cl_upper_case_p "upper_case_p"
bool cl_upper_case_p(Character_sp c) {
  claspCharacter x = clasp_char_code(c);
  return isupper(x);
};

#define ARGS_cl_both_case_p "(arg)"
#define DECL_cl_both_case_p ""
#define DOCS_cl_both_case_p "both_case_p"
bool cl_both_case_p(Character_sp c) {
  claspCharacter x = clasp_char_code(c);
  return isupper(x) || islower(x);
};

#define ARGS_cl_alphanumericp "(char)"
#define DECL_cl_alphanumericp ""
#define DOCS_cl_alphanumericp "alphanumericp"
bool cl_alphanumericp(Character_sp ch) {
  claspCharacter x = clasp_char_code(ch);
  if (x < 128) {
    return isalpha(x) || isdigit(x);
  }
  return false;
};

#define ARGS_af_charUpcase "(char)"
#define DECL_af_charUpcase ""
#define DOCS_af_charUpcase "charUpcase"
char af_charUpcase(Character_sp ch) {
  _G();
  return toupper(clasp_as_char(ch));
};

#define ARGS_af_charDowncase "(char)"
#define DECL_af_charDowncase ""
#define DOCS_af_charDowncase "charDowncase"
char af_charDowncase(Character_sp ch) {
  _G();
  return tolower(clasp_as_char(ch));
};

#define ARGS_af_char_LT_ "(&rest args)"
#define DECL_af_char_LT_ ""
#define DOCS_af_char_LT_ "Return true if characters are monotonically increasing"
T_sp af_char_LT_(List_sp args) {
  _G();
  return ((monotonic(-1, 1, args)));
};

#define ARGS_af_char_GT_ "(&rest args)"
#define DECL_af_char_GT_ ""
#define DOCS_af_char_GT_ "Return true if characters are monotonically decreasing"
T_sp af_char_GT_(List_sp args) {
  _G();
  return ((monotonic(1, 1, args)));
};

#define ARGS_af_char_LE_ "(&rest args)"
#define DECL_af_char_LE_ ""
#define DOCS_af_char_LE_ "Return true if characters are monotonically non-decreasing"
T_sp af_char_LE_(List_sp args) {
  _G();
  return (Values(monotonic(-1, 0, args)));
};

#define ARGS_af_char_GE_ "(&rest args)"
#define DECL_af_char_GE_ ""
#define DOCS_af_char_GE_ "Return true if characters are monotonically non-increasing"
T_mv af_char_GE_(List_sp args) {
  _G();
  return (Values(monotonic(1, 0, args)));
};

#define ARGS_af_charLessp "(&rest args)"
#define DECL_af_charLessp ""
#define DOCS_af_charLessp "Return true if characters are monotonically increasing, ignore case"
T_mv af_charLessp(List_sp args) {
  _G();
  return (Values(monotonic(-1, 1, args, false)));
};

#define ARGS_af_charGreaterp "(&rest args)"
#define DECL_af_charGreaterp ""
#define DOCS_af_charGreaterp "Return true if characters are monotonically decreasing, ignore case"
T_mv af_charGreaterp(List_sp args) {
  _G();
  return (Values(monotonic(1, 1, args, false)));
};

#define ARGS_af_charNotGreaterp "(&rest args)"
#define DECL_af_charNotGreaterp ""
#define DOCS_af_charNotGreaterp "Return true if characters are monotonically non-increasing, ignore case"
T_mv af_charNotGreaterp(List_sp args) {
  _G();
  return (Values(monotonic(-1, 0, args, false)));
};

#define ARGS_af_charNotLessp "(&rest args)"
#define DECL_af_charNotLessp ""
#define DOCS_af_charNotLessp "Return true if characters are monotonically non-decreasing, ignore case"
T_sp af_charNotLessp(List_sp args) {
  _G();
  return ((monotonic(1, 0, args, false)));
};

#define ARGS_af_char_NE_ "(&rest args)"
#define DECL_af_char_NE_ ""
#define DOCS_af_char_NE_ "NE_"
T_sp af_char_NE_(List_sp args) {
  _G();
  while (args.notnilp()) {
    int a = clasp_as_character(gc::As<Character_sp>(oCar(args)));
    for (List_sp cur = oCdr(args); cur.notnilp(); cur = oCdr(cur)) {
      int b = clasp_as_character(gc::As<Character_sp>(oCar(cur)));
      if (a == b)
        return ((_Nil<T_O>()));
    }
    args = oCdr(args);
  }
  return ((_lisp->_true()));
}

#define ARGS_af_char_EQ_ "(&rest args)"
#define DECL_af_char_EQ_ ""
#define DOCS_af_char_EQ_ "EQ_"
T_sp af_char_EQ_(List_sp args) {
  _G();
  if (args.nilp())
    return ((_lisp->_true()));
  int a = clasp_as_character(gc::As<Character_sp>(oCar(args)));
  args = oCdr(args);
  while (args.notnilp()) {
    int b = clasp_as_character(gc::As<Character_sp>(oCar(args)));
    if (a != b)
      return ((_Nil<T_O>()));
    args = oCdr(args);
  }
  return ((_lisp->_true()));
};

#define ARGS_af_charNotEqual "(&rest args)"
#define DECL_af_charNotEqual ""
#define DOCS_af_charNotEqual "Like char_NE_ but ignore case"
T_mv af_charNotEqual(List_sp args) {
  _G();
  while (args.notnilp()) {
    int a = clasp_as_character(gc::As<Character_sp>(oCar(args)));
    a = toupper(a);
    for (List_sp cur = oCdr(args); cur.notnilp(); cur = oCdr(cur)) {
      int b = clasp_as_character(gc::As<Character_sp>(oCar(cur)));
      b = toupper(b);
      if (a == b)
        return (Values(_Nil<T_O>()));
    }
    args = oCdr(args);
  }
  return (Values(_lisp->_true()));
}

bool clasp_charEqual2(T_sp x, T_sp y) {
  if (x.characterp() && y.characterp()) {
    int cx = toupper(x.unsafe_character());
    int cy = toupper(y.unsafe_character());
    return cx == cy;
  }
  // Get rid of this when we lose Character_O
  int icx = toupper(clasp_as_character(gc::As<Character_sp>(x)));
  int icy = toupper(clasp_as_character(gc::As<Character_sp>(y)));
  return icx == icy;
}

#define ARGS_af_charEqual "(&rest args)"
#define DECL_af_charEqual ""
#define DOCS_af_charEqual "Like char_EQ_, ignore case"
bool af_charEqual(List_sp args) {
  _G();
  if (args.nilp())
    return true;
  int a = clasp_as_character(gc::As<Character_sp>(oCar(args)));
  a = toupper(a);
  args = oCdr(args);
  while (args.notnilp()) {
    int b = clasp_as_character(gc::As<Character_sp>(oCar(args)));
    b = toupper(b);
    if (a != b)
      return false;
    args = oCdr(args);
  }
  return true;
};

#define LINE_FEED_CHAR 10
#define PAGE_CHAR 12
#define RETURN_CHAR 13

Character_sp clasp_character_create_from_name(string const &name) {
  _G();
  Character_sp ch;
  string ssup = boost::to_upper_copy(name);
  if (ssup == "TAB")
    ch = clasp_make_standard_character(9);
  else if (ssup == "NEWLINE")
    ch = clasp_make_standard_character('\n');
  else if (ssup == "LINEFEED")
    ch = clasp_make_standard_character(LINE_FEED_CHAR);
  else if (ssup == "PAGE")
    ch = clasp_make_standard_character(PAGE_CHAR);
  else if (ssup == "RETURN")
    ch = clasp_make_standard_character(RETURN_CHAR);
  else if (ssup == "SPACE")
    ch = clasp_make_standard_character(' ');
  else if (ssup == "BACKSPACE")
    ch = clasp_make_standard_character(8);
  else {
    SIMPLE_ERROR(BF("Unknown character name[%s]") % ssup);
  }
  return ch;
}
};
namespace core {

CharacterInfo::CharacterInfo() {
  this->gCharacterNames.resize(256, _Nil<T_O>());
  this->gIndexedCharacters.resize(256, _Nil<T_O>());
#define ADD_CHAR(name, char_index)                                      \
  {                                                                     \
    string upcase_name = stringUpper(name);                             \
    int fci = char_index;                                               \
    gNamesToCharacterIndex[upcase_name] = fci;                          \
    gIndexedCharacters[fci] = clasp_make_standard_character((char)fci); \
    gCharacterNames[fci] = Str_O::create(upcase_name);                  \
  }
  int ci = 0;
  gNamesToCharacterIndex["NULL"] = 0;
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
  char c = clasp_as_char(ch);
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
  return isalpha(clasp_as_char(ch));
};

Fixnum clasp_digitp(int ch, int basis) {
  if (('0' <= ch) && (ch <= '9') && (ch < '0' + basis))
    return ch - '0';
  if (('A' <= ch) && (10 < basis) && (ch < 'A' + (basis - 10)))
    return ch - 'A' + 10;
  if (('a' <= ch) && (10 < basis) && (ch < 'a' + (basis - 10)))
    return ch - 'a' + 10;
#ifdef CLASP_UNICODE
  IMPLEMENT_MEF(BF("Handle Unicode"));
#endif
  return -1;
}

#define ARGS_cl_digitCharP "(c &optional (radix 10))"
#define DECL_cl_digitCharP ""
#define DOCS_cl_digitCharP "digitCharP"
T_sp cl_digitCharP(Character_sp c, Fixnum_sp radix) {
  _G();
  Fixnum basis = unbox_fixnum(radix);
  if (basis < 2 || basis > 36) {
    QERROR_WRONG_TYPE_NTH_ARG(2, radix, Integer_O::makeIntegerType(2, 36));
  }
  Fixnum value = clasp_digitp(clasp_as_character(c), basis);
  if (value < 0) {
    return _Nil<T_O>();
  }
  return make_fixnum(value);
};

#define ARGS_af_name_char "(sname)"
#define DECL_af_name_char ""
#define DOCS_af_name_char "name_char"
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

#define ARGS_cl_char_name "(och)"
#define DECL_cl_char_name ""
#define DOCS_cl_char_name "char_name"
Str_sp cl_char_name(Character_sp och) {
  _G();
  char ch = clasp_as_char(och);
  return (_lisp->characterInfo().gCharacterNames[ch]);
};

#define ARGS_cl_char_code "(och)"
#define DECL_cl_char_code ""
#define DOCS_cl_char_code "char_code"
Fixnum_sp cl_char_code(Character_sp och) {
  _G();
  int ch = clasp_as_char(och);
  if (ch < CHAR_CODE_LIMIT) {
    return make_fixnum((int)ch);
  }
  SIMPLE_ERROR(BF("Character is beyon CHAR_CODE_LIMIT: %d") % CHAR_CODE_LIMIT);
};

#define ARGS_cl_char_int "(och)"
#define DECL_cl_char_int ""
#define DOCS_cl_char_int "char_int"
Fixnum_sp cl_char_int(Character_sp och) {
  _G();
  char ch = clasp_as_char(och);
  return make_fixnum((int)ch);
};

#define ARGS_cl_code_char "(och)"
#define DECL_cl_code_char ""
#define DOCS_cl_code_char "code_char"
Character_sp cl_code_char(Integer_sp ich) {
  _G();
  int ii = clasp_to_int(ich);
  if (ii >= 0 && ii < CHAR_CODE_LIMIT) {
    return clasp_make_character(ii);
  }
  return _Nil<Character_O>();
};

claspChar clasp_charCode(T_sp c) {
  Character_sp cc = gc::As<Character_sp>(c);
  return clasp_as_char(cc);
}

// ----------------------------------------------------------------------
//

EXPOSE_CLASS(core, Character_dummy_O);

void Character_dummy_O::exposeCando(::core::Lisp_sp lisp) {
  _G();
  ::core::class_<Character_dummy_O>();
  ClDefun(alphanumericp);
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
  ClDefun(lower_case_p);
  ClDefun(upper_case_p);
  ClDefun(both_case_p);
}

void Character_dummy_O::exposePython(::core::Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(Pkg(), Character, "", "", _LISP)
      //	.initArgs("(self)")
      ;
#endif
}

#if 0
    Character_sp Character_O::create(gctools::Fixnum c)
    {_G();
	StandardChar_sp sc = clasp_make_standard_character(CLASP_CHAR(c));
	return sc;
    }

    Character_sp Character_O::create(T_sp val)
    {_G();
	if ( af_fixnumP(val) )
	{
	    int v = unbox_fixnum(gc::As<Fixnum_sp>(val));
	    return clasp_make_character(v);
	}
	SIMPLE_ERROR(BF("Cannot create Character from %s") % _rep_(val) );
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
void Character_O::archiveBase(::core::ArchiveP node)
    {
        this->Base::archiveBase(node);
	// Archive other instance variables here
    }
#endif // defined(XML_ARCHIVE)

    void Character_O::initialize()
    {_OF();
        this->Base::initialize();
    }

    bool Character_O::equal(T_sp other) const
    {_OF();
	return this->eql_(other);
    }


    bool Character_O::equalp(T_sp other) const
    {_OF();
        if ( other.nilp() ) return false;
	if ( Character_sp cother = gc::As<Character_sp>(other) ) {
	    return ( toupper(clasp_as_char(cother)) == toupper(this->asChar()) );
	}
	return false;
    }








    EXPOSE_CLASS(core,BaseChar_O);

    void BaseChar_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<BaseChar_O>()
//	.initArgs("(self)")
	;
    }

    void BaseChar_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(Pkg(),BaseChar,"","",_LISP)
//	.initArgs("(self)")
	;
#endif
    }



    void BaseChar_O::initialize()
    {_OF();
        this->Base::initialize();
    }





    EXPOSE_CLASS(core,StandardChar_O);




    void StandardChar_O::exposeCando(Lisp_sp lisp)
    {
	class_<StandardChar_O>()
	    ;
    }

    void StandardChar_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,StandardChar,"","",_lisp)
	    ;
#endif
    }





    StandardChar_sp clasp_make_standard_character(char c)
    {
        GC_ALLOCATE(StandardChar_O,v );
	v->set(c);
	return v;
    }

#if defined(OLD_SERIALIZE)
    void StandardChar_O::serialize(serialize::SNode node)
    {_G();
	if ( node->saving() )
	{
//	    node.setObject(this->sharedThis<StandardChar_O>());
	} else
	{
	    IMPLEMENT_ME();
	}
    }
#endif

#if defined(XML_ARCHIVE)
void StandardChar_O::archiveBase(ArchiveP node)
    {
	this->Base::archiveBase(node);
	node->attribute("val",this->_Value);
    }
#endif // defined(XML_ARCHIVE)

    string StandardChar_O::__repr__() const
    {
	stringstream ss;
	ss << "#\\";
	if ( this->_Value >= ' ' )
	{
	    ss << this->_Value;
	} else
	{
	    ss << _rep_(cl_char_name(this->const_sharedThis<StandardChar_O>()));
	}
	return ss.str();
    }

    void StandardChar_O::sxhash_(HashGenerator& hg) const
    {
	hg.addPart(this->_Value);
    }



    T_sp StandardChar_O::deepCopy() const
    {_OF();
	return this->const_sharedThis<StandardChar_O>();
    }



    void StandardChar_O::setFromString(const string& strVal)
    {
	this->_Value = strVal[0];
    }


    string StandardChar_O::valueAsString() const
    {
	stringstream ss;
	ss << (unsigned char)(this->_Value);
	return ss.str();
    }

    bool StandardChar_O::operator>=(T_sp obj) const
    {_OF();
	if ( af_characterP(obj) )
	{
	    Character_sp wn = gc::As<Character_sp>(obj);
	    char v = clasp_as_char(wn);
	    return this->_Value >= v;
	}
	SIMPLE_ERROR(BF("Wrong format for ge with Char"));
    }



    bool StandardChar_O::operator<=(T_sp obj) const
    {_OF();
	if ( af_characterP(obj) )
	{
	    Character_sp wn = gc::As<Character_sp>(obj);
	    char v = clasp_as_char(wn);
	    return this->_Value <= v;
	}
	SIMPLE_ERROR(BF("Wrong format for le with Char"));
    }


    bool StandardChar_O::operator<(T_sp obj) const
    {_OF();
	if ( af_characterP(obj) )
	{
	    Character_sp wn = gc::As<Character_sp>(obj);
	    char v = clasp_as_char(wn);
	    return this->_Value < v;
	}
	SIMPLE_ERROR(BF("Wrong format for le with Char"));
    }

    bool StandardChar_O::eqn(T_sp obj) const
    {_OF();
	if ( obj.nilp() ) return false;
	if ( obj.get() == this ) return true;
	if ( af_characterP(obj) )
	{
	    Character_sp wn = gc::As<Character_sp>(obj);
	    char v = clasp_as_char(wn);
	    return this->_Value == v;
	}
	SIMPLE_ERROR(BF("Wrong format for comparison with Char"));
    }

    bool StandardChar_O::eql_(T_sp obj) const
    {_OF();
	if ( obj.nilp() ) return false;
	if ( Character_sp wn = obj.asOrNull<Character_O>() )
	{
	    char v = clasp_as_char(wn);
	    return this->_Value == v;
	}
	return false;
    }

    bool StandardChar_O::operator>(T_sp obj) const
    {_OF();
	if ( af_characterP(obj) )
	{
	    Character_sp wn = gc::As<Character_sp>(obj);
	    char v = clasp_as_char(wn);
	    return this->_Value > v;
	}
	SIMPLE_ERROR(BF("Wrong format for comparison with Char"));
    }


    Character_sp StandardChar_O::char_upcase() const
    {_OF();
	unsigned char uc = toupper(this->_Value);
	return clasp_make_standard_character(uc);
    }

    Character_sp StandardChar_O::char_downcase() const
    {_OF();
	unsigned char uc = tolower(this->_Value);
	return clasp_make_standard_character(uc);
    }


    bool StandardChar_O::upper_case_p() const
    {_OF();
	return isupper(this->_Value);
    }


    bool StandardChar_O::lower_case_p() const
    {_OF();
	return islower(this->_Value);
    }

    bool StandardChar_O::both_case_p() const
    {_OF();
	return isalpha(this->_Value);
    }

    bool StandardChar_O::alpha_char_p() const
    {_OF();
	return isalpha(this->_Value);
    }

    bool StandardChar_O::alphanumericp() const
    {_OF();
	return isalpha(this->_Value) || isdigit(this->_Value);
    }

    bool StandardChar_O::graphicCharP() const
    {_G();
	return this->_Value!='\n';
    }

















    EXPOSE_CLASS(core,ExtendedChar_O);

    void ExtendedChar_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<ExtendedChar_O>()
//	.initArgs("(self)")
	;
    }

    void ExtendedChar_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(Pkg(),ExtendedChar,"","",_LISP)
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
void ExtendedChar_O::archiveBase(::core::ArchiveP node)
    {
        this->Base::archiveBase(node);
	// Archive other instance variables here
    }
#endif // defined(XML_ARCHIVE)

    void ExtendedChar_O::initialize()
    {_OF();
        this->Base::initialize();
    }

#endif

}; /* core */
