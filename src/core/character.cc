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
//#define DEBUG_LEVEL_FULL

#pragma clang diagnostic push
//#pragma clang diagnostic ignored "-Wunused-local-typedef"
#include <boost/algorithm/string.hpp>
#pragma clang diagnostic pop
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/environment.h>
#include <clasp/core/designators.h>
#include <clasp/core/character.h>
#include <clasp/core/numberToString.h>
#include <clasp/core/wrappers.h>

namespace core {


void handleWideCharactersError(claspCharacter cc)
{
  SIMPLE_ERROR(BF("A wide character with the value %d was encountered in a function that needed a base-char") % cc);
}

/*! Return -1 if a<b
      0 if a == b
      +1 if a > b
    */
inline int claspCharacter_basic_compare(claspCharacter na, claspCharacter nb) {
  if (na < nb)
    return -1;
  if (na == nb)
    return 0;
  return 1;
}

T_sp monotonic(int s, int t, List_sp args, bool preserve_case = true) {
  claspCharacter c = clasp_as_claspCharacter(gc::As<Character_sp>(oCar(args)));
  if (!preserve_case) c = claspCharacter_upcase(c);
  claspCharacter d;
  int dir;
  args = oCdr(args);
  while (args.notnilp()) {
    d = clasp_as_claspCharacter(gc::As<Character_sp>(oCar(args)));
    if (!preserve_case) d = claspCharacter_upcase(d);
    dir = s * claspCharacter_basic_compare(c, d);
    if (dir < t) return _Nil<T_O>();
    c = d;
    args = oCdr(args);
  }
  return _lisp->_true();
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("CLHS: graphic-char-p");
CL_DEFUN bool cl__graphic_char_p(Character_sp c) {
  claspCharacter x = clasp_as_claspCharacter(c);
  return x == ' ' || isgraph(x);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("lower_case_p");
CL_DEFUN bool cl__lower_case_p(Character_sp c) {
  claspCharacter x = clasp_as_claspCharacter(c);
  return clasp_islower(x);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("upper_case_p");
CL_DEFUN bool cl__upper_case_p(Character_sp c) {
  claspCharacter x = clasp_as_claspCharacter(c);
  return clasp_isupper(x);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("both_case_p");
CL_DEFUN bool cl__both_case_p(Character_sp c) {
  claspCharacter x = clasp_as_claspCharacter(c);
  return clasp_isupper(x) || clasp_islower(x);
};

CL_LAMBDA(char);
CL_DECLARE();
CL_DOCSTRING("alphanumericp");
CL_DEFUN bool cl__alphanumericp(Character_sp ch) {
  claspCharacter x = clasp_as_claspCharacter(ch);
  if (x < 128) {
    return isalpha(x) || isdigit(x);
  }
  return false;
};

CL_LAMBDA(char);
CL_DECLARE();
CL_DOCSTRING("charUpcase");
CL_DEFUN Character_sp cl__char_upcase(Character_sp ch) {
  return clasp_make_character(claspCharacter_upcase(clasp_as_claspCharacter(ch)));
};

CL_LAMBDA(char);
CL_DECLARE();
CL_DOCSTRING("charDowncase");
CL_DEFUN Character_sp cl__char_downcase(Character_sp ch) {
  return clasp_make_character(claspCharacter_downcase(clasp_as_claspCharacter(ch)));
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("Return true if characters are monotonically increasing");
CL_DEFUN T_sp cl__char_LT_(List_sp args) {
  return ((monotonic(-1, 1, args)));
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("Return true if characters are monotonically decreasing");
CL_DEFUN T_sp cl__char_GT_(List_sp args) {
  return ((monotonic(1, 1, args)));
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("Return true if characters are monotonically non-decreasing");
CL_DEFUN T_sp cl__char_LE_(List_sp args) {
  return (Values(monotonic(-1, 0, args)));
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("Return true if characters are monotonically non-increasing");
CL_DEFUN T_mv cl__char_GE_(List_sp args) {
  return (Values(monotonic(1, 0, args)));
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("Return true if characters are monotonically increasing, ignore case");
CL_DEFUN T_mv cl__char_lessp(List_sp args) {
  return (Values(monotonic(-1, 1, args, false)));
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("Return true if characters are monotonically decreasing, ignore case");
CL_DEFUN T_mv cl__char_greaterp(List_sp args) {
  return (Values(monotonic(1, 1, args, false)));
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("Return true if characters are monotonically non-increasing, ignore case");
CL_DEFUN T_mv cl__char_not_greaterp(List_sp args) {
  return (Values(monotonic(-1, 0, args, false)));
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("Return true if characters are monotonically non-decreasing, ignore case");
CL_DEFUN T_sp cl__char_not_lessp(List_sp args) {
  return ((monotonic(1, 0, args, false)));
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("NE_");
CL_DEFUN T_sp cl__char_NE_(List_sp args) {
  while (args.notnilp()) {
    claspCharacter a = clasp_as_claspCharacter(gc::As<Character_sp>(oCar(args)));
    for (List_sp cur = oCdr(args); cur.notnilp(); cur = oCdr(cur)) {
      claspCharacter b = clasp_as_claspCharacter(gc::As<Character_sp>(oCar(cur)));
      if (a == b)
        return ((_Nil<T_O>()));
    }
    args = oCdr(args);
  }
  return ((_lisp->_true()));
}

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("EQ_");
CL_DEFUN T_sp cl__char_EQ_(List_sp args) {
  if (args.nilp())
    return ((_lisp->_true()));
  claspCharacter a = clasp_as_claspCharacter(gc::As<Character_sp>(oCar(args)));
  args = oCdr(args);
  while (args.notnilp()) {
    claspCharacter b = clasp_as_claspCharacter(gc::As<Character_sp>(oCar(args)));
    if (a != b)
      return ((_Nil<T_O>()));
    args = oCdr(args);
  }
  return ((_lisp->_true()));
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("Like char_NE_ but ignore case");
CL_DEFUN T_mv cl__char_not_equal(List_sp args) {
  while (args.notnilp()) {
    claspCharacter a = clasp_as_claspCharacter(gc::As<Character_sp>(oCar(args)));
    a = claspCharacter_upcase(a);
    for (List_sp cur = oCdr(args); cur.notnilp(); cur = oCdr(cur)) {
      claspCharacter b = clasp_as_claspCharacter(gc::As<Character_sp>(oCar(cur)));
      b = claspCharacter_upcase(b);
      if (a == b) return (Values(_Nil<T_O>()));
    }
    args = oCdr(args);
  }
  return Values(_lisp->_true());
}

bool clasp_charEqual2(T_sp x, T_sp y) {
  if (x.characterp() && y.characterp()) {
    claspCharacter cx = claspCharacter_upcase(x.unsafe_character());
    claspCharacter cy = claspCharacter_upcase(y.unsafe_character());
    return cx == cy;
  }
  return false;
}

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("Like char_EQ_, ignore case");
CL_DEFUN bool cl__char_equal(List_sp args) {
  if (args.nilp())
    return true;
  claspCharacter a = clasp_as_claspCharacter(gc::As<Character_sp>(oCar(args)));
  a = claspCharacter_upcase(a);
  args = oCdr(args);
  while (args.notnilp()) {
    claspCharacter b = clasp_as_claspCharacter(gc::As<Character_sp>(oCar(args)));
    b = claspCharacter_upcase(b);
    if (a != b) return false;
    args = oCdr(args);
  }
  return true;
};

#define LINE_FEED_CHAR 10
#define PAGE_CHAR 12
#define RETURN_CHAR 13
#define ESCAPE_CHAR 27

Character_sp clasp_character_create_from_name(string const &name) {
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
  else if (ssup == "ESCAPE")
    ch = clasp_make_standard_character(ESCAPE_CHAR);
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


const char* OrderedCharacterNames[] = {
    "NUL",
    "SOH", "STX", "ETX", "EOT",
    "ENQ", "ACK", "BEL", "BACKSPACE",
    "TAB", "NEWLINE", "VT",
    "PAGE", "RETURN", "SO", "SI",
    "DLE", "DC1", "DC2", "DC3",
    "DC4", "NAK", "SYN", "ETB",
    "CAN", "EM", "SUB", "ESC",
    "FS", "GS", "RS", "US",
    "SPACE", "EXCLAMATION_MARK", "QUOTATION_MARK", "NUMBER_SIGN",
    "DOLLAR_SIGN", "PERCENT_SIGN", "AMPERSAND", "APOSTROPHE",
    "LEFT_PARENTHESIS", "RIGHT_PARENTHESIS", "ASTERISK", "PLUS_SIGN",
    "COMMA", "HYPHEN-MINUS", "FULL_STOP", "SOLIDUS",
    "DIGIT_ZERO", "DIGIT_ONE", "DIGIT_TWO", "DIGIT_THREE",
    "DIGIT_FOUR", "DIGIT_FIVE", "DIGIT_SIX", "DIGIT_SEVEN",
    "DIGIT_EIGHT", "DIGIT_NINE", "COLON", "SEMICOLON",
    "LESS-THAN_SIGN", "EQUALS_SIGN", "GREATER-THAN_SIGN", "QUESTION_MARK",
    "COMMERCIAL_AT", "LATIN_CAPITAL_LETTER_A", "LATIN_CAPITAL_LETTER_B", "LATIN_CAPITAL_LETTER_C",
    "LATIN_CAPITAL_LETTER_D", "LATIN_CAPITAL_LETTER_E", "LATIN_CAPITAL_LETTER_F", "LATIN_CAPITAL_LETTER_G",
    "LATIN_CAPITAL_LETTER_H", "LATIN_CAPITAL_LETTER_I", "LATIN_CAPITAL_LETTER_J", "LATIN_CAPITAL_LETTER_K",
    "LATIN_CAPITAL_LETTER_L", "LATIN_CAPITAL_LETTER_M", "LATIN_CAPITAL_LETTER_N", "LATIN_CAPITAL_LETTER_O",
    "LATIN_CAPITAL_LETTER_P", "LATIN_CAPITAL_LETTER_Q", "LATIN_CAPITAL_LETTER_R", "LATIN_CAPITAL_LETTER_S",
    "LATIN_CAPITAL_LETTER_T", "LATIN_CAPITAL_LETTER_U", "LATIN_CAPITAL_LETTER_V", "LATIN_CAPITAL_LETTER_W",
    "LATIN_CAPITAL_LETTER_X", "LATIN_CAPITAL_LETTER_Y", "LATIN_CAPITAL_LETTER_Z", "LEFT_SQUARE_BRACKET",
    "REVERSE_SOLIDUS", "RIGHT_SQUARE_BRACKET", "CIRCUMFLEX_ACCENT", "LOW_LINE",
    "GRAVE_ACCENT", "LATIN_SMALL_LETTER_A", "LATIN_SMALL_LETTER_B", "LATIN_SMALL_LETTER_C",
    "LATIN_SMALL_LETTER_D", "LATIN_SMALL_LETTER_E", "LATIN_SMALL_LETTER_F", "LATIN_SMALL_LETTER_G",
    "LATIN_SMALL_LETTER_H", "LATIN_SMALL_LETTER_I", "LATIN_SMALL_LETTER_J", "LATIN_SMALL_LETTER_K",
    "LATIN_SMALL_LETTER_L", "LATIN_SMALL_LETTER_M", "LATIN_SMALL_LETTER_N", "LATIN_SMALL_LETTER_O",
    "LATIN_SMALL_LETTER_P", "LATIN_SMALL_LETTER_Q", "LATIN_SMALL_LETTER_R", "LATIN_SMALL_LETTER_S",
    "LATIN_SMALL_LETTER_T", "LATIN_SMALL_LETTER_U", "LATIN_SMALL_LETTER_V", "LATIN_SMALL_LETTER_W",
    "LATIN_SMALL_LETTER_X", "LATIN_SMALL_LETTER_Y", "LATIN_SMALL_LETTER_Z", "LEFT_CURLY_BRACKET",
    "VERTICAL_LINE", "RIGHT_CURLY_BRACKET", "TILDE", "RUBOUT"
};

void CharacterInfo::initialize() {
  int num_chars = sizeof(OrderedCharacterNames)/sizeof(OrderedCharacterNames[0]);
  this->gCharacterNames.resize(num_chars, _Nil<T_O>());
  this->gIndexedCharacters.resize(num_chars, _Nil<T_O>());
  for ( size_t fci=0; fci<num_chars; ++fci) {
    const char* name = OrderedCharacterNames[fci];
//    printf("%s:%d Adding char: %s  at: %d\n", __FILE__, __LINE__, name, fci);
    this->gNamesToCharacterIndex[name] = fci;
    this->gIndexedCharacters[fci] = clasp_make_standard_character((char)fci);
    this->gCharacterNames[fci] = SimpleBaseString_O::make(std::string(name));
  }
  gNamesToCharacterIndex["NULL"] = 0;
  gNamesToCharacterIndex["LINEFEED"] = 10;
  gNamesToCharacterIndex["ESCAPE"] = 27;
}

CL_LAMBDA(ch);
CL_DECLARE();
CL_DOCSTRING("See CLHS: standard_char_p");
CL_DEFUN bool cl__standard_char_p(Character_sp ch) {
  claspCharacter c = clasp_as_claspCharacter(ch);
  if (c>127) return false;
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
      || c == '$'                                               //      dollar sign, see ansi-test CL-TEST::STANDARD-CHAR-P.1     
      )
    return true;
  return false;
};

CL_LAMBDA(ch);
CL_DECLARE();
CL_DOCSTRING("alpha_char_p");
CL_DEFUN bool cl__alpha_char_p(Character_sp ch) {
  return isalpha(clasp_as_claspCharacter(ch));
};

Fixnum clasp_digitp(claspCharacter ch, int basis) {
  if (('0' <= ch) && (ch <= '9') && (ch < '0' + basis))
    return ch - '0';
  if (('A' <= ch) && (10 < basis) && (ch < 'A' + (basis - 10)))
    return ch - 'A' + 10;
  if (('a' <= ch) && (10 < basis) && (ch < 'a' + (basis - 10)))
    return ch - 'a' + 10;
  return -1;
}

CL_LAMBDA(c &optional (radix 10));
CL_DECLARE();
CL_DOCSTRING("digitCharP");
CL_DEFUN T_sp cl__digit_char_p(Character_sp c, Fixnum_sp radix) {
  Fixnum basis = unbox_fixnum(radix);
  if (basis < 2 || basis > 36) {
    QERROR_WRONG_TYPE_NTH_ARG(2, radix, Integer_O::makeIntegerType(2, 36));
  }
  Fixnum value = clasp_digitp(clasp_as_claspCharacter(c), basis);
  if (value < 0) return _Nil<T_O>();
  return make_fixnum(value);
};

CL_LAMBDA(sname);
CL_DECLARE();
CL_DOCSTRING("name_char");
CL_DEFUN T_mv cl__name_char(T_sp sname) {
  String_sp name = coerce::stringDesignator(sname);
  string upname = stringUpper(name->get());
  map<string, int>::const_iterator it = _lisp->characterInfo().gNamesToCharacterIndex.find(upname);
  if (it != _lisp->characterInfo().gNamesToCharacterIndex.end()) {
    return (Values(_lisp->characterInfo().gIndexedCharacters[it->second]));
  }
  return (Values(_Nil<T_O>()));
};

CL_LAMBDA(och);
CL_DECLARE();
CL_DOCSTRING("char_name");
CL_DEFUN SimpleBaseString_sp cl__char_name(Character_sp och) {
  claspCharacter ch = clasp_as_claspCharacter(och);
  if (ch<_lisp->characterInfo().gCharacterNames.size()) {
    return _lisp->characterInfo().gCharacterNames[ch];
  }
  SafeBuffer buffer;
  buffer._Buffer->fillPointerSet(0);
  buffer._Buffer->vectorPushExtend(clasp_make_character('U'));
  core__integer_to_string(buffer._Buffer,Integer_O::create((Fixnum)ch),clasp_make_fixnum(16));
  auto ret = SimpleBaseString_O::make(buffer._Buffer->length(),'\0',false,buffer._Buffer->length(), &(*buffer._Buffer)[0]);
  return ret;
};

CL_LAMBDA(och);
CL_DECLARE();
CL_DOCSTRING("char_code");
CL_DEFUN Fixnum_sp cl__char_code(Character_sp och) {
  claspCharacter ch = clasp_as_claspCharacter(och);
  if (ch < CHAR_CODE_LIMIT) {
    return make_fixnum((Fixnum)ch);
  }
  SIMPLE_ERROR(BF("Character is beyon CHAR_CODE_LIMIT: %d") % CHAR_CODE_LIMIT);
};

CL_LAMBDA(och);
CL_DECLARE();
CL_DOCSTRING("char_int");
CL_DEFUN Fixnum_sp cl__char_int(Character_sp och) {
  claspCharacter ch = clasp_as_claspCharacter(och);
  return make_fixnum((Fixnum)ch);
};

CL_LAMBDA(och);
CL_DECLARE();
CL_DOCSTRING("code_char");
CL_DEFUN Character_sp cl__code_char(Integer_sp ich) {
  int ii = clasp_to_int(ich);
  if (ii >= 0 && ii < CHAR_CODE_LIMIT) {
    return clasp_make_character(ii);
  }
  return _Nil<Character_O>();
};


// ----------------------------------------------------------------------
//



  SYMBOL_EXPORT_SC_(ClPkg, char_code);
  SYMBOL_EXPORT_SC_(ClPkg, code_char);
  SYMBOL_EXPORT_SC_(ClPkg, char_int);

  SYMBOL_EXPORT_SC_(ClPkg, name_char);
  SYMBOL_EXPORT_SC_(ClPkg, char_name);
  SYMBOL_EXPORT_SC_(ClPkg, alpha_char_p);
  SYMBOL_EXPORT_SC_(ClPkg, standard_char_p);
  SYMBOL_EXPORT_SC_(ClPkg, charUpcase);
  SYMBOL_EXPORT_SC_(ClPkg, charDowncase);
  SYMBOL_EXPORT_SC_(ClPkg, char_LT_);
  SYMBOL_EXPORT_SC_(ClPkg, char_GT_);
  SYMBOL_EXPORT_SC_(ClPkg, char_LE_);
  SYMBOL_EXPORT_SC_(ClPkg, char_GE_);
  SYMBOL_EXPORT_SC_(ClPkg, char_NE_);
  SYMBOL_EXPORT_SC_(ClPkg, char_EQ_);

  SYMBOL_EXPORT_SC_(ClPkg, charLessp);
  SYMBOL_EXPORT_SC_(ClPkg, charGreaterp);
  SYMBOL_EXPORT_SC_(ClPkg, charNotGreaterp);
  SYMBOL_EXPORT_SC_(ClPkg, charNotLessp);
  SYMBOL_EXPORT_SC_(ClPkg, charNotEqual);
  SYMBOL_EXPORT_SC_(ClPkg, charEqual);
  SYMBOL_EXPORT_SC_(ClPkg, digitCharP);



}; /* core */
