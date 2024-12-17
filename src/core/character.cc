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
// #define DEBUG_LEVEL_FULL

#pragma clang diagnostic push
// #pragma clang diagnostic ignored "-Wunused-local-typedef"
#include <boost/algorithm/string.hpp>
#pragma clang diagnostic pop
#include <clasp/core/foundation.h>
#include <clasp/core/primitives.h> // core__list_from_vaslist
#include <clasp/core/common.h>
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/character.h>
#include <clasp/core/numberToString.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/exceptions.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/evaluator.h>

namespace core {

#include "character-generated.cc"

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(Returns true if character is a graphic character other than space-like characters;
otherwise, returns false.)dx")
DOCGROUP(clasp);
CL_DEFUN bool core__printing_char_p(Character_sp c) { return printing_char_p(clasp_as_claspCharacter(c)); };

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(CLHS: graphic-char-p)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__graphic_char_p(Character_sp c) { return graphic_char_p(clasp_as_claspCharacter(c)); };

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(lower_case_p)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__lower_case_p(Character_sp c) { return lower_case_p(clasp_as_claspCharacter(c)); };

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(upper_case_p)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__upper_case_p(Character_sp c) { return upper_case_p(clasp_as_claspCharacter(c)); };

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(both_case_p)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__both_case_p(Character_sp c) { return both_case_p(clasp_as_claspCharacter(c)); };

CL_LAMBDA(char);
CL_DECLARE();
CL_DOCSTRING(R"dx(alphanumericp)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__alphanumericp(Character_sp ch) { return alphanumericp(clasp_as_claspCharacter(ch)); };

CL_LAMBDA(char);
CL_DECLARE();
CL_DOCSTRING(R"dx(charUpcase)dx");
DOCGROUP(clasp);
CL_DEFUN Character_sp cl__char_upcase(Character_sp ch) { return clasp_make_character(char_upcase(clasp_as_claspCharacter(ch))); };

CL_LAMBDA(char);
CL_DECLARE();
CL_DOCSTRING(R"dx(charDowncase)dx");
DOCGROUP(clasp);
CL_DEFUN Character_sp cl__char_downcase(Character_sp ch) {
  return clasp_make_character(char_downcase(clasp_as_claspCharacter(ch)));
};

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

// Do <=, <, whatever, based on s and t and the above.
bool character_comparison(int s, int t, Character_sp x, Character_sp y, bool preserve_case = true) {
  claspCharacter cx = clasp_as_claspCharacter(x);
  claspCharacter cy = clasp_as_claspCharacter(y);
  if (!preserve_case) {
    cx = char_upcase(cx);
    cy = char_upcase(cy);
  }
  int dir = s * claspCharacter_basic_compare(cx, cy);
  return !(dir < t);
}

bool monotonic(int s, int t, Vaslist_sp args, bool preserve_case = true) {
  Character_sp x = gc::As<Character_sp>(args->next_arg());
  Character_sp y;
  while (args->nargs() != 0) {
    y = gc::As<Character_sp>(args->next_arg());
    // If we find a false comparison we exit immediately.
    if (!(character_comparison(s, t, x, y, preserve_case)))
      return false;
    x = y;
  }
  return true;
};

CL_LAMBDA(char1 char2);
CL_DECLARE();
CL_DOCSTRING(R"dx(two-arg char<)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__two_arg_char_LT_(Character_sp char1, Character_sp char2) { return character_comparison(-1, 1, char1, char2); }

CL_LAMBDA(core:&va-rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return true if characters are monotonically increasing)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__char_LT_(Vaslist_sp args) {
  if (args->nargs_zero())
    PROGRAM_ERROR();
  else
    return monotonic(-1, 1, args);
};

CL_LAMBDA(char1 char2);
CL_DECLARE();
CL_DOCSTRING(R"dx(two-arg char>)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__two_arg_char_GT_(Character_sp char1, Character_sp char2) { return character_comparison(1, 1, char1, char2); }

CL_LAMBDA(core:&va-rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return true if characters are monotonically decreasing)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__char_GT_(Vaslist_sp args) {
  if (args->nargs_zero())
    PROGRAM_ERROR();
  else
    return monotonic(1, 1, args);
};

CL_LAMBDA(char1 char2);
CL_DECLARE();
CL_DOCSTRING(R"dx(two-arg char<=)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__two_arg_char_LE_(Character_sp char1, Character_sp char2) { return character_comparison(-1, 0, char1, char2); }

CL_LAMBDA(core:&va-rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return true if characters are monotonically non-decreasing)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__char_LE_(Vaslist_sp args) {
  if (args->nargs_zero())
    PROGRAM_ERROR();
  else
    return monotonic(-1, 0, args);
};

CL_LAMBDA(char1 char2);
CL_DECLARE();
CL_DOCSTRING(R"dx(two-arg char>=)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__two_arg_char_GE_(Character_sp char1, Character_sp char2) { return character_comparison(1, 0, char1, char2); }

CL_LAMBDA(core:&va-rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return true if characters are monotonically non-increasing)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__char_GE_(Vaslist_sp args) {
  if (args->nargs_zero())
    PROGRAM_ERROR();
  else
    return monotonic(1, 0, args);
};

CL_LAMBDA(char1 char2);
CL_DECLARE();
CL_DOCSTRING(R"dx(two-arg char-lessp)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__two_arg_char_lessp(Character_sp char1, Character_sp char2) {
  return character_comparison(-1, 1, char1, char2, false);
}

CL_LAMBDA(core:&va-rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return true if characters are monotonically increasing, ignore case)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__char_lessp(Vaslist_sp args) {
  if (args->nargs_zero())
    PROGRAM_ERROR();
  else
    return monotonic(-1, 1, args, false);
};

CL_LAMBDA(char1 char2);
CL_DECLARE();
CL_DOCSTRING(R"dx(two-arg char-greaterp)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__two_arg_char_greaterp(Character_sp char1, Character_sp char2) {
  return character_comparison(1, 1, char1, char2, false);
}

CL_LAMBDA(core:&va-rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return true if characters are monotonically decreasing, ignore case)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__char_greaterp(Vaslist_sp args) {
  if (args->nargs_zero())
    PROGRAM_ERROR();
  else
    return monotonic(1, 1, args, false);
};

CL_LAMBDA(char1 char2);
CL_DECLARE();
CL_DOCSTRING(R"dx(two-arg char-not-greaterp)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__two_arg_char_not_greaterp(Character_sp char1, Character_sp char2) {
  return character_comparison(-1, 0, char1, char2, false);
}

CL_LAMBDA(core:&va-rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return true if characters are monotonically non-increasing, ignore case)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__char_not_greaterp(Vaslist_sp args) {
  if (args->nargs_zero())
    PROGRAM_ERROR();
  else
    return monotonic(-1, 0, args, false);
};

CL_LAMBDA(char1 char2);
CL_DECLARE();
CL_DOCSTRING(R"dx(two-arg char-not-lessp)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__two_arg_char_not_lessp(Character_sp char1, Character_sp char2) {
  return character_comparison(1, 0, char1, char2, false);
}

CL_LAMBDA(core:&va-rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return true if characters are monotonically non-decreasing, ignore case)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__char_not_lessp(Vaslist_sp args) {
  if (args->nargs_zero())
    PROGRAM_ERROR();
  else
    return monotonic(1, 0, args, false);
};

CL_LAMBDA(core:&va-rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(NE_)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__char_NE_(Vaslist_sp args) {
  // Just like cl___NE_
  switch (args->nargs()) {
    /* I expect the order of likelihood is 2, 3, 1, >3, 0.
     * I don't think the compiler takes the order in a switch
     * very seriously, though, so it's just in order. */
  case 0:
    SIMPLE_PROGRAM_ERROR("CHAR/= needs at least 1 argument", nil<T_O>());
  case 1: {
    // the first arg needs to be a character - check that
    gc::As<Character_sp>(args->next_arg());
    return _lisp->_true();
  }
  case 2: {
    claspCharacter a = clasp_as_claspCharacter(gc::As<Character_sp>(args->next_arg()));
    claspCharacter b = clasp_as_claspCharacter(gc::As<Character_sp>(args->next_arg()));
    if (a == b)
      return nil<T_O>();
    else
      return _lisp->_true();
  }
  case 3: {
    claspCharacter a = clasp_as_claspCharacter(gc::As<Character_sp>(args->next_arg()));
    claspCharacter b = clasp_as_claspCharacter(gc::As<Character_sp>(args->next_arg()));
    claspCharacter c = clasp_as_claspCharacter(gc::As<Character_sp>(args->next_arg()));
    if ((a == b) || (a == c) || (b == c))
      return nil<T_O>();
    else
      return _lisp->_true();
  }
  default: {
    /* General case is a nested loop.
     * We're going to iterate over the arguments several times,
     * so a valist isn't going to cut it. */
    List_sp largs = core__list_from_vaslist(args);
    while (largs.notnilp()) {
      claspCharacter c1 = clasp_as_claspCharacter(gc::As<Character_sp>(oCar(largs)));
      for (List_sp cur = oCdr(largs); cur.notnilp(); cur = oCdr(cur)) {
        claspCharacter c2 = clasp_as_claspCharacter(gc::As<Character_sp>(oCar(cur)));
        if (c1 == c2)
          return nil<T_O>();
      }
      largs = oCdr(largs);
    }
    return _lisp->_true();
  }
  }
}

CL_LAMBDA(core:&va-rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(EQ_)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__char_EQ_(Vaslist_sp args) {
  switch (args->nargs()) {
  case 0:
    PROGRAM_ERROR();
  case 1: {
    gc::As<Character_sp>(args->next_arg());
    return _lisp->_true();
  }
  case 2: {
    claspCharacter a = clasp_as_claspCharacter(gc::As<Character_sp>(args->next_arg()));
    claspCharacter b = clasp_as_claspCharacter(gc::As<Character_sp>(args->next_arg()));
    if (a == b)
      return _lisp->_true();
    return nil<T_O>();
  }
  default: {
    claspCharacter a = clasp_as_claspCharacter(gc::As<Character_sp>(args->next_arg()));
    while (args->nargs()) {
      claspCharacter b = clasp_as_claspCharacter(gc::As<Character_sp>(args->next_arg()));
      if (a != b) {
        return ((nil<T_O>()));
      }
    }
    return _lisp->_true();
  }
  };
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(Like char_NE_ but ignore case)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__char_not_equal(List_sp args) {
  if (args.nilp())
    PROGRAM_ERROR();
  while (args.notnilp()) {
    claspCharacter a = clasp_as_claspCharacter(gc::As<Character_sp>(oCar(args)));
    a = char_upcase(a);
    for (List_sp cur = oCdr(args); cur.notnilp(); cur = oCdr(cur)) {
      claspCharacter b = clasp_as_claspCharacter(gc::As<Character_sp>(oCar(cur)));
      b = char_upcase(b);
      if (a == b)
        return (Values(nil<T_O>()));
    }
    args = oCdr(args);
  }
  return Values(_lisp->_true());
}

bool clasp_charEqual2(T_sp x, T_sp y) {
  if (x.characterp() && y.characterp()) {
    claspCharacter cx = char_upcase(x.unsafe_character());
    claspCharacter cy = char_upcase(y.unsafe_character());
    return cx == cy;
  }
  return false;
}

// FIXME: redundant with the above
CL_LAMBDA(char1 char2);
CL_DECLARE();
CL_DOCSTRING(R"dx(Two-arg char-equal)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__two_arg_char_equal(Character_sp x, Character_sp y) {
  claspCharacter cx = char_upcase(x.unsafe_character());
  claspCharacter cy = char_upcase(y.unsafe_character());
  return cx == cy;
}

CL_LAMBDA(core:&va-rest chars);
CL_DECLARE();
CL_DOCSTRING(R"dx(EQ_)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__char_equal(Vaslist_sp chars) {
  switch (chars->nargs()) {
  case 0:
    PROGRAM_ERROR();
  case 1: {
    gc::As<Character_sp>(chars->next_arg());
    return _lisp->_true();
  }
  case 2: {
    claspCharacter a = clasp_as_claspCharacter(gc::As<Character_sp>(chars->next_arg()));
    a = char_upcase(a);
    claspCharacter b = clasp_as_claspCharacter(gc::As<Character_sp>(chars->next_arg()));
    b = char_upcase(b);
    if (a == b)
      return _lisp->_true();
    return nil<T_O>();
  }
  default: {
    claspCharacter a = clasp_as_claspCharacter(gc::As<Character_sp>(chars->next_arg()));
    a = char_upcase(a);
    while (chars->nargs()) {
      claspCharacter b = clasp_as_claspCharacter(gc::As<Character_sp>(chars->next_arg()));
      b = char_upcase(b);
      if (a != b) {
        return ((nil<T_O>()));
      }
    }
    return _lisp->_true();
  }
  };
};

Character_sp clasp_character_create_from_name(string const& name) {
  Character_sp ch;
  string ssup = boost::to_upper_copy(name);
  if ((ssup == "TAB") || (ssup == "Tab"))
    ch = clasp_make_standard_character(TAB_CHAR);
  else if ((ssup == "NEWLINE") || (ssup == "Newline"))
    ch = clasp_make_standard_character(NEWLINE_CHAR);
  else if ((ssup == "LINEFEED") || (ssup == "Linefeed"))
    ch = clasp_make_standard_character(LINE_FEED_CHAR);
  else if ((ssup == "PAGE") || (ssup == "Page"))
    ch = clasp_make_standard_character(PAGE_CHAR);
  else if ((ssup == "RETURN") || (ssup == "Return"))
    ch = clasp_make_standard_character(RETURN_CHAR);
  else if ((ssup == "DEL") || (ssup == "Del"))
    ch = clasp_make_standard_character(RUBOUT_CHAR);
  else if ((ssup == "ESCAPE") || (ssup == "Escape"))
    ch = clasp_make_standard_character(ESCAPE_CHAR);
  else if ((ssup == "SPACE") || (ssup == "Space"))
    ch = clasp_make_standard_character(' ');
  else if ((ssup == "BACKSPACE") || (ssup == "Backspace"))
    ch = clasp_make_standard_character(BACKSPACE_CHAR);
  else if ((ssup == "RUBOUT") || (ssup == "Rubout"))
    ch = clasp_make_standard_character(RUBOUT_CHAR);
  else if ((ssup == "BELL") || (ssup == "Bell") || (ssup == "Bel"))
    ch = clasp_make_standard_character(BELL_CHAR);
  else {
    SIMPLE_ERROR("Unknown character name[{}]", ssup);
  }
  return ch;
}
}; // namespace core
namespace core {

const char* OrderedCharacterNames[] = {
    "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "Backspace", "Tab", "Newline", "VT", "Page", "Return", "SO", "SI",
    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US", "Space",
    "EXCLAMATION_MARK", "QUOTATION_MARK", "NUMBER_SIGN", "DOLLAR_SIGN", "PERCENT_SIGN", "AMPERSAND", "APOSTROPHE",
    "LEFT_PARENTHESIS", "RIGHT_PARENTHESIS", "ASTERISK", "PLUS_SIGN", "COMMA", "HYPHEN-MINUS", "FULL_STOP", "SOLIDUS", "DIGIT_ZERO",
    "DIGIT_ONE", "DIGIT_TWO", "DIGIT_THREE", "DIGIT_FOUR", "DIGIT_FIVE", "DIGIT_SIX", "DIGIT_SEVEN", "DIGIT_EIGHT", "DIGIT_NINE",
    "COLON", "SEMICOLON", "LESS-THAN_SIGN", "EQUALS_SIGN", "GREATER-THAN_SIGN", "QUESTION_MARK", "COMMERCIAL_AT",
    "LATIN_CAPITAL_LETTER_A", "LATIN_CAPITAL_LETTER_B", "LATIN_CAPITAL_LETTER_C", "LATIN_CAPITAL_LETTER_D",
    "LATIN_CAPITAL_LETTER_E", "LATIN_CAPITAL_LETTER_F", "LATIN_CAPITAL_LETTER_G", "LATIN_CAPITAL_LETTER_H",
    "LATIN_CAPITAL_LETTER_I", "LATIN_CAPITAL_LETTER_J", "LATIN_CAPITAL_LETTER_K", "LATIN_CAPITAL_LETTER_L",
    "LATIN_CAPITAL_LETTER_M", "LATIN_CAPITAL_LETTER_N", "LATIN_CAPITAL_LETTER_O", "LATIN_CAPITAL_LETTER_P",
    "LATIN_CAPITAL_LETTER_Q", "LATIN_CAPITAL_LETTER_R", "LATIN_CAPITAL_LETTER_S", "LATIN_CAPITAL_LETTER_T",
    "LATIN_CAPITAL_LETTER_U", "LATIN_CAPITAL_LETTER_V", "LATIN_CAPITAL_LETTER_W", "LATIN_CAPITAL_LETTER_X",
    "LATIN_CAPITAL_LETTER_Y", "LATIN_CAPITAL_LETTER_Z", "LEFT_SQUARE_BRACKET", "REVERSE_SOLIDUS", "RIGHT_SQUARE_BRACKET",
    "CIRCUMFLEX_ACCENT", "LOW_LINE", "GRAVE_ACCENT", "LATIN_SMALL_LETTER_A", "LATIN_SMALL_LETTER_B", "LATIN_SMALL_LETTER_C",
    "LATIN_SMALL_LETTER_D", "LATIN_SMALL_LETTER_E", "LATIN_SMALL_LETTER_F", "LATIN_SMALL_LETTER_G", "LATIN_SMALL_LETTER_H",
    "LATIN_SMALL_LETTER_I", "LATIN_SMALL_LETTER_J", "LATIN_SMALL_LETTER_K", "LATIN_SMALL_LETTER_L", "LATIN_SMALL_LETTER_M",
    "LATIN_SMALL_LETTER_N", "LATIN_SMALL_LETTER_O", "LATIN_SMALL_LETTER_P", "LATIN_SMALL_LETTER_Q", "LATIN_SMALL_LETTER_R",
    "LATIN_SMALL_LETTER_S", "LATIN_SMALL_LETTER_T", "LATIN_SMALL_LETTER_U", "LATIN_SMALL_LETTER_V", "LATIN_SMALL_LETTER_W",
    "LATIN_SMALL_LETTER_X", "LATIN_SMALL_LETTER_Y", "LATIN_SMALL_LETTER_Z", "LEFT_CURLY_BRACKET", "VERTICAL_LINE",
    "RIGHT_CURLY_BRACKET", "TILDE", "Rubout",
    // for the sake of a fast name-char, cover at least until #\UFF
    "U80", "U81", "U82", "U83", "U84", "U85", "U86", "U87", "U88", "U89", "U8A", "U8B", "U8C", "U8D", "U8E", "U8F", "U90", "U91",
    "U92", "U93", "U94", "U95", "U96", "U97", "U98", "U99", "U9A", "U9B", "U9C", "U9D", "U9E", "U9F", "NO-BREAK_SPACE",
    "INVERTED_EXCLAMATION_MARK", "CENT_SIGN", "POUND_SIGN", "CURRENCY_SIGN", "YEN_SIGN", "BROKEN_BAR", "SECTION_SIGN", "DIAERESIS",
    "COPYRIGHT_SIGN", "FEMININE_ORDINAL_INDICATOR", "LEFT-POINTING_DOUBLE_ANGLE_QUOTATION_MARK", "NOT_SIGN", "SOFT_HYPHEN",
    "REGISTERED_SIGN", "MACRON", "DEGREE_SIGN", "PLUS-MINUS_SIGN", "SUPERSCRIPT_TWO", "SUPERSCRIPT_THREE", "ACUTE_ACCENT",
    "MICRO_SIGN", "PILCROW_SIGN", "MIDDLE_DOT", "CEDILLA", "SUPERSCRIPT_ONE", "MASCULINE_ORDINAL_INDICATOR",
    "RIGHT-POINTING_DOUBLE_ANGLE_QUOTATION_MARK", "VULGAR_FRACTION_ONE_QUARTER", "VULGAR_FRACTION_ONE_HALF",
    "VULGAR_FRACTION_THREE_QUARTERS", "INVERTED_QUESTION_MARK", "LATIN_CAPITAL_LETTER_A_WITH_GRAVE",
    "LATIN_CAPITAL_LETTER_A_WITH_ACUTE", "LATIN_CAPITAL_LETTER_A_WITH_CIRCUMFLEX", "LATIN_CAPITAL_LETTER_A_WITH_TILDE",
    "LATIN_CAPITAL_LETTER_A_WITH_DIAERESIS", "LATIN_CAPITAL_LETTER_A_WITH_RING_ABOVE", "LATIN_CAPITAL_LETTER_AE",
    "LATIN_CAPITAL_LETTER_C_WITH_CEDILLA", "LATIN_CAPITAL_LETTER_E_WITH_GRAVE", "LATIN_CAPITAL_LETTER_E_WITH_ACUTE",
    "LATIN_CAPITAL_LETTER_E_WITH_CIRCUMFLEX", "LATIN_CAPITAL_LETTER_E_WITH_DIAERESIS", "LATIN_CAPITAL_LETTER_I_WITH_GRAVE",
    "LATIN_CAPITAL_LETTER_I_WITH_ACUTE", "LATIN_CAPITAL_LETTER_I_WITH_CIRCUMFLEX", "LATIN_CAPITAL_LETTER_I_WITH_DIAERESIS",
    "LATIN_CAPITAL_LETTER_ETH", "LATIN_CAPITAL_LETTER_N_WITH_TILDE", "LATIN_CAPITAL_LETTER_O_WITH_GRAVE",
    "LATIN_CAPITAL_LETTER_O_WITH_ACUTE", "LATIN_CAPITAL_LETTER_O_WITH_CIRCUMFLEX", "LATIN_CAPITAL_LETTER_O_WITH_TILDE",
    "LATIN_CAPITAL_LETTER_O_WITH_DIAERESIS", "MULTIPLICATION_SIGN", "LATIN_CAPITAL_LETTER_O_WITH_STROKE",
    "LATIN_CAPITAL_LETTER_U_WITH_GRAVE", "LATIN_CAPITAL_LETTER_U_WITH_ACUTE", "LATIN_CAPITAL_LETTER_U_WITH_CIRCUMFLEX",
    "LATIN_CAPITAL_LETTER_U_WITH_DIAERESIS", "LATIN_CAPITAL_LETTER_Y_WITH_ACUTE", "LATIN_CAPITAL_LETTER_THORN",
    "LATIN_SMALL_LETTER_SHARP_S", "LATIN_SMALL_LETTER_A_WITH_GRAVE", "LATIN_SMALL_LETTER_A_WITH_ACUTE",
    "LATIN_SMALL_LETTER_A_WITH_CIRCUMFLEX", "LATIN_SMALL_LETTER_A_WITH_TILDE", "LATIN_SMALL_LETTER_A_WITH_DIAERESIS",
    "LATIN_SMALL_LETTER_A_WITH_RING_ABOVE", "LATIN_SMALL_LETTER_AE", "LATIN_SMALL_LETTER_C_WITH_CEDILLA",
    "LATIN_SMALL_LETTER_E_WITH_GRAVE", "LATIN_SMALL_LETTER_E_WITH_ACUTE", "LATIN_SMALL_LETTER_E_WITH_CIRCUMFLEX",
    "LATIN_SMALL_LETTER_E_WITH_DIAERESIS", "LATIN_SMALL_LETTER_I_WITH_GRAVE", "LATIN_SMALL_LETTER_I_WITH_ACUTE",
    "LATIN_SMALL_LETTER_I_WITH_CIRCUMFLEX", "LATIN_SMALL_LETTER_I_WITH_DIAERESIS", "LATIN_SMALL_LETTER_ETH",
    "LATIN_SMALL_LETTER_N_WITH_TILDE", "LATIN_SMALL_LETTER_O_WITH_GRAVE", "LATIN_SMALL_LETTER_O_WITH_ACUTE",
    "LATIN_SMALL_LETTER_O_WITH_CIRCUMFLEX", "LATIN_SMALL_LETTER_O_WITH_TILDE", "LATIN_SMALL_LETTER_O_WITH_DIAERESIS",
    "DIVISION_SIGN", "LATIN_SMALL_LETTER_O_WITH_STROKE", "LATIN_SMALL_LETTER_U_WITH_GRAVE", "LATIN_SMALL_LETTER_U_WITH_ACUTE",
    "LATIN_SMALL_LETTER_U_WITH_CIRCUMFLEX", "LATIN_SMALL_LETTER_U_WITH_DIAERESIS", "LATIN_SMALL_LETTER_Y_WITH_ACUTE",
    "LATIN_SMALL_LETTER_THORN", "LATIN_SMALL_LETTER_Y_WITH_DIAERESIS"};

void CharacterInfo::initialize() {
  int num_chars = sizeof(OrderedCharacterNames) / sizeof(OrderedCharacterNames[0]);
  this->_NamesToCharacterIndex = HashTable_O::createEqual();
  this->gCharacterNames.resize(num_chars, nil<T_O>());
  this->gIndexedCharacters.resize(num_chars, nil<T_O>());
  for (size_t fci = 0; fci < num_chars; ++fci) {
    const char* name = OrderedCharacterNames[fci];

    // printf("%s:%d Adding char: %s  at: %d\n", __FILE__, __LINE__, name,(int) fci);
    //  we later compare with Uppercase
    SimpleBaseString_sp sname = SimpleBaseString_O::make(stringUpper(name));
    this->_NamesToCharacterIndex->setf_gethash(sname, make_fixnum(fci));
    // If fci is cast to char, the following code fails from fci = 128
    this->gIndexedCharacters[fci] = clasp_make_standard_character((claspCharacter)fci);
    // but want the names in the correct case
    this->gCharacterNames[fci] = SimpleBaseString_O::make(std::string(name));
  }
  // we later compare with Uppercase
  this->_NamesToCharacterIndex->setf_gethash(SimpleBaseString_O::make("NULL"), make_fixnum(NULL_CHAR));
  this->_NamesToCharacterIndex->setf_gethash(SimpleBaseString_O::make("BELL"), make_fixnum(BELL_CHAR));
  this->_NamesToCharacterIndex->setf_gethash(SimpleBaseString_O::make("BS"), make_fixnum(BACKSPACE_CHAR));
  this->_NamesToCharacterIndex->setf_gethash(SimpleBaseString_O::make("HT"), make_fixnum(TAB_CHAR));
  this->_NamesToCharacterIndex->setf_gethash(SimpleBaseString_O::make("LF"), make_fixnum(LINE_FEED_CHAR));
  this->_NamesToCharacterIndex->setf_gethash(SimpleBaseString_O::make("LINEFEED"), make_fixnum(LINE_FEED_CHAR));
  this->_NamesToCharacterIndex->setf_gethash(SimpleBaseString_O::make("FF"), make_fixnum(PAGE_CHAR));
  this->_NamesToCharacterIndex->setf_gethash(SimpleBaseString_O::make("CR"), make_fixnum(RETURN_CHAR));
  this->_NamesToCharacterIndex->setf_gethash(SimpleBaseString_O::make("ESCAPE"), make_fixnum(ESCAPE_CHAR));
  this->_NamesToCharacterIndex->setf_gethash(SimpleBaseString_O::make("SP"), make_fixnum(SPACE_CHAR));
  this->_NamesToCharacterIndex->setf_gethash(SimpleBaseString_O::make("DEL"), make_fixnum(RUBOUT_CHAR));
}

CL_LAMBDA(ch);
CL_DECLARE();
CL_DOCSTRING(R"dx(See CLHS: standard_char_p)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__standard_char_p(Character_sp ch) {
  // Complete list is laid out in CLHS 2.1.3: Standard Characters.
  claspCharacter c = clasp_as_claspCharacter(ch);
  if (c > 127)
    return false;
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
  if (c == '!'     //      exclamation mark
      || c == '$'  //      dollar sign
      || c == '"'  //      quotation mark, or double quote
      || c == '\'' //      apostrophe, or [single] quote
      || c == '('  //      left parenthesis, or open parenthesis
      || c == ')'  //      right parenthesis, or close parenthesis
      || c == ','  //      comma
      || c == '_'  //      low line, or underscore
      || c == '-'  //      hyphen, or minus [sign]
      || c == '.'  //      full stop, period, or dot
      || c == '/'  //      solidus, or slash
      || c == ':'  //      colon
      || c == ';'  //      semicolon
      || c == '?'  //      question mark
      || c == '+'  //      plus [sign]
      || c == '<'  //      less-than [sign]
      || c == '='  //      equals [sign]
      || c == '>'  //      greater-than [sign]
      || c == '#'  //      number sign, or sharp[sign]
      || c == '%'  //      percent [sign]
      || c == '&'  //      ampersand
      || c == '*'  //      asterisk, or star
      || c == '@'  //      commercial at, or at-sign
      || c == '['  //      left [square] bracket
      || c == '\\' //      reverse solidus, or backslash
      || c == ']'  //      right [square] bracket
      || c == '{'  //      left curly bracket, or left brace
      || c == '|'  //      vertical bar
      || c == '}'  //      right curly bracket, or right brace
      || c == '`'  //      grave accent, or backquote
      || c == '^'  //      circumflex accent
      || c == '~'  //      tilde
  )
    return true;
  return false;
};

CL_LAMBDA(ch);
CL_DECLARE();
CL_DOCSTRING(R"dx(alpha_char_p)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__alpha_char_p(Character_sp ch) { return alpha_char_p(clasp_as_claspCharacter(ch)); };

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
CL_DOCSTRING(R"dx(digitCharP)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__digit_char_p(Character_sp c, Fixnum_sp radix) {
  Fixnum basis = unbox_fixnum(radix);
  if (basis < 2 || basis > 36) {
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_digitCharP, 2, radix, Integer_O::makeIntegerType(2, 36));
  }
  Fixnum value = clasp_digitp(clasp_as_claspCharacter(c), basis);
  if (value < 0)
    return nil<T_O>();
  return make_fixnum(value);
};

int unicodeHex2int(std::string aString) {
  // parses "<U>(HexDigits)+"
  // Does not fail on "U(HexDigits)+<Space> [any Garbage]", e.g. "U100 Garbage"
  unsigned long sizeInput = aString.size();
  if (sizeInput <= 1)
    return -1;
  if (aString.at(0) == 'U') {
    std::string bString = aString.substr(1, sizeInput - 1);
    long n = strtol(bString.c_str(), NULL, 16);
    if ((n == 0) || (n == LONG_MAX) || (n == LONG_MIN)) {
      return -1;
    } else {
      return (int)n;
    }
  } else
    return -1;
}

CL_LAMBDA(sname);
CL_DECLARE();
CL_DOCSTRING(R"dx(name_char)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__name_char(T_sp sname) {
  String_sp name = coerce::stringDesignator(sname);
  String_sp upname = cl__string_upcase(name);
  T_sp it = _lisp->characterInfo()._NamesToCharacterIndex->gethash(upname);
  if (it.fixnump()) {
    return _lisp->characterInfo().gIndexedCharacters[it.unsafe_fixnum()];
  }
  // The upper exclusive bound on the value returned by the function char-code.
  // Treat U100 until U110000 -1 to be consistent with char-name
  claspCharacter conversion = unicodeHex2int(upname->get_std_string());
  if ((conversion >= 0) && (conversion < CHAR_CODE_LIMIT))
    return (Values(clasp_make_standard_character(conversion)));
  else
    return nil<T_O>();
};

CL_LAMBDA(och);
CL_DECLARE();
CL_DOCSTRING(R"dx(char_name)dx");
DOCGROUP(clasp);
CL_DEFUN SimpleBaseString_sp cl__char_name(Character_sp och) {
  claspCharacter ch = clasp_as_claspCharacter(och);
  if (ch < _lisp->characterInfo().gCharacterNames.size()) {
    return gc::As<SimpleBaseString_sp>(_lisp->characterInfo().gCharacterNames[ch]);
  }
  SafeBufferStr8Ns buffer;
  buffer._Buffer->fillPointerSet(0);
  buffer._Buffer->vectorPushExtend('U');
  core__integer_to_string(buffer._Buffer, Integer_O::create((Fixnum)ch), clasp_make_fixnum(16));
  auto ret = SimpleBaseString_O::make(buffer._Buffer->length(), '\0', false, buffer._Buffer->length(), &(*buffer._Buffer)[0]);
  return ret;
};

DOCGROUP(clasp);
CL_DEFUN T_sp core__all_characters() { return _lisp->characterInfo()._NamesToCharacterIndex->keysAsCons(); }
CL_LAMBDA(och);
CL_DECLARE();
CL_DOCSTRING(R"dx(char_code)dx");
DOCGROUP(clasp);
CL_DEFUN Fixnum_sp cl__char_code(Character_sp och) {
  claspCharacter ch = clasp_as_claspCharacter(och);
  return make_fixnum((Fixnum)ch);
};

CL_LAMBDA(och);
CL_DECLARE();
CL_DOCSTRING(R"dx(char_int)dx");
DOCGROUP(clasp);
CL_DEFUN Fixnum_sp cl__char_int(Character_sp och) {
  claspCharacter ch = clasp_as_claspCharacter(och);
  return make_fixnum((Fixnum)ch);
};

CL_LAMBDA(och);
CL_DECLARE();
CL_DOCSTRING(R"dx(code_char)dx");
DOCGROUP(clasp);
CL_DEFUN Character_sp cl__code_char(Integer_sp ich) {
  int ii = clasp_to_int(ich);
  if (ii >= 0 && ii < CHAR_CODE_LIMIT) {
    return clasp_make_character(ii);
  }
  return nil<Character_O>();
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

}; // namespace core
