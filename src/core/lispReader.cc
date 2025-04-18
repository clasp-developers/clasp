/*
    File: lispReader.cc
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
#pragma clang diagnostic push
// #pragma clang diagnostic ignored "-Wunused-local-typedef"
#include <boost/algorithm/string.hpp>
#pragma clang diagnostic pop
#include <string>
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/pathname.h>
#include <clasp/core/bformat.h>
#include <clasp/core/character.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/numbers.h>
#include <clasp/core/arguments.h>
#include <clasp/core/package.h>
#include <clasp/core/lispDefinitions.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/array.h>
#include <clasp/core/cons.h>
// #include "lisp_ParserExtern.h"
#include <clasp/core/lispReader.h>
#include <clasp/core/readtable.h>
#include <clasp/core/wrappers.h>

#if 0
FILE* fDebug = NULL;
#define LOG_READ_SETUP()                                                                                                           \
  if (!fDebug) {                                                                                                                   \
    fDebug = fopen("/tmp/read.log", "w");                                                                                          \
  }
#define LOG_READ(fmt)                                                                                                              \
  fprintf(fDebug, "%s:%d ", __FILE__, __LINE__);                                                                                   \
  fprintf(fDebug, "%s\n", (fmt).str().c_str());                                                                                    \
  fflush(fDebug);
#else
#define LOG_READ_SETUP()
#define LOG_READ(fmt)
#endif
namespace core {
// Store characters and flags about the characters

typedef Fixnum trait_chr_type;

struct Token {
  SourcePosInfo_sp sourcePosInfo;
  vector<trait_chr_type> chars;
  void clear() { this->chars.clear(); };
  void recordSourcePos(SourcePosInfo_sp sp) { this->sourcePosInfo = sp; }
  trait_chr_type* data() { return this->chars.data(); };
  void push_back(trait_chr_type c) { this->chars.push_back(c); };
  size_t size() const { return this->chars.size(); };

  trait_chr_type& operator[](int i) { return this->chars[i]; };
  const trait_chr_type& operator[](int i) const { return this->chars[i]; };
};

#define TRAIT_DIGIT 0x000100000000
#define TRAIT_ALPHABETIC 0x000200000000
#define TRAIT_PACKAGEMARKER 0x000400000000
#define TRAIT_ALPHADIGIT 0x000800000000
#define TRAIT_PLUSSIGN 0x001000000000
#define TRAIT_MINUSSIGN 0x002000000000
#define TRAIT_DOT 0x004000000000
#define TRAIT_DECIMALPOINT 0x008000000000
#define TRAIT_RATIOMARKER 0x010000000000
#define TRAIT_EXPONENTMARKER 0x020000000000
#define TRAIT_INVALID 0x040000000000
#define TRAIT_ESCAPED 0x080000000000
#define TRAIT_MASK 0x0FFF00000000
#define CHAR_MASK 0x0000FFFFFFFF
#define CHR(x) (static_cast<claspCharacter>((x)&CHAR_MASK))
#define CHR_MATCH(x, m) (CHR(x) == (m))
#define TRAIT_MATCH_ANY(x, t) (((x) & (t)) != 0)
#define CHR_TRAIT_MATCH_ANY(x, t, m) (TRAIT_MATCH_ANY(x, t) && CHR_MATCH(x, m))
#define TRAIT_MATCH_EQ(x, t)

#if 1
#define TRAP_BAD_CONS(x)
#else
#define TRAP_BAD_CONS(x)                                                                                                           \
  {                                                                                                                                \
    if ((x).consp()) {                                                                                                             \
      LOG_READ(BF("About to try trap bad cons"));                                                                                  \
      string ssss = core__file_scope(x)->fileName();                                                                               \
    }                                                                                                                              \
  }
#endif

SYMBOL_EXPORT_SC_(ClPkg, STARread_suppressSTAR);

// -----------------------------------------------------------------
// -----------

/*! Return a uint that combines the character x with its character TRAITs
      See CLHS 2.1.4.2 */
trait_chr_type constituentChar(Character_sp ch, trait_chr_type trait = 0) {
  claspCharacter x = ch.unsafe_character();
  ASSERT(x < CHAR_MASK);
  if (trait != 0)
    return (x | trait);
  trait_chr_type result = 0;
  trait_chr_type read_base = unbox_fixnum(gc::As<Fixnum_sp>(cl::_sym_STARread_baseSTAR->symbolValue()));
  ASSERT(read_base >= 2 && read_base <= 36);
  if (x >= '0' && x <= '9') {
    trait_chr_type uix = x - '0';
    if (uix < read_base) {
      result = (TRAIT_DIGIT | x);
      return result;
    }
    return (TRAIT_ALPHABETIC | x);
  }
  if (x >= 'a' && x <= 'z') {
    trait_chr_type uix = x - 'a' + 10;
    if (uix < read_base) {
      result = (TRAIT_DIGIT | x);
      goto LETTER;
    }
    result = (TRAIT_ALPHABETIC | x);
    goto LETTER;
  }
  if (x >= 'A' && x <= 'Z') {
    trait_chr_type uix = x - 'A' + 10;
    if (uix < read_base) {
      result = (TRAIT_DIGIT | x);
      goto LETTER;
    }
    result = (TRAIT_ALPHABETIC | x);
    goto LETTER;
  }
  if (x == ':')
    return (TRAIT_PACKAGEMARKER | x);
  if (x == '+')
    return (TRAIT_PLUSSIGN | x);
  if (x == '-')
    return (TRAIT_MINUSSIGN | x);
  if (x == '.')
    return (TRAIT_DOT | TRAIT_DECIMALPOINT | x);
  if (x == '/')
    return (TRAIT_RATIOMARKER | x);
  if (x > ' ' && x < 127)
    return (TRAIT_ALPHABETIC | x);
  // 2.1.4.2 Constituent Traits specifies the invalid traits
  // Backspace Tab Newline Linefeed Page Return Space Rubout
  // Nothing is said about other characters not in the list
  // We treat them as TRAIT_ALPHABETIC.
  if (x == BACKSPACE_CHAR || x == TAB_CHAR || x == NEWLINE_CHAR || x == LINE_FEED_CHAR || x == PAGE_CHAR || x == RETURN_CHAR ||
      x == ' ' || x == RUBOUT_CHAR)
    return (TRAIT_INVALID | x);
  else
    return (TRAIT_ALPHABETIC | x);
LETTER:
  if (x == 'd' || x == 'D' || x == 'e' || x == 'E' || x == 'f' || x == 'F' || x == 's' || x == 'S' || x == 'l' || x == 'L')
    result |= TRAIT_EXPONENTMARKER;
  return result;
}

T_sp constituentCharAsFixnum(Character_sp ch, trait_chr_type trait = 0) {
  trait_chr_type ct = constituentChar(ch, trait);
  return core::make_fixnum((Fixnum)ct);
}

// -----------------------------------------------------------
//
// Read symbols for reader macros #: and #\
//

/*! See SACLA reader.lisp::read-ch */
T_sp read_ch(T_sp sin) {
  T_sp nc = cl__read_char(sin, nil<T_O>(), nil<T_O>(), _lisp->_true());
  return nc;
}

/*! See SACLA reader.lisp::read-ch-or-die */
Character_sp read_ch_or_die(T_sp sin) {
  Character_sp nc = gc::As_unsafe<Character_sp>(cl__read_char(sin, _lisp->_true(), nil<T_O>(), _lisp->_true()));
  return nc;
}

/*! See SACLA reader.lisp::unread-ch */
void unread_ch(T_sp sin, Character_sp c) { stream_unread_char(sin, clasp_as_claspCharacter(c)); }

/*! See SACLA reader.lisp::collect-escaped-lexemes */
List_sp collect_escaped_lexemes(Character_sp c, T_sp sin) {
  T_sp readTable = _lisp->getCurrentReadTable();
  Symbol_sp syntax_type = core__syntax_type(readTable, c);
  if (syntax_type == kw::_sym_invalid) {
    SIMPLE_ERROR("invalid-character-error: {}", _rep_(c));
  } else if (syntax_type == kw::_sym_multiple_escape) {
    return nil<T_O>();
  } else if (syntax_type == kw::_sym_single_escape) {
    return Cons_O::create(constituentCharAsFixnum(read_ch_or_die(sin), TRAIT_ESCAPED),
                          collect_escaped_lexemes(read_ch_or_die(sin), sin));
  } else if (syntax_type == kw::_sym_constituent || syntax_type == kw::_sym_whitespace ||
             syntax_type == kw::_sym_terminating_macro || syntax_type == kw::_sym_non_terminating_macro) {
    return Cons_O::create(constituentCharAsFixnum(c, TRAIT_ESCAPED), collect_escaped_lexemes(read_ch_or_die(sin), sin));
  }
  return nil<T_O>();
}

/*! See SACLA reader.lisp::collect-lexemes.
    Set extended to true if a non base-char is seen.*/
List_sp collect_lexemes(/*Character_sp*/ T_sp tc, T_sp sin) {
  if (tc.notnilp()) {
    Character_sp c = gc::As<Character_sp>(tc);
    T_sp readTable = _lisp->getCurrentReadTable();
    Symbol_sp syntax_type = core__syntax_type(readTable, c);
    if (syntax_type == kw::_sym_invalid) {
      SIMPLE_ERROR("invalid-character-error: {}", _rep_(c));
    } else if (syntax_type == kw::_sym_whitespace) {
      if (_sym_STARpreserve_whitespace_pSTAR->symbolValue().isTrue()) {
        unread_ch(sin, c);
      }
    } else if (syntax_type == kw::_sym_terminating_macro) {
      unread_ch(sin, c);
    } else if (syntax_type == kw::_sym_multiple_escape) {
      return Cons_O::create(collect_escaped_lexemes(read_ch_or_die(sin), sin), collect_lexemes(read_ch(sin), sin));
    } else if (syntax_type == kw::_sym_single_escape) {
      return Cons_O::create(Cons_O::create(constituentCharAsFixnum(read_ch_or_die(sin)), nil<T_O>()),
                            collect_lexemes(read_ch(sin), sin));
    } else if (syntax_type == kw::_sym_constituent || syntax_type == kw::_sym_non_terminating_macro) {
      return Cons_O::create(constituentCharAsFixnum(c), collect_lexemes(read_ch(sin), sin));
    }
  }
  return nil<T_O>();
}

typedef enum { undefined, up, down, mixed } UnEscapedCase;

UnEscapedCase case_state(Fixnum c, UnEscapedCase curCase) {
  if (TRAIT_ESCAPED & c)
    return curCase;
  c = CHR(c); // strip traits
  switch (curCase) {
  case mixed:
    // nothing
    break;
  case undefined:
    if (upper_case_p(c)) {
      return up;
    } else if (lower_case_p(c)) {
      return down;
    }
    break;
  case up:
    if (lower_case_p(c))
      return mixed;
    break;
  case down:
    if (upper_case_p(c))
      return mixed;
  };
  return curCase;
}

UnEscapedCase check_case(List_sp cur_char, UnEscapedCase curCase) {
  while (cur_char.notnilp()) {
    T_sp obj = oCar(cur_char);
    if (obj.consp()) {
      return check_case(obj, curCase);
    } else if (obj.fixnump()) {
      curCase = case_state(obj.unsafe_fixnum(), curCase);
    } else if (obj.nilp()) {
      return curCase;
    }
    cur_char = oCdr(cur_char);
  }
  return curCase;
}

Character_sp lexeme_character(T_sp lexeme) {
  if (lexeme.fixnump()) {
    return core::clasp_make_character(CHR(lexeme.unsafe_fixnum()));
  }
  SIMPLE_ERROR("Unknown lexeme {}", _rep_(lexeme));
}

void make_str_upcase(StrNs_sp sout, List_sp cur_char) {
  while (cur_char.notnilp()) {
    T_sp obj = oCar(cur_char);
    if (obj.consp()) {
      make_str_upcase(sout, obj);
    } else if (obj.fixnump()) {
      if (obj.unsafe_fixnum() & TRAIT_ESCAPED)
        sout->vectorPushExtend(core::clasp_make_character(CHR(obj.unsafe_fixnum())));
      else
        sout->vectorPushExtend(core::clasp_make_character(char_upcase(CHR(obj.unsafe_fixnum()))));
    } else if (obj.nilp()) {
      // Handling name ||   do nothing
    }
    cur_char = oCdr(cur_char);
  }
}

void make_str_downcase(StrNs_sp sout, List_sp cur_char) {
  while (cur_char.notnilp()) {
    T_sp obj = oCar(cur_char);
    if (obj.consp()) {
      make_str_downcase(sout, obj);
    } else if (obj.fixnump()) {
      if (obj.unsafe_fixnum() & TRAIT_ESCAPED)
        sout->vectorPushExtend(core::clasp_make_character(CHR(obj.unsafe_fixnum())));
      else
        sout->vectorPushExtend(core::clasp_make_character(char_downcase(CHR(obj.unsafe_fixnum()))));
    } else if (obj.nilp()) {
      // Handling name ||   do nothing
    }
    cur_char = oCdr(cur_char);
  }
}

void make_str_preserve_case(StrNs_sp sout, List_sp cur_char) {
  while (cur_char.notnilp()) {
    T_sp obj = oCar(cur_char);
    if (obj.consp()) {
      make_str_preserve_case(sout, obj);
    } else if (obj.fixnump()) {
      sout->vectorPushExtend(core::clasp_make_character(CHR(obj.unsafe_fixnum())));
    } else if (obj.nilp()) {
      // Handling name ||   do nothing
    }
    cur_char = oCdr(cur_char);
  }
}

/*! Works like SACLA readtable::make-str but accumulates the characters
      into a stringstream */
void make_str(StrNs_sp sout, List_sp cur_char) {
  T_sp readtable = _lisp->getCurrentReadTable();
  Symbol_sp case_ = cl__readtable_case(readtable);
  if (case_ == kw::_sym_invert) {
    UnEscapedCase strcase = check_case(cur_char, undefined);
    switch (strcase) {
    case undefined:
    case mixed:
      make_str_preserve_case(sout, cur_char);
      return;
    case up:
      make_str_downcase(sout, cur_char);
      return;
    case down:
      make_str_upcase(sout, cur_char);
      return;
    }
    UNREACHABLE();
  } else if (case_ == kw::_sym_upcase) {
    make_str_upcase(sout, cur_char);
  } else if (case_ == kw::_sym_downcase) {
    make_str_downcase(sout, cur_char);
  } else if (case_ == kw::_sym_preserve) {
    make_str_preserve_case(sout, cur_char);
  } else {
    SIMPLE_ERROR("Bad readtable case {}", _rep_(case_));
  }
}

string fix_exponent_char(const char* cur) {
  stringstream ss;
  while (*cur) {
    if (alpha_char_p(*cur)) {
      switch (*cur) {
      case 'd':
      case 'D':
      case 'e':
      case 'E':
      case 'f':
      case 'F':
      case 'l':
      case 'L':
      case 's':
      case 'S':
        ss << "E";
        break;
      default:
        SIMPLE_ERROR("Illegal exponent character[%c]", *cur);
        break;
      }
    } else
      ss << (*cur);
    ++cur;
  }
  return ss.str();
}

typedef enum {
  tstart,
  tsyms,
  tsymf,
  tsymr,
  tsymx,
  tsymy,
  tsymdot,
  tsymz,
  tsymk,
  tsymkw,
  tsymbad,
  tsyme,
  tsymex,
  tsymp,
  tsympv,
  tintt,
  tintp,
  tratio,
  tfloat0,
  tfloate,
  tfloatp
} TokenState;

string stateString(TokenState state) {
  switch (state) {
  case tstart:
    return "tstart";
  case tsyms:
    return "tsyms";
  case tsymf:
    return "tsymf";
  case tsymr:
    return "tsymr";
  case tsymx:
    return "tsymx";
  case tsymy:
    return "tsymy";
  case tsymdot:
    return "tsymdot";
  case tsymz:
    return "tsymz";
  case tsymk:
    return "tsymk";
  case tsymkw:
    return "tsymkw";
  case tsymbad:
    return "tsymbad";
  case tsyme:
    return "tsyme";
  case tsymex:
    return "tsymex";
  case tsymp:
    return "tsymp";
  case tsympv:
    return "tsympv";
  case tintt:
    return "tintt";
  case tintp:
    return "tintp";
  case tratio:
    return "tratio";
  case tfloat0:
    return "tfloat0";
  case tfloate:
    return "tfloate";
  case tfloatp:
    return "tfloatp";
  };
  return "no-state";
}

}; // namespace core

template <typename Char> struct fmt::formatter<core::TokenState, Char> : fmt::formatter<fmt::basic_string_view<Char>> {
  template <typename FormatContext>
  auto format(const core::TokenState& o, FormatContext& ctx) const -> typename FormatContext::iterator {
    return fmt::formatter<fmt::basic_string_view<Char>>::format(stateString(o), ctx);
  }
};

namespace core {

typedef enum { undefined_exp, double_float_exp, float_exp, single_float_exp, long_float_exp, short_float_exp } FloatExponentType;

UnEscapedCase token_check_case(Token& token, size_t start, size_t end) {
  UnEscapedCase curcase = undefined;
  for (size_t i(start); i < end; ++i) {
    curcase = case_state(token[i], curcase);
  }
  return curcase;
}

void token_upcase(Token& token, size_t start, size_t end) {
  for (size_t i(start); i < end; ++i) {
    if (!(token[i] & TRAIT_ESCAPED))
      token[i] = (TRAIT_MASK & token[i]) | char_upcase(CHR(token[i]));
  }
}
void token_downcase(Token& token, size_t start, size_t end) {
  for (size_t i(start); i < end; ++i) {
    if (!(token[i] & TRAIT_ESCAPED))
      token[i] = (TRAIT_MASK & token[i]) | char_downcase(CHR(token[i]));
  }
}

void apply_readtable_case(Token& token, size_t start, size_t end) {
  T_sp readtable = _lisp->getCurrentReadTable();
  Symbol_sp case_ = cl__readtable_case(readtable);
  if (case_ == kw::_sym_invert) {
    UnEscapedCase strcase = token_check_case(token, start, end);
    switch (strcase) {
    case undefined:
    case mixed:
      return;
    case up:
      token_downcase(token, start, end);
      return;
    case down:
      token_upcase(token, start, end);
      return;
    }
    UNREACHABLE();
  } else if (case_ == kw::_sym_upcase) {
    token_upcase(token, start, end);
  } else if (case_ == kw::_sym_downcase) {
    token_downcase(token, start, end);
  } else if (case_ == kw::_sym_preserve) {
    return;
  } else {
    SIMPLE_ERROR("Bad readtable case {}", _rep_(case_));
  }
}

SimpleString_sp symbolTokenStr(T_sp stream, Token& token, size_t start, size_t end, bool only_dots_ok = false) {
  SafeBufferStrWNs buffer;
  apply_readtable_case(token, start, end);
  bool only_dots = true;
  if ((end - start) == 0) {
    printf("%s:%d The symbolTokenStr is empty\n", __FILE__, __LINE__);
  }
  for (size_t i = start, iEnd(end); i < iEnd; ++i) {
    if (TRAIT_MATCH_ANY(token[i], TRAIT_INVALID))
      READER_ERROR(SimpleBaseString_O::make("A char with trait invalid was encountered by the reader."), nil<T_O>(), stream);
    claspCharacter c = CHR(token[i]);
    if (c != '.')
      only_dots = false;
    buffer.string()->vectorPushExtend(CHR(token[i]));
  }
  if ((end - start) > 0) {
    if (only_dots) {
      if (!only_dots_ok) {
        READER_ERROR(SimpleBaseString_O::make("A string of dots was encountered by the reader."), nil<T_O>(), stream);
      }
    }
  }
  return buffer.string()->asMinimalSimpleString();
}

SimpleString_sp tokenStr(T_sp stream, const Token& token, size_t start = 0, size_t end = UNDEF_UINT, bool only_dots_ok = false) {
  if (end == UNDEF_UINT)
    end = token.size();
  SafeBufferStrWNs buffer;
  bool only_dots = true;
  if ((end - start) == 0) {
    printf("%s:%d The tokenStr is empty\n", __FILE__, __LINE__);
  }
  for (size_t i = start, iEnd(end); i < iEnd; ++i) {
    claspCharacter c = CHR(token[i]);
    if (c != '.')
      only_dots = false;
    buffer.string()->vectorPushExtend(CHR(token[i]));
  }
  if ((end - start) > 0) {
    if (only_dots) {
      if (!only_dots_ok) {
        READER_ERROR(SimpleBaseString_O::make("A string of dots was encountered by the reader."), nil<T_O>(), stream);
      }
    }
  }
  return buffer.string()->asMinimalSimpleString();
}

T_sp interpret_token_or_throw_reader_error(T_sp sin, Token& token, bool only_dots_ok) {
  LOG_READ(BF("About to interpret_token_or_throw_reader_error"));
  ASSERTF(token.size() > 0, "The token is empty!");
  const trait_chr_type* start = token.data();
  const trait_chr_type* cur = start;
  const trait_chr_type* end = token.data() + token.size();
  const trait_chr_type* package_marker = start;
  const trait_chr_type* name_marker = start;
  TokenState state = tstart;
  FloatExponentType exponent = undefined_exp;
#define EXPTEST(c, x)                                                                                                              \
  {                                                                                                                                \
    if (CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'D') || CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'd')) {                  \
      exponent = double_float_exp;                                                                                                 \
      state = x;                                                                                                                   \
      goto NEXT;                                                                                                                   \
    }                                                                                                                              \
    if (CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'E') || CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'e')) {                  \
      exponent = float_exp;                                                                                                        \
      state = x;                                                                                                                   \
      goto NEXT;                                                                                                                   \
    }                                                                                                                              \
    if (CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'F') || CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'f')) {                  \
      exponent = single_float_exp;                                                                                                 \
      state = x;                                                                                                                   \
      goto NEXT;                                                                                                                   \
    }                                                                                                                              \
    if (CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'L') || CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'l')) {                  \
      exponent = long_float_exp;                                                                                                   \
      state = x;                                                                                                                   \
      goto NEXT;                                                                                                                   \
    }                                                                                                                              \
    if (CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'S') || CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 's')) {                  \
      exponent = short_float_exp;                                                                                                  \
      state = x;                                                                                                                   \
      goto NEXT;                                                                                                                   \
    }                                                                                                                              \
  }
#define TEST(c, x)                                                                                                                 \
  if (c) {                                                                                                                         \
    state = x;                                                                                                                     \
    goto NEXT;                                                                                                                     \
  }
#define TEST_CMD(c, cmd, x)                                                                                                        \
  if (c) {                                                                                                                         \
    cmd;                                                                                                                           \
    state = x;                                                                                                                     \
    goto NEXT;                                                                                                                     \
  }
#define ELSE(x)                                                                                                                    \
  {                                                                                                                                \
    state = x;                                                                                                                     \
    goto NEXT;                                                                                                                     \
  }
  while (cur != end) {
    switch (state) {
    case tstart:
      TEST(CHR_TRAIT_MATCH_ANY(*cur, TRAIT_PLUSSIGN, '+') || CHR_TRAIT_MATCH_ANY(*cur, TRAIT_MINUSSIGN, '-'), tsyms);
      TEST(TRAIT_MATCH_ANY(*cur, TRAIT_DIGIT), tintt);
      TEST(CHR_TRAIT_MATCH_ANY(*cur, TRAIT_DOT | TRAIT_DECIMALPOINT, '.'), tsymdot);
      TEST_CMD(CHR_TRAIT_MATCH_ANY(*cur, TRAIT_PACKAGEMARKER, ':'), package_marker = cur, tsymk);
      ELSE(tsymz);
    case tsyms:
      TEST(CHR_TRAIT_MATCH_ANY(*cur, TRAIT_DOT | TRAIT_DECIMALPOINT, '.'), tsymf);
      TEST(TRAIT_MATCH_ANY(*cur, TRAIT_DIGIT), tintt);
      ELSE(tsymz);
    case tsymf:
      TEST(TRAIT_MATCH_ANY(*cur, TRAIT_DIGIT), tfloat0);
      ELSE(tsymz);
    case tintt:
      TEST(TRAIT_MATCH_ANY(*cur, TRAIT_DIGIT), tintt);
      TEST(CHR_TRAIT_MATCH_ANY(*cur, TRAIT_DOT | TRAIT_DECIMALPOINT, '.'), tintp);
      TEST(CHR_TRAIT_MATCH_ANY(*cur, TRAIT_RATIOMARKER, '/'), tsymr);
      EXPTEST(*cur, tsymx);
      ELSE(tsymz);
    case tintp:
      TEST(TRAIT_MATCH_ANY(*cur, TRAIT_DIGIT), tfloat0);
      EXPTEST(*cur, tsymx); // Handle things like 1.e-3 as floats
      ELSE(tsymz);
    case tfloat0:
      TEST(TRAIT_MATCH_ANY(*cur, TRAIT_DIGIT), tfloat0);
      EXPTEST(*cur, tsymx);
      ELSE(tsymz);
    case tsymr:
      TEST(TRAIT_MATCH_ANY(*cur, TRAIT_DIGIT), tratio);
      ELSE(tsymz);
    case tratio:
      TEST(TRAIT_MATCH_ANY(*cur, TRAIT_DIGIT), tratio);
      ELSE(tsymz);
    case tsymx:
      TEST(TRAIT_MATCH_ANY(*cur, TRAIT_DIGIT), tfloate);
      TEST(CHR_TRAIT_MATCH_ANY(*cur, TRAIT_PLUSSIGN, '+') || CHR_TRAIT_MATCH_ANY(*cur, TRAIT_MINUSSIGN, '-'), tsymy);
      ELSE(tsymz);
    case tsymy:
      TEST(TRAIT_MATCH_ANY(*cur, TRAIT_DIGIT), tfloate);
      ELSE(tsymz);
    case tfloate:
      TEST(TRAIT_MATCH_ANY(*cur, TRAIT_DIGIT), tfloate);
      ELSE(tsymz);
    case tsymdot:
      TEST(TRAIT_MATCH_ANY(*cur, TRAIT_DIGIT), tfloatp);
      ELSE(tsymz);
    case tfloatp:
      TEST(TRAIT_MATCH_ANY(*cur, TRAIT_DIGIT), tfloatp);
      EXPTEST(*cur, tsymx);
      ELSE(tsymz);
    case tsymz:
      TEST_CMD(CHR_TRAIT_MATCH_ANY(*cur, TRAIT_PACKAGEMARKER, ':'), package_marker = cur, tsyme);
      ELSE(tsymz);
    case tsyme:
      TEST(CHR_TRAIT_MATCH_ANY(*cur, TRAIT_PACKAGEMARKER, ':'), tsymp);
      name_marker = cur;
      ELSE(tsymex);
    case tsymex:
      TEST(CHR_TRAIT_MATCH_ANY(*cur, TRAIT_PACKAGEMARKER, ':'), tsymbad);
      ELSE(tsymex);
    case tsymp:
      TEST(CHR_TRAIT_MATCH_ANY(*cur, TRAIT_PACKAGEMARKER, ':'), tsymbad);
      name_marker = cur;
      ELSE(tsympv);
    case tsympv:
      TEST(CHR_TRAIT_MATCH_ANY(*cur, TRAIT_PACKAGEMARKER, ':'), tsymbad);
      ELSE(tsympv);
    case tsymk:
      TEST(CHR_TRAIT_MATCH_ANY(*cur, TRAIT_PACKAGEMARKER, ':'), tsymbad);
      name_marker = cur;
      ELSE(tsymkw);
    case tsymkw:
      TEST(CHR_TRAIT_MATCH_ANY(*cur, TRAIT_PACKAGEMARKER, ':'), tsymbad);
      ELSE(tsymkw);
    case tsymbad:
      ELSE(tsymbad);
    default:
      SIMPLE_ERROR("unhandled state[{}] for token assignment", state);
    }
  NEXT:
    ++cur;
  }
  switch (state) {
  case tstart: {
    if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue())
      return nil<T_O>();
    SimpleString_sp sym_name = SimpleBaseString_O::make("");
    Symbol_sp sym = _lisp->getCurrentPackage()->intern(sym_name);
    return sym;
  }
    SIMPLE_ERROR("There was no token!!!!");
  case tsymdot:
    LOG_READ(BF("Returning sym_dot"));
    return _sym_dot;
  case tsyms:
  case tsymf:
  case tsymr:
  case tsymx:
  case tsymy:
  case tsymz:
    // interpret symbols in current package
    {
      if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue())
        return nil<T_O>();
      SimpleString_sp sym_name = symbolTokenStr(sin, token, name_marker - token.data(), token.size(), only_dots_ok);
      Symbol_sp sym = _lisp->getCurrentPackage()->intern(sym_name);
      LOG_READ(BF("sym_name = |%s| sym->symbolNameAsString() = |%s|") % _rep_(sym_name) % sym->symbolNameAsString());
      return sym;
    }
    break;
  case tsymex:
  case tsympv: {
    if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue())
      return nil<T_O>();
    // interpret good package:name symbols
    // Get package part
    SafeBufferStrWNs packageSin;
    cur = start;
    apply_readtable_case(token, 0, package_marker - start);
    while (cur != package_marker) {
      packageSin.string()->vectorPushExtend(CHR(*cur));
      ++cur;
    }
    int separator(0);
    while (cur != name_marker) {
      ++separator;
      ++cur;
    }
    // TODO Handle proper string names
    SimpleString_sp symbol_name_str = symbolTokenStr(sin, token, name_marker - token.data(), token.size(), only_dots_ok);
    LOG_READ(BF("Interpreting token as packageName[%s] and symbol-name[%s]") % _rep_(packageSin.string()) %
             _rep_(symbol_name_str));
    Package_sp pkg = gc::As<Package_sp>(_lisp->findPackage(packageSin.string(), true));
    Symbol_sp sym;
    MultipleValues& mvn = core::lisp_multipleValues();
    if (separator == 1) { // Asking for external symbol
      Symbol_mv symmv = pkg->findSymbol_SimpleString(symbol_name_str);
      sym = symmv;
      T_sp status = mvn.second(symmv.number_of_values());
      if (status != kw::_sym_external) {
        READER_ERROR(SimpleBaseString_O::make("Cannot find the external symbol ~a in ~a"), Cons_O::createList(symbol_name_str, pkg),
                     sin);
      }
    } else {
      sym = pkg->intern(symbol_name_str);
    }
    ASSERT(sym);
    return sym;
  } break;
  case tsymkw: {
    if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue())
      return nil<T_O>();
    // interpret good keywords
    LOG_READ(BF("Token[%s] interpreted as keyword") % name_marker);
    // :\. is a valid keyword symbol, so allow only dots here
    SimpleString_sp keyword_name = symbolTokenStr(sin, token, name_marker - token.data(), token.size(), true);
    return _lisp->keywordPackage()->intern(keyword_name);
  } break;
  case tsymk: {
    if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue())
      return nil<T_O>();
    // interpret :|| keyword
    LOG_READ(BF("Token[:||] interpreted as keyword"));
    SimpleBaseString_sp keyword_name = SimpleBaseString_O::make(""); // tokenStr(sin,token, name_marker - token.data());
    return _lisp->keywordPackage()->intern(keyword_name);
  } break;
  case tsyme:
  case tsymp:
  case tsymbad:
    if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue())
      return nil<T_O>();
    // interpret failed symbols
    SIMPLE_ERROR("Error encountered while reading source file {} at character position {} - Could not interpret symbol state({}) "
                 "symbol: [{}]",
                 _rep_(stream_pathname(sin)), _rep_(stream_position(sin)), stateString(state),
                 symbolTokenStr(sin, token, start - token.data(), token.size())->get_std_string());
    break;
  case tintt:
  case tintp:
    // interpret ints
    {
      ASSERT(cl::_sym_STARread_baseSTAR->symbolValue().fixnump());
      int read_base = cl::_sym_STARread_baseSTAR->symbolValue().unsafe_fixnum();
      ASSERT(read_base >= 2 && read_base <= 36);
      SimpleString_sp ssnum = tokenStr(sin, token, start - token.data());
      string num = ssnum->get_std_string();
      if (num[0] == '+')
        num = num.substr(1, num.size());
      try {
        if (num[num.size() - 1] == '.') {
          return Integer_O::create(num.substr(0, num.size() - 1));
        } else {
          return Integer_O::create(num, read_base);
        }
      } catch (std::invalid_argument& arg) {
        SIMPLE_ERROR("Problem in mpz_class creation with {} error: {}", num, arg.what());
      }
      SIMPLE_ERROR("Problem while interpreting int from {} in reader", num);
    }
    break;
  case tratio: {
    // interpret ratio
    SimpleString_sp sRatioStr = tokenStr(sin, token, start - token.data());
    std::string ratioStr = sRatioStr->get_std_string();
    if (ratioStr[0] == '+')
      ratioStr = ratioStr.substr(1, ratioStr.size() - 1);
    vector<string> parts = split(ratioStr.c_str(), "/");
    ASSERT(parts.size() == 2);
    Integer_sp num = Integer_O::create(parts[0]);
    Integer_sp denom = Integer_O::create(parts[1]);
    return Rational_O::create(num, denom);
    break;
  }
  case tfloat0:
  case tfloate:
  case tfloatp:
    // interpret float
    {
      switch (exponent) {
      case undefined_exp: {
        char* lastValid = NULL;
        if (cl::_sym_STARreadDefaultFloatFormatSTAR->symbolValue() == cl::_sym_single_float) {
          string numstr = tokenStr(sin, token, start - token.data())->get_std_string();
          float f = ::strtof(numstr.c_str(), &lastValid);
          return clasp_make_single_float(f);
        } else if (cl::_sym_STARreadDefaultFloatFormatSTAR->symbolValue() == cl::_sym_double_float) {
          string numstr = tokenStr(sin, token, start - token.data())->get_std_string();
          double d = ::strtod(numstr.c_str(), &lastValid);
          return DoubleFloat_O::create(d);
        } else if (cl::_sym_STARreadDefaultFloatFormatSTAR->symbolValue() == cl::_sym_short_float) {
          string numstr = tokenStr(sin, token, start - token.data())->get_std_string();
          float f = ::strtof(numstr.c_str(), &lastValid);
          return clasp_make_single_float(f); // ShortFloat_O::create(f) crashes
        } else if (cl::_sym_STARreadDefaultFloatFormatSTAR->symbolValue() == cl::_sym_long_float) {
          string numstr = tokenStr(sin, token, start - token.data())->get_std_string();
          long_float_t l = ::strtod(numstr.c_str(), &lastValid);
          return LongFloat_O::create(l);
        } else {
          SIMPLE_ERROR("Handle *read-default-float-format* of {}", _rep_(cl::_sym_STARreadDefaultFloatFormatSTAR->symbolValue()));
        }
      }
      case float_exp: {
        char* lastValid = NULL;
        string numstr = fix_exponent_char(tokenStr(sin, token, start - token.data())->get_std_string().c_str());
        double d = ::strtod(numstr.c_str(), &lastValid);
        return DoubleFloat_O::create(d);
      }
      case short_float_exp: {
        char* lastValid = NULL;
        string numstr = fix_exponent_char(tokenStr(sin, token, start - token.data())->get_std_string().c_str());
        double d = ::strtod(numstr.c_str(), &lastValid);
        return ShortFloat_O::create(d);
      }
      case single_float_exp: {
        char* lastValid = NULL;
        string numstr = fix_exponent_char(tokenStr(sin, token, start - token.data())->get_std_string().c_str());
        double d = ::strtod(numstr.c_str(), &lastValid);
        return clasp_make_single_float(d);
      }
      case double_float_exp: {
        char* lastValid = NULL;
        string numstr = fix_exponent_char(tokenStr(sin, token, start - token.data())->get_std_string().c_str());
        double d = ::strtod(numstr.c_str(), &lastValid);
        return DoubleFloat_O::create(d);
      }
      case long_float_exp: {
        char* lastValid = NULL;
        string numstr = fix_exponent_char(tokenStr(sin, token, start - token.data())->get_std_string().c_str());
#ifdef CLASP_LONG_FLOAT
        long_float_t d = ::strtold(numstr.c_str(), &lastValid);
        return LongFloat_O::create(d);
#else
        double d = ::strtod(numstr.c_str(), &lastValid);
        return DoubleFloat_O::create(d);
#endif
      }
      }
      SIMPLE_ERROR("Shouldn't get here - unhandled exponent type");
    }
  }
  LOG_READ(BF("Bad state %d") % state);
  SIMPLE_ERROR("create<ReaderError_O>(sin));");
}

#if 0
    (defun read-list (char &optional (stream *standard-input*)
		      &key allow-consing-dot)
     (let ((*consing-dot-allowed* allow-consing-dot)
	   c stack values list)
      (loop
       (setq c (peek-char t stream t nil t))
       (when (char= char c)           ; found the closing parenthesis.
	(when (eq (first stack) *consing-dot*)
	 (error "Nothing appears after . in list."))
	(read-char stream t nil t)
	(setq list (if (eq (second stack) *consing-dot*)
                        (nreconc (cddr stack) (first stack))
			    (nreverse stack)))
	(return))
       (when (setq values (multiple-value-list (lisp-object? stream t nil t)))
	(if (eq (second stack) *consing-dot*)
	    (error "More than one object follows . in list.")
		(push (car values) stack))))
      list))
#endif

List_sp read_list(T_sp sin, claspCharacter end_char, bool allow_consing_dot) {
  bool got_dotted = false;
  T_sp dotted_object = nil<T_O>();
  Cons_sp first = Cons_O::create(nil<T_O>(), nil<T_O>());
  List_sp cur = first;
  while (1) {
    Character_sp cp = gc::As<Character_sp>(cl__peek_char(_lisp->_true(), sin, _lisp->_true(), nil<Character_O>(), _lisp->_true()));
    LOG_READ(BF("read_list ---> peeked char[%s]") % _rep_(cp));
    if (clasp_as_claspCharacter(cp) == end_char) {
      cl__read_char(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
      if (dotted_object.notnilp()) {
        cur.asCons()->setCdr(dotted_object);
        List_sp result = oCdr(first);
        TRAP_BAD_CONS(result);
        if (result.nilp())
          return (Values(nil<T_O>()));
        return (Values(result));
      }
      List_sp otherResult = oCdr(first);
      TRAP_BAD_CONS(otherResult);
      if (otherResult.nilp())
        return (Values(nil<T_O>()));
      return (otherResult);
    }
    int ivalues;
    T_sp obj;
    T_mv mv = lisp_object_query(sin, true, nil<T_O>(), true);
    ivalues = mv.number_of_values();
    if (ivalues > 0) {
      obj = mv;
      LOG_READ(BF("lisp_object_query: %s") % _rep_(obj));
      // Could get ending lineNumber and column here
      if (obj == _sym_dot) {
        if (allow_consing_dot) {
          got_dotted = true;
          Character_sp cdotp =
              gc::As<Character_sp>(cl__peek_char(_lisp->_true(), sin, _lisp->_true(), nil<Character_O>(), _lisp->_true()));
          if (clasp_as_claspCharacter(cdotp) == end_char) {
            READER_ERROR(SimpleBaseString_O::make("Nothing after consing dot"), nil<T_O>(), sin);
          }
          dotted_object = read_lisp_object(sin, true, nil<T_O>(), true);
        } else {
          READER_ERROR(SimpleBaseString_O::make("An illegal dot was encountered by the reader."), nil<T_O>(), sin);
        }
      } else {
        if (got_dotted) {
          READER_ERROR(SimpleBaseString_O::make("More than one object after consing dot"), nil<T_O>(), sin);
        }
        Cons_sp one = Cons_O::create(obj, nil<T_O>());
        LOG_READ(BF("One = %s") % _rep_(one));
        //        LOG_READ(BF("one->sourceFileInfo()=%s") % _rep_(core__file_scope(one)));
        //        LOG_READ(BF("one->sourceFileInfo()->fileName()=%s") % core__file_scope(one)->fileName());
        //        LOG_READ(BF("one->sourceFileInfo()->fileName().c_str() = %s") % core__file_scope(one)->fileName().c_str());
        TRAP_BAD_CONS(one);
        cur.asCons()->setCdr(one);
        cur = one;
      }
    }
  }
}

SYMBOL_SC_(CorePkg, STARsharp_equal_final_tableSTAR);

T_sp read_lisp_object(T_sp sin, bool eofErrorP, T_sp eofValue, bool recursiveP) {
  LOG_READ_SETUP();
  LOG_READ(BF("Entered read_lisp_object recursiveP=%d") % recursiveP);
  T_sp result = nil<T_O>();
  if (recursiveP) {
    while (1) {
      LOG_READ(BF("At top of while loop"));
      T_mv mv = lisp_object_query(sin, eofErrorP, eofValue, recursiveP);
      int ivalues = mv.number_of_values();
      if (ivalues > 0) {
        result = mv;
        if (result == _sym_dot) {
          READER_ERROR(SimpleBaseString_O::make("An illegal dot was encountered by the reader."), nil<T_O>(), sin);
        }
        if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
          LOG_READ(BF("read_suppress == true"));
          result = nil<T_O>();
        }
        break;
      }
    }
  } else {
    DynamicScopeManager scope(_sym_STARsharp_equal_final_tableSTAR, nil<T_O>());
    LOG_READ(BF("About to call read_lisp_object"));
    result = read_lisp_object(sin, eofErrorP, eofValue, true);
    LOG_READ(BF("Came out of read_lisp_object with: |%s|") % _rep_(result).c_str());
  }
  LOG_READ(BF("Returning from read_lisp_object"));
  return (result);
}

/*!
      Read a character from the stream and based on what it is continue to process the
      stream until a complete symbol/number of macro is processed.
      Return the result in a MultipleValues object - if it is empty then nothing was read */
T_mv lisp_object_query(T_sp sin, bool eofErrorP, T_sp eofValue, bool recursiveP) {
#if 0
  static int monitorReaderStep = 0;
  if ((monitorReaderStep % 1000) == 0 && cl__member(_sym_monitorReader, _sym_STARdebugMonitorSTAR->symbolValue(), nil<T_O>()).notnilp()) {
    printf("%s:%d:%s stream %s -> pos = %" PRF "\n", __FILE__, __LINE__, __FUNCTION__, _rep_(stream_pathname(sin)).c_str(), unbox_fixnum(gc::As<Fixnum_sp>(stream_position(sin))));
  }
  ++monitorReaderStep;
#endif
  bool only_dots_ok = false;
  Token token;
  T_sp readTable = _lisp->getCurrentReadTable();
  Character_sp xxx, y, z, X, Y, Z;
/* See the CLHS 2.2 Reader Algorithm  - continue has the effect of jumping to step 1 */
step1:
  LOG_READ(BF("step1"));
  SourcePosInfo_sp xxxsp;
  T_sp tx = cl__read_char(sin, nil<T_O>(), nil<T_O>(), _lisp->_true());
  if (tx.nilp()) {
    if (eofErrorP)
      STREAM_ERROR(sin);
    return Values(eofValue);
  }
  xxx = gc::As<Character_sp>(tx);
  LOG_READ(BF("Read character x[%d/%s]") % (int)clasp_as_claspCharacter(xxx) % (char)clasp_as_claspCharacter(xxx));
  Symbol_sp xxx_syntax_type = core__syntax_type(readTable, xxx);
  //    step2:
  if (xxx_syntax_type == kw::_sym_invalid) {
    LOG_READ(BF("step2 - invalid-character[%c]") % clasp_as_claspCharacter(xxx));
    READER_ERROR(SimpleBaseString_O::make("A char with syntax type invalid was encountered by the reader."), nil<T_O>(), sin);
  }
  //    step3:
  if (xxx_syntax_type == kw::_sym_whitespace) {
    LOG_READ(BF("step3 - whitespace character[%c/%d]") % clasp_as_claspCharacter(xxx) % clasp_as_claspCharacter(xxx));
    goto step1;
  }
  //    step4:
  if ((xxx_syntax_type == kw::_sym_terminating_macro) || (xxx_syntax_type == kw::_sym_non_terminating_macro)) {
    LOG_READ(BF("step4 - terminating-macro-character or non-terminating-macro-character char[%c]") % clasp_as_claspCharacter(xxx));
    T_sp reader_macro;
    reader_macro = cl__get_macro_character(xxx, readTable);
    ASSERT(reader_macro.notnilp());
    if (gc::IsA<Symbol_sp>(reader_macro)) {
      // At startup symbols that define reader macro functions aren't fbound yet
      // We need to read the lambda lists somehow - so hard code the reader macro calls
      Symbol_sp sreader_macro = gc::As_unsafe<Symbol_sp>(reader_macro);
      if (!sreader_macro->fboundp()) {
        if (clasp_as_claspCharacter(xxx) == '(') {
          return core__reader_list_allow_consing_dot(sin, xxx);
        } else if (clasp_as_claspCharacter(xxx) == '"') {
          return core__reader_double_quote_string(sin, xxx);
        } else if (clasp_as_claspCharacter(xxx) == '\'') {
          return core__reader_quote(sin, xxx);
        }
        printf("%s:%d Handle character '%c' in lisp_object_query\n", __FILE__, __LINE__, clasp_as_claspCharacter(xxx));
      }
    }
    T_mv results = eval::funcall(reader_macro, sin, xxx);
    if (results.number_of_values() == 0) {
      return results;
    }
    T_sp object = results;
    return object;
  }
  //    step5:
  if (xxx_syntax_type == kw::_sym_single_escape) {
    LOG_READ(BF("step5 - single-escape-character char[%c]") % clasp_as_claspCharacter(xxx));
    LOG_READ(BF("Handling single escape"));
    T_sp ty = cl__read_char(sin, _lisp->_true(), nil<T_O>(), _lisp->_true());
    if (!ty.characterp()) {
      SIMPLE_ERROR("Expected character - hit end");
    }
    y = gc::As<Character_sp>(ty);
    token.clear();
    token.push_back(constituentChar(y, TRAIT_ALPHABETIC));
    LOG_READ(BF("Read y[%d/%s]") % (int)clasp_as_claspCharacter(y) % clasp_as_claspCharacter(y));
    goto step8;
  }
  //    step6:
  if (xxx_syntax_type == kw::_sym_multiple_escape) {
    LOG_READ(BF("step6 - multiple-escape-character char[%c]") % clasp_as_claspCharacter(xxx));
    LOG_READ(BF("Handling multiple escape - clearing token"));
    token.clear();
    // |....| or ....|| or ..|.|.. is ok
    only_dots_ok = true;
    goto step9;
  }
  //    step7:
  if (xxx_syntax_type /*readTable->syntax_type(xxx)*/ == kw::_sym_constituent) {
    LOG_READ(BF("step7 - Handling constituent-character char[%s]") % _rep_(xxx));
    token.clear();
    // X = readTable->convert_case(x);
    X = xxx; // convert case once the entire token is accumulated
    token.push_back(constituentChar(X));
  }
step8:
  LOG_READ(BF("step8"));
  {
    T_sp ty = cl__read_char(sin, nil<T_O>(), nil<T_O>(), _lisp->_true());
    if (ty.nilp()) {
      LOG_READ(BF("Hit eof"));
      goto step10;
    }
    Character_sp y(gc::As_unsafe<Character_sp>(ty));
    LOG_READ(BF("Step8: Read y[%s/%c]") % clasp_as_claspCharacter(y) % (char)clasp_as_claspCharacter(y));
    Symbol_sp y8_syntax_type = core__syntax_type(readTable, y);
    LOG_READ(BF("y8_syntax_type=%s") % _rep_(y8_syntax_type));
    if ((y8_syntax_type == kw::_sym_constituent) || (y8_syntax_type == kw::_sym_non_terminating_macro)) {
      // Y = readTable->convert_case(y);
      Y = y; // convert case once the entire token is accumulated
      LOG_READ(BF("  Pushing back character %d") % constituentChar(Y));
      token.push_back(constituentChar(Y));
      goto step8;
    }
    if (y8_syntax_type == kw::_sym_single_escape) {
      z = gc::As<Character_sp>(cl__read_char(sin, _lisp->_true(), nil<T_O>(), _lisp->_true()));
      token.push_back(constituentChar(z, TRAIT_ALPHABETIC | TRAIT_ESCAPED));
      LOG_READ(BF("Single escape read z[%s] accumulated token[%s]") % clasp_as_claspCharacter(z) % tokenStr(sin, token));
      goto step8;
    }
    if (y8_syntax_type == kw::_sym_multiple_escape) {
      // |....| or ....|| or ..|.|.. is ok
      only_dots_ok = true;
      goto step9;
    }
    if (y8_syntax_type == kw::_sym_invalid)
      SIMPLE_ERROR("ReaderError_O::create()");
    if (y8_syntax_type == kw::_sym_terminating_macro) {
      LOG_READ(BF("UNREADING char y[%s]") % clasp_as_claspCharacter(y));
      stream_unread_char(sin, clasp_as_claspCharacter(y));
      goto step10;
    }
    if (y8_syntax_type == kw::_sym_whitespace) {
      LOG_READ(BF("y is whitespace"));
#if 0
      if (_sym_STARpreserve_whitespace_pSTAR->symbolValue().isTrue()) { // Can this be recursiveP?
        LOG_READ(BF("unreading y[%s]") % clasp_as_claspCharacter(y));
        stream_unread_char(sin, clasp_as_claspCharacter(y));
      }
#else
      stream_unread_char(sin, clasp_as_claspCharacter(y));
#endif
      goto step10;
    }
  }
step9:
  LOG_READ(BF("step9"));
  {
    y = gc::As<Character_sp>(cl__read_char(sin, _lisp->_true(), nil<T_O>(), _lisp->_true()));
    Symbol_sp y9_syntax_type = core__syntax_type(readTable, y);
    LOG_READ(BF("Step9: Read y[%s] y9_syntax_type[%s]") % clasp_as_claspCharacter(y) % _rep_(y9_syntax_type));
    if ((y9_syntax_type == kw::_sym_constituent) || (y9_syntax_type == kw::_sym_non_terminating_macro) ||
        (y9_syntax_type == kw::_sym_terminating_macro) || (y9_syntax_type == kw::_sym_whitespace)) {
      token.push_back(constituentChar(y, TRAIT_ALPHABETIC | TRAIT_ESCAPED));
      LOG_READ(BF("token[%s]") % tokenStr(sin, token));
      goto step9;
    }
    LOG_READ(BF("About to test y9_syntax_type[%s] single_escape[%s] are equal? ==> %d") % _rep_(y9_syntax_type) %
             _rep_(kw::_sym_single_escape) % (y9_syntax_type == kw::_sym_single_escape));
    if (y9_syntax_type == kw::_sym_single_escape) {
      LOG_READ(BF("Handling single_escape_character"));
      z = gc::As<Character_sp>(cl__read_char(sin, _lisp->_true(), nil<T_O>(), _lisp->_true()));
      token.push_back(constituentChar(z, TRAIT_ALPHABETIC | TRAIT_ESCAPED));
      LOG_READ(BF("Read z[%s] accumulated token[%s]") % clasp_as_claspCharacter(z) % tokenStr(sin, token));
      goto step9;
    }
    if (y9_syntax_type == kw::_sym_multiple_escape) {
      LOG_READ(BF("Handling multiple_escape_character"));
      // |....| or ....|| or ..|.|.. is ok
      only_dots_ok = true;
      goto step8;
    }
    if (y9_syntax_type == kw::_sym_invalid) {
      SIMPLE_ERROR("ReaderError_O::create()");
    }
    SIMPLE_ERROR("Should never get here");
  }
step10:
  LOG_READ(BF("step10"));
  // At this point convert the string in tokenSin.str() into an object
  // Lets just return it as an annotated string
  // Throw a ReaderError if the token is not valid syntax
  if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue())
    return (Values(nil<T_O>()));
  T_sp object = interpret_token_or_throw_reader_error(sin, token, only_dots_ok);
  TRAP_BAD_CONS(object);
  return (Values(object));
}

void exposeCore_lisp_reader() {

  // functions for reader
}
}; // namespace core
