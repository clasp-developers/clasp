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
#define DEBUG_LEVEL_NONE

#pragma clang diagnostic push
//#pragma clang diagnostic ignored "-Wunused-local-typedef"
#include <boost/algorithm/string.hpp>
#pragma clang diagnostic pop
#include <string>
#include <clasp/core/common.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/pathname.h>
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
#include <clasp/core/str.h>
#include <clasp/core/specialForm.h>
#include <clasp/core/cons.h>
//#include "lisp_ParserExtern.h"
#include <clasp/core/lispReader.h>
#include <clasp/core/readtable.h>
#include <clasp/core/wrappers.h>

namespace core {

#define TRAIT_DIGIT 0X000100
#define TRAIT_ALPHABETIC 0X000200
#define TRAIT_PACKAGEMARKER 0X000400
#define TRAIT_ALPHADIGIT 0X000800
#define TRAIT_PLUSSIGN 0X001000
#define TRAIT_MINUSSIGN 0X002000
#define TRAIT_DOT 0X004000
#define TRAIT_DECIMALPOINT 0X008000
#define TRAIT_RATIOMARKER 0X010000
#define TRAIT_EXPONENTMARKER 0X020000
#define TRAIT_INVALID 0X040000
#define TRAIT_MASK 0X0FFF00
#define CHAR_MASK 0X0000FF
#define CHR(x) (static_cast<char>((x)&CHAR_MASK))
#define CHR_MATCH(x, m) (CHR(x) == (m))
#define TRAIT_MATCH_ANY(x, t) (((x) & (t)) != 0)
#define CHR_TRAIT_MATCH_ANY(x, t, m) (TRAIT_MATCH_ANY(x, t) && CHR_MATCH(x, m))
#define TRAIT_MATCH_EQ(x, t)

#if 1
#define TRAP_BAD_CONS(x)
#else
#define TRAP_BAD_CONS(x)                                \
  {                                                     \
    if (cl_consp(x)) {                                  \
      LOG(BF("About to try trap bad cons"));            \
      string ssss = core_sourceFileInfo(x)->fileName(); \
    }                                                   \
  }
#endif

/*! Return a uint that combines the character x with its character TRAITs
      See CLHS 2.1.4.2 */
uint constituentChar(Character_sp ch, uint trait = 0) {
  claspChar x(clasp_as_char(ch));
  if (trait != 0)
    return (x | trait);
  uint result = 0;
  int readBase = unbox_fixnum(gc::As<Fixnum_sp>(cl::_sym_STARread_baseSTAR->symbolValue()));
  if (x >= '0' && x <= '9') {
    uint uix = x - '0';
    if (uix < readBase) {
      result = (TRAIT_DIGIT | x);
      return result;
    }
    return (TRAIT_ALPHABETIC | x);
  }
  if (x >= 'a' && x <= 'z') {
    uint uix = x - 'a' + 10;
    if (uix < readBase) {
      result = (TRAIT_DIGIT | x);
      goto LETTER;
    }
    result = (TRAIT_ALPHABETIC | x);
    goto LETTER;
  }
  if (x >= 'A' && x <= 'Z') {
    uint uix = x - 'A' + 10;
    if (uix < readBase) {
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
  return (TRAIT_INVALID | x);
LETTER:
  if (x == 'd' || x == 'D' || x == 'e' || x == 'E' || x == 'f' || x == 'F' || x == 's' || x == 'S' || x == 'l' || x == 'L')
    result |= TRAIT_EXPONENTMARKER;
  return result;
}

#define DOCS_af_nread "nread"
#define LOCK_af_nread 1
#define ARGS_af_nread "(sin &optional (eof-error-p t) eof-value)"
#define DECL_af_nread ""
T_mv af_nread(T_sp sin, T_sp eof_error_p, T_sp eof_value) {
  _G();
  T_sp result = read_lisp_object(sin, eof_error_p.isTrue(), eof_value, false);
  return (Values(result));
};

string fix_exponent_char(const char *cur) {
  _G();
  stringstream ss;
  while (*cur) {
    if (isalpha(*cur)) {
      char uc = toupper(*cur);
      switch (uc) {
      case 'D':
      case 'E':
      case 'F':
      case 'L':
      case 'S':
        ss << "E";
        break;
      default:
        SIMPLE_ERROR(BF("Illegal exponent character[%c]") % *cur);
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

typedef enum {
  undefined_exp,
  double_float_exp,
  float_exp,
  single_float_exp,
  long_float_exp,
  short_float_exp
} FloatExponentType;

string tokenStr(const vector<uint> &token, uint start = 0, uint end = UNDEF_UINT) {
  stringstream ss;
  for (uint i = start, iEnd(end == UNDEF_UINT ? token.size() : end);
       i < iEnd; ++i) {
    ss << CHR(token[i]);
  }
  return ss.str();
}

T_sp interpret_token_or_throw_reader_error(T_sp sin, const vector<uint> &token) {
  _G();
  ASSERTF(token.size() > 0, BF("The token is empty!"));
  const uint *start = token.data();
  const uint *cur = start;
  const uint *end = token.data() + token.size();
  const uint *package_marker = start;
  const uint *name_marker = start;
  TokenState state = tstart;
  FloatExponentType exponent = undefined_exp;
#define EXPTEST(c, x)                                                                                             \
  {                                                                                                               \
    if (CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'D') || CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'd')) { \
      exponent = double_float_exp;                                                                                \
      state = x;                                                                                                  \
      goto NEXT;                                                                                                  \
    }                                                                                                             \
    if (CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'E') || CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'e')) { \
      exponent = float_exp;                                                                                       \
      state = x;                                                                                                  \
      goto NEXT;                                                                                                  \
    }                                                                                                             \
    if (CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'F') || CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'f')) { \
      exponent = single_float_exp;                                                                                \
      state = x;                                                                                                  \
      goto NEXT;                                                                                                  \
    }                                                                                                             \
    if (CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'L') || CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'l')) { \
      exponent = long_float_exp;                                                                                  \
      state = x;                                                                                                  \
      goto NEXT;                                                                                                  \
    }                                                                                                             \
    if (CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 'S') || CHR_TRAIT_MATCH_ANY(c, TRAIT_EXPONENTMARKER, 's')) { \
      exponent = short_float_exp;                                                                                 \
      state = x;                                                                                                  \
      goto NEXT;                                                                                                  \
    }                                                                                                             \
  }
#define TEST(c, x) \
  if (c) {         \
    state = x;     \
    goto NEXT;     \
  }
#define TEST_CMD(c, cmd, x) \
  if (c) {                  \
    cmd;                    \
    state = x;              \
    goto NEXT;              \
  }
#define ELSE(x) \
  {             \
    state = x;  \
    goto NEXT;  \
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
      SIMPLE_ERROR(BF("unhandled state[%d] for token assignment") % state);
    }
  NEXT:
    ++cur;
  }
  switch (state) {
  case tstart:
    SIMPLE_ERROR(BF("There was no token!!!!"));
  case tsymdot:
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
        return _Nil<T_O>();
      string symbolName = tokenStr(token, name_marker - token.data());
      Symbol_sp sym = _lisp->getCurrentPackage()->intern(symbolName);
      return sym;
    }
    break;
  case tsymex:
  case tsympv: {
    if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue())
      return _Nil<T_O>();
    // interpret good package:name symbols
    // Get package part
    stringstream packageSin;
    cur = start;
    while (cur != package_marker) {
      packageSin << CHR(*cur);
      ++cur;
    }
    int separator(0);
    while (cur != name_marker) {
      ++separator;
      ++cur;
    }
    string symbolName = tokenStr(token, name_marker - token.data());
    LOG(BF("Interpreting token as packageName[%s] and symbol-name[%s]") % packageSin.str() % symbolName);
    string packageName = packageSin.str();
    Package_sp pkg = gc::As<Package_sp>(_lisp->findPackage(packageName, true));
    Symbol_sp sym;
    if (separator == 1) { // Asking for external symbol
      Symbol_mv sym_mv = pkg->findSymbol(symbolName);
      sym = sym_mv;
      T_sp status = sym_mv.second();
      if (status != kw::_sym_external) {
        SIMPLE_ERROR(BF("Cannot find the external symbol %s in %s") % symbolName % _rep_(pkg));
      }
    } else {
      sym = pkg->intern(symbolName);
    }
    ASSERT(sym);
    return sym;
  } break;
  case tsymkw: {
    if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue())
      return _Nil<T_O>();
    // interpret good keywords
    LOG(BF("Token[%s] interpreted as keyword") % name_marker);
    string keywordName = tokenStr(token, name_marker - token.data());
    return _lisp->keywordPackage()->intern(keywordName);
  } break;
  case tsyme:
  case tsymp:
  case tsymk:
  case tsymbad:
    if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue())
      return _Nil<T_O>();
    // interpret failed symbols
    SIMPLE_ERROR(BF("Could not interpret symbol state(%s) at %s symbol: [%s]") % stateString(state) % _rep_(clasp_file_position(sin)) % tokenStr(token, start - token.data()));
    break;
  case tintt:
  case tintp:
    // interpret ints
    {
      int read_base = unbox_fixnum(gc::As<Fixnum_sp>(cl::_sym_STARread_baseSTAR->symbolValue()));
      string num = tokenStr(token, start - token.data());
      if (num[0] == '+')
        num = num.substr(1, num.size());
      try {
        if (num[num.size() - 1] == '.') {
          mpz_class z10(num.substr(0, num.size() - 1), 10);
          return Integer_O::create(z10);
        } else {
          mpz_class zbase(num.c_str(), read_base);
          return Integer_O::create(zbase);
        }
      } catch (std::invalid_argument &arg) {
        SIMPLE_ERROR(BF("Problem in mpz_class creation with %s error: %s") % num % arg.what());
      }
      SIMPLE_ERROR(BF("Problem while interpreting int from %s in reader") % num);
    }
    break;
  case tratio: {
    // interpret ratio
    string ratioStr = tokenStr(token, start - token.data());
    if (ratioStr[0] == '+') {
      Ratio_sp rp = Ratio_O::create(ratioStr.substr(1, ratioStr.size() - 1).c_str());
      return rp;
    }
    Ratio_sp r = Ratio_O::create(ratioStr.c_str());
    return r;
    break;
  }
  case tfloat0:
  case tfloate:
  case tfloatp:
    // interpret float
    {
      switch (exponent) {
      case undefined_exp: {
        char *lastValid = NULL;
        if (cl::_sym_STARreadDefaultFloatFormatSTAR->symbolValue() == cl::_sym_single_float) {
          string numstr = tokenStr(token, start - token.data()).c_str();
          double d = ::strtod(numstr.c_str(), &lastValid);
          return clasp_make_single_float(d);
        } else if (cl::_sym_STARreadDefaultFloatFormatSTAR->symbolValue() == cl::_sym_DoubleFloat_O) {
          string numstr = tokenStr(token, start - token.data()).c_str();
          double d = ::strtod(numstr.c_str(), &lastValid);
          return DoubleFloat_O::create(d);
        } else {
          SIMPLE_ERROR(BF("Handle *read-default-float-format* of %s") % _rep_(cl::_sym_STARreadDefaultFloatFormatSTAR->symbolValue()));
        }
      }
      case float_exp: {
        char *lastValid = NULL;
        string numstr = fix_exponent_char(tokenStr(token, start - token.data()).c_str());
        double d = ::strtod(numstr.c_str(), &lastValid);
        return DoubleFloat_O::create(d);
      }
      case short_float_exp: {
        char *lastValid = NULL;
        string numstr = fix_exponent_char(tokenStr(token, start - token.data()).c_str());
        double d = ::strtod(numstr.c_str(), &lastValid);
        return clasp_make_single_float(d);
      }
      case single_float_exp: {
        char *lastValid = NULL;
        string numstr = fix_exponent_char(tokenStr(token, start - token.data()).c_str());
        double d = ::strtod(numstr.c_str(), &lastValid);
        return clasp_make_single_float(d);
      }
      case double_float_exp: {
        char *lastValid = NULL;
        string numstr = fix_exponent_char(tokenStr(token, start - token.data()).c_str());
        double d = ::strtod(numstr.c_str(), &lastValid);
        return DoubleFloat_O::create(d);
      }
      case long_float_exp: {
        char *lastValid = NULL;
        string numstr = fix_exponent_char(tokenStr(token, start - token.data()).c_str());
#ifdef CLASP_LONG_FLOAT
        LongFloat d = ::strtold(numstr.c_str(), &lastValid);
        return LongFloat_O::create(d);
#else
        double d = ::strtod(numstr.c_str(), &lastValid);
        return DoubleFloat_O::create(d);
#endif
      }
      }
      SIMPLE_ERROR(BF("Shouldn't get here - unhandled exponent type"));
    }
  }
  LOG(BF("Bad token[%s]") % tok);
  SIMPLE_ERROR(BF("create<ReaderError_O>(sin));"));
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

List_sp read_list(T_sp sin, char end_char, bool allow_consing_dot) {
  _G();
  af_stackMonitor();
  bool got_dotted = false;
  T_sp dotted_object = _Nil<T_O>();
  Cons_sp first = Cons_O::create(_Nil<T_O>(), _Nil<T_O>());
  List_sp cur = first;
  while (1) {
    SourcePosInfo_sp info = core_inputStreamSourcePosInfo(sin);
    Character_sp cp = gc::As<Character_sp>(cl_peekChar(_lisp->_true(), sin, _lisp->_true(), _Nil<Character_O>(), _lisp->_true()));
    LOG(BF("read_list ---> peeked char[%s]") % _rep_(cp));
    if (clasp_as_char(cp) == end_char) {
      cl_readChar(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true());
      if (dotted_object.notnilp()) {
        cur.asCons()->setCdr(dotted_object);
        List_sp result = oCdr(first);
        TRAP_BAD_CONS(result);
        if (result.nilp())
          return (Values(_Nil<T_O>()));
        return (Values(result));
      }
      List_sp otherResult = oCdr(first);
      TRAP_BAD_CONS(otherResult);
      if (otherResult.nilp())
        return (Values(_Nil<T_O>()));
      lisp_registerSourcePosInfo(otherResult, info);
      return (otherResult);
    }
    int ivalues;
    T_sp obj;
    T_mv mv = lisp_object_query(sin, true, _Nil<T_O>(), true);
    ivalues = mv.number_of_values();
    if (ivalues > 0) {
      obj = mv;
      LOG(BF("lisp_object_query: %s") % _rep_(obj));
      // Could get ending lineNumber and column here
      if (obj == _sym_dot) {
        if (allow_consing_dot) {
          got_dotted = true;
          Character_sp cdotp = gc::As<Character_sp>(cl_peekChar(_lisp->_true(), sin, _lisp->_true(), _Nil<Character_O>(), _lisp->_true()));
          if (clasp_as_char(cdotp) == end_char) {
            SIMPLE_ERROR(BF("Nothing after consing dot"));
          }
          dotted_object = read_lisp_object(sin, true, _Nil<T_O>(), true);
        } else {
          SIMPLE_ERROR(BF("Illegal consing dot"));
        }
      } else {
        if (got_dotted) {
          SIMPLE_ERROR(BF("More than one object after consing dot"));
        }
        Cons_sp one = Cons_O::create(obj, _Nil<T_O>());
        lisp_registerSourcePosInfo(one, info);
        LOG(BF("One = %s\n") % _rep_(one));
        LOG(BF("one->sourceFileInfo()=%s") % _rep_(core_sourceFileInfo(one)));
        LOG(BF("one->sourceFileInfo()->fileName()=%s") % core_sourceFileInfo(one)->fileName());
        LOG(BF("one->sourceFileInfo()->fileName().c_str() = %s") % core_sourceFileInfo(one)->fileName().c_str());
        TRAP_BAD_CONS(one);
        cur.asCons()->setCdr(one);
        cur = one;
      }
    }
  }
}

/*! Used when USE_SHARP_EQUAL_HASH_TABLES is on */
SYMBOL_SC_(CorePkg, STARsharp_equal_final_tableSTAR);
SYMBOL_SC_(CorePkg, STARsharp_equal_temp_tableSTAR);
SYMBOL_SC_(CorePkg, STARsharp_equal_repl_tableSTAR);

__thread unsigned int read_lisp_object_recursion_depth = 0;
struct increment_read_lisp_object_recursion_depth {
  increment_read_lisp_object_recursion_depth() {
    ++read_lisp_object_recursion_depth;
  }
  ~increment_read_lisp_object_recursion_depth() {
    --read_lisp_object_recursion_depth;
  }
  static void reset() {
    read_lisp_object_recursion_depth = 0;
  }
  int value() const {
    return read_lisp_object_recursion_depth;
  }
  int max() const {
    return 256;
  }
};
T_sp read_lisp_object(T_sp sin, bool eofErrorP, T_sp eofValue, bool recursiveP) {
  _G();
  T_sp result = _Nil<T_O>();
  if (recursiveP) {
    increment_read_lisp_object_recursion_depth recurse;
    if (recurse.value() > recurse.max()) {
      printf("%s:%d read_lisp_object_recursion_depth %d has exceeded max (%d) - there is a problem reading line %d\n",
             __FILE__, __LINE__, recurse.value(), recurse.max(), clasp_input_lineno(sin));
    }
    while (1) {
      T_mv mv = lisp_object_query(sin, eofErrorP, eofValue, recursiveP);
      int ivalues = mv.number_of_values();
      if (ivalues > 0) {
        result = mv;
        if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
          LOG(BF("read_suppress == true"));
          result = _Nil<T_O>();
          break;
        }
        break;
      }
    }
  } else {
    increment_read_lisp_object_recursion_depth::reset();

    DynamicScopeManager scope(_sym_STARsharp_equal_final_tableSTAR,
                              HashTableEql_O::create(40, make_fixnum(4000), 0.8));
    scope.pushSpecialVariableAndSet(_sym_STARsharp_equal_temp_tableSTAR,
                                    HashTableEql_O::create(40, make_fixnum(4000), 0.8));
    scope.pushSpecialVariableAndSet(_sym_STARsharp_equal_repl_tableSTAR,
                                    HashTableEq_O::create(40, make_fixnum(4000), 0.8));

    result = read_lisp_object(sin, eofErrorP, eofValue, true);
  }
  if (result.nilp())
    return (Values(_Nil<T_O>()));
  return (result);
}

/*!
      Read a character from the stream and based on what it is continue to process the
      stream until a complete symbol/number of macro is processed.
      Return the result in a MultipleValues object - if it is empty then nothing was read */
T_mv lisp_object_query(T_sp sin, bool eofErrorP, T_sp eofValue, bool recursiveP) {
  _G();
#if 1
  static int monitorReaderStep = 0;
  if ((monitorReaderStep % 1000) == 0 && cl_member(_sym_monitorReader, _sym_STARdebugMonitorSTAR->symbolValue(), _Nil<T_O>()).notnilp()) {
    printf("%s:%d:%s stream %s -> pos = %ld\n", __FILE__, __LINE__, __FUNCTION__, _rep_(clasp_filename(sin, false)).c_str(), unbox_fixnum(gc::As<Fixnum_sp>(clasp_file_position(sin))));
  }
  ++monitorReaderStep;
#endif
  string chars;
  vector<uint> token;
  ReadTable_sp readTable = _lisp->getCurrentReadTable();
  Character_sp x, y, z, X, Y, Z;
/* See the CLHS 2.2 Reader Algorithm  - continue has the effect of jumping to step 1 */
step1:
  LOG(BF("step1"));
  T_sp tx = cl_readChar(sin, _Nil<T_O>(), _Nil<T_O>(), _lisp->_true());
  if (tx.nilp()) {
    if (eofErrorP)
      STREAM_ERROR(sin);
    return Values(eofValue);
  }
  x = gc::As<Character_sp>(tx);
  LOG(BF("Reading character x[%s]") % clasp_as_char(x));
  Symbol_sp x1_syntax_type = readTable->syntax_type(x);
  //    step2:
  if (x1_syntax_type == kw::_sym_invalid_character) {
    LOG(BF("step2 - invalid-character[%c]") % clasp_as_char(x));
    SIMPLE_ERROR(BF("ReaderError_O::create(sin,_lisp)"));
  }
  //    step3:
  if (x1_syntax_type == kw::_sym_whitespace_character) {
    LOG(BF("step3 - whitespace character[%c/%d]") % clasp_as_char(x) % clasp_as_char(x));
    goto step1;
  }
  //    step4:
  if ((x1_syntax_type == kw::_sym_terminating_macro_character) || (x1_syntax_type == kw::_sym_non_terminating_macro_character)) {
    _BLOCK_TRACEF(BF("Processing macro character x[%s]") % clasp_as_char(x));
    LOG(BF("step4 - terminating-macro-character or non-terminating-macro-character char[%c]") % clasp_as_char(x));
    T_sp reader_macro;
    {
      MULTIPLE_VALUES_CONTEXT();
      reader_macro = readTable->get_macro_character(x);
    }
    ASSERT(reader_macro.notnilp());
    return eval::funcall(reader_macro, sin, x);
  }
  //    step5:
  if (x1_syntax_type == kw::_sym_single_escape_character) {
    LOG(BF("step5 - single-escape-character char[%c]") % clasp_as_char(x));
    LOG(BF("Handling single escape"));
    y = gc::As<Character_sp>(cl_readChar(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true()));
    token.clear();
    token.push_back(constituentChar(y, TRAIT_ALPHABETIC));
    LOG(BF("Read y[%s]") % clasp_as_char(y));
    goto step8;
  }
  //    step6:
  if (x1_syntax_type == kw::_sym_multiple_escape_character) {
    LOG(BF("step6 - multiple-escape-character char[%c]") % clasp_as_char(x));
    LOG(BF("Handling multiple escape - clearing token"));
    token.clear();
    goto step9;
  }
  //    step7:
  if (readTable->syntax_type(x) == kw::_sym_constituent_character) {
    LOG(BF("step7 - constituent-character char[%c]") % clasp_as_char(x));
    LOG(BF("Handling constituent character"));
    token.clear();
    X = readTable->convert_case(x);
    LOG(BF("Converted case X[%s]") % clasp_as_char(X));
    token.push_back(constituentChar(X));
  }
step8:
  LOG(BF("step8"));
  {
    T_sp ty = cl_readChar(sin, _Nil<T_O>(), _Nil<T_O>(), _lisp->_true());
    if (ty.nilp()) {
      LOG(BF("Hit eof"));
      goto step10;
    }
    Character_sp y(ty);
    LOG(BF("Step8: Read y[%s]") % clasp_as_char(y));
    Symbol_sp y8_syntax_type = readTable->syntax_type(y);
    LOG(BF("y8_syntax_type=%s") % _rep_(y8_syntax_type));
    if ((y8_syntax_type == kw::_sym_constituent_character) || (y8_syntax_type == kw::_sym_non_terminating_macro_character)) {
      Y = readTable->convert_case(y);
      token.push_back(constituentChar(Y));
      goto step8;
    }
    if (y8_syntax_type == kw::_sym_single_escape_character) {
      z = gc::As<Character_sp>(cl_readChar(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true()));
      token.push_back(constituentChar(z, TRAIT_ALPHABETIC));
      LOG(BF("Single escape read z[%s] accumulated token[%s]") % clasp_as_char(z) % tokenStr(token));
      goto step8;
    }
    if (y8_syntax_type == kw::_sym_multiple_escape_character)
      goto step9;
    if (y8_syntax_type == kw::_sym_invalid_character)
      SIMPLE_ERROR(BF("ReaderError_O::create()"));
    if (y8_syntax_type == kw::_sym_terminating_macro_character) {
      LOG(BF("UNREADING char y[%s]") % clasp_as_char(y));
      clasp_unread_char(clasp_as_char(y), sin);
      goto step10;
    }
    if (y8_syntax_type == kw::_sym_whitespace_character) {
      LOG(BF("y is whitespace"));
      if (_sym_STARpreserve_whitespace_pSTAR->symbolValue().isTrue()) {
        LOG(BF("unreading y[%s]") % clasp_as_char(y));
        clasp_unread_char(clasp_as_char(y), sin);
      }
      goto step10;
    }
  }
step9:
  LOG(BF("step9"));
  {
    y = gc::As<Character_sp>(cl_readChar(sin, _lisp->_true(), _Nil<T_O>(), _lisp->_true()));
    Symbol_sp y9_syntax_type = readTable->syntax_type(y);
    LOG(BF("Step9: Read y[%s] y9_syntax_type[%s]") % clasp_as_char(y) % _rep_(y9_syntax_type));
    if ((y9_syntax_type == kw::_sym_constituent_character) || (y9_syntax_type == kw::_sym_non_terminating_macro_character) || (y9_syntax_type == kw::_sym_terminating_macro_character) || (y9_syntax_type == kw::_sym_whitespace_character)) {
      token.push_back(constituentChar(y, TRAIT_ALPHABETIC));
      LOG(BF("token[%s]") % tokenStr(token));
      goto step9;
    }
    LOG(BF("About to test y9_syntax_type[%s] single_escape[%s] are equal? ==> %d") % _rep_(y9_syntax_type) % _rep_(kw::_sym_single_escape_character) % (y9_syntax_type == kw::_sym_single_escape_character));
    if (y9_syntax_type == kw::_sym_single_escape_character) {
      LOG(BF("Handling single_escape_character"));
      T_sp tz = cl_readChar(sin, _Nil<T_O>(), _Nil<T_O>(), _lisp->_true());
      if (tz.nilp())
        STREAM_ERROR(sin);
      z = gc::As<Character_sp>(tz);
      token.push_back(constituentChar(z, TRAIT_ALPHABETIC));
      LOG(BF("Read z[%s] accumulated token[%s]") % clasp_as_char(z) % tokenStr(token));
      goto step9;
    }
    if (y9_syntax_type == kw::_sym_multiple_escape_character) {
      LOG(BF("Handling multiple_escape_character"));
      goto step8;
    }
    if (y9_syntax_type == kw::_sym_invalid_character) {
      SIMPLE_ERROR(BF("ReaderError_O::create()"));
    }
    SIMPLE_ERROR(BF("Should never get here"));
  }
step10:
  LOG(BF("step10"));
  // At this point convert the string in tokenSin.str() into an object
  // Lets just return it as an annotated string
  // Throw a ReaderError if the token is not valid syntax
  if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue())
    return (Values(_Nil<T_O>()));
  T_sp object = interpret_token_or_throw_reader_error(sin, token);
  TRAP_BAD_CONS(object);
  return (Values(object));
}

void exposeCore_lisp_reader() {
  _G();
  SYMBOL_SC_(CorePkg, nread);
  Defun(nread);

  // functions for reader
}
};
