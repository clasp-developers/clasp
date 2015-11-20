/*
    File: write_symbol.cc
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
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    write_symbol.d -- print a symbol.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/symbol.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/character.h>
#include <clasp/core/strWithFillPtr.h>
#include <clasp/core/package.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/readtable.h>
#include <clasp/core/print.h>
#include <clasp/core/write_symbol.h>

namespace core {

static bool
potential_number_p(Str_sp s, int base) {
  /* See ANSI 2.3.1.1 */
  static cl_index i, l;
  claspCharacter c;
  /* A potential number must contain at least one digit */
  bool some_digit = false;

  l = s->length(); // .as<StrWithFillPtr_O>()->fillPointer();
  if (l == 0)
    return false;
  c = clasp_char(s, 0);

  /* A potential number must begin with a digit, sign or
           extension character (^ _) */
  if (clasp_digitp(c, base) >= 0)
    some_digit = true;
  else if (c != '+' && c != '-' && c != '^' && c != '_')
    return false;

  /* A potential number cannot end with a sign */
  c = clasp_char(s, l - 1);
  if (c == '+' || c == '-')
    return false;

  for (i = 1; i < l; i++) {
    c = clasp_char(s, i);
    /* It can only contain digits, signs, ratio markers,
             * extension characters and number markers. Number
             * markers are letters, but two adjacent letters fail
             * to be a number marker. */
    if (clasp_digitp(c, base) >= 0) {
      some_digit = true;
    } else if (c == '+' || c == '-' ||
               c == '/' || c == '.' || c == '^' || c == '_') {
      continue;
    } else if (isalpha(c) &&
               (((i + 1) >= l) || !isalpha(clasp_char(s, i + 1)))) {
      continue;
    } else {
      return false;
    }
  }
  return some_digit;
}

#define needs_to_be_inverted(s) (clasp_string_case(s) != 0)

static bool
all_dots(Str_sp s) {
  for (cl_index i = 0, iEnd(s->length()); i < iEnd; ++i)
    if (clasp_char(s, i) != '.')
      return 0;
  return 1;
}

static bool
needs_to_be_escaped(Str_sp s, T_sp readtable, T_sp print_case) {
  int action = gc::As<ReadTable_sp>(readtable)->getReadTableCaseAsEnum();
  if (potential_number_p(s, clasp_print_base()))
    return 1;
  /* The value of *PRINT-ESCAPE* is T. We need to check whether the
	 * symbol name S needs to be escaped. This will happen if it has some
	 * strange character, or if it has a lowercase character (because such
	 * a character cannot be read with the standard readtable) or if the
	 * string has to be escaped according to readtable case and the rules
	 * of 22.1.3.3.2. */
  for (cl_index i = 0, iEnd(s->length()); i < iEnd; i++) {
    int c = clasp_char(s, i);
    Character_sp cc = clasp_make_character(c);
    //            int syntax = clasp_readtable_get(readtable, c, 0);
    Symbol_sp syntax = gc::As<ReadTable_sp>(readtable)->syntax_type(cc);
#if 0
            if (syntax != cat_constituent ||
                clasp_invalid_character_p(c) ||
                (c) == ':')
                return 1;
#endif
    if (syntax != kw::_sym_constituent_character ||
        clasp_invalid_character_p(c) ||
        (c) == ':')
      return 1;
    if ((action == clasp_case_downcase) && isupper(c))
      return 1;
    if (islower(c))
      return 1;
  }
  return 0;
}

#define needs_to_be_inverted(s) (clasp_string_case(s) != 0)

static void
write_symbol_string(Str_sp s, int action, T_sp print_case,
                    T_sp stream, bool escape) {
  bool capitalize;
  if (action == clasp_case_invert) {
    if (!needs_to_be_inverted(s))
      action = clasp_case_preserve;
  }
  if (escape)
    clasp_write_char('|', stream);
  capitalize = 1;
  for (cl_index i = 0, iEnd(s->length()); i < iEnd; i++) {
    int c = clasp_char(s, i);
    if (escape) {
      if (c == '|' || c == '\\') {
        clasp_write_char('\\', stream);
      }
    } else if (action != clasp_case_preserve) {
      if (isupper(c)) {
        if ((action == clasp_case_invert) ||
            ((action == clasp_case_upcase) &&
             ((print_case == kw::_sym_downcase) ||
              ((print_case == kw::_sym_capitalize) && !capitalize)))) {
          c = tolower(c);
        }
        capitalize = 0;
      } else if (islower(c)) {
        if ((action == clasp_case_invert) ||
            ((action == clasp_case_downcase) &&
             ((print_case == kw::_sym_upcase) ||
              ((print_case == kw::_sym_capitalize) && capitalize)))) {
          c = toupper(c);
        }
        capitalize = 0;
      } else {
        capitalize = !clasp_alphanumericp(c);
      }
    }
    clasp_write_char(c, stream);
  }
  if (escape)
    clasp_write_char('|', stream);
}

static bool
forced_print_package(T_sp package) {
  T_sp print_package = _sym_STARprintPackageSTAR->symbolValue();
  bool result = print_package.notnilp() && (print_package != package);
  //        printf("%s:%d forced_print_package result=%d\n", __FILE__, __LINE__, result );
  return result;
}

void clasp_write_symbol(Symbol_sp x, T_sp stream) {
  ReadTable_sp readtable = _lisp->getCurrentReadTable();
  T_sp print_case = clasp_print_case();
  bool print_readably = clasp_print_readably();
  bool forced_package = 0;

  T_sp package;
  Str_sp name;
  if (x.nilp()) {
    package = cl::_sym_nil->homePackage();
    name = cl::_sym_nil->symbolName();
  } else {
    package = gc::As<Symbol_sp>(x)->homePackage();
    name = gc::As<Symbol_sp>(x)->symbolName();
  }

  if (!print_readably && !clasp_print_escape()) {
    //            printf("%s:%d quick print of symbol print_readably=%d print_escape=%d\n",__FILE__,__LINE__, print_readably, clasp_print_escape());
    write_symbol_string(name, gc::As<ReadTable_sp>(readtable)->getReadTableCaseAsEnum(),
                        print_case, stream, 0);
    return;
  }
  /* From here on, print-escape is true which means that it should
	 * be possible to recover the same symbol by reading it with
	 * the standard readtable (which has readtable-case = :UPCASE)
	 */
  if (package.nilp()) {
    if (print_readably || clasp_print_gensym())
      clasp_write_string("#:", stream);
  } else if (package == _lisp->keywordPackage()) {
    clasp_write_char(':', stream);
  } else {
    bool print_package = false;
    if ((forced_package = forced_print_package(package)))
      print_package = true;
    if (!print_package) {
      T_mv symbol_mv = cl_findSymbol(name->get(), _lisp->getCurrentPackage());
      Symbol_sp sym = symbol_mv;
      Symbol_sp intern_flag = gc::As<Symbol_sp>(symbol_mv.valueGet(1));
      if ((sym != x) || intern_flag.nilp())
        print_package = true;
    }
    if (print_package) {
      T_sp name = Str_O::create(gc::As<Package_sp>(package)->packageName());
      write_symbol_string(name, readtable->getReadTableCaseAsEnum(),
                          print_case, stream,
                          needs_to_be_escaped(name, readtable, print_case));
      if (!x.nilp()) {
        Symbol_mv sym2_mv = cl_findSymbol(x->symbolName()->get(), package);
        Symbol_sp sym2 = sym2_mv;
        Symbol_sp intern_flag2 = gc::As<Symbol_sp>(sym2_mv.valueGet(1));
        if (sym2 != x) {
          clasp_write_string("<UNPRINTABLE-SYMBOL@", stream);
          stringstream ss;
          ss << (void *)x.raw_();
          clasp_write_string(ss.str(), stream);
          clasp_write_string(">", stream);
          return;
          SIMPLE_ERROR(BF("Can't print symbol"));
        }
        if (intern_flag2 == kw::_sym_internal || forced_package) {
          clasp_write_string("::", stream);
        } else if (intern_flag2 == kw::_sym_external) {
          clasp_write_char(':', stream);
        } else {
          clasp_write_string("<PATHOLOGICAL-SYMBOL@", stream);
          stringstream ss;
          ss << (void *)x.raw_();
          clasp_write_string(ss.str(), stream);
          clasp_write_string(">", stream);
          return;
          SIMPLE_ERROR(BF("Pathological symbol --- cannot print"));
        }
      } else {
        clasp_write_char(':', stream);
      }
    }
  }
  write_symbol_string(name, readtable->getReadTableCaseAsEnum(), print_case, stream,
                      needs_to_be_escaped(name, readtable, print_case) ||
                          all_dots(name));
}

void Symbol_O::__write__(T_sp stream) const {
  if (_lisp->_PrintSymbolsProperly) {
    clasp_write_symbol(this->asSmartPtr(), stream);
  } else {
    string name = this->formattedName(false);
    clasp_write_string(name, stream);
  }
}
};
