/*
    File: pathname.cc
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
/*
    pathname.cc -- Pathnames.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister - translated into C++

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.

    BRCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

*/

/*
	O.S. DEPENDENT

	This file contains those functions that interpret namestrings.
*/

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispString.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/str.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/sequence.h>
#include <clasp/core/strWithFillPtr.h>
#include <clasp/core/primitives.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/arguments.h>
#include <clasp/core/character.h>
#include <clasp/core/pathname.h>
#include <clasp/core/wrappers.h>

SYMBOL_EXPORT_SC_(KeywordPkg, case);
SYMBOL_EXPORT_SC_(KeywordPkg, file);
SYMBOL_EXPORT_SC_(KeywordPkg, unspecific);
SYMBOL_EXPORT_SC_(KeywordPkg, local);
SYMBOL_EXPORT_SC_(KeywordPkg, common);
SYMBOL_EXPORT_SC_(KeywordPkg, wild_inferiors);
SYMBOL_EXPORT_SC_(KeywordPkg, up);
SYMBOL_EXPORT_SC_(KeywordPkg, wild);
SYMBOL_EXPORT_SC_(KeywordPkg, host);
SYMBOL_EXPORT_SC_(KeywordPkg, device);
SYMBOL_EXPORT_SC_(KeywordPkg, type);
SYMBOL_EXPORT_SC_(KeywordPkg, newest);
SYMBOL_EXPORT_SC_(KeywordPkg, version);
SYMBOL_EXPORT_SC_(KeywordPkg, directory);

namespace core {
typedef int (*delim_fn)(int);

/*
 * Translates a string into the host's preferred case.
 * See CLHS 19.2.2.1.2.2 Common Case in Pathname Components.
 */
/* We use UN*X conventions, so lower case is default.
 * However, this really should be conditionalised to the OS type,
 * and it should translate to _opposite_ of the local case.
 */

static T_sp normalize_case(T_sp path, T_sp cas) {

  if (cas == kw::_sym_local) {
    if (af_logicalPathnameP(path))
      return kw::_sym_upcase;
    return kw::_sym_downcase;
  } else if (cas == kw::_sym_common || cas == kw::_sym_downcase || cas == kw::_sym_upcase) {
    return cas;
  } else {
    SIMPLE_ERROR(BF("Not a valid pathname case :\n%s") % _rep_(cas));
  }
}

static bool
in_local_case_p(T_sp str, T_sp cas) {
  if (cas == kw::_sym_downcase)
    return clasp_string_case(gc::As<Str_sp>(str)) < 0;
  return true;
}

static bool
in_antilocal_case_p(T_sp str, T_sp cas) {
  if (cas == kw::_sym_downcase)
    return clasp_string_case(gc::As<Str_sp>(str)) > 0;
  return false;
}

static T_sp
to_local_case(T_sp str, T_sp cas) {
  if (cas == kw::_sym_downcase)
    return cl_string_downcase(str);
  return cl_string_upcase(str);
}

static Symbol_sp
host_case(T_sp host) {
  if (host.nilp())
    return kw::_sym_local;
  if (clasp_logical_hostname_p(host))
    return kw::_sym_upcase;
  return kw::_sym_downcase;
}

static T_sp
to_antilocal_case(T_sp str, T_sp cas) {
  if (cas == kw::_sym_downcase)
    return cl_string_upcase(str);
  return cl_string_upcase(str);
}

static T_sp
translate_from_common(T_sp tstr, T_sp tocase) {
  Str_sp str = gc::As<Str_sp>(tstr);
  int string_case = clasp_string_case(str);
  if (string_case > 0) { /* ALL_UPPER */
    return to_local_case(str, tocase);
  } else if (string_case < 0) { /* ALL_LOWER */
    return to_antilocal_case(str, tocase);
  } else { /* Mixed case goes unchanged */
    return str;
  }
}

static T_sp
translate_to_common(T_sp str, T_sp fromcase) {
  if (in_local_case_p(str, fromcase)) {
    return cl_string_upcase(str);
  } else if (in_antilocal_case_p(str, fromcase)) {
    return cl_string_downcase(str);
  } else {
    return str;
  }
}

static T_sp
translate_component_case(T_sp str, T_sp fromcase, T_sp tocase) {
  /* Pathnames may contain some other objects, such as symbols,
	 * numbers, etc, which need not be translated */
  if (str.nilp()) {
    return str;
  } else if (!gc::IsA<Str_sp>(str)) {
#ifdef CLASP_UNICODE
    if (CLASP_EXTENDED_STRING_P(str) && brcl_fits_in_base_string(str)) {
      str = si_coerce_to_base_string(str);
      return translate_component_case(str, fromcase, tocase);
    }
#endif
    return str;
  } else if (tocase == fromcase) {
    return str;
  } else if (tocase == kw::_sym_common) {
    return translate_to_common(str, fromcase);
  } else if (fromcase == kw::_sym_common) {
    return translate_from_common(str, tocase);
  } else {
    str = translate_to_common(str, fromcase);
    return translate_from_common(str, tocase);
  }
}

static T_sp
translate_list_case(List_sp list, T_sp fromcase, T_sp tocase) {
  /* If the argument is really a list, translate all strings in it and
	 * return this new list, else assume it is a string and translate it.
	 */
  if (!cl_consp(list)) {
    return translate_component_case(list, fromcase, tocase);
  } else {
    list = cl_copyList(list);
    for (auto l : list) {
      /* It is safe to pass anything to translate_component_case,
		 * because it will only transform strings, leaving other
		 * object (such as symbols) unchanged.*/
      T_sp name = oCar(l);
      name = cl_listp(name) ? translate_list_case(name, fromcase, tocase) : translate_component_case(name, fromcase, tocase);
      l->rplaca(name);
    }
    return list;
  }
}

static T_sp
destructively_check_directory(List_sp directory, bool logical, bool delete_back) {
  /* This function performs two tasks
	 * 1) It ensures that the list is a valid directory list
	 * 2) It ensures that all strings in the list are valid C strings without fill pointer
	 *    All strings are copied, thus avoiding problems with the user modifying the
	 *    list that was passed to MAKE-PATHNAME.
	 * 3) Redundant :back are removed.
	 */
  /* INV: directory is always a list */
  if (!cl_listp(directory)) {
    //    printf("%s:%d %s error\n", __FILE__, __LINE__, __FUNCTION__ );
    return kw::_sym_error;
  }
  if (directory.nilp())
    return directory;
  if (oCar(directory) != kw::_sym_absolute &&
      oCar(directory) != kw::_sym_relative) {
    //    printf("%s:%d %s error\n", __FILE__, __LINE__, __FUNCTION__ );
    return kw::_sym_error;
  }
BEGIN:
  T_sp ptr;
  int i;
  for (i = 0, ptr = directory; ptr.consp(); ptr = oCdr(ptr), ++i) {
    T_sp item = oCar(ptr);
    if (item == kw::_sym_back) {
      if (i == 0) {
        //        printf("%s:%d %s error\n", __FILE__, __LINE__, __FUNCTION__ );
        return kw::_sym_error;
      }
      item = cl_nth(i - 1, directory);
      if (item == kw::_sym_absolute || item == kw::_sym_wild_inferiors) {
        //        printf("%s:%d %s error\n", __FILE__, __LINE__, __FUNCTION__ );
        return kw::_sym_error;
      }
      if (delete_back && i >= 2) {
        T_sp next = oCdr(ptr);
        ptr = cl_nthcdr(i - 2, directory);
        gc::As<Cons_sp>(ptr)->rplacd(next);
        i = i - 2; // Was i--;
      }
    } else if (item == kw::_sym_up) {
      if (i == 0) {
        //        printf("%s:%d %s error\n", __FILE__, __LINE__, __FUNCTION__ );
        return kw::_sym_error;
      }
      item = cl_nth(i - 1, directory);
      if (item == kw::_sym_absolute || item == kw::_sym_wild_inferiors) {
        //        printf("%s:%d %s error\n", __FILE__, __LINE__, __FUNCTION__ );
        return kw::_sym_error;
      }
    } else if (item == kw::_sym_relative || item == kw::_sym_absolute) {
      if (i > 0) {
        //        printf("%s:%d %s error\n", __FILE__, __LINE__, __FUNCTION__ );
        return kw::_sym_error;
      }
    } else if (af_stringP(item)) {
      size_t l = cl_length(item);
#ifdef CLASP_UNICODE
//		if (clasp_fits_in_base_string(item)) {
//		    item = si_copy_to_simple_base_string(item);
//		} else {
#endif
      item = cl_copySeq(gc::As<T_sp>(item));
      gc::As<Cons_sp>(ptr)->rplaca(item);
      if (logical) {
        continue;
      }
      if (l && af_char(item, 0) == '.') {
        if (l == 1) {
          /* Single dot */
          if (i == 0) {
            //            printf("%s:%d %s error\n", __FILE__, __LINE__, __FUNCTION__ );
            return kw::_sym_error;
          }
          gc::As<Cons_sp>(cl_nthcdr(--i, directory))->rplacd(oCdr(ptr));
        } else if (l == 2 && af_char(item, 1) == '.') {
          gc::As<Cons_sp>(ptr)->rplaca(kw::_sym_up);
          goto BEGIN;
        }
      }
    } else if (item != kw::_sym_wild && item != kw::_sym_wild_inferiors) {
      //      printf("%s:%d %s error\n", __FILE__, __LINE__, __FUNCTION__ );
      return kw::_sym_error;
    }
  }
  return directory;
}

#define ARGS_Pathname_O_makePathname "(host device directory name type version fromcase &optional logical)"
#define DECL_Pathname_O_makePathname ""
#define DOCS_Pathname_O_makePathname "makePathname - force it to be logical-pathname with logical"
Pathname_sp Pathname_O::makePathname(T_sp host, T_sp device, T_sp directory,
                                     T_sp name, T_sp type, T_sp version,
                                     T_sp fromcase, bool logical) {
  T_sp x, component;
  Pathname_sp p;
  if (logical) {
    p = LogicalPathname_O::create();
  } else {
    if (af_stringP(host)) {
      if (clasp_logical_hostname_p(host)) {
        p = LogicalPathname_O::create();
        logical = true;
      } else {
        p = Pathname_O::create();
      }
    } else if (host.nilp()) {
      p = Pathname_O::create();
    } else {
      x = directory;
      component = kw::_sym_host;
      goto ERROR;
    }
  }
  if (device.notnilp() && device != kw::_sym_unspecific &&
      !(!af_logicalPathnameP(p) && af_stringP(device))) {
    x = device;
    component = kw::_sym_device;
    goto ERROR;
  }
  if (name.notnilp() && name != kw::_sym_wild && !af_stringP(name)) {
    x = name;
    component = kw::_sym_name;
    goto ERROR;
  }
  if (type.notnilp() && type != kw::_sym_unspecific && type != kw::_sym_wild && !af_stringP(type)) {
    x = type;
    component = kw::_sym_type;
    goto ERROR;
  }
  if (version != kw::_sym_unspecific && version != kw::_sym_newest &&
      version != kw::_sym_wild && version.notnilp() && !af_fixnumP(version)) {
    x = version;
    component = kw::_sym_version;
  ERROR : {
    SIMPLE_ERROR(BF("%s is not a valid pathname-%s component") % _rep_(x) % _rep_(component));
  }
  }

  if (directory.nilp()) {
// do nothing
#ifdef CLASP_UNICODE
  } else if (String_sp sd = directory.asOrNull<String_O>()) {
    directory = lisp_createList(kw::_sym_absolute, directory);
#endif
  } else if (Str_sp strdirectory = directory.asOrNull<Str_O>()) {
    directory = Cons_O::createList(kw::_sym_absolute, strdirectory);
  } else if (Symbol_sp sdirectory = directory.asOrNull<Symbol_O>()) {
    if (sdirectory == kw::_sym_wild) {
      directory = lisp_createList(kw::_sym_absolute, kw::_sym_wild_inferiors);
    } else {
      x = sdirectory;
      component = kw::_sym_directory;
      goto ERROR;
    }
  } else if (Cons_sp cdirectory = directory.asOrNull<Cons_O>()) {
    directory = cl_copyList(cdirectory);
  } else {
    x = directory;
    component = kw::_sym_directory;
    goto ERROR;
  }
  p->_Host = host;
  {
    T_sp tocase = normalize_case(p, kw::_sym_local);
    if (af_logicalPathnameP(p))
      fromcase = kw::_sym_common;
    else
      fromcase = normalize_case(p, fromcase);
    p->_Host =
        translate_component_case(host, fromcase, tocase);
    p->_Device =
        translate_component_case(device, fromcase, tocase);
    directory =
        translate_list_case(directory, fromcase, tocase); // .as<List_O>()
    p->_Directory = directory;
    p->_Name =
        translate_component_case(name, fromcase, tocase);
    p->_Type =
        translate_component_case(type, fromcase, tocase);
    p->_Version = version;
  }
  //  List_sp directory_copy = cl_copyList(directory);
  directory = destructively_check_directory(directory, af_logicalPathnameP(p), 0);
  unlikely_if(directory == kw::_sym_error) {
    eval::funcall(cl::_sym_error, cl::_sym_fileError, kw::_sym_pathname, p);
    //cl_error(3, @'file-error', kw::_sym_pathname, p);
  }
  p->_Directory = directory;
  return (p);
}

bool Pathname_O::equal(T_sp obj) const {
  if (obj.nilp())
    return false;
  if (Pathname_sp other = obj.asOrNull<Pathname_O>()) {
    if (!cl_equal(this->_Host, other->_Host))
      return false;
    if (!cl_equal(this->_Device, other->_Device))
      return false;
    if (!cl_equal(this->_Directory, other->_Directory))
      return false;
    if (!cl_equal(this->_Name, other->_Name))
      return false;
    if (!cl_equal(this->_Type, other->_Type))
      return false;
    if (!cl_equal(this->_Version, other->_Version))
      return false;
    return true;
  }
  return false;
}

void Pathname_O::sxhash_(HashGenerator &hg) const {
  if (hg.isFilling())
    hg.hashObject(this->_Host);
  if (hg.isFilling())
    hg.hashObject(this->_Device);
  if (hg.isFilling())
    hg.hashObject(this->_Directory);
  if (hg.isFilling())
    hg.hashObject(this->_Name);
  if (hg.isFilling())
    hg.hashObject(this->_Type);
  if (hg.isFilling())
    hg.hashObject(this->_Version);
}

Pathname_sp Pathname_O::tilde_expand(Pathname_sp pathname) {
  /*
	 * If the pathname is a physical one, without hostname, without device
	 * and the first element is either a tilde '~' or '~' followed by
	 * a user name, we merge the user homedir pathname with this one.
	 */
  T_sp directory, head;
  if (af_logicalPathnameP(pathname) || pathname->_Host.notnilp() || pathname->_Device.notnilp()) {
    return pathname;
  }
  directory = pathname->_Directory;
  if (!cl_consp(directory) || CONS_CAR(directory) != kw::_sym_relative || CONS_CDR(directory).nilp()) {
    return pathname;
  }
  head = oCadr(directory);
  if (af_stringP(head) && cl_length(head) > 0 &&
      af_char(head, 0) == '~') {
    /* Remove the tilde component */
    gc::As<Cons_sp>(directory)->rplacd(oCddr(directory));
    pathname = af_mergePathnames(pathname, homedirPathname(gc::As<Str_sp>(head)), kw::_sym_default);
  }
  return pathname;
}

#define WORD_INCLUDE_DELIM 1
#define WORD_ALLOW_ASTERISK 2
#define WORD_EMPTY_IS_NIL 4
#define WORD_LOGICAL 8
#define WORD_SEARCH_LAST_DOT 16
#define WORD_ALLOW_LEADING_DOT 32
#define WORD_DISALLOW_SLASH 64
#define WORD_DISALLOW_SEMICOLON 128

static T_sp
make_one(T_sp s, size_t start, size_t end) {
  return gc::As<Str_sp>(s)->subseq(start, make_fixnum((uint)end));
}

static int is_colon(int c) { return c == ':'; }
static int is_slash(int c) { return IS_DIR_SEPARATOR(c); }
static int is_semicolon(int c) { return c == ';'; }
static int is_dot(int c) { return c == '.'; }
static int is_null(int c) { return c == '\0'; }

/*
 * Parses a word from string `S' until either:
 *	1) character `DELIM' is found
 *	2) end of string is reached
 *	3) a non valid character is found
 * Output is either
 *	1) :error in case (3) above
 *	2) :wild, :wild-inferiors, :up
 *	3) "" or _Nil<T_O>() when word has no elements
 *	5) A non empty string
 */
static T_sp
parse_word(T_sp s, delim_fn delim, int flags, size_t start,
           size_t end, size_t *end_of_word) {
  size_t i, j, last_delim = end;
  bool wild_inferiors = false;

  i = j = start;
  for (; i < end; i++) {
    bool valid_char;
    size_t c = af_char(s, i);
    if (delim(c)) {
      if ((i == start) && (flags & WORD_ALLOW_LEADING_DOT)) {
        /* Leading dot is included */
        continue;
      }
      last_delim = i;
      if (!(flags & WORD_SEARCH_LAST_DOT)) {
        break;
      }
    }
    if (c == '*') {
      if (!(flags & WORD_ALLOW_ASTERISK))
        valid_char = false; /* Asterisks not allowed in this word */
      else {
        wild_inferiors = (i > start && af_char(s, i - 1) == '*');
        valid_char = true; /* single "*" */
      }
    } else if (c == ';' && (flags & WORD_DISALLOW_SEMICOLON)) {
      valid_char = 0;
    } else if (c == '/' && (flags & WORD_DISALLOW_SLASH)) {
      valid_char = 0;
    } else {
      valid_char = c != 0;
    }
    if (!valid_char) {
      *end_of_word = start;
      return kw::_sym_error;
    }
  }
  if (i > last_delim) {
    /* Go back to the position of the last delimiter */
    i = last_delim;
  }
  if (i < end) {
    *end_of_word = i + 1;
  } else {
    *end_of_word = end;
    /* We have reached the end of the string without finding
	       the proper delimiter */
    if (flags & WORD_INCLUDE_DELIM) {
      *end_of_word = start;
      return _Nil<T_O>();
    }
  }
  switch (i - j) {
  case 0:
    if (flags & WORD_EMPTY_IS_NIL)
      return _Nil<T_O>();
    return Str_O::create(""); // cl_core.null_string;
  case 1:
    if (af_char(s, j) == '*')
      return kw::_sym_wild;
    break;
  case 2: {
    size_t c0 = af_char(s, j);
    size_t c1 = af_char(s, j + 1);
    if (c0 == '*' && c1 == '*')
      return kw::_sym_wild_inferiors;
    if (!(flags & WORD_LOGICAL) && c0 == '.' && c1 == '.')
      return kw::_sym_up;
    break;
  }
  default:
    if (wild_inferiors) /* '**' surrounded by other characters */
      return kw::_sym_error;
  }
  return make_one(s, j, i);
}

/*
 * Parses a logical or physical directory tree. Output is always a
 * list of valid directory components, which may be just NIL.
 *
 * INV: When parsing of directory components has failed, a valid list
 * is also returned, and it will be later in the parsing of
 * pathname-name or pathname-type when the same error is detected.
 */

static T_sp
parse_directories(T_sp s, int flags, size_t start, size_t end,
                  size_t *end_of_dir) {
  size_t i, j;
  List_sp path = _Nil<T_O>();
  delim_fn delim = (flags & WORD_LOGICAL) ? is_semicolon : is_slash;

  flags |= WORD_INCLUDE_DELIM | WORD_ALLOW_ASTERISK;
  *end_of_dir = start;
  for (i = j = start; i < end; j = i) {
    T_sp part = parse_word(s, delim, flags, j, end, &i);
    if (part == kw::_sym_error || part.nilp())
      break;
    if (Str_sp spart = part.asOrNull<Str_O>()) {
      if (spart->get() == "") { /* "/", ";" */
        if (j != start) {
          if (flags & WORD_LOGICAL)
            return kw::_sym_error;
          *end_of_dir = i;
          continue;
        }
        part = (flags & WORD_LOGICAL) ? kw::_sym_relative : kw::_sym_absolute;
      }
    }
    *end_of_dir = i;
    path = Cons_O::create(part, path);
  }
  return cl_nreverse(path);
}

bool clasp_logical_hostname_p(T_sp host) {
  if (!af_stringP(host))
    return false;
  if (cl::_sym_assoc->fboundp()) {
    return T_sp(eval::funcall(cl::_sym_assoc, host, _lisp->pathnameTranslations(), kw::_sym_test, cl::_sym_string_equal)).notnilp();
  } else {
    if (_lisp->pathnameTranslations().notnilp()) {
      return _lisp->pathnameTranslations().asCons()->assoc(host, _Nil<T_O>(), cl::_sym_string_equal, _Nil<T_O>()).notnilp();
    }
  }
  return false;
}

/*
 * Parses a lisp namestring until the whole substring is parsed or an
 * error is found. It returns a valid pathname or NIL, plus the place
 * where parsing ended in *END_OF_PARSING.
 *
 * The rules are as follows:
 *
 * 1) If a hostname is supplied it determines whether the namestring
 *    will be parsed as logical or as physical.
 *
 * 2) If no hostname is supplied, first it tries parsing using logical
 *    pathname rules and, if no logical hostname is found, then it
 *    tries the physical pathname format.
 *
 * 3) Logical pathname syntax:
 *	[logical-hostname:][;][logical-directory-component;][pathname-name][.pathname-type]
 *
 * 4) Physical pathname syntax:
 *	[device:][[//hostname]/][directory-component/]*[pathname-name][.pathname-type]
 *
 *	logical-hostname, device, hostname = word
 *	logical-directory-component = word | wildcard-word
 *	directory-component = word | wildcard-word | '..' | '.'
 *	pathname-name, pathname-type = word | wildcard-word | ""
 *
 */
Pathname_sp
clasp_parseNamestring(T_sp s, size_t start, size_t end, size_t *ep,
                      T_sp default_host) {
  T_sp host, device, path, name, type, aux, version;
  bool logical = false;

  if (start == end) {
    host = device = path = name = type = aux = version = _Nil<T_O>();
    logical = false;
    goto make_it;
  }
  /* We first try parsing as logical-pathname. In case of
	 * failure, physical-pathname parsing is performed only when
	 * there is no supplied *logical* host name. All other failures
	 * result in _Nil<T_O>() as output.
	 */
  host = parse_word(s, is_colon, WORD_LOGICAL | WORD_INCLUDE_DELIM |
                                     WORD_DISALLOW_SEMICOLON,
                    start, end, ep);
  if (default_host.notnilp()) {
    if (host.nilp() || host == kw::_sym_error)
      host = default_host;
  }
  if (!clasp_logical_hostname_p(host))
    goto physical;
  /*
	 * Logical pathname format:
	 *	[logical-hostname:][;][logical-directory-component;][pathname-name][.pathname-type]
	 */
  logical = true;
  device = kw::_sym_unspecific;
  path = parse_directories(s, WORD_LOGICAL, *ep, end, ep);
  if (cl_consp(path)) {
    if (CONS_CAR(path) != kw::_sym_relative &&
        CONS_CAR(path) != kw::_sym_absolute)
      path = Cons_O::create(kw::_sym_absolute, path);
    path = destructively_check_directory(path, true, false);
  } else {
    path = Cons_O::create(kw::_sym_absolute, path);
  }
  if (path == kw::_sym_error)
    return _Nil<Pathname_O>();
  name = parse_word(s, is_dot, WORD_LOGICAL | WORD_ALLOW_ASTERISK |
                                   WORD_EMPTY_IS_NIL,
                    *ep, end, ep);
  if (name == kw::_sym_error)
    return _Nil<Pathname_O>();
  type = _Nil<T_O>();
  version = _Nil<T_O>();
  if (*ep == start || af_char(s, *ep - 1) != '.')
    goto make_it;
  type = parse_word(s, is_dot, WORD_LOGICAL | WORD_ALLOW_ASTERISK |
                                   WORD_EMPTY_IS_NIL,
                    *ep, end, ep);
  if (type == kw::_sym_error)
    return _Nil<Pathname_O>();
  if (*ep == start || af_char(s, *ep - 1) != '.')
    goto make_it;
  aux = parse_word(s, is_null, WORD_LOGICAL | WORD_ALLOW_ASTERISK |
                                   WORD_EMPTY_IS_NIL,
                   *ep, end, ep);
  if (aux == kw::_sym_error) {
    return _Nil<Pathname_O>();
  } else if (cl_symbolp(aux)) {
    version = aux;
  } else {
    T_mv version_mv = af_parseInteger(gc::As<Str_sp>(aux), 0, _Nil<T_O>(), 10, _lisp->_true());
    T_sp tversion = version_mv;
    Fixnum_sp parsed_length = gc::As<Fixnum_sp>(version_mv.valueGet(1));
    if (unbox_fixnum(parsed_length) == cl_length(aux) &&
        af_integerP(tversion) && clasp_plusp(gc::As<Integer_sp>(tversion))) {
      version = gc::As<Integer_sp>(tversion);
    } else if (af_string_equal(aux, kw::_sym_newest).notnilp()) {
      version = kw::_sym_newest;
    } else {
      return _Nil<Pathname_O>();
    }
  }
  goto make_it;
physical:
  /*
	 * Physical pathname format:
	 *	[[device:[//hostname]]/][directory-component/]*[pathname-name][.pathname-type]
	 */
  logical = false;
/* We only parse a hostname when the device was present. This
	 * requisite is a bit stupid and only applies to the Unix port,
	 * where "//home/" is equivalent to "/home" However, in Windows
	 * we need "//FOO/" to be separately handled, for it is a shared
	 * resource.
	 */
#if defined(CLASP_MS_WINDOWS_HOST)
  if ((start + 1 <= end) && is_slash(af_char(s, start))) {
    device = _Nil<T_O>();
    goto maybe_parse_host;
  }
#endif
  device = parse_word(s, is_colon, WORD_INCLUDE_DELIM | WORD_EMPTY_IS_NIL |
                                       WORD_DISALLOW_SLASH,
                      start, end, ep);
  if (device == kw::_sym_error || device.nilp()) {
    device = _Nil<T_O>();
    host = _Nil<T_O>();
    goto done_device_and_host;
  }
  if (!af_stringP(device)) {
    return _Nil<Pathname_O>();
  }
#if defined(CLASP_MS_WINDOWS_HOST)
maybe_parse_host:
#endif
  /* Files have no effective device. */
  if (af_string_equal(device, kw::_sym_file).notnilp())
    device = _Nil<T_O>();
  start = *ep;
  host = _Nil<T_O>();
  if ((start + 2) <= end && is_slash(af_char(s, start)) &&
      is_slash(af_char(s, start + 1))) {
    host = parse_word(s, is_slash, WORD_EMPTY_IS_NIL,
                      start + 2, end, ep);
    if (host == kw::_sym_error) {
      host = _Nil<T_O>();
    } else if (host.notnilp()) {
      if (!af_stringP(host))
        return _Nil<Pathname_O>();
      start = *ep;
      if (is_slash(af_char(s, --start)))
        *ep = start;
    }
  }
  if (cl_length(device) == 0)
    device = _Nil<T_O>();
done_device_and_host:
  path = parse_directories(s, 0, *ep, end, ep);
  if (cl_consp(path)) {
    if (CONS_CAR(path) != kw::_sym_relative &&
        CONS_CAR(path) != kw::_sym_absolute)
      path = Cons_O::create(kw::_sym_relative, path);
    path = destructively_check_directory(path, false, false);
  }
  if (path == kw::_sym_error)
    return _Nil<Pathname_O>();
  start = *ep;
  name = parse_word(s, is_dot,
                    WORD_ALLOW_LEADING_DOT | WORD_SEARCH_LAST_DOT |
                        WORD_ALLOW_ASTERISK | WORD_EMPTY_IS_NIL,
                    start, end, ep);
  if (name == kw::_sym_error)
    return _Nil<Pathname_O>();
  if ((*ep - start) <= 1 || af_char(s, *ep - 1) != '.') {
    type = _Nil<T_O>();
  } else {
    type = parse_word(s, is_null, WORD_ALLOW_ASTERISK, *ep, end, ep);
    if (type == kw::_sym_error)
      return _Nil<Pathname_O>();
  }
  version = (name.notnilp() || type.notnilp()) ? kw::_sym_newest : _Nil<Symbol_O>();
make_it:
  if (*ep >= end)
    *ep = end;
  Pathname_sp newpath = Pathname_O::makePathname(host, device, path, name, type, version, kw::_sym_local, logical);
  ASSERTF(af_logicalPathnameP(newpath) == logical, BF("The Class of path(%s) does not match what is specified by logical(%d) - it must match") % _rep_(newpath) % logical);
  return Pathname_O::tilde_expand(newpath);
}

SYMBOL_SC_(CorePkg, defaultPathnameDefaults);
Pathname_sp core_defaultPathnameDefaults(void) {
  /* This routine outputs the value of *default-pathname-defaults*
	 * coerced to type PATHNAME. Special care is taken so that we do
	 * not enter an infinite loop when using PARSE-NAMESTRING, because
	 * this routine might itself try to use the value of this variable. */
  Pathname_sp path = gc::As<Pathname_sp>(af_symbolValue(cl::_sym_STARdefaultPathnameDefaultsSTAR));
  unlikely_if(!af_pathnamep(path)) {
    DynamicScopeManager(cl::_sym_STARdefaultPathnameDefaultsSTAR, getcwd());
    ERROR_WRONG_TYPE_KEY_ARG(core::_sym_defaultPathnameDefaults, cl::_sym_STARdefaultPathnameDefaultsSTAR, path, cl::_sym_Pathname_O);
  }
  return path;
}

#define ARGS_cl_pathname "(arg)"
#define DECL_cl_pathname ""
#define DOCS_cl_pathname "pathname"
Pathname_sp cl_pathname(T_sp x) {
  _G();
  if (x.nilp()) {
    SIMPLE_ERROR(BF("The only argument for pathname is nil"));
  }
L:
  if (Str_sp strx = x.asOrNull<Str_O>()) {
    x = af_parseNamestring(strx);
  } else if (gc::IsA<Pathname_sp>(x)) {
    // do nothing
  } else if (gc::IsA<Stream_sp>(x)) {
    x = clasp_filename(x);
    goto L;
  } else {
    ERROR_WRONG_TYPE_ONLY_ARG(cl::_sym_pathname, x, Cons_O::createList(cl::_sym_or, cl::_sym_fileStream, cl::_sym_string, cl::_sym_pathname));
  }
  return gc::As<Pathname_sp>(x);
}

T_sp cl_logical_pathname(T_sp x) {
  x = cl_pathname(x);
  if (!af_logicalPathnameP(x)) {
    eval::funcall(cl::_sym_simpleTypeError,
                  kw::_sym_formatControl, Str_O::create("~S cannot be coerced to a logical pathname."),
                  kw::_sym_formatArguments, lisp_createList(x),
                  kw::_sym_expectedType, cl::_sym_LogicalPathname_O,
                  kw::_sym_datum, x);
  }
  return x;
}

#define default_device(host) _Nil<T_O>()

Pathname_sp clasp_mergePathnames(T_sp tpath, T_sp tdefaults, T_sp defaultVersion) {
  _G();
  T_sp host, device, directory, name, type, version;
  Symbol_sp tocase;

  Pathname_sp defaults = cl_pathname(tdefaults);
  Pathname_sp path = af_parseNamestring(tpath, _Nil<T_O>(), defaults);
  host = path->_Host;
  if (host.nilp())
    host = defaults->_Host;
  tocase = host_case(host);
  if (path->_Device.nilp()) {
    if (path->_Host.nilp())
      device = af_pathnameDevice(defaults, tocase);
    else if (path->_Host == defaults->_Host)
      device = defaults->_Device;
    else
      device = default_device(path->_Host);
  } else {
    device = path->_Device;
  }
  if (path->_Directory.nilp()) {
    directory = af_pathnameDirectory(defaults, tocase);
  } else if (CONS_CAR(path->_Directory) == kw::_sym_absolute) {
    directory = path->_Directory;
  } else if (defaults->_Directory.notnilp()) {
    directory = Cons_O::append(af_pathnameDirectory(defaults, tocase),
                               oCdr(path->_Directory));
    /* Eliminate redundant :back */
    directory = destructively_check_directory(directory, true, true);
  } else {
    directory = path->_Directory;
  }
  name = path->_Name;
  if (name.nilp()) {
    name = af_pathnameName(defaults, tocase);
  }
  type = path->_Type;
  if (type.nilp()) {
    type = af_pathnameType(defaults, tocase);
  }
  version = path->_Version;
  if (path->_Name.nilp()) {
    if (version.nilp())
      version = defaults->_Version;
  }
  if (version.nilp()) {
    version = defaultVersion;
  }
  if (defaultVersion == kw::_sym_default) {
    if (name.nilp() && type.nilp()) {
      version = _Nil<T_O>();
    } else {
      version = kw::_sym_newest;
    }
  }
  /*
	  In this implementation, version is not considered
	*/
  defaults = Pathname_O::makePathname(host, device, directory, name,
                                      type, version, tocase);
  return defaults;
}

#define ARGS_af_mergePathnames "(arg &optional (default-pathname *default-pathname-defaults*) (default-version :newest))"
#define DECL_af_mergePathnames ""
#define DOCS_af_mergePathnames "mergePathnames"
Pathname_sp af_mergePathnames(T_sp path, T_sp defaults, T_sp defaultVersion) {
  _G();
  if (defaults.nilp()) {
    defaults = af_symbolValue(cl::_sym_STARdefaultPathnameDefaultsSTAR);
  }
  path = cl_pathname(path);
  defaults = cl_pathname(defaults);
  return clasp_mergePathnames(path, defaults, defaultVersion);
}

/* FIXME! WILD-PATHNAME-P is missing! */

#define ARGS_af_wildPathnameP "(pathname &optional component)"
#define DECL_af_wildPathnameP ""
#define DOCS_af_wildPathnameP "wildPathnameP"
bool af_wildPathnameP(T_sp tpathname, T_sp component) {
  _G();
  bool checked = 0;
  Pathname_sp pathname = cl_pathname(tpathname);
  if (component.nilp() || component == kw::_sym_host) {
    if (pathname->_Host == kw::_sym_wild)
      return true;
    checked = 1;
  }
  if (component.nilp() || component == kw::_sym_device) {
    if (pathname->_Device == kw::_sym_wild)
      return true;
    checked = 1;
  }
  if (component.nilp() || component == kw::_sym_version) {
    if (pathname->_Version == kw::_sym_wild)
      return true;
    checked = 1;
  }
  if (component.nilp() || component == kw::_sym_name) {
    T_sp name = pathname->_Name;
    if (name.notnilp() &&
        (name == kw::_sym_wild || clasp_wild_string_p(name)))
      return true;
    checked = 1;
  }
  if (component.nilp() || component == kw::_sym_type) {
    T_sp name = pathname->_Type;
    if (name.notnilp() &&
        (name == kw::_sym_wild || clasp_wild_string_p(name)))
      return true;
    checked = 1;
  }
  if (component.nilp() || component == kw::_sym_directory) {
    T_sp list = pathname->_Directory;
    checked = 1;
    for (; list.notnilp(); list = CONS_CDR(list)) {
      T_sp name = CONS_CAR(list);
      if (name.notnilp() &&
          (name == kw::_sym_wild || name == kw::_sym_wild_inferiors ||
           clasp_wild_string_p(name))) {
        return true;
      }
    }
  }
  if (checked == 0) {
    SIMPLE_ERROR(BF("%s is not a valid pathname component") % _rep_(component));
  }
  return false;
};

/*
 * coerce_to_file_pathname(P) converts P to a physical pathname,
 * for a file which is accesible in our filesystem.
 * INV: Wildcards are allowed.
 * INV: A fresh new copy of the pathname is created.
 * INV: The pathname is absolute.
 */

#define ARGS_af_coerceToFilePathname "(tpathname)"
#define DECL_af_coerceToFilePathname ""
#define DOCS_af_coerceToFilePathname "coerceToFilePathname"
Pathname_sp af_coerceToFilePathname(T_sp tpathname) {
  _G();
  Pathname_sp pathname = af_coerceToPhysicalPathname(tpathname);
  pathname = af_mergePathnames(pathname);
#if 0
#if !defined(cygwin) && !defined(CLASP_MS_WINDOWS_HOST)
	if (pathname->_Device.notnilp())
	    FEerror("Device ~S not yet supported.", 1,
		    pathname->_Device);
	if (pathname->_Host.notnilp())
	    FEerror("Access to remote files not yet supported.", 0);
#endif
#endif
  if (pathname->_Directory.nilp() ||
      CONS_CAR(pathname->_Directory) == kw::_sym_relative) {
    pathname = af_mergePathnames(pathname, getcwd(0));
  }
  return pathname;
}

/*
 * af_coerceToPhysicalPathname(P) converts P to a physical pathname,
 * performing the appropiate transformation if P was a logical pathname.
 */

#define ARGS_af_coerceToPhysicalPathname "(x)"
#define DECL_af_coerceToPhysicalPathname ""
#define DOCS_af_coerceToPhysicalPathname "coerceToPhysicalPathname"
Pathname_sp af_coerceToPhysicalPathname(T_sp x) {
  Pathname_sp px = cl_pathname(x);
  if (af_logicalPathnameP(px))
    return af_translateLogicalPathname(px);
  return px;
}

/*
 * si_coerce_to_filename(P) converts P to a physical pathname and then to
 * a namestring. The output must always be a new simple-string which can
 * be used by the C library.
 * INV: No wildcards are allowed.
 */

#define ARGS_af_coerceToFilename "(pathname-orig)"
#define DECL_af_coerceToFilename ""
#define DOCS_af_coerceToFilename "coerceToFilename"
Str_sp af_coerceToFilename(T_sp pathname_orig) {
  _G();
  Pathname_sp pathname;

  /* We always go through the pathname representation and thus
	 * cl_namestring() always outputs a fresh new string */
  ASSERT(pathname_orig);
  pathname = af_coerceToFilePathname(pathname_orig);
  if (af_wildPathnameP(pathname, _Nil<T_O>())) {
    ERROR(cl::_sym_fileError, Cons_O::createList(kw::_sym_pathname, pathname_orig));
  }
  T_sp tnamestring = clasp_namestring(pathname,
                                      CLASP_NAMESTRING_TRUNCATE_IF_ERROR |
                                          CLASP_NAMESTRING_FORCE_BASE_STRING);
  if (tnamestring.nilp()) {
    SIMPLE_ERROR(BF("Pathname without a physical namestring:"
                    "\n :HOST %s"
                    "\n :DEVICE %s"
                    "\n :DIRECTORY %s"
                    "\n :NAME %s"
                    "\n :TYPE %s"
                    "\n :VERSION %s") %
                 _rep_(pathname->_Host) % _rep_(pathname->_Device) % _rep_(pathname->_Directory) % _rep_(pathname->_Name) % _rep_(pathname->_Type) % _rep_(pathname->_Version));
  }
  Str_sp namestring = gc::As<Str_sp>(tnamestring);
  if (_lisp->pathMax() != -1 &&
      cl_length(namestring) >= _lisp->pathMax() - 16)
    SIMPLE_ERROR(BF("Too long filename: %s.") % namestring->get());
  return namestring;
}

/*
  clasp_namestring(x, flag) converts a pathname to a namestring.
  if flag is true, then the pathname may be coerced to the requirements
  of the filesystem, removing fields that have no meaning (such as
  version, or type, etc); otherwise, when it is not possible to
  produce a readable representation of the pathname, NIL is returned.
*/
T_sp clasp_namestring(T_sp tx, int flags) {
  bool logical;
  T_sp l, y;
  T_sp host;
  bool truncate_if_unreadable = flags & CLASP_NAMESTRING_TRUNCATE_IF_ERROR;

  Pathname_sp x = cl_pathname(tx);

  /* INV: Pathnames can only be created by mergin, parsing namestrings
	 * or using clasp_make_pathname(). In all of these cases BRCL will complain
	 * at creation time if the pathname has wrong components.
	 */
  T_sp buffer = clasp_make_string_output_stream(); //(128, 1);
  logical = af_logicalPathnameP(x);
  host = x->_Host;
  if (logical) {
    if ((y = x->_Device) != kw::_sym_unspecific &&
        truncate_if_unreadable)
      return _Nil<T_O>();
    if (host.notnilp()) {
      cl_write_sequence(gc::As<Str_sp>(host), buffer, make_fixnum(0), _Nil<T_O>());
      clasp_write_string(":", buffer);
    }
  } else {
    if ((y = x->_Device).notnilp()) {
      cl_write_sequence(gc::As<Str_sp>(y), buffer, make_fixnum(0), _Nil<T_O>());
      clasp_write_string(":", buffer);
    }
    if (host.notnilp()) {
#if !defined(CLASP_MS_WINDOWS_HOST)
      if (y.nilp()) {
        clasp_write_string("file:", buffer);
      }
#endif
      clasp_write_string("//", buffer);
      cl_write_sequence(gc::As<Str_sp>(host), buffer, make_fixnum(0), _Nil<T_O>());
    }
  }
  l = x->_Directory;
  if (cl_endp(l))
    goto NO_DIRECTORY;
  y = CONS_CAR(l);
  if (y == kw::_sym_relative) {
    if (logical)
      clasp_write_char(';', buffer);
  } else {
    if (!logical)
      clasp_write_string(DIR_SEPARATOR, buffer);
  }
  l = CONS_CDR(l);
  for (; l.notnilp(); l = CONS_CDR(l)) {
    y = CONS_CAR(l);
    if (y == kw::_sym_up) {
      clasp_write_string("..", buffer);
    } else if (y == kw::_sym_wild) {
      clasp_write_string("*", buffer);
    } else if (y == kw::_sym_wild_inferiors) {
      clasp_write_string("**", buffer);
    } else if (y != kw::_sym_back) {
      cl_write_sequence(gc::As<Str_sp>(y), buffer, make_fixnum(0), _Nil<T_O>());
    } else {
      /* Directory :back has no namestring representation */
      return _Nil<T_O>();
    }
    clasp_write_char(logical ? ';' : DIR_SEPARATOR_CHAR, buffer);
  }
NO_DIRECTORY:
  if (unbox_fixnum(gc::As<Fixnum_sp>(clasp_file_position(buffer))) == 0) {
    if ((af_stringP(x->_Name) &&
         clasp_memberChar(':', x->_Name)) ||
        (af_stringP(x->_Type) &&
         clasp_memberChar(':', x->_Type)))
      clasp_write_string(":", buffer);
  }
  y = x->_Name;
  if (y.notnilp()) {
    if (y == kw::_sym_wild) {
      clasp_write_string("*", buffer);
    } else {
      cl_write_sequence(gc::As<Str_sp>(y), buffer, make_fixnum(0), _Nil<T_O>());
    }
  } else if (!logical && !x->_Type.nilp()) {
    /* #P".txt" is :NAME = ".txt" :TYPE = NIL and
	       hence :NAME = NIL and :TYPE != NIL does not have
	       a printed representation */
    return _Nil<T_O>();
  }
  y = x->_Type;
  if (y == kw::_sym_unspecific) {
    return _Nil<T_O>();
  } else if (y.notnilp()) {
    if (y == kw::_sym_wild) {
      clasp_write_string(".*", buffer);
    } else {
      clasp_write_string(".", buffer);
      cl_write_sequence(gc::As<Str_sp>(y), buffer, make_fixnum(0), _Nil<T_O>());
    }
  }
  y = x->_Version;
  if (logical) {
    if (y.notnilp()) {
      clasp_write_string(".", buffer);
      if (y == kw::_sym_wild) {
        clasp_write_string("*", buffer);
      } else if (y == kw::_sym_newest) {
        cl_write_sequence(af_symbolName(gc::As<Symbol_sp>(y)), buffer,
                          make_fixnum(0), _Nil<T_O>());
      } else {
        /* Since the printer is not reentrant,
		     * we cannot use cl_write and friends.
		     */
        int n = unbox_fixnum(gc::As<Fixnum_sp>(y));
        int i;
        char b[FIXNUM_BITS / 2];
        for (i = 0; n; i++) {
          b[i] = n % 10 + '0';
          n = n / 10;
        }
        if (i == 0)
          b[i++] = '0';
        while (i--) {
          clasp_write_char(b[i], buffer);
        }
      }
    }
  } else if (!truncate_if_unreadable) {
    /* Namestrings of physical pathnames have restrictions... */
    if (x->_Name.nilp() && x->_Type.nilp()) {
      /* Directories cannot have a version number */
      if (y.notnilp())
        return _Nil<T_O>();
    } else if (y != kw::_sym_newest) {
      /* Filenames have an implicit version :newest */
      return _Nil<T_O>();
    }
  }
  Str_sp sbuffer = gc::As<Str_sp>(cl_get_output_stream_string(buffer));
#ifdef CLASP_UNICODE
  if (CLASP_EXTENDED_STRING_P(buffer) &&
      (flags & CLASP_NAMESTRING_FORCE_BASE_STRING)) {
    unlikely_if(!clasp_fits_in_base_string(buffer))
        FEerror("The filesystem does not accept filenames "
                "with extended characters: ~S",
                1, buffer);
    buffer = si_copy_to_simple_base_string(buffer);
  }
#endif
  return sbuffer;
}

#define ARGS_cl_namestring "(pathname)"
#define DECL_cl_namestring ""
#define DOCS_cl_namestring "namestring"
T_sp cl_namestring(T_sp x) {
  _G();
  return clasp_namestring(x, CLASP_NAMESTRING_TRUNCATE_IF_ERROR);
}

#define ARGS_af_parseNamestring "(thing &optional host defaults &key (start 0) end junk-allowed)"
#define DECL_af_parseNamestring ""
#define DOCS_af_parseNamestring "parseNamestring"
T_mv af_parseNamestring(T_sp thing, T_sp host, T_sp tdefaults, Fixnum_sp start, T_sp end, bool junkAllowed) {
  _G();
  T_sp tempdefaults = (tdefaults.nilp()) ? cl::_sym_STARdefaultPathnameDefaultsSTAR->symbolValue() : gc::As<T_sp>(cl_pathname(tdefaults));
  T_sp output;
  if (host.notnilp()) {
    host = af_string(host);
  }
  if (!af_stringP(thing)) {
    output = cl_pathname(thing);
  } else {
    T_sp default_host = host;
    size_t_pair p;
    size_t ee;
    if (default_host.nilp() && tempdefaults.notnilp()) {
      tempdefaults = cl_pathname(tempdefaults);
      default_host = gc::As<Pathname_sp>(tempdefaults)->_Host;
    }
#ifdef CLASP_UNICODE
    thing = si_coerce_to_base_string(thing);
#endif
    p = sequenceStartEnd(__FILE__, __LINE__, __FUNCTION__, CurrentPkg,
                         gc::As<Str_sp>(thing), start, end);
    output = clasp_parseNamestring(thing, p.start, p.end, &ee, default_host);
    start = make_fixnum(static_cast<uint>(ee));
    if (output.nilp() || ee != p.end) {
      if (junkAllowed) {
        PARSE_ERROR(Str_O::create("Cannot parse the namestring ~S~%from ~S to ~S."),
                    Cons_O::createList(thing, start, end));
      }
      goto OUTPUT;
    }
  }
  if (output.nilp()) {
    SIMPLE_ERROR(BF("output is nil"));
  }
  if (host.notnilp() && !gc::As<Pathname_sp>(output)->_Host->equal(host)) {
    SIMPLE_ERROR(BF("The pathname %sS does not contain the required host %s.") % _rep_(thing) % _rep_(host));
  }
OUTPUT:
  return Values(output, start);
};

#define ARGS_af_makePathname "(&key (host nil hostp) (device nil devicep) (directory nil directoryp) (name nil namep) (type nil typep) (version nil versionp) ((:case scase) :local) defaults)"
#define DECL_af_makePathname ""
#define DOCS_af_makePathname "makePathname"
Pathname_sp af_makePathname(T_sp host, bool hostp, T_sp device, bool devicep, T_sp directory, bool directoryp, T_sp name, bool namep, T_sp type, bool typep, T_sp version, bool versionp, T_sp scase, T_sp odefaults) {
  _G();
  Pathname_sp x;
  Pathname_sp defaults;
  if (odefaults.nilp()) {
    defaults = core_defaultPathnameDefaults();
    defaults = Pathname_O::makePathname(defaults->_Host,
                                        _Nil<T_O>(), _Nil<T_O>(), _Nil<T_O>(), _Nil<T_O>(), _Nil<T_O>(),
                                        kw::_sym_local);
  } else {
    defaults = cl_pathname(odefaults);
  }
  if (!hostp)
    host = defaults->_Host;
  x = Pathname_O::makePathname(host, device, directory, name, type, version, scase);
  if (!devicep) {
    /* meister added the following to default device for logical pathnames to :unspecific like sbcl
		   See sbcl>>target-pathname.lisp %make-maybe-logical-pathname */
    if (af_logicalPathnameP(x)) {
      x->_Device = kw::_sym_unspecific;
    }
  }
  if (!directoryp)
    x->_Directory = defaults->_Directory;
  if (!namep)
    x->_Name = defaults->_Name;
  if (!typep)
    x->_Type = defaults->_Type;
  if (!versionp)
    x->_Version = defaults->_Version;
  return x;
}

#define ARGS_af_pathnameHost "(pname &key ((:case scase) :local))"
#define DECL_af_pathnameHost ""
#define DOCS_af_pathnameHost "pathnameHost"
T_sp af_pathnameHost(T_sp tpname, Symbol_sp scase) {
  _G();
  Pathname_sp pname = cl_pathname(tpname);
  return translate_component_case(pname->_Host,
                                  normalize_case(pname, kw::_sym_local),
                                  normalize_case(pname, scase));
}

#define ARGS_af_pathnameDevice "(pname &key ((:case scase) :local))"
#define DECL_af_pathnameDevice ""
#define DOCS_af_pathnameDevice "pathnameDevice"
T_sp af_pathnameDevice(T_sp tpname, Symbol_sp scase) {
  _G();
  Pathname_sp pname = cl_pathname(tpname);
  return translate_component_case(pname->_Device,
                                  normalize_case(pname, kw::_sym_local),
                                  normalize_case(pname, scase));
}

#define ARGS_af_pathnameDirectory "(pname &key ((:case scase) :local))"
#define DECL_af_pathnameDirectory ""
#define DOCS_af_pathnameDirectory "pathnameDirectory"
T_sp af_pathnameDirectory(T_sp tpname, Symbol_sp scase) {
  _G();
  Pathname_sp pname = cl_pathname(tpname);
  return translate_component_case(pname->_Directory,
                                  normalize_case(pname, kw::_sym_local),
                                  normalize_case(pname, scase));
  // Directory
}

#define ARGS_af_pathnameName "(pname &key ((:case scase) :local))"
#define DECL_af_pathnameName ""
#define DOCS_af_pathnameName "pathnameName"
T_sp af_pathnameName(T_sp tpname, Symbol_sp scase) {
  _G();
  Pathname_sp pname = cl_pathname(tpname);
  return translate_component_case(pname->_Name,
                                  normalize_case(pname, kw::_sym_local),
                                  normalize_case(pname, scase));
  // Name
}

#define ARGS_af_pathnameType "(pname &key ((:case scase) :local))"
#define DECL_af_pathnameType ""
#define DOCS_af_pathnameType "pathnameType"
T_sp af_pathnameType(T_sp tpname, Symbol_sp scase) {
  _G();
  Pathname_sp pname = cl_pathname(tpname);
  return translate_component_case(pname->_Type,
                                  normalize_case(pname, kw::_sym_local),
                                  normalize_case(pname, scase));
  // Type
}

#define ARGS_af_pathnameVersion "(pname)"
#define DECL_af_pathnameVersion ""
#define DOCS_af_pathnameVersion "pathnameVersion"
T_sp af_pathnameVersion(T_sp tpname) {
  _G();
  Pathname_sp pname = cl_pathname(tpname);
  return pname->_Version;
};

#define ARGS_af_fileNamestring "(tpname)"
#define DECL_af_fileNamestring ""
#define DOCS_af_fileNamestring "fileNamestring"
Str_sp af_fileNamestring(T_sp tpname) {
  Pathname_sp pname = cl_pathname(tpname);
  return clasp_namestring(Pathname_O::makePathname(_Nil<T_O>(), _Nil<T_O>(), _Nil<T_O>(),
                                                   pname->_Name,
                                                   pname->_Type,
                                                   pname->_Version,
                                                   kw::_sym_local),
                          CLASP_NAMESTRING_TRUNCATE_IF_ERROR);
}

#define ARGS_af_directoryNamestring "(tpname)"
#define DECL_af_directoryNamestring ""
#define DOCS_af_directoryNamestring "directoryNamestring"
Str_sp af_directoryNamestring(T_sp tpname) {
  _G();
  Pathname_sp pname = cl_pathname(tpname);
  return clasp_namestring(Pathname_O::makePathname(_Nil<T_O>(), _Nil<T_O>(),
                                                   pname->_Directory,
                                                   _Nil<T_O>(), _Nil<T_O>(), _Nil<T_O>(),
                                                   kw::_sym_local),
                          CLASP_NAMESTRING_TRUNCATE_IF_ERROR);
}

#define ARGS_af_hostNamestring "(tpname)"
#define DECL_af_hostNamestring ""
#define DOCS_af_hostNamestring "hostNamestring"
Str_sp af_hostNamestring(T_sp tpname) {
  _G();
  Pathname_sp pname = cl_pathname(tpname);
  T_sp host = pname->_Host;
  Str_sp shost;
  if (host.nilp() || host == kw::_sym_wild) {
    shost = Str_O::create("");
  } else {
    shost = gc::As<Str_sp>(host);
  }
  return shost;
}

//#define EN_MATCH(p1,p2,el) (clasp_equalp(p1->pathname.el, p2->pathname.el)? _Nil<T_O>() : p1->pathname.el)
#define EN_MATCH(p1, p2, el) (cl_equalp(p1->el, p2->el) ? _Nil<T_O>() : p1->el)

#define ARGS_af_enoughNamestring "(tpath &optional defaults)"
#define DECL_af_enoughNamestring ""
#define DOCS_af_enoughNamestring "enoughNamestring"
Str_sp af_enoughNamestring(T_sp tpath, T_sp tdefaults) {
  _G();
  T_sp newpath, fname;
  Pathname_sp defaults = tdefaults.nilp() ? core_defaultPathnameDefaults() : cl_pathname(tdefaults);
  Pathname_sp path = cl_pathname(tpath);
  T_sp pathdir = path->_Directory;
  T_sp defaultdir = defaults->_Directory;
  if (pathdir.nilp()) {
    pathdir = Cons_O::create(kw::_sym_relative);
  } else if (defaultdir.nilp()) {
    /* The defaults pathname does not have a directory. */
  } else if (CONS_CAR(pathdir) == kw::_sym_relative) {
    /* The pathname is relative to the default one one, so we just output the
	   original one */
  } else {
    /* The new pathname is an absolute one. We compare it with the defaults
	   and if they have some common elements, we just output the remaining ones. */
    /*Integer_sp*/ T_sp tdir_begin = eval::funcall(cl::_sym_mismatch, pathdir, defaultdir,
                                                   kw::_sym_test, cl::_sym_equal);
    if (tdir_begin.nilp()) {
      pathdir = _Nil<T_O>();
    } else {
      Integer_sp dir_begin = gc::As<Integer_sp>(tdir_begin);
      if (clasp_to_int(dir_begin) == cl_length(defaultdir)) {
        pathdir = eval::funcall(cl::_sym_subseq, pathdir, dir_begin);
        pathdir = Cons_O::create(kw::_sym_relative, pathdir);
      }
    }
  }
  fname = EN_MATCH(path, defaults, _Name);
  if (fname.nilp())
    fname = path->_Name;
  /* Create a path with all elements that do not match the default */
  newpath = Pathname_O::makePathname(EN_MATCH(path, defaults, _Host),
                                     EN_MATCH(path, defaults, _Device),
                                     pathdir, fname,
                                     EN_MATCH(path, defaults, _Type),
                                     EN_MATCH(path, defaults, _Version),
                                     kw::_sym_local);
  ASSERTF(af_logicalPathnameP(newpath) == af_logicalPathnameP(path),
          BF("Mismatch between the newpath and path - they must be the same kind and it is the responsibility of makePathname to ensure that they are the same kind"));
  return clasp_namestring(newpath, CLASP_NAMESTRING_TRUNCATE_IF_ERROR);
};
#undef EN_MATCH

/* --------------- PATHNAME MATCHING ------------------ */

bool clasp_wild_string_p(T_sp item) {
  if (af_stringP(item)) {
    size_t i, l = cl_length(item);
    for (i = 0; i < l; i++) {
      claspChar c = af_char(item, i);
      if (c == '\\' || c == '*' || c == '?')
        return 1;
    }
  }
  return 0;
}

/*
 * Take two C strings and check if the first (s) one matches against
 * the pattern given by the second one (p). The pattern is that of a
 * Unix shell except for brackets and curly braces
 */
bool clasp_stringMatch(T_sp s, size_t j, size_t ls,
                       T_sp p, size_t i, size_t lp) {
  while (i < lp) {
    size_t cp = af_char(p, i);
    switch (cp) {
    case '*': {
      /* An asterisk in the pattern matches any
		 * number of characters. We try the shortest
		 * sequence that matches. */
      size_t cn = 0, next;
      for (next = i + 1;
           next < lp && ((cn = af_char(p, next)) == '*');
           next++)
        ;
      if (next == lp) {
        return true;
      }
      while (j < ls) {
        if (clasp_stringMatch(s, j, ls, p, next, lp)) {
          return true;
        }
        j++;
      }
      return false;
      break;
    }
    case '?':
      /* Match any character */
      if (j > ls)
        return false;
      i++;
      j++;
      break;
    case '\\':
      /* Interpret a pattern character literally.
		   Trailing slash is interpreted as a slash. */
      if (++i >= lp)
        i--;
    default:
      if ((j >= ls) || (cp != af_char(s, j))) {
        /* Either there are no characters left in "s"
		     * or the next character does not match. */
        return false;
      }
      i++;
      j++;
    }
  }
  /* At the end all characters should have been matched */
  return (j >= ls);
}

static bool
path_item_match(T_sp a, T_sp mask) {
  if (mask == kw::_sym_wild)
    return true;
  /* If a component in the tested path is a wildcard field, this
	   can only be matched by the same wildcard field in the mask */
  if (!af_stringP(a) || mask.nilp())
    return (a == mask);
  if (!af_stringP(mask)) {
    SIMPLE_ERROR(BF("%s is not supported as mask for pathname-match-p") % _rep_(mask));
  }
  return clasp_stringMatch(a, 0, cl_length(a),
                           mask, 0, cl_length(mask));
}

static bool
path_list_match(T_sp a, T_sp mask) {
  T_sp item_mask;
  while (!cl_endp(mask)) {
    item_mask = CAR(mask);
    mask = CDR(mask);
    if (item_mask == kw::_sym_wild_inferiors) {
      if (cl_endp(mask))
        return true;
      while (!cl_endp(a)) {
        if (path_list_match(a, mask))
          return true;
        a = CDR(a);
      }
      return false;
    } else if (cl_endp(a)) {
      /* A NIL directory should match against :absolute
		   or :relative, in order to perform suitable translations. */
      if (item_mask != kw::_sym_absolute && item_mask != kw::_sym_relative)
        return false;
    } else if (!path_item_match(CAR(a), item_mask)) {
      return false;
    } else {
      a = CDR(a);
    }
  }
  if (!cl_endp(a))
    return false;
  return true;
}

#define ARGS_af_pathnameMatchP "(tpath tmask)"
#define DECL_af_pathnameMatchP ""
#define DOCS_af_pathnameMatchP "pathnameMatchP"
bool af_pathnameMatchP(T_sp tpath, T_sp tmask) {
  bool output = false;
  Pathname_sp path = cl_pathname(tpath);
  Pathname_sp mask = cl_pathname(tmask);
  if (af_logicalPathnameP(path) != af_logicalPathnameP(mask))
    goto OUTPUT;
#if 0
	/* INV: This was checked in the calling routine */
	if (!path_item_match(path->_Host, mask->_Host))
	    goto OUTPUT;
#endif
  /* Missing components default to :WILD */
  if (!mask->_Directory.nilp() &&
      !path_list_match(path->_Directory, mask->_Directory))
    goto OUTPUT;
  if (!path_item_match(path->_Name, mask->_Name))
    goto OUTPUT;
  if (!path_item_match(path->_Type, mask->_Type))
    goto OUTPUT;
  if (mask->_Version.nilp() ||
      path_item_match(path->_Version, mask->_Version))
    output = true;
OUTPUT:
  return output;
}

/* --------------- PATHNAME TRANSLATIONS ------------------ */

static T_sp
coerce_to_from_pathname(T_sp x, T_sp host) {
#ifdef CLASP_UNICODE
  if (String_sp stringx = x.asOrNull<String_O>()) {
    x = af_parseNamestring(x, host);
  } else
#endif
      if (Str_sp strx = x.asOrNull<Str_O>()) {
    x = af_parseNamestring(strx, host);
  }
  if (Pathname_sp pnx = x.asOrNull<Pathname_O>()) {
    if (af_logicalPathnameP(pnx)) {
      return pnx;
    }
  }
  SIMPLE_ERROR(BF("%s is not a valid from-pathname translation") % _rep_(x));
}

#define ARGS_af_pathnameTranslations "(&optional (host nil hostp) set)"
#define DECL_af_pathnameTranslations ""
#define DOCS_af_pathnameTranslations "core::pathnameTranslations"
T_sp af_pathnameTranslations(T_sp host, T_sp hostp, T_sp set) {
  _G();
  if (hostp.nilp())
    return _lisp->pathnameTranslations();
  size_t parsed_len, len;
  T_sp pair, l;
  /* Check that host is a valid host name */
  if (clasp_unlikely(!af_stringP(host)))
    QERROR_WRONG_TYPE_NTH_ARG(1, host, cl::_sym_string);
  host = cl_string_upcase(host);
  len = cl_length(host);
  parse_word(host, is_null, WORD_LOGICAL, 0, len, &parsed_len);
  if (UNLIKELY(parsed_len < len)) {
    SIMPLE_ERROR(BF("Wrong host syntax %s") % _rep_(host));
  }
  /* Find its translation list */
  if (cl::_sym_assoc->fboundp()) {
    pair = eval::funcall(cl::_sym_assoc, host, _lisp->pathnameTranslations(), kw::_sym_test, cl::_sym_string_equal);
  } else {
    // If called before _sym_assoc is setup then invoke assoc directly */
    if (_lisp->pathnameTranslations().notnilp()) {
      pair = _lisp->pathnameTranslations().asCons()->assoc(host, _Nil<T_O>(), cl::_sym_string_equal, _Nil<T_O>());
    } else {
      pair = _Nil<T_O>();
    }
  }
  if (set.nilp()) {
    return (pair.nilp()) ? _Nil<T_O>() : oCadr(pair);
  }
  /* Set the new translation list */
  if (clasp_unlikely(!cl_listp(set))) {
    QERROR_WRONG_TYPE_NTH_ARG(2, set, cl::_sym_list);
  }
  if (pair.nilp()) {
    pair = Cons_O::create(host, Cons_O::create(_Nil<T_O>(), _Nil<T_O>()));
    _lisp->setPathnameTranslations(Cons_O::create(pair, _lisp->pathnameTranslations()));
  }
  for (l = set, set = _Nil<T_O>(); !cl_endp(l); l = CDR(l)) {
    T_sp item = CAR(l);
    T_sp from = coerce_to_from_pathname(oCar(item), host);
    T_sp to = cl_pathname(oCadr(item));
    set = Cons_O::create(Cons_O::create(from, Cons_O::create(to, _Nil<T_O>())), set);
  }
  set = cl_nreverse(set);
  T_sp savedSet = set;
  gc::As<Cons_sp>(oCdr(pair))->rplaca(set);
  return set;
}

static T_sp
find_wilds(T_sp l, T_sp source, T_sp match) {
  size_t i, j, k, ls, lm;

  if (match == kw::_sym_wild)
    return Cons_O::create(source);
  if (!af_stringP(match) || !af_stringP(source)) {
    if (match != source)
      return kw::_sym_error;
    return l;
  }
  ls = cl_length(source);
  lm = cl_length(match);
  for (i = j = 0; i < ls && j < lm;) {
    size_t pattern_char = af_char(match, j);
    if (pattern_char == '*') {
      for (j++, k = i;
           k < ls && af_char(source, k) != pattern_char;
           k++)
        ;
      l = Cons_O::create(make_one(source, i, k), l);
      i = k;
      continue;
    }
    if (af_char(source, i) != pattern_char)
      return kw::_sym_error;
    i++, j++;
  }
  if (i < ls || j < lm)
    return kw::_sym_error;
  return l;
}

static T_sp
find_list_wilds(T_sp a, T_sp mask) {
  T_sp l = _Nil<T_O>(), l2;

  while (!cl_endp(mask)) {
    T_sp item_mask = CAR(mask);
    mask = CDR(mask);
    if (item_mask == kw::_sym_wild_inferiors) {
      l2 = _Nil<T_O>();
      while (!path_list_match(a, mask)) {
        if (cl_endp(a))
          return kw::_sym_error;
        l2 = Cons_O::create(CAR(a), l2);
        a = CDR(a);
      }
      l = Cons_O::create(l2, l);
    } else if (cl_endp(a)) {
      /* A NIL directory should match against :absolute
		   or :relative, in order to perform suitable translations. */
      if (item_mask != kw::_sym_absolute && item_mask != kw::_sym_relative)
        return kw::_sym_error;
    } else {
      l2 = find_wilds(l, CAR(a), item_mask);
      if (l == kw::_sym_error)
        return kw::_sym_error;
      if (!l2.nilp())
        l = Cons_O::create(l2, l);
      a = CDR(a);
    }
  }
  return cl_nreverse(l);
}

static T_sp
copy_wildcards(T_sp *wilds_list, T_sp pattern) {
  size_t i, l, j;
  bool new_string;
  T_sp wilds = *wilds_list;

  if (pattern == kw::_sym_wild) {
    if (cl_endp(wilds))
      return kw::_sym_error;
    pattern = CAR(wilds);
    *wilds_list = CDR(wilds);
    return pattern;
  }
  if (pattern == kw::_sym_wild_inferiors)
    return kw::_sym_error;
  if (!af_stringP(pattern))
    return pattern;

  new_string = false;
  l = cl_length(pattern);
  StrWithFillPtr_sp token = StrWithFillPtr_O::createBufferString();
  for (j = i = 0; i < l;) {
    size_t c = af_char(pattern, i);
    if (c != '*') {
      i++;
      continue;
    }
    if (i != j) {
      token->pushSubString(pattern, j, i);
    }
    new_string = true;
    if (cl_endp(wilds)) {
      return kw::_sym_error;
    }
    token->pushString(CAR(wilds));
    wilds = CDR(wilds);
    j = i++;
  }
  /* Only create a new string when needed */
  if (new_string) {
    pattern = Str_O::create(token->c_str(), token->size());
  }
  //	si_put_buffer_string(token);
  *wilds_list = wilds;
  return pattern;
}

static T_sp
copy_list_wildcards(T_sp *wilds, T_sp to) {
  T_sp l = _Nil<T_O>();

  while (!cl_endp(to)) {
    T_sp d, mask = CAR(to);
    if (mask == kw::_sym_wild_inferiors) {
      T_sp list = *wilds;
      if (cl_endp(list))
        return kw::_sym_error;
      else {
        T_sp dirlist = CAR(list);
        if (cl_consp(dirlist))
          l = Cons_O::append(CAR(list), l);
        else if (!(oCar(list).nilp()))
          return kw::_sym_error;
      }
      *wilds = CDR(list);
    } else {
      d = copy_wildcards(wilds, CAR(to));
      if (d == kw::_sym_error)
        return d;
      l = Cons_O::create(d, l);
    }
    to = CDR(to);
  }
  if (cl_consp(l))
    l = cl_nreverse(l);
  return l;
}

#define ARGS_af_translatePathname "(source from to &key ((:case scase) :local))"
#define DECL_af_translatePathname ""
#define DOCS_af_translatePathname "translatePathname"
Pathname_sp af_translatePathname(T_sp tsource, T_sp tfrom, T_sp tto, T_sp scase) {
  T_sp wilds, d;
  T_sp host, device, directory, name, type, version;
  T_sp tocase;
  /* The pathname from which we get the data */
  Pathname_sp source = cl_pathname(tsource);
  /* The mask applied to the source pathname */
  Pathname_sp from = cl_pathname(tfrom);
  T_sp fromcase = normalize_case(from, kw::_sym_local);
  /* The pattern which says what the output should look like */
  Pathname_sp to = cl_pathname(tto);
  tocase = normalize_case(to, kw::_sym_local);

  if (af_logicalPathnameP(source) != af_logicalPathnameP(from))
    goto error;

  /* Match host names */
  if (af_string_equal(source->_Host, from->_Host).nilp())
    goto error;
  host = to->_Host;

  /* Logical pathnames do not have devices. We just overwrite it. */
  device = to->_Device;

  /* Match directories */
  wilds = find_list_wilds(source->_Directory,
                          from->_Directory);
  if (wilds == kw::_sym_error)
    goto error;
  if ((to->_Directory).nilp()) {
    /* Missing components are replaced */
    d = translate_list_case(source->_Directory, fromcase, tocase);
  } else {
    wilds = translate_list_case(wilds, fromcase, tocase);
    d = copy_list_wildcards(&wilds, to->_Directory);
    if (d == kw::_sym_error)
      goto error;
    if (wilds.notnilp())
      goto error2;
  }
  directory = d;

  /* Match name */
  wilds = find_wilds(_Nil<T_O>(), source->_Name, from->_Name);
  if (wilds == kw::_sym_error)
    goto error2;
  if ((to->_Name.nilp())) {
    d = translate_component_case(source->_Name, fromcase, tocase);
  } else {
    wilds = translate_list_case(wilds, fromcase, tocase);
    d = copy_wildcards(&wilds, to->_Name);
    if (d == kw::_sym_error)
      goto error;
    if (wilds.notnilp())
      goto error2;
  }
  name = d;

  /* Match type */
  wilds = find_wilds(_Nil<T_O>(), source->_Type, from->_Type);
  if (wilds == kw::_sym_error)
    goto error2;
  if ((to->_Type).nilp()) {
    d = translate_component_case(source->_Type, fromcase, tocase);
  } else {
    wilds = translate_list_case(wilds, fromcase, tocase);
    d = copy_wildcards(&wilds, to->_Type);
    if (d == kw::_sym_error)
      goto error;
    if (wilds.notnilp())
      goto error2;
  }
  type = d;

  /* Match version */
  version = to->_Version;
  if (from->_Version == kw::_sym_wild) {
    if (to->_Version == kw::_sym_wild) {
      version = source->_Version;
    }
  }
  return Pathname_O::makePathname(host, device, directory, name, type,
                                  version, tocase);
error:
  SIMPLE_ERROR(BF("%s is not a specialization of path %s") % _rep_(source) % _rep_(from));
error2:
  SIMPLE_ERROR(BF("Number of wildcards in %s do not match  %s") % _rep_(from) % _rep_(to));
}

#define ARGS_af_translateLogicalPathname "(source &key)"
#define DECL_af_translateLogicalPathname ""
#define DOCS_af_translateLogicalPathname "translateLogicalPathname"
Pathname_sp af_translateLogicalPathname(T_sp tsource) {
  _G();
  Pathname_sp pathname = cl_pathname(tsource);
begin:
  if (!af_logicalPathnameP(pathname)) {
    //	    printf("%s:%d Returning non-logical pathname: %s\n", __FILE__, __LINE__, _rep_(pathname).c_str() );
    return pathname;
  }
  List_sp l = eval::funcall(core::_sym_pathnameTranslations, pathname->_Host);
  //  TESTING();
  for (auto cur : l) {     // ; !cl_endp(l); l = CDR(l)) {
    T_sp pair = oCar(cur); // I just noticed that I had oCar(l) in here!!!!!
    if (af_pathnameMatchP(pathname, CAR(pair))) {
      //      printf("%s:%d Trying to translate pathname: %s   pair: %s\n", __FILE__, __LINE__, _rep_(pathname).c_str(), _rep_(pair).c_str() );
      pathname = af_translatePathname(pathname,
                                      CAR(pair),
                                      oCadr(pair),
                                      kw::_sym_local);
      goto begin;
    }
  }
  SIMPLE_ERROR(BF("%s admits no logical pathname translations") % _rep_(pathname));
}

EXPOSE_CLASS(core, Pathname_O);

void Pathname_O::exposeCando(Lisp_sp lisp) {
  class_<Pathname_O>();
}

void Pathname_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, Pathname, "", "", _lisp);
#endif
};

string Pathname_O::__repr__() const {
  stringstream ss;
  gc::Nilable<Str_sp> str = cl_namestring(this->asSmartPtr());
  if (str.nilp()) {
    ss << "#P" << '"' << '"';
  } else {
    ss << "#P" << '"' << str->get() << '"';
  }
  return ss.str();
}

EXPOSE_CLASS(core, LogicalPathname_O);

void LogicalPathname_O::exposeCando(Lisp_sp lisp) {
  class_<LogicalPathname_O>();
}

void LogicalPathname_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, LogicalPathname, "", "", _lisp);
#endif
};

void initialize_pathname() {
  SYMBOL_EXPORT_SC_(CorePkg, coerceToFilename);
  Defun(coerceToFilename);
  SYMBOL_EXPORT_SC_(CorePkg, coerceToFilePathname);
  Defun(coerceToFilePathname);
  SYMBOL_EXPORT_SC_(CorePkg, pathnameTranslations);
  Defun(pathnameTranslations);
  SYMBOL_EXPORT_SC_(CorePkg, coerceToPhysicalPathname);
  Defun(coerceToPhysicalPathname);

  SYMBOL_EXPORT_SC_(ClPkg, pathname);
  ClDefun(pathname);
  SYMBOL_EXPORT_SC_(ClPkg, mergePathnames);
  Defun(mergePathnames);
  SYMBOL_EXPORT_SC_(ClPkg, wildPathnameP);
  Defun(wildPathnameP);
  SYMBOL_EXPORT_SC_(ClPkg, makePathname);
  Defun(makePathname);
  SYMBOL_EXPORT_SC_(ClPkg, pathnameHost);
  Defun(pathnameHost);
  SYMBOL_EXPORT_SC_(ClPkg, pathnameDevice);
  Defun(pathnameDevice);
  SYMBOL_EXPORT_SC_(ClPkg, pathnameDirectory);
  Defun(pathnameDirectory);
  SYMBOL_EXPORT_SC_(ClPkg, pathnameName);
  Defun(pathnameName);
  SYMBOL_EXPORT_SC_(ClPkg, pathnameType);
  Defun(pathnameType);
  SYMBOL_EXPORT_SC_(ClPkg, pathnameVersion);
  Defun(pathnameVersion);
  SYMBOL_EXPORT_SC_(ClPkg, pathnameMatchP);
  Defun(pathnameMatchP);
  SYMBOL_EXPORT_SC_(ClPkg, translatePathname);
  Defun(translatePathname);
  SYMBOL_EXPORT_SC_(ClPkg, translateLogicalPathname);
  Defun(translateLogicalPathname);

  SYMBOL_EXPORT_SC_(ClPkg, namestring);
  ClDefun(namestring);
  SYMBOL_EXPORT_SC_(ClPkg, parseNamestring);
  Defun(parseNamestring);
  SYMBOL_EXPORT_SC_(ClPkg, fileNamestring);
  Defun(fileNamestring);
  SYMBOL_EXPORT_SC_(ClPkg, directoryNamestring);
  Defun(directoryNamestring);
  SYMBOL_EXPORT_SC_(ClPkg, hostNamestring);
  Defun(hostNamestring);
  SYMBOL_EXPORT_SC_(ClPkg, enoughNamestring);
  Defun(enoughNamestring);
};
};
