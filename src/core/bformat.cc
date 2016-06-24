/*
    File: bformat.cc
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
#include <clasp/core/str.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/bformat.h>
#include <clasp/core/bignum.h>
#include <clasp/core/wrappers.h>
namespace core {

/*! Boost-format interface - works like CL:format but uses boost format strings
 */
CL_LAMBDA(destination control &rest args);
CL_DECLARE();
CL_DOCSTRING("Like CL format but uses C/boost format strings");
CL_DEFUN T_sp core__bformat(T_sp destination, const string &control, List_sp args) {
  T_sp output;
  if (destination.nilp()) {
    output = _lisp->bformatStringOutputStream();
  } else if (destination == _sym_printf) {
    output = destination;
  } else {
    output = coerce::outputStreamDesignator(destination);
  }
  boost::format fmter(control);
  string fmter_str;
  TRY() {
    for (auto farg : args) {
      T_sp fobj = oCar(farg);
      if (fobj.fixnump()) {
        Fixnum_sp fint = gc::As<Fixnum_sp>(fobj);
        fmter % unbox_fixnum(fint);
      } else if (fobj.characterp()) {
        Character_sp fc = gc::As<Character_sp>(fobj);
        fmter % (char)unbox_character(fc);
      } else if (fobj.single_floatp()) {
        SingleFloat_sp ff = gc::As<SingleFloat_sp>(fobj);
        fmter % unbox_single_float(ff);
      } else if (core__bignump(fobj)) {
        Bignum_sp flli = gc::As<Bignum_sp>(fobj);
        stringstream ss;
        ss << flli->valueAsString();
        fmter % ss.str();
      } else if (core__simple_string_p(fobj)) {
        Str_sp ftext = gc::As<Str_sp>(fobj);
        fmter % ftext->get();
      } else if (core__double_float_p(fobj)) {
        DoubleFloat_sp freal = gc::As<DoubleFloat_sp>(fobj);
        fmter % freal->get();
      } else {
        fmter % _rep_(fobj);
      }
    }
    fmter_str = fmter.str();
  }
  catch (boost::io::bad_format_string &err) {
    SIMPLE_ERROR(BF("bformat command error: bad format string"));
  }
  catch (boost::io::too_few_args &err) {
    SIMPLE_ERROR(BF("bformat command error: too few args"));
  }
  catch (boost::io::too_many_args &err) {
    SIMPLE_ERROR(BF("bformat command error: too many args"));
  }
  catch (boost::io::out_of_range &err) {
    SIMPLE_ERROR(BF("bformat command error: out of range"));
  }
  catch (...) {
    SIMPLE_ERROR(BF("Unknown bformat command error"));
  }
  if (output == _sym_printf) {
    printf("%s", fmter_str.c_str());
  } else {
    clasp_write_string(fmter_str, output);
  }
  if (destination.nilp()) {
    StringOutputStream_sp sout = gc::As<StringOutputStream_sp>(output);
    return sout->getAndReset();
  }
  return _Nil<T_O>();
}

CL_LAMBDA(destination control &rest args);
CL_DECLARE();
CL_DOCSTRING("Subset of CL format - this does the job until the real format is installed");
CL_DEFUN T_sp cl__format(T_sp destination, T_sp control, List_sp args) {
  stringstream tf;
  if (cl__functionp(control)) {
    SIMPLE_ERROR(BF("Add support for functions as FORMAT controls"));
  }
  if (!cl__stringp(control)) {
    SIMPLE_ERROR(BF("FORMAT control must be a string or a function - you gave: %s") % _rep_(control));
  }
  string ts = gc::As<Str_sp>(control)->get();
  const char *cur = ts.c_str();
  while (*cur) {
    if (*cur == '~') {
      ++cur;
      switch (*cur) {
      case 'c':
        tf << "%c";
        break;
      case 's':
      case 'S':
        tf << "%s";
        break;
      case 'd':
        tf << "%d";
        break;
      case 'a':
      case 'A':
        tf << "%s";
        break;
      case '&':
        tf << std::endl;
        break;
      case '%':
        tf << std::endl;
        break;
      default: {
        stringstream serr;
        serr << "Add support to BFORMAT to translate FORMAT control <tilde>" << *cur << " format control: " << ts ;
        SIMPLE_ERROR(BF("%s") % (serr.str()));
      } break;
      }
      ++cur;
    } else if (*cur == '%') {
      ++cur;
      tf << "%%";
    } else {
      tf << *cur;
      ++cur;
    }
  }
  return core__bformat(destination, tf.str(), args);
};

  SYMBOL_SC_(CorePkg, bformat);
  SYMBOL_EXPORT_SC_(ClPkg, format);


}; /* (>>>namespace<<<) */
