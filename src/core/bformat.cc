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
// #define DEBUG_LEVEL_FULL

#include <type_traits>
#include <fmt/core.h>
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/bformat.h>
#include <clasp/core/bignum.h>
#include <clasp/core/wrappers.h>
namespace core {

string::size_type find_first_format_specifier(const std::string& str, string::size_type lastPos, bool fmt = false) {
  if (fmt) {
    lastPos = str.find_first_of("{", lastPos);
    if (lastPos == string::npos)
      return str.size();
    for (string::size_type ii = lastPos; ii < str.size(); ii++) {
      if (str[ii] == '}')
        return ii + 1;
    }
  } else {
    lastPos = str.find_first_of("%", lastPos);
    if (lastPos == string::npos)
      return str.size();
    for (string::size_type ii = lastPos; ii < str.size(); ii++) {
      if (str[ii] >= 'a' && str[ii] <= 'z')
        return ii + 1;
    }
  }
  return str.size();
}

void tokenize_format_specifiers(const string& str, vector<string>& tokens, bool fmt = false) {
  tokens.erase(tokens.begin(), tokens.end());
  // Skip delimiters at beginning.
  string::size_type lastPos = 0;
  // Find first "non-delimiter".
  while (lastPos != str.size()) {
    string::size_type pos = find_first_format_specifier(str, lastPos, fmt);
    tokens.push_back(str.substr(lastPos, pos - lastPos));
    lastPos = pos;
  }
}

CL_DEFUN T_sp core__tokenize_format_specifiers(const string& str, bool fmt) {
  std::vector<std::string> parts;
  tokenize_format_specifiers(str, parts, fmt);
  ql::list ll;
  for (auto& part : parts) {
    ll << SimpleBaseString_O::make(part);
  }
  return ll.cons();
}

// If FMT is 0 then use printf(%s) formatting otherwise use fmt formatting({})
template <int FMT> struct formatter {
  std::vector<std::string> _controls;
  std::vector<std::string> _results;
  size_t _pos;
  formatter(const std::string& fmt_string) {
    tokenize_format_specifiers(fmt_string, this->_controls, FMT);
    this->_pos = 0;
  }

  template <typename T> void format(const T& obj) {
    static_assert(FMT == 1);
    const std::string& ctl = this->_controls[this->_pos];
#if (FMT_VERSION > 80000)
    this->_results.push_back(fmt::format(fmt::runtime(ctl), obj));
#else
    // FMT version less than 8.0.0 ... 80000
    this->_results.push_back(fmt::format(ctl, obj));
#endif
    this->_pos++;
  }

  T_sp write(T_sp destination, T_sp output) {
    if (output == _sym_printf) {
      for (size_t ii = 0; ii < this->_pos; ii++) {
        printf("%s", this->_results[ii].c_str());
      }
      for (size_t ii = this->_pos; ii < this->_controls.size(); ii++) {
        printf("%s", this->_controls[ii].c_str());
      }
      return nil<T_O>();
    } else {
      for (size_t ii = 0; ii < this->_pos; ii++) {
        clasp_write_string(this->_results[ii], output);
      }
      for (size_t ii = this->_pos; ii < this->_controls.size(); ii++) {
        clasp_write_string(this->_controls[ii], output);
      }
    }
    return destination.nilp() ? (T_sp)StringOutputStream_O::get_string(output) : nil<T_O>();
  }
};

CL_DEFUN std::string core__tostring(T_sp fobj) {
  if (fobj.fixnump()) {
    Fixnum_sp fint = gc::As<Fixnum_sp>(fobj);
    return fmt::format("{}", unbox_fixnum(fint));
  } else if (fobj.characterp()) {
    Character_sp fc = gc::As<Character_sp>(fobj);
    return fmt::format("{}", (char)unbox_character(fc));
  } else if (fobj.single_floatp()) {
    SingleFloat_sp ff = gc::As<SingleFloat_sp>(fobj);
    return fmt::format("{}", unbox_single_float(ff));
  } else if (core__bignump(fobj)) {
    Bignum_sp flli = gc::As<Bignum_sp>(fobj);
    stringstream ss;
    ss << clasp_to_mpz(flli);
    return ss.str();
  } else if (cl__stringp(fobj)) {
    String_sp ftext = gc::As_unsafe<String_sp>(fobj);
    return ftext->get_std_string();
  } else if (core__double_float_p(fobj)) {
    DoubleFloat_sp freal = gc::As<DoubleFloat_sp>(fobj);
    return fmt::format("{}", freal->get());
  }
  return _rep_(fobj);
}

/*! Boost-format interface - works like CL:format but uses boost format strings
 */
CL_LAMBDA(destination control &rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(Like CL format but uses fmt::format)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__fmt(T_sp destination, const string& original_control, List_sp args) {
  T_sp output;
  if (destination.nilp()) {
    output = my_thread->_BFormatStringOutputStream;
  } else if (destination == _sym_printf) {
    output = destination;
  } else if (destination == _lisp->_true()) {
    output = cl::_sym_STARstandard_outputSTAR->symbolValue();
  } else if (cl__streamp(destination)) {
    output = destination;
  } else {
    TYPE_ERROR(destination, cl::_sym_streamError);
  }
  std::stringstream scontrol;
  std::string control;
  if (original_control.size() > 1) {
    for (int i(0); i < original_control.size(); ++i) {
      if (original_control[i] == '%') {
        switch (original_control[++i]) {
        case 'a':
        case 'A':
          scontrol << '\a';
          break;
        case 'b':
        case 'B':
          scontrol << '\b';
          break;
        case 'e':
        case 'E':
          scontrol << '\x1b';
          break;
        case 'f':
        case 'F':
          scontrol << '\f';
          break;
        case 'n':
        case 'N':
          scontrol << '\n';
          break;
        case 'r':
        case 'R':
          scontrol << '\r';
          break;
        case 't':
        case 'T':
          scontrol << '\t';
          break;
        case 'v':
        case 'V':
          scontrol << '\v';
          break;
        default:
          scontrol << original_control[i];
          break;
        }
      } else {
        scontrol << original_control[i];
      }
    }
    control = scontrol.str();
  } else {
    control = original_control;
  }
  try {
    formatter<1> fmter(control);
    for (auto farg : args) {
      T_sp fobj = oCar(farg);
      if (fobj.fixnump()) {
        Fixnum_sp fint = gc::As<Fixnum_sp>(fobj);
        fmter.format(unbox_fixnum(fint));
      } else if (fobj.characterp()) {
        Character_sp fc = gc::As<Character_sp>(fobj);
        fmter.format((char)unbox_character(fc));
      } else if (fobj.single_floatp()) {
        SingleFloat_sp ff = gc::As<SingleFloat_sp>(fobj);
        fmter.format(unbox_single_float(ff));
      } else if (core__bignump(fobj)) {
        Bignum_sp flli = gc::As<Bignum_sp>(fobj);
        stringstream ss;
        ss << clasp_to_mpz(flli);
        fmter.format(ss.str());
      } else if (cl__stringp(fobj)) {
        String_sp ftext = gc::As_unsafe<String_sp>(fobj);
        fmter.format(ftext->get_std_string());
      } else if (core__double_float_p(fobj)) {
        DoubleFloat_sp freal = gc::As<DoubleFloat_sp>(fobj);
        fmter.format(freal->get());
      } else {
        fmter.format(_rep_(fobj));
      }
    }
    return fmter.write(destination, output);
  } catch (...) {
    SIMPLE_ERROR("Unknown fmt command error in format string: \"{}\"", original_control);
  }
}

DOCGROUP(clasp);
CL_DEFUN void core__fflush() { fflush(stdout); }

CL_LAMBDA(destination control &rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(Subset of CL format - this does the job until the real format is installed)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__format(T_sp destination, T_sp control, List_sp args) {
  stringstream tf;
  if (cl__functionp(control)) {
    SIMPLE_ERROR("Add support for functions as FORMAT controls");
  }
  if (!cl__stringp(control)) {
    SIMPLE_ERROR("FORMAT control must be a string or a function - you gave: {}", _rep_(control));
  }
  string ts = gc::As<String_sp>(control)->get_std_string();
  const char* cur = ts.c_str();
  bool success = true;
  while (*cur) {
    if (*cur == '~') {
      ++cur;
      switch (*cur) {
      case 'c':
      case 'C':
        tf << "{}";
        break;
      case 's':
      case 'S':
        tf << "{}";
        break;
      case 'd':
      case 'D':
        tf << "{}";
        break;
      case 'a':
      case 'A':
        tf << "{}";
        break;
      case 'v':
      case 'V':
        tf << "{}";
        break;
      case '&':
        tf << std::endl;
        break;
      case '%':
        tf << std::endl;
        break;
      default: {
        success = false;
        return core__fmt(destination, "Could not format {} {}", Cons_O::createList(control, args));
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
  if (!success) {
    return core__fmt(destination, "(failed-format <stream> {} {})", Cons_O::createList(control, args));
  }
  return core__fmt(destination, tf.str(), args);
};

CL_DEFUN T_sp core__clformat(T_sp destination, T_sp control, List_sp args) { return cl__format(destination, control, args); }

SYMBOL_SC_(CorePkg, bformat);
SYMBOL_EXPORT_SC_(ClPkg, format);

}; // namespace core
