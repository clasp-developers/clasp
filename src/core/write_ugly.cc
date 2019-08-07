/*
    File: write_ugly.cc
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
    print.d -- Print.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.

    Christian Schafmeister translated some of the ECL write_ugly.cc
    code into C++ for use in BRCL

*/

//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/array.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/pathname.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/instance.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/sysprop.h>
#include <clasp/core/numberToString.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/arguments.h>
#include <clasp/core/print.h>
#include <clasp/core/float_to_string.h>
#include <clasp/core/write_symbol.h>
#include <clasp/core/write_ugly.h>
#include <clasp/core/character.h>
#include <clasp/core/wrappers.h>

namespace core {

void Pathname_O::__writeReadable__(T_sp strm) const {
  ql::list l;
  l << cl::_sym_makePathname
    << kw::_sym_host << this->_Host
    << kw::_sym_device << this->_Device
    << kw::_sym_directory
    << eval::funcall(ext::_sym_maybeQuote, this->_Directory)
    << kw::_sym_name << this->_Name
    << kw::_sym_type << this->_Type
    << kw::_sym_version << this->_Version
    << kw::_sym_defaults << _Nil<T_O>();
  clasp_write_string("#.", strm);
  write_object(l.cons(), strm);
}

void Pathname_O::__write__(T_sp strm) const {
  Pathname_sp path = this->const_sharedThis<Pathname_O>();
  if (path.nilp()) SIMPLE_ERROR(BF("%s is about to pass NIL to clasp_namestring") % __FUNCTION__);
  T_sp namestring = clasp_namestring(path, 0);
  bool readably = clasp_print_readably();
  if (namestring.nilp()) {
    if (readably) {
      path->__writeReadable__(strm);
      return;
    }
    if (path.nilp()) SIMPLE_ERROR(BF("%s is about to pass NIL to clasp_namestring") % __FUNCTION__);
    namestring = clasp_namestring(path, 1);
    if (namestring.nilp()) {
      clasp_write_string("#<Unprintable pathname>", strm);
      return;
    }
  }
  if (readably || clasp_print_escape())
    clasp_write_string("#P", strm);
  write_ugly_object(namestring, strm);
}

void Instance_O::__write__(T_sp stream) const {
  if (_sym_STARliteral_print_objectSTAR->symbolValue().notnilp()) {
    eval::funcall(_sym_STARliteral_print_objectSTAR->symbolValue(), this->const_sharedThis<Instance_O>(), stream);
  } else if ( cl::_sym_printObject->fboundp() ) {
    eval::funcall(cl::_sym_printObject, this->const_sharedThis<Instance_O>(), stream);
  } else {
    std::string str = _rep_(this->asSmartPtr());
    clasp_write_string(str,stream);
  }
}

void FuncallableInstance_O::__write__(T_sp stream) const {
  if (_sym_STARliteral_print_objectSTAR->symbolValue().notnilp()) {
    eval::funcall(_sym_STARliteral_print_objectSTAR->symbolValue(), this->const_sharedThis<FuncallableInstance_O>(), stream);
  } else if ( cl::_sym_printObject->fboundp() ) {
    eval::funcall(cl::_sym_printObject, this->const_sharedThis<FuncallableInstance_O>(), stream);
  } else {
    std::string str = _rep_(this->asSmartPtr());
    clasp_write_string(str,stream);
  }
}

  SYMBOL_EXPORT_SC_(CorePkg, structure_print_function);
  SYMBOL_EXPORT_SC_(CorePkg, STARprint_structureSTAR);


void Integer_O::__write__(T_sp stream) const {
  SafeBufferStr8Ns buffer;
  int print_base = clasp_print_base();
  core__integer_to_string(buffer._Buffer, this->const_sharedThis<Integer_O>(),
                       make_fixnum(print_base),
                       cl::_sym_STARprint_radixSTAR->symbolValue().isTrue(),
                       true);
  cl__write_sequence(buffer._Buffer, stream, make_fixnum(0), _Nil<T_O>());
}

void Ratio_O::__write__(T_sp stream) const {  
  SafeBufferStr8Ns buffer;
  int print_base = clasp_print_base();
  core__integer_to_string(buffer._Buffer, this->num(),
                       make_fixnum(print_base),
                       cl::_sym_STARprint_radixSTAR->symbolValue().isTrue(),
                       false);
  buffer._Buffer->vectorPushExtend(clasp_make_character('/'));
  core__integer_to_string(buffer._Buffer, this->den(),
                       make_fixnum(print_base),
                       false,
                       false);
  cl__write_sequence(buffer._Buffer, stream, make_fixnum(0), _Nil<T_O>());
}

void Complex_O::__write__(T_sp stream) const {
  clasp_write_string("#C(", stream);
  write_ugly_object(this->_real, stream);
  clasp_write_char(' ', stream);
  write_ugly_object(this->_imaginary, stream);
  clasp_write_char(')', stream);
}

// Function_O also has a __repr__ method, but it displays too much low level info.
// generic functions go through the instance printer, not this.
void Function_O::__write__(T_sp stream) const {
  clasp_write_string("#<FUNCTION ", stream);
  write_ugly_object(this->functionName(), stream);
  clasp_write_char('>', stream);
}

void
_clasp_write_fixnum(gctools::Fixnum i, T_sp stream) {
  SafeBufferStr8Ns buffer;
  core__integer_to_string(buffer._Buffer,
                       clasp_make_fixnum(i), clasp_make_fixnum(clasp_print_base()), cl::_sym_STARprint_radixSTAR->symbolValue().isTrue(), true);
  cl__write_sequence(buffer._Buffer, stream, make_fixnum(0), _Nil<T_O>());
}

void write_fixnum(T_sp strm, T_sp i) {
  Fixnum_sp fn = gc::As<Fixnum_sp>(i);
  SafeBufferStr8Ns buffer;
  int print_base = clasp_print_base();
  core__integer_to_string(buffer._Buffer, fn,
                       make_fixnum(print_base),
                       cl::_sym_STARprint_radixSTAR->symbolValue().isTrue(),
                       true);
  cl__write_sequence(buffer._Buffer, strm, make_fixnum(0), _Nil<T_O>());
}

void write_single_float(T_sp strm, SingleFloat_sp i) {
  stringstream ss;
  ss << unbox_single_float(i);
  clasp_write_string(ss.str(), strm);
}

void
write_float(Float_sp f, T_sp stream) {
  T_sp result = core_float_to_string_free(f, clasp_make_fixnum(-3), clasp_make_fixnum(8));
  cl__write_sequence(result, stream, clasp_make_fixnum(0), _Nil<T_O>());
}

void write_character(T_sp strm, T_sp chr) {
  ASSERT(chr.characterp());
  // could be a unicode char, don't assume a claspChar
  claspCharacter i = clasp_as_claspCharacter(gc::As<Character_sp>(chr));
  if (!clasp_print_escape() && !clasp_print_readably()) {
    clasp_write_char(i, strm);
  } else {
    clasp_write_string("#\\", strm);
    if (i < 32 || i >= 127) {
      SimpleBaseString_sp name = cl__char_name(clasp_make_character(i));
      clasp_write_string(name->get(), strm);
    } else {
      clasp_write_char(i, strm);
    }
  }
}

T_sp write_ugly_object(T_sp x, T_sp stream) {
  if (x.fixnump()) {
    write_fixnum(stream, x);
  } else if (x.characterp()) {
    write_character(stream, x);
  } else if (x.single_floatp()) {
    write_float(gc::As<SingleFloat_sp>(x), stream);
  } else if (x.generalp() ) {
    General_sp gx(x.unsafe_general());
    if (Float_sp fx = gx.asOrNull<Float_O>()) {
      write_float(fx, stream);
    } else {
      gx->__write__(stream);
    }
  } else if (x.consp() ) {
    Cons_sp cx(x.unsafe_cons());
    cx->__write__(stream);
  } else if (x.valistp()) {
    clasp_write_string("#<VA-LIST: ", stream);
    VaList_sp vl = VaList_sp((gc::Tagged)x.raw_());
    Vaslist valist_scopy(*vl);
    VaList_sp xa(&valist_scopy); // = gc::smart_ptr<Vaslist>((gc::Tagged)last.raw_());
    ql::list l;
    int nargs = xa->remaining_nargs();
    for (int i(0); i < nargs; ++i) l << xa->next_arg();
    core::write_object(l.cons(),stream);
    clasp_write_string(">",stream);
  } else {
    stringstream ss;
    ss << "#<BAD-OBJECT! set break-point at " << __FILE__ << ":" << __LINE__ << " and check backtrace>";
    clasp_write_string(ss.str());
  }
  return x;
}

CL_LAMBDA(obj &optional strm);
CL_DECLARE();
CL_DOCSTRING("writeUglyObject");
CL_DEFUN T_sp core__write_ugly_object(T_sp obj, T_sp ostrm) {
  T_sp strm = coerce::outputStreamDesignator(ostrm);
  return write_ugly_object(obj, strm);
};

};
