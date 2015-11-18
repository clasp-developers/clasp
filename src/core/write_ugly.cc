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

#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/str.h>
#include <clasp/core/pathname.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/instance.h>
#include <clasp/core/strWithFillPtr.h>
#include <clasp/core/structureObject.h>
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
  T_sp namestring = clasp_namestring(path, 0);
  bool readably = clasp_print_readably();
  if (namestring.nilp()) {
    if (readably) {
      path->__writeReadable__(strm);
      return;
    }
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

#if 0
    void write_character(T_sp stream, Character_sp char) const
    {
        int i = char->charCode();
	if (!clasp_print_escape() && !clasp_print_readably()) {
	    clasp_write_char(i,stream);
	} else {
	    clasp_write_string("#\\",stream);
	    if (i < 32 || i >= 127) {
		Str_sp name = gc::As<Str_sp>(eval::funcall(cl::_sym_char_name,char));
		clasp_write_string(name->get(),stream);
	    } else {
		clasp_write_char(i,stream);
	    }
	}
    }
#endif

void Instance_O::__write__(T_sp stream) const {
  eval::funcall(cl::_sym_printObject, this->const_sharedThis<Instance_O>(), stream);
}

void StructureObject_O::__write__(T_sp stream) const {
  //  printf("%s:%d StructureObject_O::__write__\n", __FILE__, __LINE__);
  if (UNLIKELY(!gc::IsA<Symbol_sp>(this->_Type)))
    SIMPLE_ERROR(BF("Found a corrupt structure with an invalid type name~%  ~S") % _rep_(this->_Type));
  SYMBOL_EXPORT_SC_(CorePkg, structure_print_function);
  SYMBOL_EXPORT_SC_(CorePkg, STARprint_structureSTAR);
  T_sp print_function = af_get_sysprop(this->_Type, _sym_structure_print_function);
  if (print_function.nilp() || !_sym_STARprint_structureSTAR->symbolValue().isTrue()) {
    clasp_write_string("#S", stream);
    /* structure_to_list conses slot names and values into
	     * a list to be printed.  print shouldn't allocate
	     * memory - Beppe
	     */
    T_sp x = this->structureAsList();
    write_object(x, stream);
  } else {
    eval::funcall(print_function, this->asSmartPtr(), stream, make_fixnum(0));
  }
}

void Integer_O::__write__(T_sp stream) const {
  StrWithFillPtr_sp buffer = StrWithFillPtr_O::createBufferString(128);
  int print_base = clasp_print_base();
  core_integerToString(buffer, this->const_sharedThis<Integer_O>(),
                       make_fixnum(print_base),
                       cl::_sym_STARprint_radixSTAR->symbolValue().isTrue(),
                       true);
  cl_write_sequence(buffer, stream, make_fixnum(0), _Nil<T_O>());
}

#if 0 // working

    void
    _ecl_write_fixnum(gctools::Fixnum i, T_sp stream)
    {
        T_sp s = si_get_buffer_string();
        si_integer_to_string(s, ecl_make_fixnum(i), ecl_make_fixnum(10), ECL_NIL, ECL_NIL);
        si_do_write_sequence(s, stream, ecl_make_fixnum(0), ECL_NIL);
        si_put_buffer_string(s);
    }



    static void
    write_ratio(T_sp r, T_sp stream)
    {
        T_sp s = si_get_buffer_string();
        int print_base = ecl_print_base();
        si_integer_to_string(s, r->ratio.num, ecl_make_fixnum(print_base),
                             ecl_symbol_value(@'*print-radix*'),
                             ECL_NIL /* decimal syntax */);
        ecl_string_push_extend(s, '/');
        si_integer_to_string(s, r->ratio.den,
                             ecl_make_fixnum(print_base),
                             ECL_NIL, ECL_NIL);
        si_do_write_sequence(s, stream, ecl_make_fixnum(0), ECL_NIL);
        si_put_buffer_string(s);
    }

    static void
    write_complex(T_sp x, T_sp stream)
    {
        writestr_stream("#C(", stream);
        si_write_ugly_object(x->complex.real, stream);
        ecl_write_char(' ', stream);
        si_write_ugly_object(x->complex.imag, stream);
        ecl_write_char(')', stream);
    }


    static void
    write_character(T_sp x, T_sp stream)
    {
        int i = ECL_CHAR_CODE(x);
	if (!ecl_print_escape() && !ecl_print_readably()) {
	    ecl_write_char(i, stream);
	} else {
	    writestr_stream("#\\", stream);
	    if (i < 32 || i >= 127) {
		T_sp name = cl_char_name(ECL_CODE_CHAR(i));
		writestr_stream((char*)name->base_string.self, stream);
	    } else {
		ecl_write_char(i, stream);
	    }
	}
    }

    static void
    write_package(T_sp x, T_sp stream)
    {
        if (ecl_print_readably()) FEprint_not_readable(x);
        writestr_stream("#<", stream);
        si_write_ugly_object(x->pack.name, stream);
        writestr_stream(" package>", stream);
    }

    static void
    write_hashtable(T_sp x, T_sp stream)
    {
	if (ecl_print_readably() && !Null(ecl_symbol_value(@'*read-eval*'))) {
	    T_sp make =
		cl_list(9, @'make-hash-table',
			@':size', cl_hash_table_size(x),
			@':rehash-size', cl_hash_table_rehash_size(x),
			@':rehash-threshold', cl_hash_table_rehash_threshold(x),
			@':test', cl_hash_table_test(x));
	    T_sp init =
		Cons_O::createList( @'ext::hash-table-fill', make,
			Cons_O::createList( @'quote', si_hash_table_content(x)));
	    writestr_stream("#.", stream);
	    si_write_ugly_object(init, stream);
	} else {
	    _ecl_write_unreadable(x, "hash-table", ECL_NIL, stream);
	}
    }

    static void
    write_random(T_sp x, T_sp stream)
    {
        if (ecl_print_readably()) {
	    writestr_stream("#$", stream);
	    _ecl_write_vector(x->random.value, stream);
        } else {
	    _ecl_write_unreadable(x->random.value, "random-state", ECL_NIL, stream);
        }
    }

    static void
    write_stream(T_sp x, T_sp stream)
    {
        const char *prefix;
        T_sp tag;
        union cl_lispunion str;
#ifdef ECL_UNICODE
        ecl_character buffer[10];
#else
        ecl_base_char buffer[10];
#endif
        switch ((enum ecl_smmode)x->stream.mode) {
        case ecl_smm_input_file:
	    prefix = "closed input file";
	    tag = IO_STREAM_FILENAME(x);
	    break;
        case ecl_smm_input:
	    prefix = "closed input stream";
	    tag = IO_STREAM_FILENAME(x);
	    break;
        case ecl_smm_output_file:
	    prefix = "closed output file";
	    tag = IO_STREAM_FILENAME(x);
	    break;
        case ecl_smm_output:
	    prefix = "closed output stream";
	    tag = IO_STREAM_FILENAME(x);
	    break;
#ifdef ECL_MS_WINDOWS_HOST
        case ecl_smm_input_wsock:
	    prefix = "closed input win32 socket stream";
	    tag = IO_STREAM_FILENAME(x);
	    break;
        case ecl_smm_output_wsock:
	    prefix = "closed output win32 socket stream";
	    tag = IO_STREAM_FILENAME(x);
	    break;
        case ecl_smm_io_wsock:
	    prefix = "closed i/o win32 socket stream";
	    tag = IO_STREAM_FILENAME(x);
	    break;
        case ecl_smm_io_wcon:
	    prefix = "closed i/o win32 console stream";
	    tag = IO_STREAM_FILENAME(x);
	    break;
#endif
        case ecl_smm_io_file:
	    prefix = "closed io file";
	    tag = IO_STREAM_FILENAME(x);
	    break;
        case ecl_smm_io:
	    prefix = "closed io stream";
	    tag = IO_STREAM_FILENAME(x);
	    break;
        case ecl_smm_probe:
	    prefix = "closed probe stream";
	    tag = IO_STREAM_FILENAME(x);
	    break;
        case ecl_smm_synonym:
	    prefix = "closed synonym stream to";
	    tag = SYNONYM_STREAM_SYMBOL(x);
	    break;
        case ecl_smm_broadcast:
	    prefix = "closed broadcast stream";
	    tag = ECL_NIL;
	    break;
        case ecl_smm_concatenated:
	    prefix = "closed concatenated stream";
	    tag = ECL_NIL;
	    break;
        case ecl_smm_two_way:
	    prefix = "closed two-way stream";
	    tag = ECL_NIL;
	    break;
        case ecl_smm_echo:
	    prefix = "closed echo stream";
	    tag = ECL_NIL;
	    break;
        case ecl_smm_string_input: {
	    T_sp text = x->stream.object0;
	    cl_index ndx, l = ecl_length(text);
	    for (ndx = 0; (ndx < 8) && (ndx < l); ndx++) {
		buffer[ndx] = ecl_char(text, ndx);
	    }
	    if (l > ndx) {
		buffer[ndx-1] = '.';
		buffer[ndx-2] = '.';
		buffer[ndx-3] = '.';
	    }
	    buffer[ndx++] = 0;
	    prefix = "closed string-input stream from";
	    tag = &str;
#ifdef ECL_UNICODE
	    tag->string.t = t_string;
	    tag->string.self = buffer;
#else
	    tag->base_string.t = t_base_string;
	    tag->base_string.self = buffer;
#endif
	    tag->base_string.dim = ndx;
	    tag->base_string.fillp = ndx-1;
	    break;
        }
        case ecl_smm_string_output:
	    prefix = "closed string-output stream";
	    tag = ECL_NIL;
	    break;
        case ecl_smm_sequence_input:
	    prefix = "closed sequence-input stream";
	    tag = ECL_NIL;
	    break;
        case ecl_smm_sequence_output:
	    prefix = "closed sequence-output stream";
	    tag = ECL_NIL;
	    break;
        default:
	    ecl_internal_error("illegal stream mode");
        }
        if (!x->stream.closed)
	    prefix = prefix + 7;
        _ecl_write_unreadable(x, prefix, tag, stream);
    }

#ifdef CLOS
    static void
    write_instance(T_sp x, T_sp stream)
    {
        _ecl_funcall3(@'print-object', x, stream);
    }
#endif
    static void
    write_structure(T_sp x, T_sp stream)
    {
        T_sp print_function;
        unlikely_if (ecl_t_of(x->str.name) != t_symbol)
	    FEerror("Found a corrupt structure with an invalid type name~%"
		    "  ~S", x->str.name);
        print_function = si_get_sysprop(x->str.name, @'si::structure-print-function');
        if (Null(print_function) || !ecl_print_structure()) {
	    writestr_stream("#S", stream);
	    /* structure_to_list conses slot names and values into
	     * a list to be printed.  print shouldn't allocate
	     * memory - Beppe
	     */
	    x = structure_to_list(x);
	    si_write_object(x, stream);
        } else {
	    _ecl_funcall4(print_function, x, stream, ecl_make_fixnum(0));
        }
    }
//#endif /* !CLOS */

#endif // working

void
_clasp_write_fixnum(gctools::Fixnum i, T_sp stream) {
  StrWithFillPtr_sp buffer = StrWithFillPtr_O::createBufferString(128);
  core_integerToString(buffer,
                       clasp_make_fixnum(i), clasp_make_fixnum(clasp_print_base()), cl::_sym_STARprint_radixSTAR->symbolValue().isTrue(), true);
  cl_write_sequence(buffer, stream, make_fixnum(0), _Nil<T_O>());
}

void write_fixnum(T_sp strm, T_sp i) {
  Fixnum_sp fn = gc::As<Fixnum_sp>(i);
  StrWithFillPtr_sp buffer = StrWithFillPtr_O::createBufferString(128);
  int print_base = clasp_print_base();
  core_integerToString(buffer, fn,
                       make_fixnum(print_base),
                       cl::_sym_STARprint_radixSTAR->symbolValue().isTrue(),
                       true);
  cl_write_sequence(buffer, strm, make_fixnum(0), _Nil<T_O>());
}

void write_single_float(T_sp strm, SingleFloat_sp i) {
  stringstream ss;
  ss << unbox_single_float(i);
  clasp_write_string(ss.str(), strm);
}

void
write_float(T_sp f, T_sp stream) {
  StrWithFillPtr_sp s = _lisp->get_buffer_string();
  s = core_float_to_string_free(s, f, clasp_make_fixnum(-3), clasp_make_fixnum(8));
  cl_write_sequence(s, stream, clasp_make_fixnum(0), _Nil<T_O>());
  _lisp->put_buffer_string(s);
}

void write_character(T_sp strm, T_sp chr) {
  ASSERT(chr.characterp());
  claspChar i = chr.unsafe_character();
  if (!clasp_print_escape() && !clasp_print_readably()) {
    clasp_write_char(i, strm);
  } else {
    clasp_write_string("#\\", strm);
    if (i < 32 || i >= 127) {
      Str_sp name = cl_char_name(clasp_make_character(i));
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
  } else if (x.generalp() || x.consp()) {
    if (Float_sp fx = x.asOrNull<Float_O>()) {
      write_float(fx, stream);
    } else {
      x->__write__(stream);
    }
  } else if (x.valistp()) {
    clasp_write_string("#<VA-LIST>", stream);
  } else {
    SIMPLE_ERROR(BF("Could not write object with tag: %ul") % x.tag());
  }
  return x;
}

#define ARGS_af_writeUglyObject "(obj &optional strm)"
#define DECL_af_writeUglyObject ""
#define DOCS_af_writeUglyObject "writeUglyObject"
T_sp af_writeUglyObject(T_sp obj, T_sp ostrm) {
  _G();
  T_sp strm = coerce::outputStreamDesignator(ostrm);
  return write_ugly_object(obj, strm);
};

void initialize_write_ugly_object() {
  Defun(writeUglyObject);
}
};
