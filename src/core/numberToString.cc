/*
    File: numberToString.cc
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
    Copyright (c) 2010, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <float.h>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/array.h>
#include <clasp/core/numbers.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/bignum.h>
#include <clasp/core/numberToString.h>
#include <clasp/core/wrappers.h>

namespace core {

CL_LAMBDA(buffer x base);
CL_DECLARE();
CL_DOCSTRING("bignumToString");
CL_DEFUN StrNs_sp core__bignum_to_string(StrNs_sp buffer, const Bignum &bn, Fixnum_sp base) {
  if (unbox_fixnum(base) < 2 || unbox_fixnum(base) > 36) {
    QERROR_WRONG_TYPE_NTH_ARG(3, base, Cons_O::createList(cl::_sym_integer, make_fixnum(2), make_fixnum(36)));
  }
  int ibase = unbox_fixnum(base);
  size_t str_size = mpz_sizeinbase(bn.get_mpz_t(), ibase);
  if (bn < 0) str_size++;
  if (Str8Ns_sp buffer8 = buffer.asOrNull<Str8Ns_O>() ) {
    buffer->ensureSpaceAfterFillPointer(clasp_make_character('\0'),str_size + 2);
    char *bufferStart = (char*)&(*buffer8)[buffer8->fillPointer()];
    mpz_get_str(bufferStart, -unbox_fixnum(base), bn.get_mpz_t());
    if (bufferStart[str_size - 1] == '\0') {
      buffer->fillPointerSet(buffer->fillPointer()+str_size-1);
    } else {
      buffer->fillPointerSet(buffer->fillPointer()+str_size);
    }
  } else if (StrWNs_sp bufferw = buffer.asOrNull<StrWNs_O>()) {
    buffer->ensureSpaceAfterFillPointer(clasp_make_character(' '),str_size+1);
    char cpbuffer[str_size+1]; // use a stack allocated array for this
    mpz_get_str(cpbuffer, -unbox_fixnum(base), bn.get_mpz_t());
    for ( size_t idx(0);idx<str_size; ++idx) {
      bufferw->vectorPushExtend(cpbuffer[idx]);
    }
  } else {
    SIMPLE_ERROR(BF("The buffer for the bignum must be a string with a fill-pointer"));
  }
  return buffer;
}

static void write_base_prefix(StrNs_sp buffer, int base) {
  if (base == 2) {
    StringPushStringCharStar(buffer,"#b");
  } else if (base == 8) {
    StringPushStringCharStar(buffer,"#o");
  } else if (base == 16) {
    StringPushStringCharStar(buffer,"#x");
  } else if (base >= 10) {
    string prefix = "#00r";
    prefix[1] = base / 10 + '0';
    prefix[2] = base % 10 + '0';
    StringPushStringCharStar(buffer,prefix.c_str());
  } else {
    string prefix = "#0r";
    prefix[1] = base + '0';
    StringPushStringCharStar(buffer,prefix.c_str());
  }
}

CL_LAMBDA(buffer integer base radix decimalp);
CL_DECLARE();
CL_DOCSTRING("integerToString");
CL_DEFUN StrNs_sp core__integer_to_string(StrNs_sp buffer, Integer_sp integer,
                                           Fixnum_sp base, bool radix, bool decimalp) {
  if (radix) {
    if (!decimalp || unbox_fixnum(base) != 10) {
      buffer->ensureSpaceAfterFillPointer(clasp_make_character('\0'),10);
      write_base_prefix(buffer, unbox_fixnum(base));
    }
    buffer = core__integer_to_string(buffer, integer, base, false, false);
    if (decimalp && unbox_fixnum(base) == 10) {
      buffer->vectorPushExtend(clasp_make_character('.'));
    }
    return buffer;
  }
  if (integer.fixnump()) {
    char txt[64];
    gc::Fixnum fn = unbox_fixnum(gc::As<Fixnum_sp>(integer));
    if (fn < 0) {
      StringPushStringCharStar(buffer,"-");
      fn = - fn;
    }
    switch (unbox_fixnum(base)) {
    case 8:
      sprintf(txt, "%" PRFoctal, fn);
      StringPushStringCharStar(buffer,txt);
      break;
    case 10:
      sprintf(txt, "%" PRF, fn);
      StringPushStringCharStar(buffer,txt);
      break;
    case 16:
      sprintf(txt, "%" PRFhex, fn);
      StringPushStringCharStar(buffer,txt);
      break;
    default:
        Bignum bn(GMP_LONG(fn));
      core__bignum_to_string(buffer, bn, base);
      break;
    }
    return buffer;
  } else if (Bignum_sp bi = integer.asOrNull<Bignum_O>()) {
    core__bignum_to_string(buffer, bi->get(), base);
  } else {
    QERROR_WRONG_TYPE_NTH_ARG(2, base, cl::_sym_integer);
  }
  return buffer;
}

  SYMBOL_EXPORT_SC_(CorePkg, integerToString);

};
