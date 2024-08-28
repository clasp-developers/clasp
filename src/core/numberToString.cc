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

SYMBOL_EXPORT_SC_(CorePkg, next_to_string);

CL_LAMBDA(buffer x base);
CL_DECLARE();
DOCGROUP(clasp);
CL_DEFUN StrNs_sp core__next_to_string(StrNs_sp buffer, Bignum_sp bn, Fixnum_sp base) {
  const char* num_to_text = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  int ibase = unbox_fixnum(base);
  if (ibase < 2 || ibase > 36) {
    ERROR_WRONG_TYPE_NTH_ARG(core::_sym_next_to_string, 3, base, Cons_O::createList(cl::_sym_integer, make_fixnum(2), make_fixnum(36)));
  }
  mp_size_t len = bn->length();
  mp_size_t size = std::abs(len);
  const mp_limb_t* limbs = bn->limbs();
  size_t str_size = mpn_sizeinbase(limbs, size, ibase);
  size_t negative = (len < 0) ? 1 : 0;
  mp_limb_t copy_limbs[size]; // mpn_get_str may destroy its input
  // FIXME: memcpy copy something
  for (mp_size_t i = 0; i < size; ++i)
    copy_limbs[i] = limbs[i];

  if (Str8Ns_sp buffer8 = buffer.asOrNull<Str8Ns_O>()) {
    buffer8->ensureSpaceAfterFillPointer(clasp_make_character('\0'), str_size + negative + 2);
    unsigned char* bufferStart = (unsigned char*)&(*buffer8)[buffer8->fillPointer()];
    unsigned char* significandStart = bufferStart + negative;
    mp_size_t actual_str_size = mpn_get_str(significandStart, ibase, copy_limbs, size);
    if (negative == 1)
      bufferStart[0] = '-';
    for (size_t i = 0; i < actual_str_size; ++i)
      significandStart[i] = num_to_text[significandStart[i]];
    if (bufferStart[actual_str_size + negative - 1] == '\0') {
      buffer8->fillPointerSet(buffer8->fillPointer() + actual_str_size + negative - 1);
    } else {
      buffer8->fillPointerSet(buffer8->fillPointer() + actual_str_size + negative);
    }
  } else if (StrWNs_sp bufferw = buffer.asOrNull<StrWNs_O>()) {
    bufferw->ensureSpaceAfterFillPointer(clasp_make_character(' '), str_size + negative);
    unsigned char cpbuffer[str_size + 1]; // use a stack allocated array for this
    mp_size_t actual_str_size = mpn_get_str(cpbuffer, ibase, copy_limbs, size);
    if (negative == 1)
      bufferw->vectorPushExtend('-');
    for (size_t idx(0); idx < actual_str_size; ++idx)
      bufferw->vectorPushExtend(num_to_text[cpbuffer[idx]]);
  } else {
    SIMPLE_ERROR("The buffer for the bignum must be a string with a fill-pointer");
  }
  return buffer;
}

static void write_base_prefix(StrNs_sp buffer, int base) {
  if (base == 2) {
    StringPushStringCharStar(buffer, "#b");
  } else if (base == 8) {
    StringPushStringCharStar(buffer, "#o");
  } else if (base == 16) {
    StringPushStringCharStar(buffer, "#x");
  } else if (base >= 10) {
    string prefix = "#00r";
    prefix[1] = base / 10 + '0';
    prefix[2] = base % 10 + '0';
    StringPushStringCharStar(buffer, prefix.c_str());
  } else {
    string prefix = "#0r";
    prefix[1] = base + '0';
    StringPushStringCharStar(buffer, prefix.c_str());
  }
}

CL_LAMBDA(buffer integer base radix decimalp);
CL_DECLARE();
CL_DOCSTRING(R"dx(integerToString)dx");
DOCGROUP(clasp);
CL_DEFUN StrNs_sp core__integer_to_string(StrNs_sp buffer, Integer_sp integer, Fixnum_sp base, bool radix, bool decimalp) {
  if (radix) {
    if (!decimalp || unbox_fixnum(base) != 10) {
      buffer->ensureSpaceAfterFillPointer(clasp_make_character('\0'), 10);
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
      StringPushStringCharStar(buffer, "-");
      fn = -fn;
    }
    switch (unbox_fixnum(base)) {
    case 8:
      sprintf(txt, "%" PRFoctal, fn);
      StringPushStringCharStar(buffer, txt);
      break;
    case 10:
      sprintf(txt, "%" PRF, fn);
      StringPushStringCharStar(buffer, txt);
      break;
    case 16:
      sprintf(txt, "%" PRFhex, fn);
      StringPushStringCharStar(buffer, txt);
      break;
    default:
      if (fn == 0) {
        // Zero needs to be special cased, because
        // zero bignums would need special casing.
        // Specifically, mpn functions have undefined
        // behavior when passed them.
        StringPushStringCharStar(buffer, "0");
        return buffer;
      };
      Bignum_sp bn = core__next_from_fixnum(fn);
      core__next_to_string(buffer, bn, base);
      break;
    }
    return buffer;
  } else if (Bignum_sp bi = integer.asOrNull<Bignum_O>()) {
    core__next_to_string(buffer, bi, base);
  } else {
    ERROR_WRONG_TYPE_NTH_ARG(core::_sym_integerToString, 2, base, cl::_sym_integer);
  }
  return buffer;
}

SYMBOL_EXPORT_SC_(CorePkg, integerToString);

}; // namespace core
