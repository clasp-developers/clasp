/*
    File: cffi.cc
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
#include <dlfcn.h>
#include <boost/type_traits.hpp>
#include <stdint.h>
#include <clasp/core/common.h>
#include <clasp/core/character.h>
#include <clasp/core/str.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/environment.h>
#include <clasp/cffi/cffiPackage.h>
#include <clasp/cffi/cffi.h>
#include <clasp/core/wrappers.h>

namespace cffi {

// ----------------------------------------------------------------------
//

SYMBOL_SC_(CffiPkg, char);
SYMBOL_SC_(CffiPkg, unsigned_char);
SYMBOL_SC_(CffiPkg, short);
SYMBOL_SC_(CffiPkg, unsigned_short);
SYMBOL_SC_(CffiPkg, int);
SYMBOL_SC_(CffiPkg, unsigned_int);
SYMBOL_SC_(CffiPkg, long);
SYMBOL_SC_(CffiPkg, unsigned_long);
SYMBOL_SC_(CffiPkg, long_long);
SYMBOL_SC_(CffiPkg, unsigned_long_long);
SYMBOL_SC_(CffiPkg, int8);
SYMBOL_SC_(CffiPkg, uint8);
SYMBOL_SC_(CffiPkg, int16);
SYMBOL_SC_(CffiPkg, uint16);
SYMBOL_SC_(CffiPkg, int32);
SYMBOL_SC_(CffiPkg, uint32);
SYMBOL_SC_(CffiPkg, int64);
SYMBOL_SC_(CffiPkg, uint64);
SYMBOL_SC_(CffiPkg, size);
SYMBOL_SC_(CffiPkg, ssize);
SYMBOL_SC_(CffiPkg, ptrdiff);
SYMBOL_SC_(CffiPkg, time);
SYMBOL_SC_(CffiPkg, float);
SYMBOL_SC_(CffiPkg, double);
SYMBOL_SC_(CffiPkg, pointer);
SYMBOL_SC_(CffiPkg, void);

/*! These don't need garbage collections */
static vector<void *> _library_handles;

#define ARGS_af_PERCENTload_foreign_library "(name)"
#define DECL_af_PERCENTload_foreign_library ""
#define DOCS_af_PERCENTload_foreign_library "PERCENTload_foreign_library"
Pointer_sp af_PERCENTload_foreign_library(core::Str_sp name) {
  _G();
  const char *cname = name->get().c_str();
  void *handle = dlopen(cname, RTLD_LAZY);
  _library_handles.push_back(handle);
  return Pointer_O::create(handle);
};

#define ARGS_af_foreign_symbol_pointer "(name)"
#define DECL_af_foreign_symbol_pointer ""
#define DOCS_af_foreign_symbol_pointer "foreign_symbol_pointer"
Pointer_sp af_foreign_symbol_pointer(core::Str_sp name) {
  _G();
  const char *cname = name->get().c_str();
  void *ptr = NULL;
  for (vector<void *>::iterator it = _library_handles.begin(); it != _library_handles.end(); it++) {
    ptr = dlsym(*it, cname);
    if (ptr != NULL)
      goto GOT_IT;
  }
  ptr = NULL;
GOT_IT:
  return Pointer_O::create(ptr);
};

#define ARGS_af_PERCENTforeign_type_alignment "(atype)"
#define DECL_af_PERCENTforeign_type_alignment ""
#define DOCS_af_PERCENTforeign_type_alignment "PERCENTforeign_type_alignment"
core::Fixnum_sp af_PERCENTforeign_type_alignment(core::Symbol_sp atype) {
  _G();
  uint align;
  if (atype == _sym_char) {
    align = boost::alignment_of<char>();
  } else if (atype == _sym_unsigned_char) {
    align = boost::alignment_of<unsigned char>();
  } else if (atype == _sym_short) {
    align = boost::alignment_of<short>();
  } else if (atype == _sym_unsigned_short) {
    align = boost::alignment_of<unsigned short>();
  } else if (atype == _sym_int) {
    align = boost::alignment_of<int>();
  } else if (atype == _sym_unsigned_int) {
    align = boost::alignment_of<unsigned int>();
  } else if (atype == _sym_long) {
    align = boost::alignment_of<long>();
  } else if (atype == _sym_unsigned_long) {
    align = boost::alignment_of<unsigned long>();
  } else if (atype == _sym_long_long) {
    align = boost::alignment_of<long long>();
  } else if (atype == _sym_unsigned_long_long) {
    align = boost::alignment_of<unsigned long long>();
  } else if (atype == _sym_int8) {
    align = boost::alignment_of<int8_t>();
  } else if (atype == _sym_uint8) {
    align = boost::alignment_of<uint8_t>();
  } else if (atype == _sym_int16) {
    align = boost::alignment_of<int16_t>();
  } else if (atype == _sym_uint16) {
    align = boost::alignment_of<uint16_t>();
  } else if (atype == _sym_int32) {
    align = boost::alignment_of<int32_t>();
  } else if (atype == _sym_uint32) {
    align = boost::alignment_of<uint32_t>();
  } else if (atype == _sym_int64) {
    align = boost::alignment_of<int64_t>();
  } else if (atype == _sym_uint64) {
    align = boost::alignment_of<uint64_t>();
  } else if (atype == _sym_size) {
    align = boost::alignment_of<size_t>();
  } else if (atype == _sym_ssize) {
    align = boost::alignment_of<ssize_t>();
  } else if (atype == _sym_ptrdiff) {
    align = boost::alignment_of<ptrdiff_t>();
  } else if (atype == _sym_time) {
    align = boost::alignment_of<time_t>();
  } else if (atype == _sym_float) {
    align = boost::alignment_of<float>();
  } else if (atype == _sym_double) {
    align = boost::alignment_of<double>();
  } else if (atype == _sym_pointer) {
    align = boost::alignment_of<void *>();
  } else if (atype == _sym_void) {
    align = boost::alignment_of<void>();
  } else {
    SIMPLE_ERROR(BF("Cannot determine alignment of %s") % _rep_(atype));
  }
  return core::make_fixnum(align);
};

#define ARGS_af_PERCENTforeign_type_size "(atype)"
#define DECL_af_PERCENTforeign_type_size ""
#define DOCS_af_PERCENTforeign_type_size "PERCENTforeign_type_size"
core::Fixnum_sp af_PERCENTforeign_type_size(core::Symbol_sp atype) {
  _G();
  uint align;
  if (atype == _sym_char) {
    align = sizeof(char);
  } else if (atype == _sym_unsigned_char) {
    align = sizeof(unsigned char);
  } else if (atype == _sym_short) {
    align = sizeof(short);
  } else if (atype == _sym_unsigned_short) {
    align = sizeof(unsigned short);
  } else if (atype == _sym_int) {
    align = sizeof(int);
  } else if (atype == _sym_unsigned_int) {
    align = sizeof(unsigned int);
  } else if (atype == _sym_long) {
    align = sizeof(long);
  } else if (atype == _sym_unsigned_long) {
    align = sizeof(unsigned long);
  } else if (atype == _sym_long_long) {
    align = sizeof(long long);
  } else if (atype == _sym_unsigned_long_long) {
    align = sizeof(unsigned long long);
  } else if (atype == _sym_int8) {
    align = sizeof(int8_t);
  } else if (atype == _sym_uint8) {
    align = sizeof(uint8_t);
  } else if (atype == _sym_int16) {
    align = sizeof(int16_t);
  } else if (atype == _sym_uint16) {
    align = sizeof(uint16_t);
  } else if (atype == _sym_int32) {
    align = sizeof(int32_t);
  } else if (atype == _sym_uint32) {
    align = sizeof(uint32_t);
  } else if (atype == _sym_int64) {
    align = sizeof(int64_t);
  } else if (atype == _sym_uint64) {
    align = sizeof(uint64_t);
  } else if (atype == _sym_size) {
    align = sizeof(size_t);
  } else if (atype == _sym_ssize) {
    align = sizeof(ssize_t);
  } else if (atype == _sym_ptrdiff) {
    align = sizeof(ptrdiff_t);
  } else if (atype == _sym_time) {
    align = sizeof(time_t);
  } else if (atype == _sym_float) {
    align = sizeof(float);
  } else if (atype == _sym_double) {
    align = sizeof(double);
  } else if (atype == _sym_pointer) {
    align = sizeof(void *);
  } else if (atype == _sym_void) {
    IMPLEMENT_MEF(BF("Implement sizeof(void)"));
    //	align = sizeof(void);
  } else {
    SIMPLE_ERROR(BF("Cannot determine size of %s") % _rep_(atype));
  }
  return core::make_fixnum(align);
};

#define ARGS_af_foreign_alloc "(size)"
#define DECL_af_foreign_alloc ""
#define DOCS_af_foreign_alloc "foreign_alloc"
Pointer_sp af_foreign_alloc(core::Integer_sp size) {
  _G();
  int sz = clasp_to_int(size);
  void *ptr = malloc(sz);
  if (ptr == NULL) {
    SIMPLE_ERROR(BF("Could not allocate %ld bytes") % sz);
  }
  Pointer_sp op = Pointer_O::create(ptr);
  return op;
};

Pointer_sp Pointer_O::create(void *ptr) {
  _G();
  GC_ALLOCATE(Pointer_O, p);
  p->_ptr = ptr;
  return p;
}

#define ARGS_Pointer_O_make "(arg)"
#define DECL_Pointer_O_make ""
#define DOCS_Pointer_O_make "make"
Pointer_sp Pointer_O::make(core::Number_sp arg) {
  _G();
  if (af_fixnumP(arg)) {
    if (sizeof(unbox_fixnum(gc::As<core::Fixnum_sp>(arg))) != sizeof(void *)) {
      SIMPLE_ERROR(BF("You cannot make a pointer using an integer as the address sizeof(void*)=%d sizeof(Fixnum)=%d") % sizeof(void *) % sizeof(unbox_fixnum(gc::As<core::Fixnum_sp>(arg))));
    }
    IMPLEMENT_MEF(BF("Deal with converting Fixnum or Bignum to void*"));
#if 0
	return Pointer_O::create((void*)(arg.as<core::Fixnum_O>()->get()));
#endif
  }
  SIMPLE_ERROR(BF("Illegal type for address of pointer"));
};

#define ARGS_Pointer_O_null_pointer "(arg)"
#define DECL_Pointer_O_null_pointer ""
#define DOCS_Pointer_O_null_pointer "null_pointer"
Pointer_sp Pointer_O::null_pointer() {
  _G();
  GC_ALLOCATE(Pointer_O, res);
  res->_ptr = NULL;
  return res;
}

#define ARGS_Pointer_O_PERCENTmem_ref "((self pointer) atype &optional ( offset 0) )"
#define DECL_Pointer_O_PERCENTmem_ref ""
#define DOCS_Pointer_O_PERCENTmem_ref "PERCENTmem_ref"
core::T_sp Pointer_O::PERCENTmem_ref(core::Symbol_sp atype, core::Integer_sp offset) {
  _G();
  void *ptr = ((char *)(this->_ptr) + clasp_to_int(offset));
  if (atype == _sym_char) {
    return core::clasp_make_character(*(char *)(ptr));
  } else if (atype == _sym_unsigned_char) {
    return core::make_fixnum(*(unsigned char *)(ptr));
  } else if (atype == _sym_short) {
    return core::make_fixnum(*(short *)(ptr));
  } else if (atype == _sym_unsigned_short) {
    return core::make_fixnum(*(unsigned short *)(ptr));
  } else if (atype == _sym_int) {
    return core::make_fixnum(*(int *)(ptr));
#if 0
    } else if ( atype == _sym_long)
    {
	align = sizeof(long);
    } else if ( atype == _sym_unsigned_long)
    {
	align = sizeof(unsigned long);
    } else if ( atype == _sym_long_long)
    {
	align = sizeof(long long);
    } else if ( atype == _sym_unsigned_long_long)
    {
	align = sizeof(unsigned long long);
    } else if ( atype == _sym_int8)
    {
	align = sizeof(int8_t);
    } else if ( atype == _sym_uint8)
    {
	align = sizeof(uint8_t);
    } else if ( atype == _sym_int16)
    {
	align = sizeof(int16_t);
    } else if ( atype == _sym_uint16)
    {
	align = sizeof(uint16_t);
    } else if ( atype == _sym_int32)
    {
	align = sizeof(int32_t);
    } else if ( atype == _sym_uint32)
    {
	align = sizeof(uint32_t);
    } else if ( atype == _sym_int64)
    {
	align = sizeof(int64_t);
    } else if ( atype == _sym_uint64)
    {
	align = sizeof(uint64_t);
    } else if ( atype == _sym_size)
    {
	align = sizeof(size_t);
    } else if ( atype == _sym_ssize)
    {
	align = sizeof(ssize_t);
    } else if ( atype == _sym_ptrdiff)
    {
	align = sizeof(ptrdiff_t);
    } else if ( atype == _sym_time)
    {
	align = sizeof(time_t);
    } else if ( atype == _sym_float)
    {
	align = sizeof(float);
    } else if ( atype == _sym_double)
    {
	align = sizeof(double);
    } else if ( atype == _sym_pointer)
    {
	align = sizeof(void*);
    } else if ( atype == _sym_void)
    {
	align = sizeof(void);
#endif
  } else {
    SIMPLE_ERROR(BF("Add support for %mem_ref %s") % _rep_(atype));
  }
}

#define ARGS_Pointer_O_PERCENTsetf_mem_ref "((self pointer) atype &rest rest)"
#define DECL_Pointer_O_PERCENTsetf_mem_ref ""
#define DOCS_Pointer_O_PERCENTsetf_mem_ref "PERCENTsetf_mem_ref"
core::T_sp Pointer_O::PERCENTsetf_mem_ref(core::Symbol_sp atype, core::Cons_sp rest) {
  _G();
  core::LongLongInt offset = 0;
  core::T_sp value;
  if (rest->length() == 1) {
    value = oCar(rest);
  } else if (rest->length() == 2) {
    offset = clasp_to_int(gc::As<core::Integer_sp>(oCar(rest)));
    value = oCadr(rest);
  }
  void *ptr = ((char *)(this->_ptr) + offset);
  if (atype == _sym_char) {
    if (af_characterP(value)) {
      *(char *)(ptr) = clasp_as_char(gc::As<core::Character_sp>(value));
      return value;
    } else if (af_integerP(value)) {
      core::LongLongInt lli = clasp_to_int(gc::As<core::Integer_sp>(value));
      *(char *)(ptr) = lli;
      return value;
    }
#if 0
    } else if ( atype == _sym_unsigned_char)
    {
	return core::make_fixnum(*(unsigned char*)(ptr),_lisp);
    } else if ( atype == _sym_short)
    {
	return core::make_fixnum(*(short*)(ptr),_lisp);
    } else if ( atype == _sym_unsigned_short)
    {
	return core::make_fixnum(*(unsigned short*)(ptr),_lisp);
    } else if ( atype == _sym_int)
    {
	return core::make_fixnum(*(int*)(ptr),_lisp);
    } else if ( atype == _sym_unsigned_int)
    {
	return core::LongLongInt_O::create((core::LongLongInt)*(unsigned int*)(ptr),_lisp);
    } else if ( atype == _sym_long)
    {
	align = sizeof(long);
    } else if ( atype == _sym_unsigned_long)
    {
	align = sizeof(unsigned long);
    } else if ( atype == _sym_long_long)
    {
	align = sizeof(long long);
    } else if ( atype == _sym_unsigned_long_long)
    {
	align = sizeof(unsigned long long);
    } else if ( atype == _sym_int8)
    {
	align = sizeof(int8_t);
    } else if ( atype == _sym_uint8)
    {
	align = sizeof(uint8_t);
    } else if ( atype == _sym_int16)
    {
	align = sizeof(int16_t);
    } else if ( atype == _sym_uint16)
    {
	align = sizeof(uint16_t);
    } else if ( atype == _sym_int32)
    {
	align = sizeof(int32_t);
    } else if ( atype == _sym_uint32)
    {
	align = sizeof(uint32_t);
    } else if ( atype == _sym_int64)
    {
	align = sizeof(int64_t);
    } else if ( atype == _sym_uint64)
    {
	align = sizeof(uint64_t);
    } else if ( atype == _sym_size)
    {
	align = sizeof(size_t);
    } else if ( atype == _sym_ssize)
    {
	align = sizeof(ssize_t);
    } else if ( atype == _sym_ptrdiff)
    {
	align = sizeof(ptrdiff_t);
    } else if ( atype == _sym_time)
    {
	align = sizeof(time_t);
    } else if ( atype == _sym_float)
    {
	align = sizeof(float);
    } else if ( atype == _sym_double)
    {
	align = sizeof(double);
    } else if ( atype == _sym_pointer)
    {
	align = sizeof(void*);
    } else if ( atype == _sym_void)
    {
	align = sizeof(void);
#endif
  }
  SIMPLE_ERROR(BF("Add support for %setf_mem_ref %s") % _rep_(atype));
}

Pointer_O::Pointer_O() : Base(), _ptr(NULL){};
Pointer_O::~Pointer_O(){};

#define ARGS_Pointer_O_foreign_free "()"
#define DECL_Pointer_O_foreign_free ""
#define DOCS_Pointer_O_foreign_free "foreign_free"
void Pointer_O::foreign_free() {
  _G();
  if (this->_ptr != NULL) {
    free(this->_ptr);
    this->_ptr = NULL;
  }
};

Pointer_sp Pointer_O::inc_pointer(core::Integer_sp offset) {
  _G();
  void *new_ptr = (void *)((char *)(this->_ptr) + clasp_to_int(offset));
  return Pointer_O::create(new_ptr);
}

string Pointer_O::__repr__() const {
  _G();
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString() << " ";
  ss << this->_ptr << "> ";
  return ss.str();
}

EXPOSE_CLASS(cffi, Pointer_O);

void Pointer_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<Pointer_O>()
      .def("CFFI-SYS:foreign_free", &Pointer_O::foreign_free)
      .def("CFFI-SYS:PERCENTmem_ref", &Pointer_O::PERCENTmem_ref,
           ARGS_Pointer_O_PERCENTmem_ref,
           DECL_Pointer_O_PERCENTmem_ref,
           DOCS_Pointer_O_PERCENTmem_ref)
      .def("CFFI-SYS:PERCENTsetf_mem_ref", &Pointer_O::PERCENTsetf_mem_ref,
           ARGS_Pointer_O_PERCENTsetf_mem_ref,
           DECL_Pointer_O_PERCENTsetf_mem_ref,
           DOCS_Pointer_O_PERCENTsetf_mem_ref)
      .def("CFFI-SYS:inc-pointer", &Pointer_O::inc_pointer);
  core::af_def(CffiPkg, "make-pointer", &Pointer_O::make, ARGS_Pointer_O_make, DECL_Pointer_O_make, DOCS_Pointer_O_make);
  core::af_def(CffiPkg, "null-pointer", &Pointer_O::null_pointer, ARGS_Pointer_O_null_pointer, DECL_Pointer_O_null_pointer, DOCS_Pointer_O_null_pointer);

  SYMBOL_SC_(CffiPkg, PERCENTmem_ref);
  SYMBOL_SC_(CffiPkg, PERCENTsetf_mem_ref);
  core::add_defsetf_access_update(_sym_PERCENTmem_ref, _sym_PERCENTsetf_mem_ref);
}

void Pointer_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CffiPkg, Pointer, "", "", _lisp);
#endif
}

void initialize_cffi() {
  _G();
  SYMBOL_EXPORT_SC_(CffiPkg, PERCENTforeign_type_alignment);
  Defun(PERCENTforeign_type_alignment);
  SYMBOL_EXPORT_SC_(CffiPkg, PERCENTforeign_type_size);
  Defun(PERCENTforeign_type_size);
  SYMBOL_EXPORT_SC_(CffiPkg, foreign_alloc);
  Defun(foreign_alloc);
  SYMBOL_EXPORT_SC_(CffiPkg, foreign_symbol_pointer);
  Defun(foreign_symbol_pointer);
  SYMBOL_EXPORT_SC_(CffiPkg, PERCENTload_foreign_library);
  Defun(PERCENTload_foreign_library);
}

}; // cffi
