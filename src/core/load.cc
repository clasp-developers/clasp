/*
    File: load.cc
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
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/load.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/arguments.h>
#include <clasp/core/array.h>
#include <clasp/core/bformat.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/primitives.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/pathname.h>
#include <clasp/core/lispReader.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/compiler.h>
#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/core/predicates.h>
#include <clasp/core/wrappers.h>

namespace core {

SYMBOL_SC_(CorePkg, eof);

T_sp load_stream(T_sp strm, bool print) {
  while (true) {
    // Required to get source position correct. FIXME
    cl__peek_char(_lisp->_true(), strm, nil<T_O>(), nil<T_O>(), nil<T_O>());
    DynamicScopeManager scope(_sym_STARcurrentSourcePosInfoSTAR, clasp_simple_input_stream_source_pos_info(strm));
    bool echoReplRead = _sym_STARechoReplReadSTAR->symbolValue().isTrue();
    T_sp x = cl__read(strm, nil<T_O>(), _sym_eof, nil<T_O>());
    if (x == _sym_eof)
      break;
    if (echoReplRead) {
      clasp_write_string(fmt::format("Read: {}\n", _rep_(x)));
    }
    if (x.number_of_values() > 0) {
      if (print)
        clasp_write_string(fmt::format(";; -read- {}\n", _rep_(x)));
      eval::funcall(core::_sym_STAReval_with_env_hookSTAR->symbolValue(), x, nil<T_O>());
    }
  }
  cl__close(strm);
  return _lisp->_true();
}

CL_LAMBDA(source &optional verbose print external-format skip-shebang);
CL_DECLARE();
CL_DOCSTRING(R"dx(loadSource)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__load_source(T_sp source, bool verbose, bool print, core::T_sp externalFormat, bool skipShebang) {
  T_sp strm;
  if (source.nilp()) {
    SIMPLE_ERROR("{} was called with NIL as the source filename", __FUNCTION__);
  }
  T_sp final_format;
  if (externalFormat.nilp())
    final_format = kw::_sym_default;
  else
    final_format = externalFormat;
  strm = cl__open(source, kw::_sym_input, cl::_sym_character, nil<T_O>(), false, nil<T_O>(), false, final_format, nil<T_O>());
  if (strm.nilp())
    return nil<T_O>();

  if (source.nilp())
    SIMPLE_ERROR("{} was about to pass nil to pathname", __FUNCTION__);
  Pathname_sp pathname = cl__pathname(source);
  ASSERTF(pathname.objectp(), "Problem getting pathname of [{}] in loadSource", _rep_(source));
  Pathname_sp truename = cl__truename(source);
  ASSERTF(truename.objectp(), "Problem getting truename of [{}] in loadSource", _rep_(source));
  DynamicScopeManager scope(cl::_sym_STARloadPathnameSTAR, pathname);
  DynamicScopeManager scope2(cl::_sym_STARloadTruenameSTAR, truename);

  if (skipShebang) {
    if (clasp_peek_char(strm) == '#') {
      clasp_read_char(strm);
      if (clasp_peek_char(strm) == '!') {
        cl__read_line(strm);
      } else {
        clasp_unread_char('#', strm);
      }
    }
  }

  return load_stream(strm, print);
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__load_no_package_set(T_sp lsource, T_sp verbose, T_sp print, T_sp if_does_not_exist, T_sp external_format,
                                        T_sp search_list) {
  Pathname_sp pathname;
  T_sp pntype;
  T_sp hooks;
  T_sp filename;
  T_sp function = nil<T_O>();
  T_sp ok;
  T_sp msource = lsource;
  //        printf("%s:%d cl__load source= %s\n", __FILE__, __LINE__, _rep_(source).c_str());
  if (verbose.notnilp()) {
    clasp_write_string(fmt::format(";;; Loading {}\n", _rep_(lsource)));
  }

  /* If source is a stream, read source from it. Don't rebind load-truename or anything.
   * FIXME: Hypothetically we could load FASL streams directly as well. */
  if (cl__streamp(lsource))
    return load_stream(lsource, print.notnilp());

  // lsource must be a pathname, so
  msource = cl__merge_pathnames(lsource);
  if (msource.nilp()) {
    SIMPLE_ERROR(
        ("About to call core__coerce_to_file_pathname with NIL which was returned from cl__merge_pathnames when passed %s"),
        _rep_(lsource));
  }
  pathname = core__coerce_to_file_pathname(msource);
  if (pathname.nilp()) {
    SIMPLE_ERROR("core__coerce_to_file_pathname returned NIL for {}", _rep_(lsource));
  }

  pntype = pathname->_Type;

  filename = nil<T_O>();
  hooks = cl__symbol_value(core::_sym_STARloadHooksSTAR);
  if (pathname->_Directory.nilp() && pathname->_Host.nilp() && pathname->_Device.nilp() && !search_list.nilp()) {
    for (; search_list.notnilp(); search_list = oCdr(search_list)) {
      T_sp d = oCar(search_list);
      T_sp f = cl__merge_pathnames(pathname, d);
      T_sp ok = cl__load(f, verbose, print, nil<T_O>(), external_format, nil<T_O>());
      if (!ok.nilp()) {
        return ok;
      }
    }
  }
  filename = core__coerce_to_file_pathname(pathname);
  T_sp kind = core__file_kind(gc::As<Pathname_sp>(filename), true);
  if (kind == kw::_sym_directory) {
    ok = core__load_binary_directory(filename, verbose, print, external_format);
    if (ok.nilp()) {
      SIMPLE_ERROR("LOAD: Could not load file {}", _rep_(filename));
    }
    return _lisp->_true();
  }
  if (!pntype.nilp() && (pntype != kw::_sym_wild)) {
    /* If filename already has an extension, make sure
               that the file exists */
    // Test if pathname is nil is above
    if (pathname.nilp()) {
      SIMPLE_ERROR("In {} - about to pass NIL to core__coerce_to_file_pathname from {}", __FUNCTION__, _rep_(lsource));
    }
    filename = core__coerce_to_file_pathname(pathname);
    if (kind != kw::_sym_file && kind != kw::_sym_special) {
      filename = nil<T_O>();
    } else {
      function = nil<T_O>();
      if (hooks.consp()) {
        function = oCdr(gc::As<Cons_sp>(hooks)->assoc(pathname->_Type, nil<T_O>(), cl::_sym_equalp, nil<T_O>()));
        if (function.nilp()) {
          T_sp stream = cl__open(pathname, kw::_sym_input, ext::_sym_byte8, nil<T_O>(), false, nil<T_O>(), false, external_format,
                                 nil<T_O>());
          uint8_t bytes[4];
          clasp_read_byte8(stream, bytes, 4);
          cl__close(stream);
          T_sp magic = clasp_make_fixnum(((uint32_t)bytes[0] << 24) | ((uint32_t)bytes[1] << 16) | ((uint32_t)bytes[2] << 8) |
                                         ((uint32_t)bytes[3] << 0));
          function = oCdr(gc::As<Cons_sp>(hooks)->assoc(magic, nil<T_O>(), cl::_sym_equalp, nil<T_O>()));
        }
      }
    }
  } else {
    for (; hooks.notnilp(); hooks = oCdr(hooks)) {
      if (gc::IsA<String_sp>(oCaar(hooks))) {
        /* Otherwise try with known extensions until a matching
                     file is found */
        T_sp kind;
        filename = pathname;
        pathname->_Type = oCaar(hooks);
        function = oCdar(hooks);
        kind = core__file_kind(pathname, true);
        if (kind == kw::_sym_file || kind == kw::_sym_special)
          break;
        else
          filename = nil<T_O>();
      }
    }
  };
  if (filename.nilp()) {
    if (if_does_not_exist.nilp())
      return nil<T_O>();
    else {
      CANNOT_OPEN_FILE_ERROR(lsource);
    }
  }

  if (!function.nilp()) {
    /* We only bind these here rather than outside the condition because core:load-source,
     * being an exported function, has to bind them itself. */
    DynamicScopeManager scope(cl::_sym_STARloadPathnameSTAR, msource);
    T_sp truename = cl__truename(filename);
    DynamicScopeManager scope2(cl::_sym_STARloadTruenameSTAR, truename);
    ok = eval::funcall(function, filename, verbose, print, external_format);
  } else {
    ok = core__load_source(filename, verbose.isTrue(), print.isTrue(), external_format, false);
  }
  if (ok.nilp()) {
    SIMPLE_ERROR("LOAD: Could not load file {}", _rep_(filename));
  }
  return _lisp->_true();
}

/*! Translated from from ecl::load.d */
CL_LAMBDA(source &key (verbose *load-verbose*) (print *load-print*) (if-does-not-exist :error) (external-format :default) (search-list core::*load-search-list*));
CL_DECLARE();
CL_DOCSTRING(R"dx(CLHS: load)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__load(T_sp source, T_sp verbose, T_sp print, T_sp if_does_not_exist, T_sp external_format, T_sp search_list) {
  if (source.nilp()) {
    TYPE_ERROR(source, Cons_O::createList(cl::_sym_stream, cl::_sym_Pathname_O, cl::_sym_string));
  }
  DynamicScopeManager scope(cl::_sym_STARpackageSTAR, cl__symbol_value(cl::_sym_STARpackageSTAR));
  DynamicScopeManager scope2(cl::_sym_STARreadtableSTAR, cl__symbol_value(cl::_sym_STARreadtableSTAR));
  return core__load_no_package_set(source, verbose, print, if_does_not_exist, external_format, search_list);
};

SYMBOL_EXPORT_SC_(CorePkg, loadSource);

}; // namespace core
