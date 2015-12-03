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
#include <clasp/core/str.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/primitives.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/pathname.h>
#include <clasp/core/lispReader.h>
#include <clasp/core/evaluator.h>
#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/core/predicates.h>
#include <clasp/core/wrappers.h>

namespace core {

#define ARGS_af_loadSource "(source &optional verbose print external-format)"
#define DECL_af_loadSource ""
#define DOCS_af_loadSource "loadSource"
T_sp af_loadSource(T_sp source, bool verbose, bool print, T_sp externalFormat) {
  _G();
  T_sp strm;
  void *strmPointer;
  if (cl_streamp(source)) {
    strm = source;
    if (!clasp_input_stream_p(strm)) {
      SIMPLE_ERROR(BF("Stream must be an input stream"));
    }
  } else {
    strm = cl_open(source,
                   kw::_sym_input,
                   cl::_sym_character,
                   _Nil<T_O>(), false,
                   _Nil<T_O>(), false,
                   kw::_sym_default,
                   _Nil<T_O>());
    if (strm.nilp())
      return _Nil<T_O>();
    //    printf("%s:%d  Just created strm@%p  tagged-pointer: %p\n", __FILE__, __LINE__, &strm, strm.raw_());
    strmPointer = &(*strm);
  }
  /* Define the source file */
  SourceFileInfo_sp sfi = core_sourceFileInfo(source);
  DynamicScopeManager scope(_sym_STARcurrentSourceFileInfoSTAR, sfi);
  Pathname_sp pathname = cl_pathname(source);
  ASSERTF(pathname.objectp(), BF("Problem getting pathname of [%s] in loadSource") % _rep_(source));
  ;
  Pathname_sp truename = cl_truename(source);
  ASSERTF(truename.objectp(), BF("Problem getting truename of [%s] in loadSource") % _rep_(source));
  ;
  scope.pushSpecialVariableAndSet(cl::_sym_STARloadPathnameSTAR, pathname);
  scope.pushSpecialVariableAndSet(cl::_sym_STARloadTruenameSTAR, truename);
  /* Create a temporary closure to load the source */
  SourcePosInfo_sp spi = SourcePosInfo_O::create(sfi->fileHandle(), 0, 0, 0);
  while (true) {
    bool echoReplRead = _sym_STARechoReplReadSTAR->symbolValue().isTrue();
    DynamicScopeManager innerScope(_sym_STARsourceDatabaseSTAR, SourceManager_O::create());
    //    printf("%s:%d  Pushing stream source pos for strm@%p   tagged-ptr: %p\n", __FILE__, __LINE__, &strm, strm.raw_());
    innerScope.pushSpecialVariableAndSet(_sym_STARcurrentSourcePosInfoSTAR, core_inputStreamSourcePosInfo(strm));
    T_sp x = cl_read(strm, _Nil<T_O>(), _Unbound<T_O>(), _Nil<T_O>());
    if (x.unboundp())
      break;
    _sym_STARcurrentSourcePosInfoSTAR->setf_symbolValue(core_walkToFindSourcePosInfo(x, _sym_STARcurrentSourcePosInfoSTAR->symbolValue()));
    if (echoReplRead) {
      _lisp->print(BF("Read: %s\n") % _rep_(x));
    }
    if (x.number_of_values() > 0) {
      //                printf("%s:%d  ;; -- read- %s\n", __FILE__, __LINE__, _rep_(x).c_str() );
      if (print) {
        _lisp->print(BF(";; -- read- %s\n") % _rep_(x));
      };
      eval::funcall(core::_sym_STAReval_with_env_hookSTAR->symbolValue(), x, _Nil<T_O>());
      //                gctools::af_cleanup();
    }
  }
  //  printf("%s:%d  closing strm@%p  tagged-ptr: %p\n", __FILE__, __LINE__, &strm, strm.raw_());
  cl_close(strm);
  return _lisp->_true();
}

/*! Translated from from ecl::load.d */
#define ARGS_af_load "(source &key (verbose *load-verbose*) (print *load-print*) (if-does-not-exist :error) (external-format :default) (search-list core::*load-search-list*))"
#define DECL_af_load ""
#define DOCS_af_load "CLHS: load"
T_sp af_load(T_sp source, T_sp verbose, T_sp print, T_sp if_does_not_exist, T_sp external_format, T_sp search_list) {
  Pathname_sp pathname;
  T_sp pntype;
  T_sp hooks;
  T_sp filename;
  T_sp function = _Nil<T_O>();
  T_sp ok;
  bool not_a_filename = false;

  //        printf("%s:%d af_load source= %s\n", __FILE__, __LINE__, _rep_(source).c_str());

  /* If source is a stream, read conventional lisp code from it */
  if (cl_streamp(source)) {
    /* INV: if "source" is not a valid stream, file.d will complain */
    filename = source;
    function = _Nil<T_O>();
    not_a_filename = true;
    goto NOT_A_FILENAME;
  }
  /* INV: coerce_to_file_pathname() creates a fresh new pathname object */
  source = af_mergePathnames(source);
  //        printf("%s:%d af_load after mergePathnames source= %s\n", __FILE__, __LINE__, _rep_(source).c_str());

  pathname = af_coerceToFilePathname(source);
  pntype = pathname->_Type;

  filename = _Nil<T_O>();
  hooks = af_symbolValue(ext::_sym_STARloadHooksSTAR);
  if (pathname->_Directory.nilp() &&
      pathname->_Host.nilp() &&
      pathname->_Device.nilp() &&
      !search_list.nilp()) {
    for (; search_list.notnilp(); search_list = oCdr(search_list)) {
      T_sp d = oCar(search_list);
      T_sp f = af_mergePathnames(pathname, d);
      T_sp ok = af_load(f, verbose, print, _Nil<T_O>(), external_format, _Nil<T_O>());
      if (!ok.nilp()) {
        return ok;
      }
    }
  }
  if (!pntype.nilp() && (pntype != kw::_sym_wild)) {
    /* If filename already has an extension, make sure
	       that the file exists */
    T_sp kind;
    filename = af_coerceToFilePathname(pathname);
    kind = af_file_kind(gc::As<Pathname_sp>(filename), true);
    if (kind != kw::_sym_file && kind != kw::_sym_special) {
      filename = _Nil<T_O>();
    } else {
      function = _Nil<T_O>();
      if (cl_consp(hooks)) {
        function = oCdr(gc::As<Cons_sp>(hooks)->assoc(pathname->_Type,
                                                      _Nil<T_O>(),
                                                      cl::_sym_string_EQ_,
                                                      _Nil<T_O>()));
      }
    }
  } else {
    for (; hooks.notnilp(); hooks = oCdr(hooks)) {
      /* Otherwise try with known extensions until a matching
		   file is found */
      T_sp kind;
      filename = pathname;
      pathname->_Type = oCaar(hooks);
      function = oCdar(hooks);
      kind = af_file_kind(pathname, true);
      if (kind == kw::_sym_file || kind == kw::_sym_special)
        break;
      else
        filename = _Nil<T_O>();
    }
  };
  if (filename.nilp()) {
    if (if_does_not_exist.nilp())
      return _Nil<T_O>();
    else {
      CANNOT_OPEN_FILE_ERROR(source);
    }
  }
NOT_A_FILENAME:
  if (verbose.notnilp()) {
    eval::funcall(cl::_sym_format, _lisp->_true(),
                  Str_O::create("~&;;; Loading ~s~%"),
                  filename);
  }
  DynamicScopeManager scope(cl::_sym_STARpackageSTAR, af_symbolValue(cl::_sym_STARpackageSTAR));
  scope.pushSpecialVariableAndSet(cl::_sym_STARreadtableSTAR, af_symbolValue(cl::_sym_STARreadtableSTAR));
  scope.pushSpecialVariableAndSet(cl::_sym_STARloadPathnameSTAR, not_a_filename ? _Nil<T_O>() : source);
  T_sp truename = cl_truename(filename);
  scope.pushSpecialVariableAndSet(cl::_sym_STARloadTruenameSTAR, not_a_filename ? _Nil<T_O>() : truename);
  if (!not_a_filename)
    filename = truename;
  if (!function.nilp()) {
    ok = eval::funcall(function, filename, verbose, print, external_format);
  } else {
    SIMPLE_ERROR(BF("LOAD could not identify type of file for loading %s") % _rep_(filename));
  }
  if (ok.nilp()) {
    SIMPLE_ERROR(BF("LOAD: Could not load file %s") % _rep_(filename));
  }
  if (print.notnilp()) {
    eval::funcall(cl::_sym_format, _lisp->_true(),
                  Str_O::create("~&;;; Loading ~s~%"),
                  filename);
  }
  return _lisp->_true();
}

void initialize_load() {
  SYMBOL_EXPORT_SC_(CorePkg, loadSource);
  Defun(loadSource);
  Defun(load);
}
};
