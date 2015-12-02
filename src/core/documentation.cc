/*
    File: documentation.cc
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
#include <clasp/core/str.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/environment.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/documentation.h>
#include <clasp/core/wrappers.h>
namespace core {

#define ARGS_af_record_cons "(record key sub-key)"
#define DECL_af_record_cons ""
#define DOCS_af_record_cons "record_cons - see ECL helpfile.lsp>>record-cons"
T_sp af_record_cons(List_sp record, T_sp key, T_sp sub_key) {
  _G();
  Cons_sp cons = Cons_O::create(key, sub_key);
  for (auto cur : coerce_to_list(record)) {
    List_sp i = oCar(cur);
    T_sp obj = oCar(i);
    if (T_sp(eval::funcall(cl::_sym_equalp, obj, cons)).isTrue())
      return (i);
  }
  return (_Nil<T_O>());
}

#define ARGS_af_record_field "(record key sub-key)"
#define DECL_af_record_field ""
#define DOCS_af_record_field "record_field see ecl>>helpfile.lsp>>record-field"
T_sp af_record_field(List_sp record, T_sp key, T_sp sub_key) {
  _G();
  List_sp cons = eval::funcall(_sym_record_cons, record, key, sub_key);
  ;
  return oCdr(cons);
}

#define ARGS_af_set_record_field "(record key sub-key value)"
#define DECL_af_set_record_field ""
#define DOCS_af_set_record_field "set_record_field"
T_sp af_set_record_field(List_sp record, T_sp key, T_sp sub_key, Str_sp value) {
  _G();
  List_sp field = gc::As<List_sp>(eval::funcall(_sym_record_cons, record, key, sub_key));
  if (field.notnilp()) {
    field.asCons()->setCdr(value);
  } else {
    Cons_sp total_key = Cons_O::create(key, sub_key);
    Cons_sp new_field = Cons_O::create(total_key, value);
    record = Cons_O::create(new_field, record);
  }
  return record;
};

#define ARGS_af_rem_record_field "(record key sub-key)"
#define DECL_af_rem_record_field ""
#define DOCS_af_rem_record_field "rem_record_field"
T_sp af_rem_record_field(List_sp record, T_sp key, T_sp sub_key) {
  _G();
  List_sp x = af_record_cons(record, key, sub_key);
  if (x.notnilp()) {
    List_sp output = _Nil<T_O>();
    for (auto cur : record) {
      List_sp i = oCar(cur);
      if (i != x) {
        output = Cons_O::create(i, output);
      }
    }
    return output;
  }
  return record;
}

#define ARGS_ext_annotate "(object key sub-key value)"
#define DECL_ext_annotate ""
#define DOCS_ext_annotate "annotate - see ecl>>helpfile.lsp>>annotate; key is either 'documentation or 'setf-documentation and I currently think (object) must be a symbol so I'll trigger an exception if it isn't"
T_mv ext_annotate(T_sp object, T_sp key, T_sp sub_key, Str_sp value) {
  _G();
  HashTable_sp dict = gc::As<HashTable_sp>(oCar(_sym_STARdocumentation_poolSTAR->symbolValue()));
  List_sp record = coerce_to_list(dict->gethash(object, _Nil<T_O>()));
  record = coerce_to_list(af_set_record_field(record, key, sub_key, value));
  T_sp result = dict->hash_table_setf_gethash(object, record);
  return (Values(result));
};

#define ARGS_af_ensure_documentation "(sub-key symbol value)"
#define DECL_af_ensure_documentation ""
#define DOCS_af_ensure_documentation "ensure_documentation"
SYMBOL_EXPORT_SC_(ClPkg, documentation);
void af_ensure_documentation(T_sp sub_key, Symbol_sp symbol, Str_sp value) {
  _G();
  ext_annotate(symbol, cl::_sym_documentation, sub_key, value);
};

void initialize_documentation_primitives(Lisp_sp lisp) {
  _G();
  SYMBOL_SC_(CorePkg, record_cons);
  Defun(record_cons);
  SYMBOL_SC_(CorePkg, record_field);
  Defun(record_field);
  SYMBOL_SC_(CorePkg, set_record_field);
  Defun(set_record_field);
  SYMBOL_SC_(CorePkg, rem_record_field);
  Defun(rem_record_field);
  SYMBOL_EXPORT_SC_(ExtPkg, annotate);
  ExtDefun(annotate);
  SYMBOL_SC_(CorePkg, ensure_documentation);
  Defun(ensure_documentation);
  // TODO move help_file.dat definition somewhere better
  _sym_STARdocumentation_poolSTAR->defparameter(Cons_O::createList(HashTableEql_O::create_default(), Str_O::create("help_file.dat")));
  _sym_STARdocumentation_poolSTAR->exportYourself();
}

}; /* namespace */
