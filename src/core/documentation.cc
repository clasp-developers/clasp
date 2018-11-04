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

//#define DEBUG_LEVEL_FULL
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/primitives.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/environment.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/documentation.h>
#include <clasp/core/wrappers.h>
namespace core {

CL_LAMBDA(record key sub-key);
CL_DECLARE();
CL_DOCSTRING("record_cons - see ECL helpfile.lsp>>record-cons");
CL_DEFUN T_sp core__record_cons(List_sp record, T_sp key, T_sp sub_key) {
  Cons_sp cons = Cons_O::create(key, sub_key);
  for (auto cur : coerce_to_list(record)) {
    List_sp i = oCar(cur);
    T_sp obj = oCar(i);
    if (cl__equalp(obj, cons)) return i;
  }
  return (_Nil<T_O>());
}

CL_LAMBDA(record key sub-key);
CL_DECLARE();
CL_DOCSTRING("record_field see ecl>>helpfile.lsp>>record-field");
CL_DEFUN T_sp core__record_field(List_sp record, T_sp key, T_sp sub_key) {
  List_sp cons = core__record_cons(record, key, sub_key);
  return oCdr(cons);
}

CL_LAMBDA(record key sub-key value);
CL_DECLARE();
CL_DOCSTRING("set_record_field");
CL_DEFUN T_sp core__set_record_field(List_sp record, T_sp key, T_sp sub_key, T_sp value) {
  List_sp field = gc::As<List_sp>(core__record_cons(record, key, sub_key));
  if (field.notnilp()) {
    field.asCons()->setCdr(value);
  } else {
    Cons_sp total_key = Cons_O::create(key, sub_key);
    Cons_sp new_field = Cons_O::create(total_key, value);
    record = Cons_O::create(new_field, record);
  }
  return record;
};

CL_LAMBDA(record key sub-key);
CL_DECLARE();
CL_DOCSTRING("rem_record_field");
CL_DEFUN T_sp core__rem_record_field(List_sp record, T_sp key, T_sp sub_key) {
  List_sp x = core__record_cons(record, key, sub_key);
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

CL_LAMBDA(object key sub-key value);
CL_DECLARE();
CL_DOCSTRING("annotate - see ecl>>helpfile.lsp>>annotate; **key** is either 'documentation or 'setf-documentation **object** must be a symbol");
CL_DEFUN T_mv ext__annotate(T_sp object, T_sp key, T_sp sub_key, T_sp value) {
  HashTable_sp dict = gc::As<HashTable_sp>(oCar(_sym_STARdocumentation_poolSTAR->symbolValue()));
  List_sp record = coerce_to_list(dict->gethash(object, _Nil<T_O>()));
  record = coerce_to_list(core__set_record_field(record, key, sub_key, value));
  T_sp result = dict->hash_table_setf_gethash(object, record);
  return result;
};


CL_LAMBDA(object key sub-key);
CL_DOCSTRING(R"(Remove an annotation)");
CL_DEFUN void ext__remove_annotation(T_sp object, T_sp key, T_sp sub_key) {
  T_sp tdict = oCar(core::_sym_STARdocumentation_poolSTAR->symbolValue());
  if (tdict.notnilp()) {
    HashTable_sp dict = gc::As<HashTable_sp>(tdict);
    T_sp value = dict->gethash(object);
    T_sp record = core__rem_record_field(value,key,sub_key);
    if (record.notnilp()) {
      dict->hash_table_setf_gethash(object,record);
    } else {
      dict->remhash(object);
    }
  }
}

CL_LAMBDA(sub-key symbol value);
CL_DECLARE();
CL_DOCSTRING("ensure_documentation");
CL_DEFUN void core__ensure_documentation(T_sp sub_key, Symbol_sp symbol, String_sp value) {
  ext__annotate(symbol, cl::_sym_documentation, sub_key, value);
};

CL_LAMBDA(object doc-type string);
CL_DOCSTRING("Set the documentation of an object");
CL_DEFUN T_sp core__set_documentation(T_sp object, T_sp doc_type, T_sp string) {
  if ( !(cl__stringp(string) || string.nilp()) ) {
    SIMPLE_ERROR(BF("%s is not a valid documentation string") % _rep_(string));
  }
  T_sp key = cl::_sym_documentation;
  if (object.notnilp() && object.consp() && core__valid_function_name_p(object)) {
    object = oCadr(object);
    key = core::_sym_setf_documentation;
  }
  if (string.notnilp()) {
    ext__annotate(object,key,doc_type,string);
  } else {
    ext__remove_annotation(object,key,doc_type);
  }
  return string;
}

void initialize_documentation_primitives(Lisp_sp lisp) {
  SYMBOL_SC_(CorePkg, record_cons);
  SYMBOL_SC_(CorePkg, record_field);
  SYMBOL_SC_(CorePkg, set_record_field);
  SYMBOL_SC_(CorePkg, rem_record_field);
  SYMBOL_EXPORT_SC_(ExtPkg, annotate);
  SYMBOL_SC_(CorePkg, ensure_documentation);
  // TODO move help_file.dat definition somewhere better
//  _sym_STARdocumentation_poolSTAR->defparameter(Cons_O::createList(HashTableEql_O::create_default(), Str_O::create("help_file.dat")));
//  _sym_STARdocumentation_poolSTAR->exportYourself();
}

}; /* namespace */
