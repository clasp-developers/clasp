/*
    File: sexpSaveArchive.cc
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

#if 0
#include <clasp/core/foundation.h>
#include <clasp/core/serialize.h>
#include <clasp/core/sexpSaveArchive.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/arguments.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/write_ugly.h>
#include <clasp/core/designators.h>
#include <clasp/core/array.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/numbers.h>
#include <clasp/core/symbol.h>
#include <clasp/core/wrappers.h>

namespace core {

/*
  __BEGIN_DOC( candoScript.general.saveArchive, subsection, saveArchive)
  \scriptCmd{save}{Object::object Text::fileName}

  Save the \sa{object} to the \sa{fileName} in Cando-Archive format.
  __END_DOC
*/
const char *keywords_saveArchive[] = {":debug", ""};





void SexpSaveArchive_O::write(SNode_sp snode, HashTable_sp snodeToRef, T_sp stream) {
  DEPRECATED();
#if 0
  if (snode->refCount() > 1) {
    T_sp ref = snodeToRef->gethash(snode, _Nil<T_O>());
    if (ref.notnilp()) {
      clasp_write_char('#', stream);
      write_fixnum(stream, ref);
      clasp_write_char('#', stream);
      return;
    } else {
      ref = make_fixnum(snodeToRef->hashTableCount() + 1);
      snodeToRef->hash_table_setf_gethash(snode, ref);
      clasp_write_char('#', stream);
      ref->__write__(stream);
      clasp_write_char('=', stream);
    }
  }
  if (snode->leafSNodeP()) {
    T_sp obj = snode->object();
    write_ugly_object(obj, stream);
    clasp_write_char(' ', stream);
  } else {
    BranchSNode_sp bsnode = gc::As<BranchSNode_sp>(snode);
    clasp_write_char('\n', stream);
    clasp_write_char('(', stream);
    write_ugly_object(bsnode->_Kind, stream);
    clasp_write_string(" (", stream);
    for (List_sp cur = bsnode->_SNodePList; cur.notnilp(); cur = oCddr(cur)) {
      write_ugly_object(oCar(cur), stream);
      clasp_write_char(' ', stream);
      SNode_sp property = gc::As<SNode_sp>(oCadr(cur));
      this->write(property, snodeToRef, stream);
      clasp_write_char(' ', stream);
    }
    clasp_write_string(") ", stream);
    ASSERT(!bsnode->_VectorSNodes.unboundp());
    if (!bsnode->_VectorSNodes.unboundp() && bsnode->_VectorSNodes->length() > 0) {
      clasp_write_string(" #", stream);
      write_ugly_object(make_fixnum(bsnode->_VectorSNodes->length()), stream);
      clasp_write_string("( ", stream);
      for (int i(0), iEnd(bsnode->_VectorSNodes->length()); i < iEnd; ++i) {
        SNode_sp snode = gc::As<SNode_sp>(bsnode->_VectorSNodes->rowMajorAref(i));
        this->write(snode, snodeToRef, stream);
        clasp_write_char(' ', stream);
      }
      clasp_write_char(')', stream);
    }
    clasp_write_char(')', stream);
  }
#endif
}

CL_LISPIFY_NAME("sexpSaveArchiveWrite");
CL_DEFMETHOD void SexpSaveArchive_O::sexpSaveArchiveWrite(T_sp streamDesignator) {
  DynamicScopeManager scope(_sym_STARserializerArchiveSTAR, this->asSmartPtr());
  T_sp stream = coerce::outputStreamDesignator(streamDesignator);
  HashTable_sp sNodeToRef = HashTable_O::create(cl::_sym_eq);
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_readablySTAR, _lisp->_true());
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_escapeSTAR, _lisp->_true());
  this->write(this->_TopNode, sNodeToRef, stream);
};
};
#endif
