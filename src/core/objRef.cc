/*
    File: objRef.cc
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
#include <clasp/core/lisp.h>
#include <clasp/core/objRef.h>
#include <clasp/core/str.h>
#include <clasp/core/wrappers.h>

namespace core {

REGISTER_CLASS(core, ObjRef_O);

void ObjRef_O::initialize() {
  this->Base::initialize();
  this->_Selector = "";
  this->_Name = "";
  this->_SubRef = _Nil<ObjRef_O>();
}

#if defined(XML_ARCHIVE)
void ObjRef_O::archive(ArchiveP node) {
  node->attribute("sel", this->_Selector);
  node->attribute("name", this->_Name);
  node->attributeIfNotNil("subRef", this->_SubRef);
}
#endif // defined(XML_ARCHIVE)

T_sp ObjRef_O::relativeTo(T_sp o) {
  _G();
  T_sp obj = o->oGetReference(this->sharedThis<ObjRef_O>());
  if (this->_SubRef.notnilp()) {
    obj = gc::As<ObjRef_sp>(this->_SubRef)->relativeTo(obj);
  }
  return obj;
}

string ObjRef_O::asString() {
  stringstream ss;
  if (this->_Name != "") {
    ss << this->_Selector << "=" << this->_Name;
  } else {
    ss << this->_Selector;
  }
  if (this->_SubRef.notnilp()) {
    ss << "/" << gc::As<ObjRef_sp>(this->_SubRef)->asString();
  }
  return ss.str();
}

/*! Create an ObjRef from a string representation
 * The form of the string is ref1/ref2/ref3 ...
 * each ref# has the form selector=name or selector.
 * Eg: Constitution=Pro4/Frame=+core
 *
 */
ObjRef_sp ObjRef_O::create(Lisp_sp e, const string &asString) {
  string head, tail, orSelector, orName;
  GC_ALLOCATE(ObjRef_O, ref);
  VectorStrings parts;
  size_t split = asString.find("/");
  if (split != string::npos) {
    head = asString.substr(0, split);
    tail = asString.substr(split + 1, 99999);
  } else {
    head = asString;
    tail = "";
  }
  size_t colon = head.find("=");
  if (colon == string::npos) {
    orSelector = head;
    orName = "";
  } else {
    orSelector = head.substr(0, colon);
    orName = head.substr(colon + 1, 99999);
  }
  ref = ObjRef_O::create();
  ref->setSelector(orSelector);
  ref->setName(orName);
  if (tail != "") {
    ObjRef_sp subRef = ObjRef_O::create(e, tail);
    ref->setSubRef(subRef);
  }
  return ref;
}
};
