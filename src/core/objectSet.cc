/*
    File: objectSet.cc
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
#include <clasp/core/lisp.h>
#include <clasp/core/objectSet.h>
#include <clasp/core/environment.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/wrappers.h>

namespace core {

void ObjectSet_O::initialize() {
  this->Base::initialize();
  this->_Set = HashTableEq_O::create_default();
}

void ObjectSet_O::addObjects(ObjectSet_sp other) {
  _G();
  other->map([this](T_sp obj) {this->insert(obj); });
}

Cons_sp ObjectSet_O::asCons() {
  Cons_sp res = _Nil<Cons_O>();
  this->map([&res](T_sp o) {
	    res = Cons_O::create(o,res);
  });
  return res;
}

ObjectSet_sp ObjectSet_O::setUnion(ObjectSet_sp other) {
  ObjectSet_sp os = ObjectSet_O::create();
  os->addObjects(this->sharedThis<ObjectSet_O>());
  os->addObjects(other);
  return os;
}

ObjectSet_sp ObjectSet_O::intersection(ObjectSet_sp b) {
  _G();
  ObjectSet_sp nset;
  nset = ObjectSet_O::create();

  this->map([&b, &nset](T_sp o) {
            if ( b->contains(o) ) {
                LOG(BF("Found it!!!") );
                nset->insert(o);
            } else {
                LOG(BF("Not found") );
            }
  });
  return nset;
}

ObjectSet_sp ObjectSet_O::relativeComplement(ObjectSet_sp b) {
  ObjectSet_sp nset;
  nset = ObjectSet_O::create();
  this->map([&b, &nset](T_sp o) {
	if ( !b->contains(o) ) {
	    nset->insert(o);
	}
  });
  return nset;
}

string ObjectSet_O::asString() const {
  stringstream ss;
  this->map([&ss](T_sp si) {
                ss << _rep_(si) << " ";
  });
  return ss.str();
}

/*! Return a new set that takes every element of (this) in combination
	with every element in b separated by a comma
*/
ObjectSet_sp ObjectSet_O::cartesianProduct(ObjectSet_sp b) {
  _G();
  ObjectSet_sp nset;
  stringstream sstr;
  nset = ObjectSet_O::create();
  this->map([&b, &nset, this](T_sp si) {
            this->map( [&si,&nset] (T_sp bi) {
                    Cons_sp op = _lisp->create<Cons_O>(si,bi);
                    nset->insert(op);
                });
  });
  return nset;
}

/*! Return a new set that takes every element of (this) in combination
	with every element in b separated by a comma
*/
ObjectSet_sp ObjectSet_O::cartesianProductWrapped(ObjectSet_sp b, const ObjectSetCartesianProductWrapper &wrapper) {
  _G();
  ObjectSet_sp nset;
  stringstream sstr;
  nset = ObjectSet_O::create();

  this->map([&b, &nset, this, &wrapper](T_sp si) {
            this->map( [&si,&nset,&wrapper] (T_sp bi) {
                    T_sp op = wrapper(si,bi);
                    nset->insert(op);
                });
  });
  return nset;
}

void ObjectSet_O::map(std::function<void(T_sp)> const &fn) {
  this->_Set->mapHash([&fn](T_sp key, T_sp val) {
                fn(key);
  });
}

void ObjectSet_O::map(std::function<void(T_sp)> const &fn) const {
  this->_Set->mapHash([&fn](T_sp key, T_sp val) {
                fn(key);
  });
}

void ObjectSet_O::addObjectsInCons(Cons_sp c) {
  while (c.notnilp()) {
    this->insert(oCar(c));
    c = cCdr(c);
  }
}

#if defined(XML_ARCHIVE)
void ObjectSet_O::archive(ArchiveP node) {
  _OF();
  stringstream suid;
  if (node->saving()) {
    _BLOCK_TRACE("Saving");
    if (this->_Set.size() != 0) {
      int i = 0;
      stringstream suid;
      T_sp obj;
      set<gctools::smart_ptr<T_O>>::iterator oi;
      for (oi = this->_Set.begin(); oi != this->_Set.end(); i++, oi++) {
        obj = (*oi);
        suid.str("");
        suid << i;
        node->archiveObject(suid.str(), obj);
      }
    }
  } else {
    _BLOCK_TRACE("Loading");
    VectorNodes::iterator ci;
    T_sp object;
    this->_Set.clear();
    for (ci = node->begin_Children(); ci != node->end_Children(); ci++) {
      object = node->getArchive()->loadObjectDirectly((*ci));
      ASSERTNOTNULL(object);
      this->_Set.insert(object);
    }
  }
}
#endif // defined(XML_ARCHIVE)

void ObjectSet_O::exposeCando(Lisp_sp lisp) {
  class_<ObjectSet_O>()
      .def("insert", &core::ObjectSet_O::insert)
      .def("add", &core::ObjectSet_O::insert)
      .def("addObjectsInCons", &core::ObjectSet_O::addObjectsInCons)
      .def("addObjects", &core::ObjectSet_O::addObjects)
      .def("size", &core::ObjectSet_O::size)
      //	.def("remove",&core::ObjectSet_O::remove)
      .def("asCons", &ObjectSet_O::asCons)
      .def("relativeComplement", &ObjectSet_O::relativeComplement)
      .def("core:objectSetUnion", &ObjectSet_O::setUnion)
      .def("core:objectSetIntersection", &ObjectSet_O::intersection)
      .def("contains", &ObjectSet_O::contains);
}

void ObjectSet_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON //[
  PYTHON_CLASS(CorePkg, ObjectSet, "", "", _lisp)
      .add_property("iterate",
                    boost::python::range(&core::ObjectSet_O::begin, &core::ObjectSet_O::end))
      .def("insert", &core::ObjectSet_O::insert)
      .def("add", &core::ObjectSet_O::insert)
      .def("add", &core::ObjectSet_O::insert)
      .def("setUnion", &core::ObjectSet_O::setUnion)
      .def("size", &core::ObjectSet_O::size)
      //	.def("remove",&core::ObjectSet_O::remove)
      ;
#endif
}
EXPOSE_CLASS(core, ObjectSet_O);
};
