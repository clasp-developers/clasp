#pragma once

#include <clasp/gctools/containers.h>
#include <clasp/core/nativeVector.fwd.h>
#include <clasp/core/cxxObject.h>
namespace core {

class NativeVector_int_O : public CxxObject_O {
  LISP_CLASS(core, CorePkg, NativeVector_int_O, "NativeVector<int>", CxxObject_O);

public:
  gctools::Vec0<int> _Vector;

public:
  CL_LISPIFY_NAME("make-native-vector<int>");
  CL_DEF_CLASS_METHOD inline static gc::smart_ptr<NativeVector_int_O> make() { return gctools::GC<NativeVector_int_O>::allocate(); }

public:
  inline int& operator[](size_t i) {
    BOUNDS_ASSERT(i < this->_Vector.size());
    return this->_Vector[i];
  };
  inline const int& operator[](size_t i) const {
    BOUNDS_ASSERT(i < this->_Vector.size());
    return this->_Vector[i];
  };
  CL_LISPIFY_NAME("native-vector<int>-setf-elt");
  CL_DEFMETHOD inline void setf_elt(size_t index, int val) { this->_Vector[index] = val; };
  CL_LISPIFY_NAME("native-vector<int>-elt");
  CL_DEFMETHOD inline int elt(size_t index) { return this->_Vector[index]; };
  CL_LISPIFY_NAME("native-vector<int>-clear");
  CL_DEFMETHOD inline void clear() { this->_Vector.clear(); };
  CL_LISPIFY_NAME("native-vector<int>-push-back");
  CL_DEFMETHOD inline void push_back(int val) { this->_Vector.push_back(val); };
  CL_LISPIFY_NAME("native-vector<int>-size");
  CL_DEFMETHOD inline size_t size() { return this->_Vector.size(); };
  CL_LISPIFY_NAME("native-vector<int>-capacity");
  CL_DEFMETHOD inline size_t capacity() { return this->_Vector.capacity(); };
  CL_LISPIFY_NAME("native-vector<int>-resize");
  CL_DEFMETHOD inline void resize(size_t sz, int val) { this->_Vector.resize(sz, val); };
};

}; // namespace core
