#ifndef core_record_H
#define core_record_H

#include <clasp/core/foundation.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispVector.h>
#include <clasp/core/vectorObjectsWithFillPtr.fwd.h>

namespace core {
#if 0
#define RECORD_LOG(abf) printf("%s:%d %s\n", __FILE__, __LINE__, (abf).str().c_str());
#else
#define RECORD_LOG(abf)
#endif

T_sp record_circle_subst(T_sp repl_table, T_sp tree);

SMART(Record);
class Record_O : public T_O {
  LISP_BASE1(T_O);
  LISP_VIRTUAL_CLASS(core, CorePkg, Record_O, "Record");

public:
  typedef enum { initializing,
                 loading,
                 saving,
                 patching } RecordStage;

public:
  RecordStage _stage;
  List_sp _alist;
  T_sp _replacement_table;
  T_sp _Seen; // Was List_sp
public:       // Simple default ctor/dtor
  Record_O() : _stage(saving), _alist(_Nil<T_O>()), _Seen(_Nil<T_O>()){};
  Record_O(RecordStage stage, bool dummy, List_sp data);
  Record_O(RecordStage stage, T_sp replacement_table) : _stage(stage), _replacement_table(replacement_table), _Seen(_Nil<T_O>()){};
  virtual ~Record_O(){};

public:
  static Record_sp create_encoder() {
    Record_sp record = gctools::GCObjectAllocator<Record_O>::allocate(saving, false, _Nil<T_O>());
    return record;
  }
  static Record_sp create_initializer(List_sp data) {
    Record_sp record = gctools::GCObjectAllocator<Record_O>::allocate(initializing, false, data);
    return record;
  }
  static Record_sp create_decoder(List_sp data) {
    Record_sp record = gctools::GCObjectAllocator<Record_O>::allocate(loading, false, data);
    return record;
  }
  static Record_sp create_patcher(HashTable_sp replacement_table) {
    Record_sp record = gctools::GCObjectAllocator<Record_O>::allocate(patching, replacement_table);
    return record;
  }

public:
  List_sp data() const { return this->_alist; };
  RecordStage stage() const { return this->_stage; };

  void flagSeen(Cons_sp apair);

  void errorIfInvalidArguments();

  template <typename ST>
  void field(Symbol_sp name, ST &value) {
    RECORD_LOG(BF("field(Symbol_sp name, ST& value ) name: %s") % _rep_(name));
    switch (this->stage()) {
    case saving: {
      core::Cons_sp entry = core::Cons_O::create(name, translate::to_object<ST>::convert(value));
      RECORD_LOG(BF("saving entry: %s") % _rep_(entry));
      this->_alist = core::Cons_O::create(entry, this->_alist);
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = alist_get(this->_alist, name);
      if (find.nilp())
        SIMPLE_ERROR(BF("Could not find field %s") % _rep_(name));
      Cons_sp apair = gc::As<Cons_sp>(oCar(find));
      RECORD_LOG(BF("find apair %s\n") % _rep_(apair));
      value = translate::from_object<ST>(oCdr(apair))._v;
      if (this->stage() == initializing)
        this->flagSeen(apair);
    } break;
    case patching:
      // Do nothing for POD values
      break;
    };
  }

  template <typename OT>
  void field(Symbol_sp name, gc::smart_ptr<OT> &value) {
    RECORD_LOG(BF("field(Symbol_sp name, gc::smart_ptr<OT>& value ) name: %s") % _rep_(name));
    switch (this->stage()) {
    case saving: {
      core::Cons_sp apair = core::Cons_O::create(name, value);
      RECORD_LOG(BF("saving apair: %s") % _rep_(apair));
      this->_alist = core::Cons_O::create(apair, this->_alist);
    } break;
    case initializing:
    case loading: {
      List_sp find = alist_get(this->_alist, name);
      if (find.nilp())
        SIMPLE_ERROR(BF("Could not find field %s") % _rep_(name));
      Cons_sp apair = gc::As<Cons_sp>(oCar(find));
      RECORD_LOG(BF("init/load find apair %s\n") % _rep_(apair));
      // Set the value and ignore its type!!!!!! This is to allow placeholders
      T_sp v = oCdr(apair);
      RECORD_LOG(BF("init/load v: %s\n") % _rep_(v));
      value.setRaw_(reinterpret_cast<gc::Tagged>(v.raw_()));
      if (this->stage() == initializing)
        this->flagSeen(apair);
    } break;
    case patching:
      gc::smart_ptr<T_O> orig((gc::Tagged)value.raw_());
      T_sp patch = record_circle_subst(this->_replacement_table, orig);
      if (patch != orig) {
        RECORD_LOG(BF("Patching orig@%p: %s --> new@%p: %s\n") % _rep_(orig) % (void *)(orig.raw_()) % _rep_(patch) % (void *)(patch.raw_()));
        value.setRaw_(reinterpret_cast<gc::Tagged>(patch.raw_()));
      }
      break;
    };
  }

  template <typename OT>
  void field(Symbol_sp name, gctools::Vec0<gc::smart_ptr<OT>> &value) {
    RECORD_LOG(BF("field(Symbol_sp name, gctools::Vec0<gc::smart_ptr<OT>>& value ) name: %s") % _rep_(name));
    switch (this->stage()) {
    case saving: {
      Vector_sp vec_value = core_make_vector(cl::_sym_T_O, value.size());
      size_t idx(0);
      for (auto it : value)
        vec_value->operator[](idx++) = it;
      RECORD_LOG(BF("saving entry: %s") % _rep_(vec_value));
      Cons_sp apair = core::Cons_O::create(name, vec_value);
      this->_alist = core::Cons_O::create(apair, this->_alist);
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = alist_get(this->_alist, name);
      if (find.nilp())
        SIMPLE_ERROR(BF("Could not find field %s") % _rep_(name));
      Cons_sp apair = gc::As<Cons_sp>(oCar(find));
      RECORD_LOG(BF("loading find: %s") % _rep_(apair));
      Vector_sp vec_value = gc::As<Vector_sp>(oCdr(apair));
      RECORD_LOG(BF("vec_value: %s") % _rep_(vec_value));
      value.resize(cl_length(vec_value));
      for (size_t i(0), iEnd(cl_length(vec_value)); i < iEnd; ++i) {
        T_sp val = (*vec_value)[i];
        RECORD_LOG(BF("Loading vec0[%d] new@%p: %s\n") % i % (void *)(val.raw_()) % _rep_(val));
        value[i] = val;
      }
      if (this->stage() == initializing)
        this->flagSeen(apair);
    } break;
    case patching: {
      RECORD_LOG(BF("Patching"));
      for (size_t i(0), iEnd(value.size()); i < iEnd; ++i) {
        gc::smart_ptr<T_O> orig((gc::Tagged)value[i].raw_());
        T_sp patch = record_circle_subst(this->_replacement_table, orig);
        if (patch != orig) {
          RECORD_LOG(BF("Patching vec0[%d] orig@%p: %s --> new@%p: %s\n") % i % _rep_(orig) % (void *)(orig.raw_()) % _rep_(patch) % (void *)(patch.raw_()));
          value[i].rawRef_() = reinterpret_cast<OT *>(patch.raw_());
        }
      }
    } break;
    }
  };

  template <typename OT>
  void field_if_not_empty(Symbol_sp name, gctools::Vec0<gc::smart_ptr<OT>> &value) {
    switch (this->stage()) {
    case saving: {
      if (value.size() != 0)
        this->field(name, value);
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = alist_get(this->_alist, name);
      if (find.notnilp()) {
        this->field(name, value);
        Cons_sp apair = gc::As<Cons_sp>(oCar(find));
        if (this->stage() == initializing)
          this->flagSeen(apair);
      } else {
        value.clear();
      }
    } break;
    case patching: {
      if (value.size() != 0) {
        this->field(name, value);
      }
    } break;
    }
  };

  template <typename OT>
  void field_if_not_nil(Symbol_sp name, gc::smart_ptr<OT> &value) {
    switch (this->stage()) {
    case saving: {
      if (value.notnilp()) {
        Cons_sp apair = Cons_O::create(name, value);
        this->_alist = Cons_O::create(apair, this->_alist);
      }
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = alist_get(this->_alist, name);
      if (find.nilp())
        value = _Nil<core::T_O>();
      else {
        Cons_sp apair = gc::As<Cons_sp>(oCar(find));
        value = gc::As<gc::smart_ptr<OT>>(oCdr(apair));
        if (this->stage() == initializing)
          this->flagSeen(apair);
      }
    } break;
    case patching: {
      if (value.notnilp()) {
        gc::smart_ptr<T_O> orig((gc::Tagged)value.raw_());
        T_sp patch = record_circle_subst(this->_replacement_table, orig);
        if (patch != orig)
          value.setRaw_(reinterpret_cast<gc::Tagged>(patch.raw_()));
      }
    } break;
    }
  }

  template <typename OT>
  void field_if_not_nil(Symbol_sp name, gc::Nilable<gc::smart_ptr<OT>> &value) {
    switch (this->stage()) {
    case saving: {
      if (value.notnilp()) {
        Cons_sp apair = Cons_O::create(name, value);
        this->_alist = Cons_O::create(apair, this->_alist);
      }
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = alist_get(this->_alist, name);
      if (find.nilp())
        value = _Nil<core::T_O>();
      else {
        Cons_sp apair = gc::As<Cons_sp>(oCar(find));
        value = gc::As<gc::smart_ptr<OT>>(oCdr(apair));
        if (this->stage() == initializing)
          this->flagSeen(apair);
      }
    } break;
    case patching: {
      if (value.notnilp()) {
        gc::smart_ptr<T_O> orig((gc::Tagged)value.raw_());
        T_sp patch = record_circle_subst(this->_replacement_table, orig);
        if (patch != orig)
          value.rawRef_() = reinterpret_cast<OT *>(patch.raw_());
      }
    } break;
    }
  }

  template <typename T>
  void field_if_not_default(Symbol_sp name, T &value, const T &default_value) {
    switch (this->stage()) {
    case saving: {
      if (!(value == default_value)) {
        core::T_sp obj_value = translate::to_object<T>::convert(value);
        Cons_sp apair = Cons_O::create(name, obj_value);
        this->_alist = Cons_O::create(apair, this->_alist);
      }
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = alist_get(this->_alist, name);
      if (find.nilp()) {
        value = default_value;
      } else {
        Cons_sp apair = gc::As<Cons_sp>(oCar(find));
        value = translate::from_object<T>(oCdr(apair))._v;
        if (this->stage() == initializing)
          this->flagSeen(apair);
      }
    } break;
    case patching:
      // Do nothing for POD values
      break;
    }
  }

  template <typename T>
  void field_if_defined(Symbol_sp name, bool &defined, T &value) {
    switch (this->stage()) {
    case saving: {
      if (defined) {
        core::T_sp obj_value = translate::to_object<T>::convert(value);
        Cons_sp apair = Cons_O::create(name, obj_value);
        this->_alist = Cons_O::create(apair, this->_alist);
      }
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = alist_get(this->_alist, name);
      if (find.nilp()) {
        defined = false;
      } else {
        defined = true;
        Cons_sp apair = gc::As<Cons_sp>(oCar(find));
        value = translate::from_object<T>(oCdr(apair))._v;
        if (this->stage() == initializing)
          this->flagSeen(apair);
      }
    } break;
    case patching:
      // Do nothing for POD values
      break;
    }
  }
};
};
#endif
