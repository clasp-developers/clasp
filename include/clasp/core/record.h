#pragma once

#include <clasp/core/symbolTable.h>
#include <clasp/core/array.h>
#include <clasp/core/sequence.h> // cl__length

namespace core {

#if 0
#define DEBUG_RECORD 1
#define RECORD_LOG(...)                                                                                                            \
  fmt::print("{}:{}:{}\n", __FILE__, __LINE__, __FUNCTION__);                                                                      \
  fmt::print(__VA_ARGS__);
#else
#define RECORD_LOG(...)
#endif

T_sp record_circle_subst(Record_sp record, T_sp tree);

}; // namespace core

template <> struct gctools::GCInfo<core::Record_O> {
  static bool const NeedsInitialization = true;
  static bool const NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
  //  static bool const InlineScan = true;
  //  static bool const Roo
};

namespace core {
FORWARD(Record);
FORWARD(SharpEqualWrapper);
class Record_O : public General_O {
  LISP_CLASS(core, CorePkg, Record_O, "Record", General_O);

public:
  typedef enum { initializing, loading, saving, patching } RecordStage;

public:
  RecordStage _stage;
  List_sp _alist;
  T_sp _patching_callback;
  T_sp _Seen; // Was List_sp
public:       // Simple default ctor/dtor
  Record_O() : _stage(saving), _alist(nil<T_O>()), _Seen(nil<T_O>()){};
  Record_O(RecordStage stage, bool dummy, List_sp data);
  Record_O(RecordStage stage, T_sp callback) : _stage(stage), _patching_callback(callback), _Seen(nil<T_O>()){};

public:
  static Record_sp create_encoder() {
    auto record = gctools::GC<Record_O>::allocate(saving, false, nil<T_O>());
    return record;
  }
  static Record_sp create_initializer(List_sp data) {
    auto record = gctools::GC<Record_O>::allocate(initializing, false, data);
    return record;
  }
  static Record_sp create_decoder(List_sp data) {
    auto record = gctools::GC<Record_O>::allocate(loading, false, data);
    return record;
  }
  static Record_sp create_patcher(T_sp patcher_callback) {
    auto record = gctools::GC<Record_O>::allocate(patching, patcher_callback);
    return record;
  }

public:
  void initialize() override;
  List_sp data() const { return this->_alist; };
  T_sp seen() const { return this->_Seen; };
  RecordStage stage() const { return this->_stage; };

  void flagSeen(Cons_sp apair);

  void errorIfInvalidArguments();

  template <typename ST> void field(Symbol_sp name, ST& value) {
    RECORD_LOG(("field(Symbol_sp name, ST& value ) name: %s"), _rep_(name));
    switch (this->stage()) {
    case saving: {
      core::Cons_sp entry = core::Cons_O::create(name, translate::to_object<ST>::convert(value));
      RECORD_LOG(("saving entry: %s"), _rep_(entry));
      this->_alist = core::Cons_O::create(entry, this->_alist);
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (find.consp()) {
        RECORD_LOG(("find apair %s\n"), _rep_(find));
        value = translate::make_from_object<ST>(CONS_CDR(find));
        if (this->stage() == initializing)
          this->flagSeen(gc::As_unsafe<Cons_sp>(find));
      } else {
        SIMPLE_ERROR("Could not find field {}", _rep_(name));
      }
    } break;
    case patching:
      // Do nothing for POD values
      break;
    };
  }

  template <typename OT> void field(Symbol_sp name, gc::smart_ptr<OT>& value) {
    RECORD_LOG(("field(Symbol_sp name, gc::smart_ptr<OT>& value ) name: %s"), _rep_(name));
    switch (this->stage()) {
    case saving: {
      core::Cons_sp apair = core::Cons_O::create(name, value);
      RECORD_LOG(("saving apair: %s"), _rep_(apair));
      this->_alist = core::Cons_O::create(apair, this->_alist);
    } break;
    case initializing: {
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (!find.consp())
        SIMPLE_ERROR("Could not find field {}", _rep_(name));
      Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
      RECORD_LOG(("init/load find apair %s\n"), _rep_(apair));
      // Set the value and ignore its type!!!!!! This is to allow placeholders
      T_sp v = CONS_CDR(apair);
      if (!gc::IsA<gc::smart_ptr<OT>>(v)) {
        class_id expected_typ = reg::registered_class<OT>::id;
        lisp_errorBadCastFromT_O(expected_typ, reinterpret_cast<core::T_O*>(v.raw_()));
      }
      RECORD_LOG(("init/load v: %s v.raw_=%p\n"), _rep_(v), (void*)v.raw_());
      value.setRaw_(reinterpret_cast<gc::Tagged>(v.raw_()));
      if (this->stage() == initializing)
        this->flagSeen(apair);
    } break;
    case loading: {
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (!find.consp())
        SIMPLE_ERROR("Could not find field {}", _rep_(name));
      Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
      RECORD_LOG(("init/load find apair %s\n"), _rep_(apair));
      // Set the value and ignore its type!!!!!! This is to allow placeholders
      T_sp v = CONS_CDR(apair);
      RECORD_LOG(("init/load v: %s v.raw_=%p\n"), _rep_(v), (void*)v.raw_());
      value.setRaw_(reinterpret_cast<gc::Tagged>(v.raw_()));
      if (this->stage() == initializing)
        this->flagSeen(apair);
    } break;
    case patching:
      gc::smart_ptr<T_O> orig((gc::Tagged)value.raw_());
      T_sp patch = record_circle_subst(this->asSmartPtr(), orig);
      if (patch != orig) {
        RECORD_LOG(("Patching orig@%p: %s --> new@%p: %s\n"), (void*)(orig.raw_()), _rep_(orig), (void*)(patch.raw_()),
                   _rep_(patch));
        value.setRaw_(reinterpret_cast<gc::Tagged>(patch.raw_()));
      }
      break;
    };
  }

  template <typename OT> void field(Symbol_sp name, gctools::Vec0<gc::smart_ptr<OT>>& value) {
    RECORD_LOG(("field(Symbol_sp name, gctools::Vec0<gc::smart_ptr<OT>>& value ) name: %s"), _rep_(name));
    switch (this->stage()) {
    case saving: {
      Vector_sp vec_value = core__make_vector(cl::_sym_T_O, value.size());
      size_t idx(0);
      for (auto it : value)
        vec_value->rowMajorAset(idx++, it);
      RECORD_LOG(("saving entry: %s"), _rep_(vec_value));
      Cons_sp apair = core::Cons_O::create(name, vec_value);
      this->_alist = core::Cons_O::create(apair, this->_alist);
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (!find.consp())
        SIMPLE_ERROR("Could not find field {}", _rep_(name));
      Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
      RECORD_LOG(("loading find: %s"), _rep_(apair));
      Vector_sp vec_value = gc::As<Vector_sp>(CONS_CDR(apair));
      RECORD_LOG(("vec_value: %s"), _rep_(vec_value));
      value.resize(cl__length(vec_value));
      for (size_t i(0), iEnd(cl__length(vec_value)); i < iEnd; ++i) {
        T_sp val = vec_value->rowMajorAref(i);
        RECORD_LOG(("Loading vec0[%d] new@%p: %s\n"), i, (void*)(val.raw_()), _rep_(val));
        value[i].rawRef_() = reinterpret_cast<OT*>(val.raw_());
      }
      if (this->stage() == initializing)
        this->flagSeen(apair);
    } break;
    case patching: {
      RECORD_LOG(("Patching"));
      for (size_t i(0), iEnd(value.size()); i < iEnd; ++i) {
        gc::smart_ptr<T_O> orig((gc::Tagged)value[i].raw_());
        T_sp patch = record_circle_subst(this->asSmartPtr(), orig);
        if (patch != orig) {
          RECORD_LOG(("Patching vec0[%d] orig@%p: %s --> new@%p: %s\n"), i, _rep_(orig), (void*)(orig.raw_()), _rep_(patch),
                     (void*)(patch.raw_()));
          value[i].rawRef_() = reinterpret_cast<OT*>(patch.raw_());
        }
      }
    } break;
    }
  };

  template <typename OT> void field(Symbol_sp name, gctools::Vec0_uncopyable<gc::smart_ptr<OT>>& value) {
    RECORD_LOG(("field(Symbol_sp name, gctools::Vec0<gc::smart_ptr<OT>>& value ) name: %s"), _rep_(name));
    switch (this->stage()) {
    case saving: {
      Vector_sp vec_value = core__make_vector(cl::_sym_T_O, value.size());
      size_t idx(0);
      for (auto it : value)
        vec_value->rowMajorAset(idx++, it);
      RECORD_LOG(("saving entry: %s"), _rep_(vec_value));
      Cons_sp apair = core::Cons_O::create(name, vec_value);
      this->_alist = core::Cons_O::create(apair, this->_alist);
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (!find.consp())
        SIMPLE_ERROR("Could not find field {}", _rep_(name));
      Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
      RECORD_LOG(("loading find: %s"), _rep_(apair));
      Vector_sp vec_value = gc::As<Vector_sp>(CONS_CDR(apair));
      RECORD_LOG(("vec_value: %s"), _rep_(vec_value));
      value.resize(cl__length(vec_value));
      for (size_t i(0), iEnd(cl__length(vec_value)); i < iEnd; ++i) {
        T_sp val = vec_value->rowMajorAref(i);
        RECORD_LOG(("Loading vec0[%d] new@%p: %s\n"), i, (void*)(val.raw_()), _rep_(val));
        value[i].rawRef_() = reinterpret_cast<OT*>(val.raw_());
      }
      if (this->stage() == initializing)
        this->flagSeen(apair);
    } break;
    case patching: {
      RECORD_LOG(("Patching"));
      for (size_t i(0), iEnd(value.size()); i < iEnd; ++i) {
        gc::smart_ptr<T_O> orig((gc::Tagged)value[i].raw_());
        T_sp patch = record_circle_subst(this->asSmartPtr(), orig);
        if (patch != orig) {
          RECORD_LOG(("Patching vec0[%d] orig@%p: %s --> new@%p: %s\n"), i, _rep_(orig), (void*)(orig.raw_()), _rep_(patch),
                     (void*)(patch.raw_()));
          value[i].rawRef_() = reinterpret_cast<OT*>(patch.raw_());
        }
      }
    } break;
    }
  };

  template <typename SC> void field(Symbol_sp name, gctools::Vec0<SC>& value) {
    RECORD_LOG(("field(Symbol_sp name, gctools::Vec0<SC& value ) name: %s"), _rep_(name));
    switch (this->stage()) {
    case saving: {
      Vector_sp vec_value = core__make_vector(cl::_sym_T_O, value.size());
      size_t idx(0);
      for (auto it : value)
        vec_value->rowMajorAset(idx++, translate::to_object<SC>::convert(it));
      RECORD_LOG(("saving entry: %s"), _rep_(vec_value));
      Cons_sp apair = core::Cons_O::create(name, vec_value);
      this->_alist = core::Cons_O::create(apair, this->_alist);
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (!find.consp())
        SIMPLE_ERROR("Could not find field {}", _rep_(name));
      Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
      RECORD_LOG(("loading find: %s"), _rep_(apair));
      Vector_sp vec_value = gc::As<Vector_sp>(CONS_CDR(apair));
      RECORD_LOG(("vec_value: %s"), _rep_(vec_value));
      value.resize(cl__length(vec_value));
      for (size_t i(0), iEnd(cl__length(vec_value)); i < iEnd; ++i) {
        T_sp val = vec_value->rowMajorAref(i);
        RECORD_LOG(("Loading vec0[%d] new@%p: %s\n"), i, (void*)(val.raw_()), _rep_(val));
        value[i] = translate::make_from_object<SC>(val);
      }
      if (this->stage() == initializing)
        this->flagSeen(apair);
    } break;
    case patching: {
      RECORD_LOG(("Patching"));
      for (size_t i(0), iEnd(value.size()); i < iEnd; ++i) {
        gc::smart_ptr<T_O> orig((gc::Tagged)translate::to_object<SC>::convert(value[i]).raw_());
        T_sp patch = record_circle_subst(this->asSmartPtr(), orig);
        if (patch != orig) {
          RECORD_LOG(("Patching vec0[%d] orig@%p: %s --> new@%p: %s\n"), i, _rep_(orig), (void*)(orig.raw_()), _rep_(patch),
                     (void*)(patch.raw_()));
          value[i] = translate::make_from_object<SC>(patch);
        }
      }
    } break;
    }
  };

  template <typename OT> void field(Symbol_sp name, gctools::SmallOrderedSet<gc::smart_ptr<OT>>& value) {
    RECORD_LOG(("field(Symbol_sp name, gctools::Vec0<gc::smart_ptr<OT>>& value ) name: %s"), _rep_(name));
    switch (this->stage()) {
    case saving: {
      Vector_sp vec_value = core__make_vector(cl::_sym_T_O, value.size());
      size_t idx(0);
      for (auto it : value)
        vec_value->rowMajorAset(idx++, it);
      RECORD_LOG(("saving entry: %s"), _rep_(vec_value));
      Cons_sp apair = core::Cons_O::create(name, vec_value);
      this->_alist = core::Cons_O::create(apair, this->_alist);
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (!find.consp())
        SIMPLE_ERROR("Could not find field {}", _rep_(name));
      Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
      RECORD_LOG(("loading find: %s"), _rep_(apair));
      Vector_sp vec_value = gc::As<Vector_sp>(CONS_CDR(apair));
      RECORD_LOG(("vec_value: %s"), _rep_(vec_value));
      value.resize(cl__length(vec_value));
      for (size_t i(0), iEnd(cl__length(vec_value)); i < iEnd; ++i) {
        T_sp val = vec_value->rowMajorAref(i);
        RECORD_LOG(("Loading vec0[%d] new@%p: %s\n"), i, (void*)(val.raw_()), _rep_(val));
        value[i].rawRef_() = reinterpret_cast<OT*>(val.raw_());
      }
      if (this->stage() == initializing)
        this->flagSeen(apair);
    } break;
    case patching: {
      RECORD_LOG(("Patching"));
      for (size_t i(0), iEnd(value.size()); i < iEnd; ++i) {
        gc::smart_ptr<T_O> orig((gc::Tagged)value[i].raw_());
        T_sp patch = record_circle_subst(this->asSmartPtr(), orig);
        if (patch != orig) {
          RECORD_LOG(("Patching vec0[%d] orig@%p: %s --> new@%p: %s\n"), i, _rep_(orig), (void*)(orig.raw_()), _rep_(patch),
                     (void*)(patch.raw_()));
          value[i].rawRef_() = reinterpret_cast<OT*>(patch.raw_());
        }
      }
    } break;
    }
  };

  template <typename K, typename SV> void field(Symbol_sp name, gctools::SmallMap<K, gctools::smart_ptr<SV>>& value) {
    RECORD_LOG(("field(Symbol_sp name, gctools::SmallMap<K,gctools::smart_ptr<SV>>& value ) name: %s"), _rep_(name));
    switch (this->stage()) {
    case saving: {
      Vector_sp vec_value = core__make_vector(cl::_sym_T_O, value.size());
      size_t idx(0);
      for (auto it : value)
        vec_value->rowMajorAset(idx++, Cons_O::create(translate::to_object<K>::convert(it.first), it.second));
      RECORD_LOG(("saving entry: %s"), _rep_(vec_value));
      Cons_sp apair = core::Cons_O::create(name, vec_value);
      this->_alist = core::Cons_O::create(apair, this->_alist);
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (!find.consp())
        SIMPLE_ERROR("Could not find field {}", _rep_(name));
      Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
      RECORD_LOG(("loading find: %s"), _rep_(apair));
      Vector_sp vec_value = gc::As<Vector_sp>(CONS_CDR(apair));
      RECORD_LOG(("vec_value: %s"), _rep_(vec_value));
      value.clear();
      for (size_t i(0), iEnd(cl__length(vec_value)); i < iEnd; ++i) {
        T_sp val = vec_value->rowMajorAref(i);
        RECORD_LOG(("Loading vec0[%d] new@%p: %s\n"), i, (void*)(val.raw_()), _rep_(val));
        value.push_back(std::make_pair<K, gctools::smart_ptr<SV>>(translate::make_from_object<K>(oCar(val)),
                                                                  gc::As_unsafe<gctools::smart_ptr<SV>>(oCdr(val))));
      }
      if (this->stage() == initializing)
        this->flagSeen(apair);
    } break;
    case patching: {
      RECORD_LOG(("Patching"));
      [[maybe_unused]] size_t i = 0;
      for (auto&& pairi : value) {
        gc::smart_ptr<T_O> orig = pairi.second;
        T_sp patch = record_circle_subst(this->asSmartPtr(), orig);
        if (patch != orig) {
          RECORD_LOG(("Patching vec0[%d] orig@%p: %s --> new@%p: %s\n"), i, (void*)(orig.raw_()), _rep_(orig),
                     (void*)(patch.raw_()), _rep_(patch));
          pairi.second = gc::As_unsafe<gctools::smart_ptr<SV>>(patch);
        }
        ++i;
      }
    } break;
    }
  };

  template <typename SK, typename SV>
  void field(Symbol_sp name, gctools::SmallMap<gctools::smart_ptr<SK>, gctools::smart_ptr<SV>>& value) {
    RECORD_LOG(("field(Symbol_sp name, gctools::SmallMap<gctools::smart_ptr<SK>,gctools::smart_ptr<SV>> ) name: %s"), _rep_(name));
    switch (this->stage()) {
    case saving: {
      Vector_sp vec_value = core__make_vector(cl::_sym_T_O, value.size());
      size_t idx(0);
      for (auto it : value)
        vec_value->rowMajorAset(idx++, Cons_O::create(it.first, it.second));
      RECORD_LOG(("saving entry: %s"), _rep_(vec_value));
      Cons_sp apair = core::Cons_O::create(name, vec_value);
      this->_alist = core::Cons_O::create(apair, this->_alist);
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (!find.consp())
        SIMPLE_ERROR("Could not find field {}", _rep_(name));
      Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
      RECORD_LOG(("loading find: %s"), _rep_(apair));
      Vector_sp vec_value = gc::As<Vector_sp>(CONS_CDR(apair));
      RECORD_LOG(("vec_value: %s"), _rep_(vec_value));
      value.clear();
      for (size_t i(0), iEnd(cl__length(vec_value)); i < iEnd; ++i) {
        T_sp val = vec_value->rowMajorAref(i);
        RECORD_LOG(("Loading vec0[%d] new@%p: %s\n"), i, (void*)(val.raw_()), _rep_(val));
        value.push_back(std::make_pair<gctools::smart_ptr<SK>, gctools::smart_ptr<SV>>(
            gc::As_unsafe<gctools::smart_ptr<SK>>(oCar(val)), gc::As_unsafe<gctools::smart_ptr<SV>>(oCdr(val))));
      }
      if (this->stage() == initializing)
        this->flagSeen(apair);
    } break;
    case patching: {
      RECORD_LOG(("Patching"));
      for (auto&& pairi : value) {
        gc::smart_ptr<T_O> orig_key = pairi.first;
        gc::smart_ptr<T_O> orig_value = pairi.second;
        T_sp patch_key = record_circle_subst(this->asSmartPtr(), orig_key);
        T_sp patch_value = record_circle_subst(this->asSmartPtr(), orig_value);
        if (patch_key != orig_key)
          pairi.first = gc::As_unsafe<gctools::smart_ptr<SK>>(patch_key);
        if (patch_value != orig_value)
          pairi.second = gc::As_unsafe<gctools::smart_ptr<SV>>(patch_value);
      }
    } break;
    }
  };

  template <typename SK, typename SV, typename CMP>
  void field(Symbol_sp name, gctools::SmallMultimap<gctools::smart_ptr<SK>, gctools::smart_ptr<SV>, CMP>& value) {
    RECORD_LOG(("field(Symbol_sp name, gctools::SmallMultimap<gctools::smart_ptr<SK>,gctools::smart_ptr<SV>> ) name: %s"),
               _rep_(name));
    switch (this->stage()) {
    case saving: {
      Vector_sp vec_value = core__make_vector(cl::_sym_T_O, value.size());
      size_t idx(0);
      for (auto it : value)
        vec_value->rowMajorAset(idx++, Cons_O::create(it.first, it.second));
      RECORD_LOG(("saving entry: %s"), _rep_(vec_value));
      Cons_sp apair = core::Cons_O::create(name, vec_value);
      this->_alist = core::Cons_O::create(apair, this->_alist);
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (!find.consp())
        SIMPLE_ERROR("Could not find field {}", _rep_(name));
      Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
      RECORD_LOG(("loading find: %s"), _rep_(apair));
      Vector_sp vec_value = gc::As<Vector_sp>(CONS_CDR(apair));
      RECORD_LOG(("vec_value: %s"), _rep_(vec_value));
      value.clear();
      for (size_t i(0), iEnd(cl__length(vec_value)); i < iEnd; ++i) {
        T_sp val = vec_value->rowMajorAref(i);
        RECORD_LOG(("Loading vec0[%d] new@%p: %s\n"), i, (void*)(val.raw_()), _rep_(val));
        value.push_back(std::make_pair<gctools::smart_ptr<SK>, gctools::smart_ptr<SV>>(
            gc::As_unsafe<gctools::smart_ptr<SK>>(oCar(val)), gc::As_unsafe<gctools::smart_ptr<SV>>(oCdr(val))));
      }
      if (this->stage() == initializing)
        this->flagSeen(apair);
    } break;
    case patching: {
      RECORD_LOG(("Patching"));
      for (auto&& pairi : value) {
        gc::smart_ptr<T_O> orig_key = pairi.first;
        gc::smart_ptr<T_O> orig_value = pairi.second;
        T_sp patch_key = record_circle_subst(this->asSmartPtr(), orig_key);
        T_sp patch_value = record_circle_subst(this->asSmartPtr(), orig_value);
        if (patch_key != orig_key)
          pairi.first = gc::As_unsafe<gctools::smart_ptr<SK>>(patch_key);
        if (patch_value != orig_value)
          pairi.second = gc::As_unsafe<gctools::smart_ptr<SV>>(patch_value);
      }
    } break;
    }
  };

  template <typename SK, typename SV, typename CMP>
  void field(Symbol_sp name, gctools::SmallMultimap_uncopyable<gctools::smart_ptr<SK>, gctools::smart_ptr<SV>, CMP>& value) {
    RECORD_LOG("field(Symbol_sp name, gctools::SmallMultimap<gctools::smart_ptr<SK>,gctools::smart_ptr<SV>> ) name: {}",
               _rep_(name));
    switch (this->stage()) {
    case saving: {
      Vector_sp vec_value = core__make_vector(cl::_sym_T_O, value.size());
      size_t idx(0);
      for (auto it : value)
        vec_value->rowMajorAset(idx++, Cons_O::create(it.first, it.second));
      RECORD_LOG("saving entry: {}", _rep_(vec_value));
      Cons_sp apair = core::Cons_O::create(name, vec_value);
      this->_alist = core::Cons_O::create(apair, this->_alist);
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (!find.consp())
        SIMPLE_ERROR("Could not find field {}", _rep_(name));
      Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
      RECORD_LOG("loading find: {}", _rep_(apair));
      Vector_sp vec_value = gc::As<Vector_sp>(CONS_CDR(apair));
      RECORD_LOG("vec_value: {}", _rep_(vec_value));
      value.clear();
      for (size_t i(0), iEnd(cl__length(vec_value)); i < iEnd; ++i) {
        T_sp val = vec_value->rowMajorAref(i);
        RECORD_LOG("Loading vec0[{}] new@{}: {}\n", i, (void*)(val.raw_()), _rep_(val));
        value.push_back(std::make_pair<gctools::smart_ptr<SK>, gctools::smart_ptr<SV>>(
            gc::As_unsafe<gctools::smart_ptr<SK>>(oCar(val)), gc::As_unsafe<gctools::smart_ptr<SV>>(oCdr(val))));
      }
      if (this->stage() == initializing)
        this->flagSeen(apair);
    } break;
    case patching: {
      RECORD_LOG("Patching");
      for (auto&& pairi : value) {
        gc::smart_ptr<T_O> orig_key = pairi.first;
        gc::smart_ptr<T_O> orig_value = pairi.second;
        T_sp patch_key = record_circle_subst(this->asSmartPtr(), orig_key);
        T_sp patch_value = record_circle_subst(this->asSmartPtr(), orig_value);
        if (patch_key != orig_key)
          pairi.first = gc::As_unsafe<gctools::smart_ptr<SK>>(patch_key);
        if (patch_value != orig_value)
          pairi.second = gc::As_unsafe<gctools::smart_ptr<SV>>(patch_value);
      }
    } break;
    }
  };

  template <typename OT> void field_if_not_empty(Symbol_sp name, gctools::Vec0<gc::smart_ptr<OT>>& value) {
    switch (this->stage()) {
    case saving: {
      if (value.size() != 0)
        this->field(name, value);
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (find.consp()) {
        this->field(name, value);
        Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
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

  template <typename SC> void field_if_not_empty(Symbol_sp name, gctools::Vec0<SC>& value) {
    switch (this->stage()) {
    case saving: {
      if (value.size() != 0)
        this->field(name, value);
    } break;
    case initializing:
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (find.consp()) {
        this->field(name, value);
        Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
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

  template <typename OT> void field_if_not_nil(Symbol_sp name, gc::smart_ptr<OT>& value) {
    switch (this->stage()) {
    case saving: {
      if (value.notnilp()) {
        Cons_sp apair = Cons_O::create(name, value);
        this->_alist = Cons_O::create(apair, this->_alist);
      }
    } break;
    case initializing: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (!find.consp())
        value = nil<core::T_O>();
      else {
        Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
        value = gc::As<gc::smart_ptr<OT>>(CONS_CDR(apair));
        this->flagSeen(apair);
      }
    } break;
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (!find.consp())
        value = nil<core::T_O>();
      else {
        Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
        // When loading the object oCdr(apair) may not be of the
        // same type as value - it may be a symbol - used for patching
        // use As_unsafe for this.
        value = gc::As_unsafe<gc::smart_ptr<OT>>(CONS_CDR(apair));
      }
    } break;
    case patching: {
      if (value.notnilp()) {
        gc::smart_ptr<T_O> orig((gc::Tagged)value.raw_());
        T_sp patch = record_circle_subst(this->asSmartPtr(), orig);
        if (patch != orig)
          value.setRaw_(reinterpret_cast<gc::Tagged>(patch.raw_()));
      }
    } break;
    }
  }

  template <typename OT> void field_if_not_unbound(Symbol_sp name, gc::smart_ptr<OT>& value) {
    switch (this->stage()) {
    case saving: {
      if (!value.unboundp()) {
        Cons_sp apair = Cons_O::create(name, value);
        this->_alist = Cons_O::create(apair, this->_alist);
      }
    } break;
    case initializing: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (!find.consp())
        value = unbound<OT>();
      else {
        Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
        value = gc::As<gc::smart_ptr<OT>>(CONS_CDR(apair));
        if (!gc::IsA<gc::smart_ptr<OT>>(value)) {
          class_id expected_typ = reg::registered_class<OT>::id;
          lisp_errorBadCastFromT_O(expected_typ, reinterpret_cast<core::T_O*>(value.raw_()));
        }
        this->flagSeen(apair);
      }
    } break;
    case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (!find.consp())
        value = unbound<OT>();
      else {
        Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
        // When loading the object oCdr(apair) may not be of the
        // same type as value - it may be a symbol - used for patching
        // use As_unsafe for this.
        value = gc::As_unsafe<gc::smart_ptr<OT>>(CONS_CDR(apair));
      }
    } break;
    case patching: {
      if (!value.unboundp()) {
        gc::smart_ptr<T_O> orig((gc::Tagged)value.raw_());
        T_sp patch = record_circle_subst(this->asSmartPtr(), orig);
        if (patch != orig)
          value.setRaw_(reinterpret_cast<gc::Tagged>(patch.raw_()));
      }
    } break;
    }
  }

  template <typename OT> void field_if_not_nil(Symbol_sp name, gc::Nilable<gc::smart_ptr<OT>>& value) {
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
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (!find.consp())
        value = nil<core::T_O>();
      else {
        Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
        if (CONS_CDR(apair).nilp()) {
          value = nil<T_O>();
        } else {
          value = gc::As_unsafe<gc::smart_ptr<OT>>(CONS_CDR(apair));
        }
        if (this->stage() == initializing)
          this->flagSeen(apair);
      }
    } break;
    case patching: {
      if (value.notnilp()) {
        gc::smart_ptr<T_O> orig((gc::Tagged)value.raw_());
        T_sp patch = record_circle_subst(this->asSmartPtr(), orig);
        if (patch != orig)
          value.rawRef_() = reinterpret_cast<OT*>(patch.raw_());
      }
    } break;
    }
  }

  template <typename T> void field_if_not_default(Symbol_sp name, T& value, const T& default_value) {
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
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (!find.consp()) {
        value = default_value;
      } else {
        Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
        value = translate::make_from_object<T>(CONS_CDR(apair));
        if (this->stage() == initializing)
          this->flagSeen(apair);
      }
    } break;
    case patching:
      // Do nothing for POD values
      break;
    }
  }

  template <typename T> void field_if_defined(Symbol_sp name, bool& defined, T& value) {
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
      List_sp find = core__alist_assoc_eq(this->_alist, name);
      if (!find.consp()) {
        defined = false;
      } else {
        defined = true;
        Cons_sp apair = gc::As_unsafe<Cons_sp>(find);
        value = translate::make_from_object<T>(CONS_CDR(apair));
        if (this->stage() == initializing)
          this->flagSeen(apair);
      }
    } break;
    case patching:
      // Do nothing for POD values
      break;
    }
  }

  // The Common Lisp exposed method
  T_sp field_read(Symbol_sp name);
  void field_write(Symbol_sp name, T_sp object);
  T_sp field_patch(Symbol_sp name, T_sp object);

  Symbol_sp record_stage() const;
};
}; // namespace core
