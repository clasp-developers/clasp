#pragma once

namespace core {
FORWARD(Creator);
FORWARD(InstanceCreator);
FORWARD(FuncallableInstanceCreator);
}; // namespace core

template <> struct gctools::GCInfo<core::Creator_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

template <> struct gctools::GCInfo<core::InstanceCreator_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

template <> struct gctools::GCInfo<core::FuncallableInstanceCreator_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

class Creator_O : public Function_O {
  LISP_ABSTRACT_CLASS(core, CorePkg, Creator_O, "Creator", Function_O);

public:
  // Some Creators don't actually allocate anything -
  // classes that don't have default allocators
  virtual bool allocates() const { return true; };
  /*! If this is the allocator for a primary CxxAdapter class then return true, */
  T_sp functionName() const override { return nil<T_O>(); };
  T_sp lambda_list() const { return nil<T_O>(); };
  T_sp setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column) { return nil<T_O>(); };
  virtual int duplicationLevel() const { return 0; };
  virtual bool creates_classes() const { return false; };
  CL_NAME("CORE:CREATOR-TEMPLATED-SIZE");
  virtual CL_DEFMETHOD size_t templatedSizeof() const override = 0;
  virtual Creator_sp duplicateForClassName(core::Symbol_sp className) {
    printf("Subclass must implement Creator::duplicateForClassName\n");
    abort();
  };
  virtual core::T_sp creator_allocate() = 0;
  // use this when we inherit from Function_O
  static inline LCC_RETURN LISP_CALLING_CONVENTION() {
    LCC_RETURN v;
    return v;
  }
  template <typename... Ts>
  static inline LCC_RETURN entry_point_fixed(T_O* lcc_closure, Ts... args) {
    LCC_RETURN v;
    return v;
  }

  Creator_O(SimpleFun_sp ep) : Base(ep){};
};

template <class _W_> class WRAPPER_BuiltInObjectCreator : public core::Creator_O {
public:
  typedef core::Creator_O TemplatedBase;

public:
  size_t templatedSizeof() const { return sizeof(WRAPPER_BuiltInObjectCreator<_W_>); };
  virtual core::T_sp creator_allocate() {
    auto obj = gctools::GC<_W_>::allocate_with_default_constructor();
    return obj;
  }
  virtual void searcher(){};
  WRAPPER_BuiltInObjectCreator(core::SimpleFun_sp ep) : core::Creator_O(ep){};
};

}; // namespace core

template <typename T> class gctools::GCStamp<core::WRAPPER_BuiltInObjectCreator<T>> {
public:
  static gctools::GCStampEnum const StampWtag =
      gctools::GCStamp<typename core::WRAPPER_BuiltInObjectCreator<T>::TemplatedBase>::StampWtag;
};
template <typename T> struct gctools::GCInfo<core::WRAPPER_BuiltInObjectCreator<T>> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
class InstanceCreator_O : public Creator_O {
  LISP_CLASS(core, CorePkg, InstanceCreator_O, "InstanceCreator", Creator_O);

public:
  Instance_sp _class;

public:
  InstanceCreator_O(SimpleFun_sp ep, Instance_sp class_) : Base(ep), _class(class_){};
  T_sp creator_allocate() override;
  virtual size_t templatedSizeof() const override { return sizeof(InstanceCreator_O); };
};
}; // namespace core

namespace core {
class FuncallableInstanceCreator_O : public Creator_O {
  LISP_CLASS(core, CorePkg, FuncallableInstanceCreator_O, "FuncallableInstanceCreator", Creator_O);

public:
  Instance_sp _class;

public:
  FuncallableInstanceCreator_O(SimpleFun_sp ep, Instance_sp class_) : Base(ep), _class(class_){};
  T_sp creator_allocate() override;
  virtual size_t templatedSizeof() const override { return sizeof(FuncallableInstanceCreator_O); };
};
}; // namespace core

namespace core {
class StandardClassCreator_O : public Creator_O {
  LISP_CLASS(core, CorePkg, StandardClassCreator_O, "StandardClassCreator", Creator_O);

public:
  StandardClassCreator_O(SimpleFun_sp ep) : Base(ep){};
  T_sp creator_allocate() override;
  virtual size_t templatedSizeof() const override { return sizeof(StandardClassCreator_O); };
};
}; // namespace core

namespace core {
class DerivableCxxClassCreator_O : public Creator_O {
  LISP_CLASS(core, CorePkg, DerivableCxxClassCreator_O, "DerivableCxxClassCreator", Creator_O);

public:
  DerivableCxxClassCreator_O(SimpleFun_sp ep) : Base(ep){};
  T_sp creator_allocate() override;
  virtual size_t templatedSizeof() const override { return sizeof(DerivableCxxClassCreator_O); };
};
}; // namespace core

namespace core {
class ClassRepCreator_O : public Creator_O {
  LISP_CLASS(core, CorePkg, ClassRepCreator_O, "ClassRepCreator", Creator_O);

public:
  ClassRepCreator_O(SimpleFun_sp ep) : Base(ep){};
  T_sp creator_allocate() override;
  virtual size_t templatedSizeof() const override { return sizeof(ClassRepCreator_O); };
};
}; // namespace core
