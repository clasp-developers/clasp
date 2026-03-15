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

class Creator_O : public General_O {
  LISP_ABSTRACT_CLASS(core, CorePkg, Creator_O, "Creator", General_O);

public:
  virtual int duplicationLevel() const { return 0; };
  CL_NAME("CORE:CREATOR-TEMPLATED-SIZE");
  virtual CL_DEFMETHOD size_t templatedSizeof() const override = 0;
  virtual Creator_sp duplicateForClassName(core::Symbol_sp className) {
    printf("Subclass must implement Creator::duplicateForClassName\n");
    abort();
  };
  virtual core::T_sp creator_allocate() = 0;
};

template <class _W_> class WRAPPER_BuiltInObjectCreator : public core::Creator_O {
public:
  typedef core::Creator_O TemplatedBase;

public:
  size_t templatedSizeof() const { return sizeof(WRAPPER_BuiltInObjectCreator<_W_>); };
  virtual core::T_sp creator_allocate() {
    if constexpr(std::is_default_constructible_v<_W_>)
      return gctools::GC<_W_>::allocate();
    else
      lisp_errorCannotAllocateInstanceWithMissingDefaultConstructor(_W_::static_classSymbol());
  }
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
  InstanceCreator_O(Instance_sp class_) : Base(), _class(class_){};
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
  FuncallableInstanceCreator_O(Instance_sp class_) : Base(), _class(class_){};
  T_sp creator_allocate() override;
  virtual size_t templatedSizeof() const override { return sizeof(FuncallableInstanceCreator_O); };
};
}; // namespace core

namespace core {
class StandardClassCreator_O : public Creator_O {
  LISP_CLASS(core, CorePkg, StandardClassCreator_O, "StandardClassCreator", Creator_O);

public:
  T_sp creator_allocate() override;
  virtual size_t templatedSizeof() const override { return sizeof(StandardClassCreator_O); };
};
}; // namespace core

namespace core {
class DerivableCxxClassCreator_O : public Creator_O {
  LISP_CLASS(core, CorePkg, DerivableCxxClassCreator_O, "DerivableCxxClassCreator", Creator_O);

public:
  T_sp creator_allocate() override;
  virtual size_t templatedSizeof() const override { return sizeof(DerivableCxxClassCreator_O); };
};
}; // namespace core

namespace core {
class ClassRepCreator_O : public Creator_O {
  LISP_CLASS(core, CorePkg, ClassRepCreator_O, "ClassRepCreator", Creator_O);

public:
  T_sp creator_allocate() override;
  virtual size_t templatedSizeof() const override { return sizeof(ClassRepCreator_O); };
};
}; // namespace core
