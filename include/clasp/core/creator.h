#ifndef creator_h
#define creator_h


namespace core {
  FORWARD(Creator);
  FORWARD(InstanceCreator);
  FORWARD(FuncallableInstanceCreator);
};

template <>
struct gctools::GCInfo<core::Creator_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

template <>
struct gctools::GCInfo<core::InstanceCreator_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

template <>
struct gctools::GCInfo<core::FuncallableInstanceCreator_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};


namespace core {

  class Creator_O : public General_O {
    LISP_CLASS(core,CorePkg,Creator_O,"Creator",General_O);
  public:
  // Some Creators don't actually allocate anything -
  // classes that don't have default allocators
    virtual bool allocates() const { return true; };
  /*! If this is the allocator for a primary CxxAdapter class then return true, */
    virtual int duplicationLevel() const { return 0; };
    virtual bool creates_classes() const { return false; };
    CL_NAME("CORE:CREATOR-TEMPLATED-SIZE");
    CL_DEFMETHOD virtual size_t templatedSizeof() const = 0;
    virtual Creator_sp duplicateForClassName(core::Symbol_sp className) {
      printf("Subclass must implement Creator::duplicateForClassName\n");
      abort();
    };
    CL_NAME("CORE:CREATOR-DESCRIBE");
    CL_DEFMETHOD virtual void describe() const = 0;
    virtual core::T_sp creator_allocate() = 0;
#if 0
    // use this when we inherit from Function_O
    LCC_RETURN LISP_CALLING_CONVENTION();
    // entry_point is LISP_CALLING_CONVENTION() macro
  Creator_O() : Base(entry_point) {};
#endif
    virtual ~Creator_O() {};
  };



  template <class _W_>
    class BuiltInObjectCreator : public core::Creator_O {
  public:
    typedef core::Creator_O TemplatedBase;
  public:
    DISABLE_NEW();
    size_t templatedSizeof() const { return sizeof(BuiltInObjectCreator<_W_>); };
    virtual void describe() const {
      printf("BuiltInObjectCreator for class %s  sizeof_instances-> %zu\n", _rep_(reg::lisp_classSymbol<_W_>()).c_str(), sizeof(_W_));
    }
    virtual core::T_sp creator_allocate() {
      GC_ALLOCATE(_W_, obj);
      return obj;
    }
    virtual void searcher(){};
  };



};

template <typename T>
class gctools::GCStamp<core::BuiltInObjectCreator<T>> {
 public:
  static gctools::GCStampEnum const Stamp = gctools::GCStamp<typename core::BuiltInObjectCreator<T>::TemplatedBase>::Kind;
  static const size_t Flags = 0;
};
template <typename T>
struct gctools::GCInfo<core::BuiltInObjectCreator<T>> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
  class InstanceCreator_O : public Creator_O {
    LISP_CLASS(core,CorePkg,InstanceCreator_O,"InstanceCreator",Creator_O);
  public:
    Class_sp _class;
  public:
  InstanceCreator_O(Class_sp class_) : _class(class_){};
    void describe() const {
      printf("InstanceAllocatorFunctor for class %s\n", _rep_(this->_class).c_str());
    };
    T_sp creator_allocate();
    virtual size_t templatedSizeof() const { return sizeof(InstanceCreator_O); };
  };
};

namespace core {
  class FuncallableInstanceCreator_O : public Creator_O {
    LISP_CLASS(core,CorePkg,FuncallableInstanceCreator_O,"FuncallableInstanceCreator",Creator_O);
  public:
    Class_sp _class;
  public:
  FuncallableInstanceCreator_O(Class_sp class_) : _class(class_){};
    void describe() const {
      printf("InstanceAllocatorFunctor for class %s\n", _rep_(this->_class).c_str());
    };
    T_sp creator_allocate();
    virtual size_t templatedSizeof() const { return sizeof(InstanceCreator_O); };
  };
};

namespace core {
  FORWARD(ClassCreator);
  class ClassCreator_O : public Creator_O {
    LISP_CLASS(core,CorePkg,ClassCreator_O,"ClassCreator",Creator_O);
  public:
    Class_sp _class;
  public:
  ClassCreator_O(Class_sp class_) : _class(class_){};
    void describe() const {
      printf("ClassCreator class %s\n", _rep_(this->_class).c_str());
    };
    T_sp creator_allocate();
    virtual size_t templatedSizeof() const { return sizeof(ClassCreator_O); };
  };
};

namespace core {
  class StandardClassCreator_O : public Creator_O {
    LISP_CLASS(core,CorePkg,StandardClassCreator_O,"StandardClassCreator",Creator_O);
  public:
    StandardClassCreator_O() {};
    void describe() const {
      printf("StandardClassCreator\n");
    };
    T_sp creator_allocate();
    virtual size_t templatedSizeof() const { return sizeof(StandardClassCreator_O); };
  };
};
namespace core {
  class StructureClassCreator_O : public Creator_O {
    LISP_CLASS(core,CorePkg,StructureClassCreator_O,"StructureClassCreator",Creator_O);
  public:
    StructureClassCreator_O() {};
    void describe() const {
      printf("StructureClassCreator\n");
    };
    T_sp creator_allocate();
    virtual size_t templatedSizeof() const { return sizeof(StructureClassCreator_O); };
  };
};


#endif // ifndef creator_h
