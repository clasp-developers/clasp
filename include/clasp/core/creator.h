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

  class Creator_O : public Function_O {
    LISP_ABSTRACT_CLASS(core,CorePkg,Creator_O,"Creator",Function_O);
  public:
    CLASP_DEFAULT_CTOR Creator_O() {};
  public:
  // Some Creators don't actually allocate anything -
  // classes that don't have default allocators
    virtual bool allocates() const { return true; };
  /*! If this is the allocator for a primary CxxAdapter class then return true, */
    T_sp functionName() const override { return nil<T_O>(); };
    T_sp closedEnvironment() const override { return nil<T_O>(); };
    T_sp lambdaListHandler() const override { return nil<T_O>(); };
    T_sp lambda_list() const { return nil<T_O>(); };
    T_sp setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column ) {return nil<T_O>();};
    virtual int duplicationLevel() const { return 0; };
    virtual bool creates_classes() const { return false; };
    CL_NAME("CORE:CREATOR-TEMPLATED-SIZE");
    virtual CL_DEFMETHOD size_t templatedSizeof() const override= 0;
    virtual Creator_sp duplicateForClassName(core::Symbol_sp className) {
      printf("Subclass must implement Creator::duplicateForClassName\n");
      abort();
    };
    virtual core::T_sp creator_allocate() = 0;
    // use this when we inherit from Function_O
    static inline LCC_RETURN LISP_CALLING_CONVENTION()
    {
      LCC_RETURN v;
      return v;
    }
    static inline LISP_ENTRY_0() {
      return entry_point_n(lcc_closure,0,NULL);
    }
    static inline LISP_ENTRY_1() {
      core::T_O* args[1] = {lcc_farg0};
      return entry_point_n(lcc_closure,1,args);
    }
    static inline LISP_ENTRY_2() {
      core::T_O* args[2] = {lcc_farg0,lcc_farg1};
      return entry_point_n(lcc_closure,2,args);
    }
    static inline LISP_ENTRY_3() {
      core::T_O* args[3] = {lcc_farg0,lcc_farg1,lcc_farg2};
      return entry_point_n(lcc_closure,3,args);
    }
    static inline LISP_ENTRY_4() {
      core::T_O* args[4] = {lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3};
      return entry_point_n(lcc_closure,4,args);
    }
    static inline LISP_ENTRY_5() {
      core::T_O* args[5] = {lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3,lcc_farg4};
      return entry_point_n(lcc_closure,5,args);
    }

    Creator_O(GlobalEntryPoint_sp fdesc) : Base(fdesc) {};
//    Creator_O() : Function_O(makeFunctionDescription(_Nil<T_O>(),entry_point)) {};
    virtual ~Creator_O() {};
  };


  template <class _W_>
    class TEMPLATED_FUNCTION_BuiltInObjectCreator : public core::Creator_O {
  public:
    typedef core::Creator_O TemplatedBase;
  public:
    size_t templatedSizeof() const { return sizeof(TEMPLATED_FUNCTION_BuiltInObjectCreator<_W_>); };
    virtual core::T_sp creator_allocate() {
      auto  obj = gctools::GC<_W_>::allocate_with_default_constructor();
      return obj;
    }
    virtual void searcher(){};
    TEMPLATED_FUNCTION_BuiltInObjectCreator(core::GlobalEntryPoint_sp fdesc) : core::Creator_O(fdesc) {};
  };



};

template <typename T>
class gctools::GCStamp<core::TEMPLATED_FUNCTION_BuiltInObjectCreator<T>> {
 public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename core::TEMPLATED_FUNCTION_BuiltInObjectCreator<T>::TemplatedBase>::Stamp;
};
template <typename T>
struct gctools::GCInfo<core::TEMPLATED_FUNCTION_BuiltInObjectCreator<T>> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
  class InstanceCreator_O : public Creator_O {
    LISP_CLASS(core,CorePkg,InstanceCreator_O,"InstanceCreator",Creator_O);
  public:
    Instance_sp _class;
  public:
    InstanceCreator_O(GlobalEntryPoint_sp fdesc, Instance_sp class_) : Base(fdesc), _class(class_){};
    T_sp creator_allocate() override;
    virtual size_t templatedSizeof() const override { return sizeof(InstanceCreator_O); };
  };
};

namespace core {
  class FuncallableInstanceCreator_O : public Creator_O {
    LISP_CLASS(core,CorePkg,FuncallableInstanceCreator_O,"FuncallableInstanceCreator",Creator_O);
  public:
    Instance_sp _class;
  public:
  FuncallableInstanceCreator_O(GlobalEntryPoint_sp fdesc, Instance_sp class_) : Base(fdesc), _class(class_){};
    T_sp creator_allocate() override;
    virtual size_t templatedSizeof() const override { return sizeof(FuncallableInstanceCreator_O); };
  };
};

namespace core {
  class StandardClassCreator_O : public Creator_O {
    LISP_CLASS(core,CorePkg,StandardClassCreator_O,"StandardClassCreator",Creator_O);
  public:
  StandardClassCreator_O(GlobalEntryPoint_sp fdesc) : Base(fdesc) {};
    T_sp creator_allocate() override;
    virtual size_t templatedSizeof() const override { return sizeof(StandardClassCreator_O); };
  };
};

namespace core {
  class DerivableCxxClassCreator_O : public Creator_O {
    LISP_CLASS(core,CorePkg,DerivableCxxClassCreator_O,"DerivableCxxClassCreator",Creator_O);
  public:
  DerivableCxxClassCreator_O(GlobalEntryPoint_sp fdesc) : Base(fdesc) {};
    T_sp creator_allocate() override;
    virtual size_t templatedSizeof() const override { return sizeof(DerivableCxxClassCreator_O); };
  };
};

namespace core {
  class ClassRepCreator_O : public Creator_O {
    LISP_CLASS(core,CorePkg,ClassRepCreator_O,"ClassRepCreator",Creator_O);
  public:
  ClassRepCreator_O(GlobalEntryPoint_sp fdesc) : Base(fdesc) {};
    T_sp creator_allocate() override;
    virtual size_t templatedSizeof() const override { return sizeof(ClassRepCreator_O); };
  };
};


#endif // ifndef creator_h
