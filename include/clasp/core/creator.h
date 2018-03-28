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
    LISP_CLASS(core,CorePkg,Creator_O,"Creator",Function_O);
  public:
  // Some Creators don't actually allocate anything -
  // classes that don't have default allocators
    virtual bool allocates() const { return true; };
  /*! If this is the allocator for a primary CxxAdapter class then return true, */
    T_sp functionName() const { return _Nil<T_O>(); };
    T_sp closedEnvironment() const { return _Nil<T_O>(); };
    T_sp cleavir_ast() const { return _Nil<T_O>(); };
    T_sp docstring() const { return _Nil<T_O>(); };
    Symbol_sp getKind() const { return _Nil<Symbol_O>(); };
    int sourceFileInfoHandle() const { return 0; };
    bool macroP() const { return false; };
    void set_kind(Symbol_sp k) { };
    void setf_lambda_list(List_sp ll) {};
    void setf_cleavir_ast(T_sp v) {};
    List_sp declares() const { return _Nil<T_O>(); };
    LambdaListHandler_sp lambdaListHandler() const { return _Nil<LambdaListHandler_O>(); };
    T_sp lambda_list() const { return _Nil<T_O>(); };
    T_sp setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column ) {return _Nil<T_O>();};
    virtual int duplicationLevel() const { return 0; };
    virtual bool creates_classes() const { return false; };
    CL_NAME("CORE:CREATOR-TEMPLATED-SIZE");
    virtual CL_DEFMETHOD size_t templatedSizeof() const = 0;
    virtual Creator_sp duplicateForClassName(core::Symbol_sp className) {
      printf("Subclass must implement Creator::duplicateForClassName\n");
      abort();
    };
    virtual core::T_sp creator_allocate() = 0;
#if 1
    // use this when we inherit from Function_O
    static LCC_RETURN LISP_CALLING_CONVENTION();
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
    size_t templatedSizeof() const { return sizeof(BuiltInObjectCreator<_W_>); };
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
  static gctools::GCStampEnum const Stamp = gctools::GCStamp<typename core::BuiltInObjectCreator<T>::TemplatedBase>::Stamp;
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
    T_sp creator_allocate();
    virtual size_t templatedSizeof() const { return sizeof(FuncallableInstanceCreator_O); };
  };
};

namespace core {
  class StandardClassCreator_O : public Creator_O {
    LISP_CLASS(core,CorePkg,StandardClassCreator_O,"StandardClassCreator",Creator_O);
  public:
    StandardClassCreator_O() {};
    T_sp creator_allocate();
    virtual size_t templatedSizeof() const { return sizeof(StandardClassCreator_O); };
  };
};

namespace core {
  class DerivableCxxClassCreator_O : public Creator_O {
    LISP_CLASS(core,CorePkg,DerivableCxxClassCreator_O,"DerivableCxxClassCreator",Creator_O);
  public:
    DerivableCxxClassCreator_O() {};
    T_sp creator_allocate();
    virtual size_t templatedSizeof() const { return sizeof(DerivableCxxClassCreator_O); };
  };
};

namespace core {
  class ClassRepCreator_O : public Creator_O {
    LISP_CLASS(core,CorePkg,ClassRepCreator_O,"ClassRepCreator",Creator_O);
  public:
    ClassRepCreator_O() {};
    T_sp creator_allocate();
    virtual size_t templatedSizeof() const { return sizeof(ClassRepCreator_O); };
  };
};


#endif // ifndef creator_h
