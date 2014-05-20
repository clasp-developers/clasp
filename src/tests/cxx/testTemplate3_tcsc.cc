#include <iostream>
#include <string>
#include <sstream>

using namespace std;
 
#define STR_MAX 128
 
#define ERROR(s) {printf("%s\n",s); exit(1);}
 
struct ObjectStruct;
 
struct Object {
    Object() : px(NULL) {};
    Object(ObjectStruct* p) : px(p) {};
 
    ObjectStruct* operator->() { return this->px;};
    ObjectStruct* px;
    int numValues() { return 1;};
};


struct Object_mv : public Object {
    Object_mv(Object obj, int numValues ) : Object(obj.px), _NumValues(numValues) {};
    int _NumValues;
    int numValues() { return this->_NumValues;};
};
 
 
typedef Object ArgArray[];
 
 
 
class Functoid {
    virtual Object_mv activate(int nargs, ArgArray args)=0;
};
 
 
typedef enum { t_undef, t_int, t_double, t_cons, t_string, t_functoid } kind;
 
struct ObjectStruct {
    ObjectStruct() : _kind(t_undef) {};
    kind  _kind;
    union cons_union {
  cons_union() {};
  int   _int;
  double    _double;
  Functoid* _functoid;
  char    _str[STR_MAX];
  struct {
      Object  _car;
      Object    _cdr;
  } cons;
    } data;
};
 
 
 
 
Object globalMultipleValues[64];
 
 
 
 
 
 
 
 
template <class T>
struct from_object {};
 
template <class T>
struct to_object {};
 
 
 
template <>
struct from_object<int&> {
    int _v;
    from_object(Object o) : _v(0)
    {
  if ( o->_kind == t_int ) {
      this->_v = o->data._int;
  } else if (o->_kind == t_double ) {
      this->_v = o->data._double;
  } else {
      ERROR("Bad type");
  }
    }
};
 
 
template <>
struct from_object<int> {
    int _v;
    from_object(Object o) : _v(0)
    {
  if ( o->_kind == t_int ) {
      this->_v = o->data._int;
  } else if (o->_kind == t_double ) {
      this->_v = o->data._double;
  } else {
      ERROR("Bad type");
  }
    }
};
 
 
template <>
struct to_object<int&> {
    Object operator()(int& x) {
  Object o = new ObjectStruct();
  o->_kind = t_int;
  o->data._int = x;
  return o;
    }
};
 
 
 
Object allocateObject()
{
    Object x(new ObjectStruct);
    return x;
}
 
 
 
 
 
 
 
 
 
template <int...IS>
struct Indices
{
    enum { Size = sizeof...(IS)};
};
 
template <int>
struct Int2Type
{};
 
template <typename Head, typename... Tail, int N, int... TailI>
void fillValuesImpl(Object mv[], Indices<N, TailI...>, Int2Type<N>, Head head, Tail &&...tail)
{
    mv[N] = to_object<Head>::convert(head);
    fillValuesImpl(mv, Indices<TailI...>(), Int2Type<N + 1>(), std::forward<Tail>(tail)...);
}
 
template <typename Head, typename... Tail, int HeadI, int... TailI, int N>
void fillValuesImpl(Object mv[], Indices<HeadI, TailI...>, Int2Type<N>, Head head, Tail &&...tail)
{
  fillValuesImpl(mv, Indices<HeadI, TailI...>(), Int2Type<N + 1>(), std::forward<Tail>(tail)...);
}
 
template <typename Head, typename... Tail, int N>
void fillValuesImpl(Object mv[], Indices<>, Int2Type<N>, Head head, Tail &&...tail)
{
  fillValuesImpl(mv, Indices<>(), Int2Type<N>(), std::forward<Tail>(tail)...);
}
 
template <int N>
void fillValuesImpl(Object mv[], Indices<>, Int2Type<N>)
{}
 
 
 
 
 
template<typename MV, class RT, typename... ARGS>
class FunctionWrapper {
public:
    typedef std::function<RT(ARGS...)>  Type;
    Type        fn;
public:
    FunctionWrapper(Type f) : fn(f) {};
    Object_mv operator()(ARGS &&...args) {
  RT retval = fn(args...);
  globalMultipleValues[0] = to_object<RT>::convert(retval);
  fillValuesImpl(globalMultipleValues,MV(),Int2Type<1>(), std::forward<ARGS>(args)...);
  return Object_mv(globalMultipleValues[0],MV::Size+1);
    }
};
 
 
 
 
template<typename MV, typename... ARGS>
class FunctionWrapper<MV,void,ARGS...> {
public:
    typedef std::function<void(ARGS...)>  Type;
    Type          fn;
public:
    FunctionWrapper(Type f) : fn(f) {};
    Object_mv operator()(ARGS...args) {
  fn(args...);
  globalMultipleValues[0] = Object();
  globalMultipleValues[1] = Object();
  fillValuesImpl(globalMultipleValues,MV(),Int2Type<1>(),args...);
  return Object_mv(globalMultipleValues[1],MV::Size);
    }
};
 
 
 
 
 
template<typename MV,typename RT,class... ARGS>
class VariadicFunctoid : public Functoid{
public:
    typedef FunctionWrapper<MV,RT,ARGS...>    FunctionWrapperType;
    typedef typename FunctionWrapper<MV,RT,ARGS...>::Type   Type;
    FunctionWrapperType         fptr;
public:
    enum { NumParams = sizeof...(ARGS) };
    VariadicFunctoid(const string& name, Type ptr) : fptr(ptr) {};
    
    Object_mv activate( int nargs, ArgArray args )
    {
  if ( nargs != sizeof...(ARGS) )
  {
      stringstream ss;
      ss << "Function expected "<< (sizeof...(ARGS)) << " argument(s) but was passed " << nargs << " argument(s).";
      ERROR(ss.str().c_str());
  }
  int arg_idx(0);
  return fptr(from_object<ARGS>(args[arg_idx++])._v...);
    }
    
};
 
 
 
 
Object globalFunctionBindings;
 
 
 
 
template <typename MV=Indices<>, typename RT, typename... ARGS>
void def_(const string& name, RT (*fp)(ARGS...) )
{
    Functoid* f = new VariadicFunctoid<MV,RT,ARGS...>(name,fp);
    Object funcName = allocateObject();
    funcName->_kind = t_string;
    strcpy(funcName->data._str,name.c_str());
    Object func = allocateObject();
    func->_kind = t_functoid;
    func->data._functoid = f;
    Object binding = allocateObject();
    binding->_kind = t_cons;
    binding->data.cons._car = funcName;
    binding->data.cons._cdr = func;
    Object entry = allocateObject();
    entry->_kind = t_cons;
    entry->data.cons._car = binding;
    entry->data.cons._cdr = globalFunctionBindings;
    globalFunctionBindings = entry;
}
 
 
 
int upDown(int x, int& up)
{
    up = x+1;
    return x-1;
}
 
 
 
int main(int argc, char* argv[])
{
    def_("upDown",&upDown);
}
