/*
    File: testTemplate5_working.cc
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
#include <iostream>
#include <string>
#include <sstream>

using namespace std;

#define STR_MAX 128

struct err {
  err(const string &m) : msg(m){};
  string msg;
};

#define BAD_TYPE(datum, ty)                                             \
  {                                                                     \
    stringstream ss;                                                    \
    ss << "Wrong type " << repr(datum) << " expected " << kindname(ty); \
    throw(err(ss.str()));                                               \
  };
#define ERROR(s)       \
  {                    \
    printf("%s\n", s); \
    exit(1);           \
  }

namespace detail {

template <int N, class... Types>
struct NthParameterType;

template <class First, class... Rest>
struct NthParameterType<0, First, Rest...> {
  typedef First type;
};

template <int N, class First, class... Rest>
struct NthParameterType<N, First, Rest...> {
  typedef typename NthParameterType<N - 1, Rest...>::type type;
};

} // detail

template <int N, class... Types>
using Select_ = typename detail::NthParameterType<N, Types...>::type;

struct ObjectStruct;

struct Object {
  Object() : px(NULL){};
  Object(ObjectStruct *p) : px(p){};

  ObjectStruct *operator->() { return this->px; };
  ObjectStruct *px;
  int numValues() { return 1; };
  bool nilp() const { return this->px == NULL; };
  bool notnilp() const { return !this->nilp(); };
};

struct Object_mv : public Object {
  Object_mv(Object obj, int numValues) : Object(obj.px), _NumValues(numValues){};
  int _NumValues;
  int numValues() { return this->_NumValues; };
};

typedef Object ArgArray[];

class Functoid {
public:
  virtual Object_mv activate(int nargs, ArgArray args) = 0;
};

typedef enum { t_undef,
               t_int,
               t_double,
               t_cons,
               t_string,
               t_functoid,
               t_symbol } kind;

string kindname(kind k) {
  switch (k) {
  case t_undef:
    return "<undef>";
  case t_int:
    return "<int>";
  case t_double:
    return "<double>";
  case t_cons:
    return "<cons>";
  case t_string:
    return "<string>";
  case t_functoid:
    return "<functoid>";
  case t_symbol:
    return "<symbol>";
  default:
    return "<UNKNOWN KIND>";
  };
}

struct ObjectStruct {
  ObjectStruct() : _kind(t_undef){};
  kind _kind;
  union cons_union {
    cons_union(){};
    int _int;
    double _double;
    Functoid *_functoid;
    char _str[STR_MAX];
    struct {
      Object _car;
      Object _cdr;
    } cons;
  } data;
};

Object Nil;
Object globalMultipleValues[64];

template <class T>
struct from_object {};

template <class T>
struct to_object {
};

#if 0 // not necessary
template <>
struct from_object<int&&> {
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
#endif

#if 1
template <>
struct from_object<int> {
  int _v;
  from_object(Object o) : _v(0) {
    if (o->_kind == t_int) {
      this->_v = o->data._int;
    } else if (o->_kind == t_double) {
      this->_v = o->data._double;
    } else {
      ERROR("Bad type");
    }
  }
};
#endif

#if 1
template <>
struct from_object<int &> {
  int _v;
  from_object(Object o) : _v(0) {
    if (o->_kind == t_int) {
      this->_v = o->data._int;
    } else if (o->_kind == t_double) {
      this->_v = o->data._double;
    } else {
      ERROR("Bad type");
    }
  }
};
#endif

#if 0 // not necessary
template <>
struct to_object<int&&> {
    static Object convert(int& x) {
	Object o = new ObjectStruct();
	o->_kind = t_int;
	o->data._int = x;
	return o;
    }
};
#endif

#if 1
template <>
struct to_object<int> {
  static Object convert(int x) {
    Object o = new ObjectStruct();
    o->_kind = t_int;
    o->data._int = x;
    return o;
  }
};
#endif

Object allocateObject() {
  Object x(new ObjectStruct);
  return x;
}

template <int... IS>
struct Indices {
  enum { Size = sizeof...(IS) };
};

template <int>
struct Int2Type {};

template <typename Head, typename... Tail, int N, int... TailI>
void fillValuesImpl(Object mv[], Indices<N, TailI...>, Int2Type<N>, Head head, Tail &&... tail) {
  mv[N] = to_object<Head>::convert(head);
  fillValuesImpl(mv, Indices<TailI...>(), Int2Type<N + 1>(), std::forward<Tail>(tail)...);
}

template <typename Head, typename... Tail, int HeadI, int... TailI, int N>
void fillValuesImpl(Object mv[], Indices<HeadI, TailI...>, Int2Type<N>, Head head, Tail &&... tail) {
  fillValuesImpl(mv, Indices<HeadI, TailI...>(), Int2Type<N + 1>(), std::forward<Tail>(tail)...);
}

template <typename Head, typename... Tail, int N>
void fillValuesImpl(Object mv[], Indices<>, Int2Type<N>, Head head, Tail &&... tail) {
  fillValuesImpl(mv, Indices<>(), Int2Type<N>(), std::forward<Tail>(tail)...);
}

template <int N>
void fillValuesImpl(Object mv[], Indices<>, Int2Type<N>) {}

/* I limited this FunctionWrapper to functions that return values
   I will probably have to write a specializer that specializes on
   void functions */
template <typename MV, class RT, typename... ARGS>
class FunctionWrapper {
public:
  typedef std::function<RT(ARGS...)> Type;
  Type fn;

public:
  FunctionWrapper(Type f) : fn(f){};
  /* It was absolutely necessary to to make operator() a template function so that
   perfect forwarding can work - I cannot use the ARGS parameter pack for the 
   FunctionWrapper class */
  template <typename... FARGS>
  Object_mv operator()(FARGS &&... args) {
    RT retval = fn(std::forward<FARGS>(args)...);
    globalMultipleValues[0] = to_object<RT>::convert(retval);
    fillValuesImpl(globalMultipleValues, MV(), Int2Type<1>(), std::forward<FARGS>(args)...);
    return Object_mv(globalMultipleValues[0], MV::Size + 1);
  }
};

/* NOTE: I had to limit this class to one arity
   - I may need to duplicate the code for a fixed number of arities (ugh) */
template <typename MV, typename RT, class... ARGS>
class VariadicFunctoid : public Functoid {
public:
  typedef FunctionWrapper<MV, RT, ARGS...> FunctionWrapperType;
  typedef typename FunctionWrapper<MV, RT, ARGS...>::Type Type;
  FunctionWrapperType fptr;

public:
  enum { NumParams = sizeof...(ARGS) };
  VariadicFunctoid(const string &name, Type ptr) : fptr(ptr){};

  Object_mv activate(int nargs, ArgArray args) {
    if (nargs != sizeof...(ARGS)) {
      stringstream ss;
      ss << "Function expected " << (sizeof...(ARGS)) << " argument(s) but was passed " << nargs << " argument(s).";
      ERROR(ss.str().c_str());
    }
// int arg_idx(0);
// switch (sizeof...(ARGS)) { // switch on the number of args   PSEUDO-CODE STARTS
// case 3: {
#define TRY 1
#if TRY == 1
    // This code works
    from_object<Select_<0, ARGS...>> a0(args[0]);
    from_object<Select_<1, ARGS...>> a1(args[1]);
    from_object<Select_<2, ARGS...>> a2(args[2]);
    return fptr(a0._v, a1._v, a2._v);
#elif TRY == 2
    // This code does not work -
    // the compiler complains with
    // candidate function not viable expects an l-value for 1st arg
    return fptr(from_object<Select_<0, ARGS...>>(args[0])._v,
                from_object<Select_<1, ARGS...>>(args[1])._v,
                from_object<Select_<2, ARGS...>>(args[2])._v);
#endif
    // }
    // default: {
    //     ERROR("Add more cases");
    // }
    // };
  }
};

Object globalFunctionBindings;

template <typename MV = Indices<>, typename RT, typename... ARGS>
void def_(const string &name, RT (*fp)(ARGS...)) {
  Functoid *f = new VariadicFunctoid<MV, RT, ARGS...>(name, fp);
  Object funcName = allocateObject();
  funcName->_kind = t_symbol;
  strcpy(funcName->data._str, name.c_str());
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

Object Car(Object x);
Object Cdr(Object x);

string repr(Object o) {
  stringstream ss;
  if (o.nilp())
    return "NIL";
  switch (o->_kind) {
  case t_undef:
    return "<UNDEF>";
  case t_int:
    ss << o->data._int;
    break;
  case t_double:
    ss << o->data._double;
    break;
  case t_functoid:
    ss << "a_" << kindname(t_functoid);
    break;
  case t_string:
    ss << "\"" << o->data._str << "\"";
    break;
  case t_cons:
    ss << "(" << repr(Car(o)) << " . " << repr(Cdr(o)) << ")";
    break;
  case t_symbol:
    ss << o->data._str;
    break;
  }
  return ss.str();
}

Object Car(Object x) {
  if (x.nilp())
    return Nil;
  if (x->_kind == t_cons) {
    return x->data.cons._car;
  }
  BAD_TYPE(x, t_cons);
}

Object Cdr(Object x) {
  if (x.nilp())
    return Nil;
  if (x->_kind == t_cons) {
    return x->data.cons._cdr;
  }
  BAD_TYPE(x, t_cons);
}

string SymbolName(Object x) {
  if (x.nilp())
    return "NIL";
  if (x->_kind == t_symbol) {
    return string(x->data._str);
  }
  BAD_TYPE(x, t_symbol);
}

string repr(Object_mv omv) {
  stringstream ss;
  if (omv.numValues() > 0) {
    Object o0 = omv;
    ss << repr(o0) << endl;
  }
  for (int i(1); i < omv.numValues(); ++i) {
    ss << repr(globalMultipleValues[i]) << endl;
  }
  return ss.str();
}

/* Apply the fn to the list of args */
Object FindFunctoid(Object sym) {
  if (sym->_kind != t_symbol) {
    BAD_TYPE(sym, t_symbol);
  }
  string name = sym->data._str;
  Object cur = globalFunctionBindings;
  for (Object cur = globalFunctionBindings;
       cur.notnilp(); cur = Cdr(cur)) {
    Object entry = Car(cur);
    Object fname = Car(entry);
    Object functoid = Cdr(entry);
    if (strcmp(fname->data._str, name.c_str()) == 0) {
      return functoid;
    }
  }
  stringstream ss;
  ss << "Could not find functoid for " << name;
  ERROR(ss.str().c_str());
}

int Length(Object c) {
  if (c.nilp())
    return 0;
  int i(0);
  while (c.notnilp()) {
    i++;
    c = Cdr(c);
  }
  return i;
}

Object Cons(Object car, Object cdr = Nil) {
  Object cons = allocateObject();
  cons->_kind = t_cons;
  cons->data.cons._car = car;
  cons->data.cons._cdr = cdr;
  return cons;
}

Object Symbol(const string &name) {
  Object o = allocateObject();
  o->_kind = t_symbol;
  strcpy(o->data._str, name.c_str());
  return o;
}

Object Int(int x) {
  Object o = allocateObject();
  o->_kind = t_int;
  o->data._int = x;
  return o;
}

Object_mv Apply(Object fname, Object args) {
  Object fn = FindFunctoid(fname);
  int nargs = Length(args);
  Object argArray[64];
  int i(0);
  for (; args.notnilp(); args = Cdr(args)) {
    argArray[i++] = Car(args);
  }
  return fn->data._functoid->activate(nargs, argArray);
}

int plus123(int &p1, int &p2, int &p3) {
  int x = p1;
  p1 = x + 1;
  p2 = x + 2;
  p3 = x + 3;
  return x;
}

int sum(int x, int y, int z) {
  return x + y + z;
}

int main(int argc, char *argv[]) {
  def_<Indices<1, 2, 3>>("plus123", &plus123);
  def_("sum", &sum);

  // testing
  Object sumArgs = Cons(Int(1), Cons(Int(2), Cons(Int(3))));
  Object_mv sumResult = Apply(Symbol("sum"), sumArgs);
  printf("sum of %s  -->\n%s\n", repr(sumArgs).c_str(), repr(sumResult).c_str());

  Object plus123Args = Cons(Int(1), Cons(Int(2), Cons(Int(3))));
  Object_mv plus123Result = Apply(Symbol("plus123"), plus123Args);
  printf("plus123 of %s  -->\n%s\n", repr(plus123Args).c_str(), repr(plus123Result).c_str());
}
