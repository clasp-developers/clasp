/*
    File: testTemplate9.cc
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

#define STR_MAX	128

struct err
{
    err(const string& m) : msg(m) {};
    string	msg;
};

#define BAD_TYPE(datum,ty) {stringstream ss; ss << "Wrong type " << repr(datum) << " expected " << kindname(ty); throw(err(ss.str()));};
#define ERROR(s) {printf("%s\n",s); exit(1);}



namespace detail {

    template <int N, class ...Types> struct NthParameterType;

    template <class First, class ...Rest>
    struct NthParameterType<0, First, Rest...> {
	typedef First type; 
    };

    template <int N, class First, class... Rest>
    struct NthParameterType<N, First, Rest...> {
	typedef typename NthParameterType<N-1, Rest...>::type type;
    };


}; // detail

template <int N, class ...Types>
using Select_ = typename detail::NthParameterType<N, Types...>::type;






struct ObjectStruct;

struct Object {
    Object() : px(NULL) {};
    Object(ObjectStruct* p) : px(p) {};

    ObjectStruct* operator->() { return this->px;};
    ObjectStruct*	px;
    int numValues() { return 1;};
    bool nilp() const { return this->px==NULL;};
    bool notnilp() const { return !this->nilp();};
};


struct Object_mv : public Object {
    Object_mv(Object obj, int numValues ) : Object(obj.px), _NumValues(numValues) {};
    int _NumValues;
    int numValues() { return this->_NumValues;};
};


typedef Object ArgArray[];



class Functoid {
public:
    virtual Object_mv activate(int nargs, ArgArray args)=0;
};


typedef enum { t_undef, t_int, t_double, t_cons, t_string, t_functoid, t_symbol } kind;

string kindname(kind k)
{
    switch (k) {
    case t_undef: return "<undef>";
    case t_int: return "<int>";
    case t_double: return "<double>";
    case t_cons: return "<cons>";
    case t_string: return "<string>";
    case t_functoid: return "<functoid>";
    case t_symbol: return "<symbol>";
    default: return "<UNKNOWN KIND>";
    };
}







struct ObjectStruct {
    ObjectStruct() : _kind(t_undef) {};
    kind	_kind;
    union cons_union {
	cons_union() {};
	int		_int;
	double 		_double;
	Functoid*	_functoid;
	char 		_str[STR_MAX];
	struct {
	    char	_symbolName[STR_MAX];
	    Object	_function;
	    Object	_value;
	} symbol;
	struct {
	    Object 	_car;
	    Object  	_cdr;
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



template <>
struct from_object<int> {
    int _v;
    from_object() : _v(0) {};
    void set(Object o)
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
struct from_object<int&> {
    int _v;
    from_object() : _v(0) {};
    void set(Object o)
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
    static Object convert(int& x) {
	Object o = new ObjectStruct();
	o->_kind = t_int;
	o->data._int = x;
	return o;
    }
};


template <>
struct to_object<int> {
    static Object convert(int x) {
	Object o = new ObjectStruct();
	o->_kind = t_int;
	o->data._int = x;
	return o;
    }
};




template <typename... POLS>
struct Policies {};


const int _return = 32767;
const int _this = 32768;

template <int N>
struct OutValue {};

template <int N>
struct PureOutValue {};

template <int N>
struct Adopt;




namespace detail {
    template <typename POL, typename T> 
    struct IndexOf;

    template <typename T>
    struct IndexOf< Policies<>, T>
    {
	enum { value = -1 };
    };

    template <typename Head, typename... Tail>
    struct IndexOf< Policies<Head,Tail...>, Head>
    {
	enum { value = 0 };
    };

    template <typename Head, typename...Tail, typename T>
    struct IndexOf< Policies<Head,Tail...>, T> 
    {
    private:
	enum { temp = IndexOf<Policies<Tail...>,T>::value };
    public:
	enum { value = temp == -1 ? -1 : 1 + temp };
    };
};
template <typename Pol, typename T>
using IndexOf = typename detail::IndexOf<Pol,T>;


template <typename SET, typename T>
struct Contains 
{
    enum { value = IndexOf<SET,T>::value < 0 ? false : true };
};





namespace detail {
    template <typename POL> 
    struct CountPureOutValues;

    template <>
    struct CountPureOutValues< Policies<> >
    {
	enum { value = 0 };
    };

    template <typename... Tail, int N>
    struct CountPureOutValues< Policies<PureOutValue<N>,Tail...> >
    {
	enum { value = CountPureOutValues<Policies<Tail...> >::value + 1 };
    };

    template <typename Head, typename... Tail>
    struct CountPureOutValues< Policies<Head,Tail...> >
    {
	enum { value = CountPureOutValues<Policies<Tail...> >::value };
    };

};
template <typename Policies>
using CountPureOutValues = typename detail::CountPureOutValues<Policies>;

template <int>
struct Int2Type
{};


template <int T>
class IncrIf {
public:
    inline static void doit(int& i) { ++i; };
};
template <>
class IncrIf<0> {
public:
    inline static void doit(int& i) {};
};





template <typename Head, typename... Tail, int N, typename Policies>
void fillReturnImpl(Object mv[], int& idx, Policies, Int2Type<N>, Head&& head, Tail &&...tail)
{
    if ( Contains<Policies,OutValue<N>>::value
	 || Contains<Policies,PureOutValue<N>>::value ) {
	printf("Policy was met for arg %d\n", N );
	mv[idx++] = to_object<Head>::convert(head);
    }
    fillReturnImpl(mv, idx, Policies(), Int2Type<N + 1>(), std::forward<Tail>(tail)...);
}



template <int N, typename Policies>
void fillReturnImpl(Object mv[], int& idx, Policies, Int2Type<N>)
{}



/* I limited this FunctionWrapper to functions that return values
   I will probably have to write a specializer that specializes on
   void functions */
template<typename Pols, class RT, typename... ARGS>
class ResultHandlingFunctionAdaptor {
public:
    typedef std::function<RT(ARGS...)> 	Type;
    Type				fn;
public:
    ResultHandlingFunctionAdaptor(Type f) : fn(f) {};
/* It was absolutely necessary to to make operator() a template function so that
   perfect forwarding can work - I cannot use the ARGS parameter pack for the 
   ResultHandlingFunctionAdaptor class */
    template <typename... FARGS>
    Object_mv operator()(FARGS&& ...args) {
	RT retval = fn(std::forward<FARGS>(args)...);
	globalMultipleValues[0] = to_object<RT>::convert(retval);
	int idx=1;
	fillReturnImpl(globalMultipleValues,idx,Pols(),Int2Type<1>(), std::forward<FARGS>(args)...);
	return Object_mv(globalMultipleValues[0],idx);
    }
};

template <typename RT, typename...Args>
class ResultHandlingFunctionAdaptor<Policies<>,RT,Args...> {
public:
    typedef std::function<RT(Args...)> 	Type;
    Type				fn;
public:
    ResultHandlingFunctionAdaptor(Type f) : fn(f) {};
/* It was absolutely necessary to to make operator() a template function so that
   perfect forwarding can work - I cannot use the ARGS parameter pack for the 
   ResultHandlingFunctionAdaptor class */
    template <typename... FARGS>
    Object_mv operator()(FARGS&& ...args) {
	RT retval = fn(std::forward<FARGS>(args)...);
	return Object_mv(to_object<RT>::convert(retval),1);
    }
};


template<typename Pols, typename...Args>
class ResultHandlingFunctionAdaptor<Pols,void,Args...> {
public:
    typedef std::function<void(Args...)> 	Type;
    Type					fn;
public:
    ResultHandlingFunctionAdaptor(Type f) : fn(f) {};
/* It was absolutely necessary to to make operator() a template function so that
   perfect forwarding can work - I cannot use the ARGS parameter pack for the 
   ResultHandlingFunctionAdaptor class */
    template <typename... FARGS>
    Object_mv operator()(FARGS&& ...args) {
	fn(std::forward<FARGS>(args)...);
	int idx=0;
	fillReturnImpl(globalMultipleValues,idx,Pols(),Int2Type<1>(), std::forward<FARGS>(args)...);
	return Object_mv(globalMultipleValues[0],idx);
    }
};

template<typename...Args>
class ResultHandlingFunctionAdaptor<Policies<>,void,Args...> {
public:
    typedef std::function<void(Args...)> 	Type;
    Type					fn;
public:
    ResultHandlingFunctionAdaptor(Type f) : fn(f) {};
/* It was absolutely necessary to to make operator() a template function so that
   perfect forwarding can work - I cannot use the ARGS parameter pack for the 
   ResultHandlingFunctionAdaptor class */
    template <typename... FARGS>
    Object_mv operator()(FARGS&& ...args) {
	fn(std::forward<FARGS>(args)...);
	return Object_mv(Nil,0);
    }
};





/* NOTE: I had to limit this class to one arity
   - I may need to duplicate the code for a fixed number of arities (ugh) */
template<typename Pols,typename RT,typename ARG1=void, typename ARG2=void, typename ARG3=void>
class VariadicFunctoid : public Functoid{
public:
    typedef ResultHandlingFunctionAdaptor<Pols,RT,ARG1,ARG2,ARG3> 		ResultHandlingFunctionAdaptorType;
    typedef typename ResultHandlingFunctionAdaptorType::Type 	Type;
    ResultHandlingFunctionAdaptorType 				fptr;
public:
    enum { NumParams = 3 };
    VariadicFunctoid(const string& name, Type ptr) : fptr(ptr) {};
    
    Object_mv activate( int nargs, ArgArray args )
    {
	int countPureOutValues = CountPureOutValues<Pols>::value;
	if ( nargs != (NumParams-countPureOutValues) )
	{
	    stringstream ss;
	    ss << "Lisp function expected "<< (NumParams-countPureOutValues) << " argument(s) but was passed " << nargs << " argument(s).";
	    ERROR(ss.str().c_str());
	}
	// This code works
	from_object<ARG1> a1;
	from_object<ARG2> a2;
	from_object<ARG3> a3;
	int idx = 0;
	if ( !Contains<Pols,PureOutValue<1> >::value ) a1.set(args[idx++]);
	if ( !Contains<Pols,PureOutValue<2> >::value ) a2.set(args[idx++]);
	if ( !Contains<Pols,PureOutValue<3> >::value ) a3.set(args[idx++]);
	return fptr(a1._v,a2._v,a3._v);
    }
    
};

template<typename Pols,typename RT,typename ARG1, typename ARG2>
class VariadicFunctoid<Pols,RT,ARG1,ARG2,void> : public Functoid{
public:
    typedef ResultHandlingFunctionAdaptor<Pols,RT,ARG1,ARG2> 		ResultHandlingFunctionAdaptorType;
    typedef typename ResultHandlingFunctionAdaptorType::Type 	Type;
    ResultHandlingFunctionAdaptorType 				fptr;
public:
    enum { NumParams = 3 };
    VariadicFunctoid(const string& name, Type ptr) : fptr(ptr) {};
    
    Object_mv activate( int nargs, ArgArray args )
    {
	int countPureOutValues = CountPureOutValues<Pols>::value;
	if ( nargs != (NumParams-countPureOutValues) )
	{
	    stringstream ss;
	    ss << "Lisp function expected "<< (NumParams-countPureOutValues) << " argument(s) but was passed " << nargs << " argument(s).";
	    ERROR(ss.str().c_str());
	}
	// This code works
	from_object<ARG1> a1;
	from_object<ARG2> a2;
	int idx = 0;
	if ( !Contains<Pols,PureOutValue<1> >::value ) a1.set(args[idx++]);
	if ( !Contains<Pols,PureOutValue<2> >::value ) a2.set(args[idx++]);
	return fptr(a1._v,a2._v);
    }
    
};

template<typename Pols,typename RT,typename ARG1>
class VariadicFunctoid<Pols,RT,ARG1,void,void> : public Functoid{
public:
    typedef ResultHandlingFunctionAdaptor<Pols,RT,ARG1> 		ResultHandlingFunctionAdaptorType;
    typedef typename ResultHandlingFunctionAdaptorType::Type 	Type;
    ResultHandlingFunctionAdaptorType 				fptr;
public:
    enum { NumParams = 3 };
    VariadicFunctoid(const string& name, Type ptr) : fptr(ptr) {};
    
    Object_mv activate( int nargs, ArgArray args )
    {
	int countPureOutValues = CountPureOutValues<Pols>::value;
	if ( nargs != (NumParams-countPureOutValues) )
	{
	    stringstream ss;
	    ss << "Lisp function expected "<< (NumParams-countPureOutValues) << " argument(s) but was passed " << nargs << " argument(s).";
	    ERROR(ss.str().c_str());
	}
	// This code works
	from_object<ARG1> a1;
	int idx = 0;
	if ( !Contains<Pols,PureOutValue<1> >::value ) a1.set(args[idx++]);
	return fptr(a1._v);
    }
    
};




Object globalFunctionBindings;
Object globalSymbols;

Object makeCons(Object car, Object cdr);
Object intern(const string& name);
Object makeFunctoid(Functoid* f);
Object makeInt(int x);
Object Car(Object x);
Object Cdr(Object x);


template <typename...Pols, typename RT, typename... ARGS, int...IS>
void def_(const string& name, RT (*fp)(ARGS...), Policies<Pols...> )
{
    Functoid* f = new VariadicFunctoid<Policies<Pols...>,RT,ARGS...>(name,fp);
    Object binding = makeCons(intern(name),makeFunctoid(f));
    globalFunctionBindings = makeCons(binding,globalFunctionBindings);
}

template <typename RT, typename... ARGS>
void def_(const string& name, RT (*fp)(ARGS...))
{
    Functoid* f = new VariadicFunctoid<Policies<>,RT,ARGS...>(name,fp);
    Object binding = makeCons(intern(name),makeFunctoid(f));
    globalFunctionBindings = makeCons(binding,globalFunctionBindings);
}


Object allocateObject()
{
    Object x(new ObjectStruct);
    return x;
}

string repr(Object o)
{
    stringstream ss;
    if ( o.nilp() ) return "NIL";
    switch (o->_kind) {
    case t_undef: return "<UNDEF>";
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



Object Car(Object x)
{
    if (x.nilp()) return Nil;
    if (x->_kind==t_cons) {
	return x->data.cons._car;
    }
    BAD_TYPE(x,t_cons);
}

Object Cdr(Object x)
{
    if (x.nilp()) return Nil;
    if (x->_kind==t_cons) {
	return x->data.cons._cdr;
    }
    BAD_TYPE(x,t_cons);
}

string SymbolName(Object x)
{
    if (x.nilp()) return "NIL";
    if (x->_kind==t_symbol) {
	return string(x->data._str);
    }
    BAD_TYPE(x,t_symbol);
}




string repr(Object_mv omv)
{
    stringstream ss;
    if (omv.numValues()>0) {
	Object o0 = omv;
	ss << repr(o0) << endl;
    }
    for ( int i(1);i<omv.numValues();++i) {
	ss << repr(globalMultipleValues[i]) << endl;
    }
    return ss.str();
}


/* Apply the fn to the list of args */
Object FindFunctoid(Object sym)
{
    if (sym->_kind !=t_symbol) {
	BAD_TYPE(sym,t_symbol);
    }
    string name = sym->data._str;
    Object cur = globalFunctionBindings;
    for ( Object cur=globalFunctionBindings;
	  cur.notnilp(); cur=Cdr(cur) ) {
	Object entry = Car(cur);
	Object fname = Car(entry);
	Object functoid = Cdr(entry);
	if ( strcmp(fname->data._str,name.c_str()) == 0 ) {
	    return functoid;
	}
    }
    stringstream ss;
    ss << "Could not find functoid for " << name;
    ERROR(ss.str().c_str());
}

int Length(Object c)
{
    if (c.nilp()) return 0;
    int i(0);
    while (c.notnilp()) {
	i++;
	c=Cdr(c);
    }
    return i;
}



Object makeCons(Object car, Object cdr=Nil)
{
    Object cons = allocateObject();
    cons->_kind = t_cons;
    cons->data.cons._car = car;
    cons->data.cons._cdr = cdr;
    return cons;
}

Object makeFunctoid(Functoid* f)
{
    Object o = allocateObject();
    o->_kind = t_functoid;
    o->data._functoid = f;
    return o;
}


Object intern(const string& name)
{
    for (Object cur = globalSymbols; cur.notnilp(); cur=Cdr(cur)){
	Object curSym = Car(cur);
	if ( strcmp(curSym->data.symbol._symbolName,name.c_str()) == 0 ) {
	    return curSym;
	}
    }
    Object ns = allocateObject();
    ns->_kind = t_symbol;
    strcpy(ns->data.symbol._symbolName,name.c_str());
    globalSymbols = makeCons(ns,globalSymbols);
    return ns;
}


Object makeInt(int x)
{
    Object o = allocateObject();
    o->_kind = t_int;
    o->data._int = x;
    return o;
}



Object_mv Apply(Object fname, Object args)
{
    Object fn = FindFunctoid(fname);
    int nargs = Length(args);
    Object argArray[64];
    int i(0);
    for (; args.notnilp(); args=Cdr(args) ){
	argArray[i++] = Car(args);
    }
    return fn->data._functoid->activate(nargs,argArray);
}
	



void plus12(int x, int& p1, int& p2)
{
    p1 = x+1;
    p2 = x+2;
}

int sum(int x, int y, int z)
{
    return x+y+z;
}


int main(int argc, char* argv[])
{

    def_("sum",&sum);
    Object sumArgs = makeCons(makeInt(1),
			      makeCons(makeInt(2),
				       makeCons(makeInt(3))));
    Object_mv sumResult = Apply(intern("sum"),sumArgs);
    printf("sum of %s  -->\n%s\n", repr(sumArgs).c_str(), repr(sumResult).c_str());

    def_("plus12",&plus12,Policies<PureOutValue<2>,PureOutValue<3> >());
    Object plus12Args = makeCons(makeInt(1));
    Object_mv plus12Result = Apply(intern("plus12"),plus12Args);
    printf("plus12 of %s  -->\n%s\n", repr(plus12Args).c_str(), repr(plus12Result).c_str());


    typedef Policies< OutValue<1>, OutValue<3>, PureOutValue<2>,Adopt<_return> >	X;
    printf("    typedef Policies< OutValue<1>, OutValue<3>, PureOutValue<2>,Adopt<Return> >	X;\n");
    printf("IndexOf<X,OutValue<1>> = %d\n", IndexOf<X,OutValue<1> >::value);
    printf("IndexOf<X,OutValue<2>> = %d\n", IndexOf<X,OutValue<2> >::value);
    printf("IndexOf<X,OutValue<3>> = %d\n", IndexOf<X,OutValue<3> >::value);
    printf("IndexOf<X,PureOutValue<3>> = %d\n", IndexOf<X,PureOutValue<3> >::value);
    printf("IndexOf<X,PureOutValue<2>> = %d\n", IndexOf<X,PureOutValue<2> >::value);
    printf("IndexOf<X,PureOutValue<4>> = %d\n", IndexOf<X,PureOutValue<4> >::value);
    printf("IndexOf<X,Adopt<_return>> = %d\n", IndexOf<X,Adopt<_return> >::value);


    printf("    typedef Policies< OutValue<1>, OutValue<3>, PureOutValue<2>,Adopt<Return> >	X;\n");
    printf("Contains<X,OutValue<1>> = %d\n", Contains<X,OutValue<1> >::value);
    printf("Contains<X,OutValue<2>> = %d\n", Contains<X,OutValue<2> >::value);
    printf("Contains<X,OutValue<3>> = %d\n", Contains<X,OutValue<3> >::value);
    printf("Contains<X,PureOutValue<3>> = %d\n", Contains<X,PureOutValue<3> >::value);
    printf("Contains<X,PureOutValue<2>> = %d\n", Contains<X,PureOutValue<2> >::value);
    printf("Contains<X,PureOutValue<4>> = %d\n", Contains<X,PureOutValue<4> >::value);
    printf("Contains<X,Adopt<_return>> = %d\n", Contains<X,Adopt<_return> >::value);



    int outValues = 0;
    IncrIf<Contains<X,OutValue<1> >::value>::doit(outValues);
    IncrIf<Contains<X,OutValue<2> >::value>::doit(outValues);
    IncrIf<Contains<X,OutValue<3> >::value>::doit(outValues);
    printf("Counted the number of times OutValue was in X at runtime = %d\n", outValues);

    typedef Policies<PureOutValue<1>,PureOutValue<3>,OutValue<2>,OutValue<5>,Adopt<_return> > Pol;
    printf("Number of PureOutValue's in Policies<PureOutValue<1>,PureOutValue<3>,OutValue<2>,OutValue<5>,Adopt<_return> > -> %d\n",
	   CountPureOutValues<Pol>::value );
}

#if 0
//I'd like something like 

def_("plus12",&plus12,Policies<out_only<2>,out_only<3> >)

// out_only<N> specifies that parameter N is an out parameter only
// and it gets put into the next multiple-values return slot
//
// out_value<N> specifies that parameter N is an input and output 
// value
//
// adopt<N> N = return or 1,2,3,4 means the wrapper object
// takes ownership of that return value
//

#endif
