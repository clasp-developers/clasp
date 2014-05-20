#include <iostream>
#include <functional>


typedef enum {k_dbl,k_int,k_char} ObjectKind;

typedef union {
    double	_dbl;
    int		_int;
    char 	_char;
} ObjectU;


struct ObjectT {
    ObjectKind	_kind;
    ObjectU 	_obj;
    ObjectT(ObjectKind k) : _kind(k) {};
    void printObjectKind() { printf("Kind %d\n", this->_kind);};
};



typedef ObjectT* Object;



namespace translate {


    template <class oClass>
    struct from_object
    {								
	typedef	int ExpectedType;	
	typedef	int DeclareType;	
	DeclareType _v;
	from_object(Object o) {
	    this->_v = o->_obj._int;
	}								
    };



    template <class oClass> 
    struct to_object
    {								
	typedef	int	GivenType;	
	static Object convert(int x)
	{	
	    Object o = (Object)malloc(sizeof(ObjectT));
	    o->_kind = k_int;
	    o->_obj._int = x;
	    return o;
	}								
    };


};


template <class RT, class... ARGS>  // RT, T1,T2,T3...
class VariadicFunctor {
    std::function<RT(ARGS...)> fptr;
public:
    VariadicFunctor(std::function<RT(ARGS...)> f) : fptr(f) {};
    Object operator()(ARGS... args) {  // T1 o1, T2 o2, ...
	RT res = fptr(translate::from_object<ARGS>(args)._v...);
	return translate::to_object<RT>::convert(res);
    };
};



using namespace std;

int main (int argc, const char* argv[] )
{
    auto f = std::mem_fn(&ObjectT::printObjectKind);
    ObjectT t(k_dbl);
    f(t);
    cout << "Done";
}
