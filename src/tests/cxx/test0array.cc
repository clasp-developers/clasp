#include <new>
#include <stdio.h>
#include <stdlib.h>
   
template <class T>
size_t gc_sizeof(size_t n) { return sizeof(T)+sizeof(typename T::value_type)*n;}

template <class T >
class GCVector_impl  {
public:
    typedef T               value_type;
    typedef value_type&     reference;
    GCVector_impl(size_t num, size_t e=0) : _Capacity(num), _End(e) {};
    size_t      _Capacity; // Index one beyond the total number of elements allocated
    size_t      _End;
    T           _Data[0]; // Store _Capacity numbers of T structs/classes starting here

public:
    value_type& operator[](size_t i) { return this->_Data[i]; };
    const value_type& operator[](size_t i) const { return this->_Data[i]; };
};

struct X {
	X(int i) : x(i) {};
	int x;
};

int main(int argc, const char* argv[])
{
    size_t num = 4;
    GCVector_impl<X>*  _vecObjects = static_cast<GCVector_impl<X>*>(malloc(gc_sizeof<GCVector_impl<X>>(num*2)));
    new (_vecObjects) GCVector_impl<X>(num*2,num);
    for ( int i(0); i<num; ++i ) {
        (*_vecObjects)[i] = X(i);
    }
    printf("Dump vecObjects: ");
    for ( int j(0); j<num; ++j ) {printf("%d ", (*_vecObjects)[j].x);};
    printf("\n");
}
