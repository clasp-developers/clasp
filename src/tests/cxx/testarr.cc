#if 0

#include <stdio.h>
#include <new>

template <class T, int SZ=0>
class GCArray_impl  {
public:
    template<class U, typename Allocator> friend class GCArray;
    typedef T               value_type;
    typedef value_type&     reference;

    GCArray_impl(size_t num) : _Capacity(num)
                             , _Alive(false) {};


    template <typename...ARGS>
    GCArray_impl(size_t numExtraArgs, ARGS&&...args) : _Capacity(numExtraArgs+sizeof...(ARGS))
                                                     , _Alive(false)
                                                     , _data{args...} {};
        
    GCArray_impl() : _Capacity(0) {};

    size_t      _Capacity; // Index one beyond the total number of elements allocated
    bool        _Alive; // Indicate if the data is scannable or not
    T           _data[SZ]; // Store _Capacity numbers of T structs/classes starting here

public:
    value_type& operator[](size_t i) { return this->_data[i]; };    
    const value_type& operator[](size_t i) const { return this->_data[i]; };
};

int main(int argc, char* argv[])
{
    GCArray_impl<int,5>* x = new GCArray_impl<int,5>(0,1,2,3,4,5);
    for ( int i(0); i<5; ++i ) {printf("[%d] = %d\n", i, (*x)[i]);}
}


#endif


