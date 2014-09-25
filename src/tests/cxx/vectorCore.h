/*
    File: vectorCore.h
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
#ifndef core_mutableVector_H
#define core_mutableVector_H



namespace vec {

#ifdef USE_MPS
    template <class T>
    class MutableVectorScanner {
        GC_RESULT operator()(GC_SCAN_ARGS_PROTOTYPE, T& ref) {
            GC_SCANNER_BEGIN() {
                SMART_PTR_FIX(ref);
            } GC_SCANNER_END();
            return GC_RES_OK;
        }
    };
#else
    template <class T>
    class MutableVectorScanner {
        GC_RESULT operator()(GC_SCAN_ARGS_PROTOTYPE, T& ref) {
            // nothing
            return GC_RES_OK;
        }
    };
#endif


    template <class T, typename ScannerFunctor = MutableVectorScanner<T> >
    class MutableVector  {
    protected:
        size_t      _Capacity; // Index one beyond the total number of elements allocated
        size_t      _End;
        T           _Data[0]; // Store _Capacity numbers of T structs/classes starting here

    public:
        typedef T                                   value_type;
        typedef T*                                  pointer_type;
        typedef pointer_type                        iterator;
        typedef T&                                  reference;
        typedef MutableVector<T,ScannerFunctor>     my_type;
        typedef my_type*                            pointer_my_type;
        static const size_t                         MutableVectorPad = 8;
        constexpr static const float                MutableVectorGrow = 1.5;

    public:
        static size_t _sizeof(size_t n)
        {
            size_t headerSz = sizeof(my_type);
            size_t dataSz = sizeof(T)*n;
            size_t totalSz = headerSz+dataSz;
            LOG(("headerSz[%lu] + ( value_size[%lu] * n[%lu] -> dataSz[%lu] ) --> totalSz[%lu]\n",
                 headerSz, sizeof(T), n, dataSz, totalSz));
            return totalSz;
        };
    public:
        /*! Allocate a vector that contains at least (n) elements */
        inline static pointer_my_type create(size_t n,const value_type& initial_element=value_type()) {
            pointer_my_type myAddress(NULL);
            size_t capacity = n+MutableVector<value_type>::MutableVectorPad;
#ifdef TESTING
            {
                size_t sz = vec::MutableVector<value_type>::_sizeof(capacity);
                myAddress = (vec::MutableVector<value_type>*)malloc(vec::MutableVector<value_type>::_sizeof(capacity));
                if (!myAddress) THROW_OUT_OF_MEMORY();
                LOG(("malloc@%p %lu bytes\n",myAddress,sz));
            }
#else
            GC_VECTOR_ALLOCATE(value_type,capacity,myAddress);
#endif
            new (myAddress) my_type(capacity);
            for ( size_t i(0); i<n; ++i ) {
                T* p = &((*myAddress)[i]);
                new (p) T(initial_element);
            }
            myAddress->_End = n;
            return myAddress;
        };

    public:
        /*! Contstruct a vector with MutableVectorPad _Capacity */
        MutableVector(size_t c) : _Capacity(c)
                                , _End(0)
                                {};


        /*! Contstruct a vector with MutableVectorPad with at least n _Capacity */
        MutableVector(size_t c, size_t n, const value_type& val = value_type()) : _Capacity(c)
                                                                                , _End(n) {}


        size_t size() const { return this->_End; };
        size_t capacity() const { return this->_Capacity; };

        reference operator[](size_t n) { return this->_Data[n];};
    
        pointer_my_type __push_back_and_maybe_relocate(const value_type& x)
        {
            pointer_my_type myAddress = this;
            if ( this->_End >= this->_Capacity ) {
                // This is where we grow the Vector
                size_t newCapacity = this->_Capacity * MutableVectorGrow;
                pointer_my_type newAddress(NULL);
#ifdef TESTING
                {
                    size_t newBytes = vec::MutableVector<value_type>::_sizeof(newCapacity);
                    newAddress = (vec::MutableVector<value_type>*)malloc(newBytes);
                    if (!newAddress) THROW_OUT_OF_MEMORY();
                    LOG(("malloc@%p %lu bytes\n",newAddress,newBytes));
                    new (newAddress) my_type(newCapacity);
                    for ( size_t zi(0); zi<this->_End; ++zi ) {                   
                        // the array at newAddress is undefined - placement new to copy
                        new (&((*newAddress)[zi])) value_type(this->operator[](zi));             
                    };
                    newAddress->_End = this->_End;
                }
#else
                GC_RESERVE_MUTABLE_VECTOR_RESIZE(value_type, this, newCapacity, newAddress );
#endif
                // Placement new in the incoming value of x
                new (&(newAddress->_Data[this->_End])) value_type(x);
                ++this->_End;
                return newAddress;
            }
            // We didn't need to reallocate so just placement in the new data at _End
            new (&(this->_Data[this->_End])) value_type(x);
            ++this->_End;
            return this; // We didn't move so return our address
        }

        /*! Resize the vector so that it contains AT LEAST n elements */
        pointer_my_type __resize_and_maybe_relocate(size_t n, const value_type& x = value_type())
        {
            if ( n > this->_Capacity ) {
                size_t newCapacity = n * MutableVectorGrow;
                pointer_my_type newAddress(NULL);
#ifdef TESTING
                {
                    size_t newBytes = vec::MutableVector<value_type>::_sizeof(newCapacity);
                    newAddress = (vec::MutableVector<value_type>*)malloc(newBytes);
                    if (!newAddress) THROW_OUT_OF_MEMORY();
                    LOG(("malloc@%p %lu bytes\n",newAddress,newBytes));
                    new (newAddress) my_type(newCapacity);
                    for ( size_t zi(0); zi<this->_End; ++zi ) {                   
                        // the array at newAddress is undefined - placement new to copy
                        new (&((*newAddress)[zi])) value_type(this->operator[](zi));             
                    };
                }
#else
                GC_RESERVE_MUTABLE_VECTOR_RESIZE(value_type, this, newCapacity, newAddress );
#endif
                for ( size_t i(this->_End); i<n; ++i ) {
                    new (&(*newAddress)[i]) value_type(x);
                }
                newAddress->_End = n;
                return newAddress;
            } else if ( n < this->_Capacity * MutableVectorShrink ) {
                
            // There was enough capacity to resize the array
            this->_End = n;
            return this;
        }

        void pop_back() {
            if ( this->_End > 0 ) {
                --this->_End;
            }
        }

        pointer_type begin()
        {
            return &this->_Data[0];
        }

        pointer_type end()
        {
            return &this->_Data[this->_End];
        }

    };


    template <class MV>
    void Resize(MV*& vec, size_t n, typename MV::value_type initial_value = MV::value_type() )
    {
        MV* newVec(NULL);
        newVec = vec->__resize_and_maybe_relocate(n);
        if ( newVec != vec ) {
            MV* oldVec(vec);
            vec = newVec;
#ifdef TESTING
            LOG(("free@%p\n", oldVec));
            free(oldVec);
#else
            GC_RESERVE_MUTABLE_VECTOR_FREE(/*typename MV::value_type, */oldVec);
#endif
        }
    }


    template <class MV>
    void Push_back(MV*& vec, const typename MV::value_type& val )
    {
        MV* newVec(NULL);
        newVec = vec->__push_back_and_maybe_relocate(val);
        if ( newVec != vec ) {
            MV* oldVec(vec);
            vec = newVec;
#ifdef TESTING
            LOG(("About to free@%p\n", oldVec));
            free(oldVec);
#else
            GC_RESERVE_MUTABLE_VECTOR_FREE(/*typename MV::value_type, */oldVec);
#endif
            LOG(("Resized vec@%p\n", vec));
        }
    }

            
};

#endif //mutableVector.h
