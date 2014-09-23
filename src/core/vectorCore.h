/*
    File: vectorCore.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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

    template <class T>
    class VectorBase {
    protected:
        size_t        _End; // Index one beyond the last valid element of the VectorBase
        VectorBase() : _End(0) {};
        VectorBase(size_t n) : _End(0) {};
    };


    template <class T, typename ScannerFunctor = MutableVectorScanner<T> >
    class MutableVector : public VectorBase<T> {
    protected:
        size_t   _Capacity; // Index one beyond the total number of elements allocated
        T           _Data[0]; // Store _Capacity numbers of T structs/classes starting here

    public:
        typedef T                                   value_type;
        typedef T*                                  pointer_type;
        typedef pointer_type                        iterator;
        typedef T&                                  reference;
        typedef MutableVector<T,ScannerFunctor>     my_type;
        typedef my_type*                            pointer_my_type;
        static const size_t                         MutableVectorPad = 8;
        constexpr static const float                MutableVectorGrow = 1.2;

    public:
        static size_t _sizeof(size_t n) { return sizeof(my_type)+sizeof(T)*n; };
    public:
        /*! Allocate a vector that contains at least (n) elements */
        inline static pointer_my_type create(size_t n,const value_type& initial_element=value_type()) {
            pointer_my_type myAddress(NULL);
            GC_RESERVE_MUTABLE_VECTOR_ALLOCATE(value_type,n+MutableVector<value_type>::MutableVectorPad,myAddress);
            for ( size_t i(0); i<n; ++i ) {
                new (&(*myAddress)[i]) value_type(initial_element);
            }
            myAddress->_End = n;
            return myAddress;
        };

    public:
        /*! Contstruct a vector with MutableVectorPad _Capacity */
        MutableVector() : VectorBase<T>()
                        , _Capacity(MutableVectorPad) {};


        /*! Contstruct a vector with MutableVectorPad with at least n _Capacity */
        MutableVector(size_t n, const value_type& val = value_type()) : VectorBase<T>()
                                                                      , _Capacity(n+MutableVectorPad)
        {
        }


        size_t size() const { return this->_End; };

        reference operator[](size_t n) { return this->_Data[n];};
    
        pointer_my_type __push_back_and_maybe_relocate(const value_type& x)
        {
            pointer_my_type myAddress = this;
            if ( this->_End >= this->_Capacity ) {
                // This is where we grow the Vector
                size_t newSize = this->_Capacity * MutableVectorGrow;
                pointer_my_type newAddress(NULL);
                GC_RESERVE_MUTABLE_VECTOR_RESIZE(value_type, myAddress, newSize, newAddress );
                new (&this->_Data[this->_End]) value_type(x);
                ++this->_End;
                return newAddress;
            }
            this->_Data[this->_End] = x;
            ++this->_End;
            return this; // We didn't move so return our address
        }

        /*! Resize the vector so that it contains AT LEAST n elements */
        pointer_my_type __resize_and_maybe_relocate(size_t n, const value_type& x = value_type())
        {
            if ( n > this->size() ) {
                size_t newSize = this->_Capacity * MutableVectorGrow;
                pointer_my_type newAddress(NULL);
                GC_RESERVE_MUTABLE_VECTOR_RESIZE(value_type, this, newSize, newAddress );
                for ( size_t i(this->size()); i<n; ++i ) {
                    new (&(*newAddress)[i]) value_type(x);
                }
                return newAddress;
            }
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
    void Resize(MV*& vec, size_t n)
    {
        MV* newVec(NULL);
        newVec = vec->__resize_and_maybe_relocate(n);
        if ( newVec != NULL ) {
            MV* oldVec(vec);
            vec = newVec;
            GC_RESERVE_MUTABLE_VECTOR_FREE(/*typename MV::value_type, */oldVec);
        }
    }

    template <class MV>
    void Resize(MV*& vec, size_t n, typename MV::value_type initial_value)
    {
        MV* newVec(NULL);
        newVec = vec->__resize_and_maybe_relocate(n,initial_value);
        if ( newVec != NULL ) {
            MV* oldVec(vec);
            vec = newVec;
            GC_RESERVE_MUTABLE_VECTOR_FREE(/*typename MV::value_type,*/oldVec);
        }
    }
            
};

#endif //mutableVector.h
