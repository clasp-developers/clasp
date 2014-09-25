/*
    File: mutableVector.h
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


template <class T, typename ScannerFunctor >
class VectorBase {
protected:
    size_type        _End; // Index one beyond the last valid element of the VectorBase
    VectorBase() : _End(0) {};
    VectorBase(size_type n) : _End(n) {};
};


template <class T, typename ScannerFunctor >
class MutableVector : public VectorBase<T,ScannerFunctor> {
protected:
    size_type   _Capacity; // Index one beyond the total number of elements allocated
    T           _Data[0]; // Store _Capacity numbers of T structs/classes starting here

public:
    typedef T                                   value_type;
    typedef T*                                  pointer_type;
    typedef T&                                  reference;
    typedef MutableVector<T,ScannerFunctor>     my_type;
    typedef my_type*                            pointer_my_type;
public:
    MutableVector() : VectorBase(0)
                    , _Capacity(0) {};


    MutableVector(size_type n, const value_type& val = value_type()) : VectorBase(n)
                                                                     , _Capacity(n+MUTABLE_VECTOR_PAD)
    {
        for ( pointer_type it = this->_Data[0]; it<this->_End; ++it ) {
            new(it) value_type(val); // placement new with copy constructor
        }
    }


    reference operator[](size_type n) { return this->_Data[n];};
    
    pointer_my_type push_back_and_maybe_relocate(const value_type& x)
    {
        pointer_my_type myAddress = this;
        if ( this->_End >= this->_Capacity ) {
            // This is where we grow the Vector
            size_type newSize = this->_Capacity * MutableVectorGrow;
            pointer_my_type newAddress(NULL);
            GC_REQUEST_VECTOR_RESIZE(value_type, myAddress, newSize, newAddress );
            this->_Data[this->_End] = x;
            ++this->_End;
            return newAddress;
        }
        this->_Data[this->_End] = x;
        ++this->_End;
        return this; // We didn't move so return our address
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


#endif //mutableVector.h
