/*
    File: testarr.cc
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
