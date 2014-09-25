/*
    File: testLldb.cc
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


struct A {
    int _val;
    A(int x) : _val(x) {};
    int val() { return _val; };
};

template <typename T>
struct wrapped_ptr {
    wrapped_ptr() : px(NULL) {};
    wrapped_ptr(T* p) : px(p) {};


    T*  px;

//    T* operator->() { return this->px;};
};


template <typename T>
struct smart_ptr : public wrapped_ptr<T> {
    smart_ptr()     : wrapped_ptr<T>(NULL) {};
    smart_ptr(T* p) : wrapped_ptr<T>(p) {};

    T* operator->() { return this->px;};
};


void print(smart_ptr<A> v)
{
    printf("Int = %d\n", v->val());
}

int main(int argc, char* argv[])
{
    printf("Starting\n");
    smart_ptr<A> x = new A(5);
    print(x);
};
