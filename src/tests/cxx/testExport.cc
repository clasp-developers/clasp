/*
    File: testExport.cc
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
#include <vector>
#include <sstream>
#include <functional>


class __attribute__((weak)) MyException {
    virtual void test() __attribute__((weak));
public:
    __attribute__((weak)) MyException(int i) : _val(i) {};
    int _val;
    __attribute__((weak)) virtual ~MyException() {};
};


void MyException::test() {};

extern void foo()
{
    printf("Hi - I'm foo\n");
}




int main(int argc, char* argv[])
{
    try {
        throw MyException(1);
    } catch (MyException& e)
    {
        printf("Caught MyException\n");
    }

    // Here I would load a plugin that expects __ZTI11MyException to be external
    // but __ZTI11MyException is non-external so the exception handling will terminate
    //

    // Load_plugin_that_would_catch_MyException

}


#if 0

	clang++ -fvisibility=default -std=c++11 -stdlib=libc++  -o testExport testExport.cc

fry:cxx$ nm -mapv testExport | grep MyException
0000000100000db0 (__TEXT,__text) non-external (was a private external) __ZN11MyExceptionC1Ei
0000000100000e00 (__TEXT,__text) non-external (was a private external) __ZN11MyExceptionC2Ei
0000000100001070 (__DATA,__data) non-external (was a private external) __ZTI11MyException
0000000100000f34 (__TEXT,__const) weak external __ZTS11MyException
fry:cxx$ nm -mapv testExport | grep MyException | c++filt
0000000100000db0 (__TEXT,__text) non-external (was signed char private external) MyException::MyException(int)
0000000100000e00 (__TEXT,__text) non-external (was signed char private external) MyException::MyException(int)
0000000100001070 (__DATA,__data) non-external (was signed char private external) typeinfo for MyException
0000000100000f34 (__TEXT,__const) weak external typeinfo name for MyException

#endif
