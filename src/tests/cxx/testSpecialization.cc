/*
    File: testSpecialization.cc
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
#include <stdio.h>

namespace core {

struct T_sp {};
};

namespace translate {

struct dont_adopt_pointer {};

template <typename T>
struct test {
};

template <>
struct test<bool *> {
  static void print() { printf("Works\n"); };
};

template <class oClass, class AdoptPolicy = dont_adopt_pointer>
struct to_object {
};

template <typename T>
class to_object<T &, translate::dont_adopt_pointer> {
public:
  static core::T_sp convert(T &val) {
    int ***wrongSpecialization = T();
    printf("%p\n", wrongSpecialization);
    return core::T_sp();
  }
};

template <>
struct to_object<bool *, translate::dont_adopt_pointer> {
  typedef bool *GivenType;
  static core::T_sp convert(GivenType v) {
    //            int*** i = GivenType();
    test<bool *>::print();
    return core::T_sp();
  }
};
};

int main(int argc, const char *argv[]) {
  bool b(true);
  int i;
  translate::to_object<bool *>::convert(&b);
  translate::to_object<int &>::convert(i);
}
