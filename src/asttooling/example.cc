/*
    File: example.cc
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

#include <memory>
#include <core/foundation.h>
#include <core/evaluator.h>
#include <clbind/clbind.h>
#include <asttooling/asttoolingPackage.h>
#include <asttooling/symbolTable.h>

namespace asttooling {
class A {
public:
  A(){};
  void a1() { printf("A::a1\n"); };
  void a2() { printf("A::a2\n"); };
  void dox() { this->x(); };
  virtual void x() { printf("A::x\n"); };
  virtual ~A(){};
};

class B : public A {
public:
  B(){};
  void b1() { printf("B::b1\n"); };
  virtual void x() { printf("B::x\n"); };
  virtual ~B(){};
};

class C : public B {
public:
  C(){};
  void c1() { printf("C::c1\n"); };
  virtual void x() { printf("C::x\n"); };
  virtual ~C(){};
};

class B_Adapter : public B, public clbind::Adapter<B_Adapter> {
public:
  B_Adapter() : B(), clbind::Adapter<B_Adapter>("B_Adapter"){};
  SYMBOL_EXPORT_SC_(AstToolingPkg, x);
  virtual void x() { core::eval::funcall(_sym_x, this->wrapper_from_this()); };
  void default_x() { this->B::x(); };
  virtual ~B_Adapter(){};
};

void initialize_example() {
  using namespace clbind;
  package(AstToolingPkg)[
    class_<A>("A")
        .def_constructor("make-a", constructor<>())
        .def("a1", &A::a1)
        .def("a2", &A::a2)
        .def("dox", &A::dox),
    class_<B, bases<A>>("B")
        .def_constructor("make-b", constructor<>())
        .def("b1", &B::b1),
    class_<C, bases<B>>("C")
        .def_constructor("make-c", constructor<>())
        .def("c1", &C::c1),
    class_<B_Adapter, B>("B-Adapter")
        .def("x", &B_Adapter::default_x)
  ];
};
};

typedef clbind::Wrapper<asttooling::A> A_wrapper;
typedef clbind::Wrapper<asttooling::B> B_wrapper;
typedef clbind::Wrapper<asttooling::C> C_wrapper;
typedef clbind::Wrapper<asttooling::B_Adapter> B_Adapter_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(A_wrapper);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(B_wrapper);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(B_Adapter_wrapper);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(C_wrapper);
