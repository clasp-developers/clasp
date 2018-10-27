/*
    File: open.cc
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
// Copyright (c) 2003 Daniel Wallin and Arvid Norberg

// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
// ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
// TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
// SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
// OR OTHER DEALINGS IN THE SOFTWARE.

#define CLBIND_BUILDING

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/clbind/cl_include.h>

#include <clasp/clbind/clbind.h>
#include <clasp/clbind/class_registry.h>
#include <clasp/core/symbolTable.h>
//#include <clasp/clbind/function.h>
//#include <clasp/clbind/get_main_thread.h>

namespace clbind {

//! Take the place of __clbind_cast_graph
detail::cast_graph *globalCastGraph;
//! Take the place of __clbind_class_id_map
detail::class_id_map *globalClassIdMap;

class Test {
public:
  Test() : multiplier(1234) {};
public:
  int  multiplier;
  std::vector<int> numbers;

  void set2(int n0, int n1) {
    this->numbers.clear();
    this->numbers.push_back(n0);
    this->numbers.push_back(n1);
  }

  void set3(int n0, int n1, int n2) {
    this->numbers.clear();
    this->numbers.push_back(n0);
    this->numbers.push_back(n1);
    this->numbers.push_back(n2);
  }

  void set4(int n0, int n1, int n2, int n3) {
    this->numbers.clear();
    this->numbers.push_back(n0);
    this->numbers.push_back(n1);
    this->numbers.push_back(n2);
    this->numbers.push_back(n3);
  }

  void set5(int n0, int n1, int n2, int n3, int n4) {
    this->numbers.clear();
    this->numbers.push_back(n0);
    this->numbers.push_back(n1);
    this->numbers.push_back(n2);
    this->numbers.push_back(n3);
    this->numbers.push_back(n4);
  }

  void set6(int n0, int n1, int n2, int n3, int n4, int n5) {
    this->numbers.clear();
    this->numbers.push_back(n0);
    this->numbers.push_back(n1);
    this->numbers.push_back(n2);
    this->numbers.push_back(n3);
    this->numbers.push_back(n4);
    this->numbers.push_back(n5);
  }

  void print_numbers() {
    int idx=0;
    for (auto n : this->numbers) {
      printf("%s:%d number[%d] -> %d\n", __FILE__, __LINE__, idx, n*this->multiplier);
      ++idx;
    }
  }
};
  



CLBIND_API void initialize_clbind() {
  ClassRegistry_sp registry = ClassRegistry_O::create();
  _sym_STARtheClassRegistrySTAR->defparameter(registry);
  globalClassIdMap = new detail::class_id_map();
  globalCastGraph = new detail::cast_graph();
#if 1
  package("CLBIND-TEST",{"CLBIND-TEST"}, {"CL"})
    [
     class_<Test>("Test")
     .def_readwrite("multiplier",&Test::multiplier)
     .def("set2",&Test::set2)
     .def("set3",&Test::set3)
     .def("set4",&Test::set4)
     .def("set5",&Test::set5)
     .def("set6",&Test::set6)
     .def("print-numbers",&Test::print_numbers)
     ];
#endif
}

} // namespace clbind
 
