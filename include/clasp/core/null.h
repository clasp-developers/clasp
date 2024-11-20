#pragma once
/*
    File: null.h
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

#include <clasp/core/symbol.h>
#include <clasp/core/lispList.h>

namespace core {

FORWARD(SimpleBaseString);
FORWARD(Null);
class Null_O : public Symbol_O {
  LISP_CLASS(core, ClPkg, Null_O, "null", Symbol_O);

public:                             // ctor/dtor for classes with shared virtual base
  explicit Null_O() : Symbol_O(){}; // List_O
  explicit Null_O(SimpleBaseString_sp name) : Symbol_O(name){};
  static Null_sp create_at_boot(const string& nm);

public: // Functions here
  string __repr__() const;
};

}; // namespace core
