#pragma once

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

/*
    File: clbind.h
*/

/* -^- */

//
// Work with names of bindings
//
// RawName is for wrapping
namespace clbind {
struct RawName {
  std::string _raw_name;
  RawName(const std::string name) : _raw_name(name){};
};

inline std::string PrepareName(const std::string& name) { return core::lispify_symbol_name(name); }

inline std::string PrepareName(const RawName& name) { return name._raw_name; }
}; // namespace clbind

//
// Create a _raw operator at file scope level for passing raw names
//
// eg:  scope.def("foo<ThisIsATest>"_raw, ...)
//          ;; This will bind the symbol |foo<ThisIsATest>|
//
inline clbind::RawName operator""_raw(const char* arg, size_t len) { return clbind::RawName(std::string(arg, len)); }
