/*
    File: corePackage.h
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
#ifndef corePackage_H
#define corePackage_H

#include <clasp/core/lisp.h>
#include <clasp/core/corePackage.fwd.h>

namespace core {

extern const char *CorePkg_nicknames[];
 
 FORWARD(CoreExposer);
class CoreExposer_O : public core::Exposer_O {
  LISP_CLASS(core,CorePkg,CoreExposer_O,"CoreExposer",core::Exposer_O);
public:
  CoreExposer_O(Lisp_sp lisp);
 public:
  virtual void expose(core::Lisp_sp lisp, WhatToExpose what) const;
public:
  /*! Lisp_O::startupLispEnvironment calls this to create the core classes */
  static CoreExposer_sp create_core_packages_and_classes();

public:
  void define_essential_globals(Lisp_sp lisp);
};

void add_defsetf_access_update(Symbol_sp access_fn, Symbol_sp update_fn);
};

#endif
