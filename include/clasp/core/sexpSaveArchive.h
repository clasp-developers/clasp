/*
    File: sexpSaveArchive.h
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
#ifndef SEXP_SAVE_ARCHIVE_H //[
#define SEXP_SAVE_ARCHIVE_H

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/serialize.h>
#include <clasp/core/lispStream.fwd.h>
#include <clasp/core/lisp.h>

namespace core {

SMART(SexpSaveArchive);
class SexpSaveArchive_O : public SaveArchive_O {
  LISP_BASE1(SaveArchive_O);
  LISP_CLASS(core, CorePkg, SexpSaveArchive_O, "SexpSaveArchive");

public:
  void write(SNode_sp snode, HashTable_sp snodeToRef, T_sp stream);

  virtual void sexpSaveArchiveWrite(T_sp streamDesignator);

  DEFAULT_CTOR_DTOR(SexpSaveArchive_O);
};

}; // namespace core

TRANSLATE(core::SexpSaveArchive_O);
#endif //]
