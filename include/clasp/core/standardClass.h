/*
    File: standardClass.h
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
#ifndef _core_standardClass_H
#define _core_standardClass_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/stdClass.h>
#include <clasp/core/holder.h>

namespace core {

SMART(StandardClass);

SMART(StringSet);

SMART(StandardClass);
class StandardClass_O : public StdClass_O {
  LISP_META_CLASS(StandardClass);
  LISP_BASE1(StdClass_O);
  LISP_CLASS(core, ClPkg, StandardClass_O, "StandardClass");

public:
#if defined(XML_ARCHIVE)
  void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
  void initialize();
GCPROTECTED:
  Class_sp _InstanceCoreClass;

public:
  /*! Special creator used when starting up lisp environment */
  //	static StandardClass_sp create(Class_sp mc);

  /*! Special creator used when bootstrapping - the resulting will never be collected
          and will always be treated as a root */
  static StandardClass_sp createUncollectable();

  explicit StandardClass_O();
  virtual ~StandardClass_O(){};
};
};
template <>
struct gctools::GCInfo<core::StandardClass_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

class StandardClassInitializationFunctoid : public Functoid {
private:
  StandardClass_sp _StandardClass;

public:
  DISABLE_NEW();

  virtual const char *describe() const { return "StandardClassInitializationFunctoid"; };
  StandardClassInitializationFunctoid(T_sp name, StandardClass_sp c) : Functoid(name) { this->_StandardClass = c; };
  virtual ~StandardClassInitializationFunctoid(){};
};
};
TRANSLATE(core::StandardClass_O);
#endif //]
