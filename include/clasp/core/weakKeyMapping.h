/*
    File: weakKeyMapping.h
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
#ifndef _core_WeakKeyMapping_H
#define _core_WeakKeyMapping_H

#include <clasp/core/object.h>
#include <clasp/core/corePackage.fwd.h>

namespace core {
FORWARD(WeakKeyMapping);
class WeakKeyMapping_O : public T_O {
  LISP_CLASS(core, CorePkg, WeakKeyMapping_O, "WeakKeyMapping",T_O);
#if defined(OLD_SERIALIZE)
  DECLARE_SERIALIZE();
#endif // defined(OLD_SERIALIZE)
#if defined(XML_ARCHIVE)
  DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
  WeakKeyMapping_O() : _WeakObject(_Nil<T_O>(), _Nil<T_O>()){};
  WeakKeyMapping_O(T_sp key, T_sp val) : _WeakObject(key, val){};

public:
  static WeakKeyMapping_sp make(T_sp key, T_sp val);

public:
GCPRIVATE: // instance variables here
  typedef typename gctools::WeakKeyMappingPair::value_type value_type;
  gctools::WeakKeyMappingPair _WeakObject;

public: // Functions here
        /*! Return the (values key value t) of the mapping.  If not valid return (values nil nil nil) */
  T_mv keyValue() const;

  /*! Return true if the object referenced by _WeakObject still exists, otherwise return false
	 */
  bool valid() const;
};

}; /* core */

TRANSLATE(core::WeakKeyMapping_O);

#endif /* _core_WeakKeyMapping_H */
