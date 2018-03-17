/*
    File: iterator.h
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
#ifndef Iterator_H //[
#define Iterator_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>

namespace core {

SMART(Iterator);
class Iterator_O : public General_O {
  LISP_CLASS(core, CorePkg, Iterator_O, "Iterator",General_O);

public:
#if defined(XML_ARCHIVE)
  void archive(ArchiveP node);
#endif // defined(XML_ARCHIVE)
  void initialize();

private:
public:
  // New way of doing things

  virtual T_sp unsafeElement() const { SUBIMP(); }; // if you dereference the end of the sequence - BAD THINGS WILL HAPPEN
  virtual void step() { SUBIMP(); };
  virtual bool operator==(T_sp other) const { SUBIMP(); };
  virtual bool operator<(T_sp other) const { SUBIMP(); };
  virtual bool eql_(T_sp other) const { return this->operator==(other); };
  virtual size_t templatedSizeof() const { SUBIMP(); };

  // Old way of doing things
CL_LISPIFY_NAME("core:begin");
CL_DEFMETHOD   virtual void first() {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  };
CL_LISPIFY_NAME("next");
CL_DEFMETHOD   virtual void next() {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  };

 CL_LISPIFY_NAME("iterator_EQ_");
 CL_DEFMETHOD   virtual bool iterator_EQ_(core::T_sp other) {
   return this->operator==(other);
  };
 size_t iterator_distance(core::T_sp other);
 
CL_LISPIFY_NAME("isDone");
CL_DEFMETHOD   virtual bool isDone() {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  };
CL_LISPIFY_NAME("notDone");
CL_DEFMETHOD   virtual bool notDone() { return !this->isDone(); };
CL_LISPIFY_NAME("currentObject");
CL_DEFMETHOD   virtual T_sp currentObject() {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  };
  template <typename OType>
    gctools::smart_ptr<OType> current() { return gctools::As<gctools::smart_ptr<OType>>(this->currentObject()); };

  //	Iterator_O( const Iterator_O& ss ); //!< Copy constructor

  DEFAULT_CTOR_DTOR(Iterator_O);
};
};
#endif //]
