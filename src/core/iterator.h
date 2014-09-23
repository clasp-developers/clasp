/*
    File: iterator.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
#ifndef	Iterator_H //[
#define Iterator_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"


namespace core {

SMART(Iterator);
class Iterator_O : public T_O
{
    LISP_BASE1(T_O);
    LISP_CLASS(core,CorePkg,Iterator_O,"Iterator");
public:
#if defined(XML_ARCHIVE)
	void	archive(ArchiveP node);
#endif // defined(XML_ARCHIVE)
	void	initialize();
private:
public:

    // New way of doing things

    virtual T_sp unsafeElement() const { SUBIMP(); }; // if you dereference the end of the sequence - BAD THINGS WILL HAPPEN
    virtual void step() {SUBIMP();};
    virtual bool operator==(T_sp other) const {SUBIMP();};
    virtual bool operator<(T_sp other) const {SUBIMP();};
    virtual bool eql(T_sp other) const { return this->operator==(other);};

    virtual size_t templatedSizeof() const {SUBIMP();};

    // Old way of doing things
	virtual void first() {_OF(); SUBCLASS_MUST_IMPLEMENT(); };
	virtual void next() {_OF(); SUBCLASS_MUST_IMPLEMENT(); };
	virtual bool isDone() {_OF(); SUBCLASS_MUST_IMPLEMENT(); };
    virtual bool notDone() { return !this->isDone(); };
    virtual T_sp currentObject() {_OF(); SUBCLASS_MUST_IMPLEMENT(); };
    template <typename OType>
    gctools::smart_ptr<OType> current() { return downcast<OType>(this->currentObject());};


//	Iterator_O( const Iterator_O& ss ); //!< Copy constructor

    DEFAULT_CTOR_DTOR(Iterator_O);
};



};
TRANSLATE(core::Iterator_O);
#endif //]
