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



    // Old way of doing things
	virtual void first() {_OF(); SUBCLASS_MUST_IMPLEMENT(); };
	virtual void next() {_OF(); SUBCLASS_MUST_IMPLEMENT(); };
	virtual bool isDone() {_OF(); SUBCLASS_MUST_IMPLEMENT(); };
    virtual bool notDone() { return !this->isDone(); };
    virtual T_sp currentObject() {_OF(); SUBCLASS_MUST_IMPLEMENT(); };
    template <typename OType>
    mem::smart_ptr<OType> current() { return downcast<OType>(this->currentObject());};


//	Iterator_O( const Iterator_O& ss ); //!< Copy constructor

    DEFAULT_CTOR_DTOR(Iterator_O);
};



};
TRANSLATE(core::Iterator_O);
#endif //]
