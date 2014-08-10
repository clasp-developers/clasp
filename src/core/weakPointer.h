#ifndef	_core_WeakPointer_H
#define _core_WeakPointer_H

#include "core/object.h"
#include "corePackage.fwd.h"

namespace core
{
FORWARD(WeakPointer);
class WeakPointer_O : public T_O
{
    LISP_BASE1(T_O);
    LISP_CLASS(core,CorePkg,WeakPointer_O,"WeakPointer");
#if defined(OLD_SERIALIZE)
    DECLARE_SERIALIZE();
#endif // defined(OLD_SERIALIZE)
#if defined(XML_ARCHIVE)
    DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
    WeakPointer_O() : _WeakObject(gctools::tagged_backcastable_base_ptr<T_O>(_Nil<T_O>())) {};
    WeakPointer_O(T_sp ptr) : _WeakObject(gctools::tagged_backcastable_base_ptr<T_O>(ptr)) {};
public:
    static WeakPointer_sp make(T_sp obj);
public:
private: // instance variables here
    typedef typename gctools::WeakPointerManager::value_type value_type;
    gctools::WeakPointerManager	_WeakObject;

public: // Functions here
	/*! Value the reference to the object.
	  If the object was destroyed then return nil. */
	T_mv value() const;
	
	/*! Return true if the object referenced by _WeakObject still exists, otherwise return false
	 */
	bool valid() const;

};

}; /* core */

TRANSLATE(core::WeakPointer_O);

#endif /* _core_WeakPointer_H */


