#ifndef	_core_WeakReference_H
#define _core_WeakReference_H

#include "core/object.h"
#include "corePackage.fwd.h"

#if 0 // depreciate weak references - MPS has a different way of handling weak references
namespace core
{
FORWARD(WeakReference);
c l a ss WeakReference_O : public T_O
{
    L I S P_BASE1(T_O);
    L I S P_CLASS(core,CorePkg,WeakReference_O,"WeakReference");
#if defined(OLD_SERIALIZE)
    DECLARE_SERIALIZE();
#endif // defined(OLD_SERIALIZE)
#if defined(XML_ARCHIVE)
    DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
    DEFAULT_CTOR_DTOR(WeakReference_O);
public:
    static WeakReference_sp make(T_sp obj);
public:
	void initialize();

private: // instance variables here
	T_wp	_WeakObject;

public: // Functions here
	/*! Lock the reference to the object.
	  If the object was destroyed then return nil. */
	T_sp lock() const;
	
	/*! Return true if the object referenced by _WeakObject still exists, otherwise return false
	 */
	bool valid() const;

};

}; /* core */

TRANSLATE(core::WeakReference_O);

#endif
#endif /* _core_WeakReference_H */


