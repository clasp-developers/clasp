#ifndef	_core_WeakKeyMapping_H
#define _core_WeakKeyMapping_H

#include "core/object.h"
#include "corePackage.fwd.h"

namespace core
{
FORWARD(WeakKeyMapping);
class WeakKeyMapping_O : public T_O
{
    LISP_BASE1(T_O);
    LISP_CLASS(core,CorePkg,WeakKeyMapping_O,"WeakKeyMapping");
#if defined(OLD_SERIALIZE)
    DECLARE_SERIALIZE();
#endif // defined(OLD_SERIALIZE)
#if defined(XML_ARCHIVE)
    DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
    WeakKeyMapping_O() : _WeakObject(gctools::tagged_backcastable_base_ptr<T_O>(_Nil<T_O>())
                                     , gctools::tagged_backcastable_base_ptr<T_O>(_Nil<T_O>())) {};
    WeakKeyMapping_O(T_sp key, T_sp val) : _WeakObject(gctools::tagged_backcastable_base_ptr<T_O>(key)
                                                       , gctools::tagged_backcastable_base_ptr<T_O>(val)) {};
public:
    static WeakKeyMapping_sp make(T_sp key, T_sp val);
public:
private: // instance variables here
    typedef gctools::tagged_backcastable_base_ptr<T_O> value_type;
    gctools::WeakKeyMappingPair<value_type>	_WeakObject;

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


