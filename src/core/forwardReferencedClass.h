#ifndef	_core_ForwardReferencedClass_H
#define _core_ForwardReferencedClass_H

#include "core/foundation.h"
#include "core/metaClass.h"


namespace core {

    FORWARD(ForwardReferencedClass);

/*! Clisp says that forward-referenced-class should not be a subclass of class and specializer but should be a subclass of metaobject
 * google: forward-referenced-class clisp
 http://clisp.podval.org/impnotes/mop-overview.html#forward-referenced-class-clisp
*/
    class ForwardReferencedClass_O : public Class_O
    {
        LISP_META_CLASS(StandardClass);
        LISP_BASE1(Class_O);
        LISP_CLASS(core,CorePkg,ForwardReferencedClass_O,"ForwardReferencedClass");
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
        DEFAULT_CTOR_DTOR(ForwardReferencedClass_O);
    public:
	void initialize();

    private: // instance variables here
	BuiltInClass_sp	_InstanceCoreClass;

    public: // Functions here
	void setInstanceCoreClass(BuiltInClass_sp bic);

	void defineYourSlotsFromBinderArchiveNode(ArchiveP binderNode);

    };

}; /* core */

TRANSLATE(core::ForwardReferencedClass_O);
template<> struct gctools::GCInfo<core::ForwardReferencedClass_O> {
    static bool constexpr NeedsInitialization = true;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = false;
    static bool constexpr Atomic = false;
};

#endif /* _core_ForwardReferencedClass_H */


