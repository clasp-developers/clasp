#ifndef core_wrappedPointer_H
#define core_wrappedPointer_H

#include "foundation.h"
#include "object.h"
#include <core/instance.h>
#include "lisp.h"


namespace core {

// set this class up by hand
    SMART(WrappedPointer);
    class WrappedPointer_O : public core::T_O
    {
        LISP_BASE1(core::T_O);
	LISP_CLASS(core,CorePkg,WrappedPointer_O,"WrappedPointer");
    private:
        core::Class_sp _Class;
    public:
        virtual core::Class_sp _instanceClass() const { return this->_Class;};
        virtual T_sp instanceClassSet(Class_sp mc);

        void setInstanceClassUsingSymbol(core::Symbol_sp classSymbol);
    public:
        virtual bool validp() const {SUBIMP();};
	virtual bool eq(core::T_sp obj) const;
	virtual void* mostDerivedPointer() const {_OF(); SUBCLASS_MUST_IMPLEMENT();};
        Pointer_sp  address() const;
        virtual void* castTo(class_id cid) const {SUBIMP();};
        virtual class_id classId() const {SUBIMP();};
        template <typename T> T* castOrNull() const {
            return static_cast<T*>(this->castTo(reg::registered_class<T>::id));
        }
        template <typename T> T* cast() const {
            T* result = this->castOrNull<T>();
            if (!result) {
                SIMPLE_ERROR(BF("Is inheritance defined correctly? Could not cast WrappedPointer of class %s to %s class_id/from=%d/%s class_id/to=%d/%s")
                             % _rep_(this->_instanceClass())
                             % _rep_(reg::lisp_classSymbol<T>())
                             % this->classId()
                             % _rep_(reg::lisp_classSymbolFromClassId(this->classId()))
                             % reg::registered_class<T>::id
                             % _rep_(reg::lisp_classSymbolFromClassId(reg::registered_class<T>::id)) );
            }
            return result;
        }
        virtual void* pointerRelease() {SUBIMP();};
        virtual void pointerDelete() {SUBIMP();};
    public:
	explicit WrappedPointer_O() : Base()
                                    , _Class(_Nil<core::Class_O>())
        {};
	virtual ~WrappedPointer_O() {};
    };


};


#endif
