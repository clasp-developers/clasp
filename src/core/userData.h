#ifndef core_userData_H
#define core_userData_H

#include "foundation.h"
#include "object.h"
#include "lisp.h"


namespace core {

// set this class up by hand
    SMART(LightUserData);
    class LightUserData_O : public core::T_O // StandardObject_O
    {
	LISP_BASE1(core::T_O); // LISP_BASE1(StandardObject_O);
	LISP_CLASS(core,CorePkg,LightUserData_O,"LightUserData");
    public:
        void*   _ptr;
    public:
        static LightUserData_sp create(void* ptr) {
            GC_RESERVE(LightUserData_O,v);
            v->_ptr = ptr;
            return v;
        }
    public:
	virtual bool eq(core::T_sp obj) const
        {
            if (LightUserData_sp lud = obj.asOrNull<LightUserData_O>() )
            {
                return ( lud->_ptr == this->_ptr );
            }
            return false;
        }
        void* ptr() const { return this->_ptr;};
	explicit LightUserData_O() : Base(), _ptr(NULL) {};
	virtual ~LightUserData_O() {};
    };




    typedef void (*DestructUserDataFn)(void* data);


// set this class up by hand
    SMART(UserData);
    class UserData_O : public core::LightUserData_O // StandardObject_O
    {
	LISP_BASE1(core::LightUserData_O); // LISP_BASE1(StandardObject_O);
	LISP_CLASS(core,CorePkg,UserData_O,"UserData");
    private:
        DestructUserDataFn _Dtor;
    public:
        static UserData_sp create(size_t size,DestructUserDataFn dtor) {
            GC_RESERVE(UserData_O,v);
            v->_ptr = (void*)malloc(size);
            v->_Dtor = dtor;
            return v;
        }
    public:
	explicit UserData_O() : Base(), _Dtor(NULL) {};
	virtual ~UserData_O() {
            if (this->_Dtor) (this->_Dtor)(this->_ptr);
            if (this->_ptr != NULL ) {
                free(this->_ptr);
                this->_ptr = NULL;
            }
        };
    };


};


#endif
