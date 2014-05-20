#include "userData.h"
#include "wrappers.h"

namespace core {


    EXPOSE_CLASS(core,LightUserData_O);


    void LightUserData_O::exposeCando(core::Lisp_sp e)
    {
        class_<LightUserData_O>()
            ;
    }

    void LightUserData_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef	USEBOOSTPYTHON //[
	PYTHON_CLASS(CorePkg,LightUserData,"","",_lisp)
            ;
#endif //]
    }



    EXPOSE_CLASS(core,UserData_O);




    void UserData_O::exposeCando(core::Lisp_sp e)
    {
        class_<UserData_O>()
            ;
    }

    void UserData_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef	USEBOOSTPYTHON //[
	PYTHON_CLASS(CorePkg,UserData,"","",_lisp)
            ;
#endif //]
    }





};
