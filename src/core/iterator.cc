#define	DEBUG_LEVEL_FULL

#include "common.h"
#include "iterator.h"
#include "lisp.h"
#include "wrappers.h"



namespace core {



    
    
#define ARGS_af_iteratorUnsafeElement "(it)"
#define DECL_af_iteratorUnsafeElement ""
#define DOCS_af_iteratorUnsafeElement "iteratorUnsafeElement"
    T_sp af_iteratorUnsafeElement(Iterator_sp it)
    {_G();
        return it->unsafeElement();
    };




    
    
#define ARGS_af_iteratorStep "(it)"
#define DECL_af_iteratorStep ""
#define DOCS_af_iteratorStep "iteratorStep"
    Iterator_sp af_iteratorStep(Iterator_sp it)
    {_G();
        it->step();
        return it;
    };




    
    






    void	Iterator_O::initialize()
    {
        this->Base::initialize();
    }

#if defined(XML_ARCHIVE)
    void	Iterator_O::archive(ArchiveP node)
    {
        IMPLEMENT_ME();
    }
#endif // defined(XML_ARCHIVE)

    void Iterator_O::exposeCando(Lisp_sp lisp)
    {
	class_<Iterator_O>()
            .def("core:begin",&Iterator_O::first)
            .def("next",&Iterator_O::next)
            .def("isDone",&Iterator_O::isDone)
            .def("notDone",&Iterator_O::notDone)
            .def("currentObject",&Iterator_O::currentObject)
            ;
//	def("create_Iterator",&Iterator_O::create);
        Defun(iteratorStep);
        Defun(iteratorUnsafeElement);
    }

    void Iterator_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef	USEBOOSTPYTHON //[
	PYTHON_CLASS(CorePkg,Iterator,"","",_lisp)
            .def("core:begin",&Iterator_O::first)
            .def("next",&Iterator_O::next)
            .def("isDone",&Iterator_O::isDone)
            .def("notDone",&Iterator_O::notDone)
            .def("currentObject",&Iterator_O::currentObject)
            ;
#endif //]
    }


    EXPOSE_CLASS(core,Iterator_O);
};




