#ifndef	_core_Designators_H
#define _core_Designators_H

#include "core/object.h"
#include "corePackage.fwd.h"
#include "fileSystem.h"
#include "pathname.h"

namespace core
{

    namespace coerce
    {

	/* From CLHS:
	   function designator - a designator for a function; that is, an object that denotes a
	   function and that is one of: a symbol (denoting the function named by that symbol in
	   the global environment), or a function (denoting itself). The consequences are
	   undefined if a symbol is used as a function designator but it does not have a global
	   definition as a function, or it has a global definition as a macro or a special form.
	   See also extended function designator.
	*/
	extern Function_sp functionDesignator(T_sp obj);


	/*! Return a Path by interpreting a pathname designator */
	extern Path_sp pathDesignator(T_sp obj);

	/*! Return a Package by interpreting a package designator */
	extern Package_sp packageDesignator(T_sp obj );

	/*! Return the name of a Package by interpreting a package or a string as a name */
	extern string packageNameDesignator(T_sp obj );

	/*! Return a Cons of packages by interpreting as a list of package designators */
	extern Cons_sp listOfPackageDesignators(T_sp obj);

	/*! Return a String object by interpreting the object
	  as a string designator */
	extern	Str_sp stringDesignator(T_sp obj);

	/*! Return a Cons of strings by interpreting the
	  object as a list of string designators */
	extern Cons_sp listOfStringDesignators(T_sp obj);


	/*! Return a Cons of symbols by interpreting a designator for a list of symbols */
	extern Cons_sp listOfSymbols(T_sp obj);


/*! Convert an Object input stream designator (as described by CLHS) into a Stream */
	T_sp inputStreamDesignator(T_sp obj);


/*! Convert an Object output stream designator (as described by CLHS) into a Stream */
	T_sp outputStreamDesignator(T_sp obj);

    }; /* designators */


    extern void initialize_designators();
}; /* core */


#endif /* _core_Designators_H */


