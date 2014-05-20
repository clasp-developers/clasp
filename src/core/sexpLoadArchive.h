#ifndef	SEXP_LOAD_ARCHIVE_H //[
#define	SEXP_LOAD_ARCHIVE_H

#include "foundation.h"
#include "object.h"
#include "serialize.h"
#include "lispStream.fwd.h"
#include "lisp.h"

namespace core {


    SMART(SexpLoadArchive);
    class SexpLoadArchive_O : public LoadArchive_O
    {
	LISP_BASE1(LoadArchive_O);
	LISP_CLASS(core,CorePkg,SexpLoadArchive_O,"SexpLoadArchive");
    public:
	virtual void 	parseFromObject( T_sp object );
	virtual void	parseFromStream( T_sp streamDesignator );

	DEFAULT_CTOR_DTOR(SexpLoadArchive_O);
    };


}; // namespace core

TRANSLATE(core::SexpLoadArchive_O);
#endif //]
