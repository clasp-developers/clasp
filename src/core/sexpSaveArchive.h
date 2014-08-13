#ifndef	SEXP_SAVE_ARCHIVE_H //[
#define	SEXP_SAVE_ARCHIVE_H

#include "foundation.h"
#include "object.h"
#include "serialize.h"
#include "lispStream.fwd.h"
#include "lisp.h"

namespace core {


    SMART(SexpSaveArchive);
    class SexpSaveArchive_O : public SaveArchive_O
    {
	LISP_BASE1(SaveArchive_O);
	LISP_CLASS(core,CorePkg,SexpSaveArchive_O,"SexpSaveArchive");
    public:
	void write(SNode_sp snode, HashTable_sp snodeToRef, T_sp stream );

	virtual void 	sexpSaveArchiveWrite( T_sp streamDesignator );

	DEFAULT_CTOR_DTOR(SexpSaveArchive_O);
    };


}; // namespace core

TRANSLATE(core::SexpSaveArchive_O);
#endif //]
