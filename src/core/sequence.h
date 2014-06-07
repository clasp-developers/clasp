#ifndef	_core_Sequence_H
#define _core_Sequence_H

#include "core/foundation.h"
#include "core/object.h"
#include "corePackage.fwd.h"

namespace core
{

    /*! See CL:LENGTH */
    uint af_length(T_sp arg);

    FORWARD(Sequence);
    class Sequence_O : virtual public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,ClPkg,Sequence_O,"Sequence");

    public:
	void archiveBase(SNode_sp node);
    public:
	explicit Sequence_O(): T_O() {}
	virtual ~Sequence_O() {}

    private: // instance variables here


    public: // Functions here
	virtual Sequence_sp reverse() {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual Sequence_sp nreverse() {_OF(); SUBCLASS_MUST_IMPLEMENT();};

	virtual uint length() const {_OF(); SUBCLASS_MUST_IMPLEMENT();};

	virtual T_sp elt(int index) const {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual T_sp setf_elt(int index, T_sp value);


	virtual Sequence_sp subseq(int start, T_sp end) const {_G(); SUBCLASS_MUST_IMPLEMENT();};
	virtual Sequence_sp setf_subseq(int start, T_sp end, Sequence_sp new_subseq) {_G(); SUBCLASS_MUST_IMPLEMENT();};
    };


}; /* core */

TRANSLATE(core::Sequence_O);



namespace gctools {
    template<> inline bool isNilDowncastableTo<core::Sequence_O>() { return true;};
};

namespace core {
    T_sp af_reverse(Sequence_sp obj);
    T_sp af_nreverse(Sequence_sp obj);
    T_sp af_copy_seq(Sequence_sp seq);

    /* Return a valid start/end/length of a sequence or throw an error if invalid values are provided */
    size_t_pair sequenceStartEnd(const char* sourcefile, uint lineno, const char* functionName,
				 const string& packageName,
				 Sequence_sp seq, Fixnum_sp start, Fixnum_sp end);
};
#endif /* _core_Sequence_H */


