#ifndef	_core_List_H
#define _core_List_H

#include "core/foundation.h"
#include "sequence.h"
#include "numbers.fwd.h"

namespace core
{

FORWARD(List);
class List_O : public Sequence_O
{
    LISP_BASE1(Sequence_O);
    LISP_CLASS(core,ClPkg,List_O,"List");
//    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
//    DEFAULT_CTOR_DTOR(List_O);
public: // ctor/dtor for classes with shared virtual base
    explicit List_O() : T_O(), Base() {};
    virtual ~List_O() {};
public:
private: // instance variables here


public: // Functions here
    virtual List_sp revappend(T_sp tail) {_OF(); SUBCLASS_MUST_IMPLEMENT();};
    virtual List_sp nreconc(T_sp tail) {_OF(); SUBCLASS_MUST_IMPLEMENT();};

    virtual Cons_sp copyList() const {_G(); SUBCLASS_MUST_IMPLEMENT();};
    virtual T_sp onth(int idx) const {_G(); SUBCLASS_MUST_IMPLEMENT();};
    virtual T_sp onthcdr(int idx) const { _G(); SUBCLASS_MUST_IMPLEMENT();};

    virtual T_sp last(int idx=1) const {_G(); SUBCLASS_MUST_IMPLEMENT();};

};

    T_mv af_nreconc(List_sp list, T_sp tail);
    T_sp af_nth(int idx, T_sp arg);
    T_sp af_nthcdr(int idx, T_sp arg);
    

    T_sp af_copyList(T_sp arg);

    List_sp cl_nconc(Cons_sp rest);

    /*! Replace the last CONS of l with y and return l,  if l was nil return y */
    T_sp clasp_nconc(T_sp l, T_sp y);


}; /* core */

TRANSLATE(core::List_O);


namespace gctools {
    template<> inline bool isNilDowncastableTo<core::List_O>() { return true;};
};


namespace core {
    T_sp af_last(List_sp list, int n=1);
    List_sp af_nbutlast(List_sp list, Integer_sp n);
};


#endif /* _core_List_H */


