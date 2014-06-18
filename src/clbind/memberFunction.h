#ifndef clbind_memberFunction_H
#define clbind_memberFunction_H


#include <clbind/clbind_wrappers.h>
#include <clbind/policies.h>
#include <clbind/details.h>

namespace clbind {



    template <typename Pols , typename OT, typename MethodPtrType >
    class IndirectVariadicMethoid : public core::Functoid {
    public:
        typedef Functoid TemplatedBase;
    };



#include "clbind_methoids.h"
#include "clbind_static_members.h"

};

template <typename Pols , typename OT, typename MethodPtrType >
class gctools::GCKind<clbind::IndirectVariadicMethoid<Pols,OT,MethodPtrType>> {
public:
    static gctools::GCKindEnum const Kind = gctools::GCKind<typename clbind::IndirectVariadicMethoid<Pols,OT,MethodPtrType>::TemplatedBase>::Kind;
};




#endif




