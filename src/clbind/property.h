#ifndef clbind_property_H
#define clbind_property_H

#include <core/translators.h>

#include <clbind/clbind_wrappers.h>
#include <clbind/policies.h>
#include <clbind/details.h>

namespace clbind {


    template <typename T> struct memberpointertraits {};

    template <typename M, typename C>
    struct memberpointertraits<M C::*> {
        typedef M member_type;
        typedef C class_type;
    };



    template <typename GetterPolicies, typename OT, typename VariablePtrType >
    class GetterMethoid : public core::BuiltinClosure {
    public:
        typedef core::BuiltinClosure TemplatedBase;
    private:
        typedef typename memberpointertraits<VariablePtrType>::member_type MemberType;
        typedef clbind::Wrapper<MemberType>     WrapperType;
        VariablePtrType                 _MemberPtr;
    public:
        virtual size_t templatedSizeof() const { return sizeof(*this); };
    public:
        GetterMethoid(core::T_sp name, VariablePtrType p) : core::BuiltinClosure(name), _MemberPtr(p) {};
        DISABLE_NEW();
        void LISP_CALLING_CONVENTION() {
            OT* objPtr = (LCC_ARG0()).as<core::WrappedPointer_O>()->cast<OT>();
            MemberType& orig = (*objPtr).*(this->_MemberPtr);
            *lcc_resultP = translate::to_object<MemberType,translate::adopt_pointer>::convert(orig);
        }
    };

};

namespace clbind {
    template <typename GetterPolicies, typename OT, typename MemberType >
    class GetterMethoid<GetterPolicies,OT, MemberType*const(OT::*)> : public core::BuiltinClosure {
        typedef core::BuiltinClosure TemplatedBase;
    private:
        typedef clbind::Wrapper<MemberType>     WrapperType;
        string                          _Name;
        typedef MemberType*const(OT::*VariablePtrType);
        VariablePtrType                 _MemberPtr;
    public:
        GetterMethoid(core::T_sp name, VariablePtrType p) : BuiltinClosure(name), _MemberPtr(p) {};
        DISABLE_NEW();
        void LISP_CALLING_CONVENTION() {
            OT* objPtr = (LCC_ARG0()).as<core::WrappedPointer_O>()->cast<OT>();
            MemberType* ptr = (*objPtr).*(this->_MemberPtr);
            *lcc_resultP = translate::to_object<MemberType*,translate::dont_adopt_pointer>::convert(ptr);
        }
    };



};





template <typename GetterPolicies, typename OT, typename VariablePtrType >
class gctools::GCKind<clbind::GetterMethoid<GetterPolicies,OT,VariablePtrType> > {
public:
        static gctools::GCKindEnum const Kind = gctools::GCKind<typename clbind::GetterMethoid<GetterPolicies,OT,VariablePtrType>::TemplatedBase>::Kind;
};


#endif




