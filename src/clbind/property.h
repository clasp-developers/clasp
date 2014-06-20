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
    class GetterMethoid : public core::Functoid {
    public:
        typedef core::Functoid TemplatedBase;
    private:
        typedef typename memberpointertraits<VariablePtrType>::member_type MemberType;
        typedef clbind::Wrapper<MemberType>     WrapperType;
        string                          _Name;
        VariablePtrType                 _MemberPtr;
    public:
        virtual size_t templatedSizeof() const { return sizeof(*this); };
    public:
        GetterMethoid(const string& name, VariablePtrType p) : Functoid(name), _MemberPtr(p) {};
        DISABLE_NEW();
        virtual core::T_mv activate(core::ActivationFrame_sp closedOverFrame, int nargs, ArgArray args) {
            OT* objPtr = (*args).as<core::WrappedPointer_O>()->cast<OT>();
            MemberType& orig = (*objPtr).*(this->_MemberPtr);
            return translate::to_object<MemberType,translate::adopt_pointer>::convert(orig);
#if 0
            MemberType* copy = new MemberType(orig);
            return Values(WrapperType::create(copy,reg::registered_class<MemberType>::id));
#endif
        }
    };

};

namespace clbind {
    template <typename GetterPolicies, typename OT, typename MemberType >
    class GetterMethoid<GetterPolicies,OT, MemberType*const(OT::*)> : public core::Functoid {
        typedef core::Functoid TemplatedBase;
    private:
        typedef clbind::Wrapper<MemberType>     WrapperType;
        string                          _Name;
        typedef MemberType*const(OT::*VariablePtrType);
        VariablePtrType                 _MemberPtr;
    public:
        GetterMethoid(const string& name, VariablePtrType p) : Functoid(name), _MemberPtr(p) {};
        DISABLE_NEW();
        virtual core::T_mv activate(core::ActivationFrame_sp closedOverFrame, int nargs, ArgArray args) {
            OT* objPtr = (*args).as<core::WrappedPointer_O>()->cast<OT>();
            MemberType* ptr = (*objPtr).*(this->_MemberPtr);
            return translate::to_object<MemberType*,translate::dont_adopt_pointer>::convert(ptr);
#if 0
            return Values(WrapperType::create(ptr,reg::registered_class<MemberType>::id));
#endif
        }
    };



};





template <typename GetterPolicies, typename OT, typename VariablePtrType >
class gctools::GCKind<clbind::GetterMethoid<GetterPolicies,OT,VariablePtrType> > {
public:
        static gctools::GCKindEnum const Kind = gctools::GCKind<typename clbind::GetterMethoid<GetterPolicies,OT,VariablePtrType>::TemplatedBase>::Kind;
};


#endif




