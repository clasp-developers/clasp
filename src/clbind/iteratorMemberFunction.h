#ifndef clbind_iteratorMemberFunction_H
#define clbind_iteratorMemberFunction_H


#include <clbind/clbind_wrappers.h>
#include <clbind/policies.h>
#include <clbind/details.h>
#include <clbind/wrapped_iterator.h>
namespace clbind {

    template <typename T, typename FN>
    struct BeginReturnType {};

    template <typename T,typename RT>
    struct BeginReturnType<T,RT(T::*)()> {
        typedef  RT type;
    };

    template <typename Pols , typename OT, typename Begin, typename End >
    class IteratorMethoid : public core::BuiltinClosure {
    public:
        typedef core::BuiltinClosure TemplatedBase;
    public:
        IteratorMethoid(core::T_sp name, Begin begin, End end) : core::BuiltinClosure(name), _begin(begin), _end(end) {};
    private:
        typedef typename BeginReturnType<OT,Begin>::type      IteratorType;
        typedef Iterator<IteratorType,Pols>     WrappedIteratorType;
        Begin   _begin;
        End     _end;
    public:
        virtual size_t templatedSizeof() const { return sizeof(*this); };
    public:
        DISABLE_NEW();
        void LISP_CALLING_CONVENTION()
        {
            if ( lcc_nargs != 1 ) core::wrongNumberOfArguments(lcc_nargs,1);
            OT* objPtr = (LCC_ARG0()).as<core::WrappedPointer_O>()->cast<OT>();
            IteratorType itBegin = ((*objPtr).*(this->_begin))();
            IteratorType itEnd = ((*objPtr).*(this->_end))();
            GC_ALLOCATE_VARIADIC(WrappedIteratorType,smart_itBegin,itBegin);
            GC_ALLOCATE_VARIADIC(WrappedIteratorType,smart_itEnd,itEnd);
            *lcc_resultP = Values(smart_itBegin,smart_itEnd);
        }


    };
};

template <typename Pols , typename OT, typename Begin, typename End >
class gctools::GCKind<clbind::IteratorMethoid<Pols,OT,Begin,End> > {
public:
        static gctools::GCKindEnum const Kind = gctools::GCKind<typename clbind::IteratorMethoid<Pols,OT,Begin,End>::TemplatedBase>::Kind;
};


#endif




