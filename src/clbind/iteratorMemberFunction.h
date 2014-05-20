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
    class IteratorMethoid : public core::Functoid {
    public:
        IteratorMethoid(const string& name, Begin begin, End end) : core::Functoid(name), _begin(begin), _end(end) {};
    private:
        typedef typename BeginReturnType<OT,Begin>::type      IteratorType;
        typedef Iterator<IteratorType,Pols>     WrappedIteratorType;
        Begin   _begin;
        End     _end;
    public:
        core::T_mv activate( core::ActivationFrame_sp closedOverFrame, int numArgs, ArgArray args )
        {
            if ( numArgs != 1 )
            {
                core::wrongNumberOfArguments(numArgs,1);
            }
            OT* objPtr = (*args).as<core::WrappedPointer_O>()->cast<OT>();
            IteratorType itBegin = ((*objPtr).*(this->_begin))();
            IteratorType itEnd = ((*objPtr).*(this->_end))();
            GC_RESERVE_VARIADIC(WrappedIteratorType,smart_itBegin,itBegin);
            GC_RESERVE_VARIADIC(WrappedIteratorType,smart_itEnd,itEnd);
            return Values(smart_itBegin,smart_itEnd);
        }


    };





};

#endif




