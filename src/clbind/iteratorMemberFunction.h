/*
    File: iteratorMemberFunction.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
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
