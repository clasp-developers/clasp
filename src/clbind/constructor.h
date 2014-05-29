#ifndef clbind_constructor_H
#define clbind_constructor_H


#include <clbind/adapter.fwd.h>
#include <clbind/clbind_wrappers.h>
#include <clbind/policies.h>
#include <clbind/adapter.h>
#include <clbind/details.h>

namespace clbind {

    template <typename...SIGS>
    struct constructor {};

    typedef enum {no_default_constructor} no_default_constructor_type;

    typedef constructor<> default_constructor;




    template <typename T, typename Pointer>
    class DefaultConstructorCreator : public core::Creator
    {
    public:
        typedef Wrapper<T,Pointer>  WrapperType;
        core::Symbol_sp _mostDerivedClassSymbol;
        int _Kind;
        int _duplicationLevel;
    public:
        virtual GC_RESULT onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
        {
#ifdef USE_MPS
            MPS_SCAN_BEGIN(GC_SCAN_STATE) {
                SMART_PTR_FIX(this->_mostDerivedClassSymbol);
            } MPS_SCAN_END(GC_SCAN_STATE);
#endif
            return GC_RES_OK;
        }
    public:
        DISABLE_NEW();
        DefaultConstructorCreator() : _mostDerivedClassSymbol(reg::lisp_classSymbol<T>())
                                             , _Kind(gctools::GCInfo<WrapperType>::Kind)
                                             , _duplicationLevel(0) {};
        DefaultConstructorCreator(core::Symbol_sp cn, int kind, int dupnum)
            : _mostDerivedClassSymbol(cn)
            , _Kind(kind)
            , _duplicationLevel(dupnum) {};

        /*! If this is the allocator for the original Adapter class return true - otherwise false */
        virtual int duplicationLevel() const { return this->_duplicationLevel;};
        void describe() const {
            stringstream ss;
            core::Symbol_sp baseClassSymbol = reg::lisp_classSymbol<T>();
            ss << "DefaultConstructorCreator for class " << _rep_(baseClassSymbol);
            if ( baseClassSymbol != this->_mostDerivedClassSymbol ) {
                ss << " derived class " << _rep_(this->_mostDerivedClassSymbol);
            }
            printf("%s",ss.str().c_str());
        }
        core::T_sp allocate()
        {
            T* naked_ptr(new T());
//            printf("%s:%d - creating WrapperType\n", __FILE__,__LINE__);
            mem::smart_ptr<WrapperType> retval = WrapperType::create(naked_ptr,reg::registered_class<T>::id);
//            clbind::support_enable_wrapper_from_this<T,Pointer>(retval,naked_ptr,naked_ptr);
            return retval;
        }
        Creator* duplicateForClassName(core::Symbol_sp className) {
            Creator* allocator = gctools::allocateCreator<DefaultConstructorCreator<T,Pointer>>(className,this->_Kind,this->_duplicationLevel+1);
            return allocator;
        }
    };












    template <typename T>
    class DerivableDefaultConstructorCreator : public core::Creator
    {
    public:
        core::Symbol_sp _mostDerivedClassSymbol;
        int _Kind;
        int _duplicationLevel;
    public:
        DISABLE_NEW();
        virtual GC_RESULT onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
        {
#ifdef USE_MPS
            MPS_SCAN_BEGIN(GC_SCAN_STATE) {
                SMART_PTR_FIX(this->_mostDerivedClassSymbol);
            } MPS_SCAN_END(GC_SCAN_STATE);
#endif
            return GC_RES_OK;
        }
    public:
        DerivableDefaultConstructorCreator() : _mostDerivedClassSymbol(reg::lisp_classSymbol<T>())
                                                      , _Kind(gctools::GCInfo<T>::Kind)
                                                      , _duplicationLevel(0) {};
        DerivableDefaultConstructorCreator(core::Symbol_sp cn, int kind, int dupnum)
            : _mostDerivedClassSymbol(cn)
            , _Kind(kind)
            , _duplicationLevel(dupnum) {};

        /*! If this is the allocator for the original Adapter class return true - otherwise false */
        virtual int duplicationLevel() const { return this->_duplicationLevel;};
        void describe() const {
            stringstream ss;
            core::Symbol_sp baseClassSymbol = reg::lisp_classSymbol<T>();
            ss << "DerivableDefaultConstructorCreator for class " << _rep_(baseClassSymbol);
            if ( baseClassSymbol != this->_mostDerivedClassSymbol ) {
                ss << " derived class " << _rep_(this->_mostDerivedClassSymbol);
            }
            printf("%s",ss.str().c_str());
        }
        core::T_sp allocate()
        {
//            printf("%s:%d Allocating instance of Derivable class: %s\n", __FILE__, __LINE__, _rep_(this->_mostDerivedClassSymbol).c_str());
            GC_ALLOCATE(T,obj);
//            printf("%s:%d obj.px_ref() = %p\n", __FILE__, __LINE__, obj.px_ref());
//            printf("%s:%d obj.px_ref()->pointerToAlienWithin() = %p\n", __FILE__, __LINE__, obj.px_ref()->pointerToAlienWithin());
//            printf("%s:%d typeid(obj.px_ref())@%p  typeid(obj.px_ref()).name=%s\n", __FILE__, __LINE__, &typeid(obj.px_ref()),typeid(obj.px_ref()).name());

//            clbind::support_enable_wrapper_from_this<T,Pointer>(retval,naked_ptr,naked_ptr);
            return obj;
        }
        Creator* duplicateForClassName(core::Symbol_sp className) {
            Creator* allocator = gctools::allocateCreator<DerivableDefaultConstructorCreator<T>>(className,this->_Kind,this->_duplicationLevel+1);
            return allocator;
        }
    };








    template <typename Policies, typename T>
    class DerivableDefaultConstructorFunctoid : public core::Functoid {
    public:
        enum { NumParams = 0 };
        DerivableDefaultConstructorFunctoid(const string& name) : core::Functoid(name) {};
        DISABLE_NEW();
        core::T_mv activate( core::ActivationFrame_sp closedOverFrame, int numArgs, ArgArray args )
        {
            int countPureOutValues = CountPureOutValues<Policies>::value;
            if ( numArgs != (NumParams-countPureOutValues) )
            {
                core::wrongNumberOfArguments(numArgs,(NumParams-countPureOutValues));
            }
            GC_ALLOCATE(T,obj_gc_safe);
            printf("%s:%d Allocating instance of Derivable class: %s\n", __FILE__, __LINE__, this->_Name.c_str() );
            int oidx = 1;
            return mem::multiple_values<core::T_O>(obj_gc_safe,oidx);
        }
    };






    template <typename Pols, typename Pointer, typename T, typename Sig>
    class VariadicConstructorFunctoid : public core::Functoid {};


#include "clbind_constructor_functoids.h"




};

#endif




