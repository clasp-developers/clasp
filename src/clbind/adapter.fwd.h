#ifndef clbind_adapter_fwd_H
#define clbind_adapter_fwd_H

namespace clbind {


    inline void throwSlotsNotSupported()
    {
        SIMPLE_ERROR(BF("Slots are not supported by this class"));
    }

    /*! Add the enable_derivable type to Adapter or Derivable
      to allow Common Lisp to derive other classes from it.
    eg: See derivable.h 
    class Derivable : public ... {
    public:
        typedef int enable_derivable;
    };
    */
    template <class T>
    bool isDerivableCxxClass(typename T::enable_derivable adapter )
    {
        return true;
    }

    template <class T>
    bool isDerivableCxxClass( ... )
    {
        return false;
    }



    template <class T>
    void support_initializeSlots(int slots, typename T::enable_slots adapter )
    {
        adapter->initializeSlots(slots);
    }

    template <class T>
    void support_initializeSlots(int slots, ... )
    {
        throwSlotsNotSupported();
    }


    template <class T>
    void* support_adapterAddress( typename T::enable_slots adapter )
    {
        return adapter->address();
    }

    template <class T>
    void* support_adapterAddress( ... )
    {
        return NULL;
    }



    template <class T>
    core::T_sp support_instanceSigSet( typename T::enable_slots adapter )
    {
        return adapter->instanceSigSet();
    }

    template <class T>
    core::T_sp support_instanceSigSet( ... )
    {
        throwSlotsNotSupported();
        return _Nil<core::T_O>();
    }


    template <class T>
    core::T_sp support_instanceSig( typename T::enable_slots adapter )
    {
        return adapter->instanceSig();
    }

    template <class T>
    core::T_sp support_instanceSig( ... )
    {
        throwSlotsNotSupported();
        return _Nil<core::T_O>();
    }



    template <class T>
    core::T_sp support_instanceRef(int idx, typename T::enable_slots adapter) 
    {
        return adapter->instanceRef(idx);
    }

    template <class T>
    core::T_sp support_instanceRef(int idx, ... ) 
    {
        throwSlotsNotSupported();
        return _Nil<core::T_O>();
    }


    template <class T>
    core::T_sp support_instanceSet(int idx, core::T_sp val, typename T::enable_slots adapter) 
    {
        return adapter->instanceSet(idx,val);
    }

    template <class T>
    core::T_sp support_instanceSet(int idx, core::T_sp val, ... ) 
    {
        throwSlotsNotSupported();
        return _Nil<core::T_O>();
    }



};
#endif
