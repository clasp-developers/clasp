


template <class T>
Allocator_new {

    template <typename...ARGS> mem::smart_ptr<T> allocate(ARGS&&...args) {
        T* obj = new T(std::forward<ARGS...>(args...));
        return obj;
    }
};




template <class TY>
Allocator_mps {

    template <typename...ARGS> mem::smart_ptr<TY> allocate(ARGS&&...args) {
        mps_addr_t __reserve_addr(0);
        do {
            mps_res_t __gc_res = mps_reserve(&__reserve_addr,gctools::allocation_point<TY>::get(), sizeof_with_header<TY>());
            if ( __gc_res != MPS_RES_OK ) THROW_HARD_ERROR(BF("Could not allocate %s") % obj_name((gctools::GCKindEnum)(TY::static_Kind)));


        T* obj = new T(std::forward<ARGS...>(args...));
        return obj;
    }
};





       
