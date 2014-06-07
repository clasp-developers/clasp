#ifndef gc_gcarray_H
#define gc_gcarray_H


namespace gctools {

    template <class T, int SZ=0>
    class GCArray_moveable : public GCContainer  {
    public:
        template<class U, typename Allocator> friend class GCArray;
        typedef T               value_type;
        typedef T*              pointer_type;
        typedef value_type&     reference;
        typedef T*              iterator;
        typedef T const*        const_iterator;

        GCArray_moveable(size_t num) : _Capacity(num)
                                 , _Alive(false) {};

        template <typename...ARGS>
        GCArray_moveable(size_t numExtraArgs, ARGS&&...args) : _Capacity(numExtraArgs+sizeof...(ARGS))
                                                         , _Alive(false)
                                                         , _Data{args...} {};
        GCArray_moveable() : _Capacity(0)
                       , _Alive(false) {};

        size_t      _Capacity; // Index one beyond the total number of elements allocated
        bool        _Alive; // Indicate if the data is scannable or not
        T           _Data[SZ]; // Store _Capacity numbers of T structs/classes starting here

    public:
        size_t capacity() const { return this->_Capacity;};
        value_type* data() { return this->_Data; };
        value_type& operator[](size_t i) { return this->_Data[i]; };    
        const value_type& operator[](size_t i) const { return this->_Data[i]; };
        iterator begin() { return &this->_Data[0]; };
        iterator end() { return &this->_Data[this->_Capacity]; };
        const_iterator begin() const { return &this->_Data[0]; };
        const_iterator end() const { return &this->_Data[this->_Capacity]; };
    };


    template <class T, typename Allocator>
    class GCArray  {
#if defined(USE_MPS) && !defined(RUNNING_GC_BUILDER)
        friend GC_RESULT (::obj_scan)(mps_ss_t GC_SCAN_STATE, mps_addr_t base, mps_addr_t limit);
#endif
    public:
        // Only this instance variable is allowed
        GCArray_moveable<T>*  _Contents;
    public:
        typedef Allocator                           allocator_type;
        typedef T                                   value_type;
        typedef T*                                  pointer_type;
        typedef pointer_type                        iterator;
        typedef T const*                            const_iterator;
        typedef T&                                  reference;
        typedef GCArray<T,Allocator>              my_type;
        typedef GCArray_moveable<T>                     impl_type;
        typedef GCArray_moveable<T>*                   pointer_to_moveable;
    private:
        GCArray<T,Allocator>(const GCArray<T,Allocator>& other); // disable copy ctor
        GCArray<T,Allocator>& operator=(const GCArray<T,Allocator>&); // disable assignment
    public:
        void swap(my_type& other)
        {
            pointer_to_moveable op = other._Contents;
            other._Contents = this->_Contents;
            this->_Contents = op;
        }

        pointer_to_moveable contents() const { return this->_Contents; };
    private:
        T& errorEmpty() {
            THROW_HARD_ERROR(BF("GCArray had no contents"));
        };
        const T& errorEmpty() const {
            THROW_HARD_ERROR(BF("GCArray had no contents"));
        };

    public:

        GCArray() : _Contents(NULL) {};
        ~GCArray() 
        {
#ifdef USE_REFCOUNT
            if ( this->_Contents != NULL ) {
                Allocator alloc;
                GCArray_moveable<T>* ptr = this->_Contents;
                this->_Contents = NULL;
                alloc.deallocate(ptr,ptr->_Capacity); // When USE_MPS - does nothing
            }
#endif
        }




        template <typename...ARGS>
        void allocate(size_t numExtraArgs, const value_type& initialElement, ARGS&&...args)
        {
            if ( this->_Contents != NULL ) {
                THROW_HARD_ERROR(BF("GCArray allocate called and array has contents"));
            }
            allocator_type alloc;
            pointer_to_moveable implAddress = alloc.allocate(sizeof...(ARGS)+numExtraArgs);
            new (implAddress) GCArray_moveable<value_type,sizeof...(ARGS)>(numExtraArgs, std::forward<ARGS>(args)...);
            for ( size_t i(sizeof...(ARGS)); i<(sizeof...(ARGS)+numExtraArgs); ++i ) {
                T* p = &((*implAddress)[i]);
                alloc.construct(p,initialElement);
            }
            implAddress->_Alive = true;
            this->_Contents = implAddress;
        }            


        size_t capacity() const { return this->_Contents ? this->_Contents->_Capacity : 0; };
        bool alivep() const { return this->_Contents ? this->_Contents->_Alive : false; };

        T& operator[](size_t n) { return this->_Contents ? (*this->_Contents)[n] : this->errorEmpty();};
        const T& operator[](size_t n) const { return this->_Contents ? (*this->_Contents)[n] : this->errorEmpty();};
    
        pointer_type data() const { return this->_Contents ? this->_Contents->data() : NULL;};

        iterator begin() {return this->_Contents ? &(*this->_Contents)[0] : NULL ;}
        iterator end() {return this->_Contents ? &(*this->_Contents)[this->_Contents->_Capacity] : NULL; };

        const_iterator begin() const {return this->_Contents ? &(*this->_Contents)[0] : NULL ;}
        const_iterator end() const {return this->_Contents ? &(*this->_Contents)[this->_Contents->_Capacity] : NULL;}

    };


    template <typename Array>
    void Array0_dump(const Array& v, const char* head="" )
    {
        printf("%s Array0@%p _Alive[%d] _C[%lu]", head, v.contents(), v.alivep(), v.capacity() );
        size_t i;
        for ( i=0; i<v.capacity(); ++i ) {
            printf("[%lu]=", i);
            printf("%s ", _rep_(v[i]).c_str());
        }
        printf("\n");
    }    


            
} // namespace gctools

#endif
