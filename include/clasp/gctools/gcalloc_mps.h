namespace gctools {
extern void bad_cons_mps_reserve_error();

template <typename RuntimeStage, typename ConsType, typename Register, typename... ARGS>
#ifdef ALWAYS_INLINE_MPS_ALLOCATIONS
__attribute__((always_inline))
#else
inline
#endif
smart_ptr<ConsType>
do_cons_allocation(mps_ap_t& allocation_point, const char* ap_name, ARGS&&... args) {
  gc::smart_ptr<ConsType> tagged_obj;
  {
    RAII_DEBUG_RECURSIVE_ALLOCATIONS((size_t)STAMPWTAG_CONS);
    // printf("%s:%d cons_mps_allocation\n", __FILE__, __LINE__ );
    mps_addr_t addr;
    ConsType* cons;
    size_t cons_size = ConsSizeCalculator<RuntimeStage, ConsType, Register>::value();
    do {
      mps_res_t res = mps_reserve(&addr, allocation_point, cons_size);
      if (res != MPS_RES_OK)
        bad_cons_mps_reserve_error();
      cons = reinterpret_cast<ConsType*>(addr);
      new (cons) ConsType(std::forward<ARGS>(args)...);
      tagged_obj = smart_ptr<ConsType>((Tagged)tag_cons(cons));
    } while (!mps_commit(allocation_point, addr, cons_size));
    MAYBE_VERIFY_ALIGNMENT((void*)addr);
    //      printf("%s:%d cons_mps_allocation addr=%p size=%lu\n", __FILE__, __LINE__, addr, sizeof(Cons));
  }
  return tagged_obj;
};

extern void bad_general_mps_reserve_error(mps_ap_t* allocation_point);

template <class PTR_TYPE, typename... ARGS>
#ifdef ALWAYS_INLINE_MPS_ALLOCATIONS
__attribute__((always_inline))
#else
inline
#endif
PTR_TYPE
general_mps_allocation(const Header_s::BadgeStampWtagMtag& the_header, size_t size, mps_ap_t& allocation_point, ARGS&&... args) {
  mps_addr_t addr;
  typedef typename PTR_TYPE::Type T;
  typedef typename GCHeader<T>::HeaderType HeadT;
  RAII_DEBUG_RECURSIVE_ALLOCATIONS((size_t)STAMP_UNSHIFT_MTAG(the_header._value));
  PTR_TYPE tagged_obj;
  T* obj;
  size_t allocate_size = AlignUp(size);
#ifdef DEBUG_GUARD
  size_t tail_size = ((rand() % 8) + 1) * Alignment();
  allocate_size += tail_size;
#endif
  HeadT* header;
  do {
    mps_res_t res = mps_reserve(&addr, allocation_point, allocate_size);
    if (res != MPS_RES_OK)
      bad_general_mps_reserve_error(&allocation_point);
    header = reinterpret_cast<HeadT*>(addr);
#ifdef DEBUG_GUARD
    memset(header, 0x00, allocate_size);
    new (header) HeadT(the_header, size, tail_size, allocate_size);
#else
    new (header) HeadT(the_header);
#endif
    obj = HeaderPtrToGeneralPtr<typename PTR_TYPE::Type>(addr);
    new (obj)(typename PTR_TYPE::Type)(std::forward<ARGS>(args)...);
    tagged_obj = PTR_TYPE(obj);
  } while (!mps_commit(allocation_point, addr, allocate_size));
  MAYBE_VERIFY_ALIGNMENT((void*)addr);
  my_thread_low_level->_Allocations.registerAllocation(the_header.unshifted_stamp(), allocate_size);
#ifdef DEBUG_VALIDATE_GUARD
  header->validate();
#endif
  globalMpsMetrics.totalMemoryAllocated += allocate_size;
  return tagged_obj;
};

template <class PTR_TYPE, typename... ARGS>
inline PTR_TYPE do_weak_allocation(size_t allocate_size, mps_ap_t& allocation_point, const char* ap_name, ARGS&&... args) {
  typedef typename PTR_TYPE::Type T;
  typedef typename GCHeader<T>::HeaderType HeadT;
  PTR_TYPE tagged_obj;
  mps_addr_t addr;
  T* obj;
  RAII_DISABLE_INTERRUPTS();
  RAII_DEBUG_RECURSIVE_ALLOCATIONS((size_t)STAMPWTAG_UNUSED);
  allocate_size = AlignUp(allocate_size);
  do {
    mps_res_t res = mps_reserve(&addr, allocation_point, allocate_size);
    if (res != MPS_RES_OK)
      throw_hard_error("Out of memory");
    GC_LOG(("allocated @%p %zu bytes\n", addr, allocate_size));
    obj = reinterpret_cast<T*>(addr);
    if (!obj)
      throw_hard_error("NULL address in allocate!");
    new (obj) T(std::forward<ARGS>(args)...);
    tagged_obj = PTR_TYPE(obj);
  } while (!mps_commit(allocation_point, addr, allocate_size));
  MAYBE_VERIFY_ALIGNMENT((void*)addr);
  my_thread_low_level->_Allocations.registerAllocation(STAMPWTAG_null, allocate_size);
  if (!obj)
    throw_hard_error("Could not allocate from GCBucketAllocator<Buckets<VT,VT,WeakLinks>>");
  GC_LOG(("malloc@%p %zu bytes\n", obj, allocate_size));
  return tagged_obj;
}
};     // namespace gctools
