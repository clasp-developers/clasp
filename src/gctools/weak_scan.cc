/*
 * Object scanner - include this and modify the following macros
 *  to customize this code for different purposes.
 *
 *
#define ADDR_T mps_addr_t          // Type of addresses
#define OBJECT_SCAN fixup_objects  // Name of function
#define POINTER_FIX(field)         // Macro to fix pointer at field
#define WEAK_SCAN             // Macro to turn on #ifdef inclusion of code
#define WEAK_SKIP             // Macro to turn on #ifdef inclusion of code
#define WEAK_FWD              // Macro to turn on #ifdef inclusion of code
 */

// !!!!! DEBUG_OBJECT_SCAN can only be on when DEBUG_GUARD_VALIDATE is on!!!!!!
// #define DEBUG_OBJECT_SCAN 1
// #define DEBUG_POINTER_BITMAPS 1

// #define DEBUG_CONTAINER_SCAN 1
// #define DEBUG_CONTAINER_POINTER_BITMAPS 1

#ifdef WEAK_SCAN
ADDR_T WEAK_SCAN(ADDR_T client EXTRA_ARGUMENTS) {
  const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s*>(WEAK_PTR_TO_HEADER_PTR(client));
  switch (header._badge_stamp_wtag_mtag._value) {
  default:
      THROW_HARD_ERROR("handle other weak kind {}", header._badge_stamp_wtag_mtag._value);
  }
  return client;
}
#endif

#ifdef WEAK_SKIP
ADDR_T WEAK_SKIP(ADDR_T client, bool dbg, size_t& objectSize) {
  const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s*>(WEAK_PTR_TO_HEADER_PTR(client));
  if (header._badge_stamp_wtag_mtag.weakObjectP()) {
    THROW_HARD_ERROR("Handle weak_obj_skip other weak kind {}", header._badge_stamp_wtag_mtag._value);
  }
  client = (ADDR_T)((char*)client + objectSize + sizeof(gctools::Header_s));
  return client;
};
#endif

#ifdef WEAK_FWD
void WEAK_FWD(ADDR_T old_client, ADDR_T new_client) {
  gctools::Header_s& header = *reinterpret_cast<gctools::Header_s*>(WEAK_PTR_TO_HEADER_PTR(old_client));
  size_t objectSize;
  ADDR_T limit = WEAK_SKIP_IN_WEAK_FWD(old_client, false, objectSize);
  size_t size = (char*)limit - (char*)old_client;
  assert(size >= Align(sizeof(weak_fwd2_s)));
  header._badge_stamp_wtag_mtag.setFwdPointer((void*)new_client);
  header._badge_stamp_wtag_mtag.setFwdSize(size);
}
#endif
