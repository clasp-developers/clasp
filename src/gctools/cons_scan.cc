

/*
 * Object scanner - include this and modify the following macros
 *  to customize this code for different purposes.
 *
 *
#define ADDR_T mps_addr_t          // Type of addresses
#define OBJECT_SCAN fixup_objects  // Name of function
#define POINTER_FIX(field)         // Macro to fix pointer at field
#define CONS_SCAN                  // Macro to turn on #ifdef inclusion of code
#define CONS_SKIP                  // Macro to turn on #ifdef inclusion of code
#define EXTRA_ARGUMENTS            // Add extra arguments
 */

#ifdef CONS_SCAN
ADDR_T CONS_SCAN(ADDR_T client EXTRA_ARGUMENTS) {
  //  printf("%s:%d in cons_scan client=%p limit=%p ptag_mask=0x%lx\n", __FILE__, __LINE__, client, limit, gctools::ptag_mask );
  core::Cons_O* cons = reinterpret_cast<core::Cons_O*>(client);
  gctools::Header_s* header = (gctools::Header_s*)gctools::ConsPtrToHeaderPtr(cons);
  if (header->_badge_stamp_wtag_mtag.consObjectP()) {
#if DEBUG_VALIDATE_GUARD
    client_validate(cons->car().raw_());
    client_validate(cons->cdr().raw_());
#endif
    POINTER_FIX(&cons->_Car);
    POINTER_FIX(&cons->_Cdr);
    client = reinterpret_cast<ADDR_T>((char*)client + sizeof(core::Cons_O));
  } else if (header->_badge_stamp_wtag_mtag.fwdP()) {
    client = reinterpret_cast<ADDR_T>((char*)(client) + sizeof(core::Cons_O));
  } else if (header->_badge_stamp_wtag_mtag.pad1P()) {
    client = reinterpret_cast<ADDR_T>((char*)(client) + gctools::Alignment());
  } else if (header->_badge_stamp_wtag_mtag.padP()) {
    client = reinterpret_cast<ADDR_T>((char*)(client) + header->_badge_stamp_wtag_mtag.padSize());
  } else {
    printf("%s:%d CONS in cons_scan client=%p\n(it's not a CONS or any of MPS fwd/pad1/pad2 car=%p "
           "cdr=%p\n",
           __FILE__, __LINE__, (void*)client, cons->car().raw_(), cons->cdr().raw_());
    abort();
  }
  return client;
};
#endif // CONS_SCAN

#ifdef CONS_SKIP
ADDR_T CONS_SKIP(ADDR_T client, size_t& objectSize) {
  //  printf("%s:%d in %s\n", __FILE__, __LINE__, __FUNCTION__ );
  core::Cons_O* cons = reinterpret_cast<core::Cons_O*>(client);
  gctools::Header_s* header = (gctools::Header_s*)gctools::ConsPtrToHeaderPtr(cons);
  if (header->_badge_stamp_wtag_mtag.pad1P()) {
    client = reinterpret_cast<ADDR_T>((char*)client + gctools::Alignment());
  } else if (header->_badge_stamp_wtag_mtag.padP()) {
    client = reinterpret_cast<ADDR_T>((char*)client + header->_badge_stamp_wtag_mtag.padSize());
  } else {
    objectSize = sizeof(core::Cons_O);
    client = reinterpret_cast<ADDR_T>((char*)client + sizeof(core::Cons_O));
  }
  return client;
}
#endif // CONS_SKIP
