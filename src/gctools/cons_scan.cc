

/*
 * Object scanner - include this and modify the following macros 
 *  to customize this code for different purposes.
 *
 *
#define SCAN_STRUCT_T int          // Type of scanning struct
#define ADDR_T mps_addr_t          // Type of addresses
#define OBJECT_SCAN fixup_objects  // Name of function
#define SCAN_BEGIN(x)              // Macro for starting scanning block
#define SCAN_END(x)                // Macro for end of scanning block
#define POINTER_FIX(field)         // Macro to fix pointer at field
#define RESULT_TYPE                // Result type 
#define RESULT_OK                  // value to return on OK - MPS_RES_OK
#define CONS_SCAN                  // Macro to turn on #ifdef inclusion of code
#define CONS_SKIP                  // Macro to turn on #ifdef inclusion of code
#define CONS_FWD                   // Macro to turn on #ifdef inclusion of code
#define EXTRA_ARGUMENTS            // Add extra arguments
 */


#ifdef CONS_SCAN
RESULT_TYPE CONS_SCAN(SCAN_STRUCT_T ss, ADDR_T client, ADDR_T limit EXTRA_ARGUMENTS) {
  //  printf("%s:%d in cons_scan client=%p limit=%p ptag_mask=0x%lx\n", __FILE__, __LINE__, client, limit, gctools::ptag_mask );
  ADDR_T original_client = client;
  SCAN_BEGIN(ss) {
    while (client<limit) {
      core::Cons_O* cons = reinterpret_cast<core::Cons_O*>(client);
      gctools::Header_s* header = (gctools::Header_s*)gctools::ConsPtrToHeaderPtr(cons);
      if ( header->_badge_stamp_wtag_mtag.consObjectP() ) {
        //        printf("%s:%d It's a regular cons\n", __FILE__, __LINE__ );
#if DEBUG_VALIDATE_GUARD
        client_validate(cons->ocar().raw_());
        client_validate(cons->cdr().raw_());
#endif
        POINTER_FIX(&cons->_Car);
        POINTER_FIX(&cons->_Cdr);
        client = reinterpret_cast<ADDR_T>((char*)client+sizeof(core::Cons_O));
      } else if (header->_badge_stamp_wtag_mtag.fwdP()) {
        //        printf("%s:%d It's a fwdP\n", __FILE__, __LINE__ );
        client = reinterpret_cast<ADDR_T>((char *)(client) + sizeof(core::Cons_O));
      } else if (header->_badge_stamp_wtag_mtag.pad1P()) {
        //        printf("%s:%d It's a pad1P\n", __FILE__, __LINE__ );
        client = reinterpret_cast<ADDR_T>((char *)(client) + gctools::Alignment());
      } else if (header->_badge_stamp_wtag_mtag.padP()) {
        //        printf("%s:%d It's a padP\n", __FILE__, __LINE__ );
        client = reinterpret_cast<ADDR_T>((char *)(client) + header->_badge_stamp_wtag_mtag.padSize());
      } else {
        printf("%s:%d CONS in cons_scan client=%p original_client=%p limit=%p\n(it's not a CONS or any of MPS fwd/pad1/pad2 car=%p cdr=%p\n", __FILE__, __LINE__, (void*)client, (void*)original_client, (void*)limit, cons->ocar().raw_(), cons->cdr().raw_());
        abort();
      }};
  } SCAN_END(ss);
  return RESULT_OK;
};
#endif // CONS_SCAN



#ifdef CONS_SKIP
ADDR_T CONS_SKIP(ADDR_T client,size_t& objectSize) {
//  printf("%s:%d in %s\n", __FILE__, __LINE__, __FUNCTION__ );
  core::Cons_O* cons = reinterpret_cast<core::Cons_O*>(client);
  gctools::Header_s* header = (gctools::Header_s*)gctools::ConsPtrToHeaderPtr(cons);
  if ( header->_badge_stamp_wtag_mtag.pad1P() ) {
    client = reinterpret_cast<ADDR_T>((char*)client+gctools::Alignment());
  } else if (header->_badge_stamp_wtag_mtag.padP() ) {
    client = reinterpret_cast<ADDR_T>((char*)client + header->_badge_stamp_wtag_mtag.padSize());
  } else {
    objectSize = sizeof(core::Cons_O);
    client = reinterpret_cast<ADDR_T>((char*)client + sizeof(core::Cons_O));
  }
  return client;
}
#endif // CONS_SKIP


#ifdef CONS_FWD
#  ifdef CONS_SKIP_IN_CONS_FWD
ADDR_T CONS_SKIP_IN_CONS_FWD(ADDR_T client);
#  endif // CONS_SKIP_IN_CONS_FWD

static void CONS_FWD(ADDR_T old_client, ADDR_T new_client) {
//  printf("%s:%d in %s\n", __FILE__, __LINE__, __FUNCTION__ );
  // I'm assuming both old and new client pointers have valid headers at this point
  CONS_SKIP_IN_CONS_FWD(old_client);
  core::Cons_O* cons = reinterpret_cast<core::Cons_O*>(old_client);
  gctools::Header_s* header = (gctools::Header_s*)gctools::ConsPtrToHeaderPtr(cons);
  header->_badge_stamp_wtag_mtag.setFwdPointer((void*)new_client);
  header->_badge_stamp_wtag_mtag.setFwdSize(sizeof(core::Cons_O));
}
#endif // CONS_FWD
