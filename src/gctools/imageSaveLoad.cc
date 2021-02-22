/*
    File: imageSaveLoad.cc

*/



#define DEBUG_LEVEL_FULL

//#include <llvm/Support/system_error.h>
#include <dlfcn.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>

#include <iomanip>
#include <clasp/core/foundation.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/debugger.h>
#include <clasp/llvmo/code.h>
#include <clasp/gctools/gc_boot.h>

#ifdef USE_PRECISE_GC

#if 1
#define DBG_SL(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL(_fmt_)
#endif
#if 1
#define DBG_SL2(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL2(_fmt_)
#endif
#if 1
#define DBG_SL3(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL3(_fmt_)
#endif


namespace gctools {

struct MemoryRange {
  gctools::clasp_ptr_t _Start;
  gctools::clasp_ptr_t _End;
  
  MemoryRange(gctools::clasp_ptr_t start=NULL, gctools::clasp_ptr_t end=NULL) : _Start(start), _End(end) {};
  void set(gctools::clasp_ptr_t start, gctools::clasp_ptr_t end) {
    this->_Start = start;
    this->_End = end;
  }
  bool contains(gctools::clasp_ptr_t ptr) {
    return (this->_Start<=ptr && ptr<= this->_End);
  }
};

//
// This stores the base of the image save/load memory
//
intptr_t globalImageSaveLoadBase;

//
// This stores if the client object is in image save/load memory (true) or
// from garbage collected memory (false)
//
MemoryRange globalISLBufferRange;


gctools::Header_s* clientPointerToHeaderPointer(gctools::clasp_ptr_t client)
{
  if (globalISLBufferRange.contains(client)) {
    return (gctools::Header_s*)(client - sizeof(gctools::Header_s::StampWtagMtag));
  }
  return (gctools::Header_s*)gctools::ClientPtrToBasePtr(client);
}

gctools::clasp_ptr_t headerPointerToClientPointer(gctools::Header_s* header)
{
  if (globalISLBufferRange.contains((gctools::clasp_ptr_t)header)) {
    return (gctools::clasp_ptr_t)((const char*)header + sizeof(gctools::Header_s::StampWtagMtag));
  }
  return (gctools::clasp_ptr_t)gctools::BasePtrToMostDerivedPtr<core::General_O>((void*)header);
}

#define CLIENT_PTR_TO_HEADER_PTR(_client_) clientPointerToHeaderPointer((gctools::clasp_ptr_t)_client_)
#define HEADER_PTR_TO_CLIENT_PTR(_header_) headerPointerToClientPointer((gctools::Header_s*)_header_)

clasp_ptr_t follow_forwarding_pointer(clasp_ptr_t client, uintptr_t tag, intptr_t base) {
  uintptr_t fwd_client;
  if (tag==gctools::general_tag) {
    Header_s* header = CLIENT_PTR_TO_HEADER_PTR(client);
    fwd_client = (uintptr_t)header->_stamp_wtag_mtag.fwdPointer();
  } else if (tag==gctools::cons_tag) {
    core::Cons_O* cons = (core::Cons_O*)client;
    fwd_client = (uintptr_t)cons->fwdPointer();
  } else {
    printf("%s:%d:%s Handle tag %lu\n", __FILE__, __LINE__, __FUNCTION__, tag );
    abort();
  }
  uintptr_t relative_fwd_client = fwd_client + base; // base is signed
  return (clasp_ptr_t)(relative_fwd_client | tag);
}


#define POINTER_FIX(_ptr_)\
  {\
    gctools::clasp_ptr_t *taggedP = reinterpret_cast<gctools::clasp_ptr_t *>(_ptr_);\
    if (gctools::tagged_objectp(*taggedP)) {\
      gctools::clasp_ptr_t tagged_obj = *taggedP;\
      if (gctools::tagged_objectp(*taggedP)) { \
        gctools::clasp_ptr_t obj = gctools::untag_object<gctools::clasp_ptr_t>(tagged_obj);\
        gctools::clasp_ptr_t tag = gctools::ptag<gctools::clasp_ptr_t>(tagged_obj);\
        /*printf("%s:%d fixing taggedP@%p obj-> %p tag-> 0x%lx\n", __FILE__, __LINE__, (void*)taggedP, (void*)obj, (uintptr_t)tag);*/ \
        obj = follow_forwarding_pointer(obj,(uintptr_t)tag,globalImageSaveLoadBase); \
        /*printf("%s:%d     forwarded  obj = %p \n", __FILE__, __LINE__, (void*)obj );*/ \
        *taggedP = obj;\
      };\
    };\
  }


#define SCAN_STRUCT_T int
#define ADDR_T clasp_ptr_t
#define SCAN_BEGIN(ss)
#define SCAN_END(ss)
#define RESULT_TYPE    int
#define RESULT_OK 1
#define EXTRA_ARGUMENTS

#undef DEBUG_MPS_SIZE
#define OBJECT_SKIP_IN_OBJECT_SCAN blah_blah_blah_error
#define OBJECT_SCAN isl_obj_scan
__attribute__((optnone))
#include "obj_scan.cc"
#undef OBJECT_SCAN

#define OBJECT_SKIP isl_obj_skip
__attribute__((optnone))
#include "obj_scan.cc"
#undef OBJECT_SKIP

#define OBJECT_SKIP_IN_OBJECT_FWD isl_obj_skip
#define OBJECT_FWD isl_obj_fwd
#include "obj_scan.cc"
#undef OBJECT_FWD


#define CONS_SCAN isl_cons_scan
#define CONS_SKIP isl_cons_skip
#define CONS_FWD isl_cons_fwd
#define CONS_SKIP_IN_CONS_FWD isl_cons_skip
#include "cons_scan.cc"
#undef CONS_FWD
#undef CONS_SKIP
#undef CONS_SCAN


#define WEAK_SCAN isl_weak_scan
#define WEAK_SKIP isl_weak_skip
#define WEAK_FWD isl_weak_fwd
#define WEAK_SKIP_IN_WEAK_FWD isl_weak_skip
#include "weak_scan.cc"
#undef WEAK_FWD
#undef WEAK_SKIP
#undef WEAK_SCAN

#undef SCAN_STRUCT_T
#undef ADDR_T
#undef SCAN_BEGIN
#undef SCAN_END
#undef POINTER_FIX
#undef RESULT_TYPE
#undef RESULT_OK
#undef EXTRA_ARGUMENTS


};






namespace gctools {

/* 
 * These are strings that are visible in the image save file
 */
typedef enum { Object = 0x215443454a424f21, // !OBJECT!
    Cons = 0xC0C0C0C0C0C0C0C0, 
    Roots =  0x3c2153544f4f5221, // ROOTS
    End =    0x21444e45444e4521 } ISLKind; // END


#define MAGIC_NUMBER 348235823
struct ISLFileHeader {
  size_t _Magic;
  size_t _Size;
  size_t _NumberOfObjects;
  ISLFileHeader(size_t sz,size_t num) : _Magic(MAGIC_NUMBER), _Size(sz), _NumberOfObjects(num) {};
  bool good_magic() const {
    return (this->_Magic == MAGIC_NUMBER);
  }
};


struct walker_callback_t {
  virtual void callback(clasp_ptr_t header, size_t sz) = 0;
};

};

extern "C" {
void boehm_walker_callback(void* ptr, size_t sz, void* client_data) {
  gctools::walker_callback_t* walker = (gctools::walker_callback_t*)client_data;
  int kind;
  size_t psize;
#ifdef USE_BOEHM
  kind = GC_get_kind_and_size((void*)ptr, &psize );
#endif
    // On boehm sometimes I get unknown objects that I'm trying to avoid with the next test.
  if ( kind == gctools::global_lisp_kind ||
       kind == gctools::global_cons_kind ||
       kind == gctools::global_class_kind ||
       kind == gctools::global_container_kind ||
       kind == gctools::global_code_kind ||
       kind == gctools::global_atomic_kind ||
       kind == gctools::global_strong_weak_kind ) {
    gctools::clasp_ptr_t client = HEADER_PTR_TO_CLIENT_PTR((gctools::Header_s*)ptr);
    walker->callback(client);
  }
}
};


namespace gctools {



struct ISLHeader_s {
  ISLKind     _Kind;
  size_t      _Size;
  ISLHeader_s(ISLKind k, size_t s) : _Kind(k), _Size(s) {};
};

struct ISLConsHeader_s : public ISLHeader_s {
  ISLConsHeader_s(ISLKind k, size_t s) : ISLHeader_s(k,s) {};
};

struct ISLObjectHeader_s : public ISLHeader_s {
  Header_s::StampWtagMtag _stamp_wtag_mtag;
  ISLObjectHeader_s(ISLKind k, uintptr_t sz, Header_s::StampWtagMtag swm) : ISLHeader_s(k,sz), _stamp_wtag_mtag(swm) {};
};

struct calculate_size_t : public walker_callback_t {

  size_t _total_size;
  size_t _general_count;
  size_t _cons_count;
  size_t _weak_count;
  void callback(clasp_ptr_t cheader, size_t sz) {
    Header_s* header = reinterpret_cast<Header_s*>(cheader);
    std::string str;
    // On boehm sometimes I get unknown objects that I'm trying to avoid with the next test.
    if (header->_stamp_wtag_mtag.stampP()) {
      this->_general_count++;
      clasp_ptr_t client = (clasp_ptr_t)BasePtrToMostDerivedPtr<core::General_O>((void*)header);
      size_t delta = isl_obj_skip(client,false)-client;
      // Remove the Header_s and any tail
      size_t objectSize = delta - sizeof(Header_s) - header->tail_size();
      DBG_SL2(BF("   general header@%p value: 0x%x badge: 0x%x  sz = %lu  obj_skip = %lu\n")
              % header
              % header->_stamp_wtag_mtag._value
              % header->_stamp_wtag_mtag._header_badge
              % sz % delta );
      if (delta > sz) {
        printf("%s:%d:%s  There is a size mismatch for header %p  boehm says %lu  must be larger than obj_skip says %lu and i\n", __FILE__, __LINE__, __FUNCTION__, *(clasp_ptr_t*)header, sz, delta );
        size_t delta2 = isl_obj_skip(client,true)-client;
      }
      this->_total_size += sizeof(ISLObjectHeader_s) + objectSize;
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      DBG_SL2(BF("   cons header@%p -> %p\n") % header % *(void**)header );
      this->_cons_count++;
      clasp_ptr_t client = (clasp_ptr_t)header;
      this->_total_size += sizeof(ISLConsHeader_s) + isl_cons_skip(client)-client;
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      this->_weak_count++;
      clasp_ptr_t client = (clasp_ptr_t)BasePtrToMostDerivedPtr<core::General_O>((void*)header);
      DBG_SL2(BF("weak object header %p   client: %p\n") % (void*)header % (void*)client );
      size_t delta = isl_weak_skip(client,false)-client;
      size_t objectSize = delta - sizeof(Header_s) - header->tail_size();
      this->_total_size += sizeof(ISLObjectHeader_s) + objectSize;
    }
  }

  calculate_size_t() : _total_size(0),
                       _general_count(0),
                       _cons_count(0),
                       _weak_count(0)
  {};
};


template <typename Walker>
void walk_garbage_collected_objects(Walker& walker) {
#ifdef USE_BOEHM
  GC_enumerate_reachable_objects_inner(boehm_walker_callback, (void*)&walker);
#endif
#ifdef USE_MPS
  printf("%s:%d:%s  Walk MPS objects\n", __FILE__, __LINE__, __FUNCTION__ );
#endif
}



struct copy_objects_t : public walker_callback_t {
  char* _buffer;
  size_t _NumberOfObjects;
  copy_objects_t(char* buffer) : _buffer(buffer), _NumberOfObjects(0) {}

  char* write_buffer(size_t bytes, char* source ) {
    char* addr = this->_buffer;
    memcpy((void*)this->_buffer, (const void*)source, bytes );
    this->_buffer += bytes;
    return addr;
  }
  
  void callback(clasp_ptr_t cheader) {
    Header_s* header = reinterpret_cast<Header_s*>(cheader);
    std::string str;
    // On boehm sometimes I get unknown objects that I'm trying to avoid with the next test.
    if (header->_stamp_wtag_mtag.stampP()) {
      clasp_ptr_t clientStart = (clasp_ptr_t)BasePtrToMostDerivedPtr<core::General_O>((void*)header);
      size_t bytes = isl_obj_skip(clientStart,false) - clientStart - sizeof(Header_s) - header->tail_size();
      clasp_ptr_t clientEnd = clientStart + bytes;
      ISLObjectHeader_s islheader( Object, bytes, header->_stamp_wtag_mtag );
      this->write_buffer(sizeof(ISLObjectHeader_s), (char*)&islheader ); 
      DBG_SL2(BF("   copying general %p to %p bytes: %lu\n") % header % (void*)this->_buffer % bytes );
      char* new_addr = this->write_buffer(clientEnd-clientStart,(char*)clientStart);
      this->_NumberOfObjects++;
      header->_stamp_wtag_mtag.setFwdPointer( new_addr );
      header->_stamp_wtag_mtag.setFwdSize( bytes );
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      clasp_ptr_t client = (clasp_ptr_t)header;
      size_t bytes = isl_cons_skip(client) - client;
      ISLConsHeader_s islheader( Cons, sizeof(core::Cons_O) );
      this->write_buffer(sizeof(ISLHeader_s), (char*)&islheader );
      DBG_SL2(BF("  copying cons %p to %p bytes: %lu\n") % header % (void*)this->_buffer % bytes );
      char* new_addr = this->write_buffer(bytes,(char*)header);
      this->_NumberOfObjects++;
      header->_stamp_wtag_mtag.setFwdPointer( new_addr );
      header->_stamp_wtag_mtag.setFwdSize( bytes );
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      printf("%s:%d:%s    weak_skip\n", __FILE__, __LINE__, __FUNCTION__ );
      clasp_ptr_t clientStart = (clasp_ptr_t)BasePtrToMostDerivedPtr<core::General_O>((void*)header);
      size_t bytes = isl_obj_skip(clientStart,false)-clientStart - sizeof(Header_s) - header->tail_size();
      clasp_ptr_t clientEnd = clientStart + bytes;
      ISLObjectHeader_s islheader( Object, bytes, header->_stamp_wtag_mtag );
      this->write_buffer(sizeof(ISLObjectHeader_s), (char*)&islheader ); 
      DBG_SL2(BF("   copying weak %p to %p bytes: %lu\n") % header % (void*)this->_buffer % bytes );
      char* new_addr = this->write_buffer(clientEnd-clientStart,(char*)clientStart);
      this->_NumberOfObjects++;
      header->_stamp_wtag_mtag.setFwdPointer( new_addr );
      header->_stamp_wtag_mtag.setFwdSize( bytes );
    }
  }
};

//
// walk image save/load objects that start at cur
//
template <typename Walker>
void walk_image_save_load_objects( ISLHeader_s* cur, Walker& walker) {
  while (cur->_Kind != End) {
    if (cur->_Kind == Object) {
      char* object = (char*)cur + sizeof(ISLObjectHeader_s);
      walker.callback((gctools::clasp_ptr_t) object, cur->_Size );
      cur = (ISLHeader_s*)((char*)cur + cur->_Size + sizeof(ISLObjectHeader_s));
    } else if (cur->_Kind == Cons) {
      char* object = (char*)cur + sizeof(ISLConsHeader_s);
      walker.callback((gctools::clasp_ptr_t) object, cur->_Size );
      cur = (ISLHeader_s*)((char*)cur + cur->_Size + sizeof(ISLConsHeader_s));
      
    }
  }
}





struct fixup_objects_t : public walker_callback_t {
  bool                 _saving;
  gctools::clasp_ptr_t _buffer;
  gctools::clasp_ptr_t _vtableStart;
  gctools::clasp_ptr_t _vtableEnd;
  fixup_objects_t(bool saving, gctools::clasp_ptr_t buffer, gctools::clasp_ptr_t vtableStart, gctools::clasp_ptr_t vtableEnd) :
    _saving(saving), _buffer(buffer), _vtableStart(vtableStart), _vtableEnd(vtableEnd) {};

  void callback(clasp_ptr_t cheader, size_t sz) {
    Header_s* header = reinterpret_cast<Header_s*>(cheader);
    if (!this->_saving) {
      printf("%s:%d:%s loading header: %p %s\n", __FILE__, __LINE__, __FUNCTION__, header, header->description().c_str() );
    }
    if (header->_stamp_wtag_mtag.stampP()) {
      clasp_ptr_t client = (clasp_ptr_t)HEADER_PTR_TO_CLIENT_PTR(header);
      clasp_ptr_t client_limit = client + sz;
      //
      // This is where we would fixup vtable pointers and entry-points
      //
      // 1. vtable pointers -> offset
      // 2. entry points to library code -> offset
      // 3. entry points to Code_O objects -> offset

      isl_obj_scan( 0, client, client_limit );
      if (header->preciseIsPolymorphic()) {
        gctools::clasp_ptr_t vtable = *(gctools::clasp_ptr_t*)client;
        if (this->_saving) {
          if (! (this->_vtableStart <= vtable && vtable < this->_vtableEnd ) ) {
            printf("%s:%d:%s The object @ %p %s isPolymorphic->%d vtable = %p | %p | %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, header->description().c_str(), header->preciseIsPolymorphic(), this->_vtableStart, vtable, this->_vtableEnd );
            printf("%s:%d:%s The vtable @ %p MUST be in the vtable section %p to %p\n", __FILE__, __LINE__, __FUNCTION__, vtable, this->_vtableStart, this->_vtableEnd );
          }
      //printf("%s:%d:%s The object @ %p %s isPolymorphic->%d vtable = %p | %p | %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, header->description().c_str(), header->preciseIsPolymorphic(), this->_vtableStart, vtable, this->_vtableEnd );
          intptr_t offset = (vtable-this->_vtableStart);
          *(gctools::clasp_ptr_t*)client = (gctools::clasp_ptr_t)offset;
      // printf("%s:%d:%s       vtable offset = 0x%lx\n", __FILE__, __LINE__, __FUNCTION__, offset );
        } else {
          //
          // Loading we add the vtableStart segment address
          //
          *(gctools::clasp_ptr_t*)client = (uintptr_t)vtable + this->_vtableStart;
        }
      }
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      clasp_ptr_t client = (clasp_ptr_t)header;
      clasp_ptr_t client_limit = isl_cons_skip((clasp_ptr_t)header);
      isl_cons_scan( 0, client, client_limit );
      // printf("%s:%d:%s The object @ %p %s isPolymorphic->%d\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, header->description().c_str(), header->preciseIsPolymorphic());
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      printf("%s:%d:%s   weak_skip\n", __FILE__, __LINE__, __FUNCTION__ );
      clasp_ptr_t client = (clasp_ptr_t)BasePtrToMostDerivedPtr<core::General_O>((void*)header);
      clasp_ptr_t client_limit = isl_weak_skip(client,false);
      isl_weak_scan( 0, client, client_limit );
    }
  }
};


void image_save(const std::string& filename)
{
#ifdef USE_BOEHM
  GC_stop_world_external();
#endif

  if (sizeof(ISLObjectHeader)-offsetof(ISLObjectHeader,_stamp_wtag_mtag) != sizeof(Header_s::StampWtagMtag)) {
    printf("%s:%d:%s Sanity check for headers in image save/load failed.\n"
           "The _stamp_wtag_mtag field must be the last field in ISLObjectHeader so that it is IMMEDIATELY followed by a client\n",
           __FILE__, __LINE__, __FUNCTION__ );
    abort();
  }
  DBG_SL(BF(" Entered\n" ));
  printf("%s:%d:%s WHAT AM I GOING TO DO WITH WEAK OBJECTS\n", __FILE__, __LINE__, __FUNCTION__ );
  //
  // For real save-lisp-and-die do the following (a simple 19 step plan)
  //
  // 1. Walk all objects in memory and sum their size
  // 2. Allocate that amount of memory + space for roots -> intermediate-buffer
  // 3. Walk all objects in memory
  //     (a) copy them to next position in intermediate-buffer
  //     (b) Set a forwarding pointer in the original object
  // 4. Walk all objects in intermediate-buffer and fixup tagged pointers using forwarding pointer
  // 5. Copy roots into intermediate-buffer
  // 6. Fixup pointers in roots
  //
  //   Steps 7-14 are an attempt to eliminate objects that made it into the save-image
  //      but are garbage.  I'm not sure the GC is cleaning up enough garbage.
  //      It's basically a mark-and-sweep garbage collection cycle.
  //      Steps 8-13 are identical to 1-6, they just walk different objects.
  //
  // 7. Mark objects in intermediate-buffer accessible from roots
  // 
  // 8. Walk all marked objects and sum their size
  // 9. Allocate that amount of space + space-for roots -> save-buffer
  // 10. Walk all marked objects from intermediate-buffer
  //       (a) copy them to next position in save-buffer
  //       (b) Set a forwarding pointer in the intermediate-buffer object
  // 11. Fixup pointers in save-buffer
  // 12. Copy roots into save-buffer
  // 13. Fixup roots in save-buffer
  //
  //   C++-fixup bytecode fixes up things like std::string and std::vector that
  //     have stuff stored in C++ malloc space.   It's better to eliminate these
  //     as much as possible by redesigning the classes that contain them.
  //     Change std::string to SimpleBaseString_sp and so on.
  //     Every class that needs c++-fixup will provide a function that will generate
  //     c++-fixup bytecode that when evaluated will create C++ objects in malloc memory
  //     and write pointers to those objects into the loaded objects.
  //     Every object except for Cons_O cells will need to have it's vtable pointer fixed up.
  //
  // 15. Generate c++-fixup bytecode for each object that needs it
  // 17. Generate table of contents
  // 18. Write table of contents and save-buffer
  // 19. DIE

  //
  // 1. First walk the objects in memory and sum their size.
  //

  DBG_SL(BF("1. Sum size of all objects\n"));
  calculate_size_t calc_size;
  walk_garbage_collected_objects(calc_size);
  printf("%s", (BF("   size = %lu\n") % calc_size._total_size).str().c_str());
  printf("%s", (BF("   general_count = %lu\n") % calc_size._general_count ).str().c_str());
  printf("%s", (BF("   cons_count = %lu\n") % calc_size._cons_count ).str().c_str());
  printf("%s", (BF("   weak_count = %lu\n") % calc_size._weak_count ).str().c_str());

  //
  // 2. Allocate that amount of memory + space for roots -> intermediate-buffer
  //

  size_t numberOfObjects = 0;
  size_t roots = sizeof(ISLHeader_s)* (1 + NUMBER_OF_CORE_SYMBOLS + global_symbol_count );
  size_t total_size = calc_size._total_size            // for all objects
    + roots                                            // size for roots
    + sizeof(ISLHeader_s);                             // size of last End header
  char* islbuffer = (char*)malloc( total_size );
  //
  // 3. Walk all objects in memory
  //     (a) copy them to next position in intermediate-buffer
  //     (b) Set a forwarding pointer in the original object

  DBG_SL(BF("  Copy objects to islbuffer: %p - %p bytes: %lu\n") % (void*)islbuffer % (void*)(islbuffer+total_size) % total_size );
  copy_objects_t copy_objects(islbuffer);
  walk_garbage_collected_objects(copy_objects);
  numberOfObjects = copy_objects._NumberOfObjects;

  //
  // Write out the roots
  //
  ISLHeader_s roots1( Roots, sizeof(core::T_O*) );
  copy_objects.write_buffer( sizeof(ISLHeader_s), (char*)&roots1 );
  copy_objects.write_buffer( sizeof(void*), (char*)&_lisp );
  ISLHeader_s roots2( Roots, sizeof(core::T_O*)*NUMBER_OF_CORE_SYMBOLS );
  copy_objects.write_buffer( sizeof(ISLHeader_s), (char*)&roots2 );
  copy_objects.write_buffer( sizeof(void*)*NUMBER_OF_CORE_SYMBOLS, (char*)&global_core_symbols[0] );
  ISLHeader_s roots3( Roots, sizeof(core::T_O*)*global_symbol_count );
  copy_objects.write_buffer( sizeof(ISLHeader_s), (char*)&roots3 );
  copy_objects.write_buffer( sizeof(void*)*global_symbol_count, (char*)&global_symbols[0] );
    
  ISLHeader_s end_header( End, 0 );
  copy_objects.write_buffer( sizeof(end_header), (char*)&end_header );

  //
  // 4. Walk all objects in intermediate-buffer and fixup tagged pointers using forwarding pointer
  //

  DBG_SL(BF("  Fixing objects starting at %p\n") % (void*)islbuffer);
  globalImageSaveLoadBase = (intptr_t)islbuffer;
  globalClientInImageSaveLoad = false;
  
  gctools::clasp_ptr_t start;
  gctools::clasp_ptr_t end;
  core::executableVtableSectionRange(start,end);
  fixup_objects_t fixup_objects( true, (gctools::clasp_ptr_t)islbuffer, start, end );
  walk_image_save_load_objects((ISLHeader_s*)islbuffer,fixup_objects);


  std::ofstream wf(filename, std::ios::out | std::ios::binary);
  if (!wf) {
    printf("Cannot open file %s\n", filename.c_str());
    return;
  }
  ISLFileHeader fh(total_size,numberOfObjects);
  wf.write( (char*)&fh, sizeof(fh) );
  wf.write( (char*)islbuffer, total_size );
  wf.close();
  printf("%s:%d:%s Wrote file %s - leaving image_save\n", __FILE__, __LINE__, __FUNCTION__, filename.c_str() );
  free(islbuffer);
  exit(0);
}



struct temporary_root_holder_t {
  void* _buffer;
  size_t _Number;
  void** _Cur;

  temporary_root_holder_t(size_t num) : _Number(num){
    this->_buffer = gctools::RootClassAllocator<void*>::allocateRootsAndZero(num);
    this->_Cur = &this->_buffer;
  }

  void release() {
    gctools::RootClassAllocator<void*>::freeRoots(this->_buffer);
  }

  //
  // Write a pointer into the temporary roots
  //
  void add(void* ptr) {
    *this->_Cur = ptr;
    this->_Cur++;
  }

};


int image_load(const std::string& filename )
{
  DBG_SL(BF(" image_load entered\n"));
  int fd = open(filename.c_str(),O_RDONLY);
  off_t fsize = lseek(fd, 0, SEEK_END);
  lseek(fd,0,SEEK_SET);
  void* memory = mmap(NULL, fsize, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_FILE, fd, 0);
  if (memory==MAP_FAILED) {
    close(fd);
    SIMPLE_ERROR(BF("Could not mmap %s because of %s") % filename % strerror(errno));
  }

  ISLFileHeader* islbuffer = reinterpret_cast<ISLFileHeader*>(memory);
  if (!islbuffer->good_magic()) {
    printf("The file is not an image file\n");
    exit(1);
  }
  //
  // Allocate space for temporary roots
  //

  temporary_root_holder_t root_holder(islbuffer->_NumberOfObjects);
  
  //
  // Allocate new objects for everything we just loaded and set the forwarding pointers
  //
  {
    ISLHeader_s* cur_header = reinterpret_cast<ISLHeader_s*>(islbuffer+1);
    gctools::clasp_ptr_t startVtables;
    gctools::clasp_ptr_t end;
    core::executableVtableSectionRange(startVtables,end);
    DBG_SL(BF(" allocate objects\n"));
    for ( ; cur_header->_Kind != End; cur_header = reinterpret_cast<ISLHeader_s*>((char*)(cur_header+1)+cur_header->_Size) ) {
      if (cur_header->_Kind == Object) {
        ISLObjectHeader_s* objectHeader = (ISLObjectHeader_s*)cur_header;
        gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)(objectHeader+1);
        gctools::clasp_ptr_t clientEnd = clientStart + objectHeader->_Size;
        image_save_load_init_s init(startVtables,objectHeader->_stamp_wtag_mtag,clientStart,clientEnd);
        DBG_SL(BF("---- Working with object value: 0x%x  size: %lu\n") % objectHeader->_stamp_wtag_mtag._value % (init._clientEnd-init._clientStart) );
        core::T_sp obj;
        if (objectHeader->_stamp_wtag_mtag.stampP()) {
          init._clientStart = (gctools::clasp_ptr_t)(cur_header+1)+sizeof(Header_s);
          init._clientEnd = (gctools::clasp_ptr_t)init._clientStart + cur_header->_Size;
          obj = GCObjectAllocator<core::General_O>::image_save_load_allocate(&init);
          printf("%s:%d:%s allocated general %p stamp: %u: %s\n", __FILE__, __LINE__, __FUNCTION__, obj.raw_(), header->_stamp_wtag_mtag._value, header->description().c_str() );
          gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
          header->setFwdPointer((void*)fwd);
          root_holder.add((void*)fwd);
        } else if (objectHeader->_stamp_wtag_mtag.consObjectP()) {
          obj = gctools::ConsAllocator<core::Cons_O,gctools::DoRegister>::image_save_load_allocate(&init);
          printf("%s:%d:%s allocated cons @%p\n", __FILE__, __LINE__, __FUNCTION__, obj.raw_() );
          gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
          header->setFwdPointer((void*)fwd);
          root_holder.add((void*)fwd);
        } else if (objectHeader->_stamp_wtag_mtag.weakObjectP()) {
          gctools::GCStampEnum stamp_wtag = header->stamp_wtag();
          switch (stamp_wtag) {
          case WeakBucketKind: {
            auto wobj = gctools::GCBucketAllocator<WeakBucketsObjectType>::image_save_load_allocate(&init);
            gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)wobj.raw_());
            header->setFwdPointer((void*)fwd);
            root_holder.add((void*)fwd);
            break;
          }
          case StrongBucketKind: {
            auto wobj = gctools::GCBucketAllocator<StrongBucketsObjectType>::image_save_load_allocate(&init);
            gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)wobj.raw_());
            header->setFwdPointer((void*)fwd);
            root_holder.add((void*)fwd);
            break;
          }
          default:
              printf("%s:%d:%s  Handle allocate weak objects\n", __FILE__, __LINE__, __FUNCTION__ );
              break;
          }
        } else {
          printf("%s:%d:%s Unknown object at %p\n", __FILE__, __LINE__, __FUNCTION__, header );
        }
      } else if (cur_header->_Kind == Roots) {
        printf("%s:%d:%s Handle Roots\n", __FILE__, __LINE__, __FUNCTION__ );
      } else {
        printf("%s:%d:%s Handle unknown kind\n", __FILE__, __LINE__, __FUNCTION__ );
      }
      DBG_SL(BF("Done working with cur_header@%p\n") % (void*)cur_header );
    }
    DBG_SL(BF("Done working with all objects cur_header@%p\n") % (void*)cur_header );
  }

  //
  // Walk all the objects and fixup all the pointers
  //
  DBG_SL(BF(" fixup pointers\n"));
  globalImageSaveLoadBase = -(intptr_t)islbuffer;
  globalClientInImageSaveLoad = true;

  gctools::clasp_ptr_t start;
  gctools::clasp_ptr_t end;
  core::executableVtableSectionRange(start,end);
  fixup_objects_t fixup_objects( false, (gctools::clasp_ptr_t)islbuffer, start, end );
  walk_garbage_collected_objects(fixup_objects);

  //
  // Release the temporary roots
  //
  root_holder.release();

  
  //
  // munmap the memory
  //
  int res = munmap( memory, fsize );
  if (res!=0) {
    SIMPLE_ERROR(BF("Could not munmap memory"));
  }

  printf("Leaving image_load\n");
  return 0;
}




};


#endif // USE_PRECISE_GC






/*
Copyright (c) 2014, Christian E. Schafmeister

CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

See directory 'clasp/licenses' for full details.

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



