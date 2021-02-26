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
#include <filesystem>

#include <iomanip>
#include <clasp/core/foundation.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/debugger.h>
#include <clasp/core/functor.h>
#include <clasp/llvmo/code.h>
#include <clasp/gctools/gc_boot.h>

#ifdef USE_PRECISE_GC

#if 1
#define DBG_SL(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL(_fmt_)
#endif
#if 0
#define DBG_SL1(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL1(_fmt_)
#endif
#if 1
#define DBG_SL_ALLOCATE(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_ALLOCATE(_fmt_)
#endif
#if 1
#define DBG_SAVECOPY(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SAVECOPY(_fmt_)
#endif

#if 1
#define DBG_SL_ROOT(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_ROOT(_fmt_)
#endif

#if 1
#define DBG_SL_FWD(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_FWD(_fmt_)
#endif

#if 1
#define DBG_SL_FFWD(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_FFWD(_fmt_)
#endif

#if 1
#define DBG_SL_RELOCATE0(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_RELOCATE0(_fmt_)
#endif

#if 1
#define DBG_SL_WALK(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_WALK(_fmt_)
#endif

#if 1
#define DBG_SL_VTABLE(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_VTABLE(_fmt_)
#endif



namespace imageSaveLoad {

struct ISLLibrary {
  std::string _Name;
  gctools::clasp_ptr_t _TextStart;
  gctools::clasp_ptr_t _TextEnd;
  bool  _IsExecutable;
  ISLLibrary(const std::string& name, gctools::clasp_ptr_t start, gctools::clasp_ptr_t end, bool isexec)
    : _Name(name),
      _TextStart(start),
      _TextEnd(end),
      _IsExecutable(isexec) {};
  
};

std::vector<ISLLibrary> globalISLLibraries;

void* encodePointer(gctools::clasp_ptr_t address,size_t idx, gctools::clasp_ptr_t start) {
  uintptr_t offset = (uintptr_t)address - (uintptr_t)start;
  uintptr_t result = idx<<48 | offset;
  return (void*)result;
}

void* encodeLibrarySaveAddress(void* vaddress) {
  gctools::clasp_ptr_t address = (gctools::clasp_ptr_t)vaddress;
  for (size_t idx = 0; idx<globalISLLibraries.size(); idx++ ) {
    if (globalISLLibraries[idx]._TextStart<=address && address<globalISLLibraries[idx]._TextEnd) {
      return encodePointer(address,idx,globalISLLibraries[idx]._TextStart);
    }
  }
  gctools::clasp_ptr_t start;
  gctools::clasp_ptr_t end;
  std::string libraryPath;
  bool isExecutable;
  core::lookup_address_in_library(address,start,end,libraryPath,isExecutable);
  std::string libraryFilename = std::filesystem::path(libraryPath).filename();
  ISLLibrary lib(libraryFilename,start,end,isExecutable);
  size_t idx = globalISLLibraries.size();
  globalISLLibraries.push_back(lib);
  return encodePointer(address,idx,start);
}


void* decodeLibrarySaveAddress(void* codedAddress) {
  uintptr_t offset = (uintptr_t)codedAddress & (((uintptr_t)1<<48) - 1);
  uintptr_t libidx = (uintptr_t)codedAddress>>48;
  gctools::clasp_ptr_t ptr = globalISLLibraries[libidx]._TextStart + offset;
  return (void*)ptr;
}



void* encodeEntryPointSaveAddress(void* vaddress, llvmo::CodeBase_sp code) {
  uintptr_t address = (uintptr_t)vaddress;
  return (void*)(address - code->codeStart());
}

void* decodeEntryPointSaveAddress(void* vaddress, llvmo::CodeBase_sp code) {
  uintptr_t address = (uintptr_t)vaddress;
  return (void*)(address + code->codeStart());
}


};

namespace imageSaveLoad {

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


gctools::Header_s* generalPointerToHeaderPointer(gctools::clasp_ptr_t general)
{
  if (globalISLBufferRange.contains(general)) {
    return (gctools::Header_s*)(general - sizeof(gctools::Header_s::StampWtagMtag));
  }
  return (gctools::Header_s*)gctools::GeneralPtrToHeaderPtr(general);
}

gctools::clasp_ptr_t headerPointerToGeneralPointer(gctools::Header_s* header)
{
  if (globalISLBufferRange.contains((gctools::clasp_ptr_t)header)) {
    return (gctools::clasp_ptr_t)((const char*)header + sizeof(gctools::Header_s::StampWtagMtag));
  }
  return (gctools::clasp_ptr_t)gctools::HeaderPtrToGeneralPtr<core::General_O>((void*)header);
}

#define GENERAL_PTR_TO_HEADER_PTR(_general_) generalPointerToHeaderPointer((gctools::clasp_ptr_t)_general_)
#define HEADER_PTR_TO_GENERAL_PTR(_header_) headerPointerToGeneralPointer((gctools::Header_s*)_header_)

typedef gctools::clasp_ptr_t (*PointerFix)(gctools::clasp_ptr_t client, uintptr_t tag);
PointerFix globalPointerFix;


gctools::clasp_ptr_t follow_forwarding_pointer(gctools::clasp_ptr_t client, uintptr_t tag) {
  uintptr_t fwd_client;
  gctools::Header_s* header;
  if (tag==gctools::general_tag) {
    header = GENERAL_PTR_TO_HEADER_PTR(client);
    fwd_client = (uintptr_t)header->_stamp_wtag_mtag.fwdPointer();
  } else if (tag==gctools::cons_tag) {
    header = (gctools::Header_s*)client;
    fwd_client = (uintptr_t)header->_stamp_wtag_mtag.fwdPointer();
  } else {
    header = (gctools::Header_s*)gctools::WeakPtrToHeaderPtr(client);
    fwd_client = (uintptr_t)header->_stamp_wtag_mtag.fwdPointer();
  }
  DBG_SL_FFWD(BF("fwdPointer from %p  to header@%p  %p\n") % (void*)((uintptr_t)client | tag) % (void*)header % (void*)((uintptr_t)fwd_client | tag ));
  return (gctools::clasp_ptr_t)(fwd_client | tag);
}


#define POINTER_FIX(_ptr_) {\
    gctools::clasp_ptr_t *taggedP = reinterpret_cast<gctools::clasp_ptr_t *>(_ptr_);\
    if (gctools::tagged_objectp(*taggedP)) {\
      gctools::clasp_ptr_t tagged_obj = *taggedP;\
      if (gctools::tagged_objectp(*taggedP)) { \
        gctools::clasp_ptr_t obj = gctools::untag_object<gctools::clasp_ptr_t>(tagged_obj);\
        uintptr_t tag = (uintptr_t)gctools::ptag<gctools::clasp_ptr_t>(tagged_obj);\
        /*printf("%s:%d fixing taggedP@%p obj-> %p tag-> 0x%lx\n", __FILE__, __LINE__, (void*)taggedP, (void*)obj, (uintptr_t)tag);*/ \
        obj = (globalPointerFix)(obj,tag); \
        /*printf("%s:%d     forwarded  obj = %p \n", __FILE__, __LINE__, (void*)obj );*/ \
        *taggedP = obj;\
      };\
    };\
  }



#define SCAN_STRUCT_T int
#define ADDR_T gctools::clasp_ptr_t
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
#undef RESULT_TYPE
#undef RESULT_OK
#undef EXTRA_ARGUMENTS



//
// Fix root pointers by following the forwarding pointer
//
void fixRootPointers(gctools::clasp_ptr_t* start, size_t number) {
  for ( size_t idx = 0; idx<number; idx++ ) {
    gctools::clasp_ptr_t before = *start;
    POINTER_FIX(start);
    gctools::clasp_ptr_t after = *start;
    DBG_SL_ROOT(BF("Fixed root pointer %d from %p to %p\n") % idx % (void*)before % (void*)after );
    start++;
  }
}

#undef POINTER_FIX

};






namespace imageSaveLoad {

/* 
 * These are strings that are visible in the image save file
 */
typedef enum { Object = 0x215443454a424f21, // !OBJECT!
               Cons = 0xC0C0C0C0C0C0C0C0,
               Weak = 0xD0D0D0D0,
               Library = 0x999999,
               Roots =  0x3c2153544f4f5221, // ROOTS
               End =    0x21444e45444e4521 } ISLKind; // END


#define MAGIC_NUMBER 348235823
struct ISLFileHeader {
  size_t _Magic;
  size_t _Size;
  size_t _LibrariesOffset;
  size_t _NumberOfLibraries;
  uintptr_t _SaveBufferStart;
  size_t _NumberOfObjects;
  size_t _LispRootOffset;
  size_t _LispRootCount;
  size_t _CoreSymbolRootsOffset;
  size_t _CoreSymbolRootsCount;
  size_t _SymbolRootsOffset;
  size_t _SymbolRootsCount;
  ISLFileHeader(size_t sz,size_t num, uintptr_t sbs) : _Magic(MAGIC_NUMBER), _Size(sz), _NumberOfObjects(num), _SaveBufferStart(sbs) {};
  bool good_magic() const {
    return (this->_Magic == MAGIC_NUMBER);
  }
};


struct walker_callback_t {
  virtual void callback(gctools::Header_s* header) = 0;
};

};

extern "C" {
void boehm_walker_callback(void* ptr, size_t sz, void* client_data) {
  imageSaveLoad::walker_callback_t* walker = (imageSaveLoad::walker_callback_t*)client_data;
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
    DBG_SL1(BF("Walking to GC managed header %p\n") % (void*)ptr );
    walker->callback((gctools::Header_s*)ptr);
  }
}
};


namespace imageSaveLoad {



struct ISLHeader_s {
  ISLKind     _Kind;
  size_t      _Size;
  ISLHeader_s(ISLKind k, size_t s) : _Kind(k), _Size(s) {};
};

struct ISLEndHeader_s : public ISLHeader_s {
  ISLEndHeader_s(ISLKind k) : ISLHeader_s(k,0) {};
  ISLHeader_s* next() const { return (ISLHeader_s*)((char*)this+sizeof(*this)); };
};


struct ISLRootHeader_s : public ISLHeader_s {
  ISLRootHeader_s(ISLKind k, size_t s) : ISLHeader_s(k,s) {};
  ISLHeader_s* next() const { return (ISLHeader_s*)((char*)this+sizeof(*this)+this->_Size); };
};
  
struct ISLConsHeader_s : public ISLHeader_s {
  ISLConsHeader_s(ISLKind k, size_t s) : ISLHeader_s(k,s) {};
  ISLHeader_s* next() const { return (ISLHeader_s*)((char*)this+sizeof(*this)+this->_Size); };
};

struct ISLWeakHeader_s : public ISLHeader_s {
  gctools::Header_s::StampWtagMtag _stamp_wtag_mtag;
  ISLWeakHeader_s(ISLKind k, uintptr_t sz, gctools::Header_s::StampWtagMtag swm) : ISLHeader_s(k,sz), _stamp_wtag_mtag(swm) {};
  ISLHeader_s* next() const { return (ISLHeader_s*)((char*)this+sizeof(*this)+this->_Size); };
};

struct ISLGeneralHeader_s : public ISLHeader_s {
  gctools::Header_s::StampWtagMtag _stamp_wtag_mtag;
  ISLGeneralHeader_s(ISLKind k, uintptr_t sz, gctools::Header_s::StampWtagMtag swm) : ISLHeader_s(k,sz), _stamp_wtag_mtag(swm) {};
  ISLHeader_s* next() const { return (ISLHeader_s*)((char*)this+sizeof(*this)+this->_Size); };
};

struct ISLLibraryHeader_s : public ISLHeader_s {
  ISLLibraryHeader_s(ISLKind k, size_t s) : ISLHeader_s(k,s) {};
  ISLHeader_s* next() const { return (ISLHeader_s*)((char*)this+sizeof(*this)+this->_Size); };
};

struct calculate_size_t : public walker_callback_t {

  size_t _total_size;
  size_t _general_count;
  size_t _cons_count;
  size_t _weak_count;
  void callback(gctools::Header_s* header) {
    std::string str;
    // On boehm sometimes I get unknown objects that I'm trying to avoid with the next test.
    if (header->_stamp_wtag_mtag.stampP()) {
      this->_general_count++;
      gctools::clasp_ptr_t client = HEADER_PTR_TO_GENERAL_PTR(header);
      size_t objectSize;
      size_t delta = isl_obj_skip(client,false,objectSize)-client;
      DBG_SL1(BF("   general header@%p value: 0x%x badge: 0x%x  sz = %lu  obj_skip = %lu\n")
              % header
              % header->_stamp_wtag_mtag._value
              % header->_stamp_wtag_mtag._header_badge
              % objectSize % delta );
      this->_total_size += sizeof(ISLGeneralHeader_s) + objectSize;
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)header;
      DBG_SL1(BF("   cons header@%p -> %p\n") % header % *(void**)header );
      this->_cons_count++;
      size_t consSize;
      isl_cons_skip(client,consSize);
      this->_total_size += sizeof(ISLConsHeader_s) + consSize;
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)((char*)header+sizeof(gctools::Header_s::StampWtagMtag));
      this->_weak_count++;
      DBG_SL1(BF("weak object header %p   client: %p\n") % (void*)header % (void*)client );
      size_t objectSize;
      gctools::clasp_ptr_t nextClient = isl_weak_skip(client,false,objectSize);
      this->_total_size += sizeof(ISLWeakHeader_s) + objectSize;
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
  
  void callback(gctools::Header_s* header) {
    std::string str;
    // On boehm sometimes I get unknown objects that I'm trying to avoid with the next test.
    if (header->_stamp_wtag_mtag.stampP()) {
      gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)HEADER_PTR_TO_GENERAL_PTR(header);
      size_t objectSize;
      gctools::clasp_ptr_t dummy = isl_obj_skip(clientStart,false,objectSize);
      gctools::clasp_ptr_t clientEnd = clientStart + objectSize;
      ISLGeneralHeader_s islheader( Object, clientEnd-clientStart, header->_stamp_wtag_mtag );
      char* islh = this->write_buffer(sizeof(ISLGeneralHeader_s), (char*)&islheader ); 
      DBG_SAVECOPY(BF("   copying general %p to %p bytes: %lu\n") % header % (void*)this->_buffer % (clientEnd-clientStart) );
      char* new_addr = this->write_buffer(clientEnd-clientStart,(char*)clientStart);
      this->_NumberOfObjects++;
      header->_stamp_wtag_mtag.setFwdPointer( new_addr ); // This is a client pointer
      DBG_SL_FWD(BF("setFwdPointer general header %p new_addr -> %p  reread fwdPointer -> %p\n") % (void*)header % (void*)new_addr % (void*)header->_stamp_wtag_mtag.fwdPointer() );
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)header;
      size_t consSize;
      size_t bytes = isl_cons_skip(client,consSize) - client;
      ISLConsHeader_s islheader( Cons, sizeof(core::Cons_O) );
      this->write_buffer(sizeof(ISLConsHeader_s), (char*)&islheader );
      DBG_SAVECOPY(BF("  copying cons %p to %p bytes: %lu\n") % header % (void*)this->_buffer % bytes );
      char* new_addr = this->write_buffer(bytes,(char*)header);
      this->_NumberOfObjects++;
      header->_stamp_wtag_mtag.setFwdPointer( new_addr );
      DBG_SL_FWD(BF("setFwdPointer cons header %p new_addr -> %p\n") % (void*)header % (void*)new_addr);
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      printf("%s:%d:%s    weak_skip\n", __FILE__, __LINE__, __FUNCTION__ );
      gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)((char*)header+sizeof(gctools::Header_s::StampWtagMtag));
      size_t objectSize;
      gctools::clasp_ptr_t dummyNextClient = isl_weak_skip(clientStart,false,objectSize);
      gctools::clasp_ptr_t clientEnd = clientStart + objectSize;
      ISLWeakHeader_s islheader( Weak, clientEnd-clientStart, header->_stamp_wtag_mtag );
      this->write_buffer(sizeof(ISLWeakHeader_s), (char*)&islheader ); 
      DBG_SAVECOPY(BF("   copying weak %p to %p bytes: %lu\n") % header % (void*)this->_buffer % objectSize );
      char* new_addr = this->write_buffer(clientEnd-clientStart,(char*)clientStart);
      this->_NumberOfObjects++;
      header->_stamp_wtag_mtag.setFwdPointer( new_addr );
      DBG_SL_FWD(BF("setFwdPointer weak header %p new_addr -> %p\n") % (void*)header % (void*)new_addr);
    }
  }
};

//
// walk image save/load objects that start at cur
//
template <typename Walker>
void walk_image_save_load_objects( ISLHeader_s* cur, Walker& walker) {
  DBG_SL_WALK(BF("Starting walk cur = %p\n") % (void*)cur)
  while (cur->_Kind != End) {
    DBG_SL_WALK(BF("walk: %p 0x%lx\n") % (void*)cur % cur->_Kind );
    if (cur->_Kind == Object) {
      ISLGeneralHeader_s* generalCur = (ISLGeneralHeader_s*)cur;
      gctools::Header_s* header = (gctools::Header_s*)((char*)cur + offsetof(ISLGeneralHeader_s,_stamp_wtag_mtag));
      DBG_SL_WALK(BF("general header: %p %s  next: %p\n") % header % header->description() % (void*)generalCur->next() );
      walker.callback(header);
      cur = generalCur->next();
    } else if (cur->_Kind == Cons) {
      ISLConsHeader_s* consCur = (ISLConsHeader_s*)cur;
      gctools::Header_s* header = (gctools::Header_s*)((char*)cur + sizeof(ISLConsHeader_s));
      DBG_SL_WALK(BF("cons header: %p %s  next: %p\n") % header % header->description() % (void*)consCur->next() );
      walker.callback(header);
      cur = consCur->next();
    } else if (cur->_Kind == Weak) {
      ISLWeakHeader_s* weakCur = (ISLWeakHeader_s*)cur;
      gctools::Header_s* header = (gctools::Header_s*)((char*)cur + offsetof(ISLWeakHeader_s,_stamp_wtag_mtag));
      DBG_SL_WALK(BF("weak header: %p %s  next: %p\n") % header % header->description() % (void*)weakCur->next() ); 
      walker.callback(header);
      cur = weakCur->next();
    } else {
      printf("%s:%d:%s Hit header@%p  with unexpected kind: %lu\n", __FILE__, __LINE__, __FUNCTION__, (void*)cur, cur->_Kind );
      abort();
    }
  }
}





struct fixup_objects_t : public walker_callback_t {
  gctools::clasp_ptr_t _buffer;
  gctools::clasp_ptr_t _vtableStart;
  gctools::clasp_ptr_t _vtableEnd;
  fixup_objects_t(gctools::clasp_ptr_t buffer, gctools::clasp_ptr_t vtableStart, gctools::clasp_ptr_t vtableEnd) :
    _buffer(buffer), _vtableStart(vtableStart), _vtableEnd(vtableEnd) {};

  void callback(gctools::Header_s* header) {
    if (header->_stamp_wtag_mtag.stampP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)HEADER_PTR_TO_GENERAL_PTR(header);
      size_t objectSize;
      isl_obj_skip(client,false,objectSize);
      gctools::clasp_ptr_t client_limit = client + objectSize;
      //
      // This is where we would fixup vtable pointers and entry-points
      //
      // 1. vtable pointers -> offset
      // 2. entry points to library code -> offset
      // 3. entry points to Code_O objects -> offset

      isl_obj_scan( 0, client, client_limit );
      DBG_SL_VTABLE(BF(" header@%p  %s isPolymorphic -> %d\n") % (void*)header %  header->description() % header->preciseIsPolymorphic() );
      if (header->preciseIsPolymorphic()) {
        //
        // Fixup code pointers before we mangle the vtable
        //
        if (header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMP_core__BuiltinClosure_O)) {
          printf("%s:%d:%s Found a BuiltinClosure_O %p\n", __FILE__, __LINE__, __FUNCTION__, client );
          ((core::BuiltinClosure_O*)client)->fixupCodePointers(core::SaveOp);
        }
        // Handle other kinds of code objects
        gctools::clasp_ptr_t vtable = *(gctools::clasp_ptr_t*)client;
        if (! (this->_vtableStart <= vtable && vtable < this->_vtableEnd ) ) {
          printf("%s:%d:%s The object @ %p %s isPolymorphic->%d vtable = %p | %p | %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, header->description().c_str(), header->preciseIsPolymorphic(), this->_vtableStart, vtable, this->_vtableEnd );
          printf("%s:%d:%s The vtable @ %p MUST be in the vtable section %p to %p\n", __FILE__, __LINE__, __FUNCTION__, vtable, this->_vtableStart, this->_vtableEnd );
        }
      //printf("%s:%d:%s The object @ %p %s isPolymorphic->%d vtable = %p | %p | %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, header->description().c_str(), header->preciseIsPolymorphic(), this->_vtableStart, vtable, this->_vtableEnd );
        intptr_t offset = (vtable-this->_vtableStart);
        *(gctools::clasp_ptr_t*)client = (gctools::clasp_ptr_t)offset;
        DBG_SL_VTABLE(BF(" general vtable %p -> %p  vtable %s\n") % (void*)vtable % (void*)offset % header->description() );
      // printf("%s:%d:%s       vtable offset = 0x%lx\n", __FILE__, __LINE__, __FUNCTION__, offset );
      }
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)header;
      size_t consSkip;
      gctools::clasp_ptr_t client_limit = isl_cons_skip((gctools::clasp_ptr_t)header,consSkip);
      isl_cons_scan( 0, client, client_limit );
      // printf("%s:%d:%s The object @ %p %s isPolymorphic->%d\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, header->description().c_str(), header->preciseIsPolymorphic());
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)((char*)header+sizeof(gctools::Header_s::StampWtagMtag));
      size_t objectSize;
      gctools::clasp_ptr_t client_limit = isl_weak_skip(clientStart,false,objectSize);
      isl_weak_scan( 0, clientStart, client_limit );
      if (header->preciseIsPolymorphic()) {
        gctools::clasp_ptr_t vtable = *(gctools::clasp_ptr_t*)clientStart;
        if (! (this->_vtableStart <= vtable && vtable < this->_vtableEnd ) ) {
          printf("%s:%d:%s The object @ %p %s isPolymorphic->%d vtable = %p | %p | %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, header->description().c_str(), header->preciseIsPolymorphic(), this->_vtableStart, vtable, this->_vtableEnd );
          printf("%s:%d:%s The vtable @ %p MUST be in the vtable section %p to %p\n", __FILE__, __LINE__, __FUNCTION__, vtable, this->_vtableStart, this->_vtableEnd );
        }
      //printf("%s:%d:%s The object @ %p %s isPolymorphic->%d vtable = %p | %p | %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, header->description().c_str(), header->preciseIsPolymorphic(), this->_vtableStart, vtable, this->_vtableEnd );
        intptr_t offset = (vtable-this->_vtableStart);
        *(gctools::clasp_ptr_t*)clientStart = (gctools::clasp_ptr_t)offset;
        DBG_SL_VTABLE(BF(" weak vtable %p -> %p  %s\n") % (void*)vtable % (void*)offset % header->description() );
      // printf("%s:%d:%s       vtable offset = 0x%lx\n", __FILE__, __LINE__, __FUNCTION__, offset );
      }
    }
  }
};









ISLFileHeader* do_image_save(gctools::clasp_ptr_t& islStart, gctools::clasp_ptr_t& islEnd )
{
#ifdef USE_BOEHM
  GC_stop_world_external();
#endif

  if (sizeof(ISLGeneralHeader_s)-offsetof(ISLGeneralHeader_s,_stamp_wtag_mtag) != sizeof(gctools::Header_s::StampWtagMtag)) {
    printf("%s:%d:%s Sanity check for headers in image save/load failed.\n"
           "The _stamp_wtag_mtag field must be the last field in ISLGeneralHeader so that it is IMMEDIATELY followed by a client\n",
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
  // 4. Copy roots into intermediate-buffer
  // 5. Walk all objects in intermediate-buffer and fixup tagged pointers using forwarding pointer
  // 6. Fixup pointers in roots
  //
  // At this point we could write out the image - but it may contain garbage.
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
  size_t total_size = 
    + calc_size._total_size            // for all objects
    + roots                                            // size for roots
    + sizeof(ISLHeader_s)*2;                           // size of last End header
  char* islbuffer0 = (char*)malloc( sizeof(ISLFileHeader) + total_size );
  char* islbuffer = islbuffer0 + sizeof(ISLFileHeader);
  globalISLBufferRange._Start = (gctools::clasp_ptr_t)islbuffer;
  globalISLBufferRange._End = (gctools::clasp_ptr_t)islbuffer+total_size;
  islStart = (gctools::clasp_ptr_t)islbuffer0;
  islEnd = (gctools::clasp_ptr_t)islbuffer+total_size;

  ISLFileHeader* fileHeader = new (islbuffer0) ISLFileHeader(total_size,numberOfObjects,(uintptr_t)islbuffer);
  
  //
  // 3. Walk all objects in memory
  //     (a) copy them to next position in intermediate-buffer
  //     (b) Set a forwarding pointer in the original object
  //

  DBG_SL(BF("  islbuffer = %p\n") % (void*)islbuffer );
  DBG_SL(BF("  Copy objects to islbuffer: %p - %p bytes: %lu\n") % (void*)islbuffer % (void*)(islbuffer+total_size) % total_size );
  copy_objects_t copy_objects(islbuffer);
  walk_garbage_collected_objects(copy_objects);
  numberOfObjects = copy_objects._NumberOfObjects;

  ISLEndHeader_s end_header( End );
  copy_objects.write_buffer( sizeof(end_header), (char*)&end_header );

  //
  // 4. Copy roots into intermediate-buffer
  //
  ISLRootHeader_s roots1( Roots,  sizeof(core::T_O*) );
  fileHeader->_LispRootOffset = copy_objects.write_buffer( sizeof(ISLRootHeader_s), (char*)&roots1 ) - islbuffer;
  fileHeader->_LispRootCount = 1;
  copy_objects.write_buffer( sizeof(void*), (char*)&_lisp );
  ISLRootHeader_s roots2( Roots, sizeof(core::T_O*)*NUMBER_OF_CORE_SYMBOLS );
  fileHeader->_CoreSymbolRootsOffset = copy_objects.write_buffer( sizeof(ISLRootHeader_s), (char*)&roots2 ) - islbuffer;
  fileHeader->_CoreSymbolRootsCount = NUMBER_OF_CORE_SYMBOLS;
  copy_objects.write_buffer( sizeof(void*)*NUMBER_OF_CORE_SYMBOLS, (char*)&gctools::global_core_symbols[0] );
  ISLRootHeader_s roots3( Roots, sizeof(core::T_O*)*global_symbol_count );
  fileHeader->_SymbolRootsOffset = copy_objects.write_buffer( sizeof(ISLRootHeader_s), (char*)&roots3 ) - islbuffer;
  fileHeader->_SymbolRootsCount = global_symbol_count;
  copy_objects.write_buffer( sizeof(void*)*global_symbol_count, (char*)&global_symbols[0] );

  //
  // 5. Walk all objects in intermediate-buffer and fixup tagged pointers using forwarding pointer
  //

  DBG_SL(BF("  Fixing objects starting at %p\n") % (void*)islbuffer);
  globalImageSaveLoadBase = -(intptr_t)islbuffer;
  gctools::clasp_ptr_t start;
  gctools::clasp_ptr_t end;
  core::executableVtableSectionRange(start,end);
  fixup_objects_t fixup_objects((gctools::clasp_ptr_t)islbuffer, start, end );
  globalPointerFix = follow_forwarding_pointer;
  walk_image_save_load_objects((ISLHeader_s*)islbuffer,fixup_objects);


  //
  // 6. Fixup pointers in roots
  //

  printf("%s:%d:%s  Fixing roots islbuffer = %p - %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)islbuffer, (void*)((char*)islbuffer+total_size));
  gctools::clasp_ptr_t* lispRoot = (gctools::clasp_ptr_t*) ((char*)islbuffer + fileHeader->_LispRootOffset + sizeof(ISLRootHeader_s));
  fixRootPointers( lispRoot, fileHeader->_LispRootCount );
  gctools::clasp_ptr_t* coreSymbolRoots = (gctools::clasp_ptr_t*) ((char*)islbuffer + fileHeader->_CoreSymbolRootsOffset + sizeof(ISLRootHeader_s));
  fixRootPointers( coreSymbolRoots, fileHeader->_CoreSymbolRootsCount );
  gctools::clasp_ptr_t* symbolRoots = (gctools::clasp_ptr_t*) ((char*)islbuffer + fileHeader->_SymbolRootsOffset + sizeof(ISLRootHeader_s));
  fixRootPointers( symbolRoots, fileHeader->_SymbolRootsCount );
  return fileHeader;
}

void image_save(const std::string& filename) {
  gctools::clasp_ptr_t  start;
  gctools::clasp_ptr_t  end;
  ISLFileHeader* fileHeader = do_image_save(start,end);
  fileHeader->_LibrariesOffset = (end - start);
  fileHeader->_NumberOfLibraries = globalISLLibraries.size();

  std::ofstream wf(filename, std::ios::out | std::ios::binary);
  if (!wf) {
    printf("Cannot open file %s\n", filename.c_str());
    return;
  }
  
  wf.write( (char*)start, (end - start ));
  for (size_t idx=0; idx<globalISLLibraries.size(); idx++ ) {
    size_t strLen = globalISLLibraries[idx]._Name.size();
    size_t alignedLen = gctools::AlignUp(strLen+1);
    char* buffer = (char*)malloc(alignedLen);
    memset(buffer,'\0',alignedLen);
    strcpy(buffer,globalISLLibraries[idx]._Name.c_str());
    ISLLibraryHeader_s libhead(Library,alignedLen);
    wf.write( (char*)&libhead, sizeof(libhead) );
    wf.write( buffer, alignedLen );
    free(buffer);
  }
  
  wf.close();
  printf("%s:%d:%s Wrote file %s - leaving image_save\n", __FILE__, __LINE__, __FUNCTION__, filename.c_str() );
  free(start);
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





struct fixup_vtables_t : public walker_callback_t {
  gctools::clasp_ptr_t _buffer;
  gctools::clasp_ptr_t _vtableStart;
  gctools::clasp_ptr_t _vtableEnd;
  fixup_vtables_t(gctools::clasp_ptr_t buffer, gctools::clasp_ptr_t vtableStart, gctools::clasp_ptr_t vtableEnd) :
    _buffer(buffer), _vtableStart(vtableStart), _vtableEnd(vtableEnd) {};

  void callback(gctools::Header_s* header) {
    if (header->_stamp_wtag_mtag.stampP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)HEADER_PTR_TO_GENERAL_PTR(header);
      if (header->preciseIsPolymorphic()) {
        gctools::clasp_ptr_t vtable = *(gctools::clasp_ptr_t*)client;
        *(gctools::clasp_ptr_t*)client = (uintptr_t)vtable + this->_vtableStart;
        DBG_SL_VTABLE(BF(" general vtable %p to %p %s\n") % (void*)vtable % (void*)*(gctools::clasp_ptr_t*)client % header->description() );
      }
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      // Do nothing
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)HeaderPtrToWeakPtr(header);
      if (header->preciseIsPolymorphic()) {
        gctools::clasp_ptr_t vtable = *(gctools::clasp_ptr_t*)client;
        *(gctools::clasp_ptr_t*)client = (uintptr_t)vtable + this->_vtableStart;
        DBG_SL_VTABLE(BF(" general vtable %p to %p\n") % (void*)vtable % (void*)*(gctools::clasp_ptr_t*)client );
      }
    }
  }
};


intptr_t globalSavedBase;
intptr_t globalLoadedBase;

gctools::clasp_ptr_t relocate_pointer(gctools::clasp_ptr_t client, uintptr_t tag) {
  uintptr_t new_client;
  new_client = (uintptr_t)((intptr_t)client - globalSavedBase + globalLoadedBase);
  DBG_SL_RELOCATE0(BF("Relocate0 from %p  to  %p\n") % (void*)client % (void*)new_client );
  return (gctools::clasp_ptr_t)(new_client | tag);
}

struct relocate_objects_t : public walker_callback_t {
  relocate_objects_t()  {};
  void callback(gctools::Header_s* header) {
    if (header->_stamp_wtag_mtag.stampP()) {
      gctools::clasp_ptr_t client = HEADER_PTR_TO_GENERAL_PTR(header);
      size_t objectSize;
      gctools::clasp_ptr_t client_limit = isl_obj_skip(client,false,objectSize);
      isl_obj_scan( 0, client, client_limit );
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)header;
      size_t consSize;
      gctools::clasp_ptr_t client_limit = isl_cons_skip((gctools::clasp_ptr_t)header,consSize);
      isl_cons_scan( 0, client, client_limit );
      // printf("%s:%d:%s The object @ %p %s isPolymorphic->%d\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, header->description().c_str(), header->preciseIsPolymorphic());
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)HeaderPtrToWeakPtr(header);
      size_t objectSize;
      gctools::clasp_ptr_t client_limit = isl_weak_skip(clientStart,false,objectSize);
      isl_weak_scan( 0, clientStart, client_limit );
    }
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

  ISLFileHeader* islFileBuffer = reinterpret_cast<ISLFileHeader*>(memory);
  if (!islFileBuffer->good_magic()) {
    printf("The file is not an image file\n");
    exit(1);
  }
  gctools::clasp_ptr_t islbuffer = (gctools::clasp_ptr_t)(islFileBuffer+1);

  printf("%s:%d:%s Loaded file %s\n", __FILE__, __LINE__, __FUNCTION__, filename.c_str());
  printf("%s:%d:%s islbuffer = %p\n",  __FILE__, __LINE__, __FUNCTION__,(void*)islbuffer);
  //
  // Setup the libraries
  //
  ISLLibraryHeader_s* libheader = (ISLLibraryHeader_s*)((char*)islFileBuffer + islFileBuffer->_LibrariesOffset);
  for ( size_t idx =0; idx<islFileBuffer->_NumberOfLibraries; idx++ ) {
    char* bufferStart = (char*)(libheader+1);
    std::string name(bufferStart);
    uintptr_t start;
    uintptr_t end;
    bool isexec;
    std::string libraryPath;
    core::library_with_name(name,libraryPath,start,end,isexec);
    ISLLibrary lib(name,(gctools::clasp_ptr_t)start,(gctools::clasp_ptr_t)end,isexec);
    printf("%s:%d:%s Registered library: %s @ %p\n", __FILE__, __LINE__, __FUNCTION__, name.c_str(), (void*)start );
    globalISLLibraries.push_back(lib);
  }
  
  
  //
  // Define the buffer range
  //
  globalISLBufferRange._Start = (gctools::clasp_ptr_t)islbuffer;
  globalISLBufferRange._End = (gctools::clasp_ptr_t)islbuffer+islFileBuffer->_Size;


  //
  // Fixup the vtables
  //
  DBG_SL(BF(" image_load fixing up vtable pointers\n"));
  gctools::clasp_ptr_t start;
  gctools::clasp_ptr_t end;
  core::executableVtableSectionRange(start,end);
  fixup_vtables_t fixup_vtables( (gctools::clasp_ptr_t)islbuffer, start, end );
  walk_image_save_load_objects((ISLHeader_s*)islbuffer,fixup_vtables);


  //
  // Let's fix the pointers so that they are correct for the loaded location in memory
  //

  DBG_SL(BF(" image_load relocating addresses\n"));
  globalSavedBase = (intptr_t)islFileBuffer->_SaveBufferStart;
  globalLoadedBase = (intptr_t)islbuffer;
  DBG_SL(BF("  Starting   globalSavedBase %p    globalLoadedBase  %p\n") % (void*)globalSavedBase % (void*)globalLoadedBase );
  globalPointerFix = relocate_pointer;
  relocate_objects_t relocate_objects;
  walk_image_save_load_objects((ISLHeader_s*)islbuffer,relocate_objects);

  //
  // Fixup the CodeBase_O objects
  //
  DBG_SL(BF("Fixup the Library_O objects\n"));
  struct fixup_CodeBase_t : public walker_callback_t {
    void callback(gctools::Header_s* header) {
      if (header->_stamp_wtag_mtag.stampP() && header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMP_llvmo__Library_O)) {
        llvmo::Library_O* lib = (llvmo::Library_O*)(HEADER_PTR_TO_GENERAL_PTR(header));
        core::SimpleBaseString_sp name = lib->_Name;
        std::string libraryPath = name->get_std_string();
        std::string libraryFilename = std::filesystem::path(libraryPath).filename();
        std::string libraryName;
        uintptr_t start;
        uintptr_t end;
        bool isExecutable;
        core::library_with_name(libraryFilename,libraryName,start,end,isExecutable);
        lib->_Start = (gctools::clasp_ptr_t)start;
        lib->_End = (gctools::clasp_ptr_t)end;
        printf("%s:%d:%s Setting the .text start of %s to %p\n", __FILE__, __LINE__, __FUNCTION__, libraryName.c_str(), (void*)start );
      }
    }
  };
  fixup_CodeBase_t fixupCodeBase;
  walk_image_save_load_objects((ISLHeader_s*)islbuffer,fixupCodeBase);

  //
  // Allocate space for temporary roots
  //

  temporary_root_holder_t root_holder(islFileBuffer->_NumberOfObjects);

  //
  // Allocate new objects for everything we just loaded and set the forwarding pointers
  //
  DBG_SL(BF("Allocate objects\n"));
  {
    ISLHeader_s* cur_header = reinterpret_cast<ISLHeader_s*>(islFileBuffer+1);
    gctools::clasp_ptr_t startVtables;
    gctools::clasp_ptr_t end;
    core::executableVtableSectionRange(startVtables,end);
    DBG_SL(BF(" allocate objects\n"));
    ISLHeader_s* next_header;
    for ( ; cur_header->_Kind != End;) {
      DBG_SL_ALLOCATE(BF("-----Allocating based on cur_header %p\n") % (void*)cur_header );
      if (cur_header->_Kind == Object) {
        ISLGeneralHeader_s* generalHeader = (ISLGeneralHeader_s*)cur_header;
        next_header = generalHeader->next();
        gctools::Header_s* source_header = (gctools::Header_s*)&generalHeader->_stamp_wtag_mtag;
        gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)(generalHeader+1);
        gctools::clasp_ptr_t clientEnd = clientStart + generalHeader->_Size;
        gctools::image_save_load_init_s init(generalHeader->_stamp_wtag_mtag,clientStart,clientEnd);
        DBG_SL_ALLOCATE(BF("  source_header %p  stamp: %u  size: %lu kind: %s\n")
                        % (void*) source_header
                        % generalHeader->_stamp_wtag_mtag._value
                        % generalHeader->_Size
                        % source_header->description().c_str());
        core::T_sp obj;
        obj = gctools::GCObjectAllocator<core::General_O>::image_save_load_allocate(&init);
        gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
        DBG_SL_ALLOCATE(BF("allocated general %p fwd: %p\n")
                        % (void*) obj.raw_()
                        % (void*)fwd);
        source_header->_stamp_wtag_mtag.setFwdPointer((void*)fwd);
        root_holder.add((void*)fwd);
      } else if (cur_header->_Kind == Cons ) {
        ISLConsHeader_s* consHeader = (ISLConsHeader_s*)cur_header;
        next_header = consHeader->next();
        gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)(cur_header+1);
        gctools::clasp_ptr_t clientEnd = clientStart + sizeof(core::Cons_O);
        gctools::Header_s* header = (gctools::Header_s*)clientStart;
        core::Cons_O* cons = (core::Cons_O*)clientStart;
        auto obj = gctools::ConsAllocator<core::Cons_O,gctools::DoRegister>::image_save_load_allocate(cons->_stamp_wtag_mtag, cons->_Car.load(), cons->_Cdr.load());
        gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
        DBG_SL_ALLOCATE(BF("---- Allocated Cons %p copy from %p header: %p  set fwd to %p\n")
                        % (void*)obj.raw_()
                        % (void*) header
                        % (void*)clientStart % (void*)fwd );
        header->_stamp_wtag_mtag.setFwdPointer((void*)fwd);
        root_holder.add((void*)fwd);
      } else if (cur_header->_Kind == Weak ) {
        ISLWeakHeader_s* weakHeader = (ISLWeakHeader_s*)cur_header;
        next_header = weakHeader->next();
        gctools::Header_s* header = (gctools::Header_s*)&weakHeader->_stamp_wtag_mtag;
        gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)(weakHeader+1);
        gctools::clasp_ptr_t clientEnd = clientStart + weakHeader->_Size;
        gctools::image_save_load_init_s init(header->_stamp_wtag_mtag,clientStart,clientEnd);
        gctools::Header_s::WeakKinds kind = (gctools::Header_s::WeakKinds)header->_stamp_wtag_mtag._value;
        switch (kind) {
#if 1
        case gctools::Header_s::WeakBucketKind: {
          auto obj = gctools::GCBucketAllocator<gctools::Buckets<core::T_sp,core::T_sp,gctools::WeakLinks>>::image_save_load_allocate(&init);
          gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
          DBG_SL_ALLOCATE(BF("allocated weak %p header: %p stamp: %u: %s  fwd: %p\n")
                          % (void*) obj.raw_()
                          % (void*) header
                          % (uintptr_t)kind % (void*)fwd );
          header->_stamp_wtag_mtag.setFwdPointer((void*)fwd);
          root_holder.add((void*)fwd);
          break;
        }
        case gctools::Header_s::StrongBucketKind: {
          auto obj = gctools::GCBucketAllocator<gctools::Buckets<core::T_sp,core::T_sp,gctools::StrongLinks>>::image_save_load_allocate(&init);
          gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
          DBG_SL_ALLOCATE(BF("allocated weak %p header: %p stamp: %u: %s  fwd: %p\n")
                          % (void*) obj.raw_()
                          % (void*) header
                          % (uintptr_t)kind % (void*)fwd );
          header->_stamp_wtag_mtag.setFwdPointer((void*)fwd);
          root_holder.add((void*)fwd);
          break;
        }
#endif
#if 0
        case gctools::Header_s::WeakMappingKind: {
          auto obj = gctools::GCMappingAllocator<gctools::Mapping<core::T_sp,core::T_sp,gctools::WeakLinks>>::image_save_load_allocate(&init);
          gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
          DBG_SL_ALLOCATE(BF("allocated weak %p header: %p stamp: %u: %s  fwd: %p\n")
                          % (void*) obj.raw_()
                          % (void*) header
                          % (uintptr_t)kind % (void*)fwd );
          header->_stamp_wtag_mtag.setFwdPointer((void*)fwd);
          root_holder.add((void*)fwd);
          break;
        }
        case gctools::Header_s::StrongBucketKind: {
          auto obj = gctools::GCMappingAllocator<gctools::Mapping<core::T_sp,core::T_sp,gctools::StrongLinks>>::image_save_load_allocate(&init);
          gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
          DBG_SL_ALLOCATE(BF("allocated weak %p header: %p stamp: %u: %s  fwd: %p\n")
                          % (void*) obj.raw_()
                          % (void*) header
                          % (uintptr_t)kind % (void*)fwd );
          header->_stamp_wtag_mtag.setFwdPointer((void*)fwd);
          root_holder.add((void*)fwd);
          break;
        }
#endif
#if 0
        case gctools::Header_s::WeakPointerKind: {
          auto obj = gctools::GCWeakPointerAllocator<gctools::WeakPointer>::image_save_load_allocate(&init);
          gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
          DBG_SL_ALLOCATE(BF("allocated weak %p header: %p stamp: %u: %s  fwd: %p\n")
                          % (void*) obj.raw_()
                          % (void*) header
                          % (uintptr_t)kind % (void*)fwd );
          header->_stamp_wtag_mtag.setFwdPointer((void*)fwd);
          root_holder.add((void*)fwd);
          break;
        }
#endif
        default:
            printf("%s:%d:%s  Handle allocate weak objects\n", __FILE__, __LINE__, __FUNCTION__ );
            break;
        }
      } else {
        printf("%s:%d:%s Unknown header at offset 0x%lx qword: 0x%lx\n", __FILE__, __LINE__, __FUNCTION__, (uintptr_t)cur_header - (uintptr_t)islFileBuffer, *(uintptr_t*)cur_header );
      }
      size_t size = cur_header->_Size;
      DBG_SL(BF("Done working with cur_header@%p  advanced to %p where cur_header->_Size = %lu\n") % (void*)cur_header % (void*)next_header % size );
      cur_header = next_header;
    }
    DBG_SL(BF("Done working with all objects cur_header@%p\n") % (void*)cur_header );
  }
#if 0

  //
  // Walk all the objects and fixup all the pointers
  //
  DBG_SL(BF(" fixup pointers\n"));
  globalImageSaveLoadBase = (intptr_t)islbuffer;
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

#endif


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


}


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



