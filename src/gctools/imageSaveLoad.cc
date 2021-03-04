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
#include <clasp/core/evaluator.h>
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
#if 0
#define DBG_SL_ALLOCATE(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_ALLOCATE(_fmt_)
#endif

#if 0
#define DBG_SAVECOPY(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SAVECOPY(_fmt_)
#endif

#if 0
#define DBG_SL_ROOT(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_ROOT(_fmt_)
#endif

#if 0
#define DBG_SL_FWD(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_FWD(_fmt_)
#endif

#if 0
#define DBG_SL_FFWD(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_FFWD(_fmt_)
#endif

#if 0
#define DBG_SL_RELOCATE0(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_RELOCATE0(_fmt_)
#endif

#if 0
#define DBG_SL_DONTWALK(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_DONTWALK(_fmt_)
#endif

#if 0
#define DBG_SL_WALK_SL(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_WALK_SL(_fmt_)
#endif

#if 0
#define DBG_SL_WALK_GC(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_WALK_GC(_fmt_)
#endif

#if 0
#define DBG_SL_WALK_TEMP(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_WALK_TEMP(_fmt_)
#endif

#if 0
#define DBG_SL_VTABLE(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_VTABLE(_fmt_)
#endif

#if 0
#define DBG_SL_ENTRY_POINT(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_ENTRY_POINT(_fmt_)
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
bool globalFwdMustBeInGCMemory = false;
#define DEBUG_SL_FFWD 1

void* encodePointer(gctools::clasp_ptr_t address,size_t idx, gctools::clasp_ptr_t start) {
  uintptr_t offset = (uintptr_t)address - (uintptr_t)start;
  uintptr_t result = idx<<48 | offset;
  DBG_SL_ENTRY_POINT(BF("Library base: %p encode %p/%d -> %p\n") % (void*)start % (void*)address % idx % (void*)result );
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
  DBG_SL_ENTRY_POINT(BF("Library base: %p decode %p -> %p\n") % (void*)globalISLLibraries[libidx]._TextStart % codedAddress % (void*)ptr  );
  return (void*)ptr;
}



void* encodeEntryPointSaveAddress(void* vaddress, llvmo::CodeBase_sp code) {
  uintptr_t address = (uintptr_t)vaddress;
  void* result = (void*)(address - code->codeStart());
  DBG_SL_ENTRY_POINT(BF("base: %p encoded %p -> %p\n") % (void*)code->codeStart() % vaddress % result  );
  return result;
}

void* decodeEntryPointSaveAddress(void* vaddress, llvmo::CodeBase_sp code) {
  uintptr_t address = (uintptr_t)vaddress;
  void* result = (void*)(address + code->codeStart());
  DBG_SL_ENTRY_POINT(BF("base: %p decoded %p -> %p\n") % (void*)code->codeStart() % vaddress % result  );
  return result;
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
#define WEAK_PTR_TO_HEADER_PTR(_general_) generalPointerToHeaderPointer((gctools::clasp_ptr_t)_general_)
#define HEADER_PTR_TO_WEAK_PTR(_header_) headerPointerToGeneralPointer((gctools::Header_s*)_header_)

typedef gctools::clasp_ptr_t (*PointerFix)(gctools::clasp_ptr_t* clientAddress, gctools::clasp_ptr_t client, uintptr_t tag, void* user_data);
PointerFix globalPointerFix;


struct ISLInfo {
  core::FixupOperation _operation;
  uintptr_t _islStart;
  uintptr_t _islEnd;
  ISLInfo(core::FixupOperation op, uintptr_t s=0, uintptr_t e=0) :
    _operation(op),
    _islStart(s),
    _islEnd(e) {};
};  

gctools::clasp_ptr_t follow_forwarding_pointer(gctools::clasp_ptr_t* clientAddress, gctools::clasp_ptr_t client, uintptr_t tag, void* user_data) {
  ISLInfo* islInfo = (ISLInfo*)user_data;
  uintptr_t fwd_client;
  gctools::Header_s* header;
  if (tag==gctools::general_tag) {
    header = GENERAL_PTR_TO_HEADER_PTR(client);
    if (!header->_stamp_wtag_mtag.fwdP()) {
      printf("%s:%d:%s general header %p MUST BE A FORWARDING POINTER - got %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, *(void**)header);
      abort();
    }
  } else if (tag==gctools::cons_tag) {
    header = (gctools::Header_s*)gctools::ConsPtrToHeaderPtr(client);
    if (!header->_stamp_wtag_mtag.fwdP()) {
      printf("%s:%d:%s general header %p MUST BE A FORWARDING POINTER - got %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, *(void**)header);
      abort();
    }
    fwd_client = (uintptr_t)header->_stamp_wtag_mtag.fwdPointer();
  } else {
    header = WEAK_PTR_TO_HEADER_PTR(client);
    if (!header->_stamp_wtag_mtag.fwdP()) {
      printf("%s:%d:%s general header %p MUST BE A FORWARDING POINTER - got %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, *(void**)header);
      abort();
    }
    fwd_client = (uintptr_t)header->_stamp_wtag_mtag.fwdPointer();
  }
  fwd_client = (uintptr_t)header->_stamp_wtag_mtag.fwdPointer();
  DBG_SL_FFWD(BF("fwdPointer from %p  to header@%p  %p  GC_base %p\n")
              % (void*)((uintptr_t)client | tag)
              % (void*)header
              % ((void*)((uintptr_t)fwd_client | tag))
              % GC_base((void*)fwd_client) );
  if (islInfo->_operation == core::SaveOp) {
    if (!(islInfo->_islStart<=fwd_client)&&(fwd_client<islInfo->_islEnd)) {
      printf("%s:%d:%s Forwarded pointer does NOT point into the islbuffer\n", __FILE__, __LINE__, __FUNCTION__);
      abort();
    }
  } else if (islInfo->_operation == core::LoadOp) {
    void* maybe_base = GC_base((void*)fwd_client);
    if (!maybe_base) {
      printf("%s:%d:%s We have a pointer %p that MUST be in GC memory - but it isn't\n", __FILE__, __LINE__, __FUNCTION__, (void*)fwd_client );
      abort();
    }
  }
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
        obj = (globalPointerFix)(taggedP,obj,tag,user_data); \
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
#define EXTRA_ARGUMENTS , void* user_data

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
void followForwardingPointersForRoots(gctools::clasp_ptr_t* start, size_t number, void* user_data) {
  for ( size_t idx = 0; idx<number; idx++ ) {
    gctools::clasp_ptr_t before = *start;
    POINTER_FIX(start);
    gctools::clasp_ptr_t after = *start;
    DBG_SL_ROOT(BF("Fixed root pointer %d from %p to %p GC_base %p\n") % idx % (void*)before % (void*)after % GC_base((void*)after) );
    start++;
  }
}


void copyRoots( uintptr_t* destination, uintptr_t* source, size_t numberOfRoots ) {
  DBG_SL_ROOT(BF("Moving roots from %p to %p num: %lu\n") % (void*)source % (void*)destination % numberOfRoots );
  memcpy((char*)destination, (char*)source, numberOfRoots*sizeof(void*));
}


/* Immediately after load the root pointers will point to the location of objects
 * in the islbuffer at save time.  We need to relocate them to the load location
 * of the islbuffer
 */
void relocateLoadedRootPointers(gctools::clasp_ptr_t* start, size_t number, void* user_data ) {
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
typedef enum {
    Object  = 0xbedabb1e01010101, // !OBJECT!
    Cons    = 0xbedabb1e02020202,
    Weak    = 0xbedabb1e03030303,
    Library = 0xbedabb1e04040404,
    Roots   = 0xbedabb1e05050505, // ROOTS
    End     = 0xbedabb1e06060606 } ISLKind; // END


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
  size_t _global_JITDylibCounter;
  size_t _PageAlignedObjectFileStart;
  size_t _ObjectFileSize;
  ISLFileHeader(size_t sz,size_t num, uintptr_t sbs) : _Magic(MAGIC_NUMBER), _Size(sz), _NumberOfObjects(num), _SaveBufferStart(sbs) {
    this->_global_JITDylibCounter = llvmo::global_JITDylibCounter.load();
  };
  bool good_magic() const {
    return (this->_Magic == MAGIC_NUMBER);
  }
};


struct walker_callback_t {
  ISLInfo _info;
  virtual void callback(gctools::Header_s* header) = 0;
  walker_callback_t(const ISLInfo& info) : _info(info) {};
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
    // Either there is a tag in the low 3 bits of the ptr or the second word is not zero
    // Blocks can be passed to us that have a boehm free-list pointer in the first word and zero everywhere else
    //   a free-list pointer will have zero in the low 3 bits
    bool notFreeListPtr = ((*(uintptr_t*)ptr)&0x7);
    bool secondWordNotZero = *(((uintptr_t*)ptr)+1);
    if ( notFreeListPtr || secondWordNotZero ) {
      // If there is a non-zero header then walk it
      gctools::Header_s* header = (gctools::Header_s*)ptr;
      DBG_SL_WALK_GC(BF("Walking to GC managed header %p %s\n") % (void*)header % header->description() );
      walker->callback((gctools::Header_s*)ptr);
    } else {
      DBG_SL_DONTWALK(BF("NOT walking to GC managed header %p kind: %d value -> %p  notFreeListPtr %d  secondWordNotZero %d\n") % (void*)ptr % kind % *(void**)ptr % notFreeListPtr % secondWordNotZero );
    }
  } else {
    DBG_SL_DONTWALK(BF("NOT walking to GC managed header %p kind: %d value -> %p\n") % (void*)ptr % kind % *(void**)ptr );
  }
}
};


namespace imageSaveLoad {



struct ISLHeader_s {
  ISLKind     _Kind;
  size_t      _Size;
  ISLHeader_s(ISLKind k, size_t s) : _Kind(k), _Size(s) {};
  virtual ISLHeader_s* next() const = 0;
};

struct ISLEndHeader_s : public ISLHeader_s {
  ISLEndHeader_s(ISLKind k) : ISLHeader_s(k,0) {};
  ISLHeader_s* next() const { return (ISLHeader_s*)((char*)this+sizeof(*this)); };
  gctools::Header_s* header() const {printf("%s:%d:%s subclass must implement\n", __FILE__, __LINE__, __FUNCTION__ ); abort(); };
};


struct ISLRootHeader_s : public ISLHeader_s {
  ISLRootHeader_s(ISLKind k, size_t s) : ISLHeader_s(k,s) {};
  ISLHeader_s* next() const { return (ISLHeader_s*)((char*)this+sizeof(*this)+this->_Size); };
};
  
struct ISLConsHeader_s : public ISLHeader_s {
  gctools::Header_s::StampWtagMtag _stamp_wtag_mtag;
  ISLConsHeader_s(ISLKind k, size_t s, gctools::Header_s::StampWtagMtag swm) : ISLHeader_s(k,s), _stamp_wtag_mtag(swm) {};
  ISLHeader_s* next() const { return (ISLHeader_s*)((char*)this+sizeof(*this)+this->_Size); };
  gctools::Header_s* header() const { return (gctools::Header_s*)((char*)this + offsetof(ISLConsHeader_s,_stamp_wtag_mtag)); }
};

struct ISLWeakHeader_s : public ISLHeader_s {
  gctools::Header_s::StampWtagMtag _stamp_wtag_mtag;
  ISLWeakHeader_s(ISLKind k, uintptr_t sz, gctools::Header_s::StampWtagMtag swm) : ISLHeader_s(k,sz), _stamp_wtag_mtag(swm) {};
  ISLHeader_s* next() const { return (ISLHeader_s*)((char*)this+sizeof(*this)+this->_Size); };
  gctools::Header_s* header() const { return (gctools::Header_s*)((char*)this + offsetof(ISLWeakHeader_s,_stamp_wtag_mtag)); }
};

struct ISLGeneralHeader_s : public ISLHeader_s {
  gctools::Header_s::StampWtagMtag _stamp_wtag_mtag;
  ISLGeneralHeader_s(ISLKind k, uintptr_t sz, gctools::Header_s::StampWtagMtag swm) : ISLHeader_s(k,sz), _stamp_wtag_mtag(swm) {};
  ISLHeader_s* next() const { return (ISLHeader_s*)((char*)this+sizeof(*this)+this->_Size); };
  gctools::Header_s* header() const { return (gctools::Header_s*)((char*)this + offsetof(ISLGeneralHeader_s,_stamp_wtag_mtag));}
};

struct ISLLibraryHeader_s : public ISLHeader_s {
  ISLLibraryHeader_s(ISLKind k, size_t s) : ISLHeader_s(k,s) {};
  ISLHeader_s* next() const { return (ISLHeader_s*)((char*)this+sizeof(*this)+this->_Size); };
};


#define ISL_ERROR(_fmt_) { \
    printf("%s:%d:%s  %s\n", __FILE__, __LINE__, __FUNCTION__, (_fmt_).str().c_str()); \
    abort(); \
  }


struct prepare_for_image_save_t : public walker_callback_t {

  void callback(gctools::Header_s* header) {
    // On boehm sometimes I get unknown objects that I'm trying to avoid with the next test.
    if (header->_stamp_wtag_mtag.stampP()) {
      //
      // Fixup general objects that need it
      //
      // Handle them on a case by case basis
      if (header->preciseIsPolymorphic()) {
        core::T_O* client = (core::T_O*)HEADER_PTR_TO_GENERAL_PTR(header);
        if (cast::Cast<core::General_O*,core::T_O*>::isA(client)) {
          core::General_O* generalObject = (core::General_O*)client;
          core::FixupOperation op = core::SaveOp;
          generalObject->fixupInternalsForImageSaveLoad(op);
        }
      }
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      // Nothing
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      // Nothing
    }
  }

  prepare_for_image_save_t(const ISLInfo& info) : walker_callback_t(info) {};
};

struct calculate_size_t : public walker_callback_t {

  size_t _TotalSize;
  size_t _ObjectFileTotalSize;
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
      if (header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_llvmo__Code_O)) {
        //
        // Calculate the size of a Code_O object keeping only the literals vector
        //
        llvmo::Code_O* code = (llvmo::Code_O*)client;
        this->_TotalSize += sizeof(ISLGeneralHeader_s) + code->frontSize() + code->literalsSize();
      } else if (header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_llvmo__ObjectFile_O)) {
        llvmo::ObjectFile_O* objectFile = (llvmo::ObjectFile_O*)client;
        size_t objectFileSize = objectFile->objectFileSizeAlignedUpToPageSize(getpagesize());
        this->_ObjectFileTotalSize += objectFileSize;
        this->_TotalSize += sizeof(ISLGeneralHeader_s) + sizeof(llvmo::ObjectFile_O);
      } else {
        size_t delta = isl_obj_skip(client,false,objectSize)-client;
        DBG_SL1(BF("   general header@%p value: 0x%x badge: 0x%x  sz = %lu  obj_skip = %lu\n")
                % header
                % header->_stamp_wtag_mtag._value
                % header->_stamp_wtag_mtag._header_badge
                % objectSize % delta );
        this->_TotalSize += sizeof(ISLGeneralHeader_s) + objectSize;
      }
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)gctools::HeaderPtrToConsPtr(header);
      DBG_SL1(BF("   cons header@%p -> %p\n") % header % *(void**)header );
      this->_cons_count++;
      size_t consSize;
      isl_cons_skip(client,consSize);
      this->_TotalSize += sizeof(ISLConsHeader_s) + consSize;
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)HEADER_PTR_TO_WEAK_PTR(header);
      this->_weak_count++;
      DBG_SL1(BF("weak object header %p   client: %p\n") % (void*)header % (void*)client );
      size_t objectSize;
      gctools::clasp_ptr_t nextClient = isl_weak_skip(client,false,objectSize);
      this->_TotalSize += sizeof(ISLWeakHeader_s) + objectSize;
    }
  }

  calculate_size_t(const ISLInfo& info) :
    walker_callback_t(info),
    _TotalSize(0),
    _ObjectFileTotalSize(0),
    _general_count(0),
    _cons_count(0),
    _weak_count(0)
  {};
};

void* walk_garbage_collected_objects_with_alloc_lock(void* client_data) {
  GC_enumerate_reachable_objects_inner(boehm_walker_callback, client_data );
  return NULL;
}

template <typename Walker>
void walk_garbage_collected_objects(bool saving, Walker& walker) {
#ifdef USE_BOEHM
  if (saving) {
    // We stopped the world - so I think we already have the lock
    GC_enumerate_reachable_objects_inner(boehm_walker_callback, (void*)&walker);
  } else {
    GC_call_with_alloc_lock( walk_garbage_collected_objects_with_alloc_lock,
                             (void*)&walker );
  }    
#endif
#ifdef USE_MPS
  printf("%s:%d:%s  Walk MPS objects\n", __FILE__, __LINE__, __FUNCTION__ );
#endif
}



struct copy_objects_t : public walker_callback_t {
  char* _buffer;
  size_t _NumberOfObjects;
  char* _ObjectFileBuffer;
  size_t _ObjectFileTotalSize;
  char* _ObjectFileCur;
  char* _ObjectFileEnd;
  copy_objects_t(char* buffer, char* objectFileBuffer, size_t objectFileTotalSize, const ISLInfo& info)
    : walker_callback_t(info), _buffer(buffer),
      _ObjectFileBuffer(objectFileBuffer), _ObjectFileTotalSize(objectFileTotalSize),
      _NumberOfObjects(0) {
    this->_ObjectFileCur = objectFileBuffer;
    this->_ObjectFileEnd = objectFileBuffer+objectFileTotalSize;
  }

  char* write_buffer(size_t bytes, char* source ) {
    char* addr = this->_buffer;
    memcpy((void*)this->_buffer, (const void*)source, bytes );
    this->_buffer += bytes;
    return addr;
  }

  char* write_objectFile(size_t exactSize, char* source, size_t pagesize) {
    size_t pageAlignedSize = ((exactSize+pagesize)/pagesize)*pagesize;
    memset(this->_ObjectFileCur,'\0',pageAlignedSize);
    memcpy(this->_ObjectFileCur,source,exactSize);
    char* pos = this->_ObjectFileCur;
    this->_ObjectFileCur += pageAlignedSize;
    return pos;
  }
  
  void callback(gctools::Header_s* header) {
    std::string str;
    // On boehm sometimes I get unknown objects that I'm trying to avoid with the next test.
    if (header->_stamp_wtag_mtag.stampP()) {
      if (header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_core__Lisp_O)) {
        printf("%s:%d:%s About to save Lisp_O object\n", __FILE__, __LINE__, __FUNCTION__ );
      }
      gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)HEADER_PTR_TO_GENERAL_PTR(header);
      if (header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_llvmo__Code_O)) {
        //
        // Calculate the size of a Code_O object keeping only the literals vector
        //
        llvmo::Code_O* code = (llvmo::Code_O*)clientStart;
        gctools::clasp_ptr_t clientEnd = clientStart + code->frontSize();
        ISLGeneralHeader_s islheader( Object, clientEnd-clientStart, header->_stamp_wtag_mtag );
        char* islh = this->write_buffer(sizeof(ISLGeneralHeader_s), (char*)&islheader ); 
        char* new_addr = this->write_buffer(clientEnd-clientStart,(char*)clientStart);
        llvmo::Code_O* new_code = (llvmo::Code_O*)new_addr;
        char* literals_addr = this->write_buffer(code->literalsSize(),(char*)code->literalsStart());
        // Update the new_code object to reflect that contains literals and no code and it's in the "SaveState"
        new_code->_State = llvmo::SaveState;
        new_code->_LiteralVectorStart = literals_addr - new_addr;
        new_code->_LiteralVectorSize = code->literalsSize();
        DBG_SAVECOPY(BF("   copied general header Code_O object %p to %p - %p\n") % header % (void*)islh % (void*)this->_buffer );
        header->_stamp_wtag_mtag.setFwdPointer( new_addr ); // This is a client pointer
        DBG_SL_FWD(BF("setFwdPointer general header %p new_addr -> %p  reread fwdPointer -> %p\n") % (void*)header % (void*)new_addr % (void*)header->_stamp_wtag_mtag.fwdPointer() );
      } else if (header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_llvmo__ObjectFile_O)) {
        llvmo::ObjectFile_O* objectFile = (llvmo::ObjectFile_O*)clientStart;
        size_t objectFileSize = objectFile->objectFileSizeAlignedUpToPageSize(getpagesize());
        size_t generalSize;
        gctools::clasp_ptr_t dummy = isl_obj_skip(clientStart,false,generalSize);
        if (generalSize==0) ISL_ERROR(BF("A zero size general at %p was encountered") % (void*)clientStart );
        gctools::clasp_ptr_t clientEnd = clientStart + generalSize;
        ISLGeneralHeader_s islheader( Object, clientEnd-clientStart, header->_stamp_wtag_mtag );
        char* islh = this->write_buffer(sizeof(ISLGeneralHeader_s), (char*)&islheader ); 
        char* new_client = this->write_buffer(clientEnd-clientStart,(char*)clientStart);
        llvmo::ObjectFile_O* newObjectFile = (llvmo::ObjectFile_O*)new_client;
        char* objectFileAddress = this->write_objectFile( objectFile->objectFileSize(), (char*)objectFile->objectFileData(), getpagesize() );
        newObjectFile->_ObjectFileOffset = objectFileAddress - this->_ObjectFileBuffer;
        newObjectFile->_ObjectFileSize = objectFile->objectFileSize();
        DBG_SAVECOPY(BF("   copied general header %p to %p - %p\n") % header % (void*)islh % (void*)this->_buffer );
        header->_stamp_wtag_mtag.setFwdPointer( new_client ); // This is a client pointer
        DBG_SL_FWD(BF("setFwdPointer general header %p new_client -> %p  reread fwdPointer -> %p\n") % (void*)header % (void*)new_client % (void*)header->_stamp_wtag_mtag.fwdPointer() );
      } else {
      //
      // Now write it into the buffer
      //
        size_t generalSize;
        gctools::clasp_ptr_t dummy = isl_obj_skip(clientStart,false,generalSize);
        if (generalSize==0) ISL_ERROR(BF("A zero size general at %p was encountered") % (void*)clientStart );
        gctools::clasp_ptr_t clientEnd = clientStart + generalSize;
        ISLGeneralHeader_s islheader( Object, clientEnd-clientStart, header->_stamp_wtag_mtag );
        char* islh = this->write_buffer(sizeof(ISLGeneralHeader_s), (char*)&islheader ); 
        char* new_client = this->write_buffer(clientEnd-clientStart,(char*)clientStart);
        DBG_SAVECOPY(BF("   copied general header %p to %p - %p\n") % header % (void*)islh % (void*)this->_buffer );
        this->_NumberOfObjects++;
        header->_stamp_wtag_mtag.setFwdPointer( new_client ); // This is a client pointer
        DBG_SL_FWD(BF("setFwdPointer general header %p new_client -> %p  reread fwdPointer -> %p\n") % (void*)header % (void*)new_client % (void*)header->_stamp_wtag_mtag.fwdPointer() );
      }
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)HeaderPtrToConsPtr(header);
      size_t consSize;
      isl_cons_skip(client,consSize);
      if (consSize==0) ISL_ERROR(BF("A zero size cons at %p was encountered") % (void*)client );
      ISLConsHeader_s islheader( Cons, sizeof(core::Cons_O), header->_stamp_wtag_mtag );
      char* islh = this->write_buffer(sizeof(ISLConsHeader_s), (char*)&islheader );
      char* new_addr = this->write_buffer(consSize,(char*)client);
      core::Cons_O* cons = (core::Cons_O*)client;
      DBG_SAVECOPY(BF("   copied cons header %p to %p - %p | CAR: %p CDR: %p\n") % header % (void*)islh % (void*)this->_buffer % (void*)cons->_Car.load().raw_() % (void*)cons->_Cdr.load().raw_() );
      this->_NumberOfObjects++;
      header->_stamp_wtag_mtag.setFwdPointer( new_addr );
      DBG_SL_FWD(BF("setFwdPointer cons header %p new_addr -> %p\n") % (void*)header % (void*)new_addr);
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      printf("%s:%d:%s    weak_skip\n", __FILE__, __LINE__, __FUNCTION__ );
      gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)HEADER_PTR_TO_WEAK_PTR(header);
      size_t weakSize;
      gctools::clasp_ptr_t dummyNextClient = isl_weak_skip(clientStart,false,weakSize);
      if (weakSize==0) ISL_ERROR(BF("A zero size weak object at %p was encountered") % (void*)clientStart );
      gctools::clasp_ptr_t clientEnd = clientStart + weakSize;
      ISLWeakHeader_s islheader( Weak, clientEnd-clientStart, header->_stamp_wtag_mtag );
      char* islh = this->write_buffer(sizeof(ISLWeakHeader_s), (char*)&islheader ); 
      char* new_addr = this->write_buffer(clientEnd-clientStart,(char*)clientStart);
      DBG_SAVECOPY(BF("   copied weak header %p to %p - %p\n") % header % (void*)islh % (void*)this->_buffer );
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
  DBG_SL_WALK_SL(BF("Starting walk cur = %p\n") % (void*)cur)
  while (cur->_Kind != End) {
    DBG_SL_WALK_SL(BF("walk: %p 0x%lx\n") % (void*)cur % cur->_Kind );
    if (cur->_Kind == Object) {
      ISLGeneralHeader_s* generalCur = (ISLGeneralHeader_s*)cur;
      gctools::Header_s* header = generalCur->header();
      DBG_SL_WALK_SL(BF("general header: %p %s  next: %p\n") % header % header->description() % (void*)generalCur->next() );
      walker.callback(header);
      cur = generalCur->next();
    } else if (cur->_Kind == Cons) {
      ISLConsHeader_s* consCur = (ISLConsHeader_s*)cur;
      gctools::Header_s* header = consCur->header();
      DBG_SL_WALK_SL(BF("cons header: %p %s  next: %p\n") % header % header->description() % (void*)consCur->next() );
      walker.callback(header);
      cur = consCur->next();
    } else if (cur->_Kind == Weak) {
      ISLWeakHeader_s* weakCur = (ISLWeakHeader_s*)cur;
      gctools::Header_s* header = (gctools::Header_s*)((char*)cur + offsetof(ISLWeakHeader_s,_stamp_wtag_mtag));
      DBG_SL_WALK_SL(BF("weak header: %p %s  next: %p\n") % header % header->description() % (void*)weakCur->next() ); 
      walker.callback(header);
      cur = weakCur->next();
    } else {
      printf("%s:%d:%s Hit header@%p  with unexpected kind: %lu\n", __FILE__, __LINE__, __FUNCTION__, (void*)cur, cur->_Kind );
      abort();
    }
  }
}





struct fixup_objects_t : public walker_callback_t {
  core::FixupOperation _operation;
  gctools::clasp_ptr_t _buffer;
  fixup_objects_t(core::FixupOperation op, gctools::clasp_ptr_t buffer, const ISLInfo& info) :
    walker_callback_t(info), _operation(op), _buffer(buffer) {};

  void callback(gctools::Header_s* header) {
    if (header->_stamp_wtag_mtag.stampP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)HEADER_PTR_TO_GENERAL_PTR(header);
      size_t objectSize;
      isl_obj_skip(client,false,objectSize);
      gctools::clasp_ptr_t client_limit = client + objectSize;
      //
      // This is where we would fixup pointers and entry-points
      //
      // 1. entry points to library code -> offset
      // 2. entry points to Code_O objects -> offset

      isl_obj_scan( 0, client, client_limit, (void*)&this->_info );
#if 0
      if (header->preciseIsPolymorphic()) {
        if (header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_core__BuiltinClosure_O)) {
          printf("%s:%d       doing fixup of BuiltinClosure_O\n", __FILE__, __LINE__ );
          ((core::BuiltinClosure_O*)client)->fixupCodePointers(this->_operation);
        } else if (header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_core__GlobalEntryPoint_O)) {
          printf("%s:%d       doing fixup of GlobalEntryPoint_O\n", __FILE__, __LINE__ );
          ((core::GlobalEntryPoint_O*)client)->fixupCodePointers(this->_operation);
        }
        // Handle other kinds of code objects
      }
#endif
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)gctools::HeaderPtrToConsPtr(header);
      size_t consSkip;
      gctools::clasp_ptr_t client_limit = isl_cons_skip((gctools::clasp_ptr_t)client,consSkip);
      isl_cons_scan( 0, client, client_limit, (void*)&this->_info );
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)HEADER_PTR_TO_WEAK_PTR(header);
      size_t objectSize;
      gctools::clasp_ptr_t client_limit = isl_weak_skip(clientStart,false,objectSize);
      isl_weak_scan( 0, clientStart, client_limit, (void*)&this->_info );
    }
  }
};



struct fixup_internals_t : public walker_callback_t {
  core::FixupOperation _operation;
  fixup_internals_t(core::FixupOperation op, const ISLInfo& info) :
    walker_callback_t(info), _operation(op) {};

  void callback(gctools::Header_s* header) {
    if (header->_stamp_wtag_mtag.stampP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)HEADER_PTR_TO_GENERAL_PTR(header);
      if (header->preciseIsPolymorphic()) {
        //
        // Fixup objects when loading/saving
        //
        core::T_O* client = (core::T_O*)HEADER_PTR_TO_GENERAL_PTR(header);
        if (cast::Cast<core::General_O*,core::T_O*>::isA(client)) {
          core::General_O* generalObject = (core::General_O*)client;
          core::FixupOperation op = core::SaveOp;
          generalObject->fixupInternalsForImageSaveLoad(this->_operation);
        }
      }
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
    }
  }
};



struct fixup_vtables_t : public walker_callback_t {
  bool _encoding;
  uintptr_t _vtableRegionStart;
  uintptr_t _vtableRegionEnd;
  uintptr_t _vtableRegionSize;
  fixup_vtables_t(bool encoding, uintptr_t vtableRegionStart, uintptr_t vtableRegionEnd, const ISLInfo& info ) :
    walker_callback_t(info),
    _encoding(encoding), _vtableRegionStart(vtableRegionStart), _vtableRegionEnd(vtableRegionEnd) {
    this->_vtableRegionSize = this->_vtableRegionEnd-this->_vtableRegionStart;
  };

  void do_vtable(gctools::Header_s* header, uintptr_t client, uintptr_t& vtable, uintptr_t& new_vtable )
  {
    vtable = *(uintptr_t*)client;
    if (this->_encoding) {
      new_vtable = (uintptr_t)vtable - (uintptr_t)this->_vtableRegionStart;
      if (this->_vtableRegionSize < new_vtable) ISL_ERROR(BF("new_vtable %lu is outside of the allowed range") % new_vtable );
    } else {
      new_vtable = (uintptr_t)vtable + (uintptr_t)this->_vtableRegionStart;
      if (new_vtable < this->_vtableRegionStart || this->_vtableRegionEnd <= new_vtable) ISL_ERROR(BF("new_vtable %lu is outside of the allowed range") % new_vtable );
    }
    *(uintptr_t*)client = new_vtable;
  }
  
  void callback(gctools::Header_s* header) {
    uintptr_t new_vtable;
    if (header->_stamp_wtag_mtag.stampP()) {
      if (header->preciseIsPolymorphic()) {
        uintptr_t client = (uintptr_t)HEADER_PTR_TO_GENERAL_PTR(header);
        uintptr_t vtable;
        uintptr_t new_vtable;
        this->do_vtable(header,client,vtable,new_vtable);
        DBG_SL_VTABLE(BF(" wrote general base %p vtable in memory at %p value: %p to %p %s\n") % (void*)this->_vtableRegionStart % (void*)client % (void*)vtable % (void*)new_vtable % header->description() );
      }
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      // Do nothing
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      if (header->preciseIsPolymorphic()) {
        uintptr_t client = (uintptr_t)HEADER_PTR_TO_WEAK_PTR(header);
        uintptr_t vtable;
        uintptr_t new_vtable;
        this->do_vtable(header,client,vtable,new_vtable);
        DBG_SL_VTABLE(BF(" wrote weak base %p vtable in memory at %p value: %p to %p %s\n") % (void*)this->_vtableRegionStart % (void*)client % (void*)vtable % (void*)new_vtable % header->description() );
      }
    }
  }
};







ISLFileHeader* do_image_save(gctools::clasp_ptr_t& islStart, gctools::clasp_ptr_t& islEnd,
                             gctools::clasp_ptr_t& objectFileStart, gctools::clasp_ptr_t& objectFileEnd )
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

  ISLInfo islInfo(core::SaveOp);

  DBG_SL(BF("0. Prepare objects for image save\n"));
  prepare_for_image_save_t prepare(islInfo);
  walk_garbage_collected_objects(true,prepare);

  DBG_SL(BF("1. Sum size of all objects\n"));
  calculate_size_t calc_size(islInfo);
  walk_garbage_collected_objects(true,calc_size);
  printf("%s", (BF("   size = %lu\n") % calc_size._TotalSize).str().c_str());
  printf("%s", (BF("   general_count = %lu\n") % calc_size._general_count ).str().c_str());
  printf("%s", (BF("   cons_count = %lu\n") % calc_size._cons_count ).str().c_str());
  printf("%s", (BF("   weak_count = %lu\n") % calc_size._weak_count ).str().c_str());

  //
  // 2. Allocate that amount of memory + space for roots -> intermediate-buffer
  //

  size_t roots = sizeof(ISLHeader_s)* (1 + NUMBER_OF_CORE_SYMBOLS + global_symbol_count );
  size_t memory_size = 
    + calc_size._TotalSize            // for all objects
    + roots                                            // size for roots
    + sizeof(ISLHeader_s)*2;                           // size of last End header
  // Align up memory_size to pagesize
  size_t total_size = memory_size + sizeof(ISLFileHeader);
  total_size = ((total_size+getpagesize())/getpagesize()) * getpagesize();
  
  char* islbuffer0 = (char*)malloc( total_size );
  char* islbuffer = islbuffer0 + sizeof(ISLFileHeader);
  char* objectFileBuffer = (char*)malloc( calc_size._ObjectFileTotalSize );
  globalISLBufferRange._Start = (gctools::clasp_ptr_t)islbuffer;
  globalISLBufferRange._End = (gctools::clasp_ptr_t)islbuffer+memory_size;
  islStart = (gctools::clasp_ptr_t)islbuffer0;
  islEnd = (gctools::clasp_ptr_t)islbuffer+memory_size;

  // Add the image save load buffer limits to islInfo
  islInfo._islStart = (uintptr_t)islStart;
  islInfo._islEnd = (uintptr_t)islEnd;
 
  //
  // 3. Walk all objects in memory
  //     (a) copy them to next position in intermediate-buffer
  //     (b) Set a forwarding pointer in the original object
  //

  DBG_SL(BF("  islbuffer = %p\n") % (void*)islbuffer );
  DBG_SL(BF("  Copy objects to islbuffer: %p - %p bytes: %lu\n") % (void*)islbuffer % (void*)(islbuffer+memory_size) % memory_size );
  copy_objects_t copy_objects( islbuffer, objectFileBuffer, calc_size._ObjectFileTotalSize, islInfo );
  walk_garbage_collected_objects(true,copy_objects);

  ISLFileHeader* fileHeader = new (islbuffer0) ISLFileHeader(memory_size,copy_objects._NumberOfObjects,(uintptr_t)islbuffer);

  ISLEndHeader_s end_header( End );
  char* endend = copy_objects.write_buffer( sizeof(end_header), (char*)&end_header );
  DBG_SAVECOPY(BF("   copying END into buffer @ %p\n") % (void*)endend );

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

  fileHeader->_PageAlignedObjectFileStart = total_size;
  fileHeader->_ObjectFileSize = calc_size._ObjectFileTotalSize;
  objectFileStart = (gctools::clasp_ptr_t)objectFileBuffer;
  objectFileEnd = objectFileStart + calc_size._ObjectFileTotalSize;
  
  //
  // 5. Walk all objects in intermediate-buffer and fixup tagged pointers using forwarding pointer
  //

  DBG_SL(BF("  Fixing objects starting at %p\n") % (void*)islbuffer);
  {
    fixup_objects_t fixup_objects(core::SaveOp, (gctools::clasp_ptr_t)islbuffer, islInfo );
    globalPointerFix = follow_forwarding_pointer;
    walk_image_save_load_objects((ISLHeader_s*)islbuffer,fixup_objects);
  }


  //
  // 6. Fixup pointers in roots
  //

  {
    globalPointerFix = follow_forwarding_pointer;
    printf("%s:%d:%s  Fixing roots islbuffer = %p - %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)islbuffer, (void*)((char*)islbuffer+memory_size));
    gctools::clasp_ptr_t* lispRoot = (gctools::clasp_ptr_t*) ((char*)islbuffer + fileHeader->_LispRootOffset + sizeof(ISLRootHeader_s));
    followForwardingPointersForRoots( lispRoot, fileHeader->_LispRootCount, (void*)&islInfo );
    gctools::clasp_ptr_t* coreSymbolRoots = (gctools::clasp_ptr_t*) ((char*)islbuffer + fileHeader->_CoreSymbolRootsOffset + sizeof(ISLRootHeader_s));
    followForwardingPointersForRoots( coreSymbolRoots, fileHeader->_CoreSymbolRootsCount, (void*)&islInfo );
    gctools::clasp_ptr_t* symbolRoots = (gctools::clasp_ptr_t*) ((char*)islbuffer + fileHeader->_SymbolRootsOffset + sizeof(ISLRootHeader_s));
    followForwardingPointersForRoots( symbolRoots, fileHeader->_SymbolRootsCount, (void*)&islInfo );
  }

  //
  // Last thing - fixup vtables
  //
  {
    DBG_SL(BF(" image_save fixing up vtable pointers\n"));
    gctools::clasp_ptr_t start;
    gctools::clasp_ptr_t end;
    core::executableVtableSectionRange(start,end);
    fixup_vtables_t fixup_vtables( true, (uintptr_t)start, (uintptr_t)end, islInfo );
    walk_image_save_load_objects((ISLHeader_s*)islbuffer,fixup_vtables);
  }
  return fileHeader;
}

void image_save(const std::string& filename) {
  gctools::clasp_ptr_t  start;
  gctools::clasp_ptr_t  end;
  gctools::clasp_ptr_t  objectFileStart;
  gctools::clasp_ptr_t  objectFileEnd;
  ISLFileHeader* fileHeader = do_image_save(start,end,objectFileStart,objectFileEnd);
  fileHeader->_LibrariesOffset = (end - start);
  fileHeader->_NumberOfLibraries = globalISLLibraries.size();

  std::ofstream wf(filename, std::ios::out | std::ios::binary);
  if (!wf) {
    printf("Cannot open file %s\n", filename.c_str());
    return;
  }
  
  wf.write( (char*)start, (end - start ));
  printf( "%s:%d:%s Writing out %lu globalISLLibraries\n", __FILE__, __LINE__, __FUNCTION__, globalISLLibraries.size() );
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
  
  if (getenv("CLASP_PAUSE_EXIT")) {
    printf("%s:%d PID = %d  Paused at exit - press enter to continue: \n", __FILE__, __LINE__, getpid() );
    fflush(stdout);
    getchar();
  }
    
  free(start);
  exit(0);
}

struct temporary_root_holder_t {
  void** _buffer;
  size_t _Number;
  void** _Cur;

  temporary_root_holder_t(size_t num) : _Number(num){
    if (num==0) {
      printf("The number of objects being loaded is zero!!! That cannot be!!!\n");
      exit(1);
    }
    this->_buffer = (void**)gctools::RootClassAllocator<void*>::allocateRootsAndZero(num);
    this->_Cur = this->_buffer;
  }

  void release() {
    gctools::RootClassAllocator<void*>::freeRoots(this->_buffer);
  }

  //
  // Write a pointer into the temporary roots
  //
  void add(void* ptr) {
    if ((this->_Cur-this->_buffer) > this->_Number ) {
      printf("Overflowed temporary root buffer %lu\n", this->_Number );
      std::exit(1);
    }
    *this->_Cur = ptr;
    this->_Cur++;
  }

};


template <typename Walker>
void walk_temporary_root_objects( const temporary_root_holder_t& roots, Walker& walker) {
  DBG_SL_WALK_TEMP(BF("Starting walk of %lu roots at %p\n") % roots._Number % (void*)roots._buffer);
  for (size_t idx = 0; idx< roots._Number; idx++ ) {
    core::T_O* tagged_client = (core::T_O*)roots._buffer[idx];
    // This will handle general and weak objects
    if (gctools::tagged_generalp(tagged_client)) {
      core::T_O* untagged_client = gctools::untag_general<core::T_O*>(tagged_client);
      gctools::Header_s* header = (gctools::Header_s*)gctools::GeneralPtrToHeaderPtr(untagged_client);
      DBG_SL_WALK_TEMP(BF("Walking to GC managed general or weak header %p %s\n") % (void*)header % header->description() );
      walker.callback(header);
    } else if (gctools::tagged_consp(tagged_client)) {
      core::T_O* untagged_client = gctools::untag_cons<core::T_O*>(tagged_client);
      gctools::Header_s* header = (gctools::Header_s*)gctools::ConsPtrToHeaderPtr(untagged_client);
      DBG_SL_WALK_TEMP(BF("Walking to GC managed cons header %p %s\n") % (void*)header % header->description() );
      walker.callback(header);
    } else {
      printf("%s:%d:%s Illegal temporary root pointer %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)tagged_client);
      abort();
    }
  }
}


intptr_t globalSavedBase;
intptr_t globalLoadedBase;

gctools::clasp_ptr_t relocate_pointer(gctools::clasp_ptr_t* clientAddress, gctools::clasp_ptr_t client, uintptr_t tag, void* user_data) {
  uintptr_t new_client;
  new_client = (uintptr_t)((intptr_t)client - globalSavedBase + globalLoadedBase);
  DBG_SL_RELOCATE0(BF("Relocate0 from %p  to  %p\n") % (void*)client % (void*)new_client );
  return (gctools::clasp_ptr_t)(new_client | tag);
}

struct relocate_objects_t : public walker_callback_t {
  relocate_objects_t(const ISLInfo& info) : walker_callback_t(info) {};
  void callback(gctools::Header_s* header) {
    if (header->_stamp_wtag_mtag.stampP()) {
      gctools::clasp_ptr_t client = HEADER_PTR_TO_GENERAL_PTR(header);
      size_t objectSize;
      gctools::clasp_ptr_t client_limit = isl_obj_skip(client,false,objectSize);
      isl_obj_scan( 0, client, client_limit, (void*)&this->_info );
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)HeaderPtrToConsPtr(header);
      size_t consSize;
      gctools::clasp_ptr_t client_limit = isl_cons_skip((gctools::clasp_ptr_t)client,consSize);
      isl_cons_scan( 0, client, client_limit, (void*)&this->_info );
      // printf("%s:%d:%s The object @ %p %s isPolymorphic->%d\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, header->description().c_str(), header->preciseIsPolymorphic());
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)HEADER_PTR_TO_WEAK_PTR(header);
      size_t objectSize;
      gctools::clasp_ptr_t client_limit = isl_weak_skip(clientStart,false,objectSize);
      isl_weak_scan( 0, clientStart, client_limit, (void*)&this->_info );
    }
  }
};



int image_load(const std::string& filename )
{
  // When loading forwarding pointers must always forward into GC managed objects
  globalFwdMustBeInGCMemory = true;
  core::FunctionDescription_O funcdes;
  DBG_SL(BF("FunctionDescription_O vtable pointer is: %p\n") % *(void**)&funcdes );
  DBG_SL(BF(" image_load entered\n"));
  int fd = open(filename.c_str(),O_RDONLY);
  off_t fsize = lseek(fd, 0, SEEK_END);
  lseek(fd,0,SEEK_SET);
  void* memory = mmap(NULL, fsize, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_FILE, fd, 0);
  if (memory==MAP_FAILED) {
    close(fd);
    SIMPLE_ERROR(BF("Could not mmap %s because of %s") % filename % strerror(errno));
  }

  ISLFileHeader* fileHeader = reinterpret_cast<ISLFileHeader*>(memory);
  if (!fileHeader->good_magic()) {
    printf("The file is not an image file\n");
    exit(1);
  }
  gctools::clasp_ptr_t islbuffer = (gctools::clasp_ptr_t)(fileHeader+1);
  gctools::clasp_ptr_t islend = islbuffer+fileHeader->_Size;

  
  ISLInfo islInfo(core::LoadOp,(uintptr_t)islbuffer,(uintptr_t)islend);

  printf("%s:%d:%s Loaded file %s\n", __FILE__, __LINE__, __FUNCTION__, filename.c_str());
  printf("%s:%d:%s islbuffer = %p\n",  __FILE__, __LINE__, __FUNCTION__,(void*)islbuffer);
  //
  // Setup the libraries
  //
  ISLLibraryHeader_s* libheader = (ISLLibraryHeader_s*)((char*)fileHeader + fileHeader->_LibrariesOffset);
  printf("%s:%d:%s Registering %lu globalISLLibraries from image\n", __FILE__, __LINE__, __FUNCTION__, fileHeader->_NumberOfLibraries );
  for ( size_t idx =0; idx<fileHeader->_NumberOfLibraries; idx++ ) {
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
  printf("%s:%d:%s Number of globalISLLibraries %lu\n", __FILE__, __LINE__, __FUNCTION__, globalISLLibraries.size() );

  
  //
  // Define the buffer range
  //
  globalISLBufferRange._Start = (gctools::clasp_ptr_t)islbuffer;
  globalISLBufferRange._End = (gctools::clasp_ptr_t)islbuffer+fileHeader->_Size;


  //
  // Fixup the vtables
  //
  DBG_SL(BF(" image_load fixing up vtable pointers\n"));
  gctools::clasp_ptr_t start;
  gctools::clasp_ptr_t end;
  core::executableVtableSectionRange(start,end);
  fixup_vtables_t fixup_vtables( false, (uintptr_t)start, (uintptr_t)end, islInfo );
  walk_image_save_load_objects((ISLHeader_s*)islbuffer,fixup_vtables);

  //
  // Let's fix the pointers so that they are correct for the loaded location in memory
  //

  DBG_SL(BF(" image_load relocating addresses\n"));
  globalSavedBase = (intptr_t)fileHeader->_SaveBufferStart;
  globalLoadedBase = (intptr_t)islbuffer;
  DBG_SL(BF("  Starting   globalSavedBase %p    globalLoadedBase  %p\n") % (void*)globalSavedBase % (void*)globalLoadedBase );
  globalPointerFix = relocate_pointer;
  relocate_objects_t relocate_objects(islInfo);
  walk_image_save_load_objects( (ISLHeader_s*)islbuffer, relocate_objects );
  
  // Do the roots as well
  // After this they will be internally consistent with the loaded objects
  
  gctools::clasp_ptr_t* lispRoot = (gctools::clasp_ptr_t*) ((char*)islbuffer + fileHeader->_LispRootOffset + sizeof(ISLRootHeader_s));
  relocateLoadedRootPointers(lispRoot,1, (void*)&islInfo );
  gctools::clasp_ptr_t* coreSymbolRoots = (gctools::clasp_ptr_t*) ((char*)islbuffer + fileHeader->_CoreSymbolRootsOffset + sizeof(ISLRootHeader_s));
  relocateLoadedRootPointers( coreSymbolRoots, fileHeader->_CoreSymbolRootsCount, (void*)&islInfo );
  gctools::clasp_ptr_t* symbolRoots = (gctools::clasp_ptr_t*) ((char*)islbuffer + fileHeader->_SymbolRootsOffset + sizeof(ISLRootHeader_s));
  relocateLoadedRootPointers( symbolRoots, fileHeader->_SymbolRootsCount, (void*)&islInfo  );
    
  //
  // Fixup the CodeBase_O objects
  //
  DBG_SL(BF("Fixup the Library_O objects\n"));
  struct fixup_CodeBase_t : public walker_callback_t {
    fixup_CodeBase_t(const ISLInfo& info) : walker_callback_t(info) {};
    
    void callback(gctools::Header_s* header) {
      if (header->_stamp_wtag_mtag.stampP() && header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_llvmo__Library_O)) {
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
  fixup_CodeBase_t fixupCodeBase( islInfo );
  walk_image_save_load_objects((ISLHeader_s*)islbuffer,fixupCodeBase);

  //
  // Allocate space for temporary roots
  //

  temporary_root_holder_t root_holder(fileHeader->_NumberOfObjects);

  //
  // Allocate new objects for everything we just loaded and set the forwarding pointers
  //
  DBG_SL(BF("Allocate objects\n"));
  {
    ISLHeader_s* start_header = reinterpret_cast<ISLHeader_s*>(fileHeader+1);
    gctools::clasp_ptr_t startVtables;
    gctools::clasp_ptr_t end;
    core::executableVtableSectionRange(startVtables,end);
    DBG_SL(BF(" allocate objects\n"));
    ISLHeader_s* next_header;
    ISLHeader_s* cur_header;


    core::Lisp_O* theLisp;
    for ( cur_header = start_header; cur_header->_Kind != End; ) {
      DBG_SL_ALLOCATE(BF("-----Allocating based on cur_header %p\n") % (void*)cur_header );
      if (cur_header->_Kind == Object) {
        ISLGeneralHeader_s* generalHeader = (ISLGeneralHeader_s*)cur_header;
        if (generalHeader->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_core__Lisp_O)) {
          printf("%s:%d:%s Found Lisp_O at %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)generalHeader );
          gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)(generalHeader+1);
          theLisp = (core::Lisp_O*)clientStart;
        }
      }
      next_header = cur_header->next();
      size_t size = cur_header->_Size;
      DBG_SL1(BF("Done working with cur_header@%p  advanced to %p where cur_header->_Size = %lu\n") % (void*)cur_header % (void*)next_header % size );
      cur_header = next_header;
    }
    ///
    // Now we have the Lisp_O
    //
    // Initialize the ClaspJIT_O object
    llvmo::global_JITDylibCounter.store(fileHeader->_global_JITDylibCounter);
    llvmo::ClaspJIT_O* claspJIT = (llvmo::ClaspJIT_O*)gctools::untag_general<core::T_O*>(theLisp->_Roots._ClaspJIT.raw_());
    llvmo::JITDylib_O* mainJITDylib = (llvmo::JITDylib_O*)gc::untag_general<core::T_O*>(claspJIT->_MainJITDylib.raw_());
    new (claspJIT) llvmo::ClaspJIT_O(true,mainJITDylib);
  
    //
    // Initialize the JITDylibs
    // We can't use As at this point because we are working in the image save/load buffer
    //  and the headers aren't the same as in main memory
    //
    core::T_sp tjit = theLisp->_Roots._ClaspJIT;
    if (tjit.notnilp()) {
      llvmo::ClaspJIT_sp jit = gc::As_unsafe<llvmo::ClaspJIT_sp>(tjit);
      core::T_sp cur = theLisp->_Roots._JITDylibs.load();
      while (cur.consp()) {
        llvmo::JITDylib_sp dy = gc::As_unsafe<llvmo::JITDylib_sp>(CONS_CAR(cur));
        jit->registerJITDylibAfterLoad(&*dy);
        cur = CONS_CDR(cur);
      }
    }
    
    for ( cur_header = start_header; cur_header->_Kind != End; ) {
      DBG_SL_ALLOCATE(BF("-----Allocating based on cur_header %p\n") % (void*)cur_header );
      if (cur_header->_Kind == Object) {
        ISLGeneralHeader_s* generalHeader = (ISLGeneralHeader_s*)cur_header;
        gctools::Header_s* source_header = (gctools::Header_s*)&generalHeader->_stamp_wtag_mtag;
        gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)(generalHeader+1);
        gctools::clasp_ptr_t clientEnd = clientStart + generalHeader->_Size;
        gctools::image_save_load_init_s init(generalHeader->_stamp_wtag_mtag,clientStart,clientEnd);
        DBG_SL_ALLOCATE(BF("  source_header %p  stamp: %u  size: %lu kind: %s\n")
                        % (void*) source_header
                        % generalHeader->_stamp_wtag_mtag._value
                        % generalHeader->_Size
                        % source_header->description().c_str());
        if (generalHeader->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_core__Lisp_O)) {
          printf("%s:%d:%s About to allocate Lisp_O object\n", __FILE__, __LINE__, __FUNCTION__ );
        }
        core::T_sp obj;
        obj = gctools::GCObjectAllocator<core::General_O>::image_save_load_allocate(&init);
        gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
        DBG_SL_ALLOCATE(BF("allocated general %p fwd: %p\n")
                        % (void*) obj.raw_()
                        % (void*)fwd);
        source_header->_stamp_wtag_mtag.setFwdPointer((void*)fwd);
        root_holder.add((void*)obj.raw_());
      } else if (cur_header->_Kind == Cons ) {
        ISLConsHeader_s* consHeader = (ISLConsHeader_s*)cur_header;
        gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)(consHeader+1);
        gctools::clasp_ptr_t clientEnd = clientStart + sizeof(core::Cons_O);
        gctools::Header_s* header = consHeader->header();
        core::Cons_O* cons = (core::Cons_O*)clientStart;
        auto obj = gctools::ConsAllocator<core::Cons_O,gctools::DoRegister>::image_save_load_allocate(header->_stamp_wtag_mtag, cons->_Car.load(), cons->_Cdr.load());
        gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
        DBG_SL_ALLOCATE(BF("---- Allocated Cons %p copy from %p header: %p  set fwd to %p\n")
                        % (void*)obj.raw_()
                        % (void*) header
                        % (void*)clientStart % (void*)fwd );
        header->_stamp_wtag_mtag.setFwdPointer((void*)fwd);
        root_holder.add((void*)obj.raw_());
      } else if (cur_header->_Kind == Weak ) {
        ISLWeakHeader_s* weakHeader = (ISLWeakHeader_s*)cur_header;
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
          DBG_SL_ALLOCATE(BF("allocated weak %p header: %p stamp: %lu  fwd: %p\n")
                          % (void*) obj.raw_()
                          % (void*) header
                          % (uintptr_t)kind
                          % (void*)fwd );
          header->_stamp_wtag_mtag.setFwdPointer((void*)fwd);
          root_holder.add((void*)obj.raw_());
          break;
        }
        case gctools::Header_s::StrongBucketKind: {
          auto obj = gctools::GCBucketAllocator<gctools::Buckets<core::T_sp,core::T_sp,gctools::StrongLinks>>::image_save_load_allocate(&init);
          gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
          DBG_SL_ALLOCATE(BF("allocated weak %p header: %p stamp: %lu  fwd: %p\n")
                          % (void*) obj.raw_()
                          % (void*) header
                          % (uintptr_t)kind
                          % (void*)fwd );
          header->_stamp_wtag_mtag.setFwdPointer((void*)fwd);
          root_holder.add((void*)obj.raw_());
          break;
        }
#endif
#if 0
        case gctools::Header_s::WeakMappingKind: {
          auto obj = gctools::GCMappingAllocator<gctools::Mapping<core::T_sp,core::T_sp,gctools::WeakLinks>>::image_save_load_allocate(&init);
          gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
          DBG_SL_ALLOCATE(BF("allocated weak %p header: %p stamp: %lu fwd: %p\n")
                          % (void*) obj.raw_()
                          % (void*) header
                          % (uintptr_t)kind
                          % (void*)fwd );
          header->_stamp_wtag_mtag.setFwdPointer((void*)fwd);
          root_holder.add((void*)obj.raw_());
          break;
        }
        case gctools::Header_s::StrongBucketKind: {
          auto obj = gctools::GCMappingAllocator<gctools::Mapping<core::T_sp,core::T_sp,gctools::StrongLinks>>::image_save_load_allocate(&init);
          gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
          DBG_SL_ALLOCATE(BF("allocated weak %p header: %p stamp: %lu fwd: %p\n")
                          % (void*) obj.raw_()
                          % (void*) header
                          % (uintptr_t)kind
                          % (void*)fwd );
          header->_stamp_wtag_mtag.setFwdPointer((void*)fwd);
          root_holder.add((void*)obj.raw_());
          break;
        }
#endif
#if 0
        case gctools::Header_s::WeakPointerKind: {
          auto obj = gctools::GCWeakPointerAllocator<gctools::WeakPointer>::image_save_load_allocate(&init);
          gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
          DBG_SL_ALLOCATE(BF("allocated weak %p header: %p stamp: %lu  fwd: %p\n")
                          % (void*) obj.raw_()
                          % (void*) header
                          % (uintptr_t)kind
                          % (void*)fwd );
          header->_stamp_wtag_mtag.setFwdPointer((void*)fwd);
          root_holder.add((void*)obj.raw_());
          break;
        }
#endif
        default:
            printf("%s:%d:%s  Handle allocate weak objects\n", __FILE__, __LINE__, __FUNCTION__ );
            break;
        }
      } else {
        printf("%s:%d:%s Unknown header at offset 0x%lx qword: 0x%lx\n", __FILE__, __LINE__, __FUNCTION__, (uintptr_t)cur_header - (uintptr_t)fileHeader, *(uintptr_t*)cur_header );
      }
      next_header = cur_header->next();
      size_t size = cur_header->_Size;
      DBG_SL1(BF("Done working with cur_header@%p  advanced to %p where cur_header->_Size = %lu\n") % (void*)cur_header % (void*)next_header % size );
      cur_header = next_header;
    }
    DBG_SL(BF("Done working with all objects cur_header@%p\n") % (void*)cur_header );
  }

  //
  // Walk all the objects and fixup all the pointers
  //
  DBG_SL(BF("======================= fixup pointers\n"));
  {
    fixup_objects_t fixup_objects( core::LoadOp, (gctools::clasp_ptr_t)islbuffer, islInfo );
    globalPointerFix = follow_forwarding_pointer;
    walk_temporary_root_objects(root_holder,fixup_objects);
  }

  // Fixup the roots
  //

  {
    gctools::clasp_ptr_t* lispRoot = (gctools::clasp_ptr_t*) ((char*)islbuffer + fileHeader->_LispRootOffset + sizeof(ISLRootHeader_s));
    followForwardingPointersForRoots( lispRoot, fileHeader->_LispRootCount, (void*)&islInfo );
    copyRoots((uintptr_t*)&_lisp, (uintptr_t*)lispRoot, fileHeader->_LispRootCount );
    gctools::clasp_ptr_t* coreSymbolRoots = (gctools::clasp_ptr_t*) ((char*)islbuffer + fileHeader->_CoreSymbolRootsOffset + sizeof(ISLRootHeader_s));
    followForwardingPointersForRoots( coreSymbolRoots, fileHeader->_CoreSymbolRootsCount, (void*)&islInfo );
    copyRoots((uintptr_t*)&gctools::global_core_symbols[0], (uintptr_t*)coreSymbolRoots, fileHeader->_CoreSymbolRootsCount );
    gctools::clasp_ptr_t* symbolRoots = (gctools::clasp_ptr_t*) ((char*)islbuffer + fileHeader->_SymbolRootsOffset + sizeof(ISLRootHeader_s));
    followForwardingPointersForRoots( symbolRoots, fileHeader->_SymbolRootsCount, (void*)&islInfo );
    copyRoots((uintptr_t*)&global_symbols[0], (uintptr_t*)symbolRoots, fileHeader->_SymbolRootsCount );
  }

  printf("%s:%d:%s Number of globalISLLibraries %lu\n", __FILE__, __LINE__, __FUNCTION__, globalISLLibraries.size() );
  fixup_internals_t  internals( core::LoadOp, islInfo );
  walk_temporary_root_objects( root_holder, internals );

  //
  // Release the temporary roots
  //
  printf("%s:%d:%s Not releasing temporary roots\n", __FILE__, __LINE__, __FUNCTION__ );
  //root_holder.release();

  //
  // munmap the memory
  //
#if 1
  printf("%s:%d:%s Not munmap'ing loaded image - filling with 0xc0\n", __FILE__, __LINE__, __FUNCTION__ );
  // Fill it with 0xc0
//  memset(memory,0xc0,fsize);
#else  
  int res = munmap( memory, fsize );
  if (res!=0) SIMPLE_ERROR(BF("Could not munmap memory"));
#endif

  // 
  // Initialize the main thread
  //  and some other vital objects
  //
  SYMBOL_EXPORT_SC_(CompPkg,STARthread_local_builtins_moduleSTAR);
  
  _lisp->initializeMainThread();
  comp::_sym_STARthread_safe_contextSTAR->defparameter(llvmo::ThreadSafeContext_O::create_thread_safe_context());
  comp::_sym_STARthread_local_builtins_moduleSTAR->defparameter(_Nil<core::T_O>());
  
  {
    char* pause_startup = getenv("CLASP_PAUSE_INIT");
    if (pause_startup) {
      printf("%s:%d PID = %d Paused after image-load - press enter to continue: \n", __FILE__, __LINE__, getpid() );
      fflush(stdout);
      getchar();
    }
  }
  int exitCode;
  try {
    if (ext::_sym_STARimage_save_load_startupSTAR->symbolValue().notnilp()) {
      core::T_sp fn = ext::_sym_STARimage_save_load_startupSTAR->symbolValue();
      core::eval::funcall(fn);
    } else {
      _lisp->print(BF("Clasp (copyright Christian E. Schafmeister 2014)\n"));
      _lisp->print(BF("Low level repl\n"));
      _lisp->readEvalPrintInteractive();
      _lisp->print(BF("\n"));
    }
  } catch (core::ExitProgramException &ee) {
    exitCode = ee.getExitResult();
  }

  printf("Leaving image_load\n");
  return exitCode;
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



