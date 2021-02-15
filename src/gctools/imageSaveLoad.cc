/*
    File: imageSaveLoad.cc

*/



#define DEBUG_LEVEL_FULL

//#include <llvm/Support/system_error.h>
#include <dlfcn.h>
#include <iomanip>
#include <clasp/core/foundation.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/debugger.h>
#include <clasp/llvmo/code.h>
#include <clasp/gctools/gc_boot.h>


#if 1
#define DBG_SL(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL(_fmt_)
#endif
#if 0
#define DBG_SL2(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL2(_fmt_)
#endif
#if 0
#define DBG_SL3(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL3(_fmt_)
#endif


namespace gctools {

uintptr_t global_image_save_load_base;

clasp_ptr_t follow_forwarding_pointer(clasp_ptr_t client, uintptr_t tag, uintptr_t base) {
  uintptr_t fwd_client;
  if (tag==gctools::general_tag) {
    Header_s* header = (Header_s*)(client - sizeof(Header_s));
    clasp_ptr_t fwd = (clasp_ptr_t)header->fwdPointer();
    fwd_client = (uintptr_t)(fwd+sizeof(Header_s));
  } else if (tag==gctools::cons_tag) {
    Header_s* header = (Header_s*)client;
    fwd_client = (uintptr_t)header->fwdPointer();
  } else {
    printf("%s:%d:%s Illegal tag %lu\n", __FILE__, __LINE__, __FUNCTION__, tag );
    abort();
  }
  uintptr_t relative_fwd_client = fwd_client - base;
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
        obj = follow_forwarding_pointer(obj,(uintptr_t)tag,global_image_save_load_base); \
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

#define OBJECT_SKIP_IN_OBJECT_SCAN isl_obj_skip
#define OBJECT_SCAN isl_obj_scan
__attribute__((optnone))
#include "obj_scan.cc"
#undef OBJECT_SCAN

#define OBJECT_SKIP_IN_OBJECT_FWD isl_obj_skip
#define OBJECT_FWD isl_obj_fwd
#include "obj_scan.cc"
#undef OBJECT_FWD

#define OBJECT_SKIP isl_obj_skip
__attribute__((optnone))
#include "obj_scan.cc"
#undef OBJECT_SKIP


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
  Roots =  0x3c2153544f4f5221, // ROOTS
  End =    0x21444e45444e4521 } ISLKind; // END


#define MAGIC_NUMBER 348235823
struct ISLFileHeader {
  size_t _Magic;
  size_t _Size;
  ISLFileHeader(size_t sz) : _Magic(MAGIC_NUMBER), _Size(sz) {};
};


struct walker_callback_t {
  virtual void callback(clasp_ptr_t header, size_t sz) = 0;
};

};

extern "C" {
void boehm_walker_callback(void* ptr, size_t sz, void* client_data) {
  gctools::walker_callback_t* walker = (gctools::walker_callback_t*)client_data;
  walker->callback((gctools::clasp_ptr_t)ptr, sz);
}
};


namespace gctools {

struct calculate_size_t : public walker_callback_t {

  size_t _total_size;
  size_t _general_count;
  size_t _cons_count;
  size_t _weak_count;
  void callback(clasp_ptr_t cheader, size_t sz) {
    size_t psize;
    int kind = GC_get_kind_and_size((void*)cheader, &psize );
    Header_s* header = reinterpret_cast<Header_s*>(cheader);
    std::string str;
    // On boehm sometimes I get unknown objects that I'm trying to avoid with the next test.
    if ( kind ==global_lisp_kind ||
         kind == global_cons_kind ||
         kind == global_class_kind ||
         kind == global_container_kind ||
         kind == global_code_kind ||
         kind == global_atomic_kind ||
         kind == global_strong_weak_kind ) {
      if (header->stampP()) {
        this->_general_count++;
        clasp_ptr_t client = (clasp_ptr_t)BasePtrToMostDerivedPtr<core::General_O>((void*)header);
        size_t delta = isl_obj_skip(client,false)-client;
        DBG_SL2(BF("kind: %d size: %lu   general header@%p -> %p  sz = %lu  obj_skip = %lu\n") % kind % psize % header % *(void**)header % sz % delta );
        if (delta > sz) {
          printf("%s:%d:%s  There is a size mismatch for header %p  boehm says %lu  must be larger than obj_skip says %lu and i\n", __FILE__, __LINE__, __FUNCTION__, *(clasp_ptr_t*)header, sz, delta );
          size_t delta2 = isl_obj_skip(client,true)-client;
        }
        this->_total_size += delta;
      } else if (header->consObjectP()) {
        DBG_SL2(BF("kind: %d size: %lu   cons header@%p -> %p\n") % kind % psize % header % *(void**)header );
        this->_cons_count++;
        clasp_ptr_t client = (clasp_ptr_t)header;
        this->_total_size += isl_cons_skip(client)-client;
      } else if (header->weakObjectP()) {
        this->_weak_count++;
        clasp_ptr_t client = (clasp_ptr_t)BasePtrToMostDerivedPtr<core::General_O>((void*)header);
        DBG_SL2(BF("weak object kind: %d size: %lu header %p   client: %p\n") % kind % psize % (void*)header % (void*)client );
        this->_total_size += isl_weak_skip(client,false)-client;
      }
    } else {
      printf("%s:%d:%s kind: %d size: %lu   unknown object %p\n", __FILE__, __LINE__, __FUNCTION__, kind, psize, header );
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



struct ISLHeader_s {
  ISLKind     _Kind;
  uintptr_t   _Size;
  ISLHeader_s(ISLKind k, uintptr_t sz) : _Kind(k), _Size(sz) {};
};



struct copy_objects_t : public walker_callback_t {
  char* _buffer;
  copy_objects_t(char* buffer) : _buffer(buffer) {};

  char* write_buffer(size_t bytes, char* source ) {
    char* addr = this->_buffer;
    memcpy((void*)this->_buffer, (const void*)source, bytes );
    this->_buffer += bytes;
    return addr;
  }
  
  void callback(clasp_ptr_t cheader, size_t sz) {
    size_t psize;
    int kind = GC_get_kind_and_size((void*)cheader, &psize );
    Header_s* header = reinterpret_cast<Header_s*>(cheader);
    std::string str;
    // On boehm sometimes I get unknown objects that I'm trying to avoid with the next test.
    if ( kind ==global_lisp_kind ||
         kind == global_cons_kind ||
         kind == global_class_kind ||
         kind == global_container_kind ||
         kind == global_code_kind ||
         kind == global_atomic_kind ||
         kind == global_strong_weak_kind ) {
      if (header->stampP()) {
        clasp_ptr_t client = (clasp_ptr_t)BasePtrToMostDerivedPtr<core::General_O>((void*)header);
        size_t bytes = isl_obj_skip(client,false)-client;
        ISLHeader_s islheader( Object, bytes );
        this->write_buffer(sizeof(ISLHeader_s), (char*)&islheader ); 
        DBG_SL2(BF("kind: %d size: %lu   copying general %p to %p bytes: %lu\n") % kind % psize % header % (void*)this->_buffer % bytes );
        char* new_addr = this->write_buffer(bytes,(char*)header);
        header->setFwdPointer( new_addr );
        header->setFwdSize( bytes );
      } else if (header->consObjectP()) {
        clasp_ptr_t client = (clasp_ptr_t)header;
        size_t bytes = isl_cons_skip(client) - client;
        ISLHeader_s islheader( Object, bytes );
        this->write_buffer(sizeof(ISLHeader_s), (char*)&islheader );
        DBG_SL2(BF("kind: %d size: %lu  copying cons %p to %p bytes: %lu\n") % kind % psize % header % (void*)this->_buffer % bytes );
        char* new_addr = this->write_buffer(bytes,(char*)header);
        header->setFwdPointer( new_addr );
        header->setFwdSize( bytes );
      } else if (header->weakObjectP()) {
        printf("%s:%d:%s kind: %d size: %lu   weak_skip\n", __FILE__, __LINE__, __FUNCTION__, kind, psize );
        clasp_ptr_t client = (clasp_ptr_t)BasePtrToMostDerivedPtr<core::General_O>((void*)header);
        size_t bytes = isl_weak_skip(client,false)-client;
        ISLHeader_s islheader( Object, bytes );
        this->write_buffer(sizeof(ISLHeader_s), (char*)&islheader ); 
        DBG_SL2(BF("kind: %d size: %lu   copying weak %p to %p bytes: %lu\n") % kind % psize % header % (void*)this->_buffer % bytes );
        char* new_addr = this->write_buffer(bytes,(char*)header);
        header->setFwdPointer( new_addr );
        header->setFwdSize( bytes );
      }
    }
  }
};

//
// walk image save/load objects that start at cur
//
template <typename Walker>
void walk_image_save_load_objects( ISLHeader_s* cur, Walker& walker) {
  while (cur->_Kind != End) {
    char* object = (char*)cur+sizeof(ISLHeader_s);
    walker.callback((gctools::clasp_ptr_t) object, cur->_Size );
    cur = (ISLHeader_s*)((char*)cur + cur->_Size + sizeof(ISLHeader_s));
  }
}





struct fixup_objects_t : public walker_callback_t {
  char* _buffer;
  fixup_objects_t(char* buffer) : _buffer(buffer) {};

  void callback(clasp_ptr_t cheader, size_t sz) {
    Header_s* header = reinterpret_cast<Header_s*>(cheader);
    if (header->stampP()) {
      clasp_ptr_t client = (clasp_ptr_t)BasePtrToMostDerivedPtr<core::General_O>((void*)header);
      clasp_ptr_t client_limit = isl_obj_skip(client,false);
      isl_obj_scan( 0, client, client_limit );
    } else if (header->consObjectP()) {
      clasp_ptr_t client = (clasp_ptr_t)header;
      clasp_ptr_t client_limit = isl_cons_skip((clasp_ptr_t)header);
      isl_cons_scan( 0, client, client_limit );
    } else if (header->weakObjectP()) {
      printf("%s:%d:%s   weak_skip\n", __FILE__, __LINE__, __FUNCTION__ );
      clasp_ptr_t client = (clasp_ptr_t)BasePtrToMostDerivedPtr<core::General_O>((void*)header);
      clasp_ptr_t client_limit = isl_weak_skip(client,false);
      isl_weak_scan( 0, client, client_limit );
    }
  }
};


void image_save(const std::string& filename)
{
  GC_stop_world_external();
  
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

  size_t roots = sizeof(ISLHeader_s)* (1 + NUMBER_OF_CORE_SYMBOLS + global_symbol_count );
  size_t total_size = calc_size._total_size            // for all objects
    + (calc_size._general_count
       + calc_size._cons_count
       + calc_size._weak_count)*sizeof(ISLHeader_s)    // ISLHeader_s for each object
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
  global_image_save_load_base = (uintptr_t)islbuffer;
  fixup_objects_t fixup_objects(islbuffer);
  walk_image_save_load_objects((ISLHeader_s*)islbuffer,fixup_objects);


  std::ofstream wf(filename, std::ios::out | std::ios::binary);
  if (!wf) {
    printf("Cannot open file %s\n", filename.c_str());
    return;
  }
  ISLFileHeader fh(total_size);
  wf.write( (char*)&fh, sizeof(fh) );
  wf.write( (char*)islbuffer, total_size );
  wf.close();
  printf("%s:%d:%s Wrote file %s - leaving image_save\n", __FILE__, __LINE__, __FUNCTION__, filename.c_str() );
  free(islbuffer);
  exit(0);
}




};






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



