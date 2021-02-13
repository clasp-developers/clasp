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

#define DBG_SL(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}


namespace gctools {

#define SCAN_STRUCT_T int
#define ADDR_T clasp_ptr_t
#define SCAN_BEGIN(ss)
#define SCAN_END(ss)
#define POINTER_FIX(field) printf("%s:%d:%s POINTER_FIX\n",__FILE__,__LINE__,__FUNCTION__);
#define RESULT_TYPE    int
#define RESULT_OK 1
#define EXTRA_ARGUMENTS

#define OBJECT_SCAN isl_obj_scan
#define OBJECT_SKIP isl_obj_skip
#define OBJECT_FWD isl_obj_fwd
#define OBJECT_SKIP_IN_OBJECT_FWD isl_obj_skip
#define OBJECT_SKIP_IN_OBJECT_SCAN isl_obj_skip
#include "obj_scan.cc"
#undef OBJECT_FWD
#undef OBJECT_SKIP
#undef OBJECT_SCAN


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

struct walker_callback_t {
  virtual void callback(clasp_ptr_t header) = 0;
};

};

extern "C" {
void boehm_walker_callback(void* ptr, size_t sz, void* client_data) {
  gctools::walker_callback_t* walker = (gctools::walker_callback_t*)client_data;
  walker->callback((gctools::clasp_ptr_t)ptr);
}
};


namespace gctools {

struct calculate_size_t : public walker_callback_t {

  size_t _total_size;
  size_t _general_count;
  size_t _cons_count;
  size_t _weak_count;
  void callback(clasp_ptr_t cheader) {
    Header_s* header = reinterpret_cast<Header_s*>(cheader);
    std::string str;
    if (header->stampP()) {
      DBG_SL(BF("general header@%p -> %p\n") % header % *(void**)header );
      this->_general_count++;
      this->_total_size += isl_obj_skip((clasp_ptr_t)header+sizeof(Header_s),false)-(clasp_ptr_t)header;;
    } else if (header->consObjectP()) {
      DBG_SL(BF("cons header@%p -> %p\n") % header % *(void**)header );
      this->_cons_count++;
      this->_total_size += isl_cons_skip((clasp_ptr_t)header)-(clasp_ptr_t)header;
    } else if (header->weakObjectP()) {
      this->_weak_count++;
      printf("%s:%d:%s Figure out weak_skip\n", __FILE__, __LINE__, __FUNCTION__ );
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




void image_save()
{
  DBG_SL(BF(" Entered\n" ));
  
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
  DBG_SL(BF("   size = %lu\n") % calc_size._total_size);
  DBG_SL(BF("   general_count = %lu\n") % calc_size._general_count );
  DBG_SL(BF("      cons_count = %lu\n") % calc_size._cons_count );
  DBG_SL(BF("      weak_count = %lu\n") % calc_size._weak_count );
  
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



