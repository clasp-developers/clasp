/*
    File: debug_macosx.cc
*/

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

#include <csignal>
#include <dlfcn.h>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/arguments.h>
#include <clasp/core/myReadLine.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/pathname.h>
#include <clasp/core/debugger.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/primitives.h>
#include <clasp/core/array.h>
#include <clasp/core/bformat.h>
#include <clasp/core/write_ugly.h>
#include <clasp/core/sort.h>
#include <clasp/core/lispStream.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/core/wrappers.h>
#ifdef _TARGET_OS_DARWIN
#include <mach/mach.h>
#import <mach-o/dyld.h>
#import <mach-o/nlist.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#endif
#include <clasp/core/debugger.h>

#if defined(_TARGET_OS_DARWIN)

#if defined(__x86_64__)
#include <libproc.h>
#include <mach/mach_vm.h>
#endif

#if defined(CLASP_APPLE_SILICON)
#include <mach/mach_vm.h>
#endif

namespace core {

void walk_loaded_objects_symbol_table(SymbolCallback* callback) {
  //    printf("Add support to walk symbol tables and stackmaps for DARWIN\n");
  uint32_t num_loaded = _dyld_image_count();
  for (size_t idx = 0; idx < num_loaded; ++idx) {
    const char* filename = _dyld_get_image_name(idx);
    printf("%s:%d:%s  loaded object[%lu]: %s\n", __FILE__, __LINE__, __FUNCTION__, idx, filename);
    // Here call for library
  }
}

static void format_display_size(char buf[5], uint64_t size) {
  const char scale[] = {'B', 'K', 'M', 'G', 'T', 'P', 'E'};
  double display_size = size;
  unsigned scale_index = 0;
  while (display_size >= 999.5) {
    display_size /= 1024;
    scale_index++;
  }
  assert(scale_index < sizeof(scale) / sizeof(scale[0]));
  int precision = 0;
  if (display_size < 9.95 && display_size - (float)((int)display_size) > 0) {
    precision = 1;
  }
  int len = snprintf(buf, 5, "%.*f%c", precision, display_size, scale[scale_index]);
  assert(len > 0 && len < 5);
}

static void format_memory_protection(char buf[4], int prot) {
  int len = snprintf(buf, 4, "%c%c%c", (prot & VM_PROT_READ ? 'r' : '-'), (prot & VM_PROT_WRITE ? 'w' : '-'),
                     (prot & VM_PROT_EXECUTE ? 'x' : '-'));
  assert(len == 3);
}

static const char* share_mode_name(unsigned char share_mode) {
  switch (share_mode) {
  case SM_COW:
    return "COW";
  case SM_PRIVATE:
    return "PRV";
  case SM_EMPTY:
    return "NUL";
  case SM_SHARED:
    return "ALI";
  case SM_TRUESHARED:
    return "SHR";
  case SM_PRIVATE_ALIASED:
    return "P/A";
  case SM_SHARED_ALIASED:
    return "S/A";
  case SM_LARGE_PAGE:
    return "LPG";
  default:
    return "???";
  }
}

static bool vmmap(uintptr_t seek, uintptr_t& regionStart, uintptr_t& regionEnd, bool verbose = true, uintptr_t start = 0,
                  uintptr_t end = -1, uint32_t depth = 2048) {
  mach_port_t task = MACH_PORT_NULL;
  kern_return_t kr = task_for_pid(mach_task_self(), getpid(), &task);
  if (kr != KERN_SUCCESS) {
    printf("task_for_pid(%d) failed: %s\n", getpid(), mach_error_string(kr));
    abort();
  }
  int pid = -1;
  pid_for_task(task, &pid);
  uintptr_t next;
  for (bool first = true;; first = false) {
    mach_vm_address_t address = start;
    mach_vm_size_t size = 0;
    uint32_t depth0 = depth;
    vm_region_submap_info_data_64_t info;
    mach_msg_type_number_t count = VM_REGION_SUBMAP_INFO_COUNT_64;
    kern_return_t kr = mach_vm_region_recurse(task, &address, &size, &depth0, (vm_region_recurse_info_t)&info, &count);
    if (kr != KERN_SUCCESS || address > end) {
      if (first) {
        if (start == end) {
          printf("no virtual memory region contains address %p\n", (void*)start);
        } else {
          printf("no virtual memory region intersects %p-%p\n", (void*)start, (void*)end);
        }
      }
      break;
    }
    if (first && verbose) {
      printf("          START - END             [ VSIZE ] "
             "PRT/MAX SHRMOD DEPTH RESIDENT REFCNT TAG"
             "    FILE\n");
    }
    char vsize[5];
    format_display_size(vsize, size);
    char cur_prot[4];
    format_memory_protection(cur_prot, info.protection);
    char max_prot[4];
    format_memory_protection(max_prot, info.max_protection);
    // Get the file name for this region.
    char filename[4 + 4096] = {};
#if defined(__x86_64__)
    memset(filename, ' ', 4);
    errno = 0;
    int ret = proc_regionfilename(pid, address, filename + 4, sizeof(filename) - 4);
    if (ret <= 0 || errno != 0) {
      filename[0] = 0;
    }
#endif

    if (verbose) {
      printf("%016llx-%016llx [ %5s ] %s/%s %6s %5u %8u %6u %3u%s\n", address, address + size, vsize, cur_prot, max_prot,
             share_mode_name(info.share_mode), depth0, info.pages_resident, info.ref_count, info.user_tag, filename);
    }
    next = address + size;
    if (start <= seek && seek < next) {
      regionStart = start;
      regionEnd = next;
      return true;
    }
    start = next;
  }
  return false;
}

bool mygetsegmentsize(bool errorP, void* vmhp, const char* segname, gctools::clasp_ptr_t& segment_start, uintptr_t& segment_size) {
  const struct mach_header_64* mhp = (const struct mach_header_64*)vmhp;
  struct segment_command_64* sgp;
  struct section_64* sp;
  uint32_t i, j;
  intptr_t slide;
  uintptr_t address = (uintptr_t)vmhp;
  slide = 0;
  sp = 0;
  sgp = (struct segment_command_64*)((char*)mhp + sizeof(struct mach_header_64));
  for (i = 0; i < mhp->ncmds; i++, sgp = (struct segment_command_64*)((char*)sgp + sgp->cmdsize)) {
    if (sgp->cmd == LC_SEGMENT_64) {
      if (strcmp(sgp->segname, "__TEXT") == 0) {
        slide = (uintptr_t)mhp - sgp->vmaddr;
      }
      //      printf("%s:%d:%s Looking at segment: %s\n", __FILE__, __LINE__, __FUNCTION__, sgp->segname);
      if (strcmp(sgp->segname, segname) == 0) {
        segment_start = (gctools::clasp_ptr_t)address; // (gctools::clasp_ptr_t)sgp->vmaddr;
        segment_size = sgp->vmsize;
        return true;
      }
    }
    address += sgp->vmsize;
  }
  if (errorP) {
    printf("%s:%d:%s Could not find segment %s\n", __FILE__, __LINE__, __FUNCTION__, segname);
    abort();
  }
  return false;
}

uint8_t* mygetsectiondata(void* vmhp, const char* segname, const char* sectname, unsigned long* size) {
  const struct mach_header_64* mhp = (const struct mach_header_64*)vmhp;
  struct segment_command_64* sgp;
  struct section_64* sp;
  uint32_t i, j;
  intptr_t slide;

  slide = 0;
  sp = 0;
  sgp = (struct segment_command_64*)((char*)mhp + sizeof(struct mach_header_64));
  for (i = 0; i < mhp->ncmds; i++) {
    if (sgp->cmd == LC_SEGMENT_64) {
      if (strcmp(sgp->segname, "__TEXT") == 0) {
        slide = (uintptr_t)mhp - sgp->vmaddr;
      }
      if (strncmp(sgp->segname, segname, sizeof(sgp->segname)) == 0) {
        sp = (struct section_64*)((char*)sgp + sizeof(struct segment_command_64));
        for (j = 0; j < sgp->nsects; j++) {
          printf("%s:%d:%s sgp->segname %s   sp->sectname %s\n", __FILE__, __LINE__, __FUNCTION__, sgp->segname, sp->sectname);
          if (strncmp(sp->sectname, sectname, sizeof(sp->sectname)) == 0 &&
              strncmp(sp->segname, segname, sizeof(sp->segname)) == 0) {
            *size = sp->size;
            //   return (uint8_t*)sp;
            uint8_t* addr = ((uint8_t*)(sp->addr) + slide);
            return addr;
          }
          sp = (struct section_64*)((char*)sp + sizeof(struct section_64));
        }
      }
    }
    sgp = (struct segment_command_64*)((char*)sgp + sgp->cmdsize);
  }
  return (0);
}

uintptr_t load_stackmap_info(const char* filename, uintptr_t header, size_t& section_size) {
  // Use mygetsectiondata to walk the library because stackmaps are mmap'd
  // in places that I am not able to calculate using otool
  uint8_t* p_section = mygetsectiondata((void*)header, "__LLVM_STACKMAPS", "__llvm_stackmaps", &section_size);
  return (uintptr_t)p_section;
}

void startup_register_loaded_objects(add_dynamic_library* callback) {
  // printf("%s:%d:%s Registering loaded objects\n", __FILE__, __LINE__, __FUNCTION__);
  //    printf("Add support to walk symbol tables and stackmaps for DARWIN\n");
  uint32_t num_loaded = _dyld_image_count();
  for (size_t idx = 0; idx < num_loaded; ++idx) {
    const char* filename = _dyld_get_image_name(idx);
    //    printf("%s:%d:%s Must be a full path!!!    filename = %s\n", __FILE__, __LINE__, __FUNCTION__, filename );
    std::string libname(filename);
    uintptr_t library_origin = (uintptr_t)_dyld_get_image_header(idx);
    bool is_executable = (idx == 0);
    add_dynamic_library_using_origin(callback, is_executable, libname, library_origin, NULL, NULL, false, NULL, NULL);
  }
}

/*! Add a dynamic library.
    If library_origin points to the start of the library then that address is used,
    otherwise it uses handle to look up the start of the library. */
void add_dynamic_library_impl(add_dynamic_library* callback, bool is_executable, const std::string& libraryName, bool use_origin,
                              uintptr_t library_origin, void* handle, gctools::clasp_ptr_t dummy_text_start,
                              gctools::clasp_ptr_t dummy_text_end, bool dummyHasDataConst, gctools::clasp_ptr_t dummyDataConstStart,
                              gctools::clasp_ptr_t dummyDataConstEnd) {
  //  printf("%s:%d:%s Looking for executable?(%d) library |%s|\n", __FILE__, __LINE__, __FUNCTION__, is_executable,
  //  libraryName.c_str());
  BT_LOG(("Starting to load library: %s\n", libraryName.c_str()));
  //  printf("%s:%d:%s Entered is_executable = %d use_origin = %d library_origin = %p\n", __FILE__, __LINE__, __FUNCTION__,
  //  is_executable, use_origin, (void*)library_origin );
  WITH_READ_WRITE_LOCK(debugInfo()._OpenDynamicLibraryMutex);
  // Get the start of the library and the symbol_table
  if (!use_origin) {
    printf("%s:%d:%s Looking for library %s with handle %p\n", __FILE__, __LINE__, __FUNCTION__, libraryName.c_str(), handle);
    uint32_t num_loaded = _dyld_image_count();
    for (size_t idx = 0; idx < num_loaded; ++idx) {
      const char* filename = _dyld_get_image_name(idx);
      printf("%s:%d:%s Comparing to library: %s\n", __FILE__, __LINE__, __FUNCTION__, filename);
      if (strcmp(filename, libraryName.c_str()) == 0) {
        printf("%s:%d:%s Found library: %s !!!! I need the size!!!!!!\n", __FILE__, __LINE__, __FUNCTION__, filename);
        library_origin = (uintptr_t)_dyld_get_image_header(idx);
        break;
      }
    }
  } else {
    // printf("%s:%d:%s library: %s given library_origin @%p !!!! I need the size!!!!!!\n", __FILE__, __LINE__, __FUNCTION__,
    // libraryName.c_str(), (void*)library_origin);
  }

  uintptr_t exec_header;
  //  printf("%s:%d:%s library_origin %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)library_origin);
  dlerror();
  exec_header = (uintptr_t)dlsym(RTLD_DEFAULT, "_mh_execute_header");
  const char* dle = dlerror();
  if (dle) {
    printf("Could not find the symbol _mh_execute_header\n");
    abort();
  }
  //  printf("%s:%d:%s Executable header _mh_execute_header %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)exec_header);
  SymbolTable symbol_table;
  size_t section_size;
  //  printf("%s:%d:%s About to load_stackmap_info library_origin = %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)library_origin
  //  );
  uintptr_t p_section = load_stackmap_info(libraryName.c_str(), library_origin, section_size);
  if (p_section) {
    symbol_table._StackmapStart = p_section;
    symbol_table._StackmapEnd = p_section + section_size;
  }
  BT_LOG(("OpenDynamicLibraryInfo libraryName: %s handle: %p library_origin: %p\n", libraryName.c_str(), (void*)handle,
          (void*)library_origin));
  gctools::clasp_ptr_t text_segment_start;
  uintptr_t text_segment_size;
  mygetsegmentsize(true, (void*)library_origin, "__TEXT", text_segment_start, text_segment_size);
  //  printf("%s:%d:%s       Looking for __TEXT  library_origin = %p - %p  text_segment_start = %p - %p text_section_size = %lu\n",
  //  __FILE__, __LINE__, __FUNCTION__, (void*)library_origin, (void*)((char*)library_origin+text_segment_size),
  //  (void*)text_segment_start, (void*)((char*)text_segment_start + text_segment_size), text_segment_size );
  uintptr_t vtableRegionStart = 0;
  uintptr_t vtableRegionEnd = 0;
  bool found = false;
  if (is_executable) {
    General_O general;
    uintptr_t seek = *(uintptr_t*)&general; // get vtable
    found = vmmap(seek, vtableRegionStart, vtableRegionEnd, false);
  }
  OpenDynamicLibraryInfo* odli = NULL;
  if (is_executable) {
    odli = new ExecutableLibraryInfo(libraryName, handle, symbol_table, reinterpret_cast<gctools::clasp_ptr_t>(library_origin),
                                     reinterpret_cast<gctools::clasp_ptr_t>(library_origin),
                                     reinterpret_cast<gctools::clasp_ptr_t>(library_origin + text_segment_size),
                                     found,
                                     (gctools::clasp_ptr_t)vtableRegionStart, (gctools::clasp_ptr_t)vtableRegionEnd);
  } else {
    odli = new OpenDynamicLibraryInfo(libraryName, handle, symbol_table, reinterpret_cast<gctools::clasp_ptr_t>(library_origin),
                                      reinterpret_cast<gctools::clasp_ptr_t>(library_origin),
                                      reinterpret_cast<gctools::clasp_ptr_t>(library_origin + text_segment_size),
                                     found,
                                      (gctools::clasp_ptr_t)vtableRegionStart, (gctools::clasp_ptr_t)vtableRegionEnd);
  }
  if (callback)
    (*callback)(odli);
  debugInfo()._OpenDynamicLibraryHandles[libraryName] = odli;
}

}; // namespace core

#endif // _TARGET_OS_DARWIN
