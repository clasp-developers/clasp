/*
    File: debug_unixes.cc
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
#include <iostream>
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
#if defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_FREEBSD)
#include <err.h>
#include <fcntl.h>
#include <gelf.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <dlfcn.h>
#define _GNU_SOURCE
#include <elf.h>
#include <link.h>
#endif
#include <clasp/core/debugger.h>

namespace core {

struct ScanInfo {
  add_dynamic_library* _AdderOrNull;
  size_t _Index;
  //  std::vector<BacktraceEntry>* _Backtrace;
  //  scan_callback _Callback;
  size_t _symbol_table_memory;
  ScanInfo(add_dynamic_library* ad) : _AdderOrNull(ad), _Index(0), _symbol_table_memory(0){};
};

#if defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_FREEBSD)

const char* global_progname_full = NULL;

const char* getExecutablePath() {
  if (global_progname_full == NULL) {
    std::stringstream ss;
    ss << "/proc/" << getpid() << "/exe";
    char* result = (char*)malloc(PATH_MAX + 1);
    ssize_t count = readlink(ss.str().c_str(), result, PATH_MAX);
    result[count] = '\0';
    global_progname_full = result;
  }
  return global_progname_full;
}

std::atomic<bool> global_elf_initialized;
void ensure_libelf_initialized() {
  if (!global_elf_initialized) {
    if (elf_version(EV_CURRENT) == EV_NONE)
      SIMPLE_ERROR("ELF library initializtion failed {}", elf_errmsg(-1));
    global_elf_initialized = true;
  }
}

void walk_elf_symbol_table(struct dl_phdr_info* info, const char* filename, bool executable, uintptr_t start,
                           SymbolCallback* symbolCallback) {
  printf("%s:%d:%s entered filename = %s  start: %p  interested -> %d\n", __FILE__, __LINE__, __FUNCTION__, filename, (void*)start,
         symbolCallback->interestedInLibrary(filename, executable));

  if (!symbolCallback->interestedInLibrary(filename, executable))
    return;
  BT_LOG(("%s:%d:%s entered\n", __FILE__, __LINE__, __FUNCTION__));
  BT_LOG(("Searching symbol table %s memory-start %p\n", filename, (void*)start));
  Elf* elf;
  GElf_Shdr shdr;
  Elf_Data* data;
  int fd, ii, count;
  ensure_libelf_initialized();
  elf_version(EV_CURRENT);
  fd = open(filename, O_RDONLY);
  if (fd < 0) {
    BT_LOG(("Could not open %s", filename));
    printf("Could not open %s\n", filename);
    return;
  }
  if ((elf = elf_begin(fd, ELF_C_READ, NULL)) == NULL) {
    close(fd);
    SIMPLE_ERROR("Error with elf_begin for file {} - {}", filename, elf_errmsg(-1));
  }
  Elf_Scn* scn = NULL;
  // Search the symbol tables for functions that contain the return address
  scn = NULL;
  uintptr_t highest_end_address(0);
  uintptr_t symbol_start = 0;
  uintptr_t symbol_end;
  size_t symbolCount = 0;
  while ((scn = elf_nextscn(elf, scn)) != NULL) {
    gelf_getshdr(scn, &shdr);
    BT_LOG(("Looking at section\n"));
    if (shdr.sh_type == SHT_SYMTAB) {
      data = elf_getdata(scn, NULL);
      count = shdr.sh_size / shdr.sh_entsize;
      BT_LOG(("Found SYMTAB count: %d\n", count));
      /* Search the symbol names */
      for (ii = 0; ii < count; ++ii) {
        GElf_Sym sym;
        gelf_getsym(data, ii, &sym);
        uintptr_t addr = 0;
        symbol_start = (uintptr_t)sym.st_value + addr; // symbols are loaded at 0x0 // +start;
        symbol_end = symbol_start + sym.st_size;
        if (symbol_end > highest_end_address) {
          highest_end_address = symbol_end;
        }
        char type = '?';
        if (ELF64_ST_TYPE(sym.st_info) == STT_FUNC) {
          if (ELF64_ST_BIND(sym.st_info) == STB_GLOBAL)
            type = 'T';
          else
            type = 't';
        } else if (ELF64_ST_TYPE(sym.st_info) == STT_OBJECT) {
          if (ELF64_ST_BIND(sym.st_info) == STB_GLOBAL)
            type = 'D';
          else
            type = 'd';
        }
        BT_LOG(("Looking at symbol %s type: %d\n", elf_strptr(elf, shdr.sh_link, (size_t)sym.st_name), ELF64_ST_TYPE(sym.st_info)));
        const char* sname = elf_strptr(elf, shdr.sh_link, (size_t)sym.st_name);
        if (symbolCallback->debug()) {
          uintptr_t dlsymStart = (uintptr_t)dlsym(RTLD_DEFAULT, sname);
          if (dlsymStart != 0 && sym.st_value != 0) {
            printf("%s:%d %10s address: %p sym.st_value: %p  sym.st_shndx: %lu sym.st_size: %lu  name: %s \n", __FILE__, __LINE__,
                   ((uintptr_t)dlsymStart == (uintptr_t)sym.st_value) ? "==" : "XX!=XX", (void*)dlsymStart, (void*)sym.st_value,
                   (uintptr_t)sym.st_shndx, (uintptr_t)sym.st_size, sname);
          }
          continue;
        }
#if 1
        if (symbolCallback->interestedInSymbol(sname)) {
          uintptr_t dlsymStart = (uintptr_t)dlsym(RTLD_DEFAULT, sname);
          if (dlsymStart != 0 && (dlsymStart != symbol_start)) {
            printf("delta: 0x%0lx %s:%d:%s  The dlsym result %p does NOT match %p sym.st_shndx: %u symbol: %s\n",
                   ((intptr_t)dlsymStart - (intptr_t)symbol_start), __FILE__, __LINE__, __FUNCTION__, (void*)dlsymStart,
                   (void*)symbol_start, sym.st_shndx, sname);
          }
          symbolCallback->callback(sname, symbol_start, symbol_end);
        }
#endif
        symbolCount++;
      }
    }
  }
  //  printf("%s:%d:%s  symbolCount = %lu\n", __FILE__, __LINE__, __FUNCTION__, symbolCount );
  elf_end(elf);
  close(fd);
}

struct SearchInfo {
  const char* _Name;
  uintptr_t _Address;
  size_t _Index;
  bool _Found;
  SearchInfo(const char* name) : _Name(name), _Address((uintptr_t)NULL), _Index(0), _Found(false){};
};

int elf_search_loaded_object_callback(struct dl_phdr_info* info, size_t size, void* data) {
  SearchInfo* search_callback_info = (SearchInfo*)data;
  const char* libname;
  if (search_callback_info->_Index == 0 && strlen(info->dlpi_name) == 0) {
    libname = getExecutablePath();
  } else {
    libname = info->dlpi_name;
  }
  BT_LOG(("Name: \"%s\" address: %p (%d segments)\n", libname, (void*)info->dlpi_addr, info->dlpi_phnum));
  if (strcmp(libname, search_callback_info->_Name) == 0) {
    search_callback_info->_Address = (uintptr_t)info->dlpi_addr;
    search_callback_info->_Found = true;
    BT_LOG(("%s:%d:%s start Address: %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)info->dlpi_addr));
    BT_LOG(("%s:%d:%s dlpi_phdr = %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)info->dlpi_phdr));
    BT_LOG(("%s:%d:%s dlpi_phnum = %d\n", __FILE__, __LINE__, __FUNCTION__, info->dlpi_phnum));
  }
  search_callback_info->_Index++;
  return 0;
}

bool find_base_of_loaded_object(const char* name, uintptr_t& address) {
  BT_LOG(("%s:%d:%s \n", __FILE__, __LINE__, __FUNCTION__));
  SearchInfo search(name);
  dl_iterate_phdr(elf_search_loaded_object_callback, &search);
  if (search._Found)
    address = search._Address;
  return search._Found;
}

////////////////////////////////////////////////////////////
//
// Walk dynamic libraries
//
//

/*! Add a dynamic library.
    If library_origin points to the start of the library then that address is used,
    otherwise it uses handle to look up the start of the library. */
void walk_dynamic_library_impl(struct dl_phdr_info* info, bool is_executable, const std::string& libraryName, bool use_origin,
                               uintptr_t library_origin, SymbolCallback* symbol_callback) {
  printf("%s:%d:%s  libraryName = %s\n", __FILE__, __LINE__, __FUNCTION__, libraryName.c_str());
  if (!use_origin) {
    printf("%s:%d:%s This path should never be followed\n", __FILE__, __LINE__, __FUNCTION__);
  }
  walk_elf_symbol_table(info, libraryName.c_str(), is_executable, library_origin, symbol_callback);
}

int elf_walk_loaded_object_callback(struct dl_phdr_info* info, size_t size, void* data) {
  //  printf("%s:%d:%s Startup registering loaded object %s\n", __FILE__, __LINE__, __FUNCTION__, info->dlpi_name);
  SymbolCallback* symbol_callback = (SymbolCallback*)data;
  bool is_executable;
  const char* libname;
  if (!info->dlpi_name || strlen(info->dlpi_name) == 0) {
    libname = getExecutablePath();
    is_executable = true;
  } else {
    libname = info->dlpi_name;
    is_executable = false;
  }
  printf("%s:%d:%s   libname = %s  is_executable = %d\n", __FILE__, __LINE__, __FUNCTION__, libname, is_executable);
  gctools::clasp_ptr_t text_start;
  gctools::clasp_ptr_t text_end;
  for (int j = 0; j < info->dlpi_phnum; j++) {
    int p_type = info->dlpi_phdr[j].p_type;
#if 1
    const char* type;
    type = (p_type == PT_LOAD)           ? "PT_LOAD"
           : (p_type == PT_DYNAMIC)      ? "PT_DYNAMIC"
           : (p_type == PT_INTERP)       ? "PT_INTERP"
           : (p_type == PT_NOTE)         ? "PT_NOTE"
           : (p_type == PT_INTERP)       ? "PT_INTERP"
           : (p_type == PT_PHDR)         ? "PT_PHDR"
           : (p_type == PT_TLS)          ? "PT_TLS"
           : (p_type == PT_GNU_EH_FRAME) ? "PT_GNU_EH_FRAME"
           : (p_type == PT_GNU_STACK)    ? "PT_GNU_STACK"
           : (p_type == PT_GNU_RELRO)    ? "PT_GNU_RELRO"
                                         : NULL;
    printf("    %2d: [%14p; memsz:%7jx; END: %14p] flags: %#jx; \n", j, (void*)(info->dlpi_addr + info->dlpi_phdr[j].p_vaddr),
           (uintmax_t)info->dlpi_phdr[j].p_memsz,
           (void*)((char*)info->dlpi_addr + (uintptr_t)info->dlpi_phdr[j].p_vaddr + (uintptr_t)info->dlpi_phdr[j].p_memsz),
           (uintmax_t)info->dlpi_phdr[j].p_flags);
#endif
    if (p_type == PT_LOAD && (info->dlpi_phdr[j].p_flags & 0x1)) { // executable
      text_start = (gctools::clasp_ptr_t)(info->dlpi_addr + info->dlpi_phdr[j].p_vaddr);
      text_end = (gctools::clasp_ptr_t)(text_start + info->dlpi_phdr[j].p_memsz);
      printf("%s:%d:%s      text_start = %p     text_end = %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)text_start,
             (void*)text_end);
    }
  }
  walk_dynamic_library_impl(info, is_executable, libname, true,
                            (uintptr_t)info->dlpi_addr, // origin
                            symbol_callback);
  return 0;
}

void walk_loaded_objects_symbol_table(SymbolCallback* callback) { dl_iterate_phdr(elf_walk_loaded_object_callback, callback); }

DOCGROUP(clasp);
CL_DEFUN void core__walk_loaded_objects() {
  SymbolCallback symbol_callback;
  symbol_callback._debug = true;
  walk_loaded_objects_symbol_table(&symbol_callback);
}

int elf_startup_loaded_object_callback(struct dl_phdr_info* info, size_t size, void* data) {
//  printf("%s:%d:%s --------- Startup registering loaded object %s\n", __FILE__, __LINE__, __FUNCTION__, info->dlpi_name);
  core::General_O general;
  gctools::clasp_ptr_t vtablePtr = *(gctools::clasp_ptr_t*)&general;
//  printf("%s:%d:%s a KNOWN vtablePtr = %p\n", __FILE__, __LINE__, __FUNCTION__, vtablePtr );
  ScanInfo* scan_callback_info = (ScanInfo*)data;
  bool is_executable = false;
  const char* libname;
  if (scan_callback_info->_Index == 0 && strlen(info->dlpi_name) == 0) {
    libname = getExecutablePath();
    is_executable = true;
//    printf("%s:%d:%s getExecutablePath() libname %s is_executable = %d\n", __FILE__, __LINE__, __FUNCTION__, libname, is_executable );
  } else {
    libname = info->dlpi_name;
    is_executable = false;
//    printf("%s:%d:%s info->dlpi_name libname %s is_executable = %d\n", __FILE__, __LINE__, __FUNCTION__, libname, is_executable );
  }
  gctools::clasp_ptr_t text_start;
  gctools::clasp_ptr_t text_end;
  bool hasVtableSection = false;
  gctools::clasp_ptr_t vtableSectionStart = NULL;
  gctools::clasp_ptr_t vtableSectionEnd = NULL;
  for (int j = 0; j < info->dlpi_phnum; j++) {
    int p_type = info->dlpi_phdr[j].p_type;
    gctools::clasp_ptr_t low = (gctools::clasp_ptr_t)(info->dlpi_addr + info->dlpi_phdr[j].p_vaddr);
    gctools::clasp_ptr_t high = (gctools::clasp_ptr_t)((char*)info->dlpi_addr + (uintptr_t)info->dlpi_phdr[j].p_vaddr +
                                                       (uintptr_t)info->dlpi_phdr[j].p_memsz);
    if (p_type == PT_LOAD && (info->dlpi_phdr[j].p_flags & 0x1)) { // executable
      text_start = (gctools::clasp_ptr_t)(info->dlpi_addr + info->dlpi_phdr[j].p_vaddr);
      text_end = (gctools::clasp_ptr_t)(text_start + info->dlpi_phdr[j].p_memsz);
//      printf("%s:%d:%s      text_start = %p     text_end = %p    low = %p  high = %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)text_start, (void*)text_end, low, high );
    }
    if (low <= vtablePtr && vtablePtr < high) {
      //
      // There can be multiple ranges that contain the vtable section - for crying out loud!
      //  Use the largest one.
      if (hasVtableSection) {
        uintptr_t oldSize = vtableSectionEnd - vtableSectionStart;
        uintptr_t newSize = high - low;
        if (newSize > oldSize) {
          vtableSectionStart = low;
          vtableSectionEnd = high;
          printf("%s:%d:%s   moved vtableSection Start/End = %p/%p\n", __FILE__, __LINE__, __FUNCTION__, low, high);
        }
      } else {
        hasVtableSection = true;
        vtableSectionStart = low;
        vtableSectionEnd = high;
//        printf("%s:%d:%s   set vtableSection Start/End = %p/%p\n", __FILE__, __LINE__, __FUNCTION__, low, high );
      }
//      printf("%s:%d:%s Found vtableSection %p to %p\n", __FILE__, __LINE__, __FUNCTION__, vtableSectionStart, vtableSectionEnd );
    }
  }
  // printf("%s:%d:%s  About to call add_dynamic_library_using_origin libname = %s   is_executable = %d\n", __FILE__, __LINE__,
  // __FUNCTION__, libname, is_executable );
  add_dynamic_library_using_origin(scan_callback_info->_AdderOrNull, is_executable, libname, (uintptr_t)info->dlpi_addr, text_start,
                                   text_end, hasVtableSection, vtableSectionStart, vtableSectionEnd);
  scan_callback_info->_Index++;
  return 0;
}

void startup_register_loaded_objects(add_dynamic_library* callback) {
  ScanInfo scan(callback);
  dl_iterate_phdr(elf_startup_loaded_object_callback, &scan);
}

/*! Add a dynamic library.
    If library_origin points to the start of the library then that address is used,
    otherwise it uses handle to look up the start of the library. */
void add_dynamic_library_impl(add_dynamic_library* callback, bool is_executable, const std::string& libraryName, bool use_origin,
                              uintptr_t library_origin, void* handle, gctools::clasp_ptr_t text_start,
                              gctools::clasp_ptr_t text_end, bool hasVtableSection, gctools::clasp_ptr_t vtableSectionStart, gctools::clasp_ptr_t vtableSectionEnd) {
  // printf("%s:%d:%s libraryName = %s   is_executable = %d\n", __FILE__, __LINE__, __FUNCTION__, libraryName.c_str(), is_executable
  // );
  BT_LOG(("Starting to load library: %s\n", libraryName.c_str()));
  WITH_READ_WRITE_LOCK(debugInfo()._OpenDynamicLibraryMutex);
  // Get the start of the library and the symbol_table
  if (!use_origin) {
    printf("%s:%d:%s   This path should never be taken - use_origin must always be true!!!!!!\n", __FILE__, __LINE__, __FUNCTION__);
  }
  // Walk all objects looking for the one we just loaded
  if (!find_base_of_loaded_object(libraryName.c_str(), library_origin)) {
    // Try looking for _init symbol
    Dl_info data;
    dlerror();
    void* addr = dlsym(handle, "_init");
    const char* error = dlerror();
    if (error) {
      printf("%s:%d:%s Could not find library by walking objects or by searching for external symbol '_init' - library %s dlerror "
             "= %s\n",
             __FILE__, __LINE__, __FUNCTION__, error, libraryName.c_str());
      abort();
    }
    int ret = dladdr(addr, &data);
    if (ret == 0) {
      printf("%s:%d:%s Could not use dladdr to get start of library %s dlerror = %s\n", __FILE__, __LINE__, __FUNCTION__,
             libraryName.c_str(), error);
      abort();
    }
    library_origin = (uintptr_t)data.dli_fbase;
  }
  SymbolTable symbol_table;
  BT_LOG(("OpenDynamicLibraryInfo libraryName: %s handle: %p library_origin: %p\n", libraryName.c_str(), (void*)handle,
          (void*)library_origin));
  OpenDynamicLibraryInfo* odli = NULL;
  LoadableKind loadable;
  if (is_executable) {
    // printf("%s:%d:%s  Creating an ExecutableLibraryInfo\n", __FILE__, __LINE__, __FUNCTION__ );
    odli = new ExecutableLibraryInfo(libraryName, handle, symbol_table, (gctools::clasp_ptr_t)library_origin, text_start, text_end,
                                     hasVtableSection, vtableSectionStart, vtableSectionEnd);
    loadable = Executable;
  } else {
    // printf("%s:%d:%s  Creating an OpenDynamicLibraryInfo\n", __FILE__, __LINE__, __FUNCTION__ );
    odli =
        new OpenDynamicLibraryInfo(libraryName, handle, symbol_table, (gctools::clasp_ptr_t)library_origin, text_start, text_end,
                                   hasVtableSection, vtableSectionStart, vtableSectionEnd );
    loadable = Library;
  }
  TrapProblem trap(is_executable, libraryName, odli->loadableKind());
  if (callback)
    (*callback)(odli);
  debugInfo()._OpenDynamicLibraryHandles[libraryName] = odli;
}

#endif // _TARGET_OS_LINUX || FREEBSD

}; // namespace core
