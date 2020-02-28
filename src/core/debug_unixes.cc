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
#include <execinfo.h>
#include <dlfcn.h>
#include <clasp/core/foundation.h>

#ifdef USE_LIBUNWIND
#include <libunwind.h>
#endif
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
#ifdef _TARGET_OS_LINUX
#include <bsd/bsd.h>
#endif
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
//#include <bsd/vis.h>
#endif
#include <clasp/core/debugger.h>



namespace core {
#if defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_FREEBSD)


std::atomic<bool> global_elf_initialized;
void ensure_libelf_initialized() {
  if (!global_elf_initialized) {
    if (elf_version(EV_CURRENT) == EV_NONE) SIMPLE_ERROR(BF("ELF library initializtion failed %s") % elf_errmsg(-1));
    global_elf_initialized = true;
  }
}

SymbolTable load_linux_symbol_table(const char* filename, uintptr_t start, uintptr_t& stackmap_start, size_t& stackmap_size)
{
  stackmap_start = 0;
  SymbolTable symbol_table;
  BT_LOG((buf,"Searching symbol table %s memory-start %p\n", filename, (void*)start ));
  Elf         *elf;
  GElf_Shdr   shdr;
  Elf_Data    *data;
  int         fd, ii, count;
  ensure_libelf_initialized();
  elf_version(EV_CURRENT);
  fd = open(filename, O_RDONLY);
  if (fd < 0) {
    BT_LOG((buf,"Could not open %s", filename));
    return symbol_table;
  }
  if ((elf = elf_begin(fd, ELF_C_READ, NULL)) == NULL) {
    close(fd);
    SIMPLE_ERROR(BF("Error with elf_begin for file %s - %s") % filename % elf_errmsg(-1));
  }
  Elf_Scn     *scn = NULL;
  // Search the symbol tables for functions that contain the return address
  scn = NULL;
  uintptr_t highest_end_address(0);
  while ((scn = elf_nextscn(elf, scn)) != NULL) {
    gelf_getshdr(scn, &shdr);
    BT_LOG((buf,"Looking at section\n" ));
    if (shdr.sh_type == SHT_SYMTAB) {
      data = elf_getdata(scn, NULL);
      count = shdr.sh_size / shdr.sh_entsize;
      BT_LOG((buf,"Found SYMTAB count: %d\n", count ));
	/* Search the symbol names */
      for (ii = 0; ii < count; ++ii) {
        GElf_Sym sym;
        gelf_getsym(data, ii, &sym);
        uintptr_t symbol_start = (uintptr_t)sym.st_value+start;
        uintptr_t symbol_end = symbol_start+sym.st_size;
        if (symbol_end>highest_end_address) {
          highest_end_address = symbol_end;
        }
        char type = '?';
        if (ELF64_ST_TYPE(sym.st_info) == STT_FUNC) {
          if (ELF64_ST_BIND(sym.st_info) == STB_GLOBAL) type = 'T';
          else type = 't';
        } else if (ELF64_ST_TYPE(sym.st_info) == STT_OBJECT) {
          if (ELF64_ST_BIND(sym.st_info) == STB_GLOBAL) type = 'D';
          else type = 'd';
        }
        BT_LOG((buf,"Looking at symbol %s type: %d\n", elf_strptr(elf,shdr.sh_link , (size_t)sym.st_name), ELF64_ST_TYPE(sym.st_info)));
        std::string sname(elf_strptr(elf,shdr.sh_link , (size_t)sym.st_name));
        symbol_table.addSymbol(sname,symbol_start,type);
      }
    }
  }
  symbol_table.addSymbol("__TAIL_SYMBOL",highest_end_address,'!');
//  printf("%s:%d:%s Looking at library: %s\n", __FILE__, __LINE__, __FUNCTION__, filename);
  scn = NULL ;
  const char * name , *p;
  size_t n , shstrndx , sz ;
  if ( elf_getshdrstrndx (elf, &shstrndx ) != 0)
    SIMPLE_ERROR(BF("elf_getshdrstrndx () failed : %s.") % elf_errmsg ( -1));
  while (( scn = elf_nextscn (elf, scn )) != NULL ) { 
    if ( gelf_getshdr ( scn, &shdr ) != & shdr )
      SIMPLE_ERROR(BF("getshdr() failed : %s.") % elf_errmsg ( -1));
    name = elf_strptr (elf, shstrndx, shdr.sh_name );
    if ( name == NULL ) SIMPLE_ERROR(BF("stackmaps elf_strptr() failed : %s.") % elf_errmsg ( -1));
//    printf("%s:%d:%s Looking at section: %s\n", __FILE__, __LINE__, __FUNCTION__, name);
    if (strncmp(name,".llvm_stackmaps",strlen(".llvm_stackmaps"))==0) {
      stackmap_start = shdr.sh_addr;
      stackmap_size = shdr.sh_size;
//      printf("%s:%d:%s Found a stackmap! shdr.sh_addr = %p shdr.sh_size=%lu\n", __FILE__, __LINE__, __FUNCTION__, (void*)shdr.sh_addr, (size_t)shdr.sh_size);
    }
  }
  elf_end(elf);
  close(fd);
  return symbol_table;
}

const char* progname_full = NULL;

struct SearchInfo {
  const char* _Name;
  void* _Address;
  size_t _Index;
  SearchInfo(const char* name) : _Name(name), _Address(NULL), _Index(0) {};
};

int elf_search_loaded_object_callback(struct dl_phdr_info *info, size_t size, void* data)
{
  SearchInfo* search_callback_info = (SearchInfo*)data;
  const char* libname;
  if (search_callback_info->_Index==0 && strlen(info->dlpi_name) == 0 ) {
    if (progname_full == NULL) {
      progname_full = getprogname();
    }
    libname = progname_full;
  } else {
    libname = info->dlpi_name;
  }
  BT_LOG((buf,"Name: \"%s\" address: %p (%d segments)\n", libname.c_str(), (void*)info->dlpi_addr, info->dlpi_phnum));
  if (strcmp(libname,search_callback_info->_Name)==0) {
    search_callback_info->_Address = (void*)info->dlpi_addr;
  }
  search_callback_info->_Index++;
  return 0;
}

int elf_loaded_object_callback(struct dl_phdr_info *info, size_t size, void* data)
{
  ScanInfo* scan_callback_info = (ScanInfo*)data;
  const char *type;
  int p_type, j;
  std::string libname;
  if (scan_callback_info->_Index==0 && strlen(info->dlpi_name) == 0 ) {
    if (progname_full == NULL) {
      progname_full = getprogname();
    }
    libname = progname_full;
  } else {
    libname = info->dlpi_name;
  }
  BT_LOG((buf,"Name: \"%s\" address: %p (%d segments)\n", libname.c_str(), (void*)info->dlpi_addr, info->dlpi_phnum));
  search_symbol_table(*(scan_callback_info->_Backtrace),libname.c_str(),scan_callback_info->_symbol_table_memory);
  scan_callback_info->_Index++;
  return 0;
}

int elf_startup_loaded_object_callback(struct dl_phdr_info *info, size_t size, void* data)
{
//  printf("%s:%d:%s Startup registering loaded object %s\n", __FILE__, __LINE__, __FUNCTION__, info->dlpi_name);
  ScanInfo* scan_callback_info = (ScanInfo*)data;
  bool is_executable;
  std::string libname;
  if (scan_callback_info->_Index==0 && strlen(info->dlpi_name) == 0 ) {
    if (progname_full == NULL) {
      progname_full = getprogname();
    }
    libname = progname_full;
    is_executable = true;
  } else {
    libname = info->dlpi_name;
    is_executable = false;
  }
  add_dynamic_library_using_origin(is_executable,libname.c_str(),(uintptr_t)info->dlpi_addr);
  scan_callback_info->_Index++;
  return 0;
}


void walk_loaded_objects(std::vector<BacktraceEntry>& backtrace, size_t& symbol_table_memory)
{
  ScanInfo scan;
  scan._Backtrace = &backtrace;
    // Search the symbol tables and stackmaps
  dl_iterate_phdr(elf_loaded_object_callback,&scan);
  symbol_table_memory += scan._symbol_table_memory;
}

void* find_base_of_loaded_object(const char* name)
{
  SearchInfo search(name);
  dl_iterate_phdr(elf_search_loaded_object_callback,&search);
  return search._Address;
}

void startup_register_loaded_objects()
{
  ScanInfo scan;
  dl_iterate_phdr(elf_startup_loaded_object_callback,&scan);
}




/*! Add a dynamic library.
    If library_origin points to the start of the library then that address is used,
    otherwise it uses handle to look up the start of the library. */
void add_dynamic_library_impl(bool is_executable, const std::string& libraryName, bool use_origin, uintptr_t library_origin, void* handle) {
  // printf("%s:%d:%s Looking for executable?(%d) library |%s|\n", __FILE__, __LINE__, __FUNCTION__, is_executable, libraryName.c_str());
  BT_LOG((buf,"Starting to load library: %s\n", libraryName.c_str() ));
#ifdef CLASP_THREADS
  WITH_READ_WRITE_LOCK(debugInfo()._OpenDynamicLibraryMutex);
#endif
// Get the start of the library and the symbol_table
  if (!use_origin) {
    // Walk all objects looking for the one we just loaded
    library_origin = (uintptr_t)find_base_of_loaded_object(libraryName.c_str());
    if (library_origin==0) {
      // Try looking for _init symbol
      void* lorigin;
      Dl_info data;
      dlerror();
      void* addr = dlsym(handle,"_init");
      const char* error = dlerror();
      if (error) {
        printf("%s:%d:%s Could not find library by walking objects or by searching for external symbol '_init' - library %s dlerror = %s\n", __FILE__, __LINE__, __FUNCTION__, error, libraryName.c_str());
        abort();
      }
      int ret = dladdr(addr,&data);
      if (ret==0) {
        printf("%s:%d:%s Could not use dladdr to get start of library %s dlerror = %s\n", __FILE__, __LINE__, __FUNCTION__, libraryName.c_str(), error);
        abort();
      }
      library_origin = (uintptr_t)data.dli_fbase;
    }
  }
//  printf("%s:%d:%s data.dli_fbase = %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)data.dli_fbase);
  uintptr_t stackmap_start;
  size_t section_size;
  SymbolTable symbol_table = load_linux_symbol_table(libraryName.c_str(),library_origin,stackmap_start,section_size);
  if (is_executable) {
    symbol_table._StackmapStart = stackmap_start;
    symbol_table._StackmapEnd = stackmap_start+section_size;
  } else {
    if (stackmap_start) {
      symbol_table._StackmapStart = stackmap_start+library_origin;
      symbol_table._StackmapEnd = stackmap_start+section_size+library_origin;
    }
  }
  symbol_table.optimize();
  symbol_table.sort();
  if (!symbol_table.is_sorted()) {
    printf("%s:%d The symbol table for %s is not sorted\n", __FILE__, __LINE__, libraryName.c_str());
    abort();
  }
  BT_LOG((buf,"OpenDynamicLibraryInfo libraryName: %s handle: %p library_origin: %p\n", libraryName.c_str(),(void*)handle,(void*)library_origin));
  OpenDynamicLibraryInfo odli(libraryName,handle,symbol_table,library_origin);
  debugInfo()._OpenDynamicLibraryHandles[libraryName] = odli;
}

#endif ////////////////////////////////////////////////// _TARGET_OS_LINUX || FREEBSD

};
