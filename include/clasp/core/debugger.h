#pragma once
/*
    File: debugger.h
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

#include <clasp/core/object.h>
#include <clasp/core/lisp.h>

namespace core {

core::T_sp core__ihs_backtrace(core::T_sp outDesignator, core::T_sp msg);
int core__ihs_top();
void core__ihs_topSetLineColumn(int lineno, int column);
int core__ihs_prev(int idx);
int core__ihs_next(int idx);
core::T_sp core__ihs_fun(int idx);
core::T_sp core__ihs_arguments(int idx);
core::T_sp core__ihs_env(int idx);
/*! Return the current frame index stored in core:*ihs-current*
      Update core:*ihs-current* to a valid value */
int core__ihs_current_frame();
/*! Set the current core:*ihs-current* value.
      If the idx is out of bounds then return a valid value */
void core__gdb(T_sp msg);
int core__set_ihs_current_frame(int idx);

}; // namespace core

namespace core {

void core__low_level_backtrace();
void core__clib_backtrace(int depth = 999999999);

struct SymbolTable {
  uintptr_t _StackmapStart;
  uintptr_t _StackmapEnd;
  SymbolTable() : _StackmapStart(0), _StackmapEnd(0) {}
  ~SymbolTable(){};
};

typedef enum { Library, Executable } LoadableKind;

struct TrapProblem {
  bool _IsExecutable;
  TrapProblem(bool is_executable, const std::string& name, LoadableKind loadable) : _IsExecutable(is_executable) {
    if (is_executable != (loadable == Executable ? true : false)) {
      printf("%s:%d:%s  library: %s is_executable = %d  loadable = %s\n", __FILE__, __LINE__, __FUNCTION__, name.c_str(),
             is_executable, loadable == Executable ? "Executable" : "Library");
      printf("!!!!!!!! \n!!\n!!   is_executable does NOT match loadable\n!!\n!!\n");
    }
  }
};

struct OpenDynamicLibraryInfo {
  std::string _Filename;
  void* _Handle;
  SymbolTable _SymbolTable;
  gctools::clasp_ptr_t _LibraryStart;
  gctools::clasp_ptr_t _TextStart;
  gctools::clasp_ptr_t _TextEnd;
  bool _HasVtableSection;
  gctools::clasp_ptr_t _VtableSectionStart;
  gctools::clasp_ptr_t _VtableSectionEnd;
  OpenDynamicLibraryInfo(const std::string& f, void* h, const SymbolTable& symbol_table, gctools::clasp_ptr_t libstart,
                         gctools::clasp_ptr_t textStart, gctools::clasp_ptr_t textEnd,
                        bool hasVtableSection,
                         gctools::clasp_ptr_t vtableSectionStart, gctools::clasp_ptr_t vtableSectionEnd)
      : _Filename(f), _Handle(h), _SymbolTable(symbol_table), _LibraryStart(libstart), _TextStart(textStart), _TextEnd(textEnd),
  _HasVtableSection(hasVtableSection), _VtableSectionStart(vtableSectionStart), _VtableSectionEnd(vtableSectionEnd) {
            // printf("%s:%d:%s filename = %s\n", __FILE__, __LINE__, __FUNCTION__, f.c_str() );
        };
  virtual LoadableKind loadableKind() const { return Library; };
  OpenDynamicLibraryInfo(){};
};

struct ExecutableLibraryInfo : OpenDynamicLibraryInfo {
  ExecutableLibraryInfo(const std::string& f, void* h, const SymbolTable& symbol_table, gctools::clasp_ptr_t libstart,
                        gctools::clasp_ptr_t textStart, gctools::clasp_ptr_t textEnd,
                        bool hasVtableSection, gctools::clasp_ptr_t vtableSectionStart, gctools::clasp_ptr_t vtableSectionEnd)
      : OpenDynamicLibraryInfo(f, h, symbol_table, libstart, textStart, textEnd, hasVtableSection, vtableSectionStart, vtableSectionEnd ) {
#if 0
    printf("%s:%d:%s filename = %s\n", __FILE__, __LINE__, __FUNCTION__, f.c_str() );
    if (vtableSectionStart==NULL) {
      printf("%s:%d:%s  _VtableSectionStart was initialized to NULL\n", __FILE__, __LINE__,  __FUNCTION__ );
    } else {
      printf("%s:%d:%s  _VtableSectionStart/Stop = %p/%p\n", __FILE__, __LINE__,  __FUNCTION__, vtableSectionStart, vtableSectionEnd );
    }
#endif
  };
  virtual LoadableKind loadableKind() const { return Executable; };
};

struct add_dynamic_library {
  virtual void operator()(const OpenDynamicLibraryInfo* info) = 0;
};

struct add_library : public add_dynamic_library {
  std::vector<const OpenDynamicLibraryInfo*> _Libraries;
  virtual void operator()(const OpenDynamicLibraryInfo* info) {
    //    printf("%s:%d:%s registering library: %s start: %p  end: %p\n", __FILE__, __LINE__,  __FUNCTION__, info._Filename.c_str(),
    //    info._TextStart, info._TextEnd );
    this->_Libraries.push_back(info);
  }
};

void dbg_lowLevelDescribe(T_sp obj);
void dbg_describe_tagged_T_Optr(T_O* p);

bool check_for_frame(uintptr_t);
void frame_check(uintptr_t);

int safe_backtrace(void**& return_buffer);

bool if_dynamic_library_loaded_remove(const std::string& libraryName);

void add_dynamic_library_using_handle(add_dynamic_library* callback, const std::string& libraryName, void* handle);
void add_dynamic_library_using_origin(add_dynamic_library* callback, bool is_executable, const std::string& libraryName,
                                      uintptr_t origin, gctools::clasp_ptr_t textStart, gctools::clasp_ptr_t textEnd,
                                      bool hasVtableSection, gctools::clasp_ptr_t vtableSectionStart,
                                      gctools::clasp_ptr_t vtableSectionEnd);

bool lookup_address_in_library(gctools::clasp_ptr_t address, gctools::clasp_ptr_t& start, gctools::clasp_ptr_t& end,
                               std::string& libraryName, bool& isExecutable, uintptr_t& vtableStart, uintptr_t& vtableEnd);
bool lookup_address(uintptr_t address, const char*& symbol, uintptr_t& start, uintptr_t& end);
bool library_with_name(const std::string& name, bool isExecutable, std::string& libraryPath, uintptr_t& start, uintptr_t& end,
                       uintptr_t& vtableStart, uintptr_t& vtableEnd);

}; // namespace core


std::string _safe_rep_(core::T_sp obj);

extern "C" {
std::string dbg_safe_repr(void* raw);
void dbg_safe_print(void* raw);
void dbg_safe_println(void* raw);
void dbg_safe_backtrace();
void dbg_primitive_print_backtrace();
};

#if 0
// To turn this on enable SOURCE_DEBUG in wscript.config and set the 0 above to 1
// If you turn this on it takes a LOT of stack memory!!! and it runs even if DEBUG_SOURCE IS ON!!!!
#define BT_LOG(msg) printf msg;
#else
#define BT_LOG(msg)
#endif

namespace core {
//////////////////////////////////////////////////////////////////////
//
// Define backtrace
//

// ------------------------------------------------------------------
//
// Write messages to cl:*debug-io*
//
#if 0
#define WRITE_DEBUG_IO(fmt) core::write_bf_stream(fmt, cl::_sym_STARdebug_ioSTAR->symbolValue());
#else
#define WRITE_DEBUG_IO(fmt)
#endif

struct SymbolCallback {
  bool _debug;
  virtual bool debug() { return this->_debug; };
  virtual bool interestedInLibrary(const char* name, bool executable) { return true; };
  virtual bool interestedInSymbol(const char* name) { return true; };
  virtual void callback(const char* name, uintptr_t start, uintptr_t end){};
  SymbolCallback() : _debug(false){};
};

void walk_loaded_objects_symbol_table(SymbolCallback* symbolCallback);

struct DebugInfo {
#ifdef CLASP_THREADS
  mutable mp::SharedMutex _OpenDynamicLibraryMutex;
#endif
  map<std::string, OpenDynamicLibraryInfo*> _OpenDynamicLibraryHandles;
  DebugInfo() : _OpenDynamicLibraryMutex(OPENDYLB_NAMEWORD){};
};

void startup_register_loaded_objects(add_dynamic_library* addlib);

uintptr_t load_stackmap_info(const char* filename, uintptr_t header, size_t& section_size);
void add_dynamic_library_impl(add_dynamic_library* adder, bool is_executable, const std::string& libraryName, bool use_origin,
                              uintptr_t library_origin, void* handle, gctools::clasp_ptr_t text_start,
                              gctools::clasp_ptr_t text_end, bool hasVtableSection, gctools::clasp_ptr_t vtableSectionStart,
                              gctools::clasp_ptr_t vtableSectionEnd);

DebugInfo& debugInfo();

void executablePath(std::string& name);
void executableTextSectionRange(gctools::clasp_ptr_t& start, gctools::clasp_ptr_t& end);
void exclusiveVtableSectionRange(gctools::clasp_ptr_t& start, gctools::clasp_ptr_t& end);

}; // namespace core
