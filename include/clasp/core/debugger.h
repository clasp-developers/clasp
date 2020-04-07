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
#ifndef debugger_H
#define debugger_H

#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/stacks.h>



namespace core {

// Class for representing a backtrace frame in Lisp.
FORWARD(Frame);
class Frame_O : public General_O {
  LISP_CLASS(core, CorePkg, Frame_O, "frame", General_O);
  virtual ~Frame_O() {};
public:
  Frame_O(T_sp a_stype, T_sp a_return_address, T_sp a_raw_name,
          T_sp a_base_pointer, T_sp a_frame_offset, T_sp a_frame_size,
          T_sp a_function_start_address, T_sp a_function_end_address,
          T_sp a_function_description, T_sp a_arguments, T_sp a_closure)
    : stype(a_stype), return_address(a_return_address),
      raw_name(a_raw_name), base_pointer(a_base_pointer),
      frame_offset(a_frame_offset), frame_size(a_frame_size),
      function_start_address(a_function_start_address),
      function_end_address(a_function_end_address),
      function_description(a_function_description),
      arguments(a_arguments), closure(a_closure) {}
  static Frame_sp make(T_sp stype, T_sp return_address, T_sp raw_name,
                       T_sp base_pointer, T_sp frame_offset,
                       T_sp frame_size, T_sp function_start_address,
                       T_sp function_end_address, T_sp function_description,
                       T_sp arguments, T_sp closure) {
    GC_ALLOCATE_VARIADIC(Frame_O, ret,
                         stype, return_address, raw_name,
                         base_pointer, frame_offset, frame_size,
                         function_start_address, function_end_address,
                         function_description, arguments, closure);
    return ret;
  }
public: // NOTE: Use the accessors. Non-public only for GC reasons.
  T_sp stype;
  T_sp return_address;
  T_sp raw_name;
  T_sp function_name; // initialized later
  T_sp print_name; // initialized later
  T_sp base_pointer;
  T_sp frame_offset;
  T_sp frame_size;
  T_sp function_start_address;
  T_sp function_end_address;
  T_sp function_description;
  T_sp arguments;
  T_sp closure;
};

T_sp core__backtrace_frame_type(Frame_sp frame);
T_sp core__backtrace_frame_return_address(Frame_sp frame);
T_sp core__backtrace_frame_raw_name(Frame_sp frame);
T_sp core__backtrace_frame_function_name(Frame_sp frame);
T_sp core__backtrace_frame_print_name(Frame_sp frame);
T_sp core__backtrace_frame_base_pointer(Frame_sp frame);
T_sp core__backtrace_frame_offset(Frame_sp frame);
T_sp core__backtrace_frame_function_start_address(Frame_sp frame);
T_sp core__backtrace_frame_function_end_address(Frame_sp frame);
T_sp core__backtrace_frame_function_description(Frame_sp frame);
T_sp core__backtrace_frame_arguments(Frame_sp frame);
T_sp core__backtrace_frame_closure (Frame_sp frame);

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
/*! This class controls the single-step state of the Lisp interpreter
  in an exception safe way.
  When you want to force a form to execute in single step mode
  you declare a LispDebugger(lisp,true) in the scope where you will
  evaluate the form and then when the form finishes it will restore
  the single step state to what it was.
*/

void core__low_level_backtrace();
void core__clib_backtrace(int depth = 999999999);

FORWARD(InvocationHistoryFrameIterator);

class LispDebugger {
private:
  bool _CanContinue;
  T_sp _Condition;

public:
  /* Immediatly returns if we are not single stepping or if the step stack level
	   is less than the current stack level.
	   Otherwise print the next instruction to be evaluated and wait for
	   the user to indicate what they want to do. */
  static void step();

public:
  /*! Print the current expression */
  void printExpression(T_sp stream);

  /*! Invoke the debugger,
	  If the user is allowed to resume and opts to resume then return the resume object 
	*/
  T_sp invoke();

  InvocationHistoryFrameIterator_sp currentFrame() const;

  LispDebugger(T_sp condition);
  LispDebugger();

  virtual ~LispDebugger() {
    _G();
    _lisp->decrementDebuggerLevel();
  };
};


void dbg_lowLevelDescribe(T_sp obj);
void dbg_describe_tagged_T_Optr(T_O *p);

 bool check_for_frame(uintptr_t);
 void frame_check(uintptr_t);


void core__gotoIhsTop();
void core__gotoIhsNext();
void core__gotoIhsPrev();
void core__printCurrentIhsFrame();

int safe_backtrace(void**& return_buffer);

bool lookup_stack_map_entry(uintptr_t functionPointer, int& frameOffset, int& frameSize);
void register_jitted_object(const std::string& name, uintptr_t address, int size);

void push_one_llvm_stackmap(bool jit, uintptr_t& startAddress );

void register_llvm_stackmaps(uintptr_t startAddress, uintptr_t endAddress, size_t numberStackmaps);

 bool if_dynamic_library_loaded_remove(const std::string& libraryName);

 void add_dynamic_library_using_handle(const std::string& libraryName, void* handle);
 void add_dynamic_library_using_origin(bool is_executable, const std::string& libraryName, uintptr_t origin);
 
 void startup_register_loaded_objects();

bool lookup_address(uintptr_t address, const char*& symbol, uintptr_t& start, uintptr_t& end, char& type );

 typedef enum {undefined,symbolicated,lispFrame,cFrame} BacktraceFrameEnum ;
struct BacktraceEntry {
  BacktraceEntry() : _Stage(undefined),_ReturnAddress(0),_FunctionStart(0),_FunctionEnd(~0),_BasePointer(0),_InstructionOffset(0),_FrameSize(0),_FrameOffset(0), _FunctionDescription(0), _InvocationHistoryFrameAddress(0) {};
  BacktraceFrameEnum   _Stage;
  uintptr_t            _ReturnAddress;
  uintptr_t            _FunctionStart;
  uintptr_t            _FunctionEnd;
  uintptr_t            _BasePointer;
  int                  _InstructionOffset;
  int                  _FrameSize;
  int                  _FrameOffset;
  std::string          _SymbolName;
  uintptr_t            _FunctionDescription;
  uintptr_t            _InvocationHistoryFrameAddress;
};

};

std::string _safe_rep_(core::T_sp obj);
std::string dbg_safe_repr(uintptr_t raw);

extern "C" {
void dbg_safe_print(uintptr_t raw);
void dbg_safe_println(uintptr_t raw);
void dbg_safe_backtrace();
void dbg_safe_backtrace_stderr();
};

#if 0
// To turn this on enable SOURCE_DEBUG in wscript.config and set the 0 above to 1
// If you turn this on it takes a LOT of stack memory!!! and it runs even if DEBUG_SOURCE IS ON!!!!
#define BT_LOG(msg) {char buf[1024]; sprintf msg; LOG(BF("%s") % buf);}
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

typedef void(*scan_callback)(std::vector<BacktraceEntry>&backtrace, const std::string& filename, uintptr_t start);


struct SymbolEntry {
  uintptr_t    _Address;
  char         _Type;
  uint         _SymbolOffset;
  SymbolEntry() {};
  SymbolEntry(uintptr_t start, char type, int symbolOffset) : _Address(start), _Type(type), _SymbolOffset(symbolOffset) {};
  bool operator<(const SymbolEntry& other) {
    return this->_Address < other._Address;
  }
  const char* symbol(const char* symbol_names) {
    return symbol_names+this->_SymbolOffset;
  }
};


struct SymbolTable {
  char* _SymbolNames;
  uint   _End;
  uint   _Capacity;
  uintptr_t _SymbolsLowAddress;
  uintptr_t _SymbolsHighAddress;
  uintptr_t _StackmapStart;
  uintptr_t _StackmapEnd;
  std::vector<SymbolEntry> _Symbols;
  SymbolTable() : _End(0), _Capacity(1024), _SymbolsLowAddress(~0), _SymbolsHighAddress(0), _StackmapStart(0), _StackmapEnd(0) {
    this->_SymbolNames = (char*)malloc(this->_Capacity);
  }
  ~SymbolTable() {
  };
  void addSymbol(std::string symbol, uintptr_t start, char type);

  // Shrink the symbol table to the minimimum size
  void optimize() {
    if (this->_End>0) {
      size_t newCapacity = this->_End+16&(~0x7);
      this->_SymbolNames = (char*)realloc(this->_SymbolNames,newCapacity);
      this->_Capacity = newCapacity;
    } else {
      if (this->_SymbolNames) free(this->_SymbolNames);
      this->_SymbolNames = 0;
      this->_Capacity = 0;
    }
  }
  // Return true if a symbol is found that matches the address
  bool findSymbolForAddress(uintptr_t address,const char*& symbol, uintptr_t& startAddress, uintptr_t& endAddress, char& type, size_t& index);
  void sort(); 
  bool is_sorted() const;
  
  std::vector<SymbolEntry>::iterator begin() { return this->_Symbols.begin(); };
  std::vector<SymbolEntry>::iterator end() { return this->_Symbols.end(); };
  std::vector<SymbolEntry>::const_iterator begin() const { return this->_Symbols.begin(); };
  std::vector<SymbolEntry>::const_iterator end() const { return this->_Symbols.end(); };
};

  
struct ScanInfo {
  size_t  _Index;
  std::vector<BacktraceEntry>* _Backtrace;
  scan_callback _Callback;
  size_t _symbol_table_memory;
  ScanInfo() : _Index(0), _symbol_table_memory(0) {};
};

struct JittedObject {
  std::string _Name;
  uintptr_t _ObjectPointer;
  int       _Size;
  JittedObject() {};
  JittedObject(const std::string& name, uintptr_t fp, int fs) : _Name(name), _ObjectPointer(fp), _Size(fs) {};
};


struct FrameMap {
  uintptr_t _FunctionPointer;
  int   _FrameOffset;
  int   _FrameSize;
  FrameMap() {};
  FrameMap(uintptr_t fp, int fo, int fs) : _FunctionPointer(fp), _FrameOffset(fo), _FrameSize(fs) {};
  FrameMap(const FrameMap& o) {
    this->_FunctionPointer = o._FunctionPointer;
    this->_FrameOffset = o._FrameOffset;
    this->_FrameSize = o._FrameSize;
  }
};


struct OpenDynamicLibraryInfo {
  std::string    _Filename;
  void*          _Handle;
  SymbolTable    _SymbolTable;
  uintptr_t      _LibraryOrigin;
  OpenDynamicLibraryInfo(const std::string& f, void* h, const SymbolTable& symbol_table, uintptr_t liborig) : _Filename(f), _Handle(h), _SymbolTable(symbol_table), _LibraryOrigin(liborig) {};
  OpenDynamicLibraryInfo() {};
};

struct StackMapRange {
  uintptr_t _StartAddress;
  uintptr_t _EndAddress;
  size_t _Number;
  StackMapRange(uintptr_t s, uintptr_t e, size_t number) : _StartAddress(s), _EndAddress(e), _Number(number) {};
  StackMapRange() : _StartAddress(0), _EndAddress(0), _Number(0) {};
};



struct DebugInfo {
#ifdef CLASP_THREADS
  mutable mp::SharedMutex _OpenDynamicLibraryMutex;
#endif
  map<std::string, OpenDynamicLibraryInfo> _OpenDynamicLibraryHandles;
  mp::SharedMutex                   _StackMapsLock;
  std::map<uintptr_t,StackMapRange> _StackMaps;
  mp::SharedMutex                   _JittedObjectsLock;
  std::vector<JittedObject>         _JittedObjects;
  DebugInfo() : _OpenDynamicLibraryMutex(OPENDYLB_NAMEWORD),
                _StackMapsLock(STCKMAPS_NAMEWORD),
                _JittedObjectsLock(JITDOBJS_NAMEWORD)
  {};
};

void core__start_debugger_with_backtrace(T_sp backtrace);
T_mv core__call_with_backtrace(Function_sp closure, bool args_as_pointers);


void startup_register_loaded_objects();

uintptr_t load_stackmap_info(const char* filename, uintptr_t header, size_t& section_size);
void search_symbol_table(std::vector<BacktraceEntry>& backtrace, const char* filename, size_t& symbol_table_size);
void walk_loaded_objects(std::vector<BacktraceEntry>& backtrace, size_t& symbol_table_memory);
void add_dynamic_library_impl(bool is_executable, const std::string& libraryName, bool use_origin, uintptr_t library_origin, void* handle);
DebugInfo& debugInfo();



};

extern "C" {
// Generate a backtrace with JIT symbols resolved 
void c_bt();
void c_btcl();


};

#endif
