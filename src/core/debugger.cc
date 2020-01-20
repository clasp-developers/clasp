/*
    File: debugger.cc
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

#define DEBUG_LEVEL_FULL

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
#ifdef _TARGET_OS_DARWIN
#import <mach-o/dyld.h>
#import <mach-o/nlist.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#endif
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


#ifdef _TARGET_OS_DARWIN
namespace core {
core::SymbolTable load_macho_symbol_table(bool is_executable, const char* filename, uintptr_t header, uintptr_t exec_header);
};
#endif

#if defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_FREEBSD)
namespace core {
void* find_base_of_loaded_object(const char* name);
core::SymbolTable load_linux_symbol_table(const char* filename, uintptr_t start, uintptr_t& stackmap_start, size_t& stackmap_size);
};
#endif

namespace core {


DebugInfo* global_DebugInfo = NULL;

DebugInfo& debugInfo() {
  if (!global_DebugInfo) {
    global_DebugInfo = new DebugInfo();
  }
  return *global_DebugInfo;
}



bool SymbolTable::findSymbolForAddress(uintptr_t address,const char*& symbol, uintptr_t& startAddress, uintptr_t& endAddress, char& type, size_t& index) {
  if (this->_Symbols.size() == 0) return false;
  SymbolEntry& lastSymbol = this->_Symbols[this->_Symbols.size()-1];
  BT_LOG((buf,"findSymbolForAddress %p   symbol_table startAddress %p  endAddress %p #symbols %lu, address<first->%d address>=last->%d\n",
          (void*)address,
          (void*)this->_SymbolsLowAddress,
          (void*)this->_SymbolsHighAddress,
          this->_Symbols.size(),
          (address<this->_SymbolsLowAddress),
          (address>= this->_SymbolsHighAddress)
          ));
  if (address < this->_SymbolsLowAddress) return false;
  if (address >= this->_SymbolsHighAddress) return false;
  {
    size_t l = 0;
    size_t r = this->_Symbols.size()-1;
    while (true) {
      if (l>=r) {
        index = l-1;
        goto DONE;
      }
      size_t m = (l+r)/2;
      if (this->_Symbols[m]._Address<=address) {
        l = m+1;
      } else if (this->_Symbols[m]._Address>address) {
        r = m;
      } else {
        index = l;
        goto DONE;
      }
    }
  DONE:
    symbol = this->_SymbolNames+this->_Symbols[index]._SymbolOffset;
    startAddress = this->_Symbols[index]._Address;
    endAddress = this->_Symbols[index+1]._Address;
    type = this->_Symbols[index]._Type;
    BT_LOG((buf,"findSymbolForAddress returning index %zu max %u name: %s startAddress: %p endAddress: %p type|%c|\n", index, this->_End-1, symbol, (void*)startAddress, (void*)endAddress, type));
    return true;
  }
};

void SymbolTable::addSymbol(std::string symbol, uintptr_t start, char type) {
  BT_LOG((buf,"name: %s start: %p  type |%c|\n",symbol.c_str(),(void*)start,type));
  if (start < this->_SymbolsLowAddress) this->_SymbolsLowAddress = start;
  if (this->_SymbolsHighAddress < start ) this->_SymbolsHighAddress = start;
  if ((this->_End+symbol.size()+1)>= this->_Capacity) {
    this->_SymbolNames = (char*)realloc(this->_SymbolNames,this->_Capacity*2);
    if (this->_SymbolNames == NULL ) {
      printf("%s:%d:%s Could not realloc to size %u\n", __FILE__, __LINE__, __FUNCTION__, this->_Capacity*2);
      abort();
    }
    this->_Capacity *= 2;
  }
  uint str = this->_End;
  strncpy(this->_SymbolNames+str,symbol.c_str(),symbol.size());
  this->_SymbolNames[str+symbol.size()] = '\0';
  this->_End += symbol.size()+1;
  this->_Symbols.emplace_back(start,type,str);
  BT_LOG((buf,"Wrote symbol index %lu |%s| to %u type |%c|\n",this->_Symbols.size()-1,this->_Symbols[this->_Symbols.size()-1].symbol(this->_SymbolNames),str,type));
}

void SymbolTable::sort() {
    // printf("%s:%d:%s Sort the SymbolTable here\n", __FILE__, __LINE__, __FUNCTION__ );
  sort::quickSortMemory<SymbolEntry>(&this->_Symbols[0],0,this->_Symbols.size());
}


std::string backtrace_frame(size_t index, BacktraceEntry* frame)
{
  stringstream ss;
  ss << "Frame#" << index << " ";
  if (frame->_Stage!=undefined) {
    ss << frame->_SymbolName << " ";
    ss << "@" << (void*)frame->_FunctionStart << " ";
    ss << "end: " << (void*)(frame->_FunctionEnd);
  }
  return ss.str();
}


struct Header {
  uint8_t  version;
  uint8_t  reserved0;
  uint16_t reserved1;
};

struct StkSizeRecord {
  uint64_t  FunctionAddress;
  int64_t  StackSize;
  uint64_t  RecordCount;
};

struct Location{
  uint8_t  Type;
  uint8_t   Reserved0;
  uint16_t  LocationSize;
  uint16_t  DwarfRegNum;
  uint16_t  Reserved1;
  int32_t   OffsetOrSmallConstant;
};

struct LiveOut {
  uint16_t DwarfRegNum;
  uint8_t  Reserved;
  uint8_t SizeInBytes;
};

struct StkMapRecord {
  uint64_t PatchPointID;
  uint32_t InstructionOffset;
  uint16_t Reserved;
  std::vector<Location> Locations;
  std::vector<LiveOut> LiveOuts;
};


template <typename T>
T read_then_advance(uintptr_t& address) {
  uintptr_t original = address;
  address = address+sizeof(T);
  return *(T*)original;
}

// Return true if the header was read
bool parse_header(uintptr_t& address, uintptr_t end, Header& header, size_t& NumFunctions, size_t& NumConstants, size_t& NumRecords)
{
  uintptr_t headerAddress = address;
  header.version = read_then_advance<uint8_t>(address);
  header.reserved0 = read_then_advance<uint8_t>(address);
  header.reserved1 = read_then_advance<uint16_t>(address);
  if (address>=end) return false;
  NumFunctions = read_then_advance<uint32_t>(address);
  if (address>=end) return false;
  NumConstants = read_then_advance<uint32_t>(address);
  if (address>=end) return false;
  NumRecords = read_then_advance<uint32_t>(address);
  if (address>=end) return false;
  return true;
}

void parse_function(uintptr_t& address, StkSizeRecord& function) {
  uintptr_t functionAddress = address;
  function.FunctionAddress = read_then_advance<uint64_t>(address);
  function.StackSize = read_then_advance<uint64_t>(address);
  function.RecordCount = read_then_advance<uint64_t>(address);
  BT_LOG((buf,"Looking at function record %p function.FunctionAddress = %p\n", (void*)functionAddress, (void*)function.FunctionAddress));
}

void parse_constant(uintptr_t& address, uint64_t& constant) {
  constant = read_then_advance<uint64_t>(address);
}

void parse_record(std::vector<BacktraceEntry>& backtrace, uintptr_t& address, size_t functionIndex, const StkSizeRecord& function, StkMapRecord& record, bool library) {
  uintptr_t recordAddress = address;
  BT_LOG((buf,"Parse record at %p\n", (void*)address));
  uint64_t patchPointID = read_then_advance<uint64_t>(address);
  uint32_t instructionOffset = read_then_advance<uint32_t>(address);
  /* record.Reserved = */ read_then_advance<uint16_t>(address);
  size_t NumLocations = read_then_advance<uint16_t>(address);
  for ( size_t index=0; index<NumLocations; ++index ) {
    uintptr_t recordAddress = address;
    /* record.Locations[index].Type = */ read_then_advance<uint8_t>(address);
    uint8_t reserved0 = read_then_advance<uint8_t>(address);
    /* record.Locations[index].LocationSize = */ read_then_advance<uint16_t>(address);
    /* record.Locations[index].DwarfRegNum = */ read_then_advance<uint16_t>(address);
    uint16_t reserved1 = read_then_advance<uint16_t>(address);
    if (reserved0 !=0 || reserved1 !=0) {
      printf("%s:%d:%s stackmap record @%p is out of alignment\n", __FILE__, __LINE__, __FUNCTION__, (void*)recordAddress );
      abort();
    }
    int32_t offsetOrSmallConstant = read_then_advance<int32_t>(address);
    if (backtrace.size() == 0 ) {
      if (library) {
        WRITE_DEBUG_IO(BF("Stackmap-library function %p stack-size %ld patchPointId %u frame-offset %d\n") % (void*)function.FunctionAddress % function.StackSize % patchPointID % offsetOrSmallConstant );
      } else {
        WRITE_DEBUG_IO(BF("Stackmap-jit function %p stack-size %ld patchPointId %u frame-offset %d\n") % (void*)function.FunctionAddress % function.StackSize % patchPointID % offsetOrSmallConstant );
      }
    }
    if (patchPointID == 1234567 ) {
      BT_LOG((buf,"patchPointID matched at %p\n", (void*)recordAddress));
      for (size_t j=0; j<backtrace.size(); ++j ) {
        BT_LOG((buf,"comparing function#%lu @%p to %s\n", functionIndex, (void*)function.FunctionAddress, backtrace_frame(j,&backtrace[j]).c_str() ));
        if (function.FunctionAddress == backtrace[j]._FunctionStart) {
          backtrace[j]._Stage = lispFrame;  // anything with a stackmap is a lisp frame
          backtrace[j]._FrameSize = (int)function.StackSize; // Sometimes the StackSize is (int64_t)-1 why???????
          backtrace[j]._FrameOffset = offsetOrSmallConstant;
          BT_LOG((buf,"Identified lispFrame frameOffset = %d\n", offsetOrSmallConstant));
        }
      }
    }
  }
  BT_LOG((buf,"Done with records at %p\n", (void*)address));
  if (((uintptr_t)address)&0x7) {
    read_then_advance<uint32_t>(address);
    BT_LOG((buf,"advanced to alignment %p\n", (void*)address));
  }
  if (((uintptr_t)address)&0x7) {
    printf("%s:%d Address %lX is not word aligned - it must be!!!\n", __FILE__, __LINE__, address );
    abort();
  }
  /*Padding*/ read_then_advance<uint16_t>(address);
  size_t NumLiveOuts = read_then_advance<uint16_t>(address);
  for ( size_t index=0; index<NumLiveOuts; ++index ) {
    /* record.LiveOuts[index].DwarfRegNum = */ read_then_advance<uint16_t>(address);
    /* record.LiveOuts[index].Reserved = */ read_then_advance<uint8_t>(address);
    /* record.LiveOuts[index].SizeInBytes = */ read_then_advance<uint8_t>(address);
  }
  if (((uintptr_t)address)&0x7) {
    read_then_advance<uint32_t>(address);
    BT_LOG((buf,"advanced to alignment %p\n", (void*)address));
  }
  if (((uintptr_t)address)&0x7) {
    printf("%s:%d Address %lX is not word aligned - it must be!!!\n", __FILE__, __LINE__, address );
    abort();
  }
}  

/* ! Parse an llvm Stackmap
     The format is described here: https://llvm.org/docs/StackMaps.html#stack-map-format
*/

void walk_one_llvm_stackmap(std::vector<BacktraceEntry>&backtrace, uintptr_t& address, uintptr_t end, bool library) {
  uintptr_t stackMapAddress = address;
  Header header;
  size_t NumFunctions;
  size_t NumConstants;
  size_t NumRecords;
  bool read = parse_header(address,end,header,NumFunctions,NumConstants,NumRecords);
  if (!read) {
    printf("%s:%d:%s Walked past the end of stackmaps!!!! address = %p end = %p\n",
           __FILE__, __LINE__, __FUNCTION__, (void*)address, (void*)end);
    abort();
  }
  if (header.version!=3 || header.reserved0 !=0 || header.reserved1 !=0) {
    printf("%s:%d:%s stackmap header @%p is out of alignment header.version=%d header.reserved0=%d header.reserved1=%d\n", __FILE__, __LINE__, __FUNCTION__, (void*)stackMapAddress, (int)header.version, (int)header.reserved0, (int)header.reserved1 );
    abort();
  }
  uintptr_t functionAddress = address;
  BT_LOG((buf,"PASS1 Parse function block first pass %p\n", (void*)functionAddress ));
  for ( size_t index=0; index<NumFunctions; ++index ) {
    StkSizeRecord function;
    parse_function(address,function); // dummy - used to skip functions
    BT_LOG((buf,"PASS1 Found function #%lu at %p\n", index, (void*)function.FunctionAddress));
  }
  for ( size_t index=0; index<NumConstants; ++index ) {
    uint64_t constant;
    parse_constant(address,constant);
  }
  size_t functionIndex = 0;
  BT_LOG((buf,"Parse record block %p\n", (void*)address ));
  for ( size_t functionIndex = 0; functionIndex < NumFunctions; ++functionIndex ) {
    StkSizeRecord function;
    parse_function(functionAddress,function);
    BT_LOG((buf,"PASS2 Examining function #%lu at %p - %" PRu " records\n", functionIndex, (void*)function.FunctionAddress, function.RecordCount));
    for ( size_t index=0; index<function.RecordCount; index++) {
      StkMapRecord record;
      parse_record(backtrace,address,functionIndex,function,record,library);
    }
  }
}



/*! Register contiguous range of stackmaps.
There may be 0, 1 or any number of adjacent stackmaps.  
Pass (size_t)~0 if you don't know how many and want to rely on the memory range.
Stop parsing them when read numStackmaps or if curAddress >= endAddress */
void register_llvm_stackmaps(uintptr_t startAddress, uintptr_t endAddress, size_t numStackmaps ) {
//  printf("%s:%d register_llvm_stackmaps %p - %p num: %lu\n", __FILE__, __LINE__, (void*)startAddress, (void*)endAddress, numStackmaps);
  BT_LOG((buf,"register_llvm_stackmaps  startAddress: %p  endAddress: %p\n", (void*)startAddress, (void*)endAddress));
  WITH_READ_WRITE_LOCK(debugInfo()._StackMapsLock);
  StackMapRange range(startAddress,endAddress,numStackmaps);
  debugInfo()._StackMaps[startAddress] = range;
}


void search_jitted_stackmaps(std::vector<BacktraceEntry>& backtrace)
{
  BT_LOG((buf,"Starting search_jitted_stackmaps\n" ));
  size_t num = 0;
  WRITE_DEBUG_IO(BF("search_jitted_stackmaps\n"));
  WITH_READ_LOCK(debugInfo()._StackMapsLock);
  DebugInfo& di = debugInfo();
  for ( auto entry : di._StackMaps ) {
    uintptr_t address = entry.second._StartAddress;
    BT_LOG((buf," Stackmap start at %p up to %p\n", (void*)address, (void*)entry.second._EndAddress));
    for ( size_t num = 0; num<entry.second._Number; ++num ) {
      walk_one_llvm_stackmap(backtrace,address,entry.second._EndAddress,false);
      if (address>=entry.second._EndAddress) break;
    }
  }
  BT_LOG((buf,"Finished search_jitted_stackmaps searched %lu\n", num));
}


void register_jitted_object(const std::string& name, uintptr_t address, int size) {
  BT_LOG((buf,"Starting\n" ));
  LOG(BF("STACKMAP_LOG  %s name: %s %p %d\n") % __FUNCTION__ % name % (void*)address % size );
  WITH_READ_WRITE_LOCK(debugInfo()._JittedObjectsLock);
  debugInfo()._JittedObjects.emplace_back(JittedObject(name,address,size));
}

void search_jitted_objects(std::vector<BacktraceEntry>& backtrace, bool searchFunctionDescriptions)
{
  BT_LOG((buf,"Starting search_jitted_objects\n" ));
  WITH_READ_LOCK(debugInfo()._JittedObjectsLock);
  for ( auto entry : debugInfo()._JittedObjects ) {
    BT_LOG((buf,"Looking at jitted object name: %s @%p size: %d\n", entry._Name.c_str(), (void*)entry._ObjectPointer, entry._Size));
    if (backtrace.size()==0 && !searchFunctionDescriptions) {
      WRITE_DEBUG_IO(BF("Jitted-object object-start %p object-end %p name %s\n") % (void*)entry._ObjectPointer % (void*)(entry._ObjectPointer+entry._Size) % entry._Name);
    }
    for (size_t j=0; j<backtrace.size(); ++j ) {
      BT_LOG((buf, "Comparing to backtrace frame %lu  return address %p %s\n", j, (void*)backtrace[j]._ReturnAddress, backtrace_frame(j,&backtrace[j]).c_str()));
      if (!searchFunctionDescriptions) { // searching for functions
        if (entry._ObjectPointer<=backtrace[j]._ReturnAddress && backtrace[j]._ReturnAddress<(entry._ObjectPointer+entry._Size)) {
          backtrace[j]._Stage = lispFrame; // jitted functions are lisp functions
          backtrace[j]._FunctionStart = entry._ObjectPointer;
          backtrace[j]._FunctionEnd = entry._ObjectPointer+entry._Size;
          backtrace[j]._SymbolName = entry._Name;
          BT_LOG((buf,"MATCHED!!!\n"));
//          break;
        }
      }
      if (searchFunctionDescriptions) { // searching for function descriptions
        stringstream ss;
        ss << backtrace[j]._SymbolName;
        ss << "^DESC";
        if (ss.str() == entry._Name) {
          backtrace[j]._Stage = lispFrame; // Anything with a FunctionDescription is a lispFrame
          backtrace[j]._FunctionDescription = entry._ObjectPointer;
          BT_LOG((buf,"MATCHED!!!\n"));
//          break;
        }
      }
    }
  }
}


void add_dynamic_library_using_handle(const std::string& libraryName, void* handle) {
  add_dynamic_library_impl(false,libraryName, false, 0, handle);
}

void add_dynamic_library_using_origin(bool is_executable,const std::string& libraryName, uintptr_t origin) {
  add_dynamic_library_impl(is_executable,libraryName, true, origin, NULL);
}

bool if_dynamic_library_loaded_remove(const std::string& libraryName) {
#ifdef CLASP_THREADS
  WITH_READ_WRITE_LOCK(debugInfo()._OpenDynamicLibraryMutex);
#endif
  map<string,OpenDynamicLibraryInfo>::iterator fi = debugInfo()._OpenDynamicLibraryHandles.find(libraryName);
  bool exists = (fi!=debugInfo()._OpenDynamicLibraryHandles.end());
  if (exists) {
    if (fi->second._SymbolTable._SymbolNames) free((void*)(fi->second._SymbolTable._SymbolNames));
    BT_LOG((buf,"What about the stackmaps for this library - you need to remove them as well - I should probably NOT store stackmaps for libraries - but fetch them every time we need a backtrace!\n"));
    if (fi->second._Handle==0) {
      printf("%s:%d:%s You cannot remove the library %s\n", __FILE__, __LINE__, __FUNCTION__, libraryName.c_str());
    } else {
      dlclose(fi->second._Handle);
      debugInfo()._OpenDynamicLibraryHandles.erase(libraryName);
    }
  }
  return exists;
}


CL_DEFUN List_sp core__dynamic_library_handles() {
#ifdef CLASP_THREADS
  WITH_READ_LOCK(debugInfo()._OpenDynamicLibraryMutex);
#endif
  ql::list result;
  for ( auto entry : debugInfo()._OpenDynamicLibraryHandles ) {
    result << Cons_O::createList(SimpleBaseString_O::make(entry.second._Filename),
                                 Pointer_O::create(entry.second._Handle) );;
  }
  return result.cons();
}

bool lookup_address_main(uintptr_t address, const char*& symbol, uintptr_t& start, uintptr_t& end, char& type, bool& foundLibrary, std::string& libraryName, uintptr_t& libraryStart )
{
  foundLibrary = false;
#ifdef CLASP_THREADS
  WITH_READ_LOCK(debugInfo()._OpenDynamicLibraryMutex);
#endif
  size_t index;
  for ( auto entry : debugInfo()._OpenDynamicLibraryHandles ) {
    SymbolTable symtab = entry.second._SymbolTable;
    if (symtab.findSymbolForAddress(address,symbol,start,end,type,index)) {
      foundLibrary = true;
      libraryName = entry.second._Filename;
      libraryStart = entry.second._LibraryOrigin;
      return true;
    }
  }
  {
#ifdef CLASP_THREADS
    WITH_READ_LOCK(debugInfo()._JittedObjectsLock);
#endif
    for ( auto entry : debugInfo()._JittedObjects ) {
      BT_LOG((buf,"Looking at jitted object name: %s @%p size: %d\n", entry._Name.c_str(), (void*)entry._ObjectPointer, entry._Size));
      if (entry._ObjectPointer<=address && address<(entry._ObjectPointer+entry._Size)) {
        symbol = entry._Name.c_str();
        start = entry._ObjectPointer;
        end = entry._ObjectPointer+entry._Size;
        type = '?';
        return true;
      }
    }
  }
  return false;
}

bool lookup_address(uintptr_t address, const char*& symbol, uintptr_t& start, uintptr_t& end, char& type) {
  bool libraryFound;
  std::string libraryName;
  uintptr_t libraryStart;  
  return lookup_address_main(address,symbol,start,end,type,libraryFound,libraryName,libraryStart);
}

void search_symbol_table(std::vector<BacktraceEntry>& backtrace, const char* filename, size_t& symbol_table_size)
{
#ifdef CLASP_THREADS
  WITH_READ_LOCK(debugInfo()._OpenDynamicLibraryMutex);
#endif
  BT_LOG((buf,"search_symbol_table library: %s\n", filename));
  std::string fname(filename);
  map<std::string,OpenDynamicLibraryInfo>::iterator it = debugInfo()._OpenDynamicLibraryHandles.find(fname);
  if (it == debugInfo()._OpenDynamicLibraryHandles.end()) {
//    printf("%s:%d:%s Could not find handle for library %s\n", __FILE__, __LINE__, __FUNCTION__, fname.c_str());
  } else {
    SymbolTable& symbol_table = it->second._SymbolTable;
    symbol_table_size += sizeof(SymbolEntry)*symbol_table._Symbols.size()+symbol_table._Capacity;
    if (backtrace.size()==0) {
      WRITE_DEBUG_IO(BF("Library filename: %s\n") % filename);
      WRITE_DEBUG_IO(BF("Library symbol_table _SymbolNames %p _End %u  _Capacity %u  _StackmapStart %p    _StackmapEnd %p\n")
                     % (void*)symbol_table._SymbolNames % symbol_table._End % symbol_table._Capacity
                     % (void*)symbol_table._StackmapStart % (void*)symbol_table._StackmapEnd );
    }
    if (backtrace.size() == 0) {
      uintptr_t prev_address = 0;
      for (auto entry : symbol_table ) {
        WRITE_DEBUG_IO(BF("Symbol start %p type %c name %s\n") % (void*)entry._Address % entry._Type % entry.symbol(symbol_table._SymbolNames));
        if (prev_address>= entry._Address) {
          WRITE_DEBUG_IO(BF("Error: PREVIOUS-SYMBOL-OUT-OF-ORDER\n"));
        }
        prev_address = entry._Address;
      }
    } else {
      if (symbol_table._Symbols.size()>0) {
        for ( size_t j=0; j<backtrace.size(); ++j ) {
          size_t index;
          const char* symbolName;
          uintptr_t startAddress;
          uintptr_t endAddress;
          char type;
          BT_LOG((buf,"Looking for _ReturnAddress %p %s\n",(void*)backtrace[j]._ReturnAddress, backtrace[j]._SymbolName.c_str()));
          bool found = symbol_table.findSymbolForAddress(backtrace[j]._ReturnAddress,symbolName,startAddress,endAddress,type,index);
          if (found) {
            BT_LOG((buf, "Found symbol index %zu |%s| type: |%c| startAddress %p endAddress %p\n", index, symbolName, type, (void*)startAddress, (void*)endAddress));
            if (type == 't' || type == 'T') {
              BT_LOG((buf,"Found %s  start: %p  end: %p\n", symbolName, (void*)startAddress, (void*)endAddress ));
              backtrace[j]._Stage = symbolicated;
              backtrace[j]._FunctionStart = startAddress;
              backtrace[j]._FunctionEnd = endAddress;
              backtrace[j]._SymbolName = symbolName;
            }
          }
        }
  // Look for FunctionDescriptions
        for (auto entry : symbol_table ) {
          if (entry._Type == 'd' || entry._Type=='D' || entry._Type=='s' || entry._Type=='S') {
            std::string name(entry.symbol(symbol_table._SymbolNames));
            if (name.size()>5 && name.substr(name.size()-5,name.size()) == "^DESC") {
              std::string function_part = name.substr(0,name.size()-5);
//          printf("%s:%d:%s Found a possible FunctionDescription %s \n", __FILE__, __LINE__, __FUNCTION__, entry._Name.c_str());
              for ( size_t j=0; j<backtrace.size(); ++j ) {
                if (backtrace[j]._SymbolName == function_part) {
//              printf("%s:%d:%s Matched to backtrace frame %lu FunctionName %s \n", __FILE__, __LINE__, __FUNCTION__, j, backtrace[j]._SymbolName.c_str());
                  backtrace[j]._Stage = lispFrame; // anything with a FunctionDescription is a lisp frame
                  backtrace[j]._FunctionDescription = entry._Address;
                }
              }
            }
          }
        }
      }
    }
    uintptr_t address = symbol_table._StackmapStart;
    uintptr_t endAddress = symbol_table._StackmapEnd;
    if (address) {
      while (address<endAddress) {
        walk_one_llvm_stackmap(backtrace,address,endAddress,true);
      }
    }
  }
}



bool SymbolTable::is_sorted() const
{
  uintptr_t prev_address = 0;
  bool bad_order = false;
  for (auto entry : *this ) {
    if (prev_address> entry._Address) {
      return false;
    }
    prev_address = entry._Address;
  }
  return true;
}

void start_debugger() {
  LispDebugger dbg(_Nil<T_O>());
  dbg.invoke();
}

LispDebugger::LispDebugger(T_sp condition) : _CanContinue(false), _Condition(condition) {
  _lisp->incrementDebuggerLevel();
  af_gotoIhsTop();
}

LispDebugger::LispDebugger() : _CanContinue(true) {
  this->_Condition = _Nil<T_O>();
  _lisp->incrementDebuggerLevel();
  af_gotoIhsTop();
}

void LispDebugger::printExpression() {
  int index = core__ihs_current_frame();
  InvocationHistoryFrameIterator_sp frame = this->currentFrame();
  if (frame->isValid()) {
    stringstream ss;
    ss << frame->frame()->asString(index);
    write_bf_stream(BF("%s\n") % ss.str());
  } else {
    write_bf_stream(BF("No frame.\n"));
  }
}

InvocationHistoryFrameIterator_sp LispDebugger::currentFrame() const {
  int index = core__ihs_current_frame();
  InvocationHistoryFrameIterator_sp frame = core__get_invocation_history_frame(index);
  return frame;
}

size_t global_low_level_debugger_depth = 0;

T_sp LispDebugger::invoke() {
  if (!isatty(0)) {
    printf("The low-level debugger was entered but there is no terminal on fd0 - aborting\n");
    abort();
  }
  if ( cl::_sym_STARfeaturesSTAR
       && cl::_sym_STARfeaturesSTAR->symbolValue()
       && cl::_sym_STARfeaturesSTAR->symbolValue().consp()
       && !gctools::As<Cons_sp>(cl::_sym_STARfeaturesSTAR->symbolValue())->memberEq(kw::_sym_interactive)) {
    printf("This is not an interactive session and the low-level debugger was entered - aborting\n");
    abort();
  }
  ++global_low_level_debugger_depth;
  if ( global_low_level_debugger_depth > 10 ) {
    printf("This is not an interactive session and the low-level debugger was entered too many times - exiting\n");
    exit(1);
  }
  //	DebuggerIHF debuggerStack(my_thread->invocationHistoryStack(),_Nil<ActivationFrame_O>());
  if (this->_Condition.notnilp()) {
    write_bf_stream(BF("Debugger entered with condition: %s") % _rep_(this->_Condition));
  }
  this->printExpression();
  write_bf_stream(BF("The following restarts are available:\n"));
  write_bf_stream(BF("ABORT      a    Abort to REPL\n"));
  while (1) {
    string line;
    stringstream sprompt;
    sprompt << "Frame-" << this->currentFrame()->index() << "-";
    sprompt << "Dbg";
    if (core__ihs_env(core__ihs_current_frame()).notnilp()) {
      sprompt << "(+ENV)";
    }
    sprompt << "[" << _lisp->debuggerLevel() << "]>";
    bool end_of_transmission(false);
    line = myReadLine(sprompt.str(), end_of_transmission);
    if (end_of_transmission) {
      printf("%s:%d Exiting debugger\n", __FILE__, __LINE__ );
      throw core::ExitProgramException(0);
    }
    char cmd;
    if (line[0] == ':') {
      cmd = line[1];
    } else
      cmd = 'e';
    
    switch (cmd) {
    case '?':
    case 'h': {
      write_bf_stream(BF(":?      - help"));
      write_bf_stream(BF(":h      - help"));
      write_bf_stream(BF("sexp - evaluate sexp"));
      write_bf_stream(BF(":c sexp - continue - return values of evaluating sexp"));
      write_bf_stream(BF(":v      - list local environment"));
      write_bf_stream(BF(":x      - print current expression"));
      write_bf_stream(BF(":e      - evaluate an expression with interpreter"));
      write_bf_stream(BF(":b      - print backtrace"));
      write_bf_stream(BF(":p      - goto previous frame"));
      write_bf_stream(BF(":n      - goto next frame"));
      write_bf_stream(BF(":D      - dissasemble current function"));
      write_bf_stream(BF(":a      - abort and return to top repl"));
      write_bf_stream(BF(":l      - invoke debugger by calling core::dbg_hook (set break point in gdb"));
      write_bf_stream(BF(":g ##   - jump to frame ##"));
      break;
    }
    case 'l':
	dbg_hook("invoked from debugger");
	break;
    case 'g': {
      int is;
      for (is = 1; is < line.size(); is++) {
        if (line[is] >= '0' && line[is] <= '9')
          break;
      }
      if (is < line.size()) {
        string sexp = line.substr(is, 99999);
        int frameIdx = atoi(sexp.c_str());
        if (frameIdx < 0)
          frameIdx = 0;
        if (frameIdx > core__ihs_top()) {
          frameIdx = core__ihs_top();
        }
        write_bf_stream(BF("Switching to frame: %d") % frameIdx);
        core__set_ihs_current_frame(frameIdx);
        this->printExpression();
      } else {
        write_bf_stream(BF("You must provide a frame number\n"));
      }
      break;
    }
    case 'p':
	af_gotoIhsPrev();
	this->printExpression();
	break;
    case 'n':
	af_gotoIhsNext();
	this->printExpression();
	break;
    case 'D': {
      T_sp func = core__ihs_fun(core__ihs_current_frame());
      write_bf_stream(BF("Current function: %s\n") % _rep_(func));
      eval::funcall(cl::_sym_disassemble, func);
      break;
    }
    case 'b': {
      core__ihs_backtrace(_lisp->_true(), _Nil<T_O>());
      break;
    }
    case 'x': {
      this->printExpression();
      break;
    }
    case 'v': {
      this->printExpression();
      T_sp env = core__ihs_env(core__ihs_current_frame());
      write_bf_stream(BF("activationFrame->%p    .nilp()->%d  .nilp()->%d") % env.raw_() % env.nilp() % env.nilp());
      if (env.notnilp()) {
        write_bf_stream(BF("%s") % gc::As<Environment_sp>(env)->environmentStackAsString());
      } else {
        write_bf_stream(BF("-- Only global environment available --"));
      }
      break;
    }
    case 'a': {
      throw(DebuggerSaysAbortToRepl());
    }
    case 'c': {
      if (this->_CanContinue) {
        if (line.size() < 3) {
          return _Nil<T_O>();
        }
        string sexp = line.substr(3, 99999);
        T_mv result;
        T_sp env = core__ihs_env(core__ihs_current_frame());
        result = _lisp->readEvalPrintString(sexp, env, true);
        if (!result) {
          result = Values(_Nil<T_O>());
        }
        write_bf_stream(BF("Continuing with result: %s") % _rep_(result));
        return result;
	  //		    throw(DebuggerSaysContinue(result));
      }
      write_bf_stream(BF("You cannot resume after condition thrown"));
      break;
    };
    case 'e': {
      string sexp = line.substr(0, 99999);
      T_sp env = core__ihs_env(core__ihs_current_frame());
      try {
        _lisp->readEvalPrintString(sexp, env, true);
      } catch (DebuggerSaysAbortToRepl &err) {
	  // nothing
      }
      break;
    }
    case 'i': {
      string sexp = line.substr(2, 99999);
	//		ControlSingleStep singleStep(false);
      T_sp env = core__ihs_env(core__ihs_current_frame());
	//		DebuggerIHF dbgFrame(my_thread->invocationHistoryStack(),Environment_O::clasp_getActivationFrame(env));
      try {
        DynamicScopeManager scope(comp::_sym_STARimplicit_compile_hookSTAR, comp::_sym_implicit_compile_hook_default->symbolFunction());
        _lisp->readEvalPrintString(sexp, env, true);
      } catch (DebuggerSaysAbortToRepl &err) {
	  // nothing
      }
      break;
    }
    default: {
      write_bf_stream(BF("Unknown command[%c] - try '?'") % cmd);
    }
    }
  }
}

#if 0
CL_DEFUN void core__test_backtrace() {
  InvocationHistoryFrame *top = my_thread->_InvocationHistoryStack;
  if (top == NULL) {
    printf("Empty InvocationHistoryStack\n");
    return;
  }
  int index = 0;
  for (InvocationHistoryFrame *cur = top; cur != NULL; cur = cur->_Previous) {
    T_sp frame = cur->valist_sp();
    printf("Frame[%d] = %p\n", index, frame.raw_());
    ++index;
  }
  printf("----Done\n");
}
#endif

SYMBOL_EXPORT_SC_(CorePkg,make_shadow_backtrace_frame);

CL_DEFUN List_sp core__shadow_backtrace_as_list() {
  const InvocationHistoryFrame *top = my_thread->_InvocationHistoryStackTop;
  if (top == NULL) {
    return _Nil<T_O>();
  }
  ql::list result;
  int index = 0;
  for (const InvocationHistoryFrame *cur = top; cur != NULL; cur = cur->_Previous) {
    if (cur->_Previous) {
      T_sp frame = eval::funcall(_sym_make_shadow_backtrace_frame,
                                 INTERN_(kw,index), make_fixnum(index),
                                 INTERN_(kw,frame_address), Pointer_O::create((void*)cur),
                                 INTERN_(kw,function_name), gc::As<Closure_sp>(cur->function())->functionName(),
                                 INTERN_(kw,function), cur->function(),
                                 INTERN_(kw,arguments), cur->arguments(),
                                 INTERN_(kw,environment), cur->function());
      result << frame;
      ++index;
    }
  }
  return result.cons();
}


bool search_for_matching_close_bracket(const std::string& sin, size_t& pos, stringstream& sacc) {
  for ( size_t i=pos; i<sin.size(); ++i ) {
    if (sin[i] == '>') {
      pos = i+1;
      return true;
    }
    if (sin[i] == '<') {
      pos = i;
      return search_for_matching_close_bracket(sin,pos,sacc);
    }
    sacc << sin[i];
  }
  return false;
}


std::string global_smart_ptr_head = "gctools::smart_ptr<";

bool mangle_next_smartPtr(const std::string& str, size_t& pos, stringstream& sout) {
  size_t smartPtrStart = str.find(global_smart_ptr_head,pos);
  if (smartPtrStart != std::string::npos ) {
    sout << str.substr(pos,smartPtrStart-pos);
    size_t bracketStart = smartPtrStart+global_smart_ptr_head.size(); // length of "gctools::smartPtr<"
    stringstream sinner;
    search_for_matching_close_bracket(str,bracketStart,sinner);
    std::string innerType = sinner.str();
    if (innerType.size() > 2) {
      if (innerType.substr(innerType.size()-2,2) == "_O") {
        sout << innerType.substr(0,innerType.size()-2);
        sout << "_sp";
      } else if (innerType.substr(innerType.size()-2,2) == "_V") {
        sout << innerType.substr(0,innerType.size()-2);
        sout << "_sp";
      } else {
        sout << innerType << "_sp";
      }
    } else {
      sout << innerType;
    }
    pos = bracketStart;
    return true;
  }
  sout << str.substr(pos,str.size()-pos);
  return false;
}


CL_DEFUN SimpleBaseString_sp core__ever_so_slightly_mangle_cxx_names(const std::string& raw_name)
{
  stringstream sout;
  size_t pos = 0;
  while (mangle_next_smartPtr(raw_name,pos,sout));
  return SimpleBaseString_O::make(sout.str());
}
  
  

void low_level_backtrace(bool with_args) {
  const InvocationHistoryFrame *top = my_thread->_InvocationHistoryStackTop;
  if (top == NULL) {
    printf("Empty InvocationHistoryStack\n");
    return;
  }
  int index = 0;
  for (const InvocationHistoryFrame *cur = top; cur != NULL; cur = cur->_Previous) {
    string name = "-no-name-";
    T_sp tclosure = cur->function();
    if (!tclosure) {
      name = "-NO-CLOSURE-";
    } else if (tclosure.generalp()){
      General_sp closure = gc::As_unsafe<General_sp>(tclosure);
      if (closure.nilp()) {
        name = "NIL";
      } else if (gc::IsA<Function_sp>(closure)) {
        Function_sp func = gc::As_unsafe<Function_sp>(closure);
        if (func->functionName().notnilp()) {
          try {
            name = _rep_(func->functionName());
          } catch (...) {
            name = "-BAD-NAME-";
          }
        }
        /*Nilable?*/ T_sp sfi = core__file_scope(func->sourcePathname());
        string sourceName = "cannot-determine";
        if (sfi.notnilp()) {
          sourceName = gc::As<FileScope_sp>(sfi)->fileName();
        }
        printf("#%4d frame@%p closure@%p %s/%3d\n    %40s ", index, cur, closure.raw_(), sourceName.c_str(), func->lineNumber(), name.c_str() );
        if (with_args) {
          SimpleVector_sp args = cur->arguments();
          for ( size_t i(0), iEnd(args->length()); i<iEnd; ++i ) { printf( " %s@%p", _rep_((*args)[i]).c_str(), (*args)[i].raw_()); }
        }
        printf("\n");
        goto SKIP_PRINT;
      } else {
        name = _rep_(closure);
      }
    } else {
      name = "-BAD-CLOSURE-";
    }
    printf("_Index: %4d  Frame@%p(previous=%p)  closure@%p  closure->name[%40s]\n",
           index, cur, cur->_Previous, tclosure.raw_(), name.c_str() );
  SKIP_PRINT:
    ++index;
  }
  printf("----Done\n");
}

CL_LAMBDA(&optional with_args);
CL_DECLARE();
CL_DOCSTRING("lowLevelBacktrace");
CL_DEFUN void core__low_level_backtrace() {
  low_level_backtrace(false);
}


CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("lowLevelBacktrace");
CL_DEFUN void core__low_level_backtrace_with_args() {
  low_level_backtrace(true);
}


void operating_system_backtrace(std::vector<BacktraceEntry>& backtrace_)
{
#define START_BACKTRACE_SIZE 512
  void** return_buffer;
  size_t num = START_BACKTRACE_SIZE;
  for ( int try_=0; try_<100; ++try_ ) {
    // printf("%s:%s:%d malloc to %lu\n", __FILE__, __FUNCTION__, __LINE__, num);
    void** buffer = (void**)malloc(sizeof(void*)*num);
    size_t returned = backtrace(buffer,num);
    if (returned < num) {
      // printf("%s:%s:%d Resizing backtrace to %lu\n", __FILE__, __FUNCTION__, __LINE__, returned);
      backtrace_.resize(returned);
      // printf("%s:%s:%d Filling backtrace to %lu\n", __FILE__, __FUNCTION__, __LINE__, returned);
      char **strings = backtrace_symbols(buffer, returned);
      for ( size_t j=0; j<returned; ++j ) {
        // printf("%s:%s:%d Filling backtrace at %lu\n", __FILE__, __FUNCTION__, __LINE__, j);
        backtrace_[j]._ReturnAddress = (uintptr_t)buffer[j];
        backtrace_[j]._SymbolName = strings[j];
      }
      free(buffer);
      free(strings);
      return;
    }
    free(buffer);
    num = num*2;
  }
  printf("%s:%d Couldn't get backtrace\n", __FILE__, __LINE__ );
  abort();
}


CL_DEFUN T_sp core__maybe_demangle(core::String_sp s)
{
  char *funcname = (char *)malloc(1024);
  size_t funcnamesize = 1024;
  std::string fnName = s->get_std_string();
  int status;
  char *ret = abi::__cxa_demangle(fnName.c_str(), funcname, &funcnamesize, &status);
  if (status == 0) {
    std::string demangled(funcname);
    free(ret);
    return core__ever_so_slightly_mangle_cxx_names(demangled);
  } else {
    if (funcname) free(funcname);
    return _Nil<T_O>();
  }
}

bool check_for_frame(uintptr_t frame) {
  // We only actually do a check if we have libunwind capabilities.
#ifdef USE_LIBUNWIND
  unw_context_t context; unw_cursor_t cursor;
  unw_word_t sp;
  unw_word_t csp = (unw_word_t)frame;
  unw_getcontext(&context);
  unw_init_local(&cursor, &context);
  while (unw_step(&cursor) > 0) {
    unw_get_reg(&cursor, UNW_X86_64_RBP, &sp);
    if (sp == csp) return true;
  }
  return false;
#else
  return true;
#endif
}

void frame_check(uintptr_t frame) {
  if (!check_for_frame(frame))
    NO_INITIALIZERS_ERROR(core::_sym_outOfExtentUnwind);
}

CL_DEFUN T_sp core__libunwind_backtrace_as_list() {
#ifdef USE_LIBUNWIND
  unw_context_t context;
  unw_getcontext(&context);
  unw_cursor_t cursor;
  unw_init_local(&cursor,&context);
  char buffer[1024];
  unw_word_t offset;
  int step;
  do {
    int res = unw_get_proc_name(&cursor,buffer,1024,&offset);
    if ( res < 0 ) {
      printf("%s:%d unw_get_proc_name returned error %d\n", __FILE__, __LINE__, res);
    } else {
      printf("%s:%d  %s\n", __FILE__, __LINE__, buffer );
    }
    unw_proc_info_t proc_info;
    int pi_res = unw_get_proc_info(&cursor,&proc_info);
    if (pi_res<0) {
      printf("%s:%d unw_get_proc_info returned error %d\n", __FILE__, __LINE__, pi_res);
    } else {
      printf("          start: %p   end: %p\n", (void*)proc_info.start_ip, (void*)proc_info.end_ip);
    }
    step = unw_step(&cursor);
    if ( step < 0 ) {
      printf("%s:%d unw_step returned error %d\n", __FILE__, __LINE__, step);
    }
  } while (step>0);
  printf("%s:%d  End of backtrace\n", __FILE__, __LINE__);
#endif
  return _Nil<T_O>();
}


void fill_in_interpreted_frames(std::vector<BacktraceEntry>& backtrace) {
  const InvocationHistoryFrame* frame = my_thread->_InvocationHistoryStackTop;
  while (frame) {
    uintptr_t reg = (uintptr_t)frame->register_save_area();
//    printf("%s:%d:%s reg: %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)reg);
    for ( size_t idx = 1; idx<backtrace.size()-1; ++idx ) {
      uintptr_t bp = backtrace[idx-1]._BasePointer;
      uintptr_t bpNext = backtrace[idx]._BasePointer;
//      printf("%s:%d:%s frame: %lu %s  bp: %p bpNext: %p reg: %p\n", __FILE__, __LINE__, __FUNCTION__, idx, backtrace[idx]._SymbolName.c_str(), (void*)bp, (void*)bpNext, (void*)reg);
      if (bp<=reg && reg <bpNext) {
        backtrace[idx]._Stage = lispFrame; // Interpreted frames are lisp frames
        backtrace[idx]._SymbolName = gc::As<Symbol_sp>(gc::As<Closure_sp>(frame->function())->functionName())->formattedName(true);
        backtrace[idx]._InvocationHistoryFrameAddress = (uintptr_t)frame;
        //printf("%s:%d:%s %s %s  MATCH!!!!\n", __FILE__, __LINE__, __FUNCTION__, backtrace[idx]._SymbolName.c_str(), _rep_(backtrace[idx]._Arguments).c_str());
      }
    }
    frame = frame->_Previous;
  }
}



/*! If you pass a backtrace then it will be filled with symbol information.
    If you pass an empty backtrace then symbol information will be printed.
    eg: std::vector<BacktraceEntry> emptyBacktrace;  fill_backtrace_or_dump_info(emptyBacktrace) 
*/
void fill_backtrace_or_dump_info(std::vector<BacktraceEntry>& backtrace) {
//  printf("walk symbol tables and stackmaps for DARWIN\n");
  size_t symbol_table_memory = 0;
  walk_loaded_objects(backtrace,symbol_table_memory);
    // Now search the jitted objects
//  printf("%s:%d:%s search_jitted_objects false\n", __FILE__, __LINE__, __FUNCTION__);
  search_jitted_objects(backtrace,false);
//  printf("%s:%d:%s search_jitted_objects true\n", __FILE__, __LINE__, __FUNCTION__);
  search_jitted_objects(backtrace,true); // Search them twice to find all FunctionDescription objects
//  printf("%s:%d:%s search_jitted_stackmaps\n", __FILE__, __LINE__, __FUNCTION__);
  search_jitted_stackmaps(backtrace);
  if (backtrace.size()==0) {
    WRITE_DEBUG_IO(BF("symbol-table-memory %lu\n") % symbol_table_memory);
  }
}


SYMBOL_EXPORT_SC_(CorePkg,make_backtrace_frame);
SYMBOL_EXPORT_SC_(KeywordPkg,function_name);
SYMBOL_EXPORT_SC_(KeywordPkg,arguments);
SYMBOL_EXPORT_SC_(KeywordPkg,closure);


/*! Get the raw argument from the stack, 
argIndex==0 is closure
argIndex==1 is the number of arguments
argIndex==2... are the passed arguments
*/

uintptr_t get_raw_argument_from_stack(uintptr_t functionAddress, uintptr_t basePointer, int frameOffset, size_t argIndex, uintptr_t invocationHistoryFrameAddress)
{
  T_O** register_save_area;
  if (invocationHistoryFrameAddress) {
    // this is an interpreted function - use the invocationHistoryFrameAddress to get the register save area
    // printf("%s:%d Using the invocationHistoryFrameAddress@%p to get the register save area of interpreted function\n", __FILE__, __LINE__, (void*)invocationHistoryFrameAddress);
    InvocationHistoryFrame* ihf = (InvocationHistoryFrame*)invocationHistoryFrameAddress;
    register_save_area = (core::T_O**)ihf->_args->reg_save_area;
  } else {
    // This is a compiled function - use the frameOffset to get the register-save-area
  // printf("%s:%s:%d start basePointer: %lu frameOffset: %d\n", __FILE__, __FUNCTION__, __LINE__, basePointer, frameOffset );
    register_save_area = (T_O**)(basePointer+frameOffset);
  }
//  printf("%s:%d:%s basePointer@%p frameOffset %d reg_save_area %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)basePointer, frameOffset, register_save_area);
  // printf("%s:%s:%d get nargs basePointer: %lu frameOffset: %d\n", __FILE__, __FUNCTION__, __LINE__, basePointer, frameOffset );
  size_t nargs = (uintptr_t)register_save_area[1];
  // printf("%s:%s:%d nargs: %lu\n", __FILE__, __FUNCTION__, __LINE__, nargs );
  if (nargs>256) {
    printf("%s:%d:%s There are too many arguments %lu for function at %p in frame at %p offset: %d\n", __FILE__, __LINE__, __FUNCTION__, nargs, (void*)functionAddress, (void*)basePointer, frameOffset);
    return 0;
  }
  // printf("%s:%s:%d argIndex: %lu\n", __FILE__, __FUNCTION__, __LINE__, argIndex );
  if (argIndex < (LCC_ARGS_IN_REGISTERS+2)) {
    // printf("%s:%s:%d register_save_area: %lu\n", __FILE__, __FUNCTION__, __LINE__, argIndex );
    return (uintptr_t)register_save_area[argIndex];
  } else {
    // printf("%s:%s:%d register_save_area: %lu\n", __FILE__, __FUNCTION__, __LINE__, argIndex );
    return (uintptr_t)((T_O**)basePointer)[2+argIndex-(LCC_ARGS_IN_REGISTERS+2)];
  }
}



core::T_mv capture_arguments(uintptr_t functionAddress, uintptr_t basePointer, int frameOffset, bool asPointers, uintptr_t invocationHistoryFrameAddress)
{
  T_sp closure((gctools::Tagged)get_raw_argument_from_stack(functionAddress,basePointer,frameOffset,0,invocationHistoryFrameAddress));
  size_t nargs = core::get_raw_argument_from_stack(functionAddress,basePointer,frameOffset,1,invocationHistoryFrameAddress);
  SimpleVector_sp args = SimpleVector_O::make(nargs);
  for ( size_t i=0; i<nargs; ++i ) {
    T_sp tobj;
    if (asPointers) {
      tobj = Pointer_O::create((void*)get_raw_argument_from_stack(functionAddress,basePointer,frameOffset,2+i,invocationHistoryFrameAddress));
    } else {
      T_sp temp((gctools::Tagged)get_raw_argument_from_stack(functionAddress,basePointer,frameOffset,2+i,invocationHistoryFrameAddress));
      tobj = temp;
    }
    (*args)[i] = tobj;
  }
  if (asPointers) {
    return Values(args,Pointer_O::create((void*)closure.raw_()));
  }
  return Values(args,closure);
}


void fill_backtrace(std::vector<BacktraceEntry>& backtrace) {
  char *funcname = (char *)malloc(1024);
  size_t funcnamesize = 1024;
  uintptr_t stackTop = (uintptr_t)my_thread_low_level->_StackTop;
  BT_LOG((buf,"About to operatin_system_backtrace\n" ));
  //printf("%s:%s:%d\n", __FILE__, __FUNCTION__, __LINE__);
  operating_system_backtrace(backtrace);
  size_t nptrs = backtrace.size()-2;
  nptrs -= 2; // drop the last two frames
    // Fill in the base pointers
  BT_LOG((buf,"About to get bp's\n" ));
  //printf("%s:%s:%d\n", __FILE__, __FUNCTION__, __LINE__);
  void* bp = __builtin_frame_address(0);
  for (size_t i = 1; i < nptrs; ++i) {
    if (bp) {
      backtrace[i]._BasePointer = (uintptr_t)bp;
      bp = *(void**)bp;
    }
  }
  BT_LOG((buf,"About to walk library info\n" ));
  //
  // Walk libraries and jitted objects to fill backtrace.
  //
  // printf("%s:%s:%d\n", __FILE__, __FUNCTION__, __LINE__);
  fill_backtrace_or_dump_info(backtrace);
    // Now get the arguments
  BT_LOG((buf,"Getting arguments\n"));
  // printf("%s:%s:%d\n", __FILE__, __FUNCTION__, __LINE__);
    // fill in the interpreted frames here
  BT_LOG((buf,"fill in interpreted frames here\n"));;
  fill_in_interpreted_frames(backtrace);
};

CL_DOCSTRING("Call the thunk after setting core:*stack-top-hint* to somewhere in the current stack frame.");
CL_DEFUN T_mv core__call_with_stack_top_hint(Function_sp thunk)
{
  size_t temporary_stack_top;
  DynamicScopeManager scope(_sym_STARstack_top_hintSTAR, Pointer_O::create((void*)&temporary_stack_top));
  return eval::funcall(thunk);
}

CL_LAMBDA(closure &optional args-as-pointers);
CL_DECLARE();
CL_DOCSTRING(R"doc(Generate a backtrace and pass it to the closure for printing or debugging.
 If args-as-pointers is T then arguments and the closure are wrapped in Pointer_O objects.)doc");
CL_DEFUN T_mv core__call_with_backtrace(Function_sp closure, bool args_as_pointers) {
  std::vector<BacktraceEntry> backtrace;
  fill_backtrace(backtrace);
  uintptr_t stack_top_hint = ~0;
  if (_sym_STARstack_top_hintSTAR->symbolValue().notnilp()) {
    if (gc::IsA<Pointer_sp>(_sym_STARstack_top_hintSTAR->symbolValue())) {
      Pointer_sp stack_top_hint_ptr = gc::As_unsafe<Pointer_sp>(_sym_STARstack_top_hintSTAR->symbolValue());
      stack_top_hint = (uintptr_t)stack_top_hint_ptr->ptr();
    }
  }
  BT_LOG((buf," building backtrace as list\n" ));
    // Move the frames into Common Lisp
  uintptr_t bp = (uintptr_t)__builtin_frame_address(0);
  ql::list result;
  for ( size_t i=1; i<backtrace.size(); ++i ) {
    if (bp<backtrace[i]._BasePointer && backtrace[i]._BasePointer!=0 && backtrace[i]._BasePointer<stack_top_hint) {
      T_sp entry;
      Symbol_sp stype;
      if (backtrace[i]._Stage == lispFrame) {
        if (backtrace[i]._SymbolName.find("___LAMBDA___")!=string::npos) {
          stype = cl::_sym_lambda;
        } else {
          stype = INTERN_(kw,lisp);
        }
      } else {
        stype = INTERN_(kw,c_PLUS__PLUS_);
      }
      T_sp funcDesc = _Nil<T_O>();
      if (backtrace[i]._FunctionDescription!=0) {
        funcDesc = Pointer_O::create((void*)backtrace[i]._FunctionDescription);
      }
      ql::list args;
      core::T_sp arguments = _Nil<core::T_O>();
      core::T_sp closure = _Nil<core::T_O>();
      if (backtrace[i]._FrameOffset!=0 || backtrace[i]._InvocationHistoryFrameAddress!=0) {
        core::T_mv args_closure = capture_arguments(backtrace[i]._FunctionStart,backtrace[i]._BasePointer,backtrace[i]._FrameOffset,args_as_pointers,backtrace[i]._InvocationHistoryFrameAddress);
        arguments = args_closure;
        closure = args_closure.second();
      }
      args << INTERN_(kw,type) << stype
           << INTERN_(kw,return_address) << Pointer_O::create((void*)backtrace[i]._ReturnAddress)
           << INTERN_(kw,raw_name) <<  SimpleBaseString_O::make(backtrace[i]._SymbolName)
           << INTERN_(kw,base_pointer) << Pointer_O::create((void*)backtrace[i]._BasePointer)
           << INTERN_(kw,frame_offset) << core::make_fixnum(backtrace[i]._FrameOffset)
           << INTERN_(kw,frame_size) << core::make_fixnum(backtrace[i]._FrameSize)
           << INTERN_(kw,function_start_address) << Pointer_O::create((void*)backtrace[i]._FunctionStart)
           << INTERN_(kw,function_end_address) << Pointer_O::create((void*)backtrace[i]._FunctionEnd)
           << INTERN_(kw,function_description) << funcDesc
           << INTERN_(kw,arguments) << arguments
           << INTERN_(kw,closure) << closure;
      if (_sym_make_backtrace_frame->fboundp()) {
        entry = core__apply0(_sym_make_backtrace_frame->symbolFunction(),args.cons());
      } else {
        entry = core::Cons_O::create(_sym_make_backtrace_frame,args.cons());
      }
      result << entry;
    } else {
#if 0
      printf("%s:%d Skipping frame %lu name %s\n", __FILE__, __LINE__, i, backtrace[i]._SymbolName.c_str());
      printf("%s:%d bp<backtrace[i]._BasePointer -> %d\n", __FILE__, __LINE__, bp<backtrace[i]._BasePointer);
      printf("%s:%d backtrace[i]._BasePointer!=0 -> %d\n", __FILE__, __LINE__, backtrace[i]._BasePointer!=0);
      printf("%s:%d backtrace[i]._BasePointer<stack_top_hint -> %d\n", __FILE__, __LINE__, backtrace[i]._BasePointer<stack_top_hint);
#endif
    }
  }
  return eval::funcall(closure,result.cons());
}


CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("framePointer");
CL_DEFUN Fixnum_sp core__frame_pointer() {
  void *fp = __builtin_frame_address(0); // Constant integer only
  return make_fixnum((uintptr_t)fp);
};


CL_DOCSTRING(R"doc(Dump all symbols and stackmaps to *debug-io*.)doc");
CL_DEFUN void core__dump_symbol_and_stackmap_info() {
  std::vector<BacktraceEntry> emptyBacktrace;
  fill_backtrace_or_dump_info(emptyBacktrace);
}
};

namespace core {

#define ARGS_af_gotoIhsTop "()"
#define DECL_af_gotoIhsTop ""
#define DOCS_af_gotoIhsTop "gotoIhsTop"
void af_gotoIhsTop() {
  _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(core__ihs_top()));
};

#define ARGS_af_gotoIhsPrev "()"
#define DECL_af_gotoIhsPrev ""
#define DOCS_af_gotoIhsPrev "gotoIhsPrev"
void af_gotoIhsPrev() {
  int ihsCur = core__ihs_current_frame();
  _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(core__ihs_prev(ihsCur)));
};

#define ARGS_af_gotoIhsNext "()"
#define DECL_af_gotoIhsNext ""
#define DOCS_af_gotoIhsNext "gotoIhsNext"
void af_gotoIhsNext() {
  int ihsCur = core__ihs_current_frame();
  _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(core__ihs_next(ihsCur)));
};

#define ARGS_af_gotoIhsFrame "(frame-index)"
#define DECL_af_gotoIhsFrame ""
#define DOCS_af_gotoIhsFrame "gotoIhsFrame"
void af_gotoIhsFrame(int frame_index) {
  if (frame_index < 0)
    frame_index = 0;
  if (frame_index >= core__ihs_top())
    frame_index = core__ihs_top() - 1;
  int ihsCur = frame_index;
  _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(ihsCur));
};

#define ARGS_af_printCurrentIhsFrame "()"
#define DECL_af_printCurrentIhsFrame ""
#define DOCS_af_printCurrentIhsFrame "printCurrentIhsFrame"
void af_printCurrentIhsFrame() {
  int ihsCur = core__ihs_current_frame();
  T_sp fun = core__ihs_fun(ihsCur);
  printf("Frame[%d] %s\n", ihsCur, _rep_(fun).c_str());
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("printCurrentIhsFrameEnvironment");
CL_DEFUN void core__print_current_ihs_frame_environment() {
  T_sp args = core__ihs_arguments(core__ihs_current_frame());
  if (args.notnilp()) {
    ComplexVector_T_sp vargs = gc::As<ComplexVector_T_sp>(args);
    for (int i = 0; i < cl__length(vargs); ++i) {
      write_bf_stream(BF("arg%s --> %s") % i % _rep_(vargs->rowMajorAref(i)));
    }
  } else {
    write_bf_stream(BF("Args not available"));
  }
  T_sp env = core__ihs_env(core__ihs_current_frame());
  if (env.notnilp()) {
    printf("%s\n", gc::As<Environment_sp>(env)->environmentStackAsString().c_str());
  } else {
    printf("-- Only global environment available --\n");
  }
}

#define ARGS_af_evalPrint "(arg)"
#define DECL_af_evalPrint ""
#define DOCS_af_evalPrint "evalPrint"
void af_evalPrint(const string &expr) {
  printf("If this locks up then there was an error in the evaluation\n");
  printf("Figure out how to make debugger.cc>>af_evalPrint always return\n");
  int ihsCur = core__ihs_current_frame();
  T_sp env = core__ihs_env(ihsCur);
  _lisp->readEvalPrintString(expr, env, true);
};


CL_DEFUN void core__lowLevelDescribe(T_sp obj) {
  dbg_lowLevelDescribe(obj);
}

CL_DEFUN core::T_mv core__lookup_address(core::Pointer_sp address) {
  const char* symbol;
  uintptr_t start, end;
  char type;
  bool libraryFound;
  std::string sLibraryName;
  uintptr_t libraryStart;  
  bool foundSymbol = lookup_address_main((uintptr_t)address->ptr(),symbol,start,end,type,libraryFound,sLibraryName,libraryStart);
  if (foundSymbol) {
    core::T_sp libraryName = _Nil<T_O>();
    core::T_sp offsetFromStartOfLibraryAddress = _Nil<T_O>();
    core::T_sp library_origin = _Nil<T_O>();
    if (libraryFound) {
      libraryName = SimpleBaseString_O::make(sLibraryName);
      library_origin = Integer_O::create((uintptr_t)libraryStart);
      offsetFromStartOfLibraryAddress = Integer_O::create((uintptr_t)address->ptr()-libraryStart);
    }
    return Values(core::SimpleBaseString_O::make(symbol),
                  core::Pointer_O::create((void*)start),
                  core::Pointer_O::create((void*)end),
                  core::clasp_make_character(type),
                  libraryName,
                  library_origin,
                  offsetFromStartOfLibraryAddress);
  }
  return Values(_Nil<core::T_O>());
}

void dbg_VaList_sp_describe(T_sp obj) {
    // Convert the T_sp object into a VaList_sp object
  VaList_sp vl = VaList_sp((gc::Tagged)obj.raw_());
  printf("Original va_list at: %p\n", &((Vaslist *)gc::untag_vaslist(reinterpret_cast<Vaslist *>(obj.raw_())))->_Args);
    // Create a copy of the Vaslist with a va_copy of the va_list
  Vaslist vlcopy_s(*vl);
  VaList_sp vlcopy(&vlcopy_s);
  printf("Calling dump_Vaslist_ptr\n");
  bool atHead = dump_Vaslist_ptr(stdout,&vlcopy_s);
  if (atHead) {
    for (size_t i(0), iEnd(vlcopy->remaining_nargs()); i < iEnd; ++i) {
      T_sp v = vlcopy->next_arg();
      printf("entry@%p %3zu --> %s\n", v.raw_(), i, _rep_(v).c_str());
    }
  }
}

void dbg_lowLevelDescribe(T_sp obj) {
  if (obj.valistp()) {
    dbg_VaList_sp_describe(obj);
  } else if (obj.fixnump()) {
    printf("fixnum_tag: %" PFixnum "\n", obj.unsafe_fixnum());
  } else if (obj.single_floatp()) {
    printf("single-float: %f\n", obj.unsafe_single_float());
  } else if (obj.characterp()) {
    printf("character: %d #\\%c\n", obj.unsafe_character(), obj.unsafe_character());
  } else if (obj.generalp()) {
    printf("vtable-ptr: %p  typeid: %s\n", &*obj, typeid(obj.unsafe_general()).name());
    printf("className-> %s\n", obj.unsafe_general()->className().c_str());
    printf("contents-> [%s]\n", _rep_(obj).c_str());
    if ( Closure_sp closure = obj.asOrNull<Closure_O>() ) {
      core__closure_slots_dump(closure);
    }
  } else if (obj.consp()) {
    printf("cons_tag: %p  typeid: %s\n", &*obj, typeid(obj).name());
    printf("List:  \n");
    for (auto c : coerce_to_list(obj)) {
      printf("@%p > car@%p  cdr@%p : %s\n", c.raw_(), oCar(c).raw_(), oCdr(c).raw_(), _rep_(oCar(c)).c_str());
    }
    return;
  } else {
    printf("lowLevelDescribe handle: %p\n", obj.raw_());
  }
  fflush(stdout);
}

void dbg_describe_tagged_T_Optr(T_O *p) {
  client_describe(p);
  T_sp obj((gctools::Tagged) reinterpret_cast<T_O *>(p));
  dbg_lowLevelDescribe(obj);
}

void dbg_describe_tagged_T_Optr_header(T_O *p) {
  client_describe(p);
}

extern void dbg_describe(T_sp obj);
void dbg_describe(T_sp obj) {
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  stringstream ss;
  printf("dbg_describe object class--> %s\n", _rep_(cl__class_of(obj)->_className()).c_str());
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
  fflush(stdout);
}

void dbg_describe_cons(Cons_sp obj) {
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  stringstream ss;
  printf("dbg_describe object class--> CONS\n");
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
}

void dbg_describe_symbol(Symbol_sp obj) {
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  stringstream ss;
  printf("dbg_describe object class--> %s\n", _rep_(obj->__class()->_className()).c_str());
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
}

void dbg_describeActivationFrame(ActivationFrame_sp obj) {
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  stringstream ss;
  printf("dbg_describe ActivationFrame class--> %s\n", _rep_(obj->__class()->_className()).c_str());
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
}

void dbg_describeTPtr(uintptr_t raw) {
  if (raw == 0) {
    printf("dbg_describe: NULL\n");
    return;
  }
  T_sp obj = gctools::smart_ptr<T_O>(raw);
  printf("dbg_describeTPtr Raw pointer value: %p\n", obj.raw_());
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  stringstream ss;
  printf("dbg_describe object class--> %s\n", _rep_(lisp_instance_class(obj)->_className()).c_str());
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
  fflush(stdout);
}

void dbg_printTPtr(uintptr_t raw, bool print_pretty) {
  core::T_sp sout = cl::_sym_STARstandard_outputSTAR->symbolValue();
  T_sp obj = gctools::smart_ptr<T_O>((gc::Tagged)raw);
  clasp_write_string((BF("dbg_printTPtr Raw pointer value: %p\n") % (void *)obj.raw_()).str(), sout);
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  DynamicScopeManager scope2(cl::_sym_STARprint_readablySTAR, _lisp->_boolean(print_pretty));
  clasp_write_string((BF("dbg_printTPtr object class --> %s\n") % _rep_(lisp_instance_class(obj)->_className())).str(), sout);
  fflush(stdout);
  write_ugly_object(obj, sout);
  clasp_force_output(sout);
}


}

extern "C" {

/*! Generate text representation of a objects without using the lisp printer!
This code MUST be bulletproof!  It must work under the most memory corrupted conditions */
__attribute__((optnone)) std::string dbg_safe_repr(uintptr_t raw) {
  stringstream ss;
  core::T_sp obj((gc::Tagged)raw);
  if (gc::tagged_generalp((gc::Tagged)raw) ||gc::tagged_consp((gc::Tagged)raw)) {
    // protect us from bad pointers
    if ( raw < 0x1000) {
      ss << "BAD-TAGGED-POINTER(" << (void*)raw << ")";
      return ss.str();
    }
  }
  if (obj.generalp() ) {
    if (gc::IsA<core::Symbol_sp>(obj)) {
      core::Symbol_sp sym = gc::As_unsafe<core::Symbol_sp>(obj);
      ss << sym->formattedName(true);
    } else if (gc::IsA<core::SimpleBaseString_sp>(obj)) {
      core::SimpleBaseString_sp sobj = gc::As_unsafe<core::SimpleBaseString_sp>(obj);
      ss << "\"" << sobj->get_std_string() << "\"";
    } else if (gc::IsA<core::SimpleVector_sp>(obj)) {
      core::SimpleVector_sp svobj = gc::As_unsafe<core::SimpleVector_sp>(obj);
      ss << "#(";
      for ( size_t i=0, iEnd(svobj->length()); i<iEnd; ++i ) {
        ss << dbg_safe_repr((uintptr_t) ((*svobj)[i]).raw_()) << " ";
      }
      ss << ")@" << (void*)raw;
    } else if (gc::IsA<core::FuncallableInstance_sp>(obj)) {
      core::FuncallableInstance_sp fi = gc::As_unsafe<core::FuncallableInstance_sp>(obj);
      ss << "#<FUNCALLABLE-INSTANCE ";
      ss << _safe_rep_(fi->GFUN_NAME());
      ss << ">@" << (void*)raw;;
    } else {
      core::General_sp gen = gc::As_unsafe<core::General_sp>(obj);
      ss << "#<" << gen->className() << " " << (void*)gen.raw_() << ">";
    }
  } else if (obj.consp()) {
    ss << "(";
    while (obj.consp()) {
      ss << dbg_safe_repr((uintptr_t)CONS_CAR(obj).raw_()) << " ";
      obj = CONS_CDR(obj);
    }
    if (obj.notnilp()) {
      ss << ". ";
      ss << dbg_safe_repr((uintptr_t)obj.raw_());
    }
    ss << ")";
  } else if (obj.fixnump()) {
    ss << (gc::Fixnum)obj.unsafe_fixnum();
  } else if (obj.nilp()) {
    ss << "NIL";
  } else if (obj.valistp()) {
    ss << "#<VASLIST " << (void*)obj.raw_() << ">";
  } else if (obj.unboundp()) {
    ss << "#:UNBOUND";
  } else if (obj.characterp()) {
    ss << "#\\" << (char)obj.unsafe_character() << "[" << (int)obj.unsafe_character() << "]";
  } else if (obj.single_floatp()) {
    ss << (float)obj.unsafe_single_float();
  } else {
    ss << " #<RAW@" << (void*)obj.raw_() << ">";
  }
  if (ss.str().size() > 2048) {
    return ss.str().substr(0,2048);
  }
  return ss.str();
}

string _safe_rep_(core::T_sp obj) {
  return dbg_safe_repr((uintptr_t)obj.raw_());
}


void dbg_safe_print(uintptr_t raw) {
  printf(" %s", dbg_safe_repr(raw).c_str());
}

void dbg_safe_println(uintptr_t raw) {
  printf(" %s\n", dbg_safe_repr(raw).c_str());
}

void dbg_print_frame(FILE* fout, const std::vector<core::BacktraceEntry>& backtrace, size_t idx, bool printArgs)
{
  stringstream ss;
  fprintf(fout,"%lu: ", idx);
  fflush(fout);
  fprintf(fout,"(%s ", backtrace[idx]._SymbolName.c_str());
  fflush(fout);
  if (printArgs&&((backtrace[idx]._BasePointer!=0&&backtrace[idx]._FrameOffset!=0)||backtrace[idx]._InvocationHistoryFrameAddress!=0)) {
    uintptr_t invocationHistoryFrameAddress = backtrace[idx]._InvocationHistoryFrameAddress;
    // printf("%s:%s:%d Printing \n", __FILE__, __FUNCTION__, __LINE__);
    core::T_sp closure((gctools::Tagged)core::get_raw_argument_from_stack(backtrace[idx]._FunctionStart,backtrace[idx]._BasePointer,backtrace[idx]._FrameOffset,0,invocationHistoryFrameAddress));
    // printf("%s:%s:%d Printing \n", __FILE__, __FUNCTION__, __LINE__);
    size_t nargs = core::get_raw_argument_from_stack(backtrace[idx]._FunctionStart,backtrace[idx]._BasePointer,backtrace[idx]._FrameOffset,1,invocationHistoryFrameAddress);
    for ( size_t i=0; i<nargs; ++i ) {
      // printf("%s:%s:%d Printing arg %lu of %lu\n", __FILE__, __FUNCTION__, __LINE__, i, nargs);
      uintptr_t raw_arg = core::get_raw_argument_from_stack(backtrace[idx]._FunctionStart,backtrace[idx]._BasePointer,backtrace[idx]._FrameOffset,2+i,invocationHistoryFrameAddress);
      fprintf(fout,"%s ", dbg_safe_repr(raw_arg).c_str());
    }
  } else {
    fprintf(fout,"ARGS-NOT-SHOWN");
  }
  // printf("%s:%s:%d Printing\n", __FILE__, __FUNCTION__, __LINE__);
  fprintf(fout,")\n");
}

void dbg_safe_backtrace() {
//  gc::SafeGCPark park;
  std::vector<core::BacktraceEntry> backtrace;
  printf("Safe-backtrace\n");
  core::fill_backtrace(backtrace);
  printf("Got %lu backtrace frames - dumping\n", backtrace.size());
  for ( size_t idx=2; idx<backtrace.size()-2; ++idx ) {
    dbg_print_frame(stdout,backtrace,idx,true);
  }
}

void dbg_safe_backtrace_stderr() {
//  gc::SafeGCPark park;
  std::vector<core::BacktraceEntry> backtrace;
  fprintf(stderr,"Safe-backtrace\n");
  core::fill_backtrace(backtrace);
  fprintf(stderr,"Got %lu backtrace frames - dumping\n", backtrace.size());
  for ( size_t idx=2; idx<backtrace.size()-2; ++idx ) {
    dbg_print_frame(stderr,backtrace,idx,true);
  }
}

void dbg_safe_backtrace_no_args() {
//  gc::SafeGCPark park;
  std::vector<core::BacktraceEntry> backtrace;
  printf("Safe-backtrace\n");
  core::fill_backtrace(backtrace);
  printf("Got backtrace frames - dumping\n");
  for ( size_t idx=0; idx<backtrace.size()-2; ++idx ) {
    dbg_print_frame(stdout,backtrace,idx,false);
  }
}
void dbg_safe_lisp_backtrace() {
//  gc::SafeGCPark park;
  std::vector<core::BacktraceEntry> backtrace;
  printf("Safe-backtrace\n");
  core::fill_backtrace(backtrace);
  for ( size_t idx=0; idx<backtrace.size()-2; ++idx ) {
    if (backtrace[idx]._Stage == core::lispFrame) {
      dbg_print_frame(stdout,backtrace,idx,true);
    }
  }
}

void dbg_find_symbol(uintptr_t addr) {
}

};


namespace core {
CL_DEFUN void core__safe_backtrace() {
  dbg_safe_backtrace();
}
CL_DEFUN void core__safe_lisp_backtrace() {
  dbg_safe_lisp_backtrace();
}
};

extern "C" {

void tprint(void* ptr)
{
  core::dbg_printTPtr((uintptr_t) ptr,false);
}

void c_bt() {
  core::eval::funcall(core::_sym_bt->symbolFunction());
};

void c_btcl() {
  core::eval::funcall(core::_sym_btcl->symbolFunction());
};

void tsymbol(void* ptr)
{
  printf("%s:%d Looking up symbol at ptr->%p\n", __FILE__, __LINE__, ptr);
  core::T_sp result = llvmo::llvm_sys__lookup_jit_symbol_info(ptr);
  printf("      Result -> %s\n", _rep_(result).c_str());
}

};
namespace core {

CL_DEFUN std::string core__safe_repr(core::T_sp obj) {
  std::string result = dbg_safe_repr((uintptr_t)obj.raw_());
  return result;
}

  SYMBOL_EXPORT_SC_(CorePkg, printCurrentIhsFrameEnvironment);

};
