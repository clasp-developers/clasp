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


#if 0
// If you turn this on it takes a LOT of stack memory!!! and it runs even if DEBUG_SOURCE IS ON!!!!
#define BT_LOG(msg) {char buf[1024]; sprintf msg; LOG(BF("%s") % buf);}
#else
#define BT_LOG(msg)
#endif

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
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/primitives.h>
#include <clasp/core/array.h>
#include <clasp/core/bformat.h>
#include <clasp/core/write_ugly.h>
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

namespace core {
//////////////////////////////////////////////////////////////////////
//
// Define backtrace
//

// ------------------------------------------------------------------
//
// Write messages to cl:*debug-io*
//
#define WRITE_DEBUG_IO(fmt) core::write_bf_stream(fmt, cl::_sym_STARdebug_ioSTAR->symbolValue());

typedef void(*scan_callback)(gc::Vec0<BacktraceEntry>&backtrace, const std::string& filename, uintptr_t start);


struct ScanInfo {
  size_t  _Index;
  gc::Vec0<BacktraceEntry>* _Backtrace;
  scan_callback _Callback;
  ScanInfo() : _Index(0) {};
};

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
  OpenDynamicLibraryInfo(const std::string& f, void* h) : _Filename(f), _Handle(h) {};
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
  DebugInfo() {};
};

DebugInfo* global_DebugInfo = NULL;

DebugInfo& debugInfo() {
  if (!global_DebugInfo) {
    global_DebugInfo = new DebugInfo();
  }
  return *global_DebugInfo;
}

void add_dynamic_library_handle(const std::string& libraryName, void* handle) {
  BT_LOG((buf,"Starting\n" ));
#ifdef CLASP_THREADS
  WITH_READ_WRITE_LOCK(debugInfo()._OpenDynamicLibraryMutex);
#endif
  OpenDynamicLibraryInfo info(libraryName,handle);
  debugInfo()._OpenDynamicLibraryHandles[libraryName] = info;
}

bool if_dynamic_library_loaded_remove(const std::string& libraryName) {
#ifdef CLASP_THREADS
  WITH_READ_WRITE_LOCK(debugInfo()._OpenDynamicLibraryMutex);
#endif
  map<string,OpenDynamicLibraryInfo>::iterator fi = debugInfo()._OpenDynamicLibraryHandles.find(libraryName);
  bool exists = (fi!=debugInfo()._OpenDynamicLibraryHandles.end());
  if (exists) {
    BT_LOG((buf,"What about the stackmaps for this library - you need to remove them as well - I should probably NOT store stackmaps for libraries - but fetch them every time we need a backtrace!\n"));
    dlclose(fi->second._Handle);
    debugInfo()._OpenDynamicLibraryHandles.erase(libraryName);
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


struct Header {
  uint8_t  version;
  uint8_t  reserved0;
  uint16_t reserved1;
};

struct StkSizeRecord {
  uint64_t  FunctionAddress;
  uint64_t  StackSize;
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

void parse_record(gc::Vec0<BacktraceEntry>& backtrace, uintptr_t& address, size_t functionIndex, const StkSizeRecord& function, StkMapRecord& record, bool library) {
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
        WRITE_DEBUG_IO(BF("Stackmap-library function %p stack-size %lu patchPointId %u offset %d\n") % (void*)function.FunctionAddress % function.StackSize % patchPointID % offsetOrSmallConstant );
      } else {
        WRITE_DEBUG_IO(BF("Stackmap-jit function %p stack-size %lu patchPointId %u offset %d\n") % (void*)function.FunctionAddress % function.StackSize % patchPointID % offsetOrSmallConstant );
      }
    }
    if (patchPointID == 1234567 ) {
      BT_LOG((buf,"patchPointID matched at %p\n", (void*)recordAddress));
      for (size_t j=0; j<backtrace.size(); ++j ) {
        BT_LOG((buf,"comparing function#%lu @%p to %s\n", functionIndex, (void*)function.FunctionAddress, backtrace_frame(j,&backtrace[j]).c_str() ));
        if (function.FunctionAddress == backtrace[j]._FunctionStart) {
          backtrace[j]._Stage = lispFrame;  // anything with a stackmap is a lisp frame
          backtrace[j]._FrameSize = function.StackSize;
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


void walk_one_llvm_stackmap(gc::Vec0<BacktraceEntry>&backtrace, uintptr_t& address, uintptr_t end, bool library) {
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
    printf("%s:%d:%s stackmap header @%p is out of alignment\n", __FILE__, __LINE__, __FUNCTION__, (void*)stackMapAddress );
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
    BT_LOG((buf,"PASS2 Examining function #%lu at %p - %llu records\n", functionIndex, (void*)function.FunctionAddress, function.RecordCount));
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
  BT_LOG((buf,"register_llvm_stackmaps  startAddress: %p  endAddress: %p\n", (void*)startAddress, (void*)endAddress));
  WITH_READ_WRITE_LOCK(debugInfo()._StackMapsLock);
  StackMapRange range(startAddress,endAddress,numStackmaps);
  debugInfo()._StackMaps[startAddress] = range;
}


void search_jitted_stackmaps(gc::Vec0<BacktraceEntry>& backtrace)
{
  BT_LOG((buf,"Starting search_jitted_stackmaps\n" ));
  size_t num = 0;
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

void search_jitted_objects(gc::Vec0<BacktraceEntry>& backtrace, bool searchFunctionDescriptions)
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

#if 0
bool lookup_stack_map_entry(uintptr_t functionPointer, int& frameOffset, int& frameSize) {
  ensure_global_StackMapInfo();
  WITH_READ_LOCK(global_StackMapInfo->_FrameMapsLock);
  std::map<uintptr_t,FrameMap>::iterator find = (global_StackMapInfo->_FrameMaps).find((uintptr_t)functionPointer);
  if (find != (global_StackMapInfo->_FrameMaps).end()) {
    frameOffset = find->second._FrameOffset;
    frameSize = find->second._FrameSize;
    return true;
  }
  return false;
}
#endif
 

#if 0
bool closest_function(uintptr_t returnAddress, uintptr_t& functionAddress, uintptr_t& instructionOffset, int& frameSize, int& frameOffset) {
  instructionOffset = ~0;
  ensure_global_StackMapInfo();
  WITH_READ_LOCK(global_StackMapInfo->_FrameMapsLock);
  bool result = false;
  for ( auto entry : (global_StackMapInfo->_FrameMaps) ) {
    if (entry.second._FunctionPointer<returnAddress) {
      if ((returnAddress-entry.second._FunctionPointer)<instructionOffset) {
        result = true;
        instructionOffset = returnAddress-entry.second._FunctionPointer;
        functionAddress = entry.second._FunctionPointer;
        frameSize = entry.second._FrameSize;
        frameOffset = entry.second._FrameOffset;
      }
    }
  }
  return result;
}

bool function_name(uintptr_t returnAddress, std::string& name, uintptr_t& realFunctionAddress ) {
  ensure_global_StackMapInfo();
  WITH_READ_LOCK(global_StackMapInfo->_JittedObjectsLock);
  for ( auto entry : global_StackMapInfo->_JittedObjects ) {
    if (entry._ObjectPointer <= returnAddress
        && returnAddress <= (entry._ObjectPointer+entry._Size)) {
      realFunctionAddress = entry._ObjectPointer;
      name = entry._Name;
      BT_LOG((buf,"found address@%p in JittedObjects name: %s  realFunctionAddress: %p\n", (void*)returnAddress, name.c_str(), (void*)realFunctionAddress));
      return true;
    }
  }
  Dl_info dlinfo;
  int res = dladdr((void*)returnAddress,&dlinfo);
  if (res<=0) {
    printf("%s:%d:%s dladdr failed on returnAddress: %p - %s\n",
           __FILE__, __LINE__, __FUNCTION__, (void*)returnAddress, dlerror());
    abort();
  }
  if (dlinfo.dli_sname==0) {
    stringstream ss;
    ss << "dladdr-failed-for-return-address(" << (void*)returnAddress << ")";
    name = ss.str();
    return true;
  }
  name = dlinfo.dli_sname;
  realFunctionAddress = (uintptr_t)dlinfo.dli_saddr;
  printf("%s:%d:%s found address@%p with dladdr name: %s  realFunctionAddress: %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)returnAddress, name.c_str(), (void*)realFunctionAddress);
  return true;
}
#endif



#if defined(_TARGET_OS_DARWIN)


uint8_t * 
mygetsectiondata(
                 void* vmhp,
                 const char *segname,
                 const char *sectname,
                 unsigned long *size)
{
  const struct mach_header_64* mhp = (const struct mach_header_64*)vmhp;
  struct segment_command_64 *sgp;
  struct section_64 *sp;
  uint32_t i, j;
  intptr_t slide;
    
  slide = 0;
  sp = 0;
  sgp = (struct segment_command_64 *)
    ((char *)mhp + sizeof(struct mach_header_64));
  for(i = 0; i < mhp->ncmds; i++){
    if(sgp->cmd == LC_SEGMENT_64){
      if(strcmp(sgp->segname, "__TEXT") == 0){
        slide = (uintptr_t)mhp - sgp->vmaddr;
      }
      if(strncmp(sgp->segname, segname, sizeof(sgp->segname)) == 0){
        sp = (struct section_64 *)((char *)sgp +
                                   sizeof(struct segment_command_64));
        for(j = 0; j < sgp->nsects; j++){
          if(strncmp(sp->sectname, sectname,
                     sizeof(sp->sectname)) == 0 &&
             strncmp(sp->segname, segname,
                     sizeof(sp->segname)) == 0){
            *size = sp->size;
//   return (uint8_t*)sp;
            uint8_t* addr = ((uint8_t *)(sp->addr) + slide);
            return addr;
          }
          sp = (struct section_64 *)((char *)sp +
                                     sizeof(struct section_64));
        }
      }
    }
    sgp = (struct segment_command_64 *)((char *)sgp + sgp->cmdsize);
  }
  return(0);
}
//                          123456789.123456
#define STACKMAPS_SEGNAME  "__LLVM_STACKMAPS"
#define STACKMAPS_SECTNAME "__llvm_stackmaps"
#define LINKEDIT_SEGNAME   "__LINKEDIT"
#if 0 // keep this for the future if we read the library ourselves
__attribute__((optnone)) void search_library_macho_64( gc::Vec0<BacktraceEntry>& backtrace, const struct mach_header_64* loaded_mhp, const struct mach_header_64* file_mhp  )
{
  bool foundStackmaps = false;
  uintptr_t stackmapStart;
  size_t stackmapSize;
  struct segment_command_64 *sgp;
  struct section_64 *sp;
  uint32_t i, j;
  intptr_t slide;
  slide = 0;
  sp = 0;
  sgp = (struct segment_command_64 *) ((char *)file_mhp + sizeof(struct mach_header_64));
  for(i = 0; i < file_mhp->ncmds; i++){
//    printf("%s:%d:%s  %d/%d segment name: %s\n", __FILE__, __LINE__, __FUNCTION__, i, mhp->ncmds, sgp->segname);
    if(sgp->cmd == LC_SEGMENT_64){
      if(strcmp(sgp->segname, "__TEXT") == 0) slide = (uintptr_t)file_mhp - sgp->vmaddr;
      if(strncmp(sgp->segname, STACKMAPS_SEGNAME, sizeof(sgp->segname)) == 0){
        sp = (struct section_64 *)((char *)sgp + sizeof(struct segment_command_64));
//        printf("%s:%d:%s  found stackmaps nsects %d\n", __FILE__, __LINE__, __FUNCTION__, sgp->nsects);
        for(j = 0; j < sgp->nsects; j++){
          if(strncmp(sp->sectname, STACKMAPS_SECTNAME,
                     sizeof(sp->sectname)) == 0 &&
             strncmp(sp->segname, STACKMAPS_SEGNAME,
                     sizeof(sp->segname)) == 0){
//            printf("%s:%d:%s  found section |%s| |%s|\n", __FILE__, __LINE__, __FUNCTION__, sp->sectname, sp->segname);
            stackmapSize = sp->size;
//   return (uint8_t*)sp;
            uintptr_t address = ((uintptr_t)(sp->addr) + slide)+(uintptr_t)loaded_mhp;
            uintptr_t end = address+stackmapSize;
            printf("%s:%d:%s Stackmap start at %p up to %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)address, (void*)end);
            foundStackmaps = true;
            while (address<end) {
              walk_one_llvm_stackmap(backtrace,address,end);
            }
          }
          sp = (struct section_64 *)((char *)sp +
                                     sizeof(struct section_64));
        }
      }
    } else if (sgp->cmd == LC_SYMTAB) {
      uintptr_t u = (uintptr_t)loaded_mhp;
      const struct symtab_command* st = (const struct symtab_command*)sgp;
      printf("%s:%d:%s  found LC_SYMTAB symoff %p  nsyms %u  stroff %p strsize %u\n", __FILE__, __LINE__, __FUNCTION__,
             (void*)(u+st->symoff),
             st->nsyms,
             (void*)(u+st->stroff),
             st->strsize);
      const struct nlist_64* symabs = (const struct nlist_64*)(u+st->symoff);
      const char* strabs = (const char*)(u+st->stroff);
      printf("   loaded_mhp -> %p   symabs -> %p  strabs -> %p\n", (void*)u, symabs, strabs );
#if 0      
      for (size_t symi=0; symi<st->nsyms; symi++ ) {
        const char* symname;
        printf("      n_strx=%u  u = %p address = %p\n", (uint32_t)nl->n_un.n_strx, (void*)u, (void*)(nl->n_value));
        if (nl->n_un.n_strx==0) {
          symname = "";
        } else if (nl->n_un.n_strx<0 ||
                   (uint32_t)nl->n_un.n_strx > st->strsize) {
          symname = "BAD-SYMBOL";
        } else {
          symname = (const char*)(nl->n_un.n_strx+u+st->stroff);
        }
        printf("        symname = %s\n", symname);
        nl = nl + sizeof(const struct nlist_64);
      }
#endif
    }
    sgp = (struct segment_command_64 *)((char *)sgp + sgp->cmdsize);
  }
}



void search_mach( gc::Vec0<BacktraceEntry>& backtrace, const struct mach_header* loaded_mhp, const struct mach_header* file_mhp )
{
  bool foundStackmaps = false;
  uintptr_t stackmapStart;
  size_t stackmapSize;
  struct segment_command_64 *sgp;
  struct section_64 *sp;
  uint32_t i, j;
  intptr_t slide;
  slide = 0;
  sp = 0;
  printf("%s:%d:%s  loaded_mhp -> %p  file_mhp -> %p\n", __FILE__, __LINE__, __FUNCTION__, loaded_mhp, file_mhp );
  // Check the header - 32bit or 64bit
  uint32_t magic = file_mhp->magic;
  if (magic != MH_MAGIC_64) {
    printf("%s:%d:%s Ignore magic %x for now\n", __FILE__, __LINE__, __FUNCTION__, magic );
    return;
  }
  int is_magic_64 = (magic==MH_MAGIC_64 || magic==MH_CIGAM_64);
  int swap_bytes = (magic==MH_CIGAM || magic == MH_CIGAM_64);
  if (swap_bytes) {
    printf("%s:%d:%s when reading a macho file swap_byte is true\n", __FILE__, __LINE__, __FUNCTION__);
    abort();
  }
  if (is_magic_64) {
    search_library_macho_64(backtrace,(const struct mach_header_64*)loaded_mhp, (const struct mach_header_64*)file_mhp);
  } else {
    printf("%s:%d:%s Handle 32bit libraries\n", __FILE__, __LINE__, __FUNCTION__ );
    abort();
    //search_library_macho_32(backtrace,mhp);
  }
}
#endif

std::string& ltrim(std::string& str, const std::string& chars = "\t\n\v\f\r ")
{
  str.erase(0, str.find_first_not_of(chars));
  return str;
}
 
std::string& rtrim(std::string& str, const std::string& chars = "\t\n\v\f\r ")
{
  str.erase(str.find_last_not_of(chars) + 1);
  return str;
}
 
std::string& trim(std::string& str, const std::string& chars = "\t\n\v\f\r ")
{
  return ltrim(rtrim(str, chars), chars);
}

struct NmSymbol {
  uintptr_t _Address;
  char _Type;
  std::string _Name;
  NmSymbol(uintptr_t address, char type, const std::string& name) : _Address(address), _Type(type), _Name(name) {};
};

std::vector<NmSymbol> load_symbol_table(const char* filename, uintptr_t header) {
  std::vector<NmSymbol> symbol_table;
  stringstream nm_cmd;
  nm_cmd << "/usr/bin/nm -numeric-sort -defined-only " << filename;
  FILE* fnm = popen( nm_cmd.str().c_str(), "r");
  if (fnm==NULL) {
    printf("%s:%d:%s  Could not popen %s\n", __FILE__, __LINE__, __FUNCTION__, nm_cmd.str().c_str());
    return symbol_table;
  }
#define BUFLEN 2048
  {
    char buf[BUFLEN+1];
    char type[BUFLEN+1];
    char name[BUFLEN+1];
    while (!feof(fnm)) {
      fgets(buf,BUFLEN,fnm);
      const char* cur = buf;
//      printf("%s:%d:%s Read line: %s\n", __FILE__, __LINE__, __FUNCTION__, cur);
      // Read the address
      uintptr_t address = 0;
      uintptr_t digit;
      // Read the hex address
      while (*cur != ' ') {
        char c = *cur;
        if (c>='A'&&c<='Z') {
          digit = c-'A'+10;
        } else if (c>='0'&&c<='9') {
          digit = c-'0';
        } else if (c>='a'&&c<='z') {
          digit = c-'a'+10;
        } else {
          printf("%s:%d:%s Hit non-hex digit %c in line: %s\n", __FILE__,__LINE__,__FUNCTION__,c,buf);
          digit = 0;
        }
        address = address*16+digit;
//        printf("cur: %p c: %c digit: %lu   address: %p\n", cur, c, digit, (void*)address);
        ++cur;
      }
      // skip spaces
      while (*cur==' ') ++cur;
      // Read the type
      char type = *cur;
      cur++;
      // skip spaces
      while (*cur==' ') ++cur;
      // Read the name
      size_t nameidx = 0;
      while (*cur!='\0'&&*cur>' ') {
        name[nameidx] = *cur;
        ++cur;
        ++nameidx;
      }
      name[nameidx] = '\0';
      uintptr_t real_address = (uintptr_t)address + (uintptr_t)header;
      std::string sname(name);
//      printf("         address: %p  type: %c   name: %s\n", (void*)address, type, name);
      symbol_table.push_back(NmSymbol(real_address,type,sname));
    }
    symbol_table.push_back(NmSymbol(~0,'d',"TERMINAL_SYMBOL")); // one symbol to end them all
    pclose(fnm);
  }
  return symbol_table;
}

bool binary_search(uintptr_t address, const std::vector<NmSymbol>& symbol_table, size_t& index )
{
  size_t l = 0;
  size_t r = symbol_table.size()-1;
  if (address<symbol_table[0]._Address || address >= symbol_table[symbol_table.size()-1]._Address) return false;
  while (true) {
    if (l>=r) {
      index = l-1;
      return true;
    }
    size_t m = (l+r)/2;
    if (symbol_table[m]._Address<=address) {
      l = m+1;
    } else if (symbol_table[m]._Address>address) {
      r = m;
    } else {
      index = l;
      return true;
    }
  }
}

void search_with_otool_and_nm(gc::Vec0<BacktraceEntry>& backtrace, const char* filename, const struct mach_header* header)
{
#define BUFLEN 2048
  std::vector<NmSymbol> symbol_table = load_symbol_table(filename,(uintptr_t)header);
  if (backtrace.size() == 0) {
    for (auto entry : symbol_table ) {
      WRITE_DEBUG_IO(BF("Symbol start %p type %c name %s\n") % (void*)entry._Address % entry._Type % entry._Name);
    }
  } else {
    if (symbol_table.size()>0) {
      for ( size_t j=0; j<backtrace.size(); ++j ) {
        size_t index;
        bool found = binary_search(backtrace[j]._ReturnAddress,symbol_table,index);
        if (found && (symbol_table[index]._Type == 't' || symbol_table[index]._Type == 'T')) {
          backtrace[j]._Stage = symbolicated;
          backtrace[j]._FunctionStart = symbol_table[index]._Address;
          backtrace[j]._FunctionEnd = symbol_table[index+1]._Address;
          backtrace[j]._SymbolName = symbol_table[index]._Name;
        }
      }
  // Look for FunctionDescriptions
      for (auto entry : symbol_table ) {
        if (entry._Type == 'd' || entry._Type=='D' || entry._Type=='s' || entry._Type=='S') {
          if (entry._Name.size()>5 && entry._Name.substr(entry._Name.size()-5,entry._Name.size()) == "^DESC") {
            std::string function_part = entry._Name.substr(0,entry._Name.size()-5);
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
#if 1
  // Use mygetsectiondata to walk the library because stackmaps are mmap'd
  // in places that I am not able to calculate using otool
  unsigned long section_size;
  uint8_t* p_section =  mygetsectiondata( (void*)header,
                                          "__LLVM_STACKMAPS",
                                          "__llvm_stackmaps",
                                          &section_size );
  if (p_section!=NULL ) {
    uintptr_t address = (uintptr_t)p_section;
    uintptr_t endAddress = address+section_size;
    while (address<endAddress) {
      walk_one_llvm_stackmap(backtrace,address,endAddress,true);
    }
  }
  
#else
  {
  // Now use otool to get the stackmaps
    stringstream otool_cmd;
    otool_cmd << "/usr/bin/otool -l " << filename;
    FILE* fotool = popen( otool_cmd.str().c_str(), "r");
    if (fotool==NULL) {
      printf("%s:%d:%s  Could not popen %s\n", __FILE__, __LINE__, __FUNCTION__, otool_cmd.str().c_str());
      return;
    }
    void* address;
    void* size;
    char buf[BUFLEN+1];
    char key[BUFLEN+1];
    char arg[BUFLEN+1];
    bool found_stackmaps = false;
    while (!feof(fotool)) {
      fgets(buf,BUFLEN,fotool);
      std::string sectionbuf(buf);
      if (trim(sectionbuf) == "Section") {
        fgets(buf,BUFLEN,fotool);
        std::string sectnamebuf(buf);
        if (trim(sectnamebuf) == "sectname __llvm_stackmaps") {
          fgets(buf,BUFLEN,fotool);
          std::string segnamebuf(buf);
          if (trim(segnamebuf) == "segname __LLVM_STACKMAPS") {
            fgets(buf,BUFLEN,fotool);
            sscanf(buf,"%s %p",key,&address);
            if (strcmp(key,"addr")!=0) {
              printf("%s:%d:%s  Could not read __LLVM_STACKMAPS properly for command: %s line: %s\n", __FILE__, __LINE__, __FUNCTION__, otool_cmd.str().c_str(), buf);
              pclose(fotool);
              return;
            }
            fgets(buf,BUFLEN,fotool);
            sscanf(buf,"%s %p",key,&size);
            if (strcmp(key,"size")!=0) {
              printf("%s:%d:%s  Could not read __LLVM_STACKMAPS size properly for command: %s line: %s\n", __FILE__, __LINE__, __FUNCTION__, otool_cmd.str().c_str(), buf);
              pclose(fotool);
              return;
            }
            found_stackmaps = true;
          }
        }
      }
    }
    pclose(fotool);
    if (found_stackmaps) {
      uintptr_t stackmap_address = (uintptr_t)address+(uintptr_t)header;
      uintptr_t stackmaps_end = stackmap_address+(uintptr_t)size;
//            printf("%s:%d:%s  Read __LLVM_STACKMAPS info address: %p end: %p\n", __FILE__, __LINE__, __FUNCTION__, stackmap_address, stackmaps_end);
      while (stackmap_address<stackmaps_end) {
        BT_LOG((buf," Stackmap start at %p\n", (void*)stackmap_address));
        if (backtrace.size()==0) {
          WRITE_DEBUG_IO(BF("Walking stackmap at %p\n") % (void*)stackmap_address);
        }
        walk_one_llvm_stackmap(backtrace,stackmap_address,stackmaps_end,true);
      }
    }
  }
#endif
}

struct SafeMMap {
  const char* _Filename;
  int _FileDescriptor;
  void*  _Address;
  size_t _FileSize;
  
  SafeMMap(const char* filename, int fd, size_t size) : _Filename(filename), _FileDescriptor(fd), _FileSize(size) {
    this->_Address = mmap(NULL,size,PROT_READ, MAP_PRIVATE, fd, 0);
    if (this->_Address==0) {
      close(this->_FileDescriptor);
      SIMPLE_ERROR(BF("Could not mmap %s") % filename);
    }
  };
  ~SafeMMap() {
    int ret = munmap(this->_Address,this->_FileSize);
    if (ret!=0) {
      printf("%s:%d:%s Could not munmap file %s\n", __FILE__, __LINE__, __FUNCTION__, this->_Filename);
    }
    close(this->_FileDescriptor);
  };
};

void walk_loaded_objects(gc::Vec0<BacktraceEntry>& backtrace) {
#include <sys/stat.h>
//    printf("Add support to walk symbol tables and stackmaps for DARWIN\n");
  uint32_t num_loaded = _dyld_image_count();
  for ( size_t idx = 0; idx<num_loaded; ++idx ) {
    const char* filename = _dyld_get_image_name(idx);
    const struct mach_header* loaded_header = _dyld_get_image_header(idx);
#if 1
      // Use otool and nm
    if (backtrace.size()==0) {
      WRITE_DEBUG_IO(BF("Library %s\n") % filename );
    }
    search_with_otool_and_nm(backtrace,filename,loaded_header);
#else      
    uint32_t magic = loaded_header->magic;
    printf("%s:%d:%s Searching DARWIN filename: %s magic: %x\n", __FILE__, __LINE__, __FUNCTION__, filename, magic);
    int fd;
    if ((fd = open(filename,O_RDONLY,0)) ==-1) {
      SIMPLE_ERROR(BF("Could not open %s") % filename);
    }
    struct stat buf;
    int res = fstat(fd,&buf);
    SafeMMap file_header_mmap(filename,fd,buf.st_size);
    const struct mach_header* file_header = (const struct mach_header*)file_header_mmap._Address;
    printf("%s:%d:%s  library: %s loaded_mhp -> %p  file_mhp -> %p\n", __FILE__, __LINE__, __FUNCTION__, filename, loaded_header, file_header );
    search_mach(backtrace,loaded_header,file_header);
#endif
  }
}

#endif




#ifdef _TARGET_OS_LINUX


std::atomic<bool> global_elf_initialized;
void ensure_libelf_initialized() {
  if (!global_elf_initialized) {
    if (elf_version(EV_CURRENT) == EV_NONE) SIMPLE_ERROR(BF("ELF library initializtion failed %s") % elf_errmsg(-1));
    global_elf_initialized = true;
  }
}

void scan_elf_library_for_symbols_then_stackmaps(gc::Vec0<BacktraceEntry>&backtrace, const std::string& filename, uintptr_t start)
{
  BT_LOG((buf,"Searching symbol table %s memory-start %p\n", filename.c_str(), (void*)start ));
  Elf         *elf;
  GElf_Shdr   shdr;
  Elf_Data    *data;
  int         fd, ii, count;
  WRITE_DEBUG_IO(BF("Library %s\n") % filename );
  ensure_libelf_initialized();
  elf_version(EV_CURRENT);
  fd = open(filename.c_str(), O_RDONLY);
  if (fd < 0) {
    BT_LOG((buf,"Could not open %s", filename.c_str()));
    return;
  }
  if ((elf = elf_begin(fd, ELF_C_READ, NULL)) == NULL) {
    close(fd);
    SIMPLE_ERROR(BF("Error with elf_begin for file %s - %s") % filename % elf_errmsg(-1));
  }
  Elf_Scn     *scn = NULL;
  // Search the symbol tables for functions that contain the return address
  scn = NULL;
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
        if (backtrace.size()==0) {
          WRITE_DEBUG_IO(BF("Symbol start %p end %p name %s\n") % (void*)symbol_start % (void*)symbol_end % elf_strptr(elf,shdr.sh_link , (size_t)sym.st_name));
        }
        BT_LOG((buf,"Looking at symbol %s type: %d\n", elf_strptr(elf,shdr.sh_link , (size_t)sym.st_name), ELF64_ST_TYPE(sym.st_info)));
        if (ELF64_ST_TYPE(sym.st_info) == STT_FUNC) {
          BT_LOG((buf,"It's a function symbol\n" ));
          for ( size_t j=0; j<backtrace.size(); ++j ) {
            if (backtrace[j]._Stage==undefined) {
              if (symbol_start<=backtrace[j]._ReturnAddress && backtrace[j]._ReturnAddress<symbol_end) {
                backtrace[j]._Stage = symbolicated;
                backtrace[j]._FunctionStart = symbol_start;
                backtrace[j]._FunctionEnd = symbol_start+(uintptr_t)sym.st_size;
                backtrace[j]._SymbolName = elf_strptr(elf,shdr.sh_link , (size_t)sym.st_name);
                BT_LOG((buf,"Identified symbol name %s for frame %lu\n", backtrace[j]._SymbolName.c_str(), j));
//                break;
              }
            }
          }
        }
      }
    } 
  }
  scn = NULL;
  // Search the symbol tables for FunctionDescription objects
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
        BT_LOG((buf,"Looking at symbol %s type: %d\n", elf_strptr(elf,shdr.sh_link , (size_t)sym.st_name), ELF64_ST_TYPE(sym.st_info)));
        if (ELF64_ST_TYPE(sym.st_info) == STT_OBJECT && sym.st_size == sizeof(FunctionDescription)) { // a quick way to identify FunctionDescriptions
          BT_LOG((buf,"It may be a FunctionDescription symbol\n" ));
          for ( size_t j=0; j<backtrace.size(); ++j ) {
            if (backtrace[j]._Stage==symbolicated) {
              const char* symname = elf_strptr(elf,shdr.sh_link , (size_t)sym.st_name);
              stringstream ss;
              ss << backtrace[j]._SymbolName
                 << "^DESC";
              if (ss.str() == symname) {
                uintptr_t symbol_start = (uintptr_t)sym.st_value+start;
                backtrace[j]._Stage = lispFrame; // anything with a FunctionDescription is a LispFrame
                backtrace[j]._FunctionDescription = symbol_start;
              }
            }
          }
        }
      }
    }
  }
  scn = NULL ;
  const char * name , *p;
  size_t n , shstrndx , sz ;
  if ( elf_getshdrstrndx (elf, &shstrndx ) != 0)
    SIMPLE_ERROR(BF("elf_getshdrstrndx () failed : %s.") % elf_errmsg ( -1));
  while (( scn = elf_nextscn (elf, scn )) != NULL ) { 
    if ( gelf_getshdr ( scn, &shdr ) != & shdr )
      SIMPLE_ERROR(BF("getshdr() failed : %s.") % elf_errmsg ( -1));
    if (( name = elf_strptr (elf, shstrndx, shdr.sh_name )) == NULL ) 
      SIMPLE_ERROR(BF("stackmaps elf_strptr() failed : %s.") % elf_errmsg ( -1));
    if (strncmp(name,".llvm_stackmaps",strlen(".llvm_stackmaps"))==0) {
      uintptr_t addr = shdr.sh_addr+start;
      uintptr_t stackmap_end = addr + shdr.sh_size;
	//      ( void ) printf ( "Section %-4.4jd %s address: %lu offset: %lu  size: %lu\n", ( uintmax_t )
	//                        elf_ndxscn ( scn ), name, shdr.sh_addr, shdr.sh_offset, shdr.sh_size );
      while (addr<stackmap_end) {
        BT_LOG((buf," Stackmap start at %p\n", (void*)addr));
        walk_one_llvm_stackmap(backtrace,addr,stackmap_end,true);
      }
    }
  }
  elf_end(elf);
  close(fd);
}

  
extern "C" char* __progname_full; // The name of the executable?

int elf_loaded_object_callback(struct dl_phdr_info *info, size_t size, void* data)
{
  ScanInfo* scan_callback_info = (ScanInfo*)data;
  const char *type;
  int p_type, j;
  std::string libname;
  if (scan_callback_info->_Index==0 && strlen(info->dlpi_name) == 0 ) {
    libname = __progname_full;
  } else {
    libname = info->dlpi_name;
  }
  BT_LOG((buf,"Name: \"%s\" address: %p (%d segments)\n", libname.c_str(), (void*)info->dlpi_addr, info->dlpi_phnum));
  scan_elf_library_for_symbols_then_stackmaps(*(scan_callback_info->_Backtrace),libname.c_str(),info->dlpi_addr);
  scan_callback_info->_Index++;
  return 0;
}

#endif

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
        /*Nilable?*/ T_sp sfi = core__source_file_info(func->sourcePathname());
        string sourceName = "cannot-determine";
        if (sfi.notnilp()) {
          sourceName = gc::As<SourceFileInfo_sp>(sfi)->fileName();
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


void safe_backtrace(gc::Vec0<BacktraceEntry>& backtrace_)
{
#define START_BACKTRACE_SIZE 512
  void** return_buffer;
  size_t num = START_BACKTRACE_SIZE;
  for ( int i=0; i<100; ++i ) {
    void** buffer = (void**)malloc(sizeof(void*)*num);
    size_t returned = backtrace(buffer,num);
    if (returned < num) {
      backtrace_.resize(returned);
      for ( size_t i=0; i<returned; ++i ) {
        backtrace_[i]._ReturnAddress = (uintptr_t)buffer[i];
      }
      free(buffer);
      char **strings = backtrace_symbols(buffer, returned);
      for ( size_t i=0; i<returned; ++i ) {
        backtrace_[i]._SymbolName = strings[i];
      }
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


void fill_in_interpreted_frames(gc::Vec0<BacktraceEntry>& backtrace) {
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
        backtrace[idx]._Arguments = frame->arguments();
        //printf("%s:%d:%s %s %s  MATCH!!!!\n", __FILE__, __LINE__, __FUNCTION__, backtrace[idx]._SymbolName.c_str(), _rep_(backtrace[idx]._Arguments).c_str());
      }
    }
    frame = frame->_Previous;
  }
}




void fill_backtrace_or_dump_info(gc::Vec0<BacktraceEntry>& backtrace) {
#ifdef _TARGET_OS_LINUX
  ScanInfo scan;
  scan._Backtrace = &backtrace;
    // Search the symbol tables and stackmaps
  dl_iterate_phdr(elf_loaded_object_callback,&scan);
#endif
#ifdef _TARGET_OS_DARWIN
//    printf("walk symbol tables and stackmaps for DARWIN\n");
  walk_loaded_objects(backtrace);
#endif
    // Now search the jitted objects
  search_jitted_objects(backtrace,false);
  search_jitted_objects(backtrace,true); // Search them twice to find all FunctionDescription objects
  search_jitted_stackmaps(backtrace);
}


SYMBOL_EXPORT_SC_(CorePkg,make_backtrace_frame);
SYMBOL_EXPORT_SC_(KeywordPkg,function_name);
SYMBOL_EXPORT_SC_(KeywordPkg,arguments);
SYMBOL_EXPORT_SC_(KeywordPkg,closure);

CL_LAMBDA(&optional (depth 0));
CL_DECLARE();
CL_DOCSTRING("backtrace");
CL_DEFUN T_sp core__clib_backtrace_as_list() {
  ql::list result;
  char *funcname = (char *)malloc(1024);
  size_t funcnamesize = 1024;
  uintptr_t stackTop = (uintptr_t)my_thread_low_level->_StackTop;
  gc::Vec0<BacktraceEntry> backtrace;
  BT_LOG((buf,"About to safe_backtrace\n" ));
  safe_backtrace(backtrace);
  size_t nptrs = backtrace.size()-2;
  nptrs -= 2; // drop the last two frames
    // Fill in the base pointers
  BT_LOG((buf,"About to get bp's\n" ));
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
  fill_backtrace_or_dump_info(backtrace);

    
    // Now get the arguments
  BT_LOG((buf,"Getting arguments\n"));
  for ( size_t index=1; index<backtrace.size(); ++index) {
    if (backtrace[index]._Stage == lispFrame && backtrace[index]._FrameSize!=0) {
      T_mv arg_mv = capture_arguments(backtrace[index]._FunctionStart,backtrace[index]._BasePointer,backtrace[index]._FrameOffset );
      BT_LOG((buf,"found arguments for frame %lu\n", index ));
      backtrace[index]._Arguments = arg_mv;
      backtrace[index]._Closure = arg_mv.second();
    } else {
      backtrace[index]._Arguments = _Nil<T_O>();
      backtrace[index]._Closure = _Nil<T_O>();
    }
  }
    // fill in the interpreted frames here
  BT_LOG((buf,"fill in interpreted frames here\n"));;
  fill_in_interpreted_frames(backtrace);

  BT_LOG((buf," building backtrace as list\n" ));
    // Move the frames into Common Lisp
  for ( size_t i=1; i<backtrace.size(); ++i ) {
    T_sp entry;
    Symbol_sp stype;
    if (backtrace[i]._Stage == lispFrame) {
      stype = INTERN_(kw,lisp);
    } else {
      stype = INTERN_(kw,c_PLUS__PLUS_);
    }
    T_sp funcDesc = _Nil<T_O>();
    if (backtrace[i]._FunctionDescription!=0) {
      funcDesc = Pointer_O::create((void*)backtrace[i]._FunctionDescription);
    }
    ql::list args;
    args << INTERN_(kw,type) << stype
         << INTERN_(kw,return_address) << Pointer_O::create((void*)backtrace[i]._ReturnAddress)
         << INTERN_(kw,raw_name) <<  SimpleBaseString_O::make(backtrace[i]._SymbolName)
         << INTERN_(kw,arguments) << backtrace[i]._Arguments
         << INTERN_(kw,closure) << backtrace[i]._Closure
         << INTERN_(kw,base_pointer) << Pointer_O::create((void*)backtrace[i]._BasePointer)
         << INTERN_(kw,frame_offset) << core::make_fixnum(backtrace[i]._FrameOffset)
         << INTERN_(kw,frame_size) << core::make_fixnum(backtrace[i]._FrameSize)
         << INTERN_(kw,function_start_address) << Pointer_O::create((void*)backtrace[i]._FunctionStart)
         << INTERN_(kw,function_end_address) << Pointer_O::create((void*)backtrace[i]._FunctionEnd)
         << INTERN_(kw,function_description) << funcDesc;
    if (_sym_make_backtrace_frame->fboundp()) {
      entry = core__apply0(_sym_make_backtrace_frame->symbolFunction(),args.cons());
    } else {
      entry = core::Cons_O::create(_sym_make_backtrace_frame,args.cons());
    }
    result << entry;
  }
  return result.cons();
}


CL_LAMBDA(&optional (depth 0));
CL_DECLARE();
CL_DOCSTRING("backtrace");
CL_DEFUN void core__clib_backtrace(int depth) {
    // Play with Unix backtrace(3)
#define BACKTRACE_SIZE 1024
  printf("Entered core__clib_backtrace - symbol: %s\n", _rep_(INTERN_(core, theClibBacktraceFunctionSymbol)).c_str());
  void *buffer[BACKTRACE_SIZE];
  char *funcname = (char *)malloc(1024);
  size_t funcnamesize = 1024;
  int nptrs;
  nptrs = backtrace(buffer, BACKTRACE_SIZE);
  char **strings = backtrace_symbols(buffer, nptrs);
  if (strings == NULL) {
    printf("No backtrace available\n");
    return;
  } else {
    for (int i = 0; i < nptrs; ++i) {
      if (depth && i >= depth)
        break;
      std::string front = std::string(strings[i], 57);
      char *fnName = &strings[i][59];
      char *fnCur = fnName;
      int len = 0;
      for (; *fnCur; ++fnCur) {
        if (*fnCur == ' ')
          break;
        ++len;
      }
      int status;
      fnName[len] = '\0';
      char *rest = &fnName[len + 1];
      char *ret = abi::__cxa_demangle(fnName, funcname, &funcnamesize, &status);
      if (status == 0) {
        funcname = ret; // use possibly realloc()-ed string
        printf("  %s %s %s\n", front.c_str(), funcname, rest);
      } else {
	  // demangling failed. Output function name as a C function with
	  // no arguments.
        printf("  %s\n", strings[i]);
      }
    }
  }
  if (strings)
    free(strings);
  if (funcname)
    free(funcname);
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("framePointers");
CL_DEFUN void core__frame_pointers() {
  void *fp = __builtin_frame_address(0); // Constant integer only
  if (fp != NULL)
    printf("Frame pointer --> %p\n", fp);
};


CL_DEFUN void core__dump_symbol_and_stackmap_info() {
  gc::Vec0<BacktraceEntry> emptyBacktrace;
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
    VectorObjects_sp vargs = gc::As<VectorObjects_sp>(args);
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

void dbg_VaList_sp_describe(T_sp obj) {
    // Convert the T_sp object into a VaList_sp object
  VaList_sp vl = VaList_sp((gc::Tagged)obj.raw_());
  printf("Original va_list at: %p\n", &((Vaslist *)gc::untag_vaslist(reinterpret_cast<Vaslist *>(obj.raw_())))->_Args);
    // Create a copy of the Vaslist with a va_copy of the va_list
  Vaslist vlcopy_s(*vl);
  VaList_sp vlcopy(&vlcopy_s);
  printf("Calling dump_Vaslist_ptr\n");
  bool atHead = dump_Vaslist_ptr(&vlcopy_s);
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

void dbg_mv_lowLevelDescribe(T_mv mv_obj) {
  gc::Vec0<core::T_sp> values;
  mv_obj.saveToVec0(values);
  for (int i(0), iEnd(values.size()); i < iEnd; ++i) {
    printf("Multiple value#%d\n", i);
    dbg_lowLevelDescribe(values[i]);
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

void dbg_describeTPtr(uintptr_clasp_t raw) {
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

void dbg_printTPtr(uintptr_clasp_t raw, bool print_pretty) {
  core::T_sp sout = cl::_sym_STARstandard_outputSTAR->symbolValue();
  T_sp obj = gctools::smart_ptr<T_O>((gc::Tagged)raw);
  clasp_write_string((BF("dbg_printTPtr Raw pointer value: %p\n") % (void *)obj.raw_()).str(), sout);
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, _Nil<T_O>());
  scope.pushSpecialVariableAndSet(cl::_sym_STARprint_readablySTAR, _lisp->_boolean(print_pretty));
  clasp_write_string((BF("dbg_printTPtr object class --> %s\n") % _rep_(lisp_instance_class(obj)->_className())).str(), sout);
  fflush(stdout);
  write_ugly_object(obj, sout);
  clasp_force_output(sout);
}


}

extern "C" {
void dbg_safe_print(uintptr_t raw) {
  core::T_sp obj((gc::Tagged)raw);
  if (gc::IsA<core::Symbol_sp>(obj)) {
    core::Symbol_sp sym = gc::As_unsafe<core::Symbol_sp>(obj);
    printf(" %s", sym->formattedName(true).c_str());
  } else if (obj.consp()) {
    printf(" (");
    while (obj.consp()) {
      dbg_safe_print((uintptr_t)CONS_CAR(obj).raw_());
      obj = CONS_CDR(obj);
    }
    if (obj.notnilp()) {
      printf(" . ");
      dbg_safe_print(obj);
    }
    printf(" )");
  } else if (obj.fixnump()) {
    printf(" %" PFixnum, obj.unsafe_fixnum());
  } else if (obj.nilp()) {
    printf(" NIL");
  } else if (obj.unboundp()) {
    printf(" #:UNBOUND");
  } else if (obj.characterp()) {
    printf(" #\\%c[%d]", obj.unsafe_character(), obj.unsafe_character());
  } else if (obj.single_floatp()) {
    printf(" %f", obj.unsafe_single_float());
  } else if (obj.generalp()) {
    core::General_sp gen = gc::As_unsafe<core::General_sp>(obj);
    printf(" #<%s @%p>", gen->className().c_str(), gen.raw_());
  } else {
    printf(" #<RAW@%p\n", (void*)obj.raw_());
  }
}

void dbg_safe_println(uintptr_t raw) {
  dbg_safe_print(raw);
  printf("\n");
}

};


extern "C" {

void tprint(void* ptr)
{
  core::dbg_printTPtr((uintptr_clasp_t) ptr,false);
}

void c_ehs() {
  printf("%s:%d ExceptionStack summary\n%s\n", __FILE__, __LINE__, my_thread->exceptionStack().summary().c_str());
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

  SYMBOL_EXPORT_SC_(CorePkg, printCurrentIhsFrameEnvironment);

};
