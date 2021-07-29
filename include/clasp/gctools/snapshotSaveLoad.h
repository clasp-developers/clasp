/*
    File: snapshotSaveLoad.h
*/


#ifndef snapshotSaveLoad_H //[
#define snapshotSaveLoad_H

#include <dlfcn.h>
#include <clasp/core/common.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/gctools/snapshotSaveLoad.fwd.h>



namespace snapshotSaveLoad {

extern bool global_debugSnapshot;


enum PointerType { UninitializedPointer=0, EndPointer=1, FunctionPointer=2, VtablePointer=3 };

struct PointerBase {
  PointerType _pointerType;
  uintptr_t*   _ptrptr;
  uintptr_t   _address;
  PointerBase(PointerType pt, uintptr_t* pp, uintptr_t a) : _pointerType(pt), _ptrptr(pp), _address(a) {};
};

struct GroupedPointer {
  PointerType _pointerType;
  uintptr_t   _address;
  GroupedPointer() :
    _pointerType(UninitializedPointer),
    _address(0)
  {};
  GroupedPointer( PointerType t, uintptr_t address ) :
    _pointerType(t),
    _address(address)
  {};
};

struct SymbolInfo {
  uintptr_t _Address;
  uint16_t  _AddressOffset;
  uint16_t  _SymbolLength;
  int       _SymbolOffset;
  SymbolInfo( ) : _Address(0), _AddressOffset(0), _SymbolLength(0), _SymbolOffset(-1) {};
  SymbolInfo(uintptr_t address, uint addressOffset, int length, int offset ) : _Address(address), _AddressOffset(addressOffset), _SymbolLength(length), _SymbolOffset(offset) {};
};

struct ISLLibrary {
  std::string              _Name;
  bool                     _Executable;
  gctools::clasp_ptr_t     _TextStart;
  gctools::clasp_ptr_t     _TextEnd;
  uintptr_t                _VtableStart;
  uintptr_t                _VtableEnd;
  std::vector<char>        _SymbolBuffer;
  size_t                   _SymbolIndex;
  std::vector<PointerBase> _Pointers;
  std::vector<GroupedPointer> _GroupedPointers;
  std::vector<SymbolInfo>     _SymbolInfo;
  ISLLibrary(const std::string& name, bool executable, gctools::clasp_ptr_t start, gctools::clasp_ptr_t end, uintptr_t vtableStart, uintptr_t vtableEnd )
    : _Name(name),
      _Executable(executable),
      _TextStart(start),
      _TextEnd(end),
      _VtableStart(vtableStart),
      _VtableEnd(vtableEnd),
      _SymbolIndex(0)
  {
//    printf("%s:%d:%s  name = %s\n", __FILE__, __LINE__, __FUNCTION__, name.c_str() );
  };

  size_t nameSize() {
    size_t strLen = this->_Name.size();
    return gctools::AlignUp(strLen+1);
  };
  size_t symbolBufferSize() { return gctools::AlignUp(this->_SymbolBuffer.size()); };
  size_t symbolInfoSize() { return this->_SymbolInfo.size()*sizeof(this->_SymbolInfo[0]); };
  size_t writeSize();

  
};

struct Fixup {
  FixupOperation_           _operation;
  uintptr_t                 _memoryStart;
  std::vector<ISLLibrary>   _libraries;
 
  Fixup( FixupOperation_ op ) : _operation(op) {};

  uintptr_t fixedAddress( bool functionP, uintptr_t* ptrptr, const char* addressName );
  
  size_t ensureLibraryRegistered(uintptr_t address);
  void registerVtablePointer(size_t libraryIndex, core::T_O* vtablePtrPtr) {
    this->_libraries[libraryIndex]._Pointers.emplace_back( VtablePointer, (uintptr_t*)vtablePtrPtr, *(uintptr_t*)vtablePtrPtr );
  };

  void registerFunctionPointer(size_t libraryIndex, uintptr_t* functionPtrPtr) {
    this->_libraries[libraryIndex]._Pointers.emplace_back( FunctionPointer, (uintptr_t*)functionPtrPtr, *functionPtrPtr );
  };

private:
  Fixup(const Fixup& fixup, uintptr_t memoryStart) : _memoryStart(memoryStart) {};
};



void snapshot_save(const std::string& filename);
int snapshot_load(void* maybeStartOfSnapshot, void* maybeEndOfSnapshot, const std::string& filename);


void clearLibraries();
void registerLibraryFunctionPointer(Fixup* fixup, uintptr_t* ptrptr);
void decodeLibrarySaveAddress(Fixup* fixup, uintptr_t* ptrptr);

void encodeEntryPoint(Fixup* fixup, uintptr_t* ptrptr, llvmo::CodeBase_sp code);
void decodeEntryPoint(Fixup* fixup, uintptr_t* ptrptr, llvmo::CodeBase_sp code);


struct SymbolLookup {
  uintptr_t  _adjustAddress;
  SymbolLookup() : _adjustAddress(0) {};
  std::map<std::string,uintptr_t> _symbolToAddress;
  std::map<uintptr_t,std::string> _addressToSymbol;
  uintptr_t lookupSymbol(const char* name) {
    std::string sname(name);
    auto it = this->_symbolToAddress.find(sname);
    if (it == this->_symbolToAddress.end()) {
#ifdef _TARGET_OS_DARWIN
      stringstream sname_underscore;
      sname_underscore << "_" << sname;
      it = this->_symbolToAddress.find(sname_underscore.str());
      if (it == this->_symbolToAddress.end()) {
        return 0;
      }
#else
      return 0;
#endif
    }
    return it->second+this->_adjustAddress;
  }
  
  bool lookupAddr(uintptr_t addr, std::string& name) {
//    printf("%s:%d:%s Lookup executable address: %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)addr );
    std::map<std::uintptr_t,std::string>::iterator it = this->_addressToSymbol.find(addr-this->_adjustAddress);
    if (it == this->_addressToSymbol.end()) {
      return false;
    }
    name = it->second;
    return true;
  }
  bool dladdr_(uintptr_t address, std::string& name, size_t& hitBadPointers, PointerType pointerType,uintptr_t& saddr) {
    Dl_info info;
    int ret = dladdr( (void*)address, &info );
    if ( ret == 0 ) {
      printf("%s:%d:%s During snapshot save the address %p could not be resolved using dladdr\n",
             __FILE__, __LINE__, __FUNCTION__,
             (void*)address);
      hitBadPointers++;
    } else if (info.dli_sname == NULL) {
      std::string lookupName;
      bool found = this->lookupAddr(address,lookupName);
      if (found) {
        name = lookupName;
        saddr = address;
        return true;
      } else {
        printf("%s:%d:%s During snapshot save the address %p could not be resolved to a symbol name using dladdr \n"
               "  When this happens run 'nm <executable> | grep %p'\n"
               "   Although it may not work - sorry\n"
               "   Then write a wrapper for that function - it's probably an inlined function \n"
               "       that dladdr doesn't like - I don't know any other way around this\n"
               "     The PointerType is %lu\n"
               "     The info.dli_fname -> %s\n"
               "     The info.dli_fbase -> %p\n"
               "     The info.dli_sname -> %p\n"
               "     The info.dli_saddr -> %p\n"
               "     The lookupName -> %s\n",
               __FILE__, __LINE__, __FUNCTION__,
               (void*)address, 
               (void*)address, 
               (uintptr_t)pointerType,
               info.dli_fname,
               (void*)info.dli_fbase,
               (void*)info.dli_sname,
               (void*)info.dli_saddr,
               lookupName.c_str());
        hitBadPointers++;
      }
    } else {
      name = std::string(info.dli_sname);
      saddr = (uintptr_t)info.dli_saddr;
      return true;
    }
    return false;
  }
};

bool loadExecutableSymbolLookup(SymbolLookup& symbolLookup, FILE* fout=NULL );

};


#endif // snapshotSaveLoad_H
