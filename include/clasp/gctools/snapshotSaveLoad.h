#pragma once

/*
    File: snapshotSaveLoad.h
*/

#include <dlfcn.h>
#include <clasp/core/common.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/gctools/snapshotSaveLoad.fwd.h>

namespace snapshotSaveLoad {

extern bool global_debugSnapshot;
extern bool global_InSnapshotLoad;

enum PointerType { UninitializedPointer = '?', EndPointer = 'E', FunctionPointer = 'f', VtablePointer = 'v' };

struct PointerBase {
  PointerType _pointerType;
  uintptr_t* _ptrptr;
  uintptr_t _address;
  PointerBase(PointerType pt, uintptr_t* pp, uintptr_t a) : _pointerType(pt), _ptrptr(pp), _address(a){};
};

struct GroupedPointer {
  PointerType _pointerType;
  uintptr_t _address;
  GroupedPointer() : _pointerType(UninitializedPointer), _address(0){};
  GroupedPointer(PointerType t, uintptr_t address) : _pointerType(t), _address(address){};
};

struct SymbolInfo {
  uintptr_t _Address;
  uint16_t _AddressOffset;
  uint16_t _SymbolLength;
  int _SymbolOffset;
  SymbolInfo() : _Address(0), _AddressOffset(0), _SymbolLength(0), _SymbolOffset(-1){};
  SymbolInfo(uintptr_t address, uint addressOffset, int length, int offset)
      : _Address(address), _AddressOffset(addressOffset), _SymbolLength(length), _SymbolOffset(offset){};
};

struct ISLLibrary {
  std::string _Name;
  bool _Executable;
  gctools::clasp_ptr_t _TextStart;
  gctools::clasp_ptr_t _TextEnd;
  uintptr_t _VtableStart;
  uintptr_t _VtableEnd;
  std::vector<char> _SymbolBuffer;
  size_t _SymbolIndex;
  std::vector<PointerBase> _InternalPointers;
  std::vector<GroupedPointer> _GroupedPointers;
  std::vector<SymbolInfo> _SymbolInfo;
  ISLLibrary(const std::string& name, bool executable, gctools::clasp_ptr_t start, gctools::clasp_ptr_t end, uintptr_t vtableStart,
             uintptr_t vtableEnd)
      : _Name(name), _Executable(executable), _TextStart(start), _TextEnd(end), _VtableStart(vtableStart), _VtableEnd(vtableEnd),
        _SymbolIndex(0){
            //    printf("%s:%d:%s  name = %s\n", __FILE__, __LINE__, __FUNCTION__, name.c_str() );
        };

  size_t nameSize() {
    size_t strLen = this->_Name.size();
    return gctools::AlignUp(strLen + 1);
  };
  size_t symbolBufferSize() { return gctools::AlignUp(this->_SymbolBuffer.size()); };
  size_t symbolInfoSize() { return this->_SymbolInfo.size() * sizeof(this->_SymbolInfo[0]); };
  size_t writeSize();
};

#define CODE_LIBRARY_ID 0xff
#define LIBRARY_ID_MAX CODE_LIBRARY_ID

struct Fixup {
  FixupOperation_ _operation;
  uintptr_t _memoryStart;
  std::vector<ISLLibrary> _ISLLibraries;
  bool _trackAddressName;
  map<void*, std::string> _addressName;

  Fixup(FixupOperation_ op) : _operation(op){};

  uintptr_t fixedAddress(bool functionP, uintptr_t* ptrptr, const char* addressName);
  size_t ensureLibraryRegistered(uintptr_t address);

  void registerVtablePointer(size_t libraryIndex, core::T_O* vtablePtrPtr);

  void registerFunctionPointer(size_t libraryIndex, uintptr_t* functionPtrPtr, const char* location);

  void addAddressName(void* address, std::string name) {
    if (this->_trackAddressName) {
      this->_addressName[address] = name;
    }
  }

  std::string lookupAddressName(void* address) {
    if (this->_trackAddressName) {
      auto it = this->_addressName.find(address);
      if (it == this->_addressName.end()) {
        return "no-address-name-map";
      }
      return it->second;
    }
    return "";
  }

private:
  Fixup(const Fixup& fixup, uintptr_t memoryStart) : _memoryStart(memoryStart), _trackAddressName(true){};
};

void snapshot_save(SaveLispAndDie& data);
void snapshot_load(void* maybeStartOfSnapshot, void* maybeEndOfSnapshot, const std::string& filename);

void clearLibraries();
void encodeEntryPointInLibrary(Fixup* fixup, uintptr_t* ptrptr);
void decodeEntryPointInLibrary(Fixup* fixup, uintptr_t* ptrptr);

void encodeEntryPoint(Fixup* fixup, uintptr_t* ptrptr, core::T_sp code, core::FunctionDescription_sp functionDescription );
void decodeEntryPoint(Fixup* fixup, uintptr_t* ptrptr, core::T_sp code);

struct LibraryLookup {
  uintptr_t _loadAddress;
  std::string _libraryPath;

  LibraryLookup(const std::string libraryPath) : _loadAddress(0), _libraryPath(libraryPath){};
  std::map<std::string, uintptr_t> _symbolToAddress;
  std::map<uintptr_t, std::string> _addressToSymbol;

  bool lookupSymbol(const char* name, uintptr_t& address, bool verbose = false) {
    std::string sname(name);
    auto it = this->_symbolToAddress.find(sname);
    if (it == this->_symbolToAddress.end()) {
#ifdef _TARGET_OS_DARWIN
      stringstream sname_underscore;
      sname_underscore << "_" << sname;
      it = this->_symbolToAddress.find(sname_underscore.str());
      if (it == this->_symbolToAddress.end()) {
        address = 0;
        return false;
      }
#else
      address = 0;
      if (verbose)
        printf("%s:%d:%s Could not find symbol %s\n", __FILE__, __LINE__, __FUNCTION__, name);
      return false;
#endif
    }
    address = it->second + this->_loadAddress;
    if (verbose)
      printf("%s:%d:%s Found symbol %s rel-address: %p abs-address: %p loadAddress: %p\n", __FILE__, __LINE__, __FUNCTION__, name,
             (void*)it->second, (void*)address, (void*)this->_loadAddress);
    return true;
  }

  bool lookupAddr(uintptr_t addr, std::string& name) {
    //    printf("%s:%d:%s Lookup executable address: %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)addr );
    std::map<std::uintptr_t, std::string>::iterator it = this->_addressToSymbol.find(addr - this->_loadAddress);
    if (it == this->_addressToSymbol.end()) {
      return false;
    }
    name = it->second;
    return true;
  }
  bool dladdr_(Fixup* fixup, uintptr_t address, std::string& name, size_t& hitBadPointers, PointerType pointerType,
               uintptr_t& saddr) {
    Dl_info info;
    int ret = dladdr((void*)address, &info);
    if (ret == 0) {
      printf("%s:%d:%s During snapshot save the address %p could not be resolved using dladdr\n", __FILE__, __LINE__, __FUNCTION__,
             (void*)address);
      hitBadPointers++;
    } else if (info.dli_sname == NULL) {
      std::string lookupName;
      bool found = this->lookupAddr(address, lookupName);
      if (found) {
        name = lookupName;
        saddr = address;
        return true;
      }
    } else {
      name = std::string(info.dli_sname);
      saddr = (uintptr_t)info.dli_saddr;
      return true;
    }
    return false;
  }
};

struct SymbolLookup {
  std::vector<LibraryLookup*> _Libraries;

  SymbolLookup(){};
  ~SymbolLookup() {
    for (auto ii : this->_Libraries) {
      delete ii;
    }
  }

  bool addLibrary(const std::string& libraryPath, FILE* fout = NULL);

  void addAllLibraries(FILE* fout = NULL);

  uintptr_t lookupSymbol(const std::string& name, bool verbose = false) {
    //    printf("%s:%d:%s Looking to find symbol %s\n", __FILE__, __LINE__, __FUNCTION__, name.c_str() );
    uintptr_t address;
    for (auto ii : this->_Libraries) {
      if (ii->lookupSymbol(name.c_str(), address, verbose)) {
        return address;
      }
    }
    //    printf("%s:%d:%s Could not find symbol %s\n", __FILE__, __LINE__, __FUNCTION__, name.c_str() );
    return 0;
  }

  bool lookupAddr(uintptr_t addr, std::string& name) {
    for (auto ii : this->_Libraries) {
      if (ii->lookupAddr(addr, name)) {
        return true;
      }
    }
    return false;
  }

  bool dladdr_(Fixup* fixup, uintptr_t address, std::string& name, size_t& hitBadPointers, PointerType pointerType,
               uintptr_t& saddr) {
    Dl_info info;
    int ret = dladdr((void*)address, &info);
    if (ret == 0) {
      printf("%s:%d:%s During snapshot save the address %p could not be resolved using dladdr\n", __FILE__, __LINE__, __FUNCTION__,
             (void*)address);
      hitBadPointers++;
    } else if (info.dli_sname != NULL) {
      name = std::string(info.dli_sname);
      saddr = (uintptr_t)info.dli_saddr;
      return true;
    } else if (info.dli_sname == NULL) {
      std::string lookupName;
      for (auto lib : this->_Libraries) {
        if (lib->lookupAddr(address, lookupName)) {
          name = lookupName;
          saddr = address;
          return true;
        }
      }
      std::string fixupName = fixup->lookupAddressName((void*)address);
      printf("%s:%d:%s During snapshot save the absolute address %p \n"
             " could not be resolved to a symbol name using dladdr \n"
             "  Use the clasp --addresses <filename> option to dump the symbols that clasp uses for its dladdr\n"
             "   When this happens it's captureless lambdas that have been a problem - they have __invoke in their mangled\n"
             "   symbol names.   If you see this problem on one OS but not another - look at the one that works to find symbols\n"
             "   that are defined and not defined on the OS that doesn't work.\n"
             "  Clasp runs 'nm --defined-only <executable>' to get symbol addresses/name mappings\n"
             "   This can also happen when there is an inlined function that needs a wrapper.  Then write a wrapper for that "
             "function.\n"
             "     The fixup name is %s\n"
             "     The PointerType is %" PRIuPTR "\n"
             "     The info.dli_fname -> %s\n"
             "     The info.dli_fbase -> %p\n"
             "     The info.dli_sname -> %p\n"
             "     The info.dli_saddr -> %p\n"
             "     The lookupName -> %s\n",
             __FILE__, __LINE__, __FUNCTION__, (void*)address, fixupName.c_str(), (uintptr_t)pointerType, info.dli_fname,
             (void*)info.dli_fbase, (void*)info.dli_sname, (void*)info.dli_saddr, lookupName.c_str());
      hitBadPointers++;
    }
    return false;
  }
};

bool loadLibrarySymbolLookup(const std::string& libraryPath, LibraryLookup& libraryLookup, FILE* fout = NULL);

}; // namespace snapshotSaveLoad
