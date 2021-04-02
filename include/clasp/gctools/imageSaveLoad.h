/*
    File: imageSaveLoad.h
*/


#ifndef imageSaveLoad_H //[
#define imageSaveLoad_H

#include <clasp/core/common.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/gctools/imageSaveLoad.fwd.h>



namespace imageSaveLoad {


enum PointerType { EndPointer, FunctionPointer, VtablePointer };

struct PointerBase {
  PointerType _pointerType;
  uintptr_t*   _ptrptr;
  uintptr_t   _address;
  PointerBase(PointerType pt, uintptr_t* pp, uintptr_t a) : _pointerType(pt), _ptrptr(pp), _address(a) {};
};

struct GroupedPointer {
  uintptr_t   _address;
  GroupedPointer( uintptr_t address ) :
    _address(address)
  {};
};

struct RelocationTable {
  std::vector<GroupedPointer> _groupedPointers;
  std::vector<char>           _names;
  std::vector<uintptr_t>      _offsets;
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



void image_save(const std::string& filename);
int image_load(void* maybeStartOfImage, void* maybeEndOfImage, const std::string& filename);


void clearLibraries();
void registerLibraryFunctionPointer(Fixup* fixup, uintptr_t* ptrptr);
void decodeLibrarySaveAddress(Fixup* fixup, uintptr_t* ptrptr);

void encodeEntryPoint(Fixup* fixup, uintptr_t* ptrptr, llvmo::CodeBase_sp code);
void decodeEntryPoint(Fixup* fixup, uintptr_t* ptrptr, llvmo::CodeBase_sp code);

};


#endif // imageSaveLoad_H
