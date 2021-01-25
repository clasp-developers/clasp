/*
    File: llvmoExpose.h
*/


// #define USE_JITLINKER 1


#ifndef imageSaveLoad_H //[
#define imageSaveLoad_H

#include <clasp/core/common.h>
#include <clasp/llvmo/llvmoExpose.h>

template <>
struct gctools::GCInfo<llvmo::ObjectFile_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};



// ObjectFile_O
namespace llvmo {
  FORWARD(Code);
FORWARD(ObjectFile);
class ObjectFile_O : public core::CxxObject_O {
  LISP_CLASS(llvmo, LlvmoPkg, ObjectFile_O, "ObjectFile", core::CxxObject_O);
public:
  void*         _Start;
  size_t        _Size;
  size_t        _StartupID;
  JITDylib_sp   _JITDylib;
  std::string   _FasoName;
  size_t        _FasoIndex;
  void*         _text_segment_start;
  size_t        _text_segment_size;
  size_t        _text_segment_SectionID;
  void*         _stackmap_start;
  size_t        _stackmap_size;
  Code_sp       _Code;
  gctools::GCRootsInModule* _GCRootsInModule;
public:
  static ObjectFile_sp create(void* start, size_t size, size_t startupID, JITDylib_sp jitdylib, const std::string& fasoName, size_t fasoIndex);
 ObjectFile_O(void* start, size_t size, size_t startupID, JITDylib_sp jitdylib, const std::string& fasoName, size_t fasoIndex) : _Start(start), _Size(size), _StartupID(startupID), _JITDylib(jitdylib), _FasoName(fasoName), _FasoIndex(fasoIndex), _Code(_Unbound<Code_O>()) {};
  ~ObjectFile_O();
}; // ObjectFile_O class def
}; // llvmo
/* from_object translators */

#if 0
namespace translate {
template <>
struct from_object<llvm::object::ObjectFile *, std::true_type> {
  typedef llvm::object::ObjectFile *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::ObjectFile_sp>(object)->wrappedPtr()){};
};

};

/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::object::ObjectFile *> {
  static core::T_sp convert(llvm::object::ObjectFile *ptr) {
    return core::RP_Create_wrapped<llvmo::ObjectFile_O, llvm::object::ObjectFile *>(ptr);
  }
};
}; // namespace llvmo - ObjectFile_O done
#endif


namespace llvmo {


  /* Code_O
   * This object contains all of the code and data generated by relocating an object file.
   * The data and code is stored in _DataCode.
   * The layout is | RWData | ROData | Code 
   * We place the RWData at the top of the object so we can scan it for GC managed pointers.
   */
FORWARD(Code);
class Code_O : public core::CxxObject_O {
  LISP_CLASS(llvmo, LlvmoPkg, Code_O, "Code", core::CxxObject_O);
 public:
  static Code_sp make(uintptr_t CodeSize,
                      uint32_t CodeAlign,
                      uintptr_t RODataSize,
                      uint32_t RODataAlign,
                      uintptr_t RWDataSize,
                      uint32_t RWDataAlign);
 public:
  typedef uint8_t value_type;
 public:
  // Store the allocation sizes and alignments
  uintptr_t     _CodeSize;
  uint32_t      _CodeAlign;
  uintptr_t     _RODataSize;
  uint32_t      _RODataAlign;
  uintptr_t     _RWDataSize;
  uint32_t      _RWDataAlign;
  // Keep track of the Head and Tail indices of the memory in _Data;
  uintptr_t     _HeadOffset;
  uintptr_t     _TailOffset;
  ObjectFile_sp _ObjectFile;
  gctools::GCArray_moveable<uint8_t> _DataCode;

  void* allocateHead(uintptr_t size, uint32_t align);
  void* allocateTail(uintptr_t size, uint32_t align);

 Code_O( uintptr_t TotalSize,
         uintptr_t CodeSize,
         uint32_t CodeAlign,
         uintptr_t RODataSize,
         uint32_t RODataAlign,
         uintptr_t RWDataSize,
         uint32_t RWDataAlign) :
  _CodeSize(CodeSize)
    , _CodeAlign(CodeAlign)
    , _RODataSize(RODataSize)
    , _RODataAlign(RODataAlign)
    , _RWDataSize(RWDataSize)
    , _RWDataAlign(RWDataAlign)
    , _HeadOffset(0)
    , _TailOffset(TotalSize)
    , _ObjectFile(_Unbound<ObjectFile_O>())
    , _DataCode(TotalSize,0,true) {};
           
};
  
};



namespace llvmo {

  class ClaspSectionMemoryManager : public SectionMemoryManager {
    bool needsToReserveAllocationSpace() { return true; };
    void reserveAllocationSpace(uintptr_t CodeSize,
                                uint32_t CodeAlign,
                                uintptr_t RODataSize,
                                uint32_t RODataAlign,
                                uintptr_t RWDataSize,
                                uint32_t RWDataAlign);
    uint8_t* allocateCodeSection( uintptr_t Size, unsigned Alignment,
                                  unsigned SectionID,
                                  StringRef SectionName );
    uint8_t* allocateDataSection( uintptr_t Size, unsigned Alignment,
                                  unsigned SectionID,
                                  StringRef SectionName,
                                  bool isReadOnly);
    void 	notifyObjectLoaded (RuntimeDyld &RTDyld, const object::ObjectFile &Obj);
    bool finalizeMemory(std::string* ErrMsg = nullptr);
  public:
    uint8_t*      _CodeStart;
    size_t        _CodeSize;
  };

};

#endif // imageSaveLoad_H

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