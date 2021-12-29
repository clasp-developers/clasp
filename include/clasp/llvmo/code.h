/*
    File: code.h
*/


// #define USE_JITLINKER 1


#ifndef code_H //[
#define code_H

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

typedef enum { SaveState, RunState } CodeState_t;

  FORWARD(LibraryBase);
  class LibraryBase_O : public core::CxxObject_O {
    LISP_CLASS(llvmo, LlvmoPkg, LibraryBase_O, "LibraryBase", core::CxxObject_O);
  public:
    CLASP_DEFAULT_CTOR LibraryBase_O() {};
  public:
  };

  

  FORWARD(CodeBlock);
  FORWARD(ObjectFile);
  class ObjectFile_O : public LibraryBase_O {
    LISP_CLASS(llvmo, LlvmoPkg, ObjectFile_O, "ObjectFile", LibraryBase_O);
  public:
    typedef uint8_t value_type;
    CodeState_t    _State;
    core::SimpleBaseString_sp _CodeName;
    std::unique_ptr<llvm::MemoryBuffer> _MemoryBuffer;
    uintptr_t      _ObjectFileOffset; // Only has meaning when _State is SaveState
    uintptr_t      _ObjectFileSize; // Only has meaning when _State is SaveState
    size_t         _Size;
    size_t         _ObjectId;
    JITDylib_sp    _TheJITDylib;
    core::SimpleBaseString_sp _FasoName;
    size_t         _FasoIndex;
    //
    // Code data
    //
    gctools::GCRootsInModule* _gcroots;
    void*         _TextSectionStart;
    void*         _TextSectionEnd;
    uintptr_t     _TextSectionId;
    void*         _StackmapStart;
    uintptr_t     _StackmapSize;
// Absolute address of literals in memory - this must be in the _DataCode vector.
    uintptr_t     _LiteralVectorStart;
    size_t        _LiteralVectorSizeBytes; // size in bytes
    CodeBlock_sp  _CodeBlock;
    gctools::GCArray_moveable<uint8_t> _DataCode;
    
  public:
    ObjectFile_O( core::SimpleBaseString_sp codename, std::unique_ptr<llvm::MemoryBuffer> buffer, size_t objectId, JITDylib_sp jitdylib, core::SimpleBaseString_sp fasoName, size_t fasoIndex) :
        _State(RunState),
        _CodeName(codename),
        _MemoryBuffer(std::move(buffer)),
        _ObjectId(objectId),
        _TheJITDylib(jitdylib),
        _FasoName(fasoName),
        _FasoIndex(fasoIndex),
        _gcroots(NULL),
        _CodeBlock(unbound<CodeBlock_O>() ) {
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s   objectId = %lu\n", __FILE__, __LINE__, __FUNCTION__, objectId));
    };
    ObjectFile_O( core::SimpleBaseString_sp codename, JITDylib_sp jitdylib, size_t objectId ) :
        _State(RunState),
        _CodeName(codename),
        _TheJITDylib(jitdylib),
        _gcroots(NULL),
        _CodeBlock(unbound<CodeBlock_O>()),
        _ObjectId(objectId) {
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s   codename = %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(codename).c_str() ));
    };
    ObjectFile_O( core::SimpleBaseString_sp codename, CodeBlock_sp codeBlock, JITDylib_sp dylib, size_t objectId) :
        _State(RunState),
        _CodeName(codename),
        _TheJITDylib(dylib),
        _gcroots(NULL),
        _CodeBlock(codeBlock),
        _TextSectionStart(0),
        _TextSectionEnd(0),
        _ObjectId(objectId)
    {
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s created with CodeBlock_sp   codename = %s CodeBlock = %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(codename).c_str(), core::_rep_(codeBlock).c_str() ));
    };
    static ObjectFile_sp createForModule( const std::string& codename, JITDylib_sp jitdylib, size_t objectId );
    static ObjectFile_sp createForObjectFile( const std::string& codename, JITDylib_sp jitdylib, size_t objectId );
    static ObjectFile_sp create(const std::string& name, std::unique_ptr<llvm::MemoryBuffer> buffer, size_t objectId, JITDylib_sp jitdylib, const std::string& fasoName, size_t fasoIndex);
    static size_t sizeofInState(ObjectFile_O* code, CodeState_t state);
  public:
    ~ObjectFile_O();
    std::string __repr__() const;
    static void writeToFile(const std::string& filename, const char* start, size_t size);
    size_t objectFileSize() { return this->_MemoryBuffer->getBufferSize(); };
    void* objectFileData() { return (void*)this->_MemoryBuffer->getBufferStart(); };
    size_t objectFileSizeAlignedUp() {
      return gctools::AlignUp(this->objectFileSize());
    }
    llvm::Expected<std::unique_ptr<llvm::object::ObjectFile>> getObjectFile();

    //
    // Code methods
    //
    uintptr_t codeStart() const { return (uintptr_t)this->_TextSectionStart; };
    uintptr_t codeEnd() const { return (uintptr_t)this->_TextSectionEnd; };
    void* absoluteAddress(SectionedAddress_sp sa);
    size_t frontSize() const { return sizeof(*this); };
    size_t literalsSize() const { return this->_LiteralVectorSizeBytes; };
  // The location of the literals vector in memory
    void* literalsStart() const;
    core::T_O** TOLiteralsStart() const { return (core::T_O**)literalsStart(); }
    size_t TOLiteralsSize() const { return literalsSize()/sizeof(core::T_O*); }
    virtual std::string filename() const;
    core::T_sp codeLineTable() const;
    virtual void validateEntryPoint(const core::ClaspXepFunction& entry_point);
    
  }; // ObjectFile_O class def
}; // llvmo

namespace llvmo {
  FORWARD(ObjectFile);
  FORWARD(LibraryFile);
  class LibraryFile_O : public LibraryBase_O {
    LISP_CLASS(llvmo, LlvmoPkg, LibraryFile_O, "LibraryFile", LibraryBase_O);
  public:
    LibraryFile_O(core::SimpleBaseString_sp name) : _Library(name) {};
  public:
    core::SimpleBaseString_sp       _Library;
  public:
    static LibraryFile_sp createLibrary(const std::string& libraryName);
  };
};

namespace llvmo {
  class CodeBase_O;
  FORWARD(CodeBase);
  class CodeBase_O : public core::CxxObject_O {
    LISP_CLASS(llvmo, LlvmoPkg, CodeBase_O, "CodeBase", core::CxxObject_O);
  public:
    CLASP_DEFAULT_CTOR CodeBase_O() {};
  public:
    virtual uintptr_t codeStart() const = 0;
    virtual uintptr_t codeEnd() const = 0;
    virtual std::string filename() const = 0;
    virtual void validateEntryPoint(const core::ClaspXepFunction& entry_point) {};
    virtual void validateEntryPoint(const core::ClaspLocalFunction& entry_point) {};
  };
 
};


namespace llvmo {


bool general_entry_point_redirect_p(void* ep);

};

template <>
struct gctools::GCInfo<llvmo::CodeBlock_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = collectable_immobile;
};

namespace llvmo {


  /* CodeBlock_O
   * This object contains all of the code and data generated by relocating multiple object files.
   * The Code_O objects are stored in _DataCode
   * The layout is | RWData | ROData | Code 
   * We place the RWData at the top of the object so we can scan it for GC managed pointers.
   */
FORWARD(CodeBlock);
class CodeBlock_O : public core::CxxObject_O {
  LISP_CLASS(llvmo, LlvmoPkg, CodeBlock_O, "CodeBlock", core::CxxObject_O);
public:
  typedef uint8_t value_type;
public:
  // Store the allocation sizes and alignments
  // Keep track of the Head and Tail indices of the memory in _Data;
  uintptr_t     _HeadOffset;
  uintptr_t     _TailOffset;
  uintptr_t     _TotalSize;
  gctools::GCArray_moveable<uint8_t> _DataCode;
  static constexpr size_t DefaultSize = 8*1024*1024;
public:
  template <typename Stage>
  static CodeBlock_sp make(uintptr_t size) {
    CodeBlock_sp codeblock = gctools::GC<CodeBlock_O>::allocate_container<Stage>(false,size);
    uintptr_t top = (uintptr_t)&codeblock->_DataCode[0];
    uintptr_t bottom = (uintptr_t)&codeblock->_DataCode[size];
    size_t PageSize = getpagesize();
    bottom = (bottom/PageSize)*PageSize;
    codeblock->_TailOffset = bottom-top;
    codeblock->_HeadOffset = 0;
    return codeblock;
  };
public:
  /*! Calculate the BasicLayout based on the sizes. 
   *  Return false if it won't fit and true if it will and then lock in the allocation
   */
  void* dataStart() const { return (void*)&this->_DataCode[0]; };
  void* dataEnd() const { return (void*)&this->_DataCode[this->_HeadOffset]; };
  bool calculate(llvm::jitlink::BasicLayout& BL);
  void* calculateHead( uintptr_t size, uint32_t align, uintptr_t& headOffset );
  void* calculateTail( uintptr_t size, uint32_t align, uintptr_t& tailOffset );
  void describe() const;

  std::string __repr__() const;
  
  CodeBlock_O(uintptr_t totalSize ) :
      _HeadOffset(0)
      , _TailOffset(totalSize)
      , _DataCode(totalSize,0,true) {};

  ~CodeBlock_O();
};

};



template <>
struct gctools::GCInfo<llvmo::Library_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace llvmo {
  FORWARD(Library);
class Library_O : public CodeBase_O {
  LISP_CLASS(llvmo, LlvmoPkg, Library_O, "Library", CodeBase_O);
 public:
  bool _Executable;
  gctools::clasp_ptr_t   _Start;
  gctools::clasp_ptr_t   _End;
  uintptr_t _VtableStart;
  uintptr_t _VtableEnd;
  core::SimpleBaseString_sp _Name;
 public:
  Library_O(bool executable, gctools::clasp_ptr_t start, gctools::clasp_ptr_t end, uintptr_t vtableStart, uintptr_t vtableEnd) : _Executable(executable), _Start(start), _End(end), _VtableStart(vtableStart), _VtableEnd(vtableEnd) {};
  static Library_sp make(bool executable, gctools::clasp_ptr_t start, gctools::clasp_ptr_t end, uintptr_t vtableStart, uintptr_t vtableEnd, const std::string& name );
  std::string __repr__() const;
  uintptr_t codeStart() const { return (uintptr_t)this->_Start; };
  uintptr_t codeEnd() const { return (uintptr_t)this->_End; };
  std::string filename() const;
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




namespace llvmo {
using namespace llvm;
using namespace llvm::jitlink;


//void dumpObjectFile(const char* start, size_t size, void* codeStart = NULL );

};

namespace llvmo {
core::T_mv object_file_for_instruction_pointer(void* instruction_pointer, bool verbose);
core::T_sp only_object_file_for_instruction_pointer(void*);
SectionedAddress_sp object_file_sectioned_address(void*, ObjectFile_sp, bool);

size_t number_of_object_files();

size_t total_memory_allocated_for_object_files();


};

namespace llvmo {

/*! Guaranteed allocation of the BasicLayout within the current CodeBlock or create a new one.
 */
template <typename Stage>
inline void allocateInCodeBlock( BasicLayout& BL, CodeBlock_sp& codeBlock ) {
  WITH_READ_WRITE_LOCK(globals_->_CodeBlocksMutex);
  size_t PageSize = getpagesize();
  bool newCodeBlock = false;
  auto SegsSizes = BL.getContiguousPageBasedLayoutSizes(PageSize);
  if (_lisp->_Roots._AllCodeBlocks.load().consp()) {
    core::Cons_sp ll = gc::As_unsafe<core::Cons_sp>(_lisp->_Roots._AllCodeBlocks.load());
    codeBlock = gc::As<CodeBlock_sp>(CONS_CAR(ll));
  } else {
    size_t size = std::max((size_t)CodeBlock_O::DefaultSize, (size_t)SegsSizes->total());
    codeBlock = CodeBlock_O::make<Stage>(size);
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Created first CodeBlock size: %lu\n", __FILE__, __LINE__, __FUNCTION__, size ));
    _lisp->_Roots._AllCodeBlocks.store(core::Cons_O::createAtStage<Stage>(codeBlock,_lisp->_Roots._AllCodeBlocks.load()));
  }
  bool fits = codeBlock->calculate(BL);
  if (!fits) {
    size_t size = std::max((size_t)CodeBlock_O::DefaultSize, (size_t)SegsSizes->total());
    codeBlock = CodeBlock_O::make<Stage>(size);
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Created a fresh CodeBlock size: %lu\n", __FILE__, __LINE__, __FUNCTION__, size ));
    _lisp->_Roots._AllCodeBlocks.store(core::Cons_O::createAtStage<Stage>(codeBlock,_lisp->_Roots._AllCodeBlocks.load()));
    fits = codeBlock->calculate(BL);
    if (!fits) {
      SIMPLE_ERROR(BF("Could not allocate enough space for code %lu") % size );
    }
  }
  //
  // Set memory permissions to RWX temporarily
  //
  auto rwxProt = llvm::sys::Memory::MF_READ | llvm::sys::Memory::MF_WRITE | llvm::sys::Memory::MF_EXEC;
  for (auto &KV : BL.segments()) {
    const auto &AG = KV.first;
    auto &Seg = KV.second;
    uint64_t SegSize =
        alignTo(Seg.ContentSize + Seg.ZeroFillSize, PageSize );
    sys::MemoryBlock MB(Seg.WorkingMem, SegSize);
    sys::Memory::protectMappedMemory( MB, rwxProt );
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Temporarily Applying Protections (RWX) to range %p - %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)Seg.WorkingMem, (void*)(Seg.WorkingMem+SegSize) ));
  }
};

};

namespace llvmo {

};


namespace llvmo {
core::T_sp identify_code_or_library(gctools::clasp_ptr_t entry_point);



size_t countObjectFileNames(const std::string& name);

std::string createIRModuleObjectFileName(size_t startupId, std::string& prefix);
bool verifyIRModuleObjectFileStartupSymbol(const std::string& name);

ObjectFile_sp lookupObjectFile(const std::string& name );

bool lookupObjectFileFromEntryPoint( uintptr_t entry_point, ObjectFile_sp& objectFile );

void validateEntryPoint(core::T_sp code, uintptr_t entry_point );
void validateEntryPoint(core::T_sp code, const core::ClaspXepFunction& entry_point );
void validateEntryPoint(core::T_sp code, const core::ClaspLocalFunction& entry_point );

uintptr_t codeStart(core::T_sp codeOrLibrary);

template <typename Stage = gctools::RuntimeStage>
void registerObjectFile(ObjectFile_sp ofi)
{
  std::string name = ofi->_CodeName->get_std_string();
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Adding to _lisp->_Roots._AllObjectFiles  %p ofi->_CodeName = \"%s\"\n", __FILE__, __LINE__, __FUNCTION__, (void*)ofi.raw_(), name.c_str()));
  if (ofi->_CodeName->length()==0) {
    printf("%s:%d:%s Got zero length ObjectFile code name\n", __FILE__, __LINE__, __FUNCTION__ );
  }
  size_t count = countObjectFileNames(name);
  if (count>0) {
    printf("%s:%d:%s The object-file name %s is present %lu times\n", __FILE__, __LINE__, __FUNCTION__, name.c_str(), count );
  }
  core::T_sp expected;
  core::Cons_sp entry = core::Cons_O::createAtStage<Stage>(ofi,nil<core::T_O>());
//  printf("%s:%d:%s Registering object file with name %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(ofi->_CodeName).c_str());
  do {
    expected = _lisp->_Roots._AllObjectFiles.load();
    entry->rplacd(expected);
  } while (!_lisp->_Roots._AllObjectFiles.compare_exchange_weak(expected,entry));
}


};

#endif // code_H

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
