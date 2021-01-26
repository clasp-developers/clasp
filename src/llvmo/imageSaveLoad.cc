/*
    File: imageSaveLoad.cc

*/



#define DEBUG_LEVEL_FULL

//#include <llvm/Support/system_error.h>
#include <dlfcn.h>
#include <iomanip>
#include <clasp/core/foundation.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/debugger.h>
#include <clasp/llvmo/imageSaveLoad.h>


namespace llvmo { // ObjectFile_O

ObjectFile_sp ObjectFile_O::create(void* start, size_t size, size_t startupID, JITDylib_sp jitdylib, const std::string& fasoName, size_t fasoIndex)
{
  GC_ALLOCATE_VARIADIC(ObjectFile_O,of,start,size,startupID,jitdylib,fasoName,fasoIndex);
  DEBUG_OBJECT_FILES(("%s:%d:%s Creating ObjectFile_O start=%p size= %lu\n", __FILE__, __LINE__, __FUNCTION__, start, size ));
  return of;
}


ObjectFile_O::~ObjectFile_O() {
  printf("%s:%d dtor for ObjectFile_O %p\n", __FILE__, __LINE__, (void*)this );
  printf("%s:%d       GCRootsInModule -> %p\n", __FILE__, __LINE__, this->_GCRootsInModule );
}

}; // namespace llvmo, ObjectFile_O



template <>
struct gctools::GCInfo<llvmo::Code_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = collectable_immobile;
};


namespace llvmo {


Code_sp Code_O::make(uintptr_t scanSize, uintptr_t totalSize) {
  Code_sp code = gctools::GC<Code_O>::allocate_container_partial_scan(scanSize, totalSize);
  printf("%s:%d:%s  dataScanSize = %lu  totalSize = %lu\n", __FILE__, __LINE__, __FUNCTION__, scanSize, totalSize );
  printf("%s:%d:%s  Code_O start = %p  end = %p\n", __FILE__, __LINE__, __FUNCTION__, &*code,&code->_DataCode[totalSize]);
  return code;
}

void* Code_O::allocateHead(uintptr_t size, uint32_t align) {
  const unsigned char* head = this->_DataCode.data()+this->_HeadOffset+size;
  head = (const unsigned char*)gctools::AlignUp((uintptr_t)head,align);
  uintptr_t headOffset = (uintptr_t)head-(uintptr_t)this->_DataCode.data();
  if (headOffset > this->_TailOffset) {
    SIMPLE_ERROR(BF("There is not enough memory in the Code_O object - current size: %lu and we are over by %lu\n") % this->_DataCode.size() % (headOffset-this->_TailOffset));
  }
  intptr_t delta = headOffset-this->_HeadOffset;
  if (delta<size) {
    printf("%s:%d Bad allocation\n", __FILE__, __LINE__ );
    abort();
  }
  const unsigned char* tail = this->_DataCode.data()+this->_TailOffset;
  printf("%s:%d   head %p   tail %p\n", __FILE__, __LINE__, head, tail );
  return (void*)head;
}

void* Code_O::allocateTail(uintptr_t size, uint32_t align) {
  const unsigned char* tail = this->_DataCode.data()+this->_TailOffset-size;
  tail = (const unsigned char*)gctools::AlignDown((uintptr_t)tail,align);
  uintptr_t tailOffset = (uintptr_t)tail-(uintptr_t)this->_DataCode.data();
  if (this->_HeadOffset > tailOffset) {
    SIMPLE_ERROR(BF("There is not enough memory in the Code_O object - current size: %lu and we are over by %lu\n") % this->_DataCode.size() % (this->_HeadOffset-tailOffset));
  }
  intptr_t delta = tailOffset-this->_TailOffset;
  if (delta<size) {
    printf("%s:%d Bad allocation\n", __FILE__, __LINE__ );
    abort();
  }
  this->_TailOffset = tailOffset;
  const unsigned char* head = this->_DataCode.data()+this->_HeadOffset;
  printf("%s:%d   head %p   tail %p\n", __FILE__, __LINE__, head, tail );
  return (void*)tail;
}



};





namespace llvmo {
std::atomic<size_t> fileNum;


void dumpObjectFile(size_t num, const char* start, size_t size) {
  std::stringstream filename;
  filename << "object-file-" << num << ".o";
  std::ofstream fout;
  fout.open(filename.str(), std::ios::out | std::ios::binary );
  fout.write(start,size);
  fout.close();
}

};

// #define USE_CODE_O 1

#if 0

namespace llvmo {
#ifdef _TARGET_OS_DARWIN    
#define STACKMAPS_NAME "__llvm_stackmaps"
#elif defined(_TARGET_OS_LINUX)
#define STACKMAPS_NAME ".llvm_stackmaps"
#elif defined(_TARGET_OS_FREEBSD)
#define STACKMAPS_NAME ".llvm_stackmaps"
#else
#error "What is the name of stackmaps section on this OS??? __llvm_stackmaps or .llvm_stackmaps"
#endif
uint8_t* ClaspSectionMemoryManager::allocateDataSection( uintptr_t Size, unsigned Alignment,
                                                         unsigned SectionID,
                                                         StringRef SectionName,
                                                         bool isReadOnly) {
#ifdef USE_CODE_O
  uint8_t* ptr;
  if (isReadOnly) {
    ptr = (uint8_t*)my_thread->_Code->allocateTail(Size,Alignment);
  } else {
    ptr = (uint8_t*)my_thread->_Code->allocateHead(Size,Alignment);
  }
#else
  uint8_t* ptr = this->SectionMemoryManager::allocateDataSection(Size,Alignment,SectionID,SectionName,isReadOnly);
#endif
    if (SectionName.str() == STACKMAPS_NAME) {
      my_thread->_stackmap = (uintptr_t)ptr;
      my_thread->_stackmap_size = (size_t)Size;
#if 0
      printf("%s:%d recorded __llvm_stackmap allocateDataSection Size: %lu  Alignment: %u SectionId: %u SectionName: %s isReadOnly: %d --> allocated at: %p\n" ,
             __FILE__, __LINE__,
             Size , Alignment, SectionID, SectionName.str().c_str(), isReadOnly, (void*)ptr );
#endif
      LOG(BF("STACKMAP_LOG  recorded __llvm_stackmap allocateDataSection Size: %lu  Alignment: %u SectionId: %u SectionName: %s isReadOnly: %d --> allocated at: %p\n") %
          Size% Alignment% SectionID% SectionName.str().c_str() % isReadOnly% (void*)ptr);
    }
    if (llvmo::_sym_STARdebugObjectFilesSTAR->symbolValue().notnilp()) {
      core::write_bf_stream(BF("%s:%d  allocateDataSection Size: %lu  Alignment: %u SectionId: %u SectionName: %s isReadOnly: %d --> allocated at: %p\n") % __FILE__% __LINE__% Size% Alignment% SectionID% SectionName.str() % isReadOnly% (void*)ptr );
    }
    return ptr;
  }

void 	ClaspSectionMemoryManager::notifyObjectLoaded (RuntimeDyld &RTDyld, const object::ObjectFile &Obj) {
  if (llvmo::_sym_STARdebugObjectFilesSTAR->symbolValue().notnilp()) {
    printf("%s:%d:%s entered\n", __FILE__, __LINE__, __FUNCTION__ );
  }
#if 0
      // DONT DELETE DONT DELETE DONT DELETE
      // This is trying to use the gdb jit interface described here.
      // https://v8.dev/docs/gdb-jit
      // https://llvm.org/docs/DebuggingJITedCode.html
      // https://doc.ecoscentric.com/gnutools/doc/gdb/JIT-Interface.html
      // Example: https://sourceware.org/git/gitweb.cgi?p=binutils-gdb.git;a=blob;f=gdb/testsuite/gdb.base/jit-main.c
      // Simple example: https://stackoverflow.com/questions/20046943/gdb-jit-interface-simpliest-example
      //
      // What I don't like about this is that the ObjectFile is going to be destroyed by the caller
      // and so I make a copy of the ObjectFile here.
      //
  {
    llvm::MemoryBufferRef mem = Obj.getMemoryBufferRef();
#if 1
          // Copy the ObjectFile - I can't be sure that it will persist once this callback returns
    void* obj_file_copy = (void*)malloc(mem.getBufferSize());
    memcpy( (void*)obj_file_copy,mem.getBufferStart(),mem.getBufferSize());
    register_object_file_with_gdb(obj_file_copy,mem.getBufferSize());
#else
          // Try using the ObjectFile directly - see the comment above about it persisting
    register_object_file_with_gdb((void*)mem.getBufferStart(),mem.getBufferSize());
#endif
  }
#endif
#if 0
  uintptr_t stackmap = 0;
  size_t stackmap_size = 0;
  for ( auto section : Obj.sections() ) {
    llvm::StringRef name;
    section.getName(name);
//      printf("%s:%d name: %s\n", __FILE__, __LINE__, name.str().c_str());
    if (name=="__llvm_stackmaps") {
      auto reloc = section.getRelocatedSection();
      stackmap = (uintptr_t)reloc->getAddress();
      stackmap_size = (size_t)reloc->getSize();
      printf("%s:%d Found stackmap at %p size: %lu\n", __FILE__, __LINE__, (void*) stackmap, stackmap_size);
    }
  }
  unsigned long section_size = 0;
  void* p_section = NULL;
#else
  unsigned long section_size = 0;
  void* p_section = NULL;
#if 0
  if (my_thread->_stackmap>0) {
    p_section = reinterpret_cast<void*>(my_thread->_stackmap);
    section_size = my_thread->_stackmap_size;
    my_thread->_stackmap = 0;
  }
#endif
#endif
  if (p_section!=nullptr) {
    printf("%s:%d LLVM_STACKMAPS  p_section@%p section_size=%lu\n", __FILE__, __LINE__, (void*)p_section, section_size );
//      core::register_llvm_stackmaps((uintptr_t)p_section,(uintptr_t)p_section+section_size);
  } else {
//      printf("%s:%d     Could not find LLVM_STACKMAPS\n", __FILE__, __LINE__ );
  }
  if (llvmo::_sym_STARdebugObjectFilesSTAR->symbolValue().notnilp()) {
    llvm::MemoryBufferRef mem = Obj.getMemoryBufferRef();
    core::write_bf_stream( BF("%s:%d notifyObjectLoaded was invoked\n") % __FILE__ % __LINE__ );
    core::write_bf_stream( BF("%s:%d      --> sizeof(ObjectFile) -> %lu  MemoryBufferRef start: %p   size: %lu\n") % __FILE__ % __LINE__ % sizeof(Obj) % (void*) mem.getBufferStart() % mem.getBufferSize() );
    void** words = (void**)(&Obj);
    core::write_bf_stream( BF("%s:%d      --> ObjectFile words:\n") % __FILE__% __LINE__ );
    core::write_bf_stream( BF("%s:%d            0x00: %18p %18p\n") % __FILE__% __LINE__% words[0]% words[1]);
    core::write_bf_stream( BF("%s:%d            0x10: %18p %18p\n") % __FILE__% __LINE__% words[2]% words[3]);
    core::write_bf_stream( BF("%s:%d            0x20: %18p %18p\n") % __FILE__% __LINE__% words[4]% words[5]);
  }
  if (llvmo::_sym_STARdumpObjectFilesSTAR->symbolValue().notnilp()) {
    llvm::MemoryBufferRef mem = Obj.getMemoryBufferRef();
    dumpObjectFile(fileNum++,mem.getBufferStart(),mem.getBufferSize());
  }
}

bool ClaspSectionMemoryManager::finalizeMemory(std::string* ErrMsg ) {
#ifdef USE_CODE_O
  if (llvmo::_sym_STARdebugObjectFilesSTAR->symbolValue().notnilp()) {
    printf("%s:%d finalizeMemory\n", __FILE__, __LINE__);
  }
  LOG(BF("STACKMAP_LOG %s entered\n") % __FUNCTION__ );
  llvm::sys::MemoryBlock block(this->_CodeStart,this->_CodeSize);
  llvm::sys::Memory::protectMappedMemory(block,
                                         sys::Memory::MF_READ | sys::Memory::MF_EXEC);
  llvm::sys::Memory::InvalidateInstructionCache((void*)this->_CodeStart,this->_CodeSize);
#else
  this->SectionMemoryManager::finalizeMemory(ErrMsg);
#endif
  unsigned long section_size = 0;
  void* p_section = NULL;
  if (my_thread->_stackmap>0 && my_thread->_stackmap_size!=0) {
    p_section = reinterpret_cast<void*>(my_thread->_stackmap);
    section_size = my_thread->_stackmap_size;
    LOG(BF("STACKMAP_LOG   p_section@%p section_size=%lu\n") % (void*)p_section % section_size );
    core::register_llvm_stackmaps((uintptr_t)p_section,(uintptr_t)p_section+section_size,1);
//      core::process_llvm_stackmaps();
    my_thread->_stackmap = 0;
  } else {
//      printf("%s:%d     Could not find LLVM_STACKMAPS\n", __FILE__, __LINE__ );
  }
  return true;
}


};



#endif





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
