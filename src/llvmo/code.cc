/*
    File: code.cc

*/



#define DEBUG_LEVEL_FULL

//#include <llvm/Support/system_error.h>
#include <dlfcn.h>
#include <iomanip>
#include <clasp/core/foundation.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/debugger.h>
#include <clasp/llvmo/code.h>


namespace llvmo { // ObjectFile_O

#if 0
LibraryFile_sp LibraryFile_O::createLibrary(const std::string& slibraryName)
{
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Creating empty ObjectFile_O\n", __FILE__, __LINE__, __FUNCTION__));
  core::SimpleBaseString_sp libraryName = core::SimpleBaseString_O::make(slibraryName);
  LibraryFile_sp of = gc::GC<LibraryFile_O>::allocate(libraryName);
  return of;
}
#endif


ObjectFile_sp ObjectFile_O::create(std::unique_ptr<llvm::MemoryBuffer> buffer, size_t startupID, JITDylib_sp jitdylib, const std::string& sFasoName, size_t fasoIndex)
{
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Creating ObjectFile_O start=%p size= %lu\n", __FILE__, __LINE__, __FUNCTION__, buffer ? buffer->getBufferStart() : NULL, buffer ? buffer->getBufferSize() : 0));
  core::SimpleBaseString_sp fasoName = core::SimpleBaseString_O::make(sFasoName);
  ObjectFile_sp of = gc::GC<ObjectFile_O>::allocate(std::move(buffer),startupID,jitdylib,fasoName,fasoIndex);
  return of;
}


ObjectFile_O::~ObjectFile_O() {
  DEBUG_OBJECT_FILES_PRINT(("%s:%d dtor for ObjectFile_O %p\n", __FILE__, __LINE__, (void*)this ));
  printf("%s:%d dtor for ObjectFile_O %p\n", __FILE__, __LINE__, (void*)this );
  this->_Code = _Unbound<Code_O>();
}

Code_O::~Code_O() {
  DEBUG_OBJECT_FILES_PRINT(("%s:%d dtor for Code_O %p\n", __FILE__, __LINE__, (void*)this ));
  printf("%s:%d dtor for Code_O %p\n", __FILE__, __LINE__, (void*)this );
}


void ObjectFile_O::writeToFile(const std::string& fileName, const char* start, size_t size) {
  std::ofstream outfile;
  outfile.open(fileName, std::ios::binary | std::ios::out);
  outfile.write(start,size);
  outfile.close();
}


std::string Code_O::__repr__() const {
  stringstream ss;
  ss << "#<CODE @" << (void*)this << this->_ObjectFile->__repr__() << ">";
  return ss.str();
};

void* Code_O::literalsStart() const {
  if (this->_State == SaveState) {
    return (void*)&this->_DataCode[0];
  }
  return (void*)this->_LiteralVectorStart;
}
    
std::string ObjectFile_O::__repr__() const {
  stringstream ss;
  ss << "#<OBJECT-FILE " << this->_FasoName << " @" << (void*)this << ">";
  return ss.str();
};

std::string Library_O::__repr__() const {
  stringstream ss;
  ss << "#<LIBRARY @" << (void*)this << " :start " << (void*)this->_Start << " :end " << (void*)this->_End << " " << this->_Name->get_std_string() << ">";
  return ss.str();
};


}; // namespace llvmo, ObjectFile_O




namespace llvmo {


Code_sp Code_O::make(uintptr_t scanSize, uintptr_t totalSize) {
//  Code_sp code = gctools::GC<Code_O>::allocate_container_partial_scan(scanSize, totalSize);
  Code_sp code = gctools::GC<Code_O>::allocate_container(false,totalSize);
  // Don't put DEBUG_OBJECT_FILES_PRINT in here - this is called too early
  return code;
}

void* Code_O::allocateHead(uintptr_t size, uint32_t align) {
  const unsigned char* head = this->_DataCode.data()+this->_HeadOffset;
  head = (const unsigned char*)gctools::AlignUp((uintptr_t)head,align);
  uintptr_t headOffset = (uintptr_t)head-(uintptr_t)this->_DataCode.data()+size;
  if (headOffset > this->_TailOffset) {
    SIMPLE_ERROR(BF("There is not enough memory in the Code_O object - current size: %lu and we are over by %lu\n") % this->_DataCode.size() % (headOffset-this->_TailOffset));
  }
  const unsigned char* tail = this->_DataCode.data()+this->_TailOffset;
  if (tail<head) {
    printf("%s:%d Bad allocation tail@%p is less than head@%p\n", __FILE__, __LINE__, tail, head );
    abort();
  }
  return (void*)head;
}

void* Code_O::allocateTail(uintptr_t size, uint32_t align) {
  const unsigned char* tail = this->_DataCode.data()+this->_TailOffset-size;
  tail = (const unsigned char*)gctools::AlignDown((uintptr_t)tail,align);
  uintptr_t tailOffset = (uintptr_t)tail-(uintptr_t)this->_DataCode.data();
  if (this->_HeadOffset > tailOffset) {
    SIMPLE_ERROR(BF("There is not enough memory in the Code_O object - current size: %lu and we are over by %lu\n") % this->_DataCode.size() % (this->_HeadOffset-tailOffset));
  }
  this->_TailOffset = tailOffset;
  const unsigned char* head = this->_DataCode.data()+this->_HeadOffset;
  if (tail<head) {
    printf("%s:%d Bad allocation tail@%p is less than head@%p\n", __FILE__, __LINE__, tail, head );
    abort();
  }
  return (void*)tail;
}


void Code_O::describe() const
{
  core::write_bf_stream(BF("Code start: %p  stop: %p  size: %lu\n") % (void*)this % (void*)&this->_DataCode[this->_DataCode.size()] % (uintptr_t)((char*)&this->_DataCode[this->_DataCode.size()]-(char*)this));
};
  

};


namespace llvmo {

Library_sp Library_O::make(gctools::clasp_ptr_t start, gctools::clasp_ptr_t end, const std::string& name) {
  GC_ALLOCATE_VARIADIC(Library_O, lib, start, end );
  lib->_Name = core::SimpleBaseString_O::make(name);
  return lib;
}

};

namespace llvmo {

/*
 * Identify the CodeBase_O object for the entry-point.
 * 1. Search the _lisp->_AllLibraries list for a Library_O.
 * 2. Treat the entry_point as an interior pointer and look for the Code_O
 *     object that it corresponds to.
 * 3. If not found check if the address is in a dynamic library and create
 *      a Library_O object for it and add it to the _AllLibraries list
 * 4. If it's not one of the above then we have an entry point that
 *       I didn't think about or a serious error.
 */
CodeBase_sp identify_code_or_library(gctools::clasp_ptr_t entry_point) {

  //
  // 1. Search the _lisp->_AllLibraries list
  //
  core::T_sp allLibraries = _lisp->_Roots._AllLibraries.load();
  for ( core::T_sp cur = allLibraries; cur.consp(); cur = CONS_CDR(cur) ) {
    Library_sp lib = gc::As<Library_sp>(CONS_CAR(cur));
    if (lib->_Start <= entry_point && entry_point < lib->_End) {
//      printf("%s:%d:%s Returning library found in _lisp->_Roots._AllLibraries entry_point @%p  -> %s\n", __FILE__, __LINE__, __FUNCTION__, entry_point, _rep_(lib).c_str());
      return lib;
    }
  }

  //
  // 2. Treat the entry_point like an interior pointer and lookup the base Code_sp object
  //
  gctools::Tagged taggedCodePointer;
  bool foundBase = gctools::tagged_pointer_from_interior_pointer<Code_O>( entry_point, taggedCodePointer );
  Code_sp codeObject(taggedCodePointer);
  if (foundBase) {
//    printf("%s:%d:%s Returning Code_sp object entry_point @%p  -> %s\n", __FILE__, __LINE__, __FUNCTION__, entry_point, _rep_(codeObject).c_str());
    return codeObject;
  }

  //
  // 3. Look the entry point up in the dlopen libraries.
  //    If we find it, push an entry into the _lisp->_AllLibraries list
    
  gctools::clasp_ptr_t start, end;
  std::string libraryName;
  bool isExecutable;
  bool foundLibrary = core::lookup_address_in_library( reinterpret_cast<gctools::clasp_ptr_t>(entry_point), start, end, libraryName, isExecutable);
  if (foundLibrary) {
    // printf("%s:%d:%s For entry_point @%p found new library start: %p   end: %p  name: %s\n", __FILE__, __LINE__, __FUNCTION__, entry_point, (void*)start, (void*)end, libraryName.c_str());
    Library_sp newlib = Library_O::make(reinterpret_cast<gctools::clasp_ptr_t>(start),reinterpret_cast<gctools::clasp_ptr_t>(end),libraryName);
    core::T_sp expected;
    core::Cons_sp entry = core::Cons_O::create(newlib,_Nil<core::T_O>());
    do {
      expected = _lisp->_Roots._AllLibraries.load();
      entry->rplacd(expected);
    } while (!_lisp->_Roots._AllLibraries.compare_exchange_weak(expected,entry));
    // printf("%s:%d:%s Returning new library added to _lisp->_Roots._AllLibraries entry_point @%p  -> %s\n", __FILE__, __LINE__, __FUNCTION__, entry_point, _rep_(newlib).c_str());
    return newlib;
  }
  
  
  //
  // 4. We have hit an unidentifiable entry_point - what is it
  SIMPLE_ERROR(BF("We have hit an unidentifiable entry_point at %p - figure out what it is") % (void*)entry_point);
}

};

namespace llvmo {
std::atomic<size_t> fileNum;


void dumpObjectFile(const char* start, size_t size) {
  size_t num = fileNum++;
  std::stringstream filename;
  filename << "object-file-" << num << ".o";
  std::ofstream fout;
  printf("%s:%d:%s dumping object file to %s\n", __FILE__, __LINE__, __FUNCTION__, filename.str().c_str());
  fout.open(filename.str(), std::ios::out | std::ios::binary );
  fout.write(start,size);
  fout.close();
}





};


namespace llvmo {


void save_object_file_and_code_info(ObjectFile_sp ofi)
{
//  register_object_file_with_gdb((void*)objectFileStart,objectFileSize);
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s register object file \"%s\"\n", __FILE__, __LINE__, __FUNCTION__, core::_rep_(ofi->_FasoName).c_str()));
  core::T_sp expected;
  core::Cons_sp entry = core::Cons_O::create(ofi,_Nil<core::T_O>());
  do {
    expected = _lisp->_Roots._AllObjectFiles.load();
    entry->rplacd(expected);
  } while (!_lisp->_Roots._AllObjectFiles.compare_exchange_weak(expected,entry));
  if (globalDebugObjectFiles == DebugObjectFilesPrintSave) {
    llvm::MemoryBufferRef mem = *(ofi->_MemoryBuffer);
    dumpObjectFile(mem.getBufferStart(),mem.getBufferSize());
  }
}


CL_DOCSTRING("For an instruction pointer inside of code generated from an object file - return the relative address (the sectioned address)");
CL_LISPIFY_NAME(object_file_sectioned_address);
CL_DEFUN SectionedAddress_sp object_file_sectioned_address(void* instruction_pointer, ObjectFile_sp ofi, bool verbose) {
        // Here is the info for the SectionedAddress
  uintptr_t sectionID = ofi->_Code->_TextSegmentSectionId;
  uintptr_t offset = ((char*)instruction_pointer - (char*)ofi->_Code->_TextSegmentStart);
  SectionedAddress_sp sectioned_address = SectionedAddress_O::create(sectionID, offset);
      // now the object file
  if (verbose) {
    core::write_bf_stream(BF("faso-file: %s  object-file-position: %lu  objectID: %lu\n") % ofi->_FasoName % ofi->_FasoIndex % ofi->_StartupID);
    core::write_bf_stream(BF("SectionID: %lu    memory offset: %lu\n") % ofi->_FasoIndex % offset );
  }
  return sectioned_address;
}

CL_DOCSTRING(R"doc(Identify the object file whose generated code range contains the instruction-pointer.
Return NIL if none or (values offset-from-start object-file). The index-from-start is the number of bytes of the instruction-pointer from the start of the code range.)doc");
CL_LISPIFY_NAME(object_file_for_instruction_pointer);
CL_DEFUN core::T_mv object_file_for_instruction_pointer(void* instruction_pointer, bool verbose)
{
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s entered looking for instruction_pointer@%p search Code_O objects\n", __FILE__, __LINE__, __FUNCTION__, instruction_pointer ));
  core::T_sp cur = _lisp->_Roots._AllObjectFiles.load();
  size_t count;
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s instruction_pointer = %p  object_files = %p\n", __FILE__, __LINE__, __FUNCTION__, (char*)instruction_pointer, cur.raw_()));
  if ((cur.nilp()) && verbose){
    core::write_bf_stream(BF("No object files registered - cannot find object file for address %p\n") % (void*)instruction_pointer);
  }
  while (cur.consp()) {
    ObjectFile_sp ofi = gc::As<ObjectFile_sp>(CONS_CAR(gc::As_unsafe<core::Cons_sp>(cur)));
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Looking at object file _text %p to %p\n", __FILE__, __LINE__, __FUNCTION__, ofi->_Code->_TextSegmentStart, ofi->_Code->_TextSegmentEnd));
    if ((char*)instruction_pointer>=(char*)ofi->_Code->_TextSegmentStart&&(char*)instruction_pointer<((char*)ofi->_Code->_TextSegmentEnd)) {
      core::T_sp sectionedAddress = object_file_sectioned_address(instruction_pointer,ofi,verbose);
      return Values(sectionedAddress,ofi);
    }
    cur = CONS_CDR(gc::As_unsafe<core::Cons_sp>(cur));
    count++;
  }
  return Values(_Nil<core::T_O>());
}

CL_LISPIFY_NAME(release_object_files);
CL_DEFUN void release_object_files() {
  _lisp->_Roots._AllObjectFiles.store(_Nil<core::T_O>());
  core::write_bf_stream(BF("ObjectFiles have been released\n"));
}

CL_LISPIFY_NAME(number_of_object_files);
CL_DEFUN size_t number_of_object_files() {
  core::T_sp cur = _lisp->_Roots._AllObjectFiles.load();
  size_t count = 0;
  while (cur.consp()) {
    cur = CONS_CDR(gc::As_unsafe<core::Cons_sp>(cur));
    count++;
  }
  return count;
}

CL_LISPIFY_NAME(total_memory_allocated_for_object_files);
CL_DEFUN size_t total_memory_allocated_for_object_files() {
  core::T_sp cur = _lisp->_Roots._AllObjectFiles.load();
  size_t count = 0;
  size_t sz = 0;
  while (cur.consp()) {
    ObjectFile_sp ofi = gc::As<ObjectFile_sp>(CONS_CAR(cur));
    sz += ofi->_MemoryBuffer->getBufferSize();
    count++;
    cur = CONS_CDR(cur);
  }
  return sz;
}

CL_LISPIFY_NAME(describe_code);
CL_DEFUN void describe_code() {
  core::T_sp cur = _lisp->_Roots._AllObjectFiles.load();
  size_t count = 0;
  size_t sz = 0;
  while (cur.consp()) {
    ObjectFile_sp ofi = gc::As<ObjectFile_sp>(CONS_CAR(cur));
    Code_sp code = ofi->_Code;
    core::write_bf_stream(BF("ObjectFile start: %p  size: %lu\n") % (void*)ofi->_MemoryBuffer->getBufferStart() % ofi->_MemoryBuffer->getBufferSize());
    code->describe();
    sz += ofi->_MemoryBuffer->getBufferSize();
    count++;
    cur = CONS_CDR(cur);
  }
  core::write_bf_stream(BF("Total number of object files: %lu\n") % count);
  core::write_bf_stream(BF("  Total size of object files: %lu\n") % sz);
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
    DEBUG_OBJECT_FILES_PRINT(("%s:%d  allocateDataSection Size: %lu  Alignment: %u SectionId: %u SectionName: %s isReadOnly: %d --> allocated at: %p\n", __FILE__, __LINE__, Size, Alignment, SectionID, SectionName.str().c_str(), isReadOnly, (void*)ptr ));
    }
    return ptr;
  }

void 	ClaspSectionMemoryManager::notifyObjectLoaded (RuntimeDyld &RTDyld, const object::ObjectFile &Obj) {
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s entered\n", __FILE__, __LINE__, __FUNCTION__ ));
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
      DEBUG_OBJECT_FILES_PRINT(("%s:%d Found stackmap at %p size: %lu\n", __FILE__, __LINE__, (void*) stackmap, stackmap_size));
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
    dumpObjectFile(mem.getBufferStart(),mem.getBufferSize());
  }
}

bool ClaspSectionMemoryManager::finalizeMemory(std::string* ErrMsg ) {
#ifdef USE_CODE_O
  DEBUG_OBJECT_FILES_PRINT(("%s:%d finalizeMemory\n", __FILE__, __LINE__));
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
