/*
    File: code.cc

*/



#define DEBUG_LEVEL_FULL

//#include <llvm/Support/system_error.h>
#include <dlfcn.h>
#include <iomanip>
#include <cstdint>
#include <clasp/core/foundation.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/debugger.h>
#include <clasp/core/pointer.h>
#include <clasp/llvmo/code.h>
#include <clasp/llvmo/debugInfoExpose.h>


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


ObjectFile_sp ObjectFile_O::create(std::unique_ptr<llvm::MemoryBuffer> buffer, size_t startupID, JITDylib_sp jitdylib, const std::string& sFasoName, size_t

                                   fasoIndex)
{
//  printf("%s:%d:%s Creating ObjectFile faso: %s index: %lu\n", __FILE__, __LINE__, __FUNCTION__, sFasoName.c_str(), fasoIndex);
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Creating ObjectFile_O start=%p size= %lu\n", __FILE__, __LINE__, __FUNCTION__, buffer ? buffer->getBufferStart() : NULL, buffer ? buffer->getBufferSize() : 0));
  core::SimpleBaseString_sp fasoName = core::SimpleBaseString_O::make(sFasoName);
  ObjectFile_sp of = gc::GC<ObjectFile_O>::allocate(std::move(buffer),startupID,jitdylib,fasoName,fasoIndex);
  return of;
}

CL_LISPIFY_NAME(code);
CL_DEFMETHOD
Code_sp ObjectFile_O::code() const {
  return this->_Code;
};

ObjectFile_O::~ObjectFile_O() {
  DEBUG_OBJECT_FILES_PRINT(("%s:%d dtor for ObjectFile_O %p\n", __FILE__, __LINE__, (void*)this ));
  this->_Code = unbound<Code_O>();
}

llvm::Expected<std::unique_ptr<llvm::object::ObjectFile>> ObjectFile_O::getObjectFile() {
  llvm::MemoryBufferRef mem = *(this->_MemoryBuffer);
  return llvm::object::ObjectFile::createObjectFile(mem);
}

CL_DEFMETHOD
void* Code_O::absoluteAddress(SectionedAddress_sp sa) {
  if (sa->_value.SectionIndex != this->_TextSectionId) {
    SIMPLE_ERROR(BF("The sectioned-address section-index %lu does not match the code section-index %lu") % sa->_value.SectionIndex % this->_TextSectionId);
  }
  return (void*)((char*)this->_TextSectionStart + sa->_value.Address);
}


size_t Code_O::sizeofInState(Code_O* code, CodeState_t state ) {
  if (state == SaveState) {
    return sizeof(Code_O)+code->_LiteralVectorSizeBytes;
  }
  return gctools::sizeof_container<Code_O>(code->_DataCode.size());
}

CL_DEFMETHOD core::T_sp Code_O::codeLineTable() const {
  llvmo::ObjectFile_sp of = this->_ObjectFile;
  llvmo::DWARFContext_sp dwarfContext = llvmo::DWARFContext_O::createDWARFContext(of);
  llvm::object::SectionedAddress sa;
  sa.Address = 0;
  sa.SectionIndex = this->_TextSectionId;
  uintptr_t size = (uintptr_t)this->_TextSectionEnd - (uintptr_t)this->_TextSectionStart;
  auto lineTable = (*dwarfContext).wrappedPtr()->getLineInfoForAddressRange(sa, size );
  printf("%s:%d:%s Number of entries: %lu\n", __FILE__, __LINE__, __FUNCTION__, lineTable.size());
  return nil<T_O>();
}

std::string Code_O::filename() const {
  stringstream ss;
  ss << this->_ObjectFile->_FasoName->get_std_string() << ":" << this->_ObjectFile->_ObjectId;
  return ss.str();
}

Code_O::~Code_O() {
  DEBUG_OBJECT_FILES_PRINT(("%s:%d dtor for Code_O %p\n", __FILE__, __LINE__, (void*)this ));
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

/*! Return a pointer to the literals vector.
The number of bytes in the literals vector is returned by literalsSize().
*/
void* Code_O::literalsStart() const {
  if (this->_State == SaveState) {
    return (void*)&this->_DataCode[0];
  }
  return (void*)this->_LiteralVectorStart;
}
    
std::string ObjectFile_O::__repr__() const {
  stringstream ss;
  ss << "#<OBJECT-FILE " << this->_FasoName;
  ss << " :faso-index " << this->_FasoIndex << " ";
  ss << " :code @" << (void*)this->_Code.raw_() << " ";
  ss << " :object-file @" << (void*)this->_MemoryBuffer->getBufferStart() << " ";
  ss << " :object-file-size " << (size_t)this->_MemoryBuffer->getBufferSize() << " ";
  ss << " @" << (void*)this << ">";
  return ss.str();
};

std::string Library_O::__repr__() const {
  stringstream ss;
  ss << "#<LIBRARY @" << (void*)this << " :start " << (void*)this->_Start << " :end " << (void*)this->_End << " " << this->_Name->get_std_string() << ">";
  return ss.str();
};

CL_DOCSTRING("Return the Code object corresponding to the given ObjectFile");
CL_LISPIFY_NAME(object_file_code);
CL_DEFUN Code_sp object_file_code(ObjectFile_sp object_file) {
  return object_file->_Code;
}

}; // namespace llvmo, ObjectFile_O




namespace llvmo {


Code_sp Code_O::make(uintptr_t scanSize, uintptr_t totalSize, ObjectFile_sp objectFile) {
//  printf("%s:%d:%s Creating Code_O for objectFile: %s %lu\n", __FILE__, __LINE__, __FUNCTION__, objectFile->_FasoName->get_std_string().c_str(), objectFile->_StartupID );
//  Code_sp code = gctools::GC<Code_O>::allocate_container_partial_scan(scanSize, totalSize);
  Code_sp code = gctools::GC<Code_O>::allocate_container(false,totalSize);
  code->_ObjectFile = objectFile;
  objectFile->_Code = code;
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Allocated Code_O object and installed in objectFile->_Code %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)code.raw_() ));
  // Don't put DEBUG_OBJECT_FILES_PRINT in here - this is called too early
#if 0
  printf("%s:%d:%s Allocated code object from %p to %p\n", __FILE__, __LINE__, __FUNCTION__,
         (void*)&code->_DataCode[0],
         (void*)&code->_DataCode[code->_DataCode.size()]);
#endif
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

CL_DOCSTRING("Return the count of literals in the given Code object");
CL_LISPIFY_NAME(code_literals_length);
CL_DEFUN core::Integer_sp code_literals_length(Code_sp code) {
  return core::Integer_O::create(code->literalsSize()/sizeof(core::T_O*));
}

CL_DOCSTRING("Return an element from the Code object's literals vector. WARNING: Does not check bound.");
CL_LISPIFY_NAME(code_literals_ref);
CL_DEFUN core::T_sp code_literals_ref(Code_sp code, size_t idx) {
  core::T_O** literals = (core::T_O**)(code->literalsStart());
  core::T_sp ret((gc::Tagged)(literals[idx]));
  return ret;
}

};


namespace llvmo {

Library_sp Library_O::make(bool executable, gctools::clasp_ptr_t start, gctools::clasp_ptr_t end, uintptr_t vtableStart, uintptr_t vtableEnd, const std::string& name) {
  auto  lib = gctools::GC<Library_O>::allocate( executable, start, end, vtableStart, vtableEnd );
  lib->_Name = core::SimpleBaseString_O::make(name);
  return lib;
}

std::string Library_O::filename() const {
  return this->_Name->get_std_string();
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
  uintptr_t vtableStart, vtableEnd;
  std::string libraryName;
  bool isExecutable;
  bool foundLibrary = core::lookup_address_in_library( reinterpret_cast<gctools::clasp_ptr_t>(entry_point), start, end, libraryName, isExecutable, vtableStart, vtableEnd );
  if (foundLibrary) {
    // printf("%s:%d:%s For entry_point @%p found new library start: %p   end: %p  name: %s\n", __FILE__, __LINE__, __FUNCTION__, entry_point, (void*)start, (void*)end, libraryName.c_str());
    Library_sp newlib = Library_O::make(isExecutable, reinterpret_cast<gctools::clasp_ptr_t>(start),reinterpret_cast<gctools::clasp_ptr_t>(end), vtableStart, vtableEnd, libraryName);
    core::T_sp expected;
    core::Cons_sp entry = core::Cons_O::create(newlib,nil<core::T_O>());
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


void dumpObjectFile(const char* start, size_t size, void* codeStart) {
  size_t num = fileNum++;
  std::stringstream filename;
  filename << "object-file-" << num;
  if (codeStart) {
    filename << "-" << std::hex << (void*)codeStart;
  }
  filename << ".o";
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
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Adding to _lisp->_Roots._AllObjectFiles  %p \"%s\"\n", __FILE__, __LINE__, __FUNCTION__, (void*)ofi.raw_(), core::_rep_(ofi->_FasoName).c_str()));
  core::T_sp expected;
  core::Cons_sp entry = core::Cons_O::create(ofi,nil<core::T_O>());
  do {
    expected = _lisp->_Roots._AllObjectFiles.load();
    entry->rplacd(expected);
  } while (!_lisp->_Roots._AllObjectFiles.compare_exchange_weak(expected,entry));
  if (globalDebugObjectFiles == DebugObjectFilesPrintSave) {
    llvm::MemoryBufferRef mem = *(ofi->_MemoryBuffer);
    dumpObjectFile(mem.getBufferStart(),mem.getBufferSize(), (void*)&ofi->_Code->_DataCode[0] );
  }
}

CL_DOCSTRING("For an instruction pointer inside of code generated from an object file - return the relative address (the sectioned address)");
CL_LISPIFY_NAME(object_file_sectioned_address);
CL_DEFUN SectionedAddress_sp object_file_sectioned_address(void* instruction_pointer, ObjectFile_sp ofi, bool verbose) {
        // Here is the info for the SectionedAddress
  uintptr_t sectionID = ofi->_Code->_TextSectionId;
  uintptr_t offset = ((char*)instruction_pointer - (char*)ofi->_Code->_TextSectionStart);
  SectionedAddress_sp sectioned_address = SectionedAddress_O::create(sectionID, offset);
      // now the object file
  if (verbose) {
    core::write_bf_stream(BF("faso-file: %s  object-file-position: %lu  objectID: %lu\n") % ofi->_FasoName % ofi->_FasoIndex % ofi->_ObjectId);
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
    core::T_sp car = CONS_CAR(gc::As_unsafe<core::Cons_sp>(cur));
    ObjectFile_sp ofi = gc::As<ObjectFile_sp>(car);
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Looking at object file _text %p to %p\n", __FILE__, __LINE__, __FUNCTION__, ofi->_Code->_TextSectionStart, ofi->_Code->_TextSectionEnd));
    if ((char*)instruction_pointer>=(char*)ofi->_Code->_TextSectionStart&&(char*)instruction_pointer<((char*)ofi->_Code->_TextSectionEnd)) {
      core::T_sp sectionedAddress = object_file_sectioned_address(instruction_pointer,ofi,verbose);
      return Values(sectionedAddress,ofi);
    }
    cur = CONS_CDR(gc::As_unsafe<core::Cons_sp>(cur));
    count++;
  }
  return Values(nil<core::T_O>());
}

// FIXME: name sucks
core::T_sp only_object_file_for_instruction_pointer(void* ip) {
  core::T_sp cur = _lisp->_Roots._AllObjectFiles.load();
  while (cur.consp()) {
    ObjectFile_sp ofi = gc::As<ObjectFile_sp>(CONS_CAR(gc::As_unsafe<core::Cons_sp>(cur)));
    if (((char*)ip >= (char*)ofi->_Code->_TextSectionStart) &&
        ((char*)ip <  (char*)ofi->_Code->_TextSectionEnd))
      return ofi;
    cur = CONS_CDR(gc::As_unsafe<core::Cons_sp>(cur));
  }
  return nil<core::T_O>();
}

CL_LISPIFY_NAME(release_object_files);
CL_DEFUN void release_object_files() {
  _lisp->_Roots._AllObjectFiles.store(nil<core::T_O>());
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

struct StackmapHeader {
  uint8_t _version;
  uint8_t _reserved0;
  uint16_t _reserved1;
};

CL_LISPIFY_NAME(all_object_files);
CL_DEFUN core::T_sp all_object_files() {
  core::T_sp cur = _lisp->_Roots._AllObjectFiles.load();
  return cur;
}

extern "C" {
struct jit_code_entry
{
  struct jit_code_entry *next_entry;
  struct jit_code_entry *prev_entry;
  const char *symfile_addr;
  uint64_t symfile_size;
};

struct jit_descriptor
{
  uint32_t version;
  /* This type should be jit_actions_t, but we use uint32_t
     to be explicit about the bitwidth.  */
  uint32_t action_flag;
  struct jit_code_entry *relevant_entry;
  struct jit_code_entry *first_entry;
};

extern struct jit_descriptor __jit_debug_descriptor;

};

CL_DOCSTRING("Generate a list of jit_code_entry objects");
CL_LISPIFY_NAME(jit_code_entries)
CL_DEFUN core::T_sp jit_code_entries() {
  jit_code_entry* jce = __jit_debug_descriptor.first_entry;
  ql::list ll;
  while (jce) {
    core::T_sp obj = core::Cons_O::create(core::Pointer_O::create((void*)jce->symfile_addr),core::Integer_O::create(jce->symfile_size));
    ll << obj;
    jce = jce->next_entry;
  }
  return ll.result();
}


CL_DOCSTRING("Generate a list of JITted symbols to /tmp/perf-<pid>.map");
CL_DEFUN void ext__generate_perf_map() {
  stringstream ss;
  ss << "/tmp/perf-" << getpid() << ".map";
  core::write_bf_stream(BF("Writing to %s\n") % ss.str());
  FILE* fout = fopen(ss.str().c_str(),"w");
  jit_code_entry* jce = __jit_debug_descriptor.first_entry;
  ql::list ll;
  size_t idx;
  while (jce) {
    const char* of_start = jce->symfile_addr;
    size_t of_length = jce->symfile_size;
    llvm::StringRef sbuffer((const char*)of_start, of_length);
    stringstream ss;
    ss << "buffer" << (void*)of_start;
    std::string mem = ss.str();
    llvm::StringRef name(mem);
    std::unique_ptr<llvm::MemoryBuffer> memoryBuffer(llvm::MemoryBuffer::getMemBuffer(sbuffer,name,false));
    llvm::MemoryBufferRef memr = *(memoryBuffer);
    llvm::Expected<std::unique_ptr<llvm::object::ObjectFile>> obj = llvm::object::ObjectFile::createObjectFile(memr);
    for ( auto sym : (*obj)->symbols() ) {
      if ((*sym.getAddress())!=0)
        fprintf(fout,"%lX %lX %s\n", (uintptr_t)(*sym.getAddress()), (size_t)sym.getCommonSize(), (sym).getName()->str().c_str() );
    }
    jce = jce->next_entry;
  }
  fclose(fout);
}


CL_LISPIFY_NAME(describe_code);
CL_DEFUN void describe_code() {
  core::T_sp cur = _lisp->_Roots._AllObjectFiles.load();
  size_t count = 0;
  size_t sz = 0;
  size_t goodStackmaps = 0;
  while (cur.consp()) {
    ObjectFile_sp ofi = gc::As<ObjectFile_sp>(CONS_CAR(cur));
    Code_sp code = ofi->_Code;
    core::write_bf_stream(BF("ObjectFile start: %p  size: %lu\n") % (void*)ofi->_MemoryBuffer->getBufferStart() % ofi->_MemoryBuffer->getBufferSize());
    uintptr_t codeStart = (uintptr_t)&code->_DataCode[0];
    uintptr_t codeEnd = (uintptr_t)&code->_DataCode[code->_DataCode.size()];
    core::write_bf_stream(BF("   corresponding Code_O object: %p  code range: %p - %p\n") % (void*)code.raw_() % (void*)codeStart % (void*)codeEnd );
    uintptr_t stackmapStart = (uintptr_t)code->_StackmapStart;
    uintptr_t stackmapEnd = (uintptr_t)code->_StackmapStart+code->_StackmapSize;
    if (stackmapStart<=stackmapEnd) {
      if (codeStart <= stackmapStart && stackmapEnd <= codeEnd) {
        StackmapHeader* header = (StackmapHeader*)stackmapStart;
        if (header->_version == 3 && header->_reserved0 == 0 && header->_reserved1 == 0 ) {
          goodStackmaps++;
        }
        core::write_bf_stream(BF("      The stackmap %p %p is within the code region\n") % (void*)stackmapStart % (void*)stackmapEnd );
      } else {
        core::write_bf_stream(BF(" ERROR     The stackmap %p %p is NOT within the code region\n") % (void*)stackmapStart % (void*)stackmapEnd );
      }
    } else {
      core::write_bf_stream(BF(" ERROR     The stackmap %p %p is not a real memory region\n") % (void*)stackmapStart % (void*)stackmapEnd );
    }      
    code->describe();
    sz += ofi->_MemoryBuffer->getBufferSize();
    count++;
    cur = CONS_CDR(cur);
  }
  core::write_bf_stream(BF("Total number of object files: %lu\n") % count);
  core::write_bf_stream(BF("  Total size of object files: %lu\n") % sz);
  core::write_bf_stream(BF("             Valid stackmaps: %lu\n") % goodStackmaps );
}

};


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
