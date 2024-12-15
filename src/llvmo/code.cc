/*
    File: code.cc

*/

#define DEBUG_LEVEL_FULL

// #include <llvm/Support/system_error.h>
#include <unistd.h>
#include <dlfcn.h>
#include <iomanip>
#include <cstdint>
#include <clasp/core/foundation.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/debugger.h>
#include <clasp/core/pointer.h>
#include <clasp/llvmo/intrinsics.h>
#include <clasp/llvmo/code.h>
#include <clasp/llvmo/debugInfoExpose.h>

namespace llvmo {

void JITDataReadWriteMaybeExecute() {
#if defined(CLASP_APPLE_SILICON)
  // On Apple Silicon we turn off MEM_JIT memory write protect for this thread
  pthread_jit_write_protect_np(false);
#else
  // Nothing for now - assume memory must be RWX
#endif
}

void JITDataReadExecute() {
#if defined(CLASP_APPLE_SILICON)
  // On Apple Silicon we turn off MEM_JIT memory write protect for this thread
  pthread_jit_write_protect_np(true);
#else
  // Nothing for now - assume memory must be RWX
#endif
}

void JITMemoryReadWriteMaybeExecute(llvm::jitlink::BasicLayout& BL) {
#if defined(CLASP_APPLE_SILICON)
  // On Apple Silicon we turn off MEM_JIT memory write protect for this thread
  pthread_jit_write_protect_np(false);
#else
  size_t PageSize = getpagesize();
  auto rwxProt = llvm::sys::Memory::MF_READ | llvm::sys::Memory::MF_WRITE | llvm::sys::Memory::MF_EXEC;
  for (auto& KV : BL.segments()) {
    auto& Seg = KV.second;
    uint64_t SegSize = alignTo(Seg.ContentSize + Seg.ZeroFillSize, PageSize);
    sys::MemoryBlock MB(Seg.WorkingMem, SegSize);
    sys::Memory::protectMappedMemory(MB, rwxProt);
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Temporarily Applying Protections (RWX/%x) to range %p - %p\n", __FILE__, __LINE__,
                              __FUNCTION__, rwxProt, (void*)Seg.WorkingMem, (void*)(Seg.WorkingMem + SegSize)));
  }
#endif
}

void JITMemoryReadExecute(llvm::jitlink::BasicLayout& BL) {
  size_t PageSize = getpagesize();
#if defined(CLASP_APPLE_SILICON)
  pthread_jit_write_protect_np(true);
  for (auto& KV : BL.segments()) {
    const auto& AG = KV.first;
    auto& Seg = KV.second;
    uint64_t SegSize = alignTo(Seg.ContentSize + Seg.ZeroFillSize, Seg.Alignment.value());
    auto Prot = toSysMemoryProtectionFlags(AG.getMemProt());
    sys::MemoryBlock MB(Seg.WorkingMem, SegSize);
    if (Prot & sys::Memory::MF_EXEC)
      sys::Memory::InvalidateInstructionCache(MB.base(), MB.allocatedSize());
  }
#else
  for (auto& KV : BL.segments()) {
    const auto& AG = KV.first;
    auto& Seg = KV.second;
#ifdef DEBUG_OBJECT_FILES
    std::string back;
    llvm::raw_string_ostream ss(back);
    llvm::jitlink::operator<<(ss, AG);
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Applying Protections %s to range %p - %p\n", __FILE__, __LINE__, __FUNCTION__,
                              ss.str().c_str(), Seg.WorkingMem, (Seg.WorkingMem + Seg.ContentSize + Seg.ZeroFillSize)));
#endif
    uint64_t SegSize = alignTo(Seg.ContentSize + Seg.ZeroFillSize, PageSize);
    auto Prot = toSysMemoryProtectionFlags(AG.getMemProt());
    if ((Prot & sys::Memory::MF_RWE_MASK) == sys::Memory::MF_READ) {
      Prot = (sys::Memory::ProtectionFlags)(sys::Memory::MF_READ | sys::Memory::MF_WRITE);
    } else if ((Prot & sys::Memory::MF_EXEC)) {
      Prot = (sys::Memory::ProtectionFlags)(sys::Memory::MF_READ | sys::Memory::MF_WRITE | sys::Memory::MF_EXEC);
    }
    sys::MemoryBlock MB(Seg.WorkingMem, SegSize);
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Protecting memory from %p to %p with %x\n", __FILE__, __LINE__, __FUNCTION__,
                              (void*)(Seg.WorkingMem), (void*)(Seg.WorkingMem + SegSize), Prot));
    if (auto EC = sys::Memory::protectMappedMemory(MB, Prot)) {
      printf("%s:%d:%s There was an error returned by sys::Memory::protectMappedMemory -> %d\n", __FILE__, __LINE__, __FUNCTION__,
             EC.value()); // errorCodeToError(EC) );
      abort();
    }
    if (Prot & sys::Memory::MF_EXEC)
      sys::Memory::InvalidateInstructionCache(MB.base(), MB.allocatedSize());
  }
#endif
}

}; // namespace llvmo

namespace llvmo { // ObjectFile_O

ObjectFile_sp ObjectFile_O::createForObjectFile(const std::string& scodename, JITDylib_sp jitdylib, size_t objectId) {
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Creating ObjectFile_O codename = %s\n", __FILE__, __LINE__, __FUNCTION__, scodename.c_str()));
  core::SimpleBaseString_sp codename = core::SimpleBaseString_O::make(scodename);
  ObjectFile_sp of = gc::GC<ObjectFile_O>::allocate(codename, jitdylib, objectId);
  return of;
}

ObjectFile_sp ObjectFile_O::createForModule(const std::string& scodename, JITDylib_sp jitdylib, size_t objectId) {
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Creating ObjectFile_O codename = %s\n", __FILE__, __LINE__, __FUNCTION__, scodename.c_str()));
  core::SimpleBaseString_sp codename = core::SimpleBaseString_O::make(scodename);
  ObjectFile_sp of = gc::GC<ObjectFile_O>::allocate(codename, jitdylib, objectId);
  return of;
}

ObjectFile_sp ObjectFile_O::create(const std::string& scodename, std::unique_ptr<llvm::MemoryBuffer> buffer, size_t startupID,
                                   JITDylib_sp jitdylib, const std::string& sFasoName, size_t fasoIndex) {
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Creating ObjectFile_O start=%p size= %lu\n", __FILE__, __LINE__, __FUNCTION__,
                            buffer ? buffer->getBufferStart() : NULL, buffer ? buffer->getBufferSize() : 0));
  core::SimpleBaseString_sp codename = core::SimpleBaseString_O::make(scodename);
  core::SimpleBaseString_sp fasoName = core::SimpleBaseString_O::make(sFasoName);
  ObjectFile_sp of = gc::GC<ObjectFile_O>::allocate(codename, std::move(buffer), startupID, jitdylib, fasoName, fasoIndex);
  return of;
}

ObjectFile_O::~ObjectFile_O() { DEBUG_OBJECT_FILES_PRINT(("%s:%d dtor for ObjectFile_O %p\n", __FILE__, __LINE__, (void*)this)); }

llvm::Expected<std::unique_ptr<llvm::object::ObjectFile>> ObjectFile_O::getObjectFile() {
  llvm::MemoryBufferRef mem = *(this->_MemoryBuffer);
  return llvm::object::ObjectFile::createObjectFile(mem);
}

size_t ObjectFile_O::sizeofInState(ObjectFile_O* code, CodeState_t state) {
  if (state == SaveState) {
    return sizeof(ObjectFile_O) + code->_LiteralVectorSizeBytes;
  }
  return gctools::sizeof_container<ObjectFile_O>(code->_DataCode.size());
}

std::string ObjectFile_O::filename() const {
  stringstream ss;
  ss << this->_FasoName->get_std_string() << ":" << this->_ObjectId;
  return ss.str();
}

void ObjectFile_O::writeToFile(const std::string& fileName, const char* start, size_t size) {
  std::ofstream outfile;
  outfile.open(fileName, std::ios::binary | std::ios::out);
  outfile.write(start, size);
  outfile.close();
}

/*! Return a pointer to the literals vector.
The number of bytes in the literals vector is returned by literalsSize().
*/
void* ObjectFile_O::literalsStart() const {
  if (this->_State == SaveState) {
    return (void*)&this->_DataCode[0];
  }
  return (void*)this->_LiteralVectorStart;
}

void ObjectFile_O::setLiteralVectorStart(void* start) {
  this->_LiteralVectorStart = (uintptr_t)start;
}

void* ObjectFile_O::getLiteralVectorStart() {
  return (void*)this->_LiteralVectorStart;
}
std::string ObjectFile_O::__repr__() const {
  stringstream ss;
  ss << "#<OBJECT-FILE " << core::_rep_(this->_CodeName);
  ss << " :faso-name " << core::_rep_(this->_FasoName);
  ss << " :faso-index " << this->_FasoIndex << " ";
  ss << " :state ";
  if (this->_State==RunState) {
    ss << "Run";
  } else {
    ss << "Save";
  }
  if (this->_MemoryBuffer) {
    ss << " :object-file @" << (void*)this->_MemoryBuffer->getBufferStart() << " ";
    ss << " :object-file-size " << (size_t)this->_MemoryBuffer->getBufferSize() << " ";
  }
  ss << " :DataCode0 " << (void*)&(this->_DataCode[0]);
  ss << " :LiteralVectorStart" << (void*)(this->_LiteralVectorStart);
  ss << " @" << (void*)this << ">";
  return ss.str();
};

std::string Library_O::__repr__() const {
  stringstream ss;
  ss << "#<LIBRARY @" << (void*)this << " :start " << (void*)this->_Start << " :end " << (void*)this->_End << " "
     << this->_Name->get_std_string() << ">";
  return ss.str();
};

void Library_O::fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
  if (snapshotSaveLoad::operation(fixup) == snapshotSaveLoad::LoadOp) {
    this->_MemoryBuffer = unbound<MemoryBuffer_O>();
    this->_ObjectFile.release();
    this->_DWARFContext = unbound<DWARFContext_O>();
  }
}

}; // namespace llvmo

namespace llvmo {

CL_DOCSTRING(R"dx(Return the count of literals in the given Code object)dx");
CL_LISPIFY_NAME(code_literals_length);
DOCGROUP(clasp);
CL_DEFUN core::Integer_sp code_literals_length(ObjectFile_sp code) {
  return core::Integer_O::create(code->literalsSize() / sizeof(core::T_O*));
}

CL_DOCSTRING(R"dx(Return an element from the Code object's literals vector. WARNING: Does not check bound.)dx");
CL_LISPIFY_NAME(code_literal);
DOCGROUP(clasp);
CL_DEFUN core::T_sp code_literal_ref(ObjectFile_sp code, size_t idx) {
  core::T_O** literals = (core::T_O**)(code->literalsStart());
  core::T_sp ret((gc::Tagged)(literals[idx]));
  return ret;
}

CL_DOCSTRING(R"dx(Return the address of an element from the Code object's literals vector. WARNING: Does not check bound.)dx");
CL_LISPIFY_NAME(code_literal_address);
DOCGROUP(clasp);
CL_DEFUN core::T_sp code_literal_address(ObjectFile_sp code, size_t idx) {
  core::T_O** literals = (core::T_O**)(code->literalsStart());
  void* ptr = (void*)&literals[idx];
  core::T_sp val = core::Integer_O::create((uint64_t)ptr);
  return val;
}


CL_LISPIFY_NAME("CODE-LITERAL");
CL_DEFUN_SETF core::T_sp code_literal_set(core::T_sp lit,
                                          ObjectFile_sp code, size_t idx) {
  core::T_O** literals = (core::T_O**)(code->literalsStart());
  literals[idx] = lit.raw_();
  return lit;
}

}; // namespace llvmo

namespace llvmo {

Library_sp Library_O::make(bool executable, gctools::clasp_ptr_t start, gctools::clasp_ptr_t end, uintptr_t vtableStart,
                           uintptr_t vtableEnd, const std::string& name) {
  auto lib = gctools::GC<Library_O>::allocate(executable, start, end, vtableStart, vtableEnd);
  lib->_Name = core::SimpleBaseString_O::make(name);
  return lib;
}

std::string Library_O::filename() const { return this->_Name->get_std_string(); }

CL_DEFMETHOD DWARFContext_sp Library_O::getDwarfContext() {
  if (this->_DWARFContext.unboundp()) {
    std::string execPath = this->filename();
    // Create a MemoryBuffer from the executable file
    ErrorOr<std::unique_ptr<MemoryBuffer>> fileBuf = MemoryBuffer::getFile(execPath);
    if (!fileBuf) {
      std::cerr << "Error: " << fileBuf.getError().message() << "\n";
    }

    // Get the file format based on the file extension
    llvm::StringRef ext = llvm::sys::path::extension(execPath);
    llvm::file_magic format = llvm::identify_magic(ext);

    // Create an ObjectFile based on the file format
    Expected<std::unique_ptr<llvm::object::ObjectFile>> objOrErr =
        llvm::object::ObjectFile::createObjectFile(fileBuf.get()->getMemBufferRef(), format);
    if (!objOrErr) {
      std::cerr << "Error: " << toString(objOrErr.takeError()) << "\n";
    }
    std::unique_ptr<llvm::object::ObjectFile> obj = std::move(objOrErr.get());

    // Create a DWARFContext from the ObjectFile
    std::unique_ptr<llvm::DWARFContext> dwarfContext = llvm::DWARFContext::create(*obj.get());
    if (!dwarfContext) {
      std::cerr << "Error: Failed to create DWARFContext\n";
    }
    // Use the DWARFContext to access debugging information
    // ...

    void* rawFileBuf = fileBuf->release();
    void* rawDwarfContext = dwarfContext.release();
    auto claspFileBuf = gctools::GC<MemoryBuffer_O>::allocate(rawFileBuf);
    auto claspDwarfContext = gctools::GC<DWARFContext_O>::allocate(rawDwarfContext);
    this->_MemoryBuffer = claspFileBuf;
    this->_ObjectFile.swap(obj);
    this->_DWARFContext = claspDwarfContext;
  }
  return this->_DWARFContext;
}

}; // namespace llvmo

namespace llvmo {

/*
 * Identify the Library_O or ObjectFile_O object for the entry-point.
 * 1. Search the _lisp->_AllLibraries list for a Library_O.
 * 2. Treat the entry_point as an interior pointer and look for the ObjectFile_O
 *     object that it corresponds to.
 * 3. If not found check if the address is in a dynamic library and create
 *      a Library_O object for it and add it to the _AllLibraries list
 * 4. If it's not one of the above then we have an entry point that
 *       I didn't think about or a serious error.
 */
core::T_sp identify_code_or_library(gctools::clasp_ptr_t entry_point) {

  //
  // 1. Search the _lisp->_AllLibraries list
  //
  core::T_sp allLibraries = _lisp->_Roots._AllLibraries.load();
  for (core::T_sp cur = allLibraries; cur.consp(); cur = CONS_CDR(cur)) {
    Library_sp lib = gc::As<Library_sp>(CONS_CAR(cur));
    if (lib->_Start <= entry_point && entry_point < lib->_End) {
      return lib;
    }
  }

  //
  // 2. Treat the entry_point like an interior pointer and lookup the base Code_sp object
  //
  ObjectFile_sp of;
  bool foundBase = llvmo::lookupObjectFileFromEntryPoint((uintptr_t)entry_point, of);
  if (foundBase) {
    return of;
  }

  //
  // 3. Look the entry point up in the dlopen libraries.
  //    If we find it, push an entry into the _lisp->_AllLibraries list

  gctools::clasp_ptr_t start, end;
  uintptr_t vtableStart, vtableEnd;
  std::string libraryName;
  bool isExecutable;
  bool foundLibrary = core::lookup_address_in_library(reinterpret_cast<gctools::clasp_ptr_t>(entry_point), start, end, libraryName,
                                                      isExecutable, vtableStart, vtableEnd);
  if (foundLibrary) {
    Library_sp newlib = Library_O::make(isExecutable, reinterpret_cast<gctools::clasp_ptr_t>(start),
                                        reinterpret_cast<gctools::clasp_ptr_t>(end), vtableStart, vtableEnd, libraryName);
    core::T_sp expected;
    core::Cons_sp entry = core::Cons_O::create(newlib, nil<core::T_O>());
    do {
      expected = _lisp->_Roots._AllLibraries.load();
      entry->rplacd(expected);
    } while (!_lisp->_Roots._AllLibraries.compare_exchange_weak(expected, entry));
    return newlib;
  }

  //
  // 4. We have hit an unidentifiable entry_point - what is it
  SIMPLE_ERROR("We have hit an unidentifiable entry_point at {} - figure out what it is", (void*)entry_point);
}

}; // namespace llvmo

namespace llvmo {

/*! Return true if the pointer is to a general entry-point redirect */
bool general_entry_point_redirect_p(void* ep) {
#if NUMBER_OF_ENTRY_POINTS > 8
#error "Fix the code below if there are more than 8 entry points"
#endif
  if (ep == (void*)general_entry_point_redirect_0 || ep == (void*)general_entry_point_redirect_1 ||
      ep == (void*)general_entry_point_redirect_2 || ep == (void*)general_entry_point_redirect_3 ||
      ep == (void*)general_entry_point_redirect_4 || ep == (void*)general_entry_point_redirect_5 ||
      ep == (void*)general_entry_point_redirect_6 || ep == (void*)general_entry_point_redirect_7)
    return true;
  return false;
}

void ObjectFile_O::validateEntryPoint(const core::ClaspXepFunction& entryPoint) {
  for (size_t ii = 0; ii < core::ClaspXepFunction::Entries; ii++) {
    void* ep = (void*)entryPoint[ii];
    if (general_entry_point_redirect_p(ep))
      continue;
    if (!(this->codeStart() <= (uintptr_t)ep && (uintptr_t)ep < this->codeEnd())) {
      printf("%s:%d:%s Entrypoint %p is not bounded by the codeStart %p and codeEnd %p\n", __FILE__, __LINE__, __FUNCTION__,
             (void*)entryPoint[ii], (void*)this->codeStart(), (void*)this->codeEnd());
      abort();
    }
  }
}

CL_DOCSTRING(
    R"dx(For an instruction pointer inside of code generated from an object file - return the relative address (the sectioned address))dx");
CL_LISPIFY_NAME(object_file_sectioned_address);
DOCGROUP(clasp);
CL_DEFUN SectionedAddress_sp object_file_sectioned_address(void* instruction_pointer, ObjectFile_sp ofi, bool verbose) {
  // Here is the info for the SectionedAddress
  uintptr_t sectionID = ofi->_TextSectionId;
  uintptr_t offset = ((char*)instruction_pointer - (char*)ofi->_TextSectionStart);
  SectionedAddress_sp sectioned_address = SectionedAddress_O::create(sectionID, offset);
  // now the object file
  if (verbose) {
    core::clasp_write_string(fmt::format("faso-file: {}  object-file-position: {}  objectID: {}\n", _rep_(ofi->_FasoName),
                                         ofi->_FasoIndex, ofi->_ObjectId));
    core::clasp_write_string(fmt::format("SectionID: {}    memory offset: {}\n", ofi->_FasoIndex, offset));
  }
  return sectioned_address;
}

CL_DOCSTRING(R"dx(Identify the object file whose generated code range contains the instruction-pointer.)dx");
CL_DOCSTRING_LONG(R"dx(
Return NIL if none or (values offset-from-start object-file). 
The index-from-start is the number of bytes of the instruction-pointer from the start of the code range.)dx")
CL_LISPIFY_NAME(object_file_for_instruction_pointer);
DOCGROUP(clasp);
CL_DEFUN core::T_mv object_file_for_instruction_pointer(void* instruction_pointer, bool verbose) {
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s entered looking for instruction_pointer@%p search Code_O objects\n", __FILE__, __LINE__,
                            __FUNCTION__, instruction_pointer));
  core::T_sp cur = _lisp->_Roots._AllObjectFiles.load();
  size_t count = 0;
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s instruction_pointer = %p  object_files = %p\n", __FILE__, __LINE__, __FUNCTION__,
                            (char*)instruction_pointer, cur.raw_()));
  if ((cur.nilp()) && verbose) {
    core::clasp_write_string(
        fmt::format("No object files registered - cannot find object file for address {}\n", (void*)instruction_pointer));
  }
  while (cur.consp()) {
    core::T_sp car = CONS_CAR(gc::As_unsafe<core::Cons_sp>(cur));
    ObjectFile_sp ofi = gc::As<ObjectFile_sp>(car);
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Looking at object file _text %p to %p\n", __FILE__, __LINE__, __FUNCTION__,
                              ofi->_TextSectionStart, ofi->_TextSectionEnd));
    if ((char*)instruction_pointer >= (char*)ofi->_TextSectionStart && (char*)instruction_pointer < ((char*)ofi->_TextSectionEnd)) {
      core::T_sp sectionedAddress = object_file_sectioned_address(instruction_pointer, ofi, verbose);
      return Values(sectionedAddress, ofi);
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
    if (((char*)ip >= (char*)ofi->_TextSectionStart) && ((char*)ip < (char*)ofi->_TextSectionEnd))
      return ofi;
    cur = CONS_CDR(gc::As_unsafe<core::Cons_sp>(cur));
  }
  return nil<core::T_O>();
}

CL_LISPIFY_NAME(release_object_files);
DOCGROUP(clasp);
CL_DEFUN void release_object_files() {
  _lisp->_Roots._AllObjectFiles.store(nil<core::T_O>());
  core::clasp_write_string("ObjectFiles have been released\n");
}

CL_LISPIFY_NAME(number_of_object_files);
DOCGROUP(clasp);
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
DOCGROUP(clasp);
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
DOCGROUP(clasp);
CL_DEFUN core::T_sp all_object_files() {
  core::T_sp cur = _lisp->_Roots._AllObjectFiles.load();
  return cur;
}

extern "C" {
struct jit_code_entry {
  struct jit_code_entry* next_entry;
  struct jit_code_entry* prev_entry;
  const char* symfile_addr;
  uint64_t symfile_size;
};

struct jit_descriptor {
  uint32_t version;
  /* This type should be jit_actions_t, but we use uint32_t
     to be explicit about the bitwidth.  */
  uint32_t action_flag;
  struct jit_code_entry* relevant_entry;
  struct jit_code_entry* first_entry;
};

extern struct jit_descriptor __jit_debug_descriptor;
};

CL_DOCSTRING(R"dx(Generate a list of jit_code_entry objects)dx");
CL_LISPIFY_NAME(jit_code_entries);
DOCGROUP(clasp);
CL_DEFUN core::T_sp jit_code_entries() {
  jit_code_entry* jce = __jit_debug_descriptor.first_entry;
  ql::list ll;
  while (jce) {
    core::T_sp obj =
        core::Cons_O::create(core::Pointer_O::create((void*)jce->symfile_addr), core::Integer_O::create(jce->symfile_size));
    ll << obj;
    jce = jce->next_entry;
  }
  return ll.result();
}

CL_DOCSTRING(R"dx(Generate a list of JITted symbols to /tmp/perf-<pid>.map)dx");
DOCGROUP(clasp);
CL_DEFUN void ext__generate_perf_map() {
  stringstream ss;
  ss << "/tmp/perf-" << getpid() << ".map";
  core::clasp_write_string(fmt::format("Writing to {}\n", ss.str()));
  FILE* fout = fopen(ss.str().c_str(), "w");
  jit_code_entry* jce = __jit_debug_descriptor.first_entry;
  ql::list ll;
  while (jce) {
    const char* of_start = jce->symfile_addr;
    size_t of_length = jce->symfile_size;
    llvm::StringRef sbuffer((const char*)of_start, of_length);
    stringstream ss;
    ss << "buffer" << (void*)of_start;
    std::string mem = ss.str();
    llvm::StringRef name(mem);
    std::unique_ptr<llvm::MemoryBuffer> memoryBuffer(llvm::MemoryBuffer::getMemBuffer(sbuffer, name, false));
    llvm::MemoryBufferRef memr = *(memoryBuffer);
    llvm::Expected<std::unique_ptr<llvm::object::ObjectFile>> obj = llvm::object::ObjectFile::createObjectFile(memr);
    for (auto sym : (*obj)->symbols()) {
      if ((*sym.getAddress()) != 0)
        fprintf(fout, "%lX %lX %s\n", (uintptr_t)(*sym.getAddress()), (size_t)sym.getCommonSize(), (sym).getName()->str().c_str());
    }
    jce = jce->next_entry;
  }
  fclose(fout);
}

CL_LISPIFY_NAME(describe_code);
DOCGROUP(clasp);
CL_DEFUN void describe_code() {
  core::T_sp cur = _lisp->_Roots._AllObjectFiles.load();
  size_t count = 0;
  size_t sz = 0;
  size_t goodStackmaps = 0;
  while (cur.consp()) {
    ObjectFile_sp ofi = gc::As<ObjectFile_sp>(CONS_CAR(cur));
    core::clasp_write_string(fmt::format("ObjectFile start: {}  size: {}\n", (void*)ofi->_MemoryBuffer->getBufferStart(),
                                         ofi->_MemoryBuffer->getBufferSize()));
    uintptr_t codeStart = (uintptr_t)&ofi->_DataCode[0];
    uintptr_t codeEnd = (uintptr_t)&ofi->_DataCode[ofi->_DataCode.size()];
    core::clasp_write_string(fmt::format("   corresponding Code_O object: {}  code range: {} - {}\n", (void*)ofi.raw_(),
                                         (void*)codeStart, (void*)codeEnd));
    uintptr_t stackmapStart = (uintptr_t)ofi->_StackmapStart;
    uintptr_t stackmapEnd = (uintptr_t)ofi->_StackmapStart + ofi->_StackmapSize;
    if (stackmapStart <= stackmapEnd) {
      if (codeStart <= stackmapStart && stackmapEnd <= codeEnd) {
        StackmapHeader* header = (StackmapHeader*)stackmapStart;
        if (header->_version == 3 && header->_reserved0 == 0 && header->_reserved1 == 0) {
          goodStackmaps++;
        }
        core::clasp_write_string(
            fmt::format("      The stackmap {} {} is within the code region\n", (void*)stackmapStart, (void*)stackmapEnd));
      } else {
        core::clasp_write_string(
            fmt::format(" ERROR     The stackmap {} {} is NOT within the code region\n", (void*)stackmapStart, (void*)stackmapEnd));
      }
    } else {
      core::clasp_write_string(
          fmt::format(" ERROR     The stackmap {} {} is not a real memory region\n", (void*)stackmapStart, (void*)stackmapEnd));
    }
    sz += ofi->_MemoryBuffer->getBufferSize();
    count++;
    cur = CONS_CDR(cur);
  }
  core::clasp_write_string(fmt::format("Total number of object files: {}\n", count));
  core::clasp_write_string(fmt::format("  Total size of object files: {}\n", sz));
  core::clasp_write_string(fmt::format("             Valid stackmaps: {}\n", goodStackmaps));
}

CL_LAMBDA(&optional (size 8388608));
CL_DEFUN CodeBlock_sp llvm_sys__make_code_block(size_t size) { return CodeBlock_O::make<gctools::RuntimeStage>(size); };

CodeBlock_O::~CodeBlock_O() {
  printf("%s:%d:%s Trying to destruct CodeBlock_O - what do I do?\n", __FILE__, __LINE__, __FUNCTION__);
}

bool CodeBlock_O::calculate(BasicLayout& BL) {
  uintptr_t headOffset = this->_HeadOffset;
  uintptr_t tailOffset = this->_TailOffset;
  for (auto& KV : BL.segments()) {
    auto allocGroup = KV.first;
    uintptr_t protFlags = toSysMemoryProtectionFlags(allocGroup.getMemProt());
    auto& Seg = KV.second;
#ifdef DEBUG_OBJECT_FILES
    std::string back;
    llvm::raw_string_ostream ss(back);
    llvm::jitlink::operator<<(ss, allocGroup);
    DEBUG_OBJECT_FILES_PRINT(
        ("%s:%d:%s ------------- BL.segments() iteration AllocGroup = %s\n", __FILE__, __LINE__, __FUNCTION__, ss.str().c_str()));
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s    Seg.ContentSize -> %lu  Seg.ZeroFillSize -> %lu Seg.Alignment.value() -> %lu\n",
                              __FILE__, __LINE__, __FUNCTION__, Seg.ContentSize, (unsigned long)Seg.ZeroFillSize,
                              (unsigned long)Seg.Alignment.value()));
#endif
    uint64_t ZeroFillStart = Seg.ContentSize;
    size_t SegmentSize = (uintptr_t)gctools::AlignUp(ZeroFillStart + Seg.ZeroFillSize, Seg.Alignment.value());
    uintptr_t baseOffset;
    void* base;
    uintptr_t prevHeadOffset = headOffset;
    uintptr_t prevTailOffset = tailOffset;
    const char* allocKind = "unk";
    if ((llvm::sys::Memory::MF_RWE_MASK & protFlags) == (llvm::sys::Memory::MF_READ | llvm::sys::Memory::MF_WRITE)) {
      baseOffset = this->calculateHeadOffset(SegmentSize, Seg.Alignment.value(), headOffset);
      allocKind = "head";
    } else if ((llvm::sys::Memory::MF_RWE_MASK & protFlags) == (llvm::sys::Memory::MF_READ | llvm::sys::Memory::MF_EXEC)) {
      baseOffset = this->calculateTailOffset(SegmentSize, Seg.Alignment.value(), tailOffset);
      allocKind = "tail";
    } else {
      baseOffset = this->calculateHeadOffset(SegmentSize, Seg.Alignment.value(), headOffset);
      allocKind = "head";
    }
    base = this->address(baseOffset);
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s calculated %s from the base = %p - %p = %lu bytes \n", __FILE__, __LINE__, __FUNCTION__,
                              allocKind, base, (void*)((uintptr_t)base + SegmentSize), SegmentSize));
#if 0
    printf("%s:%d:%s allocKind %s base = %10lu _Head: %10lu  _Tail: %10lu  base: %p GC_base: %p\n", __FILE__, __LINE__, __FUNCTION__, allocKind, baseOffset, headOffset, tailOffset, base, GC_base(base) );
    if (!(prevHeadOffset<= baseOffset && baseOffset<=prevTailOffset)) {
      printf("       The baseOffset is out of range\n" );
    }
#endif
    Seg.Addr = (llvm::orc::ExecutorAddr)(uintptr_t)base;
    Seg.WorkingMem = jitTargetAddressToPointer<char*>((llvm::JITTargetAddress)base);
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s     wrote into Seg @ %p\n", __FILE__, __LINE__, __FUNCTION__, &Seg));
  }
  uintptr_t headAddress = (uintptr_t)this->address(headOffset);
  uintptr_t tailAddress = (uintptr_t)this->address(tailOffset);
  //
  // If the head overruns the tail then we don't have enough memory for this code.
  //
  if (headAddress >= tailAddress) return false;

  //
  // If the headAddress and tailAddress are on the SAME PAGE - then we don't have enough memory for this code
  //
  size_t PageSize = getpagesize();
  if ((headAddress / PageSize) == (tailAddress / PageSize))
    return false;

  //
  // The allocation will fit - let's lock it in
  //
  this->_HeadOffset = headOffset;
  this->_TailOffset = tailOffset;
  return true;
}

void* CodeBlock_O::calculateHead(uintptr_t size, uint32_t align, uintptr_t& headOffset) {
  const unsigned char* head = this->address(0) + headOffset;
  head = (const unsigned char*)gctools::AlignUp((uintptr_t)head, align);
  headOffset = (uintptr_t)head - (uintptr_t)this->address(0) + size;
  return (void*)head;
}

uintptr_t CodeBlock_O::calculateHeadOffset(uintptr_t size, uint32_t align, uintptr_t& headOffset) {
  uintptr_t head = headOffset;
  head = (uintptr_t)gctools::AlignUp((uintptr_t)head, align);
  headOffset = (uintptr_t)head + size;
  return head;
}

void* CodeBlock_O::calculateTail(uintptr_t size, uint32_t align, uintptr_t& tailOffset) {
  const unsigned char* tail = this->address(0) + tailOffset;
  tail = (const unsigned char*)gctools::AlignDown((uintptr_t)tail - size, align);
  tailOffset = (uintptr_t)tail - (uintptr_t)this->address(0);
  return (void*)tail;
}

uintptr_t CodeBlock_O::calculateTailOffset(uintptr_t size, uint32_t align, uintptr_t& tailOffset) {
  uintptr_t tail = tailOffset;
  tail = (uintptr_t)gctools::AlignDown((uintptr_t)tail - size, align);
  tailOffset = (uintptr_t)tail;
  return tail;
}

std::string CodeBlock_O::__repr__() const {
  stringstream ss;
  ss << "#<CODE-BLOCK";
  ss << " :DataCode " << (void*)&(this->_DataCode[0]);
  ss << " :DataCodeSize " << DefaultSize;
  ss << " @ " << (void*)this;
  ss << ">";
  return ss.str();
}

void CodeBlock_O::describe() const { printf("%s:%d:%s entered\n", __FILE__, __LINE__, __FUNCTION__); }

size_t countObjectFileNames(const std::string& name) {
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Lookup name %s\n", __FILE__, __LINE__, __FUNCTION__, name.c_str()));
  size_t count = 0;
  core::T_sp cur = _lisp->_Roots._AllObjectFiles.load();
  while (cur.consp()) {
    ObjectFile_sp of = gc::As<ObjectFile_sp>(CONS_CAR(cur));
    const char* codeNameStart = (const char*)of->_CodeName->_Data.data();
    if (of->_CodeName->length() == name.size()) {
      if (memcmp(codeNameStart, name.data(), name.size()) == 0) {
        count++;
      }
    }
    cur = CONS_CDR(cur);
  }
  return count;
};

CL_DEFUN core::T_sp llvm_sys__allObjectFileNames() {
  core::T_sp result = nil<core::T_O>();
  core::T_sp cur = _lisp->_Roots._AllObjectFiles.load();
  while (cur.consp()) {
    ObjectFile_sp of = gc::As<ObjectFile_sp>(CONS_CAR(cur));
    result = core::Cons_O::create(of->_CodeName, result);
    cur = CONS_CDR(cur);
  }
  return result;
};

std::string createIRModuleObjectFileName(size_t startupId, std::string& prefix) {
  stringstream ss;
  ss << "ClaspModule" << startupId;
  prefix = ss.str();
  ss << "-jitted-objectbuffer";
  return ss.str();
}

bool verifyIRModuleObjectFileName(const std::string& name) {
  if (name.size() < std::string("ClaspModule-jitted-objectbuffer").size())
    return false;
  if (name.substr(0, 11) == "ClaspModule") {
    if (name.find("jitted-objectbuffer") == std::string::npos) {
      SIMPLE_ERROR("The IRModule name {} must have the form ClaspModule#-jitted-objectbuffer...\n"
                   "This is assembled in llvm CompileUtils.cpp SimpleCompiler::operator() where it appends -jitted-objectbuffer to "
                   "the Module Identifier - if that changes then this test will fail and we need to update "
                   "createIRModuleObjectFileName to mimic the new name construction",
                   name);
    }
    return true;
  }
  return false;
}

ObjectFile_sp lookupObjectFile(const std::string& name) {
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Lookup name %s\n", __FILE__, __LINE__, __FUNCTION__, name.c_str()));
  // If it matches an IRModule name then it must have a certain structure
  verifyIRModuleObjectFileName(name);
  core::T_sp ofs = _lisp->_Roots._AllObjectFiles.load();
  core::T_sp cur = ofs;
  while (cur.consp()) {
    ObjectFile_sp of = gc::As<ObjectFile_sp>(CONS_CAR(cur));
    const char* codeNameStart = (const char*)of->_CodeName->_Data.data();
    if (of->_CodeName->length() == name.size()) {
      if (memcmp(codeNameStart, name.data(), name.size()) == 0) {
        DEBUG_OBJECT_FILES_PRINT(
            ("%s:%d:%s Returning ObjectFile_sp %p badge 0x%0x\n", __FILE__, __LINE__, __FUNCTION__, of.raw_(), lisp_badge(of)));
        return of;
      }
    }
    cur = CONS_CDR(cur);
  }
  stringstream ss;
  size_t num = 0;
  cur = ofs;
  while (cur.consp()) {
    ObjectFile_sp of = gc::As<ObjectFile_sp>(CONS_CAR(cur));
    ss << of->_CodeName->get_std_string() << " ";
    ++num;
    cur = CONS_CDR(cur);
  }
  printf("%s:%d:%s Could not find object file %s - %lu available-object-file names: %s\n", __FILE__, __LINE__, __FUNCTION__,
         name.c_str(), num, ss.str().c_str());
  SIMPLE_ERROR("Could not find object file {} - {} available object file names: {}", name, num, ss.str());
};

bool lookupObjectFileFromEntryPoint(uintptr_t entry_point, ObjectFile_sp& objectFile) {
  core::List_sp ofs = _lisp->_Roots._AllObjectFiles.load();
  for (auto cur : ofs) {
    ObjectFile_sp of = gc::As<ObjectFile_sp>(CONS_CAR(cur));
    if ((uintptr_t)of->_TextSectionStart <= entry_point && entry_point < (uintptr_t)of->_TextSectionEnd) {
      objectFile = of;
      return true;
    }
  }
  return false;
};

void validateEntryPoint(core::T_sp code, const core::ClaspXepFunction& entry_point) {
  // Nothing for now
}
void validateEntryPoint(core::T_sp code, const core::ClaspCoreFunction& entry_point) {
  // Nothing for now
}
void validateEntryPoint(core::T_sp code, uintptr_t entry_point) {
  // Nothing for now
}

uintptr_t codeStart(core::T_sp codeOrLibrary) {
  if (gc::IsA<Library_sp>(codeOrLibrary)) {
    Library_sp library = gc::As_unsafe<Library_sp>(codeOrLibrary);
    return library->codeStart();
  } else if (gc::IsA<ObjectFile_sp>(codeOrLibrary)) {
    ObjectFile_sp of = gc::As_unsafe<ObjectFile_sp>(codeOrLibrary);
    return of->codeStart();
  }
  SIMPLE_ERROR("{} must be a Library or ObjectFile", _rep_(codeOrLibrary));
}

CL_DEFUN core::T_sp llvm_sys__executable_and_libraries() {
  core::T_sp allLibraries = _lisp->_Roots._AllLibraries.load();
  return allLibraries;
}

}; // namespace llvmo
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
