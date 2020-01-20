/*
    File: compiler.cc
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

// #define EXPOSE_DLOPEN
// #define EXPOSE_DLLOAD
//#define DEBUG_LEVEL_FULL

#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <dlfcn.h>
#ifdef _TARGET_OS_DARWIN
#import <mach-o/dyld.h>
#endif

#include <clasp/core/foundation.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/cxxObject.h>
#include <clasp/core/record.h>
#include <clasp/core/lisp.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/sort.h>
#include <clasp/core/environment.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/lightProfiler.h>
#include <clasp/core/designators.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/array.h>
#include <clasp/core/character.h>
#include <clasp/core/functor.h>
#include <clasp/core/compiler.h>
#include <clasp/core/sequence.h>
#include <clasp/core/posixTime.h>
#include <clasp/core/debugger.h>
#include <clasp/core/pathname.h>
#include <clasp/core/pointer.h>
#include <clasp/core/posixTime.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/environment.h>
#include <clasp/core/cleavirPrimopsPackage.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/pointer.h>
#include <clasp/core/environment.h>
#include <clasp/llvmo/intrinsics.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/core/wrappers.h>


namespace core {


std::atomic<size_t> global_jit_compile_counter;

CL_DEFUN void core__increment_jit_compile_counter() {
  global_jit_compile_counter++;
}

CL_DEFUN size_t core__jit_compile_counter() {
  return global_jit_compile_counter;
}


MaybeDebugStartup::MaybeDebugStartup(void* fp, const char* n) : fptr(fp), start_dispatcher_count(0) {
  if (n) this->name = n;
  this->start_jit_compile_counter = global_jit_compile_counter;
  if (core::_sym_STARdebugStartupSTAR->symbolValue().notnilp()) {
    this->start = PosixTime_O::createNow();
    if (clos::_sym_dispatcher_count->fboundp()) {
      core::T_sp nu = core::eval::funcall(clos::_sym_dispatcher_count);
      this->start_dispatcher_count = nu.unsafe_fixnum();
    } else {
      this->start_dispatcher_count = 0;
    }
  }
};

MaybeDebugStartup::~MaybeDebugStartup() {
  if (core::_sym_STARdebugStartupSTAR->symbolValue().notnilp()
      && this->start) {
    PosixTime_sp end = PosixTime_O::createNow();
    PosixTimeDuration_sp diff = end->sub(this->start);
    mpz_class ms = diff->totalMicroseconds();
    size_t end_dispatcher_count = 0;
    
    if (clos::_sym_dispatcher_count->fboundp()) {
      core::T_sp nu = core::eval::funcall(clos::_sym_dispatcher_count);
      end_dispatcher_count = nu.unsafe_fixnum();
    }
    size_t dispatcher_delta = end_dispatcher_count - this->start_dispatcher_count;
    stringstream name_;
    if (this->name!="") name_ << this->name << " ";
    Dl_info di;
    dladdr((void*)this->fptr,&di);
    name_ << di.dli_sname;
    if (name_.str() == "") name_ << (void*)this->fptr;
    printf("%s us %zu gfds %zu jits: %s\n", _rep_(Integer_O::create(ms)).c_str(), dispatcher_delta, (global_jit_compile_counter-this->start_jit_compile_counter),name_.str().c_str());
  }
}

};



namespace core {

#define INITIALIZER_CAPACITY_INIT 128
#define INITIALIZER_CAPACITY_MULTIPLIER 2
size_t global_initializer_capacity = 0;
size_t global_initializer_count = 0;
InitializerFunction* global_initializer_functions = NULL;

void register_initializer_function(InitializerFunction fptr)
{
//  printf("%s:%d In register_initializer_function --> %p\n", __FILE__, __LINE__, fptr);
  if ( global_initializer_functions == NULL ) {
    global_initializer_capacity = INITIALIZER_CAPACITY_INIT;
    global_initializer_count = 0;
    global_initializer_functions = (InitializerFunction*)malloc(global_initializer_capacity*sizeof(InitializerFunction));
  } else {
    if ( global_initializer_count == global_initializer_capacity ) {
      global_initializer_capacity = global_initializer_capacity*INITIALIZER_CAPACITY_MULTIPLIER;
      global_initializer_functions = (InitializerFunction*)realloc(global_initializer_functions,global_initializer_capacity*sizeof(InitializerFunction));
    }
  }
  global_initializer_functions[global_initializer_count] = fptr;
  global_initializer_count++;
};

/*! Return the number of initializer_functions that are waiting to be run*/
size_t initializer_functions_are_waiting()
{
//  printf("%s:%d initializer_functions_are_waiting returning %" PRu "\n", __FILE__, __LINE__, global_initializer_count );
  return global_initializer_count;
};

/*! Invoke the initializer functions and clear the array of initializer functions */
void initializer_functions_invoke()
{
  if (global_initializer_count>0) {
#if 0
    printf("%s:%d In initializer_functions_invoke - there are %" PRu " initializer functions\n", __FILE__, __LINE__, global_initializer_count );
    for ( size_t i = 0; i<global_initializer_count; ++i ) {
      InitializerFunction fn = global_initializer_functions[i];
      printf("%s:%d     Initializer fn[%" PRu "]@%p\n", __FILE__, __LINE__, i, fn );
    }
    printf("%s:%d Starting to call the initializer functions\n", __FILE__, __LINE__ );
#endif
    for ( size_t i = 0; i<global_initializer_count; ++i ) {
      InitializerFunction fn = global_initializer_functions[i];
//      printf("%s:%d     About to invoke fn@%p\n", __FILE__, __LINE__, fn );
      (fn)();
    }
    global_initializer_count = 0;
    global_initializer_capacity = 0;
    free(global_initializer_functions);
    global_initializer_functions = NULL;
//    printf("%s:%d Done with startup_functions_invoke()\n", __FILE__, __LINE__ );
  }
}

};




namespace core {

StartupInfo global_startup;

std::atomic<size_t> global_next_startup_position;

CL_DEFUN size_t core__next_startup_position() {
  return global_next_startup_position++;
}

/*! Static initializers will run and try to register startup functions.
They will do this into the global_startup structure.
Once the main code starts up - this startup info needs to be transfered
to the my_thread thread-local storage.
So right after the my_thread variable is initialized, this must be called for
the main thread. */

void transfer_StartupInfo_to_my_thread() {
  if (!my_thread) {
    printf("%s:%d my_thread is NULL - you cannot transfer StartupInfo\n", __FILE__, __LINE__ );
  }
  my_thread->_Startup = global_startup;
}

void register_startup_function(size_t position, fnStartUp fptr)
{
#ifdef DEBUG_STARTUP
  printf("%s:%d In register_startup_function --> %p\n", __FILE__, __LINE__, fptr);
#endif
  StartupInfo* startup = NULL;
  // if my_thread is defined - then use its startup info
  // otherwise use the global_startup info.
  // This will only happen in cclasp when startup functions
  // are registered from static constructors.
  if (my_thread) {
    startup = &my_thread->_Startup;
  } else {
    startup = &global_startup;
  }
  if ( startup->_functions == NULL ) {
    startup->_capacity = STARTUP_FUNCTION_CAPACITY_INIT;
    startup->_count = 0;
    startup->_functions = (Startup*)malloc(startup->_capacity*sizeof(Startup));
  } else {
    if ( startup->_count == startup->_capacity ) {
      startup->_capacity = startup->_capacity*STARTUP_FUNCTION_CAPACITY_MULTIPLIER;
      startup->_functions = (Startup*)realloc(startup->_functions,startup->_capacity*sizeof(Startup));
    }
  }
  Startup one_startup(position,fptr);
  startup->_functions[startup->_count] = one_startup;
  startup->_count++;
};

/*! Return the number of startup_functions that are waiting to be run*/
size_t startup_functions_are_waiting()
{
#ifdef DEBUG_STARTUP
  printf("%s:%d startup_functions_are_waiting returning %" PRu "\n", __FILE__, __LINE__, my_thread->_Startup._count );
#endif
  return my_thread->_Startup._count;
};

/*! Invoke the startup functions and clear the array of startup functions */
core::T_O* startup_functions_invoke(T_O* literals)
{
  size_t startup_count = 0;
  Startup* startup_functions = NULL;
  {
  // Save the current list
    startup_count = my_thread->_Startup._count;
    startup_functions = my_thread->_Startup._functions;
  // Prepare to accumulate a new list
    my_thread->_Startup._count = 0;
    my_thread->_Startup._capacity = 0;
    my_thread->_Startup._functions = NULL;
  }
  // Invoke the current list
  core::T_O* result = NULL;
  if (startup_count>0) {
    sort::quickSortMemory(startup_functions,0,startup_count);
#ifdef DEBUG_STARTUP
    printf("%s:%d In startup_functions_invoke - there are %" PRsize_t " startup functions\n", __FILE__, __LINE__, startup_count );
    for ( size_t i = 0; i<startup_count; ++i ) {
      Startup& startup = startup_functions[i];
      printf("%s:%d     Startup fn[%" PRsize_t "] -> %p\n", __FILE__, __LINE__, startup._Position, startup._Function );
    }
    printf("%s:%d Starting to call the startup functions\n", __FILE__, __LINE__ );
#endif
    Startup previous(~0,NULL);
    for ( size_t i = 0; i<startup_count; ++i ) {
      Startup& startup = startup_functions[i];
      if (startup._Position == previous._Position) {
        printf("%s:%d At startup there were two adjacent startup functions with the same position value %lu - this could mean a startup order catastrophe\n", __FILE__, __LINE__, startup._Position);
      }
      previous = startup;
#ifdef DEBUG_STARTUP
      printf("%s:%d     About to invoke fn@%p\n", __FILE__, __LINE__, fn );
#endif
//      T_mv result = (fn)(LCC_PASS_MAIN());
      result = (startup._Function)(literals); // invoke the startup function
    }
#ifdef DEBUG_STARTUP
    printf("%s:%d Done with startup_functions_invoke()\n", __FILE__, __LINE__ );
#endif
    free(startup_functions);
  }
  return result;
}


}

extern "C" {

NOINLINE void start_dtrace() {
  printf("%s:%d start_dtrace\n", __FILE__, __LINE__ );
  // Do nothing
};

NOINLINE void stop_dtrace() {
  printf("%s:%d stop_dtrace\n", __FILE__, __LINE__ );
  // do nothing
}
}

namespace core {

NOINLINE CL_DEFUN T_mv core__trigger_dtrace_start(T_sp closure)
{
  start_dtrace();
  return eval::funcall(closure);
}


NOINLINE CL_DEFUN T_sp core__trigger_dtrace_stop()
{
  stop_dtrace();
  return _Nil<T_O>();
}

  


CL_DEFUN void core__startup_functions_invoke(List_sp literals)
{
  startup_functions_invoke((T_O*)literals.raw_());
  printf("%s:%d startup_functions_invoke returned -   this should never happen\n", __FILE__, __LINE__ );
  abort();
};


};

extern "C" {
gctools::return_type wrapped_test(core::T_O* arg0, core::T_O* arg1, core::T_O* arg2 )
{
  printf("%s:%d wrapped_test called (%p, %p, %p)\n", __FILE__, __LINE__, arg0, arg1, arg2 );
  return gctools::return_type();
}

};
namespace core {

int f(Environment_sp &e) {
  (void)e;
  return 1;
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("Print info about booting");
CL_DEFUN void core__help_booting() {
  printf("Useful *features*\n"
         ":clasp-min,  :bclasp, :cclasp  -- Tells Clasp what stage it's in and where to get its init file.\n"
         ":notify-on-compile (core:*notify-on-compile*) - prints messages whenever COMPILE is invoked at startup\n"
         ":trace-startup (core:*trace-startup*) - prints messages and timing for running the main function of the compiled code of each system file at startup\n"
         ":debug-startup (core:*debug-startup*) - prints a message and timing for running each top level function\n"
         "\n"
         "Commands (all in CORE package)\n"
         "(load-system <start> <end> &key interp (system *init-files*))   - Load the system files\n"
         "(compile-min) - Compile a minimal system\n"
         "(compile-full) - Compile a full system\n"
         "(compile-kernel-file filename &key reload load-bitcode recompile)   - Compile a system file and put the bitcode in the correct directory\n"
         "(link-system start end prologue-form epilogue-form &key (system *init-files*)) - Link an image together\n"
         "(default-prologue-form &optional features) - Returns a prologue form for link-system\n"
         "(default-epilogue-form) - Returns an epilogue form for link-system\n");
}


CL_DOCSTRING("Return the rdtsc performance timer value");
CL_DEFUN Fixnum core__rdtsc(){
    unsigned int lo,hi;
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return ((uint64_t)hi << 32) | lo;
}

T_sp varArgsList(int n_args, ...) {
  DEPRECATED();
  va_list ap;
  va_start(ap, n_args);
  Cons_O::CdrType_sp first = _Nil<Cons_O::CdrType_O>();
  Cons_O::CdrType_sp *curP = &first; // gctools::StackRootedPointerToSmartPtr<Cons_O::CdrType_O> cur(&first);
  for (int i = 1; i <= n_args; ++i) {
    T_sp obj = *(va_arg(ap, const T_sp *));
    Cons_sp one = Cons_O::create(obj,_Nil<T_O>());
    *curP = one;          // cur.setPointee(one); // *cur = one;
    curP = one->cdrPtr(); // cur.setPointer(one->cdrPtr()); // cur = one->cdrPtr();
  }
  va_end(ap);
  return first;
}

CL_LAMBDA(object &optional is-function);
CL_DECLARE();
CL_DOCSTRING("mangleName");
CL_DEFUN T_mv core__mangle_name(Symbol_sp sym, bool is_function) {
  SimpleBaseString_sp name;
  if (!is_function) {
    if (sym.nilp())
      name = SimpleBaseString_O::make("CLASP_NIL");
    else if (sym == _lisp->_true())
      name = SimpleBaseString_O::make("CLASP_T");
    else {
      stringstream ss;
      ss << "SYM(" << sym->symbolName()->get_std_string() << ")";
      name = SimpleBaseString_O::make(ss.str());
    }
    return Values(_Nil<T_O>(), name, make_fixnum(0), make_fixnum(CALL_ARGUMENTS_LIMIT));
  }
  Function_sp fsym = coerce::functionDesignator(sym);
  if (gc::IsA<BuiltinClosure_sp>(fsym)) {
    return Values(_lisp->_true(), SimpleBaseString_O::make("Provide-c-func-name"), make_fixnum(0), make_fixnum(CALL_ARGUMENTS_LIMIT));
  }
  return Values(_Nil<T_O>(), SimpleBaseString_O::make("Provide-func-name"), make_fixnum(0), make_fixnum(CALL_ARGUMENTS_LIMIT));
}

CL_LAMBDA("&optional (stage #\\c)");
CL_DECLARE();
CL_DOCSTRING("startupImagePathname - returns a pathname based on *features* :CLASP-MIN, :USE-MPS, :BCLASP");
CL_DEFUN T_sp core__startup_image_pathname(char stage) {
  stringstream ss;
  ss << "app-fasl:" << stage << "clasp-" << VARIANT_NAME << "-image";
  T_sp mode = core::_sym_STARclasp_build_modeSTAR->symbolValue();
  if (mode == kw::_sym_object) {
    ss << ".fasl";
  } else if (mode == kw::_sym_bitcode) {
    ss << ".fasl";
  } else if (mode == kw::_sym_fasl) {
    ss << ".lfasl";
  } else {
    SIMPLE_ERROR(BF("Add support for *clasp-build-mode* = %s") % _rep_(mode));
  }
  String_sp spath = SimpleBaseString_O::make(ss.str());
  Pathname_sp pn = cl__pathname(spath);
  return pn;
};



CL_DEFUN T_sp core__load_object(llvmo::ClaspJIT_sp jit, llvmo::JITDylib& dylib, size_t startupID, T_sp pathDesig, T_sp verbose, T_sp print, T_sp external_format) {
  Pathname_sp path = cl__pathname(pathDesig);
  String_sp fname = gc::As<String_sp>(cl__namestring(cl__probe_file(path)));
  mode_t mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
  int fd = safe_open(fname->get_std_string().c_str(), O_RDONLY, mode);
  size_t bytes = lseek(fd, 0, SEEK_END);
  char* rbuffer = (char*)malloc(bytes);
  size_t start = lseek(fd, 0, SEEK_SET);
  size_t rd = read(fd,rbuffer,bytes);
  if (rd!=bytes) {
    printf("%s:%d Could only read %lu bytes but expected %lu bytes\n", __FILE__, __LINE__, rd, bytes);
  }
  GC_ALLOCATE_VARIADIC(ObjectFile_O,of,(void*)rbuffer,bytes);
  close(fd);
  const char* bbuffer = (const char*)of->_ObjectFilePtr;
  size_t bbytes = of->_ObjectFileSize;
  jit->addObjectFile(bbuffer,bbytes,startupID,dylib);
  return of;
}

struct FasoHeader;

struct ObjectFileInfo {
  size_t    _ObjectID;
  size_t    _StartPage;
  size_t    _NumberOfPages;
  size_t    _ObjectFileSize;
};

struct FasoHeader {
  uint32_t  _Magic;
  uint32_t  _Version;
  size_t    _PageSize;
  size_t    _HeaderPageCount;
  size_t    _NumberOfObjectFiles;
  ObjectFileInfo     _ObjectFiles[0];

  static size_t calculateSize(size_t numberOfObjectFiles) {
    size_t header = sizeof(FasoHeader);
    size_t entries = numberOfObjectFiles*sizeof(ObjectFileInfo);
    return header+entries;
  }
  static size_t calculateHeaderNumberOfPages(size_t numberOfObjectFiles, size_t pageSize) {
    size_t size = FasoHeader::calculateSize(numberOfObjectFiles);
    size_t numberOfPages = (size+pageSize)/pageSize;
    return numberOfPages;
  }

  size_t calculateObjectFileNumberOfPages(size_t objectFileSize) const {
    size_t numberOfPages = (objectFileSize+this->_PageSize)/this->_PageSize;
    return numberOfPages;
  }
};

#define FASO_MAGIC_NUMBER 0xDEDEBEBE
#define ELF_MAGIC_NUMBER 0x464c457f
#define MACHO_MAGIC_NUMBER 0xfeedfacf

void setup_FasoHeader(FasoHeader* header)
{
  header->_Magic = 0xDEDEBEBE;
  header->_Version = 0;
  header->_PageSize = getpagesize();
}
CL_DEFUN void core__write_faso(T_sp pathDesig, List_sp objectFiles)
{
//  write_bf_stream(BF("Writing FASO file to %s for %d object files\n") % _rep_(pathDesig) % cl__length(objectFiles));
  FasoHeader* header = (FasoHeader*)malloc(FasoHeader::calculateSize(cl__length(objectFiles)));
  setup_FasoHeader(header);
  header->_HeaderPageCount = FasoHeader::calculateHeaderNumberOfPages(cl__length(objectFiles),getpagesize());
  header->_NumberOfObjectFiles = cl__length(objectFiles);
  List_sp cur = objectFiles;
  size_t nextPage = header->_HeaderPageCount;
  for ( size_t ii=0; ii<cl__length(objectFiles); ++ii ) {
    header->_ObjectFiles[ii]._ObjectID = ii;
    header->_ObjectFiles[ii]._StartPage = nextPage;
    Array_sp of = gc::As<Array_sp>(oCar(cur));
    cur = oCdr(cur);
    size_t num_pages = header->calculateObjectFileNumberOfPages(cl__length(of));
    header->_ObjectFiles[ii]._NumberOfPages = num_pages;
    nextPage += num_pages;
    header->_ObjectFiles[ii]._ObjectFileSize = cl__length(of);
#if 0
    write_bf_stream(BF("Object-file %d StartPage = %lu    _NumberOfPages: %lu  _ObjectFileSize: %lu\n")
                    % ii
                    % header->_ObjectFiles[ii]._StartPage
                    % header->_ObjectFiles[ii]._NumberOfPages
                    % header->_ObjectFiles[ii]._ObjectFileSize);
#endif
  }
  String_sp filename = gc::As<String_sp>(cl__namestring(pathDesig));
  FILE* fout = fopen(filename->get_std_string().c_str(),"w");
  // Write header
  size_t header_bytes = FasoHeader::calculateSize(header->_NumberOfObjectFiles);
  fwrite( (const void*)header,header_bytes,1,fout);
//  write_bf_stream(BF("Writing header %lu bytes\n") % header_bytes );

  // Fill out to the end of the page
  size_t pad_bytes = FasoHeader::calculateHeaderNumberOfPages(header->_NumberOfObjectFiles,header->_PageSize)*header->_PageSize-header_bytes;
//  write_bf_stream(BF("Padding header %lu bytes\n") % pad_bytes );
  char empty_byte(0xcc);
  for ( size_t ii=0; ii<pad_bytes; ++ii) {
    fwrite( (const void*)&empty_byte,1,1,fout);
  }
  // Write out each object file
  List_sp ocur = objectFiles;
  for (size_t ofindex=0; ofindex<header->_NumberOfObjectFiles; ofindex++) {
    size_t of_bytes = header->_ObjectFiles[ofindex]._ObjectFileSize;
//    write_bf_stream(BF("Writing object file %d with %lu bytes\n") % ofindex % of_bytes );
    Array_sp of = gc::As<Array_sp>(oCar(ocur));
    ocur = oCdr(ocur);
    fwrite( (const void*)of->rowMajorAddressOfElement_(0),of_bytes,1,fout);
    size_t of_pad_bytes = header->_ObjectFiles[ofindex]._NumberOfPages*header->_PageSize-of_bytes;
//    write_bf_stream(BF("Padding object file %d with %lu bytes\n") % ofindex % of_pad_bytes );
    for (size_t of_padi=0; of_padi<of_pad_bytes; ++of_padi) {
      fwrite( (const void*)&empty_byte,1,1,fout);
    }
  }
  fclose(fout);
};


struct MmapInfo {
  int  _FileDescriptor;
  void* _Memory;
  size_t _ObjectFileAreaStart;
  size_t _ObjectFileAreaSize;
  MmapInfo(int fd,void* mem, size_t start, size_t size) : _FileDescriptor(fd), _Memory(mem), _ObjectFileAreaStart(start), _ObjectFileAreaSize(size) {};
};


struct FasoObjectFileInfo {
  size_t _ObjectID;
  size_t _ObjectFileSize;
  FasoObjectFileInfo(size_t oid, size_t ofs) : _ObjectID(oid), _ObjectFileSize(ofs) {};
};
  
CL_LAMBDA(output-path-designator faso-files &optional (verbose nil))
CL_DEFUN void core__link_faso_files(T_sp outputPathDesig, List_sp fasoFiles, bool verbose) {
//  write_bf_stream(BF("Writing FASO file to %s for %d object files\n") % _rep_(pathDesig) % cl__length(objectFiles));
  std::vector<FasoObjectFileInfo> allObjectFiles;
  std::vector<MmapInfo> mmaps;
  List_sp cur = fasoFiles;
  for ( size_t ii=0; ii<cl__length(fasoFiles); ++ii ) {
    String_sp filename = gc::As<String_sp>(cl__namestring(oCar(cur)));
    cur = oCdr(cur);
    int fd = open(filename->get_std_string().c_str(),O_RDONLY);
    off_t fsize = lseek(fd, 0, SEEK_END);
    lseek(fd,0,SEEK_SET);
    void* memory = mmap(NULL, fsize, PROT_READ, MAP_SHARED|MAP_FILE, fd, 0);
    if (memory==MAP_FAILED) {
      close(fd);
      SIMPLE_ERROR(BF("Could not mmap %s because of %s") % _rep_(filename) % strerror(errno));
    }
    close(fd);
    FasoHeader* header = (FasoHeader*)memory;
    if (header->_Magic == FASO_MAGIC_NUMBER) {
      size_t object0_offset = (header->_HeaderPageCount*header->_PageSize);
      if (verbose) write_bf_stream(BF("object0_offset %lu  fsize-object0_offset %lu bytes\n") % object0_offset % ((size_t)fsize-object0_offset) );
      mmaps.emplace_back(MmapInfo(fd,memory,object0_offset,(size_t)fsize-object0_offset));
      for (size_t ofi = 0; ofi<header->_NumberOfObjectFiles; ++ofi) {
        size_t of_length = header->_ObjectFiles[ofi]._ObjectFileSize;
        if (verbose) write_bf_stream(BF("object file %lu   length: %lu\n") % ofi % of_length);
        FasoObjectFileInfo fofi(header->_ObjectFiles[ofi]._ObjectID,of_length);
        allObjectFiles.emplace_back(fofi);
      }
    } else {
      SIMPLE_ERROR(BF("Illegal and unknown file type - magic number: %X\n") % (size_t)header->_Magic);
    }
  }
  FasoHeader* header = (FasoHeader*)malloc(FasoHeader::calculateSize(allObjectFiles.size()));
  setup_FasoHeader(header);
  header->_HeaderPageCount = FasoHeader::calculateHeaderNumberOfPages(allObjectFiles.size(),getpagesize());
  header->_NumberOfObjectFiles = allObjectFiles.size();
  size_t nextPage = header->_HeaderPageCount;
  for (size_t ofi=0; ofi<allObjectFiles.size(); ofi++ ) {
    header->_ObjectFiles[ofi]._ObjectID = allObjectFiles[ofi]._ObjectID;
    header->_ObjectFiles[ofi]._StartPage = nextPage;
    size_t num_pages = header->calculateObjectFileNumberOfPages(allObjectFiles[ofi]._ObjectFileSize);
    header->_ObjectFiles[ofi]._NumberOfPages = num_pages;
    nextPage += num_pages;
    header->_ObjectFiles[ofi]._ObjectFileSize = allObjectFiles[ofi]._ObjectFileSize;
    if (verbose) write_bf_stream(BF("object file %lu   _StartPage=%lu  _NumberOfPages=%lu   _ObjectFileSize=%lu\n")
                                 % ofi
                                 % header->_ObjectFiles[ofi]._StartPage
                                 % header->_ObjectFiles[ofi]._NumberOfPages
                                 % header->_ObjectFiles[ofi]._ObjectFileSize);

  }
  String_sp filename = gc::As<String_sp>(cl__namestring(outputPathDesig));
  FILE* fout = fopen(filename->get_std_string().c_str(),"w");
  // Write header
  size_t header_bytes = FasoHeader::calculateSize(header->_NumberOfObjectFiles);
  if (verbose) write_bf_stream(BF("Writing %lu bytes of header\n") % header_bytes );
  fwrite( (const void*)header,header_bytes,1,fout);
  
  //  write_bf_stream(BF("Writing header %lu bytes\n") % header_bytes );
  // Fill out to the end of the page
  size_t pad_bytes = FasoHeader::calculateHeaderNumberOfPages(header->_NumberOfObjectFiles,header->_PageSize)*header->_PageSize-header_bytes;
  if (verbose) write_bf_stream(BF("Writing %lu bytes of header for padding\n") % pad_bytes );
  char empty_byte(0xcc);
  for ( size_t ii=0; ii<pad_bytes; ++ii) {
    fwrite( (const void*)&empty_byte,1,1,fout);
  }
  unsigned char pad(0xcc);
  for ( size_t mmi=0; mmi<mmaps.size(); mmi++ ) {
    size_t bytes_to_write = mmaps[mmi]._ObjectFileAreaSize;
    size_t page_size = getpagesize();
    size_t padding = (((size_t)(bytes_to_write+page_size-1)/page_size)*page_size - bytes_to_write);
    if (verbose) write_bf_stream(BF("Writing %lu bytes of object files\n") % bytes_to_write );
    fwrite( (const void*)((const char*)mmaps[mmi]._Memory+mmaps[mmi]._ObjectFileAreaStart), bytes_to_write,1,fout);
    if (verbose) write_bf_stream(BF("Writing %lu bytes of padding\n") % padding );
    for ( size_t pi=0; pi<padding; ++pi ) {
      fwrite(&pad,1,1,fout);
    }
    int res = munmap(mmaps[mmi]._Memory,mmaps[mmi]._ObjectFileAreaSize);
    if (res!=0) {
      SIMPLE_ERROR(BF("Could not munmap memory"));
    }
    close(mmaps[mmi]._FileDescriptor);
  }
  fclose(fout);
}

SYMBOL_EXPORT_SC_(CompPkg, jit_engine);

CL_LAMBDA(path-designator &optional (verbose *load-verbose*) (print t) (external-format :default))
CL_DEFUN core::T_sp core__load_faso(T_sp pathDesig, T_sp verbose, T_sp print, T_sp external_format)
{
  String_sp filename = gc::As<String_sp>(cl__namestring(pathDesig));
  int fd = open(filename->get_std_string().c_str(),O_RDONLY);
  off_t fsize = lseek(fd, 0, SEEK_END);
  lseek(fd,0,SEEK_SET);
  void* memory = mmap(NULL, fsize, PROT_READ, MAP_SHARED|MAP_FILE, fd, 0);
  if (memory==MAP_FAILED) {
    close(fd);
    SIMPLE_ERROR(BF("Could not mmap %s because of %s") % _rep_(pathDesig) % strerror(errno));
  }
  close(fd); // Ok to close file descriptor after mmap
  llvmo::ClaspJIT_sp jit = gc::As<llvmo::ClaspJIT_sp>(eval::funcall(comp::_sym_jit_engine));
  FasoHeader* header = (FasoHeader*)memory;
  llvmo::JITDylib_sp jitDylib;
  for (size_t ofi = 0; ofi<header->_NumberOfObjectFiles; ++ofi) {
    if (header->_ObjectFiles[ofi]._ObjectID==0) {
      jitDylib = jit->createAndRegisterJITDylib(filename->get_std_string());
    }
    void* of_start = (void*)((char*)header + header->_ObjectFiles[ofi]._StartPage*header->_PageSize);
    size_t of_length = header->_ObjectFiles[ofi]._ObjectFileSize;
    if (print.notnilp()) write_bf_stream(BF("%s:%d Adding faso %s object file %d to jit\n") % __FILE__ % __LINE__ % _rep_(filename) % ofi);
    jit->addObjectFile((const char*)of_start,of_length,
                       header->_ObjectFiles[ofi]._ObjectID,
                       *jitDylib->wrappedPtr(),
                       print.notnilp());
  }
  return _lisp->_true();
}

CL_DEFUN core::T_sp core__describe_faso(T_sp pathDesig)
{
  String_sp filename = gc::As<String_sp>(cl__namestring(pathDesig));
  int fd = open(filename->get_std_string().c_str(),O_RDONLY);
  off_t fsize = lseek(fd, 0, SEEK_END);
  lseek(fd,0,SEEK_SET);
  void* memory = mmap(NULL, fsize, PROT_READ, MAP_SHARED|MAP_FILE, fd, 0);
  if (memory==MAP_FAILED) {
    close(fd);
    SIMPLE_ERROR(BF("Could not mmap %s because of %s") % _rep_(pathDesig) % strerror(errno));
  }
  llvmo::ClaspJIT_sp jit = gc::As<llvmo::ClaspJIT_sp>(eval::funcall(comp::_sym_jit_engine));
  FasoHeader* header = (FasoHeader*)memory;
  write_bf_stream(BF("NumberOfObjectFiles %d\n") % header->_NumberOfObjectFiles);
  for (size_t ofi = 0; ofi<header->_NumberOfObjectFiles; ++ofi) {
    size_t ofId =  header->_ObjectFiles[ofi]._ObjectID;
    void* of_start = (void*)((char*)header + header->_ObjectFiles[ofi]._StartPage*header->_PageSize);
    size_t of_length = header->_ObjectFiles[ofi]._ObjectFileSize;
//    write_bf_stream(BF("Adding faso %s object file %d to jit\n") % _rep_(filename) % ofi);
    write_bf_stream(BF("Object file %d  ObjectID: %lu start-page: %lu  bytes: %lu pages: %lu\n")
                    % ofi
                    % header->_ObjectFiles[ofi]._ObjectID
                    % header->_ObjectFiles[ofi]._StartPage
                    % header->_ObjectFiles[ofi]._ObjectFileSize % header->_ObjectFiles[ofi]._NumberOfPages );
  }
  return _lisp->_true();
}

CL_DEFUN core::T_sp core__unpack_faso(T_sp pathDesig, T_sp prefix)
{
  String_sp filename = gc::As<String_sp>(cl__namestring(pathDesig));
  int fd = open(filename->get_std_string().c_str(),O_RDONLY);
  off_t fsize = lseek(fd, 0, SEEK_END);
  lseek(fd,0,SEEK_SET);
  void* memory = mmap(NULL, fsize, PROT_READ, MAP_SHARED|MAP_FILE, fd, 0);
  if (memory==MAP_FAILED) {
    close(fd);
    SIMPLE_ERROR(BF("Could not mmap %s because of %s") % _rep_(pathDesig) % strerror(errno));
  }
  String_sp sprefix = gc::As<String_sp>(prefix);
  string str_prefix = sprefix->get_std_string();
  llvmo::ClaspJIT_sp jit = gc::As<llvmo::ClaspJIT_sp>(eval::funcall(comp::_sym_jit_engine));
  FasoHeader* header = (FasoHeader*)memory;
  write_bf_stream(BF("NumberOfObjectFiles %d\n") % header->_NumberOfObjectFiles);
  for (size_t ofi = 0; ofi<header->_NumberOfObjectFiles; ++ofi) {
    void* of_start = (void*)((char*)header + header->_ObjectFiles[ofi]._StartPage*header->_PageSize);
    size_t of_length = header->_ObjectFiles[ofi]._ObjectFileSize;
    stringstream sfilename;
    sfilename << str_prefix << "-" << "-" << ofi << "-" << header->_ObjectFiles[ofi]._ObjectID << ".o";
    FILE* fout = fopen(sfilename.str().c_str(),"w");
    fwrite(of_start,of_length,1,fout);
    fclose(fout);
//    write_bf_stream(BF("Adding faso %s object file %d to jit\n") % _rep_(filename) % ofi);
    write_bf_stream(BF("Object file %d  start-page: %lu  bytes: %lu pages: %lu\n") % ofi % header->_ObjectFiles[ofi]._StartPage % header->_ObjectFiles[ofi]._ObjectFileSize % header->_ObjectFiles[ofi]._NumberOfPages );
  }
  return _lisp->_true();
}


CL_LAMBDA(name &optional verbose print external-format);
CL_DOCSTRING("load-binary-directory - load a binary file inside the directory");
CL_DEFUN T_mv core__load_binary_directory(T_sp pathDesig, T_sp verbose, T_sp print, T_sp external_format) {
  T_sp tpath;
  String_sp nameStr = gc::As<String_sp>(cl__namestring(cl__probe_file(pathDesig)));
  string name = nameStr->get_std_string();
  if (name[name.size()-1]=='/') {
    // strip last slash
    name = name.substr(0,name.size()-1);
  }
  struct stat stat_path;
  stat(name.c_str(),&stat_path);
  if (S_ISDIR(stat_path.st_mode) != 0) {
    //
    // If the fasl name is a directory it has the structure...
    //  /foo/bar/baz.fasl  change this to...
    //  /foo/bar/baz.fasl/baz.fasl
    size_t slash_pos = name.find_last_of('/',name.size()-1);
    if (slash_pos != std::string::npos) {
      name = name + "/fasl.fasl";
      SimpleBaseString_sp sbspath = SimpleBaseString_O::make(name);
      tpath = cl__pathname(sbspath);
      if (cl__probe_file(tpath).nilp()) {
        SIMPLE_ERROR(BF("Could not find bundle %s") % _rep_(sbspath));
      }
    } else {
      SIMPLE_ERROR(BF("Could not open %s as a fasl file") % name);
    }
  } else {
    SIMPLE_ERROR(BF("Could not find bundle %s") % _rep_(pathDesig));
  }
  return core__load_binary(tpath,verbose,print,external_format);
}




CL_DOCSTRING(R"doc(Return the startup function name and the linkage based on the current dynamic environment.
 The name contains the id as part of itself. )doc");
CL_LAMBDA(&optional (id 0) prefix)
CL_DEFUN T_mv core__startup_function_name_and_linkage(size_t id, core::T_sp prefix)
{
  stringstream ss;
  if (prefix.nilp()) {
    ss << "StartUp";
  } else if (gc::IsA<String_sp>(prefix)) {
    ss << gc::As<String_sp>(prefix)->get_std_string();
  } else {
    SIMPLE_ERROR(BF("Illegal prefix for startup function name: %s") % _rep_(prefix));
  }
  ss << "_";
  ss << id;
  Symbol_sp linkage_type;
  if (comp::_sym_STARcompile_file_parallelSTAR->symbolValue().notnilp()) {
    linkage_type = llvmo::_sym_ExternalLinkage;
  } else {
    linkage_type = llvmo::_sym_InternalLinkage;
  }
  return Values(core::SimpleBaseString_O::make(ss.str()),linkage_type);
};


CL_LAMBDA(name &optional verbose print external-format);
CL_DECLARE();
CL_DOCSTRING("load-binary");
CL_DEFUN T_mv core__load_binary(T_sp pathDesig, T_sp verbose, T_sp print, T_sp external_format) {
  DynamicScopeManager scope(_sym_STARcurrentSourcePosInfoSTAR, SourcePosInfo_O::create(0, 0, 0, 0));
  DynamicScopeManager scope2(cl::_sym_STARreadtableSTAR, cl::_sym_STARreadtableSTAR->symbolValue());
  DynamicScopeManager scope3(cl::_sym_STARpackageSTAR, cl::_sym_STARpackageSTAR->symbolValue());
  if (pathDesig.nilp()) SIMPLE_ERROR(BF("load-binary was about to pass nil to pathname"));
  Pathname_sp path = cl__pathname(pathDesig);
  if (cl__probe_file(path).notnilp())
    goto LOAD;
  path->_Type = SimpleBaseString_O::make("bundle");
  if (cl__probe_file(path).notnilp())
    goto LOAD;
  path->_Type = SimpleBaseString_O::make("fasl");
  if (cl__probe_file(path).notnilp())
    goto LOAD;
  path->_Type = SimpleBaseString_O::make("fasb"); // ECL uses fasb
  if (cl__probe_file(path).notnilp())
    goto LOAD;
  path->_Type = SimpleBaseString_O::make("dylib");
  if (cl__probe_file(path).notnilp())
    goto LOAD;
  path->_Type = SimpleBaseString_O::make("so");
  if (cl__probe_file(path).notnilp())
    goto LOAD;
  SIMPLE_ERROR(BF("Could not find bundle %s") % _rep_(pathDesig));
LOAD:
  String_sp nameStr = gc::As<String_sp>(cl__namestring(cl__probe_file(path)));
  string name = nameStr->get_std_string();

  // Check if we already have this dynamic library loaded
  bool handleIt = if_dynamic_library_loaded_remove(name);
  //	printf("%s:%d Loading dynamic library: %s\n", __FILE__, __LINE__, name.c_str());
  int mode = RTLD_NOW | RTLD_LOCAL; // | RTLD_FIRST;
  void *handle = dlopen(name.c_str(), mode);
  if (handle == NULL) {
    string error = dlerror();
    SIMPLE_ERROR(BF("Error in dlopen: %s") % error);
    //    return (Values(_Nil<T_O>(), SimpleBaseString_O::make(error)));
  }
  // Static constructors must be available and they were run by dlopen
  //   and they registered startup_functions that we will now invoke
  //   to run the top-level forms.
  if (!startup_functions_are_waiting()) {
      printf("%s:%d No static constructors were run - what do we do in this situation????\n", __FILE__, __LINE__ );
      abort();
  }
  add_dynamic_library_using_handle(name,handle);
  Pointer_sp handle_ptr = Pointer_O::create(handle);
  DynamicScopeManager scope4(_sym_STARcurrent_dlopen_handleSTAR, handle_ptr);
  if (startup_functions_are_waiting()) {
    startup_functions_invoke(NULL);
  } else {
    SIMPLE_ERROR(BF("This is not a proper FASL file - there are no startup functions waiting to be invoked"));
  }
  T_mv result;
  return (Values(Pointer_O::create(handle), _Nil<T_O>()));
};


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
std::tuple< void *, string > do_dlopen(const string& str_path, const int n_mode) {
  void * p_handle = nullptr;
  std::string str_error{ "" };

  dlerror(); // clear any previous error

  p_handle = dlopen( str_path.c_str(), n_mode );

  if ( ! p_handle ) {
    str_error = dlerror();
    fprintf( stderr, "%s:%d Could not open %s - error: %s\n",
             __FILE__, __LINE__, str_path.c_str(), str_error.c_str());
  }

  return std::make_tuple( p_handle, str_error );
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
std::tuple< int, string > do_dlclose(void * p_handle) {

  std::string str_error{ "" };
  int n_rc = 0;

  dlerror(); // clear any previous error

  if( ! p_handle ) {
    str_error = "Library handle is invalid (NULL/NIL)!";
    n_rc = -1;
  }
  else {
    n_rc = dlclose( p_handle );

    if ( n_rc != 0 ) {
      str_error = dlerror();
      fprintf( stderr, "%s:%d Could not close dynamic library (handle %p) - error: %s !\n",
             __FILE__, __LINE__, p_handle, str_error.c_str());
    }
  }

  return std::make_tuple( n_rc, str_error );
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
CL_DOCSTRING("dlopen - Open a dynamic library and return the handle. Returns (values returned-value error-message(or nil if no error))");
CL_DEFUN T_mv core__dlopen(T_sp pathDesig) {

  int mode = RTLD_NOW | RTLD_GLOBAL;
  
  if (pathDesig.nilp()) SIMPLE_ERROR(BF("%s was about to pass nil to pathname") % __FUNCTION__);
  Pathname_sp path = cl__pathname(pathDesig);
  string ts0 = gc::As<String_sp>(cl__namestring(path))->get_std_string();

  auto result = do_dlopen( ts0, mode );
  void * handle = std::get<0>( result );

  if( handle == nullptr ) {
    return (Values(_Nil<T_O>(), SimpleBaseString_O::make( get<1>( result ))));
  }
  return (Values(Pointer_O::create(handle), _Nil<T_O>()));
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
std::tuple< void *, string > do_dlsym( void * p_handle, const char * pc_symbol ) {
  std::string str_error{ "" };
  void *p_sym = nullptr;
  dlerror(); // clear any earlier error
  p_sym = dlsym( p_handle, pc_symbol );
  if( p_sym == nullptr ) {
    str_error = dlerror();
    fprintf( stderr, "%s:%d Could not get symbol address in dynamic library (handle %p) - error: %s !\n",
             __FILE__, __LINE__, p_handle, str_error.c_str() );
  }
  return std::make_tuple( p_sym, str_error );
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
CL_DOCSTRING("(dlsym handle name) handle is pointer from dlopen or :rtld-next, :rtld-self, :rtld-default or :rtld-main-only (see dlsym man page) returns ptr or nil if not found.");
CL_DEFUN T_sp core__dlsym(T_sp ohandle, String_sp name) {
  void *handle = NULL;
  if (ohandle.nilp()) {
    SIMPLE_ERROR(BF("Invalid ohandle passed -> nil"));
  } else if (Pointer_sp phandle = ohandle.asOrNull<Pointer_O>()) {
    handle = phandle->ptr();
  } else if (Symbol_sp sym = ohandle.asOrNull<Symbol_O>() ) {
//    printf("%s:%d handle is symbol: %s\n", __FILE__, __LINE__, _rep_(sym).c_str());
    SYMBOL_EXPORT_SC_(KeywordPkg, rtld_default);
    SYMBOL_EXPORT_SC_(KeywordPkg, rtld_next);
    SYMBOL_EXPORT_SC_(KeywordPkg, rtld_self);
    SYMBOL_EXPORT_SC_(KeywordPkg, rtld_main_only);
    if (sym == kw::_sym_rtld_default) {
//      printf("%s:%d handle is RDLD_DEFAULT\n", __FILE__, __LINE__ );
      handle = RTLD_DEFAULT;
//      printf("%s:%d RTLD_DEFAULT = %p\n", __FILE__, __LINE__, RTLD_DEFAULT);
//      printf("%s:%d handle = %p\n", __FILE__, __LINE__, handle);
    } else if (sym == kw::_sym_rtld_next) {
      handle = RTLD_NEXT;
#ifdef _TARGET_OS_DARWIN
    } else if (sym == kw::_sym_rtld_self) //NOT PORTABLE TO LINUX
    {
      handle = RTLD_SELF;
    } else if (sym == kw::_sym_rtld_main_only) {
      handle = RTLD_MAIN_ONLY;
#endif
    } else {
      SIMPLE_ERROR(BF("Illegal keyword[%s] for dlsym - only :rtld-next :rtld-self :rtld-default :rtld-main-only are allowed") % _rep_(sym));
    }
  } else {
    SIMPLE_ERROR(BF("Illegal handle argument[%s] for dlsym only a pointer or :rtld-next :rtld-self :rtld-default :rtld-main-only are allowed") % _rep_(ohandle));
  }
//  printf("%s:%d handle = %p\n", __FILE__, __LINE__, handle);
  string ts = name->get_std_string();
  auto result = do_dlsym(handle, ts.c_str());
  void * p_sym = std::get<0>( result );
  if( p_sym == nullptr ) {
    return ( Values(_Nil<T_O>(), SimpleBaseString_O::make( get<1>( result ))) );
  }
  return ( Values(Pointer_O::create( p_sym ), _Nil<T_O>()) );
}

CL_DOCSTRING("(call dladdr with the address and return nil if not found or the contents of the Dl_info structure as multiple values)");
CL_DEFUN void core__call_dl_main_function(Pointer_sp addr) {
  InitFnPtr mainFunctionPointer = (InitFnPtr)addr->ptr();
  (*mainFunctionPointer)(LCC_PASS_ARGS0_VA_LIST_INITFNPTR());
}

CL_DOCSTRING("(call dladdr with the address and return nil if not found or the contents of the Dl_info structure as multiple values)");
CL_DEFUN T_mv core__dladdr(Pointer_sp addr) {
  uint64_t val = (uint64_t)addr->ptr();
  void *ptr = (void *)val;
  Dl_info info;
  int ret = dladdr(ptr, &info);
  if (!ret) {
    return Values(_Nil<T_O>());
  } else {
    return Values(SimpleBaseString_O::make(info.dli_fname),
                  Pointer_O::create(info.dli_fbase),
                  SimpleBaseString_O::make(info.dli_sname),
                  Pointer_O::create(info.dli_saddr));
  }
}

CL_LAMBDA(form &optional env);
CL_DEFUN T_mv compiler__implicit_compile_hook_default(T_sp form, T_sp env) {
  // Convert the form into a thunk and return like COMPILE does
  LambdaListHandler_sp llh = LambdaListHandler_O::create(0);
  Cons_sp code = Cons_O::create(form, _Nil<T_O>());
  T_sp sourcePosInfo = _Nil<T_O>();
  stringstream ss;
  ss << "repl" << _lisp->nextReplCounter();
  Symbol_sp name = _lisp->intern(ss.str());
  ClosureWithSlots_sp ic = ClosureWithSlots_O::make_interpreted_closure(name,
                                                                        kw::_sym_function,
                                                                        _Nil<T_O>(),
                                                                        llh,
                                                                        _Nil<T_O>(),
                                                                        _Nil<T_O>(),
                                                                        code, env, SOURCE_POS_INFO_FIELDS(sourcePosInfo));
  Function_sp thunk = ic;
  return (thunk->entry.load())(LCC_PASS_ARGS0_ELLIPSIS(thunk.raw_()));
//  return eval::funcall(thunk);
};

};

extern "C" {
__attribute__((noinline)) int callByValue(core::T_sp v1, core::T_sp v2, core::T_sp v3, core::T_sp v4) {
  ASSERT(v1.fixnump());
  ASSERT(v2.fixnump());
  ASSERT(v3.fixnump());
  ASSERT(v4.fixnump());
  int f1 = v1.unsafe_fixnum();
  int f2 = v2.unsafe_fixnum();
  int f3 = v3.unsafe_fixnum();
  int f4 = v4.unsafe_fixnum();
  int res = f1 + f2 + f3 + f4;
  return res;
}

__attribute__((noinline)) int callByPointer(core::T_O *v1, core::T_O *v2, core::T_O *v3, core::T_O *v4) {
  ASSERT(gctools::tagged_fixnump(v1));
  ASSERT(gctools::tagged_fixnump(v2));
  ASSERT(gctools::tagged_fixnump(v3));
  ASSERT(gctools::tagged_fixnump(v4));
  int f1 = gctools::untag_fixnum(v1);
  int f2 = gctools::untag_fixnum(v2);
  int f3 = gctools::untag_fixnum(v3);
  int f4 = gctools::untag_fixnum(v4);
  int res = f1 + f2 + f3 + f4;
  return res;
}

__attribute__((noinline)) int callByConstRef(const core::T_sp &v1, const core::T_sp &v2, const core::T_sp &v3, const core::T_sp &v4) {
  ASSERT(v1.fixnump());
  ASSERT(v2.fixnump());
  ASSERT(v3.fixnump());
  ASSERT(v4.fixnump());
  int f1 = v1.unsafe_fixnum();
  int f2 = v2.unsafe_fixnum();
  int f3 = v3.unsafe_fixnum();
  int f4 = v4.unsafe_fixnum();
  int res = f1 + f2 + f3 + f4;
  return res;
}
};

namespace core {

T_sp allocFixnum() {
  Fixnum_sp fn = make_fixnum(3);
  return fn;
}

void dynamicCastArg(T_sp a) {
  Cons_sp c = gc::As<Cons_sp>(a);
  (void)c; // suppress warning
}

T_sp bitOLogicWithObjects() {
  T_sp val = _lisp->_true();
  T_sp val2 = _Nil<T_O>();
  if (val2.nilp()) {
    return val;
  }
  return val2;
};

T_sp allocCons() {
  Cons_sp fn = Cons_O::create(_Nil<T_O>(),_Nil<T_O>());
  return fn;
}

T_sp lexicalFrameLookup(T_sp fr, int depth, int index) {
  ASSERT(fr.isA<ActivationFrame_O>());
  ActivationFrame_sp af = gctools::reinterpret_cast_smart_ptr<ActivationFrame_O>(fr);
  T_sp val = core::value_frame_lookup_reference(af, depth, index);
  return val;
}

CL_LAMBDA(sym val thunk);
CL_DECLARE();
CL_DOCSTRING("callWithVariableBound");
CL_DEFUN T_mv core__call_with_variable_bound(Symbol_sp sym, T_sp val, T_sp thunk) {
  DynamicScopeManager scope(sym, val);
  if (!gc::IsA<Function_sp>(thunk)) {
    printf("%s:%d:%s  The thunk is NOT a Function object!!!!! thunk.raw_() = %p\n",
           __FILE__, __LINE__, __FUNCTION__, (void*)thunk.raw_());
    fflush(stdout);
  }
  Function_sp func = gc::As_unsafe<Function_sp>(thunk);
  return (func->entry.load())(LCC_PASS_ARGS0_ELLIPSIS(func.raw_()));
  // Don't put anything in here - don't mess up the MV return
}

}


extern "C" {
LCC_RETURN call_with_variable_bound(core::T_O* tsym, core::T_O* tval, core::T_O* tthunk) {
  core::Symbol_sp sym((gctools::Tagged)tsym);
  core::T_sp val((gctools::Tagged)tval);
  core::Function_sp func((gctools::Tagged)tthunk);
  core::DynamicScopeManager scope(sym, val);
  return (func->entry.load())(LCC_PASS_ARGS0_ELLIPSIS(func.raw_()));
}

};

namespace core {

// try/catch approach does work
CL_DEFUN T_mv core__funwind_protect(T_sp protected_fn, T_sp cleanup_fn) {
  T_mv result;
  try {
    Closure_sp closure = gc::As_unsafe<Closure_sp>(protected_fn);
    ASSERT(closure);
    result = closure->entry.load()(LCC_PASS_ARGS0_ELLIPSIS(closure.raw_()));
  }
  catch (...)
  {
    // Abnormal exit
    // Save return values, then cleanup, then continue exit
    size_t nvals = lisp_multipleValues().getSize();
    T_O* mv_temp[nvals];
    multipleValuesSaveToTemp(nvals, mv_temp);
    {
      Closure_sp closure = gc::As_unsafe<Closure_sp>(cleanup_fn);
      closure->entry.load()(LCC_PASS_ARGS0_ELLIPSIS(closure.raw_()));
    }
    multipleValuesLoadFromTemp(nvals, mv_temp);
    throw;  // __cxa_rethrow
  }
  // Normal exit
  // Save return values, cleanup, return
  size_t nvals = result.number_of_values();
  T_O* mv_temp[nvals];
  returnTypeSaveToTemp(nvals, result.raw_(), mv_temp);
  {
    Closure_sp closure = gc::As_unsafe<Closure_sp>(cleanup_fn);
    closure->entry.load()(LCC_PASS_ARGS0_ELLIPSIS(closure.raw_()));
  }
  return returnTypeLoadFromTemp(nvals, mv_temp);
}

CL_LAMBDA(function-designator &rest functions);
CL_DECLARE();
CL_DOCSTRING("multipleValueFuncall");
CL_DEFUN T_mv core__multiple_value_funcall(T_sp funcDesignator, List_sp functions) {
  Function_sp fmv = coerce::functionDesignator(funcDesignator);
  MAKE_STACK_FRAME(frame, fmv.raw_(), MultipleValues::MultipleValuesLimit);
  size_t numArgs = 0;
  size_t idx = 0;
  MultipleValues& mv = lisp_multipleValues();
  for (auto cur : functions) {
    Function_sp tfunc = gc::As<Function_sp>(oCar(cur));
    T_mv result = (tfunc->entry.load())(LCC_PASS_ARGS0_ELLIPSIS(tfunc.raw_()));
    ASSERT(idx < MultipleValues::MultipleValuesLimit);
    if (result.number_of_values() > 0  ) {
        (*frame)[idx] = result.raw_();
        ++idx;
        for (size_t i = 1, iEnd(result.number_of_values()); i < iEnd; ++i) {
          ASSERT(idx < MultipleValues::MultipleValuesLimit);
          (*frame)[idx] = mv._Values[i];
          ++idx;
        }
      }
  }
  frame->set_number_of_arguments(idx);
  Vaslist valist_s(frame);
  VaList_sp args(&valist_s);
  return funcall_consume_valist_<Function_O>(fmv.tagged_(),args);
}

CL_LAMBDA(tag func);
CL_DECLARE();
CL_DOCSTRING("catchFunction");
CL_DEFUN T_mv core__catch_function(T_sp tag, Function_sp thunk) {
  T_mv result;
  CLASP_BEGIN_CATCH(tag) {
    result = thunk->entry.load()(LCC_PASS_ARGS0_ELLIPSIS(thunk.raw_()));
  } CLASP_END_CATCH(tag, result);
  return result;
}

CL_LAMBDA(tag result);
CL_DECLARE();
CL_DOCSTRING("Like CL:THROW, but takes a thunk");
CL_DEFUN void core__throw_function(T_sp tag, T_sp result_form) {
  T_mv result;
  Closure_sp closure = result_form.asOrNull<Closure_O>();
  ASSERT(closure);
  result = closure->entry.load()(LCC_PASS_ARGS0_ELLIPSIS(closure.raw_()));
  result.saveToMultipleValue0();
  clasp_throw(tag);
}

CL_LAMBDA(symbols values func);
CL_DECLARE();
CL_DOCSTRING("progvFunction");
CL_DEFUN T_mv core__progv_function(List_sp symbols, List_sp values, Function_sp func) {
  if (symbols.consp()) {
    if (values.consp()) {
      DynamicScopeManager scope(gc::As<Symbol_sp>(CONS_CAR(symbols)),CONS_CAR(values));
      return core__progv_function(CONS_CDR(symbols),oCdr(values),func);
    } else {
      DynamicScopeManager scope(gc::As<Symbol_sp>(CONS_CAR(symbols)),_Unbound<core::T_O>());
      return core__progv_function(CONS_CDR(symbols),_Nil<core::T_O>(),func);
    }
  } else {
    T_mv result = (func->entry.load())(LCC_PASS_ARGS0_ELLIPSIS(func.raw_()));
  // T_mv result = eval::funcall(func);
    return result;
  }
}

 CL_DEFUN T_mv core__declared_global_inline_p(T_sp name)
 {
   return gc::As<HashTableEqual_sp>(_sym_STARfunctions_to_inlineSTAR->symbolValue())->gethash(name);
 }

  CL_DEFUN T_mv core__declared_global_notinline_p(T_sp name)
 {
   return gc::As<HashTableEqual_sp>(_sym_STARfunctions_to_notinlineSTAR->symbolValue())->gethash(name);
 }



CL_DEFUN T_sp core__run_function( T_sp object ) {
  std::string name = gc::As<String_sp>(object)->get_std_string();
  T_sp thandle = _sym_STARcurrent_dlopen_handleSTAR->symbolValue();
  uintptr_t handle = 0;
  if (thandle.notnilp() && gc::IsA<Pointer_sp>(thandle)) {
    handle = (uintptr_t)gc::As_unsafe<Pointer_sp>(thandle)->ptr();
  }
  claspFunction func = (claspFunction)dlsym((void*)handle,name.c_str());
//  printf("%s:%d:%s running function %s  at %p\n", __FILE__, __LINE__, __FUNCTION__, name.c_str(), (void*)func);
#ifdef DEBUG_SLOW
  MaybeDebugStartup startup((void*)func);
#endif
  if( func != nullptr ) {
    LCC_RETURN ret = func(LCC_PASS_ARGS0_VA_LIST(_Nil<T_O>().raw_()));
    core::T_sp res((gctools::Tagged)ret.ret0[0]);
    core::T_sp val = res;
    return val;
  }
  return _Nil<T_O>();
}

CL_DEFUN T_sp core__run_make_mlf( T_sp object ) {
  return core__run_function(object);
}

CL_DEFUN T_sp core__run_init_mlf( T_sp object ) {
  return core__run_function(object);
}

CL_DEFUN T_sp core__make_builtin_class( T_sp object ) {
  printf("%s:%d:%s  with %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(object).c_str());
  SIMPLE_ERROR(BF("Add support for core__make_builtin_class"));
}


CL_DEFUN T_sp core__handle_creator( T_sp object ) {
  SIMPLE_ERROR(BF("Handle-creator for %s") % _rep_(object).c_str());
}

 SYMBOL_EXPORT_SC_(CompPkg, STARimplicit_compile_hookSTAR);
 SYMBOL_EXPORT_SC_(CompPkg, implicit_compile_hook_default);
 SYMBOL_EXPORT_SC_(CompPkg, STARall_functions_for_one_compileSTAR);
 SYMBOL_SC_(CorePkg, dlopen);
 SYMBOL_SC_(CorePkg, dlsym);
 SYMBOL_SC_(CorePkg, dladdr);
 SYMBOL_EXPORT_SC_(CorePkg, callWithVariableBound);

template <typename T> char document() {return '\0'; };
template <> char document<char>() { return 'c'; };
template <> char document<size_t>() { return 's'; };
template <> char document<char*>() { return 'S'; };
template <> char document<T_O*>() { return 'O'; };
template <> char document<float>() { return 'f'; };
template <> char document<double>() { return 'd'; };
template <> char document<fnLispCallingConvention>() { return 'f'; };

char ll_read_char(T_sp stream, bool log, size_t& index)
{
  while (1) {
    char c = clasp_read_char(stream);
    if (c == '!') {
      std::string msg;
      char d;
      do {
        d = clasp_read_char(stream);
        if (d != '!') {
          msg += d;
        }
        index++;
      } while(d!='!');
      if (log) printf("%s:%d byte-code message: %s\n", __FILE__, __LINE__, msg.c_str());
    } else return c;
  }
}

#if 1
#define SELF_DOCUMENT(ty,stream,index) { char _xx = document<ty>(); clasp_write_char(_xx,stream); ++index; }
#define SELF_CHECK(ty,stream,index) { char _xx = document<ty>(); claspCharacter _cc = ll_read_char(stream,log,index); ++index; if (_xx!=_cc) SIMPLE_ERROR(BF("Mismatch of ltvc read types read '%c' expected '%c'") % _cc % _xx );}
#else
#define SELF_DOCUMENT(ty,stream,index) {}
#define SELF_CHECK(ty,stream,index) {}
#endif


CL_DEFUN size_t core__ltvc_write_char(T_sp object, T_sp stream, size_t index)
{
  SELF_DOCUMENT(char,stream,index);
  if (object.fixnump()) {
    clasp_write_char(object.unsafe_fixnum()&0xff,stream);
    ++index;
  } else if (object.characterp()) {
    clasp_write_char(object.unsafe_character(),stream);
    ++index;
  } else {
    SIMPLE_ERROR(BF("Expected fixnum or character - got %s") % _rep_(object));
  }
  return index;
}


    
    

char ltvc_read_char(T_sp stream, bool log, size_t& index)
{
  SELF_CHECK(char,stream,index);
  char c = clasp_read_char(stream);
  ++index;
  if (log) printf("%s:%d:%s -> '%c'/%d\n", __FILE__, __LINE__, __FUNCTION__, c, c);
  return c;
}

void compact_write_size_t(size_t data, T_sp stream, size_t& index) {
  int64_t nb = 0;
  for (nb=sizeof(data)-1; nb>=0; nb-- ) {
    if (((char*)&data)[nb] != '\0') break;
  }
  nb += 1;
  clasp_write_char('0'+nb,stream);
  clasp_write_characters((char*)&data,nb,stream);
  index += nb+1;
}

size_t compact_read_size_t(T_sp stream, size_t& index) {
  size_t data = 0;
  int64_t nb = clasp_read_char(stream)-'0';
  if (nb<0 ||nb>8) {
    printf("%s:%d Illegal size_t size %lld\n", __FILE__, __LINE__, (long long)nb);
    abort();
  }
  for (size_t ii=0; ii<nb; ++ii ) {
    ((char*)&data)[ii] = clasp_read_char(stream);
  }
  index += nb+1;
  return data;
}


  
CL_DEFUN size_t core__ltvc_write_size_t(T_sp object, T_sp stream, size_t index)
{
  SELF_DOCUMENT(size_t,stream,index);
  if (object.fixnump()) {
    size_t data = object.unsafe_fixnum();
    compact_write_size_t(data,stream,index);
  } else if (gc::IsA<Bignum_sp>(object)) {
    size_t data = gc::As_unsafe<Bignum_sp>(object)->as_size_t();
    compact_write_size_t(data,stream,index);
  } else {
    SIMPLE_ERROR(BF("Expected integer got %s") % _rep_(object));
  }
  return index;
}

size_t ltvc_read_size_t(T_sp stream, bool log, size_t& index)
{
  SELF_CHECK(size_t,stream,index);
  size_t data = compact_read_size_t(stream,index);
  if (log) printf("%s:%d:%s -> %lu\n", __FILE__, __LINE__, __FUNCTION__, data);
  return data;
}

CL_DEFUN size_t core__ltvc_write_string(T_sp object, T_sp stream, size_t index)
{
  SELF_DOCUMENT(char*,stream,index);
  std::string str = gc::As<String_sp>(object)->get_std_string();
  index = core__ltvc_write_size_t(make_fixnum(str.size()),stream,index);
  clasp_write_characters((char*)str.c_str(),str.size(),stream);
  index += str.size();
  return index;
}

std::string ltvc_read_string(T_sp stream, bool log, size_t& index)
{
  SELF_CHECK(char*,stream,index);
  size_t len = ltvc_read_size_t(stream,log,index);
  std::string str(len,' ');
  for (size_t i=0; i<len; ++i ) {
    str[i] = clasp_read_char(stream);
  }
  index += len;
  if (log) printf("%s:%d:%s -> \"%s\"\n", __FILE__, __LINE__, __FUNCTION__, str.c_str());
  return str;
}

CL_DEFUN size_t core__ltvc_write_float(T_sp object, T_sp stream, size_t index)
{
  SELF_DOCUMENT(float,stream,index);
  if (object.single_floatp()) {
    float data = object.unsafe_single_float();
    clasp_write_characters((char*)&data,sizeof(data),stream);
    index += sizeof(data);
  } else {
    SIMPLE_ERROR(BF("Expected single-float got %s") % _rep_(object));
  }
  return index;
}

float ltvc_read_float(T_sp stream, bool log, size_t& index)
{
  SELF_CHECK(float,stream,index);
  float data;
  for (size_t i=0; i<sizeof(data); ++i ) {
    ((char*)&data)[i] = clasp_read_char(stream);
  }
  index += sizeof(data);
  if (log) printf("%s:%d:%s -> '%f'\n", __FILE__, __LINE__, __FUNCTION__, data );
  return data;
}

CL_DEFUN size_t core__ltvc_write_double(T_sp object, T_sp stream, size_t index)
{
  SELF_DOCUMENT(double,stream,index);
  double data = gc::As<DoubleFloat_sp>(object)->get();
  clasp_write_characters((char*)&data,sizeof(data),stream);
  index += sizeof(data);
  return index;
}

double ltvc_read_double(T_sp stream, bool log, size_t& index)
{
  SELF_CHECK(double,stream,index);
  double data;
  for (size_t i=0; i<sizeof(data); ++i ) {
    ((char*)&data)[i] = clasp_read_char(stream);
  }
  index += sizeof(data);
  if (log) printf("%s:%d:%s -> '%lf'\n", __FILE__, __LINE__, __FUNCTION__, data );
  return data;
}

CL_DOCSTRING("tag is (0|1|2) where 0==literal, 1==transient, 2==immediate");
CL_DEFUN size_t core__ltvc_write_object(T_sp ttag, T_sp index_or_immediate, T_sp stream, size_t index)
{
  SELF_DOCUMENT(T_O*,stream,index);
  if (ttag.characterp() && (ttag.unsafe_character()=='l'||ttag.unsafe_character()=='t'||ttag.unsafe_character()=='i')) {
    char tag = ttag.unsafe_character();
    clasp_write_char(tag,stream);
    index += 1;
    uintptr_t data;
    if (ttag.unsafe_character()=='l'||ttag.unsafe_character()=='t') {
      data = index_or_immediate.unsafe_fixnum();
    } else {
      if (index_or_immediate.fixnump()) {
        data = index_or_immediate.unsafe_fixnum();
      } else {
        data = gc::As<Bignum_sp>(index_or_immediate)->as_size_t();
      }
    }
    compact_write_size_t(data,stream,index);
    return index;
  }
  SIMPLE_ERROR(BF("tag must be 0, 1 or 2 - you passed %s") % _rep_(ttag));
}

T_O* ltvc_read_object(gctools::GCRootsInModule* roots, T_sp stream, bool log, size_t& index)
{
  SELF_CHECK(T_O*,stream,index);
  char tag = clasp_read_char(stream);
  char ttag;
  if (tag=='l') ttag = 0; // literal
  else if (tag=='t') ttag = 1; // transient
  else if (tag=='i') ttag = 2; // immediate
  else {
    printf("%s:%d The object tag must be 'l', 't' or 'i'\n", __FILE__, __LINE__ );
    abort();
  }
  ++index;
  if (log) printf("%s:%d:%s    tag = %c\n", __FILE__, __LINE__, __FUNCTION__, tag);
  size_t data;
  data = compact_read_size_t(stream,index);
  if (log) printf("%s:%d:%s    index = %lu\n", __FILE__, __LINE__, __FUNCTION__, data);
  switch (tag) {
  case 'l': {
    gctools::Tagged val = roots->getLiteral(data);
    if (log) {
      T_sp o((gctools::Tagged)val);
      printf("%s:%d:%s literal -> %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(o).c_str());
    }
    return (T_O*)val;
  }
      break;
  case 't': {
    gctools::Tagged val = roots->getTransient(data);
    if (log) {
      T_sp o((gctools::Tagged)val);
      printf("%s:%d:%s transient -> %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(o).c_str());
    }
    return (T_O*)val;
  }
      break;
  case 'i': {
    gctools::Tagged val = (gctools::Tagged)data;
    if (log) {
      T_sp o((gctools::Tagged)val);
      printf("%s:%d:%s immediate -> %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(o).c_str());
    }
    return (T_O*)val;
  }
  default: {
    SIMPLE_ERROR(BF("Could not read an object for using tag %d data %p") % tag % data );
  };
  };
}

Cons_O* ltvc_read_list(gctools::GCRootsInModule* roots, size_t num, T_sp stream, bool log, size_t& index) {
  ql::list result;
  for ( size_t ii =0; ii<num; ++ii ) {
    T_sp obj((gctools::Tagged)ltvc_read_object(roots,stream,log,index));
    result << obj;
  }
  if (log) {
    printf("%s:%d:%s list -> %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(result.cons()).c_str());
  }
  return (Cons_O*)result.cons().tagged_();
}

void ltvc_fill_list_varargs(gctools::GCRootsInModule* roots, T_O* list, size_t len, Cons_O* varargs)
{
  // Copy the vargs list into the ltv one.
  // FIXME: This is obviously inefficient.
  T_sp cur((gctools::Tagged)list);
  T_sp vargs((gctools::Tagged)varargs);
  for (; len != 0; --len) {
    Cons_sp cur_cons = gc::As<Cons_sp>(cur);
    Cons_sp cur_vargs = gc::As<Cons_sp>(vargs);
    cur_cons->rplaca(cur_vargs->_Car);
    cur = cur_cons->_Cdr;
    vargs = cur_vargs->_Cdr;
  }
}

#define DEFINE_PARSERS
#include "byte-code-interpreter.cc"
#undef DEFINE_PARSERS

void byte_code_interpreter(gctools::GCRootsInModule* roots, T_sp fin, bool log)
{
  volatile uint32_t i=0x01234567;
    // return 0 for big endian, 1 for little endian.
  if ((*((uint8_t*)(&i))) == 0x67) {
    // Little endian - the code is set up for this
  } else {
    printf("%s:%d This is a big-endian architecture and the byte-code interpreter is set up for little-endian - fix this before proceeding\n", __FILE__, __LINE__ );
    abort();
  }
    

    size_t byte_index = 0;
  while(1) {
    if (log) printf("%s:%d ------- top of byte-code interpreter\n", __FILE__, __LINE__ );
    char c = ltvc_read_char(fin,log,byte_index);
    switch (c) {
    case 0: goto DONE;
#define DEFINE_SWITCH
#include "byte-code-interpreter.cc"
#undef DEFINE_SWITCH
    default: {
      printf("%s:%d illegal byte-code %d\n", __FILE__, __LINE__, c);
      abort();
    }
    }
  }
 DONE:
  return;
}

#if 0
// This is an attempt to use concatenated object files as fasl files
// I'm not sure it's a good idea - but I may want to revisit this

CL_DEFUN void core__write_fasl_file(T_sp stream, List_sp object_files) {
  size_t dead_beef = 0xEFBEADDEEFBEADDE; // DEADBEEFDEADBEEF backwards
  int filedes;
  if (core::IOFileStream_sp fs = stream.asOrNull<core::IOFileStream_O>()) {
    filedes = fs->fileDescriptor();
  } else if (core::IOStreamStream_sp iostr = stream.asOrNull<core::IOStreamStream_O>()) {
    FILE *f = iostr->file();
    filedes = fileno(f);
  } else {
    SIMPLE_ERROR(BF("Illegal file type %s for write-fasl-file") % _rep_(stream).c_str());
  }
  write(filedes,"CLASP   ",8);
  size_t num_object_files = cl__length(object_files);
  write(filedes,&num_object_files,sizeof(num_object_files));
  for ( auto cur : object_files ) {
    ObjectFile_sp of = CONS_CAR(cur);
    write(filedes,&dead_beef,8);
    size_t object_file_size = of->_ObjectFileSize;
    size_t word_aligned_object_file_size = ((object_file_size+sizeof(size_t))/sizeof(size_t))*sizeof(size_t);
    write(filedes,&word_aligned_object_file_size,sizeof(word_aligned_object_file_size));
    write(filedes,&object_file_size,sizeof(object_file_size));
    write(filedes,of->_ObjectFilePtr,word_aligned_object_file_size);
  }
}


#define TRAP_BAD_READ(sz,esz) if (sz!=esz) { SIMPLE_ERROR(BF("While reading fasl file hit end of file prematurely - the fasl file is corrupt"));}


/*! Read a fasl file and return a list of ObjectFile_sp objects
*/
CL_DEFUN List_sp core__read_fasl_file(T_sp stream) {
  int filedes;
  if (core::IOFileStream_sp fs = stream.asOrNull<core::IOFileStream_O>()) {
    filedes = fs->fileDescriptor();
  } else if (core::IOStreamStream_sp iostr = stream.asOrNull<core::IOStreamStream_O>()) {
    FILE *f = iostr->file();
    filedes = fileno(f);
  } else {
    SIMPLE_ERROR(BF("Illegal file type %s for write-fasl-file") % _rep_(stream).c_str());
  }
  char header[8];
  size_t num_read;
  num_read = read(filedes,header,sizeof(header));
  TRAP_BAD_READ(num_read,sizeof(header));
  if (strncmp(header,"CLASP   ",8)!=0) {
    SIMPLE_ERROR(BF("File %s is not a FASL file") % _rep_(stream));
  }
  size_t dead_beef = 0xEFBEADDEEFBEADDE; // DEADBEEFDEADBEEF backwards
  size_t num_object_files;
  num_read = read(filedes,&num_object_files,sizeof(size_t));
  ql::list result;
  for ( size_t idx=0; idx<num_object_files; ++idx ) {
    size_t read_dead_beef;
    num_read = read(filedes,&read_dead_beef,sizeof(read_dead_beef));
    TRAP_BAD_READ(num_read,sizeof(read_dead_beef));
    if (read_dead_beef!=dead_beef) {
      SIMPLE_ERROR(BF("While reading fasl file the internal marker was not found - the fasl file is corrupt"));
    }
    size_t word_aligned_object_file_size;
    num_read = read(filedes,&word_aligned_object_file_size,sizeof(word_aligned_object_file_size));
    TRAP_BAD_READ(num_read,sizeof(word_aligned_object_file_size));
    size_t object_file_size;
    num_read = read(filedes,&object_file_size,sizeof(object_file_size));
    TRAP_BAD_READ(num_read,sizeof(object_file_size));
    char* buffer = (char*)malloc(word_aligned_object_file_size);
    num_read = read(filedes,buffer,word_aligned_object_file_size);
    TRAP_BAD_READ(num_read,word_aligned_object_file_size);
    GC_ALLOCATE_VARIADIC(ObjectFile_O,of,buffer,object_file_size);
    result << of;
  }
  return result.cons();
}
#endif

void initialize_compiler_primitives(Lisp_sp lisp) {

  // Initialize raw object translators needed for Foreign Language Interface support 
  llvmo::initialize_raw_translators(); // See file intrinsics.cc!

  comp::_sym_STARimplicit_compile_hookSTAR->defparameter(comp::_sym_implicit_compile_hook_default->symbolFunction());
  cleavirPrimops::_sym_callWithVariableBound->setf_symbolFunction(_sym_callWithVariableBound->symbolFunction());

  return;
}

}; /* namespace */




namespace core {

std::atomic<int> global_sigchld_count{0};

CL_DEFUN int core__sigchld_count() {
  return global_sigchld_count;
}

CL_DEFUN int core__decf_sigchld_count() {
  global_sigchld_count--;
  return global_sigchld_count;
}

void sigchld(int signal) {
  global_sigchld_count++;
}

CL_DEFUN void core__install_sigchld() {
  signal(SIGCHLD, sigchld);
}

CL_DEFUN void core__uninstall_sigchld() {
  signal(SIGCHLD, SIG_DFL);
}

/* Match the offset in the alist to the expected offset
*/
void expect_offset(T_sp key, T_sp alist, size_t expected) {
  List_sp pair = core__alist_assoc_eq(alist,key);
  if (pair.nilp()) {
    SIMPLE_ERROR(BF("Could not find key %s in alist %s") % _rep_(key) % _rep_(alist));
  }
  T_sp value = CONS_CDR(pair);
  if (!value.fixnump()) {
    SIMPLE_ERROR(BF("The value %s in alist %s at key %s must be a fixnum") % _rep_(value) % _rep_(alist) % _rep_(key));
  }
  if (value.unsafe_fixnum()!=expected) {
    SIMPLE_ERROR(BF("The value %s in alist %s at key %s must match the C++ tagged offset of %d") % _rep_(value) % _rep_(alist) % _rep_(key) % expected);
  }
}

};


extern "C" {

void clasp_trap(core::T_O* val) {
  printf("%s:%d  clasp_trap called with %p\n", __FILE__, __LINE__, val);
}

void clasp_silent_trap(core::T_O* obj) {
  // Do nothing
}

};
