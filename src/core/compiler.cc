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
#include <clasp/core/debugger.h>
#include <clasp/core/pathname.h>
#include <clasp/core/pointer.h>
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
#include <clasp/llvmo/code.h>
#include <clasp/core/wrappers.h>


namespace core {


std::atomic<size_t> global_jit_compile_counter;

DOCGROUP(clasp)
CL_DEFUN size_t core__get_jit_compile_counter() {
  return global_jit_compile_counter.load();
}

DOCGROUP(clasp)
CL_DEFUN void core__update_max_jit_compile_counter(size_t val) {
  if (val < global_jit_compile_counter.load()) return;
  size_t expected;
  do {
    expected = global_jit_compile_counter.load();
  } while (!global_jit_compile_counter.compare_exchange_weak(expected, val));
//  printf("%s:%d:%s Set max_jit_compile_counter to %lu\n", __FILE__, __LINE__, __FUNCTION__, val );
}

DOCGROUP(clasp)
CL_DEFUN size_t core__next_jit_compile_counter() {
  return ++global_jit_compile_counter;
}


MaybeDebugStartup::MaybeDebugStartup(void* fp, const char* n) : fptr(fp), start_dispatcher_count(0) {
  if (n) this->name = n;
  this->start_jit_compile_counter = global_jit_compile_counter;
  if (core::_sym_STARdebugStartupSTAR->symbolValue().notnilp()) {
    this->started = true;
    this->start = std::chrono::steady_clock::now();
    if (clos::_sym_dispatcher_count->fboundp()) {
      core::T_sp nu = core::eval::funcall(clos::_sym_dispatcher_count);
      this->start_dispatcher_count = nu.unsafe_fixnum();
    } else {
      this->start_dispatcher_count = 0;
    }
  } else this->started = false;
};

NEVER_OPTIMIZE MaybeDebugStartup::~MaybeDebugStartup() {
  if (core::_sym_STARdebugStartupSTAR->symbolValue().notnilp()
      && this->started) {
    auto end = std::chrono::steady_clock::now();
    auto us = std::chrono::duration_cast<std::chrono::microseconds>(end - this->start);
    size_t end_dispatcher_count = 0;
    
    if (clos::_sym_dispatcher_count->fboundp()) {
      core::T_sp nu = core::eval::funcall(clos::_sym_dispatcher_count);
      end_dispatcher_count = nu.unsafe_fixnum();
    }
    size_t dispatcher_delta = end_dispatcher_count - this->start_dispatcher_count;
    stringstream name_;
    if (this->name!="") name_ << this->name << " ";
    Dl_info di;
    int found = dladdr((void*)this->fptr,&di);
    if (found && di.dli_sname) {
      name_ << di.dli_sname;
    } else {
      name_ << "NONAME";
    }
    if (name_.str() == "") name_ << (void*)this->fptr;
    printf("%s us %zu gfds %zu jits: %s\n", _rep_(Integer_O::create(us.count())).c_str(), dispatcher_delta, (global_jit_compile_counter-this->start_jit_compile_counter),name_.str().c_str());
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

DOCGROUP(clasp)
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

void register_startup_function(const StartUp& one_startup)
{
#ifdef DEBUG_STARTUP
  printf("%s:%d In register_startup_function type: %d at %p\n", __FILE__, __LINE__, startup._Type, startup._Function);
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
    startup->_functions = (StartUp*)malloc(startup->_capacity*sizeof(StartUp));
  } else {
    if ( startup->_count == startup->_capacity ) {
      startup->_capacity = startup->_capacity*STARTUP_FUNCTION_CAPACITY_MULTIPLIER;
      startup->_functions = (StartUp*)realloc(startup->_functions,startup->_capacity*sizeof(StartUp));
    }
  }
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
  StartUp* startup_functions = NULL;
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
    StartUp previous;
    for ( size_t i = 0; i<startup_count; ++i ) {
      StartUp& startup = startup_functions[i];
      if (startup._Position == previous._Position) {
        printf("%s:%d At startup there were two adjacent startup functions with the same position value %lu - this could mean a startup order catastrophe\n", __FILE__, __LINE__, startup._Position);
      }
      previous = startup;
#ifdef DEBUG_STARTUP
      printf("%s:%d     About to invoke fn@%p\n", __FILE__, __LINE__, fn );
#endif
//      T_mv result = (fn)(LCC_PASS_MAIN());
      switch (startup._Type) {
      case StartUp::T_O_function:
          result = ((T_OStartUp)startup._Function)(literals); // invoke the startup function
          break;
      case StartUp::void_function:
          ((voidStartUp)startup._Function)();
          result = NULL;
      }
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

DOCGROUP(clasp)
NOINLINE CL_DEFUN T_mv core__trigger_dtrace_start(T_sp closure)
{
  start_dtrace();
  return eval::funcall(closure);
}


DOCGROUP(clasp)
NOINLINE CL_DEFUN T_sp core__trigger_dtrace_stop()
{
  stop_dtrace();
  return nil<T_O>();
}

  


DOCGROUP(clasp)
CL_DEFUN void core__startup_functions_invoke(List_sp literals)
{
  startup_functions_invoke((T_O*)literals.raw_());
  printf("%s:%d startup_functions_invoke returned -   this should never happen\n", __FILE__, __LINE__ );
  abort();
};

DOCGROUP(clasp)
CL_DEFUN void core__test_simple_error(T_sp msg) {
  core::String_sp smsg = gc::As<core::String_sp>(msg);
  std::string ss = smsg->get_std_string();
  SIMPLE_ERROR(BF(ss));
}


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

CL_LAMBDA()
CL_DECLARE();
CL_DOCSTRING(R"dx(Print info about booting)dx")
DOCGROUP(clasp)
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


CL_DOCSTRING(R"dx(Return the rdtsc performance timer value)dx")
DOCGROUP(clasp)
CL_DEFUN Fixnum core__rdtsc(){
  unsigned int lo,hi;
  __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
  return ((uint64_t)hi << 32) | lo;
}

CL_LAMBDA(object &optional is-function)
CL_DECLARE();
CL_DOCSTRING(R"dx(mangleName)dx")
DOCGROUP(clasp)
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
    return Values(nil<T_O>(), name, make_fixnum(0), make_fixnum(CALL_ARGUMENTS_LIMIT));
  }
  Function_sp fsym = coerce::functionDesignator(sym);
  if (gc::IsA<BuiltinClosure_sp>(fsym)) {
    return Values(_lisp->_true(), SimpleBaseString_O::make("Provide-c-func-name"), make_fixnum(0), make_fixnum(CALL_ARGUMENTS_LIMIT));
  }
  return Values(nil<T_O>(), SimpleBaseString_O::make("Provide-func-name"), make_fixnum(0), make_fixnum(CALL_ARGUMENTS_LIMIT));
}

/*! Return the default snapshot name
 */
std::string startup_snapshot_name(Bundle& bundle) {
  stringstream ss;
  std::string executablePath;
  core::executablePath(executablePath);
  std::string name;
  size_t pos = executablePath.find_last_of('/');
  if (pos == std::string::npos) {
    name = executablePath;
  } else {
    name = executablePath.substr(pos+1);
  }
  ss << bundle._Directories->_FaslDir.string() << "/" << name << ".snapshot";
  return ss.str();
};


bool startup_snapshot_is_stale(const std::string& snapshotFileName) {
  stringstream ss;
  std::string executablePath;
  core::executablePath(executablePath);
  std::filesystem::path executable(executablePath);
  std::filesystem::path snapshot(snapshotFileName);
  if (!std::filesystem::exists(snapshot)) return true;
  return (std::filesystem::last_write_time(executable)
          > std::filesystem::last_write_time(snapshot));
};


CL_LAMBDA("&optional (stage #\\c)")
CL_DECLARE();
CL_DOCSTRING(R"dx(startupImagePathname - returns a pathname based on *features* :CLASP-MIN, :USE-MPS, :BCLASP)dx")
DOCGROUP(clasp)
CL_DEFUN T_sp core__startup_image_pathname(char stage) {
  stringstream ss;
  ss << "app-fasl:" << stage << "clasp-" << VARIANT_NAME << "-image";
  T_sp mode = core::_sym_STARclasp_build_modeSTAR->symbolValue();
  if (mode == kw::_sym_faso) {
    ss << ".fasp";
  } else if (mode == kw::_sym_fasoll) {
    ss << ".faspll";
  } else if (mode == kw::_sym_fasobc) {
    ss << ".faspbc";
  } else if (mode == kw::_sym_object) {
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

struct FasoHeader;

struct ObjectFileInfo {
  size_t    _ObjectId;
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

CL_LAMBDA(path-desig object-files &key (start-object-id 0))
CL_DOCSTRING(R"dx(Concatenate object files in OBJECT-FILES into a faso file and write it out to PATH-DESIG.
You can set the starting ObjectId using the keyword START-OBJECT-ID argument.)dx")
DOCGROUP(clasp)
CL_DEFUN void core__write_faso(T_sp pathDesig, List_sp objectFiles, T_sp tstart_object_id)
{
  //  write_bf_stream(BF("Writing FASO file to %s for %d object files\n") % _rep_(pathDesig) % cl__length(objectFiles));
  pathDesig = cl__translate_logical_pathname(pathDesig);
  size_t start_object_id = 0;
  if (tstart_object_id.fixnump()) {
    if (tstart_object_id.unsafe_fixnum()>=0) {
      start_object_id = tstart_object_id.unsafe_fixnum();
      //      printf("%s:%d assigned start_object_id = %lu\n", __FILE__, __LINE__, start_object_id );
    } else {
      SIMPLE_ERROR(BF("start-object-id must be a positive integer - got: %s") % _rep_(tstart_object_id).c_str());
    }
  }
//  printf("%s:%d start_object_id = %lu\n", __FILE__, __LINE__, start_object_id );
//  printf("%s:%d Number of object files: %lu\n", __FILE__, __LINE__, cl__length(objectFiles));
  FasoHeader* header = (FasoHeader*)malloc(FasoHeader::calculateSize(cl__length(objectFiles)));
  setup_FasoHeader(header);
  header->_HeaderPageCount = FasoHeader::calculateHeaderNumberOfPages(cl__length(objectFiles),getpagesize());
  header->_NumberOfObjectFiles = cl__length(objectFiles);
  List_sp cur = objectFiles;
  size_t nextPage = header->_HeaderPageCount;
  for ( size_t ii=0; ii<cl__length(objectFiles); ++ii ) {
    header->_ObjectFiles[ii]._ObjectId = ii+start_object_id;
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
  size_t _ObjectId;
  size_t _ObjectFileSize;
  FasoObjectFileInfo(size_t oid, size_t ofs) : _ObjectId(oid), _ObjectFileSize(ofs) {};
};
  
DOCGROUP(clasp)
CL_LAMBDA(output-path-designator faso-files &optional (verbose nil))CL_DEFUN void core__link_faso_files(T_sp outputPathDesig, List_sp fasoFiles, bool verbose) {
  if (verbose) write_bf_stream(BF("Writing FASO file to %s for %d object files\n") % _rep_(outputPathDesig) % cl__length(fasoFiles));
  std::vector<FasoObjectFileInfo> allObjectFiles;
  std::vector<MmapInfo> mmaps;
  List_sp cur = fasoFiles;
  for ( size_t ii=0; ii<cl__length(fasoFiles); ++ii ) {
    String_sp filename = gc::As<String_sp>(cl__namestring(oCar(cur)));
    cur = oCdr(cur);
    int fd = open(filename->get_std_string().c_str(),O_RDONLY);
    if (verbose) write_bf_stream(BF("mmap'ing file[%lu] %s\n") % ii % _rep_(filename)); 
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
        if (verbose) write_bf_stream(BF("%s:%d object file %lu id: %lu  length: %lu\n") % __FILE__ % __LINE__ % ofi % header->_ObjectFiles[ofi]._ObjectId % of_length);
        FasoObjectFileInfo fofi(header->_ObjectFiles[ofi]._ObjectId,of_length);
        allObjectFiles.emplace_back(fofi);
        if (verbose) write_bf_stream(BF("allObjectFiles.size() = %lu\n") % allObjectFiles.size());
      }
    } else {
      SIMPLE_ERROR(BF("Illegal and unknown file type - magic number: %X\n") % (size_t)header->_Magic);
    }
  }
  FasoHeader* header = (FasoHeader*)malloc(FasoHeader::calculateSize(allObjectFiles.size()));
  setup_FasoHeader(header);
  header->_HeaderPageCount = FasoHeader::calculateHeaderNumberOfPages(allObjectFiles.size(),getpagesize());
  header->_NumberOfObjectFiles = allObjectFiles.size();
  if (verbose) write_bf_stream(BF("Writing out all object files %lu\n") % allObjectFiles.size());
  size_t nextPage = header->_HeaderPageCount;
  for (size_t ofi=0; ofi<allObjectFiles.size(); ofi++ ) {
    header->_ObjectFiles[ofi]._ObjectId = allObjectFiles[ofi]._ObjectId;
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
  if (!fout) {
    SIMPLE_ERROR(BF("Could not open file %s") % _rep_(filename));
  }
  if (verbose) {
    write_bf_stream(BF("Writing file: %s\n") % _rep_(filename));
  }
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
    size_t mmap_size
      = mmaps[mmi]._ObjectFileAreaStart
      + mmaps[mmi]._ObjectFileAreaSize;
    int res = munmap(mmaps[mmi]._Memory,mmap_size);
    if (res!=0) {
      SIMPLE_ERROR(BF("Could not munmap memory"));
    }
  }
  if (verbose) write_bf_stream(BF("Closing %s\n") % _rep_(filename));
  fclose(fout);
  if (verbose) write_bf_stream(BF("Returning %s\n") % _rep_(filename));
}


DOCGROUP(clasp)
CL_LAMBDA(path-designator &optional (verbose *load-verbose*) (print t) (external-format :default))
CL_DEFUN core::T_sp core__load_fasoll(T_sp pathDesig, T_sp verbose, T_sp print, T_sp external_format)
{
//  printf("%s:%d:%s\n",__FILE__,__LINE__,__FUNCTION__);
  llvmo::llvm_sys__load_bitcode_ll(cl__pathname(pathDesig),verbose.notnilp(),print.notnilp(),external_format,nil<core::T_O>());
  return _lisp->_true();
}

DOCGROUP(clasp)
CL_LAMBDA(path-designator &optional (verbose *load-verbose*) (print t) (external-format :default))
CL_DEFUN core::T_sp core__load_fasobc(T_sp pathDesig, T_sp verbose, T_sp print, T_sp external_format)
{
//  printf("%s:%d:%s\n",__FILE__,__LINE__,__FUNCTION__);
  llvmo::llvm_sys__load_bitcode(cl__pathname(pathDesig),verbose.notnilp(),print.notnilp(),external_format,nil<core::T_O>());
  return _lisp->_true();
}


DOCGROUP(clasp)
CL_LAMBDA(path-designator &optional (verbose *load-verbose*) (print t) (external-format :default))
CL_DEFUN core::T_sp core__load_faso(T_sp pathDesig, T_sp verbose, T_sp print, T_sp external_format)
{
  String_sp sfilename = gc::As<String_sp>(cl__namestring(pathDesig));
  std::string filename = sfilename->get_std_string(); 
  char* name_buffer = (char*)malloc(filename.size()+1);
  strncpy(name_buffer,filename.c_str(),filename.size());
  name_buffer[filename.size()] = '\0';
  int fd = open(filename.c_str(),O_RDONLY);
  off_t fsize = lseek(fd, 0, SEEK_END);
  lseek(fd,0,SEEK_SET);
  void* memory = mmap(NULL, fsize, PROT_READ, MAP_SHARED|MAP_FILE, fd, 0);
  if (memory==MAP_FAILED) {
    close(fd);
    SIMPLE_ERROR(BF("Could not mmap %s because of %s") % _rep_(pathDesig) % strerror(errno));
  }
  close(fd); // Ok to close file descriptor after mmap
  llvmo::ClaspJIT_sp jit = llvmo::llvm_sys__clasp_jit();
  FasoHeader* header = (FasoHeader*)memory;
  llvmo::JITDylib_sp jitDylib;
  for (size_t fasoIndex = 0; fasoIndex<header->_NumberOfObjectFiles; ++fasoIndex) {
    if (!jitDylib || header->_ObjectFiles[fasoIndex]._ObjectId==0) {
      jitDylib = jit->createAndRegisterJITDylib(filename);
    }
    void* of_start = (void*)((char*)header + header->_ObjectFiles[fasoIndex]._StartPage*header->_PageSize);
    size_t of_length = header->_ObjectFiles[fasoIndex]._ObjectFileSize;
    if (print.notnilp()) write_bf_stream(BF("%s:%d Adding faso %s object file %d to jit\n") % __FILE__ % __LINE__ % filename % fasoIndex);
    llvm::StringRef sbuffer((const char*)of_start, of_length);
    std::string uniqueName = llvmo::uniqueMemoryBufferName("buffer",header->_ObjectFiles[fasoIndex]._ObjectId, fasoIndex);
    llvm::StringRef name(uniqueName);
    std::unique_ptr<llvm::MemoryBuffer> memoryBuffer(llvm::MemoryBuffer::getMemBuffer(sbuffer,name,false));
    llvmo::ObjectFile_sp of = llvmo::ObjectFile_O::create(std::move(memoryBuffer),header->_ObjectFiles[fasoIndex]._ObjectId,jitDylib,filename,fasoIndex);
    jit->addObjectFile(of,print.notnilp());
    T_mv startupName = core__startup_linkage_shutdown_names(header->_ObjectFiles[fasoIndex]._ObjectId,nil<core::T_O>());
    String_sp str = gc::As<String_sp>(startupName);
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s running startup %s\n", __FILE__, __LINE__, __FUNCTION__, str->get_std_string().c_str()));
    llvmo::Code_sp codeObject;
    jit->runStartupCode(*jitDylib->wrappedPtr(), str->get_std_string(), unbound<core::T_O>(), codeObject);
  }
  return _lisp->_true();
}

DOCGROUP(clasp)
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
  llvmo::ClaspJIT_sp jit = llvmo::llvm_sys__clasp_jit();
  FasoHeader* header = (FasoHeader*)memory;
  write_bf_stream(BF("NumberOfObjectFiles %d\n") % header->_NumberOfObjectFiles);
  for (size_t fasoIndex = 0; fasoIndex<header->_NumberOfObjectFiles; ++fasoIndex) {
    size_t fasoIndexd =  header->_ObjectFiles[fasoIndex]._ObjectId;
    void* of_start = (void*)((char*)header + header->_ObjectFiles[fasoIndex]._StartPage*header->_PageSize);
    size_t of_length = header->_ObjectFiles[fasoIndex]._ObjectFileSize;
    //    write_bf_stream(BF("Adding faso %s object file %d to jit\n") % _rep_(filename) % fasoIndex);
    write_bf_stream(BF("Object file %d  ObjectId: %lu start-page: %lu  bytes: %lu pages: %lu\n")
                    % fasoIndex
                    % header->_ObjectFiles[fasoIndex]._ObjectId
                    % header->_ObjectFiles[fasoIndex]._StartPage
                    % header->_ObjectFiles[fasoIndex]._ObjectFileSize % header->_ObjectFiles[fasoIndex]._NumberOfPages );
  }
  return _lisp->_true();
}

void clasp_unpack_faso(const std::string& path_designator) {
  size_t pos = path_designator.find_last_of('.');
  if (pos==std::string::npos) {
    SIMPLE_ERROR(BF("Could not find extension in path: %s") % path_designator);
  }
  std::string prefix = path_designator.substr(0,pos);
  int fd = open(path_designator.c_str(),O_RDONLY);
  off_t fsize = lseek(fd, 0, SEEK_END);
  lseek(fd,0,SEEK_SET);
  void* memory = mmap(NULL, fsize, PROT_READ, MAP_SHARED|MAP_FILE, fd, 0);
  if (memory==MAP_FAILED) {
    close(fd);
    SIMPLE_ERROR(BF("Could not mmap %s because of %s") % path_designator % strerror(errno));
  }
  FasoHeader* header = (FasoHeader*)memory;
  printf("NumberOfObjectFiles %lu\n", header->_NumberOfObjectFiles);
  for (size_t fasoIndex = 0; fasoIndex<header->_NumberOfObjectFiles; ++fasoIndex) {
    void* of_start = (void*)((char*)header + header->_ObjectFiles[fasoIndex]._StartPage*header->_PageSize);
    size_t of_length = header->_ObjectFiles[fasoIndex]._ObjectFileSize;
    stringstream sfilename;
    sfilename << prefix << "-" << fasoIndex << "-" << header->_ObjectFiles[fasoIndex]._ObjectId << ".o";
    FILE* fout = fopen(sfilename.str().c_str(),"w");
    fwrite(of_start,of_length,1,fout);
    fclose(fout);
    printf("Object file[%lu] ObjectId: %lu  start-page: %lu  bytes: %lu pages: %lu\n", fasoIndex , header->_ObjectFiles[fasoIndex]._ObjectId , header->_ObjectFiles[fasoIndex]._StartPage , header->_ObjectFiles[fasoIndex]._ObjectFileSize , header->_ObjectFiles[fasoIndex]._NumberOfPages );
  }
}
    
CL_DOCSTRING(R"dx(Unpack the faso/fasp file into individual object files.)dx")
DOCGROUP(clasp)
CL_DEFUN void core__unpack_faso(T_sp path_designator)
{
  Pathname_sp pn_filename = cl__pathname(path_designator);
  String_sp sname = gc::As<String_sp>(cl__namestring(pn_filename));
  clasp_unpack_faso(sname->get_std_string());
}


CL_LAMBDA(name &optional verbose print external-format)
CL_DOCSTRING(R"dx(load-binary-directory - load a binary file inside the directory)dx")
DOCGROUP(clasp)
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


CL_DOCSTRING(R"dx(Return the startup function name and the linkage based on the current dynamic environment)dx")
CL_DOCSTRING_LONG(R"dx(The name contains the id as part of itself. Return (values startup-name linkage shutdown-name).)dx")
DOCGROUP(clasp)
CL_LAMBDA(&optional (id 0) prefix)CL_DEFUN T_mv core__startup_linkage_shutdown_names(size_t id, core::T_sp prefix)
{
  stringstream sstart;
  stringstream sshutdown;
  if (gc::IsA<String_sp>(prefix)) {
    sstart << gc::As<String_sp>(prefix)->get_std_string() << "-";
    sshutdown << gc::As<String_sp>(prefix)->get_std_string() << "-";
  } else if (prefix.notnilp()) {
    SIMPLE_ERROR(BF("Illegal prefix for startup function name: %s") % _rep_(prefix));
  }    
  sstart << MODULE_STARTUP_FUNCTION_NAME << "_" << id;
  sshutdown << MODULE_SHUTDOWN_FUNCTION_NAME << "_" << id;
  Symbol_sp linkage_type = llvmo::_sym_ExternalLinkage;
  return Values(core::SimpleBaseString_O::make(sstart.str()),linkage_type,core::SimpleBaseString_O::make(sshutdown.str()));
};


DOCGROUP(clasp)
CL_LAMBDA(&optional (id 0) prefix)CL_DEFUN T_mv core__startup_linkage(size_t id, core::T_sp prefix)
{
  T_mv result = core__startup_linkage_shutdown_names(id,prefix);
  T_sp result1 = result;
  T_sp result2 = result.second();
  return Values(result1,result2);
}


CL_LAMBDA(name &optional verbose print external-format)
CL_DECLARE();
CL_DOCSTRING(R"dx(load-binary)dx")
DOCGROUP(clasp)
CL_DEFUN T_mv core__load_binary(T_sp pathDesig, T_sp verbose, T_sp print, T_sp external_format) {
  DEPRECATED();
#if 0
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
    //    return (Values(nil<T_O>(), SimpleBaseString_O::make(error)));
  }
  // Static constructors must be available and they were run by dlopen
  //   and they registered startup_functions that we will now invoke
  //   to run the top-level forms.
  if (!startup_functions_are_waiting()) {
    printf("%s:%d No static constructors were run - what do we do in this situation????\n", __FILE__, __LINE__ );
    abort();
  }
  add_library adder;
  add_dynamic_library_using_handle(&adder,name,handle);
  Pointer_sp handle_ptr = Pointer_O::create(handle);
  DynamicScopeManager scope4(_sym_STARcurrent_dlopen_handleSTAR, handle_ptr);
  if (startup_functions_are_waiting()) {
    if (print.notnilp()) {
      write_bf_stream(BF("Running startup_functions\n"));
    }
    startup_functions_invoke(NULL);
  } else {
    SIMPLE_ERROR(BF("This is not a proper FASL file - there are no startup functions waiting to be invoked"));
  }
  T_mv result;
  return (Values(Pointer_O::create(handle), nil<T_O>()));
#endif
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
    //fprintf( stderr, "%s:%d Could not open %s - error: %s\n", __FILE__, __LINE__, str_path.c_str(), str_error.c_str());
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
CL_DOCSTRING(R"dx(dlopen - Open a dynamic library and return the handle.)dx")
CL_DOCSTRING_LONG(R"dx(Returns (values returned-value error-message(or nil if no error)))dx")
DOCGROUP(clasp)
CL_DEFUN T_mv core__dlopen(T_sp pathDesig) {

  int mode = RTLD_NOW | RTLD_GLOBAL;
  
  if (pathDesig.nilp()) SIMPLE_ERROR(BF("%s was about to pass nil to pathname") % __FUNCTION__);
  Pathname_sp path = cl__pathname(pathDesig);
  string ts0 = gc::As<String_sp>(cl__namestring(path))->get_std_string();

  auto result = do_dlopen( ts0, mode );
  void * handle = std::get<0>( result );

  if( handle == nullptr ) {
    return (Values(nil<T_O>(), SimpleBaseString_O::make( get<1>( result ))));
  }
  return (Values(Pointer_O::create(handle), nil<T_O>()));
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
std::tuple< void *, string > do_dlsym( void * p_handle, const char * pc_symbol ) {
  std::string str_error{ "" };
  void *p_sym = nullptr;
  dlerror(); // clear any earlier error
//  printf("%s:%d:%s  pc_symbol: %s\n", __FILE__, __LINE__, __FUNCTION__, pc_symbol );
  p_sym = dlsym( p_handle, pc_symbol );
  if( p_sym == nullptr ) {
    str_error = dlerror();
  }
  if (globals_->_ExportedSymbolsAccumulate) {
    maybe_register_symbol_using_dladdr(p_sym);
  }
  return std::make_tuple( p_sym, str_error );
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
CL_DOCSTRING(R"dx((dlsym handle name) handle is pointer from dlopen or :rtld-next, :rtld-self, :rtld-default or :rtld-main-only (see dlsym man page) returns ptr or nil if not found.)dx")
DOCGROUP(clasp)
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
    } else if (sym == kw::_sym_rtld_self) { //NOT PORTABLE TO LINUX
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
  string ts = name->get_std_string();
  
//  printf("%s:%d:%s  handle = %p  symbol = |%s|\n", __FILE__, __LINE__, __FUNCTION__, handle, ts.c_str());
  auto result = do_dlsym(handle, ts.c_str());
  void * p_sym = std::get<0>( result );
  if( p_sym == nullptr ) {
    return ( Values(nil<T_O>(), SimpleBaseString_O::make( get<1>( result ))) );
  }
  return ( Values(Pointer_O::create( p_sym ), nil<T_O>()) );
}

CL_DOCSTRING(R"dx((call dladdr with the address and return nil if not found or the contents of the Dl_info structure as multiple values))dx")
DOCGROUP(clasp)
CL_DEFUN void core__call_dl_main_function(Pointer_sp addr) {
  InitFnPtr mainFunctionPointer = (InitFnPtr)addr->ptr();
  (*mainFunctionPointer)(LCC_PASS_ARGS0_VA_LIST_INITFNPTR());
}

CL_DOCSTRING(R"dx((call dladdr with the address and return nil if not found or the contents of the Dl_info structure as multiple values))dx")
DOCGROUP(clasp)
CL_DEFUN T_mv core__dladdr(Pointer_sp addr) {
  uint64_t val = (uint64_t)addr->ptr();
  void *ptr = (void *)val;
  Dl_info info;
  int ret = dladdr(ptr, &info);
  if (!ret) {
    return Values(nil<T_O>());
  } else {
    return Values(SimpleBaseString_O::make(info.dli_fname),
                  Pointer_O::create(info.dli_fbase),
                  SimpleBaseString_O::make(info.dli_sname),
                  Pointer_O::create(info.dli_saddr));
  }
}

CL_LAMBDA(form &optional env)
DOCGROUP(clasp)
CL_DEFUN T_mv compiler__implicit_compile_hook_default(T_sp form, T_sp env) {
  // Convert the form into a thunk and return like COMPILE does
  LambdaListHandler_sp llh = LambdaListHandler_O::create(0);
  Cons_sp code = Cons_O::create(form, nil<T_O>());
  T_sp sourcePosInfo = nil<T_O>();
  stringstream ss;
  ss << "THE-IMPLICIT-COMPILE-REPL"; // << _lisp->nextReplCounter();
  Symbol_sp name = _lisp->intern(ss.str());
  ClosureWithSlots_sp ic = ClosureWithSlots_O::make_interpreted_closure(name,
                                                                        kw::_sym_function,
                                                                        nil<T_O>(),
                                                                        llh,
                                                                        nil<T_O>(),
                                                                        nil<T_O>(),
                                                                        code, env, SOURCE_POS_INFO_FIELDS(sourcePosInfo));
  Function_sp thunk = ic;
  return (thunk->entry())(LCC_PASS_ARGS0_ELLIPSIS(thunk.raw_()));
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
  T_sp val2 = nil<T_O>();
  if (val2.nilp()) {
    return val;
  }
  return val2;
};

T_sp allocCons() {
  Cons_sp fn = Cons_O::create(nil<T_O>(),nil<T_O>());
  return fn;
}

T_sp lexicalFrameLookup(T_sp fr, int depth, int index) {
  ASSERT(fr.isA<ActivationFrame_O>());
  ActivationFrame_sp af = gctools::reinterpret_cast_smart_ptr<ActivationFrame_O>(fr);
  T_sp val = core::value_frame_lookup_reference(af, depth, index);
  return val;
}

CL_LAMBDA(symbol value thunk)
CL_DECLARE();
CL_DOCSTRING(R"dx(Call THUNK with the given SYMBOL bound to to the given VALUE.)dx")
DOCGROUP(clasp)
CL_DEFUN T_mv core__call_with_variable_bound(Symbol_sp sym, T_sp val, Function_sp thunk) {
  DynamicScopeManager scope(sym, val);
  return (thunk->entry())(LCC_PASS_ARGS0_ELLIPSIS(thunk.raw_()));
}

}


extern "C" {
LCC_RETURN call_with_variable_bound(core::T_O* tsym, core::T_O* tval, core::T_O* tthunk) {
  core::Symbol_sp sym((gctools::Tagged)tsym);
  core::T_sp val((gctools::Tagged)tval);
  core::Function_sp func((gctools::Tagged)tthunk);
  core::DynamicScopeManager scope(sym, val);
  return (func->entry())(LCC_PASS_ARGS0_ELLIPSIS(func.raw_()));
}

};

namespace core {

// try/catch approach does work
DOCGROUP(clasp)
CL_DEFUN T_mv core__funwind_protect(T_sp protected_fn, T_sp cleanup_fn) {
  T_mv result;
  try {
    Closure_sp closure = gc::As_unsafe<Closure_sp>(protected_fn);
    ASSERT(closure);
    result = closure->entry()(LCC_PASS_ARGS0_ELLIPSIS(closure.raw_()));
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
      closure->entry()(LCC_PASS_ARGS0_ELLIPSIS(closure.raw_()));
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
    closure->entry()(LCC_PASS_ARGS0_ELLIPSIS(closure.raw_()));
  }
  return returnTypeLoadFromTemp(nvals, mv_temp);
}

CL_LAMBDA(function &rest thunks)
CL_DECLARE();
CL_DOCSTRING(R"dx(multipleValueFuncall)dx")
DOCGROUP(clasp)
CL_DEFUN T_mv core__multiple_value_funcall(Function_sp fmv, List_sp thunks) {
  MAKE_STACK_FRAME(frame, fmv.raw_(), MultipleValues::MultipleValuesLimit);
  size_t numArgs = 0;
  size_t idx = 0;
  MultipleValues& mv = lisp_multipleValues();
  for (auto cur : thunks) {
    Function_sp tfunc = gc::As<Function_sp>(oCar(cur));
    T_mv result = (tfunc->entry())(LCC_PASS_ARGS0_ELLIPSIS(tfunc.raw_()));
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

CL_LAMBDA(tag func)
CL_DECLARE();
CL_DOCSTRING(R"dx(catchFunction)dx")
DOCGROUP(clasp)
CL_DEFUN T_mv core__catch_function(T_sp tag, Function_sp thunk) {
  T_mv result;
  CLASP_BEGIN_CATCH(tag) {
    result = thunk->entry()(LCC_PASS_ARGS0_ELLIPSIS(thunk.raw_()));
  } CLASP_END_CATCH(tag, result);
  return result;
}

CL_LAMBDA(tag result)
CL_DECLARE();
CL_DOCSTRING(R"dx(Like CL:THROW, but takes a thunk)dx")
DOCGROUP(clasp)
CL_DEFUN void core__throw_function(T_sp tag, T_sp result_form) {
  T_mv result;
  Closure_sp closure = result_form.asOrNull<Closure_O>();
  ASSERT(closure);
  result = closure->entry()(LCC_PASS_ARGS0_ELLIPSIS(closure.raw_()));
  result.saveToMultipleValue0();
  clasp_throw(tag);
}

CL_LAMBDA(symbols values func)
CL_DECLARE();
CL_DOCSTRING(R"dx(progvFunction)dx")
DOCGROUP(clasp)
CL_DEFUN T_mv core__progv_function(List_sp symbols, List_sp values, Function_sp func) {
  if (symbols.consp()) {
    if (values.consp()) {
      DynamicScopeManager scope(gc::As<Symbol_sp>(CONS_CAR(symbols)),CONS_CAR(values));
      return core__progv_function(CONS_CDR(symbols),oCdr(values),func);
    } else {
      DynamicScopeManager scope(gc::As<Symbol_sp>(CONS_CAR(symbols)),unbound<core::T_O>());
      return core__progv_function(CONS_CDR(symbols),nil<core::T_O>(),func);
    }
  } else {
    T_mv result = (func->entry())(LCC_PASS_ARGS0_ELLIPSIS(func.raw_()));
  // T_mv result = eval::funcall(func);
    return result;
  }
}

DOCGROUP(clasp)
CL_DEFUN T_mv core__declared_global_inline_p(T_sp name)
 {
   return gc::As<HashTableEqual_sp>(_sym_STARfunctions_to_inlineSTAR->symbolValue())->gethash(name);
 }

DOCGROUP(clasp)
CL_DEFUN T_mv core__declared_global_notinline_p(T_sp name)
 {
   return gc::As<HashTableEqual_sp>(_sym_STARfunctions_to_notinlineSTAR->symbolValue())->gethash(name);
 }



DOCGROUP(clasp)
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
    LCC_RETURN ret = func(LCC_PASS_ARGS0_VA_LIST(nil<T_O>().raw_()));
    core::T_sp res((gctools::Tagged)ret.ret0[0]);
    core::T_sp val = res;
    return val;
  }
  return nil<T_O>();
}

DOCGROUP(clasp)
CL_DEFUN T_sp core__run_make_mlf( T_sp object ) {
  return core__run_function(object);
}

DOCGROUP(clasp)
CL_DEFUN T_sp core__run_init_mlf( T_sp object ) {
  return core__run_function(object);
}

DOCGROUP(clasp)
CL_DEFUN T_sp core__make_builtin_class( T_sp object ) {
  printf("%s:%d:%s  with %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(object).c_str());
  SIMPLE_ERROR(BF("Add support for core__make_builtin_class"));
}


DOCGROUP(clasp)
CL_DEFUN T_sp core__handle_creator( T_sp object ) {
  SIMPLE_ERROR(BF("Handle-creator for %s") % _rep_(object).c_str());
}

 SYMBOL_EXPORT_SC_(CompPkg, STARimplicit_compile_hookSTAR);
 SYMBOL_EXPORT_SC_(CompPkg, implicit_compile_hook_default);
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


DOCGROUP(clasp)
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


  
DOCGROUP(clasp)
CL_DEFUN size_t core__ltvc_write_size_t(T_sp object, T_sp stream, size_t index)
{
  SELF_DOCUMENT(size_t,stream,index);
  size_t data = clasp_to_size_t(object);
  compact_write_size_t(data, stream, index);
  return index;
}

size_t ltvc_read_size_t(T_sp stream, bool log, size_t& index)
{
  SELF_CHECK(size_t,stream,index);
  size_t data = compact_read_size_t(stream,index);
  if (log) printf("%s:%d:%s -> %lu\n", __FILE__, __LINE__, __FUNCTION__, data);
  return data;
}

DOCGROUP(clasp)
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

DOCGROUP(clasp)
CL_DEFUN size_t core__ltvc_write_bignum(T_sp object, T_sp stream, size_t index)
{
  SELF_DOCUMENT(long long,stream,index);
  core::Bignum_sp bignum = gc::As<Bignum_sp>(object);
  mp_size_t length = bignum->length();
  const mp_limb_t* limbs = bignum->limbs();
  compact_write_size_t(length, stream, index);
  for (mp_size_t i = 0; i < std::abs(length); i++)
    compact_write_size_t(limbs[i], stream, index);
  return index;
}

T_O* ltvc_read_bignum(T_sp stream, bool log, size_t& index)
{
  SELF_CHECK(long long,stream,index);
  mp_size_t length = compact_read_size_t(stream, index);
  size_t size = std::abs(length);
  mp_limb_t limbs[size];
  for (mp_size_t i = 0; i < size; i++) {
    limbs[i] = compact_read_size_t(stream, index);
  }
  return reinterpret_cast<T_O*>(Bignum_O::create_from_limbs(length, 0, false, size, limbs).raw_());
}

DOCGROUP(clasp)
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

DOCGROUP(clasp)
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

CL_DOCSTRING(R"dx(tag is (0|1|2) where 0==literal, 1==transient, 2==immediate)dx")
DOCGROUP(clasp)
CL_DEFUN size_t core__ltvc_write_object(T_sp ttag, T_sp index_or_immediate, T_sp stream, size_t index)
{
  SELF_DOCUMENT(T_O*,stream,index);
  if (ttag.characterp() && (ttag.unsafe_character()=='l'||ttag.unsafe_character()=='t'||ttag.unsafe_character()=='i')) {
    char tag = ttag.unsafe_character();
    clasp_write_char(tag,stream);
    index += 1;
    size_t data;
    if (ttag.unsafe_character()=='l'||ttag.unsafe_character()=='t') {
      data = index_or_immediate.unsafe_fixnum();
    } else {
      // Immediate data.
      // Note that the immediate may be signed, so we have to convert
      // it to an unsigned like this.
      data = clasp_to_ssize_t(index_or_immediate);
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
    cur_cons->rplaca(cur_vargs->ocar());
    cur = cur_cons->cdr();
    vargs = cur_vargs->cdr();
  }
}

void ltvc_mlf_create_basic_call_varargs(gctools::GCRootsInModule* holder,
                                        char tag, size_t index,
                                        T_O* fname, size_t len, Cons_O* varargs) {
  T_sp tfname((gctools::Tagged)fname);
  T_sp tvarargs((gctools::Tagged)varargs);
  T_sp val = core__apply0(coerce::functionDesignator(tfname), tvarargs);
  holder->setTaggedIndex(tag, index, val.tagged_());
}

void ltvc_mlf_init_basic_call_varargs(gctools::GCRootsInModule* holder,
                                      T_O* fname, size_t len, Cons_O* varargs) {
  (void)len; // don't need it.
  T_sp tfname((gctools::Tagged)fname);
  T_sp tvarargs((gctools::Tagged)varargs);
  core__apply0(coerce::functionDesignator(tfname), tvarargs);
}


void dump_byte_code(T_sp fin, size_t length, bool useFrom=false, size_t from=0) {
  StringInputStream_sp sis = gc::As<StringInputStream_sp>(fin);
  string peer;
  if (useFrom) {
    peer = sis->peerFrom(from,length);
  } else {
    from = sis->_InputPosition;
    peer = sis->peer(length);
  }
  write_bf_stream(BF("%8lu: ") % from);
  for (int i=0; i<peer.size(); ++i ) {
    unsigned char cc = (unsigned char)peer[i];
    if ( cc<32 ) {
      write_bf_stream(BF("(\\%d)") % (int)cc);
    } else if (cc>=128) {
      write_bf_stream(BF("(\\%d)") % (int)cc);
    } else {
      write_bf_stream(BF("(%c\\%d)") % (char)cc % (int)cc);
    }
  }
  write_bf_stream(BF("\n"));
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
    if (log) {
      printf("%s:%d ------- top of byte-code interpreter\n", __FILE__, __LINE__ );
      printf("%s:%d byte_index = %zu\n",__FILE__, __LINE__,  byte_index);
    }
    // dump_byte_code(fin,32);
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

void initialize_compiler_primitives(LispPtr lisp) {

  // Initialize raw object translators needed for Foreign Language Interface support 
  llvmo::initialize_raw_translators(); // See file intrinsics.cc!

  comp::_sym_STARimplicit_compile_hookSTAR->defparameter(comp::_sym_implicit_compile_hook_default->symbolFunction());
  cleavirPrimops::_sym_callWithVariableBound->setf_symbolFunction(_sym_callWithVariableBound->symbolFunction());

  return;
}

}; /* namespace */




namespace core {

std::atomic<int> global_sigchld_count{0};

DOCGROUP(clasp)
CL_DEFUN int core__sigchld_count() {
  return global_sigchld_count;
}

DOCGROUP(clasp)
CL_DEFUN int core__decf_sigchld_count() {
  global_sigchld_count--;
  return global_sigchld_count;
}

void sigchld(int signal) {
  global_sigchld_count++;
}

DOCGROUP(clasp)
CL_DEFUN void core__install_sigchld() {
  signal(SIGCHLD, sigchld);
}

DOCGROUP(clasp)
CL_DEFUN void core__uninstall_sigchld() {
  signal(SIGCHLD, SIG_DFL);
}

DOCGROUP(clasp)
CL_DEFUN void core__call4(int x, int y, int z, int w) {
  printf("%s:%d call4 args: %d, %d, %d, %d\n", __FILE__, __LINE__, x, y, z, w );
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
