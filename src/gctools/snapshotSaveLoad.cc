/*
    File: snapshotSaveLoad.cc

*/



#define DEBUG_LEVEL_FULL

//#include <llvm/Support/system_error.h>
#include <dlfcn.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dlfcn.h>
#include <unistd.h>
#include <sys/mman.h>
#include <filesystem>

#include <iomanip>

#include <clasp/external/bloom/bloom_filter.h>

#include <clasp/core/foundation.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/debugger.h>
#include <clasp/core/functor.h>
#include <clasp/core/null.h>
#include <clasp/core/bundle.h>
#include <clasp/core/lisp.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/evaluator.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/core/compiler.h>
#include <clasp/core/sort.h>
#include <clasp/llvmo/code.h>
#include <clasp/gctools/gc_boot.h>
#include <clasp/gctools/snapshotSaveLoad.h>

#ifdef _TARGET_OS_LINUX
#include <elf.h>
#endif

#define __EX(var) #var
#define CXX_MACRO_STRING(var) __EX(var)

namespace snapshotSaveLoad {

size_t memory_test(bool dosleep, FILE* fout, const char* message = NULL );

FixupOperation_ operation(Fixup* fixup) { return fixup->_operation; };

bool global_debugSnapshot = false;

};

namespace snapshotSaveLoad {


struct MaybeTimeStartup {
  std::chrono::time_point<std::chrono::steady_clock> start;
  std::string  name;
  bool         timerOn;
  MaybeTimeStartup(const char* n = NULL) : timerOn(false) {
    if (n) this->name = n;
    if (getenv("CLASP_TIME_SNAPSHOT")) {
      this->timerOn = true;
      this->start = std::chrono::steady_clock::now();
    }
  };

  ~MaybeTimeStartup() {
    if (this->timerOn) {
      auto end = std::chrono::steady_clock::now();
      auto us = std::chrono::duration_cast<std::chrono::microseconds>(end - this->start);
      stringstream name_;
      if (this->name!="") name_ << this->name << " ";
      printf("%s us %s\n", _rep_(core::Integer_O::create(us.count())).c_str(), name_.str().c_str() );
    }
  };
};



/*! Build a LibraryLookup by running 'nm' on one of our loaded libraries or executable.
 *  For dynamic libraries on linux (contain .so in filename) use --dynamic because regular symbols are often stripped
 *  Look for the first 'T' symbol and dlsym it to find out where the library is loaded in memory.
 * If fout != NULL then write logging information to fout.
 */
DONT_OPTIMIZE_WHEN_DEBUG_RELEASE
bool loadLibrarySymbolLookup(const std::string& filename, LibraryLookup& libraryLookup, FILE* fout ) {
  if (fout) {
    fprintf( fout, "# Library %s\n", filename.c_str() );
  }
#define BUFLEN 2048
  int baddigit = 0;
  size_t lineno = 0;
  struct stat buf;
  if (stat(filename.c_str(),&buf)!=0) {
    return false;
  }
  stringstream nm_cmd;
  uintptr_t textRegionStart = 0;
  bool gotSearchSymbol = false;
  std::string searchSymbol;
  uintptr_t searchAddress = 0;
  uintptr_t search_dlsym = 0;
  uintptr_t loadAddress = 0;
  bool gotLoadAddress = false;
#if defined(_TARGET_OS_LINUX)
  std::string dynamic = "";
  if (filename.find(".so") != std::string::npos) dynamic = "--dynamic ";
  nm_cmd << NM_BINARY << " " << dynamic << "-p --defined-only --no-sort \"" << filename << "\"";
#elif defined(_TARGET_OS_DARWIN)
  gctools::clasp_ptr_t start;
  gctools::clasp_ptr_t end;
  core::executableTextSectionRange( start, end );
  textRegionStart = (uintptr_t)start;
  nm_cmd << NM_BINARY << " -p --defined-only \"" << filename << "\"";
#else
#error "Handle other operating systems - how is main found using dlsym and in the output of nm"
  nm_cmd << NM_BINARY << " -p --defined-only \"" << filename << "\"";
#endif
  if (fout) fprintf(fout, "# Symbols obtained by filtering: %s\n", nm_cmd.str().c_str() );
  FILE* fnm = popen( nm_cmd.str().c_str(), "r");
  if (fnm==NULL) {
    printf("%s:%d:%s  Could not popen %s\n", __FILE__, __LINE__, __FUNCTION__, nm_cmd.str().c_str());
    return false;
  }
  {
    char* buf = NULL;
    size_t buf_len = 0;
    char fullname[BUFLEN+1];
    char name[BUFLEN+1];
    const char* version;
    size_t lineno = 0;
    char prev_type = '\0';
    uintptr_t prev_real_address;
    std::string prev_sname;
    uintptr_t highest_code_address(0);
    uintptr_t lowest_other_address(~0);
    while (!feof(fnm)) {
      int result = getline(&buf,&buf_len,fnm);
      if (result==-1) {
        if (feof(fnm)) break;
        printf("%s:%d Error reading from %s errno->(%d/%s) after %lu lines\n", __FILE__, __LINE__, filename.c_str(), errno, strerror(errno), lineno);
      }
      lineno++;
      if (feof(fnm)) break;
      if (!buf) {
        printf("%s:%d buf is 0x0 when reading output from %s\n", __FILE__, __LINE__, nm_cmd.str().c_str());
        break;
      }
      const char* cur = buf;
      // printf("%s:%d:%s Read line: %s", __FILE__, __LINE__, __FUNCTION__, cur);
      // Read the address
      uintptr_t address = 0;
      uintptr_t digit;
      ++lineno;
      // Read the hex address
      while (*cur != ' ') {
        char c = *cur;
        if (c>='A'&&c<='Z') {
          digit = c-'A'+10;
        } else if (c>='0'&&c<='9') {
          digit = c-'0';
        } else if (c>='a'&&c<='z') {
          digit = c-'a'+10;
        } else {
          if (baddigit<20) {
            printf("%s:%d:%s In file: %s lineno: %lu\n", __FILE__, __LINE__, __FUNCTION__, filename.c_str(), lineno);
            printf("%s:%d:%s Hit non-hex digit %c in line: %s\n", __FILE__,__LINE__,__FUNCTION__,c,buf);
            baddigit++;
          }
          digit = 0;
        }
        address = address*16+digit;
//        printf("cur: %p c: %c digit: %lu   address: %p\n", cur, c, digit, (void*)address);
        ++cur;
      }
      // skip spaces
      while (*cur==' ') ++cur;
      // Read the type
      char type = *cur;
      cur++;
      // skip spaces
      while (*cur==' ') ++cur;
      // Read the name
      size_t nameidx = 0;
      version = NULL;
      while (nameidx<(BUFLEN-1)&&*cur!='\0'&&*cur>' ') {
        fullname[nameidx] = *cur;
        // If we hit a '@' then we are seeing an "@@LLVM_13"
        //  suffix - we need to keep track of that
        if (!version) {
          if (*cur=='@') {
            // record the start of the version string
            version = cur;
            // terminate the name part
            name[nameidx] = '\0';
          } else {
            name[nameidx] = *cur;
          }
        }
        ++cur;
        ++nameidx;
        if (nameidx>=BUFLEN) {
          printf("%s:%d The buffer size needs to be increased beyond %d\n", __FILE__, __LINE__, BUFLEN);
          abort();
        }
      }
      fullname[nameidx] = '\0';
      name[nameidx] = '\0'; // this may be redundant
      // now fullname contains the full name of the symbol - including any @@ suffix
      // name will contain the name to the end or to the first '@'
      // version will point into fullname starting at the first '@' or NULL
      // We should check that version is a library version that we accept
      if (version) {
        if (strncmp(version,"@@LLVM_",7)!=0) {
          printf("%s:%d:%s We encountered a symbol that does not have the correct version \"@@LLVM_xxx\" - it has \"%s\" - we need to generalize this version test\n", __FILE__, __LINE__, __FUNCTION__, version );
        } else {
          const char* versionNumStr = version+7; // advance to the number of @@LLVM_number
          int versionNum = atoi(versionNumStr);
          if (versionNum != LLVM_VERSION_INT) {
            printf("%s:%d:%s We encountered a symbol that does not have the correct version \"@@LLVM_%d\" - it has \"%s\"\n", __FILE__, __LINE__, __FUNCTION__, LLVM_VERSION_INT, version );
            abort();
          }
        } // fall through because we don't worry about versions right now
      }
      std::string sname(name);
      bool useSymbol = false;
      if (type == 't' ||
          type == 'T' ||
          type == 'W' ||
          type == 'V' ||
          type == 'D' ||
          type == 's' ||
          type == 'S') useSymbol = true;
      if (fout) {
        if (gotLoadAddress) {
          if (useSymbol) {
            fprintf( fout, "%p (abs: %p) %c %s\n", (void*)address, (void*)(address+loadAddress), type, sname.c_str() );
          } else {
            fprintf( fout, "#ignore %p (abs: %p) %c %s\n", (void*)address, (void*)(address+loadAddress), type, sname.c_str() );
          }
        } else {
          if (useSymbol) {
            fprintf(fout, "%p %c %s\n", (void*)address, type, sname.c_str());
          } else {
            fprintf(fout, "#ignore %p %c %s\n", (void*)address, type, sname.c_str());
          }
        }
        if (fout) fflush(fout);
      }
      if (useSymbol) {
        if (!gotSearchSymbol && type == 'T' && sname!="") {
          // Save the searchSymbol
          searchSymbol = sname;
          searchAddress = address;
          gotSearchSymbol = true;
  // We may need to fix up the searchSymbol on different OS - like macOS may need '_' prefix.
#if defined(_TARGET_OS_DARWIN)
          std::string realSearchSymbol = searchSymbol.substr(1); // WHY DO WE NEED TO STRIP AN UNDERSCORE!!!!!!!
          if (fout) fprintf( fout, "# DARWIN mangled name: %s\n", realSearchSymbol.c_str());
#elif defined(_TARGET_OS_LINUX)
          std::string realSearchSymbol = searchSymbol;
#else
# error "Handle name mangling for other OS"
#endif
          search_dlsym = (uintptr_t)dlsym( RTLD_DEFAULT, realSearchSymbol.c_str() );
          if (search_dlsym==0) {
            if (fout) fprintf( fout, "# Could not find address of \"%s\" with dlsym!!!\n", realSearchSymbol.c_str() );
            printf("%s:%d:%s Could not find address of \"%s\" with dlsym - searching for next symbol to anchor library\n", __FILE__, __LINE__, __FUNCTION__, realSearchSymbol.c_str() );
            gotSearchSymbol = false; // try again
          } else {
            loadAddress = (search_dlsym - searchAddress); // calculate where library is loaded
            gotLoadAddress = true;
            libraryLookup._loadAddress = loadAddress;
            if (fout) {
              fprintf( fout, "# realSearchSymbol = \"%s\" searchAddress = %p search_dlsym = %p  libraryLookup._loadAddress = %p\n", realSearchSymbol.c_str(), (void*)searchAddress, (void*)search_dlsym, (void*)libraryLookup._loadAddress ); 
              fprintf( fout, "# library load address is %p\n", (void*)loadAddress);
            }
          }
        }
        libraryLookup._symbolToAddress[sname] = address;
        libraryLookup._addressToSymbol[address] = sname;
        if (address>highest_code_address) {
          highest_code_address = address;
        }
      } else {
        if (highest_code_address && address > highest_code_address) {
          if (address < lowest_other_address) {
            lowest_other_address = address;
          }
        }
      }
    }
    libraryLookup._symbolToAddress["__TAIL_SYMBOL"] = lowest_other_address; // The last symbol is to define the size of the last code symbol
    libraryLookup._addressToSymbol[lowest_other_address] = "__TAIL_SYMBOL";
//    libraryLookup.addSymbol("TERMINAL_SYMBOL",~0,'d');  // one symbol to end them all
    if (buf) free(buf);
    pclose(fnm);
  }

  // Now dlsym the searchSymbol
  if (searchAddress == 0) {
    if (fout) fprintf( fout, "%s:%d:%s Could not find any symbols in %s\n", __FILE__, __LINE__, __FUNCTION__, filename.c_str() );
  }
  return true;
}


bool SymbolLookup::addLibrary( const std::string& libraryPath, FILE* fout ) {
  LibraryLookup* lib = new LibraryLookup(libraryPath);
  this->_Libraries.emplace_back(lib);
  return loadLibrarySymbolLookup( libraryPath, *lib, fout );
}


void SymbolLookup::addAllLibraries(FILE* fout) {
  for ( auto& entry : core::debugInfo()._OpenDynamicLibraryHandles ) {
    if (fout) fprintf( fout, "#  entry.name = %s\n", entry.second._Filename.c_str() );
    this->addLibrary( entry.second._Filename, fout );
  }
}


};

#ifdef USE_PRECISE_GC

// #define DEBUG_SL 1
#if 0
#define DBG_SL(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str()); fflush(stdout); }
#else
#define DBG_SL(_fmt_)
#endif

#if 0
#define DBG_SLS(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str()); fflush(stdout); }
#else
#define DBG_SLS(_fmt_)
#endif
#if 0
#define DBG_SL1(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL1(_fmt_)
#endif
#if 0
#define DBG_SL_ALLOCATE(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_ALLOCATE(_fmt_)
#endif

#if 0
#define DBG_SAVECOPY(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SAVECOPY(_fmt_)
#endif

#if 0
#define DBG_SL_ROOT(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_ROOT(_fmt_)
#endif

#if 0
#define DBG_SL_FWD(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_FWD(_fmt_)
#endif

#if 0
#define DBG_SL_FFWD(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_FFWD(_fmt_)
#endif

#if 0
#define DBG_SL_RELOCATE0(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_RELOCATE0(_fmt_)
#endif

#if 0
#define DBG_SL_DONTWALK(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_DONTWALK(_fmt_)
#endif

#if 0
#define DBG_SL_WALK_SL(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_WALK_SL(_fmt_)
#endif

#if 0
#define DBG_SL_WALK_GC(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_WALK_GC(_fmt_)
#endif

#if 0
#define DBG_SL_WALK_TEMP(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_WALK_TEMP(_fmt_)
#endif

#if 0
#define DBG_SL_VTABLE(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_VTABLE(_fmt_)
#endif

#if 0
#define DBG_SL_ENTRY_POINT(_fmt_) { printf("%s:%d:%s ", __FILE__, __LINE__, __FUNCTION__ ); printf("%s",  (_fmt_).str().c_str());}
#else
#define DBG_SL_ENTRY_POINT(_fmt_)
#endif



;

namespace snapshotSaveLoad {
bool globalFwdMustBeInGCMemory = false;
#define DEBUG_SL_FFWD 1

void check_dladdr(uintptr_t address) {
  // do nothing for now
}

void decodeRelocation_(uintptr_t codedAddress, uint8_t& firstByte, uintptr_t& libindex, uintptr_t& offset) {
//  uintptr_t codedAddress = *ptrptr;
  offset = (uintptr_t)codedAddress & (((uintptr_t)1<<32) - 1);
  libindex = ((uintptr_t)codedAddress>>32) & (uintptr_t)0xff;
  firstByte = (uint8_t)(((uintptr_t)codedAddress>>48) & 0xff);
}

uintptr_t encodeRelocation_( uint8_t firstByte, size_t libraryIndex, size_t relocationOrOffset ) {
  if ((relocationOrOffset&(((uintptr_t)1<<32)-1)) != relocationOrOffset) {
    printf("%s:%d:%s relocationOrOffset %lu is too large\n", __FILE__, __LINE__, __FUNCTION__, relocationOrOffset );
    abort();
  }
  if (libraryIndex > 256) {
    printf("%s:%d:%s libraryIndex %lu is too large\n", __FILE__, __LINE__, __FUNCTION__, libraryIndex );
    abort();
  }
  uintptr_t result = ( (uintptr_t)1<<56 | (uintptr_t)firstByte << 48) | (libraryIndex << 32) | relocationOrOffset;
  return result;
}


uintptr_t Fixup::fixedAddress(bool functionP, uintptr_t* ptrptr, const char* addressName ) {
  uint8_t firstByte;
  uintptr_t libidx;
  uintptr_t pointerIndex;
  uintptr_t value = *ptrptr;
  decodeRelocation_( value, firstByte, libidx, pointerIndex );
//  printf("%s:%d:%s libidx = %lu pointerIndex = %lu\n", __FILE__, __LINE__, __FUNCTION__, libidx, pointerIndex );
  uintptr_t address = this->_libraries[libidx]._GroupedPointers[pointerIndex]._address;
//  printf("%s:%d:%s address = %p @ %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)address, &this->_libraries[libidx]._GroupedPointers[pointerIndex]._address );
  uintptr_t addressOffset = this->_libraries[libidx]._SymbolInfo[pointerIndex]._AddressOffset;
//  printf("%s:%d:%s addressOffset = %lu\n", __FILE__, __LINE__, __FUNCTION__, addressOffset );
  uintptr_t ptr = address + addressOffset;
  if ( functionP && *(uint8_t*)ptr != firstByte) {
    printf("%s:%d:%s during decode %s %p must be readable and point to first byte: 0x%x - but it points to 0x%x  libidx: %lu\n",
           __FILE__, __LINE__, __FUNCTION__,
           addressName,
           (void*)ptr,
           (uint32_t)firstByte,
           (uint32_t)*(uint8_t*)ptr,
           libidx );
  }
//  printf("%s:%d:%s Returning fixed %s address %p @ %p \n", __FILE__, __LINE__, __FUNCTION__, addressName, (void*)ptr, (void*)ptrptr);
  return ptr;
}



void registerVtable(Fixup* fixup, core::T_O* vtablePtrPtr, uintptr_t vtableRegionStart) {
  size_t libraryIndex = fixup->ensureLibraryRegistered(*(uintptr_t*)vtablePtrPtr);
  fixup->registerVtablePointer(libraryIndex,vtablePtrPtr);
}

uintptr_t decodeVtable(Fixup* fixup, uintptr_t* vtablePtr, uintptr_t vtableRegionStart) {
  uintptr_t address = fixup->fixedAddress(false,vtablePtr,"vtable");
  gctools::clasp_ptr_t ptr = (gctools::clasp_ptr_t)address;
  return (uintptr_t)ptr;
}

#if 0 // Deprecated
void* encodePointer(Fixup* fixup, gctools::clasp_ptr_t address,size_t idx, gctools::clasp_ptr_t start) {
  check_dladdr((uintptr_t)address);
  uint8_t firstByte = *(uint8_t*)address; // read the first byte
  uintptr_t offset = (uintptr_t)address - (uintptr_t)start;
  if (!start) {
    printf("%s:%d:%s The start address can not be zero!!!\n", __FILE__, __LINE__, __FUNCTION__ );
  }
  uintptr_t result = ((uintptr_t)firstByte << 56) | idx<<40 | offset;
  DBG_SL_ENTRY_POINT(BF("Library base: %p encode %p/%d -> %p\n") % (void*)start % (void*)address % idx % (void*)result );
  return (void*)result;
}
#endif


size_t Fixup::ensureLibraryRegistered(uintptr_t address) {
  for (size_t idx = 0; idx<this->_libraries.size(); idx++ ) {
    if (((uintptr_t)this->_libraries[idx]._TextStart)<=address && address<((uintptr_t)this->_libraries[idx]._TextEnd)) {
      return idx;
    }
    if (this->_libraries[idx]._VtableStart<=address && address<this->_libraries[idx]._VtableEnd) {
      return idx;
    }
  }
  gctools::clasp_ptr_t start;
  gctools::clasp_ptr_t end;
  uintptr_t vtableStart;
  uintptr_t vtableEnd;
  std::string libraryPath;
  bool isExecutable;
  core::lookup_address_in_library( (gctools::clasp_ptr_t)address,start,end,libraryPath,isExecutable,vtableStart,vtableEnd);
  ISLLibrary lib(libraryPath,isExecutable,start,end,vtableStart,vtableEnd);
  size_t idx = this->_libraries.size();
  this->_libraries.push_back(lib);
  return idx;
};
  
uintptr_t encodeEntryPointValue( uint8_t firstByte, uint8_t epType, uintptr_t offset ) {
  uintptr_t result = encodeRelocation_( firstByte, epType, offset );
  return result;
}

void decodeEntryPointValue( uintptr_t value, uint8_t& firstByte, uintptr_t& epType, uintptr_t& offset ) {
  decodeRelocation_( value, firstByte, epType, offset );
};

uintptr_t decodeEntryPointAddress( uintptr_t offset, uintptr_t codeStart, uintptr_t codeEnd, llvmo::CodeBase_sp code ) {
  if (!codeStart || !codeEnd || !(codeStart<codeEnd) ) {
    printf("%s:%d:%s The start codeStart %p and codeEnd %p are not ascending values for code: %p!!!\n", __FILE__, __LINE__, __FUNCTION__, (void*)codeStart, (void*)codeEnd, (void*)code.raw_() );
    abort();
  }
  if (!(offset<(codeEnd-codeStart)) ) {
    printf("%s:%d:%s The offset %lu does not fall between codeStart %p and codeEnd %p (diff is %lu) for code: %p!!!\n", __FILE__, __LINE__, __FUNCTION__, offset, (void*) codeStart, (void*)codeEnd, (codeEnd-codeStart), (void*)code.raw_() );
    abort();
  }
  uintptr_t result = offset + codeStart;
  return result;
}

uintptr_t encodeEntryPointOffset( uintptr_t address, uintptr_t codeStart, uintptr_t codeEnd, llvmo::CodeBase_sp code ) {
  if (!codeStart || !codeEnd || !(codeStart<codeEnd)) {
    printf("%s:%d:%s The codeStart %p and codeEnd %p for code: %p do not have reasonable, ascending values\n", __FILE__, __LINE__, __FUNCTION__, (void*)codeStart, (void*)codeEnd, (void*)code.raw_() );
    abort();
  }
  // Check if address falls outside of the code range
  //   if this is so then it must be falling within a library
  if (address<codeStart || codeEnd <= address ) return false;
  uintptr_t offset = (address - codeStart);
  if (offset > (codeEnd-codeStart)) {
    printf("%s:%d:%s The offset %lu is too large for the codeStart %p to codeEnd %p (diff %lu)\n",
           __FILE__, __LINE__, __FUNCTION__, offset, (void*)codeStart, (void*)codeEnd, (codeEnd-codeStart ) );
  }
  if (offset<0) {
    printf("%s:%d:%s Generating a vaddress that is negative: %p address: %p codeStart: 0x%" PRIxPTR "\n", __FILE__, __LINE__, __FUNCTION__, (void*)offset, (void*)address, codeStart );
    abort();
  }
  return offset;
}

void encodeEntryPointInLibrary(Fixup* fixup, uintptr_t* ptrptr) {
  size_t libraryIndex = fixup->ensureLibraryRegistered(*ptrptr);
  fixup->registerFunctionPointer(libraryIndex,ptrptr);
}

void decodeEntryPointInLibrary(Fixup* fixup, uintptr_t* ptrptr ) {
  uintptr_t address = fixup->fixedAddress(true,ptrptr,"function-pointer");
  gctools::clasp_ptr_t ptr = (gctools::clasp_ptr_t)address;
//  printf("%s:%d:%s Fixing @%p -> %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)ptrptr, (void*)ptr );
  *ptrptr = (uintptr_t)ptr;
}



/* Return true if the entry point was encoded. 
   Return false if the entry point doesn't fall within the code region because it points
   into the executable - in that case we need to use encodeEntryPointForLibrary.
*/
bool encodeEntryPointForCompiledCode(Fixup* fixup, uintptr_t* ptrptr, llvmo::Code_sp code) {
  uintptr_t address = *ptrptr;
  uint8_t firstByte = *(uint8_t*)address;
  uintptr_t codeStart = (uintptr_t)code->codeStart();
  uintptr_t codeEnd = (uintptr_t)code->codeEnd();
  if (address<codeStart || codeEnd <= address) return false;
  uintptr_t offset = encodeEntryPointOffset( address, codeStart, codeEnd, code );
  uintptr_t result = encodeEntryPointValue(firstByte,CODE_LIBRARY_ID,offset);
  DBG_SL_ENTRY_POINT(BF("base: %p encoded %p -> %6p %s\n") % (void*)code->codeStart() % vaddress % result % code->filename() );
  *ptrptr = result;
  return true;
}

bool decodeEntryPointForCompiledCode(Fixup* fixup, uintptr_t* ptrptr, llvmo::Code_sp code) {
  uintptr_t vaddress = *ptrptr;
  uint8_t firstByte;
  uintptr_t epType;
  uintptr_t offset;
  uintptr_t codeStart = code->codeStart();
  uintptr_t codeEnd = code->codeEnd();
  decodeEntryPointValue( vaddress, firstByte, epType, offset );
  if (epType != CODE_LIBRARY_ID ) return false; // it's not a COMPILED_CODE_EPTYPE it must be to a library
  uintptr_t result = decodeEntryPointAddress( offset, codeStart, codeEnd, code );
  DBG_SL_ENTRY_POINT(BF("base: %p decoded %p -> %6p %s\n") % (void*)code->codeStart() % vaddress % result % code->filename() );
  if (*(uint8_t*)result != firstByte) {
    printf("%s:%d:%s during decode function pointer %p must be readable and point to 0x%x (first byte) - instead it points to 0x%x vaddress = %p  codeStart = %p\n",
           __FILE__, __LINE__, __FUNCTION__, (void*)result, (uint32_t)firstByte,
           (uint)(*(uint8_t*)result),
           (void*)vaddress, (void*)codeStart );
    abort();
  }
  *ptrptr = result;
  return true;
}


void encodeEntryPoint(Fixup* fixup, uintptr_t* ptrptr, llvmo::CodeBase_sp codebase) {
  if (gc::IsA<llvmo::Code_sp>(codebase)) {
    llvmo::Code_sp code = gc::As_unsafe<llvmo::Code_sp>(codebase);
    if (!encodeEntryPointForCompiledCode(fixup,ptrptr,code)) {
      // The entry point wasnt into the compiled code
      //   so it must be to one of the libraries - apply that fixup.
      encodeEntryPointInLibrary( fixup, ptrptr );
    }
  } else if (gc::IsA<llvmo::Library_sp>(codebase)) {
    encodeEntryPointInLibrary(fixup,ptrptr);
  } else {
    SIMPLE_ERROR(("The codebase must be a Code_sp or a Library_sp it is %s") , _rep_(codebase) );
  }
}

void decodeEntryPoint(Fixup* fixup, uintptr_t* ptrptr, llvmo::CodeBase_sp codebase) {
  if (gc::IsA<llvmo::Code_sp>(codebase)) {
    llvmo::Code_sp code = gc::As_unsafe<llvmo::Code_sp>(codebase);
    if (!decodeEntryPointForCompiledCode(fixup,ptrptr,code)) {
      // The entry point wasnt into the compiled code
      //   so it must be to one of the libraries - apply that fixup.
      decodeEntryPointInLibrary( fixup, ptrptr );
    }
  } else if (gc::IsA<llvmo::Library_sp>(codebase)) {
    llvmo::Library_sp library = gc::As_unsafe<llvmo::Library_sp>(codebase);
    decodeEntryPointInLibrary(fixup,ptrptr);
  } else {
    SIMPLE_ERROR(("The codebase must be a Code_sp or a Library_sp it is %s") , _rep_(codebase) );
  }
}


};

namespace snapshotSaveLoad {

struct MemoryRange {
  gctools::clasp_ptr_t _Start;
  gctools::clasp_ptr_t _End;
  
  MemoryRange(gctools::clasp_ptr_t start=NULL, gctools::clasp_ptr_t end=NULL) : _Start(start), _End(end) {};
  void set(gctools::clasp_ptr_t start, gctools::clasp_ptr_t end) {
    this->_Start = start;
    this->_End = end;
  }
  bool contains(gctools::clasp_ptr_t ptr) {
    return (this->_Start<=ptr && ptr<= this->_End);
  }
};

//
// This stores if the client object is in snapshot save/load memory (true) or
// from garbage collected memory (false)
//
MemoryRange globalISLBufferRange;


gctools::Header_s* generalPointerToHeaderPointer(gctools::clasp_ptr_t general)
{
#if 0
  // we used to only save the StampWtagMtag - but now save the whole header for debugging.
  if (globalISLBufferRange.contains(general)) {
    return (gctools::Header_s*)(general - sizeof(gctools::Header_s::StampWtagMtag));
  }
#endif
  return (gctools::Header_s*)gctools::GeneralPtrToHeaderPtr(general);
}

gctools::clasp_ptr_t headerPointerToGeneralPointer(gctools::Header_s* header)
{
#if 0
  // we used to only save the StampWtagMtag - but now save the whole header for debugging.
  if (globalISLBufferRange.contains((gctools::clasp_ptr_t)header)) {
    return (gctools::clasp_ptr_t)((const char*)header + sizeof(gctools::Header_s::StampWtagMtag));
  }
#endif
  return (gctools::clasp_ptr_t)gctools::HeaderPtrToGeneralPtr<core::General_O>((void*)header);
}

#define GENERAL_PTR_TO_HEADER_PTR(_general_) generalPointerToHeaderPointer((gctools::clasp_ptr_t)_general_)
#define HEADER_PTR_TO_GENERAL_PTR(_header_) headerPointerToGeneralPointer((gctools::Header_s*)_header_)
#define WEAK_PTR_TO_HEADER_PTR(_general_) generalPointerToHeaderPointer((gctools::clasp_ptr_t)_general_)
#define HEADER_PTR_TO_WEAK_PTR(_header_) headerPointerToGeneralPointer((gctools::Header_s*)_header_)

typedef gctools::clasp_ptr_t (*PointerFix)(gctools::clasp_ptr_t* clientAddress, gctools::clasp_ptr_t client, uintptr_t tag, void* user_data);
PointerFix globalPointerFix;
const char* globalPointerFixStage;


struct ISLInfo {
  FixupOperation_ _operation;
  uintptr_t _islStart;
  uintptr_t _islEnd;
  std::map<gctools::Header_s*,core::T_O*> _forwarding;
  ISLInfo( FixupOperation_ op, uintptr_t s=0, uintptr_t e=0) :
    _operation(op),
    _islStart(s),
    _islEnd(e) {};
};  

struct MemoryTest_t {
  ISLInfo islInfo;
  FILE*   fout;
  MemoryTest_t(FixupOperation_ fop, FILE* f) : islInfo(fop), fout(f) {};
};

//
// Control how pointer forwarding works.
//
// The global_forwardingKind controls how pointer forwarding works.
//   noStomp - forwarding uses a separate map of pointer -> forwarded-pointer.
//                This is slow and is meant for snapshot_save and to facilitate debugging.
//   stomp   - The forwarding pointer is written into the pointer address.
//                This is fast and is meant for snapshot_load
//   testStomp - maintain a map of pointer -> forwarded-pointer AND write the forwarding
//               pointer into the pointer address and compare the two on every operation.
//               This is to test stomp.
//   undef - this means global_forwardingKind wasn't set yet.
//
typedef enum { undef, stomp, noStomp, testStomp } ForwardingEnum;
ForwardingEnum global_forwardingKind = undef;

[[noreturn]] void errorBadForwardingKind() {
  printf("%s:%d:%s The global_forwardingKind wasn't set\n", __FILE__, __LINE__, __FUNCTION__ );
  abort();
}


void set_forwarding_pointer(gctools::Header_s* header, char* new_client, ISLInfo* info ) {
  if (global_forwardingKind == testStomp) {
    info->_forwarding[header] = (core::T_O*)new_client;
    if ((intptr_t)new_client < 0) {
      printf("%s:%d:%s Writing a bad forwarding pointer %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)new_client);
    }
    header->_stamp_wtag_mtag.setFwdPointer(new_client);
    if ((uintptr_t)header->_stamp_wtag_mtag.fwdPointer() != (uintptr_t)new_client) {
      printf("%s:%d:%s Forwarding pointer written and read don't match\n", __FILE__, __LINE__, __FUNCTION__);
      abort();
    }
  } else if (global_forwardingKind == noStomp) {
    info->_forwarding[header] = (core::T_O*)new_client;
  } else if (global_forwardingKind == stomp) {
    header->_stamp_wtag_mtag.setFwdPointer(new_client);
  } else {
    errorBadForwardingKind();
  }
}

bool is_forwarding_pointer(gctools::Header_s* header, ISLInfo* info) {
  if (global_forwardingKind == testStomp) {
    auto result = info->_forwarding.find(header);
    bool noStompResult = (result != info->_forwarding.end());
    bool stompResult = header->_stamp_wtag_mtag.fwdP();
    if (noStompResult!=stompResult) {
      printf("%s:%d:%s results don't match\n", __FILE__, __LINE__, __FUNCTION__);
      abort();
    }
    return stompResult;
  } else if (global_forwardingKind == noStomp) {
    auto result = info->_forwarding.find(header);
    bool noStompResult = (result != info->_forwarding.end());
    return noStompResult;
  } else if (global_forwardingKind == stomp) {
    bool stompResult = header->_stamp_wtag_mtag.fwdP();
    return stompResult;
  } else {
    errorBadForwardingKind();
  }
}

uintptr_t forwarding_pointer(gctools::Header_s* header, ISLInfo* info) {
  if (global_forwardingKind == testStomp) {
    uintptr_t noStompResult =  (uintptr_t)info->_forwarding[header];
    uintptr_t stompResult = (uintptr_t)header->_stamp_wtag_mtag.fwdPointer();
    if (noStompResult!=stompResult) {
      printf("%s:%d:%s results don't match\n", __FILE__, __LINE__, __FUNCTION__);
      abort();
    }
    return stompResult;
  } else if (global_forwardingKind == noStomp) {
    return (uintptr_t)info->_forwarding[header];
  } else if (global_forwardingKind == stomp) {
    return (uintptr_t)header->_stamp_wtag_mtag.fwdPointer();
  } else {
    errorBadForwardingKind();
  }
}


struct walker_callback_t {
  bool     _debug;
  ISLInfo* _info;
  virtual void callback(gctools::Header_s* header) = 0;
  walker_callback_t(ISLInfo* info) : _debug(false), _info(info) {};
};


struct test_objects_t : public walker_callback_t {
  std::map<gctools::Header_s*,std::vector<uintptr_t>> _corruptObjects;
public:  
  test_objects_t( ISLInfo* info ) : walker_callback_t(info) {};
  void callback(gctools::Header_s* header);
};

DONT_OPTIMIZE_WHEN_DEBUG_RELEASE gctools::clasp_ptr_t test_pointer(gctools::clasp_ptr_t* clientAddress, gctools::clasp_ptr_t client, uintptr_t tag, void* user_data) {
  DBG_SL_FFWD(BF("test_pointer clientAddress: %p client: %p\n") % (void*)clientAddress % (void*)client );
  test_objects_t* test_objects = (test_objects_t*)user_data;
  ISLInfo* islInfo = test_objects->_info;
  gctools::Header_s* header;
  if (tag==gctools::general_tag) {
    header = GENERAL_PTR_TO_HEADER_PTR(client);
  } else if (tag==gctools::cons_tag) {
    header = (gctools::Header_s*)gctools::ConsPtrToHeaderPtr(client);
  } else {
    header = WEAK_PTR_TO_HEADER_PTR(client);
  }
  auto found = islInfo->_forwarding.find(header);
  if (found == islInfo->_forwarding.end()) {
    gctools::Header_s* base = (gctools::Header_s*)GC_base(clientAddress);
    printf("%s:%d:%s In Object base: %p at %p is a client %p that was not reached when we walked all of memory\n",
           __FILE__, __LINE__, __FUNCTION__, (void*)base, (void*)clientAddress, (void*)client );
    auto ii = test_objects->_corruptObjects.find(base);
    if ( ii == test_objects->_corruptObjects.end() ) {
      std::vector<uintptr_t> badPointers;
      badPointers.push_back((uintptr_t)clientAddress);
      test_objects->_corruptObjects[base] = badPointers;
    } else {
      std::vector<uintptr_t>& badPointers = ii->second;
      badPointers.push_back((uintptr_t)clientAddress);
    }
  }
  return gctools::clasp_ptr_t((uintptr_t)client|tag);
}

gctools::clasp_ptr_t maybe_follow_forwarding_pointer(gctools::clasp_ptr_t* clientAddress, gctools::clasp_ptr_t client, uintptr_t tag, void* user_data) {
  DBG_SL_FFWD(BF("maybe_follow_forwarding_pointer clientAddress: %p client: %p\n") % (void*)clientAddress % (void*)client );
  ISLInfo* islInfo = (ISLInfo*)user_data;
  uintptr_t fwd_client;
  gctools::Header_s* header;
  if (tag==gctools::general_tag) {
    header = GENERAL_PTR_TO_HEADER_PTR(client);
  } else if (tag==gctools::cons_tag) {
    header = (gctools::Header_s*)gctools::ConsPtrToHeaderPtr(client);
  } else {
    header = WEAK_PTR_TO_HEADER_PTR(client);
  }
  DBG_SL_FFWD(BF("    maybe_follow_forwarding_pointer client %p header: %p\n") % (void*)client % (void*)header );
  if (islInfo->_operation== SaveOp && ! is_forwarding_pointer(header,islInfo)) {
    printf("%s:%d:%s general header %p MUST BE A FORWARDING POINTER - got %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, *(void**)header);
    printf("%s:%d:%s stage: %s connect a debugger to pid: %d\n", __FILE__, __LINE__, __FUNCTION__, globalPointerFixStage, getpid() );
    printf("%s:%d:%s Running memory test\n", __FILE__, __LINE__, __FUNCTION__ );
    memory_test( true, NULL, "Memory test after bad forwarding pointer was discovered" );
    printf("%s:%d:%s stage: %s Sleeping to connect a debugger to pid: %d\n", __FILE__, __LINE__, __FUNCTION__, globalPointerFixStage, getpid() );
    sleep(1000000);
  }
  if (is_forwarding_pointer(header,islInfo)) {
    fwd_client = (uintptr_t)forwarding_pointer(header,islInfo);
    DBG_SL_FFWD(BF("fwdPointer from %p  to header@%p  %p  GC_base %p\n")
                % (void*)((uintptr_t)client | tag)
                % (void*)header
                % ((void*)((uintptr_t)fwd_client | tag))
                % GC_base((void*)fwd_client) );
    if (islInfo->_operation == SaveOp) {
      if (!(islInfo->_islStart<=fwd_client)&&(fwd_client<islInfo->_islEnd)) {
        printf("%s:%d:%s Forwarded pointer does NOT point into the islbuffer\n", __FILE__, __LINE__, __FUNCTION__);
        abort();
      }
    } else if (islInfo->_operation == LoadOp) {
      if (global_debugSnapshot) {
        void* maybe_base = GC_base((void*)fwd_client);
        if (!maybe_base) {
          printf("%s:%d:%s We have a pointer %p that MUST be in GC memory - but it isn't\n", __FILE__, __LINE__, __FUNCTION__, (void*)fwd_client );
          abort();
        }
      }
    }
    return (gctools::clasp_ptr_t)(fwd_client | tag);
  }
  // When loading we have a few objects that are NOT forwarded - so we return them here
  return (gctools::clasp_ptr_t)((uintptr_t)client | tag);
}

#define POINTER_FIX(_ptr_) {\
    gctools::clasp_ptr_t *taggedP = reinterpret_cast<gctools::clasp_ptr_t *>(_ptr_);\
    if (gctools::tagged_objectp(*taggedP)) {\
      gctools::clasp_ptr_t tagged_obj = *taggedP;\
      if (gctools::tagged_objectp(*taggedP)) { \
        gctools::clasp_ptr_t obj = gctools::untag_object<gctools::clasp_ptr_t>(tagged_obj);\
        uintptr_t tag = (uintptr_t)gctools::ptag<gctools::clasp_ptr_t>(tagged_obj);\
        /*printf("%s:%d fixing taggedP@%p obj-> %p tag-> 0x%lx\n", __FILE__, __LINE__, (void*)taggedP, (void*)obj, (uintptr_t)tag);*/ \
        obj = (globalPointerFix)(taggedP,obj,tag,user_data); \
        /*printf("%s:%d     forwarded  obj = %p \n", __FILE__, __LINE__, (void*)obj );*/ \
        *taggedP = obj;\
      };\
    };\
  }



#define SCAN_STRUCT_T int
#define ADDR_T gctools::clasp_ptr_t
#define SCAN_BEGIN(ss)
#define SCAN_END(ss)
#define RESULT_TYPE    int
#define RESULT_OK 1
#define EXTRA_ARGUMENTS , void* user_data

#undef DEBUG_MPS_SIZE
#define OBJECT_SKIP_IN_OBJECT_SCAN blah_blah_blah_error
#define OBJECT_SCAN isl_obj_scan
#include "obj_scan.cc"
#undef OBJECT_SCAN

#define OBJECT_SKIP isl_obj_skip
#include "obj_scan.cc"
#undef OBJECT_SKIP

#define OBJECT_SKIP_IN_OBJECT_FWD isl_obj_skip
#define OBJECT_FWD isl_obj_fwd
#include "obj_scan.cc"
#undef OBJECT_FWD


#define CONS_SCAN isl_cons_scan
#define CONS_SKIP isl_cons_skip
#define CONS_FWD isl_cons_fwd
#define CONS_SKIP_IN_CONS_FWD isl_cons_skip
#include "cons_scan.cc"
#undef CONS_FWD
#undef CONS_SKIP
#undef CONS_SCAN


#define WEAK_SCAN isl_weak_scan
#define WEAK_SKIP isl_weak_skip
#define WEAK_FWD isl_weak_fwd
#define WEAK_SKIP_IN_WEAK_FWD isl_weak_skip
#include "weak_scan.cc"
#undef WEAK_FWD
#undef WEAK_SKIP
#undef WEAK_SCAN

#undef SCAN_STRUCT_T
#undef ADDR_T
#undef SCAN_BEGIN
#undef SCAN_END
#undef RESULT_TYPE
#undef RESULT_OK
#undef EXTRA_ARGUMENTS



//
// Fix root pointers by following the forwarding pointer
//
void followForwardingPointersForRoots(gctools::clasp_ptr_t* start, size_t number, void* user_data) {
  for ( size_t idx = 0; idx<number; idx++ ) {
    gctools::clasp_ptr_t before = *start;
    POINTER_FIX(start);
    gctools::clasp_ptr_t after = *start;
    DBG_SL_ROOT(BF("Fixed root pointer %d from %p to %p GC_base %p\n") % idx % (void*)before % (void*)after % GC_base((void*)after) );
    start++;
  }
}


void copyRoots( uintptr_t* destination, uintptr_t* source, size_t numberOfRoots ) {
  DBG_SL_ROOT(BF("Moving roots from %p to %p num: %lu\n") % (void*)source % (void*)destination % numberOfRoots );
  memcpy((char*)destination, (char*)source, numberOfRoots*sizeof(void*));
}


/* Immediately after load the root pointers will point to the location of objects
 * in the islbuffer at save time.  We need to relocate them to the load location
 * of the islbuffer
 */
void relocateLoadedRootPointers(gctools::clasp_ptr_t* start, size_t number, void* user_data ) {
  for ( size_t idx = 0; idx<number; idx++ ) {
    gctools::clasp_ptr_t before = *start;
    POINTER_FIX(start);
    gctools::clasp_ptr_t after = *start;
    DBG_SL_ROOT(BF("Fixed root pointer %d from %p to %p\n") % idx % (void*)before % (void*)after );
    start++;
  }
}


#undef POINTER_FIX

};






namespace snapshotSaveLoad {

/* 
 * These are strings that are visible in the snapshot save file
 */
typedef enum {
    General  = 0xbedabb1e01010101, // !OBJECT!
    Cons    = 0xbedabb1e02020202,
    Weak    = 0xbedabb1e03030303,
    Library = 0xbedabb1e04040404,
    Roots   = 0xbedabb1e05050505, // ROOTS
    End     = 0xbedabb1e06060606 } ISLKind; // END


#define MAGIC_NUMBER 348235823
struct ISLFileHeader {
  size_t _Magic;
  uintptr_t _LibrariesOffset;
  uintptr_t _NumberOfLibraries;

  uintptr_t _MemoryStart;
  uintptr_t _NumberOfObjects;
  uintptr_t _MemorySize;
  uintptr_t _SaveTimeMemoryAddress;
  size_t _LispRootOffset;
  size_t _LispRootCount;
  size_t _SymbolRootsOffset;
  size_t _SymbolRootsCount;

  uintptr_t _ObjectFileStart;
  uintptr_t _ObjectFileSize;
  uintptr_t _ObjectFileCount;

  uintptr_t _NextUnshiftedClbindStamp;
  uintptr_t _NextUnshiftedStamp;
  
  size_t _global_JITDylibCounter;
  size_t _global_JITCompileCounter;

  ISLFileHeader(size_t sz,size_t num, uintptr_t sbs) : _Magic(MAGIC_NUMBER), _MemorySize(sz), _NumberOfObjects(num), _MemoryStart(sbs) {
    this->_global_JITDylibCounter = llvmo::global_JITDylibCounter.load();
    this->_global_JITCompileCounter = core::core__get_jit_compile_counter();
  };
  bool good_magic() const {
    return (this->_Magic == MAGIC_NUMBER);
  }

  void describe(const std::string& mesg) {
    printf("%s\n", mesg.c_str() );
    printf(" %30s -> %lu\n", "size_t _Magic", _Magic );
    printf(" %30s -> %lu(0x%lx)\n", "uintptr_t _LibrariesOffset", _LibrariesOffset, _LibrariesOffset  );
    printf(" %30s -> %lu\n", "uintptr_t _NumberOfLibraries", _NumberOfLibraries );
    printf(" %30s -> 0x%lx\n", "uintptr_t _SaveTimeMemoryAddress", _SaveTimeMemoryAddress  );
    printf(" %30s -> %lu(0x%lx)\n", "uintptr_t _MemoryStart", _MemoryStart, _MemoryStart  );
    printf(" %30s -> %lu\n", "uintptr_t _NumberOfObjects", _NumberOfObjects );
    printf(" %30s -> %lu(0x%lx)\n", "uintptr_t _MemorySize", _MemorySize, _MemorySize  );
    printf(" %30s -> %lu\n", "size_t _LispRootOffset", _LispRootOffset );
    printf(" %30s -> %lu\n", "size_t _LispRootCount", _LispRootCount );
    printf(" %30s -> %lu\n", "size_t _SymbolRootsOffset", _SymbolRootsOffset );
    printf(" %30s -> %lu\n", "size_t _SymbolRootsCount", _SymbolRootsCount );
    printf(" %30s -> %lu(0x%lx)\n", "uintptr_t _ObjectFileStart", _ObjectFileStart, _ObjectFileStart  );
    printf(" %30s -> %lu(0x%lx)\n", "uintptr_t _ObjectFileSize", _ObjectFileSize, _ObjectFileSize  );
    printf(" %30s -> %lu(0x%lx)\n", "NextUnshiftedClbindStamp", _NextUnshiftedClbindStamp, _NextUnshiftedClbindStamp );
    printf(" %30s -> %lu(0x%lx)\n", "NextUnshiftedStamp", _NextUnshiftedStamp, _NextUnshiftedStamp );
  }
};

struct copy_buffer_t {
  char* _BufferStart;
  char* _buffer;
  size_t _Size;
  size_t _WriteCount;
  copy_buffer_t(size_t size)
    : _Size(size),
      _WriteCount(0) {
    this->_BufferStart = (char*)malloc(size);
    this->_buffer = this->_BufferStart;
    memset(this->_buffer,'\0',size);
  }
  ~copy_buffer_t() {
    delete this->_BufferStart;
  }
  uintptr_t buffer_offset() {
    return this->_buffer-this->_BufferStart;
  }
  
  char* write_buffer(char* source, size_t bytes ) {
    this->_WriteCount++;
    char* addr = this->_buffer;
    this->_buffer += bytes;
    if ((this->_BufferStart<=addr) && (addr <= (this->_BufferStart+this->_Size))
        && (this->_BufferStart <= (addr+bytes) && ((addr+bytes)<=(this->_BufferStart+this->_Size)))) {
#if 0
      for ( size_t idx = 0; idx<bytes; idx++ ) {
        if (addr[idx] != '\0') {
          printf("%s:%d:%s About to overwrite non-zero memory at %p\n", __FILE__, __LINE__, __FUNCTION__, &addr[idx]);
          abort();
        }
      }
#endif
      memcpy((void*)addr, (const void*)source, bytes );
    } else {
      printf("%s:%d:%s The memcpy of range %p - %p will fall out of the allowed destination range %p - %p\n",
             __FILE__, __LINE__, __FUNCTION__,
             (void*)addr, (void*)(addr + bytes),
             (void*)this->_BufferStart, (void*)(this->_BufferStart+this->_Size));
      abort();
    }
    if (((uintptr_t)this->_buffer&7) !=0 ) {
      printf("%s:%d:%s The write_buffer command must end on word aligned address - it ends at %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)this->_buffer);
      abort();
    }
    return addr;
  }

  void write_to_stream(std::ofstream& stream) {
    stream.write( this->_BufferStart, this->_Size );
  }

  void write_to_filedes(int filedes) {
    write( filedes, this->_BufferStart, this->_Size );
  }
};


struct Snapshot {
  ISLFileHeader*    _FileHeader;
  copy_buffer_t*    _HeaderBuffer;
  copy_buffer_t*    _Memory;
  copy_buffer_t*    _Libraries;
  copy_buffer_t*    _ObjectFiles;

  ~Snapshot() {
    delete this->_HeaderBuffer;
    delete this->_Memory;
    delete this->_Libraries;
    delete this->_ObjectFiles;
  }
};



};

extern "C" {
#if defined(USE_BOEHM)
void boehm_walker_callback(void* ptr, size_t sz, void* client_data) {
  snapshotSaveLoad::walker_callback_t* walker = (snapshotSaveLoad::walker_callback_t*)client_data;
  int kind;
  size_t psize;
  kind = GC_get_kind_and_size((void*)ptr, &psize );
    // On boehm sometimes I get unknown objects that I'm trying to avoid with the next test.
  if ( kind == gctools::global_lisp_kind ||
       kind == gctools::global_cons_kind ||
       kind == gctools::global_class_kind ||
       kind == gctools::global_container_kind ||
       kind == gctools::global_code_kind ||
       kind == gctools::global_atomic_kind ||
       kind == gctools::global_strong_weak_kind ) {
    // Either there is a tag in the low 3 bits of the ptr or the second word is not zero
    // Blocks can be passed to us that have a boehm free-list pointer in the first word and zero everywhere else
    //   a free-list pointer will have zero in the low 3 bits
    bool notFreeListPtr = ((*(uintptr_t*)ptr)&0x7);
    bool secondWordNotZero = *(((uintptr_t*)ptr)+1);
    if ( notFreeListPtr || secondWordNotZero ) {
      // If there is a non-zero header then walk it
      gctools::Header_s* header = (gctools::Header_s*)ptr;
      DBG_SL_WALK_GC(BF("Walking to GC managed header %p %s\n") % (void*)header % header->description() );
      walker->callback((gctools::Header_s*)ptr);
    } else {
      DBG_SL_DONTWALK(BF("NOT walking to GC managed header %p kind: %d value -> %p  notFreeListPtr %d  secondWordNotZero %d\n") % (void*)ptr % kind % *(void**)ptr % notFreeListPtr % secondWordNotZero );
    }
  } else {
    DBG_SL_DONTWALK(BF("NOT walking to GC managed header %p kind: %d value -> %p\n") % (void*)ptr % kind % *(void**)ptr );
  }
}
#endif
};


namespace snapshotSaveLoad {



struct ISLHeader_s {
  ISLKind     _Kind;
  size_t      _Size;
  ISLHeader_s(ISLKind k, size_t s) : _Kind(k), _Size(s) {};
  ISLHeader_s* next(ISLKind k) const;
};

struct ISLEndHeader_s : public ISLHeader_s {
  ISLEndHeader_s(ISLKind k) : ISLHeader_s(k,0) {};
  gctools::Header_s* header() const {printf("%s:%d:%s subclass must implement\n", __FILE__, __LINE__, __FUNCTION__ ); abort(); };
};


struct ISLRootHeader_s : public ISLHeader_s {
  ISLRootHeader_s(ISLKind k, size_t s) : ISLHeader_s(k,s) {};
};
  
struct ISLConsHeader_s : public ISLHeader_s {
  gctools::Header_s::StampWtagMtag _stamp_wtag_mtag;
  ISLConsHeader_s(ISLKind k, size_t s, gctools::Header_s::StampWtagMtag swm) : ISLHeader_s(k,s), _stamp_wtag_mtag(swm) {};
  gctools::Header_s* header() const { return (gctools::Header_s*)((char*)this + offsetof(ISLConsHeader_s,_stamp_wtag_mtag)); }
};

struct ISLWeakHeader_s : public ISLHeader_s {
  gctools::Header_s _Header;
  ISLWeakHeader_s(ISLKind k, uintptr_t sz, gctools::Header_s* head) : ISLHeader_s(k,sz), _Header(head) {};
  gctools::Header_s* header() const { return (gctools::Header_s*)((char*)this + offsetof(ISLWeakHeader_s,_Header)); }
};

struct ISLGeneralHeader_s : public ISLHeader_s {
  gctools::Header_s _Header;
  ISLGeneralHeader_s(ISLKind k, uintptr_t sz, gctools::Header_s* head) : ISLHeader_s(k,sz), _Header(head) {};
  gctools::Header_s* header() const { return (gctools::Header_s*)((char*)this + offsetof(ISLGeneralHeader_s,_Header));}
};

struct ISLLibraryHeader_s : public ISLHeader_s {
  bool       _Executable;
  size_t     _SymbolBufferOffset;
  size_t     _SymbolInfoOffset;
  size_t     _SymbolInfoCount;
  ISLLibraryHeader_s(ISLKind k, bool isExecutable, size_t s, size_t symbolBufferOffset, size_t symbolInfoOffset, size_t symbolInfoCount ) :
    ISLHeader_s(k,s),
    _Executable(isExecutable),
    _SymbolBufferOffset(symbolBufferOffset),
    _SymbolInfoOffset(symbolInfoOffset),
    _SymbolInfoCount(symbolInfoCount) {};
};


size_t ISLLibrary::writeSize() {
  return sizeof(ISLLibraryHeader_s) + this->nameSize() + this->symbolBufferSize() + this->symbolInfoSize();
};
  

ISLHeader_s* ISLHeader_s::next(ISLKind k) const {
  size_t headerSize = 0;
  if (k==General) {
    headerSize = sizeof(ISLGeneralHeader_s);
  } else if (k==Cons) {
    headerSize = sizeof(ISLConsHeader_s);
  } else if (k==Weak) {
    headerSize = sizeof(ISLWeakHeader_s);
  } else {
    SIMPLE_ERROR(("Add support to calculate size of ISLKind %d") , k );
  }
  return (ISLHeader_s*)((char*)this + headerSize + this->_Size);
}


#define ISL_ERROR(_fmt_) { \
    printf("%s:%d:%s  %s\n", __FILE__, __LINE__, __FUNCTION__, (_fmt_).c_str()); \
    abort(); \
  }


struct ensure_forward_t : public walker_callback_t {
  void callback(gctools::Header_s* header) {
    // On boehm sometimes I get unknown objects that I'm trying to avoid with the next test.
    if (!is_forwarding_pointer(header,this->_info)) {
      if (header->_stamp_wtag_mtag.stampP()) {
        printf("%s:%d:%s The ISL general %p %s is not a forwarding pointer\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, header->description().c_str());
      } else if (header->_stamp_wtag_mtag.consObjectP()) {
        printf("%s:%d:%s The ISL cons %p %s is not a forwarding pointer\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, header->description().c_str());
      } else if (header->_stamp_wtag_mtag.weakObjectP()) {
        printf("%s:%d:%s The ISL weak %p %s is not a forwarding pointer\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, header->description().c_str());
      }
    }
  }
  ensure_forward_t(ISLInfo* info) : walker_callback_t(info) {};
};



struct gather_info_for_snapshot_save_t : public walker_callback_t {
  // Run fixupInternalsForSnapshotSaveLoad with fixup._operation = InfoOp;
  Fixup* _fixup;
  void callback(gctools::Header_s* header) {
    // On boehm sometimes I get unknown objects that I'm trying to avoid with the next test.
    if (header->_stamp_wtag_mtag.stampP()) {
      if (header->preciseIsPolymorphic()) {
        core::T_O* client = (core::T_O*)HEADER_PTR_TO_GENERAL_PTR(header);
        if (cast::Cast<core::General_O*,core::T_O*>::isA(client)) {
          core::General_O* generalObject = (core::General_O*)client;
          generalObject->fixupInternalsForSnapshotSaveLoad(this->_fixup);
        }
      }
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      // Nothing
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      // Nothing
    }
  }
  gather_info_for_snapshot_save_t(Fixup* fixup, ISLInfo* info) : walker_callback_t(info), _fixup(fixup) {};
};


struct prepare_for_snapshot_save_t : public walker_callback_t {
  Fixup* _fixup;
  void callback(gctools::Header_s* header) {
    // On boehm sometimes I get unknown objects that I'm trying to avoid with the next test.
    if (header->_stamp_wtag_mtag.stampP()) {
      //
      // Fixup general objects that need it
      //
      if (header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_gctools__GCVector_moveable_clbind__detail__edge_)) {
//        printf("%s:%d:%s save_snapshot saw STAMPWTAG_gctools__GCVector_moveable_clbind__detail__edge_\n", __FILE__, __LINE__, __FUNCTION__ );
        gctools::GCVector_moveable<clbind::detail::edge>* edges = (gctools::GCVector_moveable<clbind::detail::edge>*)HEADER_PTR_TO_GENERAL_PTR(header);
//        printf("%s:%d:%s save_snapshot          edges->size() = %lu\n", __FILE__, __LINE__, __FUNCTION__, edges->size() );
        for ( size_t ii = 0; ii< edges->size(); ii++ ) {
//          printf("%s:%d:%s  [%lu] before   target: %lu   cast_function@%p: %p\n", __FILE__, __LINE__, __FUNCTION__, ii, (*edges)[ii].target, &(*edges)[ii].cast, (*edges)[ii].cast);
          void** ptrptr = (void**)&(*edges)[ii].cast;
          encodeEntryPointInLibrary(this->_fixup,(uintptr_t*)ptrptr );
        }
      }
      // Handle them on a case by case basis
      if (header->preciseIsPolymorphic()) {
        core::T_O* client = (core::T_O*)HEADER_PTR_TO_GENERAL_PTR(header);
        if (cast::Cast<core::General_O*,core::T_O*>::isA(client)) {
          core::General_O* generalObject = (core::General_O*)client;
          generalObject->fixupInternalsForSnapshotSaveLoad(this->_fixup);
        }
      }
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      // Nothing
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      // Nothing
    }
  }
  prepare_for_snapshot_save_t(Fixup* fixup, ISLInfo* info) : walker_callback_t(info), _fixup(fixup) {};
};


struct prepare_test_t : public walker_callback_t {
  std::set<gctools::Header_s*> _seen;
  void callback(gctools::Header_s* header) {
    this->_seen.insert(header);
  }

  prepare_test_t(ISLInfo* info) : walker_callback_t(info) {};
};



struct calculate_size_t : public walker_callback_t {

  size_t _TotalSize;
  size_t _ObjectFileTotalSize;
  size_t _general_count;
  size_t _cons_count;
  size_t _weak_count;
  size_t _ObjectFileCount;
  size_t _CodeCount;
  void callback(gctools::Header_s* header) {
    std::string str;
    // On boehm sometimes I get unknown objects that I'm trying to avoid with the next test.
    if (header->_stamp_wtag_mtag.stampP()) {
      this->_general_count++;
      gctools::clasp_ptr_t client = HEADER_PTR_TO_GENERAL_PTR(header);
      size_t objectSize;
      if (header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_llvmo__Code_O)) {
        this->_CodeCount++;
        //
        // Calculate the size of a Code_O object keeping only the literals vector
        //
        llvmo::Code_O* code = (llvmo::Code_O*)client;
        size_t saveCodeSize = llvmo::Code_O::sizeofInState(code,llvmo::SaveState);
        {
          size_t runCodeSize;
          isl_obj_skip( (gctools::clasp_ptr_t)code, false, runCodeSize );
#if 0
          printf("%s:%d:%s Calculated size of Code_O in RunState: %lu  and in SaveState: %lu\n",
                 __FILE__, __LINE__, __FUNCTION__, runCodeSize, saveCodeSize );
#endif
        }
        this->_TotalSize += sizeof(ISLGeneralHeader_s) + saveCodeSize;
      } else if (header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_llvmo__ObjectFile_O)) {
        this->_ObjectFileCount++;
        llvmo::ObjectFile_O* objectFile = (llvmo::ObjectFile_O*)client;
        size_t objectFileSize = objectFile->objectFileSizeAlignedUp();
        this->_ObjectFileTotalSize += objectFileSize;
        this->_TotalSize += sizeof(ISLGeneralHeader_s) + sizeof(llvmo::ObjectFile_O);
      } else {
        size_t delta = isl_obj_skip(client,false,objectSize)-client;
        DBG_SL1(BF("   general header@%p value: 0x%x badge: 0x%x  sz = %lu  obj_skip = %lu\n")
                % header
                % header->_stamp_wtag_mtag._value
                % header->_stamp_wtag_mtag._header_badge
                % objectSize % delta );
        this->_TotalSize += sizeof(ISLGeneralHeader_s) + objectSize;
      }
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)gctools::HeaderPtrToConsPtr(header);
      DBG_SL1(BF("   cons header@%p -> %p\n") % header % *(void**)header );
      this->_cons_count++;
      size_t consSize;
      isl_cons_skip(client,consSize);
      this->_TotalSize += sizeof(ISLConsHeader_s) + consSize;
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)HEADER_PTR_TO_WEAK_PTR(header);
      this->_weak_count++;
      DBG_SL1(BF("weak object header %p   client: %p\n") % (void*)header % (void*)client );
      size_t objectSize;
      gctools::clasp_ptr_t nextClient = isl_weak_skip(client,false,objectSize);
      this->_TotalSize += sizeof(ISLWeakHeader_s) + objectSize;
    }
  }

  calculate_size_t(ISLInfo* info) :
    walker_callback_t(info),
    _TotalSize(0),
    _ObjectFileTotalSize(0),
    _general_count(0),
    _cons_count(0),
    _weak_count(0),
    _ObjectFileCount(0),
    _CodeCount(0)
  {};
};


template <typename Walker>
void walk_gathered_objects(Walker& walker, const gctools::GatherObjects& objects ) {
  for ( auto obj : objects._Marked ) {
    walker.callback(obj);
  }
}

size_t PageAlignUp(size_t size) {
  size_t pagesize = getpagesize();
  size_t aligned = ((size+pagesize)/pagesize)*pagesize;
  return aligned;
}



struct copy_objects_t : public walker_callback_t {
  copy_buffer_t* _objects;
  size_t _NumberOfObjects;
  copy_buffer_t* _objectFiles;

  copy_objects_t( copy_buffer_t* objects, copy_buffer_t* objectFiles, ISLInfo* info)
    : _objects(objects),
      _objectFiles(objectFiles),
      _NumberOfObjects(0),
      walker_callback_t(info) {};
  
  void callback(gctools::Header_s* header) {
    std::string str;
    // On boehm sometimes I get unknown objects that I'm trying to avoid with the next test.
    if (header->_stamp_wtag_mtag.stampP()) {
      if (header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_core__Lisp)) {
//        printf("%s:%d:%s About to save Lisp object\n", __FILE__, __LINE__, __FUNCTION__ );
      }
      gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)HEADER_PTR_TO_GENERAL_PTR(header);
      if (header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_llvmo__Code_O)) {
//      printf("%s:%d:%s !!!!!!!!! REALLY DANGEROUS CODE - I AM NOT CALCULATING THE SIZE OF Code_O objects as I SHOULD BE\n", __FILE__, __LINE__, __FUNCTION__ );
        //
        // Calculate the size of a Code_O object keeping only the literals vector
        //
        llvmo::Code_O* code = (llvmo::Code_O*)clientStart;
        gctools::clasp_ptr_t clientEndFront = clientStart + code->frontSize();
        gctools::clasp_ptr_t clientEndAndLiterals = clientStart + llvmo::Code_O::sizeofInState(code,llvmo::SaveState);
        ISLGeneralHeader_s islheader( General, clientEndAndLiterals-clientStart, header );
        char* islh = this->_objects->write_buffer( (char*)&islheader, sizeof(ISLGeneralHeader_s)); 
        char* new_addr = this->_objects->write_buffer((char*)clientStart, clientEndFront-clientStart);
        llvmo::Code_O* new_code = (llvmo::Code_O*)new_addr;
        // Set the state and capacity of the new Code_O in the snapshot save load memory
        new_code->_State = llvmo::SaveState;
        new_code->_DataCode._MaybeSignedLength = clientEndAndLiterals - clientEndFront;
        // Write the bytes for the literals
        char* literals_addr = this->_objects->write_buffer((char*)code->literalsStart(), clientEndAndLiterals-clientEndFront );
        // Update the new_code object to reflect that contains literals and no code and it's in the "SaveState"
        if (code->literalsSize() !=(clientEndAndLiterals-clientEndFront)) {
          printf("%s:%d:%s There is a mismatch between the code literalsSize() %lu and the clientEndAndLiterals-clientEndFront %lu\n",
                 __FILE__, __LINE__, __FUNCTION__,
                 code->literalsSize(),
                 (clientEndAndLiterals-clientEndFront) );
          abort();
        }
        new_code->_LiteralVectorStart = literals_addr - new_addr;
        new_code->_LiteralVectorSizeBytes = clientEndAndLiterals-clientEndFront;
        DBG_SAVECOPY(BF("   copied general header Code_O object %p to %p - %p\n")
                     % header % (void*)islh % (void*)this->_buffer );
        set_forwarding_pointer(header, new_addr, this->_info ); // This is a client pointer
        DBG_SL_FWD(BF("setFwdPointer general header %p new_addr -> %p  reread fwdPointer -> %p\n")
                   % (void*)header % (void*)new_addr % (void*)forwarding_pointer(header,this->_info));
      } else if (header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_llvmo__ObjectFile_O)) {
        llvmo::ObjectFile_O* objectFile = (llvmo::ObjectFile_O*)clientStart;
        size_t objectFileSize = objectFile->objectFileSizeAlignedUp();
        size_t generalSize;
        gctools::clasp_ptr_t dummy = isl_obj_skip(clientStart,false,generalSize);
        if (generalSize==0) ISL_ERROR(fmt::sprintf("A zero size general at %p was encountered", (void*)clientStart ));
        gctools::clasp_ptr_t clientEnd = clientStart + generalSize;
        ISLGeneralHeader_s islheader( General, clientEnd-clientStart, header );
        char* islh = this->_objects->write_buffer( (char*)&islheader , sizeof(ISLGeneralHeader_s)); 
        char* new_client = this->_objects->write_buffer((char*)clientStart, clientEnd-clientStart);
        llvmo::ObjectFile_O* newObjectFile = (llvmo::ObjectFile_O*)new_client;
        char* objectFileAddress = this->_objectFiles->write_buffer( (char*)objectFile->objectFileData(),
                                                                    objectFile->objectFileSize());
        newObjectFile->_ObjectFileOffset = objectFileAddress - this->_objectFiles->_BufferStart;
        newObjectFile->_ObjectFileSize = objectFile->objectFileSize();
        // We want the _MemoryBuffer zero'd out.
        new(&newObjectFile->_MemoryBuffer) std::unique_ptr<llvm::MemoryBuffer>();
        DBG_SAVECOPY(BF("   copied general header %p to %p - %p\n") % header % (void*)islh % (void*)this->_buffer );
        set_forwarding_pointer(header, new_client, this->_info ); // This is a client pointer
        DBG_SL_FWD(BF("setFwdPointer general header %p new_client -> %p  reread fwdPointer -> %p\n")
                   % (void*)header % (void*)new_client % (void*)forwarding_pointer(header,this->_info));
      } else {
      //
      // Now write it into the buffer
      //
        size_t generalSize;
        gctools::clasp_ptr_t dummy = isl_obj_skip(clientStart,false,generalSize);
        if (generalSize==0) ISL_ERROR(fmt::sprintf("A zero size general at %p was encountered", (void*)clientStart ));
        gctools::clasp_ptr_t clientEnd = clientStart + generalSize;
        ISLGeneralHeader_s islheader( General, clientEnd-clientStart, header );
        char* islh = this->_objects->write_buffer( (char*)&islheader , sizeof(ISLGeneralHeader_s)); 
        char* new_client = this->_objects->write_buffer((char*)clientStart, clientEnd-clientStart);
        DBG_SAVECOPY(BF("   copied general header %p to %p - %p\n") % header % (void*)islh % (void*)this->_buffer );
        set_forwarding_pointer(header, new_client, this->_info ); // This is a client pointer
        DBG_SL_FWD(BF("setFwdPointer general header %p new_client -> %p  reread fwdPointer -> %p\n")
                   % (void*)header % (void*)new_client % (void*)forwarding_pointer(header,this->_info));
      }
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)HeaderPtrToConsPtr(header);
      size_t consSize;
      isl_cons_skip(client,consSize);
      if (consSize==0) ISL_ERROR(fmt::sprintf("A zero size cons at %p was encountered" , (void*)client ));
      ISLConsHeader_s islheader( Cons, sizeof(core::Cons_O), header->_stamp_wtag_mtag );
      char* islh = this->_objects->write_buffer( (char*)&islheader , sizeof(ISLConsHeader_s));
      char* new_addr = this->_objects->write_buffer((char*)client, consSize);
      core::Cons_O* cons = (core::Cons_O*)client;
      DBG_SAVECOPY(BF("   copied cons header %p to %p - %p | CAR: %p CDR: %p\n") %
                   header % (void*)islh % (void*)this->_buffer
                   % (void*)cons->_Car.load().raw_() % (void*)cons->_Cdr.load().raw_() );
      set_forwarding_pointer(header, new_addr, this->_info );
      DBG_SL_FWD(BF("setFwdPointer cons header %p new_addr -> %p\n") % (void*)header % (void*)new_addr);
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
//      printf("%s:%d:%s    weak_skip\n", __FILE__, __LINE__, __FUNCTION__ );
      gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)HEADER_PTR_TO_WEAK_PTR(header);
      size_t weakSize;
      gctools::clasp_ptr_t dummyNextClient = isl_weak_skip(clientStart,false,weakSize);
      if (weakSize==0) ISL_ERROR(fmt::sprintf("A zero size weak object at %p was encountered" , (void*)clientStart ));
      gctools::clasp_ptr_t clientEnd = clientStart + weakSize;
      ISLWeakHeader_s islheader( Weak, clientEnd-clientStart, header );
      char* islh = this->_objects->write_buffer( (char*)&islheader , sizeof(ISLWeakHeader_s)); 
      char* new_addr = this->_objects->write_buffer((char*)clientStart, clientEnd-clientStart);
      DBG_SAVECOPY(BF("   copied weak header %p to %p - %p\n") % header % (void*)islh % (void*)this->_buffer );
      set_forwarding_pointer(header, new_addr, this->_info );
      DBG_SL_FWD(BF("setFwdPointer weak header %p new_addr -> %p\n") % (void*)header % (void*)new_addr);
    }
    this->_NumberOfObjects++;
  }
};

//
// walk snapshot save/load objects that start at cur
//
template <typename Walker>
void walk_snapshot_save_load_objects( ISLHeader_s* start, Walker& walker) {
  DBG_SL_WALK_SL(BF("Starting walk cur = %p\n") % (void*)cur);
  ISLHeader_s* cur = start;
  while (cur->_Kind != End) {
    DBG_SL_WALK_SL(BF("walk: %p 0x%lx\n") % (void*)cur % cur->_Kind );
    if (walker._debug) printf("%s:%d:%s Walking %p 0x%lx\n", __FILE__, __LINE__, __FUNCTION__, (void*)cur, cur->_Kind );
    if ( cur->_Kind == General ) {
      ISLGeneralHeader_s* generalCur = (ISLGeneralHeader_s*)cur;
      gctools::Header_s* header = generalCur->header();
      DBG_SL_WALK_SL(BF("general header: %p %s  next: %p\n") % header % header->description() % (void*)generalCur->next() );
      walker.callback(header);
    } else if (cur->_Kind == Cons) {
      ISLConsHeader_s* consCur = (ISLConsHeader_s*)cur;
      gctools::Header_s* header = consCur->header();
      DBG_SL_WALK_SL(BF("cons header: %p %s  next: %p\n") % header % header->description() % (void*)consCur->next<ISLConsHeader_s>() );
      walker.callback(header);
    } else if (cur->_Kind == Weak) {
      ISLWeakHeader_s* weakCur = (ISLWeakHeader_s*)cur;
      gctools::Header_s* header = (gctools::Header_s*)((char*)cur + offsetof(ISLWeakHeader_s,_Header));
      DBG_SL_WALK_SL(BF("weak header: %p %s  next: %p\n") % header % header->description() % (void*)weakCur->next<ISLWeakHeader_s>() ); 
      walker.callback(header);
    } else {
      printf("%s:%d:%s Hit header@%p  with unexpected kind: %lu\n", __FILE__, __LINE__, __FUNCTION__, (void*)cur, cur->_Kind );
      abort();
    }
    cur = cur->next(cur->_Kind);
  }
}




struct fixup_objects_t : public walker_callback_t {
  FixupOperation_ _operation;
  gctools::clasp_ptr_t _buffer;
  fixup_objects_t(FixupOperation_ op, gctools::clasp_ptr_t buffer, ISLInfo* info) :
    walker_callback_t(info), _operation(op), _buffer(buffer) {};

  void callback(gctools::Header_s* header) {
    if (header->_stamp_wtag_mtag.stampP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)HEADER_PTR_TO_GENERAL_PTR(header);
      size_t objectSize;
      isl_obj_skip(client,false,objectSize);
      gctools::clasp_ptr_t client_limit = client + objectSize;
      //
      // This is where we would fixup pointers and entry-points
      //
      // 1. entry points to library code -> offset
      // 2. entry points to Code_O objects -> offset

      isl_obj_scan( 0, client, client_limit, (void*)this->_info );
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)gctools::HeaderPtrToConsPtr(header);
      size_t consSkip;
      gctools::clasp_ptr_t client_limit = isl_cons_skip((gctools::clasp_ptr_t)client,consSkip);
      isl_cons_scan( 0, client, client_limit, (void*)this->_info );
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)HEADER_PTR_TO_WEAK_PTR(header);
      size_t objectSize;
      gctools::clasp_ptr_t client_limit = isl_weak_skip(clientStart,false,objectSize);
      isl_weak_scan( 0, clientStart, client_limit, (void*)this->_info );
    }
  }
};



struct fixup_internals_t : public walker_callback_t {
  Fixup* _fixup;
  fixup_internals_t(Fixup* fixup, ISLInfo* info) :
    walker_callback_t(info), _fixup(fixup) {};

  void callback(gctools::Header_s* header) {
    if (header->_stamp_wtag_mtag.stampP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)HEADER_PTR_TO_GENERAL_PTR(header);
      if (header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_gctools__GCVector_moveable_clbind__detail__edge_)) {
//        printf("%s:%d:%s load_snapshot saw STAMPWTAG_gctools__GCVector_moveable_clbind__detail__edge_\n", __FILE__, __LINE__, __FUNCTION__  );
        gctools::GCVector_moveable<clbind::detail::edge>* edges = (gctools::GCVector_moveable<clbind::detail::edge>*)HEADER_PTR_TO_GENERAL_PTR(header);
//        printf("%s:%d:%s load_snapshot          edges->size() = %lu\n", __FILE__, __LINE__, __FUNCTION__, edges->size() );
        for ( size_t ii = 0; ii< edges->size(); ii++ ) {
//          printf("%s:%d:%s  [%lu] before   target: %lu   cast_function@%p: %p\n", __FILE__, __LINE__, __FUNCTION__, ii, (*edges)[ii].target, &(*edges)[ii].cast, (*edges)[ii].cast);
          decodeEntryPointInLibrary(this->_fixup,(uintptr_t*)&(*edges)[ii].cast);
        }
      }
      if (header->preciseIsPolymorphic()) {
        //
        // Fixup objects when loading/saving
        //
        core::T_O* client = (core::T_O*)HEADER_PTR_TO_GENERAL_PTR(header);
        if (cast::Cast<core::General_O*,core::T_O*>::isA(client)) {
          core::General_O* generalObject = (core::General_O*)client;
          generalObject->fixupInternalsForSnapshotSaveLoad(this->_fixup);
        }
      }
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
    }
  }
};



struct fixup_vtables_t : public walker_callback_t {
  Fixup*    _fixup;
  uintptr_t _vtableRegionStart;
  uintptr_t _vtableRegionEnd;
  uintptr_t _vtableRegionSize;
  fixup_vtables_t(Fixup* fixup, uintptr_t vtableRegionStart, uintptr_t vtableRegionEnd, ISLInfo* info ) :
    walker_callback_t(info),
    _fixup(fixup), _vtableRegionStart(vtableRegionStart), _vtableRegionEnd(vtableRegionEnd) {
    this->_vtableRegionSize = this->_vtableRegionEnd-this->_vtableRegionStart;
  };

  void do_vtable(gctools::Header_s* header, core::T_O* client, uintptr_t& vtable, uintptr_t& new_vtable )
  {
    vtable = *(uintptr_t*)client;
    if ( operation(this->_fixup) == SaveOp ) {
      registerVtable( this->_fixup, client, (uintptr_t)this->_vtableRegionStart );
    } else {
      new_vtable = decodeVtable( this->_fixup, (uintptr_t*)client, (uintptr_t)this->_vtableRegionStart );
      if (new_vtable < this->_vtableRegionStart || this->_vtableRegionEnd <= new_vtable)
        ISL_ERROR(fmt::sprintf("new_vtable %lu is outside of the allowed range %p - %p" , (void*)new_vtable , (void*)this->_vtableRegionStart , (void*)this->_vtableRegionEnd ));
      *(uintptr_t*)client = new_vtable;
    }
  }
  
  void callback(gctools::Header_s* header) {
    uintptr_t new_vtable;
    if (header->_stamp_wtag_mtag.stampP()) {
      if (header->preciseIsPolymorphic()) {
        uintptr_t client = (uintptr_t)HEADER_PTR_TO_GENERAL_PTR(header);
        uintptr_t vtable;
        uintptr_t new_vtable;
        this->do_vtable(header,(core::T_O*)client,vtable,new_vtable);
        DBG_SL_VTABLE(BF(" wrote general base %p vtable in memory at %p value: %p to %p %s\n") % (void*)this->_vtableRegionStart % (void*)client % (void*)vtable % (void*)new_vtable % header->description() );
      }
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      // Do nothing
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      if (header->preciseIsPolymorphic()) {
        uintptr_t client = (uintptr_t)HEADER_PTR_TO_WEAK_PTR(header);
        uintptr_t vtable;
        uintptr_t new_vtable;
        this->do_vtable(header,(core::T_O*)client,vtable,new_vtable);
        DBG_SL_VTABLE(BF(" wrote weak base %p vtable in memory at %p value: %p to %p %s\n") % (void*)this->_vtableRegionStart % (void*)client % (void*)vtable % (void*)new_vtable % header->description() );
      }
    }
  }
};


SYMBOL_EXPORT_SC_( CompPkg, invoke_save_hooks );


struct SaveSymbolCallback : public core::SymbolCallback {
  ISLLibrary&  _Library;

  SaveSymbolCallback( ISLLibrary& lib ) :
    _Library(lib) {};
  
  virtual void callback( const char* name, uintptr_t start, uintptr_t end ) {
    for (size_t ii = 0; ii<this->_Library._GroupedPointers.size(); ++ii ) {
      uintptr_t address = this->_Library._GroupedPointers[ii]._address;
      if ( start <= address && address < end ) {
        std::string sname(name);
        uint addressOffset = (address - start);
        this->_Library._SymbolInfo[ii] = SymbolInfo(/*Debug*/address, addressOffset,
                                                    (uint)sname.size(),
                                                    this->_Library._SymbolBuffer.size() );
        std::copy( sname.begin(), sname.end(), std::back_inserter(this->_Library._SymbolBuffer) );
        this->_Library._SymbolBuffer.push_back('\0');
#if 0
        printf("%s:%d:%s #%lu symbolLength: %u offset: %u match: %d  start: %p end: %p  %s\n",
               __FILE__, __LINE__, __FUNCTION__,
               ii,
               this->_Library._SymbolInfo[ii]._SymbolLength,
               this->_Library._SymbolInfo[ii]._SymbolOffset,
               ((uintptr_t)address == (uintptr_t)start),
               (void*)start,
               (void*)end,
               sname.c_str());
#endif
        return;
      }
    }
  }
  //
  // This generates a symbol table for the _Library
  //
  void generateSymbolTable(Fixup* fixup, SymbolLookup& symbolLookup) {
//    printf("%s:%d:%s  generateSymbolTable for library: %s\n", __FILE__, __LINE__, __FUNCTION__, this->_Library._Name.c_str() );
    size_t hitBadPointers = 0;
    for (ssize_t ii = this->_Library._GroupedPointers.size()-1; ii>=0; --ii ) {
      if (ii%1000==0) {
        printf("%6lu remaining pointers to dladdr\n", ii );
      }
      uintptr_t address = this->_Library._GroupedPointers[ii]._address;
      std::string saveName("");
      uintptr_t saddr;
      bool goodSymbol = symbolLookup.dladdr_(fixup,address,saveName,hitBadPointers,this->_Library._GroupedPointers[ii]._pointerType,saddr);
      if (goodSymbol) {
        uint addressOffset = (address - (uintptr_t)saddr);
        this->_Library._SymbolInfo[ii] = SymbolInfo(/*Debug*/address, addressOffset,
                                                    (uint)saveName.size(),
                                                    this->_Library._SymbolBuffer.size() );
        std::copy( saveName.begin(), saveName.end(), std::back_inserter(this->_Library._SymbolBuffer) );
        this->_Library._SymbolBuffer.push_back('\0');
      }
    }
    if (hitBadPointers) {
      printf("There were %lu bad pointers - we need to figure out how to get this to zero\n", hitBadPointers );
      abort();
    }
  }
};


struct LoadSymbolCallback : public core::SymbolCallback {
  ISLLibrary&  _Library;
  bloom_parameters _parameters;
  bloom_filter* _filter;
  LoadSymbolCallback( ISLLibrary& lib ) : _Library(lib) {
    this->_parameters.projected_element_count = 10000;
    this->_parameters.false_positive_probability = 0.0001; // 1 in 10000
    this->_parameters.random_seed = 0xA5A5A5A5;
    if (!this->_parameters) {
      printf("%s:%d:%s Invalid bloom filter parameters!\n", __FILE__, __LINE__, __FUNCTION__ );
      std::exit(1);
    }
    this->_parameters.compute_optimal_parameters();
    this->_filter = new bloom_filter(this->_parameters);
    for ( size_t ii = 0; ii< this->_Library._SymbolInfo.size(); ++ii ) {
      size_t offset = this->_Library._SymbolInfo[ii]._SymbolOffset;
      const char* name = (const char*)&this->_Library._SymbolBuffer[offset];
      size_t namelen = strlen(name);
      std::string str_name(name);
      this->_filter->insert(str_name);
      if (!this->_filter->contains(name,namelen)) {
        printf("%s:%d:%s Tried to add %s to bloom_filter but it doesn't contain it\n",
               __FILE__, __LINE__, __FUNCTION__, name );
      }
    }
  };
  ~LoadSymbolCallback() {
    delete this->_filter;
  }

  virtual bool interestedInLibrary( const char* name, bool executable) {
    std::string passedFilename = std::filesystem::path(name).filename();
    std::string myFilename = std::filesystem::path(this->_Library._Name).filename();
    if (myFilename == passedFilename || (executable && this->_Library._Executable) ) {
      printf("%s:%d:%s I am interested in library %s/executable:%d - mine is %s/executable:%d\n", __FILE__, __LINE__, __FUNCTION__, name, executable, this->_Library._Name.c_str(), this->_Library._Executable );
      return true;
    }
    printf("%s:%d:%s NOT interested in library %s/executable:%d - mine is %s/executable:%d\n", __FILE__, __LINE__, __FUNCTION__, passedFilename.c_str(), executable, myFilename.c_str(), this->_Library._Executable );
    return false;
  }
      
    
  virtual bool interestedInSymbol( const char* name) {
    size_t namelen = strlen(name);
    return (this->_filter->contains(name,namelen));
  }

  virtual void callback( const char* name, uintptr_t start, uintptr_t end ) {
    size_t num = this->_Library._SymbolInfo.size();
    size_t namelen = strlen(name);
    printf("%s:%d:%s Looking through %lu symbols\n", __FILE__, __LINE__, __FUNCTION__, num );
    if (this->_filter->contains(name,namelen)) {
      for (size_t ii = 0; ii<num; ++ii ) {
        size_t offset = this->_Library._SymbolInfo[ii]._SymbolOffset;
        size_t gpindex = ii;
        const char* myName = (const char*)&this->_Library._SymbolBuffer[offset];
        if ((namelen == this->_Library._SymbolInfo[ii]._SymbolLength)
            && (strcmp(name,myName) == 0) ) {
          this->_Library._GroupedPointers[gpindex]._address = start;
#if 0
          printf("%s:%d:%s GroupedPointers[%lu] saved address %p  symbol address %p @%p\n     name: %s\n",
                 __FILE__, __LINE__, __FUNCTION__,
                 gpindex,
                 (void*)start,
                 (void*)this->_Library._SymbolInfo[ii]._Address,
                 (void*)&this->_Library._SymbolInfo[ii]._Address,
                 name);
          uintptr_t dlsymStart = (uintptr_t)dlsym(RTLD_DEFAULT,name);
          if (dlsymStart!=0 && (dlsymStart != start)) {
            printf("  The dlsym result %p does NOT match delta: 0x%0lx\n", (void*)dlsymStart, ((intptr_t)dlsymStart - (intptr_t)start) );
          } else {
            printf("  The dlsym result matches!!!!!\n");
          }
#endif
          break;
        }
      }
    }
  }

  void loadSymbols(SymbolLookup& lookup) {
    size_t num = this->_Library._SymbolInfo.size();
//    printf("%s:%d:%s About to resolve %lu symbols\n", __FILE__, __LINE__, __FUNCTION__, num );
    for (size_t ii = 0; ii<num; ++ii ) {
      size_t symbolOffset = this->_Library._SymbolInfo[ii]._SymbolOffset;
      size_t gpindex = ii;
      const char* myName = (const char*)&this->_Library._SymbolBuffer[symbolOffset];
      uintptr_t mysymStart = (uintptr_t)lookup.lookupSymbol(myName);
      uintptr_t dlsymStart = (uintptr_t) dlsym(RTLD_DEFAULT,myName);
#if 1
      if (mysymStart) {
        // do nothing - keep going
      } else if (dlsymStart) {
        // we got a dlsymStart - use it
        mysymStart = dlsymStart;
      } else {
        printf("%s:%d:%s Could not resolve address with loopup.lookupSymbol or dlsym for symbol %s\n", __FILE__, __LINE__, __FUNCTION__, myName );
        abort();
      }
#else
      if (dlsymStart !=0 && mysymStart != dlsymStart) {
        printf("%s:%d:%s Mismatch between mysymStart %p and dlsymStart %p for symbol %s - lookupSymbol with verbose=true\n", __FILE__, __LINE__, __FUNCTION__, (void*)mysymStart, (void*)dlsymStart, myName );
        lookup.lookupSymbol(myName,true);
        abort();
      }
      if (!mysymStart) {
        printf("%s:%d:%s Could not resolve address with dlsym for symbol %s\n", __FILE__, __LINE__, __FUNCTION__, myName );
        abort();
      }
#endif
      this->_Library._GroupedPointers[gpindex]._address = mysymStart;
#if 0
      printf("%s:%d:%s GroupedPointers[%lu] restored address %p  offset: %lu saved symbol address %p @%p\n     name: %s\n",
             __FILE__, __LINE__, __FUNCTION__,
             gpindex,
             (void*)dlsymStart,
             this->_Library._SymbolInfo[ii]._AddressOffset,
             (void*)this->_Library._SymbolInfo[ii]._Address,
             (void*)&this->_Library._SymbolInfo[ii]._Address,
             myName);
#endif
    }
  }
};

void prepareRelocationTableForSave(Fixup* fixup, SymbolLookup& symbolLookup) {
  class OrderByAddress {
  public:
    OrderByAddress() {}
    bool operator()(const PointerBase& x, const PointerBase& y) {
        return x._address <= y._address;
    }
  };
  OrderByAddress orderer;
  DBG_SLS(BF("Step1\n" ));
  for ( size_t idx = 0; idx< fixup->_libraries.size(); idx++ ) {
    DBG_SLS(BF("Adding library #%lu: %s\n") % idx % fixup->_libraries[idx]._Name);
    symbolLookup.addLibrary(fixup->_libraries[idx]._Name);
    auto pointersBegin = fixup->_libraries[idx]._InternalPointers.begin();
    auto pointersEnd = fixup->_libraries[idx]._InternalPointers.end();
    if ( pointersBegin < pointersEnd ) {
      DBG_SLS(BF("About to quickSortFirstCheckOrder _Pointers.size(): %lu\n") % fixup->_libraries[idx]._InternalPointers.size());
      sort::quickSortFirstCheckOrder( pointersBegin, pointersEnd, orderer );
    }
  }
  DBG_SLS(BF("Step2\n" ));
  for ( size_t idx=0; idx<fixup->_libraries.size(); idx++ ) {
    int groupPointerIdx = -1;
    ISLLibrary& curLib = fixup->_libraries[idx];
//    printf("%s:%d:%s  Dealing with library: %s\n", __FILE__, __LINE__, __FUNCTION__, curLib._Name.c_str() );
//    printf("%s:%d:%s  Number of pointers before extracting unique pointers: %lu\n", __FILE__, __LINE__, __FUNCTION__, curLib._Pointers.size() );
    for ( size_t ii=0; ii<curLib._InternalPointers.size(); ii++ ) {
      if (groupPointerIdx < 0 || curLib._InternalPointers[ii]._address != curLib._InternalPointers[ii-1]._address ) {
        curLib._GroupedPointers.emplace_back( curLib._InternalPointers[ii]._pointerType, curLib._InternalPointers[ii]._address );
        groupPointerIdx++;
      }
    // Now encode the relocation
      uint8_t firstByte = *(uint8_t*)(*(void**)curLib._InternalPointers[ii]._ptrptr);
      *curLib._InternalPointers[ii]._ptrptr = encodeRelocation_( firstByte, idx, groupPointerIdx );
//      printf("%s:%d:%s Wrote relocation @%p to %p\n", __FILE__, __LINE__, __FUNCTION__, curLib._Pointers[ii]._ptrptr, *curLib._Pointers[ii]._ptrptr );
    }
//    printf("%s:%d:%s  Number of unique pointers: %lu\n", __FILE__, __LINE__, __FUNCTION__, curLib._GroupedPointers.size() );
    SaveSymbolCallback thing(curLib);
    curLib._SymbolInfo.resize(curLib._GroupedPointers.size(),SymbolInfo());
    thing.generateSymbolTable(fixup,symbolLookup);
//    printf("%s:%d:%s  Library #%lu contains %lu grouped pointers\n", __FILE__, __LINE__, __FUNCTION__, idx, curLib._GroupedPointers.size() );
    for ( size_t ii=0; ii<curLib._SymbolInfo.size(); ii++ ) {
      if (curLib._SymbolInfo[ii]._SymbolLength<0) {
        printf("%s:%d:%s The _SymbolInfo[%lu] does not have an length\n", __FILE__, __LINE__, __FUNCTION__, ii );
      }
    }
  }
  DBG_SLS(BF("Step done\n" ));
}

void updateRelocationTableAfterLoad(ISLLibrary& curLib,SymbolLookup& symbolLookup) {
  LoadSymbolCallback thing(curLib);
  curLib._GroupedPointers.resize(curLib._SymbolInfo.size(),GroupedPointer());
#if 0
  core::walk_loaded_objects_symbol_table( &thing );
#else
  thing.loadSymbols(symbolLookup);
#endif
//  printf("%s:%d:%s  Library %s contains %lu grouped pointers\n", __FILE__, __LINE__, __FUNCTION__, curLib._Name.c_str(),curLib._GroupedPointers.size() );
  for ( size_t ii=0; ii<curLib._SymbolInfo.size(); ii++ ) {
    if (curLib._GroupedPointers[ii]._address == 0) {
      printf("%s:%d:%s The _GroupedPointers[%lu] does not have an address\n", __FILE__, __LINE__, __FUNCTION__, ii );
    }
  }
}

void dump_test_results(FILE* fout, const gctools::GatherObjects& gather )
{
  if (fout) {
    fprintf( fout, "%s:%d:%s There are %lu objects in memory\n", __FILE__, __LINE__, __FUNCTION__, gather._Marked.size() );
    fprintf( fout, "%s:%d:%s There are %lu corrupt objects\n",  __FILE__, __LINE__, __FUNCTION__, gather._corruptObjects.size() );
    if (gather._corruptObjects.size()>0) {
      size_t idx(0);
      for ( auto cur : gather._corruptObjects ) {
        std::vector<uintptr_t>& badPointers = cur.second;
        gctools::Header_s* header = (gctools::Header_s*)cur.first;
        gctools::GCStampEnum stamp = header->_stamp_wtag_mtag.stamp_();
        std::string stampName = global_unshifted_nowhere_stamp_names[stamp];
        fprintf( fout, "#%lu -> %p[stamp:%lu] %s\n -- addresses: ", idx, (void*)cur.first, (uintptr_t)stamp, stampName.c_str() );
        idx++;
        for ( auto ptr : badPointers ) {
          fprintf( fout,  " [+%lu]%p(%p)", ((uintptr_t)ptr-((uintptr_t)cur.first+sizeof(void*))), (void*)ptr, *((void**)ptr) );
        }
        fprintf( fout, "\n");
      }
    }
  }
}

#if 0
void test_objects_t::callback(gctools::Header_s* header) {
  if (header->_stamp_wtag_mtag.stampP()) {
    gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)HEADER_PTR_TO_GENERAL_PTR(header);
    size_t objectSize;
    isl_obj_skip(client,false,objectSize);
    gctools::clasp_ptr_t client_limit = client + objectSize;
      //
      // This is where we would test pointers and entry-points
      //
      // 1. entry points to library code -> offset
      // 2. entry points to Code_O objects -> offset

    isl_obj_scan( 0, client, client_limit, (void*)this->_info );
  } else if (header->_stamp_wtag_mtag.consObjectP()) {
    gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)gctools::HeaderPtrToConsPtr(header);
    size_t consSkip;
    gctools::clasp_ptr_t client_limit = isl_cons_skip((gctools::clasp_ptr_t)client,consSkip);
    isl_cons_scan( 0, client, client_limit, (void*)this->_info );
  } else if (header->_stamp_wtag_mtag.weakObjectP()) {
    gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)HEADER_PTR_TO_WEAK_PTR(header);
    size_t objectSize;
    gctools::clasp_ptr_t client_limit = isl_weak_skip(clientStart,false,objectSize);
    isl_weak_scan( 0, clientStart, client_limit, (void*)this->_info );
  }
}

#endif



size_t memory_test(bool dosleep, FILE* fout, const char* message ) {
  //
  // For saving we may want to save snapshots and not die - so use noStomp forwarding.
  //
  gctools::GatherObjects gather;
  gctools::gatherAllObjects(gather);

#if 0
  // this will write out headers of ALL objects
  if (fout) {
    for ( auto xx : gather._Seen ) {
      fprintf( fout, "obj %p\n", (void*)xx );
    }
  }
#endif
  if (fout) dump_test_results( fout, gather );

  size_t result = gather._corruptObjects.size();
  if (dosleep) {
    if (result == 0) {
      printf("!\n");
      printf("!\n");
      printf("!\n");
      printf("! Passed the memory test with zero corrupt objects!\n");
      if (message) {
        printf("! %s\n", message);
      }
      printf("!\n");
      printf("!\n");
      printf("!\n");
    } else if (result>0) {
      printf("!\n");
      printf("!\n");
      printf("!\n");
      printf("! Ran a memory test\n");
      if (message) {
        printf("! %s\n", message);
      }
      printf("! %lu corrupt objects were found in memory test - sleeping for 1000000 seconds\n", result );
      dump_test_results(stdout,gather);
      printf("!  Connect a debugger to pid %d\n", getpid() );
      printf("!\n");
      printf("!\n");
      printf("!\n");
      printf("!\n");
      sleep(1000000);
    }
  }
  return result;
}


CL_LAMBDA(&key filename)
CL_DOCSTRING("Walk all objects in memory and determine how many contain pointers that are not to valid objects. Return the number of corrupt objects that were found.  If filename is provided then dump a report to that file.");
CL_DEFUN size_t gctools__memory_test(core::T_sp filename)
{
  FILE* fout = NULL;
  if (filename.notnilp()) {
    if (gc::IsA<core::String_sp>(filename)) {
      core::String_sp sfilename = gc::As_unsafe<core::String_sp>(filename);
      std::string fname = sfilename->get_std_string();
      fout = fopen(fname.c_str(),"w");
      if (!fout) {
        SIMPLE_ERROR(("Could not open %s") , fname);
      }
    } else {
      SIMPLE_ERROR(("filename must be a string"));
    }
  }
  //
  // Collect twice to try and get the mark bits set properly
  //
  GC_gcollect();
  GC_gcollect();
  size_t result = memory_test(true,fout,NULL);
  if (fout) fclose(fout);
  return result;
}




/* This is not allowed to do any allocations. */
void* snapshot_save_impl(void* data) {
  core::SaveLispAndDie* snapshot_data = (core::SaveLispAndDie*)data;
  global_debugSnapshot = getenv("CLASP_DEBUG_SNAPSHOT")!=NULL;

  printf("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__ );

  //
  // Gather all objects in memory
  //

  gctools::GatherObjects allObjects;
  gctools::gatherAllObjects(allObjects);

  
  //
  // Start the snapshot save process
  //
  Snapshot snapshot;
  
#if defined(USE_BOEHM)
//  printf("%s:%d:%s Not using GC_stop_world_external();\n", __FILE__, __LINE__, __FUNCTION__ );
//  GC_stop_world_external();
#else
  MISSING_GC_SUPPORT();
#endif
  
  if (sizeof(ISLGeneralHeader_s)-offsetof(ISLGeneralHeader_s,_Header) != sizeof(gctools::Header_s)) {
    printf("%s:%d:%s Sanity check for headers in snapshot save/load failed.\n"
           "The _Header field must be the last field in ISLGeneralHeader so that it is IMMEDIATELY followed by a client\n",
           __FILE__, __LINE__, __FUNCTION__ );
    abort();
  }
  DBG_SLS(BF(" Entered\n" ));
  //
  // For real save-lisp-and-die do the following (a simple 19 step plan)
  //
  // 1. Walk all objects in memory and sum their size
  // 2. Allocate that amount of memory + space for roots -> intermediate-buffer
  // 3. Walk all objects in memory
  //     (a) copy them to next position in intermediate-buffer
  //     (b) Set a forwarding pointer in the original object
  // 4. Copy roots into intermediate-buffer
  // 5. Walk all objects in intermediate-buffer and fixup tagged pointers using forwarding pointer
  // 6. Fixup pointers in roots
  //
  // At this point we could write out the snapshot - but it may contain garbage.
  //
  //   Steps 7-14 are an attempt to eliminate objects that made it into the save-snapshot
  //      but are garbage.  I'm not sure the GC is cleaning up enough garbage.
  //      It's basically a mark-and-sweep garbage collection cycle.
  //      Steps 8-13 are identical to 1-6, they just walk different objects.
  //
  // 7. Mark objects in intermediate-buffer accessible from roots
  // 
  // 8. Walk all marked objects and sum their size
  // 9. Allocate that amount of space + space-for roots -> save-buffer
  // 10. Walk all marked objects from intermediate-buffer
  //       (a) copy them to next position in save-buffer
  //       (b) Set a forwarding pointer in the intermediate-buffer object
  // 11. Fixup pointers in save-buffer
  // 12. Copy roots into save-buffer
  // 13. Fixup roots in save-buffer
  //
  //   C++-fixup bytecode fixes up things like std::string and std::vector that
  //     have stuff stored in C++ malloc space.   It's better to eliminate these
  //     as much as possible by redesigning the classes that contain them.
  //     Change std::string to SimpleBaseString_sp and so on.
  //     Every class that needs c++-fixup will provide a function that will generate
  //     c++-fixup bytecode that when evaluated will create C++ objects in malloc memory
  //     and write pointers to those objects into the loaded objects.
  //     Every object except for Cons_O cells will need to have it's vtable pointer fixed up.
  //
  // 15. Generate c++-fixup bytecode for each object that needs it
  // 17. Generate table of contents
  // 18. Write table of contents and save-buffer
  // 19. DIE

  ISLInfo islInfo(SaveOp);
  Fixup fixup(InfoOp);
  
  //
  // Walk the objects in memory and gather a map of function pointers to names
  //
  // Switch to InfoOp
  //
#if 1  
  fixup._operation = InfoOp;
  DBG_SLS(BF("0. Get info on objects for snapshot save\n"));
  gather_info_for_snapshot_save_t gather_info(&fixup,&islInfo);
  walk_gathered_objects( gather_info, allObjects );
#endif
  //
  // Switch to SaveOp
  //
  fixup._operation = SaveOp;

  //
  // First walk the objects in memory and sum their size.
  //

#if 0
  // I'm going to try this right before I fixup the vtables
  DBG_SLS(BF("0. Prepare objects for snapshot save\n"));
  prepare_for_snapshot_save_t prepare(&islInfo);
  walk_gathered_objects( prepare, allObjects );
#endif

  core::write_bf_stream(fmt::sprintf("bf Sum size of all objects\n"));
  DBG_SLS(BF("1. Sum size of all objects\n"));
  calculate_size_t calc_size(&islInfo);
  walk_gathered_objects( calc_size, allObjects );
  fmt::printf("   size = %lu\n", calc_size._TotalSize);
  fmt::printf("   general_count = %lu\n" , calc_size._general_count );
  fmt::printf("   cons_count = %lu\n" , calc_size._cons_count );
  fmt::printf("   weak_count = %lu\n" , calc_size._weak_count );
  fmt::printf("   ObjectFileCount = %lu\n" , calc_size._ObjectFileCount );
  fmt::printf("   CodeCount = %lu\n" , calc_size._CodeCount );

  //
  // 2. Allocate that amount of memory + space for roots -> intermediate-buffer
  //
  size_t roots = sizeof(ISLHeader_s)* (1 + NUMBER_OF_CORE_SYMBOLS + global_symbol_count );
  size_t buffer_size = 
      + calc_size._TotalSize            // for all objects
      + roots                                            // size for roots
      + sizeof(ISLHeader_s)*2;                           // size of last End header
  // Align up memory_size to pagesize

  
  // Add the snapshot save load buffer limits to islInfo
  snapshot._Memory = new copy_buffer_t( gctools::AlignUp(buffer_size) );
  snapshot._ObjectFiles = new copy_buffer_t( calc_size._ObjectFileTotalSize );
  islInfo._islStart = (uintptr_t)snapshot._Memory->_BufferStart;
  islInfo._islEnd = (uintptr_t)snapshot._Memory->_BufferStart+snapshot._Memory->_Size;
//  printf("%s:%d:%s Setting globalISLBufferRange\n", __FILE__, __LINE__, __FUNCTION__ );
  globalISLBufferRange._Start = (gctools::clasp_ptr_t)snapshot._Memory->_BufferStart;
  globalISLBufferRange._End = (gctools::clasp_ptr_t)(snapshot._Memory->_BufferStart+snapshot._Memory->_Size);

  //
  // 3. Walk all objects in memory
  //     (a) copy them to next position in intermediate-buffer
  //     (b) Set a forwarding pointer in the original object
  //
  DBG_SLS(BF("  snapshot._Memory->_BufferStart = %p\n") % (void*)snapshot._Memory->_BufferStart );
  copy_objects_t copy_objects( snapshot._Memory, snapshot._ObjectFiles, &islInfo );
  walk_gathered_objects( copy_objects, allObjects );

  snapshot._HeaderBuffer = new copy_buffer_t(getpagesize()); // enough room for header page aligned
  snapshot._FileHeader = (ISLFileHeader*)snapshot._HeaderBuffer->_BufferStart;
  new (snapshot._FileHeader) ISLFileHeader(snapshot._Memory->_Size,copy_objects._NumberOfObjects,getpagesize());

  ISLEndHeader_s end_header( End );
  char* endend = snapshot._Memory->write_buffer( (char*)&end_header ,  sizeof(end_header));
  DBG_SAVECOPY(BF("   copying END into buffer @ %p\n") % (void*)endend );

  //
  // 4. Copy roots into intermediate-buffer
  //
  ISLRootHeader_s roots1( Roots,  sizeof(core::T_O*) );
  snapshot._FileHeader->_LispRootOffset = snapshot._Memory->write_buffer( (char*)&roots1 ,  sizeof(ISLRootHeader_s)) - snapshot._Memory->_BufferStart;
  snapshot._FileHeader->_LispRootCount = 1;
  snapshot._Memory->write_buffer( (char*)&_lisp ,  sizeof(void*));
  ISLRootHeader_s roots3( Roots, sizeof(core::T_O*)*global_symbol_count );
  snapshot._FileHeader->_SymbolRootsOffset = snapshot._Memory->write_buffer( (char*)&roots3 ,  sizeof(ISLRootHeader_s)) - snapshot._Memory->_BufferStart;
  snapshot._FileHeader->_SymbolRootsCount = global_symbol_count;
  snapshot._Memory->write_buffer( (char*)&global_symbols[0] ,  sizeof(void*)*global_symbol_count);

  //
  // Save the NextUnshiftedStamp so when we load we will pick up where we left off
  // in terms of assigning stamps.
  //
  snapshot._FileHeader->_NextUnshiftedClbindStamp = gctools::global_NextUnshiftedClbindStamp;
  snapshot._FileHeader->_NextUnshiftedStamp = gctools::global_NextUnshiftedStamp;
  //
  // 5. Walk all objects in intermediate-buffer and fixup tagged pointers using forwarding pointer
  //
  DBG_SLS(BF("  Fixing objects starting at %p\n") % (void*)snapshot._Memory->_BufferStart);
  {
    fixup_objects_t fixup_objects(SaveOp, (gctools::clasp_ptr_t)snapshot._Memory->_BufferStart, &islInfo );
    globalPointerFix = maybe_follow_forwarding_pointer;
    globalPointerFixStage = "fixupObjects";
    walk_snapshot_save_load_objects((ISLHeader_s*)snapshot._Memory->_BufferStart,fixup_objects);
  }


  //
  // 6. Fixup pointers in roots
  //

  {
    globalPointerFix = maybe_follow_forwarding_pointer;
    globalPointerFixStage = "fixupRoots";
//    printf("%s:%d:%s  Fixing roots snapshot._Memory->_BufferStart = %p - %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)snapshot._Memory->_BufferStart, (void*)((char*)snapshot._Memory->_BufferStart+snapshot._Memory->_Size ));
    gctools::clasp_ptr_t* lispRoot = (gctools::clasp_ptr_t*) ((char*)snapshot._Memory->_BufferStart + snapshot._FileHeader->_LispRootOffset + sizeof(ISLRootHeader_s));
    followForwardingPointersForRoots( lispRoot, snapshot._FileHeader->_LispRootCount, (void*)&islInfo );
    gctools::clasp_ptr_t* symbolRoots = (gctools::clasp_ptr_t*) ((char*)snapshot._Memory->_BufferStart + snapshot._FileHeader->_SymbolRootsOffset + sizeof(ISLRootHeader_s));
    followForwardingPointersForRoots( symbolRoots, snapshot._FileHeader->_SymbolRootsCount, (void*)&islInfo );
  }

  //
  // Last thing - fixup vtables
  //
  // I'm going to try this right before I fixup the vtables
  DBG_SLS(BF("0. Prepare objects for snapshot save\n"));
  prepare_for_snapshot_save_t prepare(&fixup,&islInfo);
  walk_snapshot_save_load_objects((ISLHeader_s*)snapshot._Memory->_BufferStart,prepare);

  {
    DBG_SLS(BF(" snapshot_save fixing up vtable pointers\n"));
    gctools::clasp_ptr_t start;
    gctools::clasp_ptr_t end;
    core::executableVtableSectionRange(start,end);
    fixup_vtables_t fixup_vtables( &fixup, (uintptr_t)start, (uintptr_t)end, &islInfo );
    walk_snapshot_save_load_objects((ISLHeader_s*)snapshot._Memory->_BufferStart,fixup_vtables);
  }


  //
  // Now generate libraries
  //
  // Calculate the size of the libraries section
  //  printf("%s:%d:%s Setting up SymbolLookup\n", __FILE__, __LINE__, __FUNCTION__ );
  SymbolLookup lookup;
  DBG_SLS(BF(" prepareRelocationTableForSave\n"));
  prepareRelocationTableForSave( &fixup, lookup );
  
  DBG_SLS(BF("done prepareRelocationTableForSave\n"));
  size_t librarySize = 0;
  for (size_t idx=0; idx<fixup._libraries.size(); idx++ ) {
    librarySize += fixup._libraries[idx].writeSize();
  }
  DBG_SLS(BF("copy_buffer_t\n"));
  snapshot._Libraries = new copy_buffer_t(librarySize);
  for (size_t idx=0; idx<fixup._libraries.size(); idx++ ) {
    size_t alignedLen = fixup._libraries[idx].nameSize();
    char* buffer = (char*)malloc(alignedLen);
    ISLLibrary& lib = fixup._libraries[idx];
    memset(buffer,'\0',alignedLen);
    strcpy(buffer,lib._Name.c_str());
    ISLLibraryHeader_s libhead(Library,lib._Executable,lib.writeSize(), alignedLen, alignedLen+lib.symbolBufferSize(), fixup._libraries[idx]._SymbolInfo.size() );
#if 0
    printf("%s:%d:%s ------ &libhead = %p\n", __FILE__, __LINE__, __FUNCTION__, &libhead );
    printf("%s:%d:%s buffer_offset = %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)snapshot._Libraries->buffer_offset() );
    printf("%s:%d:%s libhead._Size = %lu\n", __FILE__, __LINE__, __FUNCTION__, libhead._Size );
    printf("%s:%d:%s libhead._SymbolBufferOffset = %lu\n", __FILE__, __LINE__, __FUNCTION__, libhead._SymbolBufferOffset );
    printf("%s:%d:%s libhead._SymbolInfoOffset = %lu\n", __FILE__, __LINE__, __FUNCTION__, libhead._SymbolInfoOffset );
    printf("%s:%d:%s lib.symbolBufferSize = %lu\n", __FILE__, __LINE__, __FUNCTION__, lib.symbolBufferSize() );
    printf("%s:%d:%s lib.symbolInfoSize = %lu\n", __FILE__, __LINE__, __FUNCTION__, lib.symbolInfoSize() );
    printf("%s:%d:%s Library[%lu] name: %s SymbolBuffer size: %lu\n", __FILE__, __LINE__, __FUNCTION__, idx, lib._Name.c_str(), lib._SymbolBuffer.size());
    printf("%s:%d:%s lib.writeSize() -> %lu\n", __FILE__, __LINE__, __FUNCTION__, lib.writeSize() );
    printf("%s:%d:%s first _SymbolBuffer name: %s\n", __FILE__, __LINE__, __FUNCTION__, (const char*)lib._SymbolBuffer.data());
    printf("%s:%d:%s size _SymbolBuffer %lu\n", __FILE__, __LINE__, __FUNCTION__, lib._SymbolBuffer.size());
    printf("%s:%d:%s num _SymbolInfo %lu\n", __FILE__, __LINE__, __FUNCTION__, lib._SymbolInfo.size());
    printf("%s:%d:%s first _SymbolInfo %u, %u\n", __FILE__, __LINE__, __FUNCTION__, lib._SymbolInfo[0]._SymbolLength, lib._SymbolInfo[0]._SymbolOffset );
    printf("%s:%d:%s last _SymbolInfo %u, %u\n", __FILE__, __LINE__, __FUNCTION__, lib._SymbolInfo[lib._SymbolInfo.size()-1]._SymbolLength, lib._SymbolInfo[lib._SymbolInfo.size()-1]._SymbolOffset );
#endif    
    snapshot._Libraries->write_buffer((char*)&libhead,sizeof(ISLLibraryHeader_s));
    snapshot._Libraries->write_buffer(buffer,alignedLen);
    snapshot._Libraries->write_buffer(lib._SymbolBuffer.data(),lib.symbolBufferSize());
    snapshot._Libraries->write_buffer((char*)lib._SymbolInfo.data(), lib.symbolInfoSize() );
    free(buffer);
  }

  DBG_SLS(BF(" Generating fileHeader\n"));
  
  ISLFileHeader* fileHeader = snapshot._FileHeader;
  uintptr_t offset = snapshot._HeaderBuffer->_Size;
  fileHeader->_LibrariesOffset = offset;
  fileHeader->_NumberOfLibraries = fixup._libraries.size();
  offset += snapshot._Libraries->_Size;
  fileHeader->_SaveTimeMemoryAddress = (uintptr_t)snapshot._Memory->_BufferStart;
  fileHeader->_MemoryStart = offset;
  fileHeader->_NumberOfObjects = copy_objects._NumberOfObjects;
  offset += snapshot._Memory->_Size;

  fileHeader->_ObjectFileStart = offset;
  fileHeader->_ObjectFileSize = snapshot._ObjectFiles->_Size;
  fileHeader->_ObjectFileCount = snapshot._ObjectFiles->_WriteCount;
  fileHeader->describe("Loaded");

  int filedes;
  std::string filename;
  if (snapshot_data->_Executable) {
    filedes = open(snapshot_data->_FileName.c_str(), O_CREAT | O_WRONLY );
    if (filedes<0) {
      printf("Cannot open file %s\n", snapshot_data->_FileName.c_str());
      return NULL;
    }
    filename = snapshot_data->_FileName;
  } else {
    char tfbuffer[32];
    strcpy(tfbuffer,"/tmp/ss-XXXXXXXX");
    filedes = mkstemp(tfbuffer);
    if (filedes<0) {
      printf("Cannot open temporary file for snapshot_save\n" );
      return NULL;
    }
    filename = tfbuffer;
  }
  printf("%s:%d:%s Writing snapshot to %s\n", __FILE__, __LINE__, __FUNCTION__, filename.c_str() );
  if (snapshot_data->_Executable) {
    close(filedes);
    filedes = -1;
  }

  if (snapshot_data->_Executable) {
    std::string cmd;
#ifdef _TARGET_OS_LINUX
    char tlbuffer[32];
    strcpy(tlbuffer,"/tmp/ss-XXXXXXXX");
    int lfiledes = mkstemp(tlbuffer);
    close(lfiledes);
    std::string obj_filename = tlbuffer;

    std::string mangled_name = filename;
    std::replace_if(mangled_name.begin(), mangled_name.end(),
      [](unsigned char c){ return !std::isalnum(c); }, '_');

    std::cout << "Creating binary object from snapshot..." << std::endl << std::flush;
    cmd = OBJCOPY_BINARY " --input-target binary --output-target elf64-x86-64"
      " --binary-architecture i386 " + filename + " " + obj_filename +
      " --redefine-sym _binary_" + mangled_name + "_start=" CXX_MACRO_STRING(SNAPSHOT_START)
      " --redefine-sym _binary_" + mangled_name + "_end=" CXX_MACRO_STRING(SNAPSHOT_END)
      " --redefine-sym _binary_" + mangled_name + "_size=" CXX_MACRO_STRING(SNAPSHOT_SIZE);
    if (system(cmd.c_str()) < 0) {
      std::cerr << "Creation of binary object failed." << std::endl << std::flush;
      return NULL;
    }

    cmd = CXX_BINARY " " BUILD_LINKFLAGS " -L" + snapshot_data->_LibDir +
      " -o" + snapshot_data->_FileName + " " + obj_filename + 
      " -Wl,-whole-archive -lclasp -Wl,-no-whole-archive " BUILD_LIB;
#endif
#ifdef _TARGET_OS_DARWIN
    cmd = CXX_BINARY " " BUILD_LINKFLAGS " -o" + snapshot_data->_FileName + 
      " -sectcreate " SNAPSHOT_SEGMENT " " SNAPSHOT_SECTION " " + filename +
      " -Wl,-force_load," + snapshot_data->_LibDir + "/libclasp.a " BUILD_LIB;
#endif

    std::cout << "Linking executable..." << std::endl << std::flush;
    if (system(cmd.c_str()) < 0) {
      std::cerr << "Linking of executable failed." << std::endl << std::flush;
      return NULL;
    }

#ifdef _TARGET_OS_LINUX
    std::remove(obj_filename.c_str());
#endif
    std::remove(filename.c_str());
 }

  DBG_SLS(BF(" Done snapshot_save_impl\n"));
#if 1
  if (getenv("CLASP_PAUSE_EXIT")) {
    printf("%s:%d PID = %d  Paused at exit - press enter to continue: \n", __FILE__, __LINE__, getpid() );
    fflush(stdout);
    fseek(stdin,0,SEEK_END);
    stdin = freopen(NULL,"r",stdin);
    int c;
    while ( (c = getchar()) != '\n' && c != EOF ) { };
  }
#endif
  exit(0);

#ifdef USE_BOEHM
  printf("%s:%d:%s Not using GC_start_world_external();\n", __FILE__, __LINE__, __FUNCTION__ );
#endif
  return NULL;
}

  
void snapshot_save(core::SaveLispAndDie& data) {
  //
  // For real save-lisp-and-die do the following (a simple 19 step plan)
  //
  // 1. Walk all objects in memory and sum their size
  // 2. Allocate that amount of memory + space for roots -> intermediate-buffer
  // 3. Walk all objects in memory
  //     (a) copy them to next position in intermediate-buffer
  //     (b) Set a forwarding pointer in the original object
  // 4. Walk all objects in intermediate-buffer and fixup tagged pointers using forwarding pointer
  // 5. Copy roots into intermediate-buffer
  // 6. Fixup pointers in roots
  //
  //   Steps 7-14 are an attempt to eliminate objects that made it into the save-image
  //      but are garbage.  I'm not sure the GC is cleaning up enough garbage.
  //      It's basically a mark-and-sweep garbage collection cycle.
  //      Steps 8-13 are identical to 1-6, they just walk different objects.
  //
  // 7. Mark objects in intermediate-buffer accessible from roots
  //
  // 8. Walk all marked objects and sum their size
  // 9. Allocate that amount of space + space-for roots -> save-buffer
  // 10. Walk all marked objects from intermediate-buffer
  //       (a) copy them to next position in save-buffer
  //       (b) Set a forwarding pointer in the intermediate-buffer object
  // 11. Fixup pointers in save-buffer
  // 12. Copy roots into save-buffer
  // 13. Fixup roots in save-buffer
  //
  //   C++-fixup bytecode fixes up things like std::string and std::vector that
  //     have stuff stored in C++ malloc space.   It's better to eliminate these
  //     as much as possible by redesigning the classes that contain them.
  //     Change std::string to SimpleBaseString_sp and so on.
  //     Every class that needs c++-fixup will provide a function that will generate
  //     c++-fixup bytecode that when evaluated will create C++ objects in malloc memory
  //     and write pointers to those objects into the loaded objects.
  //     Every object except for Cons_O cells will need to have it's vtable pointer fixed up.
  //
  // 15. Generate c++-fixup bytecode for each object that needs it
  // 17. Generate table of contents
  // 18. Write table of contents and save-buffer
  // 19. DIE or RETURN

  //
  // Clear out a few things
  //
  gctools::gctools__garbage_collect();
  gctools::gctools__garbage_collect();
  gctools::cl__room(nil<core::Symbol_O>());
  //
  // Test the memory before we do snapshot_save
  //
  gctools::gctools__garbage_collect();
  memory_test( true, NULL, "In preparation for snapshot_save." );

  //
  // For saving we may want to save snapshots and not die - so use noStomp forwarding.
  //
  global_forwardingKind = noStomp;
  printf("%s:%d:%s Updated with noStomp forwarding for snapshot_save\n", __FILE__, __LINE__, __FUNCTION__ );
  //
  // Call Common Lisp code to release things at snapshot-save time
  //
  //  printf("%s:%d:%s About to invoke-snapshot-save-hooks boundp -> %d\n", __FILE__, __LINE__, __FUNCTION__, comp::_sym_invoke_save_hooks->boundP());
  if (comp::_sym_invoke_save_hooks->fboundp()) {
    core::eval::funcall( comp::_sym_invoke_save_hooks );
  }

  printf("%s:%d:%s Finished invoking cmp:invoke-save-hooks\n", __FILE__, __LINE__, __FUNCTION__ );

#if defined(USE_BOEHM)
  GC_call_with_alloc_lock( snapshot_save_impl, &data );
#else
  MISSING_GC_SUPPORT();
#endif
}

struct temporary_root_holder_t {
  void** _buffer;
  size_t _Number;
  void** _Cur;
  static constexpr int Overflow = 1024;
  temporary_root_holder_t(size_t num) : _Number(num){
    if (num==0) {
      printf("The number of objects being loaded is zero!!! That cannot be!!!\n");
      exit(1);
    }
    this->_buffer = (void**)gctools::RootClassAllocator<void>::allocateRootsAndZero(num+Overflow);
    memset(this->_buffer,'\0',(num+Overflow)*sizeof(void*));
    this->_Cur = this->_buffer;
  }

  void release() {
    gctools::RootClassAllocator<void>::freeRoots(this->_buffer);
  }

  //
  // Write a pointer into the temporary roots
  //
  void add(void* ptr) {
    if ((this->_Cur-this->_buffer) > this->_Number ) {
        printf("Overflowed the temporary root buffer with max %lu current: %lu\n", this->_Number, this->_Cur-this->_buffer);
      if ((this->_Cur-this->_buffer) > (this->_Number+Overflow)) {
        printf("Overflowed the OVERFLOW temporary root buffer with max %lu current: %lu\n", this->_Number+Overflow, this->_Cur-this->_buffer);
        std::exit(1);
      }
    }
    *this->_Cur = ptr;
    this->_Cur++;
  }

};


template <typename Walker>
void walk_temporary_root_objects( const temporary_root_holder_t& roots, Walker& walker) {
  DBG_SL_WALK_TEMP(BF("Starting walk of %lu roots at %p\n") % roots._Number % (void*)roots._buffer);
  for (size_t idx = 0; idx< roots._Number; idx++ ) {
    core::T_O* tagged_client = (core::T_O*)roots._buffer[idx];
    // This will handle general and weak objects
    if (gctools::tagged_generalp(tagged_client)) {
      core::T_O* untagged_client = gctools::untag_general<core::T_O*>(tagged_client);
      gctools::Header_s* header = (gctools::Header_s*)gctools::GeneralPtrToHeaderPtr(untagged_client);
      DBG_SL_WALK_TEMP(BF("Walking to GC[%lu/%lu] managed general or weak header %p %s\n") % idx % roots._Number % (void*)header % header->description() );
      walker.callback(header);
    } else if (gctools::tagged_consp(tagged_client)) {
      core::T_O* untagged_client = gctools::untag_cons<core::T_O*>(tagged_client);
      gctools::Header_s* header = (gctools::Header_s*)gctools::ConsPtrToHeaderPtr(untagged_client);
      DBG_SL_WALK_TEMP(BF("Walking to GC[%lu/%lu] managed cons header %p %s\n") % idx % roots._Number % (void*)header % header->description() );
      walker.callback(header);
    } else if (tagged_client==NULL) {
      // Do nothing
    } else {
      printf("%s:%d:%s Illegal temporary root pointer %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)tagged_client);
      abort();
    }
  }
}


intptr_t globalSavedBase;
intptr_t globalLoadedBase;

gctools::clasp_ptr_t relocate_pointer(gctools::clasp_ptr_t* clientAddress, gctools::clasp_ptr_t client, uintptr_t tag, void* user_data) {
  uintptr_t new_client;
  new_client = (uintptr_t)((intptr_t)client - globalSavedBase + globalLoadedBase);
  DBG_SL_RELOCATE0(BF("Relocate0 from %p  to  %p\n") % (void*)client % (void*)new_client );
  return (gctools::clasp_ptr_t)(new_client | tag);
}

struct relocate_objects_t : public walker_callback_t {
  relocate_objects_t( ISLInfo* info) : walker_callback_t(info) {};
  void callback(gctools::Header_s* header) {
    if (header->_stamp_wtag_mtag.stampP()) {
      gctools::clasp_ptr_t client = HEADER_PTR_TO_GENERAL_PTR(header);
      size_t objectSize;
      gctools::clasp_ptr_t client_limit = isl_obj_skip(client,false,objectSize);
      isl_obj_scan( 0, client, client_limit, (void*)this->_info );
    } else if (header->_stamp_wtag_mtag.consObjectP()) {
      gctools::clasp_ptr_t client = (gctools::clasp_ptr_t)HeaderPtrToConsPtr(header);
      size_t consSize;
      gctools::clasp_ptr_t client_limit = isl_cons_skip((gctools::clasp_ptr_t)client,consSize);
      isl_cons_scan( 0, client, client_limit, (void*)this->_info );
      // printf("%s:%d:%s The object @ %p %s isPolymorphic->%d\n", __FILE__, __LINE__, __FUNCTION__, (void*)header, header->description().c_str(), header->preciseIsPolymorphic());
    } else if (header->_stamp_wtag_mtag.weakObjectP()) {
      gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)HEADER_PTR_TO_WEAK_PTR(header);
      size_t objectSize;
      gctools::clasp_ptr_t client_limit = isl_weak_skip(clientStart,false,objectSize);
      isl_weak_scan( 0, clientStart, client_limit, (void*)this->_info );
    }
  }
};


struct CodeFixup_t {
  llvmo::Code_O*  _oldCode;
  llvmo::Code_O*  _newCode;
  CodeFixup_t(llvmo::Code_O* o, llvmo::Code_O* n) : _oldCode(o), _newCode(n) {};
};


int snapshot_load( void* maybeStartOfSnapshot, void* maybeEndOfSnapshot, const std::string& filename ) {

  //
  // For loading we need speed - so use stomp forwarding
  //
  global_forwardingKind = stomp;
  
  {
    MaybeTimeStartup time1("Overall snapshot load time");
//    printf("%s:%d:%s entered maybeStartOfSnapshot = %p   filename = %s\n", __FILE__, __LINE__, __FUNCTION__, maybeStartOfSnapshot, filename.c_str() );
    global_debugSnapshot = getenv("CLASP_DEBUG_SNAPSHOT")!=NULL;
    if (global_debugSnapshot) {
      if (maybeStartOfSnapshot) {
        printf("%s:%d using maybeStartOfSnapshot %p\n", __FILE__, __LINE__, maybeStartOfSnapshot );
      } else {
        printf("%s:%d NOT using maybeStartOfSnapshot\n", __FILE__, __LINE__ );
      }
    }
  // When loading forwarding pointers must always forward into GC managed objects
    size_t loadTimeID = 0;
    globalFwdMustBeInGCMemory = true;
    core::FunctionDescription_O funcdes;
    DBG_SL(BF("FunctionDescription_O vtable pointer is: %p\n") % *(void**)&funcdes );
    DBG_SL(BF(" snapshot_load entered\n"));
    if (filename.size() == 0 && maybeStartOfSnapshot == NULL) {
      printf("You must specify a snapshot with -i or one must be embedded within the executable\n");
      std::exit(1);
    }
    off_t fsize = 0;
    void* memory = NULL;
    if ( filename.size() != 0) {
      int fd = open(filename.c_str(),O_RDONLY);
      fsize = lseek(fd, 0, SEEK_END);
      lseek(fd,0,SEEK_SET);
      memory = mmap(NULL, fsize, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_FILE, fd, 0);
      if (memory==MAP_FAILED) {
        close(fd);
        SIMPLE_ERROR(("Could not mmap %s because of %s") , filename , strerror(errno));
      }
    } else if (maybeStartOfSnapshot && maybeEndOfSnapshot && (maybeStartOfSnapshot<maybeEndOfSnapshot)) {
      size_t size = (uintptr_t)maybeEndOfSnapshot - (uintptr_t)maybeStartOfSnapshot;
      memory = malloc(size);
      memcpy( memory, maybeStartOfSnapshot, size);
    } else {
      printf("There is no snapshot file or embedded\n");
      abort();
    }
    ISLFileHeader* fileHeader = reinterpret_cast<ISLFileHeader*>(memory);
    gctools::global_NextUnshiftedStamp.store(fileHeader->_NextUnshiftedStamp);
    gctools::global_NextUnshiftedClbindStamp.store(fileHeader->_NextUnshiftedClbindStamp);
    char* objectFilesStartAddress = (char*)memory + fileHeader->_ObjectFileStart;
    if (!fileHeader->good_magic()) {
      printf("The file %s is not a snapshot file magic_value should be %p - read... %p\n", filename.c_str(), (void*)MAGIC_NUMBER, (void*)fileHeader->_Magic );
      abort();
    }
    gctools::clasp_ptr_t islbuffer = (gctools::clasp_ptr_t)((char*)memory + fileHeader->_MemoryStart);
    gctools::clasp_ptr_t islend = islbuffer+fileHeader->_MemorySize;
  
    ISLInfo islInfo( LoadOp,(uintptr_t)islbuffer,(uintptr_t)islend);
    Fixup fixup(LoadOp);

//  printf("%s:%d:%s Loaded file %s\n", __FILE__, __LINE__, __FUNCTION__, filename.c_str());
//  printf("%s:%d:%s islbuffer = %p\n",  __FILE__, __LINE__, __FUNCTION__,(void*)islbuffer);
  //
  // Setup the libraries
  //
//  printf("%s:%d:%s Registering %lu globalISLLibraries from snapshot\n", __FILE__, __LINE__, __FUNCTION__, fileHeader->_NumberOfLibraries );
    ISLLibraryHeader_s* libheaderStart = (ISLLibraryHeader_s*)((char*)fileHeader + fileHeader->_LibrariesOffset);
    ISLLibraryHeader_s* libheader;
    {
      MaybeTimeStartup time2("Adjust pointers");
      SymbolLookup lookup;
      FILE* fout = NULL;
      if (core::global_options->_AddressesP) {
        fout = fopen(core::global_options->_AddressesFileName.c_str(),"w");
        fprintf(fout, "# Generating addresses from %s\n", __FUNCTION__ );
        fflush(fout);
        printf("%s:%d:%s opened %s\n", __FILE__, __LINE__, __FUNCTION__, core::global_options->_AddressesFileName.c_str() );
      }

      for ( size_t idx =0; idx<fileHeader->_NumberOfLibraries; idx++ ) {
        if (idx==0) {
          libheader = libheaderStart;
        } else {
      // advance to the next ISLLibraryHeader_s
          libheader = (ISLLibraryHeader_s*)((const char*)(libheader/* + 1 */) + libheader->_Size);
        }
        if (libheader->_Kind != Library ) {
          printf("%s:%d:%s The libheader(offset %p) libheader->_Kind is %p and it must be a Library but it is not - it is %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)((uintptr_t)libheader-(uintptr_t)libheaderStart), (void*)Library, (void*)libheader->_Kind );
          abort();
        }
        char* bufferStart = (char*)(libheader+1);
        std::string execLibPath(bufferStart);
        uintptr_t start;
        uintptr_t end;
        uintptr_t vtableStart;
        uintptr_t vtableEnd;
        bool isexec = libheader->_Executable;
        std::string libraryPath;
        core::library_with_name(execLibPath,isexec,libraryPath,start,end,vtableStart,vtableEnd);
        if (isexec) {
//          printf("%s:%d:%s for %s isexec = %d\n", __FILE__, __LINE__, __FUNCTION__, execLibPath.c_str(), isexec );
//          printf("%s:%d:%s libraryPath -> %s\n", __FILE__, __LINE__, __FUNCTION__, libraryPath.c_str() );
          execLibPath = libraryPath; // swap out the old executable path for the current one
        }
        ISLLibrary lib( libraryPath, isexec, (gctools::clasp_ptr_t)start, (gctools::clasp_ptr_t)end, vtableStart, vtableEnd);
        lookup.addLibrary( libraryPath, fout ); // for debugging pass a stream
//        printf("%s:%d:%s ------ Registered library: %s @ %p\n", __FILE__, __LINE__, __FUNCTION__, libraryPath.c_str(), (void*)start );
#if 0
        ISLLibraryHeader_s& libhead = *libheader;
        printf("%s:%d:%s &libhead = %p\n", __FILE__, __LINE__, __FUNCTION__, &libhead );
        printf("%s:%d:%s (&libhead - fileHeader) = %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)((uintptr_t)libheader - (uintptr_t)fileHeader));
        printf("%s:%d:%s libhead._Size = %lu\n", __FILE__, __LINE__, __FUNCTION__, libhead._Size );
        printf("%s:%d:%s libhead._SymbolBufferOffset = %lu\n", __FILE__, __LINE__, __FUNCTION__, libhead._SymbolBufferOffset );
        printf("%s:%d:%s libhead._SymbolInfoOffset = %lu\n", __FILE__, __LINE__, __FUNCTION__, libhead._SymbolInfoOffset );
        printf("%s:%d:%s libheader = %p\n", __FILE__, __LINE__, __FUNCTION__, libheader );
        printf("%s:%d:%s libheader->_SymbolInfoOffset = %lu\n", __FILE__, __LINE__, __FUNCTION__, libheader->_SymbolInfoOffset );
        printf("%s:%d:%s libheader->_SymbolBufferOffset = %lu\n", __FILE__, __LINE__, __FUNCTION__, libheader->_SymbolBufferOffset );
#endif
        size_t symbolBufferSize = libheader->_SymbolInfoOffset - libheader->_SymbolBufferOffset;
//        printf("%s:%d:%s symbolBufferSize = %lu\n", __FILE__, __LINE__, __FUNCTION__, symbolBufferSize );
        lib._SymbolBuffer.resize(symbolBufferSize);
        const char* symbolBufferStart = (const char*)(libheader+1)+libheader->_SymbolBufferOffset;
        memcpy((char*)lib._SymbolBuffer.data(),symbolBufferStart,symbolBufferSize);
        size_t symbolInfoSize = libheader->_SymbolInfoCount*sizeof(SymbolInfo);
//    printf("%s:%d:%s symbolBufferSize = %lu\n", __FILE__, __LINE__, __FUNCTION__, symbolBufferSize );
//    printf("%s:%d:%s symbolInfoSize = %lu\n", __FILE__, __LINE__, __FUNCTION__, symbolInfoSize );
        lib._SymbolInfo.resize(libheader->_SymbolInfoCount);
        const char* symbolInfoStart = (const char*)(libheader+1)+libheader->_SymbolInfoOffset;
        memcpy((char*)lib._SymbolInfo.data(),symbolInfoStart,symbolInfoSize);
    //
    // Now lookup the pointers
    //
//        printf("%s:%d:%s About to updateRelocationTableAfterLoad\n", __FILE__, __LINE__, __FUNCTION__ );
        if (fout) fflush(fout); // flush fout if it's defined. --arguments option was passed
        updateRelocationTableAfterLoad(lib,lookup);
//        printf("%s:%d:%s Done updateRelocationTableAfterLoad\n", __FILE__, __LINE__, __FUNCTION__ );
        fixup._libraries.push_back(lib);
      }
      if (fout) fclose(fout); // Close fout if it's defined. --arguments option was passed
    }
//  printf("%s:%d:%s Number of fixup._libraries %lu\n", __FILE__, __LINE__, __FUNCTION__, fixup._libraries.size() );
  
  //
  // Define the buffer range
  //
    globalISLBufferRange._Start = (gctools::clasp_ptr_t)islbuffer;
    globalISLBufferRange._End = (gctools::clasp_ptr_t)islbuffer+fileHeader->_MemorySize;

  //
  // Fixup the vtables
  //
    DBG_SL(BF(" snapshot_load fixing up vtable pointers\n"));
    gctools::clasp_ptr_t start;
    gctools::clasp_ptr_t end;
    core::executableVtableSectionRange(start,end);
    {
      MaybeTimeStartup time3("Fixup vtables");
      fixup_vtables_t fixup_vtables( &fixup, (uintptr_t)start, (uintptr_t)end, &islInfo );
      walk_snapshot_save_load_objects((ISLHeader_s*)islbuffer,fixup_vtables);
    }

  //
  // Let's fix the pointers so that they are correct for the loaded location in memory
  //

    {
      MaybeTimeStartup time4("Relocate addresses\n");
      DBG_SL(BF(" snapshot_load relocating addresses\n"));
      globalSavedBase = (intptr_t)fileHeader->_SaveTimeMemoryAddress;
      globalLoadedBase = (intptr_t)islbuffer;
      DBG_SL(BF("  Starting   globalSavedBase %p    globalLoadedBase  %p\n") % (void*)globalSavedBase % (void*)globalLoadedBase );
      globalPointerFix = relocate_pointer;
      relocate_objects_t relocate_objects(&islInfo);
      walk_snapshot_save_load_objects( (ISLHeader_s*)islbuffer, relocate_objects );
    }
  // Do the roots as well
  // After this they will be internally consistent with the loaded objects
  
    gctools::clasp_ptr_t* lispRoot = (gctools::clasp_ptr_t*) ((char*)islbuffer + fileHeader->_LispRootOffset + sizeof(ISLRootHeader_s));
    relocateLoadedRootPointers(lispRoot,1, (void*)&islInfo );
    gctools::clasp_ptr_t* symbolRoots = (gctools::clasp_ptr_t*) ((char*)islbuffer + fileHeader->_SymbolRootsOffset + sizeof(ISLRootHeader_s));
    relocateLoadedRootPointers( symbolRoots, fileHeader->_SymbolRootsCount, (void*)&islInfo  );
    
  //
  // Fixup the CodeBase_O objects
  //
    DBG_SL(BF("Fixup the Library_O objects\n"));
    struct fixup_CodeBase_t : public walker_callback_t {
      fixup_CodeBase_t( ISLInfo* info) : walker_callback_t(info) {};
    
      void callback(gctools::Header_s* header) {
        if (header->_stamp_wtag_mtag.stampP() && header->_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_llvmo__Library_O)) {
          llvmo::Library_O* lib = (llvmo::Library_O*)(HEADER_PTR_TO_GENERAL_PTR(header));
          core::SimpleBaseString_sp name = lib->_Name;
          std::string libraryPath = name->get_std_string();
          std::string libraryFilename = std::filesystem::path(libraryPath).filename();
          std::string libraryName;
          uintptr_t start;
          uintptr_t end;
          uintptr_t vtableStart, vtableEnd;
          bool isExecutable = lib->_Executable;
          core::library_with_name(libraryFilename,isExecutable,libraryName,start,end,vtableStart,vtableEnd);
          lib->_Start = (gctools::clasp_ptr_t)start;
          lib->_End = (gctools::clasp_ptr_t)end;
          lib->_VtableStart = vtableStart;
          lib->_VtableEnd = vtableEnd;
//        printf("%s:%d:%s Setting the .text start of %s to %p\n", __FILE__, __LINE__, __FUNCTION__, libraryName.c_str(), (void*)start );
        }
      }
    };
    fixup_CodeBase_t fixupCodeBase( &islInfo );
    walk_snapshot_save_load_objects((ISLHeader_s*)islbuffer,fixupCodeBase);

  //
  // Allocate space for temporary roots
  //

    temporary_root_holder_t root_holder(fileHeader->_NumberOfObjects);

  //
  // Allocate new objects for everything we just loaded and set the forwarding pointers
  //
    DBG_SL(BF("Allocate objects\n"));
    {
      ISLHeader_s* start_header = reinterpret_cast<ISLHeader_s*>(islbuffer);
      gctools::clasp_ptr_t startVtables;
      gctools::clasp_ptr_t end;
      core::executableVtableSectionRange(startVtables,end);
      DBG_SL(BF(" allocate objects\n"));
      ISLHeader_s* next_header;
      ISLHeader_s* cur_header;


      core::Lisp* theLoadedLisp = (core::Lisp*)gc::untag_general<core::T_O*>((core::T_O*)*lispRoot);

    ///////////////////////////////////////////////////////////////
    //
    // Now we have the Lisp
    //
    // Move the Lisp object into GC managed memory 
      gctools::Header_s* lisp_source_header = NULL;
      {
        lisp_source_header = (gctools::Header_s*)GENERAL_PTR_TO_HEADER_PTR(theLoadedLisp);
        gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)(theLoadedLisp);
        if (((uintptr_t)clientStart&0x7) != 0) {
          printf("%s:%d:%s The Lisp pointer %p must be word aligned\n", __FILE__, __LINE__, __FUNCTION__, clientStart );
          abort();
        }
        gctools::clasp_ptr_t clientEnd = clientStart + sizeof(core::Lisp);
        snapshot_save_load_init_s init(lisp_source_header,clientStart,clientEnd);
        core::T_sp obj = gctools::GCObjectAllocator<core::General_O>::snapshot_save_load_allocate(&init);
        gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
        DBG_SL_ALLOCATE(BF("allocated Lisp general %p fwd: %p\n")
                        % (void*) obj.raw_()
                        % (void*)fwd);
        set_forwarding_pointer( lisp_source_header, (char*)fwd, &islInfo );
        root_holder.add((void*)obj.raw_());
        ::_lisp.thePointer = (core::Lisp*)obj.theObject;
      // Now the global _lisp is defined - don't change it below when we look at roots
      }
//    printf("%s:%d:%s the ::_lisp object is at %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)_lisp.rawRef_());
    
    //
    // Initialize the ClaspJIT_O object
    //
      llvmo::global_JITDylibCounter.store(fileHeader->_global_JITDylibCounter);
      core::core__update_max_jit_compile_counter(fileHeader->_global_JITCompileCounter);
      llvmo::ClaspJIT_O* claspJIT = (llvmo::ClaspJIT_O*)gctools::untag_general<core::T_O*>(_lisp->_Roots._ClaspJIT.raw_());
      llvmo::JITDylib_O* mainJITDylib = (llvmo::JITDylib_O*)gc::untag_general<core::T_O*>(claspJIT->_MainJITDylib.raw_());
      new (claspJIT) llvmo::ClaspJIT_O(true,mainJITDylib);
      if (mainJITDylib->_Id != 0) {
        printf("%s:%d:%s The mainJITDylib _Id MUST be zero !!!  Instead it is: %lu\n", __FILE__, __LINE__, __FUNCTION__, mainJITDylib->_Id);
        abort();
      }

    //
    // We need to handle the NIL object specially, we need to allocate it in GC memory first
    //
      gctools::Header_s* nil_source_header = NULL;
      {
        core::T_sp theNil = ::_lisp->_Roots._NilObject;
        core::General_O* generalNil = theNil.unsafe_general();
        gctools::Header_s* source_header = GENERAL_PTR_TO_HEADER_PTR(generalNil);
        gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)(generalNil);
        gctools::clasp_ptr_t clientEnd = clientStart + sizeof(core::Null_O);
        snapshot_save_load_init_s init(source_header,clientStart,clientEnd);
        core::T_sp nil = gctools::GCObjectAllocator<core::General_O>::snapshot_save_load_allocate(&init);
        gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)nil.raw_());
        DBG_SL_ALLOCATE(BF("allocated general %p fwd: %p\n")
                        % (void*) nil.raw_()
                        % (void*)fwd);
        set_forwarding_pointer(source_header,((char*)fwd), &islInfo );
        nil_source_header = source_header;
        root_holder.add((void*)nil.raw_());
        my_thread->finish_initialization_main_thread(nil);
      // Now we have NIL in 'nil' - use it to initialize a few things.
        _lisp->_Roots._AllObjectFiles.store(nil);
      }
    
    //
    // Initialize the JITDylibs
    // We can't use gc::As<xxx>(...) at this point because we are working in the snapshot save/load buffer
    //  and the headers aren't the same as in main memory
    //
      core::T_sp tjit = ::_lisp->_Roots._ClaspJIT;
      llvmo::ClaspJIT_sp jit;
      if (tjit.notnilp()) {
        jit = gc::As_unsafe<llvmo::ClaspJIT_sp>(tjit);
        core::T_sp cur = ::_lisp->_Roots._JITDylibs.load();
        while (cur.consp()) {
          llvmo::JITDylib_sp dy = gc::As_unsafe<llvmo::JITDylib_sp>(CONS_CAR(cur));
          jit->registerJITDylibAfterLoad(&*dy);
          if (dy->_Id == 0) {
            printf("%s:%d:%s The JITDylib _Id must NOT be zero - that is reserved for the main JITDylib !!!  Instead it is: %lu\n", __FILE__, __LINE__, __FUNCTION__, dy->_Id);
            abort();
          }
          cur = CONS_CDR(cur);
        }
      } else {
        printf("%s:%d:%s Could not find the LLJIT!!!!\n", __FILE__, __LINE__, __FUNCTION__ );
        abort();
      }

      std::vector<CodeFixup_t> codeFixups;
    
      size_t countNullObjects = 0;
      size_t objectFileCount = 0;
      std::map<gctools::Header_s*,char*> objectFileForwards;
      {
      // Link all the code objects
        MaybeTimeStartup time5("Object file linking");
        for ( cur_header = start_header; cur_header->_Kind != End; ) {
          DBG_SL_ALLOCATE(BF("-----Allocating based on cur_header %p\n") % (void*)cur_header );
          if (cur_header->_Kind == General) {
            ISLGeneralHeader_s* generalHeader = (ISLGeneralHeader_s*)cur_header;
            gctools::Header_s* source_header = (gctools::Header_s*)&generalHeader->_Header;
            gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)(generalHeader+1);
            gctools::clasp_ptr_t clientEnd = clientStart + generalHeader->_Size;
            snapshot_save_load_init_s init(&generalHeader->_Header,clientStart,clientEnd);
            DBG_SL_ALLOCATE(BF("  source_header %p  stamp: %u  size: %lu kind: %s\n")
                            % (void*) source_header
                            % generalHeader->_stamp_wtag_mtag._value
                            % generalHeader->_Size
                            % source_header->description().c_str());
            if ( generalHeader->_Header._stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_llvmo__ObjectFile_O) ) {
          // Handle the ObjectFile_O objects - pass them to the LLJIT
              llvmo::ObjectFile_O* loadedObjectFile = (llvmo::ObjectFile_O*)clientStart;
          //
          // Now copy the object file into C++ memory
          //
              char* loadedObjectFileStart = objectFilesStartAddress + loadedObjectFile->_ObjectFileOffset;
              size_t of_length = loadedObjectFile->_ObjectFileSize;
              char* of_start = (char*)malloc(loadedObjectFile->_ObjectFileSize);
              memcpy((void*)of_start, loadedObjectFileStart, of_length );
              DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s About to pass LLJIT ObjectFile_O @ %p  startupID: %lu  _ObjectFileOffset %lu  _ObjectFileSize %lu  of_start %p\n",
                                        __FILE__, __LINE__, __FUNCTION__,
                                        loadedObjectFile,
                                        loadedObjectFile->_ObjectId,
                                        loadedObjectFile->_ObjectFileOffset,
                                        loadedObjectFile->_ObjectFileSize,
                                        of_start ));
              llvm::StringRef sbuffer((const char*)of_start, of_length);
              std::string uniqueName = llvmo::uniqueMemoryBufferName("of",loadedObjectFile->_ObjectId, loadedObjectFile->_ObjectFileSize );
              llvm::StringRef name(uniqueName);
              std::unique_ptr<llvm::MemoryBuffer> memoryBuffer(llvm::MemoryBuffer::getMemBuffer(sbuffer,name,false));
              loadedObjectFile->_MemoryBuffer.reset();
              loadedObjectFile->_MemoryBuffer = std::move(memoryBuffer);
              llvmo::Code_sp oldCode = loadedObjectFile->_Code;
          // Allocate a new ObjectFile_O
              core::T_sp tallocatedObjectFile = gctools::GCObjectAllocator<core::General_O>::snapshot_save_load_allocate(&init);
              llvmo::ObjectFile_sp allocatedObjectFile = gc::As<llvmo::ObjectFile_sp>(tallocatedObjectFile);
              DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s About to pass a freshly allocated ObjectFile_sp %p to the LLJIT\n"
                                        "!!!!    The unix object file is at %p size: %lu\n"
                                        "!!!!   - when we get it back in ClaspReturnObjectBuffer will it be at the same place?\n"
                                        "!!!!     Turn on DEBUG_OBJECT_FILES to find out where ClaspReturnObjectBuffer sees the object file\n"
                                        ,
                                        __FILE__, __LINE__, __FUNCTION__,
                                        (void*)allocatedObjectFile.raw_(),
                                        of_start, of_length ));
              jit->addObjectFile(allocatedObjectFile->asSmartPtr(),false);
              core::T_mv startupName = core::core__startup_linkage_shutdown_names(allocatedObjectFile->_ObjectId,nil<core::T_O>());
              core::String_sp str = gc::As<core::String_sp>(startupName);
              DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s I added the ObjectFile to the LLJIT - startupName: %s  --- what do I do to get the code\n", __FILE__, __LINE__, __FUNCTION__, core::_rep_(str).c_str() ));
              void* ptr;
          //
          // Everything after this will have to change when we do multicore startup.
          // lookup will cause multicore linking and with multicore linking we have to do things after this in a thread safe way
              bool found = jit->do_lookup( *allocatedObjectFile->_JITDylib->wrappedPtr(), str->get_std_string(), ptr );
              if (!found) {
                printf("%s:%d:%s Could not find startupName: %s\n", __FILE__, __LINE__, __FUNCTION__, str->get_std_string().c_str() );
                abort();
              }
              DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s ClaspReturnObjectBuffer modified our current ObjectFile_O object\n"
                                        "!!!!     We want to get the ObjectFile_O->_Code object and use that as the forward pointer for the\n"
                                        "!!!!     Code_O object @ %p that was in the ObjectFile_O object BEFORE we called jit->addObjectFile\n"
                                        "!!!!     After jit->addObjectFile the ObjectFile_O->_Code is %p\n"
                                        ,
                                        __FILE__, __LINE__, __FUNCTION__,
                                        oldCode.raw_(),
                                        allocatedObjectFile->_Code.raw_()));
//          core::T_sp obj = gctools::GCObjectAllocator<core::General_O>::snapshot_save_load_allocate(&init);
              gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)allocatedObjectFile.raw_());
              DBG_SL_ALLOCATE(BF("allocated general %p fwd: %p\n")
                              % (void*) obj.raw_()
                              % (void*)fwd);
              objectFileForwards[source_header] = (char*)fwd;
            // set_forwarding_pointer(source_header,((char*)fwd), &islInfo );
              root_holder.add((void*)allocatedObjectFile.raw_());
          //
          // Now set the new code pointer as the forward for the oldCode object
          //
              core::T_sp code = allocatedObjectFile->_Code;
              if (code == oldCode) {
                printf("%s:%d:%s Something is wrong - the new code %p is the same as the old code %p\n",
                       __FILE__, __LINE__, __FUNCTION__,
                       (void*)code.raw_(), (void*)oldCode.raw_() );
              }
              root_holder.add((void*)code.raw_());
              codeFixups.emplace_back(CodeFixup_t((llvmo::Code_O*)oldCode.unsafe_general(),((llvmo::Code_O*)code.unsafe_general())));
              if (global_debugSnapshot) {
                printf("%s:%d:%s Passed     ObjectFile_sp %p Code_sp %p  %lu/%lu to LLJIT\n",
                       __FILE__, __LINE__, __FUNCTION__,
                       allocatedObjectFile.raw_(),
                       code.raw_(),
                       objectFileCount,
                       fileHeader->_ObjectFileCount );
              }
              llvmo::Code_sp ccode = gc::As<llvmo::Code_sp>(code);
              if (ccode->_TextSectionStart == NULL) {
                printf("%s:%d:%s The Code_sp %p object has NULL for the _TextSectionStart\n",
                       __FILE__, __LINE__, __FUNCTION__,
                       ccode.raw_());
              }
              objectFileCount++;
            }
          }
          next_header = cur_header->next(cur_header->_Kind);
          size_t size = cur_header->_Size;
          DBG_SL1(BF("Done working with cur_header@%p  advanced to %p where cur_header->_Size = %lu\n") % (void*)cur_header % (void*)next_header % size );
          cur_header = next_header;
        }
      }
      char* pause_startup = getenv("CLASP_PAUSE_OBJECTS_ADDED");
      if (pause_startup) {
#ifdef USE_USER_SIGNAL
        gctools::setup_user_signal();
        gctools::wait_for_user_signal("Paused at startup after object files added");
#else
        printf("%s:%d PID = %d  Paused at startup after object files added to JIT - press enter to continue: \n", __FILE__, __LINE__, getpid() );
        fflush(stdout);
        getchar();
#endif
      }
      {
      // Allocate all other objects
        MaybeTimeStartup time6("Allocate objects");
        for ( cur_header = start_header; cur_header->_Kind != End; ) {
          DBG_SL_ALLOCATE(BF("-----Allocating based on cur_header %p\n") % (void*)cur_header );
          if (cur_header->_Kind == General) {
            ISLGeneralHeader_s* generalHeader = (ISLGeneralHeader_s*)cur_header;
            gctools::Header_s* source_header = (gctools::Header_s*)&generalHeader->_Header;
            gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)(generalHeader+1);
            gctools::clasp_ptr_t clientEnd = clientStart + generalHeader->_Size;
            snapshot_save_load_init_s init(&generalHeader->_Header,clientStart,clientEnd);
            DBG_SL_ALLOCATE(BF("  source_header %p  stamp: %u  size: %lu kind: %s\n")
                            % (void*) source_header
                            % generalHeader->_stamp_wtag_mtag._value
                            % generalHeader->_Size
                            % source_header->description().c_str());
            if (source_header == lisp_source_header) {
//          printf("%s:%d:%s About to skip allocation of the Lisp object - this is just for information - delete this message\n", __FILE__, __LINE__, __FUNCTION__ );
            } else if (source_header == nil_source_header) {
          // Skip the NIL object we handled above
              countNullObjects++; // It's a Null_O object but its header has been obliterated by a fwd
            } else if ( generalHeader->_Header._stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_llvmo__ObjectFile_O) ) {
            // Set the forwarding pointer defined above.
              gctools::Header_s* source_header = (gctools::Header_s*)&generalHeader->_Header;
              char* fwd = objectFileForwards[source_header];
              set_forwarding_pointer( source_header, fwd, &islInfo );
            } else if ( generalHeader->_Header._stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_llvmo__Code_O) ) {
          // Don't do anything with Code_O objects - they are create by the ObjectFile_O objects
              DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Skip the Code_O object\n", __FILE__, __LINE__, __FUNCTION__ ));
            } else if ( generalHeader->_Header._stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_core__Null_O) ) {
          // Don't do anything with the only Null_O object - it was allocated above when we fished it out
          // of the Lisp object and allocated it.
              DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Skip the Null_O object\n", __FILE__, __LINE__, __FUNCTION__ ));
              countNullObjects++;
            } else {
              core::T_sp obj = gctools::GCObjectAllocator<core::General_O>::snapshot_save_load_allocate(&init);
              gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
              DBG_SL_ALLOCATE(BF("allocated general %p fwd: %p\n")
                              % (void*) obj.raw_()
                              % (void*)fwd);
              set_forwarding_pointer(source_header,((char*)fwd), &islInfo);
              root_holder.add((void*)obj.raw_());
            }
          } else if (cur_header->_Kind == Cons ) {
            ISLConsHeader_s* consHeader = (ISLConsHeader_s*)cur_header;
            gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)(consHeader+1);
            gctools::clasp_ptr_t clientEnd = clientStart + sizeof(core::Cons_O);
            gctools::Header_s* header = consHeader->header();
            core::Cons_O* cons = (core::Cons_O*)clientStart;
            auto obj = gctools::ConsAllocator<core::Cons_O,gctools::DoRegister>::snapshot_save_load_allocate(header->_stamp_wtag_mtag, cons->_Car.load(), cons->_Cdr.load());
            gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
            DBG_SL_ALLOCATE(BF("---- Allocated Cons %p copy from %p header: %p  set fwd to %p\n")
                            % (void*)obj.raw_()
                            % (void*) header
                            % (void*)clientStart % (void*)fwd );
            set_forwarding_pointer(header,((char*)fwd), &islInfo);
            root_holder.add((void*)obj.raw_());
          } else if (cur_header->_Kind == Weak ) {
            ISLWeakHeader_s* weakHeader = (ISLWeakHeader_s*)cur_header;
            gctools::Header_s* header = (gctools::Header_s*)&weakHeader->_Header;
            gctools::clasp_ptr_t clientStart = (gctools::clasp_ptr_t)(weakHeader+1);
            gctools::clasp_ptr_t clientEnd = clientStart + weakHeader->_Size;
            snapshot_save_load_init_s init(header,clientStart,clientEnd);
            gctools::Header_s::WeakKinds kind = (gctools::Header_s::WeakKinds)header->_stamp_wtag_mtag._value;
            switch (kind) {
#if 1
            case gctools::Header_s::WeakBucketKind: {
              auto obj = gctools::GCBucketAllocator<gctools::Buckets<core::T_sp,core::T_sp,gctools::WeakLinks>>::snapshot_save_load_allocate(&init);
              gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
              DBG_SL_ALLOCATE(BF("allocated weak %p header: %p stamp: %lu  fwd: %p\n")
                              % (void*) obj.raw_()
                              % (void*) header
                              % (uintptr_t)kind
                              % (void*)fwd );
              set_forwarding_pointer(header,((char*)fwd), &islInfo);
              root_holder.add((void*)obj.raw_());
              break;
            }
            case gctools::Header_s::StrongBucketKind: {
              auto obj = gctools::GCBucketAllocator<gctools::Buckets<core::T_sp,core::T_sp,gctools::StrongLinks>>::snapshot_save_load_allocate(&init);
              gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
              DBG_SL_ALLOCATE(BF("allocated weak %p header: %p stamp: %lu  fwd: %p\n")
                              % (void*) obj.raw_()
                              % (void*) header
                              % (uintptr_t)kind
                              % (void*)fwd );
              set_forwarding_pointer(header,((char*)fwd), &islInfo);
              root_holder.add((void*)obj.raw_());
              break;
            }
#endif
#if 0
            case gctools::Header_s::WeakMappingKind: {
              auto obj = gctools::GCMappingAllocator<gctools::Mapping<core::T_sp,core::T_sp,gctools::WeakLinks>>::snapshot_save_load_allocate(&init);
              gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
              DBG_SL_ALLOCATE(BF("allocated weak %p header: %p stamp: %lu fwd: %p\n")
                              % (void*) obj.raw_()
                              % (void*) header
                              % (uintptr_t)kind
                              % (void*)fwd );
              set_forwarding_pointer(header,((void*)fwd),this->_info);
              root_holder.add((void*)obj.raw_());
              break;
            }
            case gctools::Header_s::StrongBucketKind: {
              auto obj = gctools::GCMappingAllocator<gctools::Mapping<core::T_sp,core::T_sp,gctools::StrongLinks>>::snapshot_save_load_allocate(&init);
              gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
              DBG_SL_ALLOCATE(BF("allocated weak %p header: %p stamp: %lu fwd: %p\n")
                              % (void*) obj.raw_()
                              % (void*) header
                              % (uintptr_t)kind
                              % (void*)fwd );
              set_forwarding_pointer(header,((void*)fwd),this->_info);
              root_holder.add((void*)obj.raw_());
              break;
            }
#endif
#if 0
            case gctools::Header_s::WeakPointerKind: {
              auto obj = gctools::GCWeakPointerAllocator<gctools::WeakPointer>::snapshot_save_load_allocate(&init);
              gctools::Tagged fwd = (gctools::Tagged)gctools::untag_object<gctools::clasp_ptr_t>((gctools::clasp_ptr_t)obj.raw_());
              DBG_SL_ALLOCATE(BF("allocated weak %p header: %p stamp: %lu  fwd: %p\n")
                              % (void*) obj.raw_()
                              % (void*) header
                              % (uintptr_t)kind
                              % (void*)fwd );
              set_forwarding_pointer(header,((void*)fwd),this->_info);
              root_holder.add((void*)obj.raw_());
              break;
            }
#endif
            default:
                printf("%s:%d:%s  Handle allocate weak objects\n", __FILE__, __LINE__, __FUNCTION__ );
                break;
            }
          } else {
            printf("%s:%d:%s Unknown header at offset 0x%lx qword: 0x%lx\n", __FILE__, __LINE__, __FUNCTION__, (uintptr_t)cur_header - (uintptr_t)fileHeader, *(uintptr_t*)cur_header );
          }
          next_header = cur_header->next(cur_header->_Kind);
          size_t size = cur_header->_Size;
          DBG_SL1(BF("Done working with cur_header@%p  advanced to %p where cur_header->_Size = %lu\n") % (void*)cur_header % (void*)next_header % size );
          cur_header = next_header;
        }
      }

    //
    // Fixup all the code objects now
    //
      for ( size_t idx = 0; idx<codeFixups.size(); idx++ ) {
        llvmo::Code_O* oldCodeClient = codeFixups[idx]._oldCode;
        llvmo::Code_O* newCodeClient = codeFixups[idx]._newCode;
      //
      // This is where we move the literals and change the state of the new code
      //
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s  Fixup code old: %p new: %p  new literals start %p  size: %lu\n",
                                  __FILE__, __LINE__, __FUNCTION__,
                                  oldCodeClient, newCodeClient,
                                  (void*)newCodeClient->literalsStart(),
                                  newCodeClient->literalsSize() ));
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s This is where I would copy the literals from the oldCodeClient to the newCodeClient\n", __FILE__, __LINE__, __FUNCTION__ ));
        if (oldCodeClient->_State != llvmo::SaveState) {
          printf("%s:%d:%s The oldCodeClient at %p must be in SaveState\n", __FILE__, __LINE__, __FUNCTION__, oldCodeClient );
          abort();
        }
        if (newCodeClient->_State != llvmo::RunState) {
          printf("%s:%d:%s The newCodeClient at %p must be in RunState\n", __FILE__, __LINE__, __FUNCTION__, newCodeClient );
          abort();
        }
        if (oldCodeClient->literalsSize() != newCodeClient->literalsSize() ) {
          printf("%s:%d:%s The oldCodeClient at %p has literalsSize() %lu and the newCodeClient at %p has literalsSize() %lu - the do not match and they must match\n",
                 __FILE__, __LINE__, __FUNCTION__,
                 oldCodeClient, oldCodeClient->literalsSize(),
                 newCodeClient, newCodeClient->literalsSize() );
          abort();
        }
        uintptr_t newCodeLiteralsStart = (uintptr_t)newCodeClient->literalsStart();
        uintptr_t newCodeLiteralsEnd = (uintptr_t)newCodeClient->literalsStart() + newCodeClient->literalsSize();
        uintptr_t newCodeDataStart = (uintptr_t)&newCodeClient->_DataCode[0];
        uintptr_t newCodeDataEnd = (uintptr_t)&newCodeClient->_DataCode[newCodeClient->_DataCode.size()];
      
        if (oldCodeClient->literalsSize() != 0 &&
            !( newCodeDataStart <= newCodeLiteralsStart
               && newCodeLiteralsEnd <= newCodeDataEnd ) ) {
          printf("%s:%d:%s The newCodeClient literals range %p - %p must be within the bounds of the newCodeClient data range %p - %p and it is not\n",
                 __FILE__, __LINE__, __FUNCTION__,
                 (void*)newCodeLiteralsStart,
                 (void*)newCodeLiteralsEnd,
                 (void*)newCodeDataStart,
                 (void*)newCodeDataEnd );
          abort();
        }
      //
      // Finally, finally, finally - we copy the oldCodeClient literal vector into the newCodeClient literal vector.
      //
        if (oldCodeClient->literalsSize()!=0) {
          memcpy( (void*)newCodeLiteralsStart, (void*)oldCodeClient->literalsStart(), newCodeClient->literalsSize() );
        }
      //
      // Now set the forwarding pointer from the oldCode object to the newCodeClient
      //
        gctools::Header_s* oldCode_source_header = GENERAL_PTR_TO_HEADER_PTR(oldCodeClient);
        set_forwarding_pointer(oldCode_source_header,((char*)newCodeClient), &islInfo );
        DBG_SL_FWD(BF("setFwdPointer code header %p new_addr -> %p  reread fwdPointer -> %p\n")
                   % (void*)oldCode_source_header % (void*)newCodeClient % (void*)forwarding_pointer(oldCode_source_header,&islInfo));
      }
    
      if (countNullObjects!=1) {
        printf("%s:%d:%s The number of Null_O objects in the loaded snapshot must be exactly 1(one) - it is: %lu\n"
               "This means that more than one Null_O objects was saved at snapshot save time - that's wrong wrong wrong\n"
               "Figure out why there are more than one Null_O objects\n", __FILE__, __LINE__, __FUNCTION__, countNullObjects);
        abort();
      }
      DBG_SL(BF("Done working with all objects cur_header@%p\n") % (void*)cur_header );
    }

  //
  // Ensure all isl buffer objects are forwarding
  //
    {
      ensure_forward_t ensure_forward( &islInfo );
      walk_snapshot_save_load_objects((ISLHeader_s*)islbuffer,ensure_forward);
    }
  
  //
  // Walk all the objects and fixup all the pointers
  //
    
    DBG_SL(BF("======================= fixup pointers\n"));
    {
      fixup_objects_t fixup_objects( LoadOp, (gctools::clasp_ptr_t)islbuffer, &islInfo );
      globalPointerFix = maybe_follow_forwarding_pointer;
      globalPointerFixStage = "snapshot_load/fixupObjects";
      walk_temporary_root_objects(root_holder,fixup_objects);
    }

  //
  // Fixup the roots
  //
    {
      gctools::clasp_ptr_t* lispRoot = (gctools::clasp_ptr_t*) ((char*)islbuffer + fileHeader->_LispRootOffset + sizeof(ISLRootHeader_s));
//    followForwardingPointersForRoots( lispRoot, fileHeader->_LispRootCount, (void*)&islInfo );
//    copyRoots((uintptr_t*)&_lisp, (uintptr_t*)lispRoot, fileHeader->_LispRootCount );
      gctools::clasp_ptr_t* symbolRoots = (gctools::clasp_ptr_t*) ((char*)islbuffer + fileHeader->_SymbolRootsOffset + sizeof(ISLRootHeader_s));
      followForwardingPointersForRoots( symbolRoots, fileHeader->_SymbolRootsCount, (void*)&islInfo );
      copyRoots((uintptr_t*)&global_symbols[0], (uintptr_t*)symbolRoots, fileHeader->_SymbolRootsCount );
    }

//  printf("%s:%d:%s Number of fixup._libraries %lu\n", __FILE__, __LINE__, __FUNCTION__, fixup._libraries.size() );
    fixup_internals_t  internals( &fixup, &islInfo );
    walk_temporary_root_objects( root_holder, internals );

  //
  // Release the temporary roots
  //
#if 0
    printf("%s:%d:%s Not releasing temporary roots\n", __FILE__, __LINE__, __FUNCTION__ );
#else
    root_holder.release();
#endif

  //
  // munmap the memory
  //
#if 0
    printf("%s:%d:%s Not munmap'ing loaded snapshot - filling with 0xc0\n", __FILE__, __LINE__, __FUNCTION__ );
  // Fill it with 0xc0
//  memset(memory,0xc0,fsize);
#else  
//  printf("%s:%d:%s munmap'ing loaded snapshot - filling with 0xc0\n", __FILE__, __LINE__, __FUNCTION__ );
    if (maybeStartOfSnapshot==NULL) {
      int res = munmap( memory, fsize );
      if (res!=0) SIMPLE_ERROR("Could not munmap memory");
    } else {
    // It's a copy of the embedded snapshot
      free(memory);
    }
#endif

  // 
  // Initialize the main thread
  //  and some other vital objects
  //
    SYMBOL_EXPORT_SC_(CompPkg,STARthread_local_builtins_moduleSTAR);
  
    _lisp->initializeMainThread();
    comp::_sym_STARthread_safe_contextSTAR->defparameter(llvmo::ThreadSafeContext_O::create_thread_safe_context());
    comp::_sym_STARthread_local_builtins_moduleSTAR->defparameter(nil<core::T_O>());
    FILE *null_out = fopen("/dev/null", "w");
    _lisp->_Roots._NullStream = core::IOStreamStream_O::makeIO("/dev/null", null_out);

  //
  // Setup the pathname info for wherever the executable was loaded
  //
//  printf("%s:%d:%s Calling setup_pathname_translations\n", __FILE__, __LINE__, __FUNCTION__);
    globals_->_Bundle->setup_pathname_translations();
    core::getcwd(true);         // set *default-pathname-defaults*
    {
      char* pause_startup = getenv("CLASP_PAUSE_INIT");
      if (pause_startup) {
#if 1
        gctools::wait_for_user_signal("Paused at startup before all initialization");
#else
        printf("%s:%d PID = %d Paused after snapshot-load - press enter to continue: \n", __FILE__, __LINE__, getpid() );
        fflush(stdout);
        getchar();
#endif
      }
    }
  }

  _lisp->parseCommandLineArguments(*core::global_options);

  int exitCode;
  try {
    if (ext::_sym_STARsnapshot_save_load_startupSTAR->symbolValue().notnilp()) {
      core::T_sp fn = ext::_sym_STARsnapshot_save_load_startupSTAR->symbolValue();
      core::eval::funcall(fn);
    } else {
      core::write_bf_stream(fmt::sprintf("Clasp (copyright Christian E. Schafmeister 2014)\n"));
      core::write_bf_stream(fmt::sprintf("ext:*snapshot-save-load-startup* is nil so dropping into a simple repl\n"));
      core::write_bf_stream(fmt::sprintf("Low level repl\n"));
      _lisp->readEvalPrintInteractive();
      core::write_bf_stream(fmt::sprintf("\n"));
    }
  } catch (core::ExitProgramException &ee) {
    exitCode = ee.getExitResult();
  }


  //
  // Clear out a few things
  //

  return exitCode;
}


};

#endif // USE_PRECISE_GC






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



