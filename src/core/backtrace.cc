#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/ql.h>
#include <clasp/core/array.h>
#include <clasp/core/pointer.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/functor.h> // function description stuff
#include <clasp/core/sourceFileInfo.h>
#include <clasp/llvmo/debugInfoExpose.h>
#include <clasp/llvmo/code.h>
#include <clasp/core/stackmap.h>
#include <clasp/core/backtrace.h>
#ifdef USE_LIBUNWIND
#define UNW_LOCAL_ONLY
#include <libunwind.h>
#else
#include <execinfo.h> // backtrace
#endif
#include <stdio.h> // debug messaging
#include <stdlib.h> // calloc, realloc, free

/*
 * This file contains the low-ish level code to get backtrace information.
 * The main entry point is core:call-with-frame, which collects this information
 * and passes it to a provided thunk. This makes more sense than a function
 * that simply _returns_ this information, since after all the control stack
 * inherently has only dynamic extent.
 * This information takes the form of objects called "frames"; see backtrace.h
 * for definitions. Hopefully should be simple to understand.
 * There are two versions - one using libunwind, and one using the "backtrace"
 * family of functions, which are not POSIX standard but roughly present on both
 * Linux and BSD derivatives (e.g. Mac). At the time of writing, libunwind
 * appears to be broken on some platformers in relation to JITted code.
 */

namespace core {

void DebuggerFrame_O::fields(Record_sp node) {
  node->field(INTERN_(kw,function_name),this->fname);
  node->field(INTERN_(kw,return_address),this->return_address);
  node->field(INTERN_(kw,source_position),this->source_position);
  node->field(INTERN_(kw,function_description),this->function_description);
  node->field(INTERN_(kw,closure),this->closure);
  node->field(INTERN_(kw,args),this->args);
  node->field(INTERN_(kw,lang),this->lang);
  node->field(INTERN_(kw,up),this->up);
  node->field(INTERN_(kw,down),this->down);
}

std::string DebuggerFrame_O::__repr__() const {
  stringstream ss;
  ss << "#<DEBUGGER-FRAME ";
  ss << _rep_(this->fname) << " ";
  ss << _rep_(this->return_address);
  ss << ">";
  return ss.str();
}


#ifdef USE_LIBUNWIND
static SimpleBaseString_sp lu_procname(unw_cursor_t* cursorp) {
  size_t nbytes = 64; // numbers chosen arbitrarily
  do {
    char fname[nbytes];
    unw_word_t ignore; // not sure if unw_get_proc_name accepts NULL, so
    switch (unw_get_proc_name(cursorp, fname, nbytes, &ignore)) {
    case 0: return SimpleBaseString_O::make(fname);
    case UNW_ENOMEM: break;
    case UNW_ENOINFO: return SimpleBaseString_O::make("<name not available>");
    default: return SimpleBaseString_O::make("<unknown libunwind error>");
    }
    if (nbytes >= 4096) {
      // Too long. Give up, putting an ellipsis on the end
      char fnamedot[nbytes+3];
      fnamedot[0] = '\0';
      strcat(fnamedot, fname);
      strcat(fnamedot, "...");
      return SimpleBaseString_O::make(fnamedot);
    } else nbytes <<= 1;
  } while (true);
}
#endif // USE_LIBUNWIND

static T_sp dwarf_spi(llvmo::DWARFContext_sp dcontext,
                      llvmo::SectionedAddress_sp sa) {
    // FIXME: Might be better to just use the llvm::DILineInfo directly.
  T_mv lineinfo = llvmo::getLineInfoForAddress( dcontext, sa, false );
  if (lineinfo.notnilp()) {
    SimpleBaseString_sp filename = gc::As<SimpleBaseString_sp>(lineinfo);
    Integer_sp line = gc::As<Integer_sp>(lineinfo.valueGet_(3));
    Integer_sp column = gc::As<Integer_sp>(lineinfo.valueGet_(4));
    return core__makeSourcePosInfo(filename->get_std_string(), true,
                                   0, true, // eck
                                 // See above FIXME; these should all be fine,
                                 // but then why go through Integer_O at all?
                                   line.unsafe_fixnum(), true,
                                   column.unsafe_fixnum(), true );
  } else return nil<T_O>();
}

static T_sp dwarf_ep(llvmo::ObjectFile_sp ofi,
                     llvmo::DWARFContext_sp dcontext,
                     llvmo::SectionedAddress_sp sa,
                     bool& XEPp) {
  auto eranges = llvmo::getAddressRangesForAddressInner(dcontext, sa);
  if (eranges) {
    auto ranges = eranges.get();
    llvmo::Code_sp code = ofi->_Code;
    uintptr_t code_start = code->codeStart();
    T_O** rliterals = code->TOLiteralsStart();
    size_t nliterals = code->TOLiteralsSize();
    for (size_t i = 0; i < nliterals; ++i) {
      T_sp literal((gc::Tagged)(rliterals[i]));
      if (gc::IsA<LocalEntryPoint_sp>(literal)) {
        LocalEntryPoint_sp ep = gc::As_unsafe<LocalEntryPoint_sp>(literal);
        uintptr_t ip = (uintptr_t)(ep->_EntryPoint) - code_start;
        for (auto range : ranges) {
          if ((range.LowPC <= ip) && (ip < range.HighPC))
            return ep;
        }
      } else if (gc::IsA<GlobalEntryPoint_sp>(literal)) {
        GlobalEntryPoint_sp ep = gc::As_unsafe<GlobalEntryPoint_sp>(literal);
        for (size_t j = 0; j < NUMBER_OF_ENTRY_POINTS; ++j) {
          uintptr_t ip = (uintptr_t)(ep->_EntryPoints[j]) - code_start;
          for (auto range : ranges) {
            if ((range.LowPC <= ip) && (ip < range.HighPC)) {
              XEPp = true;
              return ep;
            }
          }
        }
      }
    }
    // no hits
    return nil<T_O>();
  } else {
    // FIXME: signal error?
    return nil<T_O>();
  }
}

__attribute__((optnone))
static bool args_from_offset(void* frameptr, int32_t offset,
                             T_sp& closure, T_sp& args) {
  if (frameptr) {
    T_O** register_save_area = (T_O**)((uintptr_t)frameptr + offset);
    T_sp tclosure((gc::Tagged)register_save_area[LCC_CLOSURE_REGISTER]);
    if (!gc::IsA<Function_sp>(tclosure)) {
      fprintf(stderr, "%s:%d:%s When trying to get arguments from CL frame read what should be a closure %p but it isn't\n", __FILE__, __LINE__, __FUNCTION__, tclosure.raw_());
      return false;
    }
    closure = tclosure;
    size_t nargs = (size_t)(register_save_area[LCC_NARGS_REGISTER]);
    if (nargs>256) {
      fprintf(stderr, "%s:%d:%s  There are too many arguments %lu\n", __FILE__, __LINE__, __FUNCTION__, nargs);
      return false;
    }
    ql::list largs;
    // Get the first args from the register save area
    for (size_t i = 0; i < std::min(nargs, (size_t)LCC_ARGS_IN_REGISTERS);
         ++i) {
      T_sp temp((gctools::Tagged)(register_save_area[i+2]));
      largs << temp;
    }
    // and the rest from the stack frame
    for (size_t i = LCC_ARGS_IN_REGISTERS; i < nargs; ++i) {
      T_O* rarg = ((T_O**)frameptr)[i + 2 - LCC_ARGS_IN_REGISTERS];
      T_sp temp((gctools::Tagged)rarg);
      largs << temp;
    }
    args = largs.cons();
    return true;
  } else return false;
}

bool sanity_check_args(void* frameptr, int32_t offset) {
  if (frameptr) {
    T_O** register_save_area = (T_O**)((uintptr_t)frameptr + offset);
    T_sp tclosure((gc::Tagged)register_save_area[LCC_CLOSURE_REGISTER]);
    if (!gc::IsA<Function_sp>(tclosure)) {
      fprintf(stderr, "%s:%d:%s When trying to get arguments from CL frame read what should be a closure %p but it isn't\n", __FILE__, __LINE__, __FUNCTION__, tclosure.raw_());
      return false;
    }
    size_t nargs = (size_t)(register_save_area[LCC_NARGS_REGISTER]);
    if (nargs > 256) {
      fprintf(stderr, "%s:%d:%s  There are too many arguments %lu\n", __FILE__, __LINE__, __FUNCTION__, nargs);
      return false;
    }
    return true;
  } return true; // information not being available is unfortunate but sane
}

__attribute__((optnone))
static bool args_for_entry_point(llvmo::ObjectFile_sp ofi, T_sp ep,
                                 void* frameptr,
                                 T_sp& closure, T_sp& args) {
  if (ep.nilp()) return false;
  uintptr_t stackmap_start = (uintptr_t)(ofi->_Code->_StackmapStart);
  if (!stackmap_start) return false; // ends up as null sometimes apparently
  uintptr_t stackmap_end = stackmap_start + ofi->_Code->_StackmapSize;
  bool args_available = false;
  if (gc::IsA<LocalEntryPoint_sp>(ep)) {
    LocalEntryPoint_sp lep = gc::As_unsafe<LocalEntryPoint_sp>(ep);
    auto thunk = [&](size_t _, const smStkSizeRecord& function,
                     int32_t offsetOrSmallConstant) {
      if (function.FunctionAddress == (uintptr_t)(lep->_EntryPoint)) {
        if (args_from_offset(frameptr, offsetOrSmallConstant, closure, args))
          args_available |= true;
        return;
      }
    };
    walk_one_llvm_stackmap(thunk, stackmap_start, stackmap_end);
    return args_available;
  } else if (gc::IsA<GlobalEntryPoint_sp>(ep)) {
    GlobalEntryPoint_sp gep = gc::As_unsafe<GlobalEntryPoint_sp>(ep);
    auto thunk = [&](size_t _, const smStkSizeRecord& function,
                     int32_t offsetOrSmallConstant) {
      for (size_t j = 0; j < NUMBER_OF_ENTRY_POINTS; ++j) {
        if (function.FunctionAddress == (uintptr_t)(gep->_EntryPoints[j])) {
          if (args_from_offset(frameptr, offsetOrSmallConstant,
                               closure, args))
            args_available |= true;
          return;
        }
      }
    };
    walk_one_llvm_stackmap(thunk, stackmap_start, stackmap_end);
    return args_available;
  }
  // dwarf_ep returned something impossible; FIXME error?
  return false;
}

__attribute__((optnone))
static DebuggerFrame_sp make_lisp_frame(void* ip, llvmo::ObjectFile_sp ofi,
                                        void* fbp) {
  llvmo::SectionedAddress_sp sa = object_file_sectioned_address(ip, ofi, false);
  llvmo::DWARFContext_sp dcontext = llvmo::DWARFContext_O::createDwarfContext(ofi);
  T_sp spi = dwarf_spi(dcontext, sa);
  bool XEPp = false;
  T_sp ep = dwarf_ep(ofi, dcontext, sa, XEPp);
  T_sp fd = ep.notnilp() ? gc::As_unsafe<EntryPointBase_sp>(ep)->_FunctionDescription : nil<FunctionDescription_O>();
  T_sp closure = nil<T_O>(), args = nil<T_O>();
  bool args_available = args_for_entry_point(ofi, ep, fbp, closure, args);
  T_sp fname = nil<T_O>();
  if (fd.notnilp())
    fname = gc::As_unsafe<FunctionDescription_sp>(fd)->functionName();
  return DebuggerFrame_O::make(fname, Cons_O::create(sa,ofi), spi, fd, closure, args, args_available,
                               INTERN_(kw, lisp), XEPp);
}

bool maybe_demangle(const std::string& fnName, std::string& output)
{
  char *funcname = (char *)malloc(1024);
  size_t funcnamesize = 1024;
  int status;
  char *ret = abi::__cxa_demangle(fnName.c_str(), funcname, &funcnamesize, &status);
  if (status == 0) {
    std::string demangled(funcname);
    free(ret);
    output = demangled;
    return true;
  }
  if (fnName[0] == '_') {
      // try stripping off the first underscore
    std::string shortFnName = fnName.substr(1,std::string::npos);
    char *ret = abi::__cxa_demangle(shortFnName.c_str(), funcname, &funcnamesize, &status);
    if (status == 0) {
      std::string demangled(funcname);
      free(ret);
      output = demangled;
      return true;
    }
  }
  if (funcname) free(funcname);
  return false;
}

static DebuggerFrame_sp make_cxx_frame(void* ip, const char* cstring) {
#ifdef USE_LIBUNWIND
  std::string linkname(cstring);
#else // with os backtraces, we have to parse the function name out.
  std::string str(cstring);
  std::string linkname;
#if defined(_TARGET_OS_DARWIN)
  // The string is divided into:
  // index executable address name plus offset
  // but we only care about the name.
  std::vector<std::string> parts = split(str, " ");
  if (parts.size()>3) {
    linkname = parts[3];
  } else {
    linkname = str;
  }
#elif defined(_TARGET_OS_LINUX)
    /* Some examples of what backtrace_symbols(...) returns on Linux...
"/home/meister/Development/cando-main/build/boehm_d/iclasp-boehm-d(_ZN4core6Lisp24readEvalPrintInteractiveEv+0x5b) [0x4989f2b]"
"/home/meister/Development/cando-main/build/boehm_d/iclasp-boehm-d(_ZN4core6Lisp3runEv+0xb85) [0x498e5c5]"
"/home/meister/Development/cando-main/build/boehm_d/iclasp-boehm-d() [0x47235db]"
*/
  size_t nameStart = str.find('(');
  size_t nameEnd = str.rfind(')');
  if ((nameEnd-nameStart)>1) {
    linkname = str.substr(nameStart+1,nameEnd);
    size_t pluspos = linkname.rfind('+');
    if (pluspos!=std::string::npos) {
      linkname = linkname.substr(0,pluspos); // remove +0x###
    }
  } else {
    linkname = str;
  }
#else
    // I don't know what other OS's backtrace_symbols return - punt
  linkname = str;
#endif
#endif // USE_LIBUNWIND
  std::string name;
  if (!(maybe_demangle(linkname, name)))
    // couldn't demangle, so just use the unadulterated string
    name = linkname;
  T_sp lname = SimpleBaseString_O::make(name);
  return DebuggerFrame_O::make(lname, Pointer_O::create(ip),
                               nil<T_O>(), nil<T_O>(), nil<T_O>(),
                               nil<T_O>(), false, INTERN_(kw, c_PLUS__PLUS_),
                               false);
}

__attribute__((optnone))
static DebuggerFrame_sp make_frame(void* ip, const char* string, void* fbp, bool firstFrame) {
  if (!firstFrame) {
    ip = (void*)((uintptr_t)ip-1); // For everything but the first frame subtract 1
  }
  T_sp of = llvmo::only_object_file_for_instruction_pointer(ip);
  if (of.nilp()) return make_cxx_frame(ip, string);
  else return make_lisp_frame(ip, gc::As_unsafe<llvmo::ObjectFile_sp>(of), fbp);
}

static bool sanity_check_frame(void* ip, void* fbp) {
  T_sp of = llvmo::only_object_file_for_instruction_pointer(ip);
  if (of.nilp()) return true; // C++ frames are always fine for our purposes
  else if (gc::IsA<llvmo::ObjectFile_sp>(of)) {
    llvmo::ObjectFile_sp ofi = gc::As_unsafe<llvmo::ObjectFile_sp>(of);
    llvmo::SectionedAddress_sp sa = object_file_sectioned_address(ip, ofi, false);
    llvmo::DWARFContext_sp dcontext = llvmo::DWARFContext_O::createDwarfContext(ofi);
    bool XEPp = false;
    T_sp ep = dwarf_ep(ofi, dcontext, sa, XEPp);
    uintptr_t stackmap_start = (uintptr_t)(ofi->_Code->_StackmapStart);
    uintptr_t stackmap_end = stackmap_start + ofi->_Code->_StackmapSize;
    if (gc::IsA<LocalEntryPoint_sp>(ep)) {
      LocalEntryPoint_sp lep = gc::As_unsafe<LocalEntryPoint_sp>(ep);
      bool result = true;
      auto thunk = [&](size_t _, const smStkSizeRecord& function,
                       int32_t offsetOrSmallConstant) {
        if (function.FunctionAddress == (uintptr_t)(lep->_EntryPoint)) {
          if (!sanity_check_args(fbp, offsetOrSmallConstant)) result = false;
          return;
        }
      };
      walk_one_llvm_stackmap(thunk, stackmap_start, stackmap_end);
      return result;
    } else if (gc::IsA<GlobalEntryPoint_sp>(ep)) {
      GlobalEntryPoint_sp gep = gc::As_unsafe<GlobalEntryPoint_sp>(ep);
      bool result = true;
      auto thunk = [&](size_t _, const smStkSizeRecord& function,
                       int32_t offsetOrSmallConstant) {
        for (size_t j = 0; j < NUMBER_OF_ENTRY_POINTS; ++j) {
          if (function.FunctionAddress == (uintptr_t)(gep->_EntryPoints[j])) {
            if (!sanity_check_args(fbp, offsetOrSmallConstant)) result = false;
            return;
          }
        }
      };
      walk_one_llvm_stackmap(thunk, stackmap_start, stackmap_end);
      return result;
    } else if (ep.nilp()) return true; // annoying but sane
    else {
      fprintf(stderr, "%s:%d:%s  dwarf_fp returned object of wrong type\n",
              __FILE__, __LINE__, __FUNCTION__);
      return false;
    }
  } else {
    fprintf(stderr, "%s:%d:%s  only_object_file_for_instruction_pointer returned object of wrong type\n",
            __FILE__, __LINE__, __FUNCTION__);
    return false;
  }
}

#ifdef USE_LIBUNWIND
__attribute__((optnone))
static T_mv lu_call_with_frame(std::function<T_mv(DebuggerFrame_sp)> f) {
  unw_cursor_t cursor;
  unw_word_t ip, fbp;
  unw_context_t uc;

  unw_getcontext(&uc);
  unw_init_local(&cursor, &uc);

  int resip = unw_get_reg(&cursor, UNW_REG_IP, &ip);
  int resbp = unw_get_reg(&cursor, UNW_X86_64_RBP, &fbp);
  if (resip || resbp) {
    fprintf(stderr, "%s:%d:%s  unw_get_reg resip=%d ip = %p  resbp=%d rbp = %p\n", __FILE__, __LINE__, __FUNCTION__, resip, (void*)ip, resbp, (void*)fbp);
  }
  // This is slightly inefficient in that we allocate a simple base string
  // only to get a C string from it, but writing it to use stack allocation
  // is a pain in the ass for very little gain.
  std::string sstring = lu_procname(&cursor)->get_std_string();
  DebuggerFrame_sp bot = make_frame((void*)ip, sstring.c_str(), (void*)fbp, true );
  DebuggerFrame_sp prev = bot;
  while (unw_step(&cursor) > 0) {
    resip = unw_get_reg(&cursor, UNW_REG_IP, &ip);
    resbp = unw_get_reg(&cursor, UNW_X86_64_RBP, &fbp);
    if (resip || resbp) {
      printf("%s:%d:%s  unw_get_reg resip=%d ip = %p  resbp=%d rbp = %p\n", __FILE__, __LINE__, __FUNCTION__, resip, (void*)ip, resbp, (void*)fbp);
    }
    std::string sstring = lu_procname(&cursor)->get_std_string();
    DebuggerFrame_sp frame = make_frame((void*)ip, sstring.c_str(), (void*)fbp, false );
    frame->down = prev;
    prev->up = frame;
    prev = frame;
  }
  return f(bot);
}
static bool lu_sanity_check_backtrace() {
  unw_cursor_t cursor;
  unw_word_t ip, fbp;
  unw_context_t uc;
  unw_getcontext(&uc);
  unw_init_local(&cursor, &uc);
  do {
    int resip = unw_get_reg(&cursor, UNW_REG_IP, &ip);
    int resbp = unw_get_reg(&cursor, UNW_X86_64_RBP, &fbp);
    if (resip || resbp) {
      fprintf(stderr, "%s:%d:%s  unw_get_reg resip=%d ip = %p  resbp=%d rbp = %p\n", __FILE__, __LINE__, __FUNCTION__, resip, (void*)ip, resbp, (void*)fbp);
      return false;
    }
    if (!(sanity_check_frame((void*)ip, (void*)fbp))) return false;
    int resstep = unw_step(&cursor);
    if (resstep == 0) return true;
    else if (resstep < 0) { // error
      fprintf(stderr, "%s:%d:%s  unw_step returned error %d",
              __FILE__, __LINE__, __FUNCTION__, resstep);
      return false;
    }
  } while (true);
}

#else // non-libunwind version
__attribute__((optnone))
static T_mv os_call_with_frame(std::function<T_mv(DebuggerFrame_sp)> f) {
#define START_BACKTRACE_SIZE 512
#define MAX_BACKTRACE_SIZE_LOG2 20
  size_t num = START_BACKTRACE_SIZE;
  void** buffer = (void**)calloc(sizeof(void*), num);
  for (size_t attempt = 0; attempt < MAX_BACKTRACE_SIZE_LOG2; ++attempt) {
    size_t returned = backtrace(buffer,num);
    if (returned < num) {
      char **strings = backtrace_symbols(buffer, returned);
      void* fbp = __builtin_frame_address(0); // TODO later
      uintptr_t bplow = (uintptr_t)&fbp;
      uintptr_t bphigh = (uintptr_t)my_thread_low_level->_StackTop;
      DebuggerFrame_sp bot = make_frame(buffer[0], strings[0], fbp, true);
      DebuggerFrame_sp prev = bot;
      void* newfbp;
      for (size_t j = 1; j < returned; ++j) {
        if (fbp) newfbp = *(void**)fbp;
        if (newfbp && !(bplow<(uintptr_t)newfbp && (uintptr_t)newfbp<bphigh)) {  // newfbp is out of the stack.
//          fprintf(stderr, "%s:%d:%s The frame pointer walk went out of bounds bplow is %p bphigh is %p and newfbp is %p\n",
//                 __FILE__, __LINE__, __FUNCTION__, (void*)bplow, (void*)bphigh, newfbp );
          newfbp = NULL;
        } else if (newfbp && !((uintptr_t)fbp < (uintptr_t)newfbp) ) {             // fbp < newfbp
          fprintf(stderr, "%s:%d:%s The frame pointer is not monotonically increasing fbp is %p and newfbp is %p\n",
                 __FILE__, __LINE__, __FUNCTION__, fbp, newfbp );
          newfbp = NULL;
        }
        fbp = newfbp;
        DebuggerFrame_sp frame = make_frame(buffer[j], strings[j], fbp, false);
        frame->down = prev;
        prev->up = frame;
        prev = frame;
      }
      free(buffer);
      free(strings);
      return f(bot);
    }
    // realloc_array would be nice, but macs don't have it
    num *= 2;
    buffer = (void**)realloc(buffer, sizeof(void*)*num);
  }
  fprintf(stderr, "%s:%d Couldn't get backtrace\n", __FILE__, __LINE__ );
  abort();
}
static bool os_sanity_check_backtrace() {
  uintptr_t stacktop = (uintptr_t)my_thread_low_level->_StackTop;
  size_t num = START_BACKTRACE_SIZE;
  void** buffer = (void**)calloc(sizeof(void*), num);
  for (size_t attempt = 0; attempt < MAX_BACKTRACE_SIZE_LOG2; ++attempt) {
    size_t returned = backtrace(buffer, num);
    if (returned < num) {
      void* fbp = __builtin_frame_address(0);
      uintptr_t stackbot = (uintptr_t)&fbp;
      for (size_t j = 0; j < returned; ++j) {
        /*
        if (fbp && !((stackbot <= (uintptr_t)fbp) && ((uintptr_t)fbp <= stacktop))) {
          fprintf(stderr, "%s:%d:%s In backtrace sanity check, the frame pointer went out of bounds. Stack low is %p stack high is %p frame pointer is %p\n",
                  __FILE__, __LINE__, __FUNCTION__,
                  (char*)stackbot, (char*)stacktop, fbp);
          return false;
        }
        */
        if (!sanity_check_frame(buffer[j], fbp)) return false;
        void* nfbp = *(void**)fbp;
        if (nfbp && !((uintptr_t)fbp < (uintptr_t)nfbp)) {
          fprintf(stderr, "%s:%d:%s In backtrace sanity check, the frame pointer is not monotonically increasing from %p to %p\n",
                  __FILE__, __LINE__, __FUNCTION__, fbp, nfbp);
          return false;
        }
        fbp = nfbp;
      }
      free(buffer);
      return true;
    } else {
      num *= 2;
      buffer = (void**)realloc(buffer, sizeof(void*)*num);
    }
  }
  fprintf(stderr, "%s:%d:%s  too many frames\n",
          __FILE__, __LINE__, __FUNCTION__);
  return false;
}
#endif // USE_LIBUNWIND

T_mv call_with_frame(std::function<T_mv(DebuggerFrame_sp)> f) {
#ifdef USE_LIBUNWIND
  return lu_call_with_frame(f);
#else
  return os_call_with_frame(f);
#endif
}
CL_DEFUN bool core__sanity_check_backtrace() {
#ifdef USE_LIBUNWIND
  return lu_sanity_check_backtrace();
#else
  return os_sanity_check_backtrace();
#endif
}

__attribute__((optnone))
CL_DEFUN T_mv core__call_with_frame(Function_sp function) {
  auto th = [&](DebuggerFrame_sp bot){ return eval::funcall(function, bot); };
  return call_with_frame(th);
}

CL_DEFUN T_sp core__debugger_frame_fname(DebuggerFrame_sp df) {
  return df->fname;
}
CL_DEFUN T_sp core__debugger_frame_source_position(DebuggerFrame_sp df) {
  return df->source_position;
}
CL_DEFUN T_sp core__debugger_frame_function_description(DebuggerFrame_sp df) {
  return df->function_description;
}
CL_DEFUN T_sp core__debugger_frame_lang(DebuggerFrame_sp df) {
  return df->lang;
}
CL_DEFUN T_sp core__debugger_frame_closure(DebuggerFrame_sp df) {
  return df->closure;
}
CL_DEFUN T_sp core__debugger_frame_args(DebuggerFrame_sp df) {
  return df->args;
}
CL_DEFUN bool core__debugger_frame_args_available_p(DebuggerFrame_sp df) {
  return df->args_available;
}
CL_DEFUN bool core__debugger_frame_xep_p(DebuggerFrame_sp df) {
  return df->is_xep;
}
CL_DEFUN T_sp core__debugger_frame_up(DebuggerFrame_sp df) {
  return df->up;
}
CL_DEFUN T_sp core__debugger_frame_down(DebuggerFrame_sp df) {
  return df->down;
}

CL_DEFUN T_sp core__debugger_local_fname(DebuggerLocal_sp dl) {
  return dl->fname;
}
CL_DEFUN T_sp core__debugger_local_name(DebuggerLocal_sp dl) {
  return dl->name;
}
CL_DEFUN T_sp core__debugger_local_declfile(DebuggerLocal_sp dl) {
  return dl->declfile;
}
CL_DEFUN T_sp core__debugger_local_declline(DebuggerLocal_sp dl) {
  return dl->declline;
}

}; // namespace core
