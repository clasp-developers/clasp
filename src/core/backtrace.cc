#include <unistd.h>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/ql.h>
#include <clasp/core/array.h>
#include <clasp/core/pointer.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/function.h> // function description stuff
#include <clasp/core/bytecode.h> // for bytecode frames
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
#include <stdio.h>  // debug messaging
#include <stdlib.h> // calloc, realloc, free
#include <regex>

// #define DEBUG_BACKTRACE 1
#ifdef DEBUG_BACKTRACE
#define D(x)                                                                                                                       \
  do {                                                                                                                             \
    x                                                                                                                              \
  } while (0)
#else
#define D(x)                                                                                                                       \
  do {                                                                                                                             \
  } while (0)
#endif

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

size_t global_MaybeTraceDepth = 0;
struct MaybeTrace {
  string _Msg;
  size_t _depth;
#ifdef DEBUG_BACKTRACE
  MaybeTrace(const string& msg) : _Msg(msg), _depth(global_MaybeTraceDepth++) {
    printf("%s%2lu> %s\n", this->spaces().c_str(), this->_depth, msg.c_str());
  };
  ~MaybeTrace() {
    printf("%s<%2lu %s\n", this->spaces().c_str(), this->_depth, this->_Msg.c_str());
    global_MaybeTraceDepth--;
  }
  std::string spaces(size_t depth = global_MaybeTraceDepth) const {
    std::string spcs = "|||||||||||||||||||||||||||||||||||||||";
    if (this->_depth < spcs.size()) {
      return spcs.substr(0, this->_depth);
    }
    return spcs;
  }

#else
  MaybeTrace(const string& msg){};
#endif
};

namespace core {

CL_DEFMETHOD T_sp DebuggerFrame_O::returnAddress() const { return this->return_address; }

void DebuggerFrame_O::fields(Record_sp node) {
  node->field(INTERN_(kw, function_name), this->fname);
  node->field(INTERN_(kw, return_address), this->return_address);
  node->field(INTERN_(kw, source_position), this->source_position);
  node->field(INTERN_(kw, function_description), this->function_description);
  node->field(INTERN_(kw, closure), this->closure);
  node->field(INTERN_(kw, args), this->args);
  node->field(INTERN_(kw, lang), this->lang);
  node->field(INTERN_(kw, up), this->up);
  node->field(INTERN_(kw, down), this->down);
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
    case 0:
      return SimpleBaseString_O::make(fname);
    case UNW_ENOMEM:
      break;
    case UNW_ENOINFO:
      return SimpleBaseString_O::make("<name not available>");
    default:
      return SimpleBaseString_O::make("<unknown libunwind error>");
    }
    if (nbytes >= 4096) {
      // Too long. Give up, putting an ellipsis on the end
      char fnamedot[nbytes + 3];
      fnamedot[0] = '\0';
      strcat(fnamedot, fname);
      strcat(fnamedot, "...");
      return SimpleBaseString_O::make(fnamedot);
    } else
      nbytes <<= 1;
  } while (true);
}
#endif // USE_LIBUNWIND

static T_sp getSourcePosInfoForAddress(llvmo::DWARFContext_sp dcontext, llvmo::SectionedAddress_sp sa) {
  static std::regex logical_pathname_regex("LOGICAL-PATHNAME=([:;.\\-[:alnum:]]+)");
  llvm::DILineInfoSpecifier lispec;
  lispec.FNKind = llvm::DILineInfoSpecifier::FunctionNameKind::None;
  lispec.FLIKind = llvm::DILineInfoSpecifier::FileLineInfoKind::AbsoluteFilePath;
  llvm::DILineInfo info = dcontext->wrappedPtr()->getLineInfoForAddress(sa->_value, lispec);

  if (info.FileName == info.BadString) {
    return nil<T_O>();
  }

  std::string source_path = info.FileName;
#if LLVM_VERSION_MAJOR < 16
  if (info.Source.hasValue()) {
    std::smatch match;
    std::string source = info.Source.getValue().str();
    if (std::regex_search(source, match, logical_pathname_regex)) {
      source_path = match[1];
    }
  }
#else
  if (info.Source.has_value()) {
    std::smatch match;
    std::string source = info.Source.value().str();
    if (std::regex_search(source, match, logical_pathname_regex)) {
      source_path = match[1];
    }
  }
#endif

  return core__makeSourcePosInfo(source_path, true, 0, false, info.Line, true, info.Column, true);
}

T_sp dwarf_ep(size_t frameIndex, llvmo::ObjectFile_sp ofi, llvmo::DWARFContext_sp dcontext, llvmo::SectionedAddress_sp sa,
              void*& codeStart, void*& functionStartAddress, bool& XEPp, int& arityCode) {
  MaybeTrace trace(__FUNCTION__);
  functionStartAddress = NULL;
  D(printf("%s:%d:%s frameIndex = %lu\n", __FILE__, __LINE__, __FUNCTION__, frameIndex););
  //
  // If the object file contains the interpreter_trampoline - then we are in the interpreter
  //
  if (ofi->codeStart() <= (uintptr_t)bytecode_trampoline && (uintptr_t)bytecode_trampoline < ofi->codeEnd()) {
    functionStartAddress = (void*)bytecode_trampoline;
    D(printf("%s:%d:%s bytecode trampoline functionStartAddress = %p\n", __FILE__, __LINE__, __FUNCTION__, functionStartAddress););
    return nil<T_O>();
  }
  auto expected_ranges = llvmo::getAddressRangesForAddressInner(dcontext, sa);
  if (expected_ranges) {
    auto ranges = expected_ranges.get();
    if (ranges.size() == 0) {
      D(printf("%s:%d:%s No ranges were found for %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(sa).c_str()););
    } else {
      if (ranges.size() > 1) {
        printf("%s:%d:%s There is more than one range - there are %lu\n", __FILE__, __LINE__, __FUNCTION__, ranges.size());
      } else {
        llvmo::ObjectFile_sp code = ofi;
        codeStart = (void*)code->codeStart();
        uintptr_t absolute_LowPC = ranges.begin()->LowPC + (uintptr_t)codeStart;
        functionStartAddress = (void*)(absolute_LowPC);
        D(printf("%s:%d:%s Calculated functionStartAddress = %p\n", __FILE__, __LINE__, __FUNCTION__, functionStartAddress););
        T_O** rliterals = code->TOLiteralsStart();
        size_t nliterals = code->TOLiteralsSize();
        D(printf("%s%s:%d:%s sectioned address %s\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__,
                 _rep_(sa).c_str()););
        D(printf("%s%s:%d:%s codeStart = %p - %p\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, (void*)codeStart,
                 (void*)codeEnd););
        D(printf("%s%s:%d:%s objectFile = %s\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, _rep_(ofi).c_str()););
        D(for (auto range
               : ranges) {
          uintptr_t absolute_LowPC = range.LowPC + (uintptr_t)codeStart;
          uintptr_t absolute_HighPC = range.HighPC + (uintptr_t)codeStart;
          printf("%s%s:%d:%s found range %p - %p  Absolute %p - %p\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__,
                 (void*)range.LowPC, (void*)range.HighPC, (void*)absolute_LowPC, (void*)absolute_HighPC);
        } printf("%s%s:%d:%s rliterals = %p nliterals = %lu\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, rliterals,
                 nliterals););
        for (size_t i = 0; i < nliterals; ++i) {
          T_sp literal((gc::Tagged)(rliterals[i]));
          if (gc::IsA<CoreFun_sp>(literal)) {
            CoreFun_sp ep = gc::As_unsafe<CoreFun_sp>(literal);
            uintptr_t absolute_entry = (uintptr_t)(ep->_Entry);
            D(printf("%s%s:%d:%s CoreFun_sp %s  absolute_entry = %p   FunctionDescription name %s\n", trace.spaces().c_str(),
                     __FILE__, __LINE__, __FUNCTION__, _rep_(ep).c_str(), (void*)absolute_entry,
                     _rep_(ep->functionDescription()).c_str()););
            for (auto range : ranges) {
              uintptr_t absolute_LowPC = range.LowPC + (uintptr_t)codeStart;
              uintptr_t absolute_HighPC = range.HighPC + (uintptr_t)codeStart;
              if ((absolute_LowPC <= absolute_entry) && (absolute_entry < absolute_HighPC)) {
                D(printf("%s%s:%d:%s Matched absolute_LowPC/absolute_HighPC %p/%p\n", trace.spaces().c_str(), __FILE__, __LINE__,
                         __FUNCTION__, (void*)absolute_LowPC, (void*)absolute_HighPC););
                XEPp = false;
                // This will be identical to the entry point address in CoreFun_sp ep
                return ep;
              } else {
                D(printf("%s%s:%d:%s DID NOT match absolute_LowPC/absolute_HighPC %p/%p\n", trace.spaces().c_str(), __FILE__,
                         __LINE__, __FUNCTION__, (void*)absolute_LowPC, (void*)absolute_HighPC););
              }
            }
          } else if (gc::IsA<SimpleFun_sp>(literal)) {
            SimpleFun_sp ep = gc::As_unsafe<SimpleFun_sp>(literal);
            D(printf("%s%s:%d:%s SimpleCoreFun_sp %s  FunctionDescription name %s\n", trace.spaces().c_str(), __FILE__, __LINE__,
                     __FUNCTION__, _rep_(ep).c_str(), _rep_(ep->functionDescription()).c_str()););
            for (size_t j = 0; j < NUMBER_OF_ENTRY_POINTS; ++j) {
              uintptr_t absolute_entry = (uintptr_t)(ep->_EntryPoints[j]);
              for (auto range : ranges) {
                uintptr_t absolute_LowPC = range.LowPC + (uintptr_t)codeStart;
                uintptr_t absolute_HighPC = range.HighPC + (uintptr_t)codeStart;
                if ((absolute_LowPC <= absolute_entry) && (absolute_entry < absolute_HighPC)) {
                  D(printf("%s%s:%d:%s Matched arityCode: %lu absolute_LowPC/absolute_HighPC %p/%p\n", trace.spaces().c_str(),
                           __FILE__, __LINE__, __FUNCTION__, j, (void*)absolute_LowPC, (void*)absolute_HighPC););
                  XEPp = true;
                  // This will be identical to ONE of the the entry point address in SimpleFun_sp ep
                  // arityCode is the index into the SimpleFun_sp vector corresponding to functionStartAddress
                  arityCode = j;
                  return ep;
                } else {
                  // D(printf("%s%s:%d:%s absolute_entry -> %p DID NOT match absolute_LowPC/absolute_HighPC %p/%p\n",
                  // trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, (void*)absolute_entry, (void*)absolute_LowPC,
                  // (void*)absolute_HighPC ););
                }
              }
            }
          }
        }
        // no hits
        return nil<T_O>();
      }
    }
  }
  D(printf("%s:%d:%s No eranges\n", __FILE__, __LINE__, __FUNCTION__););
  return nil<T_O>();
}

__attribute__((optnone)) static bool args_from_offset(size_t fi, void* ip, const char* string, T_sp name, bool XEPp, int arityCode,
                                                      void* frameptr, int32_t offset32, T_sp& closure, T_sp& args,
                                                      int64_t patch_point_id) {
  MaybeTrace trace(__FUNCTION__);
  int64_t offset64 = static_cast<int64_t>(offset32);
  D(printf("%s%s:%d:%s fi=%lu ip=%p fp = %p patch_point_id 0x%lx  offset64 = %ld\n", trace.spaces().c_str(), __FILE__, __LINE__,
           __FUNCTION__, fi, ip, frameptr, patch_point_id, offset64););
  int64_t arity_code;
  if (frameptr && is_entry_point_arity(patch_point_id, arity_code)) {
    D(printf("%s%s:%d:%s entry_point arity_code %ld\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, arity_code););
    T_O** register_save_area = (T_O**)((intptr_t)frameptr + offset64);
    D(printf("%s%s:%d:%s fi=%lu ip=%p fp %p arity_code %ld rsa=%p %s XEP(y=%d,a=%d) ", trace.spaces().c_str(), __FILE__, __LINE__,
             __FUNCTION__, fi, ip, frameptr, arity_code, register_save_area, _rep_(name).c_str(), XEPp, arityCode););
    D(if (string) printf("%s", string););
    D(printf("\n"););
    T_sp tclosure((gc::Tagged)register_save_area[LCC_CLOSURE_REGISTER]);
    if (!gc::IsA<Function_sp>(tclosure)) {
      D(printf("%s%s:%d:%s bad tclosure %p\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, tclosure.raw_()););
      fprintf(stderr, "%s:%d:%s When trying to get arguments from CL frame read what should be a closure %p but it isn't\n",
              __FILE__, __LINE__, __FUNCTION__, tclosure.raw_());
      return false;
    }
    closure = tclosure;
    D(printf("%s%s:%d:%s closure = %s\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, _rep_(closure).c_str()););
    if (arity_code == 0) {
      // For the general entry point the registers are (closure, nargs, arg_ptr)
      size_t nargs = (size_t)(register_save_area[LCC_NARGS_REGISTER]);
      D(printf("%s%s:%d:%s nargs = %lu\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, nargs););
      if (nargs > 256) {
        fprintf(stderr, "%s:%d:%s  There are too many arguments %lu\n", __FILE__, __LINE__, __FUNCTION__, nargs);
        return false;
      }
      ql::list largs;
      // Get the arg ptr from the register save area
      T_O** arg_ptr = (T_O**)register_save_area[LCC_ARGS_PTR_REGISTER];
      // get the args from the arg_ptr
      D(printf("%s%s:%d:%s About to read %lu xep args\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, nargs););
      for (size_t i = 0; i < nargs; ++i) {
        T_O* rarg = arg_ptr[i];
        T_sp temp((gctools::Tagged)rarg);
        D(printf("%s%s:%d:%s     read xep(general) arg %lu -> %s\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, i,
                 _rep_(temp).c_str()););
        largs << temp;
      }
      args = largs.cons();
    } else {
      // for fixed arity entry points calculate the nargs from the arity_code
      size_t arity_nargs = arity_code - 1;
      ASSERT(ENTRY_POINT_ARITY_BEGIN == 0); // maybe in the future we may want to support something else
      size_t nargs = arity_nargs + ENTRY_POINT_ARITY_BEGIN;
      D(printf("%s%s:%d:%s About to read %lu xep%lu args\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, nargs,
               arity_code - 1););
      // Get the first args from the register save area
      ql::list largs;
      size_t args_in_rsa = std::min(nargs, (size_t)(LCC_WORDS_IN_REGISTER_SAVE_AREA - 1)); // -1 to remove closure arg
      int args_on_stack = nargs - (LCC_WORDS_IN_REGISTER_SAVE_AREA - 1);
      for (size_t i = 0; i < args_in_rsa; ++i) {
        T_sp temp((gctools::Tagged)(register_save_area[i + 1])); // +1 to skip closure arg
        D(printf("%s%s:%d:%s     read xep%lu arg %lu -> %s\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, nargs, i,
                 _rep_(temp).c_str()););
        largs << temp;
      }
      // and the rest from the stack frame if we support xepN functions that exhaust the available register arguments
      if (args_on_stack > 0) {
        printf("%s:%d:%s Check if extraction of arguments from the stack works properly\n", __FILE__, __LINE__, __FUNCTION__);
        for (size_t i = 0; i < args_on_stack; ++i) {
          T_O* rarg = ((T_O**)frameptr)[i + 2]; // +2 skips to where args are
          T_sp temp((gctools::Tagged)rarg);
          if (((uintptr_t)temp.raw_() & UNBOUND_TAG) == UNBOUND_TAG) {
            printf("%s:%d:%s And UNBOUND special value %p is leaking into a backtrace\n", __FILE__, __LINE__, __FUNCTION__,
                   temp.raw_());
          }
          largs << temp;
        }
      }
      args = largs.cons();
    }
    return true;
  } else {
    D(printf("%s%s:%d:%s entry_point no stackmap entry\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__););
    return false;
  }
  D(printf("%s%s:%d:%s Extracted args: %s\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, _rep_(args).c_str()););
}

bool sanity_check_args(void* frameptr, int32_t offset32, int64_t patch_point_id) {
  MaybeTrace trace(__FUNCTION__);
  int64_t offset = static_cast<int64_t>(offset32);
  if (frameptr) {
    T_O** register_save_area = (T_O**)((intptr_t)frameptr + offset);
    T_sp tclosure((gc::Tagged)register_save_area[LCC_CLOSURE_REGISTER]);
    if (!gc::IsA<Function_sp>(tclosure)) {
      fprintf(stderr, "%s:%d:%s When trying to get arguments from CL frame read what should be a closure %p but it isn't\n",
              __FILE__, __LINE__, __FUNCTION__, tclosure.raw_());
      return false;
    }
    int64_t arity_code;
    if (is_entry_point_arity(patch_point_id, arity_code)) {
      if (arity_code == 0) { // general entry point arity is the only one with nargs
        size_t nargs = (size_t)(register_save_area[LCC_NARGS_REGISTER]);
        if (nargs > 256) {
          fprintf(stderr, "%s:%d:%s  There are too many arguments %lu\n", __FILE__, __LINE__, __FUNCTION__, nargs);
          return false;
        }
      }
      // check if first argument is a tagged pointer (must be closure)
      core::T_O* closure = (core::T_O*)register_save_area[LCC_CLOSURE_REGISTER];
      T_sp tclosure((gctools::Tagged)closure);
      if (!gc::IsA<Function_sp>(tclosure)) {
        fprintf(stderr, "%s:%d:%s  The first entry in register_save_area %p is not a closure\n", __FILE__, __LINE__, __FUNCTION__,
                closure);
        return false;
      }
    }
    return true;
  }
  return true; // information not being available is unfortunate but sane
}

__attribute__((optnone)) static bool args_for_function(size_t fi, void* ip, const char* string, bool XEPp, int arityCode,
                                                       void* code_start, void* functionStartAddress, llvmo::ObjectFile_sp ofi,
                                                       T_sp& functionDescriptionOrNil, void* frameptr, T_sp& closure, T_sp& args) {
  MaybeTrace trace(__FUNCTION__);
  if (!functionStartAddress) {
    D(printf("%s:%d:%s functionStartAddress is NULL returning\n", __FILE__, __LINE__, __FUNCTION__););
    return false;
  }
  uintptr_t stackmap_start = (uintptr_t)(ofi->_StackmapStart);
  if (!stackmap_start) {
    D(printf("%s:%d:%s stackmap_start is NULL returning\n", __FILE__, __LINE__, __FUNCTION__););
    return false; // ends up as null sometimes apparently
  }
  uintptr_t stackmap_end = stackmap_start + ofi->_StackmapSize;
  bool args_available = false;
  T_sp name = nil<T_O>();
  ;
  if (gc::IsA<FunctionDescription_sp>(functionDescriptionOrNil))
    name = gc::As_unsafe<FunctionDescription_sp>(functionDescriptionOrNil);
  D(printf("%s%s:%d:%s functionStartAddress FunctionDescription %s  code_start = %p  functionStartAddress = %p\n",
           trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, _rep_(name).c_str(), (void*)code_start,
           functionStartAddress););
  auto thunk = [&](size_t _, const smStkSizeRecord& function, int32_t offsetOrSmallConstant, int64_t patchPointId) {
    if (function.FunctionAddress == (uintptr_t)functionStartAddress) {
      MaybeTrace tracel("functionStartAddress_thunk");
      D(printf("%s%s:%d:%s function.FunctionAddress = %p  functionStartAddress = %p\n", trace.spaces().c_str(), __FILE__, __LINE__,
               __FUNCTION__, (void*)function.FunctionAddress, functionStartAddress););
      if (args_from_offset(fi, ip, string, name, XEPp, arityCode, frameptr, offsetOrSmallConstant, closure, args, patchPointId))
        args_available |= true;
      return;
    }
  };
  {
    MaybeTrace tracew("walk_one_llvm_stackmap");
    walk_one_llvm_stackmap(thunk, stackmap_start, stackmap_end);
  }
  D(printf("%s%s:%d:%s args_available = %d\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, args_available););
  return args_available;
}

static DebuggerFrame_sp make_lisp_frame(size_t frameIndex, void* absolute_ip, const char* string, llvmo::ObjectFile_sp ofi,
                                        void* fbp) {
  MaybeTrace trace(__FUNCTION__);
  llvmo::SectionedAddress_sp sa = object_file_sectioned_address(absolute_ip, ofi, false);
  llvmo::DWARFContext_sp dcontext = llvmo::DWARFContext_O::createDWARFContext(ofi);
  D(printf("%s%s:%d:%s sa= %s\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, _rep_(sa).c_str()););
  T_sp spi = getSourcePosInfoForAddress(dcontext, sa);
  D(printf("%s%s:%d:%s spi= %s\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, _rep_(spi).c_str()););
  bool XEPp = false;
  int arityCode;
  void* codeStart;
  void* functionStartAddress;
  T_sp ep = dwarf_ep(frameIndex, ofi, dcontext, sa, codeStart, functionStartAddress, XEPp, arityCode);
  D(printf("%s:%d:%s dwarf_ep returned ep = %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(ep).c_str()););
  D(printf("%s:%d:%s dwarf_ep returned functionStartAddress = %p\n", __FILE__, __LINE__, __FUNCTION__,
           (void*)functionStartAddress););
  T_sp functionDescriptionOrNil = nil<T_O>();
  if (gc::IsA<CoreFun_sp>(ep))
    functionDescriptionOrNil = gc::As_unsafe<CoreFun_sp>(ep)->functionDescription();
  else if (gc::IsA<SimpleFun_sp>(ep))
    functionDescriptionOrNil = gc::As_unsafe<SimpleFun_sp>(ep)->functionDescription();
  D(printf("%s:%d:%s functionDescriptionOrNil = %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(functionDescriptionOrNil).c_str()););
  T_sp closure = nil<T_O>(), args = nil<T_O>();
  bool args_available = args_for_function(frameIndex, absolute_ip, string, XEPp, arityCode, codeStart, functionStartAddress, ofi,
                                          functionDescriptionOrNil, fbp, closure, args);
  T_sp fname = nil<T_O>();
  if (gc::IsA<FunctionDescription_sp>(functionDescriptionOrNil)) {
    fname = gc::As_unsafe<FunctionDescription_sp>(functionDescriptionOrNil)->functionName();
    D(printf("%s%s:%d:%s Using functionDescriptionOrNil %s to get name\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__,
             _rep_(fname).c_str()););
  } else if (args_available && gc::IsA<Function_sp>(closure)) {
    fname = gc::As_unsafe<Function_sp>(closure)->functionName();
    D(printf("%s%s:%d:%s Using closure %s to get name\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__,
             _rep_(fname).c_str()););
  }
  D(printf("%s%s:%d:%s fname = %s\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, _rep_(fname).c_str()););
  if (fname.nilp() && string) {
    D(printf(
          "%s:%d:%s So sad - the function-description name is NIL and the function-description.nilp()->%d trying to use string\n",
          __FILE__, __LINE__, __FUNCTION__, functionDescriptionOrNil.nilp()););
    fname = SimpleBaseString_O::make(std::string(string));
  }
  D(printf("%s%s:%d:%s string = %s\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, string););
  return DebuggerFrame_O::make(fname, Cons_O::create(sa, ofi), spi, functionDescriptionOrNil, closure, args, args_available,
                               nil<T_O>(), INTERN_(kw, lisp), XEPp);
}

static DebuggerFrame_sp make_bytecode_frame_from_function(BytecodeSimpleFun_sp fun, void* bpc, T_O** bfp) {
  // We can get the closure easy if the function actually isn't one.
  // Otherwise we'd have to poke through bytecode_vm arguments or maybe
  // the vm stack?
  T_sp closure = (fun->environmentSize() == 0) ? (T_sp)fun : nil<T_O>();
  List_sp bindings = bytecode_bindings_for_pc(fun->code(), bpc, bfp);
  T_sp spi = bytecode_spi_for_pc(fun->code(), bpc);
  // Grab arguments.
  T_sp tnargs((gctools::Tagged)(*(bfp - BYTECODE_FRAME_NARGS_OFFSET)));
  size_t nargs = tnargs.unsafe_fixnum();
  T_O** argptr = (T_O**)*(bfp - BYTECODE_FRAME_ARGS_OFFSET);
  ql::list largs;
  for (size_t i = 0; i < nargs; ++i) {
    T_O* rarg = argptr[i];
    T_sp temp((gctools::Tagged)rarg);
    largs << temp;
  }
  // Finally make the frame.
  return DebuggerFrame_O::make(fun->functionName(), Pointer_O::create(bpc), spi, fun->fdesc(), closure, largs.cons(), true, bindings,
                               INTERN_(kw, bytecode), false);
}

static DebuggerFrame_sp make_bytecode_frame(size_t frameIndex, unsigned char*& pc, T_O**& fp) {
  // Get the PC and frame pointer for the next frame.
  void* bpc = pc;
  T_O** bfp = fp;
  if (fp) { // null fp means we've hit the end.
    pc = (unsigned char*)(*(fp - BYTECODE_FRAME_PC_OFFSET));
    fp = (T_O**)(*(fp - BYTECODE_FRAME_FP_OFFSET));
  }
  // Find the bytecode module containing the current pc.
  List_sp modules = _lisp->_Roots._AllBytecodeModules.load(std::memory_order_relaxed);
  for (auto mods : modules) {
    BytecodeModule_sp mod = gc::As_assert<BytecodeModule_sp>(oCar(mods));
    if (bytecode_module_contains_address_p(mod, bpc)) {
      T_sp fun = bytecode_function_for_pc(mod, bpc);
      if (gc::IsA<BytecodeSimpleFun_sp>(fun))
        return make_bytecode_frame_from_function(gc::As_unsafe<BytecodeSimpleFun_sp>(fun), bpc, bfp);
    }
  }
  return DebuggerFrame_O::make(INTERN_(kw, bytecode), Pointer_O::create(bpc), nil<T_O>(), nil<T_O>(), nil<T_O>(), nil<T_O>(), false,
                               nil<T_O>(), INTERN_(kw, bytecode), false);
}

bool maybe_demangle(const std::string& fnName, std::string& output) {
  char* funcname = (char*)malloc(1024);
  size_t funcnamesize = 1024;
  int status;
  char* ret = abi::__cxa_demangle(fnName.c_str(), funcname, &funcnamesize, &status);
  if (status == 0) {
    std::string demangled(funcname);
    free(ret);
    output = demangled;
    return true;
  }
  if (fnName[0] == '_') {
    // try stripping off the first underscore
    std::string shortFnName = fnName.substr(1, std::string::npos);
    char* ret = abi::__cxa_demangle(shortFnName.c_str(), funcname, &funcnamesize, &status);
    if (status == 0) {
      std::string demangled(funcname);
      free(ret);
      output = demangled;
      return true;
    }
  }
  if (funcname)
    free(funcname);
  return false;
}

static DebuggerFrame_sp make_cxx_frame(size_t fi, void* ip, const char* cstring, unsigned char*& bytecode_pc, T_O**& bytecode_fp) {
  MaybeTrace trace(__FUNCTION__);
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
  if (parts.size() > 3) {
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
  if ((nameEnd - nameStart) > 1) {
    linkname = str.substr(nameStart + 1, nameEnd);
    size_t pluspos = linkname.rfind('+');
    if (pluspos != std::string::npos) {
      linkname = linkname.substr(0, pluspos); // remove +0x###
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
  // Look for bytecode frames.
  // NOTE: This is a little fragile. Beware.
  if (name == "bytecode_call")
    return make_bytecode_frame(fi, bytecode_pc, bytecode_fp);
  T_sp lname = SimpleBaseString_O::make(name);
  D(printf("%s%s:%d:%s lname %s\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, name.c_str()););
  return DebuggerFrame_O::make(lname, Pointer_O::create(ip), nil<T_O>(), nil<T_O>(), nil<T_O>(), nil<T_O>(), false, nil<T_O>(),
                               INTERN_(kw, c_PLUS__PLUS_), false);
}

static DebuggerFrame_sp make_frame(size_t fi, void* absolute_ip, const char* string, void* fbp, unsigned char*& bytecode_pc,
                                   T_O**& bytecode_fp) {
  MaybeTrace trace(__FUNCTION__);
  T_sp of = llvmo::only_object_file_for_instruction_pointer(absolute_ip);
  if (of.nilp())
    return make_cxx_frame(fi, absolute_ip, string, bytecode_pc, bytecode_fp);
  // The absolute_ip is in an ObjectFile_O object - so it must be a lisp frame
  else
    return make_lisp_frame(fi, absolute_ip, string, gc::As_unsafe<llvmo::ObjectFile_sp>(of), fbp);
}

static bool sanity_check_frame(size_t frameIndex, void* ip, void* fbp) {
  MaybeTrace trace(__FUNCTION__);
  T_sp of = llvmo::only_object_file_for_instruction_pointer(ip);
  if (of.nilp())
    return true; // C++ frames are always fine for our purposes
  else if (gc::IsA<llvmo::ObjectFile_sp>(of)) {
    llvmo::ObjectFile_sp ofi = gc::As_unsafe<llvmo::ObjectFile_sp>(of);
    llvmo::SectionedAddress_sp sa = object_file_sectioned_address(ip, ofi, false);
    llvmo::DWARFContext_sp dcontext = llvmo::DWARFContext_O::createDWARFContext(ofi);
    void* codeStart;
    void* functionStartAddress;
    bool XEPp = false;
    int arityCode;
    T_sp ep = dwarf_ep(frameIndex, ofi, dcontext, sa, codeStart, functionStartAddress, XEPp, arityCode);
    uintptr_t stackmap_start = (uintptr_t)(ofi->_StackmapStart);
    uintptr_t stackmap_end = stackmap_start + ofi->_StackmapSize;
    if (gc::IsA<CoreFun_sp>(ep)) {
      CoreFun_sp localEntryPoint = gc::As_unsafe<CoreFun_sp>(ep);
      bool result = true;
      auto thunk = [&](size_t _, const smStkSizeRecord& function, int32_t offsetOrSmallConstant, int64_t patchPointId) {
        if (function.FunctionAddress == (uintptr_t)(localEntryPoint->_Entry)) {
          if (!sanity_check_args(fbp, offsetOrSmallConstant, patchPointId))
            result = false;
          return;
        }
      };
      {
        MaybeTrace tracew("walk_one_llvm_stackmap");
        walk_one_llvm_stackmap(thunk, stackmap_start, stackmap_end);
      }
      return result;
    } else if (gc::IsA<SimpleFun_sp>(ep)) {
      SimpleFun_sp globalEntryPoint = gc::As_unsafe<SimpleFun_sp>(ep);
      bool result = true;
      auto thunk = [&](size_t _, const smStkSizeRecord& function, int32_t offsetOrSmallConstant, int64_t patchPointId) {
        if (function.FunctionAddress == (uintptr_t)(globalEntryPoint->_EntryPoints[arityCode])) {
          if (!sanity_check_args(fbp, offsetOrSmallConstant, patchPointId))
            result = false;
          return;
        }
      };
      {
        MaybeTrace tracew("walk_one_llvm_stackmap");
        walk_one_llvm_stackmap(thunk, stackmap_start, stackmap_end);
      }
      return result;
    } else if (ep.nilp())
      return true; // annoying but sane
    else {
      fprintf(stderr, "%s:%d:%s  dwarf_fp returned object of wrong type\n", __FILE__, __LINE__, __FUNCTION__);
      return false;
    }
  } else {
    fprintf(stderr, "%s:%d:%s  only_object_file_for_instruction_pointer returned object of wrong type\n", __FILE__, __LINE__,
            __FUNCTION__);
    return false;
  }
}

#ifdef USE_LIBUNWIND
__attribute__((optnone)) static T_mv lu_call_with_frame(std::function<T_mv(DebuggerFrame_sp)> f) {
  unw_cursor_t cursor;
  unw_word_t ip, fbp;
  unw_context_t uc;

  unw_getcontext(&uc);
  unw_init_local(&cursor, &uc);

  int resip = unw_get_reg(&cursor, UNW_REG_IP, &ip);
  int resbp = unw_get_reg(&cursor, UNW_X86_64_RBP, &fbp);

  VirtualMachine& vm = my_thread->_VM;
  unsigned char* bytecode_pc = vm._pc;
  T_O** bytecode_fp = vm._framePointer;

  if (resip || resbp) {
    fprintf(stderr, "%s:%d:%s  unw_get_reg resip=%d ip = %p  resbp=%d rbp = %p\n", __FILE__, __LINE__, __FUNCTION__, resip,
            (void*)ip, resbp, (void*)fbp);
  }
  // This is slightly inefficient in that we allocate a simple base string
  // only to get a C string from it, but writing it to use stack allocation
  // is a pain in the ass for very little gain.
  std::string sstring = lu_procname(&cursor)->get_std_string();
  int fi(0);
  DebuggerFrame_sp bot = make_frame(fi++, (void*)ip, sstring.c_str(), (void*)fbp, bytecode_pc, bytecode_fp);
  DebuggerFrame_sp prev = bot;
  while (unw_step(&cursor) > 0) {
    resip = unw_get_reg(&cursor, UNW_REG_IP, &ip);
    resbp = unw_get_reg(&cursor, UNW_X86_64_RBP, &fbp);
    if (resip || resbp) {
      printf("%s:%d:%s  unw_get_reg resip=%d ip = %p  resbp=%d rbp = %p\n", __FILE__, __LINE__, __FUNCTION__, resip, (void*)ip,
             resbp, (void*)fbp);
    }
    // Subtract 1 from IPs in case they are just beyond the end of the function
    // as happens with return instructions sometimes.
    --ip;
    std::string sstring = lu_procname(&cursor)->get_std_string();
    DebuggerFrame_sp frame = make_frame(fi++, (void*)ip, sstring.c_str(), (void*)fbp, bytecode_pc, bytecode_fp);
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
  size_t frameIndex(0);
  do {
    int resip = unw_get_reg(&cursor, UNW_REG_IP, &ip);
    int resbp = unw_get_reg(&cursor, UNW_X86_64_RBP, &fbp);
    if (resip || resbp) {
      fprintf(stderr, "%s:%d:%s  unw_get_reg resip=%d ip = %p  resbp=%d rbp = %p\n", __FILE__, __LINE__, __FUNCTION__, resip,
              (void*)ip, resbp, (void*)fbp);
      return false;
    }
    if (!(sanity_check_frame(frameIndex, (void*)ip, (void*)fbp)))
      return false;
    int resstep = unw_step(&cursor);
    if (resstep == 0)
      return true;
    else if (resstep < 0) { // error
      fprintf(stderr, "%s:%d:%s  unw_step returned error %d", __FILE__, __LINE__, __FUNCTION__, resstep);
      return false;
    }
    frameIndex++;
  } while (true);
}

#else // non-libunwind version

bool accessible_memory_p(void* ptr) {
  int fd[2];
  bool res = true;
  if (pipe(fd) >= 0) {
    if (write(fd[1], ptr, 128) > 0)
      res = true;
    else
      res = false;
    close(fd[0]);
    close(fd[1]);
  }
  return res;
}

static T_mv os_call_with_frame(std::function<T_mv(DebuggerFrame_sp)> func) {
#define START_BACKTRACE_SIZE 512
#define MAX_BACKTRACE_SIZE_LOG2 20
  MaybeTrace trace(__FUNCTION__);
  size_t num = START_BACKTRACE_SIZE;
  void** buffer = (void**)calloc(sizeof(void*), num);

  VirtualMachine& vm = my_thread->_VM;
  unsigned char* bytecode_pc = vm._pc;
  T_O** bytecode_fp = vm._framePointer;

  for (size_t attempt = 0; attempt < MAX_BACKTRACE_SIZE_LOG2; ++attempt) {
    size_t returned = backtrace(buffer, num);
    if (returned < num) {
      char** strings = backtrace_symbols(buffer, returned);
      void* fbp = __builtin_frame_address(0); // TODO later
      uintptr_t bplow = (uintptr_t)&fbp;
      uintptr_t bphigh = (uintptr_t)my_thread_low_level->_StackTop;
      DebuggerFrame_sp bot = make_frame(0, buffer[0], strings[0], fbp, bytecode_pc, bytecode_fp);
      DebuggerFrame_sp prev = bot;
      void* newfbp;
      for (size_t j = 1; j < returned; ++j) {
        if (!accessible_memory_p((void*)fbp)) {
          newfbp = NULL;
        } else if (fbp) {
          newfbp = *(void**)fbp;
          if (newfbp && !(bplow < (uintptr_t)newfbp && (uintptr_t)newfbp < bphigh)) { // newfbp is out of the stack.
            //          fprintf(stderr, "%s:%d:%s The frame pointer walk went out of bounds bplow is %p bphigh is %p and newfbp is
            //          %p\n",
            //                 __FILE__, __LINE__, __FUNCTION__, (void*)bplow, (void*)bphigh, newfbp );
            newfbp = NULL;
          } else if (newfbp && !((uintptr_t)fbp < (uintptr_t)newfbp)) { // fbp < newfbp
            fprintf(stderr, "%s:%d:%s The frame pointer is not monotonically increasing fbp is %p and newfbp is %p\n", __FILE__,
                    __LINE__, __FUNCTION__, fbp, newfbp);
            newfbp = NULL;
          }
        }
        fbp = newfbp;
        // Subtract one from IPs in case they are just beyond the end of the
        // function, as happens with return instructions at times.
        void* absolute_ip = (void*)((uintptr_t)buffer[j] - 1);
        D(printf("%s%s:%d:%s top-frame[%lu] absolute_ip = %p %s\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__, j,
                 absolute_ip, strings[j]););
        DebuggerFrame_sp frame = make_frame(j, absolute_ip, strings[j], fbp, bytecode_pc, bytecode_fp);
        frame->down = prev;
        prev->up = frame;
        prev = frame;
      }
      free(buffer);
      free(strings);
      D(printf("%s%s:%d:%s Calling func with frame\n", trace.spaces().c_str(), __FILE__, __LINE__, __FUNCTION__););
      return func(bot);
    }
    // realloc_array would be nice, but macs don't have it
    num *= 2;
    buffer = (void**)realloc(buffer, sizeof(void*) * num);
  }
  fprintf(stderr, "%s:%d Couldn't get backtrace\n", __FILE__, __LINE__);
  abort();
}
static bool os_sanity_check_backtrace() {
  MaybeTrace trace(__FUNCTION__);
  size_t num = START_BACKTRACE_SIZE;
  void** buffer = (void**)calloc(sizeof(void*), num);
  for (size_t attempt = 0; attempt < MAX_BACKTRACE_SIZE_LOG2; ++attempt) {
    size_t returned = backtrace(buffer, num);
    if (returned < num) {
      void* fbp = __builtin_frame_address(0);
      for (size_t j = 0; j < returned; ++j) {
        /*
        if (fbp && !((stackbot <= (uintptr_t)fbp) && ((uintptr_t)fbp <= stacktop))) {
          fprintf(stderr, "%s:%d:%s In backtrace sanity check, the frame pointer went out of bounds. Stack low is %p stack high is
        %p frame pointer is %p\n",
                  __FILE__, __LINE__, __FUNCTION__,
                  (char*)stackbot, (char*)stacktop, fbp);
          return false;
        }
        */
        if (!sanity_check_frame(0, buffer[j], fbp))
          return false;
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
      buffer = (void**)realloc(buffer, sizeof(void*) * num);
    }
  }
  fprintf(stderr, "%s:%d:%s  too many frames\n", __FILE__, __LINE__, __FUNCTION__);
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
DOCGROUP(clasp);
CL_DEFUN bool core__sanity_check_backtrace() {
#ifdef USE_LIBUNWIND
  return lu_sanity_check_backtrace();
#else
  return os_sanity_check_backtrace();
#endif
}

DOCGROUP(clasp);
__attribute__((optnone)) CL_DEFUN T_mv core__call_with_frame(Function_sp function) {
  MaybeTrace trace(__FUNCTION__);
  auto th = [&](DebuggerFrame_sp bot) { return eval::funcall(function, bot); };
  return call_with_frame(th);
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__debugger_frame_fname(DebuggerFrame_sp df) { return df->fname; }
DOCGROUP(clasp);
CL_DEFUN T_sp core__debugger_frame_source_position(DebuggerFrame_sp df) { return df->source_position; }
DOCGROUP(clasp);
CL_DEFUN T_sp core__debugger_frame_function_description(DebuggerFrame_sp df) { return df->function_description; }
DOCGROUP(clasp);
CL_DEFUN T_sp core__debugger_frame_lang(DebuggerFrame_sp df) { return df->lang; }
DOCGROUP(clasp);
CL_DEFUN List_sp core__debugger_frame_locals(DebuggerFrame_sp df) { return df->locals; }
DOCGROUP(clasp);
CL_DEFUN T_sp core__debugger_frame_closure(DebuggerFrame_sp df) { return df->closure; }
DOCGROUP(clasp);
CL_DEFUN T_sp core__debugger_frame_args(DebuggerFrame_sp df) { return df->args; }
DOCGROUP(clasp);
CL_DEFUN bool core__debugger_frame_args_available_p(DebuggerFrame_sp df) { return df->args_available; }
DOCGROUP(clasp);
CL_DEFUN bool core__debugger_frame_xep_p(DebuggerFrame_sp df) { return df->is_xep; }
DOCGROUP(clasp);
CL_DEFUN T_sp core__debugger_frame_up(DebuggerFrame_sp df) { return df->up; }
DOCGROUP(clasp);
CL_DEFUN T_sp core__debugger_frame_down(DebuggerFrame_sp df) { return df->down; }

DOCGROUP(clasp);
CL_DEFUN T_sp core__debugger_local_fname(DebuggerLocal_sp dl) { return dl->fname; }
DOCGROUP(clasp);
CL_DEFUN T_sp core__debugger_local_name(DebuggerLocal_sp dl) { return dl->name; }
DOCGROUP(clasp);
CL_DEFUN T_sp core__debugger_local_declfile(DebuggerLocal_sp dl) { return dl->declfile; }
DOCGROUP(clasp);
CL_DEFUN T_sp core__debugger_local_declline(DebuggerLocal_sp dl) { return dl->declline; }

}; // namespace core
