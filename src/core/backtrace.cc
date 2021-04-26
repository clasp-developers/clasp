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
#include <clasp/core/backtrace.h>
#include <stdlib.h> // calloc, realloc, free
#include <execinfo.h> // backtrace

/*
 * This file contains the low-ish level code to get backtrace information.
 * The main entry point is core:call-with-frame, which collects this information
 * and passes it to a provided thunk. This makes more sense than a function
 * that simply _returns_ this information, since after all the control stack
 * inherently has only dynamic extent.
 * This information takes the form of objects called "frames"; see backtrace.h
 * for definitions. Hopefully should be simple to understand.
 * This mechanism uses backtrace, backtrace_symbols, and DWARF metadata.
 *
 * The function core:call-with-operating-system-backtrace is similar but lower
 * level, collecting only information from the system backtrace and
 * backtrace_symbols function, and stack frame pointers. This is not used by
 * call-with-frame but may very occasionally be useful on its own, mostly for
 * debugging the debugger (gods help you).
 */

namespace core {
NEVER_OPTIMIZE
CL_DEFUN T_mv core__call_with_operating_system_backtrace(Function_sp function) {
  // Get an operating system backtrace, i.e. with the backtrace and
  // backtrace_symbols functions (which are not POSIX, but are present in both
  // GNU and Apple systems).
  // backtrace requires a number of frames to get, and will fill only that many
  // entries. To get the full backtrace, we repeatedly try larger frame numbers
  // until backtrace finally doesn't fill everything in.
#define START_BACKTRACE_SIZE 512
#define MAX_BACKTRACE_SIZE_LOG2 20
  size_t num = START_BACKTRACE_SIZE;
  void** buffer = (void**)calloc(sizeof(void*), num);
  for (size_t attempt = 0; attempt < MAX_BACKTRACE_SIZE_LOG2; ++attempt) {
    size_t returned = backtrace(buffer,num);
    if (returned < num) {
      char **strings = backtrace_symbols(buffer, returned);
      ql::list pointers;
      ql::list names;
      ql::list basepointers;
      void* bp = __builtin_frame_address(0);
      uintptr_t bplow = (uintptr_t)&bp;
      uintptr_t bphigh = (uintptr_t)my_thread_low_level->_StackTop;
      for (size_t j = 0; j < returned; ++j) {
        pointers << Pointer_O::create(buffer[j]);
        names << SimpleBaseString_O::make(strings[j]);
        basepointers << Pointer_O::create(bp);
        if (bp) bp = *(void**)bp;
        if (!(bplow<(uintptr_t)bp && (uintptr_t)bp<bphigh)) {
          bp = NULL;
        }
      }
      free(buffer);
      free(strings);
      return eval::funcall(function, pointers.cons(), names.cons(),
                           basepointers.cons());
    }
    // realloc_array would be nice, but macs don't have it
    num *= 2;
    buffer = (void**)realloc(buffer, sizeof(void*)*num);
  }
  printf("%s:%d Couldn't get backtrace\n", __FILE__, __LINE__ );
  abort();
}

static T_sp dwarf_spi(llvmo::DWARFContext_sp dcontext,
                      llvmo::SectionedAddress_sp sa) {
    // FIXME: Might be better to just use the llvm::DILineInfo directly.
  T_mv lineinfo = llvmo::getLineInfoForAddress(dcontext, sa);
  if (lineinfo.notnilp()) {
    SimpleBaseString_sp filename = gc::As<SimpleBaseString_sp>(lineinfo);
    Integer_sp line = gc::As<Integer_sp>(lineinfo.valueGet_(3));
    Integer_sp column = gc::As<Integer_sp>(lineinfo.valueGet_(4));
    return SourcePosInfo_O::make(filename->get_std_string(),
                                 0, // eck
                                 // See above FIXME; these should all be fine,
                                 // but then why go through Integer_O at all?
                                 line.unsafe_fixnum(), column.unsafe_fixnum());
  } else return _Nil<T_O>();
}

static T_sp dwarf_fd(llvmo::ObjectFile_sp ofi,
                     llvmo::DWARFContext_sp dcontext,
                     llvmo::SectionedAddress_sp sa) {
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
            return ep->_FunctionDescription;
        }
      } else if (gc::IsA<GlobalEntryPoint_sp>(literal)) {
        GlobalEntryPoint_sp ep = gc::As_unsafe<GlobalEntryPoint_sp>(literal);
        for (size_t j = 0; j < NUMBER_OF_ENTRY_POINTS; ++j) {
          uintptr_t ip = (uintptr_t)(ep->_EntryPoints[j]) - code_start;
          for (auto range : ranges) {
            if ((range.LowPC <= ip) && (ip < range.HighPC))
              return ep->_FunctionDescription;
          }
        }
      }
    }
    // no hits
    return _Nil<T_O>();
  } else {
    // FIXME: signal error?
    return _Nil<T_O>();
  }
}

static DebuggerFrame_sp make_lisp_frame(void* ip, llvmo::ObjectFile_sp ofi) {
  llvmo::SectionedAddress_sp sa = object_file_sectioned_address(ip, ofi, false);
  llvmo::DWARFContext_sp dcontext = llvmo::DWARFContext_O::createDwarfContext(ofi);
  T_sp spi = dwarf_spi(dcontext, sa);
  T_sp fd = dwarf_fd(ofi, dcontext, sa);
  T_sp fname = _Nil<T_O>();
  if (fd.notnilp())
    fname = gc::As_unsafe<FunctionDescription_sp>(fd)->functionName();
  return DebuggerFrame_O::make(fname, spi, fd, INTERN_(kw, lisp));
}

// FIXME: remove 2. This is a stupid disambiguation.
static bool maybe_demangle2(const std::string& fnName, std::string& output)
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

static DebuggerFrame_sp make_cxx_frame(void* ip, char* cstring) {
  std::string str(cstring);
  std::vector<std::string> parts = split(str, " ");
  // The string is divided into:
  // index executable address name plus offset
  // but we only care about the name.
  // FIXME: Format probably varies by OS.
  std::string linkname = parts[3];
  std::string name;
  if (!(maybe_demangle2(linkname, name)))
    // couldn't demangle, so just use the unadulterated string
    name = linkname;
  T_sp lname = SimpleBaseString_O::make(name);
  return DebuggerFrame_O::make(lname, _Nil<T_O>(), _Nil<T_O>(),
                               INTERN_(kw, c_PLUS__PLUS_));
}

static DebuggerFrame_sp make_frame(void* ip, char* string) {
  T_sp of = llvmo::only_object_file_for_instruction_pointer(ip);
  if (of.nilp()) return make_cxx_frame(ip, string);
  else return make_lisp_frame(ip, gc::As_unsafe<llvmo::ObjectFile_sp>(of));
}

CL_DEFUN T_mv core__call_with_frame(Function_sp function) {
    size_t num = START_BACKTRACE_SIZE;
  void** buffer = (void**)calloc(sizeof(void*), num);
  for (size_t attempt = 0; attempt < MAX_BACKTRACE_SIZE_LOG2; ++attempt) {
    size_t returned = backtrace(buffer,num);
    if (returned < num) {
      char **strings = backtrace_symbols(buffer, returned);
      // void* bp = __builtin_frame_address(0); // TODO later
      DebuggerFrame_sp bot = make_frame(buffer[0], strings[0]);
      DebuggerFrame_sp prev = bot;
      for (size_t j = 1; j < returned; ++j) {
        DebuggerFrame_sp frame = make_frame(buffer[j], strings[j]);
        frame->down = prev;
        prev->up = frame;
        prev = frame;
        // if (bp) bp = *(void**)bp;
      }
      free(buffer);
      free(strings);
      return eval::funcall(function, bot);
    }
    // realloc_array would be nice, but macs don't have it
    num *= 2;
    buffer = (void**)realloc(buffer, sizeof(void*)*num);
  }
  printf("%s:%d Couldn't get backtrace\n", __FILE__, __LINE__ );
  abort();
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
