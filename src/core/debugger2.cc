#include <iomanip>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/arguments.h>
#include <clasp/core/myReadLine.h>
#include <clasp/core/primitives.h>
#include <clasp/core/write_ugly.h>
#include <clasp/core/bformat.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/backtrace.h>

namespace core {

static void debugger_helpmsg() {
  write_bf_stream(BF(":?      - help\n"));
  write_bf_stream(BF(":h      - help\n"));
  write_bf_stream(BF(":e sexp - evaluate sexp\n"));
  write_bf_stream(BF("          (A sexp by itself also works)\n"));
  write_bf_stream(BF(":c sexp - continue - return values of evaluating sexp\n"));
  // write_bf_stream(BF(":v      - list local environment\n")); // TODO
  write_bf_stream(BF(":x      - print current expression\n"));
  write_bf_stream(BF(":b      - print backtrace\n"));
  write_bf_stream(BF(":u      - goto caller frame\n"));
  write_bf_stream(BF(":d      - goto callee frame\n"));
  // write_bf_stream(BF(":D      - dissasemble current function\n")); // TODO?
  write_bf_stream(BF(":a      - abort\n"));
  write_bf_stream(BF(":g ##   - jump to frame ##\n"));

}

// Return the frame shift frames away from cur. Shift may be positive or
// negative. If the top or bottom is reached, stays there. The index of the
// current frame frame is read from index, and then the new frame's index is
// stored there.
static DebuggerFrame_sp debugger_frame_rel(DebuggerFrame_sp cur, int shift,
                                           int& index) {
  if (shift > 0) {
    while (shift > 0) {
      T_sp next = cur->up;
      if (next.notnilp()) {
        cur = gc::As<DebuggerFrame_sp>(next); --shift; ++index;
      } else // hit top frame
        return cur;
    }
    return cur;
  } else if (shift < 0) {
    while (shift < 0) {
      T_sp next = cur->down;
      if (next.notnilp()) {
        cur = gc::As<DebuggerFrame_sp>(next); ++shift; --index;
      } else // hit bottom frame
        return cur;
    }
    return cur;
  } else return cur;
}

static std::string thing_as_string(T_sp obj) {
  if (gc::IsA<SimpleBaseString_sp>(obj)) {
    return gc::As_unsafe<SimpleBaseString_sp>(obj)->get_std_string();
  } else if (gc::IsA<SimpleCharacterString_sp>(obj)) {
    return gc::As_unsafe<SimpleBaseString_sp>(obj)->get_std_string();
  } else if (gc::IsA<Str8Ns_sp>(obj)) {
    return gc::As_unsafe<Str8Ns_sp>(obj)->get_std_string();
  } else if (gc::IsA<StrWNs_sp>(obj)) {
    return gc::As_unsafe<StrWNs_sp>(obj)->get_std_string();
  } else return _rep_(obj);
}

static void debugger_display_frame(DebuggerFrame_sp cur, int index) {
  // TODO: arguments, source location?
  T_sp stream = cl::_sym_STARstandard_outputSTAR->symbolValue();
  stringstream num;
  num << std::setw(4);
  num << index;
  clasp_write_string(num.str(), stream);
  clasp_write_string(": ", stream);
  clasp_write_string(thing_as_string(cur->fname), stream);
  clasp_write_string("\n", stream);
}

SYMBOL_EXPORT_SC_(CorePkg,primitive_print_backtrace);

static void debugger_backtrace(DebuggerFrame_sp cur, int index) {
  if (_sym_primitive_print_backtrace.boundp() && _sym_primitive_print_backtrace->fboundp()) {
    printf("%s:%d:%s Calling sys:primitive-print-backtrace\n", __FILE__, __LINE__, __FUNCTION__ );
    eval::funcall(_sym_primitive_print_backtrace);
  } else {
    while (1) {
      debugger_display_frame(cur, index);
      T_sp next = cur->up;
      if (next.nilp()) break;
      else cur = gc::As<DebuggerFrame_sp>(next);
      ++index;
    }
  }
}

static bool debugger_parse_integer(string s, int& new_frame_index) {
  try {
    new_frame_index = std::stoi(s);
    return true;
  } catch (...) {
    return false;
  }
}

T_mv early_debug_inner(DebuggerFrame_sp bot, bool can_continue) {
  int frame_index = 0;
  DebuggerFrame_sp cur = bot;
  debugger_display_frame(cur, frame_index);
  while (1) {
    string line;
    stringstream sprompt;
    sprompt << "Frame-" << frame_index << "-Dbg>";
    bool end_of_transmission(false);
    line = myReadLine(sprompt.str(), end_of_transmission);
    if (end_of_transmission) {
      printf("%s:%d Exiting debugger\n", __FILE__, __LINE__);
      throw ExitProgramException(0);
    }
    char cmd;
    string eline;
    if (line.empty()) continue;
    if ((line[0] == ':') && (line.size() > 1)) {
      cmd = line[1];
      eline = line.substr(2);
    } else {
      cmd = 'e';
      eline = line;
    }
    switch (cmd) {
    case '?':
    case 'h': // help
        debugger_helpmsg();
        break;
    case 'g': { // go to frame
      int new_frame_index;
      if (debugger_parse_integer(eline, new_frame_index)) {
        write_bf_stream(BF("Switching to frame: %d\n") % new_frame_index);
        cur = debugger_frame_rel(cur, new_frame_index - frame_index, frame_index);
      } else write_bf_stream(BF("You must provide a frame index\n"));
      break;
    }
    case 'u': // up
        cur = debugger_frame_rel(cur, 1, frame_index);
        break;
    case 'd': // down
        cur = debugger_frame_rel(cur, -1, frame_index);
        break;
    case 'b': // backtrace
        debugger_backtrace(cur, frame_index);
        break;
    case 'x': // examine current frame
        debugger_display_frame(cur, frame_index);
        break;
    case 'a': // abort
        throw(DebuggerSaysAbortToRepl());
    case 'c': // continue with given result
        if (can_continue)
          return _lisp->readEvalPrintString(eline, nil<T_O>(), true);
        else write_bf_stream(BF("Cannot continue from this error\n"));
        break;
    case 'e': // evaluate
        try { _lisp->readEvalPrintString(eline, nil<T_O>(), true); }
        // If the debugger is entered recursively and aborted out of,
        // return here.
        catch (DebuggerSaysAbortToRepl &err) {}
        break;
    default: write_bf_stream(BF("Unknown command[%c] - try ':?'\n") % cmd);
    } // cmd switch
  } // read eval print loop
}

struct DebuggerLevelRAII {
  DebuggerLevelRAII() { ++(globals_->_DebuggerLevel); }
  ~DebuggerLevelRAII() { --(globals_->_DebuggerLevel); }
};

T_mv early_debug(T_sp condition, bool can_continue) {
  if (!isatty(0)) {
    printf("The low-level debugger was entered but there is no terminal on fd0 - aboring\n");
    abort();
  }
  if (globals_->_DebuggerDisabled) {
    printf("This is not an interactive session and the low-level debugger was entered - aborting\n");
    abort();
  }
  struct DebuggerLevelRAII dumb;
  if (globals_->_DebuggerLevel > 10) {
    printf("The low-level debugger was recursively entered too many times - exiting\n");
  }
  if (condition.notnilp()) {
    write_bf_stream(BF("Debugger entered with condition: %s\n")
                    % _rep_(condition));
  }
  return call_with_frame([=](auto frame){return early_debug_inner(frame, can_continue);});
}

DOCGROUP(clasp)
CL_DEFUN T_mv core__early_debug(T_sp condition) {
  return early_debug(condition, true);
}

}; // namespace core
