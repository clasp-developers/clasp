/*
    File: commandLineOptions.cc
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

#include <iostream>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include "clasp/core/compiler.h"
#include "clasp/core/ql.h"
#include "clasp/core/lisp.h"
#include <clasp/gctools/snapshotSaveLoad.h>
#include <clasp/core/commandLineOptions.h>
#include <version.h>

namespace core {

bool global_debug_start_code = false;

const char* help = R"dx(Usage: clasp <options>
Options:
  -n, --noinit
      Don't load the default image or init.lisp (very minimal environment)
  -I, --ignore-image
      Don't load the boot image, instead load init.lisp
  -B, --base
      Load the base image instead of the extension image. If Clasp was not
      built with extensions this has no effect.
  -i, --image <file>
      Use <file> as the boot image.
  --snapshot <file>
      Use <file> as the boot snapshot.
  -L, --llvm-debug <options>
      Pass <options> to llvm::cl::ParseCommandLineOptions --debug-only. Lets you
      debug llvm with LLVM_DEBUG(...)
  -g, --debug
      Describe the clasp data structures for lldb Python API to /tmp/clasp.py
  -d, --describe <file>
      Describe the clasp data structures for lldb Python API to <file>
  -U, --unpack-faso <file>
      Unpack the faso <file> into separate object files
  --noinform
      Don't print startup banner text
  --noprint
      Don't prompt or print in read-eval loop
  -D, --disable-debugger
      If the default debugger would be entered, Clasp instead quits
  -G, --dump-debugger-info
      Dump the info for the gdb/udb/lldb debugger extension.
  --quit
      Don't start a REPL
  -a, --addresses <file>
      Dump all symbol addresses in executable to <file>
  -N, --non-interactive
      Short for --disable-debugger --quit
  -m, --disable-mpi
      Don't use mpi even if built with mpi
  -t, --trampolines
      Generate trampolines around bytecode functions for profiling and debugging.
      This slows performance a bit.
  -v, --version
      Print version
  -s, --verbose
      Print more info while booting
  -y, --snapshot-symbols
      Check symbols while starting up and verify that snapshot save will
      work. The symbols can be recovered at runtime calling by
      (core:mangled-symbol-names <stream>).
  -f, --feature <feature>
      Add the feature to *features*
  -e, --eval <form>
      Evaluate a <form>
  -l, --load <file>
      LOAD the <file>
  --script <file>
      LOAD the <file> and skip any leading shebang. This also adds the --norc,
      --noinform and --non-interactive options.
  --rc <file>
      Specify name of the RC file (default .clasprc)
  -r, --norc
      Don't load the RC file
  -S, --seed <n>
      Seed the random number generator with <n>
  -w, --wait
      Print the PID and wait for the user to hit a key
  -- <argument>*
      Trailing <argument> not processed and are added to
      core:*command-line-arguments*

*features* values:
  sanitizer=thread
      Setup codegen for thread sanitizer
  sanitizer=memory
      Setup codegen for memory sanitizer
  sanitizer=address
      Setup codegen for address sanitizer
  debug-startup
      Print a message for every top level form at startup (requires DEBUG_SLOW)
  debug-startup-verbose
      Print a message for every top level form and literal read at startup
      (requires DEBUG_SLOW)
  debug-run-clang
      Print every clang invocation
  exit-backtrace
      Print a backtrace if a non-zero exit code is used to exit
  jit-log-symbols
      Generate a log of JITted symbols
  pause-pid
      Print the PID and pause at startup for a debugger to attach
  ignore-extensions
      Ignore all extensions. This includes both Lisp and C code associated with
      the extension.
  clasp-builder-repl
      Stop in the clasp builder repl to debug bootstrapping
  use-human-readable-bitcode
      Write .ll files instead of .bc files
  disable-profiling
      Set cmp::*enable-profiling* to NIL and
  disable-dbg-generate-dwarf
      Set cmp::*dbg-generate-dwarf* to NIL
  disable
      generation of DWARF metadata during compilations
  force-compile-file-serial
      Force compile-file-serial to be used for compilation

Environment variables:
  CLASP_DEBUG=<filenames>
      Define files that generate log info when DEBUG_LEVEL_FULL is set at top
      of file. <filenames> is separated with spaces or commas.
  CLASP_DEBUGGER_SUPPORT=1
      Generate files that lldb/gdb/udb can use to debug clasp.
  CLASP_ENABLE_TRAMPOLINES=1
      Set this environment variable if you want good profiling.
  CLASP_NO_JIT_GDB=1
      Don't register object files with gdb/lldb for source level debugging.
  CLASP_SNAPSHOT=1
      Debug snapshot generation.
  CLASP_DONT_HANDLE_CRASH_SIGNALS=1
      Don't insert signal handlers for crash signals.
  CLASP_GC_MESSAGES=1
      Print a message when garbage collection takes place.
  CLASP_TIME_EXIT=1 (or wait-start|wait-end|wait-start-end)
      Print time to shutdown clasp - time from calling exit to the last atexit call.
  CLASP_HOME=<dir>
      Define where clasp source code lives
  CLASP_TIME_SNAPSHOT=1
      Turn on timing of snapshot load
  CLASP_OPTIMIZATION_LEVEL=0|1|2|3
      Set the llvm optimization level for compiled code
  CLASP_TRAP_INTERN=PKG:SYMBOL
      Trap the intern of the symbol
  CLASP_VERBOSE_BUNDLE_SETUP
      Dump info during bundle setup
  CLASP_DEBUG_START_CODE
      Dump info during startup for every start-code
  CLASP_EXIT_ON_WAIT_FOR_USER_SIGNAL
      Exit if wait-for-user-signal is encountered. Used for debugging under live-record.
  CLASP_DEBUG_OBJECT_FILES=save
      Saves all object files, anything else prints info about object file
      generation
  CLASP_PAUSE_STARTUP
      Pause right at startup before basic initialization
  CLASP_PAUSE_OBJECTS_ADDED
      Pause right at startup during snapshot load after objects are added to the
      jit
  CLASP_PAUSE_INIT
      Pause after startup and after basic initialization
  CLASP_DUMP_FUNCTIONS
      Dump all function definitions at startup
  CLASP_TELEMETRY_MASK=1|2
      Turn on telemetry. 1 is for the gc, 2 is for the stack.
  CLASP_TELEMETRY_FILE=<file>
      <file> to write telemetry
  CLASP_QUICKLISP_DIRECTORY=<directory>
      <directory> that contains quicklisp setup.lisp
  CLASP_FEATURES=<features>
      Set *features* (separate multiple features with spaces or commas)
  CLASP_MEMORY_PROFILE=<size-threshold> <number-theshold>
      Options means call HitAllocationSizeThreshold every time 16000000 bytes
      are allocated and call HitAllocationNumberThreshold every time 1024
      allocations take place
  CLASP_BACKTRACE_ALLOCATIONS=<stamp>
      Generate a backtrace to /tmp/stamp<stamp>.backtraces everytime a <stamp>
      object is allocates (VERY EXPENSIVE)
  CLASP_DEBUG_STAMP_INFO=1
      Generate info about stamps.
  CLASP_MPS_CONFIG=<arenaMb> <spareCommitLimitMb> <nurseryKb>
                   <nurseryMortalityPercent> <generation1Kb>
                   <generation1MortalityPercent> <keyExtendByKb>)dx";

bool CommandLineOptions::validStartupTypeOption(const std::string& arg) {
  if (this->_FreezeStartupType) {
    fmt::print(std::cerr, "Ignoring {} option because startup type has already been set to ", arg);
    switch (this->_StartupType) {
    case cloNone:
      fmt::print("ignore init.\n");
      break;
    case cloInitLisp:
      fmt::print("init.lisp.\n");
      break;
    case cloBaseImage:
      fmt::print("base image.\n");
      break;
    case cloExtensionImage:
      fmt::print("extension image.\n");
      break;
    case cloImageFile:
      fmt::print("image file.\n");
      break;
    case cloSnapshotFile:
      fmt::print("snapshot file.\n");
      break;
    case cloEmbeddedSnapshot:
      fmt::print("embedded snapshot.\n");
      break;
    }
    return false;
  }

  this->_FreezeStartupType = true;
  return true;
}

void CommandLineOptions::printVersion() {
  std::cout << gctools::program_name();
#ifdef USE_MPS
  std::cout << "-mps-";
#endif
#if defined(USE_BOEHM)
#ifdef USE_PRECISE_GC
  std::cout << "-boehmprecise-";
#else
  std::cout << "-boehm-";
#endif
#elif defined(USE_MMTK)
#ifdef USE_PRECISE_GC
  std::cout << "-mmtkprecise-";
#else
  std::cout << "-mmtk-";
#endif
#endif
  std::cout << CLASP_VERSION;
}

void process_clasp_arguments(CommandLineOptions* options) {
  std::set<std::string> parameter_required = {"-i",
                                              "--image",
                                              "--snapshot",
                                              "--type",
                                              "-L",
                                              "--llvm-debug",
                                              "-d",
                                              "--describe",
                                              "-a",
                                              "--addresses",
                                              "-e",
                                              "--eval",
                                              "-l",
                                              "--load",
                                              "--script",
                                              "-z",
                                              "--snapshot-symbols-save",
                                              "--rc",
                                              "-S",
                                              "--seed",
                                              "-f",
                                              "--feature"};
  for (auto arg = options->_KernelArguments.cbegin(), end = options->_KernelArguments.cend(); arg != end; ++arg) {
    if (parameter_required.find(*arg) != parameter_required.end() && (arg + 1) == end) {
      std::cerr << "Missing parameter for " << *arg << " option." << std::endl;
      exit(1);
    }
    if (*arg == "-h" || *arg == "--help") {
      std::cout << help << std::endl;
      exit(0);
    } else if (*arg == "-U" || *arg == "--unpack-faso") {
      clasp_unpack_faso(*++arg);
      exit(0);
    } else if (*arg == "-v" || *arg == "--version") {
      options->printVersion();
      std::cout << std::endl;
      exit(0);
    } else if (*arg == "-a" || *arg == "--addresses") {
      options->_AddressesP = true;
      options->_AddressesFileName = *++arg;
    } else if (*arg == "--noinform") {
      options->_NoInform = true;
    } else if (*arg == "--noprint") {
      options->_NoPrint = true;
    } else if (*arg == "-D" || *arg == "--disable-debugger") {
      options->_DebuggerDisabled = true;
    } else if (*arg == "-G" || *arg == "--dump-debugger-info") {
      core::dumpDebuggingLayouts();
      std::cout << global_python_virtual_machine_codes;
      std::cout << global_python_class_layouts;
      std::exit(0);
    } else if (*arg == "--quit") {
      options->_Interactive = false;
    } else if (*arg == "-N" || *arg == "--non-interactive") {
      options->_DebuggerDisabled = true;
      options->_Interactive = false;
    } else if (*arg == "--rc") {
      options->_RCFileName = *++arg;
    } else if (*arg == "-r" || *arg == "--norc") {
      options->_NoRc = true;
    } else if (*arg == "-w" || *arg == "--wait") {
      options->_PauseForDebugger = true;
    } else if (*arg == "-y" || *arg == "--snapshot-symbols") {
      options->_ExportedSymbolsCheck = true;
    } else if (*arg == "-m" || *arg == "--disable-mpi") {
      options->_DisableMpi = true;
    } else if (*arg == "-v" || *arg == "--version") {
      options->_Version = true;
    } else if (*arg == "-s" || *arg == "--verbose") {
      options->_SilentStartup = false;
    } else if (*arg == "-t" || *arg == "--trampolines") {
      options->_GenerateTrampolines = true;
    } else if (*arg == "-f" || *arg == "--feature") {
      options->_Features.insert(core::lispify_symbol_name(*++arg));
    } else if (*arg == "-n" || *arg == "--noinit") {
      if (options->validStartupTypeOption(*arg)) {
        options->_StartupType = cloNone;
      }
    } else if (*arg == "-I" || *arg == "--ignore-image") {
      if (options->validStartupTypeOption(*arg)) {
        options->_StartupType = cloInitLisp;
      }
    } else if (*arg == "-B" || *arg == "--base") {
      if (options->validStartupTypeOption(*arg)) {
        options->_StartupType = cloBaseImage;
      }
    } else if (*arg == "-i" || *arg == "--image") {
      if (options->validStartupTypeOption(*arg)) {
        options->_StartupFile = *++arg;
        options->_StartupType = cloImageFile;
      } else {
        ++arg;
      }
    } else if (*arg == "--snapshot") {
      if (options->validStartupTypeOption(*arg)) {
        options->_StartupFile = *++arg;
        options->_StartupType = cloSnapshotFile;
      } else {
        ++arg;
      }
    } else if (*arg == "-L" || *arg == "--llvm-debug") {
      const char* bogus_args[3];
      bogus_args[0] = "clasp";
      bogus_args[1] = "--debug-only";
      char* opt = (char*)malloc((++arg)->size() + 1);
      strcpy(opt, arg->c_str());
      bogus_args[2] = opt;
      printf("%s:%d:%s Passing arguments: <%s>\n", __FILE__, __LINE__, __FUNCTION__, bogus_args[2]);
      llvm::cl::ParseCommandLineOptions(3, bogus_args, "clasp");
    } else if (*arg == "-g" || *arg == "--debug") {
      options->_HasDescribeFile = true;
      options->_DescribeFile = "/tmp/clasp-layout.py";
    } else if (*arg == "-d" || *arg == "--describe") {
      options->_HasDescribeFile = true;
      options->_DescribeFile = *++arg;
    } else if (*arg == "-e" || *arg == "--eval") {
      options->_LoadEvalList.push_back(pair<LoadEvalEnum, std::string>(std::make_pair(cloEval, *++arg)));
    } else if (*arg == "-l" || *arg == "--load") {
      options->_LoadEvalList.push_back(pair<LoadEvalEnum, std::string>(std::make_pair(cloLoad, *++arg)));
    } else if (*arg == "--script") {
      options->_NoInform = true;
      options->_NoRc = true;
      options->_NoPrint = true;
      options->_DebuggerDisabled = true;
      options->_Interactive = false;
      options->_LoadEvalList.push_back(pair<LoadEvalEnum, std::string>(std::make_pair(cloScript, *++arg)));
    } else if (*arg == "-S" || *arg == "--seed") {
      options->_RandomNumberSeed = atoi((*++arg).c_str());
    } else {
      fmt::print(std::cerr, "{}: unrecognized option '{}'\n", gctools::program_name(), *arg);
      exit(1);
    }
  }

  if (!options->_NoInform) {
    fmt::print("Starting ");
    options->printVersion();
    switch (options->_StartupType) {
    case cloNone:
      fmt::print(" with no init\n");
      break;
    case cloInitLisp:
      fmt::print(" from init.lisp\n");
      break;
    case cloBaseImage:
      fmt::print(" from base image\n");
      break;
    case cloExtensionImage:
      fmt::print(" from extension image\n");
      break;
    case cloImageFile:
      fmt::print(" from image file {}\n", options->_StartupFile);
      break;
    case cloSnapshotFile:
      fmt::print(" from snapshot file {}\n", options->_StartupFile);
      break;
    case cloEmbeddedSnapshot:
      fmt::print(" from embedded snapshot\n");
      break;
    }
  }
}

CommandLineOptions::CommandLineOptions(int argc, const char* argv[])
    : _ProcessArguments(process_clasp_arguments), _DisableMpi(false), _AddressesP(false), _StartupType(DEFAULT_STARTUP_TYPE),
      _FreezeStartupType(false), _HasDescribeFile(false), _StartupFile(""), _ExportedSymbolsCheck(false),
      _RandomNumberSeed(0), _NoInform(false), _NoPrint(false), _DebuggerDisabled(false),
      _Interactive(true), _Version(false), _SilentStartup(true), _GenerateTrampolines(false),
      _RCFileName(std::string(getenv("HOME")) + "/.clasprc"), _NoRc(false), _PauseForDebugger(false) {
  if (argc == 0) {
    this->_RawArguments.push_back("./");
  } else {
    bool lisp_arg = false;
    for (int i = 0; i < argc; ++i) {
      this->_RawArguments.push_back(argv[i]);
      if (i == 0)
        continue;
      if (lisp_arg) {
        this->_LispArguments.push_back(argv[i]);
      } else if (this->_RawArguments[i] == "--") {
        lisp_arg = true;
      } else {
        this->_KernelArguments.push_back(argv[i]);
      }
    }
  }
  this->_ExecutableName = this->_RawArguments[0];
  const char* environment_features = getenv("CLASP_FEATURES");
  if (environment_features) {
    vector<string> features = core::split(std::string(environment_features), " ,");
    for (auto feature : features) {
      if (!feature.empty()) {
        this->_Features.insert(core::lispify_symbol_name(feature));
      }
    }
  }
}

DOCGROUP(clasp);
CL_DEFUN List_sp core__command_line_load_eval_sequence() {
  List_sp loadEvals = nil<T_O>();
  for (auto it : global_options->_LoadEvalList) {
    Cons_sp one;
    switch (it.first) {
    case cloEval:
      one = Cons_O::create(kw::_sym_eval, SimpleBaseString_O::make(it.second));
      break;
    case cloLoad:
      one = Cons_O::create(kw::_sym_load, SimpleBaseString_O::make(it.second));
      break;
    case cloScript:
      one = Cons_O::create(kw::_sym_script, SimpleBaseString_O::make(it.second));
      break;
    default:
      SIMPLE_ERROR("Unknown load type {} for {}%N", it.first, it.second);
      break;
    }
    loadEvals = Cons_O::create(one, loadEvals);
  }
  return cl__nreverse(loadEvals);
}

void maybeHandleAddressesOption(CommandLineOptions* options) {
  if (options->_AddressesP) {
    FILE* fout = fopen(options->_AddressesFileName.c_str(), "w");
    fprintf(fout, "# Generating addresses from %s\n", __FUNCTION__);
    snapshotSaveLoad::SymbolLookup lookup;
    lookup.addAllLibraries(fout);
    // snapshotSaveLoad::loadExecutableSymbolLookup(lookup, fout);
    fclose(fout);
  }
}

}; // namespace core
