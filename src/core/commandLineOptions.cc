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
#include <boost/program_options.hpp>
#include "clasp/core/compiler.h"
#include "clasp/core/ql.h"
#include "clasp/core/lisp.h"
#include <clasp/gctools/snapshotSaveLoad.h>
#include <clasp/core/commandLineOptions.h>

namespace core {

bool global_debug_byte_code = false;


bool CommandLineOptions::optionArgP(int& iarg,std::string& val, const std::string& default_) {
  // If there are arguments left and it doesn't start with - then
  // treat it like an argument for the option
  if (((iarg+1) >= this->_EndArg) || (this->_RawArguments[iarg+1][0] == '-')) {
    val = default_;
    return false;
  }
  val = this->_RawArguments[iarg+1];
  iarg += 1;
  return true;
}

void process_clasp_arguments(CommandLineOptions* options)
{
  int endArg = options->_RawArguments.size();
  for (int i = 0; i < options->_RawArguments.size(); ++i) {
    if (options->_RawArguments[i] == "--") {
      endArg = i;
    }
  }
  options->_EndArg = endArg;
  options->_ExecutableName = options->_RawArguments[0];
  // The most basic processing of the arguments
  int iarg = 1;
  while (iarg < endArg) {
    string arg = options->_RawArguments[iarg];
    if (arg == "-h" || arg == "--help") {
      printf("clasp options\n"
             "-I/--ignore-image    - Don't load the boot image/start with init.lsp\n"
             "-i/--image file      - Use the file as the boot image. If file ends in .snapshot then treat as a snapshot.\n"
             "-T/--type (default|snapshot|image) - Set the type of the default startup file to use (default means snapshot->image).\n"
             "-L/--llvm-debug (options) - Pass arg to llvm::cl::ParseCommandLineOptions --debug-only. Lets you debug llvm with LLVM_DEBUG(...).\n"
             "-g/--debug           - Describe the clasp data structures for lldb Python API to /tmp/clasp.py\n"
             "-d/--describe [file] - Describe the clasp data structures for lldb Python API [file] default is /tmp/clasp.py\n"
             "-t/--stage (a|b|c)   - Start the specified stage of clasp 'c' is default\n"
             "-U/--unpack-faso (faso-file) - Unpack the faso file into separate object files\n"
             "--noinform           - Don't print startup banner text\n"
             "--noprint            - Don't prompt or print in read-eval loop\n"
             "-D/--disable-debugger - If the default debugger would be entered, Clasp instead quits\n"
             "--quit               - Don't start a REPL\n"
             "-a/--addresses [file]- Dump all symbol addresses in executable to file\n"
             "-N/--non-interactive - Short for --disable-debugger --quit\n"
             "-m/--disable-mpi     - Don't use mpi even if built with mpi\n"
             "-v/--version         - Print version\n"
             "--resource-dir       - Options directory is treated as the executable directory\n"
             "                       and it is used to start the search for resource directories\n"
             "-s/--verbose         - Print more info while booting\n"
             "-y/--snapshot-symbols {file} - Accumulate symbols while starting up and verify\n"
             "                       that snapshot save will work. The symbols can be recovered\n"
             "                       at runtime calling (core:mangled-symbol-names <stream>).\n"
             "                       They are written out to the {file} on exit.\n"
             "-f/--feature feature - Add the feature to *features*\n"
             "-e/--eval {form}     - Evaluate a form\n"
             "-l/--load {file}     - LOAD the file\n"
             "--rc {file}          - Specify name of the RC file (default .clasprc)\n"
             "-r/--norc            - Don't load the RC file\n"
             "-n/--noinit          - Don't load the init.lsp (very minimal environment)\n"
             "-S/--seed #          - Seed the random number generator\n"
             "-w/--wait            - Print the PID and wait for the user to hit a key\n"
             "-- {ARGS}*           - Trailing are added to core:*command-line-arguments*\n"
             "*feature* settings\n"
             " sanitizer=thread    - Setup codegen for thread sanitizer\n"
             " sanitizer=memory    - Setup codegen for memory sanitizer\n"
             " sanitizer=address   - Setup codegen for address sanitizer\n"
             " debug-startup       - Print a message for every top level form at startup (requires DEBUG_SLOW)\n"
             " debug-startup-verbose - Print a message for every top level form and literal read at startup (requires DEBUG_SLOW)\n"
             " debug-run-clang     - Print every clang invocation\n"
             " compile-file-debug-dump-module - Dump compile-file modules\n"
             " compile-debug-dump-module - Dump compile modules\n"
             " exit-backtrace      - Print a backtrace if a non-zero exit code is used to exit\n"
             " pause-pid           - Print the PID and pause at startup for a debugger to attach\n"
             " ignore-extensions   - Ignore any extensions startup scripts\n"
             " clasp-builder-repl  - Stop in the clasp builder repl to debug bootstrapping\n"
             " use-human-readable-bitcode - Write .ll files instead of .bc files\n"
             " disable-profiling   - Set cmp::*enable-profiling* to NIL and \n"
             " disable-dbg-generate-dwarf   - Set cmp::*dbg-generate-dwarf* to NIL \n"
             "                       disable generation of DWARF metadata during compilations\n"
             " force-compile-file-serial - Force compile-file-serial to be used for compilation\n"
             " dont-start-cando-user - Cando option that disables start-cando-user - lets you start\n"
             "                         slime and THEN start cando with (start-cando-user)\n"
             "Environment variables:\n"
             "export CLASP_DEBUG=<file-names-space-or-comma-separated>  Define files that\n"
             "                        generate log info when DEBUG_LEVEL_FULL is set at top of file.\n"
             "export CLASP_DEBUGGER_SUPPORT=1 Generate files that lldb/gdb/udb can use to debug clasp.\n"
             "export CLASP_NO_JIT_GDB=1 Don't register object files with gdb/lldb for source level debugging.\n"
             "export CLASP_SNAPSHOT=1  Debug snapshot generation.\n"
             "export CLASP_DONT_HANDLE_CRASH_SIGNALS=1  Don't insert signal handlers for crash signals.\n"
             "export CLASP_GC_MESSAGES=1 Print a message when garbage collection takes place.\n"
             "export CLASP_HOME=<dir>   Define where clasp source code lives\n"
             "export CLASP_TIME_SNAPSHOT=1   Turn on timing of snapshot load\n"
             "export CLASP_JIT_LOG_SYMBOLS=1  Turn on generation of /tmp/perf-<pid>.map file\n"
             "export CLASP_OPTIMIZATION_LEVEL=0|1|2|3 Set the llvm optimization level for compiled code\n"
             "export CLASP_TRAP_INTERN=PKG:SYMBOL Trap the intern of the symbol\n"
             "export CLASP_VERBOSE_BUNDLE_SETUP   Dump info during bundle setup\n"
             "export CLASP_DEBUG_BYTE_CODE   Dump info during startup for every byte-code\n"
             "export CLASP_DEBUG_SNAPSHOT  Dump info during snapshot loading\n"
             "export CLASP_DEBUG_OBJECT_FILES \"save\" saves all object files, anything else prints info about object file generation\n"
             "export CLASP_PAUSE_STARTUP (set to anything)  Pause right at startup before basic initialization\n"
             "export CLASP_PAUSE_OBJECTS_ADDED (set to anything)  Pause right at startup during snapshot load after objects are added to the jit\n"
             "export CLASP_PAUSE_INIT (set to anything)  Pause after startup and after basic initialization\n"
             "export CLASP_PAUSE_FORKED_CHILD (set to anything)  Pause forked children until they receive SIGUSR1\n"
             "export CLASP_DUMP_FUNCTIONS (set to anything)  Dump all function definitions at startup\n"
             "export CLASP_TELEMETRY_MASK=1  #turn on telemetry for (1=gc,2=stack)\n"
             "export CLASP_TELEMETRY_FILE=/tmp/clasp.tel # (file to write telemetry)\n"
             "export CLASP_QUICKLISP_DIRECTORY=<dir> # (directory that contains quicklisp setup.lisp)\n"
             "export CLASP_FEATURES=clasp-builder-repl  # Set *features* (separate multiple features with spaces)\n"
             "export CLASP_FEATURES=dump-repl-object-files  # Dump all repl object files to /tmp/clasp_repl_xxx.o\n"
             "export CLASP_FEATURES=debug-object-files  # Dump lots of info on object file creation\n"
             "export CLASP_JIT_THREADS=1... # Define the number of threads the JIT will use\n"
             "export CLASP_MEMORY_PROFILE <size-threshold> <number-theshold> # Options means call \n"
             "                      # HitAllocationSizeThreshold every time 16000000 bytes are allocated\n"
             "                      # and call HitAllocationNumberThreshold every time 1024 allocations take place\n"
             "export CLASP_BACKTRACE_ALLOCATIONS <stamp-val> # generate a backtrace to /tmp/stamp<stamp-val>.backtraces\n"
             "                      # everytime a <stamp-val> object is allocates (VERY EXPENSIVE)\n"
             "# to control MPS\n"
             "export CLASP_MPS_CONFIG=\"320 320 6400 80 25600 50\" \n"
             "                        \"32 32 16 80 32 80 64\" # for lots of GC's\n"
             "       # these values are:\n"
             "       # arenaMb, spareCommitLimitMb, nurseryKb, nurseryMortalityPercent, \n"
             "       #   generation1Kb, generation1MortalityPercent, keyExtendByKb\n"
             );
      exit(0);
    } else if (arg == "-U" || arg == "--unpack-faso") {
      if (iarg+1<endArg) {
        std::string faso_name = options->_RawArguments[iarg+1];
        clasp_unpack_faso(faso_name);
      }
      iarg++;
      exit(0);
    } else if (arg == "-v" || arg == "--version") {
      std::cout << program_name();
#ifdef USE_MPS
      std::cout << "-mps-";
#endif
#if defined(USE_BOEHM)
# ifdef USE_PRECISE_GC 
      std::cout << "-boehmprecise-";
# else
      std::cout << "-boehm-";
# endif
#elif defined(USE_MMTK)
# ifdef USE_PRECISE_GC 
      std::cout << "-mmtkprecise-";
# else
      std::cout << "-mmtk-";
# endif
#endif
      std::cout << CLASP_VERSION << std::endl;
      exit(0);
    }
    else if (arg == "-a" || arg == "--addresses") {
      string filename = options->_RawArguments[iarg+1];
      iarg++;
      options->_AddressesP = true;
      options->_AddressesFileName = filename;
    } else if (arg == "-I" || arg == "--ignore-image") {
      options->_DontLoadImage = true;
    } else if (arg == "--noinform") {
      options->_NoInform = true;
    } else if (arg == "--noprint") {
      options->_NoPrint = true;
    } else if (arg == "-D" || arg == "--disable-debugger") {
      options->_DebuggerDisabled = true;
    } else if (arg == "--quit") {
      options->_Interactive = false;
    } else if (arg == "-N" || arg == "--non-interactive") {
      options->_DebuggerDisabled = true;
      options->_Interactive = false;
    } else if (arg == "-R" || arg == "--resource-dir") {
      options->_ResourceDir = options->_RawArguments[iarg+1];
      iarg++;
    } else if (arg == "-T" || arg == "--type") {
      std::string type = options->_RawArguments[iarg+1];
      if (type == "default" || type == "DEFAULT" || type == "Default" ) {
        options->_DefaultStartupType = cloDefault;
      } else if (type == "snapshot" || type == "SNAPSHOT" || type == "Snapshot" ) {
        options->_DefaultStartupType = cloSnapshot;
      } else if (type == "image" || type == "IMAGE" || type == "Image" ) {
        options->_DefaultStartupType = cloImage;
      } else {
        options->_DefaultStartupType = cloDefault;
      }
      iarg++;
    } else if (arg == "--rc") {
      options->_RCFileName = options->_RawArguments[iarg+1];
      iarg++;
    } else if (arg == "-r" || arg == "--norc") {
      options->_NoRc = true;
    } else if (arg == "-w" || arg == "--wait") {
      options->_PauseForDebugger = true;
    } else if (arg == "-y" || arg == "--snapshot-symbols") {
      options->_ExportedSymbolsAccumulate = true;
      options->_ExportedSymbolsFilename = options->_RawArguments[iarg+1];
      iarg++;
    } else if (arg == "-m" || arg == "--disable-mpi") {
      options->_DisableMpi = true;
    } else if (arg == "-n" || arg == "--noinit") {
      options->_DontLoadInitLsp = true;
    } else if (arg == "-v" || arg == "--version") {
      options->_Version = true;
    } else if (arg == "-s" || arg == "--verbose") {
      options->_SilentStartup = false;
    } else if (arg == "-f" || arg == "--feature") {
      ASSERTF(iarg < (endArg + 1), BF("Missing argument for --feature,-f"));
      options->_Features.push_back(options->_RawArguments[iarg + 1]);
      iarg++;
    } else if (arg == "-i" || arg == "--image") {
      ASSERTF(iarg < (endArg + 1), BF("Missing argument for --image,-i"));
      options->_StartupFileP = true;
      std::string startupFile = options->_RawArguments[iarg + 1];
      iarg++;
      options->_StartupFile = startupFile;
      if (startupFile.compare(startupFile.length()-9,9,".snapshot")==0) {
        options->_StartupFileType = cloSnapshot;
      } else {
        options->_StartupFileType = cloImage;
      }
    } else if (arg == "-L" || arg == "--llvm-debug") {
      ASSERTF(iarg < (endArg + 1), BF("Missing argument for --eval,-e"));
      std::string args = options->_RawArguments[iarg+1];
      const char* bogus_args[3];
      bogus_args[0] = "clasp";
      bogus_args[1] = "--debug-only";
      char* opt = (char*)malloc(args.size()+1);
      strcpy(opt,args.c_str());
      bogus_args[2] = opt;
      printf("%s:%d:%s Passing arguments: <%s>\n", __FILE__, __LINE__, __FUNCTION__, bogus_args[2] );
      llvm::cl::ParseCommandLineOptions(3,bogus_args,"clasp");
      iarg++;
    } else if (arg == "-g" || arg == "--debug") {
      options->_HasDescribeFile = true;
      options->_DescribeFile = "/tmp/clasp-layout.py";
    } else if (arg == "-d" || arg == "--describe") {
      ASSERTF(iarg < (endArg + 1), BF("Missing argument for --describe,-d"));
      options->_HasDescribeFile = true;
      options->optionArgP(iarg,options->_DescribeFile,"/tmp/clasp_layout.py");
    } else if (arg == "-t" || arg == "--stage") {
      ASSERTF(iarg < (endArg + 1), BF("Missing argument for --stage,-t"));
      options->_Stage = options->_RawArguments[iarg + 1][0];
      iarg++;
    } else if (arg == "-e" || arg == "--eval") {
      ASSERTF(iarg < (endArg + 1), BF("Missing argument for --eval,-e"));
      pair<LoadEvalEnum, std::string> eval(std::make_pair(cloEval, options->_RawArguments[iarg + 1]));
      options->_LoadEvalList.push_back(eval);
      iarg++;
    } else if (arg == "-l" || arg == "--load") {
      ASSERTF(iarg < (endArg + 1), BF("Missing argument for --load,-l"));
      pair<LoadEvalEnum, std::string> eval(std::make_pair(cloLoad, options->_RawArguments[iarg + 1]));
      options->_LoadEvalList.push_back(eval);
      iarg++;
    } else if (arg == "-S" || arg == "--seed") {
      options->_RandomNumberSeed = atoi(options->_RawArguments[iarg + 1].c_str());
      iarg++;
    } else {
      options->_Args.push_back(arg);
    }
    iarg++;
  }
}


CommandLineOptions::CommandLineOptions(int argc, char *argv[])
  : _ProcessArguments(process_clasp_arguments),
    _DontLoadImage(false),
    _DontLoadInitLsp(false),
    _DisableMpi(false),
    _AddressesP(false),
    _StartupFileP(false),
    _StartupFileType(cloDefault),
    _HasDescribeFile(false),
    _Stage('c'),
    _StartupFile(""),
    _DefaultStartupType(cloDefault),
    _ExportedSymbolsAccumulate(false),
    _RandomNumberSeed(0),
    _NoInform(false),
    _NoPrint(false),
    _DebuggerDisabled(false),
    _Interactive(true),
    _Version(false),
    _SilentStartup(true),
    _RCFileName(".clasprc"),
    _NoRc(false),
    _PauseForDebugger(false)

{
  for (int i = 0; i < argc; ++i) {
    this->_RawArguments.push_back(argv[i]);
  }
  this->_ExecutableName = this->_RawArguments[0];
  // --resource-dir is the one argument we must process now
  int iarg = 1;
  while ( iarg<this->_RawArguments.size() && this->_RawArguments[iarg] != "--" ) {
    std::string arg = this->_RawArguments[iarg];
    if (arg == "--resource-dir") {
      this->_ResourceDir = this->_RawArguments[iarg+1];
      iarg++;
    } else if (arg == "--verbose") {
      this->_SilentStartup = false;
      iarg++;
    } else if (arg == "--snapshot-symbols") {
      this->_ExportedSymbolsAccumulate = true;
      iarg++;
      this->_ExportedSymbolsFilename = this->_RawArguments[iarg];
      iarg++;
    }
    iarg++;
  }
}




DOCGROUP(clasp)
CL_DEFUN List_sp core__command_line_load_eval_sequence() {
  List_sp loadEvals = nil<T_O>();
  for (auto it : global_options->_LoadEvalList) {
    Cons_sp one;
    if (it.first == cloEval) {
      one = Cons_O::create(kw::_sym_eval, SimpleBaseString_O::make(it.second));
    } else {
      one = Cons_O::create(kw::_sym_load, SimpleBaseString_O::make(it.second));
    }
    loadEvals = Cons_O::create(one, loadEvals);
  }
  return cl__nreverse(loadEvals);
}


void maybeHandleAddressesOption(CommandLineOptions* options) {
  if (options->_AddressesP) {
    FILE* fout = fopen(options->_AddressesFileName.c_str(),"w");
    fprintf(fout, "# Generating addresses from %s\n", __FUNCTION__ );
    snapshotSaveLoad::SymbolLookup lookup;
    lookup.addAllLibraries(fout);
      // snapshotSaveLoad::loadExecutableSymbolLookup(lookup, fout);
    fclose(fout);
  }
}

};
