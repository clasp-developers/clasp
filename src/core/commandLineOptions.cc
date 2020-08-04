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
#include <clasp/core/commandLineOptions.h>

namespace core {

bool global_debug_byte_code = false;



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
             "-i/--image file      - Use the file as the boot image\n"
             "-d/--describe file   - Describe the clasp data structures for lldb Python API\n"
             "-t/--stage (a|b|c)   - Start the specified stage of clasp 'c' is default\n"
             "-U/--unpack-faso (faso-file) - Unpack the faso file into separate object files\n"
             "--noinform           - Don't print startup banner text\n"
             "--noprint            - Don't prompt or print in read-eval loop\n"
             "-D/--disable-debugger - If the default debugger would be entered, Clasp instead quits\n"
             "--quit               - Don't start a REPL\n"
             "-N/--non-interactive - Short for --disable-debugger --quit\n"
             "-m/--disable-mpi     - Don't use mpi even if built with mpi\n"
             "-v/--version         - Print version\n"
             "--resource-dir       - Options directory is treated as the executable directory\n"
             "                       and it is used to start the search for resource directories\n"
             "-s/--verbose         - Print more info while booting\n"
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
             " debug-startup       - Print a message for every top level form at startup (requires DEBUG_SLOW)\n"
             " debug-startup-verbose - Print a message for every top level form and literal read at startup (requires DEBUG_SLOW)\n"
             " debug-run-clang     - Print every clang invocation\n"
             " exit-backtrace      - Print a backtrace if a non-zero exit code is used to exit\n"
             " jit-log-symbols     - Generate a log of JITted symbols\n"
             " pause-pid           - Print the PID and pause at startup for a debugger to attach\n"
             " ignore-extensions   - Ignore any extensions startup scripts\n"
             " clasp-builder-repl  - Stop in the clasp builder repl to debug bootstrapping\n"
             " use-human-readable-bitcode - Write .ll files instead of .bc files\n"
             " disable-profiling   - Set cmp::*enable-profiling* to NIL and \n"
             " disable-dbg-generate-dwarf   - Set cmp::*dbg-generate-dwarf* to NIL \n"
             "                       disable generation of DWARF metadata during compilations\n"
             "Environment variables:\n"
             "export CLASP_DEBUG=<file-names-space-or-comma-separated>  Define files that\n"
             "                        generate log info when DEBUG_LEVEL_FULL is set at top of file.\n"
             "export CLASP_DONT_HANDLE_CRASH_SIGNALS=1  Don't insert signal handlers for crash signals.\n"
             "export CLASP_GC_MESSAGES=1 Print a message when garbage collection takes place.\n"
             "export CLASP_HOME=<dir>   Define where clasp source code lives\n"
             "export CLASP_OPTIMIZATION_LEVEL=0|1|2|3 Set the llvm optimization level for compiled code\n"
             "export CLASP_TRAP_INTERN=PKG:SYMBOL Trap the intern of the symbol\n"
             "export CLASP_VERBOSE_BUNDLE_SETUP   Dump info during bundle setup\n"
             "export CLASP_DEBUG_BYTE_CODE   Dump info during startup for every byte-code\n"
             "export CLASP_PAUSE_STARTUP (set to anything)  Pause right at startup\n"
             "export CLASP_DUMP_FUNCTIONS (set to anything)  Dump all function definitions at startup\n"
             "export CLASP_TELEMETRY_MASK=1  #turn on telemetry for (1=gc,2=stack)\n"
             "export CLASP_TELEMETRY_FILE=/tmp/clasp.tel # (file to write telemetry)\n"
             "export CLASP_QUICKLISP_DIRECTORY=<dir> # (directory that contains quicklisp setup.lisp)\n"
             "export CLASP_FEATURES=clasp-builder-repl  # Set *features* (separate multiple features with spaces)\n"
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
#ifdef USE_BOEHM
      std::cout << "-boehm-";
#endif
      std::cout << CLASP_VERSION << std::endl;
      exit(0);
    }
    else if (arg == "-I" || arg == "--ignore-image") {
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
    } else if (arg == "--rc") {
      options->_RCFileName = options->_RawArguments[iarg+1];
      iarg++;
    } else if (arg == "-r" || arg == "--norc") {
      options->_NoRc = true;
    } else if (arg == "-w" || arg == "--wait") {
      options->_PauseForDebugger = true;
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
      options->_HasImageFile = true;
      options->_ImageFile = options->_RawArguments[iarg + 1];
      iarg++;
    } else if (arg == "-d" || arg == "--describe") {
      ASSERTF(iarg < (endArg + 1), BF("Missing argument for --describe,-d"));
      options->_HasDescribeFile = true;
      options->_DescribeFile = options->_RawArguments[iarg + 1];
      iarg++;
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
    _HasImageFile(false),
    _HasDescribeFile(false),
    _Stage('c'),
    _ImageFile(""),
    _GotRandomNumberSeed(false),
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
    }
    iarg++;
  }
}

};
