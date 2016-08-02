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
#include <clasp/core/commandLineOptions.h>

namespace core {

CommandLineOptions::CommandLineOptions(int argc, char *argv[])
    : _DontLoadImage(false),
      _DontLoadInitLsp(false),
      _HasImageFile(false),
      _ImageFile(""),
      _GotRandomNumberSeed(false),
      _RandomNumberSeed(0),
      _Interactive(true),
      _Version(false),
      _SilentStartup(true),
      _NoRc(false),
      _PauseForDebugger(false)

{
  int endArg = argc;
  for (int i = 0; i < argc; ++i) {
    if (strcmp(argv[i], "--") == 0) {
      endArg = i;
    }
  }
  this->_EndArg = endArg;
  this->_ExecutableName = argv[0];
  int iarg = 1;
  while (iarg < endArg) {
    string arg = argv[iarg];
    if (arg == "-h" || arg == "--help") {
      printf("clasp options\n"
             "-I/--ignore-image    - Don't load the boot image/start with init.lsp\n"
             "-i/--image file      - Use the file as the boot image\n"
             "-N/--non-interactive - Suppress all repls\n"
             "-v/--version         - Print version\n"
             "-R/--resource-dir    - This directory is treated as the executable directory\n"
             "                       and it is used to start the search for resource directories\n"
             "-s/--verbose         - Print more info while booting\n"
             "-f/--feature feature - Add the feature to *features*\n"
             "-e/--eval {form}     - Evaluate a form\n"
             "-l/--load {file}     - LOAD the file\n"
             "-r/--norc            - Don't load the ~/.clasprc file\n"
             "-n/--noinit          - Don't load the init.lsp (very minimal environment)\n"
             "-S/--seed #          - Seed the random number generator\n"
             "-w/--wait            - Print the PID and wait for the user to hit a key\n"
             "-- {ARGS}*           - Trailing are added to core:*command-line-arguments*\n"
             "*feature* settings\n"
             "  debug-startup      - Print a message for every top level form at startup\n"
             "Environment variables:\n"
             "export CLASP_TRAP_INTERN=PKG:SYMBOL Trap the intern of the symbol\n"
             "export CLASP_VERBOSE_BUNDLE_SETUP   Dump info during bundle setup\n"
             "export CLASP_TELEMETRY_MASK=1  #turn on telemetry for (1=gc,2=stack)\n"
             "export CLASP_TELEMETRY_FILE=/tmp/clasp.tel # (file to write telemetry)\n"
             "# to control MPS\n"
             "export CLASP_MPS_CONFIG=\"32 32 16 80 32 80 64\" # for lots of GC's\n");
      exit(0);
    } else if (arg == "-I" || arg == "--ignore-image") {
      this->_DontLoadImage = true;
    } else if (arg == "-N" || arg == "--non-interactive") {
      this->_Interactive = false;
    } else if (arg == "-R" || arg == "--resource-dir") {
      this->_ResourceDir = argv[iarg+1];
      iarg++;
    } else if (arg == "-r" || arg == "--norc") {
      this->_NoRc = true;
    } else if (arg == "-w" || arg == "--wait") {
      this->_PauseForDebugger = true;
    } else if (arg == "-n" || arg == "--noinit") {
      this->_DontLoadInitLsp = true;
    } else if (arg == "-v" || arg == "--version") {
      this->_Version = true;
    } else if (arg == "-s" || arg == "--verbose") {
      this->_SilentStartup = false;
    } else if (arg == "-f" || arg == "--feature") {
      ASSERTF(iarg < (endArg + 1), BF("Missing argument for --feature,-f"));
      this->_Features.push_back(argv[iarg + 1]);
      iarg++;
    } else if (arg == "-i" || arg == "--image") {
      ASSERTF(iarg < (endArg + 1), BF("Missing argument for --image,-i"));
      this->_HasImageFile = true;
      this->_ImageFile = argv[iarg + 1];
      iarg++;
    } else if (arg == "-e" || arg == "--eval") {
      ASSERTF(iarg < (endArg + 1), BF("Missing argument for --eval,-e"));
      pair<LoadEvalEnum, std::string> eval(std::make_pair(cloEval, argv[iarg + 1]));
      this->_LoadEvalList.push_back(eval);
      iarg++;
    } else if (arg == "-l" || arg == "--load") {
      ASSERTF(iarg < (endArg + 1), BF("Missing argument for --load,-l"));
      pair<LoadEvalEnum, std::string> eval(std::make_pair(cloLoad, argv[iarg + 1]));
      this->_LoadEvalList.push_back(eval);
      iarg++;
    } else if (arg == "-S" || arg == "--seed") {
      this->_RandomNumberSeed = atoi(argv[iarg + 1]);
      iarg++;
    } else {
      this->_Args.push_back(arg);
    }
    iarg++;
  }
}
};
