/*
    File: commandLineOptions.h
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
#ifndef CommandLineOptions_H
#define CommandLineOptions_H

namespace core {

class CommandLineOptions;

typedef void (*process_arguments_callback)(CommandLineOptions *);

extern bool global_debug_start_code;

typedef enum { cloLoad, cloEval, cloScript } LoadEvalEnum;

typedef enum { cloDefault, cloImage, cloSnapshot } ImageTypeEnum;

struct CommandLineOptions {
  CommandLineOptions(int argc, char *argv[]);
  process_arguments_callback _ProcessArguments;
  std::string _ExecutableName;
  bool _JITLogSymbols;
  bool _IgnoreInitImage;
  bool _IgnoreInitLsp;
  bool _DisableMpi{false};
  std::vector<std::string> _RawArguments;
  std::vector<std::string> _KernelArguments;
  std::vector<std::string> _LispArguments;
  std::set<std::string> _Features;
  std::vector<pair<LoadEvalEnum, std::string>> _LoadEvalList;
  bool _AddressesP;
  std::string _AddressesFileName;
  bool _StartupFileP;
  ImageTypeEnum _StartupFileType;
  std::string _StartupFile;
  ImageTypeEnum _DefaultStartupType;
  bool _HasDescribeFile;
  std::string _DescribeFile;
  char _StartupStage;
  long _RandomNumberSeed;
  bool _ExportedSymbolsAccumulate;
  std::string _ExportedSymbolsFilename;
  bool _NoInform;
  bool _NoPrint;
  bool _DebuggerDisabled;
  bool _Interactive;
  bool _Version;
  bool _SilentStartup;
  std::string _RCFileName;
  bool _NoRc;
  bool _PauseForDebugger;
};

void maybeHandleAddressesOption(CommandLineOptions *options);

}; // namespace core
#endif

