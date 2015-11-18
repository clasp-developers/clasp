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
    : _TrapIntern(""),
      _DontLoadImage(false),
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

#if 0 // uses boost::program_options which is broken on OS X with -std=c++11
    {
	try 
	{ 
	    /** Define and parse the program options 
	     */ 
	    namespace po = boost::program_options; 
	    po::options_description desc("Options"); 
	    desc.add_options()
		("help,h", "produce help message")
		("image,I","ignore the initialization image - puts :ignore-init-image in *features*")
		("feature,f",po::value< std::vector<std::string> >(),"turn on feature")
		("exec,e",po::value<std::string>(),"execute string")
		("noload,n","dont load the startup code - go straight into repl")
		("seed,s",po::value<int>(),"seed the random number generators")
		("interactive,i","Don't bring up interactive REPL after script/command evaluated")
		;
	    po::parsed_options parsed = po::command_line_parser(argc,argv)
		.options(desc)
//	    .allow_unregistered()
		.run();
	    po::variables_map vm; 

	    try 
	    { 
		po::store(parsed,vm);
 
		/** --help option 
		 */ 
		if ( vm.count("help") )
		{
		    std::stringstream ss;
		    ss << desc;
		    printf("%s\n",ss.str().c_str());
		    return;
		}

		//
		//
		const char* execName = argv[0]+strlen(argv[0])-1;
		while (execName >= argv[0] && *execName != '/') execName--;
		execName++;
		this->_FunctionName = execName;
		this->_RCFileName = KERNEL_NAME "/init.lsp";
		DLINE();
		if ( vm.count("image") )
		{
		    SYMBOL_EXPORT_SC_(KeywordPkg,ignoreInitImage);
		    List_sp features = cl::_sym_STARfeaturesSTAR->symbolValue();
		    features = Cons_O::create(kw::_sym_ignoreInitImage,features);
		    cl::_sym_STARfeaturesSTAR->setf_symbolValue(features);
		}
		DLINE();
		if ( vm.count("feature") )
		{
		    vector<string> feature = vm["feature"].as< vector<string> >();
		    List_sp features = cl::_sym_STARfeaturesSTAR->symbolValue();
		    for ( vector<string>::iterator fit=feature.begin(); fit!=feature.end(); fit++ )
		    {
			if ( (*fit) == "" )
			{
			    SIMPLE_ERROR(BF("You must provide a feature with -+"));
			}
			features = Cons_O::create(_lisp->internKeyword(lispify_symbol_name(*fit)),features);
		    }
		    cl::_sym_STARfeaturesSTAR->setf_symbolValue(features);
		}
		if ( vm.count("noload") )
		{
		    this->_dont_load_startup = true;
		    this->_Interactive = true;
		}
		if ( vm.count("interactive") )
		{
		    this->_Interactive = false;
		}
		if ( vm.count("seed") )
		{
		    string seed= vm["seed"].as<string>();
		    long iseed = atol(seed.c_str());
		    LOG(BF("Parsing option -s %s") % seed.c_str()  );
		    seedRandomNumberGenerators(iseed);
		} else
		{
		    seedRandomNumberGenerators(this->mpiRank());
		}

		//
		// Pass whatever is left over to the Lisp environment
		//
		LOG(BF("Parsing what is left over into lisp environment arguments") );
		Vector_sp args = Cons_O::createFromVectorStringsCommandLineArguments(to_pass_further,_lisp);
		LOG(BF(" Command line arguments are being set in Lisp to: %s") % _rep_(args) );
		this->_Roots._CommandLineArguments = args;

		if ( !compileInputFile ) return;

		//
		// Get the script from the command line or the input-file
		//
		_sym_STARargsSTAR->setf_symbolValue(_Nil<T_O>());

		if ( vm.count("exec") != 0 )
		{
		    string script = vm["exec"].as<string>();
		    this->_ScriptInFile = false;
		    this->_FileNameOrCode = script+"\n";
		} else 
		{
		    LOG(BF("Evaluating first argument as the script name") );
		    _sym_STARargsSTAR->setf_symbolValue(this->_Roots._CommandLineArguments);
//        this->globalEnvironment()->extend(sym,this->_CommandLineArguments);
		    this->_ScriptInFile = true;
		    T_sp cla = oCar(this->_Roots._CommandLineArguments);
		    this->_FileNameOrCode = "";
		    if ( cla.notnilp() ) this->_FileNameOrCode = oCar(this->_Roots._CommandLineArguments).as<Str_O>()->get();
		}
		po::notify(vm); // throws on error, so do after help in case 
		// there are any problems 
	    } 
	    catch(po::error& e) 
	    { 
		std::cerr << "ERROR: " << e.what() << std::endl << std::endl; 
		std::cerr << desc << std::endl; 
		throw(e);
	    } 
 
	    // application code here // 
 
	} 
	catch(std::exception& e) 
	{ 
	    std::cerr << "Unhandled Exception reached the top of main: " 
		      << e.what() << ", application will now exit" << std::endl; 
	    throw(e);
	} 
    }
#else
{
  this->_ExecutableName = argv[0];
  int iarg = 1;
  while (iarg < argc) {
    string arg = argv[iarg];
    if (arg == "-h" || arg == "--help") {
      printf("clasp options\n"
             "-I/--ignore-image    - Don't load the boot image/start with init.lsp\n"
             "-i/--image file      - Use the file as the boot image\n"
             "-N/--non-interactive - Suppress all repls\n"
             "-v/--version         - Print version\n"
             "-s/--verbose         - Print more info while booting\n"
             "-f/--feature feature - Add the feature to *features*\n"
             "-e/--eval {form}     - Evaluate a form\n"
             "-l/--load {file}     - LOAD the file\n"
             "-r/--norc            - Don't load the ~/.clasprc file\n"
             "-n/--noinit          - Don't load the init.lsp (very minimal environment)\n"
             "-S/--seed #          - Seed the random number generator\n"
             "-t/--trap {symbol}   - Trap when a specific symbol is INTERN'd\n"
             "-w/--wait            - Print the PID and wait for the user to hit a key\n"
             "-- {ARGS}*           - Trailing are added to core:*command-line-arguments*\n"
             "*feature* settings\n"
             "  debug-startup      - Print a message for every top level form at startup\n"
             "Environment variables:\n"
             "export CLASP_TELEMETRY_MASK=1  #turn on telemetry for (1=gc,2=stack)\n"
             "export CLASP_TELEMETRY_FILE=/tmp/clasp.tel # (file to write telemetry)\n"
             "# to control MPS\n"
             "export CLASP_MPS_CONFIG=\"32 32 16 80 32 80\" # for lots of GC's\n");
      exit(0);
    } else if (arg == "-I" || arg == "--ignore-image") {
      this->_DontLoadImage = true;
    } else if (arg == "-N" || arg == "--non-interactive") {
      this->_Interactive = false;
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
    } else if (arg == "-t" || arg == "--trap") {
      this->_TrapIntern = argv[iarg + 1];
      iarg++;
    } else if (arg == "-f" || arg == "--feature") {
      ASSERTF(iarg < (argc + 1), BF("Missing argument for --feature,-f"));
      this->_Features.push_back(argv[iarg + 1]);
      iarg++;
    } else if (arg == "-i" || arg == "--image") {
      ASSERTF(iarg < (argc + 1), BF("Missing argument for --image,-i"));
      this->_HasImageFile = true;
      this->_ImageFile = argv[iarg + 1];
      iarg++;
    } else if (arg == "-e" || arg == "--eval") {
      ASSERTF(iarg < (argc + 1), BF("Missing argument for --eval,-e"));
      pair<LoadEvalEnum, std::string> eval(std::make_pair(cloEval, argv[iarg + 1]));
      this->_LoadEvalList.push_back(eval);
      iarg++;
    } else if (arg == "-l" || arg == "--load") {
      ASSERTF(iarg < (argc + 1), BF("Missing argument for --load,-l"));
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
#endif
};
