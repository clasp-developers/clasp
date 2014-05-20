
#include <iostream>
#include "boost/program_options.hpp"
#include "commandLineOptions.h"

namespace core {


    void CommandLineOptions::process(int argc, char* argv[])
    {
	try 
	{ 
	    /** Define and parse the program options 
	     */ 
	    namespace po = boost::program_options; 
	    po::options_description desc("Options"); 
	    desc.add_options()
		("help,h", "produce help message")
		("exec,e",po::value<std::string>(),"execute string")
		("feature,f",po::value< std::vector<std::string> >(),"turn on feature")
		("noload,n","dont load the startup code - go straight into repl")
		("core,c","Use the specified core file (defines macros/system functions)  rather than the default")
		("image,I","ignore the initialization image - puts :ignore-init-image in *features*")
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
		    stringstream ss;
		    ss << desc;
		    _lisp->print(BF("%s")% ss.str());
		    return;
		}

#if 0
		//
		//
		const char* execName = argv[0]+strlen(argv[0])-1;
		while (execName >= argv[0] && *execName != '/') execName--;
		execName++;
		this->_FunctionName = execName;
		this->_RCFileName = EXECUTABLE_NAME "/init.lsp";
		DLINE();
		if ( vm.count("image") )
		{
		    SYMBOL_EXPORT_SC_(KeywordPkg,ignoreInitImage);
		    Cons_sp features = cl::_sym_STARfeaturesSTAR->symbolValue().as<Cons_O>();
		    features = Cons_O::create(kw::_sym_ignoreInitImage,features,_lisp);
		    cl::_sym_STARfeaturesSTAR->setf_symbolValue(features);
		}
		DLINE();
		if ( vm.count("feature") )
		{
		    vector<string> feature = vm["feature"].as< vector<string> >();
		    Cons_sp features = cl::_sym_STARfeaturesSTAR->symbolValue().as<Cons_O>();
		    for ( vector<string>::iterator fit=feature.begin(); fit!=feature.end(); fit++ )
		    {
			if ( (*fit) == "" )
			{
			    SIMPLE_ERROR(BF("You must provide a feature with -+"));
			}
			features = Cons_O::create(_lisp->internKeyword(lispify_symbol_name(*fit)),features,_lisp);
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
		Cons_sp args = Cons_O::createFromVectorStringsCommandLineArguments(to_pass_further,_lisp);
		LOG(BF(" Command line arguments are being set in Lisp to: %s") % _rep_(args) );
		this->_Roots._CommandLineArguments = args;

		if ( !compileInputFile ) return;

		//
		// Get the script from the command line or the input-file
		//
		if ( vm.count("exec") != 0 )
		{
		    string script = vm["exec"].as<string>();
		    this->_ScriptInFile = false;
		    this->_FileNameOrCode = script+"\n";
		} else 
		{
		    LOG(BF("Evaluating first argument as the script name") );
		    Symbol_sp sym = _sym_STARARGSSTAR;
		    LOG(BF("Binding symbol(%s) to: %s") % sym->fullName() % _rep_(this->_CommandLineArguments) );
		    sym->setf_symbolValue(this->_Roots._CommandLineArguments);
//        this->globalEnvironment()->extend(sym,this->_CommandLineArguments);
		    this->_ScriptInFile = true;
		    T_sp cla = oCar(this->_Roots._CommandLineArguments);
		    this->_FileNameOrCode = "";
		    if ( cla.notnilp() ) this->_FileNameOrCode = oCar(this->_Roots._CommandLineArguments).as<Str_O>()->get();
		}
#endif 
		po::notify(vm); // throws on error, so do after help in case 
		// there are any problems 
	    } 
	    catch(po::error& e) 
	    { 
		std::cerr << "ERROR: " << e.what() << std::endl << std::endl; 
		std::cerr << desc << std::endl; 
		return ERROR_IN_COMMAND_LINE; 
	    } 
 
	    // application code here // 
 
	} 
	catch(std::exception& e) 
	{ 
	    std::cerr << "Unhandled Exception reached the top of main: " 
		      << e.what() << ", application will now exit" << std::endl; 
	    return ERROR_UNHANDLED_EXCEPTION; 
 	} 
 
	return 0; 
    }


};
