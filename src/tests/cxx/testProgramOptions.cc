/*
    File: testProgramOptions.cc
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
#include "boost/program_options.hpp"

#define ERROR_IN_COMMAND_LINE 1
#define ERROR_UNHANDLED_EXCEPTION 2


int main(int argc, char* argv[])
{
    try 
    { 
	/** Define and parse the program options 
	 */ 
	namespace po = boost::program_options; 
	po::options_description desc("Options"); 
#ifdef SIMPLE
	desc.add_options() 
	    ("help", "Print help messages") 
	    ("add", "additional options") 
	    ("like", "this"); 
	po::variables_map vm; 
#else
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
#endif

	try 
	{ 
#ifdef SIMPLE
	    po::store(po::parse_command_line(argc, argv, desc), vm); // can throw 
#else
	    po::store(parsed,vm);
#endif
 
	    /** --help option 
	     */ 
	    if ( vm.count("help")  ) 
	    { 
		std::cout << "Basic Command Line Parameter App" << std::endl 
			  << desc << std::endl; 
		return 0;
	    } 
 
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
