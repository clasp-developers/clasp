/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/


#include "vt_dyn.h"

#include "BPatch_module.h"
#include "BPatch_point.h"
#include "BPatch_process.h"
#include "BPatch_snippet.h"
#include "BPatch_statement.h"

#include <fstream>
#include <map>
#include <sstream>
#include <ctype.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>

// local functions
//

// parse command line options
static bool parseCommandLine( int argc, char ** argv );

// show usage text
static void showUsage( void );

// print verbose message
static void vPrint( uint8_t level, const char * fmt, ... );

// local variables
//

// output stream for verbose messages
// (stdout if vtdyn is started from the command line, otherwise stderr)
static FILE * verboseStream = stdout;

// global variables
//

const std::string ExeName = "vtdyn"; // name of program's (mutator) executable
const int ExePid = getpid();         // mutator's PID
ParamsS Params;                      // mutator's parameters
MutatorC * theMutator;               // instance of class MutatorC

int
main( int argc, char ** argv )
{
   int rc = 0;

   do
   {
      // parse command line options
      //
      if( !parseCommandLine( argc, argv ) )
      {
         rc = 1;
         break;
      }

      // show VT version, if desired
      //
      if( Params.show_version )
      {
         std::cout << PACKAGE_VERSION << std::endl;
         break;
      }

      // show usage text, if necessary/desired
      //
      if( Params.show_usage ||
          ( Params.mutatee.length() == 0 && Params.mutatee_pid == -1 ) )
      {
         showUsage();
         break;
      }

      // create instance of class MutatorC
      //
      theMutator = new MutatorC();
      vt_assert( theMutator );

      // start mutation
      //
      rc = theMutator->run() ? 0 : 1;

      // cleanup
      delete theMutator;

   } while( false );

   return rc;
}

static bool
parseCommandLine( int argc, char ** argv )
{
   bool error = false;

   int i, j;

   for( i = 1; i < argc; i++ )
   {
      // -h, --help
      //
      if( strcmp( argv[i], "-h" ) == 0
          || strcmp( argv[i], "--help" ) == 0 )
      {
         Params.show_usage = true;
         break;
      }
      // -V, --version
      //
      else if( strcmp( argv[i], "-V" ) == 0
               || strcmp( argv[i], "--version" ) == 0 )
      {
         Params.show_version = true;
         break;
      }
      // -v, --version
      //
      else if( strcmp( argv[i], "-v" ) == 0
               || strcmp( argv[i], "--verbose" ) == 0 )
      {
         Params.verbose_level++;
      }
      // -q, --quiet
      //
      else if( strcmp( argv[i], "-q" ) == 0
               || strcmp( argv[i], "--quiet" ) == 0 )
      {
         Params.verbose_level = 0;
      }
      // -o, --output
      //
      else if( strcmp( argv[i], "-o" ) == 0
               || strcmp( argv[i], "--output" ) == 0 )
      {
         if( i == argc - 1 )
         {
            std::cerr << ExeName << ": [" << ExePid << "]: FILE "
                      << "expected -- " << argv[i] << std::endl;
            error = true;
            break;
         }

         Params.outfile = argv[++i];
         Params.mode = MODE_REWRITE;
      }
      // -s, --shlibs
      //
      else if( strcmp( argv[i], "-s" ) == 0
               || strcmp( argv[i], "--shlibs" ) == 0 )
      {
         if( i == argc - 1 )
         {
            std::cerr << ExeName << ": [" << ExePid << "]: SHLIBS expected "
                      << "-- " << argv[i] << std::endl;
            error = true;
            break;
         }

         char shlibs_str[STRBUFSIZE];
         char * tk;

         strcpy( shlibs_str, argv[++i] );
         tk = strtok( shlibs_str, "," );
         do
         {
            Params.shlibs.push_back( tk );
         } while( (tk = strtok( 0, "," )) );
      }
      // --f, --filter
      //
      else if( strcmp( argv[i], "-f" ) == 0
               || strcmp( argv[i], "--filter" ) == 0 )
      {
         if( i == argc - 1 )
         {
            std::cerr << ExeName << ": [" << ExePid << "]: FILE expected "
                      << "-- " << argv[i] << std::endl;
            error = true;
            break;
         }

         Params.filtfile = argv[++i];
      }
      // --outer-loops
      //
      else if( strcmp( argv[i], "--outer-loops" ) == 0 )
      {
         Params.outer_loops = true;
      }
      // --inner-loops
      //
      else if( strcmp( argv[i], "--inner-loops" ) == 0 )
      {
         Params.outer_loops = Params.inner_loops = true;
      }
      // --loop-iters
      //
      else if( strcmp( argv[i], "--loop-iters" ) == 0 )
      {
         Params.outer_loops = Params.loop_iters = true;
      }
      // --ignore-nodbg
      //
      else if( strcmp( argv[i], "--ignore-nodbg" ) == 0 )
      {
         Params.ignore_no_dbg = true;
      }

      // hidden options - only for using within the VampirTrace library
      //

      // -p, --pid
      //
      else if( strcmp( argv[i], "-p" ) == 0
               || strcmp( argv[i], "--pid" ) == 0 )
      {
         if( i == argc - 1 )
         {
            std::cerr << ExeName << ": [" << ExePid << "]: PID expected "
                      << "-- " << argv[i] << std::endl;
            error = true;
            break;
         }

         Params.mutatee_pid = atoi( argv[++i] );
         Params.mode = MODE_ATTACH;

         // vtdyn is obvious started from the Dyninst attach library
         // (libvt-dynatt); set output stream for verbose messages to stderr
         verboseStream = stderr;
      }
      // --nodetach
      //
      else if( strcmp( argv[i], "--nodetach" ) == 0 )
      {
          Params.detach = false;
      }

      // <executable> [arguments ...]
      //
      else
      {
         if( Params.mutatee.length() == 0 )
         {
            Params.mutatee = argv[i];
            Params.mutatee_args.push_back( Params.mutatee );
         }
         else
         {
            for( j = 1; i < argc; i++, j++ )
               Params.mutatee_args.push_back( argv[i] );
            break;
         }
      }
   }

   return !error;
}

static void
showUsage()
{
   std::cout << std::endl
      << " " << ExeName << " - binary instrumentor (Dyninst mutator) for VampirTrace." << std::endl
      << std::endl
      << " Syntax: " << ExeName << " [options] <executable> [arguments ...]" << std::endl
      << std::endl
      << "   options:" << std::endl
      << "     -h, --help          Show this help message." << std::endl
      << std::endl
      << "     -V, --version       Show VampirTrace version." << std::endl
      << std::endl
      << "     -v, --verbose       Increase output verbosity." << std::endl
      << "                         (can be used more than once)" << std::endl
      << std::endl
      << "     -q, --quiet         Enable quiet mode." << std::endl
      << "                         (only emergency output)" << std::endl
      << std::endl
      << "     -o, --output FILE   Rewrite instrumented executable to specified pathname." << std::endl
      << std::endl
      << "     -f, --filter FILE   Pathname of input filter file." << std::endl
      << std::endl
      << "     -s, --shlibs LIB[,...]" << std::endl
      << "                         Comma-separated list of shared libraries to instrument." << std::endl
      << std::endl
      << "     --outer-loops       Do instrument outer loops within functions." << std::endl
      << std::endl
      << "     --inner-loops       Do instrument inner loops within outer loops." << std::endl
      << "                         (implies --outer-loops)" << std::endl
      << std::endl
      << "     --loop-iters        Do instrument loop iterations." << std::endl
      << "                         (implies --outer-loops)" << std::endl
      << std::endl
      << "     --ignore-nodbg      Don't instrument functions which have no debug" << std::endl
      << "                         information." << std::endl
      << std::endl;
}

static void
vPrint( uint8_t level, const char * fmt, ... )
{
   va_list ap;

   if( Params.verbose_level >= level )
   {
      va_start( ap, fmt );

      char msg[1024] = "";

      snprintf( msg, sizeof( msg ) - 1, "%s: [%d]: ", ExeName.c_str(), ExePid );
      vsnprintf( msg + strlen( msg ), sizeof( msg ) - 1, fmt, ap );

      fprintf( verboseStream, "%s", msg );

      va_end( ap );
   }
}

//////////////////// class MutatorC ////////////////////

// public methods
//

MutatorC::MutatorC()
   : m_appAddrSpace( 0 ), m_appImage( 0 ), m_vtStartFunc( 0 ), m_vtEndFunc( 0 ),
     m_filter( 0 )
{
   // Empty
}

MutatorC::~MutatorC()
{
   // Empty
}

bool
MutatorC::run()
{
   bool error = false;

   // vector of function regions to instrument
   std::vector<FunctionS*> func_regions;

   do
   {
      // create/attach to a process or open binary for rewriting
      if( ( error = !initialize() ) )
         break;

      // get functions to instrument
      if( ( error = !getFunctions( func_regions ) ) )
         break;

      // instrument functions
      if( ( error = !instrumentFunctions( func_regions ) ) )
         break;

   } while( false );

   // clear vector of function regions to instrument
   //
   for( uint32_t i = 0; i < func_regions.size(); i++ )
      delete func_regions[i];
   func_regions.clear();

   // continue execution of mutatee or rewrite binary
   error = !finalize( error );

   return !error;
}

// private methods
//

bool
MutatorC::initialize()
{
   bool error = false;

   do
   {
      // set recommended optimizations to reduce runtime overhead
      //

      // turn on inlined trampolines
      m_bpatch.setMergeTramp( true );

      // turn on trampoline recursion because there is no way for the snippets
      // to call themselves
      m_bpatch.setTrampRecursive( true );

      // turn off stack frames in instrumentation
      m_bpatch.setInstrStackFrames( false );

      // turn on floating point saves due to the instrumentation does clobber
      // floating point registers
      //
      m_bpatch.setSaveFPR( true );
#ifdef DYNINST_7_0
      m_bpatch.forceSaveFPR( true );
#endif // DYNINST_7_0

      // read input filter file and add default filter rules
      //

      // get RFG filter object
      //
      m_filter = RFG_Filter_init();
      vt_assert( m_filter );

      // read input filter files, if given
      //
      if( Params.filtfile.length() > 0 )
      {
         // set input filter file name
         RFG_Filter_setDefFile( m_filter, Params.filtfile.c_str() );

         // read input filter file to get global filter rules (rank = -1)
         if( ( error = !RFG_Filter_readDefFile( m_filter, -1, 0 ) ) )
            break;
      }

      // NOTE: at this point one can add some default filter rules
      //

      // examples:

      //RFG_Filter_addRegionRules( m_filter, "foo", 0, 0, 0 );
      //RFG_Filter_addRegionRules( m_filter, "bar", 0, 0, 0 );

      // instrumenting this function generated by the Intel compiler
      // causes a segmentation fault at application runtime
      RFG_Filter_addRegionRules( m_filter, "__intel_cpu_indicator_init",
        0, 0, 0 );

      // this function, also a built-in function from the Intel compiler,
      // causes aborting instrumentation
      // (reported by David Shrader / LANL)
      RFG_Filter_addRegionRules( m_filter, "__intel_proc_init_N", 0, 0, 0 );

      // create/attach to a process or open binary for rewriting
      //
      switch( Params.mode )
      {
         case MODE_CREATE:
         {
            vPrint( 1, "Creating process\n" );

            vt_assert( Params.mutatee.length() > 0 );

            char ** mutatee_args;
            uint32_t i;

            // convert vector of mutatee arguments to char array
            //

            mutatee_args = new char*[Params.mutatee_args.size()+1];
            vt_assert( mutatee_args );

            for( i = 0; i < Params.mutatee_args.size(); i++ )
            {
               mutatee_args[i] = new char[Params.mutatee_args[i].length()+1];
               strcpy( mutatee_args[i], Params.mutatee_args[i].c_str() );
            }
            mutatee_args[Params.mutatee_args.size()] = 0;

            // create process
            m_appAddrSpace =
               m_bpatch.processCreate(
                  Params.mutatee.c_str(), (const char**)mutatee_args );

            // catch possible error
            //
            if( !m_appAddrSpace ||
               dynamic_cast<BPatch_process*>(m_appAddrSpace)->isTerminated() )
            {
               std::cerr << ExeName << ": [" << ExePid << "]: "
                         << "Error: Could not create process. Aborting."
                         << std::endl;
               error = true;
            }

            // free array of mutatee arguments
            //
            for( i = 0; i < Params.mutatee_args.size(); i++ )
               delete [] mutatee_args[i];
            delete [] mutatee_args;

            break;
         }
         case MODE_ATTACH:
         {
            vt_assert( Params.mutatee_pid );

            vPrint( 1, "Attaching to PID %d\n", Params.mutatee_pid );

            // attach to running process
            m_appAddrSpace =
               m_bpatch.processAttach(
                  Params.mutatee.c_str(), Params.mutatee_pid );

            // catch possible error
            //
            if( !m_appAddrSpace ||
               dynamic_cast<BPatch_process*>(m_appAddrSpace)->isTerminated() )
            {
               std::cerr << ExeName << ": [" << ExePid << "]: "
                         << "Error: Could not attach to PID "
                         << Params.mutatee_pid << ". Aborting." << std::endl;

               if( Params.mutatee.length() == 0 )
               {
                  std::cerr << ExeName << ": [" << ExePid << "]: "
                            << "A possible solution to the problem is to set "
                            << "the environment variable VT_APPPATH to the "
                            << "path of your application." << std::endl;
               }

               error = true;
            }

            break;
         }
         case MODE_REWRITE:
         {
            vt_assert( Params.mutatee.length() > 0 );

            vPrint( 1, "Opening %s\n", Params.mutatee.c_str() );

            // open binary for rewriting
            m_appAddrSpace =
               m_bpatch.openBinary( Params.mutatee.c_str(), true );

            // catch possible error
            //
            if( !m_appAddrSpace )
            {
               std::cerr << ExeName << ": [" << ExePid << "]: "
                         << "Error: Could not open " << Params.mutatee
                         << ". Aborting." << std::endl;
               error = true;
            }

            break;
         }
      }
      if( error )
         break;

      // load user-specified shared libraries into mutatee
      //
      for( uint32_t i = 0; i < Params.shlibs.size() && !error; i++ )
      {
         if( !m_appAddrSpace->loadLibrary( Params.shlibs[i].c_str() ) )
         {
            std::cerr << ExeName << ": [" << ExePid << "]: "
                      << "Error: Could not load shared library "
                      << Params.shlibs[i] << ". Aborting." << std::endl;
            error = true;
         }
      }
      if( error )
         break;

      // read the mutatee image and get an associated image object
      //
      m_appImage = m_appAddrSpace->getImage();
      if( !m_appImage )
      {
         std::cerr << ExeName << ": [" << ExePid << "]: "
                   << "Error: Could not get an image object. Aborting."
                   << std::endl;
         error = true;
         break;
      }

      // search instrumentation functions to insert at entry/exit points
      //
      m_vtStartFunc = findFunction( "vt_dyn_start" );
      m_vtEndFunc = findFunction( "vt_dyn_end" );
      if( !m_vtStartFunc || !m_vtEndFunc )
      {
         std::cerr << ExeName << ": [" << ExePid << "]: "
                   << "Error: Could not find instrumentation functions. "
                   << "Is VampirTrace library linked? Aborting." << std::endl;
         error = true;
         break;
      }

   } while( false );

   return !error;
}

bool
MutatorC::finalize( bool & error )
{
   switch( Params.mode )
   {
      case MODE_CREATE:
      case MODE_ATTACH:
      {
         BPatch_process * app_process =
            dynamic_cast<BPatch_process*>( m_appAddrSpace );

         // either continue execution of mutatee ...
         //
         if( !error )
         {
            vPrint( 1, "Continuing process execution\n" );

            if( !app_process->isStopped() || app_process->isTerminated() )
            {
               std::cerr << ExeName << ": [" << ExePid << "]: "
                         << "Error: Could not continue process execution. "
                         << "Aborting." << std::endl;
               error = true;
            }
            else
            {
               // send mutatee process a signal to continue execution
               //
               if( Params.mutatee_pid != -1 )
                  kill( Params.mutatee_pid, VT_DYNINST_CONT_SIGNUM );

               if( !Params.detach )
               {
                  // continue execution of mutatee
                  app_process->continueExecution();

                  // wait until mutatee is terminated
                  //
                  while( !app_process->isTerminated() )
                  {
                     m_bpatch.waitForStatusChange();
                     sleep(1);
                  }

                  vPrint( 1, "End of process\n" );
                  vPrint( 1, "Done\n" );
               }
               else
               {
                  // continue execution of mutatee and detach from its process
                  app_process->detach( true );
               }
            }
         }
         // ... or terminate execution on error
         //
         else
         {
            if( app_process && !app_process->isTerminated() )
               app_process->terminateExecution();
         }

         break;
      }
      case MODE_REWRITE:
      {
         if( !error )
         {
            vPrint( 1, "Writing %s\n", Params.outfile.c_str() );

            BPatch_binaryEdit * app_editor =
               dynamic_cast<BPatch_binaryEdit*>(m_appAddrSpace);

            // rewrite instrumented binary
            //
            if( !app_editor->writeFile( Params.outfile.c_str() ) )
            {
               std::cerr << ExeName << ": [" << ExePid << "]: "
                         << "Error: Could not write " << Params.outfile
                         << ". Aborting." << std::endl;
               error = true;
            }
            else
            {
               vPrint( 1, "Done\n" );
            }
         }

         break;
      }
   }

   // free RFG filter object
   RFG_Filter_free( m_filter );

   return !error;
}

bool
MutatorC::getFunctions( std::vector<FunctionS*> & funcRegions ) const
{
   bool error = false;

   vPrint( 1, "Collecting functions to instrument\n" );

   do
   {
      // get modules of image
      //
      const BPatch_Vector<BPatch_module*> * modules =
         m_appImage->getModules();
      if( !modules )
      {
         std::cerr << ExeName << ": [" << ExePid << "]: "
                   << "Error: Could not get modules of image. Aborting."
                   << std::endl;
         error = true;
         break;
      }

      // iterate over all modules
      for( uint32_t i = 0; i < modules->size() && !error; i++ )
      {
         BPatch_module * module = (*modules)[i];

         char buffer[STRBUFSIZE] = "";

         // get module name
         //
         std::string module_name;
         module->getName( buffer, STRBUFSIZE );
         module_name = buffer;

         // check whether module is excluded from instrumentation
         //
         if( constraintModule( module_name ) )
         {
            vPrint( 2, " Skip module '%s' (excluded)\n", module_name.c_str() );
            continue;
         }

         // get functions of module
         //

         const BPatch_Vector<BPatch_function*> * functions =
            module->getProcedures();

         if( !functions )
         {
            std::cerr << ExeName << ": [" << ExePid << "]: "
                      << "Error: Could not get functions of module "
                      << module_name << ". Aborting." << std::endl;
            error = true;
            break;
         }

         // iterate over all functions
         for( uint32_t j = 0; j < functions->size() && !error; j++ )
         {
            BPatch_function * function = (*functions)[j];

            // get function name
            //

            std::string function_name;

            // try to get the full function prototype
            function->getTypedName( buffer, STRBUFSIZE );
            if( strlen( buffer ) == 0 )
            {
               // if failed, try to get the plain function name
               function->getName( buffer, STRBUFSIZE );

               // if failed again, ignore function
               if( strlen( buffer ) == 0 )
                  continue;
            }

            function_name = buffer;

            // check whether function is instrumentable
            //
            if( !function->isInstrumentable() )
            {
               vPrint( 2, " Skip function '%s' (not instrumentable)\n",
                       function_name.c_str() );
               continue;
            }

            // get function's entry and exit instrumentation points
            //

            FunctionS::InstPointsS function_inst_points;

            function_inst_points.entries = findPoint( function, BPatch_entry );
            if( !function_inst_points.entries ||
                function_inst_points.entries->empty() )
            {
               vPrint( 2, " Skip function '%s' "
                          "(no entry instrumentation point found)\n",
                       function_name.c_str() );
               continue;
            }

            function_inst_points.exits = findPoint( function, BPatch_exit );
            if( !function_inst_points.exits ||
                function_inst_points.exits->empty() )
            {
               vPrint( 2, " Skip function '%s' "
                          "(no exit instrumentation point found)\n",
                       function_name.c_str() );
               continue;
            }

            // check whether function is excluded from instrumentation
            //
            if( constraintRegion( function_name ) )
            {
               vPrint( 2, " Skip function '%s' (excluded)\n",
                       function_name.c_str() );
               continue;
            }

            // get function's source code location
            //

            FunctionS::SclS function_scl;

            std::vector<BPatch_statement> function_stmts;

            module->getSourceLines( (unsigned long)function->getBaseAddr(),
               function_stmts );

            if( !function_stmts.empty() && function_stmts[0].fileName() &&
                function_stmts[0].lineNumber() > 0 )
            {
               function_scl.file_name = function_stmts[0].fileName();
               function_scl.line_number = function_stmts[0].lineNumber();

               // check whether source file is excluded from instrumentation
               //
               if( constraintModule( function_scl.file_name ) )
               {
                  vPrint( 2, " Skip function '%s' "
                             "(source file '%s' excluded)\n",
                          function_name.c_str(),
                          function_scl.file_name.c_str() );
                  continue;
               }
            }
            else if( Params.ignore_no_dbg )
            {
               vPrint( 2, " Skip function '%s' (no debug information)\n",
                       function_name.c_str() );
               continue;
            }

            // create new function region and add them to vector
            //

            vPrint( 2, " Add function '%s' for instrumenting\n",
                    function_name.c_str() );

            FunctionS * function_region =
               new FunctionS( function_name, function_scl,
                  function_inst_points );
            if( !function_region )
            {
               error = true;
               break;
            }

            funcRegions.push_back( function_region );

            // get loops of function, if desired
            //
            if( Params.outer_loops )
            {
               // get function's flow graph
               //
               BPatch_flowGraph * function_cfg = function->getCFG();
               if( !function_cfg )
               {
                  vPrint( 2, " Skip loops of function '%s' "
                             "(no flow graph information)\n",
                          function_name.c_str() );
                  continue;
               }

               // get function's outer (and inner) loops
               //

               BPatch_Vector<BPatch_basicBlockLoop*> loops;

               if( Params.inner_loops )
                  function_cfg->getLoops( loops );
               else
                  function_cfg->getOuterLoops( loops );

               // iterate over all loops
               for( uint32_t k = 0; k < loops.size() && !error; k++ )
               {
                  BPatch_basicBlockLoop * loop = loops[k];

                  // get loop's index
                  // (will be appended to the loop name, if the source code
                  // location cannot be determined)
                  const uint32_t loop_index = k + 1;

                  // get loop's entry and exit instrumentation points
                  //

                  LoopS::InstPointsS loop_inst_points;

                  loop_inst_points.entries =
                     findPoint( function_cfg, BPatch_locLoopEntry, loop );
                  if( !loop_inst_points.entries ||
                      loop_inst_points.entries->empty() )
                  {
                     vPrint( 2, " Skip loop #%u in function '%s' "
                                "(no entry instrumentation point found)\n",
                             loop_index, function_name.c_str() );
                     continue;
                  }

                  loop_inst_points.exits =
                     findPoint( function_cfg, BPatch_locLoopExit, loop );
                  if( !loop_inst_points.exits ||
                      loop_inst_points.exits->empty() )
                  {
                     vPrint( 2, " Skip loop #%u in function '%s' "
                                "(no exit instrumentation point found)\n",
                             loop_index, function_name.c_str() );
                     continue;
                  }

                  // get loop's source code location
                  //

                  LoopS::SclS loop_scl;

                  std::vector<BPatch_statement> loop_stmts;

                  module->getSourceLines(
                     loop->getLoopHead()->getStartAddress(), loop_stmts );

                  if( !loop_stmts.empty() && loop_stmts[0].fileName() &&
                      loop_stmts[0].lineNumber() > 0 )
                  {
                     loop_scl.file_name = loop_stmts[0].fileName();
                     loop_scl.line_number = loop_stmts[0].lineNumber();
                  }

                  // compose loop name
                  //
                  // if the source code location is available:
                  //    <function> loop @<file>:<line>
                  // otherwise:
                  //    <function> loop #<loop index>
                  //

                  std::string loop_name;

                  {
                     std::ostringstream oss;

                     oss << function_name << " loop ";

                     if( loop_scl.valid() )
                     {
                        oss << "@" << loop_scl.file_name
                            << ":" << loop_scl.line_number;
                     }
                     else
                     {
                        oss << "#" << loop_index;
                     }

                     loop_name = oss.str();
                  }

                  // check whether loop is excluded from instrumentation
                  //
                  if( constraintRegion( loop_name ) )
                  {
                     vPrint( 2, " Skip loop '%s' (excluded)\n",
                             loop_name.c_str() );
                     continue;
                  }

                  // create new loop region and add them to vector
                  //

                  vPrint( 2, " Add loop '%s' for instrumenting\n",
                          loop_name.c_str() );

                  LoopS * loop_region =
                     new LoopS( loop_name, loop_scl, loop_inst_points );
                  if( !loop_region )
                  {
                     error = true;
                     break;
                  }

                  function_region->loops.push_back( loop_region );

                  // get iteration of loop, if desired
                  //
                  if( Params.loop_iters )
                  {
                     do
                     {
                        // get loop iteration's entry and exit
                        // instrumentation points
                        //

                        LoopS::IterationT::InstPointsS loop_iter_inst_points;

                        loop_iter_inst_points.entries =
                           findPoint( function_cfg, BPatch_locLoopStartIter,
                              loop );
                        if( !loop_iter_inst_points.entries ||
                            loop_iter_inst_points.entries->empty() )
                        {
                           vPrint( 2, " Skip iteration of loop '%s' in "
                                      "function '%s' "
                                      "(no start instrumentation point "
                                      "found)\n",
                                   loop_name.c_str(), function_name.c_str() );
                           break;
                        }

                        loop_iter_inst_points.exits =
                           findPoint( function_cfg, BPatch_locLoopEndIter,
                              loop );
                        if( !loop_iter_inst_points.exits ||
                            loop_iter_inst_points.exits->empty() )
                        {
                           vPrint( 2, " Skip iteration of loop '%s' in "
                                      "function '%s' "
                                      "(no end instrumentation point "
                                      "found)\n",
                                   loop_name.c_str(), function_name.c_str() );
                           break;
                        }

                        // compose loop iteration name based on the name of
                        // surrounding loop
                        //
                        // <function> loop <@file:line|#loop index>
                        //                 ^ insert "iter " at this point
                        //

                        std::string loop_iter_name = loop_name;

                        {
                           std::string::size_type si =
                              loop_iter_name.find_last_of( "@#" );
                           vt_assert( si != std::string::npos );

                           loop_iter_name.insert( si, "iter " );
                        }

                        // check whether loop iteration is excluded from
                        // instrumentation
                        //
                        if( constraintRegion( loop_iter_name ) )
                        {
                           vPrint( 2, " Skip loop iteration '%s' (excluded)\n",
                                   loop_iter_name.c_str() );
                        }

                        // create new loop iteration region
                        //

                        vPrint( 2, " Add loop iteration '%s' for "
                                   "instrumenting\n",
                                loop_iter_name.c_str() );

                        loop_region->iteration =
                           new LoopS::IterationT( loop_iter_name, loop_scl,
                              loop_iter_inst_points );
                        if( !loop_region->iteration )
                        {
                           error = true;
                           break;
                        }

                     } while( false );
                  }
               }
            }
         }
      }

   } while( false );

   // check whether we have functions to instrument
   //
   if( !error && funcRegions.empty() )
   {
      std::cerr << ExeName << ": [" << ExePid << "]: "
                << "Error: No functions to instrument. Aborting."
                << std::endl;
      error = true;
   }

   return !error;
}

bool
MutatorC::instrumentFunctions( const std::vector<FunctionS*> & functions ) const
{
   bool error = false;

   vPrint( 1, "Instrumenting functions\n" );

   do
   {
      // instrument functions
      //
      for( uint32_t i = 0; i < functions.size(); i++ )
      {
         const FunctionS * function = functions[i];

         // begin insertion set
         m_appAddrSpace->beginInsertionSet();

         // instrument function's entry point
         if( ( error = !instrumentRegionEntry( function, false ) ) )
            break;

         // instrument function's loops
         //
         for( uint32_t j = 0; j < function->loops.size(); j++ )
         {
            const LoopS * loop = function->loops[j];

            // instrument loop's entry point
            if( ( error = !instrumentRegionEntry( loop, true ) ) )
               break;

            // instrument loop's iterations
            //
            if( loop->iteration )
            {
               // instrument loop iteration's entry point
               if( ( error = !instrumentRegionEntry( loop->iteration, true ) ) )
                  break;

               // instrument loop iteration's exit point
               if( ( error = !instrumentRegionExit( loop->iteration, true ) ) )
                  break;
            }
            if( error ) break;

            // instrument loop's exit point
            if( ( error = !instrumentRegionExit( loop, true ) ) )
               break;
         }
         if( error ) break;

         // instrument function's exit point(s)
         if( ( error = !instrumentRegionExit( function, false ) ) )
            break;

         // finalize insertion set
         //
         if( !m_appAddrSpace->finalizeInsertionSet( true, 0 ) )
         {
            std::cerr << ExeName << ": [" << ExePid << "]: "
                      << "Error: Could not finalize instrumentation set for "
                      << "function '" << function->name << "'. Aborting."
                      << std::endl;
            error = true;
            break;
         }
      }

   } while( false );

   return !error;
}

bool
MutatorC::instrumentRegionEntry( const RegionS * region, bool isLoop ) const
{
   bool error = false;

   vPrint( 2, " Instrumenting-> '%s' Entry\n", region->name.c_str() );

   // set callee arguments
   //

   static BPatch_Vector<BPatch_snippet*> callee_args( 5 );

   // region index
   //
   BPatch_constExpr const_expr_findex( region->index );
   callee_args[0] = &const_expr_findex;

   // region name
   //
   BPatch_constExpr const_expr_fname( region->name.c_str() );
   callee_args[1] = &const_expr_fname;

   // source file name
   //
   BPatch_constExpr const_expr_file( region->scl.file_name.c_str() );
   callee_args[2] = &const_expr_file;

   // line number
   //
   BPatch_constExpr const_expr_lno( region->scl.line_number );
   callee_args[3] = &const_expr_lno;

   // flag for indicating a loop region
   //
   BPatch_constExpr const_expr_loop( (uint32_t)( isLoop ? 1 : 0 ) );
   callee_args[4] = &const_expr_loop;

   // create instrumentation snippet
   BPatch_snippet snippet = BPatch_funcCallExpr( *m_vtStartFunc, callee_args );

   // insert instrumentation snippet
   //

   BPatchSnippetHandle * snippet_handle;

   if( isLoop )
   {
      snippet_handle =
         m_appAddrSpace->insertSnippet( snippet,
            *(region->inst_points.entries) );
   }
   else
   {
      snippet_handle =
         m_appAddrSpace->insertSnippet( snippet,
            *(region->inst_points.entries), BPatch_callBefore,
            BPatch_firstSnippet );
   }

   if( !snippet_handle )
   {
      std::cerr << ExeName << ": [" << ExePid << "]: "
                << "Error: Could not instrument entry points of "
                << "region '" << region->name << "'. Aborting."
                << std::endl;
      error = true;
   }

   return !error;
}

bool
MutatorC::instrumentRegionExit( const RegionS * region, bool isLoop ) const
{
   bool error = false;

   vPrint( 2, " Instrumenting-> '%s' Exit\n", region->name.c_str() );

   // set callee argument
   //

   static BPatch_Vector<BPatch_snippet*> callee_args( 1 );

   // function index
   //
   BPatch_constExpr const_expr_findex( region->index );
   callee_args[0] = &const_expr_findex;

   // create instrumentation snippet
   BPatch_snippet snippet = BPatch_funcCallExpr( *m_vtEndFunc, callee_args );

   // insert instrumentation snippet
   //

   BPatchSnippetHandle * snippet_handle;

   if( isLoop )
   {
      snippet_handle =
         m_appAddrSpace->insertSnippet( snippet, *(region->inst_points.exits) );
   }
   else
   {
      snippet_handle =
         m_appAddrSpace->insertSnippet( snippet, *(region->inst_points.exits),
            BPatch_callAfter, BPatch_lastSnippet );
   }

   if( !snippet_handle )
   {
      std::cerr << ExeName << ": [" << ExePid << "]: "
                << "Error: Could not instrument exit points of "
                << "region '" << region->name << "'. Aborting."
                << std::endl;
      error = true;
   }

   return !error;
}

bool
MutatorC::constraintModule( const std::string & name ) const
{
   int len = name.length();

   if( name.compare( "DEFAULT_MODULE" ) == 0 ||
       name.compare( "LIBRARY_MODULE" ) == 0 ||
       ( ( name.compare( 0, 3, "vt_" ) != 0     &&
           name.compare( 0, 4, "rfg_" ) != 0    &&
           name.compare( 0, 4, "OTF_" ) != 0    &&
           name.compare( 0, 5, "pomp_" ) != 0 ) &&
         ( ( len >= 2 && name.compare( len-2, 2, ".c" ) == 0 )   ||
           ( len >= 2 && name.compare( len-2, 2, ".C" ) == 0 )   ||
           ( len >= 3 && name.compare( len-3, 3, ".cc" ) == 0 )  ||
           ( len >= 4 && name.compare( len-4, 4, ".cpp" ) == 0 ) ||
           ( len >= 2 && name.compare( len-2, 2, ".f" ) == 0 )   ||
           ( len >= 2 && name.compare( len-2, 2, ".F" ) == 0 )   ||
           ( len >= 4 && name.compare( len-4, 4, ".f77" ) == 0 ) ||
           ( len >= 4 && name.compare( len-4, 4, ".F77" ) == 0 ) ||
           ( len >= 4 && name.compare( len-4, 4, ".f90" ) == 0 ) ||
           ( len >= 4 && name.compare( len-4, 4, ".F90" ) == 0 ) ||
           ( len >= 4 && name.compare( len-4, 4, ".f95" ) == 0 ) ||
           ( len >= 4 && name.compare( len-4, 4, ".F95" ) == 0 ) ) ) )
   {
      return false;   // ok, module should be instrumented
   }
   else
   {
      // check for user specified constraints on modules
      //
      std::vector<std::string>::const_iterator it =
         std::find( Params.shlibs.begin(), Params.shlibs.end(), name );

      return it == Params.shlibs.end();
   }
}

bool
MutatorC::constraintRegion( const std::string & name ) const
{
   if( isMPI() && name.compare( 0, 4, "MPI_" ) == 0 )
   {
      return true;           // don't instrument MPI functions
                             // (already done by function wrapper)
   }
   else if( name.compare( 0, 7, "UNIMCI_" ) == 0 )
   {
      return true;
   }
   else
   {
      int32_t limit;
      uint8_t flags;
      RFG_Filter_getRegionRules( m_filter, name.c_str(), 0, &limit, 0, &flags );

      // don't instrument function if call limit is 0 and function isn't
      // filtered recursively
      return ( limit == 0 && (flags & RFG_FILTER_FLAG_RECURSIVE) == 0 );
   }
}

bool
MutatorC::isMPI() const
{
   static int is_mpi = -1;

   if( is_mpi == -1 )
      is_mpi = ( findFunction( "MPI_Init" ) != 0 ) ? 1 : 0;

   return is_mpi;
}

BPatch_function *
MutatorC::findFunction( const std::string & name ) const
{
   BPatch_Vector<BPatch_function*> functions;

   m_appImage->findFunction( name.c_str(), functions, false );

   if( !functions.empty() )
      return functions[0];
   else
      return 0;
}

BPatch_Vector<BPatch_point*> *
MutatorC::findPoint( void * where, BPatch_procedureLocation loc,
   BPatch_basicBlockLoop * loop ) const
{
   BPatch_Vector<BPatch_point*> * points;

   // search for instrumentation point in given...
   //
   // ... function
   if( !loop )
   {
      points = static_cast<BPatch_function*>(where)->findPoint( loc );
   }
   // ... or loop
   else
   {
      points =
         static_cast<BPatch_flowGraph*>(where)->findLoopInstPoints( loc, loop );
   }

#ifdef NOTRAPS

   // check whether inserting instrumentation at found point(s) requires using
   // a trap
   //
   for( uint32_t i = 0; points && i < points->size(); i++ )
   {
     if( (*points)[i]->usesTrap_NP() )
        points = 0;
   }

#endif // NOTRAPS

   return points;
}

//////////////////// struct MutatorC::RegionS ////////////////////

uint32_t MutatorC::RegionS::Count = 0;

MutatorC::RegionS::RegionS( const std::string & _name, const SclS & _scl,
   const InstPointsS & _inst_points )
   : name( _name ), scl( _scl ), inst_points( _inst_points )
{
   // the new operator should already catch this case
   vt_assert( Count < VT_MAX_DYNINST_REGIONS );

   // set region's index and increment counter
   index = Count++;
}

MutatorC::RegionS::~RegionS()
{
   // Empty
}

void *
MutatorC::RegionS::operator new( size_t size ) throw()
{
   // maximum number of regions to instrument exceeded?
   if( Count >= VT_MAX_DYNINST_REGIONS )
   {
      std::cerr << ExeName << ": [" << ExePid << "]: "
                << "Error: Too many regions to instrument (max. "
                << VT_MAX_DYNINST_REGIONS << "). Aborting." << std::endl;

      return 0;
   }

   return malloc( size );
}

void
MutatorC::RegionS::operator delete( void * ptr )
{
   free( ptr );
}
