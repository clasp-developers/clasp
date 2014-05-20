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

#include "vt_unify_hooks_prof.h"

#include <iostream>

#include <stdio.h>

//////////////////// class HooksProfC ////////////////////

// public methods
//

HooksProfC::HooksProfC() : HooksBaseC(),
   m_numProcs( 0 ), m_timerRes( 1 )
{
   // Empty
}

HooksProfC::~HooksProfC()
{
   // Empty
}

// private methods
//

// vvvvvvvvvvvvvvvvvvvv HOOK METHODS vvvvvvvvvvvvvvvvvvvv

// initialization/finalization hooks
//

void
HooksProfC::initHook()
{
   // Empty
}

void
HooksProfC::finalizeHook( const bool & error )
{
   MASTER
   {
      if( !error && haveFuncProf() )
      {
         // write summary function profile to file
         printFuncProf( m_sumFuncProfs, Params.prof_out_file );

         // print summary function profile to stdout
         //
         if( !Params.bequiet )
         {
            std::cout << std::endl;
            printFuncProf( m_sumFuncProfs );
            std::cout << std::endl
                      << "The complete function summary was written to file '"
                      << Params.prof_out_file << "'." << std::endl << std::endl;
         }
      }
   }
}

// phase hooks
//

void
HooksProfC::phaseHook_GetUnifyControls_post()
{
   // return, if the input trace has no statistics
   if( !UnifyControlS::have_stats() )
      return;

   // create function profile maps for each assigned stream
   // to allow simultaneous access from multiple threads
   //
   for( uint32_t i = 0; i < MyStreamIds.size(); i++ )
   {
      const uint32_t & streamid = MyStreamIds[i];
      m_procId2FuncProf[streamid] = std::map<uint32_t, FuncProfS>();
   }
}

void
HooksProfC::phaseHook_UnifyStatistics_post()
{
   VPrint( 2, " Generating summary function profile from statistics\n" );

#ifdef VT_MPI
   if( NumRanks > 1 )
   {
      // gather function profiles from all ranks
      gatherFuncProfs();
   }
#endif // VT_MPI

   MASTER
   {
      // get summary function profile
      getFuncProf( m_sumFuncProfs );
   }
}

// record hooks
//

void
HooksProfC::writeRecHook_DefTimerResolution( HooksC::VaArgsT & args )
{
   // return, if the input trace has no statistics
   if( !UnifyControlS::have_stats() )
      return;

   // get hook arguments
   //

   //OTF_WStream ** wstream   = (OTF_WStream**)args[0];
   uint64_t *     timer_res = (uint64_t*)args[1];
   bool *         do_write  = (bool*)args[2];

   // set timer resolution
   if( *do_write )
      m_timerRes = *timer_res;
}

void
HooksProfC::writeRecHook_DefProcess( HooksC::VaArgsT & args )
{
   // return, if the input trace has no statistics
   if( !UnifyControlS::have_stats() )
      return;

   // get hook arguments
   //

   //OTF_WStream ** wstream   = (OTF_WStream**)args[0];
   //uint32_t *     proc_id   = (uint32_t*)args[1];
   //std::string *  name      = (std::string*)args[2];
   //uint32_t *     parent_id = (uint32_t*)args[3];
   bool *         do_write  = (bool*)args[4];

   // increment number of processes
   if( *do_write )
      m_numProcs++;
}

void
HooksProfC::writeRecHook_DefFunction( HooksC::VaArgsT & args )
{
   // return, if the input trace has no statistics
   if( !UnifyControlS::have_stats() )
      return;

   // get hook arguments
   //

   //OTF_WStream ** wstream    = (OTF_WStream**)args[0];
   uint32_t *     func_id    = (uint32_t*)args[1];
   std::string *  func_name  = (std::string*)args[2];
   //uint32_t *     func_group = (uint32_t*)args[3];
   //uint32_t *     func_scl   = (uint32_t*)args[4];
   bool *         do_write   = (bool*)args[5];

   // add function
   if( *do_write )
      m_funcId2Name[*func_id] = *func_name;
}

void
HooksProfC::writeRecHook_FunctionSummary( HooksC::VaArgsT & args )
{
   // get hook arguments
   //

   //OTF_WStream ** wstream  = (OTF_WStream**)args[0];
   //uint64_t *     time     = (uint64_t*)args[1];
   uint32_t *     func_id  = (uint32_t*)args[2];
   uint32_t *     proc_id  = (uint32_t*)args[3];
   uint64_t *     count    = (uint64_t*)args[4];
   uint64_t *     excl     = (uint64_t*)args[5];
   uint64_t *     incl     = (uint64_t*)args[6];
   bool *         do_write = (bool*)args[7];

   // process function statistics
   if( *do_write )
      processFuncStat( *proc_id, *func_id, *count, *incl, *excl );
}

// ^^^^^^^^^^^^^^^^^^^^ HOOK METHODS ^^^^^^^^^^^^^^^^^^^^

void
HooksProfC::processFuncStat( const uint32_t & procId, const uint32_t & funcId,
                             const uint64_t & count, const uint64_t & incl,
                             const uint64_t & excl )
{
   // search function profile map by process id
   //
   std::map<uint32_t, std::map<uint32_t, FuncProfS> >::iterator proc_it =
      m_procId2FuncProf.find( procId );
   vt_assert( proc_it != m_procId2FuncProf.end() );

   // search function profile by function id
   std::map<uint32_t, FuncProfS>::iterator func_it =
      proc_it->second.find( funcId );

   // create function profile, if not found
   //
   if( func_it == proc_it->second.end() )
   {
      func_it =
         proc_it->second.insert(
            std::make_pair( funcId, FuncProfS( funcId ) ) ).first;
   }

   // (overwrite) function profile values
   //
   func_it->second.count = (double)count;
   func_it->second.incl = incl;
   func_it->second.excl = excl;
}

void
HooksProfC::getFuncProf( std::vector<HooksProfC::FuncProfS> & funcProfs,
                         const uint32_t & procId )
{
   // iterate over all process function profiles
   for( std::map<uint32_t, std::map<uint32_t, FuncProfS> >::const_iterator
        proc_it = m_procId2FuncProf.begin();
        proc_it != m_procId2FuncProf.end(); ++proc_it )
   {
      // continue, if process function profile of interest?
      if( procId != 0 && proc_it->first != procId )
         continue;

      for( std::map<uint32_t, FuncProfS>::const_iterator loc_prof_it =
           proc_it->second.begin(); loc_prof_it != proc_it->second.end();
           ++loc_prof_it )
      {
         const FuncProfS & loc_prof = loc_prof_it->second;

         // search for already created function profile
         std::vector<FuncProfS>::iterator out_prof_it =
            std::find( funcProfs.begin(), funcProfs.end(),
                       FuncProfS( loc_prof.funcid ) );

         // create it, if not found
         //
         if( out_prof_it == funcProfs.end() )
         {
            funcProfs.push_back(
               FuncProfS( loc_prof.funcid, getFuncNameById( loc_prof.funcid ),
                  loc_prof.count, loc_prof.incl, loc_prof.excl ) );
         }
         // otherwise, re-calculate values
         //
         else
         {
            *out_prof_it += loc_prof;
         }
      }
   }

   // compute average values, if summary
   //
   if( procId == 0 )
   {
      vt_assert( m_numProcs > 0 );

      for( uint32_t i = 0; i < funcProfs.size(); i++ )
      {
         funcProfs[i].count /= (double)m_numProcs;
         funcProfs[i].incl /= m_numProcs;
         funcProfs[i].excl /= m_numProcs;
      }
   }

   // sort vector of function profiles
   std::sort( funcProfs.begin(), funcProfs.end(), std::less<FuncProfS>() );
}

bool
HooksProfC::printFuncProf( const std::vector<FuncProfS> & funcProfs,
                           const std::string & outFile )
{
   const uint32_t max_lines_on_stdout = 10;

   FILE * out;

   // open function profile output file, if given
   //
   if( outFile.length() != 0 )
   {
      if( !( out = fopen( outFile.c_str(), "w" ) ) )
      {
         std::cerr << ExeName << ": Error: "
                   << "Could not open file " << outFile << std::endl;
         return false;
      }
   }
   // otherwise, print on stdout
   else
   {
      out = stdout;
   }

   int sort_flags = Params.prof_sort_flags;

   // print out function profiles
   //
   fprintf( out, "                                   %cexcl. time %cincl. time\n",
            (sort_flags & FUNC_PROF_SORT_FLAG_EXCL_CALL) ? '*' : ' ',
            (sort_flags & FUNC_PROF_SORT_FLAG_INCL_CALL) ? '*' : ' ' );

   fprintf( out, "%cexcl. time %cincl. time      calls      / call      / call %cname\n",
            (sort_flags & FUNC_PROF_SORT_FLAG_EXCL) ? '*' : ' ',
            (sort_flags & FUNC_PROF_SORT_FLAG_INCL) ? '*' : ' ',
            (sort_flags & FUNC_PROF_SORT_FLAG_FUNCNAME) ? '*' : ' ' );

   // reduce output lines, if necessary
   //
   uint32_t size = funcProfs.size();
   if( out == stdout && size > max_lines_on_stdout )
      size = max_lines_on_stdout;

   for( uint32_t i = 0; i < size; i++ )
   {
      std::string str_excl = formatTime( funcProfs[i].excl );
      std::string str_incl = formatTime( funcProfs[i].incl );
      std::string str_excl_call =
         formatTime( (uint64_t)((double)funcProfs[i].excl / funcProfs[i].count) );
      std::string str_incl_call =
         formatTime( (uint64_t)((double)funcProfs[i].incl / funcProfs[i].count) );
      std::string str_funcname = funcProfs[i].funcname;

      if( out == stdout ) str_funcname = shortName( funcProfs[i].funcname ); 

      fprintf( out,
               "%11s %11s %10.*f %11s %11s  %s\n",
               str_excl.c_str(),
               str_incl.c_str(),
               ((double)((uint64_t)funcProfs[i].count) ==
                funcProfs[i].count) ? 0 : 2,
               funcProfs[i].count,
               str_excl_call.c_str(),
               str_incl_call.c_str(),
               str_funcname.c_str() );
   }

   if( out == stdout && size < funcProfs.size() )
   {
      fprintf( out, "Displayed %u from %u functions.\n",
               size, (uint32_t)funcProfs.size() );
   }

   // close function profile output file, if necessary
   if( out != stdout ) fclose( out );

   return true;
}

bool
HooksProfC::haveFuncProf( const uint32_t & procId )
{
   bool avail = false;

   std::map<uint32_t, std::map<uint32_t, FuncProfS> >::const_iterator proc_it;

   // either check for function profile of specific process ...
   //
   if( procId != 0 )
   {
      proc_it = m_procId2FuncProf.find( procId );
      if( proc_it != m_procId2FuncProf.end() )
         avail = !proc_it->second.empty();
   }
   // ... or check for any function profile
   //
   else
   {
      // iterate over all process function profiles
      for( proc_it = m_procId2FuncProf.begin();
           proc_it != m_procId2FuncProf.end(); ++proc_it )
      {
         if( !proc_it->second.empty() )
         {
            avail = true;
            break;
         }
      }
   }

   return avail;
}

std::string
HooksProfC::getFuncNameById( const uint32_t & funcId )
{
   // search function name by function id
   //
   std::map<uint32_t, std::string>::const_iterator it =
      m_funcId2Name.find( funcId );
   vt_assert( it != m_funcId2Name.end() );

   return it->second;
}

std::string
HooksProfC::shortName( const std::string & longName, uint32_t len )
{
   vt_assert( len >= 5 );

   std::string short_name;

   if( longName.length() <= len ) 
   {
      short_name = longName;
   }
   else
   {
      std::string f, b;

      f = longName.substr( 0, (len-3) / 2 ) + "...";
      b = longName.substr( longName.length()-(len-f.length()));
      short_name = f+b;
   }

   return short_name;
}

std::string
HooksProfC::formatTime( const uint64_t & time )
{
   char str[20];
   double d_time = (double)time;
   double d_res = (double)m_timerRes;
   double sec = d_time / d_res;

   static const char unit[4][3] = { "s ", "ms", "us", "ns" };

   for( uint32_t i = 0; i < 4; i++ )
   {
      if( i == 3 || sec >= 0.1 )
      {
         snprintf( str, sizeof( str ) - 1, "%.3f%s", sec, unit[i] );
         break;
      }
      sec *= 1000.0;
   }

   return std::string( str );
}

#ifdef VT_MPI

void
HooksProfC::gatherFuncProfs()
{
   vt_assert( NumRanks > 1 );

   // block until all ranks have reached this point
   CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );

   VPrint( 2, "  Gathering function profiles\n" );

   uint32_t proc_map_size;

   char * send_buffer;
   VT_MPI_INT send_buffer_size;
   VT_MPI_INT send_buffer_pos;

   // get size needed for the send buffer
   //

   VT_MPI_INT size;

   // m_procId2FuncProf.size()
   CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD,
                            &send_buffer_size ) );

   SLAVE
   {
      // m_procId2FuncProf
      //
      for( std::map<uint32_t, std::map<uint32_t, FuncProfS> >::iterator
           proc_it = m_procId2FuncProf.begin();
           proc_it != m_procId2FuncProf.end(); ++proc_it )
      {
         // m_procId2FuncProf.first
         //
         CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
         send_buffer_size += size;

         // m_procId2FuncProf.second.size()
         //
         CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
         send_buffer_size += size;

         // m_procId2FuncProf.second
         //
         for( std::map<uint32_t, FuncProfS>::iterator
              prof_it = proc_it->second.begin();
              prof_it != proc_it->second.end(); ++prof_it )
         {
            send_buffer_size += prof_it->second.getPackSize();
         }
      }
   }

   // allocate memory for the send buffer
   //
   send_buffer = new char[send_buffer_size];
   vt_assert( send_buffer );

   // pack send buffer
   //

   send_buffer_pos = 0;

   // m_procId2FuncProf.size()
   //
   proc_map_size = m_procId2FuncProf.size();
   CALL_MPI( MPI_Pack( &proc_map_size, 1, MPI_UNSIGNED, send_buffer,
                       send_buffer_size, &send_buffer_pos, MPI_COMM_WORLD ) );

   SLAVE
   {
      // m_procId2FuncProf
      //
      for( std::map<uint32_t, std::map<uint32_t, FuncProfS> >::iterator
           proc_it = m_procId2FuncProf.begin();
           proc_it != m_procId2FuncProf.end(); ++proc_it )
      {
         // m_procId2FuncProf.first
         //
         uint32_t proc_id = proc_it->first;
         CALL_MPI( MPI_Pack( &proc_id, 1, MPI_UNSIGNED, send_buffer,
                             send_buffer_size, &send_buffer_pos,
                             MPI_COMM_WORLD ) );

         // m_procId2FuncProf.second.size()
         //
         uint32_t prof_map_size = proc_it->second.size();
         CALL_MPI( MPI_Pack( &prof_map_size, 1, MPI_UNSIGNED, send_buffer,
                             send_buffer_size, &send_buffer_pos,
                             MPI_COMM_WORLD ) );

         // m_procId2FuncProf.second
         //
         for( std::map<uint32_t, FuncProfS>::iterator
              prof_it = proc_it->second.begin();
              prof_it != proc_it->second.end(); ++prof_it )
         {
            prof_it->second.pack( send_buffer, send_buffer_size,
                                  send_buffer_pos );
         }
      }
   }

   char * recv_buffer = 0;
   VT_MPI_INT recv_buffer_size = 0;
   VT_MPI_INT * recv_buffer_sizes = 0;
   VT_MPI_INT * recv_buffer_displs = 0;

   MASTER
   {
      // allocate memory for the receive buffer sizes
      //
      recv_buffer_sizes = new VT_MPI_INT[NumRanks];
      vt_assert( recv_buffer_sizes );
   }

   // gather buffer sizes
   CALL_MPI( MPI_Gather( &send_buffer_size, 1, MPI_INT, recv_buffer_sizes, 1,
                         MPI_INT, 0, MPI_COMM_WORLD ) );

   MASTER
   {
      // allocate memory for displacements
      //
      recv_buffer_displs = new VT_MPI_INT[NumRanks];
      vt_assert( recv_buffer_displs );

      // compute displacements and receive buffer size
      //
      for( VT_MPI_INT i = 0; i < NumRanks; i++ )
      {
         recv_buffer_size += recv_buffer_sizes[i];
         recv_buffer_displs[i] = 0;
         if( i > 0 )
         {
            recv_buffer_displs[i] =
               recv_buffer_displs[i-1] + recv_buffer_sizes[i-1];
         }
      }

      // allocate memory for the receive buffer
      //
      recv_buffer = new char[recv_buffer_size];
      vt_assert( recv_buffer );
   }

   // gather packed function profiles
   CALL_MPI( MPI_Gatherv( send_buffer, send_buffer_size, MPI_PACKED,
                          recv_buffer, recv_buffer_sizes, recv_buffer_displs,
                          MPI_PACKED, 0, MPI_COMM_WORLD ) );

   // free memory of send buffer
   delete [] send_buffer;

   MASTER
   {
      // unpack receive buffer
      //
      for( VT_MPI_INT i = 1; i < NumRanks; i++ )
      {
         char * buffer = recv_buffer + recv_buffer_displs[i];
         VT_MPI_INT buffer_size = recv_buffer_sizes[i];
         VT_MPI_INT buffer_pos = 0;

         // m_procId2FuncProf.size()
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &buffer_pos,
                               &proc_map_size, 1, MPI_UNSIGNED,
                               MPI_COMM_WORLD ) );

         // m_procId2FuncProf
         //
         for( uint32_t j = 0; j < proc_map_size; j++ )
         {
            // m_procId2FuncProf.first
            //
            uint32_t proc_id;
            CALL_MPI( MPI_Unpack( buffer, buffer_size, &buffer_pos,
                                  &proc_id, 1, MPI_UNSIGNED, MPI_COMM_WORLD ) );

            // m_procId2FuncProf.second.size()
            //
            uint32_t prof_map_size;
            CALL_MPI( MPI_Unpack( buffer, buffer_size, &buffer_pos,
                                  &prof_map_size, 1, MPI_UNSIGNED,
                                  MPI_COMM_WORLD ) );

            // m_procId2FuncProf.second
            //
            for( uint32_t k = 0; k < prof_map_size; k++ )
            {
               FuncProfS new_func_prof;
               new_func_prof.unpack( buffer, buffer_size, buffer_pos );

               m_procId2FuncProf[proc_id][new_func_prof.funcid] +=
                  new_func_prof;
            }
         }
      }

      // free some memory
      delete [] recv_buffer;
      delete [] recv_buffer_sizes;
      delete [] recv_buffer_displs;
   }
}

#endif // VT_MPI

//////////////////// struct HooksProfC::FuncProfS ////////////////////

#ifdef VT_MPI

VT_MPI_INT
HooksProfC::FuncProfS::getPackSize()
{
   VT_MPI_INT buffer_size = 0;
   VT_MPI_INT size;

   // funcid
   //
   CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // funcname (ignored)

   // count
   //
   CALL_MPI( MPI_Pack_size( 1, MPI_DOUBLE, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // incl + excl
   //
   CALL_MPI( MPI_Pack_size( 2, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   return buffer_size;
}

void
HooksProfC::FuncProfS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                             VT_MPI_INT & bufferPos )
{
   // funcid
   CALL_MPI( MPI_Pack( &funcid, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // funcname (ignored)

   // count
   CALL_MPI( MPI_Pack( &count, 1, MPI_DOUBLE, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // incl
   CALL_MPI( MPI_Pack( &incl, 1, MPI_LONG_LONG_INT, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // excl
   CALL_MPI( MPI_Pack( &excl, 1, MPI_LONG_LONG_INT, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );
}

void
HooksProfC::FuncProfS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                               VT_MPI_INT & bufferPos )
{
   // funcid
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &funcid, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // funcname (ignored)

   // count
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &count, 1,
                         MPI_DOUBLE, MPI_COMM_WORLD ) );

   // incl
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &incl, 1,
                         MPI_LONG_LONG_INT, MPI_COMM_WORLD ) );

   // excl
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &excl, 1,
                         MPI_LONG_LONG_INT, MPI_COMM_WORLD ) );

}

#endif // VT_MPI

HooksProfC::FuncProfS &
HooksProfC::FuncProfS::operator+=( const FuncProfS & a )
{
   if( funcid == 0 )
      funcid = a.funcid;

   count += a.count;
   incl += a.incl;
   excl += a.excl;

   return *this;
}

bool
HooksProfC::FuncProfS::operator==( const FuncProfS & a ) const
{
   return funcid == a.funcid;
}

bool
HooksProfC::FuncProfS::operator<( const FuncProfS & a) const
{
   int flags = Params.prof_sort_flags;

   if( (flags & FUNC_PROF_SORT_FLAG_FUNCNAME) &&
       (flags & FUNC_PROF_SORT_FLAG_DIR_UP ) )
   {
      return funcname.compare( a.funcname ) < 0;
   }
   else if( (flags & FUNC_PROF_SORT_FLAG_FUNCNAME) &&
            (flags & FUNC_PROF_SORT_FLAG_DIR_DOWN ) )
   {
      return funcname.compare( a.funcname ) > 0;
   }
   else if( (flags & FUNC_PROF_SORT_FLAG_COUNT) &&
            (flags & FUNC_PROF_SORT_FLAG_DIR_UP ) )
   {
      return count < a.count;
   }
   else if( (flags & FUNC_PROF_SORT_FLAG_COUNT) &&
            (flags & FUNC_PROF_SORT_FLAG_DIR_DOWN ) )
   {
      return count > a.count;
   }
   else if( (flags & FUNC_PROF_SORT_FLAG_INCL) &&
            (flags & FUNC_PROF_SORT_FLAG_DIR_UP ) )
   {
      return incl < a.incl;
   }
   else if( (flags & FUNC_PROF_SORT_FLAG_INCL) &&
            (flags & FUNC_PROF_SORT_FLAG_DIR_DOWN ) )
   {
      return incl > a.incl;
   }
   else if( (flags & FUNC_PROF_SORT_FLAG_EXCL) &&
            (flags & FUNC_PROF_SORT_FLAG_DIR_UP ) )
   {
      return excl < a.excl;
   }
   else if( (flags & FUNC_PROF_SORT_FLAG_EXCL) &&
            (flags & FUNC_PROF_SORT_FLAG_DIR_DOWN ) )
   {
      return excl > a.excl;
   }
   else if( (flags & FUNC_PROF_SORT_FLAG_INCL_CALL) &&
            (flags & FUNC_PROF_SORT_FLAG_DIR_UP ) )
   {
      return incl / count < a.incl / a.count;
   }
   else if( (flags & FUNC_PROF_SORT_FLAG_INCL_CALL) &&
            (flags & FUNC_PROF_SORT_FLAG_DIR_DOWN ) )
   {
      return incl / count > a.incl / a.count;
   }
   else if( (flags & FUNC_PROF_SORT_FLAG_EXCL_CALL) &&
            (flags & FUNC_PROF_SORT_FLAG_DIR_UP ) )
   {
      return excl / count < a.excl / a.count;
   }
   else if( (flags & FUNC_PROF_SORT_FLAG_EXCL_CALL) &&
            (flags & FUNC_PROF_SORT_FLAG_DIR_DOWN ) )
   {
      return excl / count > a.excl / a.count;
   }
   else
   {
      return true;
   }
}
