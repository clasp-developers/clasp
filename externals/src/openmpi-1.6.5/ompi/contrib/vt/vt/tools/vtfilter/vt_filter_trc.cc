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

#include "vt_filter.h"
#include "vt_filter_trc.h"
#include "vt_filter_trc_hdlr.h"

#include "rfg.h"

#include <iostream>
#include <list>

#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

#if defined(HAVE_OMP) && HAVE_OMP
# include <omp.h>
#endif // HAVE_OMP

//////////////////// class FilterTraceC ////////////////////

// public methods
//

FilterTraceC::FilterTraceC() : FilterCommonC()
{
  // empty
}

FilterTraceC::~FilterTraceC()
{
  // empty
}

bool
FilterTraceC::run()
{
  bool error = false;

  VPrint( 1, "Filtering trace file\n" );

  do
  {
    MASTER
    {
      // check whether input and output trace file are the same
      //
      std::string output_trcfile = Params.f_output_trcfile + ".otf";
      struct stat output_trcfile_stat;
      if( stat( output_trcfile.c_str(), &output_trcfile_stat ) == 0 )
      {
        std::string input_trcfile = Params.input_trcfile + ".otf";
        struct stat input_trcfile_stat;
        int input_trcfile_stat_rc =
          stat( input_trcfile.c_str(), &input_trcfile_stat );
        vt_assert( input_trcfile_stat_rc == 0 );

        // fail, if device id and inode number are equal
        //
        if( input_trcfile_stat.st_dev == output_trcfile_stat.st_dev &&
            input_trcfile_stat.st_ino == output_trcfile_stat.st_ino )
        {
          std::cerr << ExeName << ": Error: Input and output trace file are "
                    << "the same. Aborting." << std::endl;
          error = true;
          break;
        }
      }

      // read definitions of input trace file to get processes and functions
      //
      std::vector<std::pair<uint32_t, uint32_t> > procs; // process/parent ids
      std::map<uint32_t, std::string> funcs;             // function ids/names
      if( ( error = !readDefinitions( procs, funcs ) ) )
        break;

      // read input filter file to get filter rules of each process and function
      //
      if( ( error = !readFilter( procs, funcs ) ) )
        break;

      // functions not needed anymore; free memory
      funcs.clear();

      // calculate the max. number of output streams, if not specified by
      // command line option
      //

      uint32_t max_output_streams = Params.f_max_output_streams;

      if( max_output_streams == 0 )
      {
        // get number of input streams
        //
        if( ( error = !getNumStreams( max_output_streams ) ) )
          break;

#ifdef VT_MPI
        // if we have more ranks as input streams set the max. number of
        // output streams to the number of ranks,
        //
        if( max_output_streams < (uint32_t)NumRanks )
          max_output_streams = NumRanks;
#endif // VT_MPI
      }

      // distribute enabled processes to output streams
      //

      uint32_t stream = 1;
      while( !procs.empty() )
      {
        std::vector<std::pair<uint32_t, uint32_t> >::iterator it =
          procs.begin();

        const uint32_t& proc = it->first;

        // process enabled?
        if( !m_filter.isProcOff( proc ) )
        {
          // yes, add it to stream
          m_streamProcs[stream].insert( proc );

          // get next stream id
          //
          if( stream == max_output_streams )
            stream = 1;
          else
            stream++;
        }

        // remove process from vector
        procs.erase( it );
      }

      // write OTF master control of output trace file
      //
      if( ( error = !writeMasterControl() ) )
        break;

      // read/write trace definitions
      //
      if( ( error = !processDefinitions() ) )
        break;

      // read/write trace markers
      //
      if( ( error = !processMarkers() ) )
        break;

    }

#ifdef VT_MPI
    if( NumRanks > 1 )
    {
      // block until all ranks have reached this point
      MPI_Barrier( MPI_COMM_WORLD );

      // get number and communicator of worker ranks regarding to the number of
      // output streams
      //
      getWorkerComm( m_streamProcs.size() );

      // leave this block and do nothing, if my rank is out of work
      //
      if( MyRank >= m_numWorkerRanks )
        break;

      // share filter rules to worker ranks
      //
      if( ( error = !shareFilter() ) )
        break;
    }
#endif // VT_MPI

    // do the actual work
    //

    // read/write trace events and statistics
    //
    if( ( error = !processEventsAndStatistics() ) )
      break;

    VPrint( 1, "Done\n" );

  } while( false );

  return !error;
}

// private methods
//

bool
FilterTraceC::readDefinitions(
  std::vector<std::pair<uint32_t, uint32_t> >& procs,
  std::map<uint32_t, std::string>& funcs )
{
  bool error = false;

  VPrint( 1, " Reading process and function definitions\n" );

  // open OTF file manager
  //
  OTF_FileManager* manager = OTF_FileManager_open( 1 );
  vt_assert( manager );

  // open OTF reader stream
  //
  OTF_RStream* rstream =
    OTF_RStream_open( Params.input_trcfile.c_str(), 0, manager );
  vt_assert( rstream );
  VPrint( 2, "  Opened OTF reader stream [namestub %s id 0]\n",
          Params.input_trcfile.c_str() );

  // get OTF handler array
  //
  OTF_HandlerArray* handlers = OTF_HandlerArray_open();
  vt_assert( handlers );

  // initialize first handler argument
  //
  FiltTrc__DefHandler_FirstArg_ReadS first_arg =
    FiltTrc__DefHandler_FirstArg_ReadS( procs, funcs );

  // set handlers
  //

  // OTF_DEFPROCESS_RECORD
  //
  OTF_HandlerArray_setHandler( handlers,
    (OTF_FunctionPointer*) FiltTrc__Handle_DefProcess,
    OTF_DEFPROCESS_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handlers,
    &first_arg, OTF_DEFPROCESS_RECORD );

  // OTF_DEFFUNCTION_RECORD
  //
  OTF_HandlerArray_setHandler( handlers,
    (OTF_FunctionPointer*) FiltTrc__Handle_DefFunction,
    OTF_DEFFUNCTION_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handlers,
    &first_arg, OTF_DEFFUNCTION_RECORD );

  // read definitions
  //
  uint64_t records_read;
  records_read = OTF_RStream_readDefinitions( rstream, handlers );
  if( records_read == OTF_READ_ERROR )
  {
    std::cerr << ExeName << ": Error: Could not read definitions of input "
              << "trace file " << Params.input_trcfile << ". Aborting."
              << std::endl;
    error = true;
  }

  // close OTF reader stream
  OTF_RStream_close( rstream );
  VPrint( 2, "  Closed OTF reader stream [namestub %s id 0]\n",
          Params.input_trcfile.c_str() );

  // close OTF handler array
  OTF_HandlerArray_close( handlers );
  // close OTF file manager
  OTF_FileManager_close( manager );

  return !error;
}

bool
FilterTraceC::readFilter(
  const std::vector<std::pair<uint32_t, uint32_t> >& procs,
  const std::map<uint32_t, std::string>& funcs )
{
  bool error = false;

  VPrint( 1, " Reading filter file\n" );

  // get RFG filter object
  //
  RFG_Filter* rfg_filter = RFG_Filter_init();
  vt_assert( rfg_filter );

  // set input filter file name
  RFG_Filter_setDefFile( rfg_filter, Params.f_input_filtfile.c_str() );

  do
  {
    // get global filter rules
    //

    // read input filter file to get global filter rules (rank = -1)
    //
    VPrint( 2, "  Reading global filter rules\n" );
    if( ( error = !RFG_Filter_readDefFile( rfg_filter, -1, 0 ) ) )
      break;

    // get call limits for each function
    //

    // add map for global filter rules
    m_filter.procFuncLimits[0] = std::map<uint32_t, int32_t>();

    // iterate over all functions
    for( std::map<uint32_t, std::string>::const_iterator it = funcs.begin();
         it != funcs.end(); it++ )
    {
      const uint32_t& func = it->first;
      const std::string& func_name = it->second;
      int32_t limit;

      // get function's call limit
      // TODO: consider function groups and recursiveness
      RFG_Filter_getRegionRules( rfg_filter, func_name.c_str(), 0, &limit,
                                 0, 0 );

      // if it's not the default call limit, assign it to function
      //
      if( limit != -1 )
        m_filter.procFuncLimits[0][func] = limit;
    }

    // get process specific filter rules
    //
    VPrint( 2, "  Reading process specific filter rules\n" );

    // iterate over all processes
    for( uint32_t i = 0; i < procs.size(); i++ )
    {
      const uint32_t& proc = procs[i].first;
      const uint32_t& parent = procs[i].second;

      vt_assert( proc > 0 );

      // parent process is disabled?
      uint8_t off = (uint8_t)( parent != 0 && m_filter.isProcOff( parent ) );

      // read input filter file to get process specific filter rules
      //
      VPrint( 2, "   Reading filter rules for process %d\n", proc );
      if( !off &&
          !RFG_Filter_readDefFile( rfg_filter, (int)proc - 1, &off ) )
      {
        error = true;
        break;
      }

      // process disabled?
      if( off )
      {
        // yes, add it to the set of disabled processes
        m_filter.procsOff.insert( proc );
        VPrint( 2, "    process %d disabled\n", proc );
      }
      else
      {
        // no, get process specific call limits for each function
        //

        // iterate over all functions
        for( std::map<uint32_t, std::string>::const_iterator it = funcs.begin();
             it != funcs.end(); it++ )
        {
          const uint32_t func = it->first;
          const std::string& func_name = it->second;
          int32_t limit;

          // get function's call limit
          // TODO: consider function groups and recursiveness
          RFG_Filter_getRegionRules( rfg_filter, func_name.c_str(), 0, &limit,
                                     0, 0 );

          // if it's not the default call limit and the function has no global
          // filter rule, assign the process specific call limit to function
          //
          if( limit != -1 &&
              m_filter.procFuncLimits[0].find( func ) ==
                m_filter.procFuncLimits[0].end() )
          {
            m_filter.procFuncLimits[proc][func] = limit;
          }
        }
      }
    }

  } while( false );

  // free RFG filter object
  RFG_Filter_free( rfg_filter );

  return !error;
}

#ifdef VT_MPI

bool
FilterTraceC::shareFilter()
{
  bool error = false;

  VPrint( 1, " Sharing filter rules\n" );

  vt_assert( m_numWorkerRanks > 1 );

  char* buffer;
  VT_MPI_INT buffer_size;
  VT_MPI_INT buffer_pos;

  const VT_MPI_INT msg_tag = 100;

  MASTER
  {
    int rank;

    // determine which rank gets which stream(s) to write
    //

    std::vector<std::vector<uint32_t> > streams( m_numWorkerRanks );

    rank = 0;

    // iterate over all output streams
    for( std::map<uint32_t, std::set<uint32_t> >::const_iterator stream_it =
         m_streamProcs.begin(); stream_it != m_streamProcs.end(); stream_it++ )
    {
      const uint32_t& stream = stream_it->first;
      streams[rank++].push_back( stream );
      if( rank == m_numWorkerRanks )
        rank = 0;
    }

    // send stream/process mapping and filter rules to worker ranks
    //

    // list of request handles and send buffers which are in use
    std::list<std::pair<MPI_Request, char*> > send_buffers;

    // iterate over all worker ranks
    for( VT_MPI_INT rank = 1; rank < m_numWorkerRanks; rank++ )
    {
      // remove request handles and send buffers from list which are not in use
      //
      VT_MPI_INT not_in_use = 1;
      while( send_buffers.size() > 0 && not_in_use )
      {
        MPI_Request& request = send_buffers.front().first;
        char*& request_buffer = send_buffers.front().second;

        // test for completed send
        //
        MPI_Status status;
        MPI_Test( &request, &not_in_use, &status );

        // send completed?
        // (request handle and send buffer isn't needed anymore)
        if( not_in_use )
        {
          // free memory of send buffer
          delete [] request_buffer;
          // remove request handle and send buffer from list
          send_buffers.pop_front();
        }
      }

      // get size needed for the send buffer
      //

      VT_MPI_INT size;

      buffer_size = 0;

      // streams[rank].size()
      //
      MPI_Pack_size( 1, MPI_UNSIGNED, m_workerComm, &size );
      buffer_size += size;

      // iterate over all output streams of receiver rank
      for( uint32_t i = 0; i < streams[rank].size(); i++ )
      {
        const uint32_t stream = streams[rank][i];

        // stream
        //
        MPI_Pack_size( 1, MPI_UNSIGNED, m_workerComm, &size );
        buffer_size += size;

        std::map<uint32_t, std::set<uint32_t> >::const_iterator stream_it =
          m_streamProcs.find( stream );
        vt_assert( stream_it != m_streamProcs.end() );

        const std::set<uint32_t>& procs = stream_it->second;

        // procs.size()
        //
        MPI_Pack_size( 1, MPI_UNSIGNED, m_workerComm, &size );
        buffer_size += size;

        // iterate over all processes of output stream
        for( std::set<uint32_t>::const_iterator proc_it = procs.begin();
             proc_it != procs.end(); proc_it++ )
        {
          const uint32_t& proc = *proc_it;

          // proc
          //
          MPI_Pack_size( 1, MPI_UNSIGNED, m_workerComm, &size );
          buffer_size += size;

          // m_filter.procFuncLimits[proc]
          //
          buffer_size += m_filter.getPackSize( proc, m_workerComm );
        }
      }

      // m_filter.procFuncLimits[0] + m_filter.procsOff
      //
      buffer_size += m_filter.getPackSize( 0, m_workerComm );

      // allocate memory for the send buffer
      //
      buffer = new char[buffer_size];
      vt_assert( buffer );

      // pack send buffer
      //

      buffer_pos = 0;

      // streams[rank].size()
      //
      uint32_t streams_size = streams[rank].size();
      MPI_Pack( &streams_size, 1, MPI_UNSIGNED, buffer, buffer_size,
                &buffer_pos, m_workerComm );

      // iterate over all output streams of receiver rank
      for( uint32_t i = 0; i < streams_size; i++ )
      {
        uint32_t stream = streams[rank][i];

        // stream
        //
        MPI_Pack( &stream, 1, MPI_UNSIGNED, buffer, buffer_size, &buffer_pos,
                  m_workerComm );

        std::map<uint32_t, std::set<uint32_t> >::iterator stream_it =
          m_streamProcs.find( stream );
        vt_assert( stream_it != m_streamProcs.end() );

        const std::set<uint32_t>& procs = stream_it->second;

        // procs.size()
        //
        uint32_t procs_size = procs.size();
        MPI_Pack( &procs_size, 1, MPI_UNSIGNED, buffer, buffer_size,
                  &buffer_pos, m_workerComm );

        // iterate over all processes of output stream
        for( std::set<uint32_t>::const_iterator proc_it = procs.begin();
             proc_it != procs.end(); proc_it++ )
        {
          uint32_t proc = *proc_it;

          // proc
          //
          MPI_Pack( (VT_MPI_INT*)&proc, 1, MPI_UNSIGNED, buffer, buffer_size, &buffer_pos,
                    m_workerComm );

          // m_filter.procFuncLimits[proc]
          //
          m_filter.pack( proc, buffer, buffer_size, buffer_pos,
                         m_workerComm );
        }

        // remove distributed stream from the output streams of rank 0
        m_streamProcs.erase( stream_it );
      }

      // m_filter.procFuncLimits[0] + m_filter.procsOff
      //
      m_filter.pack( 0, buffer, buffer_size, buffer_pos, m_workerComm );

      // send buffer to responsible rank
      //
      PVPrint( 2, " Sending filter rules to rank %d\n", rank );
      MPI_Request request;
      MPI_Isend( buffer, buffer_size, MPI_PACKED, rank, msg_tag, m_workerComm,
                 &request );

      // add request handle and send buffer to list
      send_buffers.push_back( std::make_pair( request, buffer ) );
    }

    // remove all request handles and send buffers from list
    //
    while( send_buffers.size() > 0 )
    {
      MPI_Status status;
      MPI_Request& request = send_buffers.front().first;
      char*& request_buffer = send_buffers.front().second;

      // wait until send is completed
      MPI_Wait( &request, &status );

      // free memory of send buffer
      delete [] request_buffer;
      // remove request handle and send buffer from list
      send_buffers.pop_front();
    }
  }
  else // SLAVE
  {
    MPI_Status status;

    // get buffer size
    //
    MPI_Probe( 0, msg_tag, m_workerComm, &status );
    MPI_Get_count( &status, MPI_PACKED, &buffer_size );

    // allocate memory for the receive buffer
    //
    buffer = new char[buffer_size];
    vt_assert( buffer );

    // receive buffer from rank 0
    PVPrint( 2, " Receiving filter rules from rank 0\n" );
    MPI_Recv( buffer, buffer_size, MPI_PACKED, 0, msg_tag, m_workerComm,
              &status );

    // unpack filter rules from the buffer
    //

    buffer_pos = 0;

    // number of output streams
    //
    uint32_t streams_size;
    MPI_Unpack( buffer, buffer_size, &buffer_pos, &streams_size, 1,
                MPI_UNSIGNED, m_workerComm );

    for( uint32_t i = 0; i < streams_size; i++ )
    {
      // stream
      //
      uint32_t stream;
      MPI_Unpack( buffer, buffer_size, &buffer_pos, &stream, 1, MPI_UNSIGNED,
                  m_workerComm );

      // m_streamProcs[stream].size()
      //
      uint32_t m_streamProcs_size;
      MPI_Unpack( buffer, buffer_size, &buffer_pos, &m_streamProcs_size, 1,
                  MPI_UNSIGNED, m_workerComm );

      for( uint32_t j = 0; j < m_streamProcs_size; j++ )
      {
        // proc
        //
        uint32_t proc;
        MPI_Unpack( buffer, buffer_size, &buffer_pos, &proc, 1, MPI_UNSIGNED,
                    m_workerComm );

        m_streamProcs[stream].insert( proc );

        // m_filter.procFuncLimits[proc]
        //
        m_filter.unpack( proc, buffer, buffer_size, buffer_pos,
                         m_workerComm );
      }
    }

    // m_filter.procFuncLimits[0] + m_filter.procsOff
    //
    m_filter.unpack( 0, buffer, buffer_size, buffer_pos, m_workerComm );

    // free memory of the receive buffer
    delete [] buffer;
  }

  return !error;
}

#endif // VT_MPI

bool
FilterTraceC::writeMasterControl()
{
  bool error = false;

  VPrint( 1, " Writing OTF master control\n" );

  // open OTF file manager
  //
  OTF_FileManager* manager = OTF_FileManager_open( 1 );
  vt_assert( manager );

  // create OTF master control
  //
  OTF_MasterControl* mc = OTF_MasterControl_new( manager );
  vt_assert( mc );
  VPrint( 2, "  Created new OTF master control object\n" );

  // append stream/process mappings to OTF master control
  //

  VPrint( 2, "  Appending stream/process mappings\n" );

  // iterate over all output streams
  for( std::map<uint32_t, std::set<uint32_t> >::const_iterator stream_it =
       m_streamProcs.begin(); stream_it != m_streamProcs.end(); stream_it++ )
  {
    const uint32_t& stream = stream_it->first;
    const std::set<uint32_t>& procs = stream_it->second;

    // append processes of output stream to OTF master control
    //

    // iterate over all processes of output stream
    for( std::set<uint32_t>::const_iterator proc_it = procs.begin();
         proc_it != procs.end(); proc_it++ )
    {
      const uint32_t& proc = *proc_it;

      // append process to output stream
      //
      int append_rc = OTF_MasterControl_append( mc, stream, proc );
      vt_assert( append_rc );
    }
  }

  // write OTF master control
  //

  VPrint( 2, "  Writing OTF master control to %s.otf\n",
          Params.f_output_trcfile.c_str() );

  if( !OTF_MasterControl_write( mc, Params.f_output_trcfile.c_str() ) )
  {
    std::cerr << ExeName << ": Error: Could not write OTF master control of "
              << "output trace file " << Params.f_output_trcfile << ". "
              << "Aborting." << std::endl;
    error = true;
  }

  // close OTF master control
  OTF_MasterControl_close( mc );
  // close OTF file manager
  OTF_FileManager_close( manager );

  return !error;
}

bool
FilterTraceC::processDefinitions()
{
  bool error = false;

  VPrint( 1, " Writing definitions\n" );

  // open OTF file manager
  // (max. number of simultaneously open files = 2: 1 reader, 1 writer)
  //
  OTF_FileManager* manager = OTF_FileManager_open( 2 );
  vt_assert( manager );

  // open OTF reader stream
  //
  OTF_RStream* rstream =
    OTF_RStream_open( Params.input_trcfile.c_str(), 0, manager );
  vt_assert( rstream );
  VPrint( 2, "  Opened OTF reader stream [namestub %s id 0]\n",
          Params.input_trcfile.c_str() );

  // open OTF writer stream
  //
  OTF_WStream* wstream =
    OTF_WStream_open( Params.f_output_trcfile.c_str(), 0, manager );
  vt_assert( wstream );
  VPrint( 2, "  Opened OTF writer stream [namestub %s id 0]\n",
          Params.f_output_trcfile.c_str() );

  // set compression of writer stream
  //
  OTF_WStream_setCompression( wstream,
                              (OTF_FileCompression)Params.f_compress_level );

  // get OTF handler array
  //
  OTF_HandlerArray* handlers = OTF_HandlerArray_open();
  vt_assert( handlers );

  // get OTF copy handlers
  OTF_HandlerArray_getCopyHandler_stream( handlers, wstream );

  // initialize first handler argument
  //
  FiltTrc__DefHandler_FirstArg_WriteS first_arg =
    FiltTrc__DefHandler_FirstArg_WriteS( wstream, m_filter );

  // set handlers
  //

  // OTF_DEFPROCESS_RECORD
  //
  OTF_HandlerArray_setHandler( handlers,
    (OTF_FunctionPointer*) FiltTrc__Handle_DefProcess,
    OTF_DEFPROCESS_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handlers,
    &first_arg, OTF_DEFPROCESS_RECORD );

  // OTF_DEFPROCESSGROUP_RECORD
  //
  OTF_HandlerArray_setHandler( handlers,
    (OTF_FunctionPointer*) FiltTrc__Handle_DefProcessGroup,
    OTF_DEFPROCESSGROUP_RECORD );
  OTF_HandlerArray_setFirstHandlerArg( handlers,
    &first_arg, OTF_DEFPROCESSGROUP_RECORD );

  // read/write definitions
  //
  uint64_t records_read;
  records_read = OTF_RStream_readDefinitions( rstream, handlers );
  if( records_read == OTF_READ_ERROR )
  {
    std::cerr << ExeName << ": Error: Could not write definitions to output "
              << "trace file " << Params.f_output_trcfile << ". Aborting."
              << std::endl;
    error = true;
  }

  // close OTF writer stream
  OTF_WStream_close( wstream );
  VPrint( 2, "  Closed OTF writer stream [namestub %s id 0]\n",
          Params.f_output_trcfile.c_str() );

  // close OTF reader stream
  OTF_RStream_close( rstream );
  VPrint( 2, "  Closed OTF reader stream [namestub %s id 0]\n",
          Params.input_trcfile.c_str() );

  // close OTF handler array
  OTF_HandlerArray_close( handlers );
  // close OTF file manager
  OTF_FileManager_close( manager );

  return !error;
}

bool
FilterTraceC::processMarkers()
{
  bool error = false;

  VPrint( 1, " Writing markers\n" );

  // open OTF file manager
  // (max. number of simultaneously open files = 2: 1 reader, 1 writer)
  //
  OTF_FileManager* manager = OTF_FileManager_open( 2 );
  vt_assert( manager );

  // open OTF reader stream
  //
  OTF_RStream* rstream =
    OTF_RStream_open( Params.input_trcfile.c_str(), 0, manager );
  vt_assert( rstream );
  VPrint( 2, "  Opened OTF reader stream [namestub %s id 0]\n",
          Params.input_trcfile.c_str() );

  // get OTF buffer to check whether the input trace file has markers
  OTF_RBuffer* rbuffer = OTF_RStream_getMarkerBuffer( rstream );

  if( rbuffer )
  {
    // open OTF writer stream
    //
    OTF_WStream* wstream =
      OTF_WStream_open( Params.f_output_trcfile.c_str(), 0, manager );
    vt_assert( wstream );
    VPrint( 2, "  Opened OTF writer stream [namestub %s id 0]\n",
            Params.f_output_trcfile.c_str() );

    // set compression of writer stream
    //
    OTF_WStream_setCompression( wstream,
                                (OTF_FileCompression)Params.f_compress_level );

    // get OTF handler array
    //
    OTF_HandlerArray* handlers = OTF_HandlerArray_open();
    vt_assert( handlers );

    // get OTF copy handlers
    OTF_HandlerArray_getCopyHandler_stream( handlers, wstream );

    // read/write markers
    //
    uint64_t records_read;
    records_read = OTF_RStream_readMarker( rstream, handlers );
    if( records_read == OTF_READ_ERROR )
    {
      std::cerr << ExeName << ": Error: Could not write markers to output "
                << "trace file " << Params.f_output_trcfile << ". Aborting. "
                << std::endl;
      error = true;
    }

    // close OTF writer stream
    OTF_WStream_close( wstream );
    VPrint( 2, "  Closed OTF writer stream [namestub %s id 0]\n",
            Params.f_output_trcfile.c_str() );

    // close OTF handler array
    OTF_HandlerArray_close( handlers );

  }
  else
  {
    VPrint( 2, "   no markers present\n" );
  }

  // close OTF reader stream
  OTF_RStream_close( rstream );
  VPrint( 2, "  Closed OTF reader stream [namestub %s id 0]\n",
          Params.input_trcfile.c_str() );

  // close OTF file manager
  OTF_FileManager_close( manager );

  return !error;
}

bool
FilterTraceC::processEventsAndStatistics()
{
  bool error = false;

  VPrint( 1, " Writing events and statistics\n" );

  // prepare progress, if enabled
  //
  if( Params.show_progress )
  {
    uint64_t max_bytes;

    if( !getMaxBytesToRead( max_bytes ) )
      return false;

    prepareProgress( max_bytes );
  }

  // put output stream ids into a vector, so we can iterate over it by an index
  // (necessary for the 'omp parallel for' below)
  //
  std::vector<uint32_t> streams;
  for( std::map<uint32_t, std::set<uint32_t> >::const_iterator stream_it =
       m_streamProcs.begin(); stream_it != m_streamProcs.end(); stream_it++ )
  {
    streams.push_back( stream_it->first );
  }

  uint32_t max_file_handles = Params.f_max_file_handles;

#if defined(HAVE_OMP) && HAVE_OMP
  // set number of threads
  //
  if( (int)streams.size() < omp_get_max_threads() )
    omp_set_num_threads( (int)streams.size() );
  PVPrint( 2, " Using %d thread(s)\n", omp_get_max_threads() );

  // calculate number of simultaneously open files in respect of
  // number of threads
  //
  max_file_handles /= omp_get_max_threads();
  if( max_file_handles == 0 )
    max_file_handles = 1;
#endif // HAVE_OMP

  // iterate over all output streams (OpenMP-parallel, if possible)
  int size = (int)streams.size();
  int i;
#if defined(HAVE_OMP) && HAVE_OMP
# pragma omp parallel for private(i)
#endif // HAVE_OMP
  for( i = 0; i < size; i++ )
  {
    const uint32_t& stream = streams[i];
    const std::set<uint32_t>& procs = m_streamProcs[stream];

    // open OTF file manager
    //
    OTF_FileManager* manager = OTF_FileManager_open( max_file_handles );
    vt_assert( manager );

    // open OTF reader
    //
    OTF_Reader* reader =
      OTF_Reader_open( Params.input_trcfile.c_str(), manager );
    vt_assert( reader );
    PVPrint( 2, " Opened OTF reader [namestub %s]\n",
             Params.input_trcfile.c_str() );

    // disable all processes to read
    OTF_Reader_setProcessStatusAll( reader, 0 );

    // map for call stack of each process
    std::map<uint32_t, std::stack<int32_t> > stack;

    // get local copy of filter rules
    FilterS filter = m_filter;

    // remove global filter rules from local copy
    //
    vt_assert( filter.procFuncLimits.find( 0 ) != filter.procFuncLimits.end() );
    filter.procFuncLimits.erase( filter.procFuncLimits.begin() );

    // iterate over all processes of output stream
    for( std::set<uint32_t>::const_iterator proc_it = procs.begin();
         proc_it != procs.end(); proc_it++ )
    {
      const uint32_t& proc = *proc_it;

      // add global filter rules to process specific filter rules
      //

      if( filter.procFuncLimits[proc].empty() )
        filter.procFuncLimits[proc] = std::map<uint32_t, int32_t>();

      // iterate over all functions
      for( std::map<uint32_t, int32_t>::const_iterator func_it =
           m_filter.procFuncLimits[0].begin();
           func_it != m_filter.procFuncLimits[0].end(); func_it++ )
      {
        const uint32_t& func = func_it->first;
        const int32_t& limit = func_it->second;

        // set global call limit for function
        filter.procFuncLimits[proc][func] = limit;
      }

      // add call stack for process
      stack[proc] = std::stack<int32_t>();

      // enable process for reading
      OTF_Reader_enableProcess( reader, proc );
    }

    // open OTF writer stream
    //
    OTF_WStream* wstream =
      OTF_WStream_open( Params.f_output_trcfile.c_str(), stream, manager );
    vt_assert( wstream );
    PVPrint( 2, " Opened OTF writer stream [namestub %s id %d]\n",
             Params.f_output_trcfile.c_str(), stream );

    // set compression of writer stream
    //
    OTF_WStream_setCompression( wstream,
                                (OTF_FileCompression)Params.f_compress_level );

    // get OTF handler array
    //
    OTF_HandlerArray* handlers = OTF_HandlerArray_open();
    vt_assert( handlers );

    // get OTF copy handlers
    OTF_HandlerArray_getCopyHandler_stream( handlers, wstream );

    // initialize first handler argument
    //
    FiltTrc__EventHandler_FirstArgS first_arg =
      FiltTrc__EventHandler_FirstArgS( wstream, filter, stack );

    // set handlers
    //

    // OTF_BEGINCOLLOP_RECORD
    //
    OTF_HandlerArray_setHandler( handlers,
      (OTF_FunctionPointer*) FiltTrc__Handle_BeginCollectiveOperation,
      OTF_BEGINCOLLOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &first_arg,
      OTF_BEGINCOLLOP_RECORD );

    // OTF_BEGINFILEOP_RECORD
    //
    OTF_HandlerArray_setHandler( handlers,
      (OTF_FunctionPointer*) FiltTrc__Handle_BeginFileOperation,
      OTF_BEGINFILEOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &first_arg,
      OTF_BEGINFILEOP_RECORD );

    // OTF_COLLOP_RECORD
    //
    OTF_HandlerArray_setHandler( handlers,
      (OTF_FunctionPointer*) FiltTrc__Handle_CollectiveOperation,
      OTF_COLLOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &first_arg,
      OTF_COLLOP_RECORD );

    // OTF_COUNTER_RECORD
    //
    OTF_HandlerArray_setHandler( handlers,
      (OTF_FunctionPointer*) FiltTrc__Handle_Counter,
      OTF_COUNTER_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &first_arg,
      OTF_COUNTER_RECORD );

    // OTF_ENDCOLLOP_RECORD
    //
    OTF_HandlerArray_setHandler( handlers,
      (OTF_FunctionPointer*) FiltTrc__Handle_EndCollectiveOperation,
      OTF_ENDCOLLOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &first_arg,
      OTF_ENDCOLLOP_RECORD );

    // OTF_ENTER_RECORD
    //
    OTF_HandlerArray_setHandler( handlers,
      (OTF_FunctionPointer*) FiltTrc__Handle_Enter,
      OTF_ENTER_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &first_arg,
      OTF_ENTER_RECORD );

    // OTF_ENDFILEOP_RECORD
    //
    OTF_HandlerArray_setHandler( handlers,
      (OTF_FunctionPointer*) FiltTrc__Handle_EndFileOperation,
      OTF_ENDFILEOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &first_arg,
      OTF_ENDFILEOP_RECORD );

    // OTF_FILEOPERATION_RECORD
    //
    OTF_HandlerArray_setHandler( handlers,
      (OTF_FunctionPointer*) FiltTrc__Handle_FileOperation,
      OTF_FILEOPERATION_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &first_arg,
      OTF_FILEOPERATION_RECORD );

    // OTF_LEAVE_RECORD
    //
    OTF_HandlerArray_setHandler( handlers,
      (OTF_FunctionPointer*) FiltTrc__Handle_Leave,
      OTF_LEAVE_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &first_arg,
      OTF_LEAVE_RECORD );

    // OTF_RECEIVE_RECORD
    //
    OTF_HandlerArray_setHandler( handlers,
      (OTF_FunctionPointer*) FiltTrc__Handle_RecvMsg,
      OTF_RECEIVE_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &first_arg,
      OTF_RECEIVE_RECORD );

    // OTF_RMAEND_RECORD
    //
    OTF_HandlerArray_setHandler( handlers,
      (OTF_FunctionPointer*) FiltTrc__Handle_RMAEnd,
      OTF_RMAEND_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &first_arg,
      OTF_RMAEND_RECORD );

    // OTF_RMAGET_RECORD
    //
    OTF_HandlerArray_setHandler( handlers,
      (OTF_FunctionPointer*) FiltTrc__Handle_RMAGet,
      OTF_RMAGET_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &first_arg,
      OTF_RMAGET_RECORD );

    // OTF_RMAPUT_RECORD
    //
    OTF_HandlerArray_setHandler( handlers,
      (OTF_FunctionPointer*) FiltTrc__Handle_RMAPut,
      OTF_RMAPUT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &first_arg,
      OTF_RMAPUT_RECORD );

    // OTF_RMAPUTRE_RECORD
    //
    OTF_HandlerArray_setHandler( handlers,
      (OTF_FunctionPointer*) FiltTrc__Handle_RMAPutRemoteEnd,
      OTF_RMAPUTRE_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &first_arg,
      OTF_RMAPUTRE_RECORD );

    // OTF_SEND_RECORD
    //
    OTF_HandlerArray_setHandler( handlers,
      (OTF_FunctionPointer*) FiltTrc__Handle_SendMsg,
      OTF_SEND_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, &first_arg,
      OTF_SEND_RECORD );

    // set record limit, if progress is enabled
    //
    if( Params.show_progress )
      OTF_Reader_setRecordLimit( reader, 1000000 );

    // read/write events and statistics
    //

    uint64_t records_read = 0;
    uint64_t last_cur = 0;

    // read/write events
    //
    while( ( records_read = OTF_Reader_readEvents( reader, handlers ) )
           != OTF_READ_ERROR )
    {
      // update progress, if enabled
      //
      if( Params.show_progress )
      {
        uint64_t min, cur, max;
        OTF_Reader_eventBytesProgress( reader, &min, &cur, &max );

        updateProgress( cur - last_cur );
        last_cur = cur;
      }

      // leave loop, if reading is done
      //
      if( records_read == 0 )
        break;
    }

    // show error message, if an error occurred during reading/writing events
    //

#if defined(HAVE_OMP) && HAVE_OMP
#   pragma omp critical
    {
#endif // HAVE_OMP
    if( records_read == OTF_READ_ERROR )
    {
      std::cerr << ExeName
                << ": An error occurred during reading/writing events. "
                << "Aborting." << std::endl;
      error = true;
    }
#if defined(HAVE_OMP) && HAVE_OMP
    } // omp critical
#endif // HAVE_OMP

    // read/write statistics, if no error occurred
    //
    if( !error )
    {
      records_read = 0;
      last_cur = 0;
      while( ( records_read = OTF_Reader_readStatistics( reader, handlers ) )
             != OTF_READ_ERROR )
      {
        // update progress, if enabled
        //
        if( Params.show_progress )
        {
          uint64_t min, cur, max;
          OTF_Reader_statisticBytesProgress( reader, &min, &cur, &max );

          updateProgress( cur - last_cur );
          last_cur = cur;
        }

        // leave loop, if reading is done
        //
        if( records_read == 0 )
          break;
      }

      // ignore read error, because a trace must not have statistics
    }

    // close OTF writer stream
    OTF_WStream_close( wstream );
    PVPrint( 2, " Closed OTF writer stream [namestub %s id %d]\n",
             Params.f_output_trcfile.c_str(), stream );

    // close OTF reader
    OTF_Reader_close( reader );
    PVPrint( 2, " Closed OTF reader [namestub %s]\n",
             Params.input_trcfile.c_str() );

    // close OTF handler array
    OTF_HandlerArray_close( handlers );
    // close OTF file manager
    OTF_FileManager_close( manager );
  }

  // finish progress, if enabled
  //
  if( Params.show_progress )
    finishProgress();

  return !error;
}

bool
FilterTraceC::getNumStreams( uint32_t& numStreams )
{
  bool error = false;

  // open OTF file manager
  //
  OTF_FileManager* manager = OTF_FileManager_open( 1 );
  vt_assert( manager );

  // create OTF master control
  //
  OTF_MasterControl* mc = OTF_MasterControl_new( manager );
  vt_assert( mc );

  // read OTF master control of input trace file
  //
  int read_rc = OTF_MasterControl_read( mc, Params.input_trcfile.c_str() );
  vt_assert( read_rc );

  // get number of input streams
  numStreams = OTF_MasterControl_getCount( mc );

  // close OTF master control
  OTF_MasterControl_close( mc );
  // close OTF file manager
  OTF_FileManager_close( manager );

  return !error;
}

bool
FilterTraceC::getMaxBytesToRead( uint64_t& maxBytes )
{
  bool error = false;

  maxBytes = 0;

  // iterate over all output streams
  std::map<uint32_t, std::set<uint32_t> >::const_iterator stream_it;
  for( stream_it = m_streamProcs.begin(); stream_it != m_streamProcs.end();
       stream_it++ )
  {
    const std::set<uint32_t>& procs = stream_it->second;

    // open OTF file manager
    //
    OTF_FileManager* manager = OTF_FileManager_open( Params.f_max_file_handles );
    vt_assert( manager );

    // open OTF reader
    //
    OTF_Reader* reader =
      OTF_Reader_open( Params.input_trcfile.c_str(), manager );
    vt_assert( reader );

    // get OTF handler array
    //
    OTF_HandlerArray* handlers = OTF_HandlerArray_open();
    vt_assert( handlers );

    // select processes to read
    //
    OTF_Reader_setProcessStatusAll( reader, 0 );
    for( std::set<uint32_t>::const_iterator proc_it = procs.begin();
         proc_it != procs.end(); proc_it++ )
    {
      const uint32_t& proc = *proc_it;
      OTF_Reader_enableProcess( reader, proc );
    }

    // set record limit to 0 to get only the progress information
    OTF_Reader_setRecordLimit( reader, 0 );

    uint64_t min, cur, max;

    // get max. bytes of events
    //
    if( OTF_Reader_readEvents( reader, handlers ) != OTF_READ_ERROR )
    {
      OTF_Reader_eventBytesProgress( reader, &min, &cur, &max );
      maxBytes += max;
    }

    // get max. bytes of statistics
    //
    if( OTF_Reader_readStatistics( reader, handlers ) != OTF_READ_ERROR )
    {
      OTF_Reader_statisticBytesProgress( reader, &min, &cur, &max );
      maxBytes += max;
    }

    // close OTF handler array
    OTF_HandlerArray_close( handlers );
    // close OTF reader
    OTF_Reader_close( reader );
    // close OTF file manager
    OTF_FileManager_close( manager );
  }

  return !error;
}

//////////////////// struct FilterTraceC::FilterS ////////////////////

#ifdef VT_MPI

VT_MPI_INT
FilterTraceC::FilterS::getPackSize( const uint32_t& proc, const MPI_Comm& comm )
{
  VT_MPI_INT buffer_size = 0;
  VT_MPI_INT size;

  if( proc == 0 )
  {
    // procsOff.size() + procsOff
    //
    MPI_Pack_size( 1 + procsOff.size(), MPI_UNSIGNED, comm, &size );
    buffer_size += size;
  }

  // procFuncLimits[proc].size()
  //
  MPI_Pack_size( 1, MPI_UNSIGNED, comm, &size );
  buffer_size += size;

  std::map<uint32_t, std::map<uint32_t, int32_t> >::const_iterator proc_it =
    procFuncLimits.find( proc );

  if( proc_it != procFuncLimits.end() )
  {
    const std::map<uint32_t, int32_t>& func_limits = proc_it->second;

    // procFuncLimits[proc]
    //
    std::map<uint32_t, int32_t>::const_iterator func_it;
    for( func_it = func_limits.begin(); func_it != func_limits.end();
         func_it++ )
    {
      // procFuncLimits[proc].first
      //
      MPI_Pack_size( 1, MPI_UNSIGNED, comm, &size );
      buffer_size += size;

      // procFuncLimits[proc].second
      //
      MPI_Pack_size( 1, MPI_INT, comm, &size );
      buffer_size += size;
    }
  }

  return buffer_size;
}

void
FilterTraceC::FilterS::pack( const uint32_t& proc, char*& buffer,
                             const VT_MPI_INT& bufferSize,
                             VT_MPI_INT& bufferPos, const MPI_Comm& comm )
{
   if( proc == 0 )
  {
    // procsOff.size()
    //
    uint32_t procsOff_size = procsOff.size();
    MPI_Pack( &procsOff_size, 1, MPI_UNSIGNED, buffer, bufferSize, &bufferPos,
              comm );

    // procsOff
    //
    for( std::set<uint32_t>::const_iterator proc_it = procsOff.begin();
         proc_it != procsOff.end(); proc_it++ )
    {
      uint32_t off_proc = *proc_it;
      MPI_Pack( &off_proc, 1, MPI_UNSIGNED, buffer, bufferSize, &bufferPos,
                comm );
    }
  }

  // procFuncLimits[proc].size()
  //
  uint32_t procFuncLimits_size;
  std::map<uint32_t, std::map<uint32_t, int32_t> >::const_iterator proc_it =
    procFuncLimits.find( proc );

  if( proc_it == procFuncLimits.end() )
    procFuncLimits_size = 0;
  else
    procFuncLimits_size = proc_it->second.size();

  MPI_Pack( &procFuncLimits_size, 1, MPI_UNSIGNED, buffer, bufferSize,
            &bufferPos, comm );

  if( proc_it != procFuncLimits.end() )
  {
    const std::map<uint32_t, int32_t>& func_limits = proc_it->second;

    // procFuncLimits[proc]
    //
    std::map<uint32_t, int32_t>::const_iterator func_it;
    for( func_it = func_limits.begin(); func_it != func_limits.end();
         func_it++ )
    {
      uint32_t func = func_it->first;
      int32_t limit = func_it->second;

      // func
      MPI_Pack( &func, 1, MPI_UNSIGNED, buffer, bufferSize, &bufferPos, comm );

      // limit
      MPI_Pack( &limit, 1, MPI_INT, buffer, bufferSize, &bufferPos, comm );
    }
  }
}

void
FilterTraceC::FilterS::unpack( const uint32_t& proc, char*& buffer,
                               const VT_MPI_INT& bufferSize,
                               VT_MPI_INT& bufferPos, const MPI_Comm& comm )
{
  if( proc == 0 )
  {
    // procsOff.size()
    //
    uint32_t procsOff_size;
    MPI_Unpack( buffer, bufferSize, &bufferPos, &procsOff_size, 1, MPI_UNSIGNED,
                comm );

    // procsOff
    //
    for( uint32_t i = 0; i < procsOff_size; i++ )
    {
      uint32_t off_proc;
      MPI_Unpack( buffer, bufferSize, &bufferPos, &off_proc, 1, MPI_UNSIGNED, comm );

      procsOff.insert( proc );
    }
  }

  // procFuncLimits[proc].size()
  //
  uint32_t procFuncLimits_size;
  MPI_Unpack( buffer, bufferSize, &bufferPos, &procFuncLimits_size, 1,
              MPI_UNSIGNED, comm );

  // procFuncLimits[proc]
  //
  if( proc == 0 && procFuncLimits_size == 0 )
  {
    procFuncLimits[0] = std::map<uint32_t, int32_t>();
  }
  else
  {
    for( uint32_t i = 0; i < procFuncLimits_size; i++ )
    {
      uint32_t func;
      int32_t limit;

      // func
      MPI_Unpack( buffer, bufferSize, &bufferPos, &func, 1, MPI_UNSIGNED, comm );

      // limit
      MPI_Unpack( buffer, bufferSize, &bufferPos, &limit, 1, MPI_INT, comm );

      procFuncLimits[proc][func] = limit;
    }
  }
}

#endif // VT_MPI
