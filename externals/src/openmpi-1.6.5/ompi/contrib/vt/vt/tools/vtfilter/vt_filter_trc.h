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

#ifndef _VT_FILTER_TRC_H_
#define _VT_FILTER_TRC_H_

#include "vt_filter_common.h"

#include <map>
#include <set>
#include <string>
#include <vector>

//
// FilterTraceC class
//
class FilterTraceC : public FilterCommonC
{
public:

  //
  // data structure for filter rules
  //
  struct FilterS
  {
    // check whether a process id is disabled
    inline bool isProcOff( const uint32_t& proc ) const
    {
      return ( procsOff.find( proc ) != procsOff.end() );
    }

#ifdef VT_MPI
    // The following functions are significant for the MPI-parallel version of
    // vtfilter to transfer the filter rules to all worker ranks.

    // get size needed to pack filter information
    VT_MPI_INT getPackSize( const uint32_t& proc,
                            const MPI_Comm& comm = MPI_COMM_WORLD );

    // pack filter rules into a buffer
    void pack( const uint32_t& proc, char*& buffer,
               const VT_MPI_INT& bufferSize, VT_MPI_INT& bufferPos,
               const MPI_Comm& comm = MPI_COMM_WORLD );

    // unpack filter rules from a buffer
    void unpack( const uint32_t& proc, char*& buffer,
                 const VT_MPI_INT& bufferSize, VT_MPI_INT& bufferPos,
                 const MPI_Comm& comm = MPI_COMM_WORLD );
#endif // VT_MPI

    // set of disabled process ids
    std::set<uint32_t>                              procsOff;

    // map for global and process specific filter rules
    // (process id 0 holds the global filter rules)
    std::map<uint32_t,          /* process id */
             std::map<uint32_t, /* function id */
             int32_t            /* call limit */> > procFuncLimits;

  };

  // contructor
  FilterTraceC();

  // destructor
  ~FilterTraceC();

  // filter input trace file
  bool run();

private:

  // read definitions of input trace file to get processes and functions
  bool readDefinitions( std::vector<std::pair<uint32_t, uint32_t> >& procs,
                        std::map<uint32_t, std::string>& funcs );

  // read input filter file to get filter rules of each process and function
  bool readFilter( const std::vector<std::pair<uint32_t, uint32_t> >& procs,
                   const std::map<uint32_t, std::string>& funcs );

#ifdef VT_MPI
  // share filter rules to worker ranks
  bool shareFilter( void );
#endif // VT_MPI

  // write OTF master control of output trace file
  bool writeMasterControl( void );

  // read/write trace definitions
  bool processDefinitions( void );

  // read/write trace markers
  bool processMarkers( void );

  // read/write trace events and statistics
  bool processEventsAndStatistics( void );

  // get the number of input streams
  bool getNumStreams( uint32_t& numStreams );

  // get max. bytes to read (significant only for progress)
  bool getMaxBytesToRead( uint64_t& maxBytes );

  // map of output stream/process ids
  std::map<uint32_t, std::set<uint32_t> > m_streamProcs;

  // storage of filter rules
  FilterS                                 m_filter;

};

#endif // _VT_FILTER_TRC_H_
