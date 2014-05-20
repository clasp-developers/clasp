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
#include "vt_filter_trc_hdlr.h"

#include <iostream>

// Define this macro to avoid writing of events which have relationships to
// disabled processes.
// For example: Do not write a send message event where its receive process is
// disabled.
//
// #define AGGRESSIVE_MESSAGE_FILTER

bool
FiltTrc__EventHandler_FirstArgS::stackPush( const uint32_t& proc,
                                            const uint32_t& func )
{
  // get function filter rules of process
  //
  std::map<uint32_t, std::map<uint32_t, int32_t> >::iterator proc_it =
    filter.procFuncLimits.find( proc );
  vt_assert( proc_it != filter.procFuncLimits.end() );

  // get current function's call limit
  std::map<uint32_t, int32_t>::iterator func_it = proc_it->second.find( func );

  int32_t limit = 1;

  // decrement function's call limit, if present
  //
  if( func_it != proc_it->second.end() )
  {
    if( func_it->second > 0 )
      --( func_it->second );
    limit = func_it->second;
  }

  // push call limit to call stack
  stack[proc].push( limit );

  return ( limit > 0 );
}

bool
FiltTrc__EventHandler_FirstArgS::stackPop( const uint32_t& proc )
{
  // show error message, if call stack is empty
  //
  if( stack.empty() )
  {
    std::cerr << ExeName << "error: stack underflow on process "
              << proc << std::endl;
    return false;
  }

  // pop call stack
  stack[proc].pop();

  return true;
}

bool
FiltTrc__EventHandler_FirstArgS::stackTop( const uint32_t& proc )
{
  if( stack[proc].empty() )
    return true;

  return ( stack[proc].top() > 0 );
}

// record handler definitions
//

int
FiltTrc__Handle_DefProcess( FiltTrc__DefHandler_FirstArg_BaseS* farg,
                            uint32_t streamid, uint32_t deftoken,
                            const char* name, uint32_t parent,
                            OTF_KeyValueList* list )
{
  int handler_rc = OTF_RETURN_OK;

  if( farg->isReading() )
  {
    FiltTrc__DefHandler_FirstArg_ReadS* farg_read =
      static_cast<FiltTrc__DefHandler_FirstArg_ReadS*>( farg );

    // add process and parent id to vector
    farg_read->addProc( deftoken, parent );
  }
  else // farg->isWriting()
  {
    FiltTrc__DefHandler_FirstArg_WriteS* farg_write =
      static_cast<FiltTrc__DefHandler_FirstArg_WriteS*>( farg );

    // write process definition, if it is enabled
    //
    if( !farg_write->filter.isProcOff( deftoken ) )
    {
      if( !OTF_WStream_writeDefProcessKV( farg_write->wstream, deftoken,
                                          name, parent, list ) )
        handler_rc = OTF_RETURN_ABORT;
    }
  }

  return handler_rc;
}

int
FiltTrc__Handle_DefProcessGroup( FiltTrc__DefHandler_FirstArg_BaseS* farg,
                                 uint32_t streamid, uint32_t deftoken,
                                 const char* name, uint32_t n,
                                 const uint32_t* array,
                                 OTF_KeyValueList* list )
{
  int handler_rc = OTF_RETURN_OK;

  vt_assert( farg->isWriting() );

  FiltTrc__DefHandler_FirstArg_WriteS* farg_write =
    static_cast<FiltTrc__DefHandler_FirstArg_WriteS*>( farg );

  // remove disabled processes from array of group members
  //

  uint32_t* new_array;
  uint32_t new_n;

  // keep array unchanged, if there are no disabled processes
  //
  if( farg_write->filter.procsOff.size() == 0 )
  {
    new_array = const_cast<uint32_t*>( array );
    new_n = n;
  }
  // otherwise, remove disabled processes from array
  //
  else
  {
    new_array = new uint32_t[n];
    new_n = 0;
    for( uint32_t i = 0; i < n; i++ )
    {
      if( !farg_write->filter.isProcOff( array[i] ) )
        new_array[new_n++] = array[i];
    }
  }

  // write process group definition, if its array of members isn't empty
  //
  if( new_n > 0 )
  {
    if( !OTF_WStream_writeDefProcessGroupKV( farg_write->wstream, deftoken,
                                             name, new_n, new_array, list ) )
      handler_rc = OTF_RETURN_ABORT;
  }

  if( new_array != array )
    delete [] new_array;

  return handler_rc;
}

int
FiltTrc__Handle_DefFunction( FiltTrc__DefHandler_FirstArg_BaseS* farg,
                             uint32_t streamid, uint32_t deftoken,
                             const char* name, uint32_t group,
                             uint32_t scltoken, OTF_KeyValueList* list )
{
  vt_assert( farg->isReading() );

  FiltTrc__DefHandler_FirstArg_ReadS* farg_read =
    static_cast<FiltTrc__DefHandler_FirstArg_ReadS*>( farg );

  // add function id and name to map
  farg_read->addFunc( deftoken, name );

  return OTF_RETURN_OK;
}

int
FiltTrc__Handle_BeginCollectiveOperation( FiltTrc__EventHandler_FirstArgS* farg,
                                          uint64_t time, uint32_t proc,
                                          uint32_t collOp, uint64_t matchingId,
                                          uint32_t procGroup, uint32_t rootProc,
                                          uint64_t sent, uint64_t received,
                                          uint32_t scltoken,
                                          OTF_KeyValueList* list )
{
  int handler_rc = OTF_RETURN_OK;

  bool write = farg->stackTop( proc );

  // write event, if its calling function isn't filtered
  //
  if( write )
  {
    if( !OTF_WStream_writeBeginCollectiveOperationKV( farg->wstream, time, proc,
                                                      collOp, matchingId,
                                                      procGroup, rootProc, sent,
                                                      received, scltoken,
                                                      list ) )
    {
      handler_rc = OTF_RETURN_ABORT;
    }
  }

  return handler_rc;
}

int
FiltTrc__Handle_BeginFileOperation( FiltTrc__EventHandler_FirstArgS* farg,
                                    uint64_t time, uint32_t proc,
                                    uint64_t matchingId, uint32_t scltoken,
                                    OTF_KeyValueList* list )
{
  int handler_rc = OTF_RETURN_OK;

  bool write = farg->stackTop( proc );

  // write event, if its calling function isn't filtered
  //
  if( write )
  {
    if( !OTF_WStream_writeBeginFileOperationKV( farg->wstream, time, proc,
                                                matchingId, scltoken, list ) )
    {
      handler_rc = OTF_RETURN_ABORT;
    }
  }

  return handler_rc;
}

int
FiltTrc__Handle_CollectiveOperation( FiltTrc__EventHandler_FirstArgS* farg,
                                     uint64_t time, uint32_t proc,
                                     uint32_t func, uint32_t communicator,
                                     uint32_t rootproc, uint32_t sent,
                                     uint32_t received, uint64_t duration,
                                     uint32_t scltoken, OTF_KeyValueList* list )
{
  int handler_rc = OTF_RETURN_OK;

  bool write = farg->stackTop( proc );

  // write event, if its calling function isn't filtered
  //
  if( write )
  {
    if( !OTF_WStream_writeCollectiveOperationKV( farg->wstream, time, proc,
                                                 func, communicator, rootproc,
                                                 sent, received, duration,
                                                 scltoken, list ) )
      handler_rc = OTF_RETURN_ABORT;
  }

  return handler_rc;
}

int
FiltTrc__Handle_Counter( FiltTrc__EventHandler_FirstArgS* farg, uint64_t time,
                         uint32_t proc, uint32_t cnttoken, uint64_t value,
                         OTF_KeyValueList* list )
{
  int handler_rc = OTF_RETURN_OK;

  bool write = farg->stackTop( proc );

  // write event, if its calling function isn't filtered
  //
  if( write )
  {
    if( !OTF_WStream_writeCounterKV( farg->wstream, time, proc, cnttoken, value,
                                     list ) )
      handler_rc = OTF_RETURN_ABORT;
  }

  return handler_rc;
}

int
FiltTrc__Handle_EndCollectiveOperation( FiltTrc__EventHandler_FirstArgS* farg,
                                        uint64_t time, uint32_t proc,
                                        uint64_t matchingId,
                                        OTF_KeyValueList* list )
{
  int handler_rc = OTF_RETURN_OK;

  bool write = farg->stackTop( proc );

  // write event, if its calling function isn't filtered
  //
  if( write )
  {
    if( !OTF_WStream_writeEndCollectiveOperationKV( farg->wstream, time, proc,
                                                    matchingId, list ) )
    {
      handler_rc = OTF_RETURN_ABORT;
    }
  }

  return handler_rc;
}

int
FiltTrc__Handle_Enter( FiltTrc__EventHandler_FirstArgS* farg, uint64_t time,
                       uint32_t func, uint32_t proc, uint32_t scltoken,
                       OTF_KeyValueList* list )
{
  int handler_rc = OTF_RETURN_OK;

  // decrement function's call limit push it to call stack
  // write enter event, if call limit > 0
  //
  if( farg->stackPush( proc, func ) )
  {
    if( !OTF_WStream_writeEnterKV( farg->wstream, time, func, proc, scltoken,
                                   list ) )
    handler_rc = OTF_RETURN_ABORT;
  }

  return handler_rc;
}

int
FiltTrc__Handle_EndFileOperation( FiltTrc__EventHandler_FirstArgS* farg,
                                  uint64_t time, uint32_t proc, uint32_t fileid,
                                  uint64_t matchingId, uint64_t handleId,
                                  uint32_t operation, uint64_t bytes,
                                  uint32_t scltoken, OTF_KeyValueList* list )
{
  int handler_rc = OTF_RETURN_OK;

  bool write = farg->stackTop( proc );

  // write event, if its calling function isn't filtered
  //
  if( write )
  {
    if( !OTF_WStream_writeEndFileOperationKV( farg->wstream, time, proc, fileid,
                                              matchingId, handleId, operation,
                                              bytes, scltoken, list ) )
    {
      handler_rc = OTF_RETURN_ABORT;
    }
  }

  return handler_rc;
}

int
FiltTrc__Handle_FileOperation( FiltTrc__EventHandler_FirstArgS* farg,
                               uint64_t time, uint32_t fileid, uint32_t proc,
                               uint64_t handleid, uint32_t operation,
                               uint64_t bytes, uint64_t duration,
                               uint32_t source, OTF_KeyValueList* list )
{
  int handler_rc = OTF_RETURN_OK;

  bool write = farg->stackTop( proc );

  // write event, if its calling function isn't filtered
  //
  if( write )
  {
    if( !OTF_WStream_writeFileOperationKV( farg->wstream, time, fileid, proc,
                                           handleid, operation, bytes, duration,
                                           source, list ) )
    {
      handler_rc = OTF_RETURN_ABORT;
    }
  }

  return handler_rc;
}

int
FiltTrc__Handle_Leave( FiltTrc__EventHandler_FirstArgS* farg, uint64_t time,
                       uint32_t func, uint32_t proc, uint32_t scltoken,
                       OTF_KeyValueList* list )
{
  int handler_rc = OTF_RETURN_OK;

  bool write = farg->stackTop( proc );

  // write event, if function's enter event isn't filtered
  //
  if( write )
  {
    if( !OTF_WStream_writeLeaveKV( farg->wstream, time, func, proc, scltoken,
                                   list ) )
    handler_rc = OTF_RETURN_ABORT;
  }

  // pop call stack
  if( !farg->stackPop( proc ) )
    handler_rc = OTF_RETURN_ABORT;

  return handler_rc;
}

int
FiltTrc__Handle_RecvMsg( FiltTrc__EventHandler_FirstArgS* farg, uint64_t time,
                         uint32_t receiver, uint32_t sender,
                         uint32_t communicator, uint32_t msgtype,
                         uint32_t msglength, uint32_t scltoken,
                         OTF_KeyValueList* list )
{
  int handler_rc = OTF_RETURN_OK;

  // check whether calling function isn't filtered
  bool write = farg->stackTop( receiver );
#ifdef AGGRESSIVE_MESSAGE_FILTER
  // ... and whether sender process is enabled
  write = ( write && !farg->filter.isProcOff( sender ) );
#endif // AGGRESSIVE_MESSAGE_FILTER

  // write event, if allowed
  //
  if( write )
  {
    if( !OTF_WStream_writeRecvMsgKV( farg->wstream, time, receiver, sender,
                                     communicator, msgtype, msglength, scltoken,
                                     list ) )
    {
      handler_rc = OTF_RETURN_ABORT;
    }
  }

  return handler_rc;
}

int
FiltTrc__Handle_RMAEnd( FiltTrc__EventHandler_FirstArgS* farg, uint64_t time,
                        uint32_t proc, uint32_t remote, uint32_t communicator,
                        uint32_t tag, uint32_t scltoken,
                        OTF_KeyValueList* list )
{
  int handler_rc = OTF_RETURN_OK;

  // check whether calling function isn't filtered
  bool write = farg->stackTop( proc );
#ifdef AGGRESSIVE_MESSAGE_FILTER
  // ... and whether remote process is enabled
  write = ( write && remote > 0 && !farg->filter.isProcOff( remote ) );
#endif // AGGRESSIVE_MESSAGE_FILTER

  // write event, if allowed
  //
  if( write )
  {
    if( !OTF_WStream_writeRMAEndKV( farg->wstream, time, proc, remote,
                                    communicator, tag, scltoken, list ) )
    {
      handler_rc = OTF_RETURN_ABORT;
    }
  }

  return handler_rc;
}

int
FiltTrc__Handle_RMAGet( FiltTrc__EventHandler_FirstArgS* farg, uint64_t time,
                        uint32_t proc, uint32_t origin, uint32_t target,
                        uint32_t communicator, uint32_t tag, uint64_t bytes,
                        uint32_t scltoken, OTF_KeyValueList* list )
{
  int handler_rc = OTF_RETURN_OK;

  // check whether calling function isn't filtered
  bool write = farg->stackTop( proc );
#ifdef AGGRESSIVE_MESSAGE_FILTER
  // ... and whether origin and target process is enabled
  write = ( write && !farg->filter.isProcOff( target ) &&
            ( origin == 0 || !farg->filter.isProcOff( origin ) ) );
#endif // AGGRESSIVE_MESSAGE_FILTER

  // write event, if allowed
  //
  if( write )
  {
    if( !OTF_WStream_writeRMAGetKV( farg->wstream, time, proc, origin, target,
                                    communicator, tag, bytes, scltoken, list ) )
    {
      handler_rc = OTF_RETURN_ABORT;
    }
  }

  return handler_rc;
}

int
FiltTrc__Handle_RMAPut( FiltTrc__EventHandler_FirstArgS* farg, uint64_t time,
                        uint32_t proc, uint32_t origin, uint32_t target,
                        uint32_t communicator, uint32_t tag, uint64_t bytes,
                        uint32_t scltoken, OTF_KeyValueList* list )
{
  int handler_rc = OTF_RETURN_OK;

  // check whether calling function isn't filtered
  bool write = farg->stackTop( proc );
#ifdef AGGRESSIVE_MESSAGE_FILTER
  // ... and whether origin and target process is enabled
  write = ( write && !farg->filter.isProcOff( target ) &&
            ( origin == 0 || !farg->filter.isProcOff( origin ) ) );
#endif // AGGRESSIVE_MESSAGE_FILTER

  // write event, if allowed
  //
  if( write )
  {
    if( !OTF_WStream_writeRMAPutKV( farg->wstream, time, proc, origin, target,
                                    communicator, tag, bytes, scltoken, list ) )
    {
      handler_rc = OTF_RETURN_ABORT;
    }
  }

  return handler_rc;
}

int
FiltTrc__Handle_RMAPutRemoteEnd( FiltTrc__EventHandler_FirstArgS* farg,
                                 uint64_t time, uint32_t proc, uint32_t origin,
                                 uint32_t target, uint32_t communicator,
                                 uint32_t tag, uint64_t bytes,
                                 uint32_t scltoken, OTF_KeyValueList* list )
{
  int handler_rc = OTF_RETURN_OK;

  // check whether calling function isn't filtered
  bool write = farg->stackTop( proc );
#ifdef AGGRESSIVE_MESSAGE_FILTER
  // ... and whether origin and target process is enabled
  write = ( write && !farg->filter.isProcOff( target ) &&
            ( origin == 0 || !farg->filter.isProcOff( origin ) ) );
#endif // AGGRESSIVE_MESSAGE_FILTER

  // write event, if allowed
  //
  if( write )
  {
    if( !OTF_WStream_writeRMAPutRemoteEndKV( farg->wstream, time, proc, origin,
                                             target, communicator, tag, bytes,
                                             scltoken, list ) )
    {
      handler_rc = OTF_RETURN_ABORT;
    }
  }

  return handler_rc;
}

int
FiltTrc__Handle_SendMsg( FiltTrc__EventHandler_FirstArgS* farg,
                         uint64_t time, uint32_t sender, uint32_t receiver,
                         uint32_t communicator, uint32_t msgtype,
                         uint32_t msglength, uint32_t scltoken,
                         OTF_KeyValueList* list )
{
  int handler_rc = OTF_RETURN_OK;

  // check whether calling function isn't filtered
  bool write = farg->stackTop( sender );
#ifdef AGGRESSIVE_MESSAGE_FILTER
  // ... and whether receiver process is enabled
  write = ( write && !farg->filter.isProcOff( receiver ) );
#endif // AGGRESSIVE_MESSAGE_FILTER

  // write event, if allowed
  //
  if( write )
  {
    if( !OTF_WStream_writeSendMsgKV( farg->wstream, time, sender, receiver,
                                     communicator, msgtype, msglength, scltoken,
                                     list ) )
    {
      handler_rc = OTF_RETURN_ABORT;
    }
  }

  return handler_rc;
}
