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

#ifndef _VT_FILTER_TRC_HDLR_H_
#define _VT_FILTER_TRC_HDLR_H_

#include "vt_filter_trc.h"

#include "otf.h"

#include <stack>

//
// data structure for first argument of definition handlers
// (base)
//
struct FiltTrc__DefHandler_FirstArg_BaseS
{
  // constructor
  FiltTrc__DefHandler_FirstArg_BaseS( int _mode )
  {
    mode = static_cast<mode_e>( _mode );
  }

  // check whether handler mode is reading
  inline bool isReading( void ) { return ( mode == READING ); }

  // check whether handler mode is writing
  inline bool isWriting( void ) { return ( mode == WRITING ); }

  // handler mode (reading or writing)
  enum mode_e { READING, WRITING } mode;

};

//
// data structure for first argument of definition handlers
// (reading)
//
struct FiltTrc__DefHandler_FirstArg_ReadS
  : public FiltTrc__DefHandler_FirstArg_BaseS
{
  // constructor
  FiltTrc__DefHandler_FirstArg_ReadS(
    std::vector<std::pair<uint32_t, uint32_t> >& _procs,
    std::map<uint32_t, std::string>&             _funcs )
    : FiltTrc__DefHandler_FirstArg_BaseS( READING ),
      procs( _procs ), funcs( _funcs ) {}

  // add process/parent id to vector
  inline void addProc( const uint32_t& proc, const uint32_t& parent )
  {
    procs.push_back( std::make_pair( proc, parent ) );
  }

  // add function id/name to map
  inline void addFunc( const uint32_t& func, const std::string& name )
  {
    funcs.insert( std::make_pair( func, name ) );
  }

  // vector of process/parent ids
  std::vector<std::pair<uint32_t, uint32_t> >& procs;

  // map of function ids/names
  std::map<uint32_t, std::string>&             funcs;

};

//
// data structure for first argument of definition handlers
// (writing)
//
struct FiltTrc__DefHandler_FirstArg_WriteS
  : public FiltTrc__DefHandler_FirstArg_BaseS
{
  // constructor
  FiltTrc__DefHandler_FirstArg_WriteS( OTF_WStream*& _wstream,
                                       const FilterTraceC::FilterS& _filter )
    : FiltTrc__DefHandler_FirstArg_BaseS( WRITING ),
      wstream( _wstream ), filter( _filter ) {}

  // OTF writer stream
  OTF_WStream*&                wstream;

  // filter rules
  const FilterTraceC::FilterS& filter;

};

//
// data structure for first argument of event handlers
//
struct FiltTrc__EventHandler_FirstArgS
{
  // constructor
  FiltTrc__EventHandler_FirstArgS( OTF_WStream*& _wstream,
    FilterTraceC::FilterS& _filter,
    std::map<uint32_t, std::stack<int32_t> >& _stack )
    : wstream( _wstream ), filter( _filter ), stack( _stack ) {}

  // decrement function's call limit and push it to call stack
  // returns true, if call limit > 0
  inline bool stackPush( const uint32_t& proc, const uint32_t& func );

  // pop call stack
  // returns false, if a stack underflow occurred
  inline bool stackPop( const uint32_t& proc );

  // returns true, if call limit on call stack's top > 0
  inline bool stackTop( const uint32_t& proc );

  // OTF writer stream
  OTF_WStream*& wstream;

  // filter rules
  FilterTraceC::FilterS& filter;

  // map for call stack of each process
  std::map<uint32_t, std::stack<int32_t> >& stack;

};

// record handler declarations
//

int FiltTrc__Handle_DefProcess(
      FiltTrc__DefHandler_FirstArg_BaseS* farg, uint32_t streamid,
      uint32_t deftoken, const char* name, uint32_t parent,
      OTF_KeyValueList* list );

int FiltTrc__Handle_DefProcessGroup(
      FiltTrc__DefHandler_FirstArg_BaseS* farg, uint32_t streamid,
      uint32_t deftoken, const char* name, uint32_t n, const uint32_t* array,
      OTF_KeyValueList* list );

int FiltTrc__Handle_DefFunction(
      FiltTrc__DefHandler_FirstArg_BaseS* farg, uint32_t streamid,
      uint32_t deftoken, const char* name, uint32_t group, uint32_t scltoken,
      OTF_KeyValueList* list );

int FiltTrc__Handle_BeginCollectiveOperation(
      FiltTrc__EventHandler_FirstArgS* farg, uint64_t time, uint32_t proc,
      uint32_t collOp, uint64_t matchingId, uint32_t procGroup,
      uint32_t rootProc, uint64_t sent, uint64_t received, uint32_t scltoken,
      OTF_KeyValueList* list );

int FiltTrc__Handle_BeginFileOperation(
      FiltTrc__EventHandler_FirstArgS* farg, uint64_t time, uint32_t proc,
      uint64_t matchingId, uint32_t scltoken, OTF_KeyValueList* list );

int FiltTrc__Handle_CollectiveOperation(
      FiltTrc__EventHandler_FirstArgS* farg, uint64_t time, uint32_t proc,
      uint32_t func, uint32_t communicator, uint32_t rootproc, uint32_t sent,
      uint32_t received, uint64_t duration, uint32_t scltoken,
      OTF_KeyValueList* list );

int FiltTrc__Handle_Counter(
      FiltTrc__EventHandler_FirstArgS* farg, uint64_t time, uint32_t proc,
      uint32_t cnttoken, uint64_t value, OTF_KeyValueList* list );

int FiltTrc__Handle_EndCollectiveOperation(
      FiltTrc__EventHandler_FirstArgS* farg, uint64_t time, uint32_t proc,
      uint64_t matchingId, OTF_KeyValueList* list );

int FiltTrc__Handle_Enter(
      FiltTrc__EventHandler_FirstArgS* farg, uint64_t time, uint32_t func,
      uint32_t proc, uint32_t scltoken, OTF_KeyValueList* list );

int FiltTrc__Handle_EndFileOperation(
      FiltTrc__EventHandler_FirstArgS* farg, uint64_t time, uint32_t proc,
      uint32_t fileid, uint64_t matchingId, uint64_t handleId,
      uint32_t operation, uint64_t bytes, uint32_t scltoken,
      OTF_KeyValueList* list );

int FiltTrc__Handle_FileOperation(
      FiltTrc__EventHandler_FirstArgS* farg, uint64_t time, uint32_t fileid,
      uint32_t proc, uint64_t handleid, uint32_t operation, uint64_t bytes,
      uint64_t duration, uint32_t source, OTF_KeyValueList* list );

int FiltTrc__Handle_Leave( FiltTrc__EventHandler_FirstArgS* farg, uint64_t time,
      uint32_t func, uint32_t proc, uint32_t scltoken, OTF_KeyValueList* list );

int FiltTrc__Handle_RecvMsg(
      FiltTrc__EventHandler_FirstArgS* farg, uint64_t time, uint32_t receiver,
      uint32_t sender, uint32_t communicator, uint32_t msgtype,
      uint32_t msglength, uint32_t scltoken, OTF_KeyValueList* list );

int FiltTrc__Handle_RMAEnd(
      FiltTrc__EventHandler_FirstArgS* farg, uint64_t time, uint32_t proc,
      uint32_t remote, uint32_t communicator, uint32_t tag, uint32_t scltoken,
      OTF_KeyValueList* list );

int FiltTrc__Handle_RMAGet(
      FiltTrc__EventHandler_FirstArgS* farg, uint64_t time, uint32_t proc,
      uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
      uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list );

int FiltTrc__Handle_RMAPut(
      FiltTrc__EventHandler_FirstArgS* farg, uint64_t time, uint32_t proc,
      uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
      uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list );

int FiltTrc__Handle_RMAPutRemoteEnd(
      FiltTrc__EventHandler_FirstArgS* farg, uint64_t time, uint32_t proc,
      uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
      uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list );

int FiltTrc__Handle_SendMsg(
      FiltTrc__EventHandler_FirstArgS* farg, uint64_t time, uint32_t sender,
      uint32_t receiver, uint32_t communicator, uint32_t msgtype,
      uint32_t msglength, uint32_t scltoken, OTF_KeyValueList* list );

#endif // _VT_FILTER_TRC_HDLR_H_
