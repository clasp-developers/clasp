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

#ifndef _VT_UNIFY_HOOKS_MSGMATCH_SNAPS_H_
#define _VT_UNIFY_HOOKS_MSGMATCH_SNAPS_H_

#include "vt_unify.h"
#include "vt_unify_hooks_base.h"
#include "vt_unify_lvector.hh"

#include "otf.h"
#include "otfaux.h"

#include <sstream>

// define to produce inline snapshots
#define VT_UNIFY_HOOKS_MSGMATCH_SNAPS__INLINE_SNAPSHOTS

//
// HooksMsgMatchAndSnapsC class
//
class HooksMsgMatchAndSnapsC : public HooksBaseC
{
public:

   // constructor
   HooksMsgMatchAndSnapsC();

   // destructor
   ~HooksMsgMatchAndSnapsC();

   // is this hook enabled?
   static bool isEnabled()
   {
      return ( Params.domsgmatch || Params.createsnaps );
   }

private:

   // maximum thumbnail width and height
   //
   static const uint32_t MAX_THUMBNAIL_WIDTH = 4096;
   static const uint32_t MAX_THUMBNAIL_HEIGHT = 512;

   // thumbnail file suffix
   // TODO: has to be provided by the OTFAUX library
   static const char* THUMBNAIL_FILE_SUFFIX() { return ".thumb"; }

   //
   // message matching bumps structure
   // (holds statistics about irregularities encountered during
   //  massage matching)
   //
   struct MsgMatchBumpsS
   {
      // warning messages about message matching irregularities to be added
      // to the definition comments and printed to stdout
      //
      static const char* UNMATCHED_WARNING_FMT()
      {
         return "Warning: This trace contains %llu (%s) message send "
                "events which could not be matched.\n";
      }
      static const char* REVERSED_WARNING_FMT()
      {
         return "Warning: This trace contains %llu (%s) message events "
                "which are in wrong order (i.e. receive before send event).\n";
      }

      // constructor
      MsgMatchBumpsS()
         : num_unmatched( 0 ), num_reversed( 0 ), num_messages( 0 ),
           wstream( 0 ), def_comment_idx( 0 ) {}

      // operator to add other message matching bumps statistics
      MsgMatchBumpsS & operator+=( const MsgMatchBumpsS & a )
      {
         num_unmatched += a.num_unmatched;
         num_reversed += a.num_reversed;
         num_messages += a.num_messages;

         return *this;
      }

      // test whether message matching bumps statistics are empty
      bool empty() const { return ( !num_unmatched && !num_reversed ); }

      // number of unmatched messages
      // (i.e. #send msg. which do not have a matching receive message)
      uint64_t num_unmatched;

      // number of reversed "backward running" messages
      // (i.e. receive before send event)
      uint64_t num_reversed;

      // total number of messages for calculating percentage of
      // unmatched/reversed messages
      uint64_t num_messages;

      // the following variables are significant only for rewriting the global
      // definitions to insert warning comments about message matching
      // irregularities
      //

      // OTF writer stream
      OTF_WStream * wstream;

      // index of definition comments where the warning comments
      // will be inserted (usually, index before the first user comment)
      uint32_t def_comment_idx;

   };

   //
   // receive message structure
   // (holds receive message information of a single receive message event
   //  for message matching)
   //
   struct RecvMsgS
   {
      // constructors
      //
      RecvMsgS()
         : time( 0 ), sender( 0 ), receiver( 0 ), comm( 0 ), tag( 0 )
           /*, length( 0 ), scl( 0 )*/ {}
      RecvMsgS( const uint64_t & _time, const uint32_t & _sender,
                const uint32_t & _receiver, const uint32_t & _comm,
                const uint32_t & _tag
                /*, const uint32_t & _length, const uint32_t & _scl*/ )
         : time( _time ), sender( _sender ), receiver( _receiver ),
           comm( _comm ), tag( _tag ) /*, length( _length ), scl( _scl )*/ {}

      uint64_t time;     // timestamp on which the receive message
                         // event occurred
      uint32_t sender;   // sender process id
      uint32_t receiver; // receiver process id
      uint32_t comm;     // comm. process group token
      uint32_t tag;      // message tag
      /*uint32_t length;*/   // received bytes
      /*uint32_t scl;*/      // source code location of recv. message event

   };

   //
   // stream context structure
   // (holds intermediate per-stream data during reading events)
   //
   struct StreamContextS
   {
      // constructor
      StreamContextS( const uint32_t _streamid )
         : streamid( _streamid ), snapshot_cnt( 0 ), last_snapshot_time( 0 )
      {
         // create OTFAUX state context
         //
         auxstate = OTFAUX_State_create();
         vt_assert( auxstate );
      }

      // destructor
      ~StreamContextS()
      {
         // destroy OTFAUX state context, if necessary
         //
         if( auxstate )
         {
            OTFAUX_State_destroy( auxstate );
            auxstate = 0;
         }
      }

      OTFAUX_State * auxstate;           // OTFAUX state context
      uint32_t       streamid;           // associated stream id
      uint32_t       snapshot_cnt;       // number of snapshots written
      uint64_t       last_snapshot_time; // last timestamp when a snapshot
                                         // was written
      MsgMatchBumpsS msgmatch_bumps;     // message matching bumps statistics

   };

   // handler for rewriting definition comment records to insert warning
   // comments about message matching irregularities
   static int HandleDefComment( MsgMatchBumpsS * msgMatchBumps,
                 uint32_t stream, const char * comment );

   // event record handlers for reading receive messages for message matching
   //

   static int HandleEventComment( void * userData,
                 uint64_t time, uint32_t proc, const char * comment );

   static int HandleRecvMsg( LargeVectorC<RecvMsgS*> * recvMsgs,
                 uint64_t time, uint32_t receiver, uint32_t sender,
                 uint32_t comm, uint32_t tag, uint32_t length, uint32_t scl );

   // callback function for releasing snapshot event data (i.e. key-values)
   static int ReleaseEventDataCB( void * userData,
                 OTF_KeyValueList * kvs );

   // callback functions called when writing a snapshot
   //

   static int WriteEnterSnapshotCB( OTF_WStream * wstream,
                 uint64_t snapshotTime, uint64_t eventTime, uint64_t proc,
                 uint32_t func, uint32_t scl, OTF_KeyValueList * kvs );

   static int WriteSendSnapshotCB( OTF_WStream * wstream, uint64_t snapshotTime,
                 uint64_t eventTime, uint64_t sender, uint64_t receiver,
                 uint32_t comm, uint32_t tag, uint32_t length, uint32_t scl,
                 uint64_t recvTime, uint32_t recvLength, uint32_t recvScl,
                 OTF_KeyValueList * kvs );

   static int WriteOpenFileSnapshotCB( OTF_WStream * wstream,
                 uint64_t snapshotTime, uint64_t eventTime, uint64_t proc,
                 uint32_t file, uint64_t handleid, uint32_t scl,
                 OTF_KeyValueList * kvs );

   static int WriteBeginFileOpSnapshotCB( OTF_WStream * wstream,
                 uint64_t snapshotTime, uint64_t eventTime, uint64_t proc,
                 uint64_t matchid, uint32_t scl, OTF_KeyValueList * kvs );

   static int WriteBeginCollopSnapshotCB( OTF_WStream * wstream,
                 uint64_t snapshotTime, uint64_t eventTime, uint64_t proc,
                 uint32_t op, uint64_t matchid, uint32_t comm, uint32_t root,
                 uint64_t sent, uint64_t recvd, uint32_t scl,
                 OTF_KeyValueList * kvs );

   static int WriteCollopCountSnapshotCB( OTF_WStream * wstream,
                 uint64_t snapshotTime, uint64_t proc, uint32_t comm,
                 uint64_t count );

   static int WriteCounterSnapshotCB( OTF_WStream * wstream,
                 uint64_t snapshotTime, uint64_t eventTime, uint64_t proc,
                 uint32_t counter, uint64_t value,
                 OTF_KeyValueList * kvs );

#ifdef VT_MPI

   // user-defined reduction operation to aggregate per-rank message matching
   // bumps statistics
   static void MsgMatchBumpsReduceOp( uint64_t * invec, uint64_t * inoutvec,
                  VT_MPI_INT * len, MPI_Datatype * datatype );

#endif // VT_MPI

   // vvvvvvvvvvvvvvvvvvvv HOOK METHODS vvvvvvvvvvvvvvvvvvvv

   // initialization/finalization hooks
   //

   void initHook();
   void finalizeHook( const bool & error );

   // phase hooks
   //

   void phaseHook_UnifyDefinitions_pre();
   void phaseHook_UnifyEvents_pre();
   void phaseHook_UnifyEvents_post();
   void phaseHook_CleanUp_post();

   // record hooks
   //

   // definition records

   void writeRecHook_DefComment( HooksC::VaArgsT & args );
   void writeRecHook_DefTimeRange( HooksC::VaArgsT & args );

   // event records

   void writeRecHook_Enter( HooksC::VaArgsT & args );
   void writeRecHook_Leave( HooksC::VaArgsT & args );
   void writeRecHook_BeginFileOp( HooksC::VaArgsT & args );
   void writeRecHook_EndFileOp( HooksC::VaArgsT & args );
   void writeRecHook_SendMsg( HooksC::VaArgsT & args );
   void writeRecHook_RecvMsg( HooksC::VaArgsT & args );
   void writeRecHook_BeginCollOp( HooksC::VaArgsT & args );
   void writeRecHook_EndCollOp( HooksC::VaArgsT & args );
   void writeRecHook_RMAPut( HooksC::VaArgsT & args );
   void writeRecHook_RMAPutRemoteEnd( HooksC::VaArgsT & args );
   void writeRecHook_RMAGet( HooksC::VaArgsT & args );
   void writeRecHook_RMAEnd( HooksC::VaArgsT & args );
   void writeRecHook_Counter( HooksC::VaArgsT & args );
   void writeRecHook_EventComment( HooksC::VaArgsT & args );

   // generic hook
   void genericHook( const uint32_t & id, HooksC::VaArgsT & args );

   // ^^^^^^^^^^^^^^^^^^^^ HOOK METHODS ^^^^^^^^^^^^^^^^^^^^

   // read receive messages
   bool getRecvMsgs( LargeVectorC<RecvMsgS*> & recvMsgs );

#ifdef VT_MPI

   // distribute receive messages to ranks which handle
   // the corresponding sender streams
   bool distRecvMsgs( LargeVectorC<RecvMsgS*> & recvMsgs );

#endif // VT_MPI

   // enqueue receive messages to sender stream contexts
   bool enqueueRecvMsgs( LargeVectorC<RecvMsgS*> & recvMsgs );

   // process message matching bumps statistics
   // (i.e. aggregate them from all streams and add warning comments
   //  to definitions)
   bool processMsgMatchBumps();

   // write the thumbnail file
   bool writeThumbnail();

   // write outstanding snapshot(s) until given timestamp
   inline bool writeSnapshots( StreamContextS * streamContext,
                  const uint64_t time, OTF_WStream * wstream ) const;

   // get stream context by id
   inline StreamContextS * getStreamContext( const uint32_t streamid ) const;

   // maximum number of threads to use for unifying events
   int m_maxThreads;

   // maximum trace timestamp for calculating snapshot interval time
   // and thumbnail width
   uint64_t m_maxTime;

   // global key tokens for matching receive timestamp, length, and
   // source code location
   //
   enum { KEY_TIME /*, KEY_LENGTH, KEY_SCL*/, KEY_NUM };
   uint32_t m_msgMatchKeyTokens[KEY_NUM];

   // message matching bumps statistics over all streams
   MsgMatchBumpsS m_msgMatchBumps;

   // number of snapshots to generate
   uint32_t m_numSnapshots;

   // interval between snapshot timestamps
   uint64_t m_snapshotInterval;

   // thumbnail width in pixels
   uint32_t m_thumbnailWidth;

   // thumbnail height in pixels
   uint32_t m_thumbnailHeight;

   // map stream id <-> stream context
   std::map<uint32_t, StreamContextS*> m_streamId2StreamContext;

};

#endif // _VT_UNIFY_HOOKS_MSGMATCH_SNAPS_H_
