/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifndef CONTROL_H
#define CONTROL_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include <set>

#include "OTF_inttypes.h"
#include "otf.h"
#include "otfaux.h"

#include "Stats.h"


struct Control {


	OTFAUX_State *aux_state;
	OTF_KeyValueList *kvlist;
	Stats *stats;

	bool collectRecvsOnly;
	bool doSnapshots;
	bool inlineSnapshots;
	bool msgMatching;
	bool copyEvents;

	/* time stamps where to generate a snapshots */
	std::set<uint64_t> timestamps;

	uint64_t nextTime;

	OTF_Writer* writer;
	OTF_WStream* def_wstream;

	uint64_t minTime;
	uint64_t maxTime;
	bool haveTimeRange;
	std::vector<uint32_t> all_processes;
	
	bool verbose;
	
	int usefunctiongroups;

	uint32_t recvTimeToken;
	uint32_t recvLengthToken;
	uint32_t recvSclToken;
	uint32_t maxKeyToken;

	Control( OTF_Writer* writer= NULL, OTF_WStream* def_wstream= NULL,
		bool _verbose= false,
		bool _usefunctiongroups= false, bool _usefilegroups= false,
		bool _doSnapshots= true, bool _doThumbnail= true,
		bool _doStatistics= true, bool _inlineSnapshots= false,
		bool _msgMatching= false, bool _copyEvents= false );
	~Control();

	/** add time stamp where to generate a snapshot */
	void addTime( uint64_t time );
	
	uint64_t getLastTime();

	bool checkTime( uint64_t time );

	static void releaseEventData( void* userData,
	                              void* eventData );

	static int writeEnterSnapshot( void*    userData,
	                               uint64_t snapshotTime,
	                               uint64_t eventTime,
	                               uint64_t processId,
	                               uint32_t function,
	                               uint32_t scl,
	                               void*    eventData );

	static int writeSendSnapshot( void*    userData,
	                              uint64_t snapshotTime,
	                              uint64_t eventTime,
	                              uint64_t senderProcessId,
	                              uint64_t receiverProcessId,
	                              uint32_t comm,
	                              uint32_t tag,
	                              uint32_t length,
	                              uint32_t scl,
	                              uint64_t recvTime,
	                              uint32_t recvLength,
	                              uint32_t recvScl,
	                              void*    eventData );

	static int writeOpenFileSnapshot( void*    userData,
	                                  uint64_t snapshotTime,
	                                  uint64_t eventTime,
	                                  uint64_t processId,
	                                  uint32_t fileId,
	                                  uint64_t handleId,
	                                  uint32_t scl,
	                                  void*    eventData );

	static int writeBeginCollopSnapshot( void*    userData,
	                                     uint64_t snapshotTime,
	                                     uint64_t eventTime,
	                                     uint64_t processId,
	                                     uint32_t collOp,
	                                     uint64_t matchingId,
	                                     uint32_t comm,
	                                     uint32_t root,
	                                     uint64_t sent,
	                                     uint64_t received,
	                                     uint32_t scl,
	                                     void*    eventData );

	static int writeBeginFileOpSnapshot( void*    userData,
	                                     uint64_t snapshotTime,
	                                     uint64_t eventTime,
	                                     uint64_t processId,
	                                     uint64_t matchingId,
	                                     uint32_t scl,
	                                     void*    eventData );

	static int writeCollopCountSnapshot( void*    userData,
	                                     uint64_t snapshotTime,
	                                     uint64_t processId,
	                                     uint32_t comm,
	                                     uint64_t count );

	static int writeCounterSnapshot( void* userData,
	                                 uint64_t snapshotTime,
	                                 uint64_t eventTime,
	                                 uint64_t processId,
	                                 uint32_t counter,
	                                 uint64_t value,
	                                 void*  eventData );
};


#endif /* CONTROL_H */

