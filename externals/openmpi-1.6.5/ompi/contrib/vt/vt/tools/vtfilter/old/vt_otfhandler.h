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

#ifndef _VT_OTFHANDLER_H_
#define _VT_OTFHANDLER_H_

#include "otf.h"

#include "vt_filterc.h"

struct HandlerArgument {

	/* we use one filter per process -> enables parallelisation */
	std::map<uint32_t /*process*/, Filter> p2f;
};


int handleDefTimerResolution( void* userData,
	uint32_t streamid, uint64_t tickspersecond );

int handleDefFunction( void* userData, uint32_t streamid,
	uint32_t deftoken, const char* name, uint32_t group, uint32_t scltoken );

int handleEnter( void* userData, uint64_t time, uint32_t function,
	uint32_t process, uint32_t source );

int handleLeave( void* userData, uint64_t time, uint32_t function, 
	uint32_t process, uint32_t source );

int handleCollectiveOperation( void* firsthandlerarg, uint64_t time,
    uint32_t process, uint32_t functionToken, uint32_t communicator, 
    uint32_t rootprocess, uint32_t sent, uint32_t received, 
    uint64_t duration, uint32_t scltoken );

int handleRecvMsg( void* firsthandlerarg, uint64_t time,
	uint32_t receiver, uint32_t sender, uint32_t communicator, 
	uint32_t msgtype, uint32_t msglength,
	uint32_t scltoken );

int handleSendMsg( void* firsthandlerarg, uint64_t time,
	uint32_t sender, uint32_t receiver, uint32_t communicator, 
	uint32_t msgtype, uint32_t msglength, uint32_t scltoken );

#endif /* _VT_OTFHANDLER_H_ */
