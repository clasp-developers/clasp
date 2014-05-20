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

#ifndef _VT_FILTHANDLER_H_
#define _VT_FILTHANDLER_H_

#include <iostream>
#include <map>
#include <stack>
#include <string>
#include <vector>

#include "otf.h"

#include "vt_inttypes.h"

struct Process {
	std::stack<bool> fstack; /* function 2 limit - limit decreases for every function call in the eventloop */
	std::map<uint32_t,uint64_t> f2l; /* contains the functionstack of every process - written = true, ignored = false */
};
struct FiltHandlerArgument {

	FiltHandlerArgument() : wstream(0), mc(0) {}

	FiltHandlerArgument(const FiltHandlerArgument& fha);

	std::map<std::string, uint32_t> nm2tok; /* created during definition reading*/
	std::map<uint32_t, Process > t2p;
	OTF_WStream* wstream;
	OTF_MasterControl* mc;
};


int handleFiltDefinitionComment( void* userData, 
                                   uint32_t stream, 
                                   const char* comment );

int handleFiltDefTimerResolution( void* userData, 
                                    uint32_t stream,
                                    uint64_t ticksPerSecond );

int handleFiltDefProcess( void* userData, 
                            uint32_t stream,
                            uint32_t process, 
                            const char* name, 
                            uint32_t parent );

int handleFiltDefProcessGroup( void* userData, 
                                 uint32_t stream,
                                 uint32_t procGroup, 
                                 const char* name, 
                                 uint32_t numberOfProcs, 
                                 const uint32_t* procs );

int handleFiltDefFunction( void* userData, 
                             uint32_t stream,
                             uint32_t func, 
                             const char* name, 
                             uint32_t funcGroup, 
                             uint32_t source );

int handleFiltDefFunctionGroup( void* userData, 
                                  uint32_t stream,
                                  uint32_t funcGroup, 
                                  const char* name );

int handleFiltDefCollectiveOperation( void* userData, 
                                        uint32_t stream,
                                        uint32_t collOp,
                                        const char* name,
                                        uint32_t type );

int handleFiltDefCounter( void* userData, 
                            uint32_t stream,
                            uint32_t counter,
                            const char* name, 
                            uint32_t properties,
                            uint32_t counterGroup,
                            const char* unit );

int handleFiltDefCounterGroup( void* userData, 
                                 uint32_t stream,
                                 uint32_t counterGroup, 
                                 const char* name );

int handleFiltDefScl( void* userData, 
                        uint32_t stream,
                        uint32_t source,
                        uint32_t sourceFile, 
                        uint32_t line );

int handleFiltDefSclFile( void* userData, 
                            uint32_t stream,
                            uint32_t sourceFile,
                            const char* name );

int handleFiltDefCreator( void* userData, 
                            uint32_t stream,
                            const char* creator );

int handleFiltDefVersion( void* userData,
                            uint32_t stream,
                            uint8_t major,
                            uint8_t minor,
                            uint8_t sub,
                            const char* string );

int handleFiltDefFile( void* userData,
                         uint32_t stream,
                         uint32_t token,
                         const char *name,
                         uint32_t group );

int handleFiltDefFileGroup( void* userData,
                              uint32_t stream,
                              uint32_t token,
                              const char *name );

int handleFiltEnter( void* userData, 
                       uint64_t time, 
                       uint32_t function, 
                       uint32_t process, 
                       uint32_t source );

int handleFiltLeave( void* userData, 
                       uint64_t time, 
                       uint32_t function, 
                       uint32_t process, 
                       uint32_t source );

int handleFiltSendMsg( void* userData, 
                         uint64_t time, 
                         uint32_t sender, 
                         uint32_t receiver, 
                         uint32_t group, 
                         uint32_t type, 
                         uint32_t length, 
                         uint32_t source );

int handleFiltRecvMsg( void* userData, 
                         uint64_t time, 
                         uint32_t recvProc, 
                         uint32_t sendProc, 
                         uint32_t group, 
                         uint32_t type, 
                         uint32_t length, 
                         uint32_t source );

int handleFiltCounter( void* userData, 
                         uint64_t time, 
                         uint32_t process, 
                         uint32_t counter, 
                         uint64_t value );

int handleFiltCollectiveOperation( void* userData, 
                                     uint64_t time, 
                                     uint32_t process, 
                                     uint32_t collective, 
                                     uint32_t procGroup, 
                                     uint32_t rootProc, 
                                     uint32_t sent, 
                                     uint32_t received, 
                                     uint64_t duration, 
                                     uint32_t source );

int handleFiltEventComment( void* userData, 
                              uint64_t time, 
                              uint32_t process, 
                              const char* comment );

int handleFiltBeginProcess( void* userData, 
                              uint64_t time, 
                              uint32_t process );

int handleFiltEndProcess( void* userData, 
                            uint64_t time, 
                            uint32_t process );

int handleFiltFileOperation( void* userData,
                               uint64_t time,
                               uint32_t fileid,
                               uint32_t process,
                               uint64_t handleid,
                               uint32_t operation,
                               uint64_t bytes,
                               uint64_t duration,
                               uint32_t source );

int handleFiltSnapshotComment( void* userData, 
                                     uint64_t time, 
                                     uint32_t process, 
                                     const char* comment );

int handleFiltEnterSnapshot( void *userData, 
                           uint64_t time, 
                           uint64_t originaltime, 
                           uint32_t function, 
                           uint32_t process, 
                           uint32_t source );

int handleFiltSendSnapshot( void *userData,
                           uint64_t time,
                           uint64_t originaltime,
                           uint32_t sender,
                           uint32_t receiver,
                           uint32_t procGroup,
                           uint32_t tag,
                           uint32_t length,
                           uint32_t source );

int handleFiltOpenFileSnapshot( void* userData,
                                  uint64_t time,
                                  uint64_t originaltime,
                                  uint32_t fileid,
                                  uint32_t process,
                                  uint64_t handleid,
                                  uint32_t source );

int handleFiltSummaryComment( void * userData, 
                                uint64_t time, 
                                uint32_t process, 
                                const char* comment );

int handleFiltFunctionSummary( void* userData, 
                                 uint64_t time, 
                                 uint32_t function, 
                                 uint32_t process, 
                                 uint64_t invocations, 
                                 uint64_t exclTime, 
                                 uint64_t inclTime );

int handleFiltFunctionGroupSummary( void* userData, 
                                      uint64_t time,  
                                      uint32_t funcGroup,  
                                      uint32_t process,  
                                      uint64_t invocations,  
                                      uint64_t exclTime,  
                                      uint64_t inclTime );

int handleFiltMessageSummary( void* userData, 
                                uint64_t time, 
                                uint32_t process, 
                                uint32_t peer,
                                uint32_t comm,
                                uint32_t type,  
                                uint64_t sentNumber, 
                                uint64_t receivedNumber, 
                                uint64_t sentBytes, 
                                uint64_t receivedBytes );

int handleFiltFileOperationSummary( void* userData,
                                      uint64_t time,
                                      uint32_t fileid,
                                      uint32_t process,
                                      uint64_t nopen,
                                      uint64_t nclose,
                                      uint64_t nread,
                                      uint64_t nwrite,
                                      uint64_t nseek,
                                      uint64_t bytesread,
                                      uint64_t byteswrite );

int handleFiltFileGroupOperationSummary( void* userData,
                                           uint64_t time,
                                           uint32_t groupid,
                                           uint32_t process,
                                           uint64_t nopen,
                                           uint64_t nclose,
                                           uint64_t nread,
                                           uint64_t nwrite,
                                           uint64_t nseek,
                                           uint64_t bytesread,
                                           uint64_t byteswrite );


#endif /* _VT_FILTHANDLER_H_ */

