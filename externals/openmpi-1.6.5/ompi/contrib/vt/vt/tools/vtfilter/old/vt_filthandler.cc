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

#include "vt_filthandler.h"

using namespace std;

//#define FF cerr << omp_get_thread_num() << " " << __func__ << endl;
#define FF

/* MJ: Workaround for Pathscale problem (Open MPI Ticket: #1318)
   It seems that the Pathscale compiler doesn't allow the implementation
   of the FiltHandlerArgument's copy-constructor in the header file
   'vt_filthandler.h'. It looks very stange for me. :-( */
FiltHandlerArgument::FiltHandlerArgument(const FiltHandlerArgument& fha)
{
	nm2tok = fha.nm2tok;
	t2p = fha.t2p;
	wstream = fha.wstream;
	mc = fha.mc;
}

int handleFiltDefinitionComment( void* userData,
                                   uint32_t stream, 
                                   const char* comment ) { FF

	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeDefinitionComment(fha->wstream, comment) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltDefTimerResolution( void* userData, 
                                    uint32_t stream,
                                    uint64_t ticksPerSecond ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeDefTimerResolution(fha->wstream,ticksPerSecond) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltDefProcess( void* userData, 
                            uint32_t stream,
                            uint32_t process, 
                            const char* name, 
                            uint32_t parent ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	fha->t2p.insert(pair<uint32_t, Process >(process, Process()) );

	return OTF_WStream_writeDefProcess(fha->wstream,process,name,parent) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltDefProcessGroup( void* userData, 
                                 uint32_t stream,
                                 uint32_t procGroup, 
                                 const char* name, 
                                 uint32_t numberOfProcs, 
                                 const uint32_t* procs ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeDefProcessGroup(fha->wstream,procGroup,name,
		numberOfProcs,procs) == 0 ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltDefFunction( void* userData, 
                             uint32_t stream,
                             uint32_t func, 
                             const char* name, 
                             uint32_t funcGroup, 
                             uint32_t source ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	if( fha->nm2tok.find(name) == fha->nm2tok.end() )
		fha->nm2tok.insert(pair<string,uint32_t>(name,func));

	return OTF_WStream_writeDefFunction(fha->wstream,func,name,funcGroup,source) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltDefFunctionGroup( void* userData, 
                                  uint32_t stream,
                                  uint32_t funcGroup, 
                                  const char* name ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeDefFunctionGroup(fha->wstream,funcGroup,name) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltDefCollectiveOperation( void* userData, 
                                        uint32_t stream,
                                        uint32_t collOp,
                                        const char* name,
                                        uint32_t type ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeDefCollectiveOperation(fha->wstream,collOp,name,type) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltDefCounter( void* userData, 
                            uint32_t stream,
                            uint32_t counter,
                            const char* name, 
                            uint32_t properties,
                            uint32_t counterGroup,
                            const char* unit ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeDefCounter(fha->wstream,counter,name,properties,
		counterGroup,unit) == 0 ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltDefCounterGroup( void* userData, 
                                 uint32_t stream,
                                 uint32_t counterGroup, 
                                 const char* name ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeDefCounterGroup(fha->wstream,counterGroup,name) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltDefScl( void* userData, 
                        uint32_t stream,
                        uint32_t source,
                        uint32_t sourceFile, 
                        uint32_t line ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeDefScl(fha->wstream,source,sourceFile,line) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltDefSclFile( void* userData, 
                            uint32_t stream,
                            uint32_t sourceFile,
                            const char* name ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeDefSclFile(fha->wstream,sourceFile,name) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltDefCreator( void* userData, 
                            uint32_t stream,
                            const char* creator ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeDefCreator(fha->wstream,creator) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltDefFile( void* userData,
                         uint32_t stream,
                         uint32_t token,
                         const char *name,
                         uint32_t group ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeDefFile(fha->wstream,token,name,group) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltDefFileGroup( void* userData,
                              uint32_t stream,
                              uint32_t token,
                              const char *name ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeDefFileGroup(fha->wstream,token,name) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltEnter( void* userData, 
                       uint64_t time, 
                       uint32_t function, 
                       uint32_t process, 
                       uint32_t source ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	Process& proc = fha->t2p.find( process )->second;

	map<uint32_t,uint64_t>::iterator it = proc.f2l.find(function);
	if( it != proc.f2l.end() ) {
		if( it->second > 0 ) {
			--it->second;
		} else {
			proc.fstack.push(false);
			return OTF_RETURN_OK;
		}
	}

	proc.fstack.push(true);

	/*cerr << omp_get_thread_num() << " process " << process << " stream " << OTF_MasterControl_mapReverse(
		fha->mc,process) << " " << endl;*/

	return OTF_WStream_writeEnter(fha->wstream,time,function,process,source) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltLeave( void* userData, 
                       uint64_t time, 
                       uint32_t function, 
                       uint32_t process, 
                       uint32_t source ) { FF

	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	Process& proc = fha->t2p.find( process )->second;
	
	if( proc.fstack.top() == false ) {
		proc.fstack.pop();
		return OTF_RETURN_OK;
	}

	proc.fstack.pop();

	return OTF_WStream_writeLeave(fha->wstream,time,function,process,source) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltSendMsg( void* userData, 
                         uint64_t time, 
                         uint32_t sender, 
                         uint32_t receiver, 
                         uint32_t group, 
                         uint32_t type, 
                         uint32_t length, 
                         uint32_t source ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeSendMsg(fha->wstream,time,sender,receiver,group,type,length,
		source) == 0 ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltRecvMsg( void* userData, 
                         uint64_t time, 
                         uint32_t recvProc, 
                         uint32_t sendProc, 
                         uint32_t group, 
                         uint32_t type, 
                         uint32_t length, 
                         uint32_t source ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeRecvMsg(fha->wstream,time,recvProc,sendProc,group,type,
		length,source) == 0 ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltCounter( void* userData, 
                         uint64_t time, 
                         uint32_t process, 
                         uint32_t counter, 
                         uint64_t value ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	Process& proc = fha->t2p.find( process )->second;
	
	if( proc.fstack.size() > 0 && proc.fstack.top() == false )
			return OTF_RETURN_OK;

	return OTF_WStream_writeCounter(fha->wstream,time,process,counter,value) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltCollectiveOperation( void* userData, 
                                     uint64_t time, 
                                     uint32_t process, 
                                     uint32_t collective, 
                                     uint32_t procGroup, 
                                     uint32_t rootProc, 
                                     uint32_t sent, 
                                     uint32_t received, 
                                     uint64_t duration, 
                                     uint32_t source ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeCollectiveOperation(fha->wstream,time,process,collective,
		procGroup,rootProc,sent,received,duration,source) == 0 ? OTF_RETURN_ABORT :
		OTF_RETURN_OK;
}

int handleFiltEventComment( void* userData, 
                              uint64_t time, 
                              uint32_t process, 
                              const char* comment ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeEventComment(fha->wstream,time,process,comment) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltBeginProcess( void* userData, 
                              uint64_t time, 
                              uint32_t process ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeBeginProcess(fha->wstream,time,process) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltEndProcess( void* userData, 
                            uint64_t time, 
                            uint32_t process ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeEndProcess(fha->wstream,time,process) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltFileOperation( void* userData,
                               uint64_t time,
                               uint32_t fileid,
                               uint32_t process,
                               uint64_t handleid,
                               uint32_t operation,
                               uint64_t bytes,
                               uint64_t duration,
                               uint32_t source ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeFileOperation(fha->wstream,time,fileid,process,handleid,
		operation,bytes,duration,source) == 0 ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltSnapshotComment( void* userData, 
                                     uint64_t time, 
                                     uint32_t process, 
                                     const char* comment ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeSnapshotComment(fha->wstream,time,process,comment) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltEnterSnapshot( void *userData, 
                           uint64_t time, 
                           uint64_t originaltime, 
                           uint32_t function, 
                           uint32_t process, 
                           uint32_t source ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeEnterSnapshot(fha->wstream,time,originaltime,function,process,
		source) == 0 ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltSendSnapshot( void *userData,
                           uint64_t time,
                           uint64_t originaltime,
                           uint32_t sender,
                           uint32_t receiver,
                           uint32_t procGroup,
                           uint32_t tag,
                           uint32_t length,
                           uint32_t source ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeSendSnapshot(fha->wstream,time,originaltime,sender,receiver,
		procGroup,tag,length,source) == 0 ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltOpenFileSnapshot( void* userData,
                                  uint64_t time,
                                  uint64_t originaltime,
                                  uint32_t fileid,
                                  uint32_t process,
                                  uint64_t handleid,
                                  uint32_t source ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeOpenFileSnapshot(fha->wstream,time,originaltime,fileid,process,
		handleid,source) == 0 ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltSummaryComment( void * userData, 
                                uint64_t time, 
                                uint32_t process, 
                                const char* comment ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeSummaryComment(fha->wstream,time,process,comment) == 0 ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltFunctionSummary( void* userData, 
                                 uint64_t time, 
                                 uint32_t function, 
                                 uint32_t process, 
                                 uint64_t invocations, 
                                 uint64_t exclTime, 
                                 uint64_t inclTime ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeFunctionSummary(fha->wstream,time,function,process,
		invocations,exclTime,inclTime) == 0 ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltFunctionGroupSummary( void* userData, 
                                      uint64_t time,  
                                      uint32_t funcGroup,  
                                      uint32_t process,  
                                      uint64_t invocations,  
                                      uint64_t exclTime,  
                                      uint64_t inclTime ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeFunctionGroupSummary(fha->wstream,time,funcGroup,process,
		invocations,exclTime,inclTime) == 0 ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int handleFiltMessageSummary( void* userData, 
                                uint64_t time, 
                                uint32_t process, 
                                uint32_t peer,
                                uint32_t comm,
                                uint32_t type,  
                                uint64_t sentNumber, 
                                uint64_t receivedNumber, 
                                uint64_t sentBytes, 
                                uint64_t receivedBytes ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeMessageSummary(fha->wstream,time,process,peer,comm,type,
		sentNumber,receivedNumber,sentBytes,receivedBytes) == 0 ? OTF_RETURN_ABORT
		: OTF_RETURN_OK;
}

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
                                      uint64_t byteswrite ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeFileOperationSummary(fha->wstream,time,fileid,process,nopen,
		nclose,nread,nwrite,nseek,bytesread,byteswrite) == 0 ? OTF_RETURN_ABORT
		: OTF_RETURN_OK;
}

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
                                           uint64_t byteswrite ) { FF
	
	FiltHandlerArgument* fha = static_cast<FiltHandlerArgument*>(userData);

	return OTF_WStream_writeFileGroupOperationSummary(fha->wstream,time,groupid,process,
		nopen,nclose,nread,nwrite,nseek,bytesread,byteswrite) == 0 ? OTF_RETURN_ABORT
		: OTF_RETURN_OK;
}

