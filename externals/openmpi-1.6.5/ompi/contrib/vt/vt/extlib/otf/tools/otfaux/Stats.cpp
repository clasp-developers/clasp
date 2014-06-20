/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include "OTF_Platform.h"

#include "Stats.h"


#include <iostream>
#include <cassert>
using namespace std;


FunctionCall::FunctionCall( uint64_t _time, uint32_t _token ) {
  
    time = _time;
    token = _token;
}

FunctionCall::FunctionCall( const FunctionCall& fc ) {
  
    time = fc.time;
    token = fc.token;
}

FunctionCall::~FunctionCall() {

}

FunctionCall FunctionCall::operator=( const FunctionCall& fc ) {
  
    if( this == &fc )
        return *this;
  
    time = fc.time;
    token = fc.token;
    
    return *this;
    
}


BeginCollOperation::BeginCollOperation( uint32_t _col, uint32_t _type,
    uint64_t _invoc_sent, uint64_t _invoc_recv, uint64_t _bytesSent,
    uint64_t _bytesRecv ) {
  
    col = _col;
    type = _type;
    invoc_sent = _invoc_sent;
    invoc_recv = _invoc_recv;
    bytesSent = _bytesSent;
    bytesRecv = _bytesRecv;
}
    
BeginCollOperation::BeginCollOperation( const BeginCollOperation& cop ) {
  
    col = cop.col;
    type = cop.type;
    invoc_sent = cop.invoc_sent;
    invoc_recv = cop.invoc_recv;
    bytesSent = cop.bytesSent;
    bytesRecv = cop.bytesRecv;
}

BeginCollOperation::~BeginCollOperation() {
  
}

BeginCollOperation BeginCollOperation::operator=( const BeginCollOperation& cop ) {
  
    if( this == &cop )
        return *this;
  
    col = cop.col;
    type = cop.type;
    invoc_sent = cop.invoc_sent;
    invoc_recv = cop.invoc_recv;
    bytesSent = cop.bytesSent;
    bytesRecv = cop.bytesRecv;
    
    return *this;
    
}


/* *** ProcessStats *** ********************************* */


void ProcessStats::enterFunction( uint64_t time, uint32_t token ) {


	fstack.push_back( FunctionCall( time, token ) );
	
	FunctionStatistics& stat= fstatistics[ token ];
	stat.occurrences++;
}


void ProcessStats::leaveFunction( uint64_t time, uint32_t token ) {


	assert( ! fstack.empty() );

	const FunctionCall& call= fstack.back();

	/* if not special token 0 tokens must match */
	if ( ( 0 != token ) && ( call.token != token ) ) {

		cerr << "  leave at " << time << " with corrupt stack " << 
			call.token << " != " << token << endl;
	}

	/* update stack */
	fstack.pop_back();

	/* update statistics */
	FunctionStatistics& stat= fstatistics[ call.token ];

	stat.exclusiveTime += time - call.time;
	stat.inclusiveTime += time - call.time;

	/* subtract time from parents 'exclusiveTime' */
	if ( ! fstack.empty() ) {

		const FunctionCall& parent= fstack.back();

		FunctionStatistics& parentstat= fstatistics[ parent.token ];
		
		parentstat.exclusiveTime -= time - call.time;
	}
}

void ProcessStats::collOperation( uint32_t col,
                                  uint32_t type,
                                  uint32_t numSent,
                                  uint32_t numRecv,
                                  uint32_t bytesSent,
                                  uint32_t bytesRecv ) {

	CollOps.numSent[type] += numSent;
	CollOps.numRecv[type] += numRecv;
	CollOps.bytesSent[type] += bytesSent;
	CollOps.bytesRecv[type] += bytesRecv;
	CollOps.Type2Col[type] = col;
}

int ProcessStats::beginCollOperation( uint64_t matchingId,
        uint32_t col, uint32_t type,
        uint64_t invoc_sent, uint64_t invoc_recv,
        uint64_t bytesSent, uint64_t bytesRecv ) {
  
  
    std::map<uint64_t, BeginCollOperation>::iterator it; 

    it = beginCollOps.find( matchingId );

    if( it != beginCollOps.end() ) {
           
#       ifdef OTF_VERBOSE
            fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
                "MatchingId %llu has already been used. aborting\n",
                __FUNCTION__, __FILE__, __LINE__, (long long unsigned) matchingId );
#       endif

        return OTF_RETURN_ABORT;
      
    }
    
    /* insert the record into the list of begun collective operations */
    beginCollOps.insert( pair<uint64_t,BeginCollOperation>( matchingId,
        BeginCollOperation( col, type, invoc_sent, invoc_recv, bytesSent, bytesRecv ) ) );
      
      
    return OTF_RETURN_OK;
  
}

int ProcessStats::endCollOperation( uint32_t matchingId ) {
 
    std::map<uint64_t, BeginCollOperation>::iterator it; 

    it = beginCollOps.find( matchingId );

    if( it == beginCollOps.end() ) {
      
#       ifdef OTF_VERBOSE
            fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
                "MatchingId %llu was not found. aborting\n",
                __FUNCTION__, __FILE__, __LINE__, (long long unsigned) matchingId );
#       endif

        return OTF_RETURN_ABORT;
      
    }
    
    collOperation( it->second.col,
                   it->second.type,
                   it->second.invoc_sent,
                   it->second.invoc_recv,
                   it->second.bytesSent,
                   it->second.bytesRecv );
                   
    beginCollOps.erase( it );

    return OTF_RETURN_OK;
        
}

void ProcessStats::sendMessage( uint32_t msglength ) {
	
	sstatistics.bytes_sent+= msglength;
	sstatistics.number_sent++;
}


void ProcessStats::recvMessage( uint32_t msglength ) {

	sstatistics.bytes_recvd+= (uint64_t) msglength;
	sstatistics.number_recvd++;
}


int ProcessStats::openFile( uint32_t fileid ) {


	map<uint32_t,FileOperationStatistics>::iterator it;

	it= fostatistics.find( fileid );

	if( it != fostatistics.end() ) {

		FileOperationStatistics& fos= it->second;

		++fos.nopen;

	} else {
		fostatistics.insert( pair<uint32_t, FileOperationStatistics> ( fileid,
			FileOperationStatistics( 1, 0, 0, 0, 0, 0, 0 ) ) );
	}

	return OTF_RETURN_OK;
}


int ProcessStats::closeFile( uint32_t fileid ) {

    /* make the statistics */
    map<uint32_t,FileOperationStatistics>::iterator it;

    it= fostatistics.find( fileid );

    if( it != fostatistics.end() ) {

        FileOperationStatistics& fos= it->second;

        ++fos.nclose;

    } else {

        fostatistics.insert( pair<uint32_t, FileOperationStatistics> ( fileid,
            FileOperationStatistics( 0, 1, 0, 0, 0, 0, 0 ) ) );
    }

    return OTF_RETURN_OK;
}


int ProcessStats::writeFile( uint32_t fileid, uint64_t bytes ) {


	map<uint32_t,FileOperationStatistics>::iterator it;

	it= fostatistics.find( fileid );

	if( it != fostatistics.end() ) {

		FileOperationStatistics& fos= it->second;

		++fos.nwrite;
		fos.byteswrite+= bytes;

	} else {
	
		fostatistics.insert( pair<uint32_t, FileOperationStatistics> ( fileid,
			FileOperationStatistics( 0, 0, 0, 1, 0, 0, bytes ) ) );

	}
	
	return OTF_RETURN_OK;
}


int ProcessStats::readFile( uint32_t fileid, uint64_t bytes ) {


	map<uint32_t,FileOperationStatistics>::iterator it;

	it= fostatistics.find( fileid );

	if( it != fostatistics.end() ) {

		FileOperationStatistics& fos= it->second;

		++fos.nread;
		fos.bytesread+= bytes;

	} else {
	
		fostatistics.insert( pair<uint32_t, FileOperationStatistics> ( fileid,
			FileOperationStatistics( 0, 0, 1, 0, 0, bytes, 0 ) ) );

	}
	
	return OTF_RETURN_OK;
}


int ProcessStats::seekFile( uint32_t fileid, uint64_t bytes ) {
	map<uint32_t,FileOperationStatistics>::iterator it;

	it= fostatistics.find( fileid );

	if( it != fostatistics.end() ) {

		FileOperationStatistics& fos= it->second;

		++fos.nseek;

		return OTF_RETURN_OK;

	} else {
	
#		ifdef OTF_VERBOSE
			fprintf( stderr, "ERROR in function %s, file: %s, line: %i:\n "
				"Trying to seek in a not yet opened file. aborting\n",
				__FUNCTION__, __FILE__, __LINE__ );
#		endif

		return OTF_RETURN_ABORT;
	}
}

void ProcessStats::printStatistics( uint32_t processid, uint64_t time,
	map< uint32_t, uint32_t> *functiongroups,
	map< uint32_t, uint32_t> *filegroups ) const {

	
	map<uint32_t,FunctionStatistics> localStatistics;
	map<uint32_t,FunctionStatistics> groupStatistics;

	uint64_t lasttime= time;

	deque<FunctionCall>::const_reverse_iterator it= fstack.rbegin();
	deque<FunctionCall>::const_reverse_iterator itend= fstack.rend();
	for ( ; it != itend; ++it ) {

		FunctionStatistics& lstats= localStatistics[ it->token ];
		/* lstats.occurrences++; */
		lstats.exclusiveTime += lasttime - it->time;
		lstats.inclusiveTime += time - it->time;

		assert( lasttime >= it->time );
			
		lasttime= it->time;
	}


	/* actually write statistics */
	cerr << "  statistics of process " << processid << endl;

	map<uint32_t,FunctionStatistics>::const_iterator jt= fstatistics.begin();
	map<uint32_t,FunctionStatistics>::const_iterator jtend= fstatistics.end();

	for ( ; jt != jtend; ++jt ) {

		uint64_t oc= jt->second.occurrences;
		uint64_t ex= jt->second.exclusiveTime;
		uint64_t in= jt->second.inclusiveTime;

		map<uint32_t,FunctionStatistics>::const_iterator kt= localStatistics.find( jt->first );
		map<uint32_t,FunctionStatistics>::const_iterator ktend= localStatistics.end();

		if ( kt != ktend ) {
		
			oc += kt->second.occurrences;
			ex += kt->second.exclusiveTime;
			in += kt->second.inclusiveTime;
		}

		if ( NULL == functiongroups ) {
		
			cerr << "    func " << jt->first << ":    " << 
				oc << ",  " <<
				ex << ",  " <<
				in << ",  " << endl;
				
		} else {
					
			groupStatistics[(*functiongroups)[jt->first]].occurrences+= oc;
			groupStatistics[(*functiongroups)[jt->first]].exclusiveTime+= ex;
			groupStatistics[(*functiongroups)[jt->first]].inclusiveTime+= in;
		}
	}
	
	/* write functiongroup summary */
	if ( NULL != functiongroups ) {
	
		map<uint32_t,FunctionStatistics>::const_iterator kt= groupStatistics.begin();
		map<uint32_t,FunctionStatistics>::const_iterator ktend= groupStatistics.end();
		
		for( ; kt != ktend; kt++ ) {
		
			uint64_t oc= kt->second.occurrences;
			uint64_t ex= kt->second.exclusiveTime;
			uint64_t in= kt->second.inclusiveTime;
		
			cerr << "    funcgroup" << kt->first << ":    " << 
				oc << ",  " <<
				ex << ",  " <<
				in << ",  " << endl;
		}	
	}
	
	/* write the message summary if any message was sent */
	cerr << "  " << sstatistics.number_sent << " messages sent, " << sstatistics.number_recvd
		<< " messages received, " << sstatistics.bytes_sent <<
		" sent bytes, " << sstatistics.bytes_recvd << " bytes received" << endl;


	/* write file operation statistics */
	map<uint32_t,FileOperationStatistics>::const_iterator itfo;
	map<uint32_t,FileOperationStatistics>::const_iterator itendfo= fostatistics.end();

	if( NULL == filegroups ) {

		/* print out alle statistics */
		for( itfo= fostatistics.begin(); itfo != itendfo; ++itfo ) {

			const FileOperationStatistics& fost= itfo->second;

			cerr << "  file " << itfo->first << ": " << fost.nopen << " opened files, "
				<< fost.nclose << " closed files, " << fost.nread << " read events, "
				<< fost.nwrite << " write events, " << fost.nseek << " seek events, "
				<< fost.bytesread << " read bytes, " << fost.byteswrite << " written bytes "
				<< endl;
		}
		
	} else {

		map<uint32_t/*groupid*/,FileOperationStatistics> groupstats;
		map<uint32_t/*groupid*/,FileOperationStatistics>::iterator itgr;
		map<uint32_t/*groupid*/,FileOperationStatistics>::iterator itendgr;

		/* calculate group statistics */
		for( itfo= fostatistics.begin(); itfo != itendfo; ++itfo ) {

			itgr= groupstats.find( (*filegroups)[itfo->first] );

			if( itgr != groupstats.end() ) {

				FileOperationStatistics& fost= itgr->second;
				
				fost.nopen+= itfo->second.nopen;
				fost.nclose+= itfo->second.nclose;
				fost.nread+= itfo->second.nread;
				fost.nwrite+= itfo->second.nwrite;
				fost.nseek+= itfo->second.nseek;
				fost.bytesread+= itfo->second.bytesread;
				fost.byteswrite+= itfo->second.byteswrite;

			} else {

				groupstats.insert( pair<uint32_t,FileOperationStatistics>(
					(*filegroups)[itfo->first],
					FileOperationStatistics( itfo->second.nopen, itfo->second.nclose,
					itfo->second.nread, itfo->second.nwrite, itfo->second.nseek,
					itfo->second.bytesread, itfo->second.byteswrite ) ) );
				
			}
		}


		/* print out all group statistics */
		for( itgr= groupstats.begin(), itendgr= groupstats.end(); itgr != itendgr; ++itgr ) {

			const FileOperationStatistics& fost= itgr->second;

			cerr << "  filegroup " << itgr->first << ": " << fost.nopen << " opened files, "
				<< fost.nclose << " closed files, " << fost.nread << " read events, "
				<< fost.nwrite << " write events, " << fost.nseek << " seek events, "
				<< fost.bytesread << " read bytes, " << fost.byteswrite << " written bytes "
				<< endl;
		}
	}
}


void ProcessStats::writeStatistics( OTF_Writer* writer, uint64_t time,
		uint32_t processid, map< uint32_t,uint32_t> *functiongroups,
		map< uint32_t,uint32_t> *filegroups ) const {


	/* all past function calls are considered in 'fstatistics' already,
	now the time and occurrences of all active functions need to be added.
	one could modify & restore the 'fstatistics' but this seems unsafe. 
	furthermore, this is a 'const' method. therfore we use a temporary
	data structure even though it is kind of overkill :( */

	map<uint32_t,FunctionStatistics> localStatistics;
	map<uint32_t,FunctionStatistics> groupStatistics;

	uint64_t lasttime= time;


	/* cerr << "writeStatistics p" << processid << ", t" << time << endl; */


	deque<FunctionCall>::const_reverse_iterator it= fstack.rbegin();
	deque<FunctionCall>::const_reverse_iterator itend= fstack.rend();
	for ( ; it != itend; ++it ) {

		FunctionStatistics& lstats= localStatistics[ it->token ];
		/* lstats.occurrences++; */
		lstats.exclusiveTime += lasttime - it->time;
		lstats.inclusiveTime += time - it->time;

		assert( lasttime >= it->time );
			
		lasttime= it->time;
	}


	/* actually write statistics */

	map<uint32_t,FunctionStatistics>::const_iterator jt= fstatistics.begin();
	map<uint32_t,FunctionStatistics>::const_iterator jtend= fstatistics.end();

	for ( ; jt != jtend; ++jt ) {

		uint64_t oc= jt->second.occurrences;
		uint64_t ex= jt->second.exclusiveTime;
		uint64_t in= jt->second.inclusiveTime;

		map<uint32_t,FunctionStatistics>::const_iterator kt= localStatistics.find( jt->first );
		map<uint32_t,FunctionStatistics>::const_iterator ktend= localStatistics.end();

		if ( kt != ktend ) {
		
			oc += kt->second.occurrences;
			ex += kt->second.exclusiveTime;
			in += kt->second.inclusiveTime;
		}

		if ( NULL == functiongroups ) {
		
			OTF_Writer_writeFunctionSummary( writer, 
				time /* uint64_t time */, 
				jt->first /* uint32_t function */, 
				processid /* uint32_t process */, 
				oc /* uint64_t count */, 
				ex /* uint64_t excltime */, 
				in /* uint64_t incltime */ );
				
		} else {
					
			groupStatistics[(*functiongroups)[jt->first]].occurrences+= oc;
			groupStatistics[(*functiongroups)[jt->first]].exclusiveTime+= ex;
			groupStatistics[(*functiongroups)[jt->first]].inclusiveTime+= in;
		}
			
	}
	
	/* write functiongroup summary */
	if ( NULL != functiongroups ) {
	
		map<uint32_t,FunctionStatistics>::const_iterator kt= groupStatistics.begin();
		map<uint32_t,FunctionStatistics>::const_iterator ktend= groupStatistics.end();
		
		for( ; kt != ktend; kt++ ) {
		
			uint64_t oc= kt->second.occurrences;
			uint64_t ex= kt->second.exclusiveTime;
			uint64_t in= kt->second.inclusiveTime;
		
			OTF_Writer_writeFunctionGroupSummary( writer, 
				time /* uint64_t time */, 
				kt->first /* uint32_t functiongroup */, 
				processid /* uint32_t process */, 
				oc /* uint64_t count */, 
				ex /* uint64_t excltime */, 
				in /* uint64_t incltime */ );
		}	
	}
	
	/* write the message summary if any message was sent */

	if ( sstatistics.number_sent > 0 || sstatistics.number_recvd > 0) {
	
		OTF_Writer_writeMessageSummary( writer, time /* current time */,
			processid /* id of the process */, 0 /* peer */, 0 /* communicator */,
			0 /* message tag */, sstatistics.number_sent, sstatistics.number_recvd,
			sstatistics.bytes_sent, sstatistics.bytes_recvd );
	}

	/* write the collop summary */
	map<uint32_t,uint32_t>::iterator Iter;

	for(Iter=CollOps.Type2Col.begin(); Iter!=CollOps.Type2Col.end(); ++Iter) {
	     OTF_Writer_writeCollopSummary(writer,time,processid,0,Iter->second,CollOps.numSent[Iter->first],
                 CollOps.numRecv[Iter->first],CollOps.bytesSent[Iter->first],CollOps.bytesRecv[Iter->first]);
	}

	/* write file operation statistics */
	map<uint32_t,FileOperationStatistics>::const_iterator itfo;
	map<uint32_t,FileOperationStatistics>::const_iterator itendfo= fostatistics.end();

	if( NULL == filegroups ) {

		/* print out alle statistics */
		for( itfo= fostatistics.begin(); itfo != itendfo; ++itfo ) {

			const FileOperationStatistics& fost= itfo->second;

			OTF_Writer_writeFileOperationSummary( writer, time, itfo->first,
				processid, fost.nopen, fost.nclose, fost.nread, fost.nwrite,
				fost.nseek, fost.bytesread, fost.byteswrite );
		}
		
	} else {

		map<uint32_t/*groupid*/,FileOperationStatistics> groupstats;
		map<uint32_t/*groupid*/,FileOperationStatistics>::iterator itgr;
		map<uint32_t/*groupid*/,FileOperationStatistics>::iterator itendgr;

		/* calculate group statistics */
		for( itfo= fostatistics.begin(); itfo != itendfo; ++itfo ) {

			itgr= groupstats.find( (*filegroups)[itfo->first] );

			if( itgr != groupstats.end() ) {

				FileOperationStatistics& fost= itgr->second;
				
				fost.nopen+= itfo->second.nopen;
				fost.nclose+= itfo->second.nclose;
				fost.nread+= itfo->second.nread;
				fost.nwrite+= itfo->second.nwrite;
				fost.nseek+= itfo->second.nseek;
				fost.bytesread+= itfo->second.bytesread;
				fost.byteswrite+= itfo->second.byteswrite;

			} else {

				groupstats.insert( pair<uint32_t,FileOperationStatistics>(
					(*filegroups)[itfo->first],
					FileOperationStatistics( itfo->second.nopen, itfo->second.nclose,
					itfo->second.nread, itfo->second.nwrite, itfo->second.nseek,
					itfo->second.bytesread, itfo->second.byteswrite ) ) );
				
			}
		}


		/* print out all group statistics */
		for( itgr= groupstats.begin(), itendgr= groupstats.end(); itgr != itendgr; ++itgr ) {

			const FileOperationStatistics& fost= itgr->second;

			OTF_Writer_writeFileGroupOperationSummary( writer, time,
				itgr->first, processid, fost.nopen, fost.nclose, fost.nread,
				fost.nwrite, fost.nseek, fost.bytesread, fost.byteswrite );
		}
	}
}


void Stats::defProcess( uint32_t processid ) {


	/* explicit creation is not necessary. 
	it would furthermore disturbs selective creation of statistics
	
	processes[ processid ];
	*/
}


void Stats::defFunction( uint32_t function, uint32_t group ) {


	functiongroups[function]= group;
}


void Stats::defFile( uint32_t fileid, uint32_t group ) {


	filegroups[fileid]= group;
}

void Stats::defCollOp( uint32_t col, uint32_t type) {

	Col2Type[col] = type;
}

void Stats::enterFunction( uint64_t time, uint32_t processid, uint32_t token ) {


	/* cerr << "  " << hex << time << dec << " enter " << token << " on " << processid << endl; */
	
	processes[ processid ].enterFunction( time, token );

}


void Stats::leaveFunction( uint64_t time, uint32_t processid, uint32_t token ) {


	/* cerr << "  " << hex << time << dec << " leave " << token << " on " << processid << endl; */
	
	processes[ processid ].leaveFunction( time, token );

}


void Stats::sendMessage( uint32_t sender, uint32_t length ) {


	processes[ sender ].sendMessage( length );
}


void Stats::recvMessage( uint32_t receiver, uint32_t msglength ) {


	processes[ receiver ].recvMessage( msglength );
}


void Stats::collOperation( uint32_t proc,
                           uint32_t root,
                           uint32_t col,
                           uint32_t bytesSent,
                           uint32_t bytesRecv ) {

	uint32_t invoc_sent = 0;
  	uint32_t invoc_recv = 0;

	switch (Col2Type[col])
  	{
    	  case OTF_COLLECTIVE_TYPE_ALL2ONE:
	    if(proc == root) {
	      invoc_sent = 1;
	      invoc_recv = 1;
	    } else {
 	      invoc_sent = 1;
	    }
	    break;
          case OTF_COLLECTIVE_TYPE_ONE2ALL:
	    if(proc == root) {
	      invoc_sent = 1;
	      invoc_recv = 1;
	    } else {
 	      invoc_recv = 1;
	    }
            break;
          case OTF_COLLECTIVE_TYPE_ALL2ALL:
	    invoc_sent = 1;
	    invoc_recv = 1;
            break;
    	  case OTF_COLLECTIVE_TYPE_BARRIER:
	    invoc_sent = 1;
	    break;
 	}	
	
    processes[proc].collOperation( col,
                                   Col2Type[col],
                                   invoc_sent,
                                   invoc_recv,
                                   bytesSent,
                                   bytesRecv );
}


int Stats::fileOperation( uint32_t process, uint32_t fileid,
	uint32_t operation, uint64_t bytes ) {

	switch ( operation & OTF_FILEOP_BITS ) {

		case OTF_FILEOP_OPEN:
			return processes[ process ].openFile( fileid );
		case OTF_FILEOP_CLOSE:
			return processes[ process ].closeFile( fileid );
		case OTF_FILEOP_READ:
			return processes[ process ].readFile( fileid, bytes );
		case OTF_FILEOP_WRITE:
			return processes[ process ].writeFile( fileid, bytes );
		case OTF_FILEOP_SEEK:
			return processes[ process ].seekFile( fileid, bytes );
	}

	return OTF_RETURN_OK;
}

int Stats::beginCollOperation( uint32_t proc, uint32_t root,
    uint32_t col, uint64_t matchingId, uint64_t bytesSent, uint64_t bytesRecv ) {
  
  
    uint64_t invoc_sent = 0;
    uint64_t invoc_recv = 0;
    
    switch (Col2Type[col])
    {
        case OTF_COLLECTIVE_TYPE_ALL2ONE:
          
            if(proc == root) {
                invoc_sent = 1;
                invoc_recv = 1;
            } else {
                invoc_sent = 1;
            }
            break;
            
        case OTF_COLLECTIVE_TYPE_ONE2ALL:
          
            if(proc == root) {
                invoc_sent = 1;
                invoc_recv = 1;
            } else {
                invoc_recv = 1;
            }
            break;
            
        case OTF_COLLECTIVE_TYPE_ALL2ALL:
          
            invoc_sent = 1;
            invoc_recv = 1;
            break;
            
        case OTF_COLLECTIVE_TYPE_BARRIER:
          
            invoc_sent = 1;
            break;
    }
     
    return processes[proc].beginCollOperation( matchingId, col, Col2Type[col],
                                               invoc_sent, invoc_recv,
                                               bytesSent, bytesRecv );
  
}

int Stats::endCollOperation( uint32_t proc, uint64_t matchingId ) {
  
    
    return processes[proc].endCollOperation( matchingId );
   
}
 
void Stats::printStatistics( uint64_t time ) {


	map<uint32_t,ProcessStats>::const_iterator it= processes.begin();
	map<uint32_t,ProcessStats>::const_iterator itend= processes.end();
	
	for ( ; it != itend; ++it ) {

		it->second.printStatistics( it->first, time, false == usefunctiongroups ?
				NULL : &functiongroups, false == usefilegroups ? NULL : &filegroups );
	}
	
}


void Stats::writeStatistics( OTF_Writer* writer,
                             OTF_WStream* def_wstream,
                             uint64_t time ) {


	assert( NULL != writer );
	assert( NULL != def_wstream );

    /* write the aux sample point to the definitions */
    OTF_WStream_writeDefAuxSamplePoint( def_wstream,
                                        time,
                                        OTF_AUX_SAMPLE_POINT_SUMMARY,
                                        NULL );

	/* cout << " STATISTICS " << time << endl; */
	map<uint32_t,ProcessStats>::const_iterator it= processes.begin();
	map<uint32_t,ProcessStats>::const_iterator itend= processes.end();
	
	for ( ; it != itend; ++it ) {
	
		it->second.writeStatistics( writer, time,
			it->first /* processid */, false == usefunctiongroups ?
			NULL : &functiongroups, false == usefilegroups ? NULL : &filegroups );
	}
}
