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

#include "vt_otfhandler.h"

using namespace std;


int handleDefTimerResolution( void* ud,
	uint32_t streamid, uint64_t tickspersecond ) {


	map<uint32_t, Filter>::iterator it = ((HandlerArgument*)ud)->p2f.find( 0 );

	it->second.setTimerResolution( tickspersecond );

	
	return OTF_RETURN_OK;
}


int handleDefFunction( void* ud, uint32_t streamid,
	uint32_t func, const char* name, uint32_t /*group*/, uint32_t /*scltoken*/ ) {


	map<uint32_t, Filter>::iterator it = ((HandlerArgument*)ud)->p2f.find( 0 );

	string escaped;

	for( uint32_t i= 0; i < strlen(name); ++i ) {
	
		if( '*' == name[i] ) {
			escaped.append( 1, '\\' );
		}
		
		escaped.append( 1, name[i] );
	}


	it->second.addFunction( func, escaped );
	

	return OTF_RETURN_OK;
}


int handleEnter( void* ud, uint64_t time, uint32_t function,
	uint32_t process, uint32_t /*source*/ ) {

	map<uint32_t, Filter>::iterator it = ((HandlerArgument*)ud)->p2f.find( process );
	
	it->second.addEnter( function, process, time );


	return OTF_RETURN_OK;
}


int handleLeave( void* ud, uint64_t time, uint32_t function,
	uint32_t process, uint32_t /*source*/ ) {
	
	
	map<uint32_t, Filter>::iterator it = ((HandlerArgument*)ud)->p2f.find( process );
	

	it->second.addLeave( process, time );

	
	return OTF_RETURN_OK;
}


int handleCollectiveOperation( void* ud, uint64_t /*time*/,
    uint32_t process, uint32_t /*functionToken*/, uint32_t /*communicator*/,
    uint32_t /*rootprocess*/, uint32_t /*sent*/, uint32_t /*received*/, 
    uint64_t /*duration*/, uint32_t /*scltoken*/ ) {


	map<uint32_t, Filter>::iterator it = ((HandlerArgument*)ud)->p2f.find( process );
	

	it->second.incrCollectiveCount();


	return OTF_RETURN_OK;
}


int handleRecvMsg( void* ud, uint64_t /*time*/,
	uint32_t receiver, uint32_t /*sender*/, uint32_t /*communicator*/,
	uint32_t /*msgtype*/, uint32_t /*msglength*/,
	uint32_t /*scltoken*/ ) {


	map<uint32_t, Filter>::iterator it = ((HandlerArgument*)ud)->p2f.find( receiver );
	

	it->second.incrMessageCount();


	return OTF_RETURN_OK;
}


int handleSendMsg( void* ud, uint64_t /*time*/,
	uint32_t sender, uint32_t /*receiver*/, uint32_t /*communicator*/,
	uint32_t /*msgtype*/, uint32_t /*msglength*/, uint32_t /*scltoken*/ ) {


	map<uint32_t, Filter>::iterator it = ((HandlerArgument*)ud)->p2f.find( sender );


	it->second.incrMessageCount();


	return OTF_RETURN_OK;
}
