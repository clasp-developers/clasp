/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifndef STATE_H
#define STATE_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include <vector>
#include <deque>
#include <map>
#include <list>

#include "OTF_inttypes.h"

#include "otf.h"


/* *** function stuff *** ********************************* */

struct FunctionCall {


public:

	uint64_t time;
	uint32_t token;


public:

	/** constructor */
	FunctionCall( uint64_t _time, uint32_t _token );

	/** copy constructor */
	FunctionCall( const FunctionCall& fc );
    
    /** destructor */
    ~FunctionCall();
    
    /** assignment operator */
    FunctionCall operator=( const FunctionCall& fc );
};


struct FunctionStatistics {


public:

	int64_t occurrences;
	int64_t exclusiveTime;
	int64_t inclusiveTime;


public:

	/** constructor */
	FunctionStatistics( int64_t o= 0, int64_t ex= 0, int64_t in= 0 ) : 
		occurrences( o ), exclusiveTime( ex ), inclusiveTime( in ) {};

	/** copy constructor */
	FunctionStatistics( const FunctionStatistics& other ) : 
		occurrences( other.occurrences ), exclusiveTime( other.exclusiveTime ), 
		inclusiveTime( other.inclusiveTime ) {};
};


struct SendStatistics {

public:
	
	uint64_t number_sent;
	uint64_t number_recvd;
	uint64_t bytes_sent;
	uint64_t bytes_recvd;
	
public:

	SendStatistics( uint64_t ns= 0, uint64_t nr= 0, uint64_t bs= 0, uint64_t br= 0 ) :
		number_sent( ns ), number_recvd( nr ), bytes_sent( bs ),
		bytes_recvd( br ) {};
	
	SendStatistics( const SendStatistics& other ) :
		number_sent( other.number_sent ), number_recvd( other.number_recvd ),
		bytes_sent( other.bytes_sent ), bytes_recvd( other.bytes_recvd ) {};
};


struct CollectiveOperations {

public:

	mutable std::map<uint32_t,uint64_t> numSent;
	mutable std::map<uint32_t,uint64_t> numRecv;
	mutable std::map<uint32_t,uint64_t> bytesSent;
	mutable std::map<uint32_t,uint64_t> bytesRecv;
	mutable std::map<uint32_t,uint32_t> Type2Col;
};


struct BeginCollOperation {

public:
  
    uint32_t col;
    uint32_t type;
    uint64_t invoc_sent;
    uint64_t invoc_recv;
    uint64_t bytesSent;
    uint64_t bytesRecv;
    
public:    
 
    /** constructor */
    BeginCollOperation(
        uint32_t _col, uint32_t _type, uint64_t _invoc_sent, uint64_t _invoc_recv,
        uint64_t _bytesSent, uint64_t _bytesRecv );
        
    /** copy constructor */
    BeginCollOperation( const BeginCollOperation& cop );
    
    /** destructor */
    ~BeginCollOperation();
    
    /** assignment operator */
    BeginCollOperation operator=( const BeginCollOperation& cop );
  
};

struct FileOperationStatistics {

public:

	uint64_t nopen;
	uint64_t nclose;
	uint64_t nread;
	uint64_t nwrite;
	uint64_t nseek;
	uint64_t bytesread;
	uint64_t byteswrite;

public:

	FileOperationStatistics( uint64_t no= 0, uint64_t nc= 0, uint64_t nr= 0,
		uint64_t nw= 0, uint64_t ns= 0, uint64_t br= 0, uint64_t bw= 0 ) :
		nopen( no ), nclose( nc ), nread( nr ), nwrite( nw ), nseek( ns ),
		bytesread( br ), byteswrite( bw ) {};

	FileOperationStatistics( const FileOperationStatistics& other ) :
		nopen( other.nopen ), nclose( other.nclose ), nread( other.nread ),
		nwrite( other.nwrite ), nseek( other.nseek ),
		bytesread( other.bytesread ), byteswrite( other.byteswrite ) {};
};


/* *** ProcessStats *** ********************************* */


/** class containing the state of a process trace at a given */
class ProcessStats {


public:

	/* function stack */
	std::deque<FunctionCall> fstack;
	
    /* map of begun collective operations */
    std::map<uint64_t/*matchingId*/, BeginCollOperation> beginCollOps;
    
	/* statistic per function since the beginning of the trace */
	std::map<uint32_t,FunctionStatistics> fstatistics;

	/* statistics for messages */
	SendStatistics sstatistics;

	/* statistics for collective operations */
	CollectiveOperations CollOps;

	/* statistic per file since the beginning of the trace */
	std::map<uint32_t,FileOperationStatistics> fostatistics;


public:

	/** constructor */
	ProcessStats() {};


	void enterFunction( uint64_t time, uint32_t token );

	void leaveFunction( uint64_t time, uint32_t token );

	void sendMessage( uint32_t msglength );

    void collOperation( uint32_t col,
                        uint32_t type,
                        uint32_t numSent,
                        uint32_t numRecv,
                        uint32_t bytesSent,
                        uint32_t bytesRecv );

	void recvMessage( uint32_t msglength );

	int openFile( uint32_t fileid );
	int closeFile( uint32_t fileid );
	int writeFile( uint32_t fileid, uint64_t bytes );
	int readFile( uint32_t fileid, uint64_t bytes );
	int seekFile( uint32_t fileid, uint64_t bytes );
    
    int beginCollOperation( uint64_t matchingId,
        uint32_t col, uint32_t type, uint64_t invoc_sent,
        uint64_t invoc_recv, uint64_t bytesSent, uint64_t bytesRecv );
        
    int endCollOperation( uint32_t matchingId );
    
	void printStatistics( uint32_t processid, uint64_t time,
		std::map< uint32_t, uint32_t> *functiongroups,
		std::map< uint32_t, uint32_t> *filegroups ) const;

	void writeStatistics( OTF_Writer* writer, uint64_t time,
		uint32_t processid,
		std::map< uint32_t, uint32_t> *functiongroups,
		std::map< uint32_t, uint32_t> *filegroups ) const;
};

/* *** Stats *** **************************************** */

/** state of a whole trace */
class Stats {


	std::map<uint32_t,ProcessStats> processes;

	/* maps the collective operation to its type */
	std::map<uint32_t,uint32_t> Col2Type;

	/* maps the function to its funtiongroupid */
	std::map< uint32_t, uint32_t> functiongroups;
	
	/* maps the files to its filegroupid */
	std::map< uint32_t, uint32_t> filegroups;

	bool usefunctiongroups;
	bool usefilegroups;

public:

	/** constructor */
	Stats( bool _usefunctiongroups= false, bool _usefilegroups= false ) :
		usefunctiongroups( _usefunctiongroups ), usefilegroups( _usefilegroups ) {};


	void defProcess( uint32_t processid );

	void defFunction( uint32_t function, uint32_t group );

	void defFile( uint32_t fileid, uint32_t group );

	void defCollOp( uint32_t col, uint32_t type );

	void enterFunction( uint64_t time, uint32_t processid, uint32_t token );

	void leaveFunction( uint64_t time, uint32_t processid, uint32_t token );
	
	void sendMessage( uint32_t sender, uint32_t length );

	void recvMessage( uint32_t receiver, uint32_t msglength );

    void collOperation( uint32_t proc,
                        uint32_t root,
                        uint32_t col,
                        uint32_t bytesSent,
                        uint32_t bytesRecv );

	int fileOperation( uint32_t process, uint32_t fileid,
		uint32_t operation, uint64_t bytes );
        
    int beginCollOperation( uint32_t proc, uint32_t root,
        uint32_t col, uint64_t matchingId, uint64_t bytesSent, uint64_t bytesRecv );
        
    int endCollOperation( uint32_t proc, uint64_t matchingId );
    
	void printStatistics( uint64_t time );
	void writeStatistics( OTF_Writer* writer, OTF_WStream* def_wstream, uint64_t time );

};

#endif /* STATE_H */

