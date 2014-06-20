/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "OTF_Definitions.h"
#include "OTF_Platform.h"
#include "OTF_Parse.h"
#include "OTF_Keywords.h"
#include "OTF_Errno.h"


/* *** local headers *** **************************************************** */

/* *** Definition records *** ************************************* */


/**	This function reads a COMMENT record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefinitionComment( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers, uint32_t streamid );

/**	This function reads a DEFTIMERRESOLUTION record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefTimerResolution( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers, uint32_t streamid );

/**	This function reads a DEFPROCESS record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefProcess( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers, uint32_t streamid );

/**	This function reads a DEFPROCESSGROUP record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefProcessGroup( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers, uint32_t streamid );

/**	This function reads a DEFATTRLIST record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefAttributeList( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers, uint32_t streamid );

/**	This function reads a DEFPROCESSORGROUPATTR record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefProcessOrGroupAttributes( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers, uint32_t streamid );

/**	This function reads a DEFFUNCTION record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefFunction( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers, uint32_t streamid );

/**	This function reads a DEFFUNCTIONGROUP record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefFunctionGroup( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers, uint32_t streamid );

/**	This function reads a DEFCOLLOP record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefCollectiveOperation( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid );

/**	This function reads a DEFCOUNTER record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefCounter( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers, uint32_t streamid );

/**	This function reads a DEFCOUNTERGROUP record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefCounterGroup( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers, uint32_t streamid );

/**	This function reads a DEFSCL record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefScl( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers, uint32_t streamid );

/**	This function reads a DEFSCLFILE record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefSclFile( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers, uint32_t streamid );

/**	This function reads a DEFUNIQUEID record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefUniqueId( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers, uint32_t streamid );

/**	This function reads a DEFVERSION record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefVersion( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers, uint32_t streamid );

/**	This function reads a DEFCREATOR record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefCreator( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers, uint32_t streamid );

/**	This function reads a DEFFILE record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefFile( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers, uint32_t streamid );

/**	This function reads a DEFFILEGROUP record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefFileGroup( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers, uint32_t streamid );

int OTF_Reader_readDefKeyValue( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers, uint32_t streamid );

int OTF_Reader_readDefTimeRange( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers, uint32_t streamid );

int OTF_Reader_readDefCounterAssignments( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers, uint32_t streamid );

int OTF_Reader_readDefProcessSubstitutes( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers, uint32_t streamid );

int OTF_Reader_readDefAuxSamplePoint( OTF_RBuffer*      buffer,
                                      OTF_HandlerArray* handlers,
                                      uint32_t          streamid );

/* *** Event records *** ****************************************** */


/**	This function reads a NOOP record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readNoOp( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );

/**	This function reads a COMMENT record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readEventComment( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );

/**	This function reads a COUNTER record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readCounter( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );

/**	This function reads an ENTER record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readEnter( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );

/**	This function reads a LEAVE record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readLeave( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );

/**	This function reads a COLLECTIVEOPERATION record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readCollectiveOperation( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );

/**	This function reads a BEGINCOLLECTIVEOPERATION record from buffer,
	parses the parameter of the record and calls the
	appropriate handler.*/
int OTF_Reader_readBeginCollectiveOperation( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );

/**	This function reads an ENDCOLLECTIVEOPERATION record from buffer,
	parses the parameter of the record and calls the
	appropriate handler.*/
int OTF_Reader_readEndCollectiveOperation( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );

/**	This function reads a RECVMSG record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readRecvMsg( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );

/**	This function reads a SENDMSG record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readSendMsg( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );

/** This function reads a PROCESSBEGIN record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
	
int OTF_Reader_readBeginProcess( OTF_RBuffer* buffer,
		OTF_HandlerArray* handlers );

/** This function reads a PROCESSEND record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readEndProcess( OTF_RBuffer* buffer,
		OTF_HandlerArray* handlers );

/** This function reads a FILEOPERATION record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readFileOperation( OTF_RBuffer* buffer,
		OTF_HandlerArray* handlers );

/** This function reads a BEGINFILEOPERATION record from buffer,
	parses the parameter of the record and calls the
	appropriate handler.*/
int OTF_Reader_readBeginFileOperation( OTF_RBuffer* buffer,
		OTF_HandlerArray* handlers );

/** This function reads an ENDFILEOPERATION record from buffer,
	parses the parameter of the record and calls the
	appropriate handler.*/
int OTF_Reader_readEndFileOperation( OTF_RBuffer* buffer,
		OTF_HandlerArray* handlers );

/** This function reads an ENDFILEOPERATION record from buffer,
	parses the parameter of the record and calls the
	appropriate handler. THIS is for an older version but needs 
    to stay as a bug fix. */
int OTF_Reader_readEndFileOperation_old( OTF_RBuffer* buffer,
		OTF_HandlerArray* handlers );

/** This function reads a RMAPUT record from buffer,
        parse the parameter of the record and call the
        appropriate handler.*/
int OTF_Reader_readRMAPut( OTF_RBuffer* buffer,
                OTF_HandlerArray* handlers );

/** This function reads a RMAPUTRE record from buffer,
        parse the parameter of the record and call the
        appropriate handler.*/
int OTF_Reader_readRMAPutRemoteEnd( OTF_RBuffer* buffer,
                OTF_HandlerArray* handlers );

/** This function reads a RMAGET record from buffer,
        parse the parameter of the record and call the
        appropriate handler.*/
int OTF_Reader_readRMAGet( OTF_RBuffer* buffer,
                OTF_HandlerArray* handlers );

/** This function reads a RMAEND record from buffer,
        parse the parameter of the record and call the
        appropriate handler.*/
int OTF_Reader_readRMAEnd( OTF_RBuffer* buffer,
                OTF_HandlerArray* handlers );


/* *** Snapshot records *** ****************************************** */


/**	This function reads a COMMENT record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readSnapshotComment( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );

/** This function reads a TENTER record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readEnterSnapshot( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );

/** This function reads a TSEND record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readSendSnapshot( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );

/** This function reads a TOPENFILE record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readOpenFileSnapshot( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers );

/** This function reads a TBEGINCOLLOP record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readBeginCollopSnapshot( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers );


/** This function reads a TBEGINFILEOP record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readBeginFileOpSnapshot( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers );

/** This function reads a TCOLLOPCOUNT record from buffer,
    parse the parameter of the record and call the
    appropriate handler.*/
int OTF_Reader_readCollopCountSnapshot( OTF_RBuffer* buffer,
    OTF_HandlerArray* handlers );

/** This function reads a TCOUNTER record from buffer,
    parse the parameter of the record and call the
    appropriate handler.*/
int OTF_Reader_readCounterSnapshot( OTF_RBuffer* buffer,
    OTF_HandlerArray* handlers );

/* *** Summary records *** ****************************************** */


/**	This function reads a COMMENT record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readSummaryComment( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );

/** This function reads a SUMFUNCTION record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readFunctionSummary( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );

/** This function reads a SUMFUNCTIONGROUP record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readFunctionGroupSummary( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );

/** This function reads a SUMMESSAGE record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readMessageSummary( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );

/** This function reads a COLLOPMESSAGE record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readCollopSummary( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers );
	
/** This function reads a SUMFILEOPERATION record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readFileOperationSummary( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers );

/** This function reads a SUMFILEGROUPOPERATION record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readFileGroupOperationSummary( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers );


/* *** Marker records *** ***************************************** */


/**	This function reads a DefMarker record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readDefMarker( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t stream );

/**	This function reads a Marker record from buffer,
	parse the parameter of the record and call the
	appropriate handler.*/
int OTF_Reader_readMarkerSpot( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers );


/* ************************************************************************* */



/* *** public function bodies *** ********************************* */


int OTF_Reader_parseEventRecord( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers ) {

	/* 
	look at this code very carfully until you understand it!!!

	the order of cases depends on the first characters of the 
	'OTF_KEYWORD_' macros - so changes there requires changes here!

	see comments in 'OTF_Keywords.h' also!
	*/

	/* accept empty lines */
	OTF_RBuffer_skipSpaces( buffer );

	switch( buffer->buffer[buffer->pos] ) {


	case OTF_KEYWORD_F_EVENTCOMMENT /*'#'*/ :

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_EVENTCOMMENT ) || 
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_EVENTCOMMENT ) ) {

			OTF_RBuffer_skipKeyword( buffer );
			return OTF_Reader_readEventComment( buffer, handlers );
		}

		break;

	case OTF_KEYWORD_F_KEYVALUE_PREFIX /* 'K' */ :

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_KEYVALUE_PREFIX ) || 
                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_KEYVALUE_PREFIX ) ) {

			OTF_RBuffer_skipKeyword( buffer );

			return OTF_RBuffer_readKeyValueList( buffer );
		}

		break;

	case OTF_KEYWORD_F_NOOP /* 'N' */ :

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_NOOP ) || 
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_NOOP ) ) {

			OTF_RBuffer_skipKeyword( buffer );
			return OTF_Reader_readNoOp( buffer, handlers );
		}

		break;

	case OTF_KEYWORD_F_ENTER /* 'E' */ :

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_ENTER ) || 
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_ENTER ) ) {

			OTF_RBuffer_skipKeyword( buffer );
			return OTF_Reader_readEnter( buffer, handlers );
		}

		break;

	case OTF_KEYWORD_F_LEAVE /* 'L' */ :

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LEAVE ) || 
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LEAVE ) ) {

			OTF_RBuffer_skipKeyword( buffer );
			return OTF_Reader_readLeave( buffer, handlers );
		}

		break;

	case OTF_KEYWORD_F_SEND /* 'S' */ :

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_SEND ) || 
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_SEND ) ) {

			OTF_RBuffer_skipKeyword( buffer );
			return OTF_Reader_readSendMsg( buffer, handlers );
		}

		break;

	case OTF_KEYWORD_F_RECEIVE /* 'R' */ :

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_RECEIVE ) || 
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_RECEIVE ) ) {

			return OTF_Reader_readRecvMsg( buffer, handlers );
		}

		break;

	case OTF_KEYWORD_F_COUNTER /* 'C' */ :
    	/* case OTF_KEYWORD_F_COLLECTIVEOPERATION / * C * / : */
		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_COUNTER ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_COUNTER ) ) {

			return OTF_Reader_readCounter( buffer, handlers );
		}

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_COLLECTIVEOPERATION ) || 
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_COLLECTIVEOPERATION ) ) {

			OTF_RBuffer_skipKeyword( buffer );
			return OTF_Reader_readCollectiveOperation( buffer, handlers );
		}

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_BEGINCOLLECTIVEOPERATION ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_BEGINCOLLECTIVEOPERATION ) ) {

			OTF_RBuffer_skipKeyword( buffer );
			return OTF_Reader_readBeginCollectiveOperation( buffer, handlers );
		}

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_ENDCOLLECTIVEOPERATION ) || 
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_ENDCOLLECTIVEOPERATION ) ) {

			OTF_RBuffer_skipKeyword( buffer );
			return OTF_Reader_readEndCollectiveOperation( buffer, handlers );
		}

		break;

	case OTF_KEYWORD_F_FILEOPERATION /* F */ :

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_FILEOPERATION ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_FILEOPERATION ) ) {

			return OTF_Reader_readFileOperation( buffer, handlers );
		}

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_BEGINFILEOPERATION_NEW ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_BEGINFILEOPERATION_NEW ) ) {

			return OTF_Reader_readBeginFileOperation( buffer, handlers );
		}

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_ENDFILEOPERATION_NEW ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_ENDFILEOPERATION_NEW ) ) {

			return OTF_Reader_readEndFileOperation( buffer, handlers );
		}

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_BEGINFILEOPERATION_OLD ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_BEGINFILEOPERATION_OLD ) ) {

			return OTF_Reader_readBeginFileOperation( buffer, handlers );
		}

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_ENDFILEOPERATION_OLD ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_ENDFILEOPERATION_OLD ) ) {

			return OTF_Reader_readEndFileOperation_old( buffer, handlers );
		}

		break;

	case OTF_KEYWORD_F_BEGINPROCESS /* 'P' */ :
		/* case OTF_KEYWORD_F_ENDPROCESS / * 'P' * / : */
		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_BEGINPROCESS ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_BEGINPROCESS ) ) {

			return OTF_Reader_readBeginProcess( buffer, handlers );
		}

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_ENDPROCESS ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_ENDPROCESS ) ) {

			return OTF_Reader_readEndProcess( buffer, handlers );
		}

		break;

        case OTF_KEYWORD_F_RMAPUT /* T */ :

                if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_RMAPUT ) ||
                                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_RMAPUT ) ) {

                        return OTF_Reader_readRMAPut( buffer, handlers );
                }

                /* try inline snapshots */
                return OTF_Reader_parseSnapshotsRecord( buffer, handlers );

        case OTF_KEYWORD_F_RMAPUTRE /* U */ :

                if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_RMAPUTRE ) ||
                                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_RMAPUTRE ) ) {

                        return OTF_Reader_readRMAPutRemoteEnd( buffer, handlers );
                }

                break;

        case OTF_KEYWORD_F_RMAGET /* G */ :

                if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_RMAGET ) ||
                                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_RMAGET ) ) {

                        return OTF_Reader_readRMAGet( buffer, handlers );
                }

                break;

        case OTF_KEYWORD_F_RMAEND /* M */ :

                if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_RMAEND ) ||
                                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_RMAEND ) ) {

                        return OTF_Reader_readRMAEnd( buffer, handlers );
		}

		break;

	case '\n':

		/* accept empty lines */
		return OTF_RBuffer_readNewline( buffer );
	}
	

	return OTF_Reader_readUnknownRecord( buffer, handlers );
}


int OTF_Reader_parseDefRecord( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid ) {

	/* check prefix */
	if ( OTF_RBuffer_testPrefix( buffer, OTF_KEYWORD_L_DEF_PREFIX ) ||
			OTF_RBuffer_testPrefix( buffer, OTF_KEYWORD_S_DEF_PREFIX ) ) {

		/* fine */

	} else if ( OTF_RBuffer_testPrefix( buffer, OTF_KEYWORD_L_KEYVALUE_PREFIX ) ||
             OTF_RBuffer_testPrefix( buffer, OTF_KEYWORD_S_KEYVALUE_PREFIX ) ) {

		return OTF_RBuffer_readKeyValueList( buffer );

	} else if ( buffer->buffer[buffer->pos] == '\n' ) {

		return OTF_RBuffer_readNewline( buffer );

	} else {

		return OTF_Reader_readUnknownDefRecord( buffer, handlers, streamid );
	}

	switch( buffer->buffer[buffer->pos] ) {

	/* case OTF_KEYWORD_F_DEFAUXSAMPLEPOINT 'A' : */
	case OTF_KEYWORD_F_DEFATTRLIST /* 'A' */ :

		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_DEFATTRLIST ) ||
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_DEFATTRLIST ) ) {

			return OTF_Reader_readDefAttributeList( buffer, handlers, streamid );
		}

        if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_DEFAUXSAMPLEPOINT ) ||
                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_DEFAUXSAMPLEPOINT ) ) {

            return OTF_Reader_readDefAuxSamplePoint( buffer, handlers, streamid );
        }

		break;

	/* case OTF_KEYWORD_F_DEFINITIONCOMMENT 'C' :*/
	/* case OTF_KEYWORD_F_DEFCOUNTER 'C' : */
	/* case OTF_KEYWORD_F_DEFCOUNTERGROUP 'C' : */
	/* case OTF_KEYWORD_F_DEFCOLLOP 'C' : */
    /* case OTF_KEYWORD_F_DEFCOUNTERASSIGNMENTS / * C * / : */
	case OTF_KEYWORD_F_DEFCREATOR /* 'C' */ :

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_DEFINITIONCOMMENT ) || 
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_DEFINITIONCOMMENT ) ) {

			OTF_RBuffer_skipKeyword( buffer );
			return OTF_Reader_readDefinitionComment( buffer, handlers, streamid );
		}
		
		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_DEFCOUNTERGROUP ) ||
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_DEFCOUNTERGROUP ) ) {

			return OTF_Reader_readDefCounterGroup( buffer, handlers, streamid );

		}
		
		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_DEFCREATOR ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_DEFCREATOR ) ) {

			return OTF_Reader_readDefCreator( buffer, handlers, streamid );
		}

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_DEFCOLLOP ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_DEFCOLLOP ) ) {

			return OTF_Reader_readDefCollectiveOperation( buffer, handlers, streamid );
		}

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_DEFCOUNTER ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_DEFCOUNTER ) ) {

			return OTF_Reader_readDefCounter( buffer, handlers, streamid );
		}

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_DEFCOUNTERASSIGNMENTS ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_DEFCOUNTERASSIGNMENTS ) ) {

			return OTF_Reader_readDefCounterAssignments( buffer, handlers, streamid );
		}

		break;

	/* case OTF_KEYWORD_F_DEFFUNCTION 'F' : */
	/* case OTF_KEYWORD_F_DEFFFILE 'F' : */
	/* case OTF_KEYWORD_F_DEFFFILEGROUP 'F' : */
	case OTF_KEYWORD_F_DEFFUNCTIONGROUP /* 'F' */ :

		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_DEFFUNCTIONGROUP ) ||
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_DEFFUNCTIONGROUP ) ) {

			return OTF_Reader_readDefFunctionGroup( buffer, handlers, streamid );
		}

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_DEFFUNCTION ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_DEFFUNCTION ) ) {

			return OTF_Reader_readDefFunction( buffer, handlers, streamid );
		}


		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_DEFFILEGROUP ) ||
				OTF_RBuffer_testKeyword( buffer,
					OTF_KEYWORD_L_DEFFILEGROUP ) ) {

			return OTF_Reader_readDefFileGroup( buffer, handlers, streamid );
		}

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_DEFFILE ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_DEFFILE ) ) {

			return OTF_Reader_readDefFile( buffer, handlers, streamid );
		}


		
		break;


	case OTF_KEYWORD_F_DEFKEYVALUE /* 'K' */ :

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_DEFKEYVALUE ) ||
		     OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_DEFKEYVALUE ) ) {

			return OTF_Reader_readDefKeyValue( buffer, handlers, streamid );
		}

		break;


	/* case OTF_KEYWORD_F_DEFPROCESSORGROUPATTR 'P' : */
	/* case OTF_KEYWORD_F_DEFPROCESS 'P' : */
	case OTF_KEYWORD_F_DEFPROCESSGROUP /* 'P' */ :

		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_DEFPROCESSORGROUPATTR ) ||
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_DEFPROCESSORGROUPATTR ) ) {

			return OTF_Reader_readDefProcessOrGroupAttributes( buffer, handlers, streamid );
		}

		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_DEFPROCESSGROUP ) ||
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_DEFPROCESSGROUP ) ) {

			return OTF_Reader_readDefProcessGroup( buffer, handlers, streamid );
		}

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_DEFPROCESS ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_DEFPROCESS ) ) {

			return OTF_Reader_readDefProcess( buffer, handlers, streamid );
		}

		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_DEFPROCESSSUBSTITUTES ) ||
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_DEFPROCESSSUBSTITUTES ) ) {

			return OTF_Reader_readDefProcessSubstitutes( buffer, handlers, streamid );
		}

		break;

	/* case OTF_KEYWORD_F_DEFSCL 'S' : */
	case OTF_KEYWORD_F_DEFSCLFILE /* 'S' */ :

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_DEFSCLFILE ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_DEFSCLFILE ) ) {

			return OTF_Reader_readDefSclFile( buffer, handlers, streamid );
		}

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_DEFSCL ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_DEFSCL ) ) {

			return OTF_Reader_readDefScl( buffer, handlers, streamid );
		}

		break;

    /* case OTF_KEYWORD_F_DEFTIMERANGE / * T * / : */
	case OTF_KEYWORD_F_DEFTIMERRESOLUTION /* 'T' */ :

		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_DEFTIMERRESOLUTION ) ||
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_DEFTIMERRESOLUTION ) ) {

			return OTF_Reader_readDefTimerResolution( buffer, handlers, streamid );
		}

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_DEFTIMERANGE ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_DEFTIMERANGE ) ) {

			return OTF_Reader_readDefTimeRange( buffer, handlers, streamid );
		}

		break;

	case OTF_KEYWORD_F_DEFVERSION /* 'V' */ :

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_DEFVERSION ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_DEFVERSION ) ) {

			return OTF_Reader_readDefVersion( buffer, handlers, streamid );
		}

		break;

	case OTF_KEYWORD_F_DEFUNIQUEID /* 'U' */ :

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_DEFUNIQUEID ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_DEFUNIQUEID ) ) {

			return OTF_Reader_readDefUniqueId( buffer, handlers, streamid );
		}

		break;
	}


	return OTF_Reader_readUnknownDefRecord( buffer, handlers, streamid );
}


int OTF_Reader_parseStatisticsRecord( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers ) {

	/* check prefix */
	if ( OTF_RBuffer_testPrefix( buffer, OTF_KEYWORD_L_SUM_PREFIX ) ||
			OTF_RBuffer_testPrefix( buffer, OTF_KEYWORD_S_SUM_PREFIX ) ) {

		/* fine */

	} else if ( OTF_RBuffer_testPrefix( buffer, OTF_KEYWORD_L_KEYVALUE_PREFIX ) ||
            OTF_RBuffer_testPrefix( buffer, OTF_KEYWORD_S_KEYVALUE_PREFIX ) ) {

		return OTF_RBuffer_readKeyValueList( buffer );

	} else if ( buffer->buffer[buffer->pos] == '\n' ) {

		return OTF_RBuffer_readNewline( buffer );

	} else {

		return OTF_Reader_readUnknownRecord( buffer, handlers );
	}


	switch( buffer->buffer[buffer->pos] ) {

	/* case OTF_KEYWORD_F_SUMFILEOPERATION 'F' : */
	/* case OTF_KEYWORD_F_SUMFILEGROUPOPERATION 'F' : */
	case OTF_KEYWORD_F_SUMFUNCTION /* 'F' */ :

		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_SUMFUNCTION ) || 
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_SUMFUNCTION ) ) {

			return OTF_Reader_readFunctionSummary( buffer, handlers );
		}

		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_SUMFILEGROUPOPERATION ) ||
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_SUMFILEGROUPOPERATION ) ) {

			return OTF_Reader_readFileGroupOperationSummary( buffer, handlers );
		}
		
		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_SUMFILEOPERATION ) ||
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_SUMFILEOPERATION ) ) {

			return OTF_Reader_readFileOperationSummary( buffer, handlers );
		}

		/** This is necessary to read a FunctionGroupSummary record in long format, because 
		    the first character of the long and short keyword differs. */
		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_SUMFUNCTIONGROUP ) || 
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_SUMFUNCTIONGROUP ) ) {

			return OTF_Reader_readFunctionGroupSummary( buffer, handlers );
		}

		break;

	case OTF_KEYWORD_F_SUMFUNCTIONGROUP /* 'G' */ :

		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_SUMFUNCTIONGROUP ) || 
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_SUMFUNCTIONGROUP ) ) {

			return OTF_Reader_readFunctionGroupSummary( buffer, handlers );
		}

		break;

	case OTF_KEYWORD_F_SUMMESSAGE /* 'M' */ :

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_SUMMESSAGE ) || 
				 OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_SUMMESSAGE ) ) {

			return OTF_Reader_readMessageSummary( buffer, handlers );
		}

		break;

	case OTF_KEYWORD_F_SUMCOMMENT /* 'C' */ :
	/*case OTF_KEYWORD_F_COLLOPMESSAGE*/ /* 'C' */

		if ( OTF_RBuffer_testKeyword( buffer,
				OTF_KEYWORD_S_SUMCOMMENT ) || 
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_SUMCOMMENT ) ) {

			return OTF_Reader_readSummaryComment( buffer, handlers );
		}

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_COLLOPMESSAGE ) || 
				 OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_COLLOPMESSAGE ) ) {

			return OTF_Reader_readCollopSummary( buffer, handlers );
		}

		break;
	}


	return OTF_Reader_readUnknownRecord( buffer, handlers );
}


int OTF_Reader_parseSnapshotsRecord( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers ) {

	
	/* check prefix */
	if ( OTF_RBuffer_testPrefix( buffer, OTF_KEYWORD_L_SNAPSHOT_PREFIX ) ||
			OTF_RBuffer_testPrefix( buffer, OTF_KEYWORD_S_SNAPSHOT_PREFIX ) ) {

		/* fine */

	} else if ( OTF_RBuffer_testPrefix( buffer, OTF_KEYWORD_L_KEYVALUE_PREFIX ) || 
            OTF_RBuffer_testPrefix( buffer, OTF_KEYWORD_S_KEYVALUE_PREFIX ) ) {

		return OTF_RBuffer_readKeyValueList( buffer );

	} else if ( buffer->buffer[buffer->pos] == '\n' ) {

		return OTF_RBuffer_readNewline( buffer );

	} else {

		return OTF_Reader_readUnknownRecord( buffer, handlers );
	}


	switch( buffer->buffer[buffer->pos] ) {

	case OTF_KEYWORD_F_SNAPSHOT_ENTER /* 'E' */ :

		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_SNAPSHOT_ENTER ) || 
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_SNAPSHOT_ENTER ) ) {

			return OTF_Reader_readEnterSnapshot( buffer, handlers );
		}

		break;

	case OTF_KEYWORD_F_SNAPSHOT_SEND /* 'S' */ :

		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_SNAPSHOT_SEND ) || 
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_SNAPSHOT_SEND ) ) {

			return OTF_Reader_readSendSnapshot( buffer, handlers );
		}

		break;
	
	case OTF_KEYWORD_F_SNAPSHOT_OPENFILE /* 'O' */ :

		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_SNAPSHOT_OPENFILE ) || 
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_SNAPSHOT_OPENFILE ) ) {

			return OTF_Reader_readOpenFileSnapshot( buffer, handlers );
		}

		break;
	
	case OTF_KEYWORD_F_SNAPSHOT_COMMENT /* 'C' */ :
    /*case OTF_KEYWORD_F_SNAPSHOT_COLLOPCOUNT*/ /* 'C' */
    /*case OTF_KEYWORD_F_SNAPSHOT_COUNTER*/ /* 'C' */

		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_SNAPSHOT_COMMENT ) || 
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_SNAPSHOT_COMMENT ) ) {

			return OTF_Reader_readSnapshotComment( buffer, handlers );
		}

        if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_SNAPSHOT_COLLOPCOUNT )
            || OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_SNAPSHOT_COLLOPCOUNT ) ) {

            return OTF_Reader_readCollopCountSnapshot( buffer, handlers );
        }

        if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_SNAPSHOT_COUNTER )
            || OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_SNAPSHOT_COUNTER ) ) {

            return OTF_Reader_readCounterSnapshot( buffer, handlers );
        }

		break;

    case OTF_KEYWORD_F_SNAPSHOT_BEGINCOLLOP /* 'B' */ :
    /*case OTF_KEYWORD_F_SNAPSHOT_BEGINFILEOP*/ /* 'B' */

        if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_SNAPSHOT_BEGINCOLLOP ) || 
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_SNAPSHOT_BEGINCOLLOP ) ) {

			return OTF_Reader_readBeginCollopSnapshot( buffer, handlers );

        }

        if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_SNAPSHOT_BEGINFILEOP ) || 
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_SNAPSHOT_BEGINFILEOP ) ) {

			return OTF_Reader_readBeginFileOpSnapshot( buffer, handlers );
        }

        break;

	}


	return OTF_Reader_readUnknownRecord( buffer, handlers );
}


int OTF_Reader_parseMarkerRecord( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid ) {

	/* check prefix */
	if ( OTF_RBuffer_testPrefix( buffer, OTF_KEYWORD_L_MARKER_PREFIX ) ||
			OTF_RBuffer_testPrefix( buffer, OTF_KEYWORD_S_MARKER_PREFIX ) ) {

		/* fine */

	} else if ( OTF_RBuffer_testPrefix( buffer, OTF_KEYWORD_L_KEYVALUE_PREFIX ) || 
            OTF_RBuffer_testPrefix( buffer, OTF_KEYWORD_S_KEYVALUE_PREFIX ) ) {

		return OTF_RBuffer_readKeyValueList( buffer );

	} else if ( buffer->buffer[buffer->pos] == '\n' ) {

		return OTF_RBuffer_readNewline( buffer );

	} else {

		return OTF_Reader_readUnknownMarkerRecord( buffer, handlers, streamid );
	}

	switch( buffer->buffer[buffer->pos] ) {

	case OTF_KEYWORD_F_MARKER_DEFMARKER /* 'D' */ :

		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_MARKER_DEFMARKER ) || 
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_MARKER_DEFMARKER ) ) {

			OTF_RBuffer_skipKeyword( buffer );
			return OTF_Reader_readDefMarker( buffer, handlers, streamid );
		}
		
		break;

	case OTF_KEYWORD_F_MARKER_MARKERSPOT /* 'S' */ :

		if ( OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_S_MARKER_MARKERSPOT ) ||
				OTF_RBuffer_testKeyword( buffer, 
				OTF_KEYWORD_L_MARKER_MARKERSPOT ) ) {

			return OTF_Reader_readMarkerSpot( buffer, handlers );
		}

		break;
	}

	return OTF_Reader_readUnknownMarkerRecord( buffer, handlers, streamid );
}


/* *** local function bodies *** ********************************** */


/* *** Definition records *** ************************************* */


int OTF_Reader_readDefinitionComment( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers, uint32_t streamid ) {
	

	const char* comment;


	if ( handlers->pointer[OTF_DEFINITIONCOMMENT_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	comment = OTF_RBuffer_readString( buffer );

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefinitionComment*)
			handlers->pointer[OTF_DEFINITIONCOMMENT_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFINITIONCOMMENT_RECORD],
			streamid, comment, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}

int OTF_Reader_readDefTimerResolution( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	uint64_t ticksPerSecond;


	if ( handlers->pointer[OTF_DEFTIMERRESOLUTION_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	ticksPerSecond = OTF_RBuffer_readUint64( buffer );

	if ( OTF_RBuffer_readNewline( buffer ) )	{

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefTimerResolution*)
			handlers->pointer[OTF_DEFTIMERRESOLUTION_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFTIMERRESOLUTION_RECORD],
			streamid, ticksPerSecond, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
}


int OTF_Reader_readDefProcess( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	uint32_t deftoken;
	uint32_t parent = 0;
	const char* name;

	if ( handlers->pointer[OTF_DEFPROCESS_RECORD] == NULL )	{

		return OTF_RBuffer_readNewline( buffer );
	}

	deftoken = OTF_RBuffer_readUint32( buffer );

	if ( buffer->buffer[buffer->pos] == '\n' ) {

		name = NULL;
		parent = 0;

	} else {

		/** check if the token matchs with the format of the record */
		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, "N" )/*deprecated keyword*/) {


			name = OTF_RBuffer_readString( buffer );

			if ( name == NULL ) {

			PARSE_ERROR( buffer );

				return 0;
			}

			if ( buffer->buffer[buffer->pos] == '\n' ) {

				parent = 0;

			} else {

				/** check if the token matchs with the format 
				of the record */
				if ( OTF_RBuffer_testKeyword( buffer,
						OTF_KEYWORD_S_LOCAL_PARENT ) ||
						OTF_RBuffer_testKeyword( buffer,
						OTF_KEYWORD_L_LOCAL_PARENT )  ||
						OTF_RBuffer_testKeyword( buffer, "P")
						/*deprecated keyword*/) {

					parent = OTF_RBuffer_readUint32( buffer );
				}
			}

		} else if ( OTF_RBuffer_testKeyword( buffer,
						OTF_KEYWORD_S_LOCAL_PARENT ) ||
						OTF_RBuffer_testKeyword( buffer,
						OTF_KEYWORD_L_LOCAL_PARENT ) ||
						OTF_RBuffer_testKeyword( buffer, "P")
						/*deprecated keyword*/) {


			parent = OTF_RBuffer_readUint32( buffer );
			name = NULL;

		} else {

		PARSE_ERROR( buffer );

			return 0;
		}
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefProcess*)
			handlers->pointer[OTF_DEFPROCESS_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFPROCESS_RECORD], 
			streamid, deftoken, name, parent, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
}


int OTF_Reader_readDefProcessGroup( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	int ret;
	uint32_t n;
	uint32_t deftoken;
	const char* name;
    uint8_t skip_keyword = 0;


	if ( handlers->pointer[OTF_DEFPROCESSGROUP_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	deftoken = OTF_RBuffer_readUint32( buffer );

	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_MEMBERS ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_MEMBERS ) ) {

		n= OTF_RBuffer_readArray( buffer, &(buffer->array), &(buffer->arraysize) );

	} else {
      
        /* maybe an empty array was written, therefore read the following 2 keywords together */
        if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_MEMBERS OTF_KEYWORD_S_LOCAL_NAME ) ||
            OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_MEMBERS OTF_KEYWORD_L_LOCAL_NAME ) ) {
          
            skip_keyword = 1;
        
            n = 0;
          
        } else {

		    PARSE_ERROR( buffer );
		
		    return 0;
            
        }
	}

	/** check if the token matchs with the format of the record */
	if (    skip_keyword ||
            OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, "N" )/*deprecated keyword*/) {

		name = OTF_RBuffer_readString( buffer );

		if ( name == NULL ) {

			PARSE_ERROR( buffer );

			return 0;
		}

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		ret= ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefProcessGroup*)
			handlers->pointer[OTF_DEFPROCESSGROUP_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFPROCESSGROUP_RECORD],
			streamid, deftoken, name, n, buffer->array, buffer->list ) ) );

		return ret;

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
}

int OTF_Reader_readDefAttributeList( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	int ret;
	uint32_t num;
	uint32_t attr_token;

	if ( handlers->pointer[OTF_DEFATTRLIST_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	attr_token = OTF_RBuffer_readUint32( buffer );

	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_MEMBERS ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_MEMBERS ) ) {

		num = OTF_RBuffer_readArray( buffer, &(buffer->array), &(buffer->arraysize) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		ret= ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefAttributeList*)
			handlers->pointer[OTF_DEFATTRLIST_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFATTRLIST_RECORD],
			  streamid, attr_token, num, (OTF_ATTR_TYPE*)buffer->array,
			  buffer->list ) ) );

		return ret;

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

}

int OTF_Reader_readDefProcessOrGroupAttributes( OTF_RBuffer* buffer,
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	int ret;
	uint32_t proc_token;
	uint32_t attr_token;

	if ( handlers->pointer[OTF_DEFPROCESSORGROUPATTR_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	proc_token = OTF_RBuffer_readUint32( buffer );

	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_VALUE ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_VALUE ) ) {

		attr_token = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		ret= ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefProcessOrGroupAttributes*)
			handlers->pointer[OTF_DEFPROCESSORGROUPATTR_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFPROCESSORGROUPATTR_RECORD],
			streamid, proc_token, attr_token, buffer->list ) ) );

		return ret;

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

}

int OTF_Reader_readDefFunction( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	uint32_t deftoken;
	uint32_t group;
	uint32_t scltoken;
	const char* name;


	if ( handlers->pointer[OTF_DEFFUNCTION_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	deftoken= OTF_RBuffer_readUint32( buffer );

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_GROUP ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_GROUP ) ) {

		group= OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, "N" )/*deprecated keyword*/) {

		name= OTF_RBuffer_readString( buffer );

		if ( NULL == name ) {

		PARSE_ERROR( buffer );

			return 0;
		}

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	if ( buffer->buffer[buffer->pos] == '\n' ) {

		scltoken= 0;

	} else {

		/** check if the token matchs with the format of the record */
		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_SCL )
				|| OTF_RBuffer_testKeyword( buffer, "S" )/*deprecated keyword*/
				) {

			scltoken= OTF_RBuffer_readUint32( buffer );

		} else {

		PARSE_ERROR( buffer );
			
			return 0;
		}
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefFunction*)
			handlers->pointer[OTF_DEFFUNCTION_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFFUNCTION_RECORD],
			streamid, deftoken, name, group, scltoken, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readDefFunctionGroup( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	uint32_t deftoken;
	const char* name;


	if ( handlers->pointer[OTF_DEFFUNCTIONGROUP_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	deftoken = OTF_RBuffer_readUint32( buffer );

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, "N" )/*deprecated keyword*/) {


		name = OTF_RBuffer_readString( buffer );

		if ( name == NULL ) {

		PARSE_ERROR( buffer );

			return 0;
		}

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefFunctionGroup*)
			handlers->pointer[OTF_DEFFUNCTIONGROUP_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFFUNCTIONGROUP_RECORD],
			streamid, deftoken, name, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readDefCollectiveOperation( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	uint32_t collOp;
	const char* name;
	uint32_t type;


	if ( handlers->pointer[OTF_DEFCOLLOP_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	collOp = OTF_RBuffer_readUint32( buffer );

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, "N" )/*deprecated keyword*/) {

		name = OTF_RBuffer_readString( buffer );

		if ( name == NULL ) {

		PARSE_ERROR( buffer );

			return 0;
		}

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	/** check if the token matches with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_TYPE ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_TYPE ) ||
			OTF_RBuffer_testKeyword( buffer, "T")/*deprecated keyword*/) {

		type = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefCollectiveOperation*)
			handlers->pointer[OTF_DEFCOLLOP_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFCOLLOP_RECORD], 
			streamid, collOp, name, type, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readDefCounter( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	uint32_t deftoken;
	uint32_t properties;
	uint32_t countergroup;
	const char* unit;
	const char* name;


	if ( handlers->pointer[OTF_DEFCOUNTER_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	deftoken = OTF_RBuffer_readUint32( buffer );

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_GROUP ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_GROUP ) ) {

		countergroup = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, "N" )/*deprecated keyword*/) {

		name = OTF_RBuffer_readString( buffer );

		if ( name == NULL ) {

		PARSE_ERROR( buffer );
			
			return 0;
		}

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_PROPERTIES ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_PROPERTIES )
			) {

		properties = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	/** WARNING: double reading is intentionally, because in former versions of otf
	it wrote the group twice(bug) ... if it doesnt find this second group, it is simply
	skipped */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_GROUP ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_GROUP )
			) {

		countergroup = OTF_RBuffer_readUint32( buffer );
	}

	/** check if the token matchs with the format of the record */
	if (	OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_UNIT) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_UNIT ) ) {

		unit = OTF_RBuffer_readString( buffer );

		if ( unit == NULL ) {

		PARSE_ERROR( buffer );

			return 0;
		}

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefCounter*)
			handlers->pointer[OTF_DEFCOUNTER_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFCOUNTER_RECORD],
			streamid, deftoken, name, properties, countergroup, unit, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readDefCounterGroup( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	uint32_t deftoken;
	const char* name;


	if ( handlers->pointer[OTF_DEFCOUNTERGROUP_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	deftoken = OTF_RBuffer_readUint32( buffer );

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, "N" )/*deprecated keyword*/) {


		name = OTF_RBuffer_readString( buffer );

		if ( name == NULL ) {

		PARSE_ERROR( buffer );
			
			return 0;
		}
	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	if ( OTF_RBuffer_readNewline( buffer ) )	{

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefCounterGroup*)
			handlers->pointer[OTF_DEFCOUNTERGROUP_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFCOUNTERGROUP_RECORD],
			streamid, deftoken, name, buffer->list ) ) );
	
	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readDefScl( OTF_RBuffer* buffer, OTF_HandlerArray* handlers,
		uint32_t streamid ) {


	uint32_t deftoken;
	uint32_t sclfile;
	uint32_t sclline;


	if ( handlers->pointer[OTF_DEFSCL_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	deftoken = OTF_RBuffer_readUint32( buffer );

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_FILE ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_FILE ) ) {

		sclfile = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_LINE ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_LINE  ) ||
			OTF_RBuffer_testKeyword( buffer, "L" )/*deprecated keyword*/ ) {

		sclline = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefScl*)
			handlers->pointer[OTF_DEFSCL_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFSCL_RECORD],
			streamid, deftoken, sclfile, sclline, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readDefSclFile( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	uint32_t deftoken;
	const char* filename;


	if ( handlers->pointer[OTF_DEFSCLFILE_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	deftoken = OTF_RBuffer_readUint32( buffer );

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, "N" )/*deprecated keyword*/) {


		filename = OTF_RBuffer_readString( buffer );

		if ( filename == NULL ) {

			PARSE_ERROR( buffer );
			
			return 0;
		}

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefSclFile*)
			handlers->pointer[OTF_DEFSCLFILE_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFSCLFILE_RECORD], 
			streamid, deftoken, filename, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readDefUniqueId( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	uint64_t uid;


	if ( handlers->pointer[OTF_DEFUNIQUEID_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	uid = OTF_RBuffer_readUint64( buffer );

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefUniqueId*)
			handlers->pointer[OTF_DEFUNIQUEID_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFUNIQUEID_RECORD],
			streamid, uid, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readDefVersion( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	uint8_t major;
	uint8_t minor;
	uint8_t sub;
	const char* string;


	if ( handlers->pointer[OTF_DEFVERSION_RECORD] == NULL )	{

		return OTF_RBuffer_readNewline( buffer );
	}

	major = (uint8_t) OTF_RBuffer_readUint32( buffer );

	if ( ! OTF_RBuffer_testChar( buffer, '.' ) ) {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	minor = (uint8_t) OTF_RBuffer_readUint32( buffer );

	if ( ! OTF_RBuffer_testChar( buffer, '.' ) ) {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	sub = (uint8_t) OTF_RBuffer_readUint32( buffer );

	string = OTF_RBuffer_readString( buffer );

	if ( OTF_RBuffer_readNewline( buffer ) )	{

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefVersion*)
			handlers->pointer[OTF_DEFVERSION_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFVERSION_RECORD],
			streamid, major, minor, sub, string, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readDefCreator( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	const char* creator;


	if ( handlers->pointer[OTF_DEFCREATOR_RECORD] == NULL )	{

		return OTF_RBuffer_readNewline( buffer );
	}

	creator = OTF_RBuffer_readString( buffer );

	if ( OTF_RBuffer_readNewline( buffer ) )	{

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefCreator*)
			handlers->pointer[OTF_DEFCREATOR_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFCREATOR_RECORD],
			streamid, creator, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readDefFile( OTF_RBuffer* buffer,
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	uint32_t token;
	const char* name;
	uint32_t group;
	

	if ( handlers->pointer[OTF_DEFFILE_RECORD] == NULL )	{

		return OTF_RBuffer_readNewline( buffer );
	}


	token= OTF_RBuffer_readUint32( buffer );

	
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NAME ) ) {

		name= OTF_RBuffer_readString( buffer );

		if ( NULL == name ) {

			PARSE_ERROR( buffer );

			return 0;
		}

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
	

	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_GROUP ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_GROUP ) ) {

		group= OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	
	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefFile*)
			handlers->pointer[OTF_DEFFILE_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFFILE_RECORD],
			streamid, token, name, group, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}	
}

int OTF_Reader_readDefFileGroup( OTF_RBuffer* buffer,
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	uint32_t token;
	const char* name;

	
	if ( handlers->pointer[OTF_DEFFILEGROUP_RECORD] == NULL )	{

		return OTF_RBuffer_readNewline( buffer );
	}


	token= OTF_RBuffer_readUint32( buffer );

	
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NAME ) ) {

		name= OTF_RBuffer_readString( buffer );

		if ( NULL == name ) {

			PARSE_ERROR( buffer );

			return 0;
		}

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
	

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefFileGroup*)
			handlers->pointer[OTF_DEFFILEGROUP_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFFILEGROUP_RECORD],
			streamid, token, name, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	
	

}

int OTF_Reader_readDefKeyValue( OTF_RBuffer* buffer,
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	uint32_t key;
	OTF_Type type;
	const char* name;
	const char* description;


	if ( handlers->pointer[OTF_DEFKEYVALUE_RECORD] == NULL )	{

		return OTF_RBuffer_readNewline( buffer );
	}


	key= OTF_RBuffer_readUint32( buffer );

	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_TYPE ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_TYPE ) ) {

		type= (OTF_Type) OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
	
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NAME ) ) {

		name= OTF_RBuffer_readString( buffer );

		if ( NULL == name ) {

			PARSE_ERROR( buffer );

			return 0;
		}

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_TAG ) ) {

		description= OTF_RBuffer_readString( buffer );

		if ( NULL == description ) {

			PARSE_ERROR( buffer );

			return 0;
		}

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
	
	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */

		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefKeyValue*)
			handlers->pointer[OTF_DEFKEYVALUE_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFKEYVALUE_RECORD],
			streamid, key, type, name, description, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}

int OTF_Reader_readDefTimeRange( OTF_RBuffer* buffer,
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	uint64_t minTime;
	uint64_t maxTime;

	if ( handlers->pointer[OTF_DEFTIMERANGE_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	minTime= OTF_RBuffer_readUint64( buffer );

	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_TIME ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_TIME ) ) {

		maxTime= OTF_RBuffer_readUint64( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers,
		but the current function returns 0 on errors! */

		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefTimeRange*)
			handlers->pointer[OTF_DEFTIMERANGE_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFTIMERANGE_RECORD],
			streamid,
			minTime,
			maxTime,
			buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
}

int OTF_Reader_readDefCounterAssignments( OTF_RBuffer* buffer,
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	uint32_t counter;
	uint32_t number_of_members;

	if ( handlers->pointer[OTF_DEFCOUNTERASSIGNMENTS_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	counter= OTF_RBuffer_readUint32( buffer );

	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_MEMBERS ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_MEMBERS ) ) {

		number_of_members= OTF_RBuffer_readArray( buffer,
							  &buffer->array,
							  &buffer->arraysize );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers,
		but the current function returns 0 on errors! */

		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefCounterAssignments*)
			handlers->pointer[OTF_DEFCOUNTERASSIGNMENTS_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFCOUNTERASSIGNMENTS_RECORD],
			streamid,
			counter,
			number_of_members,
			buffer->array,
			buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
}

int OTF_Reader_readDefProcessSubstitutes( OTF_RBuffer* buffer,
		OTF_HandlerArray* handlers, uint32_t streamid ) {


	uint32_t representative;
	uint32_t number_of_procs;

	if ( handlers->pointer[OTF_DEFPROCESSSUBSTITUTES_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	representative= OTF_RBuffer_readUint32( buffer );

	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_MEMBERS ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_MEMBERS ) ) {

		number_of_procs= OTF_RBuffer_readArray( buffer,
							&buffer->array,
							&buffer->arraysize );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers,
		but the current function returns 0 on errors! */

		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefProcessSubstitutes*)
			handlers->pointer[OTF_DEFPROCESSSUBSTITUTES_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFPROCESSSUBSTITUTES_RECORD],
			streamid,
			representative,
			number_of_procs,
			buffer->array,
			buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
}

int OTF_Reader_readDefAuxSamplePoint( OTF_RBuffer*      buffer,
                                      OTF_HandlerArray* handlers,
                                      uint32_t          streamid ) {


    uint64_t time;
    OTF_AuxSamplePointType type;

    if ( handlers->pointer[OTF_DEFAUXSAMPLEPOINT_RECORD] == NULL ) {

        return OTF_RBuffer_readNewline( buffer );
    }

    time= OTF_RBuffer_readUint64( buffer );

    if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_TYPE ) ||
            OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_TYPE ) ) {

        type= (OTF_AuxSamplePointType) OTF_RBuffer_readUint32( buffer );

    } else {

        PARSE_ERROR( buffer );

        return 0;
    }

    if ( OTF_RBuffer_readNewline( buffer ) ) {

        /* 0 is considered as the non-error return value of call-back handlers,
        but the current function returns 0 on errors! */

        return ( OTF_RETURN_OK /*0*/ == ( (
                (OTF_Handler_DefAuxSamplePoint*)
                handlers->pointer[OTF_DEFAUXSAMPLEPOINT_RECORD] )
                ( handlers->firsthandlerarg[OTF_DEFAUXSAMPLEPOINT_RECORD],
                        streamid, time, type, buffer->list ) ) );

    } else {

        PARSE_ERROR( buffer );

        return 0;
    }
}

/* *** Event records *** ****************************************** */


int OTF_Reader_readEventComment( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers ) {
	

	const char* comment;


	if ( handlers->pointer[OTF_EVENTCOMMENT_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	comment = OTF_RBuffer_readString( buffer );

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_EventComment*)
			handlers->pointer[OTF_EVENTCOMMENT_RECORD] )
			( handlers->firsthandlerarg[OTF_EVENTCOMMENT_RECORD],
			buffer->time, buffer->process, comment, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readCounter( OTF_RBuffer* buffer, OTF_HandlerArray* handlers ) {


	int ret= 1;

	int32_t counter_token;
	uint64_t value;


	if ( handlers->pointer[OTF_COUNTER_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	counter_token = OTF_RBuffer_readUint32( buffer );

	ret= ret && 
		( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_VALUE ) ||
		OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_VALUE ) );

	value = OTF_RBuffer_readUint64( buffer );

	ret= ret && OTF_RBuffer_readNewline( buffer );

	if ( ret ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_Counter*)
			handlers->pointer[OTF_COUNTER_RECORD] )
			( handlers->firsthandlerarg[OTF_COUNTER_RECORD],
			buffer->time, buffer->process, counter_token, value, buffer->list ) ) );
	} else {
	
		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readNoOp( OTF_RBuffer* buffer, OTF_HandlerArray* handlers ) {
  
  	if ( handlers->pointer[OTF_NOOP_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}
  
  	if ( OTF_RBuffer_readNewline( buffer ) )	{

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_NoOp*)
			handlers->pointer[OTF_NOOP_RECORD] )
			( handlers->firsthandlerarg[OTF_NOOP_RECORD],
			buffer->time, buffer->process, buffer->list ) ) );

	} else {
	
		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readEnter( OTF_RBuffer* buffer, OTF_HandlerArray* handlers ) {


	uint32_t statetoken;
	uint32_t scltoken;


	if ( handlers->pointer[OTF_ENTER_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	statetoken = OTF_RBuffer_readUint32( buffer );

	if ( buffer->buffer[buffer->pos] == '\n' ) {

		scltoken = 0;

	} else {

		/** check if the token matchs with the format of the record */
		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_SCL ) ||
				OTF_RBuffer_testKeyword( buffer, "S" )/*deprecated keyword*/) {

			scltoken = OTF_RBuffer_readUint32( buffer );

		} else {
		
			PARSE_ERROR( buffer );

			return 0;
		}
	}

	if ( OTF_RBuffer_readNewline( buffer ) )	{

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_Enter*)
			handlers->pointer[OTF_ENTER_RECORD] )
			( handlers->firsthandlerarg[OTF_ENTER_RECORD],
			buffer->time, statetoken, buffer->process, scltoken, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readCollectiveOperation( OTF_RBuffer* buffer, 
    	OTF_HandlerArray* handlers ) {


    uint32_t functionToken;
    uint32_t communicator;
    uint32_t rootprocess;
    uint32_t sent;
    uint32_t received;
    uint32_t scltoken;
    uint64_t duration;


    if ( handlers->pointer[OTF_COLLOP_RECORD] == NULL ) {

    	return OTF_RBuffer_readNewline( buffer );
	}

	functionToken = OTF_RBuffer_readUint32( buffer );

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_COMMUNICATOR )
			) {

		communicator = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_ROOT ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_ROOT ) ) {

		rootprocess = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SENT ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_SENT ) ) {

		sent = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_RECVD ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_RECVD ) ) {

		received = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_DURATION ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_DURATION ) ) {

		duration = OTF_RBuffer_readUint64( buffer );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	if ( buffer->buffer[buffer->pos] == '\n' ) {

		scltoken = 0;

	} else {

		/** check if the token matchs with the format of the record */
		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_SCL ) ||
				OTF_RBuffer_testKeyword( buffer, "S" )/*deprecated keyword*/) {

			scltoken = OTF_RBuffer_readUint32( buffer );

		} else {

			PARSE_ERROR( buffer );
			
			return 0;
		}
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_CollectiveOperation*)
			handlers->pointer[OTF_COLLOP_RECORD] )
			( handlers->firsthandlerarg[OTF_COLLOP_RECORD],
			buffer->time, buffer->process, functionToken, communicator,
			rootprocess, sent, received, duration, scltoken, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readBeginCollectiveOperation( OTF_RBuffer* buffer,
                OTF_HandlerArray* handlers )
{
	uint32_t collOp;
	uint64_t matchingId;
	uint32_t procGroup;
	uint32_t rootProc;
	uint64_t sent;
	uint64_t received;
	uint32_t scltoken;

	if( handlers->pointer[OTF_BEGINCOLLOP_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	collOp = OTF_RBuffer_readUint32( buffer );

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_HANDLEID )
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_HANDLEID ) ) {

		matchingId = OTF_RBuffer_readUint64( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR )
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_COMMUNICATOR ) ) {

		procGroup = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_ROOT )
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_ROOT ) ) {

		rootProc = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SENT )
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_SENT ) ) {

		sent = OTF_RBuffer_readUint64( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_RECVD )
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_RECVD ) ) {

		received = OTF_RBuffer_readUint64( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	if( buffer->buffer[buffer->pos] == '\n' ) {

		scltoken = 0;

	} else {

		/** check if the token matches with the format of the record */
		if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL )
		                || OTF_RBuffer_testKeyword( buffer,
		                                OTF_KEYWORD_L_LOCAL_SCL )
		                || OTF_RBuffer_testKeyword( buffer, "S" )/*deprecated keyword*/) {

			scltoken = OTF_RBuffer_readUint32( buffer );

		} else {

			PARSE_ERROR( buffer );

			return 0;
		}
	}

	if( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		 but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_BeginCollectiveOperation*)
			handlers->pointer[OTF_BEGINCOLLOP_RECORD] )
			( handlers->firsthandlerarg[OTF_BEGINCOLLOP_RECORD],
			buffer->time, buffer->process, collOp, matchingId,
			procGroup, rootProc, sent, received, scltoken, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
}


int OTF_Reader_readEndCollectiveOperation( OTF_RBuffer* buffer,
                OTF_HandlerArray* handlers )
{
	uint64_t matchingId;

	if( handlers->pointer[OTF_ENDCOLLOP_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	matchingId = OTF_RBuffer_readUint64( buffer );

	if( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		 but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_EndCollectiveOperation*)
			handlers->pointer[OTF_ENDCOLLOP_RECORD] )
			( handlers->firsthandlerarg[OTF_ENDCOLLOP_RECORD],
			buffer->time, buffer->process, matchingId, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
}


int OTF_Reader_readRecvMsg( OTF_RBuffer* buffer, OTF_HandlerArray* handlers ) {


	uint32_t sender;
	uint32_t communicator;
	uint32_t msgtype;
	uint32_t msglength;
	uint32_t scltoken;


	if ( handlers->pointer[OTF_RECEIVE_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	sender = OTF_RBuffer_readUint32( buffer );

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_LENGTH ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_LENGTH ) ) {

		msglength = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_TAG ) ) {

		msgtype = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR ) || 
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_COMMUNICATOR )
			) {

		communicator = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}


	if ( buffer->buffer[buffer->pos] == '\n' ) {

		scltoken = 0;

	} else {

		/** check if the token matchs with the format of the record */
		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_SCL ) ||
				OTF_RBuffer_testKeyword( buffer, "S" )/*deprecated keyword*/) {

			scltoken = OTF_RBuffer_readUint32( buffer );
		} else {

			PARSE_ERROR( buffer );
			
			return 0;
		}
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_RecvMsg*)
			handlers->pointer[OTF_RECEIVE_RECORD] )
			( handlers->firsthandlerarg[OTF_RECEIVE_RECORD],
			buffer->time, buffer->process, sender, communicator, 
			msgtype, msglength, scltoken, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readSendMsg( OTF_RBuffer* buffer, OTF_HandlerArray* handlers ) {


	uint32_t receiver;
	uint32_t communicator;
	uint32_t msgtype;
	uint32_t msglength;
	uint32_t scltoken;


	if ( handlers->pointer[OTF_SEND_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	receiver = OTF_RBuffer_readUint32( buffer );

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_LENGTH ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_LENGTH ) ) {

		msglength = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG ) ||
		OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_TAG  ) ) {

		msgtype = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	/** check if the token matchs with the format of the record */
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_COMMUNICATOR )
			) {

		communicator = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	if ( buffer->buffer[buffer->pos] == '\n' ) {

		scltoken = 0;

	} else {

		/** check if the token matchs with the format of the record */
		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_SCL ) ||
				OTF_RBuffer_testKeyword( buffer, "S" )/*deprecated keyword*/) {

			scltoken = OTF_RBuffer_readUint32( buffer );

		} else {

			PARSE_ERROR( buffer );

			return 0;
		}
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_SendMsg*)
			handlers->pointer[OTF_SEND_RECORD] )
			( handlers->firsthandlerarg[OTF_SEND_RECORD],
			buffer->time, buffer->process, receiver, communicator, 
			msgtype, msglength, scltoken, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readLeave( OTF_RBuffer* buffer, OTF_HandlerArray* handlers ) {


	uint32_t statetoken;
	uint32_t scltoken;


	if ( handlers->pointer[OTF_LEAVE_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	if ( buffer->buffer[buffer->pos] == '\n' ) {

		statetoken = 0;
		scltoken = 0;

	} else {
	
		statetoken = OTF_RBuffer_readUint32( buffer );

		if ( buffer->buffer[buffer->pos] == '\n' ) {
	
			scltoken = 0;
	
		} else {
	
			/** check if the token matchs with the format of the record */
			if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL ) ||
					OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_SCL ) ||
					OTF_RBuffer_testKeyword( buffer, "S" )/*deprecated keyword*/) {
	
				scltoken = OTF_RBuffer_readUint32( buffer );
	
			} else {
	
				PARSE_ERROR( buffer );
	
				return 0;
			}
		}
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_Leave*)
			handlers->pointer[OTF_LEAVE_RECORD] )
			( handlers->firsthandlerarg[OTF_LEAVE_RECORD],
			buffer->time, statetoken, buffer->process, scltoken, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readBeginProcess( OTF_RBuffer* buffer,
		OTF_HandlerArray* handlers ) {


	if ( handlers->pointer[OTF_BEGINPROCESS_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_BeginProcess*)
			handlers->pointer[OTF_BEGINPROCESS_RECORD] )
			( handlers->firsthandlerarg[OTF_BEGINPROCESS_RECORD],
			buffer->time, buffer->process, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readEndProcess( OTF_RBuffer* buffer,
		OTF_HandlerArray* handlers ) {


	if ( handlers->pointer[OTF_ENDPROCESS_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_EndProcess*)
			handlers->pointer[OTF_ENDPROCESS_RECORD] )
			( handlers->firsthandlerarg[OTF_ENDPROCESS_RECORD],
			buffer->time, buffer->process, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readFileOperation( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers ) {


	uint32_t ret= 1;

	uint32_t fileid;
	uint64_t handleid;
	uint32_t operation;
	uint64_t bytes;
	uint64_t duration;
	uint32_t source;

	if ( handlers->pointer[OTF_FILEOPERATION_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	
	
	fileid = OTF_RBuffer_readUint32( buffer );

	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_HANDLEID ) ||
		OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_HANDLEID ) );
	handleid = OTF_RBuffer_readUint32( buffer );

	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_OPERATION ) ||
		OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_OPERATION ) );
	operation = OTF_RBuffer_readUint32( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTES ) ||
		OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_BYTES  ) );
	bytes = OTF_RBuffer_readUint64( buffer );

	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_DURATION ) ||
		OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_DURATION ) );
	duration = OTF_RBuffer_readUint64( buffer );

	if( 0 == ret ) {

		PARSE_ERROR( buffer );

		return 0;
	}

	if ( buffer->buffer[buffer->pos] == '\n' ) {

		source = 0;

	} else {

		
		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_SCL ) ) {

			source = OTF_RBuffer_readUint32( buffer );

		} else {

			PARSE_ERROR( buffer );

			return 0;
		}
	}

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_FileOperation*)
			handlers->pointer[OTF_FILEOPERATION_RECORD] )
			( handlers->firsthandlerarg[OTF_FILEOPERATION_RECORD],
			buffer->time, fileid, buffer->process, handleid, operation, bytes,
			duration, source, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readBeginFileOperation( OTF_RBuffer* buffer,
                OTF_HandlerArray* handlers )
{
	uint64_t matchingId;
	uint32_t scltoken;

	if( handlers->pointer[OTF_BEGINFILEOP_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	matchingId = OTF_RBuffer_readUint64( buffer );

	if( buffer->buffer[buffer->pos] == '\n' ) {

		scltoken = 0;

	} else {

		if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL )
		                || OTF_RBuffer_testKeyword( buffer,
		                                OTF_KEYWORD_L_LOCAL_SCL ) ) {

			scltoken = OTF_RBuffer_readUint32( buffer );

		} else {

			PARSE_ERROR( buffer );

			return 0;
		}
	}

	if( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		 but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_BeginFileOperation*)
			handlers->pointer[OTF_BEGINFILEOP_RECORD] )
			( handlers->firsthandlerarg[OTF_BEGINFILEOP_RECORD],
			buffer->time, buffer->process, matchingId, scltoken, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
}


int OTF_Reader_readEndFileOperation( OTF_RBuffer* buffer,
                OTF_HandlerArray* handlers )
{
	uint32_t fileid;
	uint64_t matchingId;
    uint64_t handleId;
	uint32_t operation;
	uint64_t bytes;
	uint32_t scltoken;

	if( handlers->pointer[OTF_ENDFILEOP_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	fileid = OTF_RBuffer_readUint32( buffer );

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_MATCHID )
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_MATCHID ) ) {

		matchingId = OTF_RBuffer_readUint64( buffer );

	} else {

        PARSE_ERROR( buffer );
        return 0;
	}

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_HANDLEID )
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_HANDLEID ) ) {

		handleId = OTF_RBuffer_readUint64( buffer );

	} else {

        PARSE_ERROR( buffer );
        return 0;
	}

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_OPERATION)
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_OPERATION) ) {

		operation = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );
		return 0;
	}

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTES)
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_BYTES) ) {

		bytes = OTF_RBuffer_readUint64( buffer );

	} else {

		PARSE_ERROR( buffer );
		return 0;
	}

	if( buffer->buffer[buffer->pos] == '\n' ) {

		scltoken = 0;

	} else {

		if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL )
		                || OTF_RBuffer_testKeyword( buffer,
		                                OTF_KEYWORD_L_LOCAL_SCL ) ) {

			scltoken = OTF_RBuffer_readUint32( buffer );

		} else {

			PARSE_ERROR( buffer );
			return 0;
		}
	}

	if( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		 but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_EndFileOperation*)
			handlers->pointer[OTF_ENDFILEOP_RECORD] )
			( handlers->firsthandlerarg[OTF_ENDFILEOP_RECORD],
			buffer->time, buffer->process, fileid, matchingId,
            handleId, operation, bytes, scltoken, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		return 0;
	}
}


/* the OLD/NEW thing fixes upward compatibility after changing the record contents 
 - the new version is above, it will read the current version of the record
 - the old version is just below this line, it will read the old version of this record
    which misses one value */

int OTF_Reader_readEndFileOperation_old( OTF_RBuffer* buffer,
                OTF_HandlerArray* handlers )
{
	uint32_t fileid;
	uint64_t handleid;
	uint32_t operation;
	uint64_t bytes;
	uint32_t scltoken;


	if( handlers->pointer[OTF_ENDFILEOP_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	fileid = OTF_RBuffer_readUint32( buffer );

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_HANDLEID )
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_HANDLEID ) ) {

		handleid = OTF_RBuffer_readUint64( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_OPERATION)
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_OPERATION) ) {

		operation = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTES)
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_BYTES) ) {

		bytes = OTF_RBuffer_readUint64( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	if( buffer->buffer[buffer->pos] == '\n' ) {

		scltoken = 0;

	} else {

		if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL )
		                || OTF_RBuffer_testKeyword( buffer,
		                                OTF_KEYWORD_L_LOCAL_SCL ) ) {

			scltoken = OTF_RBuffer_readUint32( buffer );

		} else {

			PARSE_ERROR( buffer );

			return 0;
		}
	}

	if( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		 but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_EndFileOperation*)
			handlers->pointer[OTF_ENDFILEOP_RECORD] )
			( handlers->firsthandlerarg[OTF_ENDFILEOP_RECORD],
			buffer->time, buffer->process, fileid, 
            handleid /* matchingID */,
			0 /* handleID */, 
            operation, bytes, scltoken, buffer->list ) ) );

        /* this odd code above fixes upward compatibility after changing the 
        record contents. in this case, the matchingID was missing, so we 
        decided to take the handleID as the mathingID and pretend there is no 
        handleID -- this is the only useful rescue in our opinion */

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
}


int OTF_Reader_readRMAPut( OTF_RBuffer* buffer,
        OTF_HandlerArray* handlers ) {


        uint32_t ret= 1;

        uint32_t origin;
        uint32_t target;
        uint32_t communicator;
        uint32_t tag;
        uint64_t bytes;
        uint32_t scltoken;

        if ( handlers->pointer[OTF_RMAPUT_RECORD] == NULL ) {

                return OTF_RBuffer_readNewline( buffer );
        }



        origin = OTF_RBuffer_readUint32( buffer );


        ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_PROCESS ) ||
                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_PROCESS ) );
        target = OTF_RBuffer_readUint32( buffer );

        ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR ) ||
                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_COMMUNICATOR ) );
        communicator = OTF_RBuffer_readUint32( buffer );

        ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG ) ||
                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_TAG ) );
        tag = OTF_RBuffer_readUint32( buffer );

        ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_LENGTH ) ||
                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_LENGTH  ) );
        bytes = OTF_RBuffer_readUint64( buffer );

        if( 0 == ret ) {

                PARSE_ERROR( buffer );

                return 0;
        }

        if ( buffer->buffer[buffer->pos] == '\n' ) {

                scltoken = 0;

        } else {


                if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL ) ||
                                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_SCL ) ) {

                        scltoken = OTF_RBuffer_readUint32( buffer );

                } else {

                        PARSE_ERROR( buffer );

                        return 0;
                }
        }

        if ( OTF_RBuffer_readNewline( buffer ) ) {

                /* 0 is considered as the non-error return value of call-back handlers,
                but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_RMAPut*)
			handlers->pointer[OTF_RMAPUT_RECORD] )
			( handlers->firsthandlerarg[OTF_RMAPUT_RECORD],
			buffer->time, buffer->process, origin, target,
			communicator, tag, bytes, scltoken, buffer->list ) ) );

        } else {

                PARSE_ERROR( buffer );

                return 0;
        }
}


int OTF_Reader_readRMAPutRemoteEnd( OTF_RBuffer* buffer,
        OTF_HandlerArray* handlers ) {


        uint32_t ret= 1;

        uint32_t origin;
        uint32_t target;
        uint32_t communicator;
        uint32_t tag;
        uint64_t bytes;
        uint32_t scltoken;

        if ( handlers->pointer[OTF_RMAPUTRE_RECORD] == NULL ) {

                return OTF_RBuffer_readNewline( buffer );
        }



        origin = OTF_RBuffer_readUint32( buffer );


        ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_PROCESS ) ||
                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_PROCESS ) );
        target = OTF_RBuffer_readUint32( buffer );

        ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR ) ||
                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_COMMUNICATOR ) );
        communicator = OTF_RBuffer_readUint32( buffer );

        ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG ) ||
                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_TAG ) );
        tag = OTF_RBuffer_readUint32( buffer );

        ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_LENGTH ) ||
                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_LENGTH  ) );
        bytes = OTF_RBuffer_readUint64( buffer );

        if( 0 == ret ) {

                PARSE_ERROR( buffer );

                return 0;
        }

        if ( buffer->buffer[buffer->pos] == '\n' ) {

                scltoken = 0;

        } else {


                if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL ) ||
                                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_SCL ) ) {

                        scltoken = OTF_RBuffer_readUint32( buffer );

                } else {

                        PARSE_ERROR( buffer );

                        return 0;
                }
        }

        if ( OTF_RBuffer_readNewline( buffer ) ) {

                /* 0 is considered as the non-error return value of call-back handlers,
                but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_RMAPutRemoteEnd*)
			handlers->pointer[OTF_RMAPUTRE_RECORD] )
			( handlers->firsthandlerarg[OTF_RMAPUTRE_RECORD],
			buffer->time, buffer->process, origin, target,
			communicator, tag, bytes, scltoken, buffer->list ) ) );

        } else {

                PARSE_ERROR( buffer );

                return 0;
        }
}


int OTF_Reader_readRMAGet( OTF_RBuffer* buffer,
        OTF_HandlerArray* handlers ) {


        uint32_t ret= 1;

        uint32_t origin;
        uint32_t target;
        uint32_t communicator;
        uint32_t tag;
        uint64_t bytes;
        uint32_t scltoken;

        if ( handlers->pointer[OTF_RMAGET_RECORD] == NULL ) {

                return OTF_RBuffer_readNewline( buffer );
        }



        origin = OTF_RBuffer_readUint32( buffer );


        ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_PROCESS ) ||
                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_PROCESS ) );
        target = OTF_RBuffer_readUint32( buffer );

        ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR ) ||
                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_COMMUNICATOR ) );
        communicator = OTF_RBuffer_readUint32( buffer );

        ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG ) ||
                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_TAG ) );
        tag = OTF_RBuffer_readUint32( buffer );

        ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_LENGTH ) ||
                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_LENGTH  ) );
        bytes = OTF_RBuffer_readUint64( buffer );

        if( 0 == ret ) {

                PARSE_ERROR( buffer );

                return 0;
        }

        if ( buffer->buffer[buffer->pos] == '\n' ) {

                scltoken = 0;

        } else {


                if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL ) ||
                                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_SCL ) ) {

                        scltoken = OTF_RBuffer_readUint32( buffer );

                } else {

                        PARSE_ERROR( buffer );

                        return 0;
                }
        }

        if ( OTF_RBuffer_readNewline( buffer ) ) {

                /* 0 is considered as the non-error return value of call-back handlers,
                but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_RMAGet*)
			handlers->pointer[OTF_RMAGET_RECORD] )
			( handlers->firsthandlerarg[OTF_RMAGET_RECORD],
			buffer->time, buffer->process, origin, target,
			communicator, tag, bytes, scltoken, buffer->list ) ) );

        } else {

                PARSE_ERROR( buffer );

                return 0;
        }
}


int OTF_Reader_readRMAEnd( OTF_RBuffer* buffer,
        OTF_HandlerArray* handlers ) {


        uint32_t ret= 1;

	uint32_t remote;
        uint32_t communicator;
        uint32_t tag;
        uint32_t scltoken;

        if ( handlers->pointer[OTF_RMAEND_RECORD] == NULL ) {

                return OTF_RBuffer_readNewline( buffer );
        }



	remote = OTF_RBuffer_readUint32( buffer );


        ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_COMMUNICATOR ) ||
                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_COMMUNICATOR ) );
        communicator = OTF_RBuffer_readUint32( buffer );

        ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG ) ||
                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_TAG ) );
        tag = OTF_RBuffer_readUint32( buffer );

        if( 0 == ret ) {

                PARSE_ERROR( buffer );

                return 0;
        }

        if ( buffer->buffer[buffer->pos] == '\n' ) {

                scltoken = 0;

        } else {


                if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL ) ||
                                OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_SCL ) ) {

                        scltoken = OTF_RBuffer_readUint32( buffer );

                } else {

                        PARSE_ERROR( buffer );

                        return 0;
                }
        }

        if ( OTF_RBuffer_readNewline( buffer ) ) {

                /* 0 is considered as the non-error return value of call-back handlers,
                but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_RMAEnd*)
			handlers->pointer[OTF_RMAEND_RECORD] )
			( handlers->firsthandlerarg[OTF_RMAEND_RECORD],
			buffer->time, buffer->process, remote, communicator,
			tag, scltoken, buffer->list ) ) );

        } else {

                PARSE_ERROR( buffer );

                return 0;
        }
}


/* *** Snapshot records *** ****************************************** */


int OTF_Reader_readSnapshotComment( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers ) {
	

	const char* comment;


	if ( handlers->pointer[OTF_SNAPSHOTCOMMENT_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	comment = OTF_RBuffer_readString( buffer );

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_SnapshotComment*)
			handlers->pointer[OTF_SNAPSHOTCOMMENT_RECORD] )
			( handlers->firsthandlerarg[OTF_SNAPSHOTCOMMENT_RECORD],
			buffer->time, buffer->process, comment, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readEnterSnapshot( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers ) {
	
	
	uint64_t originaltime;
	uint32_t function;
	uint32_t source;
	
	if ( NULL == handlers->pointer[OTF_ENTERSNAPSHOT_RECORD] ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	function= OTF_RBuffer_readUint32( buffer );
	
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_OTIME)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_OTIME) ) {

		originaltime= OTF_RBuffer_readUint64( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
	
	if ( buffer->buffer[buffer->pos] == '\n' ) {

		source = 0;

	} else {

		/** check if the token matchs with the format of the record */
		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_SCL ) ||
				OTF_RBuffer_testKeyword( buffer, "S" )/*deprecated keyword*/) {

			source = OTF_RBuffer_readUint32( buffer );

		} else {

		PARSE_ERROR( buffer );
			
			return 0;
		}
	}

		
	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_EnterSnapshot*)
			handlers->pointer[OTF_ENTERSNAPSHOT_RECORD] )
			( handlers->firsthandlerarg[OTF_ENTERSNAPSHOT_RECORD],
			buffer->time, originaltime, function, buffer->process,
			source, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}

int OTF_Reader_readSendSnapshot( OTF_RBuffer* buffer, 
	OTF_HandlerArray* handlers ) {
	
	
	int ret= 1;
	
	uint64_t originaltime;
	uint32_t receiver;
	uint32_t procGroup;
	uint32_t tag;
    uint32_t length;
	uint32_t source;
	
	if ( NULL == handlers->pointer[OTF_SENDSNAPSHOT_RECORD] ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	receiver= OTF_RBuffer_readUint32( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_OTIME)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_OTIME) );
	originaltime= OTF_RBuffer_readUint64( buffer );

	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_GROUP)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_GROUP) );
	procGroup= OTF_RBuffer_readUint32( buffer );

	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_TAG) );
	tag= OTF_RBuffer_readUint32( buffer );

	if ( 0 == ret ) {

		PARSE_ERROR( buffer );

		return 0;
	}

    if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_LENGTH)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_LENGTH) ) {

		length= OTF_RBuffer_readUint32( buffer );

	} else {

        /* support for old tracefiles */
        length = 0;		

	}
	

	if ( buffer->buffer[buffer->pos] == '\n' ) {

		source = 0;

	} else {

		/** check if the token matchs with the format of the record */
		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_SCL ) ||
				OTF_RBuffer_testKeyword( buffer, "S" )/*deprecated keyword*/) {

			source = OTF_RBuffer_readUint32( buffer );

		} else {

		PARSE_ERROR( buffer );
			
			return 0;
		}
	}
	
	
	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_SendSnapshot*)
			handlers->pointer[OTF_SENDSNAPSHOT_RECORD] )
			( handlers->firsthandlerarg[OTF_SENDSNAPSHOT_RECORD],
			buffer->time, originaltime, buffer->process, receiver,
			procGroup, tag, length, source, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readOpenFileSnapshot( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers ) {


	uint64_t originaltime;
	uint32_t fileid;
	uint64_t handleid;
	uint32_t source;


	if ( NULL == handlers->pointer[OTF_OPENFILESNAPSHOT_RECORD] ) {

		return OTF_RBuffer_readNewline( buffer );
	}
	

	fileid= OTF_RBuffer_readUint32( buffer );
	
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_OTIME)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_OTIME) ) {

		originaltime= OTF_RBuffer_readUint64( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
	
	if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_HANDLEID)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_HANDLEID) ) {

		handleid= OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
	
	if ( buffer->buffer[buffer->pos] == '\n' ) {

		source = 0;

	} else {

		/** check if the token matchs with the format of the record */
		if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL ) ||
				OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_SCL ) ||
				OTF_RBuffer_testKeyword( buffer, "S" )/*deprecated keyword*/) {

			source = OTF_RBuffer_readUint32( buffer );

		} else {

		PARSE_ERROR( buffer );
			
			return 0;
		}
	}

		
	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers,
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_OpenFileSnapshot*)
			handlers->pointer[OTF_OPENFILESNAPSHOT_RECORD] )
			( handlers->firsthandlerarg[OTF_OPENFILESNAPSHOT_RECORD],
			buffer->time, originaltime, fileid, buffer->process,
			handleid, source, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}

}


int OTF_Reader_readBeginCollopSnapshot( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers ) {


    uint64_t originaltime;
	uint32_t collOp;
	uint64_t matchingId;
	uint32_t procGroup;
    uint32_t rootProc;
    uint64_t sent;
    uint64_t received;
    uint32_t scltoken;


	if ( NULL == handlers->pointer[OTF_BEGINCOLLOPSNAPSHOT_RECORD] ) {

		return OTF_RBuffer_readNewline( buffer );
	}


    originaltime= OTF_RBuffer_readUint64( buffer );

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_COLLECTIVE )
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_COLLECTIVE ) ) {

		collOp = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
    
	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_HANDLEID )
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_HANDLEID ) ) {

		matchingId = OTF_RBuffer_readUint64( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_GROUP )
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_GROUP ) ) {

		procGroup = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_ROOT )
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_ROOT ) ) {

		rootProc = OTF_RBuffer_readUint32( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SENT )
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_SENT ) ) {

		sent = OTF_RBuffer_readUint64( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_RECVD )
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_RECVD ) ) {

		received = OTF_RBuffer_readUint64( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}

	if( buffer->buffer[buffer->pos] == '\n' ) {

		scltoken = 0;

	} else {

		/** check if the token matches with the format of the record */
		if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL )
		                || OTF_RBuffer_testKeyword( buffer,
		                                OTF_KEYWORD_L_LOCAL_SCL ) ) {

			scltoken = OTF_RBuffer_readUint32( buffer );

		} else {

			PARSE_ERROR( buffer );

			return 0;
		}
	}

	if( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		 but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_BeginCollopSnapshot*)
			handlers->pointer[OTF_BEGINCOLLOPSNAPSHOT_RECORD] )
			( handlers->firsthandlerarg[OTF_BEGINCOLLOPSNAPSHOT_RECORD],
			buffer->time, originaltime, buffer->process, collOp, matchingId,
			procGroup, rootProc, sent, received, scltoken, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}


}

int OTF_Reader_readBeginFileOpSnapshot( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers ) {


    uint64_t originaltime;
	uint64_t matchingId;
	uint32_t scltoken;

	if( handlers->pointer[OTF_BEGINFILEOPSNAPSHOT_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

    originaltime = OTF_RBuffer_readUint64( buffer );

	/** check if the token matches with the format of the record */
	if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_MATCHID )
	                || OTF_RBuffer_testKeyword( buffer,
	                                OTF_KEYWORD_L_LOCAL_MATCHID ) ) {

		matchingId = OTF_RBuffer_readUint64( buffer );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}


	if( buffer->buffer[buffer->pos] == '\n' ) {

		scltoken = 0;

	} else {

		if( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SCL )
		                || OTF_RBuffer_testKeyword( buffer,
		                                OTF_KEYWORD_L_LOCAL_SCL ) ) {

			scltoken = OTF_RBuffer_readUint32( buffer );

		} else {

			PARSE_ERROR( buffer );

			return 0;
		}
	}

	if( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		 but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_BeginFileOpSnapshot*)
			handlers->pointer[OTF_BEGINFILEOPSNAPSHOT_RECORD] )
			( handlers->firsthandlerarg[OTF_BEGINFILEOPSNAPSHOT_RECORD],
			buffer->time, originaltime, buffer->process, matchingId,
            scltoken, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );

		return 0;
	}


}

int OTF_Reader_readCollopCountSnapshot( OTF_RBuffer* buffer,
                                        OTF_HandlerArray* handlers ) {

    uint32_t communicator;
    uint64_t count;

    if ( handlers->pointer[OTF_COLLOPCOUNTSNAPSHOT_RECORD] == NULL ) {

        return OTF_RBuffer_readNewline( buffer );
    }

    communicator = OTF_RBuffer_readUint32( buffer );

    if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_COUNT )
            || OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_COUNT ) ) {

            count = OTF_RBuffer_readUint64( buffer );

    } else {

        PARSE_ERROR( buffer );

        return 0;
    }

    if ( OTF_RBuffer_readNewline( buffer ) ) {

        /* 0 is considered as the non-error return value of call-back handlers,
         but the current function returns 0 on errors! */
        return ( OTF_RETURN_OK /*0*/ == ( (
            (OTF_Handler_CollopCountSnapshot*)
            handlers->pointer[OTF_COLLOPCOUNTSNAPSHOT_RECORD] )
            ( handlers->firsthandlerarg[OTF_COLLOPCOUNTSNAPSHOT_RECORD],
            buffer->time, buffer->process, communicator,
            count, buffer->list ) ) );

    } else {

        PARSE_ERROR( buffer );

        return 0;
    }
}

int OTF_Reader_readCounterSnapshot( OTF_RBuffer* buffer,
                                    OTF_HandlerArray* handlers ) {

    uint64_t originaltime;
    uint32_t counter;
    uint64_t value;

    if ( handlers->pointer[OTF_COUNTERSNAPSHOT_RECORD] == NULL ) {

        return OTF_RBuffer_readNewline( buffer );
    }

    originaltime = OTF_RBuffer_readUint64( buffer );

    if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_COUNTER )
            || OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_COUNTER ) ) {

            counter = OTF_RBuffer_readUint32( buffer );

    } else {

        PARSE_ERROR( buffer );

        return 0;
    }

    if ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_VALUE )
            || OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_VALUE ) ) {

            value = OTF_RBuffer_readUint64( buffer );

    } else {

        PARSE_ERROR( buffer );

        return 0;
    }

    if ( OTF_RBuffer_readNewline( buffer ) ) {

        /* 0 is considered as the non-error return value of call-back handlers,
         but the current function returns 0 on errors! */
        return OTF_RETURN_OK /*0*/ == ( (OTF_Handler_CounterSnapshot*)
            handlers->pointer[OTF_COUNTERSNAPSHOT_RECORD] )(
                handlers->firsthandlerarg[OTF_COUNTERSNAPSHOT_RECORD],
                buffer->time,
                originaltime,
                buffer->process,
                counter,
                value,
                buffer->list );

    } else {

        PARSE_ERROR( buffer );

        return 0;
    }
}


/* *** Summary records *** ****************************************** */


int OTF_Reader_readSummaryComment( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers ) {
	

	const char* comment;


	if ( handlers->pointer[OTF_SUMMARYCOMMENT_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	comment = OTF_RBuffer_readString( buffer );

	if ( OTF_RBuffer_readNewline( buffer ) ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_SummaryComment*)
			handlers->pointer[OTF_SUMMARYCOMMENT_RECORD] )
			( handlers->firsthandlerarg[OTF_SUMMARYCOMMENT_RECORD],
			buffer->time, buffer->process, comment, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readFunctionSummary( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers ) {


	int ret= 1;

	uint32_t function;
	uint64_t count;
	uint64_t excltime;
	uint64_t incltime;


	if ( NULL == handlers->pointer[OTF_FUNCTIONSUMMARY_RECORD] ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	/* read data items */

	/* tested outside:
	ret= ret && OTF_RBuffer_testKeyword( buffer, "SUMF" ); */

	function= OTF_RBuffer_readUint32( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_COUNT )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_COUNT )
			|| OTF_RBuffer_testKeyword( buffer, "C" )/*deprecated keyword*/);
	count= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_EXCLTIME)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_EXCLTIME) );
	excltime= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_INCLTIME)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_INCLTIME) );
	incltime= OTF_RBuffer_readUint64( buffer );

	ret= ret && OTF_RBuffer_readNewline( buffer );

	if ( ret ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_FunctionSummary*)
			handlers->pointer[OTF_FUNCTIONSUMMARY_RECORD] )
			( handlers->firsthandlerarg[OTF_FUNCTIONSUMMARY_RECORD],
			buffer->time, function, buffer->process,
			count, excltime, incltime, buffer->list ) ) );
	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
}


int OTF_Reader_readFunctionGroupSummary( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers ) {


	int ret= 1;

	uint32_t functiongroup;
	uint64_t count;
	uint64_t excltime;
	uint64_t incltime;


	if ( NULL == handlers->pointer[OTF_FUNCTIONGROUPSUMMARY_RECORD] ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	/* read data items */

	/* tested outside:
	ret= ret && OTF_RBuffer_testKeyword( buffer, "SUMG" ); */

	functiongroup= OTF_RBuffer_readUint32( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_COUNT )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_COUNT )
			|| OTF_RBuffer_testKeyword( buffer, "C" )/*deprecated keyword*/);
	count= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_EXCLTIME)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_EXCLTIME) );
	excltime= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_INCLTIME)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_INCLTIME) );
	incltime= OTF_RBuffer_readUint64( buffer );

	ret= ret && OTF_RBuffer_readNewline( buffer );

	if ( ret ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_FunctionGroupSummary*)
			handlers->pointer[OTF_FUNCTIONGROUPSUMMARY_RECORD] )
			( handlers->firsthandlerarg[OTF_FUNCTIONGROUPSUMMARY_RECORD],
			buffer->time, functiongroup, buffer->process,
			count, excltime, incltime, buffer->list ) ) );
	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
}


int OTF_Reader_readMessageSummary( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers ) {


	int ret= 1;

	uint32_t peer;
	uint32_t comm;
	uint32_t tag;
	uint64_t number_sent;
	uint64_t number_recvd;
	uint64_t bytes_sent;
	uint64_t bytes_recved;


	if ( NULL == handlers->pointer[OTF_MESSAGESUMMARY_RECORD] ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	/* read data items */

	/* tested outside:
	OTF_RBuffer_testKeyword( buffer, "SUMM" ); */

	peer= OTF_RBuffer_readUint32( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer,
			OTF_KEYWORD_S_LOCAL_COMMUNICATOR ) || 
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_COMMUNICATOR));
	comm= OTF_RBuffer_readUint32( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_TAG ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_TAG ) ||
			OTF_RBuffer_testKeyword( buffer, "A" ) );
	tag= OTF_RBuffer_readUint32( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERSENT )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NUMBERSENT ) );
	number_sent= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERRECVD )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NUMBERRECVD ) );
	number_recvd= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SENT ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_SENT ) );
	bytes_sent= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_RECVD )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_RECVD ) );
	bytes_recved= OTF_RBuffer_readUint64( buffer );

	ret= ret && OTF_RBuffer_readNewline( buffer );

	if ( ret ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_MessageSummary*)
			handlers->pointer[OTF_MESSAGESUMMARY_RECORD] )
			( handlers->firsthandlerarg[OTF_MESSAGESUMMARY_RECORD],
			buffer->time, buffer->process, peer, comm, tag,
			number_sent, number_recvd, bytes_sent, bytes_recved, buffer->list ) ) );
	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
}

int OTF_Reader_readCollopSummary( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers ) {


	int ret= 1;

	uint32_t comm;
	uint32_t collective;
	uint64_t number_sent;
	uint64_t number_recved;
	uint64_t bytes_sent;
	uint64_t bytes_recved;

	if ( NULL == handlers->pointer[OTF_COLLOPSUMMARY_RECORD] ) {

		return OTF_RBuffer_readNewline( buffer );
	}
	/* read data items */

	/* tested outside:
	OTF_RBuffer_testKeyword( buffer, "SUMM" ); */

	comm = OTF_RBuffer_readUint32( buffer );

	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_COLLECTIVE )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_COLLECTIVE ) );

	collective = OTF_RBuffer_readUint32( buffer );

	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERSENT )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NUMBERSENT ) );
	number_sent= OTF_RBuffer_readUint64( buffer );

	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERRECVD )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NUMBERRECVD ) );
	number_recved= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_SENT ) ||
			OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_SENT ) );
	bytes_sent= OTF_RBuffer_readUint64( buffer );

	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_RECVD )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_RECVD ) );
	bytes_recved= OTF_RBuffer_readUint64( buffer );

	ret= ret && OTF_RBuffer_readNewline( buffer );

	if ( ret ) {

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_CollopSummary*)
			handlers->pointer[OTF_COLLOPSUMMARY_RECORD] )
			( handlers->firsthandlerarg[OTF_COLLOPSUMMARY_RECORD],
			buffer->time, buffer->process, comm, collective,
			number_sent, number_recved, bytes_sent, bytes_recved, buffer->list ) ) );
	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
}

int OTF_Reader_readFileOperationSummary( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers ) {


	uint32_t ret= 1;

	uint32_t fileid;
	uint64_t nopen;
	uint64_t nclose;
	uint64_t nread;
	uint64_t nwrite;
	uint64_t nseek;
	uint64_t bytesread;
	uint64_t byteswrite;


	if ( NULL == handlers->pointer[OTF_FILEOPERATIONSUMMARY_RECORD] ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	/* read data items */

	fileid= OTF_RBuffer_readUint32( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBEROPEN )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NUMBEROPEN ) );
	nopen= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERCLOSE )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NUMBERCLOSE ) );
	nclose= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERREAD)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NUMBERREAD) );
	nread= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERWRITE)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NUMBERWRITE) );
	nwrite= OTF_RBuffer_readUint64( buffer );
	

	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERSEEK )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NUMBERSEEK ) );
	nseek= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTESREAD)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_BYTESREAD) );
	bytesread= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTESWRITE)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_BYTESWRITE) );
	byteswrite= OTF_RBuffer_readUint64( buffer );

	ret= ret && OTF_RBuffer_readNewline( buffer );

	if ( ret ) {

		/* 0 is considered as the non-error return value of call-back handlers,
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_FileOperationSummary*)
			handlers->pointer[OTF_FILEOPERATIONSUMMARY_RECORD] )
			( handlers->firsthandlerarg[OTF_FILEOPERATIONSUMMARY_RECORD],
			buffer->time, fileid, buffer->process, nopen, nclose,
			nread, nwrite, nseek, bytesread, byteswrite, buffer->list ) ) );
			
	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
}



int OTF_Reader_readFileGroupOperationSummary( OTF_RBuffer* buffer,
	OTF_HandlerArray* handlers ) {


	uint32_t ret= 1;

	uint32_t groupid;
	uint64_t nopen;
	uint64_t nclose;
	uint64_t nread;
	uint64_t nwrite;
	uint64_t nseek;
	uint64_t bytesread;
	uint64_t byteswrite;


	if ( NULL == handlers->pointer[OTF_FILEGROUPOPERATIONSUMMARY_RECORD] ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	/* read data items */

	groupid= OTF_RBuffer_readUint32( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBEROPEN )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NUMBEROPEN ) );
	nopen= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERCLOSE )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NUMBERCLOSE ) );
	nclose= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERREAD)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NUMBERREAD) );
	nread= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERWRITE)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NUMBERWRITE) );
	nwrite= OTF_RBuffer_readUint64( buffer );
	

	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NUMBERSEEK )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NUMBERSEEK ) );
	nseek= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTESREAD)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_BYTESREAD) );
	bytesread= OTF_RBuffer_readUint64( buffer );
	
	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_BYTESWRITE)
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_BYTESWRITE) );
	byteswrite= OTF_RBuffer_readUint64( buffer );

	ret= ret && OTF_RBuffer_readNewline( buffer );

	if ( ret ) {

		/* 0 is considered as the non-error return value of call-back handlers,
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_FileGroupOperationSummary*)
			handlers->pointer[OTF_FILEGROUPOPERATIONSUMMARY_RECORD] )
			( handlers->firsthandlerarg[OTF_FILEGROUPOPERATIONSUMMARY_RECORD],
			buffer->time, groupid, buffer->process, nopen, nclose,
			nread, nwrite, nseek, bytesread, byteswrite, buffer->list ) ) );
			
	} else {

		PARSE_ERROR( buffer );

		return 0;
	}
}


/* *** Marker records *** ******************************************* */


int OTF_Reader_readDefMarker( OTF_RBuffer* buffer, OTF_HandlerArray* handlers, uint32_t streamid ) {


	uint32_t ret= 1;

	uint32_t token;
	const char* name;
	uint32_t type;


	if ( handlers->pointer[OTF_DEFMARKER_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	token = OTF_RBuffer_readUint32( buffer );

	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_NAME )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_NAME ) );

	name= OTF_RBuffer_readString( buffer );

	if ( name == NULL ) {

		PARSE_ERROR( buffer );

		return 0;
	}

	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_TYPE )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_TYPE ) );

	type = OTF_RBuffer_readUint32( buffer );

	if ( OTF_RBuffer_readNewline( buffer ) )	{

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_DefMarker*)
			handlers->pointer[OTF_DEFMARKER_RECORD] )
			( handlers->firsthandlerarg[OTF_DEFMARKER_RECORD], streamid,
			token, name, type, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}


int OTF_Reader_readMarkerSpot( OTF_RBuffer* buffer,  OTF_HandlerArray* handlers ) {


	uint32_t ret= 1;

	uint32_t token;
	uint64_t time;
	uint32_t process;
	const char* text;
	

	if ( handlers->pointer[OTF_MARKER_RECORD] == NULL ) {

		return OTF_RBuffer_readNewline( buffer );
	}

	token= OTF_RBuffer_readUint32( buffer );

	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_TIME )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_TIME ) );
	time= OTF_RBuffer_readUint64( buffer );

	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_PROCESS )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_PROCESS ) );
	process= OTF_RBuffer_readUint32( buffer );

	ret= ret && ( OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_S_LOCAL_VALUE )
			|| OTF_RBuffer_testKeyword( buffer, OTF_KEYWORD_L_LOCAL_VALUE ) );
	text= OTF_RBuffer_readString( buffer );

	if ( text == NULL ) {

		PARSE_ERROR( buffer );

		return 0;
	}

	if ( OTF_RBuffer_readNewline( buffer ) )	{

		/* 0 is considered as the non-error return value of call-back handlers, 
		but the current function returns 0 on errors! */
		return ( OTF_RETURN_OK /*0*/ == ( (
			(OTF_Handler_Marker*)
			handlers->pointer[OTF_MARKER_RECORD] )
			( handlers->firsthandlerarg[OTF_MARKER_RECORD],
			time, process, token, text, buffer->list ) ) );

	} else {

		PARSE_ERROR( buffer );
		
		return 0;
	}
}



/* *** unknown records *** ****************************************** */


int OTF_Reader_readUnknownRecord( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers ) {
		
	int ret;
	char *string;
	
	if ( NULL == handlers->pointer[OTF_UNKNOWN_RECORD] ) {

		return OTF_RBuffer_readNewline( buffer );
	}
		
	string= OTF_RBuffer_printRecord( buffer );
	
	/* 0 is considered as the non-error return value of call-back handlers, 
	but the current function returns 0 on errors! */
	ret= ( OTF_RETURN_OK /*0*/ == ( (
		(OTF_Handler_UnknownRecord*)
		handlers->pointer[OTF_UNKNOWN_RECORD] )
		( handlers->firsthandlerarg[OTF_UNKNOWN_RECORD],
		buffer->time, buffer->process, string ) ) );
		
	free( string );
	string= NULL;

	/* don't check if newline is actually there 
	because this could be the very parse error */
	OTF_RBuffer_readNewline( buffer );

	return ret;
}


int OTF_Reader_readUnknownDefRecord( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid ) {
		
	int ret;
	char *string;
	
	if ( NULL == handlers->pointer[OTF_UNKNOWN_RECORD] ) {

		return OTF_RBuffer_readNewline( buffer );
	}
		
	string= OTF_RBuffer_printRecord( buffer );
	
	/* 0 is considered as the non-error return value of call-back handlers, 
	but the current function returns 0 on errors! */
	ret= ( OTF_RETURN_OK /*0*/ == ( (
		(OTF_Handler_UnknownRecord*)
		handlers->pointer[OTF_UNKNOWN_RECORD])
		( handlers->firsthandlerarg[OTF_UNKNOWN_RECORD],
		(uint64_t) -1, streamid, string ) ) );
		
	free( string );
	string= NULL;

	/* don't check if newline is actually there 
	because this could be the very parse error */
	OTF_RBuffer_readNewline( buffer );

	return ret;
}


int OTF_Reader_readUnknownMarkerRecord( OTF_RBuffer* buffer, 
		OTF_HandlerArray* handlers, uint32_t streamid ) {

	int ret;
	char *string;

	if ( NULL == handlers->pointer[OTF_UNKNOWN_RECORD] ) {

		return OTF_RBuffer_readNewline( buffer );
	}
		
	string= OTF_RBuffer_printRecord( buffer );
	
	/* 0 is considered as the non-error return value of call-back handlers, 
	but the current function returns 0 on errors! */
	ret= ( OTF_RETURN_OK /*0*/ == ( (
		(OTF_Handler_UnknownRecord*)
		handlers->pointer[OTF_UNKNOWN_RECORD])
		( handlers->firsthandlerarg[OTF_UNKNOWN_RECORD], (uint64_t) 0,
 		streamid, string ) ) );
		
	free( string );
	string= NULL;

	/* don't check if newline is actually there 
	because this could be the very parse error */
	OTF_RBuffer_readNewline( buffer );

	return ret;
}


