/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include <cassert>

#include "Handler.h"

int printKeyValueList(Control* c, OTF_KeyValueList* list) {
  	
  	uint32_t key;
	uint32_t type;
    uint32_t len;
	uint32_t i = 0;
	uint32_t j = 0;
	OTF_Value value;

    uint8_t *byte_array = NULL;
    
	if( (c->show_keyvalue == KV_QUIET_MODE) || (OTF_KeyValueList_getCount(list) <= 0) ) {
	   /* empty list */ 
	   fprintf(c->outfile, "\n");
	   
	   return 0;
	} 
	 
	fprintf(c->outfile, ", KeyValue: ");
	
  	while( ! OTF_KeyValueList_getKeyByIndex( list, i, &key) ) {
	 	
	  	type = OTF_KeyValueList_getTypeForKey( list, key );
		
		switch( type ) {
		  case OTF_CHAR:
		        OTF_KeyValueList_getChar( list, key, &(value.otf_char) );
		    	fprintf(c->outfile, "%u:%c", key, value.otf_char);
			
			break;
		  case OTF_UINT8:
		        OTF_KeyValueList_getUint8( list, key, &(value.otf_uint8) );
		    	fprintf(c->outfile, "%u:%hu", key, (unsigned short)value.otf_uint8);
			
			break;
			
		  case OTF_INT8:
		        OTF_KeyValueList_getInt8( list, key, &(value.otf_int8) );
		    	fprintf(c->outfile, "%u:%hd", key, (short)value.otf_int8);
		    
		   	break;
		  
		  case OTF_UINT16:
		        OTF_KeyValueList_getUint16( list, key, &(value.otf_uint16) );
		    	fprintf(c->outfile, "%u:%hu", key, value.otf_uint16);
			
			break;
			
		  case OTF_INT16:
		        OTF_KeyValueList_getInt16( list, key, &(value.otf_int16) );
		    	fprintf(c->outfile, "%u:%hd", key, value.otf_int16);
		    
		   	break;
		  case OTF_UINT32:
		        OTF_KeyValueList_getUint32( list, key, &(value.otf_uint32) );
		    	fprintf(c->outfile, "%u:%u", key, value.otf_uint32);
			
			break;
			
		  case OTF_INT32:
		        OTF_KeyValueList_getInt32( list, key, &(value.otf_int32) );
		    	fprintf(c->outfile, "%u:%d", key, value.otf_int32);
		    
		   	break;
			
		  case OTF_UINT64:
		        OTF_KeyValueList_getUint64( list, key, &(value.otf_uint64) );
			fprintf(c->outfile, "%u:%llu", key,
				(unsigned long long int)value.otf_uint64);
			
			break;
			
		  case OTF_INT64:
		        OTF_KeyValueList_getInt64( list, key, &(value.otf_int64) );
			fprintf(c->outfile, "%u:%lld", key,
				(long long int)value.otf_int64);
		
			break;
			
		  case OTF_FLOAT:
		        OTF_KeyValueList_getFloat( list, key, &(value.otf_float) );
		    	fprintf(c->outfile, "%u:%.3f", key, value.otf_float);
		    
		   	break;
			
		  case OTF_DOUBLE:
		        OTF_KeyValueList_getDouble( list, key, &(value.otf_double) );
		    	fprintf(c->outfile, "%u:%.3f", key, value.otf_double);
		    
		   	break;
			
		  case OTF_BYTE_ARRAY:
                
                OTF_KeyValueList_getArrayLength( list, key, &len );

                if( c->show_keyvalue == KV_BASIC_MODE ) {

                    fprintf(c->outfile, "%u:byte-array(%u)", key, len);
                    break;

                }
                   
                byte_array = (uint8_t*) malloc( len * sizeof( uint8_t ) );

                OTF_KeyValueList_getByteArray( list, key, byte_array, &len );

			    fprintf(c->outfile, "%u:", key);
		
			    for( j = 0; j < len; j++ ) {
			
		            fprintf(c->outfile, "%02hX", (unsigned short)byte_array[j]);
				
			    }

                free( byte_array );
		        			
		   	break;
		}
	  	  
	  	i++;
		
		if( i < OTF_KeyValueList_getCount(list) ) {
			fprintf(c->outfile, ", ");
		}

	}
	
	fprintf(c->outfile, "\n");
	
	return 0;
}

int handleDefinitionComment( void* userData, uint32_t stream,
	const char* comment, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;

	if( c->records[OTF_DEFINITIONCOMMENT_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \tDefComment: stream %u, comment \"%s\"",
				(long long unsigned) c->num, stream, comment );
				
			printKeyValueList(c, kvlist);
		}
	}

	return OTF_RETURN_OK;
}


int handleDefTimerResolution( void* userData, uint32_t stream,
	uint64_t ticksPerSecond, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFTIMERRESOLUTION_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \tDefTimerResolution: stream %u, TicksPerSecond %llu",
				(long long unsigned) c->num, stream,
				(long long unsigned) ticksPerSecond );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleDefProcess( void* userData, uint32_t stream, uint32_t process,
	const char* name, uint32_t parent, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFPROCESS_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \tDefProcess: stream %u, process %u, name \"%s\", parent %u",
				(long long unsigned) c->num, stream, process, name, parent );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleDefProcessGroup( void* userData, uint32_t stream,
	uint32_t group, const char* name, uint32_t numberOfProcs,
	const uint32_t* procs, OTF_KeyValueList* kvlist ) {


  	uint32_t i;
	Control* c= (Control*) userData;


	if( c->records[OTF_DEFPROCESSGROUP_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \tDefProcessGroup: stream %u, group %u, name \"%s\", procs ",
				(long long unsigned) c->num, stream, group, name );

			const char* sep= "";
			for( i= 0; i < numberOfProcs; ++i ) {
				fprintf( c->outfile, "%s%u", sep, procs[i] );
				sep= ", ";
			}
		
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}

#define CASE_RETURN(type) \
	case OTF_ATTR_##type: return #type

static const char* attrToString( OTF_ATTR_TYPE type ) {

	switch( type ) {
	CASE_RETURN( IsMPIRank );
	CASE_RETURN( IsPThread );
	CASE_RETURN( IsOMPThread );
	CASE_RETURN( IsCellSPUThread );
	CASE_RETURN( hasGroupCounters );
	CASE_RETURN( hasEnterLeaveRecords );
	CASE_RETURN( IsCommunicator );

	case OTF_ATTR_UNKNOWN:
	default: {
		static char unknown_buffer[ 64 ];
		sprintf( unknown_buffer, "UNKNOWN <%u>", type );
		return unknown_buffer;
	}

	}
}

#undef CASE_RETURN

int handleDefAttributeList( void* userData, uint32_t stream,
	uint32_t attr_token, uint32_t num, OTF_ATTR_TYPE* array,
	OTF_KeyValueList* kvlist ) {


	uint32_t i;
	Control* c= (Control*) userData;


	if( c->records[OTF_DEFATTRLIST_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \tDefAttributeList: stream %u, attr_token %u, attributes ",
				(long long unsigned) c->num, stream, attr_token);

			const char* sep = "";
			for( i= 0; i < num; ++i ) {
				fprintf( c->outfile, "%s%s", sep, attrToString( array[i] ) );
				sep = ", ";
			}
			
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}

int handleDefProcessOrGroupAttributes( void* userData, uint32_t stream,
	uint32_t proc_token, uint32_t attr_token, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFPROCESSORGROUPATTR_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \tDefProcessOrGroupAttributes: stream %u, process %u, attr_token %u",
				(long long unsigned) c->num, stream, proc_token, attr_token);
				
			printKeyValueList(c, kvlist);
		}
	}




	return OTF_RETURN_OK;
}

int handleDefFunction( void* userData, uint32_t stream, uint32_t func,
	const char* name, uint32_t funcGroup, uint32_t source, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFFUNCTION_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \tDefFunction: stream %u, function %u, name \"%s\", group %u, source %u",
				(long long unsigned) c->num, stream, func, name, funcGroup, source );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleDefFunctionGroup( void* userData, uint32_t stream,
	uint32_t funcGroup, const char* name, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFFUNCTIONGROUP_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \tDefFunctionGroup: stream %u, group %u, name \"%s\"",
				(long long unsigned) c->num, stream, funcGroup, name );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


#define CASE_RETURN(name) \
	case OTF_COLLECTIVE_TYPE_##name: return #name

static const char* collOpTypeToString( uint32_t type ) {

	switch( type ) {
	CASE_RETURN( BARRIER );
	CASE_RETURN( ONE2ALL );
	CASE_RETURN( ALL2ONE );
	CASE_RETURN( ALL2ALL );

	case OTF_COLLECTIVE_TYPE_UNKNOWN:
	default: {
		static char unknown_buffer[ 64 ];
		sprintf( unknown_buffer, "UNKNOWN <%u>", type );
		return unknown_buffer;
	}

	}
}

#undef CASE_RETURN

int handleDefCollectiveOperation( void* userData, uint32_t stream,
	uint32_t collOp, const char* name, uint32_t type, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFCOLLOP_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \tDefCollective: stream %u, collective %u, name \"%s\", type %s",
				(long long unsigned) c->num, stream,  collOp, name, collOpTypeToString( type ) );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


#define CASE_RETURN(prefix, name) \
	case OTF_COUNTER_##prefix##_##name: return #name

static const char* counterTypeToString( uint32_t properties ) {

	switch( properties & OTF_COUNTER_TYPE_BITS ) {
	CASE_RETURN( TYPE, ACC );
	CASE_RETURN( TYPE, ABS );

	default: {
		static char unknown_buffer[ 64 ];
		sprintf( unknown_buffer, "UNKNOWN <%u>",
			properties & OTF_COUNTER_TYPE_BITS );
		return unknown_buffer;
	}

	}
}

static const char* counterScopeToString( uint32_t properties ) {

	switch( properties & OTF_COUNTER_SCOPE_BITS ) {
	CASE_RETURN( SCOPE, START );
	CASE_RETURN( SCOPE, POINT );
	CASE_RETURN( SCOPE, LAST );
	CASE_RETURN( SCOPE, NEXT );

	default: {
		static char unknown_buffer[ 64 ];
		sprintf( unknown_buffer, "UNKNOWN <%u>",
			properties & OTF_COUNTER_SCOPE_BITS );
		return unknown_buffer;
	}

	}
}

static const char* counterVarTypeToString( uint32_t properties ) {

	switch( properties & OTF_COUNTER_VARTYPE_BITS ) {
	CASE_RETURN( VARTYPE, UNSIGNED8 );
	CASE_RETURN( VARTYPE, SIGNED8 );
	CASE_RETURN( VARTYPE, UNSIGNED4 );
	CASE_RETURN( VARTYPE, SIGNED4 );
	CASE_RETURN( VARTYPE, UNSIGNED2 );
	CASE_RETURN( VARTYPE, SIGNED2 );
	CASE_RETURN( VARTYPE, FLOAT );
	CASE_RETURN( VARTYPE, DOUBLE );

	default: {
		static char unknown_buffer[ 64 ];
		sprintf( unknown_buffer, "UNKNOWN <%u>",
			properties & OTF_COUNTER_VARTYPE_BITS );
		return unknown_buffer;
	}

	}
}

#undef CASE_RETURN

int handleDefCounter( void* userData, uint32_t stream, uint32_t counter,
	const char* name, uint32_t properties, uint32_t counterGroup,
	const char* unit, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;

	c->counter_props[counter] = properties;

	if( c->records[OTF_DEFCOUNTER_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \tDefCounter: stream %u, "
				"counter %u, name \"%s\", properties %s/%s/%s, group %u, unit \"%s\"",
				(long long unsigned) c->num, stream, counter, name,
				counterTypeToString( properties ),
				counterScopeToString( properties ),
				counterVarTypeToString( properties ),
				counterGroup, unit );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleDefCounterGroup( void* userData, uint32_t stream,
	uint32_t counterGroup, const char* name, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFCOUNTERGROUP_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \tDefCounterGroup: stream %u, group %u, name \"%s\"",
				(long long unsigned) c->num, stream, counterGroup, name );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleDefScl( void* userData, uint32_t stream, uint32_t source,
	uint32_t sourceFile, uint32_t line, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFSCL_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \tDefScl: stream %u, source %u, file %u, line %u",
				(long long unsigned) c->num, stream, source, sourceFile, line );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleDefSclFile( void* userData, uint32_t stream,
	uint32_t sourceFile, const char* name, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFSCLFILE_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \tDefSclFile: stream %u, file %u, name \"%s\"",
				(long long unsigned) c->num, stream, sourceFile, name );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleDefCreator( void* userData, uint32_t stream,
	const char* creator, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_DEFCREATOR_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \tDefCreator: stream %u, creator \"%s\"",
				(long long unsigned) c->num, stream, creator );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleDefUniqueId( void* userData, uint32_t stream, uint64_t uid ) {
	Control* c= (Control*) userData;


	if( c->records[OTF_DEFUNIQUEID_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \tDefUniqueId: stream %u, uid: %llu\n",
				(long long unsigned) c->num, stream, (long long unsigned) uid );
		}
	}


	return OTF_RETURN_OK;
}


int handleDefVersion( void* userData, uint32_t stream, uint8_t major,
	uint8_t minor, uint8_t sub, const char* string ) {
	Control* c= (Control*) userData;


	if( c->records[OTF_DEFVERSION_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \tDefVersion: stream %u, version: %u.%u.%u \"%s\"\n",
				(long long unsigned) c->num, stream, major, minor, sub, string );
		}
	}


	return OTF_RETURN_OK;
}


int handleDefFile( void* userData, uint32_t stream,
           uint32_t token, const char *name,
           uint32_t group, OTF_KeyValueList* kvlist )
{
        Control* c= (Control*) userData;

        if( c->records[OTF_DEFFILE_RECORD] ) {

                ++c->num;
                if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

                        fprintf( c->outfile, "(#%llu) \tDefFile: %s, stream %llu, "
                                             "token %llu, group %llu",
                                (long long unsigned) c->num, name, (long long unsigned) stream,
                                (long long unsigned) token, (long long unsigned) group );
				
			            printKeyValueList(c, kvlist);
                }
        }
        return OTF_RETURN_OK;
}


int handleDefFileGroup( void* userData, uint32_t stream,
                uint32_t token, const char *name, OTF_KeyValueList* kvlist )
{
        Control* c= (Control*) userData;

        if( c->records[OTF_DEFFILEGROUP_RECORD] ) {

                ++c->num;
                if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

                        fprintf( c->outfile, "(#%llu) \tDefFileGroup: %s, token %llu, "
                                             "stream %llu",
                                (long long unsigned) c->num, name,
                                (long long unsigned) token, (long long unsigned) stream);
				
			            printKeyValueList(c, kvlist);
                }
        }
        return OTF_RETURN_OK;
}


#define CASE_RETURN(type) \
	case OTF_##type: return #type

static const char* keyValueTypeToString( OTF_Type type ) {

	switch( type ) {
	CASE_RETURN( CHAR );
	CASE_RETURN( INT8 );
	CASE_RETURN( UINT8 );
	CASE_RETURN( INT16 );
	CASE_RETURN( UINT16 );
	CASE_RETURN( INT32 );
	CASE_RETURN( UINT32 );
	CASE_RETURN( INT64 );
	CASE_RETURN( UINT64 );
	CASE_RETURN( FLOAT );
	CASE_RETURN( DOUBLE );
	CASE_RETURN( BYTE_ARRAY );

	case OTF_UNKNOWN:
	default: {
		static char unknown_buffer[ 64 ];
		sprintf( unknown_buffer, "UNKNOWN <%u>", type );
		return unknown_buffer;
	}

	}
}

#undef CASE_RETURN

int handleDefKeyValue( void *userData, uint32_t streamid, uint32_t token,
                OTF_Type type, const char *name, const char* desc,
                OTF_KeyValueList* kvlist )
{
	Control* c= (Control*) userData;


	if( c->records[OTF_DEFKEYVALUE_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \tDefKeyValue: stream %u, token %u, type %s, name \"%s\", desc \"%s\"",
				(long long unsigned) c->num, streamid, token,
				keyValueTypeToString( type ), name, desc );
				
			printKeyValueList(c, kvlist);
		}
	}

	return OTF_RETURN_OK;
}


int handleDefTimeRange( void* userData, uint32_t streamid, uint64_t minTime,
                uint64_t maxTime, OTF_KeyValueList* kvlist )
{
	Control* c= (Control*) userData;

	if ( c->records[OTF_DEFTIMERANGE_RECORD] ) {

		++c->num;
		if ( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile,
				"(#%llu) \tDefTimeRange: stream %u, "
				"[%llu, %llu]",
				(long long unsigned) c->num,
				streamid,
				(long long unsigned) minTime,
				(long long unsigned) maxTime );

			printKeyValueList( c, kvlist );
		}
	}

	return OTF_RETURN_OK;
}


int handleDefCounterAssignments( void* userData, uint32_t streamid,
                uint32_t counter_token, uint32_t number_of_members,
                const uint32_t* procs_or_groups, OTF_KeyValueList* kvlist )
{
	Control* c= (Control*) userData;
	uint32_t i;

	if ( c->records[OTF_DEFCOUNTERASSIGNMENTS_RECORD] ) {

		++c->num;
		if ( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			const char* sep= "";

			fprintf( c->outfile,
				"(#%llu) \tDefCounterAssignments: stream %u, "
				"counter_token %u, assignees ",
				(long long unsigned) c->num,
				streamid,
				counter_token );

			for ( i= 0; i < number_of_members; ++i ) {

				fprintf( c->outfile, "%s%u", sep, procs_or_groups[i] );
				sep= ", ";
			}

			printKeyValueList( c, kvlist );
		}
	}

	return OTF_RETURN_OK;
}


int handleDefProcessSubstitutes( void* userData, uint32_t streamid,
	uint32_t representative, uint32_t numberOfProcs, const uint32_t* procs,
	OTF_KeyValueList* kvlist ) {

	Control* c= (Control*) userData;
	uint32_t i;

	if ( c->records[OTF_DEFPROCESSSUBSTITUTES_RECORD] ) {

		++c->num;
		if ( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			const char* sep= "";

			fprintf( c->outfile,
				"(#%llu) \tDefProcessSubstitutes: stream %u, "
				"representative process %u, procs ",
				(long long unsigned) c->num,
				streamid,
				representative );

			for ( i= 0; i < numberOfProcs; ++i ) {

				fprintf( c->outfile, "%s%u", sep, procs[i] );
				sep= ", ";
			}

			printKeyValueList( c, kvlist );
		}
	}

	return OTF_RETURN_OK;
}


static const char* auxSamplePointTypeToString( OTF_AuxSamplePointType type ) {


    #define CASE_RETURN(type) \
            case OTF_AUX_SAMPLE_POINT_##type: return #type

    switch( type ) {
    CASE_RETURN( SNAPSHOT );
    CASE_RETURN( SUMMARY );
    CASE_RETURN( INLINE_SNAPSHOT );

    default:
		static char unknown_buffer[ 64 ];
		sprintf( unknown_buffer, "UNKNOWN <%u>", type );
		return unknown_buffer;
    }

    #undef CASE_RETURN
}

int handleDefAuxSamplePoint( void*                  userData,
                             uint32_t               streamid,
                             uint64_t               time,
                             OTF_AuxSamplePointType type,
                             OTF_KeyValueList*      kvlist ) {


    Control* c= (Control*) userData;


    if ( c->records[OTF_DEFAUXSAMPLEPOINT_RECORD] ) {

        ++c->num;
        if ( c->num >= c->minNum && c->num <= c->maxNum && !c->silent_mode ) {

            fprintf( c->outfile, "(#%llu) \tDefAuxSamplePoint: stream %u, time %llu, type %s",
                    (long long unsigned) c->num, streamid,
                    (long long unsigned) time,
                    auxSamplePointTypeToString( type ) );

            printKeyValueList(c, kvlist);
        }
    }

    return OTF_RETURN_OK;
}


int handleNoOp( void* userData, uint64_t time, uint32_t process,
	OTF_KeyValueList* kvlist ) {
  
  
  	Control* c= (Control*) userData;


	if( c->records[OTF_NOOP_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu NoOp: process %u",
				(long long unsigned) c->num, (long long unsigned) time,
				process );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleEnter( void* userData, uint64_t time, uint32_t function,
	uint32_t process, uint32_t source, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_ENTER_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu Enter: function %u, process %u, source %u",
				(long long unsigned) c->num, (long long unsigned) time,
				function, process, source );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleLeave( void* userData, uint64_t time, uint32_t function,
	uint32_t process, uint32_t source, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_LEAVE_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu Leave: function %u, process %u, source %u",
				(long long unsigned) c->num, (long long unsigned) time,
				 function, process, source);
				 
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleSendMsg( void* userData, uint64_t time, uint32_t sender,
	uint32_t receiver, uint32_t group, uint32_t type, uint32_t length,
	uint32_t source, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_SEND_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu SendMessage: sender %u, receiver %u, group %u, type %u, length %u, source %u",
				(long long unsigned) c->num, (long long unsigned) time,
				sender, receiver, group, type, length, source );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleRecvMsg( void* userData, uint64_t time, uint32_t recvProc,
	uint32_t sendProc, uint32_t group, uint32_t type, uint32_t length, 
	uint32_t source, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_RECEIVE_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu ReceiveMessage: receiver %u, sender %u, group %u, type %u, length %u, source %u",
				(long long unsigned) c->num, (long long unsigned) time,
				recvProc, sendProc, group, type, length, source );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


static void printCounterValue( Control* c, uint32_t counter, uint64_t value ) {

	uint32_t properties = OTF_COUNTER_VARTYPE_UNSIGNED8;
	std::map<uint32_t,uint32_t>::const_iterator it
		= c->counter_props.find( counter );
	if( it != c->counter_props.end() ) {

		properties= it->second & OTF_COUNTER_VARTYPE_BITS;
	}

	switch( properties ) {

	case OTF_COUNTER_VARTYPE_UNSIGNED8:
	case OTF_COUNTER_VARTYPE_UNSIGNED4:
	case OTF_COUNTER_VARTYPE_UNSIGNED2: {

		uint64_t conv_value = OTF_Counter2Unsigned( value );
		fprintf( c->outfile, "%llu", (unsigned long long) conv_value );
		break;
	}

	case OTF_COUNTER_VARTYPE_SIGNED8:
	case OTF_COUNTER_VARTYPE_SIGNED4:
	case OTF_COUNTER_VARTYPE_SIGNED2: {

		int64_t conv_value = OTF_Counter2Signed( value );
		fprintf( c->outfile, "%lld", (long long) conv_value );
		break;
	}

	case OTF_COUNTER_VARTYPE_FLOAT: {

		float conv_value = OTF_Counter2Float( value );
		fprintf( c->outfile, "%f", conv_value );
		break;
	}

	case OTF_COUNTER_VARTYPE_DOUBLE: {

		double conv_value = OTF_Counter2Double( value );
		fprintf( c->outfile, "%f", conv_value );
		break;
	}

	default:
		fprintf( c->outfile, "INVALID TYPE <%08llx>",
			(unsigned long long)value );
	}
}

int handleCounter( void* userData, uint64_t time, uint32_t process,
	uint32_t counter, uint64_t value, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_COUNTER_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu Counter: process %u, counter %u, value ",
				(long long unsigned) c->num, (long long unsigned) time,
				process, counter );
			printCounterValue( c, counter, value );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleCollectiveOperation( void* userData, uint64_t time,
	uint32_t process, uint32_t collective, uint32_t procGroup,
	uint32_t rootProc, uint32_t sent, uint32_t received, uint64_t duration, 
	uint32_t source, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_COLLOP_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu Collective: process %u, collective %u, group %u, root %u, sent %u, received %u, duration %llu, source %u",
				(long long unsigned) c->num, (long long unsigned) time,
				process, collective, procGroup, rootProc, sent, received,
				(long long unsigned) duration, source );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleBeginCollectiveOperation( void* userData, uint64_t time,
		uint32_t process, uint32_t collOp, uint64_t matchingId,
		uint32_t procGroup, uint32_t rootProc, uint64_t sent,
		uint64_t received, uint32_t scltoken, OTF_KeyValueList* kvlist )
{
	Control* c = (Control*) userData;

	if( c->records[OTF_BEGINCOLLOP_RECORD] ) {

		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu BeginCollective: "
					"process %u, collective %u, "
					"group %u, matchingId %llu, "
					"root %u, sent %llu, "
					"received %llu, source %u",
					(long long unsigned) c->num,
					(long long unsigned) time, process,
					collOp, procGroup,
					(long long unsigned) matchingId,
					rootProc,
					(long long unsigned) sent,
					(long long unsigned) received,
					scltoken );
			
			printKeyValueList(c, kvlist);
		}
	}

	return OTF_RETURN_OK;
}


int handleEndCollectiveOperation( void* userData, uint64_t time,
		uint32_t process, uint64_t matchingId, OTF_KeyValueList* kvlist )
{
	Control* c = (Control*) userData;

	if( c->records[OTF_ENDCOLLOP_RECORD] ) {

		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu EndCollective: "
					"process %u, matchingId %llu",
					(long long unsigned) c->num,
					(long long unsigned) time, process,
					(long long unsigned) matchingId );
					
			printKeyValueList(c, kvlist);
		}
	}

	return OTF_RETURN_OK;
}


int handleEventComment( void* userData, uint64_t time, uint32_t process,
	const char* comment, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_EVENTCOMMENT_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu EventComment: process %u, comment \"%s\"",
				(long long unsigned) c->num, (long long unsigned) time,
				process, comment );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleBeginProcess( void* userData, uint64_t time, uint32_t process, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_BEGINPROCESS_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu BeginProcess: process %u",
				(long long unsigned) c->num, (long long unsigned) time,
				process );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleEndProcess( void* userData, uint64_t time, uint32_t process, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_ENDPROCESS_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu EndProcess: process %u",
				(long long unsigned) c->num, (long long unsigned) time,
				process );
			
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleSnapshotComment( void* userData, uint64_t time,
	uint32_t process, const char* comment, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_SNAPSHOTCOMMENT_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu SnapComment: process %u, comment \"%s\"",
				(long long unsigned) c->num, (long long unsigned) time,
				process, comment );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleEnterSnapshot( void *userData, uint64_t time,
	uint64_t originaltime, uint32_t function, uint32_t process,
	uint32_t source, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_ENTERSNAPSHOT_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu SnapEnter: otime %llu, process %u, function %u, source %u",
				(long long unsigned) c->num, (long long unsigned) time,
				(long long unsigned) originaltime, process, function, source );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleSendSnapshot( void *userData, uint64_t time,
	uint64_t originaltime, uint32_t sender, uint32_t receiver,
	uint32_t procGroup, uint32_t tag, uint32_t length, uint32_t source,
    OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_SENDSNAPSHOT_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu SnapSend: otime %llu, sender %u, receiver %u, group %u, tag %u, length %u, source %u",
				(long long unsigned) c->num, (long long unsigned) time,
				(long long unsigned) originaltime, sender, receiver, procGroup,
				tag, length, source );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleOpenFileSnapshot( void* userData, uint64_t time,
    uint64_t originaltime, uint32_t fileid, uint32_t process, uint64_t handleid,
    uint32_t source, OTF_KeyValueList* kvlist ) {
 
  
    Control* c= (Control*) userData;
  
    
    if( c->records[OTF_OPENFILESNAPSHOT_RECORD] ) {
    
        ++c->num;
        if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

            fprintf( c->outfile, "(#%llu) \t%llu SnapOpenFile: otime %llu, process %u, fileid %u,"
                     " handle ID %llu, source %u",
                (long long unsigned) c->num, (long long unsigned) time,
                (long long unsigned) originaltime, process, fileid,
	        (long long unsigned) handleid, source );
                
            printKeyValueList(c, kvlist);
        }
    }
    
  
    return OTF_RETURN_OK;
}


int handleBeginCollopSnapshot( void *userData, uint64_t time, uint64_t originaltime,
    uint32_t process, uint32_t collOp, uint64_t matchingId, uint32_t procGroup,
    uint32_t rootProc, uint64_t sent, uint64_t received, uint32_t scltoken,
    OTF_KeyValueList *kvlist) {
  
  
    Control* c= (Control*) userData;
    
    
    if( c->records[OTF_BEGINCOLLOPSNAPSHOT_RECORD ] ) {
    
        ++c->num;
        if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

            fprintf( c->outfile, "(#%llu) \t%llu SnapBeginCollop: otime %llu, process %u, collOp %u,"
                     " matchingID %llu, group %u, root %u, sent %llu, received %llu, source %u",
                (long long unsigned) c->num, (long long unsigned) time,
                (long long unsigned) originaltime, process, collOp,
                (long long unsigned) matchingId, procGroup, rootProc,
                (long long unsigned) sent, (long long unsigned) received, scltoken );
                
            printKeyValueList(c, kvlist);
        }
    }
    
    
    return OTF_RETURN_OK;
}

    
int handleBeginFileOpSnapshot( void *userData, uint64_t time,
    uint64_t originaltime, uint32_t process, uint64_t matchingId,
    uint32_t scltoken, OTF_KeyValueList *kvlist ) {
  
  
    Control* c= (Control*) userData;
  
    
    if( c->records[OTF_BEGINFILEOPSNAPSHOT_RECORD ] ) {
    
        ++c->num;
        if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

            fprintf( c->outfile, "(#%llu) \t%llu SnapBeginFileOp: otime %llu, process %u,"
                     " matchingID %llu, source %u",
                (long long unsigned) c->num, (long long unsigned) time,
                (long long unsigned) originaltime, process,
                (long long unsigned) matchingId, scltoken );
                
            printKeyValueList(c, kvlist);
        }
    }
    
  
    return OTF_RETURN_OK;
}


int handleCollopCountSnapshot( void *userData, uint64_t time,
    uint32_t process, uint32_t communicator, uint64_t count,
    OTF_KeyValueList *kvlist) {

    Control* c= (Control*) userData;

    if ( c->records[OTF_COLLOPCOUNTSNAPSHOT_RECORD] ) {

        ++c->num;
        if ( c->num >= c->minNum && c->num <= c->maxNum && !c->silent_mode ) {

            fprintf( c->outfile, "(#%llu) \t%llu SnapCollopCount: process %u, "
                     "communicator %u, count %llu",
                (long long unsigned) c->num, (long long unsigned) time,
                process, communicator, (long long unsigned) count );

            printKeyValueList(c, kvlist);
        }
    }

    return OTF_RETURN_OK;
}


int handleCounterSnapshot( void             *userData,
                           uint64_t          time,
                           uint32_t          process,
                           uint64_t          originaltime,
                           uint32_t          counter,
                           uint64_t          value,
                           OTF_KeyValueList *kvlist ) {

    Control* c= (Control*) userData;

    if ( c->records[OTF_COUNTERSNAPSHOT_RECORD] ) {

        ++c->num;
        if ( c->num >= c->minNum && c->num <= c->maxNum && !c->silent_mode ) {

            fprintf( c->outfile,
                "(#%llu) \t%llu SnapCounter: "
                "otime %llu, process %u, counter %u, value ",
                (long long unsigned) c->num,
                (long long unsigned) time,
                (long long unsigned) originaltime,
                process,
                counter );
            printCounterValue( c, counter, value );

            printKeyValueList(c, kvlist);
        }
    }

    return OTF_RETURN_OK;
}


int handleSummaryComment( void * userData, uint64_t time,
	uint32_t process, const char* comment, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_SUMMARYCOMMENT_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu StatComment: process %u, comment \"%s\"",
				(long long unsigned) c->num, (long long unsigned) time,
				process, comment );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleFunctionSummary( void* userData, uint64_t time,
	uint32_t function, uint32_t process, uint64_t invocations,
	uint64_t exclTime, uint64_t inclTime, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_FUNCTIONSUMMARY_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu StatFunction: process %u, function %u, invocations %llu, excltime %llu, incltime %llu",
				(long long unsigned) c->num, (long long unsigned) time,
				process, function, (long long unsigned) invocations,
				(long long unsigned) exclTime, (long long unsigned) inclTime );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleFunctionGroupSummary( void* userData, uint64_t time,
	uint32_t funcGroup, uint32_t process, uint64_t invocations,
	uint64_t exclTime, uint64_t inclTime, OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_FUNCTIONGROUPSUMMARY_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu StatFunctionGroup: process %u, group %u, invocations %llu, excltime %llu, incltime %llu",
				(long long unsigned) c->num, (long long unsigned) time,
				process, funcGroup, (long long unsigned) invocations,
				(long long unsigned) exclTime, (long long unsigned) inclTime );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleMessageSummary( void* userData, uint64_t time, uint32_t process,
	uint32_t peer, uint32_t comm, uint32_t type, uint64_t sentNumber,
	uint64_t receivedNumber, uint64_t sentBytes, uint64_t receivedBytes,
	OTF_KeyValueList* kvlist ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_MESSAGESUMMARY_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu StatMessage: process %u, "
				"peer %u, group %u, type %u, numsent %llu, numreceived %llu, "
				"bytessent %llu, bytesreceived %llu",
				(long long unsigned) c->num, (long long unsigned) time,
				process, peer, comm, type, (long long unsigned) sentNumber,
				(long long unsigned) receivedNumber, (long long unsigned) sentBytes,
				(long long unsigned) receivedBytes );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


int handleCollopSummary( void* userData, uint64_t time, uint32_t process,
	uint32_t comm, uint32_t collective, uint64_t sentNumber, uint64_t receivedNumber,
	uint64_t sentBytes, uint64_t receivedBytes, OTF_KeyValueList* kvlist) {


	Control* c= (Control*) userData;


	if( c->records[OTF_COLLOPSUMMARY_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu StatCollOp: process %u, group %u, "
				"col %u, numsent %llu, numreceived %llu, "
				"bytessent %llu, bytesreceived %llu",
				(long long unsigned) c->num, (long long unsigned) time,
				process, comm, collective, (long long unsigned) sentNumber,
				(long long unsigned) receivedNumber, (long long unsigned) sentBytes,
				(long long unsigned) receivedBytes );
				
			printKeyValueList(c, kvlist);
		}
	}


	return OTF_RETURN_OK;
}


#define CASE_RETURN(op) \
	case OTF_FILEOP_##op: return #op

static const char* fileOpToString( uint32_t operation ) {

	switch( operation & OTF_FILEOP_BITS ) {
	CASE_RETURN( OPEN );
	CASE_RETURN( CLOSE );
	CASE_RETURN( READ );
	CASE_RETURN( WRITE );
	CASE_RETURN( SEEK );
	CASE_RETURN( UNLINK );
	CASE_RETURN( RENAME );
	CASE_RETURN( DUP );
	CASE_RETURN( SYNC );
	CASE_RETURN( LOCK );
	CASE_RETURN( UNLOCK );

	case OTF_FILEOP_OTHER:
	default: {
		static char unknown_buffer[ 64 ];
		sprintf( unknown_buffer, "UNKNOWN <%u>",
			operation & OTF_FILEOP_BITS );
		return unknown_buffer;
	}

	}
}

#undef CASE_RETURN

#define PRINT_FLAG(flag) \
	if( io_flags & OTF_IOFLAG_##flag ) { \
		fprintf( c->outfile, "%s%s", sep, #flag ); \
		io_flags &= ~OTF_IOFLAG_##flag; \
		sep = ", "; \
	}

static void printIoFlags( Control* c, uint32_t operation ) {


	uint32_t io_flags = operation & OTF_IOFLAGS_BITS;

	if( !io_flags ) {
		fprintf( c->outfile, "%s", "EMPTY" );
	}

	const char* sep = "";
	PRINT_FLAG( IOFAILED )
	PRINT_FLAG( ASYNC )
	PRINT_FLAG( COLL )
	PRINT_FLAG( DIRECT )
	PRINT_FLAG( SYNC )
	PRINT_FLAG( ISREADLOCK )

	if( io_flags ) {
		fprintf( c->outfile, "%sUNKNOWN <%u>", sep, io_flags );
	}
}

#undef PRINT_FLAG

int handleFileOperation( void* userData, uint64_t time,
                 uint32_t fileid, uint32_t process,
                 uint64_t handleid, uint32_t operation,
                 uint64_t bytes, uint64_t duration,
                 uint32_t source, OTF_KeyValueList* kvlist )
{
        Control* c= (Control*) userData;

        if( c->records[OTF_FILEOPERATION_RECORD] ) {

                ++c->num;
                if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {
        
                        fprintf( c->outfile, "(#%llu) \t%llu FileOperation: file ID %llu, "
                                             "process %llu, handle ID %llu, operation %s, flags ",
                                (long long unsigned) c->num, (long long unsigned) time,
                                (long long unsigned) fileid, (long long unsigned) process,
                                (long long unsigned) handleid, fileOpToString( operation ) );
                        printIoFlags( c, operation );
                        fprintf( c->outfile, ", bytes %llu, duration %llu, source %llu",
                                (long long unsigned) bytes, (long long unsigned) duration,
                                (long long unsigned) source);
				
			printKeyValueList(c, kvlist);
                }
        }
        return OTF_RETURN_OK;
}


int handleBeginFileOperation( void* userData, uint64_t time,
		uint32_t process, uint64_t matchingId, uint32_t scltoken,
		OTF_KeyValueList* kvlist )
{
        Control* c = (Control*) userData;

	if( c->records[OTF_BEGINFILEOP_RECORD] ) {

		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu BeginFileOperation: "
					"process %llu, matching ID %llu, "
					"source %llu",
					(long long unsigned) c->num,
					(long long unsigned) time,
					(long long unsigned) process,
					(long long unsigned) matchingId,
					(long long unsigned) scltoken );
					
			printKeyValueList(c, kvlist);
		}
	}
	return OTF_RETURN_OK;
}


int handleEndFileOperation( void* userData, uint64_t time,
		uint32_t process, uint32_t fileid, uint64_t matchingId,
        uint64_t handleId, uint32_t operation, uint64_t bytes,
        uint32_t scltoken, OTF_KeyValueList* kvlist )
{
	Control* c = (Control*) userData;

	if( c->records[OTF_ENDFILEOP_RECORD] ) {

		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu EndFileOperation: "
					"process %llu, file ID %llu, "
					"matching ID %llu, handle ID %llu, "
					"operation %s, flags ",
					(long long unsigned) c->num,
					(long long unsigned) time,
					(long long unsigned) process,
					(long long unsigned) fileid,
					(long long unsigned) matchingId,
					(long long unsigned) handleId,
					fileOpToString( operation ) );
			printIoFlags( c, operation );
			fprintf( c->outfile, ", bytes %llu, source %llu",
					(long long unsigned) bytes,
					(long long unsigned) scltoken );
					
			printKeyValueList(c, kvlist);
		}
	}
	return OTF_RETURN_OK;
}


int handleRMAPut( void* userData, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t source, OTF_KeyValueList* kvlist )
{
        Control* c= (Control*) userData;

        if( c->records[OTF_RMAPUT_RECORD] ) {

                ++c->num;
                if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {
  
                    if( origin != 0 ) {
                        /* transfer initiated from 3rd-party process */
                        fprintf( c->outfile, "(#%llu) \t%llu RMAPut: initiator %u, "
                                             "origin %u, target %u, communicator %u, "
                                             "tag %u, bytes %llu, source %llu",
                                 (long long unsigned) c->num, (long long unsigned) time,
                                 process, origin, target,
                                 communicator, tag, (long long unsigned) bytes,
                                 (long long unsigned) source);
                    }
                    else {
                        fprintf( c->outfile, "(#%llu) \t%llu RMAPut: "
                                             "origin %u, target %u, communicator %u, "
                                             "tag %u, bytes %llu, source %llu",
                                 (long long unsigned) c->num, (long long unsigned) time,
                                 process, target,
                                 communicator, tag, (long long unsigned) bytes,
                                 (long long unsigned) source);
                    }
		    
		    printKeyValueList(c, kvlist);
                }
        }
        return OTF_RETURN_OK;
}


int handleRMAPutRemoteEnd( void* userData, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target,
        uint32_t communicator, uint32_t tag, uint64_t bytes,
	uint32_t source, OTF_KeyValueList* kvlist )
{
        Control* c= (Control*) userData;

        if( c->records[OTF_RMAPUTRE_RECORD] ) {

                ++c->num;
                if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

                    if( origin != 0 ) {
                        /* transfer initiated from 3rd-party process */
                        fprintf( c->outfile, "(#%llu) \t%llu RMAPutRemoteEnd: initiator %u, "
                                             "origin %u, target %u, communicator %u, "
                                             "tag %u, bytes %llu, source %llu",
                             (long long unsigned) c->num, (long long unsigned) time,
                             process, origin, target,
                             communicator, tag, (long long unsigned) bytes,
                             (long long unsigned) source);
                    }
                    else {
                        fprintf( c->outfile, "(#%llu) \t%llu RMAPutRemoteEnd: "
                                             "origin %u, target %u, communicator %u, "
                                             "tag %u, bytes %llu, source %llu",
                             (long long unsigned) c->num, (long long unsigned) time,
                             process, target,
                             communicator, tag, (long long unsigned) bytes,
                             (long long unsigned) source);
                    }
		    
		    printKeyValueList(c, kvlist);
                }
        }
        return OTF_RETURN_OK;
}


int handleRMAGet( void* userData, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t source, OTF_KeyValueList* kvlist)
{
        Control* c= (Control*) userData;

        if( c->records[OTF_RMAGET_RECORD] ) {

                ++c->num;
                if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {
  
                    if( origin != 0 ) {
                        /* transfer initiated from 3rd-party process */
                        fprintf( c->outfile, "(#%llu) \t%llu RMAGet: initiator %u, "
                                             "origin %u, target %u, communicator %u, "
                                             "tag %u, bytes %llu, source %llu",
                                 (long long unsigned) c->num, (long long unsigned) time,
                                 process, origin, target,
                                 communicator, tag, (long long unsigned) bytes,
                                 (long long unsigned) source);
                    }
                    else {
                        fprintf( c->outfile, "(#%llu) \t%llu RMAGet: "
                                             "origin %u, target %u, communicator %u, "
                                             "tag %u, bytes %llu, source %llu",
                                 (long long unsigned) c->num, (long long unsigned) time,
                                 process, target,
                                 communicator, tag, (long long unsigned) bytes,
                                 (long long unsigned) source);
                    }
		    
		    printKeyValueList(c, kvlist);
                }
        }
        return OTF_RETURN_OK;
}


int handleRMAEnd( void* userData, uint64_t time, uint32_t process,
        uint32_t remote, uint32_t communicator, uint32_t tag,
	uint32_t source, OTF_KeyValueList* kvlist )
{
        Control* c= (Control*) userData;

        if( c->records[OTF_RMAEND_RECORD] ) {

                ++c->num;
                if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {
  
                    if( remote != 0 ) {
                        /* transfer initiated from 3rd-party process */
                        fprintf( c->outfile, "(#%llu) \t%llu RMAEnd: initiator %u, "
                                             "remote %u, communicator %u, "
                                             "tag %u, source %llu",
                                 (long long unsigned) c->num, (long long unsigned) time,
                                 process, remote,
                                 communicator, tag,
                                 (long long unsigned) source);
                    }
                    else {
                        fprintf( c->outfile, "(#%llu) \t%llu RMAEnd: "
                                             "process %u, communicator %u, "
                                             "tag %u, source %llu",
                                 (long long unsigned) c->num, (long long unsigned) time,
                                 process, communicator, tag,
                                 (long long unsigned) source);
                    }
		    
		    printKeyValueList(c, kvlist);
                }
        }
        return OTF_RETURN_OK;
}


int handleUnknown( void* userData, uint64_t time, uint32_t process,
	const char* record ) {


	Control* c= (Control*) userData;


	if( c->records[OTF_UNKNOWN_RECORD] ) {
	
		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu Unknown: process %u, record \"%s\"\n",
				(long long unsigned) c->num, (long long unsigned) time,
				process, record );
		}
	}


	return OTF_RETURN_OK;
}


#define CASE_RETURN(name) \
	case OTF_MARKER_TYPE_##name: return #name

static const char* markerTypeToString( uint32_t type ) {

	switch( type ) {
	CASE_RETURN( ERROR );
	CASE_RETURN( WARNING );
	CASE_RETURN( HINT );

	default: {
		static char unknown_buffer[ 64 ];
		sprintf( unknown_buffer, "UNKNOWN <%u>", type );
		return unknown_buffer;
	}

	}
}

#undef CASE_RETURN

int handleDefMarker( void *userData, uint32_t stream, uint32_t token, const char* name,
	uint32_t type, OTF_KeyValueList* kvlist ) {

	Control* c= (Control*) userData;

	if( c->records[OTF_DEFMARKER_RECORD] ) {

		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) DefMarker: ID %u, name \"%s\", type %s",
				(long long unsigned) c->num, token, name,
				markerTypeToString( type ) );
				
			printKeyValueList(c, kvlist);
		}
	}

	return OTF_RETURN_OK;
}


int handleMarker( void *userData, uint64_t time, uint32_t process, 
		uint32_t token, const char* text, OTF_KeyValueList* kvlist ) {

	Control* c= (Control*) userData;

	if( c->records[OTF_MARKER_RECORD] ) {

		++c->num;
		if( c->num >= c->minNum && c->num <= c->maxNum && ! c->silent_mode ) {

			fprintf( c->outfile, "(#%llu) \t%llu Marker: ID %u, process %u, text \"%s\"",
				(long long unsigned) c->num, (long long unsigned) time, token, process, text );
				
			printKeyValueList(c, kvlist);
		}
	}

	return OTF_RETURN_OK;

}

