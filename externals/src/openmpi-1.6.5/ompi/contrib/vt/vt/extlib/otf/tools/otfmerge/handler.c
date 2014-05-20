/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Johannes Spazier
*/

#include "handler.h"


void setDefinitionHandlerArray( OTF_HandlerArray* handlers,
         OTF_WStream* wstream) {

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefinitionComment,
        OTF_DEFINITIONCOMMENT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFINITIONCOMMENT_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefTimerResolution,
        OTF_DEFTIMERRESOLUTION_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFTIMERRESOLUTION_RECORD);

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefProcess,
        OTF_DEFPROCESS_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFPROCESS_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefProcessGroup,
        OTF_DEFPROCESSGROUP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFPROCESSGROUP_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefAttributeList,
        OTF_DEFATTRLIST_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFATTRLIST_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefProcessOrGroupAttributes,
        OTF_DEFPROCESSORGROUPATTR_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFPROCESSORGROUPATTR_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefFunction,
        OTF_DEFFUNCTION_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFFUNCTION_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefFunctionGroup,
        OTF_DEFFUNCTIONGROUP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFFUNCTIONGROUP_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefCollectiveOperation,
        OTF_DEFCOLLOP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFCOLLOP_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefCounter,
        OTF_DEFCOUNTER_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFCOUNTER_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefCounterGroup,
        OTF_DEFCOUNTERGROUP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFCOUNTERGROUP_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefScl,
        OTF_DEFSCL_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFSCL_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefSclFile,
        OTF_DEFSCLFILE_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFSCLFILE_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefUniqueId,
        OTF_DEFUNIQUEID_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFUNIQUEID_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefVersion,
        OTF_DEFVERSION_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFVERSION_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefCreator,
        OTF_DEFCREATOR_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFCREATOR_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefFile,
        OTF_DEFFILE_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFFILE_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefFileGroup,
        OTF_DEFFILEGROUP_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFFILEGROUP_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefKeyValue,
        OTF_DEFKEYVALUE_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFKEYVALUE_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefMarker,
        OTF_DEFMARKER_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFMARKER_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleMarker,
        OTF_MARKER_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_MARKER_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefTimeRange,
        OTF_DEFTIMERANGE_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFTIMERANGE_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefCounterAssignments,
        OTF_DEFCOUNTERASSIGNMENTS_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFCOUNTERASSIGNMENTS_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefProcessSubstitutes,
        OTF_DEFPROCESSSUBSTITUTES_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFPROCESSSUBSTITUTES_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) handleDefAuxSamplePoint,
        OTF_DEFAUXSAMPLEPOINT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_DEFAUXSAMPLEPOINT_RECORD );

    OTF_HandlerArray_setHandler( handlers, 
        (OTF_FunctionPointer*) handleUnknownRecord,
        OTF_UNKNOWN_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers,
        (void*) wstream, OTF_UNKNOWN_RECORD );
}

void setEventHandlerArray( OTF_HandlerArray* handlers, OTF_WStream* wstream) {

    /* no special handlers needed for processing events; use copy handlers */
    OTF_HandlerArray_getCopyHandler_stream( handlers, wstream );
}


/* handlers */

int handleDefinitionComment( void *userData, uint32_t stream,
        const char *comment, OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefinitionCommentKV( wstream, comment,
                          list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefTimerResolution( void *userData, uint32_t stream,
        uint64_t ticksPerSecond, OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefTimerResolutionKV( wstream,
                          ticksPerSecond,
                          list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefProcess( void *userData, uint32_t stream, uint32_t process,
        const char *name, uint32_t parent, OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefProcessKV( wstream, process, name,
                          parent, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefProcessGroup( void *userData, uint32_t stream, uint32_t procGroup,
        const char *name, uint32_t numberOfProcs, const uint32_t *procs,
        OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefProcessGroupKV( wstream, procGroup,
                          name, numberOfProcs, procs,
                          list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefAttributeList( void *userData, uint32_t stream,
        uint32_t attr_token, uint32_t num, OTF_ATTR_TYPE *array,
        OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefAttributeListKV( wstream, attr_token,
                          num, array,
                          list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefProcessOrGroupAttributes( void *userData, uint32_t stream,
        uint32_t proc_token, uint32_t attr_token, OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefProcessOrGroupAttributesKV( wstream,
                          proc_token, attr_token,
                          list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefFunction( void *userData, uint32_t stream, uint32_t func,
        const char *name, uint32_t funcGroup, uint32_t source,
        OTF_KeyValueList *list) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefFunctionKV( wstream, func, name,
                          funcGroup, source,
                          list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefFunctionGroup( void *userData, uint32_t stream, uint32_t funcGroup,
        const char *name, OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefFunctionGroupKV( wstream, funcGroup,
                          name, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefCollectiveOperation( void *userData, uint32_t stream,
        uint32_t collOp, const char *name, uint32_t type,
        OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefCollectiveOperationKV( wstream,
                          collOp, name, type,
                          list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefCounter( void *userData, uint32_t stream, uint32_t counter,
        const char *name, uint32_t properties, uint32_t counterGroup,
        const char *unit, OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefCounterKV( wstream, counter, name,
                          properties, counterGroup, unit,
                          list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}

/* *** Event handler *** ****************************************** */

int handleDefCounterGroup( void *userData, uint32_t stream,
        uint32_t counterGroup, const char *name, OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefCounterGroupKV( wstream, counterGroup,
                          name, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefScl( void *userData, uint32_t stream, uint32_t source,
        uint32_t sourceFile, uint32_t line, OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefSclKV( wstream, source, sourceFile,
                          line, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefSclFile( void *userData, uint32_t stream, uint32_t sourceFile,
        const char *name, OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefSclFileKV( wstream, sourceFile,
                          name, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefCreator( void *userData, uint32_t stream, const char *creator,
        OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefCreatorKV( wstream, creator,
                          list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefUniqueId( void *userData, uint32_t stream, uint64_t uid ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeUniqueId( 
                          wstream ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefVersion( void *userData, uint32_t stream, uint8_t major,
        uint8_t minor, uint8_t sub, const char *string ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeOtfVersion( 
                          wstream ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefFile( void *userData, uint32_t stream, uint32_t token,
        const char *name, uint32_t group, OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefFileKV( wstream, token, name,
                          group, list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefFileGroup( void *userData, uint32_t stream, uint32_t token,
        const char *name, OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefFileGroupKV( wstream, token, name,
                          list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefKeyValue( void *userData, uint32_t stream, uint32_t token,
        OTF_Type type, const char *name, const char *desc,
        OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local definitions. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefKeyValueKV( wstream, token, type,
                          name, desc,
                          list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefTimeRange( void* userData, uint32_t stream, uint64_t minTime,
        uint64_t maxTime, OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

         fprintf( stderr,
                  "Error: cannot merge traces with local definitions. "
                  "Aborting.\n" );

         return OTF_RETURN_ABORT;

    } else {

         return ( 0 == OTF_WStream_writeDefTimeRange( wstream, minTime,
                           maxTime, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    }
}


int handleDefCounterAssignments( void* userData, uint32_t stream,
        uint32_t counter_token, uint32_t number_of_members,
        const uint32_t* procs_or_groups, OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

         fprintf( stderr,
                  "Error: cannot merge traces with local definitions. "
                  "Aborting.\n" );

         return OTF_RETURN_ABORT;

    } else {

         return ( 0 == OTF_WStream_writeDefCounterAssignments( wstream,
                           counter_token, number_of_members, procs_or_groups,
                           list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
    }
}


int handleDefProcessSubstitutes( void* userData, uint32_t stream,
        uint32_t representative, uint32_t numberOfProcs, const uint32_t* procs,
        OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != stream ) {

         fprintf( stderr,
                  "Error: cannot merge traces with local definitions. "
                  "Aborting.\n" );

         return OTF_RETURN_ABORT;

    } else {

         return ( 0 == OTF_WStream_writeDefProcessSubstitutes( wstream,
                           representative, numberOfProcs, procs,
                           list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
    }
}


int handleDefMarker( void *userData, uint32_t stream, uint32_t token,
        const char *name, uint32_t type, OTF_KeyValueList *list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != wstream->id ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local markers. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefMarkerKV( wstream, token, name, type,
                          list) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
    }
}


int handleMarker( void *userData, uint64_t time, uint32_t process,
        uint32_t token, const char* text, OTF_KeyValueList* list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != wstream->id ) {

        fprintf( stderr,
                 "Error: cannot merge traces with local markers. "
                 "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeMarkerKV( wstream, time, process, token,
                          text, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
    }
}


int handleDefAuxSamplePoint( void*                  userData,
                             uint32_t               streamid,
                             uint64_t               time,
                             OTF_AuxSamplePointType type,
                             OTF_KeyValueList*      list ) {

    OTF_WStream* wstream = (OTF_WStream*) userData;

    if( 0 != streamid ) {

         fprintf( stderr,
                  "Error: cannot merge traces with local definitions. "
                  "Aborting.\n" );

        return OTF_RETURN_ABORT;

    } else {

        return ( 0 == OTF_WStream_writeDefAuxSamplePoint( wstream,
                time,
                type,
                list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
    }
}


int handleUnknownRecord( void *userData, uint64_t time, uint32_t process,
        const char *record) {

    fprintf( stderr, "Error: unknown record >%s< at process 0x%x. Aborting.\n",
             record, process );

    return OTF_RETURN_ABORT;
}
