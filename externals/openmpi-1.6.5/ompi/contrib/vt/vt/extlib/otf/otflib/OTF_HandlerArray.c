/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include "OTF_Platform.h"
#include "OTF_HandlerArray.h"
#include "OTF_Definitions.h"
#include "OTF_CopyHandler.h"
#include "OTF_CopyHandler_stream.h"
#include "OTF_Errno.h"


/** Constructor - internal use only */
int OTF_HandlerArray_init( OTF_HandlerArray* handlers );

/** Destructor - internal use only */
int OTF_HandlerArray_finish( OTF_HandlerArray* handlers );


/*********************************************************************/


int OTF_HandlerArray_init( OTF_HandlerArray* handlers ) {


	uint32_t i;


	for ( i = 0; i < OTF_NRECORDS; ++i )	{

		handlers->pointer[i] = NULL;
		handlers->firsthandlerarg[i] = NULL;
	}

	return 1;
}


int OTF_HandlerArray_finish( OTF_HandlerArray* handlers ) {


	free( handlers->pointer );
	free( handlers->firsthandlerarg );

	handlers->pointer = NULL;
	handlers->firsthandlerarg = NULL;

	return 1;
}


OTF_HandlerArray* OTF_HandlerArray_open() {


	OTF_HandlerArray* ret;

	ret = (OTF_HandlerArray*) malloc( sizeof( OTF_HandlerArray ) );
	if( NULL == ret ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	ret->pointer = (OTF_FunctionPointer**) malloc( 
		OTF_NRECORDS * sizeof( OTF_FunctionPointer* ) );
	if( NULL == ret->pointer ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		free( ret );
		ret= NULL;

		return NULL;
	}

	ret->firsthandlerarg = (void**) malloc( OTF_NRECORDS * sizeof( void* ) );
	if( NULL == ret->firsthandlerarg ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		free( ret->pointer );
		ret->pointer= NULL;
		free( ret );
		ret= NULL;

		return NULL;
	}

	OTF_HandlerArray_init( ret );

	return ret;
}


int OTF_HandlerArray_close( OTF_HandlerArray* handlers ) {


	if( NULL == handlers ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"handlers have not been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

	OTF_HandlerArray_finish( handlers );

	free( handlers );
	handlers = NULL;

	return 1;
}


int OTF_HandlerArray_setHandler( OTF_HandlerArray* handlers, 
		OTF_FunctionPointer* pointer, uint32_t recordtype ) {


	if( recordtype >= OTF_NRECORDS ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"unknown record type %u.\n",
				__FUNCTION__, __FILE__, __LINE__, recordtype );

		return 0;
	}
	handlers->pointer[recordtype] = pointer;

	return 1;
}


int OTF_HandlerArray_setFirstHandlerArg( OTF_HandlerArray* handlers, 
		void* firsthandlerarg, uint32_t recordtype ) {


	if( recordtype >= OTF_NRECORDS ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"unknown record type %u.\n",
				__FUNCTION__, __FILE__, __LINE__, recordtype );

		return 0;
	}
	handlers->firsthandlerarg[recordtype] = firsthandlerarg;

	return 1;
}


int OTF_HandlerArray_getCopyHandler( OTF_HandlerArray* handlers, 
		OTF_Writer* writer ) {


	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_DefinitionComment,
		OTF_DEFINITIONCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_DEFINITIONCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_DefTimerResolution,
		OTF_DEFTIMERRESOLUTION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_DEFTIMERRESOLUTION_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_DefProcess,
		OTF_DEFPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_DEFPROCESS_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_DefProcessGroup,
		OTF_DEFPROCESSGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_DEFPROCESSGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_DefAttributeList,
		OTF_DEFATTRLIST_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer,
		OTF_DEFATTRLIST_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_DefProcessOrGroupAttributes,
		OTF_DEFPROCESSORGROUPATTR_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer,
		OTF_DEFPROCESSORGROUPATTR_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_DefFunction,
		OTF_DEFFUNCTION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_DEFFUNCTION_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_DefFunctionGroup,
		OTF_DEFFUNCTIONGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_DEFFUNCTIONGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_DefCollectiveOperation,
		OTF_DEFCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_DEFCOLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_DefCounter,
		OTF_DEFCOUNTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_DEFCOUNTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_DefCounterGroup,
		OTF_DEFCOUNTERGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_DEFCOUNTERGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_DefScl,
		OTF_DEFSCL_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_DEFSCL_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_DefSclFile,
		OTF_DEFSCLFILE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_DEFSCLFILE_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_DefCreator,
		OTF_DEFCREATOR_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_DEFCREATOR_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_DefFile,
		OTF_DEFFILE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_DEFFILE_RECORD );
	
	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_DefFileGroup,
		OTF_DEFFILEGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_DEFFILEGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_DefKeyValue,
		OTF_DEFKEYVALUE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_DEFKEYVALUE_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_DefTimeRange,
		OTF_DEFTIMERANGE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer,
		OTF_DEFTIMERANGE_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_DefCounterAssignments,
		OTF_DEFCOUNTERASSIGNMENTS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer,
		OTF_DEFCOUNTERASSIGNMENTS_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_DefProcessSubstitutes,
		OTF_DEFPROCESSSUBSTITUTES_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer,
		OTF_DEFPROCESSSUBSTITUTES_RECORD );
	
    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) OTF_CopyHandler_DefAuxSamplePoint,
        OTF_DEFAUXSAMPLEPOINT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, writer,
        OTF_DEFAUXSAMPLEPOINT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_NoOp,
		OTF_NOOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_NOOP_RECORD );
		
	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_Enter,
		OTF_ENTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_ENTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_Leave,
		OTF_LEAVE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_LEAVE_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_SendMsg,
		OTF_SEND_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_SEND_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_RecvMsg,
		OTF_RECEIVE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_RECEIVE_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_Counter,
		OTF_COUNTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_COUNTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_CollectiveOperation,
		OTF_COLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_COLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_BeginCollectiveOperation,
		OTF_BEGINCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_BEGINCOLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_EndCollectiveOperation,
		OTF_ENDCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_ENDCOLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_EventComment,
		OTF_EVENTCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_EVENTCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_BeginProcess,
		OTF_BEGINPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_BEGINPROCESS_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_EndProcess,
		OTF_ENDPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_ENDPROCESS_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_FileOperation,
		OTF_FILEOPERATION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_FILEOPERATION_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_BeginFileOperation,
		OTF_BEGINFILEOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_BEGINFILEOP_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_EndFileOperation,
		OTF_ENDFILEOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_ENDFILEOP_RECORD );

        OTF_HandlerArray_setHandler( handlers, 
                (OTF_FunctionPointer*) OTF_CopyHandler_RMAPut,
                OTF_RMAPUT_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers, writer,
                OTF_RMAPUT_RECORD );

        OTF_HandlerArray_setHandler( handlers, 
                (OTF_FunctionPointer*) OTF_CopyHandler_RMAPutRemoteEnd,
                OTF_RMAPUTRE_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers, writer,
                OTF_RMAPUTRE_RECORD );

        OTF_HandlerArray_setHandler( handlers, 
                (OTF_FunctionPointer*) OTF_CopyHandler_RMAGet,
                OTF_RMAGET_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers, writer,
                OTF_RMAGET_RECORD );

        OTF_HandlerArray_setHandler( handlers, 
                (OTF_FunctionPointer*) OTF_CopyHandler_RMAEnd,
                OTF_RMAEND_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers, writer,
                OTF_RMAEND_RECORD );


	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_SnapshotComment,
		OTF_SNAPSHOTCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_SNAPSHOTCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_EnterSnapshot,
		OTF_ENTERSNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_ENTERSNAPSHOT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_SendSnapshot,
		OTF_SENDSNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_SENDSNAPSHOT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_OpenFileSnapshot,
		OTF_OPENFILESNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_OPENFILESNAPSHOT_RECORD );


	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_BeginCollopSnapshot,
		OTF_BEGINCOLLOPSNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_BEGINCOLLOPSNAPSHOT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_BeginFileOpSnapshot,
		OTF_BEGINFILEOPSNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_BEGINFILEOPSNAPSHOT_RECORD );


    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) OTF_CopyHandler_CollopCountSnapshot,
        OTF_COLLOPCOUNTSNAPSHOT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, writer,
        OTF_COLLOPCOUNTSNAPSHOT_RECORD );


    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) OTF_CopyHandler_CounterSnapshot,
        OTF_COUNTERSNAPSHOT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, writer,
        OTF_COUNTERSNAPSHOT_RECORD );


	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_SummaryComment,
		OTF_SUMMARYCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_SUMMARYCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_FunctionSummary,
		OTF_FUNCTIONSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_FUNCTIONSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_FunctionGroupSummary,
		OTF_FUNCTIONGROUPSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_FUNCTIONGROUPSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_MessageSummary,
		OTF_MESSAGESUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_MESSAGESUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_CollopSummary,
		OTF_COLLOPSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_COLLOPSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_FileOperationSummary,
		OTF_FILEOPERATIONSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_FILEOPERATIONSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_FileGroupOperationSummary,
		OTF_FILEGROUPOPERATIONSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_FILEGROUPOPERATIONSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_DefMarker,
		OTF_DEFMARKER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_DEFMARKER_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_Marker,
		OTF_MARKER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, writer, 
		OTF_MARKER_RECORD );

	return 1;
}

int OTF_HandlerArray_getCopyHandler_stream( OTF_HandlerArray* handlers, 
		OTF_WStream* wstream ) {


	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefinitionComment,
		OTF_DEFINITIONCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFINITIONCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefTimerResolution,
		OTF_DEFTIMERRESOLUTION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFTIMERRESOLUTION_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefProcess,
		OTF_DEFPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFPROCESS_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefProcessGroup,
		OTF_DEFPROCESSGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFPROCESSGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefAttributeList,
		OTF_DEFATTRLIST_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream,
		OTF_DEFATTRLIST_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefProcessOrGroupAttributes,
		OTF_DEFPROCESSORGROUPATTR_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream,
		OTF_DEFPROCESSORGROUPATTR_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefFunction,
		OTF_DEFFUNCTION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFFUNCTION_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefFunctionGroup,
		OTF_DEFFUNCTIONGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFFUNCTIONGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefCollectiveOperation,
		OTF_DEFCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFCOLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefCounter,
		OTF_DEFCOUNTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFCOUNTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefCounterGroup,
		OTF_DEFCOUNTERGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFCOUNTERGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefScl,
		OTF_DEFSCL_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFSCL_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefSclFile,
		OTF_DEFSCLFILE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFSCLFILE_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefUniqueId,
		OTF_DEFUNIQUEID_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFUNIQUEID_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefVersion,
		OTF_DEFVERSION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFVERSION_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefCreator,
		OTF_DEFCREATOR_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFCREATOR_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefFile,
		OTF_DEFFILE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFFILE_RECORD );
	
	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefFileGroup,
		OTF_DEFFILEGROUP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFFILEGROUP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefKeyValue,
		OTF_DEFKEYVALUE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFKEYVALUE_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefTimeRange,
		OTF_DEFTIMERANGE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream,
		OTF_DEFTIMERANGE_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefCounterAssignments,
		OTF_DEFCOUNTERASSIGNMENTS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream,
		OTF_DEFCOUNTERASSIGNMENTS_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefProcessSubstitutes,
		OTF_DEFPROCESSSUBSTITUTES_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream,
		OTF_DEFPROCESSSUBSTITUTES_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) OTF_CopyHandler_stream_DefAuxSamplePoint,
        OTF_DEFAUXSAMPLEPOINT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, wstream,
        OTF_DEFAUXSAMPLEPOINT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_NoOp,
		OTF_NOOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_NOOP_RECORD );
		
	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_Enter,
		OTF_ENTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_ENTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_Leave,
		OTF_LEAVE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_LEAVE_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_SendMsg,
		OTF_SEND_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_SEND_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_RecvMsg,
		OTF_RECEIVE_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_RECEIVE_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_Counter,
		OTF_COUNTER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_COUNTER_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_CollectiveOperation,
		OTF_COLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_COLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_BeginCollectiveOperation,
		OTF_BEGINCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_BEGINCOLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_EndCollectiveOperation,
		OTF_ENDCOLLOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_ENDCOLLOP_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_EventComment,
		OTF_EVENTCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_EVENTCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_BeginProcess,
		OTF_BEGINPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_BEGINPROCESS_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_EndProcess,
		OTF_ENDPROCESS_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_ENDPROCESS_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_FileOperation,
		OTF_FILEOPERATION_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_FILEOPERATION_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_BeginFileOperation,
		OTF_BEGINFILEOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_BEGINFILEOP_RECORD );

	OTF_HandlerArray_setHandler( handlers,
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_EndFileOperation,
		OTF_ENDFILEOP_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_ENDFILEOP_RECORD );

        OTF_HandlerArray_setHandler( handlers, 
                (OTF_FunctionPointer*) OTF_CopyHandler_stream_RMAPut,
                OTF_RMAPUT_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers, wstream,
                OTF_RMAPUT_RECORD );

        OTF_HandlerArray_setHandler( handlers, 
                (OTF_FunctionPointer*) OTF_CopyHandler_stream_RMAPutRemoteEnd,
                OTF_RMAPUTRE_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers, wstream,
                OTF_RMAPUTRE_RECORD );

        OTF_HandlerArray_setHandler( handlers, 
                (OTF_FunctionPointer*) OTF_CopyHandler_stream_RMAGet,
                OTF_RMAGET_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers, wstream,
                OTF_RMAGET_RECORD );

        OTF_HandlerArray_setHandler( handlers, 
                (OTF_FunctionPointer*) OTF_CopyHandler_stream_RMAEnd,
                OTF_RMAEND_RECORD );
        OTF_HandlerArray_setFirstHandlerArg( handlers, wstream,
                OTF_RMAEND_RECORD );


	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_SnapshotComment,
		OTF_SNAPSHOTCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_SNAPSHOTCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_EnterSnapshot,
		OTF_ENTERSNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_ENTERSNAPSHOT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_SendSnapshot,
		OTF_SENDSNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_SENDSNAPSHOT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_OpenFileSnapshot,
		OTF_OPENFILESNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_OPENFILESNAPSHOT_RECORD );


	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_BeginCollopSnapshot,
		OTF_BEGINCOLLOPSNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_BEGINCOLLOPSNAPSHOT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_BeginFileOpSnapshot,
		OTF_BEGINFILEOPSNAPSHOT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_BEGINFILEOPSNAPSHOT_RECORD );

    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) OTF_CopyHandler_stream_CollopCountSnapshot,
        OTF_COLLOPCOUNTSNAPSHOT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, wstream,
        OTF_COLLOPCOUNTSNAPSHOT_RECORD );


    OTF_HandlerArray_setHandler( handlers,
        (OTF_FunctionPointer*) OTF_CopyHandler_stream_CounterSnapshot,
        OTF_COUNTERSNAPSHOT_RECORD );
    OTF_HandlerArray_setFirstHandlerArg( handlers, wstream,
        OTF_COUNTERSNAPSHOT_RECORD );


	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_SummaryComment,
		OTF_SUMMARYCOMMENT_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_SUMMARYCOMMENT_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_FunctionSummary,
		OTF_FUNCTIONSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_FUNCTIONSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_FunctionGroupSummary,
		OTF_FUNCTIONGROUPSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_FUNCTIONGROUPSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_MessageSummary,
		OTF_MESSAGESUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_MESSAGESUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_CollopSummary,
		OTF_COLLOPSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_COLLOPSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_FileOperationSummary,
		OTF_FILEOPERATIONSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_FILEOPERATIONSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_FileGroupOperationSummary,
		OTF_FILEGROUPOPERATIONSUMMARY_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_FILEGROUPOPERATIONSUMMARY_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_DefMarker,
		OTF_DEFMARKER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_DEFMARKER_RECORD );

	OTF_HandlerArray_setHandler( handlers, 
		(OTF_FunctionPointer*) OTF_CopyHandler_stream_Marker,
		OTF_MARKER_RECORD );
	OTF_HandlerArray_setFirstHandlerArg( handlers, wstream, 
		OTF_MARKER_RECORD );

	return 1;
}

