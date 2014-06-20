#! /usr/bin/python

################################################################################
# 1st parameter = trace to read
################################################################################

from otf import *
import sys


def handleDefComment( fha, stream, comment, kvlist ):
	
	print ( ' handleDefComment: \"%s\", c\"%s\"' % (fha,comment) )

	return OTF_RETURN_OK


def handleDefTimerResolution( fha, stream, ticksPerSecond, kvlist ):
	
	print ( ' handleDefTimerResolution: \"%s\", s%u, s%u' % (fha,stream,ticksPerSecond) )

	return OTF_RETURN_OK


def handleDefProcess( fha, stream, process, name, parent, kvlist ):
	
	print ( ' handleDefProcess: \"%s\", s%u, t%u, nm\"%s\", p%u' \
		% (fha,stream,process,name, parent) )

	return OTF_RETURN_OK


def handleDefProcGroup( fha, stream, group, name, nr, proclist, kvlist ):
	
	output=' handleDefProcGroup: \"%s\", s%u, t%u, nm\"%s\", nr%u { ' \
		% (fha,stream,group,name,nr)
	for proc in proclist:
		output+= '%i, ' % proc
	output += ' }'
	print ( output )

	return OTF_RETURN_OK


def handleDefFunction( fha, stream, func, name, group, source, kvlist ):
	
	print ( ' handleDefFunction: \"%s\", s%u, t%u, nm\"%s\", g%u, src%u' \
		% (fha,stream, func,name,group,source) )

	return OTF_RETURN_OK


def handleDefFunctionGroup( fha, stream, group, name, kvlist ):
	
	print ( ' handleDefFunctionGroup: \"%s\", s%u, t%u, nm\"%s\"' \
		% (fha,stream,group,name) )

	return OTF_RETURN_OK


def handleDefCollectiveOperation( fha, stream, coll, name, typee, kvlist ):
	
	print ( ' handleDefCollectiveOperation: \"%s\", s%u, t%u, nm\"%s\", tp%u' \
		% (fha,stream,coll,name,typee) )

	return OTF_RETURN_OK


def handleDefCounter( fha, stream, counter, name, properties, group, unit, kvlist ):
	
	print ( ' handleDefCounter: \"%s\", s%u, t%u, nm\"%s\", prp%u, g%u, u\"%s\"' \
		% (fha, stream, counter, name, properties, group, unit) )

	return OTF_RETURN_OK


def handleDefCounterGroup( fha, stream, group, name, kvlist ):

	print ( ' handleDefCounterGroup: \"%s\", s%u, t%u, nm\"%s\"' \
		% (fha, stream, group, name) )

	return OTF_RETURN_OK


def handleDefScl( fha, stream, source, file, line, kvlist ):

	print ( ' handleDefScl: \"%s\", s%u, t%u, f%u, l%u' \
		% (fha, stream, source, file, line) )

	return OTF_RETURN_OK


def handleDefSclFile( fha, stream, file, name, kvlist ):

	print ( ' handleDefSclFile: \"%s\", s%u, t%u, nm\"%s\"' % (fha, stream, file, name) )

	return OTF_RETURN_OK


def handleDefCreator( fha, stream, creator, kvlist ):

	print ( ' handleDefCreator: \"%s\", s%u, cr\"%s\"' % (fha, stream, creator) )

	return OTF_RETURN_OK


def handleDefVersion( fha, stream, major, minor, sub, string, kvlist ):

	print ( ' handleDefVersion: \"%s\", s%u, %u.%u.%u \"%s\"' \
		% (fha, stream, major, minor, sub, string) )

	return OTF_RETURN_OK


def handleDefFile( fha, stream, token, name, group, kvlist ):

	print ( ' handleDefFile: \"%s\", s%u, t%u, nm\"%s\", g%u' \
		% (fha, stream, token, name, group) )

	return OTF_RETURN_OK


def handleDefFileGroup( fha, stream, token, name, kvlist ):

	print ( ' handleDefFileGroup: \"%s\", s%u, t%u, nm\"%s\"' \
		% (fha, stream, token, name) )

	return OTF_RETURN_OK


def handleEnter( fha, time, function, process, source, kvlist ):

	print ( ' handleEnter: \"%s\", t%u, f%u, p%u, src%u' \
		% (fha, time, function, process, source) )

	return OTF_RETURN_OK


def handleLeave( fha, time, function, process, source, kvlist ):

	print ( ' handleLeave: \"%s\", t%u, f%u, p%u, src%u' \
		% (fha, time, function, process, source) )

	return OTF_RETURN_OK


def handleSendMsg( fha, time, sender, receiver, group, typee, length,
	source, kvlist ):

	print ( ' handleSendMsg: \"%s\", t%u, s%u, r%u, g%u, tp%u, l%u, src%u' \
		% (fha, time, sender, receiver, group, typee, length, source) )

	return OTF_RETURN_OK


def handleRecvMsg( fha, time, recvProc, sendProc, group, typee, length,
	source, kvlist ):

	print ( ' handleRecvMsg: \"%s\", t%u, r%u, s%u, g%u, tp%u, l%u, src%u' \
		% (fha, time, recvProc, sendProc, group, typee, length, source) )

	return OTF_RETURN_OK


def handleCounter( fha, time, process, counter, value, kvlist ):

	print ( ' handleCounter: \"%s\", t%u, p%u, c%u, v%u' \
		% (fha, time, process, counter, value) )

	return OTF_RETURN_OK


def handleCollectiveOperation( fha, time, process, collective, procGroup,
	rootProc, sent, received, duration, source, kvlist ):

	print ( ' handleCollectiveOperation: \"%s\", t%u, p%u, c%u, pg%u, rt%u, sent%u, recvd%u, dur%u, src%u' \
		% (fha, time, process, collective, procGroup,rootProc, sent, received, duration, source) )

	return OTF_RETURN_OK


def handleEventComment( fha, time, process, comment, kvlist ):

	print ( ' handleEventComment: \"%s\", t%u, p%u, c\"%s\"' \
		% (fha,time, process, comment) )

	return OTF_RETURN_OK


def handleBeginProcess( fha, time, process, kvlist ):

	print ( ' handleBeginProcess: \"%s\", t%u, p%u' % (fha, time, process) )

	return OTF_RETURN_OK


def handleEndProcess( fha, time, process, kvlist ):

	print ( ' handleEndProcess: \"%s\", t%u, p%u' % (fha, time, process) )

	return OTF_RETURN_OK


def handleFileOperation( fha, time, fileid, process, handleid, operation,
	bytes, duration, source, kvlist ):

	print ( ' handleFileOperation: \"%s\", t%u, f%u, p%u, h%u, op%u, b%u, dur%u, src%u' \
		% (fha, time, fileid, process, handleid, operation,bytes, duration, source) )

	return OTF_RETURN_OK


def handleBeginFileOperation( fha, time, process, matchingid, scl, kvlist ):
	
	print ( ' handleBeginFileOp: \"%s\", t%u, p%u, m%u, src%u' \
		% (fha, time, process, matchingid, scl) )
	return OTF_RETURN_OK


def handleEndFileOperation( fha, time, process, fileid, matchingId, handleid, operation, bytes, scl, kvlist ):
	
	print ( ' handleEndFileOp: \"%s\", t%u, p%u, f%u, m%u, h%u, op%u, b%u, src%u' \
		% ( fha, time, process, fileid, matchingId, handleid, operation, bytes, scl ) )
	return OTF_RETURN_OK



def handleSnapshotComment( fha, time, process, comment, kvlist ):

	print ( ' handleSnapshotComment: \"%s\", t%u, p%u, c\"%s\"' \
		% (fha, time, process, comment) )

	return OTF_RETURN_OK


def handleEnterSnapshot( fha, time, originaltime, function, process,
	source, kvlist ):

	print ( ' handleEnterSnapshot: \"%s\", t%u, ot%u, f%u, p%u, src%u' \
		% (fha, time, originaltime, function, process, source) )

	return OTF_RETURN_OK


def handleSendSnapshot( fha, time, originaltime, sender, receiver,
	procGroup, tag, length, source, kvlist ):

	print ( ' handleSendSnapshot: \"%s\", t%u, ot%u, s%u, r%u, pg%u, tag%u, len%u src%u' \
		% (fha, time, originaltime, sender, receiver,procGroup, tag, length, source) )

	return OTF_RETURN_OK


def handleOpenFileSnapshot( fha, time, originaltime, fileid,
	process, handleid, source, kvlist ):

	print ( ' handleOpenFileSnapshot: \"%s\", t%u, ot%u, f%u, p%u, h%u, src%u' \
		% (fha, time, originaltime, fileid,process, handleid, source) )

	return OTF_RETURN_OK


def handleSummaryComment( fha, time, process, comment, kvlist ):

	print ( ' handleSummaryComment: \"%s\", t%u, p%u, c\"%s\"' \
		% (fha, time, process, comment) )

	return OTF_RETURN_OK


def handleFunctionSummary( fha, time, function, process, invocations,
	exclTime, inclTime, kvlist ):

	print ( ' handleFunctionSummary: \"%s\", t%u, f%u, p%u, inv%u, ext%u, int%u' \
		% (fha, time, function, process, invocations, exclTime, inclTime) )

	return OTF_RETURN_OK


def handleFunctionGroupSummary( fha, time, group, process,
	invocations, exclTime, inclTime, kvlist ):

	print ( ' handleFunctionGroupSummary: \"%s\", t%u, g%u, p%u, inv%u, ext%u, int%u' \
		% (fha, time, group, process,invocations, exclTime, inclTime) )

	return OTF_RETURN_OK


def handleMessageSummary( fha, time, process, peer, comm, typee,
	sentNumber, receivedNumber, sentBytes, receivedBytes, kvlist ):

	print ( ' handleMessageSummary: \"%s\", t%u, p%u, peer%u, comm%u, tp%u, sentnr%u, recvdnr%u, sentbt%u, recvdbt%u' \
		% (fha, time, process, peer, comm, typee,sentNumber, receivedNumber, sentBytes, receivedBytes) )

	return OTF_RETURN_OK


def handleFileOperationSummary( fha, time, fileid, process, nopen, nclose,
	nread, nwrite, nseek, bytesread, byteswrite, kvlist ):

	print ( ' handleFileOperationSummary: \"%s\", t%u, f%u, p%u, nopen%u, nclose%u, nrd%u, nwrt%u, nseek%u, btrd%u, btwrt%u' \
		% (fha, time, fileid, process, nopen, nclose, nread, nwrite, nseek, bytesread, byteswrite) )

	return OTF_RETURN_OK


def handleFileGroupOperationSummary( fha, time, groupid, process, nopen,
	nclose, nread, nwrite, nseek, bytesread, byteswrite, kvlist ):

	print ( ' handleFileGroupOperationSummary: \"%s\", t%u, g%u, p%u, nopen%u, nclose%u, nrd%u, nwrt%u, nseek%u, btrd%u, btwrt%u' \
		% (fha, time, groupid, process, nopen, nclose, nread, nwrite, nseek, bytesread, byteswrite) )

	return OTF_RETURN_OK


def handleUnknownRecord( fha, time, process, record, kvlist ):

	print ( ' handleUnknownRecord: \"%s\", t%u, p%u, r\"%s\"' \
		% (fha, time, process, record) )

	return OTF_RETURN_OK


#################################################################################


if __name__ == '__main__':
	
	manager= OTF_FileManager_open( 100 )
	reader= OTF_Reader_open( sys.argv[1], manager )
	handlers= OTF_HandlerArray_open()

	OTF_HandlerArray_setHandler( handlers, handleDefComment, OTF_DEFINITIONCOMMENT_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleDefTimerResolution, OTF_DEFTIMERRESOLUTION_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleDefProcess, OTF_DEFPROCESS_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleDefProcGroup, OTF_DEFPROCESSGROUP_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleDefFunction, OTF_DEFFUNCTION_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleDefFunctionGroup, OTF_DEFFUNCTIONGROUP_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleDefCollectiveOperation, OTF_DEFCOLLOP_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleDefCounter, OTF_DEFCOUNTER_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleDefCounterGroup, OTF_DEFCOUNTERGROUP_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleDefScl, OTF_DEFSCL_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleDefSclFile, OTF_DEFSCLFILE_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleDefCreator, OTF_DEFCREATOR_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleDefVersion, OTF_DEFVERSION_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleDefFile, OTF_DEFFILE_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleDefFileGroup, OTF_DEFFILEGROUP_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleEnter, OTF_ENTER_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleLeave, OTF_LEAVE_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleSendMsg, OTF_SEND_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleRecvMsg, OTF_RECEIVE_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleCounter, OTF_COUNTER_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleCollectiveOperation, OTF_COLLOP_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleEventComment, OTF_EVENTCOMMENT_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleBeginProcess, OTF_BEGINPROCESS_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleEndProcess, OTF_ENDPROCESS_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleFileOperation, OTF_FILEOPERATION_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleBeginFileOperation, OTF_BEGINFILEOP_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleEndFileOperation, OTF_ENDFILEOP_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleSnapshotComment, OTF_SNAPSHOTCOMMENT_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleEnterSnapshot, OTF_ENTERSNAPSHOT_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleSendSnapshot, OTF_SENDSNAPSHOT_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleOpenFileSnapshot, OTF_OPENFILESNAPSHOT_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleSummaryComment, OTF_SUMMARYCOMMENT_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleFunctionSummary, OTF_FUNCTIONSUMMARY_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleFunctionGroupSummary, OTF_FUNCTIONGROUPSUMMARY_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleMessageSummary, OTF_MESSAGESUMMARY_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleFileOperationSummary, OTF_FILEOPERATIONSUMMARY_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleFileGroupOperationSummary, OTF_FILEGROUPOPERATIONSUMMARY_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleUnknownRecord, OTF_UNKNOWN_RECORD )

#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_DEFINITIONCOMMENT_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_DEFTIMERRESOLUTION_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_DEFPROCESS_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_DEFPROCESSGROUP_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_DEFFUNCTION_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_DEFFUNCTIONGROUP_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_DEFCOLLOP_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_DEFCOUNTER_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_DEFCOUNTERGROUP_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_DEFSCL_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_DEFSCLFILE_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_DEFCREATOR_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_DEFVERSION_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_DEFFILE_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_DEFFILEGROUP_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_ENTER_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_LEAVE_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_SEND_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_RECEIVE_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_COUNTER_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_COLLOP_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_EVENTCOMMENT_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_BEGINPROCESS_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_ENDPROCESS_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_FILEOPERATION_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_SNAPSHOTCOMMENT_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_ENTERSNAPSHOT_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_SENDSNAPSHOT_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_OPENFILESNAPSHOT_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_SUMMARYCOMMENT_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_FUNCTIONSUMMARY_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_FUNCTIONGROUPSUMMARY_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_MESSAGESUMMARY_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_FILEOPERATIONSUMMARY_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_FILEGROUPOPERATIONSUMMARY_RECORD )
#	OTF_HandlerArray_setFirstHandlerArg( handlers, "", OTF_UNKNOWN_RECORD )


	OTF_Reader_readDefinitions( reader, handlers )


	# read events in portions
	OTF_Reader_setRecordLimit( reader, 100000 )

	while 1 :

		read= OTF_Reader_readEventsUnsorted( reader, handlers )
		if 0 == read:
			break

		btprg= OTF_Reader_eventBytesProgress( reader )
		tmprg= OTF_Reader_eventTimeProgress( reader )
		print ( " ret_tm%u ret_bt%u INTERRUPT: read %u - timeprogress: %u < %u < %u - bytesprogress: %u < %u < %u" \
			% ( tmprg[0], btprg[0], read, tmprg[1], tmprg[2], tmprg[3], btprg[1], btprg[2], btprg[3] ) )


	OTF_Reader_setRecordLimit( reader, 1000000000 )


	OTF_Reader_readSnapshotsUnsorted( reader, handlers )

	OTF_Reader_readStatisticsUnsorted( reader, handlers )


	OTF_HandlerArray_close( handlers )
	OTF_Reader_close( reader )
	OTF_FileManager_close( manager )
