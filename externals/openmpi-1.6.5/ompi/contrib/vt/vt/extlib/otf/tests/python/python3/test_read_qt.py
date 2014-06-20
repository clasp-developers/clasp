#! /usr/bin/python

# This script uses Python bindings for Qt4 (python-qt4).
# After installation you maybe have to add the module-path (e.g. /usr/lib/python2.5/site-packages)
# to the environment variable $PYTHONPATH

##################################################################################################
# 1st parameter = trace to read
##################################################################################################

import otf
import sys                                           
from PyQt4.QtGui import *
from PyQt4.QtCore import *

liste = [0 for j in range(47)]
names = ["DefinitionComment", "DefTimerResolution", "DefProcess", "DefProcessGroup", "DefFunction", "DefFunctionGroup", "DefCollectiveOperation", "DefCounter", "DefCounterGroup", "DefScl", "DefSclFile", "DefCreator", "DefVersion", "DefFile", "DefFileGroup", "Enter", "Leave", "SendMsg", "RecvMsg", "Counter", "CollectiveOperation", "BeginCollectiveOperation", "EndCollectiveOperation", "EventComment", "BeginProcess", "EndProcess", "FileOperation", "BeginFileOperation", "EndFileOperation", "SnapshotComment", "EnterSnapshot", "SendSnapshot", "OpenFileSnapshot", "SummaryComment", "FunctionSummary", "FunctionGroupSummary", "MessageSummary", "CollopSummary", "FileOperationSummary", "FileGroupOperationSummary", "UnknownRecord", "DefMarker", "Marker", "RMAPut", "RMAPutRemoteEnd", "RMAGet", "RMAEnd"]

#### Definitions ####
def handleDefFunction(userData, stream, func, name, funcGroup, source, kvlist ):
	liste[4] = liste[4] + 1
	return otf.OTF_RETURN_OK

def handleDefFunctionGroup(userData, stream, funcGroup, name, kvlist ):
	liste[5] = liste[5] + 1
	return otf.OTF_RETURN_OK

def handleDefinitionComment(userData, stream, comment, kvlist ):
	liste[0] = liste[0] + 1
	return otf.OTF_RETURN_OK

def handleTimerResolution(userData, stream, ticksPerSecond, kvlist ):
	liste[1] = liste[1] + 1
	return otf.OTF_RETURN_OK

def handleDefProcess(userData, stream, process, name, parent, kvlist ):
	liste[2] = liste[2] + 1;
	return otf.OTF_RETURN_OK

def handleDefProcessGroup(userData, stream, procGroup, name, numberOfProcs, procs, kvlist ):
	liste[3] = liste[3] + 1
	return otf.OTF_RETURN_OK

def handleDefCollectiveOperation (userData, stream, collOp, name, typ, kvlist ):
	liste[6] = liste[6] + 1
	return otf.OTF_RETURN_OK

def handleDefCounter (userData, stream, counter, name, properties, counterGroup, unit, kvlist ):
	liste[7] = liste[7] + 1
	return otf.OTF_RETURN_OK

def handleDefCounterGroup(userData, stream, counterGroup, name, kvlist ):
	liste[8] = liste[8] + 1
	return otf.OTF_RETURN_OK

def handleDefScl(userData, stream, source, sourceFile, line, kvlist ):
	liste[9] = liste[9] + 1
	return otf.OTF_RETURN_OK

def handleDefSclFile (userData, stream, sourceFile, name, kvlist ):
	liste[10] = liste[10] + 1;
	return otf.OTF_RETURN_OK

def handleDefCreator (userData, stream, creator, kvlist ):
	liste[11] = liste[11] + 1
	return otf.OTF_RETURN_OK

def handleDefVersion (userData,stream, major, minor, sub, string, kvlist ):
	liste[12] = liste[12] + 1
	return otf.OTF_RETURN_OK

def handleDefFile (userData, stream, token, name, group, kvlist ):
	liste[13] = liste[13] + 1
	return otf.OTF_RETURN_OK

def handleDefFileGroup (userData, stream, token, name, kvlist ):
	liste[14] = liste[14] + 1
	return otf.OTF_RETURN_OK

#### Events ####
def handleEnter(userData, time, function, process, source, kvlist ):
	liste[15] = liste[15] + 1
	return otf.OTF_RETURN_OK

def handleLeave(userData, time, function, process, source, kvlist ):
	liste[16] = liste[16] + 1
	return otf.OTF_RETURN_OK

def handleSendMsg (userData, time, sender, receiver, group, typ, length, source, kvlist ):
	liste[17] = liste[17] + 1
	return otf.OTF_RETURN_OK

def handleRecvMsg (userData, time, recvProc, sendProc, group, typ, length, source, kvlist ):
	liste[18] = liste[18] + 1
	return otf.OTF_RETURN_OK

def handleCounter (userData, time, process, counter, value, kvlist ):
	liste[19] = liste[19] + 1
	return otf.OTF_RETURN_OK

def handleCollectiveOperation (userData, time, process, collective, procGroup, rootProc, sent, received, duration, source, kvlist ):
	liste[20] = liste[20] + 1;
	return otf.OTF_RETURN_OK

def handleBeginCollectiveOperation (uData, time, process, collOp, matchingId, procGroup, rootProc, sent, received, scltoken, kvlist ):
	liste[21] = liste[21] + 1
	return otf.OTF_RETURN_OK

def handleEndCollectiveOperation (uData, time, process, matchingId, kvlist ): 
	liste[22] = liste[22] + 1
	return otf.OTF_RETURN_OK

def handleEventComment (userData, time, process, comment, kvlist ):
	liste[23] = liste[23] + 1
	return otf.OTF_RETURN_OK

def handleBeginProcess (userData, time, process, kvlist ):
	liste[24] = liste[24] + 1
	return otf.OTF_RETURN_OK

def handleEndProcess (userData, time, process, kvlist ):
	liste[25] = liste[25] + 1
	return otf.OTF_RETURN_OK

def handleFileOperation (userData, time, fileid, process, handleid, operation, bytes, duration, source, kvlist ):
	liste[26] = liste[26] + 1
	return otf.OTF_RETURN_OK

def handleBeginFileOperation (writer, time, process, handleid, scltoken, kvlist ):
	liste[27] = liste[27] + 1
	return otf.OTF_RETURN_OK

def handleEndFileOperation (writer, time, process, fileid, matchingId, handleid, operation, bytes, scltoken, kvlist ):
	liste[28] = liste[28] + 1
	return otf.OTF_RETURN_OK

#### Snapshots ####
def handleSnapshotComment (userData, time, process, comment, kvlist ):
	liste[29] = liste[29] + 1
	return otf.OTF_RETURN_OK

def handleEnterSnapshot (userData, time, originaltime, function, process, source, kvlist ):
	liste[30] = liste[30] + 1
	return otf.OTF_RETURN_OK

def handleSendSnapshot (userData, time, originaltime, sender, receiver, procGroup, tag, length, source, kvlist ):
	liste[31] = liste[31] + 1
	return otf.OTF_RETURN_OK

def handleOpenFileSnapshot (userData, time, originaltime, fileid, process, handleid, source, kvlist ):
	liste[32] = liste[32] + 1
	return otf.OTF_RETURN_OK

#### Summary ####
def handleSummaryComment (userData, time, process, comment, kvlist ):
	liste[33] = liste[33] + 1
	return otf.OTF_RETURN_OK

def handleFunctionSummary (userData, time, function, process, invocations, exclTime, inclTime, kvlist ):
	liste[34] = liste[34] + 1
	return otf.OTF_RETURN_OK

def handleFunctionGroupSummary (userData, time, funcGroup, process, invocations, exclTime, inclTime, kvlist ):
	liste[35] = liste[35] + 1
	return otf.OTF_RETURN_OK

def handleMessageSummary (userData, time, process, peer, comm, typ, sentNumber, receivedNumber, sentBytes, receivedBytes, kvlist ):
	liste[36] = liste[36] + 1
	return otf.OTF_RETURN_OK

def handleCollopSummary (userData, time, process, comm, collective, sentNumber, receivedNumber, sentBytes, receivedBytes, kvlist ):
	liste[37] = liste[37] + 1
	return otf.OTF_RETURN_OK

def handleFileOperationSummary (userData, time, fileid, process, nopen, nclose, nread, nwrite, nseek, bytesread, byteswrite, kvlist ):
	liste[38] = liste[38] + 1;
	return otf.OTF_RETURN_OK

def handleFileGroupOperationSummary (uD, time, groupid, process, nopen, nclose, nread, nwrite, nseek, bytesread, byteswrite, kvlist ):
	liste[39] = liste[39] + 1
	return otf.OTF_RETURN_OK


def handleUnknownRecord (userData, time, process, record, kvlist ):
	liste[40] = liste[40] + 1
	return otf.OTF_RETURN_OK

#### Marker ####
def handleDefMarker (userData, stream, token, name, typ, kvlist ):
	liste[41] = liste[41] + 1
	return otf.OTF_RETURN_OK

def handleMarker (userData, time, process, token, text, kvlist ):
	liste[42] = liste[42] + 1
	return otf.OTF_RETURN_OK

def handleRMAPut (userData, time, process, origin, target, communicator, tag, bytes, source, kvlist ):
	liste[43] = liste[43] + 1
	return otf.OTF_RETURN_OK

def handleRMAPutRemoteEnd (userData, time, process, origin, target, communicator, tag, bytes, source, kvlist ):
	liste[44] = liste[44] + 1
	return otf.OTF_RETURN_OK

def handleRMAGet (userData, time, process, origin, target, communicator, tag, bytes, source, kvlist ):
	liste[45] = liste[45] + 1
	return otf.OTF_RETURN_OK

def handleRMAEnd (userData, time, process, remote, communicator, tag, source, kvlist ):
	liste[46] = liste[46] + 1
	return otf.OTF_RETURN_OK

if len(sys.argv) <= 1:
	print ( "No tracefile given. Abort." )
	exit()

print ( "Tracefile: ", sys.argv[1] )

manager = otf.OTF_FileManager_open(100)
handlers = otf.OTF_HandlerArray_open()
reader = otf.OTF_Reader_open(sys.argv[1], manager)

#### DEFINITIONS ####
otf.OTF_HandlerArray_setHandler( handlers, handleDefFunction, otf.OTF_DEFFUNCTION_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleDefFunctionGroup, otf.OTF_DEFFUNCTIONGROUP_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleDefinitionComment, otf.OTF_DEFINITIONCOMMENT_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleTimerResolution, otf.OTF_DEFTIMERRESOLUTION_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleDefProcess, otf.OTF_DEFPROCESS_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleDefProcessGroup, otf.OTF_DEFPROCESSGROUP_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleDefCollectiveOperation, otf.OTF_DEFCOLLOP_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleDefCounter, otf.OTF_DEFCOUNTER_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleDefCounterGroup, otf.OTF_DEFCOUNTERGROUP_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleDefScl, otf.OTF_DEFSCL_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleDefSclFile, otf.OTF_DEFSCLFILE_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleDefCreator, otf.OTF_DEFCREATOR_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleDefVersion, otf.OTF_DEFVERSION_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleDefFile, otf.OTF_DEFFILE_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleDefFileGroup, otf.OTF_DEFFILEGROUP_RECORD )

#### EVENTS ####
otf.OTF_HandlerArray_setHandler( handlers, handleEnter, otf.OTF_ENTER_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleLeave, otf.OTF_LEAVE_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleSendMsg, otf.OTF_SEND_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleRecvMsg, otf.OTF_RECEIVE_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleCounter, otf.OTF_COUNTER_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleCollectiveOperation, otf.OTF_COLLOPSUMMARY_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleBeginCollectiveOperation, otf.OTF_BEGINCOLLOP_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleEndCollectiveOperation, otf.OTF_ENDCOLLOP_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleEventComment, otf.OTF_EVENTCOMMENT_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleBeginProcess, otf.OTF_BEGINPROCESS_RECORD)
otf.OTF_HandlerArray_setHandler( handlers, handleEndProcess, otf.OTF_ENDPROCESS_RECORD)
otf.OTF_HandlerArray_setHandler( handlers, handleFileOperation, otf.OTF_FILEOPERATION_RECORD)
otf.OTF_HandlerArray_setHandler( handlers, handleBeginFileOperation, otf.OTF_BEGINFILEOP_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleEndFileOperation, otf.OTF_ENDFILEOP_RECORD )

#### SNAPSHOTS ####
otf.OTF_HandlerArray_setHandler( handlers, handleSnapshotComment, otf.OTF_SNAPSHOTCOMMENT_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleEnterSnapshot, otf.OTF_ENTERSNAPSHOT_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleSendSnapshot, otf.OTF_SENDSNAPSHOT_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleOpenFileSnapshot, otf.OTF_OPENFILESNAPSHOT_RECORD )

#### SUMMARY ####
otf.OTF_HandlerArray_setHandler( handlers, handleSummaryComment, otf.OTF_SUMMARYCOMMENT_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleFunctionSummary, otf.OTF_FUNCTIONSUMMARY_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleFunctionGroupSummary, otf.OTF_FUNCTIONGROUPSUMMARY_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleMessageSummary, otf.OTF_MESSAGESUMMARY_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleCollopSummary, otf.OTF_COLLOPSUMMARY_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleFileOperationSummary, otf.OTF_FILEOPERATIONSUMMARY_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleFileGroupOperationSummary, otf.OTF_FILEGROUPOPERATIONSUMMARY_RECORD )

#### UNKNOWN ####
otf.OTF_HandlerArray_setHandler( handlers, handleUnknownRecord, otf.OTF_UNKNOWN_RECORD )

#### MARKER ####
otf.OTF_HandlerArray_setHandler( handlers, handleDefMarker, otf.OTF_DEFMARKER_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleMarker, otf.OTF_MARKER_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleRMAPut, otf.OTF_RMAPUT_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleRMAPutRemoteEnd, otf.OTF_RMAPUTRE_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleRMAGet, otf.OTF_RMAGET_RECORD )
otf.OTF_HandlerArray_setHandler( handlers, handleRMAEnd, otf.OTF_RMAEND_RECORD )

otf.OTF_Reader_readDefinitions(reader, handlers)
otf.OTF_Reader_readEvents(reader, handlers)
otf.OTF_Reader_readSnapshots(reader, handlers)
otf.OTF_Reader_readStatistics(reader, handlers)
otf.OTF_Reader_readMarkers(reader, handlers)

otf.OTF_Reader_close(reader)
otf.OTF_HandlerArray_close(handlers)
otf.OTF_FileManager_close(manager)

app = QApplication(sys.argv)

window = QWidget()
window.setWindowTitle("OTF-Reader in Python")
window.setGeometry(400,300,400,400)
window.setFixedSize(400,400)

tabelle = QTableWidget(window)
tabelle.setGeometry(5,5,390,390)

tabelle.setRowCount(0)
tabelle.setColumnCount(2)

tabelle.setHorizontalHeaderItem(0,QTableWidgetItem("Records"))
tabelle.setHorizontalHeaderItem(1,QTableWidgetItem("Calls"))

tabelle.setColumnWidth(0,245)
tabelle.horizontalHeader().setResizeMode(QHeaderView.Fixed);
tabelle.verticalHeader().setResizeMode(QHeaderView.Fixed)

n = 0
k = 0
for i in liste:
    if i > 0:
	    tabelle.setRowCount(tabelle.rowCount() + 1)
	    tabelle.setItem(k,0,QTableWidgetItem(names[n]))
	    tabelle.setItem(k,1,QTableWidgetItem(str(i)))
	    k = k + 1
    n = n + 1
      
window.show()
app.exec_()
