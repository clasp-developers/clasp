/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/**
 *  @file OTF_HandlerArray.h
 *
 *  @brief Provides read access to OTF traces which consist of multiple
 *  streams.
 *
 *  \ingroup handler
 *  \ingroup ha
 */

/** \defgroup handler Handler Interface
 *
 * In the following, the handler interfaces for all record types are
 * specified. The signature of callback handler functions is equal to the
 * signature of corresponding record write functions except for the first
 * argument. The first argument common to all callback handler functions is
 * \em userData -- a generic pointer to custom user data. The second common
 * argument to all callback hander functions is \em stream which identifies the
 * stream where the definition occurred. A stream parameter = 0 indicates a
 * global definition which is the default.
 */

/**
 * \defgroup ha Handler Array Interface
 *
 * Using this interface you can setup a handler array for reading traces.
 *
 */

#ifndef OTF_HANDLERARRAY_H
#define OTF_HANDLERARRAY_H


#include "OTF_Writer.h"
#include "OTF_WStream.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/** Generic function pointer for OTF record handlers. \ingroup ha*/
typedef int (OTF_FunctionPointer) ( void *userData );


/** Object structure which holds OTF record handlers. */
struct struct_OTF_HandlerArray {

        /** Array of function pointer. */
        OTF_FunctionPointer** pointer;

        /** Array of first handler arguments. */
        void** firsthandlerarg;
};

/** Object type which holds OTF record handlers. \ingroup ha */
typedef struct struct_OTF_HandlerArray OTF_HandlerArray;


/** Open a new array of handlers. \ingroup ha */
OTF_HandlerArray* OTF_HandlerArray_open( void );

/** Close and delete a OTF_HandlerArray object. \ingroup ha */
int OTF_HandlerArray_close( OTF_HandlerArray* handlers );

/** Assign the function pointer to your own handler of the given record
    type. \ingroup ha */
int OTF_HandlerArray_setHandler( OTF_HandlerArray* handlers,
        OTF_FunctionPointer* pointer, uint32_t recordtype );

/** Assign the first argument to your own handler of the given record type.
\ingroup ha */
int OTF_HandlerArray_setFirstHandlerArg( OTF_HandlerArray* handlers,
        void* firsthandlerarg, uint32_t recordtype );

/** Provide copy handlers to every record type. \ingroup ha */
int OTF_HandlerArray_getCopyHandler( OTF_HandlerArray* handlers,
        OTF_Writer* writer );

/** Provide copy handlers to every record type for OTF streams. \ingroup ha */
int OTF_HandlerArray_getCopyHandler_stream( OTF_HandlerArray* handlers,
        OTF_WStream* writer );


/* typdefs for OTF definition records *************************** */

/** @cond typedef */

/** the following lines are ignored by doxygen */

/* - the following part is also used to create the python wrapper automatically
   - respect these rules to avoid problems while generating:
        - write the definition macro as a comment (with a '#' placed in front)
          one line above the typedef itself
        - the use of the following datatypes is possible: char, uint8_t, uint32_t, uint64_t
        - the first parameter must be a "void*" pointer
        - to use pointer as parameter is very critical and requires some additional rules:
                - pointer of type "char*" are allowed anytime
                - pointer of type "uint32_t*" are allowed but the previous parameter must
                  be of type "uint32_t" and contain the number of elements that are in the array
                  the pointer references to
                - pointer of type "void*" are allowed as the first parameter only

    - if you have to add a record that breaks one of the above-mentioned rules the generation
      will skip this record and you manually have to update the python wrapper ( see
      otflib_py/README_python for further information )
    - check and test the new record with the python interface to avoid bugs caused by the
      automatic generation

    do not remove the following line, because it is necessary to generate the python wrapper
    #START_GEN_PYWRAPPER
*/

/* # OTF_DEFINITIONCOMMENT_RECORD */
typedef int (OTF_Handler_DefinitionComment) ( void* userData,
                                              uint32_t stream,
                                              const char* comment,
                                              OTF_KeyValueList *list );

/* # OTF_DEFTIMERRESOLUTION_RECORD */
typedef int (OTF_Handler_DefTimerResolution) ( void* userData,
                                               uint32_t stream,
                                               uint64_t ticksPerSecond,
                                               OTF_KeyValueList *list );

/* # OTF_DEFPROCESS_RECORD */
typedef int (OTF_Handler_DefProcess) ( void* userData,
                                       uint32_t stream,
                                       uint32_t process,
                                       const char* name,
                                       uint32_t parent,
                                       OTF_KeyValueList *list );

/* # OTF_DEFPROCESSGROUP_RECORD */
typedef int (OTF_Handler_DefProcessGroup) ( void* userData,
                                            uint32_t stream,
                                            uint32_t procGroup,
                                            const char* name,
                                            uint32_t numberOfProcs,
                                            const uint32_t* procs,
                                            OTF_KeyValueList *list );

/* # OTF_DEFATTRLIST_RECORD */
typedef int (OTF_Handler_DefAttributeList) ( void* userData,
                                             uint32_t stream,
                                             uint32_t attr_token,
                                             uint32_t num,
                                             OTF_ATTR_TYPE* array,
                                             OTF_KeyValueList *list );

/* # OTF_DEFPROCESSORGROUPATTR_RECORD */
typedef int (OTF_Handler_DefProcessOrGroupAttributes) ( void* userData,
                                                        uint32_t stream,
                                                        uint32_t proc_token,
                                                        uint32_t attr_token,
                                                        OTF_KeyValueList *list );

/* # OTF_DEFFUNCTION_RECORD */
typedef int (OTF_Handler_DefFunction) ( void* userData,
                                        uint32_t stream,
                                        uint32_t func,
                                        const char* name,
                                        uint32_t funcGroup,
                                        uint32_t source,
                                        OTF_KeyValueList *list );

/* # OTF_DEFFUNCTIONGROUP_RECORD */
typedef int (OTF_Handler_DefFunctionGroup) ( void* userData,
                                             uint32_t stream,
                                             uint32_t funcGroup,
                                             const char* name,
                                             OTF_KeyValueList *list );

/* # OTF_DEFCOLLOP_RECORD */
typedef int (OTF_Handler_DefCollectiveOperation) ( void* userData,
                                                   uint32_t stream,
                                                   uint32_t collOp,
                                                   const char* name,
                                                   uint32_t type,
                                                   OTF_KeyValueList *list );

/* # OTF_DEFCOUNTER_RECORD */
typedef int (OTF_Handler_DefCounter) ( void* userData,
                                       uint32_t stream,
                                       uint32_t counter,
                                       const char* name,
                                       uint32_t properties,
                                       uint32_t counterGroup,
                                       const char* unit,
                                       OTF_KeyValueList *list );

/* # OTF_DEFCOUNTERGROUP_RECORD */
typedef int (OTF_Handler_DefCounterGroup) ( void* userData,
                                            uint32_t stream,
                                            uint32_t counterGroup,
                                            const char* name,
                                            OTF_KeyValueList *list );

/* # OTF_DEFSCL_RECORD */
typedef int (OTF_Handler_DefScl) ( void* userData,
                                   uint32_t stream,
                                   uint32_t source,
                                   uint32_t sourceFile,
                                   uint32_t line,
                                   OTF_KeyValueList *list );

/* # OTF_DEFSCLFILE_RECORD */
typedef int (OTF_Handler_DefSclFile) ( void* userData,
                                       uint32_t stream,
                                       uint32_t sourceFile,
                                       const char* name,
                                       OTF_KeyValueList *list );

/* # OTF_DEFCREATOR_RECORD */
typedef int (OTF_Handler_DefCreator) ( void* userData,
                                       uint32_t stream,
                                       const char* creator,
                                       OTF_KeyValueList *list );

/* # OTF_DEFUNIQUEID_RECORD */
typedef int (OTF_Handler_DefUniqueId) ( void* userData,
                                        uint32_t stream,
                                        uint64_t uid,
                                        OTF_KeyValueList *list );

/* # OTF_DEFVERSION_RECORD */
typedef int (OTF_Handler_DefVersion) ( void* userData,
                                       uint32_t stream,
                                       uint8_t major,
                                       uint8_t minor,
                                       uint8_t sub,
                                       const char* string,
                                       OTF_KeyValueList *list );

/* # OTF_DEFFILE_RECORD */
typedef int (OTF_Handler_DefFile) ( void* userData,
                                    uint32_t stream,
                                    uint32_t token,
                                    const char *name,
                                    uint32_t group,
                                    OTF_KeyValueList *list );

/* # OTF_DEFFILEGROUP_RECORD */
typedef int (OTF_Handler_DefFileGroup) ( void* userData,
                                         uint32_t stream,
                                         uint32_t token,
                                         const char *name,
                                         OTF_KeyValueList *list );

/* # OTF_DEFKEYVALUE_RECORD */
typedef int (OTF_Handler_DefKeyValue) (  void* userData,
                                         uint32_t stream,
                                         uint32_t key,
                                         OTF_Type type,
                                         const char *name,
                                         const char *description,
                                         OTF_KeyValueList *list );

/* # OTF_DEFTIMERANGE_RECORD */
typedef int (OTF_Handler_DefTimeRange) ( void* userData,
                                         uint32_t stream,
                                         uint64_t minTime,
                                         uint64_t maxTime,
                                         OTF_KeyValueList* list );

/* # OTF_DEFCOUNTERASSIGNMENTS_RECORD */
typedef int (OTF_Handler_DefCounterAssignments) ( void* userData,
                                                  uint32_t stream,
                                                  uint32_t counter,
                                                  uint32_t number_of_members,
                                                  const uint32_t* procs_or_groups,
                                                  OTF_KeyValueList* list );

/* # OTF_DEFPROCESSSUBSTITUTES_RECORD */
typedef int (OTF_Handler_DefProcessSubstitutes) ( void* userData,
                                                  uint32_t stream,
                                                  uint32_t representative,
                                                  uint32_t numberOfProcs,
                                                  const uint32_t* procs,
                                                  OTF_KeyValueList* list );

/* # OTF_DEFAUXSAMPLEPOINT_RECORD */
typedef int (OTF_Handler_DefAuxSamplePoint) ( void* userData,
                                              uint32_t stream,
                                              uint64_t time,
                                              OTF_AuxSamplePointType type,
                                              OTF_KeyValueList* list );

/* typedefs for OTF event records ****************************************** */

/* # OTF_NOOP_RECORD */
typedef int (OTF_Handler_NoOp) ( void* userData,
                                 uint64_t time,
                                 uint32_t process,
                                 OTF_KeyValueList *list );

/* # OTF_ENTER_RECORD */
typedef int (OTF_Handler_Enter) ( void* userData,
                                  uint64_t time,
                                  uint32_t function,
                                  uint32_t process,
                                  uint32_t source,
                                  OTF_KeyValueList *list );

/* # OTF_LEAVE_RECORD */
typedef int (OTF_Handler_Leave) ( void* userData,
                                  uint64_t time,
                                  uint32_t function,
                                  uint32_t process,
                                  uint32_t source,
                                  OTF_KeyValueList *list );

/* # OTF_SEND_RECORD */
typedef int (OTF_Handler_SendMsg) ( void* userData,
                                    uint64_t time,
                                    uint32_t sender,
                                    uint32_t receiver,
                                    uint32_t group,
                                    uint32_t type,
                                    uint32_t length,
                                    uint32_t source,
                                    OTF_KeyValueList *list );

/* # OTF_RECEIVE_RECORD */
typedef int (OTF_Handler_RecvMsg) ( void* userData,
                                    uint64_t time,
                                    uint32_t recvProc,
                                    uint32_t sendProc,
                                    uint32_t group,
                                    uint32_t type,
                                    uint32_t length,
                                    uint32_t source,
                                    OTF_KeyValueList *list );

/* # OTF_COUNTER_RECORD */
typedef int (OTF_Handler_Counter) ( void* userData,
                                    uint64_t time,
                                    uint32_t process,
                                    uint32_t counter,
                                    uint64_t value,
                                    OTF_KeyValueList *list );

/* # OTF_COLLOP_RECORD */
typedef int (OTF_Handler_CollectiveOperation) ( void* userData,
                                                uint64_t time,
                                                uint32_t process,
                                                uint32_t collective,
                                                uint32_t procGroup,
                                                uint32_t rootProc,
                                                uint32_t sent,
                                                uint32_t received,
                                                uint64_t duration,
                                                uint32_t source,
                                                OTF_KeyValueList *list );

/* # OTF_BEGINCOLLOP_RECORD */
typedef int (OTF_Handler_BeginCollectiveOperation) ( void* userData,
                                                     uint64_t time,
                                                     uint32_t process,
                                                     uint32_t collOp,
                                                     uint64_t matchingId,
                                                     uint32_t procGroup,
                                                     uint32_t rootProc,
                                                     uint64_t sent,
                                                     uint64_t received,
                                                     uint32_t scltoken,
                                                     OTF_KeyValueList *list );

/* # OTF_ENDCOLLOP_RECORD */
typedef int (OTF_Handler_EndCollectiveOperation) ( void* userData,
                                                   uint64_t time,
                                                   uint32_t process,
                                                   uint64_t matchingId,
                                                   OTF_KeyValueList *list );

/* # OTF_EVENTCOMMENT_RECORD */
typedef int (OTF_Handler_EventComment) ( void* userData,
                                         uint64_t time,
                                         uint32_t process,
                                         const char* comment,
                                         OTF_KeyValueList *list );

/* # OTF_BEGINPROCESS_RECORD */
typedef int (OTF_Handler_BeginProcess) ( void* userData,
                                         uint64_t time,
                                         uint32_t process,
                                         OTF_KeyValueList *list );

/* # OTF_ENDPROCESS_RECORD */
typedef int (OTF_Handler_EndProcess) ( void* userData,
                                       uint64_t time,
                                       uint32_t process,
                                       OTF_KeyValueList *list );


/* # OTF_FILEOPERATION_RECORD */
typedef int (OTF_Handler_FileOperation) ( void* userData,
                                          uint64_t time,
                                          uint32_t fileid,
                                          uint32_t process,
                                          uint64_t handleid,
                                          uint32_t operation,
                                          uint64_t bytes,
                                          uint64_t duration,
                                          uint32_t source,
                                          OTF_KeyValueList *list );

/* # OTF_BEGINFILEOP_RECORD */
typedef int (OTF_Handler_BeginFileOperation) ( void* userData,
                                               uint64_t time,
                                               uint32_t process,
                                               uint64_t matchingId,
                                               uint32_t scltoken,
                                               OTF_KeyValueList *list );

/* # OTF_ENDFILEOP_RECORD */
typedef int (OTF_Handler_EndFileOperation) ( void* userData,
                                             uint64_t time,
                                             uint32_t process,
                                             uint32_t fileid,
                                             uint64_t matchingId,
                                             uint64_t handleId,
                                             uint32_t operation,
                                             uint64_t bytes,
                                             uint32_t scltoken,
                                             OTF_KeyValueList *list );

/* # OTF_RMAPUT_RECORD */
typedef int (OTF_Handler_RMAPut) ( void* userData,
                                   uint64_t time,
                                   uint32_t process,
                                   uint32_t origin,
                                   uint32_t target,
                                   uint32_t communicator,
                                   uint32_t tag,
                                   uint64_t bytes,
                                   uint32_t source,
                                   OTF_KeyValueList *list );

/* # OTF_RMAPUTRE_RECORD */
typedef int (OTF_Handler_RMAPutRemoteEnd) ( void* userData,
                                            uint64_t time,
                                            uint32_t process,
                                            uint32_t origin,
                                            uint32_t target,
                                            uint32_t communicator,
                                            uint32_t tag,
                                            uint64_t bytes,
                                            uint32_t source,
                                            OTF_KeyValueList *list );

/* # OTF_RMAGET_RECORD */
typedef int (OTF_Handler_RMAGet) ( void* userData,
                                   uint64_t time,
                                   uint32_t process,
                                   uint32_t origin,
                                   uint32_t target,
                                   uint32_t communicator,
                                   uint32_t tag,
                                   uint64_t bytes,
                                   uint32_t source,
                                   OTF_KeyValueList *list );

/* # OTF_RMAEND_RECORD */
typedef int (OTF_Handler_RMAEnd) ( void* userData,
                                   uint64_t time,
                                   uint32_t process,
                                   uint32_t remote,
                                   uint32_t communicator,
                                   uint32_t tag,
                                   uint32_t source,
                                   OTF_KeyValueList *list );

/* typedefs for OTF snapshot records *************************************** */


/* # OTF_SNAPSHOTCOMMENT_RECORD */
typedef int (OTF_Handler_SnapshotComment) ( void* userData,
                                            uint64_t time,
                                            uint32_t process,
                                            const char* comment,
                                            OTF_KeyValueList *list );

/* # OTF_ENTERSNAPSHOT_RECORD */
typedef int (OTF_Handler_EnterSnapshot) ( void *userData,
                                          uint64_t time,
                                          uint64_t originaltime,
                                          uint32_t function,
                                          uint32_t process,
                                          uint32_t source,
                                          OTF_KeyValueList *list );

/* # OTF_SENDSNAPSHOT_RECORD */
typedef int (OTF_Handler_SendSnapshot) ( void *userData,
                                         uint64_t time,
                                         uint64_t originaltime,
                                         uint32_t sender,
                                         uint32_t receiver,
                                         uint32_t procGroup,
                                         uint32_t tag,
                                         uint32_t length,
                                         uint32_t source,
                                         OTF_KeyValueList *list );

/* # OTF_OPENFILESNAPSHOT_RECORD */
typedef int (OTF_Handler_OpenFileSnapshot) ( void* userData,
                                             uint64_t time,
                                             uint64_t originaltime,
                                             uint32_t fileid,
                                             uint32_t process,
                                             uint64_t handleid,
                                             uint32_t source,
                                             OTF_KeyValueList *list );

/* # OTF_BEGINCOLLOPSNAPSHOT_RECORD */
typedef int (OTF_Handler_BeginCollopSnapshot) ( void* userData,
                                                uint64_t time,
                                                uint64_t originaltime,
                                                uint32_t process,
                                                uint32_t collOp,
                                                uint64_t matchingId,
                                                uint32_t procGroup,
                                                uint32_t rootProc,
                                                uint64_t sent,
                                                uint64_t received,
                                                uint32_t scltoken,
                                                OTF_KeyValueList *list );

/* # OTF_BEGINFILEOPSNAPSHOT_RECORD */
typedef int (OTF_Handler_BeginFileOpSnapshot) ( void* userData,
                                                uint64_t time,
                                                uint64_t originaltime,
                                                uint32_t process,
                                                uint64_t matchingId,
                                                uint32_t scltoken,
                                                OTF_KeyValueList *list );

/* # OTF_COLLOPCOUNTSNAPSHOT_RECORD */
typedef int (OTF_Handler_CollopCountSnapshot) ( void* userData,
                                                uint64_t time,
                                                uint32_t process,
                                                uint32_t communicator,
                                                uint64_t count,
                                                OTF_KeyValueList *list );

/* # OTF_COUNTERSNAPSHOT_RECORD */
typedef int (OTF_Handler_CounterSnapshot) ( void* userData,
                                            uint64_t time,
                                            uint64_t originaltime,
                                            uint32_t process,
                                            uint32_t counter,
                                            uint64_t value,
                                            OTF_KeyValueList *list );


/* typedefs for OTF summary records **************************************** */


/* # OTF_SUMMARYCOMMENT_RECORD */
typedef int (OTF_Handler_SummaryComment) ( void* userData,
                                           uint64_t time,
                                           uint32_t process,
                                           const char* comment,
                                           OTF_KeyValueList *list );

/* # OTF_FUNCTIONSUMMARY_RECORD */
typedef int (OTF_Handler_FunctionSummary) ( void* userData,
                                            uint64_t time,
                                            uint32_t function,
                                            uint32_t process,
                                            uint64_t invocations,
                                            uint64_t exclTime,
                                            uint64_t inclTime,
                                            OTF_KeyValueList *list );

/* # OTF_FUNCTIONGROUPSUMMARY_RECORD */
typedef int (OTF_Handler_FunctionGroupSummary) ( void* userData,
                                                 uint64_t time,
                                                 uint32_t funcGroup,
                                                 uint32_t process,
                                                 uint64_t invocations,
                                                 uint64_t exclTime,
                                                 uint64_t inclTime,
                                                 OTF_KeyValueList *list );

/* # OTF_MESSAGESUMMARY_RECORD */
typedef int (OTF_Handler_MessageSummary) ( void* userData,
                                           uint64_t time,
                                           uint32_t process,
                                           uint32_t peer,
                                           uint32_t comm,
                                           uint32_t type,
                                           uint64_t sentNumber,
                                           uint64_t receivedNumber,
                                           uint64_t sentBytes,
                                           uint64_t receivedBytes,
                                           OTF_KeyValueList *list );

/* # OTF_COLLOPSUMMARY_RECORD */
typedef int (OTF_Handler_CollopSummary) ( void *userData,
                                          uint64_t time,
                                          uint32_t process,
                                          uint32_t comm,
                                          uint32_t collective,
                                          uint64_t sentNumber,
                                          uint64_t receivedNumber,
                                          uint64_t sentBytes,
                                          uint64_t receivedBytes,
                                          OTF_KeyValueList *list );

/* # OTF_FILEOPERATIONSUMMARY_RECORD */
typedef int (OTF_Handler_FileOperationSummary) ( void* userData,
                                                 uint64_t time,
                                                 uint32_t fileid,
                                                 uint32_t process,
                                                 uint64_t nopen,
                                                 uint64_t nclose,
                                                 uint64_t nread,
                                                 uint64_t nwrite,
                                                 uint64_t nseek,
                                                 uint64_t bytesread,
                                                 uint64_t byteswrite,
                                                 OTF_KeyValueList *list );

/* # OTF_FILEGROUPOPERATIONSUMMARY_RECORD */
typedef int (OTF_Handler_FileGroupOperationSummary) ( void* userData,
                                                      uint64_t time,
                                                      uint32_t groupid,
                                                      uint32_t process,
                                                      uint64_t nopen,
                                                      uint64_t nclose,
                                                      uint64_t nread,
                                                      uint64_t nwrite,
                                                      uint64_t nseek,
                                                      uint64_t bytesread,
                                                      uint64_t byteswrite,
                                                      OTF_KeyValueList *list );

/* # OTF_UNKNOWN_RECORD */
typedef int (OTF_Handler_UnknownRecord) ( void *userData,
                                          uint64_t time,
                                          uint32_t process,
                                          const char *record );


/* typedefs for OTF marker records **************************************** */


/* # OTF_DEFMARKER_RECORD */
typedef int (OTF_Handler_DefMarker) ( void *userData,
                                      uint32_t stream,
                                      uint32_t token,
                                      const char* name,
                                      uint32_t type,
                                      OTF_KeyValueList *list );

/* # OTF_MARKER_RECORD */
typedef int (OTF_Handler_Marker) ( void *userData,
                                   uint64_t time,
                                   uint32_t process,
                                   uint32_t token,
                                   const char* text,
                                   OTF_KeyValueList *list );

/** @endcond */

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_HANDLERARRAY_H */
