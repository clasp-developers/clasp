/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_Definitions.h
 * 
 *  @brief Provides many many macros for different purposes.
 *
 *  \ingroup misc
 */


#ifndef OTF_DEFINITIONS_H
#define OTF_DEFINITIONS_H


#include "OTF_Version.h"

/* definitions of record type identifiers */


/* Event records -- have them numbered and sorted, 
this does not affect the format at all. 
This is always safe with recompilation and re-linking, 
yet it breaks the link compatibility of library versions.*/

#define OTF_EVENTCOMMENT_RECORD					0
#define OTF_COUNTER_RECORD						1
#define OTF_ENTER_RECORD						2

#define OTF_NOOP_RECORD						58

#define OTF_COLLOP_RECORD						5

#define OTF_RECEIVE_RECORD						10
#define OTF_SEND_RECORD							11
#define OTF_LEAVE_RECORD						12

#define OTF_BEGINPROCESS_RECORD					35
#define OTF_ENDPROCESS_RECORD					36

#define OTF_FILEOPERATION_RECORD				42

#define OTF_RMAPUT_RECORD                                       47
#define OTF_RMAPUTRE_RECORD                                     48
#define OTF_RMAGET_RECORD                                       49
#define OTF_RMAEND_RECORD                                       50

#define OTF_BEGINCOLLOP_RECORD					51
#define OTF_ENDCOLLOP_RECORD					52

#define OTF_BEGINFILEOP_RECORD					53
#define OTF_ENDFILEOP_RECORD					54

/* Definition records*/

#define OTF_DEFTIMERRESOLUTION_RECORD			13
#define OTF_DEFPROCESS_RECORD					14
#define OTF_DEFPROCESSGROUP_RECORD				15
#define OTF_DEFATTRLIST_RECORD						55
#define OTF_DEFPROCESSORGROUPATTR_RECORD				56
#define OTF_DEFPROCESSSUBSTITUTES_RECORD				63
#define OTF_DEFFUNCTION_RECORD					16
#define OTF_DEFFUNCTIONGROUP_RECORD				17
#define OTF_DEFCOUNTER_RECORD					18
#define OTF_DEFCOUNTERGROUP_RECORD				19
#define OTF_DEFCOLLOP_RECORD					20
#define OTF_DEFSCL_RECORD						21
#define OTF_DEFSCLFILE_RECORD					22
#define OTF_DEFVERSION_RECORD					23
#define OTF_DEFCREATOR_RECORD					24
#define OTF_DEFFILE_RECORD						25
#define OTF_DEFFILEGROUP_RECORD					26
#define OTF_DEFTIMERANGE_RECORD					61
#define OTF_DEFCOUNTERASSIGNMENTS_RECORD		62
#define OTF_DEFKEYVALUE_RECORD						57
#define OTF_DEFUNIQUEID_RECORD						64
#define OTF_DEFAUXSAMPLEPOINT_RECORD            65


#define OTF_FUNCTIONSUMMARY_RECORD				28
#define OTF_FUNCTIONGROUPSUMMARY_RECORD			29
#define OTF_MESSAGESUMMARY_RECORD				30
#define OTF_COLLOPSUMMARY_RECORD				44
#define OTF_FILEOPERATIONSUMMARY_RECORD			31
#define OTF_FILEGROUPOPERATIONSUMMARY_RECORD	32

#define OTF_DEFINITIONCOMMENT_RECORD			34

#define OTF_ENTERSNAPSHOT_RECORD				37
#define OTF_SENDSNAPSHOT_RECORD					38

#define OTF_SUMMARYCOMMENT_RECORD				39
#define OTF_SNAPSHOTCOMMENT_RECORD				40
#define OTF_OPENFILESNAPSHOT_RECORD				43
#define OTF_BEGINCOLLOPSNAPSHOT_RECORD          59
#define OTF_BEGINFILEOPSNAPSHOT_RECORD          60

#define OTF_COLLOPCOUNTSNAPSHOT_RECORD			66
#define OTF_COUNTERSNAPSHOT_RECORD              67

#define OTF_UNKNOWN_RECORD						41

#define OTF_DEFMARKER_RECORD					45
#define OTF_MARKER_RECORD						46

/* Number of records */
#define OTF_NRECORDS							68

/* Stream format definition */

#define OTF_WSTREAM_FORMAT_SHORT		0
#define OTF_WSTREAM_FORMAT_LONG			1
#define OTF_WSTREAM_FORMAT_INLINE_SNAPSHOTS	2


/* 

Counter properties 

There are three groups of counter properties that can be combined. 
Not all combinations make perfect sense, some are only there for completeness.

For all groups the _BITS macro allows to identify the bits that are valid for 
this group. This might be useful to AND away all other bits.
*/


/*
The counter type uses the first bit (the second is reserved so far). It says 
whether the counter is an accumulating counter ( OTF_COUNTER_TYPE_ACC) that is 
monotonously increasing. Then it's probably a good idea to visualize it's 
derivative instead of the original value. The alternative is an absolute 
value (OTF_COUNTER_TYPE_ABS) that should be displayed as is.
*/


/* 1st-2nd bit */
#define OTF_COUNTER_TYPE_BITS		3
#define OTF_COUNTER_TYPE_ACC		0
#define OTF_COUNTER_TYPE_ABS		1

/* 
The counter scope says when the values have been collected or for which 
interval of time they are valid. It uses the 3rd and 4th bit(bit 5 is 
reserved again). The default is OTF_COUNTER_SCOPE_START which means the values 
belong to the time interval since the beginning of the measurement. 
OTF_COUNTER_SCOPE_POINT means the value is only valid at a point in time but 
not necessarily for any interval of time. OTF_COUNTER_SCOPE_LAST means the 
value is related to the time interval since the last counter sample of the 
same counter, i.e. the immediate past. And finally OTF_COUNTER_SCOPE_NEXT 
means it is valid from now until the next counter sample, i.e. the future 
right ahead.
*/

#define OTF_COUNTER_SCOPE_BITS 		12
#define OTF_COUNTER_SCOPE_START 	0
#define OTF_COUNTER_SCOPE_POINT 	4
#define OTF_COUNTER_SCOPE_LAST		8
#define OTF_COUNTER_SCOPE_NEXT		12

/* 
Finally, there is the variable type which occupies the 6th - 9th bit. The bit 
values are chosen to allow easy distinction of integers and non-integers as 
well as unsigned and signed.
Furthermore, there are some macros to ask for the type. Some similar macros 
could be added for convenience.
*/

#define OTF_COUNTER_VARTYPE_ISINTEGER(x) (x < 256)
#define OTF_COUNTER_VARTYPE_ISSIGNED(x) ((x&32) == 32)
#define OTF_COUNTER_VARTYPE_ISUNSIGNED(x) ((x&32) == 0)

#define OTF_COUNTER_VARTYPE_BITS				480 /* 1111xxxxx */
#define OTF_COUNTER_VARTYPE_UNSIGNED8			0   /* 0000 */
#define OTF_COUNTER_VARTYPE_SIGNED8				32  /* 0001 */
#define OTF_COUNTER_VARTYPE_UNSIGNED4			64  /* 0010 */
#define OTF_COUNTER_VARTYPE_SIGNED4				96  /* 0011 */
#define OTF_COUNTER_VARTYPE_UNSIGNED2			128 /* 0100 */
#define OTF_COUNTER_VARTYPE_SIGNED2				160 /* 0101 */
#define OTF_COUNTER_VARTYPE_FLOAT				256 /* 1000 */
#define OTF_COUNTER_VARTYPE_DOUBLE				288 /* 1001 */



#define OTF_COUNTER_PROP_DEFAULT	0


/* Types of collective operations */

#define OTF_COLLECTIVE_TYPE_UNKNOWN 	0
#define OTF_COLLECTIVE_TYPE_BARRIER 	1
#define OTF_COLLECTIVE_TYPE_ONE2ALL 	2
#define OTF_COLLECTIVE_TYPE_ALL2ONE 	3
#define OTF_COLLECTIVE_TYPE_ALL2ALL 	4


/*
File Operations - 32-bit
The bits 0-5 contain the identifier of the file operation that has happened.
The bits 6-31 are bit flags that carry additional information on the operation.
A macro allows for accessing the file operation in a convenient way.
*/
#define OTF_FILEOP_BITS			0x0000001f
#define OTF_FILEOP_OPEN			0
#define OTF_FILEOP_CLOSE		1
#define OTF_FILEOP_READ			2
#define OTF_FILEOP_WRITE		3
#define OTF_FILEOP_SEEK			4
#define OTF_FILEOP_UNLINK		5
#define OTF_FILEOP_RENAME		6
#define OTF_FILEOP_DUP                  7
#define OTF_FILEOP_SYNC                 8
#define OTF_FILEOP_LOCK                 9
#define OTF_FILEOP_UNLOCK               10
#define OTF_FILEOP_OTHER        	31
#define OTF_IOFLAGS_BITS		0xffffffe0
#define OTF_IOFLAG_IOFAILED		32
#define OTF_IOFLAG_ASYNC		64
#define OTF_IOFLAG_COLL			128
#define OTF_IOFLAG_DIRECT		256
#define OTF_IOFLAG_SYNC			512
#define OTF_IOFLAG_ISREADLOCK           1024
#define OTF_FILEOP(x) (x & OTF_FILEOP_BITS)

/* Types of markers */

#define OTF_MARKER_TYPE_UNKNOWN     0
#define OTF_MARKER_TYPE_ERROR       1
#define OTF_MARKER_TYPE_WARNING     2
#define OTF_MARKER_TYPE_HINT        3

/* otf_errno related defines */
#define OTF_ERR_LEN 1000
#define OTF_NO_ERROR 0
#define OTF_ERROR -1

extern char otf_strerr[OTF_ERR_LEN];
extern int otf_errno;

/* OTF_KeyValueList related defines */

/* This macro defines the maximum length of a byte array that can be
   appended internally to an OTF_KeyValueList() --> internal use only
   For the user a byte array can contain an unlimited number of bytes.
NOTE: Do not edit this constant unless you know exactly what you do! */
#define OTF_KEYVALUE_MAX_ARRAY_LEN 100

/* enum used for DefAttributeList record */
/**  An enum which holds all values that are possible to set with datatype OTF_ATTR_TYPE().*/
typedef enum OTF_ATTR_TYPE_enum { 
	OTF_ATTR_UNKNOWN = 0, /**< */
	OTF_ATTR_IsMPIRank = 1, /**< */
	OTF_ATTR_IsPThread = 2, /**< */
	OTF_ATTR_IsOMPThread = 3, /**< */
	OTF_ATTR_IsCellSPUThread = 4, /**< */
	OTF_ATTR_hasGroupCounters = 5, /**< */
	OTF_ATTR_hasEnterLeaveRecords = 6, /**< */
	OTF_ATTR_IsCommunicator = 7 /**< */
} OTF_ATTR_TYPE;

/** Types of AuxSamplePoints */
typedef enum OTF_AuxSamplePointType_enum {
    /** There are snapshot information available at this time. */
    OTF_AUX_SAMPLE_POINT_SNAPSHOT = 0,
    /** There are summary information available at this time. */
    OTF_AUX_SAMPLE_POINT_SUMMARY = 1,
    /** There are snapshot information available in the event stream at this time. */
    OTF_AUX_SAMPLE_POINT_INLINE_SNAPSHOT = 2
} OTF_AuxSamplePointType;

/* return values for handlers. they are not yet evaluated!!! */

/** When writing an own handler, use these macros to tell OTF, what to do.
if you return OTF_RETURN_OK OTF continues reading the trace and calling the
appropriate handlers. If you return OTF_RETURN_BREAK or OTF_RETURN_ABORT OTF
stops reading immediately */
#define OTF_RETURN_OK					0
/** @see OTF_RETURN_OK */
#define OTF_RETURN_BREAK				1
/** @see OTF_RETURN_OK */
#define OTF_RETURN_ABORT				1

#define OTF_READ_ERROR					(uint64_t)-1
#define OTF_READ_MAXRECORDS				(uint64_t)-2

#define OTF_ZBUFFER_DEFAULTSIZE                         (1 * 1024 * 1024)

enum enum_OTF_IofslMode {
	OTF_IOFSL_DISABLED        = 0,
	OTF_IOFSL_MULTIFILE_SPLIT = 1,
	OTF_IOFSL_MULTIFILE       = 2
};
typedef enum enum_OTF_IofslMode OTF_IofslMode;

/** size in number of IdxEntry's (16 byte each) */
#define OTF_IOFSL_INDEX_BUFFER_DEFAULTLENGTH		(1 * 1024)

#define OTF_IOFSL_FLAG_NONBLOCKING			(1<<0)

#endif /* OTF_DEFINITIONS_H */
