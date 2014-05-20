/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/**
 *  \mainpage Open Trace Format API Documentation
 *
 *  \section introduction Introduction
 *
 *  This library supports efficient parallel and distributed access to trace
 *  data and offers selective reading access regarding arbitrary time
 *  intervals, process selection and record types. Optional auxiliary
 *  information can assist this selective access.
 *
 *  The Open Trace Format makes use of a portable ASCII encoding.
 *  It distributes single traces to multiple so called streams
 *  with one or more files each. Merging of records from multiple files is done
 *  transparently by the OTF library. The number of possible streams is not
 *  limited by the number of available file handles.
 *
 *
 *   For more information read the specification (docu/api/specification.pdf)
 *
 *   OTF is available under the BSD open source license that allows free usage
 *   for academic and commercial applications.
 *
 *
 *  \section quicklinks Quick Links
 *
 *   \subsection interfaces Interfaces
 *     - \ref reader "Global Reader" - \ref rstream "Stream Reader"
 *     - \ref writer "Global Writer" - \ref wstream "Stream Writer "
 *     - \ref handler "Handlers" - \ref ha "Handler Array"
 *
 *     - \ref fm "File Manager"
 *     - \ref mc "Master Control"
 *
 *     - \ref keyvalue "KeyValueList"
 *
 *     - \ref misc "Miscellaneous"
 *
 *   \subsection examples Examples
 *     - \ref reader_example1 "Reader"
 *     - \ref reader_example2 "Reader 2"
 *     - \ref rstream_example "Stream Reader"
 *     - \ref writer_example "Writer"
 *     - \ref wstream_example "Stream Writer"
 *     - \ref mc_example "Master Control"
 *     - \ref keyvalue_read_example "KeyValueList - Read"
 *     - \ref keyvalue_write_example "KeyValueList - Write"
 *
 *   \subsection Contact
 *     - report bugs to andreas.knuepfer@tu-dresden.de
 *     - <a href="http://www.tu-dresden.de/zih/otf">OTF-Home</a>
 *
 */

/** 
 *  @file otf.h
 * 
 *  @brief Main include file for applications using OTF.
 */


/**
 * \defgroup misc Miscellaneous
 */

/**
 * \defgroup internal Internal Interfaces
 *
 * All files in this module contain Interfaces that are not meant to be accessed directly.
 */

#ifndef OTF_H
#define OTF_H


#include "OTF_Definitions.h"
#include "OTF_KeyValue.h"
#include "OTF_FileManager.h"
#include "OTF_Filenames.h"
#include "OTF_HandlerArray.h"
#include "OTF_MasterControl.h"
#include "OTF_RStream.h"
#include "OTF_Reader.h"
#include "OTF_WStream.h"
#include "OTF_Writer.h"


#endif /* OTF_H */

