/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#ifndef SUMMARIZE_DATA_H
#define SUMMARIZE_DATA_H


#include "datastructs.h"


/* summarize the data for all trace processes on the current worker */
bool SummarizeData( AllData& alldata );

/* summarize the dispersion data for all trace processes on the current worker */
bool SummarizeDataDispersion( AllData& alldata );

#endif /* SUMMARIZE_DATA_H */

