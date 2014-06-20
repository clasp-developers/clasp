/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#ifndef REDUCE_DATA_H
#define REDUCE_DATA_H


#include "datastructs.h"


/* reduce the data to the master process */
bool ReduceData( AllData& alldata );

/* reduce the dispersion data to the master process */
bool ReduceDataDispersion( AllData& alldata );

#endif /* REDUCE_DATA_H */

