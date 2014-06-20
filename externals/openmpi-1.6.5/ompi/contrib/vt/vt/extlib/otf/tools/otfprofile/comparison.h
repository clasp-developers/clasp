/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#ifndef COMPARISON_KMEANS_H
#define COMPARISON_KMEANS_H


#include <sstream>

#include "datastructs.h"


/* process comparison; analyze process properties to determine similar or
   different behavior patterns */

/* do process comparison using the CLINKAGE algorithm */
bool ProcessComparisonCLINKAGE( AllData& alldata, ostringstream& mapdata );

/* do process comparison using the K-means algorithm */
bool ProcessComparisonKMEANS( AllData& alldata, ostringstream& mapdata );


#endif /* COMPARISON_KMEANS_H */
