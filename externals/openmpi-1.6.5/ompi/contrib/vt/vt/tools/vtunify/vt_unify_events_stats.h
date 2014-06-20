/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_UNIFY_EVENTS_STATS_H_
#define _VT_UNIFY_EVENTS_STATS_H_

#include "vt_unify.h"

//
// EventsAndStatsC class
//
class EventsAndStatsC
{
public:

   // unify either events or statistics
   typedef enum { SCOPE_EVENTS, SCOPE_STATS } ScopeTypeT;

   // constructor
   EventsAndStatsC( const ScopeTypeT & scope );

   // destructor
   ~EventsAndStatsC();

   // unify events/statistics
   bool run();

   // rename temporary output files
   bool cleanUp();

private:

   // rewrite events/statistics
   bool rewrite();

   // scope to process by this class (events or statistics)
   ScopeTypeT m_scope;

};

// instances of class EventsAndStatsC
//
extern EventsAndStatsC * theEvents;
extern EventsAndStatsC * theStatistics;

#endif // _VT_UNIFY_EVENTS_STATS_H_
