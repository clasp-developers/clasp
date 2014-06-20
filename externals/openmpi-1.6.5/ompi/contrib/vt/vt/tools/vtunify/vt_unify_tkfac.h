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

#ifndef _VT_UNIFY_TKFAC_H_
#define _VT_UNIFY_TKFAC_H_

#include "vt_unify.h"
#include "vt_unify_defs_recs.h"
#include "vt_unify_tkfac_scope.h"

//
// TokenFactory class
//
class TokenFactoryC
{
public:

   // constructor
   TokenFactoryC();

   // destructor
   ~TokenFactoryC();

   // add scope instance of certain def. record type
   void addScope( const DefRecTypeT & type, TokenFactoryScopeI * scope );

   // delete scope instance for certain def. record type
   void deleteScope( const DefRecTypeT & type );

   // get scope instance for certain def. record type
   TokenFactoryScopeI * getScope( const DefRecTypeT & type ) const;

#ifdef VT_MPI

   // distribute token translation tables
   bool distTranslations( const VT_MPI_INT & destRank = 0,
           const bool wait = false );

#endif // VT_MPI

private:

   // map def. type <-> scope
   std::map<DefRecTypeT, TokenFactoryScopeI*> m_def2scope;

};

// instance of class TokenFactory
extern TokenFactoryC * theTokenFactory;

#endif // _VT_UNIFY_TKFAC_H_
