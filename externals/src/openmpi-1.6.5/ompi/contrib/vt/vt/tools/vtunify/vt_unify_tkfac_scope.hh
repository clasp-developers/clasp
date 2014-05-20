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

#ifndef _VT_UNIFY_TKFAC_SCOPE_HH_
#define _VT_UNIFY_TKFAC_SCOPE_HH_

//////////////////// class TokenFactoryScopeC ////////////////////

// public methods
//

template <class T>
TokenFactoryScopeC<T>::TokenFactoryScopeC( std::set<T> * _globdefs,
   const uint32_t & _tkoffs )
   : m_globDefs( _globdefs ), m_seqToken( _tkoffs )
{
   // Empty
}

template <class T>
TokenFactoryScopeC<T>::~TokenFactoryScopeC()
{
   // Empty
}

template <class T>
uint32_t
TokenFactoryScopeC<T>::create( const void * localDef, uint32_t globalToken )
{
   const T & local_def = *static_cast<const T*>(localDef);

   // search for already created global definition
   typename std::set<T>::const_iterator it = m_globDefs->find( local_def );

   // get its global token, if found
   //
   if( it != m_globDefs->end() )
   {
      globalToken = it->deftoken;
   }
   // otherwise, create global definition
   //
   else
   {
      T global_def = local_def;

      global_def.loccpuid = 0;

      if( globalToken == 0 )
         global_def.deftoken = globalToken = getNextToken();
      else
         global_def.deftoken = globalToken;

      m_globDefs->insert( global_def );
   }

   // set token translation for process, if necessary
   if( local_def.loccpuid != 0 && local_def.deftoken != 0 )
      setTranslation( local_def.loccpuid, local_def.deftoken, globalToken );

   return globalToken;
}

template <class T>
void
TokenFactoryScopeC<T>::setTranslation( const uint32_t & process,
   const uint32_t & localToken, const uint32_t & globalToken )
{
   // get master process id
   uint32_t mprocess = process & VT_TRACEID_BITMASK;

   // set token translation
   m_proc2TokenMap[mprocess][localToken] = globalToken;
}

template <class T>
uint32_t
TokenFactoryScopeC<T>::translate( const uint32_t & process,
   const uint32_t & localToken, const bool showError ) const
{
   uint32_t global_token = 0;

   // get master process id
   uint32_t mprocess = process & VT_TRACEID_BITMASK;

   // search token mappings of process
   std::map<uint32_t, std::map<uint32_t, uint32_t> >::const_iterator
      proc_it = m_proc2TokenMap.find( mprocess );

   // found?
   if( proc_it != m_proc2TokenMap.end() )
   {
      // search token mapping by local token
      std::map<uint32_t, uint32_t>::const_iterator map_it =
         proc_it->second.find( localToken );

      // get global token, if found
      if( map_it != proc_it->second.end() )
         global_token = map_it->second;
   }

   // show error message, if no token translation found
   //
   if( global_token == 0 && showError )
   {
      std::cerr << ExeName << ": Error: No translation found for "
                << "local token " << localToken << " on process "
                << process << std::endl;
   }

   return global_token;
}

template <class T>
uint32_t
TokenFactoryScopeC<T>::getNextToken()
{
   return m_seqToken++;
}

#ifdef VT_MPI

template <class T>
VT_MPI_INT
TokenFactoryScopeC<T>::getPackSize( const uint32_t & process )
{
   VT_MPI_INT buffer_size = 0;
   VT_MPI_INT size;

   // process + m_proc2TokenMap[process].size()
   //
   CALL_MPI( MPI_Pack_size( 2, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // get token translation table of process
   std::map<uint32_t, std::map<uint32_t, uint32_t> >::const_iterator
      token_map_it = m_proc2TokenMap.find( process );

   // m_proc2TokenMap[process]
   //
   if( token_map_it != m_proc2TokenMap.end() &&
       !token_map_it->second.empty() )
   {
      CALL_MPI( MPI_Pack_size( token_map_it->second.size() * 2, MPI_UNSIGNED,
                               MPI_COMM_WORLD, &size ) );
      buffer_size += size;
   }

   return buffer_size;
}

template <class T>
void
TokenFactoryScopeC<T>::pack( const uint32_t & process,
   char *& buffer, const VT_MPI_INT & bufferSize, VT_MPI_INT & bufferPos,
   const bool clear )
{
   // process
   CALL_MPI( MPI_Pack( const_cast<uint32_t*>( &process ), 1, MPI_UNSIGNED,
                       buffer, bufferSize, &bufferPos, MPI_COMM_WORLD ) );

   // get token translation table of process
   std::map<uint32_t, std::map<uint32_t, uint32_t> >::iterator token_map_it =
      m_proc2TokenMap.find( process );

   // m_proc2TokenMap[process].size()
   //
   uint32_t token_map_size =
      ( token_map_it != m_proc2TokenMap.end() ) ?
         token_map_it->second.size() : 0;
   CALL_MPI( MPI_Pack( &token_map_size, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // m_proc2TokenMap[process]
   //
   if( token_map_it != m_proc2TokenMap.end() )
   {
      for( std::map<uint32_t, uint32_t>::const_iterator token_pair_it =
           token_map_it->second.begin();
           token_pair_it != token_map_it->second.end(); ++token_pair_it )
      {
         uint32_t token_pair[2] =
            { token_pair_it->first, token_pair_it->second };
         CALL_MPI( MPI_Pack( token_pair, 2, MPI_UNSIGNED, buffer,
                             bufferSize, &bufferPos, MPI_COMM_WORLD ) );
      }

      // clear token translation table of certain process id
      if( clear )
         m_proc2TokenMap.erase( token_map_it );
   }
}

template <class T>
void
TokenFactoryScopeC<T>::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
   VT_MPI_INT & bufferPos )
{
   // process
   //
   uint32_t process;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &process, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // m_proc2TokenMap[process].size()
   //
   uint32_t token_map_size;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &token_map_size, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // m_proc2TokenMap[process]
   //
   if( token_map_size > 0 )
   {
      for( uint32_t i = 0; i < token_map_size; i++ )
      {
         uint32_t token_pair[2];
         CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, token_pair, 2,
                               MPI_UNSIGNED, MPI_COMM_WORLD ) );

         // set token translation
         m_proc2TokenMap[process][token_pair[0]] = token_pair[1];
      }
   }
}

#endif // VT_MPI

#endif // _VT_UNIFY_TKFAC_SCOPE_HH_
