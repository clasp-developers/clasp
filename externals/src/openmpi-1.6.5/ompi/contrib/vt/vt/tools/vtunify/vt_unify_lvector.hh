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

#ifndef _VT_UNIFY_LVECTOR_HH_
#define _VT_UNIFY_LVECTOR_HH_

#include <vector>

//
// LargeVectorC class
//
template <class T>
class LargeVectorC
{
public:

   typedef typename std::allocator<T>::size_type size_type;
   typedef typename std::vector<T>::iterator iterator;
   typedef typename std::vector<T>::const_iterator const_iterator;

   // default chunk size (num. of elements to pre-allocate)
   static const size_type DEFAULT_CHUNK_SIZE = 100000;

   // constructors
   //
   LargeVectorC()
      : m_chunkSize( DEFAULT_CHUNK_SIZE ), m_size( 0 ) {}
   LargeVectorC( const size_type & chunkSize )
      : m_size( 0 )
   {
      vt_assert( chunkSize > 0 );
      m_chunkSize = chunkSize;
   }

   // destructor
   ~LargeVectorC() {}

   // check whether the vector is empty
   bool empty() const
   {
      return m_size == 0;
   }

   // get number of elements
   size_type size() const
   {
      return m_size;
   }

   // get chunk size
   size_type chunkSize() const
   {
      return m_chunkSize;
   }

   // remove all elements
   void clear()
   {
      m_vector.clear();
      m_size = 0;
   }

   // get iterator to the first element
   typename std::vector<T>::iterator begin()
   {
      return m_vector.begin();
   }

   // get iterator to the element following the last element
   typename std::vector<T>::iterator end()
   {
      if( m_size == 0 )
         return m_vector.end();
      else
         return m_vector.begin() + m_size;
   }

   // access the first element
   T & front()
   {
      vt_assert( m_size > 0 );
      return m_vector[0];
   }

   // access the last element
   T & back()
   {
      vt_assert( m_size > 0 );
      return m_vector[m_size-1];
   }

   // insert an element to the end
   void push_back( const T & val )
   {
      // enlarge vector, if necessary
      if( m_vector.size() == m_size )
         m_vector.resize( m_vector.size() + m_chunkSize );

      // add value to vector
      m_vector[m_size++] = val;
   }

   // get reference to element at specific location
   T & operator[]( size_type pos )
   {
      vt_assert( pos < m_size );
      return m_vector[pos];
   }

   // get reference to element at specific location (const version)
   const T & operator[]( size_type pos ) const
   {
      vt_assert( pos < m_size );
      return m_vector[pos];
   }

private:

   // actual vector
   std::vector<T> m_vector;

   // number of elements which shall be pre-allocated
   size_type m_chunkSize;

   // number of current vector elements
   size_type m_size;

};


#endif // _VT_UNIFY_LVECTOR_HH_
