#ifndef	sort_H //[
#define sort_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"

//#define	DEBUG_SORT

namespace core {

namespace sort {


/* Example of an Ocomp class
class	OrderByLessThan
{
public:
    bool operator()(T_sp x, T_sp y )
    {
	return x < y;
    }
};

*/

template <typename Oelement>
void swap( Oelement& x, Oelement& y)
{
    Oelement t;
    t = x;
    x = y;
    y = t;
};


template <typename _RandomAccessIterator, typename Ocomp >
    void quickSort( _RandomAccessIterator m, _RandomAccessIterator en, Ocomp comparer )
{_G();
    _RandomAccessIterator k;
    _RandomAccessIterator n = en-1;
    typedef typename std::iterator_traits<_RandomAccessIterator>::value_type _ValueType;
    if ( m < n )
    {
#ifdef	DEBUG_SORT
	LOG(BF( "Sorting list at start" ));
	for ( _RandomAccessIterator ii=m; ii<=n; ii++ )
	{
	    LOG(BF("element %d = %s") % (ii-m) % (*ii)->__repr__());
	}
#endif
	int half = (n-m);
	half = half/2;
        k = m+half;	// pivot
	swap<_ValueType>(*m,*k);
#ifdef	DEBUG_SORT
	LOG(BF( "Sorting list pivot is now first element: " ));
	for ( _RandomAccessIterator ii=m; ii<=n; ii++ )
	{
	    LOG(BF("element %d = %s") % (ii-m) % (*ii)->__repr__() );
	}
#endif
	_RandomAccessIterator i = m+1;
	_RandomAccessIterator j = n;
	while ( i <= j )
	{_BLOCK_TRACEF(BF("Top of while i(%d) <= j(%d)")%(i-m)%(j-m));
	    while ( ( i <= n ) && (comparer(*i,*m)) ) 
	    {
		LOG(BF( "skipping lower bin index: %d value: %s") % (i-m) % _rep_((*i)) );
		i++;
	    }
	    while ( ( j >= i ) && (!comparer(*j,*m)) ) 
	    {
		LOG(BF("skipping upper bin index: %d value: %s") % (j-m) % _rep_((*j)) );
		j--;
	    }
	    if ( i < j ) 
	    {
		LOG(BF( "swapping value lower index: %d value: %s") % (i-m) % _rep_((*i)) );
		LOG(BF( "swapping value upper index: %d value: %s") % (j-m) % _rep_((*j)) );
		swap<_ValueType>(*i,*j);
	    }
	}
	swap<_ValueType>(*m,*j);
#ifdef	DEBUG_SORT
	LOG(BF( "After pivot list is now: " ));
	for ( _RandomAccessIterator ii=m; ii<=n; ii++ )
	{
	    LOG(BF( "element %d = %s") % (ii-m) % (*ii)->__repr__() );
	}
#endif
	LOG(BF("element at j -- index: %d value: %s") % (j-m) % _rep_((*j)) );
	quickSort(m,j,comparer);
	quickSort(j+1,n+1,comparer);
#ifdef	DEBUG_SORT
	LOG(BF("After sort list is now: "));
	for ( _RandomAccessIterator ii=m; ii<=n; ii++ )
	{
	    LOG(BF("element %d = %s") % (ii-m) % (*ii)->__repr__() );
	}
#endif
    }
}



template <typename _RandomAccessIterator>
    void quickSort( _RandomAccessIterator m, _RandomAccessIterator en )
{_G();
    _RandomAccessIterator k;
    _RandomAccessIterator n = en-1;
    typedef typename std::iterator_traits<_RandomAccessIterator>::value_type _ValueType;
    if ( m < n )
    {
	int half = (n-m);
	half = half/2;
        k = m+half;	// pivot
	swap<_ValueType>(*m,*k);
	_RandomAccessIterator i = m+1;
	_RandomAccessIterator j = n;
	while ( i <= j )
	{
	    while ( ( i <= n ) && ((*i)<=(*m)) ) i++;
	    while ( ( j >= m ) && ((*j)>(*m)) ) j--;
	    if ( i < j ) 
	    {
		swap<_ValueType>(*i,*j);
	    }
	}
	swap<_ValueType>(*m,*j);
	quickSort(m,j);
	quickSort(j+1,n+1);
    }
}


template <class Oit>
void reverse(Oit m, Oit n )
{
    n--;
    while ( m < n )
    {
	swap(*m,*n);
	m++;
	n--;
    }
}

};
};
#endif //]
