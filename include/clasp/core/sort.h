/*
    File: sort.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
#ifndef sort_H //[
#define sort_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>

//#define	DEBUG_SORT

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
    void swap(Oelement &x, Oelement &y) {
    Oelement t;
    t = x;
    x = y;
    y = t;
  };

  template <typename _RandomAccessIterator, typename Ocomp>
    void quickSort(_RandomAccessIterator m, _RandomAccessIterator en, Ocomp comparer) {
    _RandomAccessIterator k;
    _RandomAccessIterator n = en - 1;
    if ((en-m) <= 1) return;
  //  typedef typename std::iterator_traits<_RandomAccessIterator>::value_type _ValueType;
    typedef typename _RandomAccessIterator::value_type _ValueType;
    if (m < n) {
#ifdef DEBUG_SORT
      LOG(BF("Sorting list at start"));
      for (_RandomAccessIterator ii = m; ii <= n; ii++) {
        LOG(BF("element %d = %s") % (ii - m) % (*ii)->__repr__());
      }
#endif
      ssize_t half = (n - m);
      half = half / 2;
      k = m + half; // pivot
      swap<_ValueType>(*m, *k);
#ifdef DEBUG_SORT
      LOG(BF("Sorting list pivot is now first element: "));
      for (_RandomAccessIterator ii = m; ii <= n; ii++) {
        LOG(BF("element %d = %s") % (ii - m) % (*ii)->__repr__());
      }
#endif
      _RandomAccessIterator i = m + 1;
      _RandomAccessIterator j = n;
      while (i <= j) {
        _BLOCK_TRACEF(BF("Top of while i(%d) <= j(%d)") % (i - m) % (j - m));
        while ((i <= n) && (comparer(*i, *m))) {
          LOG(BF("skipping lower bin index: %d value: %s") % (i - m) % _rep_((*i)));
          i++;
        }
        while ((j >= i) && (!comparer(*j, *m))) {
          LOG(BF("skipping upper bin index: %d value: %s") % (j - m) % _rep_((*j)));
          j--;
        }
        if (i < j) {
          LOG(BF("swapping value lower index: %d value: %s") % (i - m) % _rep_((*i)));
          LOG(BF("swapping value upper index: %d value: %s") % (j - m) % _rep_((*j)));
          swap<_ValueType>(*i, *j);
        }
      }
      swap<_ValueType>(*m, *j);
#ifdef DEBUG_SORT
      LOG(BF("After pivot list is now: "));
      for (_RandomAccessIterator ii = m; ii <= n; ii++) {
        LOG(BF("element %d = %s") % (ii - m) % (*ii)->__repr__());
      }
#endif
      LOG(BF("element at j -- index: %d value: %s") % (j - m) % _rep_((*j)));
      quickSort(m, j, comparer);
      quickSort(j + 1, n + 1, comparer);
#ifdef DEBUG_SORT
      LOG(BF("After sort list is now: "));
      for (_RandomAccessIterator ii = m; ii <= n; ii++) {
        LOG(BF("element %d = %s") % (ii - m) % (*ii)->__repr__());
      }
#endif
    }
  }

  template <typename _RandomAccessIterator>
    void quickSort(_RandomAccessIterator m, _RandomAccessIterator en) {
    _G();
    _RandomAccessIterator k;
    _RandomAccessIterator n = en - 1;
    if ((en-m)<=1) return;
    typedef typename std::iterator_traits<_RandomAccessIterator>::value_type _ValueType;
    if (m < n) {
      ssize_t half = (n - m);
      half = half / 2;
      k = m + half; // pivot
      swap<_ValueType>(*m, *k);
      _RandomAccessIterator i = m + 1;
      _RandomAccessIterator j = n;
      while (i <= j) {
        while ((i <= n) && ((*i) <= (*m)))
          i++;
        while ((j >= m) && ((*j) > (*m)))
          j--;
        if (i < j) {
          swap<_ValueType>(*i, *j);
        }
      }
      swap<_ValueType>(*m, *j);
      quickSort(m, j);
      quickSort(j + 1, n + 1);
    }
  }

  template <class Oit>
    void reverse(Oit m, Oit n) {
    n--;
    while (m < n) {
      swap(*m, *n);
      m++;
      n--;
    }
  }




  template <typename ValueType, typename Ocomp>
    void quickSortVec0(gctools::Vec0<ValueType>& array,ssize_t m, ssize_t en, Ocomp comparer) {
    ssize_t k;
    ssize_t n = en - 1;
    if ((en-m)<=1) return;
  //  typedef typename std::iterator_traits<_RandomAccessIterator>::value_type _ValueType;
    if (m < n) {
#if 0
      printf("%s:%d  Sorting from m=%d n=%d\n", __FILE__, __LINE__, m,  n);
      for (ssize_t ii = m; ii <= n; ii++) {
        printf("%s:%d  element %d = %s\n", __FILE__, __LINE__, ii,  _rep_(array[ii]).c_str());
      }
#endif
      ssize_t half = (n - m);
      half = half / 2;
      k = m + half; // pivot
      swap<ValueType>(array[m], array[k]);
      ssize_t i = m + 1;
      ssize_t j = n;
      while (i <= j) {
        while ((i <= n) && (comparer(array[i], array[m]))) i++;
        while ((j >= i) && (!comparer(array[j], array[m]))) j--;
        if (i < j) swap<ValueType>(array[i], array[j]);
      }
      swap<ValueType>(array[m], array[j]);
#if 0
      printf("%s:%d  After separate from m=%d j=%d n=%d\n", __FILE__, __LINE__, m, j,  n);
      for (ssize_t ii = m; ii <= n; ii++) {
        printf("%s:%d  element %d = %s\n", __FILE__, __LINE__, ii,  _rep_(array[ii]).c_str());
      }
#endif
      quickSortVec0(array, m, j, comparer);
      quickSortVec0(array, j + 1, n + 1, comparer);
    }
  }


 // The default sorter, increasing order
  template <typename ValueType>
    void quickSortVec0(gctools::Vec0<ValueType>& array,ssize_t m, ssize_t en) {
    if ((en-m)<=1) return;
    ssize_t k;
    ssize_t n = en - 1;
  //  typedef typename std::iterator_traits<_RandomAccessIterator>::value_type _ValueType;
    if (m < n) {
      ssize_t half = (n - m);
      half = half / 2;
      k = m + half; // pivot
      swap<ValueType>(array[m], array[k]);
      ssize_t i = m + 1;
      ssize_t j = n;
      while (i <= j) {
        while ((i <= n) && ( array[i] < array[m] )) i++;
        while ((j >= i) && (!(array[j] < array[m]))) j--;
        if (i < j) swap<ValueType>(array[i], array[j]);
      }
      swap<ValueType>(array[m], array[j]);
      quickSortVec0(array, m, j);
      quickSortVec0(array, j + 1, n + 1);
    }
  }


   // The default sorter, increasing order
  template <typename ValueType>
    void quickSortMemory(ValueType* array,ssize_t m, ssize_t en) {
    if ((en-m) <= 1) return;
    ssize_t k;
    ssize_t n = en - 1;
  //  typedef typename std::iterator_traits<_RandomAccessIterator>::value_type _ValueType;
    if (m < n) {
      ssize_t half = (n - m);
      half = half / 2;
      k = m + half; // pivot
      swap<ValueType>(array[m], array[k]);
      ssize_t i = m + 1;
      ssize_t j = n;
      while (i <= j) {
        while ((i <= n) && ( array[i] < array[m] )) i++;
        while ((j >= i) && (!(array[j] < array[m]))) j--;
        if (i < j) swap<ValueType>(array[i], array[j]);
      }
      swap<ValueType>(array[m], array[j]);
      quickSortMemory(array, m, j);
      quickSortMemory(array, j + 1, n + 1);
    }
  }

};
#endif //]
