#pragma once
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

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>

// #define	DEBUG_SORT

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

template <typename _RandomAccessIterator> struct SortWork {
  _RandomAccessIterator _Begin;
  _RandomAccessIterator _End;
  SortWork(_RandomAccessIterator b, _RandomAccessIterator e) : _Begin(b), _End(e){};
  SortWork(){};
};

template <typename Oelement> void swap(Oelement& x, Oelement& y) {
  Oelement t(x);
  x = y;
  y = t;
};

template <typename _RandomAccessIterator, typename Ocomp>
void quickSort(_RandomAccessIterator m, _RandomAccessIterator en, Ocomp comparer) {
  std::vector<SortWork<_RandomAccessIterator>> work;
  work.emplace_back(m, en);
  while (work.size() > 0) {
    SortWork<_RandomAccessIterator> one(work.back());
    work.pop_back();
    m = one._Begin;
    en = one._End;
    _RandomAccessIterator k;
    _RandomAccessIterator n = en - 1;
    if ((en - m) <= 1)
      continue;
    //  typedef typename std::iterator_traits<_RandomAccessIterator>::value_type _ValueType;
    typedef typename _RandomAccessIterator::value_type _ValueType;
    if (m < n) {
#ifdef DEBUG_SORT
      LOG(("Sorting list at start"));
      for (_RandomAccessIterator ii = m; ii <= n; ii++) {
        LOG(("element %d = %s"), (ii - m), (*ii)->__repr__());
      }
#endif
      ssize_t half = (n - m);
      half = half / 2;
      k = m + half; // pivot
      swap<_ValueType>(*m, *k);
#ifdef DEBUG_SORT
      LOG(("Sorting list pivot is now first element: "));
      for (_RandomAccessIterator ii = m; ii <= n; ii++) {
        LOG(("element %d = %s"), (ii - m), (*ii)->__repr__());
      }
#endif
      _RandomAccessIterator i = m + 1;
      _RandomAccessIterator j = n;
      while (i <= j) {
        while ((i <= n) && (comparer(*i, *m))) {
          LOG(("skipping lower bin index: %d value: %s"), (i - m), _rep_((*i)));
          i++;
        }
        while ((j >= i) && (!comparer(*j, *m))) {
          LOG(("skipping upper bin index: %d value: %s"), (j - m), _rep_((*j)));
          j--;
        }
        if (i < j) {
          LOG(("swapping value lower index: %d value: %s"), (i - m), _rep_((*i)));
          LOG(("swapping value upper index: %d value: %s"), (j - m), _rep_((*j)));
          swap<_ValueType>(*i, *j);
        }
      }
      swap<_ValueType>(*m, *j);
#ifdef DEBUG_SORT
      LOG(("After pivot list is now: "));
      for (_RandomAccessIterator ii = m; ii <= n; ii++) {
        LOG(("element %d = %s"), (ii - m), (*ii)->__repr__());
      }
#endif
      LOG(("element at j -- index: %d value: %s"), (j - m), _rep_((*j)));
      work.emplace_back(m, j);
      work.emplace_back(j + 1, n + 1);
#ifdef DEBUG_SORT
      LOG(("After sort list is now: "));
      for (_RandomAccessIterator ii = m; ii <= n; ii++) {
        LOG(("element %d = %s"), (ii - m), (*ii)->__repr__());
      }
#endif
    }
  }
}

template <typename _RandomAccessIterator, typename Ocomp>
void quickSortFirstCheckOrder(_RandomAccessIterator m, _RandomAccessIterator en, Ocomp comparer) {
  std::vector<SortWork<_RandomAccessIterator>> work;
  work.emplace_back(m, en);
  while (work.size() > 0) {
    SortWork<_RandomAccessIterator> one(work.back());
    work.pop_back();
    m = one._Begin;
    en = one._End;
    _RandomAccessIterator k;
    _RandomAccessIterator n = en - 1;
    // First check if in order already
    if ((en - m) <= 1)
      continue;
    bool ordered = true;
    for (auto ii = m; ii < n; ii++) {
      ordered &= comparer(*ii, *(ii + 1));
      if (!ordered)
        break;
    }
    if (ordered)
      continue;
    typedef typename _RandomAccessIterator::value_type _ValueType;
    if (m < n) {
#ifdef DEBUG_SORT
      LOG(("Sorting list at start"));
      for (_RandomAccessIterator ii = m; ii <= n; ii++) {
        LOG(("element %d = %s"), (ii - m), (*ii)->__repr__());
      }
#endif
      ssize_t half = (n - m);
      half = half / 2;
      k = m + half; // pivot
      swap<_ValueType>(*m, *k);
#ifdef DEBUG_SORT
      LOG(("Sorting list pivot is now first element: "));
      for (_RandomAccessIterator ii = m; ii <= n; ii++) {
        LOG(("element %d = %s"), (ii - m), (*ii)->__repr__());
      }
#endif
      _RandomAccessIterator i = m + 1;
      _RandomAccessIterator j = n;
      while (i <= j) {
        while ((i <= n) && (comparer(*i, *m))) {
          i++;
        }
        while ((j >= i) && (!comparer(*j, *m))) {
          j--;
        }
        if (i < j) {
          swap<_ValueType>(*i, *j);
        }
      }
      swap<_ValueType>(*m, *j);
#ifdef DEBUG_SORT
      LOG(("After pivot list is now: "));
      for (_RandomAccessIterator ii = m; ii <= n; ii++) {
        LOG(("element %d = %s"), (ii - m), (*ii)->__repr__());
      }
#endif
      work.emplace_back(m, j);
      work.emplace_back(j + 1, n + 1);
      // quickSortFirstCheckOrder(m, j, comparer, start, debug );
      // quickSortFirstCheckOrder(j + 1, n + 1, comparer, start, debug);
#ifdef DEBUG_SORT
      LOG(("After sort list is now: "));
      for (_RandomAccessIterator ii = m; ii <= n; ii++) {
        LOG(("element %d = %s"), (ii - m), (*ii)->__repr__());
      }
#endif
    }
  }
}

#if 0
template <typename _RandomAccessIterator, typename Ocomp>
__attribute__((optnone))
void quickSortDebugDepth(_RandomAccessIterator m, _RandomAccessIterator en, Ocomp comparer, size_t depth) {
  printf("%s:%d:%s  number of entries: %lu\n", __FILE__, __LINE__, __FUNCTION__, (en - m) );
  if (depth==0) {
    printf("%s:%d:%s Hit max depth - figure out why\n", __FILE__, __LINE__, __FUNCTION__ );
    abort();
  }
  _RandomAccessIterator k;
  _RandomAccessIterator n = en - 1;
  if ((en-m) <= 1) return;
  //  typedef typename std::iterator_traits<_RandomAccessIterator>::value_type _ValueType;
  typedef typename _RandomAccessIterator::value_type _ValueType;
  if (m < n) {
#ifdef DEBUG_SORT
    LOG(("Sorting list at start"));
    for (_RandomAccessIterator ii = m; ii <= n; ii++) {
      LOG(("element %d = %s") , (ii - m) , (*ii)->__repr__());
    }
#endif
    ssize_t half = (n - m);
    half = half / 2;
    k = m + half; // pivot
    swap<_ValueType>(*m, *k);
#ifdef DEBUG_SORT
    LOG(("Sorting list pivot is now first element: "));
    for (_RandomAccessIterator ii = m; ii <= n; ii++) {
      LOG(("element %d = %s") , (ii - m) , (*ii)->__repr__());
    }
#endif
    _RandomAccessIterator i = m + 1;
    _RandomAccessIterator j = n;
    while (i <= j) {
      while ((i <= n) && (comparer(*i, *m))) {
        LOG(("skipping lower bin index: %d value: %s") , (i - m) , _rep_((*i)));
        i++;
      }
      while ((j >= i) && (!comparer(*j, *m))) {
        LOG(("skipping upper bin index: %d value: %s") , (j - m) , _rep_((*j)));
        j--;
      }
      if (i < j) {
        LOG(("swapping value lower index: %d value: %s") , (i - m) , _rep_((*i)));
        LOG(("swapping value upper index: %d value: %s") , (j - m) , _rep_((*j)));
        swap<_ValueType>(*i, *j);
      }
    }
    swap<_ValueType>(*m, *j);
#ifdef DEBUG_SORT
    LOG(("After pivot list is now: "));
    for (_RandomAccessIterator ii = m; ii <= n; ii++) {
      LOG(("element %d = %s") , (ii - m) , (*ii)->__repr__());
    }
#endif
    LOG(("element at j -- index: %d value: %s") , (j - m) , _rep_((*j)));
    quickSortDebugDepth(m, j, comparer,depth-1);
    quickSortDebugDepth(j + 1, n + 1, comparer,depth-1);
#ifdef DEBUG_SORT
    LOG(("After sort list is now: "));
    for (_RandomAccessIterator ii = m; ii <= n; ii++) {
      LOG(("element %d = %s") , (ii - m) , (*ii)->__repr__());
    }
#endif
  }
}
#endif

template <typename _RandomAccessIterator> void quickSort(_RandomAccessIterator m, _RandomAccessIterator en) {
  std::vector<SortWork<_RandomAccessIterator>> work;
  work.emplace_back(m, en);
  while (work.size() > 0) {
    SortWork<_RandomAccessIterator> one(work.back());
    work.pop_back();
    m = one._Begin;
    en = one._End;
    _RandomAccessIterator k;
    _RandomAccessIterator n = en - 1;
    if ((en - m) <= 1)
      continue;
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
      work.emplace_back(m, j);
      work.emplace_back(j + 1, n + 1);
    }
  }
}

template <class Oit> void reverse(Oit m, Oit n) {
  n--;
  while (m < n) {
    swap(*m, *n);
    m++;
    n--;
  }
}

template <typename ValueType, typename Ocomp>
void quickSortVec0(gctools::Vec0<ValueType>& array, ssize_t m, ssize_t en, Ocomp comparer) {
  std::vector<SortWork<size_t>> work;
  work.emplace_back(m, en);
  while (work.size() > 0) {
    SortWork<size_t> one(work.back());
    work.pop_back();
    m = one._Begin;
    en = one._End;
    ssize_t k;
    ssize_t n = en - 1;
    if ((en - m) <= 1)
      continue;
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
        while ((i <= n) && (comparer(array[i], array[m])))
          i++;
        while ((j >= i) && (!comparer(array[j], array[m])))
          j--;
        if (i < j)
          swap<ValueType>(array[i], array[j]);
      }
      swap<ValueType>(array[m], array[j]);
#if 0
      printf("%s:%d  After separate from m=%d j=%d n=%d\n", __FILE__, __LINE__, m, j,  n);
      for (ssize_t ii = m; ii <= n; ii++) {
        printf("%s:%d  element %d = %s\n", __FILE__, __LINE__, ii,  _rep_(array[ii]).c_str());
      }
#endif
      work.emplace_back(m, j);
      work.emplace_back(j + 1, n + 1);
    }
  }
}

// The default sorter, increasing order
template <typename ValueType> void quickSortVec0(gctools::Vec0<ValueType>& array, ssize_t m, ssize_t en) {
  std::vector<SortWork<size_t>> work;
  work.emplace_back(m, en);
  while (work.size() > 0) {
    SortWork<size_t> one(work.back());
    work.pop_back();
    m = one._Begin;
    en = one._End;
    if ((en - m) <= 1)
      continue;
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
        while ((i <= n) && (array[i] < array[m]))
          i++;
        while ((j >= i) && (!(array[j] < array[m])))
          j--;
        if (i < j)
          swap<ValueType>(array[i], array[j]);
      }
      swap<ValueType>(array[m], array[j]);
      work.emplace_back(m, j);
      work.emplace_back(j + 1, n + 1);
    }
  }
}

// The default sorter, increasing order
template <typename ValueType> void quickSortMemory(ValueType* array, ssize_t m, ssize_t en) {
  std::vector<SortWork<size_t>> work;
  work.emplace_back(m, en);
  while (work.size() > 0) {
    SortWork<size_t> one(work.back());
    work.pop_back();
    m = one._Begin;
    en = one._End;
    if ((en - m) <= 1)
      continue;
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
        while ((i <= n) && (array[i] < array[m]))
          i++;
        while ((j >= i) && (!(array[j] < array[m])))
          j--;
        if (i < j)
          swap<ValueType>(array[i], array[j]);
      }
      swap<ValueType>(array[m], array[j]);
      work.emplace_back(m, j);
      work.emplace_back(j + 1, n + 1);
    }
  }
}

}; // namespace sort
