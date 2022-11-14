/*
    File: mmtkGarbageCollection.h
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
#ifndef _clasp_mmtkGarbageCollection_H
#define _clasp_mmtkGarbageCollection_H


namespace gctools {

  // Given an interior_pointer
  // Return true and the base object if the interior_pointer points into an object
  // Return false and undefined for object if it does not.
  // This is the General_O object case
template <typename GeneralType>
inline bool tagged_pointer_from_interior_pointer( clasp_ptr_t interior_pointer, Tagged& tagged_pointer ) {
  printf("%s:%d:%s What do I do here?\n", __FILE__, __LINE__, __FUNCTION__ );
  return false;
}

  // core::Cons_sp specializer
template <>
inline bool tagged_pointer_from_interior_pointer<core::Cons_O>( clasp_ptr_t interior_pointer, Tagged& tagged_pointer ) {
  printf("%s:%d:%s Need support for interior pointers.\n", __FILE__, __LINE__, __FUNCTION__ );
  return false;
}

};

namespace gctools {

int initializeMmtk( int argc, char *argv[], bool mpiEnabled, int mpiRank, int mpiSize);

};

#endif // _clasp_mmtkGarbageCollection_H
