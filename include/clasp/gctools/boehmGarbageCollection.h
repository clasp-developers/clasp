/*
    File: boehmGarbageCollection.h
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
#ifndef _clasp_boehmGarbageCollection_H
#define _clasp_boehmGarbageCollection_H

/*! Return the most derived pointer of the object pointed to by the smart_ptr */
#define GC_BASE_ADDRESS_FROM_SMART_PTR(_smartptr_) (dynamic_cast<void *>(_smartptr_.px_ref()))
#define GC_BASE_ADDRESS_FROM_PTR(_ptr_) (const_cast<void *>(dynamic_cast<const void *>(_ptr_)))


namespace gctools {
  void* boehm_create_shadow_table(size_t nargs);
};

namespace gctools {
/*! Initialize the memory pool system and call the startup function which
      has the type: int startup(int argc, char* argv[]) just like main.
      Also pass an optional object-format for MPS
    */
int initializeMemoryManagement(MainFunctionType startup, int argc, char *argv[], void *dummy);
};

namespace gctools {
  // Looks like an MPS mps_ld_s structure
  // So that field_offset table for MPS can be used by boehm
  // Instance variable must be called _LocationDependency
 struct BogusBoehmLocationDependencyTracker {
    unsigned long _epoch;
    unsigned long _rs;
  };


};

namespace gctools {

  void boehm_set_finalizer_list(gctools::Tagged object, gctools::Tagged finalizers );
  void boehm_clear_finalizer_list(gctools::Tagged object);

  void clasp_warn_proc(char *msg, GC_word arg);

  int initializeBoehm(MainFunctionType startupFn, int argc, char *argv[], bool mpiEnabled, int mpiRank, int mpiSize);

};

extern "C" {
// Do the same thing that mps_park and mps_release do
void boehm_park();
void boehm_release();
};
#endif // _clasp_boehmGarbageCollection_H
