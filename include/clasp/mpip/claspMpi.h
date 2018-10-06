/*
    File: claspMpi.h
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
#ifndef claspMpi_H //[
#define claspMpi_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>
#ifdef USE_MPI
#include <boost/mpi.hpp>
namespace bmpi = boost::mpi;
#endif
#include <clasp/mpip/mpiPackage.h>

namespace mpip {

FORWARD(Mpi);
class Mpi_O : public core::General_O {
  LISP_CLASS(mpip, MpiPkg, Mpi_O, "Mpi",core::General_O);
private:
  //	bool		_Running;
  int _LastReturnCode;
  int _Source;
  int _Tag;
#ifdef USE_MPI
  boost::mpi::communicator _Communicator;
#endif
public:
  static void initializeGlobals(core::Lisp_sp lisp);

public:
  void initialize();

public:
  /*! Equivalent to MPI_Init(argc,argv)
	 * Returns the new commandLineArguments as a list
	 */
  static void Init(int &argc, char **&argv, bool &mpiEnabled, int &rank, int &msize);

  static Mpi_sp mpiCommWorld();
  /*! Quick way to get the number of processes
	 * If mpi is not enabled, always returns 1
	 */
  static int mpiSize();

  /*! Quick way to get the rank of current process
	 * If mpi is not enabled, always returns 0
	 */
  static int mpiRank();

  //! Like MPI_Finalize()
  static void Finalize();

public:
  /*!Return false if mpi is not enabled, true otherwise */
  static bool mpiEnabled();

  //! Return the sender of the last Recv'd message
  int Get_source();

  //! Return the tag of the last Recv'd message
  int Get_tag();

  //! Return the number of processes communicating through MPI
  int Get_size();
  //! Return the rank of this process
  int Get_rank();

  //! Blocking send an object to the destination
  core::T_sp prim_Send(int dest, int tag, core::T_sp obj);

  /*! Blocking receive an object, return (values object source tag)
	  Alternatively you can recover the source and tag using (get-source (mpi-comm-world))
	  or (get-tag (mpi-comm-world)) */

  core::T_mv prim_Recv(int source, int tag);

  DEFAULT_CTOR_DTOR(Mpi_O);
};
};

#endif //]
