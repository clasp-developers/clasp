/*
    File: mpiPackage.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
#include "core/common.h"
#include "core/package.h"
#include "mpiPackage.h"

namespace mpi
{


    void MpiPackage::expose(core::Lisp_sp lisp, WhatToExpose what) const
    {_G();
	switch (what)
	{
	case candoClasses:
	{
	    core::Package_sp cp = _lisp->makePackage(this->packageName());
	    _lisp->inPackage(this->packageName());
	    _lisp->usePackage(CorePkg);
#define ALL_STAGES
#define	Use_MpiPkg
#include "initClasses.inc"
#undef Use_MpiPkg
	    core::Package_sp up = _lisp->getPackage(UserPackage);
	    up->usePackage(cp);
	}
	break;
	case CandoFunctions:
	{
	    // nothing
	}
	break;
	case candoGlobals:
	{_G();
	    setupCandoPrimitives(_lisp);
	    initializeElementsAndHybridization(_lisp);
	    CandoDatabase_sp cdb = CandoDatabase_O::nil(_lisp);
	    CREATE_PREDEFINED_SYMBOL(_sym_candoDatabase,MpiPkg,"*DATABASE*");
	    _lisp->defvar(_lisp->symbol(_sym_candoDatabase),cdb);
	}
	break;
	case pythonClasses:
	{
	    IMPLEMENT_ME();
	}
	break;
	case pythonFunctions:
	{_OF();
	    // nothing
	}
	break;
	case pythonGlobals:
	{_OF();
	    // nothing
	}
	break;
	}
    }




    CandoDatabase_sp getCandoDatabase(const core::Lisp_sp& lisp)
    {_G();
	return _lisp->symbol(_sym_candoDatabase)->symbolValue()->as<CandoDatabase_O>();
    }





};


// Access command line parameters

#if 0
    if ( vm.count("database") )
    {
	dbFileName = vm["database"].as<string>();
	LOG(BF( "About to open database(%s)")% dbFileName );
	TRY()
	{
	    this->loadCandoDatabase(dbFileName,0);
	} catch ( core::Condition& err )
	  {
	      this->error(err.conditionObject(),this->nil<core::Environment_O>());
	  } catch (...)
	    {
		throw;
	    }
    }
#endif
