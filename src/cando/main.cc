/*
    File: main.cc
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
#define	DEBUG_LEVEL_FULL

#include <string>
#include "core/foundation.h"
#include "core/bundle.h"
#include "core/object.h"
#include "core/lisp.h"
#include "core/archiveNode.h"
#include "core/candoOpenMp.h"
#include "core/cons.h"
#include "packageManager/packageManager.h"


int main(int argc, char* argv[] )
{	// Do not touch debug log until after MPI init

    int maxThreads = 1;
#pragma omp parallel shared(maxThreads)
    {
#pragma omp single 
	{
	    maxThreads = core::cando_omp_get_num_threads();
	}
    }
		// 
		// Start processing 
		//
    core::LispHolder lispHolder(false,0,1);
    core::Lisp_sp _lisp = lispHolder.lisp();


    try
    {_BLOCK_TRACE("main loop");
        lispHolder.startup(argc, argv, "CANDO_APP");
	installAllCommonPackages(_lisp);
	_lisp->run();
	return 0;
    } catch ( core::ExitProgram& ee )
    {
        // Do nothing
        printf("Caught ExitProgram in %s:%d\n", __FILE__, __LINE__);
    } catch ( core::TerminateProgramIfBatch& ee )
    {
        // Do nothing
        printf("Caught TerminateProgramIfBatch in %s:%d\n", __FILE__, __LINE__);
    } catch (core::Condition& ee)
    {
        printf("Caught Condition at %s:%d - %s\n", __FILE__, __LINE__, ee.message().c_str() );
        printf("Stack trace:\n%s", ee.conditionObject()->getStackTraceDump().c_str() );
    }
    catch (core::HardError& ee)
    {
	_lisp->print(BF("At %s:%d - HardError caught: %s") % __FILE__ % __LINE__ % ee.message() );
    }
    catch ( ... )
    {
    	_lisp->print(BF("Unknown exception in main - everything should be caught lower down %s:%d") % __FILE__ % __LINE__);
    }
    _lisp->print(BF("CANDO terminated normally - exiting main"));
    return 0;
}
