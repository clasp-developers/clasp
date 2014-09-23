/*
    File: profiler.cc
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

#include "foundation.h"
#include "executables.fwd.h"
#include "lisp.h"
#include "lightProfiler.h"

#include "profiler.h"

namespace core {


int	timerLibrary;
int	timerPreconditioner;
int	timerPreconditionerSetup;
int	timerPreconditionerSolver;
int	timerPreconditionerSolverFactor;
int	timerPreconditionerSolverFactorFocus;
int	timerPreconditionerSolverBackSubstitute;
int	timerEnergy;
int	timerBondAngleDihedral;
int	timerBond;
int	timerAngle;
int	timerDihedral;
int	timerNonbond;
int	timerNonbondEnergy;
int	timerNonbondGradient;
int	timerNonbondSub;
int	timerImproperRestraint;
int	timerChiralRestraint;
int	timerAnchorRestraint;
int	timerFixedNonbondRestraint;
int	forcesGreaterThan10000;



    void initializeProfiler(LightProfiler& profiler, const Lisp_sp& lisp)
    {_G();
        LOG(BF("Initializing this") );
	timerLibrary = profiler.createTimer(0,"Force field library");
	timerPreconditioner = profiler.createTimer(timerLibrary,"Preconditioning");
	timerPreconditionerSetup = profiler.createTimer(timerPreconditioner,"Preconditioning setup");
	timerPreconditionerSolver = profiler.createTimer(timerPreconditioner,"Preconditioning solver");
	timerPreconditionerSolverFactor = profiler.createTimer(timerPreconditionerSolver,"Matrix factorize");
	timerPreconditionerSolverFactorFocus = profiler.createTimer(timerPreconditionerSolverFactor,"Matrix factorize focus");
	timerPreconditionerSolverBackSubstitute = profiler.createTimer(timerPreconditionerSolver,"Back substitute");
	timerEnergy = profiler.createTimer(timerLibrary,"Total energy/force");
	timerBondAngleDihedral = profiler.createTimer(timerEnergy,"Bond/Angle/Dihedral energy");
	timerBond = profiler.createTimer(timerBondAngleDihedral,"Bond energy");
	timerAngle = profiler.createTimer(timerBondAngleDihedral,"Angle energy");
	timerDihedral = profiler.createTimer(timerBondAngleDihedral,"Dihedral energy");
	timerNonbond = profiler.createTimer(timerEnergy,"Nonbond energy/gradient");
	timerNonbondEnergy = profiler.createTimer(timerNonbond,"Nonbond energy");
	timerNonbondGradient = profiler.createTimer(timerNonbond,"Nonbond gradient");
	timerNonbondSub = profiler.createTimer(timerNonbond,"Nonbond sub");
	timerImproperRestraint = profiler.createTimer(timerEnergy,"Improper restraint energy");
	timerChiralRestraint = profiler.createTimer(timerEnergy,"Chiral restraint energy");
	timerAnchorRestraint = profiler.createTimer(timerEnergy,"Anchor restraint energy");
	timerFixedNonbondRestraint = profiler.createTimer(timerEnergy,"Fixed nonbond restraint energy");
	forcesGreaterThan10000 = profiler.createEventCounter("Forces greater than 10000");
	profiler.timer(timerLibrary).start();
    }


};
