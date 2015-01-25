/*
    File: profiler.h
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
#ifndef	ForceField_profiler_H
#define	ForceField_profiler_H

#include <clasp/core/lightProfiler.h>

namespace core {

    extern void initializeProfiler(LightProfiler& profiler, const Lisp_sp& lisp);

extern	int	timerOverall;
extern	int	timerPreconditioner;
extern	int	timerPreconditionerSetup;
extern	int	timerPreconditionerSolver;
extern	int	timerPreconditionerSolverFactor;
extern	int	timerPreconditionerSolverFactorFocus;
extern	int	timerPreconditionerSolverBackSubstitute;
extern	int	timerEnergy;
extern	int	timerBondAngleDihedral;
extern	int	timerBond;
extern	int	timerAngle;
extern	int	timerDihedral;
extern	int	timerNonbond;
extern	int	timerNonbondGradient;
extern	int	timerNonbondEnergy;
extern	int	timerNonbondSub;
extern	int	timerImproperRestraint;
extern	int	timerChiralRestraint;
extern	int	timerAnchorRestraint;
extern	int	timerFixedNonbondRestraint;

extern	int	forcesGreaterThan10000;
#if 0
extern	void	disableProfiler();
#endif
};
#endif
