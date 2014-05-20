#ifndef	ForceField_profiler_H
#define	ForceField_profiler_H

#include "lightProfiler.h"

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
