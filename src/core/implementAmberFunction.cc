/*
    File: implementAmberFunction.cc
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

class FiniteDifferenceMismatch {
public:
  string functionName;
  string termName;
  int index;
  string getError() { return "FiniteDifferenceMismatch"; } //  @"+this->functionName()+" term:("+this->termName+")";};
};

#define delta2 0.00001

#undef TEST_FORCE
#define TEST_FORCE(func, delta, argLow, argHigh, term, idx)    \
  {                                                            \
    double eLow = this->func argLow;                           \
    double eHigh = this->func argHigh;                         \
    double numForce = -(eHigh - eLow) / (delta);               \
    if (!_areValuesClose(numForce, term, #func, #term, idx)) { \
      fails++;                                                 \
    };                                                         \
  }

#undef TEST_DIAGONAL_HESSIAN
#define TEST_DIAGONAL_HESSIAN(func, delta, argLow, argMiddle, argHigh, term, idx)             \
  {                                                                                           \
    double eLow = this->func argLow;                                                          \
    double eMiddle = this->func argMiddle;                                                    \
    double eHigh = this->func argHigh;                                                        \
    double numHessian = (eHigh + eLow - 2.0 * eMiddle) / (((delta) / 2.0) * ((delta) / 2.0)); \
    if (!_areValuesClose(numHessian, term, #func, #term, idx)) {                              \
      fails++;                                                                                \
    };                                                                                        \
  }

#undef TEST_OFF_DIAGONAL_HESSIAN
#define TEST_OFF_DIAGONAL_HESSIAN(func, delta, argMM, argPM, argMP, argPP, term, idx) \
  {                                                                                   \
    double eMM = this->func argMM;                                                    \
    double eMP = this->func argMP;                                                    \
    double ePM = this->func argPM;                                                    \
    double ePP = this->func argPP;                                                    \
    double numHessian = ((ePP - ePM) - (eMP - eMM)) / (delta * delta);                \
    if (!_areValuesClose(numHessian, term, #func, #term, idx)) {                      \
      fails++;                                                                        \
    };                                                                                \
  }

//
// Interface to amberFunction.cc
//

void EnergyFunction_O::setupHessianPreconditioner(core::NVector_sp nvPosition,
                                                  core::AbstractLargeSquareMatrix_sp m) {
  _G();
  _lisp->profiler().timer(timerPreconditioner).start();
  _lisp->profiler().timer(timerPreconditionerSetup).start();
  bool calcForce = true;
  bool calcDiagonalHessian = true;
  bool calcOffDiagonalHessian = true;

  m->fill(0.0);

  this->_Stretch->setupHessianPreconditioner(nvPosition, m);
  this->_Angle->setupHessianPreconditioner(nvPosition, m);
  this->_Dihedral->setupHessianPreconditioner(nvPosition, m);
  // Nonbond doesn't contribute to hessian preconditioner
  //    this->_Nonbond->setupHessianPreconditioner(nvPosition, m );
  this->_ImproperRestraint->setupHessianPreconditioner(nvPosition, m);
  this->_ChiralRestraint->setupHessianPreconditioner(nvPosition, m);
  this->_AnchorRestraint->setupHessianPreconditioner(nvPosition, m);

  _lisp->profiler().timer(timerPreconditionerSetup).stop();
  _lisp->profiler().timer(timerPreconditioner).stop();
}

int EnergyFunction_O::countTermsBeyondThreshold() {
  int terms;
  terms = 0;
  SIMPLE_ERROR(BF("Should there be something here?"));
  //    node = rawAccumulateTermsBeyondThresholdAsXml(terms);
  return terms;
}

//

double EnergyFunction_O::evaluateAll(
    core::NVector_sp pos,
    bool calcForce,
    core::NVector_sp force,
    bool calcDiagonalHessian,
    bool calcOffDiagonalHessian,
    core::AbstractLargeSquareMatrix_sp hessian,
    core::NVector_sp hdvec, core::NVector_sp dvec) {
  _G();

#if 0 //[
#define ForceAcc(i, o, v)                                               \
  {                                                                     \
    if (hasForce) {                                                     \
      force->setElement((i) + (o), (v) + force->getElement((i) + (o))); \
    }                                                                   \
  }

//
// Accumulate an off diagonal Hessian element
//
#define OffDiagHessAcc(i1, o1, i2, o2, v)                               \
  {                                                                     \
    if (hasHessian) {                                                   \
      hessian->addToElement((i1) + (o1), (i2) + (o2), v);               \
    }                                                                   \
    if (hasHdAndD) {                                                    \
      hdvec->addToElement((i1) + (o1), v * dvec->element((i2) + (o2))); \
      hdvec->addToElement((i2) + (o2), v * dvec->element((i1) + (o1))); \
    }                                                                   \
  }

//
// Accumulate a diagonal Hessian element
//
#define DiagHessAcc(i1, o1, i2, o2, v)                                  \
  {                                                                     \
    if (hasHessian) {                                                   \
      hessian->addToElement((i1) + (o1), (i2) + (o2), v);               \
    }                                                                   \
    if (hasHdAndD) {                                                    \
      hdvec->addToElement((i1) + (o1), v * dvec->element((i1) + (o1))); \
    }                                                                   \
  }
#endif //]

  bool fail = false;
  ANN(force);
  ANN(hessian);
  ANN(hdvec);
  ANN(dvec);
  bool hasForce = force.notnilp();
  bool hasHessian = hessian.notnilp();
  bool hasHdAndD = (hdvec.notnilp()) && (dvec.notnilp());

#ifdef DEBUG_ON //[
                // Summarize entry state for debugging
  LOG(BF("calcForce = %d") % calcForce);
  LOG(BF("calcDiagonalHessian = %d") % calcDiagonalHessian);
  LOG(BF("calcOffDiagonalHessian = %d") % calcOffDiagonalHessian);
  LOG(BF("hasForce = %d") % hasForce);
  LOG(BF("hasHessian = %d") % hasHessian);
  LOG(BF("hasHdAndD = %d") % hasHdAndD);
  if (hasForce && force->size() < pos->size()) {
    SIMPLE_ERROR(BF("Force does not have the necessary dimensions"));
  }
#endif //]

  if (!calcForce && (calcDiagonalHessian || calcOffDiagonalHessian)) {
    SIMPLE_ERROR(BF("Inconsistant arguments: if you want to calcDiagonalHessian or calcOffDiagonalHessian you must calcForce"));
  }
  if (!calcDiagonalHessian & calcOffDiagonalHessian) {
    SIMPLE_ERROR(BF("Inconsistant arguments: if you want to calcOffDiagonalHessian you must calcDiagonalHessian"));
  }

  profiler.pushTimerStates();
  try {
    ALL_ENERGY_COMPONENTS(zeroEnergy());

    if (hasForce)
      force->zero();
    if (hasHessian)
      hessian->zero();
    if (hasHdAndD) {
      LOG(BF("Zeroing hdvec"));
      hdvec->zero(); // Zero the result
    }

    LOG(BF("Starting evaluation of energy"));
    _lisp->profiler().timer(timerEnergy).start();

    _lisp->profiler().timer(timerBondAngleDihedral).start();

    //
    // Evaluate the stretch term
    //
    _lisp->profiler().timer(timerBond).start();
    this->_Stretch->evaluateAll(pos, calcForce, force,
                                calcDiagonalHessian,
                                calcOffDiagonalHessian,
                                hessian, hdvec, dvec);
    _lisp->profiler().timer(timerBond).stop();

    _lisp->profiler().timer(timerAngle).start();
    this->_Angle->evaluateAll(pos, calcForce, force,
                              calcDiagonalHessian,
                              calcOffDiagonalHessian,
                              hessian, hdvec, dvec);
    _lisp->profiler().timer(timerAngle).stop();

    _lisp->profiler().timer(timerDihedral).start();
    this->_Dihedral->evaluateAll(pos, calcForce, force,
                                 calcDiagonalHessian,
                                 calcOffDiagonalHessian,
                                 hessian, hdvec, dvec);
    _lisp->profiler().timer(timerDihedral).stop();
    _lisp->profiler().timer(timerBondAngleDihedral).stop();

    _lisp->profiler().timer(timerNonbond).start();
    this->_Nonbond->evaluateAll(pos, calcForce, force,
                                calcDiagonalHessian, calcOffDiagonalHessian, hessian, hdvec, dvec);
    _lisp->profiler().timer(timerNonbond).stop();

    _lisp->profiler().timer(timerImproperRestraint).start();
    this->_ImproperRestraint->evaluateAll(pos, calcForce, force,
                                          calcDiagonalHessian, calcOffDiagonalHessian, hessian, hdvec, dvec);
    _lisp->profiler().timer(timerImproperRestraint).stop();

    _lisp->profiler().timer(timerChiralRestraint).start();
    this->_ChiralRestraint->evaluateAll(pos, calcForce, force,
                                        calcDiagonalHessian, calcOffDiagonalHessian, hessian, hdvec, dvec);
    _lisp->profiler().timer(timerChiralRestraint).stop();

    _lisp->profiler().timer(timerAnchorRestraint).start();
    this->_AnchorRestraint->evaluateAll(pos, calcForce, force,
                                        calcDiagonalHessian, calcOffDiagonalHessian, hessian, hdvec, dvec);
    _lisp->profiler().timer(timerAnchorRestraint).stop();

    this->_TotalEnergy = this->_Stretch->getEnergy();
    this->_TotalEnergy += this->_Angle->getEnergy();
    this->_TotalEnergy += this->_Dihedral->getEnergy();
    this->_TotalEnergy += this->_Nonbond->getEnergy();
    this->_TotalEnergy += this->_ImproperRestraint->getEnergy();
    this->_TotalEnergy += this->_ChiralRestraint->getEnergy();
    this->_TotalEnergy += this->_AnchorRestraint->getEnergy();

    _lisp->profiler().timer(timerEnergy).stop();

#if 0  //[
	if ( this->isDebugEnergyEnabled() ) {
	    vector<EnergyAtom>::iterator	ai;
	    int					z;
	    for ( z=1,ai=this->_AtomTable->begin(); ai<this->_AtomTable->end(); ai++,z++) {
		core::Atom_sp aa = ai->_Atom;
		LOG(BF("MEISTER total %d atom %s") % (z) % aa->getName() );
		LOG(BF("MEISTER total %d residue %d") % (z) % (aa->getResidueContainedBy()->getId()) );
		LOG(BF("MEISTER total %d x %5.3lf") % (z) % (pos->getElement(ai->_CoordinateIndex+0) ) );
		LOG(BF("MEISTER total %d y %5.3lf") % (z) % (pos->getElement(ai->_CoordinateIndex+1) ) );
		LOG(BF("MEISTER total %d z %5.3lf") % (z) % (pos->getElement(ai->_CoordinateIndex+2) ) );
		if ( force ) {
		    LOG(BF("MEISTER total %d fx %5.3lf") % (z) % (-force->getElement(ai->_CoordinateIndex+0) ) );
		    LOG(BF("MEISTER total %d fy %5.3lf") % (z) % (-force->getElement(ai->_CoordinateIndex+1) ) );
		    LOG(BF("MEISTER total %d fz %5.3lf") % (z) % (-force->getElement(ai->_CoordinateIndex+2) ) );
		} else {
		    LOG(BF("No forceient") );
		}
	    }
	}
#endif //]
  } catch (InteractionProblem ld) {
    profiler.popTimerStates();
    rethrow ld;
  }
  profiler.popTimerStates();

  // More energy terms
  return this->_TotalEnergy;
}

/*!
 * Compare the analytical force and hessian components term by term with 
 * numerical ones.  Print a message for every mismatch
 * Return the number of mismatches
 */
int EnergyFunction_O::compareAnalyticalAndNumericalForceAndHessianTermByTerm(core::NVector_sp pos) {
  int fails = 0;
  bool calcForce = true;
  bool calcDiagonalHessian = true;
  bool calcOffDiagonalHessian = true;

  {
    this->_Stretch->compareAnalyticalAndNumericalForceAndHessianTermByTerm(pos);
    this->_Angle->compareAnalyticalAndNumericalForceAndHessianTermByTerm(pos);
    this->_Dihedral->compareAnalyticalAndNumericalForceAndHessianTermByTerm(pos);
    this->_Nonbond->compareAnalyticalAndNumericalForceAndHessianTermByTerm(pos);
    this->_ImproperRestraint->compareAnalyticalAndNumericalForceAndHessianTermByTerm(pos);
    this->_ChiralRestraint->compareAnalyticalAndNumericalForceAndHessianTermByTerm(pos);
    this->_AnchorRestraint->compareAnalyticalAndNumericalForceAndHessianTermByTerm(pos);
  }
  return fails;
}

/*!
 * Look for bad geometry
 *
 * Extract the coordinates of the atoms
 */
int EnergyFunction_O::checkForBeyondThresholdInteractions() {
  core::NVector_sp pos;
  stringstream info;
  int fails = 0;
  bool calcForce = false;
  bool calcDiagonalHessian = false;
  bool calcOffDiagonalHessian = false;

  info.str("");
  pos = core::NVector_O::create(this->getNVectorSize(), _lisp);
  this->extractCoordinatesFromAtoms(pos);
  {
    fails = 0;
    fails += this->_Stretch->checkForBeyondThresholdInteractions(info, pos);
    fails += this->_Angle->checkForBeyondThresholdInteractions(info, pos);
    fails += this->_Dihedral->checkForBeyondThresholdInteractions(info, pos);
    fails += this->_Nonbond->checkForBeyondThresholdInteractions(info, pos);
    fails += this->_ImproperRestraint->checkForBeyondThresholdInteractions(info, pos);
    fails += this->_ChiralRestraint->checkForBeyondThresholdInteractions(info, pos);
    fails += this->_AnchorRestraint->checkForBeyondThresholdInteractions(info, pos);
  }

  this->_Message = info.str();
  return fails;
}
