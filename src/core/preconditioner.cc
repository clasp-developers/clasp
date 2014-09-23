/*
    File: preconditioner.cc
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
       
#define STRETCH_CALC_FORCE 
#define STRETCH_CALC_DIAGONAL_HESSIAN 
#define STRETCH_CALC_OFF_DIAGONAL_HESSIAN 
#define ANGLE_CALC_FORCE 
#define ANGLE_CALC_DIAGONAL_HESSIAN 
#define ANGLE_CALC_OFF_DIAGONAL_HESSIAN 
#define DIHEDRAL_CALC_FORCE 
#define DIHEDRAL_CALC_DIAGONAL_HESSIAN 
#define DIHEDRAL_CALC_OFF_DIAGONAL_HESSIAN 
#define IMPROPER_RESTRAINT_CALC_FORCE 
#define IMPROPER_RESTRAINT_CALC_DIAGONAL_HESSIAN 
#define IMPROPER_RESTRAINT_CALC_OFF_DIAGONAL_HESSIAN 
#define CHIRAL_RESTRAINT_CALC_FORCE 
#define CHIRAL_RESTRAINT_CALC_DIAGONAL_HESSIAN 
#define CHIRAL_RESTRAINT_CALC_OFF_DIAGONAL_HESSIAN 

    if ( this->isStretchEnergyEnabled() ) {
	for ( vector<EnergyStretch>::iterator si=this->_StretchTerms.begin();
		    si!=this->_StretchTerms.end(); si++ ) {
	    #include	"_stretchTerm.cc"
	}
    }
    if ( this->isAngleEnergyEnabled() ) {
	for ( vector<EnergyAngle>::iterator ai=this->_AngleTerms.begin();
		    ai!=this->_AngleTerms.end(); ai++ ) {
	    #include	"_angleTerm.cc"
	}
    }
    if ( this->isDihedralEnergyEnabled() ) {
	for ( vector<EnergyDihedral>::iterator di=this->_DihedralTerms.begin();
		    di!=this->_DihedralTerms.end(); di++ ) {
	    #include	"_dihedralTerm.cc"
	}
    }
    if ( this->isImproperRestraintEnergyEnabled() ) {
	for ( vector<AmberImproperRestraint>::iterator iri=this->_ImproperRestraintTerms.begin();
		    iri!=this->_ImproperRestraintTerms.end(); iri++ ) {
	    #include	"_improperRestraintTerm.cc"
	}
    }
    if ( this->isChiralRestraintEnergyEnabled() ) {
	for ( vector<AmberChiralRestraint>::iterator cri=this->_ChiralRestraintTerms.begin();
		    cri!=this->_ChiralRestraintTerms.end(); cri++ ) {
	    #include	"_chiralRestraintTerm.cc"
	}
    }
