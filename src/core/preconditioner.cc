       
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
