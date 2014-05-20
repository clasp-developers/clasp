;; ---------------------------------------------------
;; ---------------------------------------------------
;;
;; Testing
;;
;; ---------------------------------------------------
;; ---------------------------------------------------


(defparameter *current-package* "OmmPkg")
(clear-external-classes)


(e-class "context" "Context_O" "core::ExternalObject_O"
	 (e-wrapped "OpenMM::Context" "_externalContext" )
	 (e-methods "setTime"
		    "getParameter"
		    "setParameter"
		    "setPeriodicBoxVectors"
		    "applyConstraints"
		    "reinitialize" )
	 (e-init :lisp-args "(self &key omm::system omm::integrator omm::platform)"
		 :code "
    SYMBOL_SC_(system);
    SYMBOL_SC_(integrator);
    SYMBOL_SC_(platform);
    System_sp system = bargs->lookup(_sym_system)->as<System_O>();
    Integrator_sp integrator = bargs->lookup(_sym_integrator)->as<Integrator_O>();
    Platform_sp platform = bargs->lookup(_sym_platform)->as<Platform_O>();
    this->setup(system,integrator,platform);
    return lisp->onil();
" )
	 (o-method :name "setPositions"
		   :define "void setPositions(units::Quantity_sp positions);"
		   :code "
void Context_O::setPositions(units::Quantity_sp coords)
{_OF();
    vector<OpenMM::Vec3> positions;
    positions.resize(coords->size());
    for ( int i=0; i<(int)coords->size(); i++ )
    {
        Vector3 orig = coords->getElement_in_unit_asVector3(i,_lisp->symbol(units::_sym_nanometers)->dyn<units::Unit_O>());
        OpenMM::Vec3 vec(orig.getX(),orig.getY(),orig.getZ());
        positions[i] = vec;
    }
    this->wrapped()->setPositions(positions);
}" )
	 )

(e-write-one-class t *prev-class*)

(external-class-exposed-object-methods *prev-class*)
(expand-declaration *prev-class*)

;;	 (e-ctor-default)


