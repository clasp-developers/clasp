(require :clang-tool)

;;;
;;; Setup a compilation-tool-database
;;; It describes an entire hierarchy of source files
;;; that will be refactored all in one sweep.
;;;
(progn
  (defclass def ()
    ((exposed-name :initarg :exposed-name :accessor exposed-name)
     (method-name :initarg :method-name :accessor method-name)
     (source-location :initarg :source-location :accessor source-location)))
  (defparameter *verbose-callback* nil)
  (defun translate-include (args)
    "* Arguments
- args :: A vector of strings (compilation arguments)
* Description
Convert -Iinclude to -I<main-sourcefile-pathname>/include. Uses dynamic variable *main-directory-namestring*."
    (let ((main-directory-namestring (namestring (make-pathname :name nil :type nil :defaults (clang-tool:main-pathname)))))
      (dotimes (i (length args))
        (when (string= (elt args i) "-Iinclude")
          (setf (elt args i) (format nil "-I~a/include" main-directory-namestring))))
      args))
  (defun setup-db ()
    "* Description
Setup the compilation-tool-database."
    (let ((db (clang-tool:load-compilation-tool-database
               "app-resources:build-databases;cando_compile_commands.json"
               :convert-relative-includes-to-absolute t)))
      (push #'translate-include (clang-tool:arguments-adjuster-list db))
      db))
  (defparameter *db* (setup-db)))

;;;
;;; Setup the ASTMatcher to recognize .def("foo",&Instance_O::foo)
;;;
(progn
  (defconstant +source-path+ (ext:getenv "CLASP_HOME"))
  (defconstant +source-path-length+ (length +source-path+))
  (defparameter *find-def-matcher*
    '(:member-call-expr
      (:bind :whole (:member-call-expr))
      (:has-argument 0
       (:expr
        (:has-descendant
         (:string-literal
          (:bind :arg0-string (:string-literal))))
        (:bind :arg0 (:expr))))
      (:has-argument 1
       (:unary-operator
        (:has-descendant
         (:decl-ref-expr
          (:has-declaration
           (:method-decl
            (:bind :arg1-decl (:method-decl))))))))
      (:callee
       (:decl
        (:bind :the-callee (:decl))))))
  (defparameter *defs* (make-hash-table :test #'equal))
  (defun find-def-initializer ()
    (clrhash *defs*))
  (defun find-def-callback (match-info)
    (when (string= (clang-tool:mtag-loc-start match-info :whole) +source-path+ :end1 +source-path-length+)
      (let* ((node (clang-tool:mtag-node match-info :whole))
             (source-pos (clang-tool:mtag-loc-start match-info :whole))
             (string-node (clang-tool:mtag-node match-info :arg0-string))
             (exposed-name (cast:get-string string-node))
             (arg1-decl (clang-tool:mtag-node match-info :arg1-decl))
             (method-name (cast:get-qualified-name-as-string arg1-decl)))
        (when *verbose-callback*
          (format t "exposed-name: ~a~%" exposed-name)
          (format t "arg1-decl: ~a~%" (cast:get-qualified-name-as-string arg1-decl)))
        (setf (gethash method-name *defs*)  (make-instance 'def
                                                           :exposed-name exposed-name
                                                           :method-name method-name
                                                           :source-location source-pos))))))

;;; Load a subset of the ASTs for quick testing.
(defun load-subset-asts ()
  (setf (clang-tool:source-namestrings *db*) (clang-tool:select-source-namestrings *db* "str.cc"))
  (clang-tool:load-asts *db*))
;;; Run the matcher on the subset of ASTs
(defun run-matcher-on-subset ()
  (clang-tool:with-compilation-tool-database *db*
    (let ((*verbose-callback* t))
      (funcall #'find-def-initializer)
      (clang-tool:match-run-loaded-asts
       *find-def-matcher*
       :limit 100000
       :callback
       (make-instance
        'clang-tool:code-match-callback
        :match-code #'find-def-callback)))))


(disassemble (fdefinition 'find-def-callback))


;;; Select just one of the source files for testing
#+(or)
(setf (clang-tool:source-namestrings *db*)
      (clang-tool:select-source-namestrings *db* ".*str\.cc.*$"))

;;; Select all of the source files
#+(or)
(setf (clang-tool:source-namestrings *db*)
      (clang-tool:select-source-namestrings *db*))

;;; Setup and run the multitool on the compilation-tool-database
;;; This will identify all of the class_<Foo_O>(...).def("bar",&Foo_O::bar);
(defun run-find-def ()
  (defparameter *tool* (clang-tool:make-multitool))
  (clang-tool:multitool-add-matcher
   *tool*
   :name :find-defs
   :matcher (clang-tool:compile-matcher *find-def-matcher*)
   :initializer #'find-def-initializer
   :callback (make-instance 'clang-tool:code-match-callback
                            :match-code #'find-def-callback))
  (clang-tool:with-compilation-tool-database *db*
    (let ((*verbose-callback* t))
      (clang-tool:batch-run-multitool *tool*
                                      :compilation-tool-database *db*
                                      :run-and-save nil)))
  (format t "Done stage1~%"))


(defun save-defs (filename defs)
  (with-open-file (fout filename :direction :output :if-exists :supersede)
    (format fout "(~%")
    (maphash (lambda (k v)
               (format fout "( ~s ~s ~s)~%" (method-name v)
                       (exposed-name v)
                       (source-location v)))
             defs)
    (format fout ")~%")))

(setq *default-pathname-defaults* #P"/Users/meister/Development/clasp/src/lisp/modules/cxx-refactor/")
(save-defs "exposed.dat" *defs*)

(defparameter *defs* (make-hash-table :test #'equal))

(defun load-defs (filename)
  (let ((defs-list (with-open-file (fin filename :direction :input)
                     (read fin)))
        (ht (make-hash-table :test #'equal)))
    (dolist (e defs-list)
      (let ((method-name (first e))
            (exposed-name (second e))
            (source (third e)))
        (setf (gethash method-name ht) (make-instance 'def
                                                      :method-name method-name
                                                      :exposed-name exposed-name
                                                      :source-location source))))
    ht))

#+(or)(defparameter *defs* (load-defs "exposed.dat"))

(progn
  (defparameter *fix-method-matcher*
    '(:method-decl
      (:is-definition)
      (:has-name "grizzly-bear")
      #+(or)(:any-of
             (:matches-name "adapt::IndexedObjectBag_O")
             (:matches-name "adapt::IterateCons_O")
             (:matches-name "adapt::ObjectSet_O")
             (:matches-name "adapt::QDomNode_O")
             (:matches-name "adapt::StringList_O")
             (:matches-name "adapt::StringSet_O")
             (:matches-name "adapt::SymbolSet_O")
             (:matches-name "asttooling::Diagnostics")
             (:matches-name "cffi::Pointer_O")
             (:matches-name "chem::Aggregate_O")
             (:matches-name "chem::AtomId_O")
             (:matches-name "chem::AtomIdToAtomMap_O")
             (:matches-name "chem::Atom_O")
             (:matches-name "chem::AtomTable_O")
             (:matches-name "chem::Bond_O")
             (:matches-name "chem::CandoDatabase_O")
             (:matches-name "chem::CDFragment_O")
             (:matches-name "chem::ChemDraw_O")
             (:matches-name "chem::ChemInfoMatch_O")
             (:matches-name "chem::ChemInfo_O")
             (:matches-name "chem::CipPrioritizer_O")
             (:matches-name "chem::ConformationExplorerEntry_O")
             (:matches-name "chem::ConformationExplorerEntryStage_O")
             (:matches-name "chem::ConformationExplorerMatch_O")
             (:matches-name "chem::ConformationExplorer_O")
             (:matches-name "chem::ConstitutionAtom_O")
             (:matches-name "chem::ConstitutionAtoms_O")
             (:matches-name "chem::Constitution_O")
             (:matches-name "chem::CoordinateSystem_O")
             (:matches-name "chem::Coupling_O")
             (:matches-name "chem::DirectionalCoupling_O")
             (:matches-name "chem::EnergyComponent_O")
             (:matches-name "chem::EnergyFunction_O")
             (:matches-name "chem::EntityNameSetBase_O")
             (:matches-name "chem::EntityNameSet_O")
             (:matches-name "chem::EntityNameSetWithCap_O")
             (:matches-name "chem::Entity_O")
             (:matches-name "chem::FFNonbondDb_O")
             (:matches-name "chem::FFStretchDb_O")
             (:matches-name "chem::FFTypesDb_O")
             (:matches-name "chem::ForceField_O")
             (:matches-name "chem::FrameRecognizer_O")
             (:matches-name "chem::InPlug_O")
             (:matches-name "chem::IterateBonds_O")
             (:matches-name "chem::IterateMatter_O")
             (:matches-name "chem::JumpPlug_O")
             (:matches-name "chem::Mate_O")
             (:matches-name "chem::Matter_O")
             (:matches-name "chem::Minimizer_O")
             (:matches-name "chem::Molecule_O")
             (:matches-name "chem::MonomerContext_O")
             (:matches-name "chem::MonomerCoordinates_O")
             (:matches-name "chem::Monomer_O")
             (:matches-name "chem::MonomerPack_O")
             (:matches-name "chem::MultiMonomer_O")
             (:matches-name "chem::Oligomer_O")
             (:matches-name "chem::OutPlug_O")
             (:matches-name "chem::PdbMonomerDatabase_O")
             (:matches-name "chem::PdbWriter_O")
             (:matches-name "chem::Plug_O")
             (:matches-name "chem::PlugWithMates_O")
             (:matches-name "chem::ReadAmberParameters_O")
             (:matches-name "chem::RepresentedEntityNameSet_O")
             (:matches-name "chem::Residue_O")
             (:matches-name "chem::RestrainedPiBond_O")
             (:matches-name "chem::RestraintAnchor_O")
             (:matches-name "chem::RestraintList_O")
             (:matches-name "chem::Restraint_O")
             (:matches-name "chem::RingClosingPlug_O")
             (:matches-name "chem::RingCoupling_O")
             (:matches-name "chem::RingFinder_O")
             (:matches-name "chem::SearchStatistics_O")
             (:matches-name "chem::SpanningLoop_O")
             (:matches-name "chem::SpecificContext_O")
             (:matches-name "chem::SpecificContextSet_O")
             (:matches-name "chem::StereoConfiguration_O")
             (:matches-name "chem::StereoInformation_O")
             (:matches-name "chem::StereoisomerAtom_O")
             (:matches-name "chem::StereoisomerAtoms_O")
             (:matches-name "chem::Stereoisomer_O")
             (:matches-name "chem::SuperposeEngine_O")
             (:matches-name "chem::Topology_O")
             (:matches-name "chem::Trajectory_O")
             (:matches-name "chem::TwisterDriver_O")
             (:matches-name "chem::Twister_O")
             (:matches-name "chem::VirtualSphere_O")
             (:matches-name "chem::WildElementDict_O")
             (:matches-name "chem::ZMatrixAngleInternal_O")
             (:matches-name "chem::ZMatrixBondInternal_O")
             (:matches-name "chem::ZMatrixDihedralInternal_O")
             (:matches-name "chem::ZMatrixEntry_O")
             (:matches-name "chem::ZMatrixInternal_O")
             (:matches-name "chem::ZMatrix_O")
             (:matches-name "core::Array_O")
             (:matches-name "core::Bignum_O")
             (:matches-name "core::BitVector_O")
             (:matches-name "core::Instance_O")
             (:matches-name "core::Cons_O")
             (:matches-name "core::DirectoryEntry_O")
             (:matches-name "core::Environment_O")
             (:matches-name "core::ExternalObject_O")
             (:matches-name "core::FileStatus_O")
             (:matches-name "core::Float_O")
             (:matches-name "core::ForeignData_O")
             (:matches-name "core::Function_O")
             (:matches-name "core::FunctionValueEnvironment_O")
             (:matches-name "core::HashTable_O")
             (:matches-name "core::InvocationHistoryFrameIterator_O")
             (:matches-name "core::Iterator_O")
             (:matches-name "core::LambdaListHandler_O")
             (:matches-name "core::LoadArchive_O")
             (:matches-name "core::LoadTimeValues_O")
             (:matches-name "core::MacroletEnvironment_O")
             (:matches-name "core::MultiStringBuffer_O")
             (:matches-name "core::Package_O")
             (:matches-name "core::Path_O")
             (:matches-name "core::PosixTimeDuration_O")
             (:matches-name "core::PosixTime_O")
             (:matches-name "core::RegexMatch_O")
             (:matches-name "core::Regex_O")
             (:matches-name "core::SaveArchive_O")
             (:matches-name "core::SexpLoadArchive_O")
             (:matches-name "core::SexpSaveArchive_O")
             (:matches-name "core::SingleDispatchGenericFunction_O")
             (:matches-name "core::SingleDispatchMethod_O")
             (:matches-name "core::SmallMap_O")
             (:matches-name "core::SmallMultimap_O")
             (:matches-name "core::SNode_O")
             (:matches-name "core::SourceFileInfo_O")
             (:matches-name "core::Str_O")
             (:matches-name "core::SymbolMacroletEnvironment_O")
             (:matches-name "core::Symbol_O")
             (:matches-name "core::SymbolToEnumConverter_O")
             (:matches-name "core::TagbodyEnvironment_O")
             (:matches-name "core::UnwindProtectEnvironment_O")
             (:matches-name "core::ValueEnvironment_O")
             (:matches-name "core::Vector_O")
             (:matches-name "core::VectorObjectsWithFillPtr_O")
             (:matches-name "core::WeakKeyHashTable_O")
             (:matches-name "core::WeakKeyMapping_O")
             (:matches-name "core::WeakPointer_O")
             (:matches-name "core::WrappedPointer_O")
             (:matches-name "geom::BoundingBox_O")
             (:matches-name "geom::CoordinateArray_O")
             (:matches-name "geom::CoordinateArrayWithHash_O")
             (:matches-name "geom::OMatrix_O")
             (:matches-name "geom::OVector2_O")
             (:matches-name "geom::OVector3_O")
             (:matches-name "llvmo::APInt_O")
             (:matches-name "llvmo::BasicBlock_O")
             (:matches-name "llvmo::DataLayout_O")
             (:matches-name "llvmo::DebugLoc_O")
             (:matches-name "llvmo::DIBuilder_O")
             (:matches-name "llvmo::EngineBuilder_O")
             (:matches-name "llvmo::ExecutionEngine_O")
             (:matches-name "llvmo::Function_O")
             (:matches-name "llvmo::Instruction_O")
             (:matches-name "llvmo::IRBuilderBase_O")
             (:matches-name "llvmo::IRBuilder_O")
             (:matches-name "llvmo::LLVMTargetMachine_O")
             (:matches-name "llvmo::Module_O")
             (:matches-name "llvmo::NamedMDNode_O")
             (:matches-name "llvmo::StructType_O")
             (:matches-name "llvmo::SwitchInst_O")
             (:matches-name "llvmo::TargetMachine_O")
             (:matches-name "llvmo::TargetOptions_O")
             (:matches-name "llvmo::Type_O")
             (:matches-name "units::Quantity_O")
             (:matches-name "units::Unit_O"))
      (:bind :whole (:method-decl))))
  (defparameter *fixed* nil)
  (defun fix-method-initializer () (setf *fixed* (make-hash-table :test #'equal)))
  (defun fix-method-callback (match-info)
    (when (string= (clang-tool:mtag-loc-start match-info :whole)
                   +source-path+ :end1 +source-path-length+)
      (let* ((node (clang-tool:mtag-node match-info :whole))
             (source-pos (clang-tool:mtag-loc-start match-info :whole))
             (method-name (cast:get-qualified-name-as-string node))
             (def-info (gethash method-name *defs*))
             (fixed-already (gethash method-name *fixed*)))
        (when (and *verbose-callback*
                   def-info
                   (not fixed-already))
          (format t "Exposed method: ( ~s ~s ~s)~%" method-name (exposed-name def-info) (clang-tool:mtag-loc-start match-info :whole))
          (when (and def-info (not fixed-already))
            (setf (gethash method-name *fixed*) t)
            (clang-tool:mtag-replace
             match-info
             :whole
             (lambda (minfo tag)
               (let ((source (clang-tool:mtag-source minfo tag)))
                 (format nil "CL_NAME(~s);~%CL_DEFMETHOD ~a" (exposed-name def-info) source))))))))))

(defun run-fix-matcher ()
  (defparameter *tool* (clang-tool:make-multitool))
  (clang-tool:multitool-add-matcher
   *tool*
   :name :fix-expose
   :matcher (clang-tool:compile-matcher *fix-method-matcher*)
   :initializer #'fix-method-initializer
   :callback (make-instance 'clang-tool:code-match-callback
                            :match-code #'fix-method-callback))
  (time (clang-tool:with-compilation-tool-database *db*
          (let ((*verbose-callback* t))
            (clang-tool:batch-run-multitool *tool*
                                            :compilation-tool-database *db*
                                            :run-and-save nil))))
  (format t "Done stage2~%"))



(defun run-all ()
  (run-find-def)
  (save-defs "exposed.dat" *defs*)
  (run-fix-matcher))
