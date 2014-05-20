(include "CANDO/db/globalOptions.l" )
(include "CANDO/db/jobSetup.l" )

(include "CANDO/db/stereotype.l")
;;
;; Dynamic variable that stores Chemdraw constitution information
;; - filled using 'addFragmentProperties
(defvar *allChemdrawConstitutions* ())
(defun setChemdraw ( cd )
  (setq *allChemdrawConstitutions* cd))

(defun getChemdraw ()
  *allChemdrawConstitutions*)

;;
;; Dynamic variable that stores stereotypes
;;
(defvar *allStereotypes* (Binder))
(defun addNeighborStereotype ( plugName groupName )
  (let ((symbolSet (if (contains *allStereotypes* plugName)
		       (lookup *allStereotypes* plugName)
		     (extend *allStereotypes* plugName (SymbolSet)))))
    (insert symbolSet groupName)))

(defun getNeighborStereotype (plugName)
  (if (contains *allStereotypes* plugName)
      (lookup *allStereotypes* plugName)
    ()))




(defun useNeighborStereotypesToDefinePlugMates (db)
  (for constitution (constitutionsAsCons db)
       (print (% "Constitution: %s" (chem:getName constitution)))
       (for plug (plugsWithMatesAsCons constitution)
	    (print (% "    plug = %s" (repr plug)))
	    (let* ((mate (car (let ((mm (matesAsCons plug)))
				(assert (== (length mm) 1 ) (% "There should only be one mate but there are %d" (length mates)))
				mm))))
	      (expandToTerminalEntityNames mate)
	      (print (% "      before expanded: %s" (repr mate)))
	      (let ((stereotypes (getNeighborStereotype (chem:getName plug))))
		(when (notNil stereotypes)
		  (contractEntityNames mate stereotypes)))
	      (print (% "       after contracted: %s" (repr mate)))
	      ))))







(defun prepareSeed ()
  (prognLog "Prepare seed"
	    (let ((metaInclude (getGlobalOption 'metaDatabase)))
	      (print (% "Including <%s>" metaInclude))
	      (include metaInclude))
					;
					; full_definitions defines all of the fragments and their
					; properties and puts it in the full_chemdraw global variable
					;
					;include "CANDO/db/full_definitions.l"

	    ;;
	    ;; Load the file with the fileName in monomerSetup and it will call '#setChemdraw to
	    ;; set the Chemdraw fragments
	    ;;
	    (load (getGlobalOption 'monomerSetup))
    
	    (print "Creating the MetaDatabase - use tail on the log file to monitor progress")
	    (let ((chemdrawConstitutions (getChemdraw)))
	      (assert (> (length chemdrawConstitutions) 0 ) "There are no Chemdraw fragments!!!" )
	      ;;
	      ;;Define *metaDatabase* as a Dynamic variable so that it can be accessed
	      ;; by the MetaFrameRecognizer::resolve method
	      ;;
	      (defvar *metaDatabase* (createMetaDatabase))
	      (defineMatesWithRepresentativesForAllPlugs *metaDatabase*)
	      (prognLog  "ConnectRingClosingMates: "
			 (updateRingClosingMatesForRingClosingPlugs *metaDatabase* ))
	      (prognLog  "Check*MetaDatabase*: " (checkMetaDatabaseForProblems *metaDatabase* ) )
	      ;;
	      ;; Now read out the :cap property from any of the Chemdraw constitutions
	      ;; that have them and tell the MetaDatabase 
	      (prognLog "Chemdraw realize Constitutions via MetaDatabase: "
			(addChemdrawFragments *metaDatabase* chemdrawConstitutions ))
	      (prognLog  "ChemdrawCheck: " (checkChemdraw *metaDatabase* ))
	      ))
  )