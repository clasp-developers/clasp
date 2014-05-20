
(defun findInPlug (plugs topologyName groupName)
  (for p plugs
       (when (getIsIn p) (return-from findInPlug p)))
  (raise (LispError (% "There has to be an InPlug in topology[%s] groupName[%s] but I can't find it" topologyName groupName ))))


(defun findOutPlugs (plugs)
  (let ((outs ()))
    (for p plugs
	 (when (not (getIsIn p)) (setq outs (Cons p outs))))
    outs))
       

(defun getAllOutPlugBond0AtomsAsSet (plugs)
  (let ((res (ObjectSet)))
    (for p plugs
	 (let ((atomName (getB0 p)))
	   (assert (not (contains res atomName)) (% "The atom name[%s] is already in the set[%s]" atomName (asString res)))
	   (insert res atomName)))
    res))

(defun getAllOutPlugBond1AtomsAsSet (plugs outPlugBond0AtomsSet )
  (let ((res (ObjectSet)))
    (for p plugs
	 (when (hasB1 p)
	   (let ((atomName (getB1 p)))
	     (assert (not (contains res atomName)) (% "The atom name[%s] is already in the set[%s]" atomName (asString res)))
	     (assert (not (contains outPlugBond0AtomsSet)
			  (% "An atom can't be both a bond0 and bond1 atom!!! Something is wrong with the plugs")))
	      (insert res atomName))))
    res))

(defun getAllStubPivotAtomsAsSet (plugs outPlugBond0AtomsSet )
  (let ((res (ObjectSet))
	(plugCounter 0))
    (for p plugs
	 (when (hasStubPivotAtom)
	   (let ((atomName (getB1 p)))
	     (assert (not (contains res atomName)) (% "The atom name[%s] is already in the set[%s]" atomName (asString res)))
	     (assert (not (contains outPlugBond0AtomsSet)
			  (% "An atom can't be both a bond0 and bond1 atom!!! Something is wrong with the plugs")))
	      (insert res atomName)))
	 (setq plugCounter (+ plugCounter 1)))
    res))


;;
;; Define the stub atoms in terms of
;; 1) parent older-sibling even-older-sibling
;; or if no even-older-sibling 2) parent older-sibling grand-parent
;; or if no older-sibling      3) parent grand-parent great-grand-parent
;; unless your parent is a OddIcoorAtom then just use
;; parent grand-parent
;;
;; (defun setStubAtoms (buildAtom parent)
;;   (IMPLEMENT_ME))
;; 
;; 
;; (defun addAtomToICoorTableRecursively (icoorTable parent me)
;;   (let ((buildAtom (cond
;; 		    ((getProperty me :bond1Atom) (OddICoorAtom (atomName me)))
;; 		    ( true (NormalICoorAtom (atomName me))))))
;;     (setProperty me :buildAtom buildAtom)
;; ;    (setStubAtoms buildAtom parent)
;;     (appendEntry icoorTable buildAtom)
;;     (for child (getProperty me :children)
;; 	 (addAtomToIcoorTableRecursively icoorTable me child))))
;; 

(defun oneAtomAsString (me constitutionAtoms)
  (let ((target (StringOutStream))
	(result (StringOutStream)))
    (write target (kin:atom-template-name me constitutionAtoms))
    (write target (% "/%s" (className me)))
    (write result (% "%30s: " (str target)))
    (for child (kin:bonded-atom-template-children me)
	 (let ((childName (kin:atom-template-name child constitutionAtoms)))
	   (write result (% " %s/%s" childName (kin:atom-template-comment child)))))
    (str result))
)



(defun dumpBuildOrderRecursively (parentTemplate curTemplate constitutionAtoms sout)
  (bformat sout "%s\n" (oneAtomAsString curTemplate constitutionAtoms))
  (dolist (child (kin:bonded-atom-template-children curTemplate))
       (dumpBuildOrderRecursively curTemplate child constitutionAtoms sout)))




(defun atomTemplateFactory (parentTemplate atom constitutionAtoms constitutionName topologyName fragment)
  (let* ((outPlugAtomProp (getPropertyOrDefault atom :outPlug ()))
	 (entityToDelayChildrenFor (getPropertyOrDefault atom :entityToDelayChildrenFor ()))
	 (rootAtomProp (getPropertyOrDefault atom :rootAtom ()))
	 (atomIndex (index constitutionAtoms (atomName atom)))
	 (atomName (atomName (atomWithId constitutionAtoms atomIndex)))
	 (comment (% "%s/w[%d]" atomName (getPropertyOrDefault atom :weight -1))) )
    (cond
      (entityToDelayChildrenFor
       (when rootAtomProp
	 (assert (not rootAtomProp) (% "AtomTemplateFactory problem - atom has entityToDelayChildrenFor and rootAtomProp - atom[%s] constitutionName[%s] topologyName[%s]" (description atom) constitutionName topologyName )))
       (let ((checkpoint (if (isA entityToDelayChildrenFor Plug)
			     (kin:CheckpointOutPlugAtom
			      :constitutionName constitutionName
			      :topologyName topologyName
			      :outPlug entityToDelayChildrenFor)
			     (kin:CheckpointAtom
			      :constitutionName constitutionName
			      :topologyName topologyName
			      :atomName (chem:atomName entityToDelayChildrenFor)))))
	 (kin:DelayedBondedAtomTemplate :id atomIndex
					:parent parentTemplate
					:checkpoint checkpoint
					:comment comment
					:outPlug outPlugAtomProp )))
      (rootAtomProp
       (kin:RootBondedAtomTemplate :id atomIndex
				   :parent parentTemplate
				   :constitutionName constitutionName
				   :topologyName topologyName
				   :inPlug rootAtomProp
				   :comment comment
				   :outPlug outPlugAtomProp))
      (true (kin:BondedAtomTemplate :id atomIndex
				    :parent parentTemplate
				    :comment comment
				    :outPlug outPlugAtomProp))))
  )


	 

;;
;; Build an AtomTreeTemplate recursively using the properties defined
;; for each atom
;;
(defun buildAtomTreeTemplateRecursively (parent fragment root constitutionAtoms constitutionName topologyName )
  (let ((rootTemplate (atomTemplateFactory parent root constitutionAtoms constitutionName topologyName fragment))
	(children (progn
		    (log (propertiesAsString root))
		    (getProperty root :children))))
    (for child children
	 (let ((childTemplate (buildAtomTreeTemplateRecursively rootTemplate fragment child constitutionAtoms constitutionName topologyName )))
	   (kin:addChild rootTemplate childTemplate)))
    rootTemplate))



(defun buildInternalCoordinateAtomTemplateTree (chemdrawFragment constitutionAtoms plugs constitutionName topologyName )
  (let* ((residue (makeResidue constitutionAtoms))
	 (inPlug (findInPlug plugs topologyName (getProperty chemdrawFragment :group)))
	 (outPlugs (findOutPlugs plugs))
	 (rootAtomName (rootAtomName inPlug))
	 (rootAtom (atomWithName residue rootAtomName))
	 (spanningLoop (SpanningLoop :root rootAtom))
	 (allSpanningAtoms ()))
    ;; First walk a spanning loop outwards from the root
    ;; and tag atoms as being :outPlug :bond1Atom
    ;; and set the :weight to 1
    ;; and set an empty list as the :children property
    ;; also accumulate the atoms in the spanning loop in 
    ;; order of their distance to the root
    (print (% "Building internal coordinate table for monomer[%s] topology[%s]" (getProperty chemdrawFragment :name) topologyName ))
    (log (% "Starting a SpanningLoop with root atom[%s]" (repr rootAtom)))
    (log (% "The atoms bonded to rootAtom are: %s" (repr (bondedAtomsAsList rootAtom))))
    (prognLog
     "Define the root atom"
     (setProperty rootAtom :rootAtom inPlug))
    (prognLog
     "Spanning loop of atoms in residue"
     (while (advance spanningLoop)
       (let ((atom (getAtom spanningLoop)))
	 (log (% "Spanning over atom[%s]" (description atom)))
	 (setProperty atom :outPlug false)
	 (setProperty atom :bond1Atom false)
	 (setProperty atom :weight 1)
	 (setProperty atom :children ())
	 (log (% "%s" (propertiesAsString atom)))
	 ;; Accumulate a list of all the spanning atoms in order of distance to root
	 (setq allSpanningAtoms (Cons atom allSpanningAtoms)))))
    ;;
    ;; Now iterate over the plugs and assign atom properties that will 
    ;; define the build order and identify for OutPlugs
    ;; 1) :highestPriority atoms (atoms that bond0 atoms depend on)
    ;; 2) :highPriority atoms (bond0 atoms)
    ;; 3) atoms that need to delay building their children until another atom is defined
    ;;     identify by setting :entityToDelayChildrenFor XXXX where
    ;;           XXXX == :nextResidueBond1
    ;;        or XXXX == the stubPivotAtom that you need to build your children
    ;;
    (prognLog
     "Looping over plugs"
     (dolist (plug plugs)
       (prognLog
	(% "Plug: %s" plug)
	(cond
	  ((isA plug OutPlug)
	   (let ((bond0Atom (atomWithName residue (getB0 plug)))
		 (bond1Atom (if (hasB1 plug)
				(atomWithName residue (getB1 plug))
				()))
		 (stubPivotAtom (if (hasStubPivotAtom plug)
				    (atomWithName residue (getStubPivotAtom plug))
				    ())))
	     (log (% "bond0Atom[%s] bond1Atom[%s] stubPivotAtom[%s]"
		     (description bond0Atom)
		     (description bond1Atom)
		     (description stubPivotAtom)))
	     (assert (not (and (or bond1Atom stubPivotAtom) (== bond1Atom stubPivotAtom)))
		     (% "The same plug cannot define a bond1Atom[%s] and a stubPivotAtom[%s] at the same time" bond1Atom stubPivotAtom))
	     (log (% "Setting atom[%s] :highPriority=t and :outPlug property to: %s" (description bond0Atom) (description plug)))
	     (setProperty bond0Atom :outPlug plug)
	     (setProperty bond0Atom :highPriority true)
	     (cond
	       (bond1Atom (progn
			    (log (% "Setting bond1Atom properties atom[%s]" (description bond1Atom)))
			    (setProperty bond1Atom :bond1Atom true)
			    (setProperty bond1Atom :highestPriority true)
			    (setProperty bond1Atom :buildBefore bond0Atom)
			    (setProperty bond1Atom :entityToDelayChildrenFor plug )))
	       (stubPivotAtom (progn
				(let ((bond0DependsOnAtom (getPropertyOrDefault bond0Atom :buildBefore ())))
				  (if (== bond0DependsOnAtom stubPivotAtom)
				      (prognLog
				       (% "Setting atom[%s] :entityToDelayChildrenFor to-> %s" (description bond0Atom) stubPivotAtom)
				       (setProperty bond0Atom :entityToDelayChildrenFor stubPivotAtom))
				      (prognLog
				       (% "Setting stubPivotAtom[%s] :highestPriority=t and :buildBefore to-> %s" (description stubPivotAtom) (description bond0Atom))
				       (setProperty stubPivotAtom :buildBefore bond0Atom)
				       (setProperty stubPivotAtom :highestPriority true))))))
	       (true ()))))
	  (true ())))
       )
     )
    ;; Now iterate over the atoms from farthest to the nearest to the root
    ;; and add yourself to your backSpanning atoms :children
    (prognLog
     "Adding atoms to their parents"
     (dolist (atom allSpanningAtoms)
       (let ((parentAtom (getBackSpan atom)))
	 (when parentAtom
	   (let ((parentChildren (getProperty parentAtom :children)))
	     (setq parentChildren (Cons atom parentChildren))
	     (setProperty parentAtom :children parentChildren)))))
     )
    ;; Now iterate over the atoms from farthest to the nearest and assign a branch weight
    ;; to each atom
    ;; 1) Get the largest weight of this atoms children
    ;; 2) If the current atom is a bond0Atom then add 1000 to its weight
    ;; 3) otherwise if the current atom is a bond1Atom then add 10000 to its weight
    ;; 4) otherwise add 1 to its weight
    ;;   This is to try and ensure that branches that involve Plugs that make two bonds are
    ;;   built first and that the bond1Atom of these plugs gets built before the bond0Atom.
    ;;   This is so that the bond1Atom will define the dihedral to the root atom of the next residue
    ;;   that is connected through the bond0Atom.
    ;;   This will help to isolate residues from each other in terms of internal dihedral coordinates
    ;;   so that bis-amino acids are built from the amino acid atoms of the previous residue rather
    ;;   than building from the inside of the previous bis-amino acid. 
    ;;   Here is a concrete example - If we had a pro4(SS) residue X modeled on another pro4(SS) residue Y and
    ;;   the dihedral for X's root were defined relative to the CB of Y then if we built X onto a pro4(SR) 
    ;;   residue Z then that dihedral would be wrong.  If instead we define X's root atom dihedral in terms
    ;;   of the CGC of Y then it won't matter what stereoisomer of Z we put on.
    ;;
    (prognLog
     "Assigning branch weights"
     (dolist (atom allSpanningAtoms)
       (let ((atomWeight (cond
			   ((getPropertyOrDefault atom :highestPriority false) 10000)
			   ((getPropertyOrDefault atom :highPriority false)     1000)
			   (true                                                   1))))
	 (let ((maxChildWeight 0))
	   (for child (getProperty atom :children)
		(setq maxChildWeight (max maxChildWeight (getProperty child :weight))))
	   (setq atomWeight (+ atomWeight maxChildWeight))
	   (setProperty atom :weight atomWeight))))
     )
    ;;
    ;; Now sort the children of each atom in descending order of their weight
    ;; and if there are ties (like methylene or methyl hydrogens) sort them
    ;; in increasing alphabetical order by their name
    ;;
    (prognLog
     "Sorting children by weight"
     (dolist (atom allSpanningAtoms)
       (let* ((children (getProperty atom :children))
	      (sortedChildren (sort children (lambda (a b)
					       (let ((wa (getProperty a :weight))
						     (wb (getProperty b :weight)))
						 (cond
						   ((> wa wb) true)
						   ((< wa wb) false)
						   (true (< (atomName a) (atomName b)))))))))
	 (setProperty atom :children sortedChildren)
	 ))
     )
    ;;
    ;; Ok, now every atom has :children defined and they are sorted in order of how they
    ;; should be built - now we are ready to build the InternalCoordinateTable for the residue
    ;;
    (prognLog
     "Building AtomTreeTemplate - then dumping it"
     (let ((treeTemplate (buildAtomTreeTemplateRecursively () chemdrawFragment rootAtom constitutionAtoms constitutionName topologyName )))
       (prognLog
	"Dump of final build order"
	(let ((buildOrder (with-output-to-string (sout)
			    (dumpBuildOrderRecursively () treeTemplate constitutionAtoms sout))))
	  (bformat t "Build order\n%s\n" buildOrder)
	  (log (bformat nil "Build order\n%s\n" buildOrder))))
       treeTemplate)
     )
    )
  )


