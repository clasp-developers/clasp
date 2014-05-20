(include "CANDO/db/atomTree/renderAtomTemplate.l")
(include "CANDO/patterns/aromaticity.l")

(defClass Chi () (_Atom1 _Atom2 _Atom3 _Atom4 _Residue))

(defMethod __init__ Chi (self atom residue)
  "A Chi dihedral stores four AtomTemplates
and the residue that contains atoms with the ids
refered to by the AtomTemplates"
  (let* ((parent (kin:atom-template-parent atom))
	 (grandParent (kin:atom-template-parent parent))
	 (greatGrandParent (kin:atom-template-parent grandParent)))
    (setq (slot self _Atom1) greatGrandParent)
    (setq (slot self _Atom2) grandParent)
    (setq (slot self _Atom3) parent)
    (setq (slot self _Atom4) atom)
    (setq (slot self _Residue) residue)
    )
  )

(defMethod chi-has-nil-atoms Chi (self)
  (not (and (slot self _Atom1) (slot self _Atom2) (slot self _Atom3))))

(defMethod chi-atom-templates Chi (self)
  "Return the atom templates"
  (values (slot self _Atom1)
	  (slot self _Atom2)
	  (slot self _Atom3)
	  (slot self _Atom4)))

(defMethod chi-atom-names-as-string Chi (self)
  (bformat nil "%s %s %s %s"
	   (if (chi-atom1 self) (chem:getName (chi-atom1 self)) "-")
	   (if (chi-atom2 self) (chem:getName (chi-atom2 self)) "-")
	   (if (chi-atom3 self) (chem:getName (chi-atom3 self)) "-")
	   (chem:getName (chi-atom4 self))
  ))


(defMethod chi-create-chi-dihedral Chi (self)
  (multiple-value-bind (at1 at2 at3 at4) (chi-atom-templates self)
    (kin:ChiDihedral
     (if at1 (kin:atom-template-id at1) -1)
     (if at2 (kin:atom-template-id at2) -1)
     (if at3 (kin:atom-template-id at3) -1)
     (kin:atom-template-id at4))))


(defMethod chi-atom4 Chi (self)
  (if (slot self _Atom4)
      (chem:atomWithAtomId (slot self _Residue)
			   (AtomId :molecule 0 :residue 0 :atom (kin:atom-template-id (slot self _Atom4))))
      ()))

(defMethod chi-atom3 Chi (self)
  (if (slot self _Atom3)
      (chem:atomWithAtomId (slot self _Residue)
			   (AtomId :molecule 0 :residue 0 :atom (kin:atom-template-id (slot self _Atom3))))
      ()))

(defMethod chi-atom2 Chi (self)
  (if (slot self _Atom2)
      (chem:atomWithAtomId (slot self _Residue)
		       (AtomId :molecule 0 :residue 0 :atom (kin:atom-template-id (slot self _Atom2))))
      ()))


(defMethod chi-atom1 Chi (self)
  (if (slot self _Atom1)
      (chem:atomWithAtomId (slot self _Residue)
		       (AtomId :molecule 0 :residue 0 :atom (kin:atom-template-id (slot self _Atom1))))
      ()))


(defMethod chi-atom4-element Chi (self)
  (chem:getElement (chi-atom4 self)))

(defMethod chi-atom3-element Chi (self)
  (if (chi-atom3 self)
      (chem:getElement (chi-atom3 self))
      ()))





(defun ring-contains-both-atoms (atom1Id atom2Id ring)
  (let (contains1 contains2)
    (dolist (ringAtom ring)
      (let ((ringAtomId (chem:getAtomId ringAtom)))
	(when (== ringAtomId atom1Id) (setq contains1 t))
	(when (== ringAtomId atom2Id) (setq contains2 t))
	)
      )
    (and contains1 contains2)))


;;
;; Calculate if a chi is rotatable or not
;; If the two central atoms are in the same aromatic ring
;; then they are not
;;
(defMethod chi-is-rotatable Chi (self aromaticRings dbg)
  (when aromaticRings
    (let ((atom2Id (kin:atom-template-id (slot self _Atom2)))
	  (atom3Id (kin:atom-template-id (slot self _Atom3)))
	  bothInRing)
      (dolist (ring aromaticRings)
	(when (ring-contains-both-atoms atom2Id atom3Id ring)
	  (return-from chi-is-rotatable nil))))
    )
  t
  )


(defun assemble-raw-chi-atom-list (parent atom atomIndex)
  "Recursively assemble the list of terminal atoms that may make up ChiDihedrals.
These are AtomTemplates that are the first child of their parent and
whose parents aren't Delayed"
  (let (newList
	(childIndex 0))
    (dolist (child (kin:bonded-atom-template-children atom))
      (setq newList (append (assemble-raw-chi-atom-list atom child childIndex) newList))
      (setq childIndex (+ 1 childIndex)))
    (when (== atomIndex 0)
      (unless (== (class parent) kin:DelayedBondedAtomTemplate)
	(setq newList (Cons atom newList))))
    newList
    )
  )
  


;; Last atom in each Chi Dihedral is a child#0 of one of the nodes.
;; The first three atoms are the greatGrandParent:grandParent:parent:lastAtom
;; If the last atom is an H then we only care about CHIs where the second last atom is O or S
;; If the second and third atoms are part of the same aromatic ring then
;; they are in a constrained, aromatic ring then ignore it
;; 

(defun chi-dihedral-is-important (chi aromaticRings dbg)
  (cond
    ;; If any atom is nil then keep the chi
    ((chi-has-nil-atoms chi)
     t)
    ;; If the third atom has an outplug then ignore the chi
    ((multiple-value-bind (at1 at2 at3 at4) (chi-atom-templates chi)
       (and (isA at3 kin:BondedAtomTemplate)
	    (kin:bonded-atom-template-outplug at3)))
     ())
	  ;; The last atom is a hydrogen and we only care about
	  ;; chi's of hydrogen connected to oxygen/sulphur
    ((== (chi-atom4-element chi) :H)
     (if (or (== (chi-atom3-element chi) :O) (== (chi-atom3-element chi) :S))
	 t
	 ()))
	  ;; The last atom is a heavy atom and we only care about
	  ;; heavy atom chi's if the middle two atoms aren't locked
	  ;; in an aromatic ring
    ((chi-is-rotatable chi aromaticRings dbg)
     t)
    (t
     ()))
  )





(defun build-chi-list (atomTemplate constitutionAtoms fragment topologyName )
  (let ((debugName (bformat nil "./debug/atom-template-%s--topology-%s.dot"
			    (symbol-name (getProperty fragment :name)) (symbol-name topologyName) )))
    (render-atom-template:render-dot atomTemplate constitutionAtoms
				     (getProperty fragment :name)
				     (symbol-name topologyName)
				     (Path debugName))
    )
  ;;
  ;; Here build the chi list
  ;;
  (with-open-file (dbg (Path (bformat nil "./debug/build-chi-list-%s--topology-%s.txt"
				      (symbol-name (getProperty fragment :name))
				      (symbol-name topologyName))) :direction :output)
    (let ((rawChiAtoms (assemble-raw-chi-atom-list () atomTemplate 0)))
      (bformat dbg "# Raw chi terminal atoms: %s\n" (map (lambda (x) (kin:atom-template-comment x)) rawChiAtoms ))
      ;;
      ;; Build the raw ChiList
      ;;
      (let* ((residue (makeResidue constitutionAtoms))
	     (mol (let ((m (Molecule))) (addMatter m residue) m))
	     (aromaticRings (aromaticity:identify-aromatic-rings mol))
	     (chiList (kin:ChiList))
	     (chiIndex 0))
	(when aromaticRings
	  (bformat dbg "# Monomer contains aromatic rings\n")
	  (dolist (ring aromaticRings)
	    (bformat dbg "# aromatic ring: ")
	    (dolist (atom ring)
	      (bformat dbg " %s-id#%d" (chem:atomName atom) (chem:getAtomId atom)))
	    (bformat dbg "\n")
	    )
	  )
	(dolist (atom rawChiAtoms)
	  (let ((chiCandidate (Chi atom residue)))
	    (when (chi-dihedral-is-important chiCandidate aromaticRings dbg)
	      (let ((chiDih (chi-create-chi-dihedral chiCandidate)))
		(kin:chi-list-append chiList chiDih)
		(bformat dbg "chi %d %d %d %d # %s\n"
			 (kin:chi-dihedral-atom1-index chiDih)
			 (kin:chi-dihedral-atom2-index chiDih)
			 (kin:chi-dihedral-atom3-index chiDih)
			 (kin:chi-dihedral-atom4-index chiDih)
			 (chi-atom-names-as-string chiCandidate))
		(setq chiIndex (+ chiIndex 1))
		)
	      )
	    )
	  )
	chiList)
      )
    )
  )




;;
;;  (let ((chis))
;;    (setq chis (recursively-walk-atom-template-tree-build-chis chis atomTemplate 0))
;;    )
;;  (debug)
;;)
  


