

(make-package :render-atom-tree)
(in-package :render-atom-tree)
(core::use-package 'core)
(use-package 'u)

(defun node-color (atom)
  (let ((atomType (kin:typeSymbol atom)))
    (cond
      ((== atomType 'kin:bonded) "green")
      ((== atomType 'kin:jump) "red")
      ((== atomType 'kin:delayed) "cyan")
      ((== atomType 'kin:rootBonded) "purple")
      (t "black"))))

(defun relative-atom-id (atom otherAtom)
  (multiple-value-bind (atomMid atomRid atomAid) (kin:atom-id-values atom)
    (multiple-value-bind (otherMid otherRid otherAid) (kin:atom-id-values otherAtom)
      (if (== atomMid otherMid)
	  (if (== atomRid otherRid)
	      (bformat nil "(%s)" otherAid)
	      (bformat nil "(%d,%d)" otherRid otherAid)
	      )
	  (bformat nil "(%d,%d,%d)" otherMid otherRid otherAid)))))
    

(defun atom-label (atom)
  (let* ((parent (kin:parent atom))
	 (atomid (kin:atomId atom))
	 (childIndex (kin:indexOfChild parent atom))
	 (stub1Name (relative-atom-id atom (kin:stubAtom1 atom)))
	 (stub2Name (relative-atom-id atom (kin:stubAtom2 atom)))
	 (stub3Name (relative-atom-id atom (kin:stubAtom3 atom)))
	 )
    (bformat nil "%s\\n%s:atomid%d:child%d\\nstub1%s\\nstub2%s\\nstub3%s"
	     (kin:typeSymbol atom)
	     (kin:comment atom)
	     (caddr (kin:atomId atom))
	     childIndex
	     stub1Name
	     stub2Name
	     stub3Name
	     )
    
    ))


(defun atom-node-name (atom)
  (multiple-value-bind (mid rid aid) (kin:atom-id-values atom)
    (bformat nil " <mol%d/res%d/atom%d>" mid rid aid)
    )
  )

(defun monomer-info (foldTree mid rid)
  (let ((monomerNode (kin:fold-tree-lookup-monomer-id foldTree (list mid rid))))
    (log (% "Got monomerNode: %s" (repr monomerNode)))
    (let* ((topology (kin:monomer-node-topology monomerNode))
	   (constitution (getConstitution topology))
	   (monomerName (kin:monomerName monomerNode))
	   )
      (bformat nil "Mol%d/Res%d\\nMonomerName[%s]\\nConstitution[%s]\\nTopology[%s]"
	       mid rid
	       (symbol-name monomerName)
	       (constitutionName constitution)
	       (chem:getName topology)
	       )
      )
    )
  )


(defun edge-style (parent atom)
  (let ((childIndex (kin:indexOfChild parent atom)))
    (if (== childIndex 0)
	"style=bold,color=red"
	""
	)))

(defun render-atom-tree-dot (conformation outputFilePath &key renderStubsForAtomIds)
  "Render the atom-tree as a dot file. Pass atomIds through renderStubsForAtomIds
to render edges that represent stub atom relationships"
  (let ((atomTree (kin:getAtomTree conformation))
	(foldTree (kin:getFoldTree conformation))
	(edges ())
	(atoms ()))
    (ensure-directories-exist outputFilePath)
    (with-open-file (fout outputFilePath :direction :output)
      (bformat fout "digraph G {\n")
      (bformat fout "size=\"11,8\";\n")
      (bformat fout "margin=0;\n")
      (bformat fout "orientation=landscape;")
      (dotimes (mid (kin:numberOfMolecules atomTree))
	(bformat fout "subgraph clustermol%d {\n" mid )
	(bformat fout "label=\"Molecule#%d\";\n" mid )
	(dotimes (rid (kin:numberOfResiduesInMolecule atomTree mid))
	  (bformat fout "subgraph clustermol%dres%d {\n" mid rid )
	  (bformat fout "label=\"%s\";\n" (monomer-info foldTree mid rid))
	  (dotimes (aid (kin:numberOfAtomsInResidueInMolecule atomTree mid rid))
	    (let* ((atom (kin:atom-tree-lookup-atomid atomTree (list mid rid aid)))
		   (parent (kin:parent atom)))
	      (bformat fout " %s" (atom-node-name atom))
	      (bformat fout " [ label=\"%s\",color=%s];\n" (atom-label atom) (node-color atom))
	      (when parent
		(setq edges (acons parent atom edges)))
	      (setq atoms (cons atom atoms)))
	    )
	  (bformat fout "}\n")
	  )
	(bformat fout "}\n")
	(dolist (e edges)
	  (bformat fout "%s -> %s [%s];\n" (atom-node-name (car e)) (atom-node-name (cdr e)) (edge-style (car e) (cdr e) ))
	  )
	(dolist (a atoms)
	  (when (or (== (kin:typeSymbol a) 'kin:delayed)
		    (member (kin:atomId a) renderStubsForAtomIds))
	    (bformat fout "%s -> %s [arrowhead=crow,label=\"s1\"];\n" (atom-node-name (kin:stubAtom1 a)) (atom-node-name a))
	    (bformat fout "%s -> %s [arrowhead=crow,label=\"s2\"];\n" (atom-node-name (kin:stubAtom2 a)) (atom-node-name a))
	    (bformat fout "%s -> %s [arrowhead=crow,label=\"s3\"];\n" (atom-node-name (kin:stubAtom3 a)) (atom-node-name a))
	    )
	  )
	(bformat fout "}\n")
	)
      )
    )
  )

(export 'render-atom-tree-dot)


(defun render-for-trainer (trainer)
  (let ((oligomer (u:trainer-oligomer trainer)))
    (let ((filename (bformat nil "%s.dot" (sequenceAsFileName oligomer) )))
      (let ((conformation (kin:Conformation :oligomers (list oligomer) )))
	(let ((atomTree (kin:getAtomTree conformation)))
	  (let ((outpath (append (copyPath (getPath (getHeader trainer))) filename)))
;;	    (debug)
	    (render-atom-tree-dot conformation outpath)
	    )
	  )
	)
      )
    )
  )
(export 'render-for-trainer)
	 