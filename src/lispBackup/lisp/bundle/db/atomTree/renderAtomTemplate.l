
(make-package :render-atom-template)
(in-package :render-atom-template)
(core::use-package 'core)


(defun node-color (atom)
  "Return the border and internal colors (if there is one) for the node"
  (cond
    ((== (class atom) kin:BondedAtomTemplate) "green")
    ((== (class atom) kin:RootBondedAtomTemplate) "red")
    ((== (class atom) kin:DelayedBondedAtomTemplate) "cyan")
    (t "black"))
  )
    

(defun atom-label (atom constitutionAtoms idx)
  (let ((atomid (kin:atom-template-id atom))
	(atomName (kin:atom-template-name atom constitutionAtoms))
	(comment (kin:atom-template-comment atom))
	(outPlug (kin:bonded-atom-template-outplug atom)))
    (bformat nil "%s\\n%s#%d:ch%d\\n%s" (className atom) comment atomid idx
	     (if outPlug
		 (bformat nil "-OUTPLUG[%s]-" (chem:getName outPlug))
		 ""
		 ))
    ))


(defun atom-node-name (atomTemplate)
    (bformat nil " <atom%d>" (kin:atom-template-id atomTemplate)
    )
  )



(defun edge-style (parent atom childIndex outPlug)
  (if (and (== childIndex 0) (not outPlug))
      "style=bold,color=red"
      ""
      )
  )


(defun atom-template-render-recursively (atomTemplate constitutionAtoms idx fout)
  (bformat fout "%s [label=\"%s\",color=%s];\n"
	   (atom-node-name atomTemplate)
	   (atom-label atomTemplate constitutionAtoms idx)
	   (node-color atomTemplate))
  (let ((cidx 0))
    (dolist (child (kin:bonded-atom-template-children atomTemplate))
      (bformat fout "%s -> %s [%s];\n"
	       (atom-node-name atomTemplate)
	       (atom-node-name child)
	       (edge-style atomTemplate child cidx (kin:bonded-atom-template-outplug atomTemplate))
	       )
      (atom-template-render-recursively child constitutionAtoms cidx fout)
      (setq cidx (+ cidx 1))
      ))
  )



(defun render-dot (atomTemplate constitutionAtoms constitutionName topologyName outputFilePath &key renderStubsForAtomIds)
  "Render the atom-template tree as a dot file"
  (ensure-directories-exist outputFilePath)
   (with-open-file (fout outputFilePath :direction :output)
      (bformat fout "digraph G {\n")
      (bformat fout "size=\"7.5,11\";\n")
      (bformat fout "subgraph cluster0 {\n")
      (bformat fout "label=\"Constitution[%s]  Topology[%s]\";\n" constitutionName topologyName)
      (atom-template-render-recursively atomTemplate constitutionAtoms 0 fout)
      (bformat fout "}\n")
      (bformat fout "}\n")
      )
    )


(export 'render-dot)


