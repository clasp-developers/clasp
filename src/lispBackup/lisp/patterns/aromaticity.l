;;
;; Recognize aromaticity in Matter
;;
;;

(print "Loading aromaticity")
(include "CANDO/tools/common.l")


(make-package :aromaticity)
(in-package :aromaticity)
(core::use-package 'core)
(use-package 'u)

(export 'identify-aromatic-rings
	'is-aromatic
	)



;;
;;
;; Describe ring membership in a string
;;
(defun ringMembershipAsString (a)
  (let ((member (% "%s%s%s%s%s"
		   (if (inRingSize a 3) "3" "")
		   (if (inRingSize a 4) "4" "")
		   (if (inRingSize a 5) "5" "")
		   (if (inRingSize a 6) "6" "")
		   (if (inRingSize a 7) "7" ""))))
    (if (!= member "") (% "ring%s" member) "")))




;; Set the aromatic type of an atom
(defun setAromaticityType (a arom rule)
  (setIsAromatic a true)
  (setProperty a 'Aromaticity arom)
  (setProperty a 'stepAssigned rule))


;; Return the aromaticity type of the atom followed by the stepAssigned or (values nil nil)
(defun is-aromatic (a)
  (values (getPropertyOrDefault a 'Aromaticity ())
	  (getPropertyOrDefault a 'stepAssigned ())))



(defparameter *rule1a* (ChemInfo :smarts "[x]1-[x]=[x]-[x]=[x]-[x]=[x&?1]"))

;; Apply aromaticity rule1 of Jakalian, Jack, and Bayly • Vol. 23, No. 16 • Journal of Computational Chemistry
(defun aromaticRule1 (atomsInRings)
    (for a atomsInRings
;	 (print (% "test1 on %s" (description a)))
	 (when (matches *rule1a* a)
	   (setAromaticityType a 'ar6 'rule1))
	 ))

(defparameter *ar6test* (list 'ar6 (lambda (a) (== (getPropertyOrDefault a 'Aromaticity false) 'ar6))))

(defparameter *rule2a* (ChemInfo :tests *ar6test* :smarts "[x]1=[x]-[x]=[x]-[<ar6>]-[<ar6>]-[x&?1]"))
(defparameter *rule2b* (ChemInfo :tests *ar6test* :smarts "[x]1-[x]=[x]-[<ar6>]-[<ar6>]-[x]=[x&?1]"))


;; Apply aromaticity rule of Jakalian, Jack, and Bayly • Vol. 23, No. 16 • Journal of Computational Chemistry
(defun aromaticRule2 (atomsInRings)
  (let ((atomsLeft atomsInRings)
	(failedAtoms ())
	(rulePass 0)
	(foundSome true))
      (while foundSome
	(setq foundSome false)
	(for a atomsLeft
	     (if (or (matches *rule2a* a)
		     (matches *rule2b* a))
		 (progn
		   (setq foundSome true)
		   (setAromaticityType a 'ar6 'rule2))
	       (setq failedAtoms (Cons a failedAtoms))))
	(setq atomsLeft failedAtoms)
	(setq rulePass (+ rulePass 1)))))


(defparameter *rule3a* (ChemInfo :tests *ar6test*
			    :smarts "[x]1=[x]-[<ar6>]-[<ar6>]-[<ar6>]-[<ar6>]-[x&?1]"))

;; Apply aromaticity rule of Jakalian, Jack, and Bayly • Vol. 23, No. 16 • Journal of Computational Chemistry
(defun aromaticRule3 (atomsInRings)
  (let ((atomsLeft atomsInRings)
	  (failedAtoms ())
	  (rulePass 0)
	  (foundSome true))
      (while foundSome
	(setq foundSome false)
	(for a atomsLeft
	     (if (matches *rule3a* a)
		 (progn
		   (setq foundSome true)
		   (setAromaticityType a 'ar6 'rule3)
		   )
	       (setq failedAtoms (Cons a failedAtoms))))
	(setq atomsLeft failedAtoms)
	(setq rulePass (+ rulePass 1)))))

(defparameter *rule4a* (ChemInfo :smarts "[x]1-[x]=[x]-[C&+]-[x]=[x]-[x]=[x&?1]"))
(defparameter *rule4b* (ChemInfo :smarts "[x]1=[x]-[C&+]-[x]=[x]-[x]=[x]-[x&?1]"))
(defparameter *rule4c* (ChemInfo :smarts "[x]1-[C&+]-[x]=[x]-[x]=[x]-[x]=[x&?1]"))
(defparameter *rule4d* (ChemInfo :smarts "[C&+]1-[x]=[x]-[x]=[x]-[x]=[x]-[C&?1]"))

;; Apply aromaticity rule of Jakalian, Jack, and Bayly • Vol. 23, No. 16 • Journal of Computational Chemistry
(defun aromaticRule4 (atomsInRings)
    (for a atomsInRings
	 (when (or (matches *rule4a* a)
		   (matches *rule4b* a)
		   (matches *rule4c* a)
		   (matches *rule4d* a)
		   )
	   (setAromaticityType a 'ar7 'rule4)
	   )))

(defparameter *ar67test* (list 'ar67 (lambda (a) (or (== (getPropertyOrDefault a 'Aromaticity false) 'ar6)
					       (== (getPropertyOrDefault a 'Aromaticity false) 'ar7)))))
(defparameter *rule5a* (ChemInfo :tests *ar67test*
			   :smarts "[y]1-[x&!<ar67>]=[x&!<ar67>]-[x&!<ar67>]=[x&!<ar67>]-[y&?1]"))
(defparameter *rule5b* (ChemInfo :tests *ar67test*
			   :smarts "[x&!<ar67>]1-[y]-[x&!<ar67>]=[x&!<ar67>]-[x&!<ar67>]=[x&?1]"))
(defparameter *rule5c* (ChemInfo :tests *ar67test*
			   :smarts "[x&!<ar67>]1=[x&!<ar67>]-[y]-[x&!<ar67>]=[x&!<ar67>]-[x&?1]"))

;; Apply aromaticity rule of Jakalian, Jack, and Bayly • Vol. 23, No. 16 • Journal of Computational Chemistry
(defun aromaticRule5 (atomsInRings)
    (for a atomsInRings
	 (when (or (matches *rule5a* a)
		   (matches *rule5b* a)
		   (matches *rule5c* a))
	   (setAromaticityType a 'ar5 'rule5)
	   )))


(defun is-ring-aromatic (ring)
  (dolist (atom ring)
    (when (not (is-aromatic atom)) (return-from is-ring-aromatic nil))
    )
  t)
	     
(defun select-aromatic-rings (allRings)
  (let (aromaticRings)
    (dolist (ring allRings)
      (when (is-ring-aromatic ring)
	(setq aromaticRings (Cons ring aromaticRings))))
    aromaticRings))





;;
;; Identify all rings, isolate atoms in rings and apply all of the aromaticity rules
;; from Jakalian, Jack, and Bayly • Vol. 23, No. 16 • Journal of Computational Chemistry
;; Return the rings for bond type assignment
(defun identify-aromatic-rings (mol)
  (let ((allRings (identifyRings mol))
	(atomsInRings (common:filter (lambda (x) (isInRing x)) (allAtomsAsCons mol false))))
    (aromaticRule1 atomsInRings)
    (aromaticRule2 atomsInRings)
    (aromaticRule3 atomsInRings)
    (aromaticRule4 atomsInRings)
    (aromaticRule5 atomsInRings)
    (select-aromatic-rings allRings)))



