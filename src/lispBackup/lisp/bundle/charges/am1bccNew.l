(include "CANDO/db/pattern/aromaticity.l")
(include "CANDO/db/buildMolecule.l")







(defvar *Am1BccParameters* (IntegerKeyObjectDictionary))
(print "About to set up parameters")
(for pair '((110111 0.0000)
	    (110112 0.0042) (110113 -0.0753) (110114 -0.0500) (110115 -0.0269)
	    (110116 0.0073) (110117 -0.0943) (110121 0.1582) (110122 0.0374) (110123 -0.0193)
	    (110124 0.1252) (110125 0.1825) (110131 0.0718) (110141 0.2579)
	    (110142 0.4078) (110151 0.1821) (110152 0.4263) (110153 0.4608) (110161 0.1540)
	    (110171 0.0713) (110172 0.0734) (110173 0.1274) (110174 0.3009)
	    (110191 0.0393) (120112 0.0000) (120113 -0.0866) (120114 -0.0192) (120115 0.0333)
	    (120116 0.0118) (120117 -0.1098) (120121 0.0820) (120122 0.0026)
	    (120123 -0.0271) (120124 0.1406) (120125 0.4897) (120131 0.0441) (120141 0.2986)
	    (120142 0.4154) (120151 0.2406) (120152 0.4307) (120153 0.4509)
	    (120161 0.2128) (120171 0.0625) (120172 0.0844) (120173 0.1257) (170151 0.3273)
	    (170152 0.5416) (170153 0.5169) (170171 0.1402) (170172 0.1660)
	    (170173 0.2312) (170174 0.3528) (170191 0.1369) (210121 0.0000) (210122 -0.0302)
	    (210123 -0.1185) (210124 0.0867) (210131 -0.0517) (120174 0.2728)
	    (120191 0.0000 ) (130113 0.0000) (130114 0.0693) (130115 0.0302) (130116 0.0839)
	    (130117 -0.0297) (130121 0.1522) (130122 0.0069) (130123 0.0388)
	    (130124 0.2193) (130125 0.2406) (130131 0.1336) (130141 0.3529) (130142 0.5230)
	    (130151 0.2242) (130152 0.4296) (130153 0.5077) (130171 0.1055)
	    (130172 0.1123) (130173 0.1253) (130174 0.2752) (130191 0.1292) (140114 0.0000)
	    (140115 -0.0308) (140116 0.0206) (140117 -0.0895) (140122 0.0670)
	    (140123 -0.0432) (140124 0.1394) (140125 0.0070) (140131 0.0901) (140141 0.2828)
	    (140142 0.4820) (140151 0.1826) (140152 0.4319) (140153 0.4840)
	    (140171 0.1115) (140172 0.0486) (140173 0.1106) (140174 0.2601) (140191 0.0928)
	    (150115 0.0000) (150116 0.0040) (150117 -0.1070) (230153 0.3418)
	    (230161 0.1766) (230171 0.0561) (230172 0.0642) (230173 0.1802) (230191 -0.0497)
	    (240124 0.0000) (240125 0.1043) (240131 -0.0985) (240141 0.1287)
	    (240142 0.1498) (240151 0.0315) (240152 0.1731) (150121 0.0558) (150122 0.0062)
	    (150123 -0.0543) (150124 0.0520) (150125 0.0798) (150131 0.0286)
	    (150141 0.3987) (150142 0.3545) (150151 0.2251) (150152 0.4586) (150153 0.4018)
	    (150161 0.2343) (150171 0.0021) (150172 0.0899) (150173 0.2338)
	    (150174 0.2886) (150191 0.0575) (160116 0.0000) (160117 -0.1075) (160121 0.0816)
	    (160122 -0.0074) (160123 -0.0452) (160124 0.1394) (160125 0.1877)
	    (160131 0.0451) (160141 0.3270) (160142 0.3895) (160151 0.2269) (160152 0.4285)
	    (160153 0.4445) (160161 0.1938) (160171 0.0349) (160172 0.0784)
	    (160173 0.1401) (160174 0.2859) (160191 0.0000) (170117 0.0000) (170121 0.1929)
	    (170122 0.0799) (170123 0.0635) (170124 0.2190) (170125 0.2100)
	    (170131 0.1703) (170141 0.4597) (170142 0.4543) (420171 -0.1976) (420172 -0.3077)
	    (420173 -0.2425) (420191 -0.3756) (510151 0.0000) (510152 0.2576)
	    (510153 0.2971) (510161 -0.0936) (510171 -0.0547) (510172 -0.0260) (510173 0.1105)
	    (510191 -0.1718) (520152 0.0000) (210141 0.2527) (210142 0.2947)
	    (210151 0.0757) (210152 0.3531) (210153 0.3359) (210161 0.1037) (210171 -0.0204)
	    (210172 -0.0369) (210173 0.0876) (210174 0.1898) (210191 -0.2048)
	    (220122 0.0000) (220123 -0.0327) (220124 0.1326) (220125 0.0236) (220131 -0.0059)
	    (220141 0.1682) (220142 0.3046) (220151 0.1593) (220152 0.3657)
	    (220153 0.3322) (220171 -0.0155) (220172 -0.0015) (220173 0.0838) (220174 0.2067)
	    (220191 -0.0865) (230123 0.0000) (230124 0.1794 ) (230131 0.0202)
	    (230141 0.2538) (230142 0.4175) (230151 0.2024) (240141 0.0535) (240142 0.4287)
	    (240151 0.1684) (240152 0.3764) (240153 0.4265) (250225 0.0000)
	    (310241 0.1315) (310242 0.2707) (310251 0.1583) (310252 0.2718) (310253 0.2792)
	    (410241 0.0000) (410251 -0.5630) (510252 0.3295) (150215 0.0000)
	    (150325 -0.1168) (210631 0.0465) (330631 0.1317) (240631 -0.0817) (250631 -0.0543)
	    (250651 0.1473) (160716 0.0000) (160717 -0.0653) (160723 -0.0243)
	    (160731 -0.1518) (160741 0.2240) (240153 0.2714) (240161 0.1150) (240171 -0.0554)
	    (240172 -0.0603) (240173 0.0192) (240174 0.1171) (240191 -0.2444)
	    (250191 -0.1634) (310131 0.0000) (310141 0.2229) (310142 0.2336) (310151 0.0442)
	    (310152 0.2152) (310153 0.2093) (310161 0.0083) (310171 0.0375)
	    (310172 0.0014) (310173 0.0699) (310174 0.1712) (310191 0.2010) (410141 0.0000)
	    (410142 0.2762) (410151 -0.1072) (410161 0.0592) (410171 -0.2769)
	    (410172 -0.2065) (410173 -0.1309) (410174 0.0181) (410191 -0.1593) (420142 0.0000)
	    (420151 -0.3626) (420152 0.0675) (160751 0.2699) (170717 0.0000)
	    (170723 0.0885) (170724 0.2630) (170731 0.1294) (170741 0.2464) (170751 0.3144)
	    (230723 0.0000) (230724 0.1698) (230731 0.0677) (230741 0.1806)
	    (230751 0.1565) (240724 0.0000) (240731 -0.0723) (240741 0.0456) (240751 0.0419)
	    (310741 0.0710) (310751 0.0983) (410751 0.0587) (510751 0.0000)
	    (160816 0.0000) (160817 -0.0653) (160823 -0.0243) (160831 -0.1518) (160841 0.2240)
	    (160851 0.2699) (170817 0.0000) (170823 0.0885) (520172 -0.2350)
	    (520191 -0.4040) (530171 -0.2041) (530172 -0.2761) (530173 -0.1970) (530174 -0.1490)
	    (530191 -0.4588) (610172 -0.0555) (610191 -0.0258) (120212 0.0000)
	    (120215 -0.0631) (130223 0.0547) (130224 0.2877) (130225 0.1624) (130241 0.2496)
	    (130242 0.6836) (140231 0.2391) (140232 0.1890) (140233 0.2755)
	    (140251 0.2834) (140252 0.4447) (140253 0.5617) (150215 0.0000) (150224 0.2204)
	    (150231 0.2335) (150242 0.7150) (150251 0.3429) (150252 0.3052)
	    (230224 0.1731) (240224 0.0000) (240225 0.0040) (240231 -0.0467) (170824 0.2630)
	    (170831 0.1294) (170841 0.2464) (170851 0.3144) (230824 0.1698)
	    (240824 0.0000) (240841 0.0456) (110931 0.1615) (110951 0.0833) (120931 0.1844)
	    (120951 0.1879) (130931 0.3046) (130951 0.1969) (140931 0.2653)
	    (140951 0.1713) (150931 0.2549) (150951 0.3281) (160931 0.2797) (160951 0.2541)
	    (170931 0.3370) (170951 0.3476) (230931 -0.1500) (310941 0.2354)
	    (310942 0.3195) (310951 0.1530) (310952 0.3916) (310953 0.3228) (510953 0.5218)
	    )
     (print (% "Adding term %s" (repr pair))
     (put *Am1BccParameters* (car pair) (cadr pair))))





(defun setAm1BccType (a id)
  (setProperty a 'Am1BccType id))

(defun getAm1BccType (a)
  (getPropertyOrDefault a 'Am1BccType ()))



;;
;; Return a text description of an atom
(defun describeAtom (a)
  (if a
      (multiple-value-bind (aromatic stepAssigned) (aromaticity:is-aromatic a)
	(% "%s type[%s] %s %s %s" (chem:getName a)
	   (getAm1BccType a)
	   (ringMembershipAsString a)
	   aromatic
	   stepAssigned)
	)
    (debug))
  )




;;
;; Set the a property in the bond that registers the Am1Bcc bond type
(defun setBondType (b id)
;  (print (% "Setting bond type[%d] for bond: %s" id (repr b)))
  (setProperty b 'Am1BccType id))

(defun hasBondType (b)
  (hasProperty b 'Am1BccType))

(defun getBondType (b)
  (getPropertyOrDefault b 'Am1BccType ()))




(defparameter *carboxylNitroPattern* (ChemInfo :smarts "[$(C1(=O2)-[O&-;X1]3),$([N;+]1(=O2)-[O&-;X1]3)]"))

;;
;; Assign Am1Bcc bond types for all bonds
;; For the rules see Jakalian, Jack, and Bayly • Vol. 23, No. 16 • Journal of Computational Chemistry
(defun assignAm1BccBondTypes (mol allRings)
					; Identify the aromatic bonds
					; any pair of atoms in the same ring that has a single or
					; double bond is an aromatic bond
  (for ring allRings
       (let ((ringBonds (ringBonds ring)))
	 (for bond ringBonds
	      (let ((a1 (getAtom1 bond))
		    (a2 (getAtom2 bond)))
		(when (and (aromaticity:is-aromatic a1)
			   (aromaticity:is-aromatic a2))
		  (cond
		    ((== (getOrder bond) :singleBond)
		     (setBondType bond 7))
		    ((== (getOrder bond) :doubleBond)
		     (setBondType bond 8))
		    (true ())))))))
					; First look for delocalized bonds - carboxyl groups, nitro groups
    (for a (allAtomsAsCons mol false)
	 (when (matches *carboxylNitroPattern* a)
	   (let ((match (getMatch carboxylNitroPattern a)))
	     (let* ((a1 (tag match "1"))
		    (a2 (tag match "2"))
		    (a3 (tag match "3"))
		    (doubleBond (bondTo a1 a2))
		    (singleBond (bondTo a1 a3)))
	       (setBondType doubleBond 9)
	       (setBondType singleBond 9)))))
  (for bond (allBondsAsCons mol false)
       (when (not (hasBondType bond))
	 (let ((a1 (getAtom1 bond))
	       (a2 (getAtom2 bond)))
	   (cond
	    ((== (getOrder bond) :singleBond)
	     (cond
	      ((or (and (== (getIonization a1) -1) (== (getIonization a2) +1))
		   (and (== (getIonization a1) 1) (== (getIonization a2) -1)))
	       (setBondType bond 06))	; +- polarized bond
	      ((or (and (== (getIonization a1) 0) (== (getIonization a2) -1))
		   (and (== (getIonization a1) -1) (== (getIonization a2) 0)))
	       (setBondType bond 09))	    ; 0- polarized bond
	      (true (setBondType bond 1)))) ; simple single bond
	    ((== (getOrder bond) :doubleBond)
	     (setBondType bond 02))	      ; simple double bond
	    (true (setBondType bond 03))))))  ; triple bond
  )
  


;; CSMARTS rules for carbon atom types
(defvar *Cx3=C* (ChemInfo :smarts "[C&X3]=C"))
(defvar *Cx3=NorP* (ChemInfo :smarts "[C&X3]=[N,P]"))
(defvar *Cx3=OorS* (ChemInfo :smarts "[C&X3]=[O,S]"))
(defvar *cX3-oX2_cX3~nX2* (ChemInfo :smarts "[$([c&X3]-[o&X2]),$([c&X3]~[n&X2])]"))

(defun applyCarbonAtomTypes (a)
  (let ((numBonds (numberOfBonds a)))
    (cond
     ((== numBonds 4) (setAm1BccType a 11))
     ((!= numBonds 3) (setAm1BccType a 15))
     ((not (aromaticity:is-aromatic a))
      (cond
       ((matches *Cx3=C* a)    (setAm1BccType a 12)) 
       ((matches *Cx3=NorP* a) (setAm1BccType a 13))
       ((matches *Cx3=OorS* a)  (setAm1BccType a 14))
       (true
	(raise (LispError (% "Could not assign type for %s" (description a)))))))
     (true
      (cond
       ((matches *cX3-oX2_cX3~nX2* a) (setAm1BccType a 17))
       (true (setAm1BccType a 16)))))))



(defvar *matchLactoneLactam* (ChemInfo :smarts "[$(O=C1-[O,N]~[*]~[*]~[*;?1]),$(O=C1-[O,N]~[*]~[*]~[*]~[*;?1]),$(O=C1-[O,N]~[*]~[*]~[*]~[*]~[*;?1]),$(O=C1-[O,N]~[*]~[*]~[*]~[*]~[*]~[*;?1])]"))

(defvar *matchEsterAcid* (ChemInfo :smarts "O=C-[O,N]"))

(defun applyOxygenAtomTypes (a)
  (cond
   ((== (numberOfBonds a) 2)         (setAm1BccType a 31))
   ((matches *matchLactoneLactam* a) (setAm1BccType a 33))
   ((matches *matchEsterAcid* a)     (setAm1BccType a 32))
   (true (setAm1BccType a 31))))

(defvar *N_highly_deloc* (ChemInfo :smarts "N(-[*]=[*])-[*]=[*]"))
(defvar *Ndeloc* (ChemInfo :smarts "N-[*]=[*]"))


;; Figure out what these properties deloc and hdeloc really mean
(defun isHighlyDeloc (a) (matches *N_highly_deloc* a))
(defun isDeloc (a) (matches *Ndeloc* a))


(defun applyNitrogenAtomTypes (a)
  (let ((numBonds (numberOfBonds a)))
    (cond
     ((== numBonds 4) (setAm1BccType a 21))
     ((== numBonds 3)
      (cond
       ((isHighlyDeloc a) (setAm1BccType a 23))
       ((isDeloc a)       (setAm1BccType a 22))
       (true              (setAm1BccType a 21))))
     ((!= numBonds 2) (setAm1BccType a 25))
     (true
      (cond
       ((>= (getIonization a) 0) ; not anion
	(if (hasBondWithOrder a :doubleBond)
	    (setAm1BccType a 24)
	  (setAm1BccType a 25)))
       ((isDeloc a) (setAm1BccType a 22))
       (true (setAm1BccType a 21)))))))


(defun applyPhosphorousAtomTypes (a)
  (cond
   ((== (numberOfBonds a) 4) (setAm1BccType a 42))
   ((!= (numberOfBonds a) 3) (setAm1BccType a 41))
   ((hasBondOfOrder a :doubleBond) (setAm1BccType a 42))
   (true (setAm1BccType a 41))))


;; Calculate all atom types
;; See Jakalian, Jack, and Bayly • Vol. 23, No. 16 • Journal of Computational Chemistry
(defun applyAtomTypes (mol)
  (for a (allAtomsAsCons mol false)
       (let ((ae (chem:getElement a)))
	 (cond
	  ((== ae :C) (applyCarbonAtomTypes a))
	  ((== ae :N) (applyNitrogenAtomTypes a))
	  ((== ae :O) (applyOxygenAtomTypes a))
	  ((== ae :S)
	   (let ((numBonds (numberOfBonds a)))
	     (cond
	      ((== numBonds 4) (setAm1BccType a 53))
	      ((== numBonds 3) (setAm1BccType a 52))
	      (true  (setAm1BccType a 51)))))
	  ((== ae :P) (applyPhosphorousAtomTypes a))
	  ((== ae :Si) (setAm1BccType a 61))
	  ((== ae :H)  (setAm1BccType a 91))
	  ((== ae :F)  (setAm1BccType a 71))
	  ((== ae :Cl) (setAm1BccType a 72))
	  ((== ae :Br) (setAm1BccType a 73))
	  ((== ae :I)  (setAm1BccType a 74))
	  (true (raise (LispError (% "Unhandled element[%s]" ae))))))))



(defun lookup-am1-bcc-key (a1 bond a2)
  (let ((type1 (getAm1BccType a1))
	(type2 (getAm1BccType a2))
	(bondType (getBondType bond))
	(sign 1.0))
    (when ( > type1 type2 )
      (let (sw)
	(setq sw a1) (setq a1 a2) (setq a2 sw)
	(setq type1 (getAm1BccType a1))
	(setq type2 (getAm1BccType a2))
	(setq sign -1.0)))
    (list (+ (* 10000 type1) (* 100 bondType) type2) sign)
    ))

(defun lookup-am1-bcc-correction (a1 bond a2)
  (let* ((keySign (lookup-am1-bcc-key a1 bond a2))
	 (key (car keySign))
	 (sign (cadr keySign)))
    (* (get *Am1BccParameters* key) sign)))

;;
;; Given a molecule (mol) assign the Am1Bcc atom and bond types
;; these type values are put into the 'Am1BccType property for each
;; atom and bond
(defun calculate-am1-bcc-corrections (mol)
  "Return an ObjectMap from Atoms to Am1Bcc charge corrections"
  (let ((allRings (aromaticity:identify-aromatic-rings mol))
	(correctionMap (ObjectMap)))
;    (print "assign bond types")
    (assignAm1BccBondTypes mol allRings)
    (applyAtomTypes mol)
    (for a (allAtomsAsCons mol false)
	 (let ((atomCorrection 0.0))
	   (for b (bondsAsList a)
		(let* ((other (getOtherAtom b a))
		       (bondCorrection (lookup-am1-bcc-correction a b other)))
		  (setq atomCorrection (+ atomCorrection bondCorrection))))
	   (extend correctionMap a atomCorrection)))
    correctionMap
    ))





(defun calculate-am1bcc-charges (atomsToAm1Charges atomsToBccCorrections)
  "Combine Am1 charges with Bcc corrections and return an ObjectMap of atoms to am1bcc charges"
  (let ((atomsToAm1bccCharges (ObjectMap)))
    (for atom (allKeysAsCons atomsToAm1Charges)
	 (let* ((am1Charge (lookup atomsToAm1Charges atom))
		(bccCorrection (lookup atomsToBccCorrections atom)))
	   (extend atomsToAm1bccCharges atom (+ am1Charge bccCorrection))))
    atomsToAm1bccCharges))






(defun describeFragmentAndResidue (frag res am1Charges bccCorrections am1bccCharges)
  (for a (contentsAsCons res)
       (print (% "        atom: %s" (describeAtom a))))
  (print "Describing bonds")
  (for a (allAtomsAsCons res false)
       (print (% "Atom %s -----" (chem:getName a)))
       (for b (bondsAsList a)
	    (let* ((other (getOtherAtom b a))
		   (keySign (lookup-am1-bcc-key a b other)))
	      (print (% " am1-bcc bond key: %s" (repr keySign)))))
       (let ((am1Charge (lookup am1Charges a))
	     (bccCorrection (lookup bccCorrections))
	     (charge (lookup am1bccCharges a)))
	 (print ( % "   am1[%8.3f] bccCorrection[%8.3f] am1bccCharge[%8.3f]" am1Charge bccCorrection charge))
	 )))



(when (isTopLevelScript)
  (let ((cd (ChemDraw :fileName "aspirin.cdxml")))
    ;;
    ;; Put the residues into molecules and
    ;; start classifying the atoms
    ;;
    (for frag (getFragments cd)
	 (let* ((mol (Molecule))
		(res (getEntireResidue frag)))
	   (let* ((name (getProperty frag :name)))
	     (print "-------------------")
	     (print (% "Fragment: %s" frag))
	     (print (% "    name: %s" name))
	     (print (% "    properties====="))
	     (print (% "%s" (getProperties frag))))
	   (addMatter mol res)
	   (buildMolecule mol)
	   (let ((bccCorrections (calculate-am1-bcc-corrections mol))))
	   (describeFragmentAndResidue frag res)
	   ))))
