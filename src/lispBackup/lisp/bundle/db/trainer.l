
;;
;; A few additional methods for trainers
;;

(defMethod trainer-dynamics Trainer (trainer &optional val)
  (if val
      (setData trainer 'dynamics val)
      (if (hasData trainer 'dynamics) 
	  (getData trainer 'dynamics)
	  ())))


(defMethod trainer-has-mopac-interface Trainer (trainer)
    (hasData trainer 'MopacInterface))


(defMethod trainer-mopac-interface Trainer (trainer &optional value)
  (if value
      (setData trainer 'MopacInterface value)
    (getData trainer 'MopacInterface)))


(defMethod trainer-bcc-corrections Trainer (trainer &optional value)
  (if value
      (setData trainer 'BccCorrections value)
    (getData trainer 'BccCorrections)))

(defMethod trainer-focus-residue-charges Trainer (trainer &optional charges)
  (if charges
      (setData trainer 'FocusResidueCharges charges)
    (getData trainer 'FocusResidueCharges)))


(defMethod trainer-focus-residue Trainer (trainer &optional focusResidue)
  (if focusResidue 
      (setData trainer 'FocusResidue focusResidue)
    (getData trainer 'FocusResidue)))



(defMethod trainer-focus-monomer Trainer (trainer &optional value)
  (if value
      (setData trainer 'FocusMonomer value)
    (getData trainer 'FocusMonomer)))

(defMethod trainer-oligomer Trainer (trainer &optional value)
  (if value
      (setData trainer 'oligomer value)
    (getData trainer 'oligomer)))







(defMethod trainer-header-focus-constitution-name TrainerHeader (header &optional value)
  (if value
      (setData header 'FocusConstitutionName value)
    (getData header 'FocusConsitutionName)))


(defMethod trainer-header-focus-stereoisomer-name TrainerHeader (header &optional value)
  (if value
      (setData header 'FocusStereoisomerName value)
    (getData header 'FocusStereoisomerName)))


(defMethod trainer-header-focus-topology-name TrainerHeader (header &optional value)
  (if value
      (setData header 'FocusTopologyName value)
    (getData header 'FocusTopologyName)))


