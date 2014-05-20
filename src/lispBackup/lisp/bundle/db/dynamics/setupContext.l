

(require "CANDO/db/dynamics/openmm.l")


(defvar *struct* (cadr *ARGS*))
(print (% "Loading file[%s]" *struct*))
(let* ((matter (loadArchive *struct*))
       (context (setup-gaff-reference-context :matter matter)))
  (save-system context (Path "_system.dat"))
  (save-checkpoint context "_checkpoint.dat"))
 