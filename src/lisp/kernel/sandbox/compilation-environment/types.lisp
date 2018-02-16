(in-package #:compilation-environment)

;;;; Information in the environment about low level type details.
;;;; FIXME: That system should be removed, along with this file.
;;;; Failing that, these should be set up as flags proper.

#+clasp
(progn
  (defmethod cleavir-env:has-extended-char-p ((environment compilation-environment))
    #+unicode t #-unicode nil)
  (defmethod cleavir-env:float-types ((environment compilation-environment))
    '(#+short-float short-float single-float double-float #+long-float long-float))
  (defmethod cleavir-env:upgraded-complex-part-types ((environment compilation-environment))
    '(real))
  (defmethod cleavir-env:upgraded-array-element-types ((environment compilation-environment))
    core::+upgraded-array-element-types+))

#-clasp
(error "Can't build from non-clasp yet :(")
