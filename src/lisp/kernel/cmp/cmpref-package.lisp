(defpackage #:cmpref
  (:use #:cl)
  (:export #:generate-virtual-machine-header)
  (:export #:*startup-primitives-as-list*
           #:+bytecode-ltv-ops+ #:+uaet-codes+ #:+debug-info-ops+)
  (:export #:constant-arg-p #:label-arg-p #:keys-arg-p #:unmask-arg
           #:decode-instr))
