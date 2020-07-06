
;; Test that header stamps work for builtin classes
(test stamp-of-builtin
      (= (core:instance-stamp (make-hash-table)) (core:class-stamp-for-instances (class-of (make-hash-table)))))
;; Test that rack stamps work for CLOS classes
(defclass mygoofytestclass () ())
(test stamp-from-rack
      (= (core:instance-stamp (make-instance 'mygoofytestclass))
         (core:class-stamp-for-instances (find-class 'mygoofytestclass))))
;; Test that wrapped classes get their stamp from the wrapper
#-use-mps
(test stamp-from-wrapper
      (= (core:instance-stamp (ast-tooling:make-compiler-instance))
         (core:class-stamp-for-instances (find-class 'ast-tooling:compiler-instance))))
;;  This last one is to test Derivable classes
;;     I set up a special case for these where it calls the General_O::get_stamp_() method - but it doesn't look like it is necessary
#-use-mps
(test stamp-of-derivable
      (= (core:instance-stamp (make-instance 'ast-tooling:match-callback))
         (core:class-stamp-for-instances (find-class 'ast-tooling:match-callback))))
