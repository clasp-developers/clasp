(defsystem :static-gfs
  :components
  ((:file "package")
   (:file "flag" :depends-on ("package"))
   (:file "constructor" :depends-on ("package"))
   (:file "precompile" :depends-on ("package"))
   ;; (clos depends on the above files, a bit)
   (:file "cell" :depends-on ("package"))
   (:file "effective-method" :depends-on ("package"))
   (:file "svuc" :depends-on ("package"))
   (:file "shared-initialize" :depends-on ("effective-method" "svuc" "package"))
   (:file "initialize-instance" :depends-on ("shared-initialize"
                                             "effective-method" "package"))
   (:file "allocate-instance" :depends-on ("package"))
   (:file "make-instance" :depends-on ("initialize-instance" "shared-initialize"
                                       "allocate-instance" "effective-method" "package"))
   (:file "compute-constructor" :depends-on ("make-instance" "constructor" "cell" "package"))
   (:file "dependents" :depends-on ("compute-constructor" "package"))
   (:file "compiler-macros" :depends-on ("compute-constructor" "package"))))
