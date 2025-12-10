(asdf:defsystem #:cross-clasp
  :depends-on (:maclina :closer-mop :extrinsicl :extrinsicl/maclina :anatomicl
                        :alexandria :ecclesia :clostrum-basic
               :trivial-package-local-nicknames :eclector-concrete-syntax-tree
               (:feature :clasp :cleavir-maclina-to-bir/module))
  :components ((:file "packages")
               (:file "vm-clasp" :depends-on ("packages") :if-feature :clasp)
               (:file "trucler-clasp" :depends-on ("packages")
                      :if-feature :clasp)
               (:file "environment"
                :depends-on ((:feature :clasp "vm-clasp")
                             (:feature :clasp "trucler-clasp")
                             "packages"))
               (:file "macrology" :depends-on ("packages"))
               (:file "condition-system-macros" :depends-on ("packages"))
               (:file "mp-macros" :depends-on ("macrology" "packages"))
               (:file "mp-atomics" :depends-on ("packages"))
               (:module "clos" :depends-on ("packages")
                :components ((:file "cpl")
                             (:file "classes")
                             (:file "method-combination" :depends-on ("classes"))
                             (:file "discriminate" :depends-on ("method-combination"))
                             (:file "make-method-lambda")
                             (:file "generics" :depends-on ("make-method-lambda"
                                                            "discriminate"))
                             (:file "dump" :depends-on ("generics"))
                             (:file "define-method-combination")))
               (:file "defstruct" :depends-on ("clos"))
               (:file "with-package-iterator" :depends-on ("environment" "packages"))
               (:file "define-unicode-tables" :depends-on ("packages"))
               (:file "cst" :depends-on ("packages"))
               (:file "opt" :depends-on ("packages"))
               (:file "source-pos-info" :depends-on ("packages"))
               (:file "native" :depends-on ("packages")
                :if-feature :clasp)
               (:file "base" :depends-on ("environment" "clos" "defstruct"
                                                        "condition-system-macros"
                                                        "mp-macros" "mp-atomics"
                                                        "define-unicode-tables"
                                                        "cst" "packages"))
               (:file "build" :depends-on ("base" "packages"))))
