(asdf:defsystem #:koga
  :description "A lisp based metabuilder for Clasp."
  :depends-on (#:alexandria
               #:asdf-groveler
               #:clasp-scraper
               #:closer-mop
               #:ninja
               #:shasht
               #:trivial-features)
  :components ((:file "packages")
               (:file "utilities")
               (:file "header")
               (:file "source")
               (:file "configure")
               (:file "setup")
               (:file "help")
               (:file "target-sources")
               (:file "units")
               (:file "scripts")
               (:file "config-header")
               (:file "ninja")
               (:file "compile-commands")))
