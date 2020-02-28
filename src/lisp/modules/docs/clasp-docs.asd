(asdf:defsystem clasp-docs
  :components ((:file "docs"))
  :depends-on (:staple
               :staple-markdown
               :pathname-utils))
