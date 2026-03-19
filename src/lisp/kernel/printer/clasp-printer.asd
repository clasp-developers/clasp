(asdf:defsystem "clasp-printer"
  :depends-on ("incless-native"
               "invistra"
               "quaviver/schubfach")
  :components ((:file "packages")
               (:file "implementation" :depends-on ("packages"))))
