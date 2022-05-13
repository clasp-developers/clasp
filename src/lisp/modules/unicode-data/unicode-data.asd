(asdf:defsystem #:unicode-data
  :description "Unicode Data parsing and analysis"
  :version "1.0.0"
  :author "Tarn W. Burton"
  :licence "LGPL-3.0"
  :serial t
  :depends-on (#:ninja
               #:split-sequence
               #:trivial-http)
  :components ((:file "packages")
               (:file "unicode")))
