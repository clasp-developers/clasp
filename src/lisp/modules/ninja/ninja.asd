(asdf:defsystem #:ninja
  :description "Ninja writing interface along with utility streams for build files."
  :depends-on (#:trivial-gray-streams)
  :components ((:file "packages")
               (:file "timestamp-preserving-stream")
               (:file "line-wrapping-stream")
               (:file "ninja")))
