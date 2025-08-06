(asdf:defsystem #:encoding-generator
  :version "1.0.0"
  :author ("Bike <aeshtaer@gmail.com>" "Karsten Poeck")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :license "LGPL-3.0"
  :depends-on (#:alexandria)
  :components ((:file "packages")
               (:file "generate" :depends-on ("packages"))))
