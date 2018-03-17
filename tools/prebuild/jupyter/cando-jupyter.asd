(in-package :asdf-user)

(asdf:defsystem cando-jupyter
  :version "1.2.3"
  :license "Artistic"
  :author "Christian Schafmeister <meister@temple.edu>"
  :maintainer "Christian Schafmeister <meister@temple.edu>"
  :description "A library providing cando+jupyter notebooks"
  :homepage ""
  :serial T
  :components ()
  :build-operation asdf:monolithic-compile-bundle-op
  :build-pathname "cando-jupyter"
  :depends-on (:cando :cl-jupyter :nglview))
