(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(load "scraper.lisp")
(in-package :cscrape)

(defparameter *clang-path* nil)
(defparameter *tags* nil)

(load "foundation.lisp")
(load "serialize.lisp")
(load "parse.lisp")
(load "compile-commands.lisp")
(load "tags.lisp")
(load "conditions.lisp")
(load "sourcepos.lisp")
(load "interpret-tags.lisp")
(load "format.lisp")
(load "csubst.lisp")
(load "code-generator.lisp")
