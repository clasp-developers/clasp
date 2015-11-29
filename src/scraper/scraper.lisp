(setq *default-pathname-defaults* #P"/Users/meister/Development/clasp/src/scraper/")

(load "packages.lisp")
(in-package :cscrape)

(defparameter *clang-path* "/Users/meister/Development/externals-clasp/build/release/bin/clang++")

(load "foundation.lisp")
(load "tags.lisp")
(load "compile-commands.lisp")


(defparameter *file* "/tmp/commands.txt")
(defparameter *cc* (read-compile-commands *file*))
(update-cpps *cc*)
(defparameter *q* (read-entire-file (car *cc*)))
(defparameter *tags* (extract-all-tags *q*))
*tags*


