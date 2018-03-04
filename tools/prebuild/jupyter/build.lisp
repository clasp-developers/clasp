(require :asdf)

(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(translate-logical-pathname #P"sys:"))
   (:tree ,(translate-logical-pathname #P"/Users/meister/quicklisp/"))
   :ignore-inherited-configuration))

(time
 (progn
   (asdf:load-asd "/Users/meister/Development/cando/tools/prebuild/jupyter/cando-jupyter.asd")
   (format t "Done asdf:load-asd cando-jupyter.asd~%")))

(time
 (progn
  (asdf:make :cando-jupyter)
  (format t "Done asdf:make~%")))


(progn
  (asdf:operate 'asdf:monolithic-compile-bundle-op :cando-jupyter)
  (format t "Done asdf:monolithic-compile-bundle-op~%"))

(time
 (progn
  (format t "Starting asdf:monolithic-load-bundle-op~%")
  (asdf:operate 'asdf:monolithic-load-bundle-op :cando-jupyter)
  (format t "Done asdf:monolithic-load-bundle-op~%")))

(progn
  (asdf:load-system :cando-jupyter :print t)
  (format t "Done asdf:load-system :cando-jupyter~%"))


(asdf:operate 'asdf:monolithic-load-bundle-op :cando-jupyter)


(apropos "monolithic")





(load "sys:kernel;cleavir;asdf-system-groveler.lisp")

(time (defparameter *files* (asdf-system-groveler::determine-complete-set-of-asdf-source-files-absolute-path (list :cando :nglview :cl-jupyter))))

(defparameter *cache* #P"/tmp/build/")

(compile-file-build-pathname #P"/Users/meister/quicklisp/dists/quicklisp/software/cl-base64-20150923-git/decode.lisp")
(compile-all-files *files*)



*files*



(asdf:load-system :alexandria)

(asdf/plan:plan-actions 'asdf:concatenate-source-op :alexandria)

(asdf:operate 'asdf:concatenate-source-op :cl-jupyter)
