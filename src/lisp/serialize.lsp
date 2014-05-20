(provide 'serialize)

(defpackage #:serialize 
  (:nicknames :ser)
  (:use :cl)
  (:export save-archive load-archive)
)

(in-package #:serialize)


;;(pushnew :use-with-open-file *features*)


(defun save-archive (obj filename)
  (let ((archive (core:make-sexp-save-archive)))
    (core:put archive :only obj)
    #-use-with-open-file (let ((fout (open filename :direction :output :if-exists :supersede)))
			   (core::sexp-save-archive-write archive fout)
			   (close fout :abort t))
    #+use-with-open-file (with-open-file (fout filename :direction :output :if-exists :supersede)
			   (core:sexp-save-archive-write archive fout))
    ))




(defun load-archive (filename)
  "Load an object from a file that contains a serialized archive"
  (let ((archive (core:make-sexp-load-archive)))
    (with-open-file (fin filename :direction :input)
      (let ((object (read fin)))
	(core:parse-from-object archive object))
      (core:get archive :only))))




(defun test-archive ()
  (let ((fn "archive.dat")
	(a (make-hash-table :test 'eq)))
    (format t "Writing test archive to ~a~%" fn)
    (setf (gethash :first a) #(1 2 3))
    (setf (gethash :second a) "Hi there")
    (setf (gethash :third a) '(a bunch of symbols))
    (setf (gethash :circular a) a)
    (ser:save-archive a fn)
    (let ((b (ser:load-archive fn))
	  (fn2 "archive2.dat"))
      (format t "Re-writing test archive to ~a~%" fn2)
      (ser:save-archive b fn2))))

