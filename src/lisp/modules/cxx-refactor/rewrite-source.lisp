(defpackage #:rewrite
  (:use :cl)
  )

(in-package :rewrite)


(defun read-rewrites (filename)
  (with-open-file (fin filename :direction :input)
    (let ((data (read fin)))
      data)))

(defun organize-rewrites (rewrites)
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (rule rewrites)
      (let* ((method-name (first rule))
             (exposed-name (second rule))
             (source-loc (third rule))
             (colon1 (position #\: source-loc))
             (colon2 (position #\: source-loc :start (1+ colon1)))
             (source-file (subseq source-loc 0 colon1))
             (source-line (subseq source-loc (1+ colon1) colon2)))
        (format t "source |~a| line |~a|~%" source-file source-line)))))

(setq *default-pathname-defaults* #P"/home/meister/Dev/clasp/src/lisp/modules/cxx-refactor/")

(defparameter *rewrites* (read-rewrites "rewrites.txt"))

(organize-rewrites *rewrites*)




(defun read-entire-cpp-file (cc)
  (format t "Scraping ~a~%" (cpp-name cc))
  (with-open-file (stream (pathname (cpp-name cc)))
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (make-instance 'buffer-stream
                     :buffer data
                     :buffer-pathname (pathname (input cc))
                     :buffer-stream (make-string-input-stream data)))))
