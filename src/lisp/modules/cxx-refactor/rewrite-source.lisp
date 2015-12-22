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
        (format t "source |~a| line |~a|~%" source-file source-line)
        (push (list method-name exposed-name (parse-integer source-line)) (gethash source-file ht))))
    (maphash (lambda (k v) (setf (gethash k ht) (nreverse v))) ht)
    ht))

(defun read-entire-file (filename)
  (with-open-file (stream filename)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun rewrite-source (filename rewrites)
  (let ((file-in-string (read-entire-file filename)))
    (with-input-from-string (sin file-in-string)
      (format t "Loaded file~%")
      (with-open-file (fout filename :direction :output :if-exists :supersede)
        (block write-file
          (let ((line-number 0))
            (dolist (rewrite rewrites)
              (format t "Applying rewrite ~s~%" rewrite)
              (block rewrite
                (let ((method-name (first rewrite))
                      (exposed-name (second rewrite))
                      (target-line (third rewrite)))
                  (loop
                     (let ((line (read-line sin nil 'eof)))
                       (incf line-number)
;;                       (format t "Read line[~d]: ~a~%" line-number line)
                       (when (eq line 'eof) (return-from write-file nil))
                       (when (= line-number target-line)
                         (format t "Exposing function: ~a~%" method-name)
                         (format fout "CL_NAME(~s);~%" exposed-name)
                         (format fout "CL_DEFMETHOD ~a~%" line)
                         (return-from rewrite nil))
                       (format fout "~a~%" line))))))
            (format t "Done rewrites~%")
            (loop
               (let ((line (read-line sin nil 'eof)))
                 (incf line-number)
;;                 (format t "Read line[~d]: ~a~%" line-number line)
                 (when (eq line 'eof) (return-from write-file nil))
                 (format fout "~a~%" line)))))))))

                
(setq *default-pathname-defaults* #P"/home/meister/Dev/clasp/src/lisp/modules/cxx-refactor/")

(defparameter *rewrites* (read-rewrites "rewrites.txt"))

(defparameter *organized-rewrites* (organize-rewrites *rewrites*))

(maphash (lambda (k v) (print (list k v)) (if (search "cons.cc" k) (setq *cons* v)) ) *organized-rewrites*)

(rewrite-source "/home/meister/Dev/clasp/src/core/cons.cc" *cons*)

(maphash (lambda (k v) (rewrite-source k v)) *organized-rewrites*)






