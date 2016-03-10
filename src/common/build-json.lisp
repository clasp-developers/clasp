(defpackage :build-json
  (:use :cl)
  (:export #:build-db ))

(in-package :build-json)

(defparameter *compile-regex* (core:make-regex (format nil "^.*~a.*[[:space:]]-c[[:space:]].*$" "clang")))

(defun read-entire-file (filename)
  (let (cmds)
    (with-open-file (fin (pathname filename))
      (loop for line = (read-line fin nil :eof)
         until (eq line :eof)
         do (when (core:regex-matches *compile-regex* line)
              (push line cmds))))
    cmds))

(defun replace-substr (line oldstr newstr)
  (let ((pos (search oldstr line)))
    (concatenate 'string (subseq line 0 pos) newstr (subseq line (+ pos (length oldstr))))))

(defun split-by-one-char (string &optional (c #\Space))
  "Returns a list of substrings of string
divided by ONE char each.
Note: Two consecutive char will be seen as
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
     as j = (position c string :start i)
     collect (subseq string i j)
     while j))

(defun build-db (bjam-cmds database-name)
  (let ((lines (read-entire-file bjam-cmds))
        (db-pn (make-pathname :host "APP-RESOURCES"
                              :directory '(:absolute "build-databases")
                              :name database-name
                              :type "json")))
    (with-open-file (fout db-pn :direction :output)
      (format fout "[~%")
      (loop for (line . rest ) on lines
         for compile-command = (replace-substr line " -c " " -c -v ")
         for parts = (split-by-one-char compile-command)
         for source-path = (car (last parts))
         for source-path-parts = (split-by-one-char source-path #\/)
         for source-file-name = (string-trim '(#\") (car (last source-path-parts)))
         for directory = (string-trim '(#\") (subseq source-path 0 (- (length source-path) (length (car (last source-path-parts))))))
         for abs-path = (truename (make-pathname :name nil :type nil :defaults (merge-pathnames (pathname directory) bjam-cmds)))
         do (progn
              (format fout "  {~%")
              (format fout "  \"directory\" : ~a,~%" (namestring abs-path))
              (format fout "  \"command\" : ~s,~%" compile-command)
              (format fout "  \"file\" : ~s~%" source-file-name)
              (format fout "  }~a~%" (if rest "," ""))))
      (format fout "]~%"))
    (format t "Wrote out ~d compilation commands~%" (length lines))))
           

