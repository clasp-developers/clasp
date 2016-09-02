(defpackage :build-json
  (:use :cl)
  (:export #:build-db
           #:dump-source-files))

(in-package :build-json)

(defun compilation-line-p (line)
  (and (search "clang" line) (search " -c " line)))

(defun read-entire-file (filename)
  (let (cmds)
    (with-open-file (fin (pathname filename))
      (loop for line = (read-line fin nil :eof)
         until (eq line :eof)
         do (when (compilation-line-p line)
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

(defun build-db (bjam-cmds output)
  (let ((lines (read-entire-file bjam-cmds))
        (db-pn (pathname output)))
    (with-open-file (fout db-pn :direction :output :if-exists :supersede)
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
              (format fout "  ~s : ~s,~%" "directory" (namestring abs-path))
              (format fout "  \"command\" : ~s,~%" compile-command)
              (format fout "  \"file\" : ~s~%" source-file-name)
              (format fout "  }~a~%" (if rest "," ""))))
      (format fout "]~%"))
    (format t "Wrote out ~d compilation commands to ~a~%" (length lines) (truename db-pn))))



(defun dump-source-files (bjam-cmds output)
  (let* ((lines (read-entire-file bjam-cmds)))
    (with-open-file (fout output :direction :output :if-exists :supersede)
      (dolist (line lines)
        (let* ((split (split-by-one-char line #\space))
               (source (car (last split))))
          (format fout "~a~%" (subseq source 1 (- (length source) 1))))))))
