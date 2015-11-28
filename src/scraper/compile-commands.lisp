(in-package :cscrape)

;;; Everything necessary to run the preprocessor on one input file
;;; original - original compile command as a string
;;; cpp-name - namestring to write/read the preprocessed file to/from
;;; input - the original source input name
(defclass compile-command ()
  ((original :initarg :original :accessor original)
   (output :initarg :output :accessor output)
   (cpp-name :initarg :cpp-name :accessor cpp-name)
   (input :initarg :input :accessor input)))

;;; Return s/<orig>/<replace>/  on eq
(defun string-replace-once (seq orig replace)
  (let ((pos (search orig seq)))
    (concatenate 'string
                 (subseq seq 0 pos)
                 replace
                 (subseq seq (+ pos (length orig))))))

;;; Generate a CPP compilation command from a compile-command
(defun generate-cpp-command (cc)
  (let* ((original (original cc))
         (output (output cc))
         (cpp-name (cpp-name cc))
         (output-pos (search output original))
         (subst-output (string-replace-once original output cpp-name))
         (insert-cpp-flag (string-replace-once subst-output "-x c" "-E -x c")))
    insert-cpp-flag))
                                       
(defun read-compile-command-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil 'foo)
       until (eq line 'foo)
       when (search "-x c" line)
       collect line)))

(defun split-line-using-read (line)
  (with-input-from-string (sin line)
    (let ((*readtable* (copy-readtable)))
      (setf (readtable-case *readtable*) :preserve)
      (loop for part = (read sin nil 'foo)
         until (eq part 'foo)
         collect part))))

(defun extract-output-filename (line)
  (let ((minuso (search "-o " line)))
    (if minuso (read (make-string-input-stream (subseq line (+ minuso 3))))
        (error "Could not find -o in ~a" line))))

(defun extract-after (parts option)
  (let ((pos (position option parts)))
    (nth (1+ pos) parts)))

(defun read-compile-commands (filename)
  (let ((lines (read-compile-command-lines filename)))
    (mapcar (lambda (l)
              (let* ((parts (split-line-using-read l))
                     (output (extract-after parts '|-o|))
                     (extension-start (search ".o" output))
                     (cpp-name (concatenate 'string (subseq output 0 extension-start) ".i")))
                (make-instance 'compile-command
                               :original l
                               :cpp-name cpp-name
                               :input (car (last parts)))))
            lines)))
|#

(defparameter *file* "sample-compile-commands.txt")
(defparameter *lines* (read-compile-command-lines *file*))

(defparameter *cc* (read-compile-commands *file*))

;;; Generate a CPP compilation command from one of the compile-commands
(generate-cpp-command (car *cc*))
