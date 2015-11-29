(defpackage #:cscrape
  (:use :cl)
  (:export ))


(defparameter *clang-path* "/Users/meister/Development/externals-clasp/build/release/bin/clang++")


;;; Everything necessary to run the preprocessor on one input file
;;; original - original compile command as a string
;;; cpp-name - namestring to write/read the preprocessed file to/from
;;; input - the original source input name
(defclass compile-command ()
  ((original :initarg :original :accessor original)
   (parts :initarg :parts :accessor parts)
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
  (let* ((parts (parts cc))
         (output (output cc))
         (cpp-name (cpp-name cc))
         (output-pos (position output parts :test #'string=))
         (from-output (nthcdr output-pos parts)))
    (concatenate 'list
                 (list *clang-path* "-E" "-DSCRAPING")
                 (subseq parts 1 output-pos)
                 (list cpp-name)
                 (cdr from-output))))

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
         collect (if (string= (string part) "-DAPPLICATION_CONFIG=")
                     (with-output-to-string (sout)
                       (write-string (string part) sout)
                       (write-string (read sin nil) sout))
                     (string part))))))

(defun extract-output-filename (line)
  (let ((parts (split-line-using-read line)))
    (extract-after parts "-o")))

(defun extract-after (parts option)
  (let ((pos (position option parts :test #'string= )))
    (nth (1+ pos) parts)))

(defun read-compile-commands (filename)
  (let ((lines (read-compile-command-lines filename)))
    (mapcar (lambda (l)
              (let* ((parts (split-line-using-read l))
                     (output (extract-after parts "-o"))
                     (cpp-name (string-replace-once output ".o" ".i")))
                (make-instance 'compile-command
                               :original l
                               :parts parts
                               :output output
                               :cpp-name cpp-name
                               :input (car (last parts)))))
            lines)))


(defun run-cpp (cc &key print)
  "Run the C-preprocessor on the compile-command"
  (let* ((cpp-cmd (generate-cpp-command cc)))
    (when print
      (format t "~a~%" (with-output-to-string (sout)
                         (loop for x in cpp-cmd
                            do (write-string x sout)
                            do (write-string " " sout)))))
    (ensure-directories-exist (pathname (cpp-name cc)))
    (with-output-to-string (serr)
      (with-output-to-string (sout)
        (sb-ext:run-program (car cpp-cmd) (cdr cpp-cmd) :output sout :error serr)
        (unless (string= (get-output-stream-string serr) "")
          (format t "error: ~a~%" (get-output-stream-string serr)))
        (values (get-output-stream-string sout) (get-output-stream-string serr))))))


(defun update-cpps (ccs)
  "Run the c-preprocessor on the commands"
  (loop for cc in ccs
       do (run-cpp cc :print t)))



#|

(setf *default-pathname-defaults* #P"/Users/meister/Development/clasp/src/scraper/")
(defparameter *file* "/tmp/commands.txt")
(defparameter *lines* (read-compile-command-lines *file*))

(defparameter *cc* (read-compile-commands *file*))
(car *cc*)
;;; Generate a CPP compilation command from one of the compile-commands
(cpp-needs-run (car *cc*))
(run-cpp (car *cc*))

*cc*
(generate-cpp-command (car *cc*))

(pathname (cpp-name (car *cc*)))
(update-cpps *cc*)
(cpp-needs-run (cadr *cc*))

|#


