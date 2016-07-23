(in-package :cscrape)

;;; Everything necessary to run the preprocessor on one input file
;;; original - original compile command as a string
;;; cpp-name - namestring to write/read the preprocessed file to/from
;;; input - the original source input name

(require :sb-posix)

(defun use-multiprocessing ()
  (when (and *use-multiprocessing-if-available* (find-package "SB-POSIX"))
    (and (find-symbol "FORK" "SB-POSIX")
         (find-symbol "GETENV" "SB-POSIX")
         (find-symbol "WAIT" "SB-POSIX"))))

(defclass compile-command ()
  ((original :initarg :original :accessor original)
   (parts :initarg :parts :accessor parts)
   (output :initarg :output :accessor output)
   (cpp-name :initarg :cpp-name :accessor cpp-name)
   (sif-name :initarg :sif-name :accessor sif-name)
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
                     (cpp-name (string-replace-once output ".o" ".i"))
                     (sif-name (string-replace-once output ".o" ".sif")))
                (make-instance 'compile-command
                               :original l
                               :parts parts
                               :output output
                               :cpp-name cpp-name
                               :sif-name sif-name
                               :input (car (last parts)))))
            lines)))


(defun run-cpp (cc forki &key print)
  "Run the C-preprocessor on the compile-command"
  (let* ((cpp-cmd (generate-cpp-command cc)))
    (format t "[~d] Preprocess/scrape-> ~a~%" forki (cpp-name cc))
    (when print
      (format t "~a~%" (with-output-to-string (sout)
                         (loop for x in cpp-cmd
                            do (write-string x sout)
                            do (write-string " " sout)))))
    (ensure-directories-exist (pathname (cpp-name cc)))
    (with-output-to-string (serr)
      (with-output-to-string (sout)
        (let ((exit (sb-ext:run-program (car cpp-cmd) (cdr cpp-cmd) :output sout :error serr)))
          (unless (eql (sb-ext:process-exit-code exit) 0)
            (format t "error: ~a~%" (get-output-stream-string serr))))
        (update-sif-file cc forki)
        (values (get-output-stream-string sout) (get-output-stream-string serr))))))

(defun split-cpps (ccs &optional (num 1))
  "* Arguments
- ccs :: A list of compile commands.
- num :: A number
* Description
Split the list of ccs into a number of lists."
  (let ((lists (make-array num :initial-element nil))
        (i 0))
    (dolist (c ccs)
      (push c (elt lists i))
      (incf i)
      (when (>= i num) (setf i 0)))
    (coerce lists 'list)))

(defun serial-update-cpps (ccs)
  "Run the c-preprocessor on the commands"
  (format t "Using serial-update-cpps to update ~d preprocessor files~%" (length ccs))
  (when (> (length ccs) 0)
    (format t "Running ~d preprocessor jobs~%" (length ccs))
    (loop for job in ccs
       do (run-cpp job 0))))

(defun parallel-update-cpps (ccs)
  "Run the c-preprocessor on the commands"
  (when ccs
    (let* ((pjobs (min (length ccs)
                       (parse-integer (sb-ext:posix-getenv "PJOBS"))))
           (jobs (split-cpps ccs pjobs)))
      (format t "Running ~d preprocessor jobs in ~d forks~%" (length ccs) pjobs)
      (loop for job in jobs
            for forki from 0
            do (when (= (funcall (find-symbol "FORK" "SB-POSIX")) 0)
                 (loop for cc in job
                       do (run-cpp cc forki))
                 (SB-EXT:exit)))
      (loop repeat pjobs
            do
           (funcall (find-symbol "WAIT" "SB-POSIX"))))))


(defun update-cpps (ccs)
    (if (use-multiprocessing)
        (parallel-update-cpps ccs)
        (serial-update-cpps ccs)))

(defclass buffer-stream ()
  ((buffer :initarg :buffer :accessor buffer)
   (buffer-pathname :initarg :buffer-pathname :accessor buffer-pathname)
   (buffer-stream :initarg :buffer-stream :accessor buffer-stream)))

(defun buffer-peek (bufs)
  (let* ((pos (file-position (buffer-stream bufs)))
         (epos (position #\newline (buffer bufs) :start pos :test #'char=)))
    (subseq (buffer bufs) pos (1- epos))))


(defun buffer-peek-char (bufs)
  (declare (optimize (debug 3)))
  (let ((pos (file-position (buffer-stream bufs)))
        (len (length (buffer bufs))))
  (if (>= pos len)
      nil
      (elt (buffer bufs) pos))))

(defun empty-out-file (filename output)
  (with-open-file (stream (pathname filename) :direction :output :if-exists :supersede)
    (format stream "This file has been overwritten once its contents were parsed to generate the ~a file~%" output)))

(defun read-entire-file (filename)
  (with-open-file (stream (pathname filename))
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (make-instance 'buffer-stream
                     :buffer data
                     :buffer-pathname (truename (pathname filename))
                     :buffer-stream (make-string-input-stream data)))))

(defun read-entire-cpp-file (cc)
  (read-entire-file (cpp-name cc)))

(defun peek-for-element (bufs tag end)
  (let ((tag-pos (search tag (buffer bufs) :start2 (file-position (buffer-stream bufs)) :end2 end)))
    (when tag-pos
      (let ((next-pos (+ tag-pos (length tag))))
        (values tag-pos next-pos)))))

(defun search-for-element (bufs tag)
  (let ((tag-pos (search tag (buffer bufs) :start2 (file-position (buffer-stream bufs)))))
    (when tag-pos
      (let ((next-pos (+ tag-pos (length tag))))
        (file-position (buffer-stream bufs) next-pos)
        (values tag-pos next-pos)))))

(defun skip-char (bufs)
  (read-char (buffer-stream bufs)))

(defun search-for-white-space (bufs)
  (declare (optimize (debug 3)))
  (let* ((start (file-position (buffer-stream bufs)))
         (ch-pos (position-if (lambda (c) (or (char= c #\space) (char= c #\newline) (char= c #\tab))) (buffer bufs) :start start)))
    (when ch-pos
      (file-position (buffer-stream bufs) (1+ ch-pos))
      ch-pos)))

(defun skip-white-space (bufs)
  (declare (optimize (debug 3)))
  (let* ((start (file-position (buffer-stream bufs)))
         (ch-pos (position-if (lambda (c) (not (or (char= c #\space) (char= c #\newline) (char= c #\tab)))) (buffer bufs) :start start)))
    (when ch-pos
      (file-position (buffer-stream bufs) ch-pos)
      ch-pos)))

(defun search-for-character (bufs ch)
  (let ((ch-pos (position ch (buffer bufs) :start (file-position (buffer-stream bufs)))))
    (when ch-pos
      (file-position (buffer-stream bufs) (1+ ch-pos))
      ch-pos)))

(defun read-identifier (bufs)
  "Read a C++ identifier, assume we start on the first char"
  (declare (optimize (debug 3)))
  (let* ((buffer (buffer bufs))
         (start (file-position (buffer-stream bufs)))
         (cur start))
    (let ((first-char (elt buffer cur)))
      (when (or (alpha-char-p first-char) (char= #\_ first-char))
        (incf cur)
        (loop for next-char = (elt buffer cur)
           until (not (or (alphanumericp next-char) (char= #\_ next-char)))
           do (incf cur))))
    (file-position (buffer-stream bufs) cur)
    (subseq buffer start cur)))

(defun read-string-to-character (bufs ch &optional keep-last-char)
  "Read up to the character and keep it if (keep-last-char)"
  (let* ((start (file-position (buffer-stream bufs)))
         (ch-pos (search-for-character bufs ch))
         (keep-to (if keep-last-char
                      (1+ ch-pos)
                      ch-pos)))
    (subseq (buffer bufs) start keep-to)))

(defun read-string-to-white-space (bufs &optional keep-last-char)
  "Read up to the character and keep it if (keep-last-char)"
  (let* ((start (file-position (buffer-stream bufs)))
         (ch-pos (search-for-white-space bufs))
         (keep-to (if keep-last-char
                      (1+ ch-pos)
                      ch-pos)))
    (subseq (buffer bufs) start keep-to)))



(defun read-string-to-tag (bufs tag)
  (let ((start (file-position (buffer-stream bufs)))
        (end (search-for-element bufs tag)))
    (subseq (buffer bufs) start end)))


(defun parse-tag (bufs tag tag-handlers)
  (let ((handler (gethash tag tag-handlers)))
    (if handler
        (funcall (tags:handler-code handler) bufs)
        (error 'tags:unknown-tag :tag tag))))


(defun namespace-recognizer (bufs tags)
  "Recognize namespace name {"
  (declare (optimize (debug 3)))
  ;; Advance to after "namespace"
  (let* ((pos (skip-white-space bufs))
         (namespace-name (read-identifier bufs))
         (pos2 (skip-white-space bufs))
         (char2 (buffer-peek-char bufs)))
    (declare (ignore pos pos2))
    (when (and (char= char2 #\{) (not (string= namespace-name "")))
      (push (make-instance 'tags:namespace-tag
                           :namespace% namespace-name
                           :file% (buffer-pathname bufs)
                           :line% 0)
            tags))
    tags))

(defun begin-tag-recognizer (bufs tags)
  (declare (optimize (debug 3)))
  ;; Recognize BEGIN_TAGxxxx <tag info>
  (let* ((pos (skip-white-space bufs))
         (tag-name (read-string-to-white-space bufs))
         (parsed (parse-tag bufs tag-name tags:*tag-handlers*)))
    (declare (ignore pos))
    (when parsed
      (push parsed tags)))
  tags)

;;; A list of (string . callback) pairs
(defparameter *recognition-elements*
  (list
   (cons "namespace" #'namespace-recognizer)
   (cons *begin-tag* #'begin-tag-recognizer)))


(defun next-recognition-element (bufs)
  (let (end-pos
        nearest-pos
        nearest-after-pos
        nearest-element)
    (dolist (rec-el *recognition-elements*)
      (multiple-value-bind (pos after-pos)
          (peek-for-element bufs (car rec-el) end-pos)
        (when (and pos (or (null nearest-pos) (< pos nearest-pos)))
          (setf nearest-pos pos
                nearest-element rec-el
                nearest-after-pos after-pos
                end-pos pos))))
    (values nearest-element nearest-pos nearest-after-pos)))

(defun process-all-recognition-elements (bufs)
  (declare (optimize (debug 3)))
  (let (tags)
    (loop
       (multiple-value-bind (rec-el nearest-pos nearest-after-pos)
           (next-recognition-element bufs)
         (declare (ignore nearest-pos))
         (unless rec-el
           (return (nreverse tags)))
         (file-position (buffer-stream bufs) nearest-after-pos)
         (setq tags (funcall (cdr rec-el) bufs tags))))))


(defun sif-file-needs-updating (compile-command)
  "* Arguments
- compile-command :: The compile-command.
* Description
Return T if the scraped-info-file for this compile-command doesn't exist or if its write-date is earlier than the c-preprocessed file."
  (let ((cpp-pathname (pathname (cpp-name compile-command)))
        (sif-pathname (pathname (sif-name compile-command))))
    (or (not (probe-file sif-pathname))
        (< (file-write-date sif-pathname) (file-write-date cpp-pathname)))))

(defun generate-sif-file (input output)
  "* Arguments
- input :: A pathname.
- output :: A pathname.
* Description
Read the input .i file and extract the tags from it and write out a .sif file"
  (let* ((bufs (read-entire-file input))
         (tags (process-all-recognition-elements bufs))
         (sif-pathname output))
    #+(or)(empty-out-file input output)
    (with-open-file (fout sif-pathname :direction :output :if-exists :supersede)
      (let ((*print-readably* t)
            (*print-pretty* nil))
        (prin1 tags fout)))))


(defun update-sif-file (compile-command forki)
  "* Arguments
- compile-command :: The compile-command.
* Description
Read the c-preprocessed file and scrape all of the tags.
Then dump the tags into the sif-file."
  (let ((input (cpp-name compile-command))
        (output (sif-name compile-command)))
    (generate-sif-file input output)))

(defun read-sif-file (sif-file)
  "* Arguments
- compile-command :: The compile-command.
* Description
Read a list of tags from the sif file."
  (let* ((sif-pathname (pathname sif-file)))
    (with-open-file (fin sif-pathname :direction :input)
      (read fin))))
