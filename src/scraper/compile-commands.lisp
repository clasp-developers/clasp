(in-package :cscrape)

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
    (format t "Running preprocessor to generate ~a~%" (cpp-name cc))
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
        (values (get-output-stream-string sout) (get-output-stream-string serr))))))


(defun update-cpps (ccs)
  "Run the c-preprocessor on the commands"
  (loop for cc in ccs
       do (run-cpp cc)))


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

(defun read-entire-file (cc)
  (format t "Scraping ~a~%" (cpp-name cc))
  (with-open-file (stream (pathname (cpp-name cc)))
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (make-instance 'buffer-stream
                     :buffer data
                     :buffer-pathname (pathname (input cc))
                     :buffer-stream (make-string-input-stream data)))))

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
  (with-output-to-string (sout)
    (block done
      (let ((first-char (buffer-peek-char bufs)))
        (when (or (alpha-char-p first-char) (char= #\_ first-char))
          (princ first-char sout)
          (skip-char bufs)
          (loop
             (let ((next-char (buffer-peek-char bufs)))
               (if (or (alphanumericp next-char) (char= #\_ next-char))
                   (progn
                     (princ next-char sout)
                     (skip-char bufs))
                   (return-from done nil)))))))))

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
                           :namespace namespace-name
                           :file (buffer-pathname bufs))
            tags))
    tags))

(defparameter *tag-handlers* (tags:make-handler-hash-table))

(defun begin-tag-recognizer (bufs tags)
  (declare (optimize (debug 3)))
  ;; Recognize BEGIN_TAGxxxx <tag info>
  (let* ((pos (skip-white-space bufs))
         (tag-name (read-string-to-white-space bufs))
         (parsed (parse-tag bufs tag-name *tag-handlers*)))
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

