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
   (buffer-stream :initarg :buffer-stream :accessor buffer-stream)))

(defun read-entire-file (cc)
  (with-open-file (stream (pathname (cpp-name cc)))
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (make-instance 'buffer-stream
                     :buffer data
                     :buffer-stream (make-string-input-stream data)))))

(defun search-for-tag (bufs tag)
  (let ((tag-pos (search tag (buffer bufs) :start2 (file-position (buffer-stream bufs)))))
    (when tag-pos
      (let ((next-pos (+ tag-pos (length tag))))
        (file-position (buffer-stream bufs) next-pos)
        (values tag-pos next-pos)))))

(defun skip-char (bufs)
  (read-char (buffer-stream bufs)))

(defun skip-tag (bufs tag)
  (search-for-tag bufs tag))

(defun search-for-character (bufs ch)
  (let ((ch-pos (position ch (buffer bufs) :start (file-position (buffer-stream bufs)))))
    (when ch-pos
      (file-position (buffer-stream bufs) (1+ ch-pos))
      ch-pos)))

(defun read-string-to-character (bufs ch &optional keep-last-char)
  "Read up to the character and keep it if (keep-last-char)"
  (let* ((start (file-position (buffer-stream bufs)))
         (ch-pos (search-for-character bufs ch))
         (keep-to (if keep-last-char
                      (1+ ch-pos)
                      ch-pos)))
    (subseq (buffer bufs) start keep-to)))

(defun next-tag-name (bufs)
  (let ((tag-start (search-for-tag bufs *begin-tag*)))
    (when tag-start
      (skip-char bufs)
      (let ((tag-kind-start (file-position (buffer-stream bufs)))
            (tag-kind-end (search-for-character bufs #\space)))
        (let ((tag-name (subseq (buffer bufs) tag-kind-start tag-kind-end)))
          tag-name)))))

(defun read-string-to-tag (bufs tag)
  (let ((start (file-position (buffer-stream bufs)))
        (end (search-for-tag bufs tag)))
    (subseq (buffer bufs) start end)))


(defun parse-tag (bufs tag tag-handlers)
  (let ((handler (gethash tag tag-handlers)))
    (if handler
        (funcall (tags:handler-code handler) bufs)
        (error 'tags:unknown-tag :tag tag))))

(defun extract-all-tags (bufs)
  (declare (optimize (debug 3)))
  (let (tags
        (tag-handlers (tags:make-handler-hash-table)))
    (loop
       (let ((next-tag (next-tag-name bufs)))
         (unless next-tag
           (return (nreverse tags)))
         (push (parse-tag bufs next-tag tag-handlers) tags)))))


(defun read-all-tags-all-compile-commands (all-cc)
  (loop for cc in all-cc
     for bufs = (read-entire-file cc)
     for tags = (extract-all-tags bufs)
     do (interpret-tags tags)
     nconc tags))
