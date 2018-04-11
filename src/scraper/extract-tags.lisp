(in-package :cscrape)

;;; ------------------------------------------------------------
;;;
;;; (1) Read preprocessed files into memory and search for tags
;;; (2) Generate .sif files containing the tags

;; NOTE: it has nothing to do with CL:STREAM
(defclass buffer-stream ()
  ((buffer :initarg :buffer :accessor buffer)
   (buffer-pathname :initarg :buffer-pathname :accessor buffer-pathname)
   (buffer-stream :initarg :buffer-stream :accessor buffer-stream))
  (:documentation "Store the entire contents of a preprocessed file in memory along with a stream on the buffer and its original file name"))

(defun buffer-stream-file-position (bufs &optional new-pos)
  "* Arguments
  - bufs :: buffer-stream
- new-pos :: nil or integer
* Description
Works like file-position for buffer-stream."
  (if new-pos
      (file-position (buffer-stream bufs) new-pos)
      (file-position (buffer-stream bufs))))

#+DEPRECIATED
(defun buffer-peek (bufs)
  "* Arguments
- bufs :: A buffer-stream.
* Description
Peek into the buffer and return from the file-position to the end of the current line"
  (let* ((pos (file-position (buffer-stream bufs)))
         (epos (position #\newline (buffer bufs) :start pos :test #'char=)))
    (subseq (buffer bufs) pos (1- epos))))

(defun buffer-peek-char (bufs)
  "* Arguments
- bufs :: A buffer-stream.
* Description
Peek into the buffer and return the next char at the file-position"
  (declare (optimize (speed 3)))
  (let ((pos (buffer-stream-file-position bufs))
        (len (length (buffer bufs))))
  (if (>= pos len)
      nil
      (elt (buffer bufs) pos))))

#+DEPRECIATED
(defun empty-out-file (filename output)
  (with-open-file (stream (pathname filename)
                          :direction :output
                          :if-exists :supersede)
    (format stream "This file has been overwritten once its contents were parsed to generate the ~a file~%" output)))

(defun read-entire-file (filename)
  "* Arguments
- filename : A pathname
* Description
Read the contents of the filename into memory and return a buffer-stream on it."
  (with-open-file (stream (pathname filename) :external-format :utf-8)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (make-instance 'buffer-stream
                     :buffer data
                     :buffer-pathname (truename (pathname filename))
                     :buffer-stream (make-string-input-stream data)))))

#+DEPRECIATED
(defun read-entire-cpp-file (cc)
  (read-entire-file (cpp-name cc)))

(defun peek-for-element (bufs tag end)
  "* Arguments
- bufs :: Buffer-stream
- tag :: String
- end :: Integer
* Description
Search from the current file-position in bufs up to end for the string tag.
If found advance return the tag position and the position after the tag.
If not found return NIL."
  (declare (optimize speed))
  (let ((tag-pos (search (the simple-string tag)
                         (the simple-string (buffer bufs))
                         :start2 (buffer-stream-file-position bufs) :end2 end)))
    (when tag-pos
      (let ((next-pos (+ tag-pos (length tag))))
        (values tag-pos next-pos)))))

(defun search-for-element (bufs tag)
  "* Arguments
- bufs :: Buffer-stream
- tag :: String
* Description
Search from the current file-position in bufs up to end for the string tag.
Return the tag position and the position after the tag.
If not found return NIL."
  (let ((tag-pos (search tag (buffer bufs) :start2 (buffer-stream-file-position bufs))))
    (when tag-pos
      (let ((next-pos (+ tag-pos (length tag))))
        (values tag-pos next-pos)))))

#+DEPRECIATED
(defun skip-char (bufs)
  (read-char (buffer-stream bufs)))

(defun search-for-white-space (bufs)
  "* Arguments
- bufs : buffer-stream
* Description
Search from file-position of bufs to the first white-space character.
Return the position of the first white-space-character."
  (declare (optimize (speed 3)))
  (let* ((start (buffer-stream-file-position bufs))
         (ch-pos (position-if (lambda (c) (or (char= c #\space) (char= c #\newline) (char= c #\tab))) (buffer bufs) :start start)))
    (when ch-pos
      ch-pos)))

(defun skip-white-space (bufs)
  "* Arguments
- bufs : buffer-stream
* Description
Search from file-position of bufs to the first non-white-space character.
Advance the file-position to the position of the first non-white-space character.
Return the position of the first non-white-space character."
  (declare (optimize (speed 3)))
  (let* ((start (buffer-stream-file-position bufs))
         (ch-pos (position-if (lambda (c) (not (or (char= c #\space) (char= c #\newline) (char= c #\tab)))) (buffer bufs) :start start)))
    (when ch-pos
      (buffer-stream-file-position bufs ch-pos)
      ch-pos)))

(defun search-for-character (bufs ch)
  "* Arguments
- bufs : buffer-stream
- ch : character
* Description
Search for the character (ch) and return the position of the found character.
If not found return NIL."
  (position ch (buffer bufs)
            :start (buffer-stream-file-position bufs)))

(defun read-identifier (bufs)
  "* Arguments
- bufs :: buffer-stream
* Description
Read a C++ identifier, assume we start on the first char.
Advance the file-position to the character after the identifier.
Return the identifier."
  (declare (optimize (speed 3)))
  (let* ((buffer (buffer bufs))
         (start (buffer-stream-file-position bufs))
         (cur start))
    (let ((first-char (elt buffer cur)))
      (when (or (alpha-char-p first-char) (char= #\_ first-char))
        (incf cur)
        (loop for next-char = (elt buffer cur)
           until (not (or (alphanumericp next-char) (char= #\_ next-char)))
           do (incf cur))))
    (buffer-stream-file-position bufs cur)
    (subseq buffer start cur)))

(defun read-string-to-character (bufs ch &optional keep-last-char)
  "* Arguments
- bufs :: buffer-stream
- ch :: character
- keep-last-char :: boolean
* Description
Read up to the character and leave file-position pointing to it if (keep-last-char) otherwise advance one past it."
  (let* ((start (buffer-stream-file-position bufs))
         (ch-pos (search-for-character bufs ch))
         (keep-to (if keep-last-char
                      (1+ ch-pos)
                      ch-pos)))
    (buffer-stream-file-position bufs keep-to)
    (subseq (buffer bufs) start keep-to)))

(defun read-string-to-white-space (bufs &optional keep-last-char)
  "* Arguments
- bufs :: buffer-stream
- ch :: character
- keep-last-char :: boolean
* Description
Read up to the first white-space character and
leave file-position pointing to it if (keep-last-char)
otherwise advance one past it.
Return the string that either includes it or not depending on (keep-last-char)."
  (let* ((start (buffer-stream-file-position bufs))
         (ch-pos (search-for-white-space bufs))
         (keep-to (if keep-last-char
                      (1+ ch-pos)
                      ch-pos)))
    (buffer-stream-file-position bufs keep-to)
    (subseq (buffer bufs) start keep-to)))

(defun read-string-to-tag (bufs tag)
  "* Arguments
- bufs :: buffer-stream
- tag :: string
* Description
Read the string up to the tag and advance the file-position to the first character after the tag."
  (let ((start (buffer-stream-file-position bufs)))
    (multiple-value-bind (tag-start after-tag)
        (search-for-element bufs tag)
      (buffer-stream-file-position bufs after-tag)
      (subseq (buffer bufs) start tag-start))))

(defun parse-tag (bufs tag tag-handlers)
  "* Arguments
- bufs :: buffer-stream
- tag :: string
- tag-handlers :: hash-table
* Description
Look up the handler for the tag and dispatch to it to 
parse the tag using the current position in bufs."
  (let ((handler (gethash tag tag-handlers)))
    (if handler
        (funcall (tags:handler-code handler) bufs)
        (error 'tags:unknown-tag :tag tag))))

(defun namespace-recognizer (bufs tags)
  "* Arguments
- bufs :: buffer-stream
- tags :: A list.
* Description
The string 'namespace' was found in bufs.
Check if it is followed by an identifier and #\{.
If it has the form 'namespace [identifier] {' then parse the identifier
and insert a tags:namespace-tag into the tags list.
Return the new tags list."
  (declare (optimize (speed 3)))
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
  "* Arguments
- bufs :: buffer-stream
- tags :: A list.
* Description
A tag was found in bufs - parse the tag, insert it into the tags list,
advance the file-pointer. Return the new tags list."
  (declare (optimize (speed 3)))
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
   (cons +begin-tag+ #'begin-tag-recognizer)))

(defun next-recognition-element (bufs)
  "* Arguments
- bufs :: A buffer-stream
* Description
Identify the next recognition element in bufs.
Return the name of the element, the start and the position right after the name of the element."
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
  "* Arguments
- bufs :: A buffer-stream
*Description
Extract all of the tags from bufs and return them as a list. "
  (declare (optimize (speed 3)))
  (let (tags)
    (loop
       (multiple-value-bind (rec-el nearest-pos nearest-after-pos)
           (next-recognition-element bufs)
         (declare (ignore nearest-pos))
         (unless rec-el
           (return (nreverse tags)))
         (buffer-stream-file-position bufs nearest-after-pos)
         (setq tags (funcall (cdr rec-el) bufs tags))))))
