(in-package #:ninja)

(defclass timestamp-preserving-stream
    (trivial-gray-streams:fundamental-character-output-stream)
  ((old-stream :accessor timestamp-preserving-stream-old-stream
               :initarg :old-stream)
   (new-stream :accessor timestamp-preserving-stream-new-stream
               :initarg :new-stream)
   (old-path :reader timestamp-preserving-stream-old-path
             :initarg :old-path)
   (new-path :reader timestamp-preserving-stream-new-path
             :initarg :new-path)))

(defun make-timestamp-preserving-stream (path &rest options
                         &aux (new-path (merge-pathnames (make-pathname :type "new")
                                                         path)))
  (make-instance 'timestamp-preserving-stream :old-path path :new-path new-path
                              :old-stream (when (probe-file path)
                                            (apply #'open path options))
                              :new-stream (apply #'open
                                                 new-path
                                                 :direction :output
                                                 :if-exists :supersede
                                                 :if-does-not-exist :create
                                                 options)))

(defmacro with-timestamp-preserving-stream ((stream filespec &rest options) &body body)
  `(let ((,stream (make-timestamp-preserving-stream ,filespec ,@options)))
     (unwind-protect (progn ,@body)
       (close ,stream))))

(defmethod trivial-gray-streams:stream-write-char
    ((stream timestamp-preserving-stream) char)
  (with-accessors ((old-stream timestamp-preserving-stream-old-stream)
                   (new-stream timestamp-preserving-stream-new-stream))
      stream
    (when (and old-stream
               (not (equal char (read-char old-stream nil nil))))
      (close old-stream)
      (setf old-stream nil))
    (write-char char new-stream)))

(defmethod trivial-gray-streams:stream-finish-output ((stream timestamp-preserving-stream))
  (finish-output (timestamp-preserving-stream-old-stream stream)))

(defmethod trivial-gray-streams:stream-line-column ((stream timestamp-preserving-stream))
  #+abcl (ext:charpos (timestamp-preserving-stream-new-stream stream))
  #+allegro (excl:charpos (timestamp-preserving-stream-new-stream stream))
  #+(or clasp ecl) (sys:file-column (timestamp-preserving-stream-new-stream stream))
  #+cmucl (lisp::charpos (timestamp-preserving-stream-new-stream stream))
  #+sbcl (sb-kernel:charpos (timestamp-preserving-stream-new-stream stream)))

(defmethod cl:close ((stream timestamp-preserving-stream) &key abort)
  (with-accessors ((old-stream timestamp-preserving-stream-old-stream)
                   (new-stream timestamp-preserving-stream-new-stream)
                   (old-path timestamp-preserving-stream-old-path)
                   (new-path timestamp-preserving-stream-new-path))
      stream
    (cond ((null new-stream)
           nil)
          (abort
           (close new-stream)
           (delete-file new-path)
           (when old-stream
             (close old-stream))
           (setf old-stream nil
                 new-stream nil)
           t)
          (t
           (let ((replace-old (or (null old-stream)
                                  (read-char old-stream nil nil))))
             (when old-stream
               (close old-stream))
             (close new-stream)
             (setf old-stream nil
                   new-stream nil)
             (cond (replace-old
                    (when (probe-file old-path)
                      (delete-file old-path))
                    (rename-file new-path
                                 (make-pathname :name (pathname-name old-path)
                                                :type (pathname-type old-path))))
                   (t
                    (delete-file new-path)))
              t)))))

