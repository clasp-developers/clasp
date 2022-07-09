(in-package #:ninja)

(defun logical-char-p (ch)
  (or (char= #\- ch)
      (lower-case-p ch)
      (digit-char-p ch)))

(defun find-minimal-pathname-translation (path)
  (if (and (every #'logical-char-p (pathname-name path))
           (every #'logical-char-p (pathname-type path)))
    (let ((tail (member-if (lambda (x)
                             (notevery #'logical-char-p x))
                           (reverse (cdr (pathname-directory path))))))
      (when tail
        (make-pathname :directory (list* :relative (nreverse (cons :wild-inferiors tail)))
                       :name :wild
                       :type :wild)))
     path))

(defun translate-component (component)
  (if (stringp component)
    (substitute-if-not #\- #'alphanumericp (string-upcase component))
    component))

(defun make-logical-pathname-representation (host path &key version prefix)
  (make-pathname :host host
                 :directory (list* :absolute
                                   (mapcar #'translate-component
                                           (if prefix
                                               (cons prefix (cdr (pathname-directory path)))
                                               (cdr (pathname-directory path)))))
                 :name (translate-component (pathname-name path))
                 :type (translate-component (pathname-type path))
                 :version version))
