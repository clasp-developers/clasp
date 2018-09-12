;;;; Package functions that need the condition system to be up
;;;; so that we can have restarts etc.
;;;; This is actually more of them than we have here- there are
;;;; some places where we need to be signaling continuable
;;;; errors and aren't.

(in-package #:ext)

(defmacro with-package-read-lock ((package) &body body)
  `(core:call-with-package-read-lock ,package (lambda () ,@body)))
(defmacro with-package-read-write-lock ((package) &body body)
  `(core:call-with-package-read-write-lock ,package (lambda () ,@body)))

(defun package-local-nicknames (package-designator)
  "Return an alist (string . package) of local nicknames in the given package.
See also: :LOCAL-NICKNAMES option to DEFPACKAGE."
  (let ((package (find-package package-designator)))
    (with-package-read-lock (package)
      (core:package-local-nicknames-internal package))))

(defun add-package-local-nickname (nickname-designator actual-package &optional (package-designator *package*))
  "Add a nickname for actual-package, local to the designated package.
Signals a continuable error if the new nickname is already a nickname in the designated package
for a different package.
See also: :LOCAL-NICKNAMES option to DEFPACKAGE."
  (let* ((nickname (string nickname-designator))
         (actual (find-package actual-package))
         (package (find-package package-designator))
         old
         (force nil))
    (tagbody
     loop
       (with-package-read-write-lock (package)
         (let* ((locals (core:package-local-nicknames-internal package))
                (existing (assoc nickname locals :test #'string=)))
           (cond ((null existing)
                  (push (cons nickname actual) (core:package-local-nicknames-internal package)))
                 ((eq (cdr existing) actual)) ; already in there; do nothing.
                 (force (setf (cdr existing) actual))
                 (t (setf old (cdr existing)) (go err)))))
       (go done)
     err ; signal errors without holding the lock.
       (restart-case
           (error 'core:simple-package-error
                  :package package
                  :format-control "Cannot add ~a as local nickname for ~a in ~a: ~
it's already a local nickname for ~a."
                  :format-arguments (list nickname (package-name actual) (package-name package) (package-name old)))
         (keep-old ()
           :report (lambda (s)
                     (format s "Keep ~a as local nickname for ~a."
                             nickname (package-name old))))
         (change-nick ()
           :report (lambda (s)
                     (format s "Use ~a as local nickname for ~a instead."
                             nickname (package-name actual)))
           (setf force t)
           (go loop)))
     done)
    package))

(defun remove-package-local-nickname (nickname-designator &optional (package-designator *package*))
  "If the designated package has the nickname locally, it is removed and a true value is returned.
Otherwise NIL is returned.
See also: :LOCAL-NICKNAMES option to DEFPACKAGE."
  (let ((package (find-package package-designator))
        (nickname (string nickname-designator)))
    (with-package-read-write-lock (package)
      (let* ((locals (core:package-local-nicknames-internal package))
             (pair (assoc nickname locals :test #'string=)))
        (when pair ; else return NIL
          (setf (core:package-local-nicknames-internal package) (remove pair locals :test #'eq))
          t)))))

(defun package-locally-nicknamed-by-list (package-designator)
  "Return a list of packages that have a local nickname for the designated package.
See also: :LOCAL-NICKNAMES option to DEFPACKAGE."
  (let ((package (find-package package-designator))
        (result nil))
    (dolist (p (list-all-packages) result)
      (when (find package (package-local-nicknames p) :key #'cdr :test #'eq)
        (push p result)))))
