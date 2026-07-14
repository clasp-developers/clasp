(in-package #:core)

(make-package "ECCLESIA" :use '("CL"))
(make-package "QUAVIVER/SCHUBFACH" :use '("CL"))
(make-package "INCLESS" :use '("CL"))

(make-package "INCLESS-INTRINSIC" :use '("CL"))
(make-package "INRAVINA" :use '("CL"))
(shadow '(#:copy-pprint-dispatch
          #:pprint-dispatch
          #:pprint-exit-if-list-exhausted
          #:pprint-fill
          #:pprint-indent
          #:pprint-linear
          #:pprint-logical-block
          #:pprint-newline
          #:pprint-pop
          #:pprint-tab
          #:pprint-tabular
          #:set-pprint-dispatch)
        "INRAVINA")
(make-package "INRAVINA-INTRINSIC" :use '("CL"))
(make-package "INVISTRA" :use '("CL"))
(make-package "INVISTRA-INTRINSIC" :use '("CL"))

(in-package #:invistra)

(defvar *format-output* nil)

(defparameter *extra-space* nil)

(defparameter *line-length* nil)

(defparameter *newline-kind* nil)

(defvar *more-arguments-p* nil)

(defvar *argument-index* nil)

(defvar *remaining-argument-count* nil)

(defvar *pop-argument* nil)

(defvar *go-to-argument* nil)

(defvar *pop-remaining-arguments* nil)

(defvar *inner-exit-if-exhausted* nil)

(defvar *outer-exit-if-exhausted* nil)

(defvar *inner-exit* nil)

(defvar *outer-exit* nil)

(defun format-with-client (client destination control &rest args)
  (declare (ignore client))
  (let ((*format-output* (cond ((null destination)
                                (make-string-output-stream))
                               ((eq destination t)
                                *standard-output*)
                               (t
                                destination))))
    (apply control *format-output* args)
    (if (null destination)
        (get-output-stream-string *format-output*)
        nil)))

(in-package #:incless)

(declaim (ftype (function (t t t t t t) t) write-unreadable-object))

(in-package #:incless-intrinsic)

(defvar *client* nil)

(in-package #:inravina-intrinsic)

(defvar *standard-pprint-dispatch* nil)
