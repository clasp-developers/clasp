(in-package :cscrape)

(define-condition interpret-error () ())

(define-condition missing-base (interpret-error)
  ((tag :initarg :tag :accessor tag))
  (:report (lambda (condition stream)
             (format stream "~a Missing base for ~a ~a."
                     (tags:source-pos (tag condition))
                     (tags:tag-code (tag condition))
                     (tags:identifier (tag condition))))))

(define-condition tag-error (interpret-error)
  ((message :initarg :message :accessor message)
   (tag :initarg :tag :accessor tag)
   (message-args :initform nil :initarg :message-args :accessor message-args))
  (:report (lambda (condition stream)
             (format stream "~a ~a."
                     (tags:source-pos (tag condition))
                     (apply #'format nil (message condition) (message-args condition))))))

(define-condition bad-pointer (interpret-error)
  ((pointer-text :initarg :pointer-text :accessor pointer-text))
  (:report (lambda (condition stream)
             (format stream "~a Bad pointer text ~a."
                     (tags:source-pos (tag condition))
                     (tags:pointer% (tag condition))))))

(define-condition missing-namespace (interpret-error)
  ((tag :initarg :tag :accessor tag))
  (:report (lambda (condition stream)
             (format stream "~a Missing namespace for ~a ~a."
                     (tags:source-pos (tag condition))
                     (tags:tag-code (tag condition))
                     (tags:identifier (tag condition))))))

(define-condition namespace-mismatch (interpret-error)
  ((tag :initarg :tag :accessor tag))
  (:report (lambda (condition stream)
             (format stream "~a Mismatch between enclosed namespace and ~a ~a"
                     (tags:source-pos (tag condition))
                     (tags:tag-code (tag condition))
                     (tags:identifier (tag condition))))))

(defparameter *maximum-unaccounted-tag-lines* 20
  "How many lines of source NOT belonging to a declaration may separate its modifier tags from its
CL_DEFUN/CL_DEFMETHOD.

The check exists to catch an ORPHANED modifier - a CL_LISPIFY_NAME whose function was deleted or
moved, which would otherwise attach silently to whatever CL_DEFMETHOD came next and rename the wrong
function.  Intervening SOURCE is the signal for that.  A modifier's own text is not, which is what
TAG-EXTRA-LINES below discounts.")

(defun tag-payload-text (tag)
  "TAG's own multi-line body, or NIL when it has none.

A tag's LINE% is where it STARTS, so a tag with a long body pushes everything after it out of range
and is charged for its own length.  A 40-line CL_DOCSTRING is not 40 lines of unrelated code coming
between a declaration and its name - it IS the declaration."
  (let ((text (typecase tag
                (tags:cl-docstring-tag (tags:docstring% tag))
                (tags:cl-docstring-long-tag (tags:docstring-long% tag))
                (tags:cl-lambda-tag (tags:lambda-list% tag))
                (tags:cl-declare-tag (tags:declare-form% tag))
                (t nil))))
    (and (stringp text) text)))

(defun tag-extra-lines (tag)
  "Lines TAG's body occupies BEYOND its first - the amount it inflates any distance measured across
it.  Zero for an ordinary single-line tag."
  (let ((text (tag-payload-text tag)))
    (if text (count #\Newline text) 0)))

(defun error-if-bad-expose-info-setup* (tag other-tag &optional (slack (tag-extra-lines other-tag)))
  "SLACK defaults to OTHER-TAG's own body length.

That is the right answer for a caller checking ONE modifier - a class tag against the CL_DOCSTRING
above it, say - where the only thing inflating the distance is that tag itself.  The group check
below passes the combined total instead, because there the tag being measured is often not the one
with the long body: a CL_LISPIFY_NAME sitting above a 40-line CL_DOCSTRING has no body of its own
and would get no discount at all."
  (declare (optimize (speed 3)))
  (unless (and (string= (tags:file% tag) (tags:file% other-tag))
               (< (- (tags:line% tag) (tags:line% other-tag) slack)
                  *maximum-unaccounted-tag-lines*))
    (error 'bad-cl-defun/defmethod
           :tag tag :other-tag other-tag
           :gap (- (tags:line% tag) (tags:line% other-tag))
           :slack slack
           :allowed *maximum-unaccounted-tag-lines*)))

(defun error-if-bad-expose-info-setup (tag cur-name cur-lambda cur-declare cur-docstring cur-docstring-long &optional cur-priority)
  ;; SLACK is the combined body length of EVERY modifier in this group, computed once and applied to
  ;; all of them.  Discounting each tag against only its own body would not help the case that
  ;; actually bites: a long CL_DOCSTRING pushing a CL_LISPIFY_NAME that sits ABOVE it out of range,
  ;; where the tag being measured is not the one inflating the distance.
  (let* ((modifiers (remove nil (list cur-name cur-lambda cur-declare
                                      cur-docstring cur-docstring-long cur-priority)))
         (slack (reduce #'+ modifiers :key #'tag-extra-lines :initial-value 0)))
    (dolist (modifier modifiers)
      (error-if-bad-expose-info-setup* tag modifier slack))))

;;; A WARNING, not an INTERPRET-ERROR like everything above, because the check that signals it
;;; is a heuristic - it cannot do C++ name resolution, so it can flag a type that is genuinely
;;; visible at global scope.  Re-parent it to INTERPRET-ERROR if it turns out to be clean
;;; across the tree.
(define-condition unqualified-signature-type (warning)
  ((tag :initarg :tag :accessor tag)
   (kind :initarg :kind :accessor kind)
   (type-name :initarg :type-name :accessor type-name)
   (namespace :initarg :namespace :accessor namespace))
  (:report (lambda (condition stream)
             (format stream "~a ~(~a~) type ~s is unqualified - the generated wrapper is ~
                             emitted outside namespace ~a.  Did you mean ~a::~a?"
                     (tags:source-pos (tag condition))
                     (kind condition)
                     (type-name condition)
                     (namespace condition)
                     (namespace condition)
                     (string-trim "*& " (type-name condition))))))
