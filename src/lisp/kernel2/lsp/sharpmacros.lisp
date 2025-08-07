;;;; reading circular data: the #= and ## readmacros

(in-package #:core)

;;; Based on the SBCL version
(defconstant +sharp-marker+ '+sharp-marker+)

(defun circle-subst (circle-table tree)
  (cond ((and (sharp-equal-wrapper-p tree)
              (not (eq (sharp-equal-wrapper-value tree) +sharp-marker+)))
         (sharp-equal-wrapper-value tree))
        ((null (gethash tree circle-table))
         (setf (gethash tree circle-table) t)
         (cond ((consp tree)
                (let ((a (circle-subst circle-table (car tree)))
                      (d (circle-subst circle-table (cdr tree))))
                  (unless (eq a (car tree))
                    (rplaca tree a))
                  (unless (eq d (cdr tree))
                    (rplacd tree d))))
               ((arrayp tree)
                (do ((i 0 (+ i 1)))
                    ((>= i (array-total-size tree)))
                  (let* ((old (row-major-aref tree i))
                         (new (circle-subst circle-table old)))
                    (unless (eq old new)
                      (setf (row-major-aref tree i) new)))))
               ((hash-table-p tree)
                (let ((to-add nil))
                  (maphash (lambda (key value)
                             (let ((subst-key (circle-subst circle-table key))
                                   (subst-value (circle-subst circle-table value)))
                               (cond ((eq subst-key key)
                                      ;; easy case - just alter the value.
                                      (setf (gethash key tree) subst-value))
                                     (t
                                      ;; if the key needed substitution, though,
                                      ;; things are trickier. We can't alter the
                                      ;; key of a k-v pair in-place, so we have
                                      ;; to remove the old pair and add a new one.
                                      ;; And we can't add the new one during maphash.
                                      (remhash key tree)
                                      (push (cons subst-key subst-value) to-add)))))
                           tree)
                  ;; Add new pairs from the key-subst case.
                  (dolist (pair to-add)
                    (setf (gethash (car pair) tree) (cdr pair)))))
               ;; Do something for builtin objects
               ((cxx-object-p tree)
                (let ((record (make-record-patcher (lambda (object)
                                                     (circle-subst circle-table object)))))
                  (patch-object tree record)))
               ((typep tree 'structure-object)
                (dotimes (i (clos::class-size (class-of tree)))
                  (setf (clos:standard-instance-access tree i)
                        (circle-subst circle-table
                                      (clos:standard-instance-access tree i)))))
               ;; For general objects go full MOP
               ((typep tree 'standard-object)
                (let ((class (class-of tree)))
                  (dolist (slotd (clos:class-slots class))
                    (when (clos:slot-boundp-using-class class tree slotd)
                      (setf (clos:slot-value-using-class class tree slotd)
                            (circle-subst circle-table
                                          (clos:slot-value-using-class class tree slotd))))))))
         tree)
        (t tree)))

(defun sharp-equal (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-equal (values)))
  (unless label
    (simple-reader-error stream "missing label for #=" label))
  (cond ((not *sharp-equal-final-table*)
         (setf *sharp-equal-final-table* (make-hash-table)))
        ((gethash label *sharp-equal-final-table*)
         (simple-reader-error stream "multiply defined label: #~D=" label)))
  (let ((tag (setf (gethash label *sharp-equal-final-table*)
                   (make-sharp-equal-wrapper label)))
        (obj (read stream t nil t)))
    (when (eq obj tag)
      (simple-reader-error stream
                           "must tag something more than just #~D#"
                           label))
    (setf (sharp-equal-wrapper-value tag) obj)
    (circle-subst (make-hash-table :test 'eq) obj)))

(defun sharp-sharp (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-sharp nil))
  (unless label
    (simple-reader-error stream "missing label for ##" label))
  ;; Has this label been defined previously? (Don't read
  ;; ANSI "2.4.8.15 Sharpsign Equal-Sign" and worry that
  ;; it requires you to implement forward references,
  ;; because forward references are disallowed in
  ;; "2.4.8.16 Sharpsign Sharpsign".)
  (let ((entry (and
                *sharp-equal-final-table*
                (gethash label *sharp-equal-final-table*))))
    (cond ((not entry)
           (simple-reader-error stream
                                "reference to undefined label #~D#"
                                label))
          ((eq (sharp-equal-wrapper-value entry) +sharp-marker+)
           entry)
          (t
           (sharp-equal-wrapper-value entry)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Reader macro for builtin objects
;;
(defun read-cxx-object (stream char n)
  (declare (ignore char n))
  (let ((description (read stream t nil t)))
    (apply #'load-cxx-object (car description) (cdr description))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Reader macro for builtin objects
;;
(defun do-read-dense-specialized-array (stream char n)
  (declare (ignore char))
  (read-dense-specialized-array stream n))

(defun sharpmacros-enhance (readtable)
  (set-dispatch-macro-character #\# #\= #'sharp-equal readtable)
  (set-dispatch-macro-character #\# #\# #'sharp-sharp readtable)
  (set-dispatch-macro-character #\# #\I #'read-cxx-object readtable)
  (set-dispatch-macro-character #\# #\D #'read-dense-specialized-array readtable))

(sharpmacros-enhance *readtable*)
(sharpmacros-enhance core:+standard-readtable+)
