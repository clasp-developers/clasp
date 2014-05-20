;;;; reading circular data: the #= and ## readmacros

;;; objects already seen by CIRCLE-SUBST
(defvar *sharp-equal-circle-table*)
(declaim (type hash-table *sharp-equal-circle-table*))

;; This function is kind of like NSUBLIS, but checks for circularities and
;; substitutes in arrays and structures as well as lists. The first arg is an
;; alist of the things to be replaced assoc'd with the things to replace them.
(defun circle-subst (old-new-alist tree)
  (cond ((not (typep tree '(or cons (array t) instance funcallable-instance)))
         (let ((entry (find tree old-new-alist :key #'second)))
           (if entry (third entry) tree)))
        ((null (gethash tree *sharp-equal-circle-table*))
         (setf (gethash tree *sharp-equal-circle-table*) t)
         (cond ((consp tree)
                (let ((a (circle-subst old-new-alist (car tree)))
                      (d (circle-subst old-new-alist (cdr tree))))
                  (unless (eq a (car tree))
                    (rplaca tree a))
                  (unless (eq d (cdr tree))
                    (rplacd tree d))))
               ((arrayp tree)
                (with-array-data ((data tree) (start) (end))
                  (declare (fixnum start end))
                  (do ((i start (1+ i)))
                      ((>= i end))
                    (let* ((old (aref data i))
                           (new (circle-subst old-new-alist old)))
                      (unless (eq old new)
                        (setf (aref data i) new))))))
               ((typep tree 'instance)
                (let* ((n-untagged (layout-n-untagged-slots (%instance-layout tree)))
                       (n-tagged (- (%instance-length tree) n-untagged)))
                  ;; N-TAGGED includes the layout as well (at index 0), which
                  ;; we don't grovel.
                  (do ((i 1 (1+ i)))
                      ((= i n-tagged))
                    (let* ((old (%instance-ref tree i))
                           (new (circle-subst old-new-alist old)))
                      (unless (eq old new)
                        (setf (%instance-ref tree i) new))))
                  (do ((i 0 (1+ i)))
                      ((= i n-untagged))
                    (let* ((old (%raw-instance-ref/word tree i))
                           (new (circle-subst old-new-alist old)))
                      (unless (= old new)
                        (setf (%raw-instance-ref/word tree i) new))))))
               ((typep tree 'funcallable-instance)
                (do ((i 1 (1+ i))
                     (end (- (1+ (get-closure-length tree)) sb!vm:funcallable-instance-info-offset)))
                    ((= i end))
                  (let* ((old (%funcallable-instance-info tree i))
                         (new (circle-subst old-new-alist old)))
                    (unless (eq old new)
                      (setf (%funcallable-instance-info tree i) new))))))
         tree)
        (t tree)))



;;; Sharp-equal works as follows. When a label is assigned (i.e. when
;;; #= is called) we GENSYM a symbol is which is used as an
;;; unforgeable tag. *SHARP-SHARP-ALIST* maps the integer tag to this
;;; gensym.
;;;
;;; When SHARP-SHARP encounters a reference to a label, it returns the
;;; symbol assoc'd with the label. Resolution of the reference is
;;; deferred until the read done by #= finishes. Any already resolved
;;; tags (in *SHARP-EQUAL-ALIST*) are simply returned.
;;;
;;; After reading of the #= form is completed, we add an entry to
;;; *SHARP-EQUAL-ALIST* that maps the gensym tag to the resolved
;;; object. Then for each entry in the *SHARP-SHARP-ALIST, the current
;;; object is searched and any uses of the gensysm token are replaced
;;; with the actual value.
(defvar *sharp-sharp-alist* ())

(defun sharp-equal (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-equal (values)))
  (unless label
    (simple-reader-error stream "missing label for #=" label))
  (when (or (assoc label *sharp-sharp-alist*)
            (assoc label *sharp-equal-alist*))
    (simple-reader-error stream "multiply defined label: #~D=" label))
  (let* ((tag (gensym))
         (*sharp-sharp-alist* (acons label tag *sharp-sharp-alist*))
         (obj (read stream t nil t)))
    (when (eq obj tag)
      (simple-reader-error stream
                     "must tag something more than just #~D#"
                     label))
    (push (list label tag obj) *sharp-equal-alist*)
    (let ((*sharp-equal-circle-table* (make-hash-table :test 'eq :size 20)))
      (circle-subst *sharp-equal-alist* obj))))

(defun sharp-sharp (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-sharp nil))
  (unless label
    (simple-reader-error stream "missing label for ##" label))

  (let ((entry (assoc label *sharp-equal-alist*)))
    (if entry
        (third entry)
        (let (;; Has this label been defined previously? (Don't read
              ;; ANSI "2.4.8.15 Sharpsign Equal-Sign" and worry that
              ;; it requires you to implement forward references,
              ;; because forward references are disallowed in
              ;; "2.4.8.16 Sharpsign Sharpsign".)
              (pair (assoc label *sharp-sharp-alist*)))
          (unless pair
            (simple-reader-error stream
                                 "reference to undefined label #~D#"
                                 label))
          (cdr pair)))))
