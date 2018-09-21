;;;; reading circular data: the #= and ## readmacros

(in-package :core)

(defun %reader-error (stream msg &rest arguments)
  (apply #'simple-reader-error stream msg arguments))

;;; Based on the SBCL version
(defconstant +sharp-marker+ '+sharp-marker+)

(defun circle-subst (circle-table tree)
  (cond ((and (core:sharp-equal-wrapper-p tree)
              (not (eq (core:sharp-equal-wrapper-value tree) +sharp-marker+)))
         (core:sharp-equal-wrapper-value tree))
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
                (do ((i 0 (1+ i)))
                    ((>= i (array-total-size tree)))
                  (let* ((old (row-major-aref tree i))
                         (new (circle-subst circle-table old)))
                    (unless (eq old new)
                      (setf (row-major-aref tree i) new))))
		)
               #| TODO: MUST PROVIDE FIXUP FOR HASH-TABLES!!!! |#
               ((hash-table-p tree)
                (error "Handle hash-tables in circle-subst"))
               ;; Do something for builtin objects
               ((typep tree 'cxx-object)
                (error "Handle cxx-object in circle-subst tree: ~s" tree)
                #+(or)(let ((record (make-record-patcher circle-table)))
                        (patch-object tree record)))
               #+clos ((typep tree 'instance)
                       (error "Handle instance in circle-subst")
                       #+(or)(let* ((n-untagged (layout-n-untagged-slots (%instance-layout tree)))
                                    (n-tagged (- (%instance-length tree) n-untagged)))
                               ;; N-TAGGED includes the layout as well (at index 0), which ; ;
                               ;; we don't grovel. ; ;
                               (do ((i 1 (1+ i)))
                                   ((= i n-tagged))
                                 (let* ((old (%instance-ref tree i))
                                        (new (circle-subst circle-table old)))
                                   (unless (eq old new)
                                     (setf (%instance-ref tree i) new))))
                               (do ((i 0 (1+ i)))
                                   ((= i n-untagged))
                                 (let* ((old (%raw-instance-ref/word tree i))
                                        (new (circle-subst circle-table old)))
                                   (unless (= old new)
                                     (setf (%raw-instance-ref/word tree i) new))))))
               #+(or) ((typep tree 'funcallable-instance)
                       (error "Handle funcallable-instance in circle-subst")
                       #+(or)(do ((i 1 (1+ i))
                                  (end (- (1+ (get-closure-length tree)) %funcallable-instance-info-offset)))
                                 ((= i end))
                               (let* ((old (%funcallable-instance-info tree i))
                                      (new (circle-subst circle-table old)))
                                 (unless (eq old new)
                                   (setf (%funcallable-instance-info tree i) new))))))
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
                   (core:make-sharp-equal-wrapper)))
        (obj (read stream t nil t)))
    (when (eq obj tag)
      (simple-reader-error stream
                           "must tag something more than just #~D#"
                           label))
    (setf (core:sharp-equal-wrapper-value tag) obj)
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
          ((eq (core:sharp-equal-wrapper-value entry) +sharp-marker+)
           entry)
          (t
           (core:sharp-equal-wrapper-value entry)))))

(defun sharpmacros-enhance ()
  (set-dispatch-macro-character #\# #\= #'sharp-equal)
  (set-dispatch-macro-character #\# #\# #'sharp-sharp))

(sharpmacros-enhance)
