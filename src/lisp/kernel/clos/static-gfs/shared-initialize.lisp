(in-package #:static-gfs)

;;; The slot-names argument must be constant.

(defun reconstruct-arguments (keys params)
  (loop for key in keys for param in params
        collect `',key collect param))

(defun shared-initialize-form (class slot-names iform keys params)
  (let ((patch-list
          (list
           (cons (find-method #'shared-initialize nil
                              (list (find-class 't) (find-class 't)))
                 #'standard-shared-initialize-form)))
        (methods (compute-applicable-methods
                  #'shared-initialize
                  (list (clos:class-prototype class) slot-names))))
    (if (can-static-effective-method-p methods patch-list)
        (static-effective-method
         #'shared-initialize methods (list class slot-names iform keys params)
         patch-list
         (list* iform `',slot-names (reconstruct-arguments keys params)))
        (default-shared-initialize-form slot-names iform keys params))))

(defun default-shared-initialize-form (slot-names iform keys params)
  `(locally
       (declare (notinline shared-initialize))
     (shared-initialize ,iform ',slot-names
                        ,@(reconstruct-arguments keys params))))

(defun initarg-position (initargs keys)
  (position-if (lambda (key) (member key initargs :test #'eq)) keys))

(defun setf-slot-from-initform-form (class iform slotd)
  (when (clos:slot-definition-initfunction slotd)
    `(unless (static-slot-boundp-using-class
              ,class ,iform (ltv-slotd ,class ,slotd))
       (setf (static-slot-value-using-class ,class ,iform
                                            (ltv-slotd ,class ,slotd))
             ;; I don't think there's much way around this (for compile-file).
             ;; The initform is useless if it refers to a lexical environment,
             ;; and we don't know if it does.
             (funcall
              (load-time-value
               (or (clos:slot-definition-initfunction ,(slotd-form class slotd))
                   (error "BUG: initfunction disappeared"))))))))

(defun standard-shared-initialize-form (class slot-names iform keys params)
  ;; NOTE: CLHS 7.1.4 is really tricky. I'm not sure I'm doing it right.
  ;; I'm also not sure we're doing it right at runtime...
  (let ((slotds (clos:class-slots class)))
    `(progn
       ,@(loop for slotd in slotds
               for initargs = (clos:slot-definition-initargs slotd)
               for pos = (initarg-position initargs keys)
               if pos
                 ;; argument was supplied
                 collect `(setf (static-slot-value-using-class
                                 ,class ,iform (ltv-slotd ,class ,slotd))
                                ,(nth pos params))
               else ;; not supplied, must use initform
               when (or (eq slot-names t)
                        (member (clos:slot-definition-name slotd)
                                slot-names))
                 collect (setf-slot-from-initform-form class iform slotd))
       ,iform)))
