(in-package #:static-gfs)

;;; specifying slot-names is not supported.
;;; (which is fine, how often are we going to be able to optimize
;;;  reinitialize-instance)

(defun reconstruct-arguments (keys params)
  (loop for key in keys for param in params
        collect `',key collect param))

(defun shared-initialize-form (class iform keys params)
  (let ((patch-list
          (list
           (cons (find-method #'shared-initialize nil
                              (list (find-class 't) (find-class 't)))
                 #'standard-shared-initialize-form)))
        (methods (compute-applicable-methods
                  #'shared-initialize
                  (list (clos:class-prototype class) 't))))
    (if (can-static-effective-method-p methods patch-list)
        (static-effective-method
         #'shared-initialize methods (list class iform keys params) patch-list
         (list* iform t (reconstruct-arguments keys params)))
        (default-shared-initialize-form iform keys params))))

(defun default-shared-initialize-form (iform keys params)
  `(locally
       (declare (notinline shared-initialize))
     (apply #'shared-initialize ,iform ,@(reconstruct-arguments keys params))))

(defun initarg-position (initargs keys)
  (position-if (lambda (key) (member key initargs :test #'eq)) keys))

(defun slotd-form (class slotd)
  (let ((name (clos:slot-definition-name slotd)))
    `(or (find ',name (clos:class-slots ,class) :key #'clos:slot-definition-name)
         (error "BUG: Slotd disappeared"))))

(defun ltv-slotd-form (class slotd)
  ;; This exists because I'd like to make these forms acceptable to COMPILE-FILE.
  ;; For regular COMPILE it should be redundant but harmless.
  `(load-time-value ,(slotd-form class slotd) t))

(defun uncustomizable-slot-p (class slotd)
  (and (or (eq (class-of class) #.(find-class 'standard-class))
           (eq (class-of class) #.(find-class 'clos:funcallable-standard-class)))
       (eq (class-of slotd) #.(find-class 'clos:standard-effective-slot-definition))))

(defun setf-slot-form (class iform slotd value)
  (if (uncustomizable-slot-p class slotd)
      (fast-setf-slot-form class iform slotd value)
      `(setf (clos:slot-value-using-class ,class ,iform
                                          ,(ltv-slotd-form class slotd))
             ,value)))

(defun fast-setf-slot-form (class iform slotd value)
  (ecase (clos:slot-definition-allocation slotd)
    (:instance
     `(si:instance-set ,iform
                       ,(clos:slot-definition-location slotd)
                       ,value))
    (:class
     `(rplaca (load-time-value (clos:slot-definition-location
                                ,(slotd-form class slotd)))
              ,value))))

(defun fast-slot-unboundp-form (class iform slotd)
  `(eq (load-time-value (core:unbound) t)
       ,(ecase (clos:slot-definition-allocation slotd)
          (:instance
           `(si:instance-ref ,iform ,(clos:slot-definition-location slotd)))
          (:class
           `(car (load-time-value
                  (clos:slot-definition-location
                   ,(slotd-form class slotd))))))))

(defun setf-slot-from-initform-form (class iform slotd)
  (when (clos:slot-definition-initfunction slotd)
    `(let (;; I don't think there's much way around this (for compile-file).
           ;; The initform is useless if it refers to a lexical environment,
           ;; and we don't know if it does.
           (initfunction
             (load-time-value
              (or (clos:slot-definition-initfunction ,(slotd-form class slotd))
                  (error "BUG: initfunction disappeared"))
              t)))
       ,(if (uncustomizable-slot-p class slotd)
            `(when ,(fast-slot-unboundp-form class iform slotd)
               ,(fast-setf-slot-form class iform slotd '(funcall initfunction)))
            `(let ((rslotd ,(ltv-slotd-form class slotd)))
               (unless (clos:slot-boundp-using-class ,class ,iform rslotd)
                 (setf (clos:slot-value-using-class ,class ,iform rslotd)
                       (funcall initfunction))))))))

(defun standard-shared-initialize-form (class iform keys params)
  ;; This is pretty much the most important part of the static-gfs system.
  ;; NOTE: CLHS 7.1.4 is really tricky. I'm not sure I'm doing it right.
  ;; I'm also not sure we're doing it right at runtime...
  (let ((slotds (clos:class-slots class)))
    `(progn
       ,@(loop for slotd in slotds
               for initargs = (clos:slot-definition-initargs slotd)
               for pos = (initarg-position initargs keys)
               if pos
                 ;; argument was supplied
                 collect (setf-slot-form
                          class iform slotd (nth pos params))
               else ;; not supplied, must use initform
               collect (setf-slot-from-initform-form class iform slotd))
       ,iform)))
