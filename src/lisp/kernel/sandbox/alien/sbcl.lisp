(in-package #:clasp-sandbox)

;;; these few are kind of pointless and would be the same on any implementation,
;;; but uniformity is nice.
(defmethod sicl-genv:symbol-value (symbol (env sb-kernel:lexenv))
  (symbol-value symbol))

(defmethod sicl-genv:fdefinition (symbol (env sb-kernel:lexenv))
  (fdefinition symbol))

(defmethod sicl-genv:find-class (symbol (env sb-kernel:lexenv))
  (find-class symbol nil env))

;;; less pointless.

(defmethod sicl-genv:declarations ((env sb-kernel:lexenv))
  (sb-cltl2:declaration-information 'declaration env))

(defmethod sicl-genv:type-expander (symbol (env sb-kernel:lexenv))
  (sb-int:info :type :expander symbol))

(defmethod sicl-genv:setf-expander (symbol (env sb-kernel:lexenv))
  (let ((info (sb-int:info :setf :expander symbol)))
    (typecase info
      ;; short DEFSETF. the symbol is the update-fn.
      (symbol
       (lambda (place env)
         (let ((temps (loop for arg in (cdr place) collect (gensym "SETF-TEMP")))
               (vals (cdr place))
               (stores (list (gensym "SETF-STORE"))))
           (values temps vals stores
                   `(,info ,@temps ,(first stores))
                   `(,symbol ,@temps)))))
      ;; long DEFSETF. car is the number of values accepted, cdr is a function.
      (cons
       (let ((num-stores (car info))
             (expander (the function (cdr info))))
         ;; could probably compute the stores outside the closure as well, but it's fine.
         (lambda (place env)
           (let ((stores (loop repeat num-stores collect (gensym "SETF-STORE")))
                 (temps (loop for arg in (cdr place) collect (gensym "SETF-TEMP")))
                 (vals (cdr place)))
             (values temps vals stores
                     (apply expander temps env stores)
                     `(,symbol ,@temps))))))
      ;; DEFINE-SETF-EXPANDER. expander takes the same args.
      (function info)
      ;; no info. in this case it might still be a struct setf.
      ;; however, sbcl defines a setf function in addition to
      ;; the setf expansion. convenient. therefore we can fall back
      ;; to the default in this case. (i.e., say there is no expander.)
      (null nil))))
