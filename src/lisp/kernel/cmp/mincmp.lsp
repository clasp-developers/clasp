(in-package :cmp)
;;; (create-cell)
;;; (activation-frame-read af <depth> <idx>)
;;; (activation-frame-write af <depth> <idx> <val>)
;;; (enclose <function-with-extra-arg> <cells>*)


(defun mincomp-progn (expr env)
  `(progn ,@(mapcar (lambda (x) (mincomp x env)) (rest expr))))


(defun mincomp-function (expr env)
  (let ((head (cadr expr))
        (func-env (clcenv:add-closure env)))
    (format t "mincomp-function head: ~a~%" head)
    (cond
      ((symbolp head) `(function ,(cadr expr)))
      ((and (consp head)
            (eq (car head) 'setf))
       `(function ,head))
      ((and (consp head)
            (eq (car head) 'lambda))
       (let ((lambda-list (cadr head))
             (body (cddr head)))
         (multiple-value-bind (declares code docstring specials)
             (process-declarations body t)
           (format t "It's a lambda - what do I do?: ~a~%" head)
           (format t "     lambda-list: ~s~%" lambda-list)
           (format t "     body: ~s~%" body)
           (format t "     declares: ~s~%" declares)
           (format t "     docstring: ~s~%" docstring)
           (format t "     specials: ~s~%" specials)
           (format t "        code:~s~%" code)
       
         ;; stuff here
         )))
      (t (error "FUNCTION special operator doesn't support ~a" head)))))
       
(defun separate-pairs (alternating-list)
  (let (vars vals)
    (do* ((cur alternating-list (cddr cur))
          (cur-var (car cur) (car cur))
          (cur-val (cadr cur) (cadr cur)))
         ((endp cur) (values (nreverse vars) (nreverse vals)))
      (push cur-var vars)
      (push cur-val vals))))

(defun mincomp-if (expr env)
  `(if ,(mincomp (cadr expr) env)
       ,(mincomp (caddr expr) env)
       ,(mincomp (cadddr expr) env)))

(defun mincomp-classify-setq (env var val)
  (let ((var-info (clcenv:variable-info env var)))
    `(classify-the-setq ,var ,val ,var-info)))

(defun mincomp-setq (expr env)
  (multiple-value-bind (vars vals)
      (separate-pairs (cdr expr))
    (mapcar (lambda (var val)
              (let ((expanded (macroexpand var env)))
                (if (eq expanded var)
                    (mincomp-classify-setq env var val)
                    (mincomp `(setf ,expanded ,val) env))))
            vars vals)))

(defun mincomp-block (expr env)
  (let* ((block-symbol (cadr expr))
         (body (cddr expr))
         (block-env (clcenv:add-block env block-symbol)))
    `(block ,block-symbol
       ,@(mapcar (lambda (f)
                   (mincomp f block-env))
                 body))))

(defun mincomp-return-from (expr env)
  (let* ((block-symbol (cadr expr))
         (return-form (caddr expr)))
    (multiple-value-bind (block-info non-local-return-from)
        (clcenv:block-info env block-symbol)
      (if non-local-return-from
          `(return-from-non-local ,block-symbol ,(mincomp return-form env))
          `(return-from-local ,block-symbol ,(mincomp return-form env))))))

(defun mincomp-tagbody (expr env)
  (let ((tag-env env))
    (mapc (lambda (f)
            (if (symbolp f)
                (setq tag-env (add-tag tag-env f))))
          (cdr expr))
    `(tagbody
        ,@(mapcar (lambda (f)
                    (if (symbolp f)
                        f
                        (mincomp f tag-env)))
                  (cdr expr)))))

(defun mincomp-go (expr env)
  (let ((symbol (cadr expr)))
    (multiple-value-bind (info nonlocal)
        (tag-info env symbol)
      (if nonlocal
          `(nonlocal-go ,symbol)
          `(local-go ,symbol)))))
        
(defun mincomp (expr env)
  (if (atom expr)
      (if (symbolp expr)
          (macroexpand expr env)
          expr)
      (let* ((head (car expr))
             (rest (rest expr))
             (special-operator (gethash head *special-operator-dispatch*))
             (sp-op (first special-operator)))
        (if sp-op
            (funcall sp-op expr env)
            (mincomp-application expr env)))))








(defun mctl (expr)
  (let ((global-env (make-cxx-object 'clcenv:global-environment)))
    (mincomp expr global-env)))

(defmacro test (expr match)
  (let ((ev (gensym)))
    `(let ((,ev (mctl ,expr)))
       (if (equal ,ev ,match)
           (format t "Passed.~%")
           (format t "~a compiles to ~a and this does not match ~a~%" ,expr ,ev ,match)))))

(test '(progn (if 1 2) 3 ) '(progn (if 1 2 nil) 3))
(test '(function (setf foo)) #'(setf foo))
(test '(block foo (return-from foo 1)) '(BLOCK FOO (RETURN-FROM-LOCAL FOO 1)))
(mctl '(block foo (function (lambda (x) "doca" (declare (special x y)) (declare (special z)) (return-from foo 1)))))



cmp::*special-operator-dispatch*
               
      
  
