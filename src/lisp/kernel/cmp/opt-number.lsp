(in-package #:cmp)

(core:bclasp-define-compiler-macro min (&whole form &rest args)
  (if (null args) ; invalid
      form
      (let ((arg0 (first args)) (args (rest args)))
        (if (null args)
            `(the t ,arg0) ; preserve nontoplevelness
            (let ((s (gensym)))
              `(let ((,s ,arg0)
                     (minrest (min ,@args)))
                 (if (<= ,s minrest) ,s minrest)))))))
(core:bclasp-define-compiler-macro max (&whole form &rest args)
  (if (null args) ; invalid
      form
      (let ((arg0 (first args)) (args (rest args)))
        (if (null args)
            `(the t ,arg0) ; preserve nontoplevelness
            (let ((s (gensym)))
              `(let ((,s ,arg0)
                     (maxrest (max ,@args)))
                 (if (>= ,s maxrest) ,s maxrest)))))))

(core:bclasp-define-compiler-macro + (&rest numbers)
  (core:expand-associative '+ 'core:two-arg-+ numbers 0))

(core:bclasp-define-compiler-macro * (&rest numbers)
  (core:expand-associative '* 'core:two-arg-* numbers 1))

(core:bclasp-define-compiler-macro - (minuend &rest subtrahends)
  (if (proper-list-p subtrahends)
      (if subtrahends
          `(core:two-arg-- ,minuend ,(core:expand-associative '+ 'core:two-arg-+ subtrahends 0))
          `(core:negate ,minuend))
      (error "The - operator can not be part of a form that is a dotted list.")))

(core:bclasp-define-compiler-macro < (&whole form &rest numbers)
  (core:expand-compare form 'core:two-arg-< numbers))

(core:bclasp-define-compiler-macro <= (&whole form &rest numbers)
  (core:expand-compare form 'core:two-arg-<= numbers))

(core:bclasp-define-compiler-macro > (&whole form &rest numbers)
  (core:expand-compare form 'core:two-arg-> numbers))

(core:bclasp-define-compiler-macro >= (&whole form &rest numbers)
  (core:expand-compare form 'core:two-arg->= numbers))

(core:bclasp-define-compiler-macro = (&whole form &rest numbers)
  (core:expand-compare form 'core:two-arg-= numbers))

(core:bclasp-define-compiler-macro /= (&whole form &rest numbers)
  (core:expand-uncompare form 'core:two-arg-= numbers))

(core:bclasp-define-compiler-macro 1+ (x)
  `(core:two-arg-+ ,x 1))

(core:bclasp-define-compiler-macro 1- (x)
  `(core:two-arg-- ,x 1))
