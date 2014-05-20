(bformat t "\n")


;;
;; Setup the llvm-context, module and irbuilder
;;
(llvm-sys:initialize-native-target)
(defparameter *llvm-context* (llvm-sys::get-global-context))
(defparameter *the-module* (llvm-sys:make-module "test" *llvm-context*))
(if (is-undefined *the-module*)
    (error "The LLVM module could not be created"))
(defparameter *irbuilder* (llvm-sys:make-irbuilder *llvm-context*))
(bformat t "*the-module* = %s\n" *the-module*)
(bformat t "*irbuilder* = %s\n" *irbuilder*)




(defparameter float7 (llvm-sys:make-apfloat 7.1))
(defparameter float5 (llvm-sys:make-apfloat 5.2))
(defparameter g7 (llvm-sys:constant-fp-get *llvm-context* float7))
(defparameter g5 (llvm-sys:constant-fp-get *llvm-context* float5))
(bformat t "float7 = %s\n" float7 )
(bformat t "g7 = %s\n" g7)
(bformat t "float5 = %s\n" float5 )
(bformat t "g5 = %s\n" g5)

(defparameter fadd (llvm-sys:create-fadd *irbuilder* g5 g7 "add" nil))
(llvm-sys:dump *the-module*)

;; Define constants for different llvm types
(defconstant +double-ty+ (llvm-sys:type-get-double-ty *llvm-context*))




;;
;; Define functions to ease function creation
(defun function-type-get (return-type arg-types &key var-args)
  (llvm-sys:function-type-get return-type arg-types var-args))

(defun function-create (name function-type)
  (llvm-sys:function-create function-type 'llvm-sys::External-linkage name *the-module*))



(defparameter ft (function-type-get +double-ty+ (list +double-ty+ +double-ty+)))
(defparameter fn (function-create "function-test" ft))


(defparameter args (llvm-sys:get-argument-list fn))
(bformat t "function args = %s\n" args )
(defparameter arg-names '("X" "Y"))
(do ((ai args (cdr ai))
     (arg-name arg-names (cdr arg-names)))
    ((not ai))
  (bformat t "Assigning argument %s name: %s\n" (car ai) (car arg-name))
  (llvm-sys:set-name (car ai) (car arg-name)))

(bformat t "Dumping args-------\n")
(dolist (arg args)
  (llvm-sys:dump arg))

(bformat t "Step L\n")
(defparameter bb (llvm-sys:basic-block-create *llvm-context* "entry" fn))
(bformat t "block = %s\n" bb)
(llvm-sys::set-insert-point *irbuilder* bb)
(llvm-sys::Create-Ret *irbuilder* fadd)
(bformat t "Function fn E verified = %s\n" (llvm-sys::verify-function fn 'llvm-sys:print-message-action))

(llvm-sys::verify-function fn 'llvm-sys:print-message-action)

(llvm-sys::dump *the-module*)


(defparameter *engine-builder* (llvm-sys:make-engine-builder *the-module*))
(defparameter *the-execution-engine* (llvm-sys:create *engine-builder*))

(if (is-undefined *the-execution-engine*)
  (error "The execution engine could not be created ~a" (llvm-sys:error-string *engine-builder*)))

(defparameter *function-pass-manager* (llvm-sys:make-function-pass-manager *the-module*))

(bformat t "*the-execution-engine* = %s\n" *the-execution-engine*)
(let ((target-data (llvm-sys:get-target-data *the-execution-engine*)))
  (bformat t "target-data = %s\n" target-data)
  )
(llvm-sys:function-pass-manager-add *function-pass-manager* (llvm-sys:target-data-copy (llvm-sys:get-target-data *the-execution-engine*)))
(llvm-sys:function-pass-manager-add *function-pass-manager* (llvm-sys:create-basic-alias-analysis-pass))
(llvm-sys:function-pass-manager-add *function-pass-manager* (llvm-sys:create-instruction-combining-pass))
(llvm-sys:function-pass-manager-add *function-pass-manager* (llvm-sys:create-reassociate-pass))
(llvm-sys:function-pass-manager-add *function-pass-manager* (llvm-sys:create-gvnpass nil))
(llvm-sys:function-pass-manager-add *function-pass-manager* (llvm-sys:create-cfgsimplification-pass))
(llvm-sys:do-initialization *function-pass-manager*)

(defparameter *lisp-func* (llvm-sys:get-lisp-function *the-execution-engine* fn))

(bformat t "I think I have a lisp-function\n")


;; Now try to create an external linkage to cos





(bformat t "All functions in module= %s\n" (mapcar #'(lambda (x) (llvm-sys::get-name x))  (llvm-sys::module-get-function-list *the-module*)))

