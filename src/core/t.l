

(define-condition too-few-arguments-error (error)
  ((called-function :initarg :called-function :reader called-function)
   (given-number-of-arguments :initarg :given-number-of-arguments :reader given-number-of-arguments)
   (required-number-of-arguments :initarg :required-number-of-arguments :reader required-number-of-arguments))
  (:report (lambda (condition stream)
             (format stream "Too few arguments for ~S, given ~S - required ~S."
                     (called-function condition) (given-number-of-arguments condition) (required-number-of-arguments condition)))))



(defparameter *c* (make-condition 'too-few-arguments-error :called-function nil :required-number-of-arguments 10))

(required-number-of-arguments *c*)


