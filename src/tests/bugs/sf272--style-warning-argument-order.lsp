;; https://sourceforge.net/p/ecls/bugs/272

(compile nil
         `(lambda (x) (1+ (the (values integer string) (funcall x)))))

(deftest sf272--style-warning-argument-order
         (let ((warning nil))
           (assert
             (eq :ok
                 (handler-bind
                   ((style-warning 
                      (lambda (c) 
                        (format t "got style-warning: ~s~%" c)
                        (setf warning c))))
                   (block nil
                          (tagbody
                            (return (multiple-value-bind () (go :fail) :bad))
                            :fail
                            (return :ok))))))
           (assert (not warning))))
