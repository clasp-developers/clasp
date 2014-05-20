
(defun add_arg (spaces idx)
  (let ((sidx (format nil "~a" idx))
	(sidx-1 (format nil "~a" (- idx 1))))
    (concatenate 'string
		 spaces
		 "    frame->setf_element(" sidx-1 ", translate::to_object<P" sidx ">::convert(arg" sidx "));~%")))

(defun add-all-args (spaces arity )
  (format nil (eval `(concatenate
	  'string ,@(loop for i from 1 to arity
		       collect (add_arg spaces  i))))))


(defun gen-typename (arity)
  (eval `(concatenate 'string ,@(loop for i from 1 to arity collect (format nil (if (= i 1) "typename P~a" ", typename P~a") i)))))
#|
(gen-typename 3)
|#

(defun gen-args (arity)
  (eval `(concatenate 'string ,@(loop for i from 1 to arity collect (format nil (if (= i 1) "P~a arg~a" ", P~a arg~a") i i)))))



(defun generate-declare (sout arity)
  (let ((template (list
		   "#ifdef SYMBOL_APPLY_HEADER~%"
		   "template <" (gen-typename arity) ">~%"
		   "	T_sp funcall(" (gen-args arity) ");~%"
		   "#endif~%"
		   )))
    (format sout (eval `(concatenate 'string ,@template)))))


(defun generate-funcall (sout arity)
  (let ((template (list
;;		   "#ifdef SYMBOL_APPLY_IMPLEMENTATION~%"
		   "template <" (gen-typename arity) ">~%"
		   "	T_sp funcall(" (gen-args arity) ")~%"
		   "    {_G();~%"
		   "	    ValueFrame_sp frame(ValueFrame_O::create(" (format nil "~d" arity) ",ActivationFrame_O::_nil));~%"
		   (add-all-args "        " arity)
		   "	    T_sp result = lisp_apply(this->sharedThis<Symbol_O>(),frame);~%"
		   "	    return( Values(result));~%"
		   "    }~%"
;;		   "#endif ~%"
		   )))
    (format sout (eval `(concatenate 'string ,@template)))))

(generate-funcall t 5)


(with-open-file (sout "symbol_apply.inc" :direction :output :if-exists :supersede)
  (dotimes (i 10)
;;    (generate-declare sout (1+ i))
    (generate-funcall sout (1+ i))
))
