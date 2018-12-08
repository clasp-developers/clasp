(in-package :cscrape)

(defun generate-maybe-namespace-type (namespace type)
  (cond
    ((and (or (search "_mv" type) (search "_sp" type)) (not (search "::" type)))
     (format nil "~a::~a" namespace type))
    ((search "Nilable<" type)
     (let* ((nilable-pos (search "Nilable<" type))
            (nilable-end (+ nilable-pos (length "Nilable<")))
            (head (subseq type 0 nilable-end))
            (rest (subseq type nilable-end)))
       (if (not (search "::" rest))
           (concatenate 'string head namespace "::" rest)
           type)))
    (t type)))

(eval-when (:compile-toplevel :load-toplevel)

;;; Modified from stassats: https://github.com/stassats/inline-js
(defun parse-c++ (stream arg char)
  (declare (optimize (speed 3)))
  (declare (ignore arg char))
  (let* (args
         previous-1
         (string
           (with-output-to-string (result)
             (loop with quote
                for char = (read-char stream nil nil t)
                do
                  (case char
                    (#\#
                     (cond
                       ((eql previous-1 #\l)
                        (return))
                       (t (write-char char result))))
                    (#\$
                     (push (read stream nil nil t) args)
                            (write-string "~a" result))
                    (#\~
                     (write-string "~~" result))
                    (#\\
                     (write-char (if quote
                                     (read-char stream nil nil t)
                                     char)
                                 result))
                    ((#\' #\")
                     (write-char char result)
                     (cond ((eql quote char)
                            (setf quote nil))
                           ((not quote)
                            (setf quote char))))
                    (t
                     (shiftf previous-1 char)
                     (write-char char result)))))))
    `(format nil ,(string-trim #(#\Space #\Newline)
                               (subseq string 0 (- (length string) 1)))
             ,@(nreverse args))))

;; TODO FIXME it's not nice to install this globally
(set-dispatch-macro-character #\# #\l
                              #'parse-c++)
) ; eval-when

;;
;; NOTE: some of the semicolons below are part of C, not Lisp!
;;
(defun generate-return-value (namespace function-name return-type arg-indexes)
  (cond
    ((string= return-type "void")
     #l
     $namespace ::$function-name($(format nil "~{a~a._v~^,~}" arg-indexes));
     return Values0<core::T_O>();
     l#)
    ((search "_mv" return-type) ;(or (string= return-type "T_mv") (string= return-type "core::T_mv"))
     #l
     auto ret = $namespace ::$function-name($(format nil "~{a~a._v~^,~}" arg-indexes));
     return ret.as_return_type();
     l#)
    (t
     #l
     $(generate-maybe-namespace-type namespace return-type) ret = $namespace ::$function-name($(format nil "~{a~a._v~^,~}" arg-indexes));
     return translate::to_object<$(generate-maybe-namespace-type namespace return-type)>::convert(ret).as_return_type();
     l#)))

(defun generate-wrapped-function (wrapped-name namespace function-name return-type types &key extern)
  (let* ((nargs (length types))
         (arg-indexes (loop for x below nargs collect x))
         (extern-attribute (if extern "extern " "")))
    (when extern
        (setf wrapped-name (format nil "extern_~a" wrapped-name)))
    #l
    $extern-attribute LCC_RETURN $wrapped-name ($(format nil "~{core::T_O* in~a~^,~}" arg-indexes)) {
    $(with-output-to-string (sout)
       (loop
         :for x in arg-indexes
         :for type in types
         :do (format sout
                     #l
                     translate::from_object<$(generate-maybe-namespace-type namespace type)> a$x (core::T_sp((gc::Tagged)(in$x)));
                     $""
                     l#)))
    $(generate-return-value namespace function-name return-type arg-indexes)
    }
    l#))
