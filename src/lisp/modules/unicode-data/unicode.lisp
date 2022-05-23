(in-package #:unicode-data)

(defclass interval ()
  ((lower-bound :accessor lower-bound
                :initarg :lower-bound
                :type integer)
   (upper-bound :accessor upper-bound
                :initarg :upper-bound
                :type integer)))

(defun make-interval (value)
  (make-instance 'interval :lower-bound value :upper-bound value))

(defun lower-adjacent-p (interval value)
  (= (1- (lower-bound interval)) value))

(defun upper-adjacent-p (interval value)
  (= (1+ (upper-bound interval)) value))

(defun adjacent-p (interval value)
  (or (lower-adjacent-p interval value)
      (upper-adjacent-p interval value)))

(defun width (interval)
  (- (upper-bound interval) (lower-bound interval)))

(defun extend-interval (object value)
  (with-accessors ((lower-bound lower-bound)
                   (upper-bound upper-bound))
      object
    (setf lower-bound (min value lower-bound)
          upper-bound (max value upper-bound))))

(defun print-interval (object name stream)
  (with-accessors ((lower-bound lower-bound)
                   (upper-bound upper-bound))
      object
    (cond ((= lower-bound upper-bound)
           (format stream "~a == 0x~x" name lower-bound))
          ((= (1+ lower-bound) upper-bound)
           (format stream "~a == 0x~x || " name lower-bound)
           (pprint-newline :fill stream)
           (format stream "~a == 0x~x" name upper-bound))
          (t
           (format stream "(~a >= 0x~x && ~a <= 0x~x)" name lower-bound name upper-bound)))))

(defclass mapping ()
  ((from :accessor from
         :initarg :from
         :type interval)
   (to :accessor to
       :initarg :to
       :type interval)))

(defun make-mapping (from to)
  (make-instance 'mapping :from (make-interval from) :to (make-interval to)))

(defun print-mapping (object name stream)
  (with-accessors ((from from)
                   (to to))
      object
    (case (width from)
      (0
       (format stream "    case 0x~x: return 0x~x;~%"
         (lower-bound from) (lower-bound to)))
      (1
       (format stream "    case 0x~x: return 0x~x;~%    case 0x~x: return 0x~x;~%"
         (lower-bound from) (lower-bound to) (upper-bound from) (upper-bound to)))
      (otherwise
       (format stream "  if ~a {~%    return ~a ~:[-~;+~] 0x~x;~%  }~%"
              (print-interval from name nil) name
              (> (lower-bound to) (lower-bound from))
              (abs (- (lower-bound to) (lower-bound from))))))))

(defun parse-hexadecimal (value)
  (when (and (stringp value)
             (plusp (length value)))
    (parse-integer value :radix 16)))

(defun add-interval (from to intervals)
  (when to
    (let ((interval (find-if (lambda (interval)
                               (adjacent-p interval from))
                             intervals)))
      (if interval
          (extend-interval interval from)
          (vector-push-extend (make-interval from) intervals)))))

(defun add-mapping (from to mappings)
  (when to
    (let ((mapping (find-if (lambda (mapping)
                              (or (and (lower-adjacent-p (from mapping) from)
                                       (lower-adjacent-p (to mapping) to))
                                  (and (upper-adjacent-p (from mapping) from)
                                       (upper-adjacent-p (to mapping) to))))
                            mappings)))
      (cond (mapping
             (extend-interval (from mapping) from)
             (extend-interval (to mapping) to))
            (t
             (vector-push-extend (make-mapping from to) mappings))))))

(defun print-interval-function (function-name variable-name intervals stream)
  (format stream "~%bool ~a(claspCharacter ~a) {~%" function-name variable-name)
  (pprint-logical-block (stream nil :prefix "  return " :suffix ";")
    (loop for interval across (sort intervals #'interval-less-p)
          for previous = nil then t
          when previous
            do (format stream " || ")
               (pprint-newline :fill stream)
          do (print-interval interval variable-name stream)))
  (format stream "~%}~%"))

(defun print-mapping-function (function-name variable-name mappings stream)
  (sort mappings #'mapping-less-p)
  (format stream "~%claspCharacter ~a(claspCharacter ~a) {~%" function-name variable-name)
  (loop for mapping across mappings
        when (> (width (from mapping)) 1)
        do (print-mapping mapping variable-name stream))
  (format stream "  switch (~a) {~%" variable-name)
  (loop for mapping across mappings
        when (< (width (from mapping)) 2)
        do (print-mapping mapping variable-name stream))
  (format stream "    default: return ~a;~%  }~%}~%" variable-name))

(defun interval-less-p (x y)
  (or (> (width x) (width y))
      (and (= (width x) (width y))
           (< (lower-bound x) (lower-bound y)))))

(defun mapping-less-p (x y)
  (interval-less-p (from x) (from y)))

(defun print-header (prefix url header stream)
  (format stream "~a This file is automatically generated.~%~a command: ninja -C build update-unicode~%~a url: ~a~%~a last modified: ~a~%"
          prefix prefix prefix url prefix (cdr (assoc :last-modified header))))

(defun parse (url)
  (destructuring-bind (status header stream url)
      (trivial-http:http-resolve url)
    (declare (ignore status resolve))
    (loop with uppercase-intervals = (make-array 100 :adjustable t :fill-pointer 0 :element-type 'interval)
          with lowercase-intervals = (make-array 100 :adjustable t :fill-pointer 0 :element-type 'interval)
          with bothcase-intervals = (make-array 100 :adjustable t :fill-pointer 0 :element-type 'interval)
          with names = nil
          with uppercase-mappings = (make-array 100 :adjustable t :fill-pointer 0 :element-type 'mapping)
          with lowercase-mappings = (make-array 100 :adjustable t :fill-pointer 0 :element-type 'mapping)
          for row = (split-sequence:split-sequence #\; (read-line stream nil))
          for code-value = (parse-hexadecimal (first row))
          for character-name = (substitute #\_ #\Space (or (second row) ""))
          for uppercase-mapping = (parse-hexadecimal (nth 12 row))
          for lowercase-mapping = (parse-hexadecimal (nth 13 row))
          while (and row (> (length row) 13))
          unless (char= (char character-name 0) #\<)
            do (push (cons code-value character-name) names)
          do (add-mapping code-value uppercase-mapping uppercase-mappings)
             (add-mapping code-value lowercase-mapping lowercase-mappings)
             (add-interval code-value uppercase-mapping lowercase-intervals)
             (add-interval code-value lowercase-mapping uppercase-intervals)
             (add-interval code-value (or uppercase-mapping lowercase-mapping) bothcase-intervals)
          finally (return (values header uppercase-intervals lowercase-intervals bothcase-intervals
                                  names uppercase-mappings lowercase-mappings)))))

(defun generate (c-file names-file &optional (url "http://unicode.org/Public/UNIDATA/UnicodeData.txt"))
  (multiple-value-bind (header uppercase-intervals lowercase-intervals bothcase-intervals
                        names uppercase-mappings lowercase-mappings)
      (parse url)
    (let ((*print-right-margin* 132))
      (ninja:with-timestamp-preserving-stream (stream c-file)
        (print-header "//" url header stream)
        (print-interval-function "clasp_isupper" "cc" uppercase-intervals stream)
        (print-interval-function "clasp_islower" "cc" lowercase-intervals stream)
        (print-interval-function "clasp_isboth" "cc" bothcase-intervals stream)
        (print-mapping-function "clasp_toupper" "cc" uppercase-mappings stream)
        (print-mapping-function "clasp_tolower" "cc" lowercase-mappings stream))
      (let ((*print-radix* t)
            (*print-base* 16))
        (ninja:with-timestamp-preserving-stream (stream names-file)
          (print-header ";;;;" url header stream)
          (pprint-linear stream (sort names (lambda (x y) (< (car x) (car y)))) t))))))
