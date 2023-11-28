(in-package #:unicode-data)

(defparameter +numeric-categories+ '("Nd"))
(defparameter +graphic-categories+
  '("Ll" "Lm" "Lo" "Lt" "Lu" "Mc" "Me" "Mn" "Nd" "Nl" "No" "Pc"
    "Pd" "Pe" "Pf" "Pi" "Po" "Ps" "Sc" "Sk" "Sm" "So" "Zs"))
(defparameter +printing-categories+
  '("Ll" "Lm" "Lo" "Lt" "Lu" "Mc" "Me" "Mn" "Nd" "Nl" "No" "Pc"
    "Pd" "Pe" "Pf" "Pi" "Po" "Ps" "Sc" "Sk" "Sm" "So"))
(defconstant +max-code-value+ #x110000)

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

(defun parse-interval (value)
  (let ((values (mapcar #'parse-hexadecimal (split-sequence:split-sequence #\. value :remove-empty-subseqs t))))
    (make-instance 'interval :lower-bound (first values) :upper-bound (if (cdr values) (second values) (first values)))))

(defun add-interval (from to intervals)
  (when to
    (setf (aref intervals from) 1)))

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

(defun add-interval-range (value intervals)
  (let ((values (mapcar #'parse-hexadecimal (split-sequence:split-sequence #\. value :remove-empty-subseqs t))))
    (cond ((= (length values) 1)
           (setf (aref intervals (first values)) 1))
          ((= (length values) 2)
           (loop for val from (first values) upto (second values)
                 do (setf (aref intervals val) 1))))))

(defun make-intervals (bit-array)
  (loop with interval = nil
        for code-value from 0
        for bit across bit-array
        if (and (zerop bit) interval)
          do (setf (upper-bound interval) (1- code-value)
                   interval nil)
        else if (and (= 1 bit) (null interval))
          collect (setf interval (make-instance 'interval
                                                :lower-bound code-value
                                                :upper-bound (1- (length bit-array))))))

(defun print-interval-function (function-name variable-name bit-array stream)
  (format stream "~%bool ~a(claspCharacter ~a) {~%" function-name variable-name)
  (pprint-logical-block (stream nil :prefix "  return " :suffix ";")
    (loop for interval in (sort (make-intervals bit-array) #'interval-less-p)
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

(defun print-header (prefix stream &rest urls)
  (loop for (url header) on urls by #'cddr
        initially (format stream "~a This file is automatically generated.~%~a command: ninja -C build update-unicode~%"
                          prefix prefix)
        finally (format stream "~a clang-format off~%"
                        prefix)
        do (format stream "~a url: ~a~%~a last modified: ~a~%"
                   prefix url prefix (cdr (assoc :last-modified header)))))

(defun read-row (stream)
  (let* ((line (read-line stream nil))
         (pos (position #\# line :test #'char=)))
    (if line
        (mapcar (lambda (x)
                  (string-trim " " x))
                (split-sequence:split-sequence #\;
                                               (if pos
                                                   (subseq line 0 pos)
                                                   line)))
        stream)))

(defstruct unicode-data
  (uppercase-intervals (make-array +max-code-value+ :element-type 'bit))
  (lowercase-intervals (make-array +max-code-value+ :element-type 'bit))
  (bothcase-intervals (make-array +max-code-value+ :element-type 'bit))
  (alpha-intervals (make-array +max-code-value+ :element-type 'bit))
  (alphanumeric-intervals (make-array +max-code-value+ :element-type 'bit))
  (graphic-intervals (make-array +max-code-value+ :element-type 'bit))
  (printing-intervals (make-array +max-code-value+ :element-type 'bit))
  (names nil)
  (uppercase-mappings (make-array 100 :adjustable t :fill-pointer 0 :element-type 'mapping))
  (lowercase-mappings (make-array 100 :adjustable t :fill-pointer 0 :element-type 'mapping)))         

(defun parse-properties (url data)
  (destructuring-bind (status header stream url)
      (trivial-http:http-resolve url)
    (declare (ignore status resolve))
    (loop for row = (read-row stream)
          until (eq row stream)
          when (equal (second row) "Alphabetic")
            do (add-interval-range (first row) (unicode-data-alpha-intervals data))
               (add-interval-range (first row) (unicode-data-alphanumeric-intervals data)))
    header))

(defun parse-data (url data)
  (destructuring-bind (status header stream url)
      (trivial-http:http-resolve url)
    (declare (ignore status resolve))
    (loop for row = (split-sequence:split-sequence #\; (read-line stream nil))
          for code-value = (parse-hexadecimal (first row))
          for character-name = (substitute #\_ #\Space (or (second row) ""))
          for category = (third row)
          for uppercase-mapping = (parse-hexadecimal (nth 12 row))
          for lowercase-mapping = (parse-hexadecimal (nth 13 row))
          while (and row (> (length row) 13))
          unless (char= (char character-name 0) #\<)
            do (push (cons code-value character-name) (unicode-data-names data))
          do (add-mapping code-value uppercase-mapping (unicode-data-uppercase-mappings data))
             (add-mapping code-value lowercase-mapping (unicode-data-lowercase-mappings data))
             (add-interval code-value (member category +numeric-categories+ :test #'equal) 
                           (unicode-data-alphanumeric-intervals data))
             (add-interval code-value (member category +graphic-categories+ :test #'equal)
                           (unicode-data-graphic-intervals data))
             (add-interval code-value (member category +printing-categories+ :test #'equal)
                           (unicode-data-printing-intervals data))
             (add-interval code-value uppercase-mapping (unicode-data-lowercase-intervals data))
             (add-interval code-value lowercase-mapping (unicode-data-uppercase-intervals data))
             (add-interval code-value (or uppercase-mapping lowercase-mapping) (unicode-data-bothcase-intervals data)))
    header))

(defun generate (c-file names-file &optional (data-url "http://unicode.org/Public/UNIDATA/UnicodeData.txt")
                                             (properties-url "http://unicode.org/Public/UNIDATA/DerivedCoreProperties.txt"))
  (let* ((data (make-unicode-data))
         (properties-header (parse-properties properties-url data))
         (data-header (parse-data data-url data))
         (*print-right-margin* 132))
    (ninja:with-timestamp-preserving-stream (stream c-file)
      (print-header "//" stream data-url data-header properties-url properties-header)
      (print-interval-function "graphic_char_p" "cc" (unicode-data-graphic-intervals data) stream)
      (print-interval-function "printing_char_p" "cc" (unicode-data-printing-intervals data) stream)
      (print-interval-function "upper_case_p" "cc" (unicode-data-uppercase-intervals data) stream)
      (print-interval-function "lower_case_p" "cc" (unicode-data-lowercase-intervals data) stream)
      (print-interval-function "both_case_p" "cc" (unicode-data-bothcase-intervals data) stream)
      (print-interval-function "alpha_char_p" "cc" (unicode-data-alpha-intervals data) stream)
      (print-interval-function "alphanumericp" "cc" (unicode-data-alphanumeric-intervals data) stream)
      (print-mapping-function "char_upcase" "cc" (unicode-data-uppercase-mappings data) stream)
      (print-mapping-function "char_downcase" "cc" (unicode-data-lowercase-mappings data) stream))
    (let ((*print-radix* t)
          (*print-base* 16))
      (ninja:with-timestamp-preserving-stream (stream names-file)
        (print-header ";;;;" stream data-url data-header properties-url properties-header)
        (pprint-linear stream (sort (unicode-data-names data) (lambda (x y) (< (car x) (car y)))) t)))))
