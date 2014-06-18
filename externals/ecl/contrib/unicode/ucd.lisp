;;; Common

(defvar *extension-directory*
  (make-pathname :directory (pathname-directory *load-truename*)))

(defparameter *unicode-char-limit* #x110000)
(defparameter *page-size-exponent* 8)
(defparameter *page-size* (ash 1 *page-size-exponent*))

(defun total-ucd-pages ()
  (floor *unicode-char-limit* *page-size*))

(defun cp-high (cp)
  (ash cp (- *page-size-exponent*)))

(defun cp-low (cp)
  (ldb (byte *page-size-exponent* 0) cp))

;;; Generator

(defstruct ucd misc transform)

(defparameter *unicode-character-database*
  (make-pathname :directory (pathname-directory *load-truename*)))

(defparameter *ucd-base* nil)
(defparameter *unicode-names* (make-hash-table))

(defparameter *last-uppercase* nil)
(defparameter *uppercase-transition-count* 0)
(defparameter *different-titlecases* nil)
(defparameter *different-numerics* nil)
(defparameter *name-size* 0)
(defparameter *misc-classless* 0)
(defparameter *misc-hash* (make-hash-table :test #'equal))
(defparameter *misc-index* -1)
(defparameter *misc-table* nil)
(defparameter *misc-mapping* nil)
(defparameter *both-cases* nil)
(defparameter *decompositions* nil)
(defparameter *decomposition-length-max* nil)
(defparameter *decomposition-types* nil)
(defparameter *decomposition-base* nil)

(defun hash-misc (gc-index bidi-index ccc-index decimal-digit digit
                  bidi-mirrored cl-both-case-p)
  (let* ((list (list gc-index bidi-index ccc-index decimal-digit digit
                     bidi-mirrored cl-both-case-p))
         (index (gethash list *misc-hash*)))
    (or index
        (progn
          (vector-push list *misc-table*)
          (setf (gethash list *misc-hash*)
                (incf *misc-index*))))))

(defun compare-misc-entry (left right)
  (destructuring-bind (left-gc-index left-bidi-index left-ccc-index
                       left-decimal-digit left-digit left-bidi-mirrored
                       left-cl-both-case-p)
      left
    (destructuring-bind (right-gc-index right-bidi-index right-ccc-index
                         right-decimal-digit right-digit right-bidi-mirrored
                         right-cl-both-case-p)
        right
      (or (and left-cl-both-case-p (not right-cl-both-case-p))
          (and (or left-cl-both-case-p (not right-cl-both-case-p))
               (or (< left-gc-index right-gc-index)
                   (and (= left-gc-index right-gc-index)
                        (or (< left-bidi-index right-bidi-index)
                            (and (= left-bidi-index right-bidi-index)
                                 (or (< left-ccc-index right-ccc-index)
                                     (and (= left-ccc-index right-ccc-index)
                                          (or (string< left-decimal-digit
                                                       right-decimal-digit)
                                              (and (string= left-decimal-digit
                                                            right-decimal-digit)
                                                   (or (string< left-digit right-digit)
                                                       (and (string= left-digit
                                                                     right-digit)
                                                            (string< left-bidi-mirrored
                                                                     right-bidi-mirrored))))))))))))))))

(defun build-misc-table ()
  (sort *misc-table* #'compare-misc-entry)
  (setq *misc-mapping* (make-array (1+ *misc-index*)))
  (loop for i from 0 to *misc-index*
        do (setf (aref *misc-mapping*
                       (gethash (aref *misc-table* i) *misc-hash*))
                 i)))

(defun slurp-ucd ()
  (setq *last-uppercase* nil)
  (setq *uppercase-transition-count* 0)
  (setq *different-titlecases* nil)
  (setq *different-numerics* nil)
  (setq *name-size* 0)
  (setq *misc-hash* (make-hash-table :test #'equal))
  (setq *misc-index* -1)
  (setq *misc-table* (make-array *page-size* :fill-pointer 0))
  (setq *both-cases* nil)
  (setq *decompositions* 0)
  (setq *decomposition-types* (make-hash-table :test #'equal))
  (setq *decomposition-length-max* 0)
  (setq *decomposition-base* (make-array (total-ucd-pages) :initial-element nil))
  (setq *ucd-base* (make-array (total-ucd-pages) :initial-element nil))
  (with-open-file (*standard-input*
		   (make-pathname :name "UnicodeData" :type "txt"
				  :defaults *extension-directory*)
                   :direction :input :external-format :default)
    (loop for line = (read-line nil nil)
          while line
          do (slurp-ucd-line line)))
  (setf *misc-classless* (hash-misc 0 0 0 "" "" "N" nil))
  (second-pass)
  (build-misc-table)
  *decompositions*)

(defun split-string (line character)
  (loop for prev-position = 0 then (1+ position)
        for position = (position character line :start prev-position)
        collect (subseq line prev-position position)
        do (unless position
             (loop-finish))))

(defun init-indices (strings)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for string in strings
          for index from 0
          do (setf (gethash string hash) index))
    hash))

(defparameter *general-categories*
  (init-indices '("Lu" "Ll" "Lt" "Lm" "Lo" "Cc" "Cf" "Co" "Cs" "Mc"
                  "Me" "Mn" "Nd" "Nl" "No" "Pc" "Pd" "Pe" "Pf" "Pi"
                  "Po" "Ps" "Sc" "Sk" "Sm" "So" "Zl" "Zp" "Zs")))
(defparameter *bidi-classes*
  (init-indices '("AL" "AN" "B" "BN" "CS" "EN" "ES" "ET" "L" "LRE" "LRO"
                  "NSM" "ON" "PDF" "R" "RLE" "RLO" "S" "WS")))


(defparameter *block-first* nil)

(defun normalize-character-name (name)
  (when (find #\_ name)
    (error "Bad name for a character: ~A" name))
  (unless (or (zerop (length name)) (find #\< name) (find #\> name))
    (substitute #\_ #\Space name)))

;;;   3400  --  4DB5  : cjk ideograph extension a ;Lo;0;L;;;;;N;;;;;
;;;   AC00  --  D7A3  : hangul syllables ;Lo;0;L;;;;;N;;;;;
;;;   D800  --  F8FF  : surrogates and private use
;;;  20000  --  2A6D6 : cjk ideograph extension b ;Lo;0;L;;;;;N;;;;;
;;;  F0000  --  FFFFD : private use
;;; 100000  --  10FFFD: private use
(defun encode-ucd-line (line code-point)
  (destructuring-bind (name general-category canonical-combining-class
                            bidi-class decomposition-type-and-mapping
                            decimal-digit digit numeric bidi-mirrored
                            unicode-1-name iso-10646-comment simple-uppercase
                            simple-lowercase simple-titlecase)
      line
    (declare (ignore unicode-1-name iso-10646-comment))
    (if (and (> (length name) 8)
             (string= ", First>" name :start2 (- (length name) 8)))
        (progn
          (setq *block-first* code-point)
          nil)
        (let* ((gc-index (or (gethash general-category *general-categories*)
                             (error "unknown general category ~A"
                                    general-category)))
               (bidi-index (or (gethash bidi-class *bidi-classes*)
                               (error "unknown bidirectional class ~A"
                                      bidi-class)))
               (ccc-index (parse-integer canonical-combining-class))
               (digit-index (unless (string= "" decimal-digit)
                              (parse-integer decimal-digit)))
               (upper-index (unless (string= "" simple-uppercase)
                              (parse-integer simple-uppercase :radix 16)))
               (lower-index (unless (string= "" simple-lowercase)
                              (parse-integer simple-lowercase :radix 16)))
               (title-index (unless (string= "" simple-titlecase)
                              (parse-integer simple-titlecase :radix 16)))
               (cl-both-case-p
                (not (null (or (and (= gc-index 0) lower-index)
                               (and (= gc-index 1) upper-index)))))
               (misc-index (hash-misc gc-index bidi-index ccc-index
                                      decimal-digit digit bidi-mirrored
                                      cl-both-case-p)))
          (declare (ignore digit-index))
          (incf *name-size* (length name))
          (when (string/= "" decomposition-type-and-mapping)
            (let ((split (split-string decomposition-type-and-mapping
                                       #\Space)))
              (when (char= #\< (aref (first split) 0))
                (setf (gethash (pop split) *decomposition-types*) t))
              (unless (aref *decomposition-base* (cp-high code-point))
                (setf (aref *decomposition-base* (cp-high code-point))
                      (make-array (ash 1 *page-size-exponent*)
                                  :initial-element nil)))
              (setf (aref (aref *decomposition-base* (cp-high code-point))
                          (cp-low code-point))
                    (mapcar #'(lambda (string)
                                (parse-integer string :radix 16))
                            split))
              (setq *decomposition-length-max*
                    (max *decomposition-length-max* (length split)))
              (incf *decompositions* (length split))))
          (when (and (string/= "" simple-uppercase)
                     (string/= "" simple-lowercase))
            (push (list code-point upper-index lower-index) *both-cases*))
          (when (string/= simple-uppercase simple-titlecase)
            (push (cons code-point title-index) *different-titlecases*))
          (when (string/= digit numeric)
            (push (cons code-point numeric) *different-numerics*))
          (cond
            ((= gc-index 8)
             (unless *last-uppercase*
               (incf *uppercase-transition-count*))
             (setq *last-uppercase* t))
            (t
             (when *last-uppercase*
               (incf *uppercase-transition-count*))
             (setq *last-uppercase* nil)))
          (when (> ccc-index 255)
            (error "canonical combining class too large ~A" ccc-index))
          (let ((result (make-ucd :misc misc-index
                                  :transform (or upper-index lower-index 0))))
            (when (and (> (length name) 7)
                       (string= ", Last>" name :start2 (- (length name) 7)))
              (let ((page-start (ash (+ *block-first*
                                        (ash 1 *page-size-exponent*)
                                        -1)
                                     (- *page-size-exponent*)))
                    (page-end (ash code-point (- *page-size-exponent*))))
                (loop for point from *block-first*
                      below (ash page-start *page-size-exponent*)
                      do (setf (aref (aref *ucd-base* (cp-high point))
                                     (cp-low point))
                               result))
                (loop for page from page-start below page-end
                      do (setf (aref *ucd-base* page)
                               (make-array (ash 1 *page-size-exponent*)
                                           :initial-element result)))
                (loop for point from (ash page-end *page-size-exponent*)
                      below code-point
                      do (setf (aref (aref *ucd-base* (cp-high point))
                                     (cp-low point))
                               result))))
            (values result (normalize-character-name name)))))))

(defun slurp-ucd-line (line)
  (let* ((split-line (split-string line #\;))
         (code-point (parse-integer (first split-line) :radix 16))
         (code-high (ash code-point (- *page-size-exponent*)))
         (code-low (ldb (byte *page-size-exponent* 0) code-point)))
    (unless (aref *ucd-base* code-high)
      (setf (aref *ucd-base* code-high)
            (make-array (ash 1 *page-size-exponent*)
                        :initial-element nil)))
    (multiple-value-bind (encoding name)
        (encode-ucd-line (cdr split-line) code-point)
      (setf (aref (aref *ucd-base* code-high) code-low) encoding
            (gethash code-point *unicode-names*) name))))

(defun second-pass ()
  (loop for i from 0 below (length *ucd-base*)
        when (aref *ucd-base* i)
        do (loop for j from 0 below (length (aref *ucd-base* i))
                 for result = (aref (aref *ucd-base* i) j)
                 when result
                 when (let* ((transform-point (ucd-transform result))
                             (transform-high (ash transform-point
                                                  (- *page-size-exponent*)))
                             (transform-low (ldb (byte *page-size-exponent* 0)
                                                 transform-point)))
                        (and (plusp transform-point)
                             (/= (ucd-transform
                                  (aref (aref *ucd-base* transform-high)
                                        transform-low))
                                 (+ (ash i *page-size-exponent*) j))))
                 do (destructuring-bind (gc-index bidi-index ccc-index
                                         decimal-digit digit bidi-mirrored
                                         cl-both-case-p)
                        (aref *misc-table* (ucd-misc result))
                      (declare (ignore cl-both-case-p))
                      (format t "~A~%" (+ (ash i *page-size-exponent*) j))
                      (setf (ucd-misc result)
                            (hash-misc gc-index bidi-index ccc-index
                                       decimal-digit digit bidi-mirrored
                                       nil))))))

(defun write-3-byte (triplet stream)
  (write-2-byte triplet stream)
  (write-byte (ldb (byte 8 16) triplet) stream))

(defun write-2-byte (doublet stream)
  (write-byte (ldb (byte 8 0) doublet) stream)
  (write-byte (ldb (byte 8 8) doublet) stream))

(defun digit-to-byte (digit)
  (if (string= "" digit)
      255
      (parse-integer digit)))

(defun output (&optional small-unicode)
  (let* ((num-pages (/ (if small-unicode #x10000 *unicode-char-limit*)
                       *page-size*))
         (ucd-file-name (concatenate 'base-string "ucd" (if small-unicode "16" "")))
         (hash (make-hash-table :test #'equalp))
         (index 0))
    (loop for page across *ucd-base*
       for i from 0 below num-pages
       do (when page
            (unless (gethash page hash)
              (setf (gethash page hash)
                    (incf index)))))
    (let ((array (make-array (1+ index))))
      (maphash #'(lambda (key value)
                   (setf (aref array value) key))
               hash)
      (setf (aref array 0)
            (make-array (ash 1 *page-size-exponent*) :initial-element nil))
      (with-open-file (stream (make-pathname :name ucd-file-name
                                             :type "dat"
                                             :defaults *extension-directory*)
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede
                              :if-does-not-exist :create)
	(let ((offset (* (length *misc-table*) 8)))
	  (write-byte (mod offset *page-size*) stream)
	  (write-byte (floor offset *page-size*) stream))
        (loop for (gc-index bidi-index ccc-index decimal-digit digit
                            bidi-mirrored)
              across *misc-table*
              do (write-byte gc-index stream)
              do (write-byte bidi-index stream)
              do (write-byte ccc-index stream)
              do (write-byte (digit-to-byte decimal-digit) stream)
              do (write-byte (digit-to-byte digit) stream)
              do (write-byte (if (string= "N" bidi-mirrored) 0 1) stream)
              do (write-byte 0 stream)
              do (write-byte 0 stream))
        (print (length *misc-table*))
        (loop for page across *ucd-base*
           for i from 0 below num-pages
           do (write-byte (if page (gethash page hash) 0) stream))
        (loop for page across array
           for i from 0 below num-pages
           do (loop for entry across page
                 do (write-byte (if entry
                                    (aref *misc-mapping* (ucd-misc entry))
                                    *misc-classless*)
                                stream)
                 do (funcall (if small-unicode 'write-2-byte 'write-3-byte)
                             (if entry (ucd-transform entry) 0)
                             stream))))))
  #+(or)
  (with-open-file (f (make-pathname :name "ucd-names" :type "lisp-expr"
                                    :defaults *extension-directory*)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (with-standard-io-syntax
      (write-string ";;; Do not edit by hand: generated by ucd.lisp" f)
      (maphash (lambda (code name)
                 (when name
                  (print code f)
                  (prin1 name f)))
               *unicode-names*))
    (setf *unicode-names* nil))
  (with-open-file (*standard-output*
                   (make-pathname :name "numerics"
                                  :type "lisp-expr"
                                  :defaults *extension-directory*)
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (let ((*print-pretty* t))
      (prin1 (mapcar #'(lambda (x) (cons (car x) (read-from-string (cdr x))))
                     *different-numerics*))))
  (with-open-file (*standard-output*
                   (make-pathname :name "titlecases"
                                  :type "lisp-expr"
                                  :defaults *extension-directory*)
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (let ((*print-pretty* t))
      (prin1 *different-titlecases*)))
  (with-open-file (*standard-output*
                   (make-pathname :name "misc"
                                  :type "lisp-expr"
                                  :defaults *extension-directory*)
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (let ((*print-pretty* t))
      (prin1 `(:length ,(length *misc-table*)
               :uppercase ,(loop for (gc-index) across *misc-table*
                                 for i from 0
                                 when (= gc-index 0)
                                 collect i)
               :lowercase ,(loop for (gc-index) across *misc-table*
                                 for i from 0
                                 when (= gc-index 1)
                                 collect i)
               :titlecase ,(loop for (gc-index) across *misc-table*
                                 for i from 0
                                 when (= gc-index 2)
                                 collect i)))))
  (values))

(defmacro with-c-file ((stream-var name) &rest body)
  `(with-open-file (,stream-var ,name
                                :direction :output
                                :external-format #-unicode :default #+unicode :us-ascii
                                :if-exists :supersede
                                :if-does-not-exist :create)
     ,@body))

(defun output-c (&optional small-unicode)
  (let* ((num-pages (/ (if small-unicode #x10000 *unicode-char-limit*)
                       *page-size*))
         (ucd-file-name (concatenate 'base-string "ucd" (if small-unicode "16" "")))
         (hash (make-hash-table :test #'equalp))
         (index 0)
         array)
    (with-c-file (stream (make-pathname :name ucd-file-name
                                        :type "c"
                                        :defaults *extension-directory*))
      (format stream "~%extern const char ecl_ucd_page_table_0[];")
      (loop for page across *ucd-base*
         for i from 0 below num-pages
         do (when page
              (unless (gethash page hash)
                (setf (gethash page hash) (incf index))
                (print index)
                (format stream "~%extern const char ecl_ucd_page_table_~D[];" index))))
      (setf array (make-array (incf index)))
      (maphash #'(lambda (key value)
                   (setf (aref array value) key))
               hash)
      (setf (aref array 0)
            (make-array (ash 1 *page-size-exponent*) :initial-element nil))
      (format stream "~%~%const unsigned char ecl_ucd_misc_table[~D] = {"
              (* 8 (length *misc-table*)))
      (loop with comma = ""
         for (gc-index bidi-index ccc-index decimal-digit digit
                       bidi-mirrored)
         across *misc-table*
         do (format stream "~%~A~D, ~D, ~D, ~D, ~D, ~D, ~D, ~D"
                    comma
                    gc-index bidi-index ccc-index
                    (digit-to-byte decimal-digit)
                    (digit-to-byte digit)
                    (if (string= "N" bidi-mirrored) 0 1)
                    0
                    0)
         do (setf comma ","))
      (print *misc-table*)
      (print (length *misc-table*))
      (format stream "~%};")
      (format stream "~%~%const unsigned char *const ecl_ucd_page_table[~D] = {"
              num-pages)
      (loop with comma = ""
         for page across *ucd-base*
         for i from 0 below num-pages
         for name = (if page
                        (format nil "ecl_ucd_page_table_~D" (gethash page hash))
                        ;; fixme, this was so previously
                        "ecl_ucd_page_table_0")
         do (format stream "~%~A~A"
                    comma
                    name)
         do (setf comma ","))
      (format stream "~%};"))
    (print index)
    (loop for i from 0 below index by 16
       for next = (min index (+ i 16))
       for c-file = (format nil "~A-~4,'0D" ucd-file-name i)
       do (with-c-file (stream (make-pathname :name c-file
                                              :type "c"
                                              :defaults *extension-directory*))
            (loop for j from i below next
               for page = (aref array j)
               do (format stream "~%const unsigned char ecl_ucd_page_table_~D[] = {" j)
               do (loop with comma = ""
                     for entry across page
                     for other-case = (if entry (ucd-transform entry) 0)
                     do (format stream
                                (if small-unicode
                                    "~%~A~D,~D,~D"
                                    "~%~A~D,~D,~D,~D")
                                comma
                                (if entry
                                    (aref *misc-mapping* (ucd-misc entry))
                                    *misc-classless*)
                                (ldb (byte 8 0) other-case)
                                (ldb (byte 8 8) other-case)
                                (ldb (byte 8 16) other-case))
                     do (setf comma ","))
               do (format stream "~%};")))))
  (values))

(defun read-compiled-ucd ()
  (with-open-file (stream (make-pathname :name *ucd-file-name*
                                         :type "dat"
                                         :defaults *extension-directory*)
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((length (file-length stream)))
      (setq *compiled-ucd*
            (make-array length :element-type '(unsigned-byte 8)))
      (read-sequence *compiled-ucd* stream)))
  (values))

#|
(slurp-ucd)
(output)
(output t)
|#
