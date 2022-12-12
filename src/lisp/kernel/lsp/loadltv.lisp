(defpackage #:loadltv
  (:use #:cl)
  (:export #:load-bytecode))

(in-package #:loadltv)

(defparameter +ops+
  '((nil 65 sind)
    (t 66 sind)
    (ratio 67) ; TODO
    (complex 68) ; TODO
    (cons 69 sind)
    (rplaca 70 ind1 ind2) ; (setf (car [ind1]) [ind2])
    (rplacd 71 ind1 ind2)
    (make-array 74 sind rank . dims)
    (setf-row-major-aref 75 arrayind rmindex valueind)
    (make-hash-table 76 sind test count)
    (setf-gethash 77 htind keyind valueind)
    (make-sb64 78 sind sb64)
    (find-package 79 sind nameind)
    (make-bignum 80 sind size . words) ; size is signed
    (intern 82 sind packageind nameind) ; make-symbol
    (make-character 83 sind ub32) ; ub64 in clasp, i think?
    (make-pathname 85) ; TODO
    (funcall-create 93 sind fnind)
    (funcall-initialize 94 fnind)
    ;; set-ltv-funcall in clasp- redundant
    (make-specialized-array 97 sind rank dims etype . elems)))

;;; Read an unsigned n-byte integer from a ub8 stream, big-endian.
(defun read-ub (n stream)
  ;; read-sequence might be better but bla bla consing
  (loop with int = 0
        repeat n
        do (setf int (logior (ash int 8) (read-byte stream)))
        finally (return int)))

(defun read-ub64 (stream) (read-ub 8 stream))
(defun read-ub32 (stream) (read-ub 4 stream))
(defun read-ub16 (stream) (read-ub 2 stream))

(defconstant +magic+ #x8d7498b1) ; randomly chosen bytes.

(defun load-magic (stream)
  (let ((magic (read-ub32 stream)))
    (unless (= magic +magic+)
      (error "~s is not a valid bytecode FASL: invalid magic identifier ~d"
             stream magic))))

;; Bounds for major and minor version understood by this loader.
;; It might be smarter for reverse compatibility to make the version
;; look up a loader? This will become more obvious once there are actually
;; multiple versions in existence.
(defparameter *min-version* '(0 0))
(defparameter *max-version* '(0 0))

(defun loadable-version-p (major minor)
  (and
   ;; minimum
   (if (= major (first *min-version*))
       (>= minor (second *min-version*))
       (> major (first *min-version*)))
   ;; maximum
   (if (= major (first *max-version*))
       (<= minor (second *max-version*))
       (< major (first *max-version*)))))

(defun load-version (stream)
  (let ((major (read-ub16 stream)) (minor (read-ub16 stream)))
    (unless (loadable-version-p major minor)
      (error "Don't know how to load bytecode FASL format version ~d.~d
(This loader only understands ~d.~d to ~d.~d)"
             major minor (first *min-version*) (second *min-version*)
             (first *max-version*) (second *max-version*)))
    (values major minor)))

;; how many bytes are needed to represent an index?
(defvar *index-bytes*)

(defun read-index (stream)
  (ecase *index-bytes*
    ((1) (read-byte stream))
    ((2) (read-ub16 stream))
    ((4) (read-ub32 stream))
    ((8) (read-ub64 stream))))

(defun read-mnemonic (stream)
  (let* ((opcode (read-byte stream))
         (info (find opcode +ops+ :key #'second)))
    (if info
        (first info)
        (error "BUG: Unknown opcode ~x" opcode))))

(defgeneric %load-instruction (mnemonic constants stream))

(defmethod %load-instruction ((mnemonic (eql 'nil)) constants stream)
  (setf (aref constants (read-index stream)) nil))

(defmethod %load-instruction ((mnemonic (eql 't)) constants stream)
  (setf (aref constants (read-index stream)) t))

(defmethod %load-instruction ((mnemonic (eql 'cons)) constants stream)
  (setf (aref constants (read-index stream)) (cons nil nil)))

(defmethod %load-instruction ((mnemonic (eql 'rplaca)) constants stream)
  (setf (car (aref constants (read-index stream)))
        (aref constants (read-index stream))))

(defmethod %load-instruction ((mnemonic (eql 'rplacd)) constants stream)
  (setf (car (aref constants (read-index stream)))
        (aref constants (read-index stream))))

(defmethod %load-instruction ((mnemonic (eql 'make-array)) constants stream)
  (let* ((index (read-index stream))
         (rank (read-byte stream))
         (dimensions (loop repeat rank collect (read-ub16 stream))))
    (setf (aref constants index) (make-array dimensions))))

(defmethod %load-instruction ((mnemonic (eql 'setf-row-major-aref))
                              constants stream)
  (setf (row-major-aref (aref constants (read-index stream))
                        (read-ub16 stream))
        (aref constants (read-index stream))))

(defmethod %load-instruction ((mnemonic (eql 'make-hash-table))
                              constants stream)
  (let* ((index (read-index stream))
         (testcode (read-byte stream))
         (test (ecase testcode
                 ((#b00) 'eq)
                 ((#b01) 'eql)
                 ((#b10) 'equal)
                 ((#b11) 'equalp)))
         (count (read-ub16 stream)))
    (setf (aref constants index) (make-hash-table :test test :size count))))

(defmethod %load-instruction ((mnemonic (eql 'setf-gethash)) constants stream)
  (let ((htind (read-index stream))
        (keyind (read-index stream)) (valind (read-index stream)))
    (setf (gethash (aref constants keyind) (aref constants htind))
          (aref constants valind))))

(defun read-sb64 (stream)
  (let ((word (read-ub64 stream)))
    ;; Read sign bit and make this negative if it's set.
    ;; FIXME: Do something more efficient probably.
    (- word (ash (ldb (byte 1 63) word) 64))))

(defmethod %load-instruction ((mnemonic (eql 'make-sb64)) constants stream)
  (setf (aref constants (read-index stream)) (read-sb64 stream)))

(defmethod %load-instruction ((mnemonic (eql 'find-package)) constants stream)
  (setf (aref constants (read-index stream))
        (find-package (aref constants (read-index stream)))))

(defmethod %load-instruction ((mnemonic (eql 'make-bignum)) constants stream)
  (setf (aref constants (read-index stream))
        (let* ((ssize (read-sb64 stream))
               (size (abs ssize))
               (negp (minusp ssize)))
          (loop with result = 0
                repeat size
                do (setf result (logior (ash result 64) (read-ub64 stream)))
                finally (return (if negp (- result) result))))))

(defmethod %load-instruction ((mnemonic (eql 'intern)) constants stream)
  (setf (aref constants (read-index stream))
        (let ((package (aref constants (read-index stream)))
              (name (aref constants (read-index stream))))
          (intern name package))))

(defmethod %load-instruction ((mnemonic (eql 'make-character)) constants stream)
  (setf (aref constants (read-index stream))
        (code-char (read-ub32 stream))))

(defvar +uaet-codes+
  '((t         #b00000000)
    (base-char #b10000000)
    (character #b11000000)))

(defun decode-uaet (uaet-code)
  (or (first (find uaet-code +uaet-codes+ :key #'second))
      (error "BUG: Unknown UAET code ~x" uaet-code)))

(defmethod %load-instruction ((mnemonic (eql 'make-specialized-array))
                              constants stream)
  (let* ((index (read-index stream))
         (rank (read-byte stream))
         (dims (loop repeat rank collect (read-ub16 stream)))
         (total-size (reduce #'* dims))
         (etype-code (read-byte stream))
         (etype (decode-uaet etype-code))
         (arr (make-array dims :element-type etype)))
    (setf (aref constants index) arr)
    (ecase etype
      (base-char
       (dotimes (i total-size)
         (setf (row-major-aref arr i) (code-char (read-byte stream)))))
      (character
       (dotimes (i total-size)
         (setf (row-major-aref arr i) (code-char (read-ub32 stream))))))))

(defun load-instruction (constants stream)
  (%load-instruction (read-mnemonic stream) constants stream))

;; TODO: Check that the FASL actually defines all of the constants.
;; Make sure it defines them in order, i.e. not reading from uninitialized
;; portions of the vector.
;; Shrink the constants after loading.

(defun load-bytecode (stream)
  (load-magic stream)
  (load-version stream)
  (let* ((nobjs (read-ub64 stream))
         ;; Next power of two.
         (*index-bytes* (ash 1 (1- (ceiling (integer-length nobjs) 8))))
         (constants (make-array nobjs)))
    ;; CLHS is sort of written like LISTEN only works on character streams,
    ;; but that would be pretty pointless. Clasp and SBCL at least allow it
    ;; on byte streams.
    (loop while (listen stream)
          do (load-instruction constants stream))
    constants))
