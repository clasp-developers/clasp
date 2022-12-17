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
    (make-bytecode-function 87)
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

(defmacro dbgprint (message &rest args)
  `(when *load-verbose*
     (format *error-output* ,(concatenate 'string "~&; " message "~%") ,@args)))

(defun load-magic (stream)
  (let ((magic (read-ub32 stream)))
    (unless (= magic +magic+)
      (error "~s is not a valid bytecode FASL: invalid magic identifier ~d"
             stream magic))
    (dbgprint "Magic number matches: ~x" magic)))

;; Bounds for major and minor version understood by this loader.
;; It might be smarter for reverse compatibility to make the version
;; look up a loader? This will become more obvious once there are actually
;; multiple versions in existence.
(defparameter *min-version* '(0 0))
(defparameter *max-version* '(0 1))

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
    (dbgprint "File version ~d.~d (loader accepts ~d.~d-~d.~d)"
              major minor (first *min-version*) (second *min-version*)
              (first *max-version*) (second *max-version*))
    (values major minor)))

;; Module of Lisp bytecode.
(defvar *module*)

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

;; Return how many bytes were read.
(defgeneric %load-instruction (mnemonic constants stream))

(defmethod %load-instruction ((mnemonic (eql 'nil)) constants stream)
  (let ((index (read-index stream)))
    (dbgprint " (nil ~d)" index)
    (setf (aref constants index) nil))
  *index-bytes*)

(defmethod %load-instruction ((mnemonic (eql 't)) constants stream)
  (let ((index (read-index stream)))
    (dbgprint " (t ~d)" index)
    (setf (aref constants index) t))
  *index-bytes*)

(defmethod %load-instruction ((mnemonic (eql 'cons)) constants stream)
  (let ((index (read-index stream)))
    (dbgprint " (cons ~d)" index)
    (setf (aref constants index) (cons nil nil)))
  *index-bytes*)

(defmethod %load-instruction ((mnemonic (eql 'rplaca)) constants stream)
  (let ((cons (read-index stream)) (value (read-index stream)))
    (dbgprint " (rplaca ~d ~d)" cons value)
    (setf (car (aref constants cons)) (aref constants value)))
  (* 2 *index-bytes*))

(defmethod %load-instruction ((mnemonic (eql 'rplacd)) constants stream)
  (let ((cons (read-index stream)) (value (read-index stream)))
    (dbgprint " (rplacd ~d ~d)" cons value)
    (setf (cdr (aref constants cons)) (aref constants value)))
  (* 2 *index-bytes*))

(defmethod %load-instruction ((mnemonic (eql 'make-array)) constants stream)
  (let ((index (read-index stream)) (rank (read-byte stream)))
    (dbgprint " (make-array ~d ~d)" index rank)
    (let ((dimensions (loop repeat rank collect (read-ub16 stream))))
      (dbgprint "  dimensions ~a" dimensions)
      (setf (aref constants index) (make-array dimensions)))
    (+ *index-bytes* 1 (* rank 2))))

(defmethod %load-instruction ((mnemonic (eql 'setf-row-major-aref))
                              constants stream)
  (let ((index (read-index stream)) (aindex (read-ub16 stream))
        (value (read-index stream)))
    (dbgprint " ((setf row-major-aref) ~d ~d ~d" index aindex value)
    (setf (row-major-aref (aref constants index) aindex)
          (aref constants value)))
  (+ *index-bytes* 2 *index-bytes*))

(defmethod %load-instruction ((mnemonic (eql 'make-hash-table))
                              constants stream)
  (let ((index (read-index stream)))
    (dbgprint " (make-hash-table ~d)" index)
    (let* ((testcode (read-byte stream))
           (test (ecase testcode
                   ((#b00) 'eq)
                   ((#b01) 'eql)
                   ((#b10) 'equal)
                   ((#b11) 'equalp)))
          (count (read-ub16 stream)))
      (dbgprint "  test = ~a, count = ~d" test count)
      (setf (aref constants index) (make-hash-table :test test :size count))))
  (+ *index-bytes* 1 2))

(defmethod %load-instruction ((mnemonic (eql 'setf-gethash)) constants stream)
  (let ((htind (read-index stream))
        (keyind (read-index stream)) (valind (read-index stream)))
    (dbgprint " ((setf gethash) ~d ~d ~d)" htind keyind valind)
    (setf (gethash (aref constants keyind) (aref constants htind))
          (aref constants valind)))
  (+ *index-bytes* *index-bytes* *index-bytes*))

(defun read-sb64 (stream)
  (let ((word (read-ub64 stream)))
    ;; Read sign bit and make this negative if it's set.
    ;; FIXME: Do something more efficient probably.
    (- word (ash (ldb (byte 1 63) word) 64))))

(defmethod %load-instruction ((mnemonic (eql 'make-sb64)) constants stream)
  (let ((index (read-index stream)) (sb64 (read-sb64 stream)))
    (dbgprint " (make-sb64 ~d ~d)" index sb64)
    (setf (aref constants index) sb64))
  (+ *index-bytes* 8))

(defmethod %load-instruction ((mnemonic (eql 'find-package)) constants stream)
  (let ((index (read-index stream)) (name (read-index stream)))
    (dbgprint " (find-package ~d ~d)" index name)
    (setf (aref constants index) (find-package (aref constants name))))
  (+ *index-bytes* *index-bytes*))

(defmethod %load-instruction ((mnemonic (eql 'make-bignum)) constants stream)
  (let ((index (read-index stream)) (ssize (read-sb64 stream)))
    (dbgprint " (make-bignum ~d ~d)" index ssize)
    (setf (aref constants index)
          ;; Using loop repeat is messy for fencepost reasons.
          (let ((result 0) (size (abs ssize)) (negp (minusp ssize)))
            (loop (when (zerop size) (return (if negp (- result) result)))
                  (let ((word (read-ub64 stream)))
                    (dbgprint  "#x~x" word)
                    (setf result (logior (ash result 64) word))))))
    (+ *index-bytes* 8 (* 8 (abs ssize)))))

(defmethod %load-instruction ((mnemonic (eql 'intern)) constants stream)
  (let ((index (read-index stream))
        (package (read-index stream)) (name (read-index stream)))
    (dbgprint " (intern ~d ~d ~d)" index package name)
    (setf (aref constants index)
          (intern (aref constants name) (aref constants package))))
  (+ *index-bytes* *index-bytes* *index-bytes*))

(defmethod %load-instruction ((mnemonic (eql 'make-character)) constants stream)
  (let* ((index (read-index stream)) (code (read-ub32 stream))
         (char (code-char code)))
    (dbgprint " (make-character ~d #x~x) ; ~c" index code char)
    (setf (aref constants index) char))
  (+ *index-bytes* 4))

(defvar +uaet-codes+
  '((t         #b00000000)
    (base-char #b10000000)
    (character #b11000000)))

(defun decode-uaet (uaet-code)
  (or (first (find uaet-code +uaet-codes+ :key #'second))
      (error "BUG: Unknown UAET code ~x" uaet-code)))

(defmethod %load-instruction ((mnemonic (eql 'make-specialized-array))
                              constants stream)
  (let ((index (read-index stream))
        (rank (read-byte stream)))
    (dbgprint " (make-specialized-array ~d ~d)" index rank)
    (let* ((dims (loop repeat rank collect (read-ub16 stream)))
           (total-size (reduce #'* dims))
           (etype-code (read-byte stream))
           (etype (decode-uaet etype-code))
           (arr (make-array dims :element-type etype)))
      (dbgprint "  dimensions ~a" dims)
      (dbgprint "  element type ~a" etype)
      (setf (aref constants index) arr)
      (ecase etype
        (base-char
         (dotimes (i total-size)
           (setf (row-major-aref arr i) (code-char (read-byte stream)))))
        (character
         (dotimes (i total-size)
           (setf (row-major-aref arr i) (code-char (read-ub32 stream))))))
      (dbgprint "  array ~s" arr)
      (+ *index-bytes* 1 (* rank 2)
         1 (* (ecase etype (base-char 1) (character 4)) total-size)))))

(defmethod %load-instruction ((mnemonic (eql 'make-bytecode-function))
                              constants stream)
  (let ((index (read-index stream))
        (entry-point (read-ub32 stream))
        (nlocals (read-ub16 stream))
        (nclosed (read-ub16 stream))
        (namei (read-index stream))
        (lambda-listi (read-index stream))
        (docstringi (read-index stream)))
    (dbgprint " (make-bytecode-function ~d ~d ~d ~d ~d ~d ~d)"
              index entry-point nlocals nclosed namei lambda-listi docstringi)
    (let ((name (aref constants namei))
          (lambda-list (aref constants lambda-listi))
          (docstring (aref constants docstringi))
          ;; TODO
          (source-pathname nil)
          (lineno -1) (column -1) (filepos -1))
      (setf (aref constants index)
            (core:global-bytecode-simple-fun/make
             (core:function-description/make
              :function-name name :lambda-list lambda-list :docstring docstring
              :source-pathname source-pathname
              :lineno lineno :column column :filepos filepos)
             *module* nlocals nclosed entry-point
             (cmp:compile-trampoline name)))))
  (+ *index-bytes* 4 2 2 *index-bytes* *index-bytes* *index-bytes*))

;; Return how many bytes were read.
(defun load-instruction (constants stream)
  (1+ (%load-instruction (read-mnemonic stream) constants stream)))

;; TODO: Check that the FASL actually defines all of the constants.
;; Make sure it defines them in order, i.e. not reading from uninitialized
;; portions of the vector.
;; Shrink the constants after loading.

(defun load-bytecode-module (constants stream)
  (dbgprint "Loading Lisp bytecode")
  (let ((nbytes (read-ub32 stream)))
    (dbgprint "File reports ~d bytes." nbytes)
    (let ((bytes (make-array nbytes :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      (core:bytecode-module/setf-bytecode *module* bytes)
      (core:bytecode-module/setf-literals *module* constants)
      (dbgprint "Loaded Lisp bytecode module")
      *module*)))

(defun load-toplevels (constants stream)
  (dbgprint "Loading toplevels")
  (let ((ntops (read-ub32 stream)))
    (dbgprint "File reports ~d toplevel forms." ntops)
    (loop repeat ntops
          for i from 0
          for index = (read-index stream)
          for tl = (aref constants index)
          do (dbgprint "Calling toplevel #~d" i)
             (funcall tl))))

(defun load-bytecode-stream (stream
                             &key ((:verbose *load-verbose*) *load-verbose*))
  (load-magic stream)
  (multiple-value-bind (major minor) (load-version stream)
    (declare (ignore major))
    (let* ((nobjs (read-ub64 stream))
           (nfbytes (when (>= minor 1) (read-ub64 stream)))
           ;; Next power of two.
           (*index-bytes* (ash 1 (1- (ceiling (integer-length nobjs) 8))))
           (*module* (when (>= minor 1) (core:bytecode-module/make)))
           (constants (make-array nobjs)))
      (dbgprint "File reports ~d objects. Index length = ~d bytes."
                nobjs *index-bytes*)
      (dbgprint "Executing FASL bytecode")
      (when (>= minor 1) (dbgprint "File reports bytecode is ~d bytes" nfbytes))
      ;; CLHS is sort of written like LISTEN only works on character streams,
      ;; but that would be pretty pointless. Clasp and SBCL at least allow it
      ;; on byte streams.
      (loop for bytes-read = 0
              then (+ bytes-read (load-instruction constants stream))
            do (dbgprint "  read ~d bytes" bytes-read)
            while (< bytes-read nfbytes)
            finally
               (unless (= bytes-read nfbytes)
                 (error "Mismatch in bytecode between reported length ~d and actual length ~d"
                        nfbytes bytes-read)))
      (when (>= minor 1)
        (load-bytecode-module constants stream)
        (load-toplevels constants stream))))
  (values))

(defun load-bytecode (filespec
                      &key
                        ((:verbose *load-verbose*) *load-verbose*)
                        ((:print *load-print*) *load-print*)
                        (if-does-not-exist :error)
                        (external-format :default))
  (with-open-file (input filespec :element-type '(unsigned-byte 8)
                                  :if-does-not-exist if-does-not-exist
                                  :external-format external-format)
    (load-bytecode-stream input)))
