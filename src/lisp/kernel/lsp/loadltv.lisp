(defpackage #:loadltv
  (:use #:cl)
  (:export #:load-bytecode))

(in-package #:loadltv)

(defparameter +ops+
  '((nil 65 sind)
    (t 66 sind)
    (ratio 67)
    (complex 68)
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
    (make-symbol 81) ; make-bitvector in clasp, but that's under arrays here
    (intern 82 sind packageind nameind) ; make-symbol in clasp
    (make-character 83 sind ub32) ; ub64 in clasp, i think?
    (make-pathname 85)
    (make-bytecode-function 87)
    (make-bytecode-module 88)
    (setf-literals 89 modind litsind)
    (make-single-float 90 sind ub32)
    (make-double-float 91 sind ub64)
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

;;; Read a signed n-byte integer from a ub8 stream, big-endian.
(defun read-sb (n stream)
  (let ((word (read-ub n stream))
        (nbits (* n 8)))
    (declare (type (integer 1 64) nbits))
    ;; Read sign bit and make this negative if it's set.
    ;; FIXME: Do something more efficient probably.
    (- word (ash (ldb (byte 1 (1- nbits)) word) nbits))))

(defun read-sb64 (stream) (read-sb 8 stream))
(defun read-sb32 (stream) (read-sb 4 stream))
(defun read-sb16 (stream) (read-sb 2 stream))
(defun read-sb8  (stream) (read-sb 1 stream))

(defconstant +magic+ #x8d7498b1) ; randomly chosen bytes.

(defmacro verboseprint (message &rest args)
  `(when *load-verbose*
     (format t ,(concatenate 'string "~&; " message "~%") ,@args)))
(defmacro printprint (message &rest args)
  `(when *load-print*
     (format t ,(concatenate 'string "~&; " message "~%") ,@args)))

(defvar *debug-loader* nil)

(defmacro dbgprint (message &rest args)
  `(when *debug-loader*
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
(defparameter *max-version* '(0 3))

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

;; Major and minor version of the file being read.
(defvar *major*)
(defvar *minor*)

;; Module of Lisp bytecode (only used for 0.1).
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

(defmacro read-sub-byte (array stream nbits)
  (let ((perbyte (floor 8 nbits))
        (a (gensym "ARRAY")) (s (gensym "STREAM")))
    `(let* ((,a ,array) (,s ,stream)
            (total-size (array-total-size ,a)))
       (multiple-value-bind (full-bytes remainder) (floor total-size 8)
         (loop for byteindex below full-bytes
               for index = (* ,perbyte byteindex)
               for byte = (read-byte ,s)
               do ,@(loop for j below perbyte
                          for bit-index
                            = (* nbits (- perbyte j 1))
                          for bits = `(ldb (byte ,nbits ,bit-index)
                                           byte)
                          for arrindex = `(+ index ,j)
                          collect `(setf (row-major-aref array ,arrindex) ,bits)))
         ;; write remainder
         (let* ((index (* ,perbyte full-bytes))
                (byte (read-byte ,s)))
           (loop for j below remainder
                 for bit-index = (* ,nbits (- ,perbyte j 1))
                 for bits = (ldb (byte ,nbits bit-index) byte)
                 do (setf (row-major-aref ,a (+ index j)) bits)))))))

(defmethod %load-instruction ((mnemonic (eql 'make-array)) constants stream)
  (if (<= *minor* 2)
      (let ((index (read-index stream)) (rank (read-byte stream)))
        (dbgprint " (make-array ~d ~d)" index rank)
        (let ((dimensions (loop repeat rank collect (read-ub16 stream))))
          (dbgprint "  dimensions ~a" dimensions)
          (setf (aref constants index) (make-array dimensions)))
        (+ *index-bytes* 1 (* rank 2)))
      (let* ((index (read-index stream)) (uaet-code (read-byte stream))
             (uaet (decode-uaet uaet-code))
             (packing-code (read-byte stream))
             (packing-type (decode-packing packing-code))
             (rank (read-byte stream))
             (dimensions (loop repeat rank collect (read-ub16 stream)))
             (array (make-array dimensions :element-type uaet)))
        (setf (aref constants index) array)
        (macrolet ((undump (form)
                     `(loop for i below (array-total-size array)
                            for elem = ,form
                            do (setf (row-major-aref array i) elem))))
          (cond ((equal packing-type 'nil))
                ((equal packing-type 'base-char)
                 (undump (code-char (read-byte stream))))
                ((equal packing-type 'character)
                 (undump (code-char (read-ub32 stream))))
                ((equal packing-type 'single-float)
                 (undump (ext:bits-to-single-float (read-ub32 stream))))
                ((equal packing-type 'double-float)
                 (undump (ext:bits-to-double-float (read-ub64 stream))))
                ((equal packing-type '(complex single-float))
                 (undump
                  (complex (ext:bits-to-single-float (read-ub32 stream))
                           (ext:bits-to-single-float (read-ub32 stream)))))
                ((equal packing-type '(complex double-float))
                 (undump
                  (complex (ext:bits-to-double-float (read-ub64 stream))
                           (ext:bits-to-double-float (read-ub64 stream)))))
                ((equal packing-type 'bit) (read-sub-byte array stream 1))
                ((equal packing-type '(unsigned-byte 2))
                 (read-sub-byte array stream 2))
                ((equal packing-type '(unsigned-byte 4))
                 (read-sub-byte array stream 4))
                ((equal packing-type '(unsigned-byte 8))
                 (read-sequence array stream))
                ((equal packing-type '(unsigned-byte 16))
                 (undump (read-ub16 stream)))
                ((equal packing-type '(unsigned-byte 32))
                 (undump (read-ub32 stream)))
                ((equal packing-type '(unsigned-byte 64))
                 (undump (read-ub64 stream)))
                ((equal packing-type '(signed-byte 8))
                 (undump (read-sb8  stream)))
                ((equal packing-type '(signed-byte 16))
                 (undump (read-sb16 stream)))
                ((equal packing-type '(signed-byte 32))
                 (undump (read-sb32 stream)))
                ((equal packing-type '(signed-byte 64))
                 (undump (read-sb64 stream)))
                ;; TODO: signed bytes
                ((equal packing-type 't)) ; setf-aref takes care of it
                (t (error "BUG: Unknown packing-type ~s" packing-type)))))))

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
                    (dbgprint  "#x~8,'0x" word)
                    (setf result (logior (ash result 64) word))))))
    (+ *index-bytes* 8 (* 8 (abs ssize)))))

(defmethod %load-instruction ((mnemonic (eql 'make-single-float))
                              constants stream)
  (let ((index (read-index stream)) (bits (read-ub32 stream)))
    (dbgprint " (make-single-float ~d #x~4,'0x)" index bits)
    (setf (aref constants index) (ext:bits-to-single-float bits)))
  (+ *index-bytes* 4))

(defmethod %load-instruction ((mnemonic (eql 'make-double-float))
                              constants stream)
  (let ((index (read-index stream)) (bits (read-ub64 stream)))
    (dbgprint " (make-double-float ~d #x~8,'0x)" index bits)
    (setf (aref constants index) (ext:bits-to-double-float bits)))
  (+ *index-bytes* 8))

(defmethod %load-instruction ((mnemonic (eql 'ratio)) constants stream)
  (let ((index (read-index stream))
        (numi (read-index stream)) (deni (read-index stream)))
    (dbgprint " (ratio ~d ~d ~d)" index numi deni)
    (setf (aref constants index)
          ;; a little inefficient.
          (/ (aref constants numi) (aref constants deni))))
  (* 3 *index-bytes*))

(defmethod %load-instruction ((mnemonic (eql 'complex)) constants stream)
  (let ((index (read-index stream))
        (reali (read-index stream)) (imagi (read-index stream)))
    (dbgprint " (complex ~d ~d ~d)" index reali imagi)
    (setf (aref constants index)
          (complex (aref constants reali) (aref constants imagi))))
  (* 3 *index-bytes*))

(defmethod %load-instruction ((mnemonic (eql 'make-symbol))
                              constants stream)
  (let ((index (read-index stream))
        (namei (read-index stream)))
    (dbgprint " (make-symbol ~d ~d)" index namei)
    (setf (aref constants index) (make-symbol (aref constants namei))))
  (+ *index-bytes* *index-bytes*))

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

(defmethod %load-instruction ((mnemonic (eql 'make-pathname)) constants stream)
  (let ((index (read-index stream))
        (hosti (read-index stream)) (devicei (read-index stream))
        (directoryi (read-index stream)) (namei (read-index stream))
        (typei (read-index stream)) (versioni (read-index stream)))
    (dbgprint " (make-pathname ~d ~d ~d ~d ~d ~d ~d)"
              index hosti devicei directoryi namei typei versioni)
    (setf (aref constants index)
          (make-pathname :host (aref constants hosti)
                         :device (aref constants devicei)
                         :directory (aref constants directoryi)
                         :name (aref constants namei)
                         :type (aref constants typei)
                         :version (aref constants versioni))))
  (* *index-bytes* 7))

(defvar +array-packing-infos+
  '((nil                    #b00000000)
    (base-char              #b10000000)
    (character              #b11000000)
    ;;(short-float          #b10100000) ; i.e. binary16
    (single-float           #b00100000) ; binary32
    (double-float           #b01100000) ; binary64
    ;;(long-float           #b11100000) ; binary128?
    ;;((complex short...)   #b10110000)
    ((complex single-float) #b00110000)
    ((complex double-float) #b01110000)
    ;;((complex long...)    #b11110000)
    (bit                    #b00000001) ; (2^(code-1)) bits
    ((unsigned-byte 2)      #b00000010)
    ((unsigned-byte 4)      #b00000011)
    ((unsigned-byte 8)      #b00000100)
    ((unsigned-byte 16)     #b00000101)
    ((unsigned-byte 32)     #b00000110)
    ((unsigned-byte 64)     #b00000111)
    ;;((unsigned-byte 128) ??)
    ((signed-byte 8)        #b10000100)
    ((signed-byte 16)       #b10000101)
    ((signed-byte 32)       #b10000110)
    ((signed-byte 64)       #b10000111)
    (t                      #b11111111)))

(defun decode-uaet (uaet-code)
  (or (first (find uaet-code +array-packing-infos+ :key #'second))
      (error "BUG: Unknown UAET code ~x" uaet-code)))

(defun decode-packing (code) (decode-uaet code)) ; same for now

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
        (modulei (when (>= *minor* 2) (read-index stream)))
        (namei (read-index stream))
        (lambda-listi (read-index stream))
        (docstringi (read-index stream)))
    (dbgprint " (make-bytecode-function ~d ~d ~d ~d~@[ ~d~] ~d ~d ~d)"
              index entry-point nlocals nclosed
              modulei namei lambda-listi docstringi)
    (let ((module (if (<= *minor* 1) *module* (aref constants modulei)))
          (name (aref constants namei))
          (lambda-list (aref constants lambda-listi))
          (docstring (aref constants docstringi))
          ;; TODO
          (source-pathname nil)
          (lineno -1) (column -1) (filepos -1))
      (dbgprint "  entry-point = ~d, nlocals = ~d, nclosed = ~d"
                entry-point nlocals nclosed)
      (dbgprint "  module-index = ~d" modulei)
      (dbgprint "  name = ~a, lambda-list = ~a, docstring = ~a"
                name lambda-list docstring)
      (setf (aref constants index)
            (core:global-bytecode-simple-fun/make
             (core:function-description/make
              :function-name name :lambda-list lambda-list :docstring docstring
              :source-pathname source-pathname
              :lineno lineno :column column :filepos filepos)
             module nlocals nclosed entry-point
             (cmp:compile-trampoline name)))))
  (+ *index-bytes* 4 2 2 *index-bytes* *index-bytes* *index-bytes*))

(defmethod %load-instruction ((mnemonic (eql 'make-bytecode-module))
                              constants stream)
  (let* ((index (read-index stream))
         (len (read-ub32 stream))
         (bytecode (make-array len :element-type '(unsigned-byte 8)))
         (module (core:bytecode-module/make)))
    (dbgprint " (make-bytecode-module ~d ~d)" index len)
    (read-sequence bytecode stream)
    (dbgprint "  bytecode:~{ ~2,'0x~}" (coerce bytecode 'list))
    (setf (aref constants index) module)
    (core:bytecode-module/setf-bytecode module bytecode)
    ;; pointless but harmless if followed by a setf-literals instruction.
    (core:bytecode-module/setf-literals module constants)
    (+ *index-bytes* 4 len)))

(defmethod %load-instruction ((mnemonic (eql 'setf-literals)) constants stream)
  (let ((modi (read-index stream)) (litsi (read-index stream)))
    (dbgprint " (setf-literals ~d ~d)" modi litsi)
    (core:bytecode-module/setf-literals
     (aref constants modi) (aref constants litsi))))

(defmethod %load-instruction ((mnemonic (eql 'funcall-create))
                              constants stream)
  (let ((index (read-index stream)) (funi (read-index stream)))
    (dbgprint " (funcall-create ~d ~d)" index funi)
    (setf (aref constants index) (funcall (aref constants funi))))
  (* 2 *index-bytes*))

(defmethod %load-instruction ((mnemonic (eql 'funcall-initialize))
                              constants stream)
  (let ((funi (read-index stream)))
    (dbgprint " (funcall-initialize ~d)" funi)
    (dbgprint "  calling ~s" (aref constants funi))
    (funcall (aref constants funi)))
  *index-bytes*)

;; Return how many bytes were read.
(defun load-instruction (constants stream)
  (if (<= *minor* 2)
      (1+ (%load-instruction (read-mnemonic stream) constants stream))
      (%load-instruction (read-mnemonic stream) constants stream)))

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
  (multiple-value-bind (*major* *minor*) (load-version stream)
    (let* ((nobjs (read-ub64 stream))
           (nfbytes (when (<= 1 *minor* 2) (read-ub64 stream)))
           (ninsts (when (>= *minor* 3) (read-ub64 stream)))
           ;; Next power of two.
           (*index-bytes* (ash 1 (1- (ceiling (integer-length nobjs) 8))))
           (*module* (when (= *minor* 1) (core:bytecode-module/make)))
           (constants (make-array nobjs)))
      (dbgprint "File reports ~d objects. Index length = ~d bytes."
                nobjs *index-bytes*)
      (dbgprint "Executing FASL bytecode")
      (cond ((<= 1 *minor* 2)
             (dbgprint "File reports bytecode is ~d bytes" nfbytes))
            ((>= *minor* 3)
             (dbgprint "File reports ~d instructions" ninsts)))
      ;; CLHS is sort of written like LISTEN only works on character streams,
      ;; but that would be pretty pointless. Clasp and SBCL at least allow it
      ;; on byte streams.
      (cond ((<= 1 *minor* 2)
             (loop for bytes-read = 0
                     then (+ bytes-read (load-instruction constants stream))
                   do (dbgprint "  read ~d bytes" bytes-read)
                   while (< bytes-read nfbytes)
                   finally
                      (unless (= bytes-read nfbytes)
                        (error "Mismatch in bytecode between reported length ~d and actual length ~d"
                               nfbytes bytes-read))))
            ((>= *minor* 3)
             (loop repeat ninsts
                   do (load-instruction constants stream))
             #-clasp ; listen is broken on clasp
             (when (listen stream)
               (error "Bytecode continues beyond end of instructions: ~a"
                      (alexandria:read-stream-content-into-byte-vector stream)))))
      (when (= *minor* 1)
        (load-bytecode-module constants stream)
        (load-toplevels constants stream))))
  (values))

(defun load-bytecode (filespec
                      &key
                        ((:verbose *load-verbose*) *load-verbose*)
                        ((:print *load-print*) *load-print*)
                        ((:debug *debug-loader*) *debug-loader*)
                        (if-does-not-exist :error)
                        (external-format :default))
  (with-open-file (input filespec :element-type '(unsigned-byte 8)
                                  :if-does-not-exist if-does-not-exist
                                  :external-format external-format)
    ;; check for :if-does-not-exist nil failure
    (unless input (return-from load-bytecode nil))
    (verboseprint "Loading ~a as FASL" filespec)
    (load-bytecode-stream input)
    t))
