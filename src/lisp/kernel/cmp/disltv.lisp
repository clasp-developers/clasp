;;;; Code for introspecting bytecode FASLs and performing various
;;;; operations on them, such as validation (TODO) and linking.
;;;; TODO: Maybe store any warnings in the FASL object?

(in-package #:cmpltv)

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

(defun read-index (stream)
  (ecase *index-bytes*
    ((1) (read-byte stream))
    ((2) (read-ub16 stream))
    ((4) (read-ub32 stream))
    ((8) (read-ub64 stream))))

(defun load-magic (stream)
  (let ((magic (read-ub32 stream)))
    (unless (= magic +magic+)
      (error "~s is not a valid bytecode FASL: invalid magic identifier ~d"
             stream magic))
    (dbgprint "Magic number matches: ~x" magic)))

;; Bounds for major and minor version understood by this loader.
(defparameter *min-version* '(0 4))
(defparameter *max-version* '(0 4))

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

(defclass fasl ()
  (;; The version is recorded for introspection but is ignored when
   ;; writing out a FASL object. The writer just puts out whatever
   ;; version of the format it does.
   (%major-version :initarg :major :reader major-version
                   :type (unsigned-byte 16))
   (%minor-version :initarg :minor :reader minor-version
                   :type (unsigned-byte 16))
   (%object-count :initarg :nobjs :reader object-count
                  :type (unsigned-byte 64))
   (%instructions :initarg :instructions :reader instructions
                  :type (simple-array t (*)))
   (%attributes :initarg :attributes :reader attributes
                :type (simple-array t (*)))))

(defmethod print-object ((object fasl) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "(~d ~d ~d)"
            (object-count object)
            (length (instructions object))
            (length (attributes object)))))

;; Major and minor version of the file being loaded.
(defvar *load-major*)
(defvar *load-minor*)

;; Mapping from object indices to loaded creators.
;; (Note that this loader does not in general create the lisp objects
;;  described in the file, as this can require arbitrary side effects
;;  that may not make sense in the examining image, e.g. package refs.)
(defvar *index-to-creator*)

(defun creator (index)
  (let ((inst (aref *index-to-creator* index)))
    (or inst
        ;; If there's a problem, WARN, and return NIL.
        (warn "Invalid FASL: Creator for index ~d not yet loaded"
              index))))

(defun (setf creator) (inst index)
  (let ((existing (aref *index-to-creator* index)))
    (if existing
        (warn "Invalid FASL: Index ~d defined more than once" index)
        (setf (aref *index-to-creator* index) inst)))
  inst)

(defmacro read-creator (stream) `(creator (read-index ,stream)))

;;; Return a new INSTRUCTION instance.
(defgeneric %load-instruction (mnemonic stream))

(defmethod %load-instruction ((mnemonic (eql 'nil)) stream)
  (setf (read-creator stream)
        (make-instance 'singleton-creator :prototype nil)))

(defmethod %load-instruction ((mnemonic (eql 't)) stream)
  (setf (read-creator stream)
        (make-instance 'singleton-creator :prototype t)))

(defmethod %load-instruction ((mnemonic (eql 'cons)) stream)
  (setf (read-creator stream)
        (make-instance 'cons-creator)))

(defmethod %load-instruction ((mnemonic (eql 'rplaca)) stream)
  (make-instance 'rplaca-init
    :cons (read-creator stream)
    :value (read-creator stream)))

(defmethod %load-instruction ((mnemonic (eql 'rplacd)) stream)
  (make-instance 'rplacd-init
    :cons (read-creator stream)
    :value (read-creator stream)))

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

(defun decode-uaet (uaet-code)
  (or (first (find uaet-code +array-packing-infos+ :key #'second))
      (error "BUG: Unknown UAET code ~x" uaet-code)))
(defun decode-packing (code) (decode-uaet code)) ; same for now

(defmethod %load-instruction ((mnemonic (eql 'make-array)) stream)
  (let* ((index (read-index stream)) (uaet-code (read-byte stream))
         (uaet (decode-uaet uaet-code))
         (packing-code (read-byte stream))
         (packing-type (decode-packing packing-code))
         (rank (read-byte stream))
         (dimensions (loop repeat rank collect (read-ub16 stream)))
         (array (unless (eq packing-type 't)
                  ;; for T packing we don't make the array now,
                  ;; as doing so is side-effect-based.
                  (make-array dimensions :element-type uaet))))
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
            ((equal packing-type 't))
            (t (error "BUG: Unknown packing-type ~s" packing-type))))
    (setf (creator index)
          (if (eq packing-type 't)
              (make-instance 'array-creator
                :dimensions dimensions
                :packing-info (%uaet-info 't) ; kludgeish
                :uaet-code uaet-code)
              (make-instance 'array-creator
                :dimensions dimensions
                :packing-info (%uaet-info uaet) ; kludgeish
                :uaet-code uaet-code
                :prototype array)))))

(defmethod %load-instruction ((mnemonic (eql 'setf-row-major-aref)) stream)
  (make-instance 'setf-aref
    :array (read-creator stream)
    :index (read-ub16 stream) :value (read-creator stream)))

(defmethod %load-instruction ((mnemonic (eql 'make-hash-table)) stream)
  (let ((index (read-index stream)) (testcode (read-byte stream))
        (count (read-ub16 stream)))
    (setf (creator index)
          (make-instance 'hash-table-creator
            :test (ecase testcode
                    ((#b00) 'eq)
                    ((#b01) 'eql)
                    ((#b10) 'equal)
                    ((#b11) 'equalp))
            :count count))))

(defmethod %load-instruction ((mnemonic (eql 'setf-gethash)) stream)
  (make-instance 'setf-gethash
    :hash-table (read-creator stream)
    :key (read-creator stream)
    :value (read-creator stream)))

(defmethod %load-instruction ((mnemonic (eql 'make-sb64)) stream)
  (setf (read-creator stream)
        (make-instance 'sb64-creator :prototype (read-sb64 stream))))

(defmethod %load-instruction ((mnemonic (eql 'find-package)) stream)
  (setf (read-creator stream)
        (make-instance 'package-creator
          :name (read-creator stream))))

(defmethod %load-instruction ((mnemonic (eql 'make-bignum)) stream)
  (setf (read-creator stream)
        (make-instance 'bignum-creator
          (let* ((ssize (read-sb64 stream)) (result 0) (size (abs ssize))
                 (negp (minusp size)))
            (loop (when (zerop size) (return (if negp (- result) result)))
                  (let ((word (read-ub64 stream)))
                    (setf result (logior (ash result 64) word))))))))

(defmethod %load-instruction ((mnemonic (eql 'make-single-float)) stream)
  (setf (read-creator stream)
        (make-instance 'single-float-creator
          :prototype (ext:bits-to-single-float (read-ub32 stream)))))

(defmethod %load-instruction ((mnemonic (eql 'make-double-float)) stream)
  (setf (read-creator stream)
        (make-instance 'double-float-creator
          :prototype (ext:bits-to-double-float (read-ub64 stream)))))

(defmethod %load-instruction ((mnemonic (eql 'ratio)) stream)
  (setf (read-creator stream)
        (make-instance 'ratio-creator
          :numerator (read-creator stream)
          :denominator (read-creator stream))))

(defmethod %load-instruction ((mnemonic (eql 'complex)) stream)
  (setf (read-creator stream)
        (make-instance 'complex-creator
          :realpart (read-creator stream)
          :denominator (read-creator stream))))

(defmethod %load-instruction ((mnemonic (eql 'make-symbol)) stream)
  (setf (read-creator stream)
        (make-instance 'symbol-creator
          :name (read-creator stream))))

(defmethod %load-instruction ((mnemonic (eql 'intern)) stream)
  (setf (read-creator stream)
        (make-instance 'interned-symbol-creator
          :package (read-creator stream) :name (read-creator stream))))

(defmethod %load-instruction ((mnemonic (eql 'make-character)) stream)
  (setf (read-creator stream)
        (make-instance 'character-creator
          :prototype (code-char (read-ub32 stream)))))

(defmethod %load-instruction ((mnemonic (eql 'make-pathname)) stream)
  (setf (read-creator stream)
        (make-instance 'pathname-creator
          :host (read-creator stream) :device (read-creator stream)
          :directory (read-creator stream) :name (read-creator stream)
          :type (read-creator stream) :version (read-creator stream))))

(defmethod %load-instruction ((mnemonic (eql 'make-bytecode-function)) stream)
  (setf (read-creator stream)
        (make-instance 'bytefunction-creator
          :entry-point (read-ub32 stream)
          :nlocals (read-ub16 stream)
          :nclosed (read-ub16 stream)
          :module (read-creator stream)
          :name (read-creator stream)
          :lambda-list (read-creator stream)
          :docstring (read-creator stream))))

(defmethod %load-instruction ((mnemonic (eql 'make-bytecode-module)) stream)
  (setf (read-creator stream)
        (let ((lispcode (make-array (read-ub32 stream)
                                    :element-type '(unsigned-byte 8))))
          (read-sequence lispcode stream)
          (make-instance 'bytemodule-creator
            :lispcode lispcode))))

(defmethod %load-instruction ((mnemonic (eql 'setf-literals)) stream)
  (make-instance 'setf-literals
    :module (read-creator stream) :literals (read-creator stream)))

(defmethod %load-instruction ((mnemonic (eql 'funcall-create)) stream)
  (setf (read-creator stream)
        (make-instance 'general-creator
          :function (read-creator stream))))

(defmethod %load-instruction ((mnemonic (eql 'funcall-initialize)) stream)
  (make-instance 'general-initializer :function (read-creator stream)))

(defun read-mnemonic (stream)
  (let* ((opcode (read-byte stream))
         (info (find opcode +ops+ :key #'second)))
    (if info
        (first info)
        (error "BUG: Unknown opcode ~x" opcode))))

(defun load-instruction (stream)
  (%load-instruction (read-mnemonic stream) stream))

;;;

(defgeneric %load-attribute (mnemonic name-creator stream))

(defmethod %load-attribute ((mnemonic string) ncreator stream)
  (declare (ignore ncreator))
  ;; skip. we could record this as a pseudo-attribute, hypothetically
  (loop repeat (read-ub32 stream) do (read-byte stream)))

;;; Results from non-string (invalid) attribute name.
(defmethod %load-attribute ((mnemonic null) ncreator stream)
  (declare (ignore ncreator))
  (loop repeat (read-ub32 stream) do (read-byte stream)))

#+clasp
(defmethod %load-attribute ((mnemonic (eql 'source-pos-info)) ncreator stream)
  ;; read and ignore nbytes.
  (read-ub32 stream)
  ;; read SPI.
  (make-instance 'spi-attr
    :name ncreator
    :function (read-creator stream)
    :pathname (read-creator stream)
    :lineno (read-ub64 stream)
    :column (read-ub64 stream)
    :filepos (read-ub64 stream)))

(defparameter *attr-map*
  (let ((ht (make-hash-table :test #'equal)))
    #+clasp (setf (gethash "clasp:source-pos-info" ht) 'source-pos-info)
    ht)
  #+(or)
  (alexandria:alist-hash-table
   '(#+clasp("clasp:source-pos-info" . source-pos-info))
   :test #'equal))

(defun load-attribute (stream)
  (let* ((acreator (read-creator stream))
         ;; For this to work, we have to actually make arrays from
         ;; make-array instructions, as we do.
         (aname (if (and (typep acreator 'array-creator)
                         (slot-boundp acreator '%prototype))
                    (prototype acreator)
                    (warn "Invalid FASL: Attribute name ~a is not a string"
                          acreator))))
    (%load-attribute (or (gethash aname *attr-map*) aname) acreator stream)))

;;;

(defun load-bytecode-stream (stream)
  (load-magic stream)
  (multiple-value-bind (*load-major* *load-minor*) (load-version stream)
    (let* ((nobjs (read-ub64 stream))
           (ninsts (read-ub64 stream))
           (*index-bytes* (ash 1 (1- (ceiling (integer-length nobjs) 8))))
           ;; Unlike in the compiler, here instructions and attributes
           ;; are vectors, and in forward order.
           (*instructions* (make-array ninsts))
           (*index-to-creator* (make-array nobjs :initial-element nil))
           (_
             (loop for i below ninsts
                   do (setf (aref *instructions* i)
                            (load-instruction stream))))
           (nattrs (read-ub32 stream))
           (*attributes* (make-array nattrs)))
      (declare (ignore _))
      (loop for i below nattrs
            do (setf (aref *attributes* i) (load-attribute stream)))
      (make-instance 'fasl
        :major *load-major* :minor *load-minor* :nobjs nobjs
        :instructions *instructions* :attributes *attributes*)))
  #-clasp ; listen is broken on clasp
  (when (listen stream)
    (error "Bytecode continues beyond end of instructions: ~a"
           (alexandria:read-stream-content-into-byte-vector stream))))

(defun load-bytecode (filespec
                      &key
                        (if-does-not-exist :error)
                        (external-format :default))
  (with-open-file (input filespec :element-type '(unsigned-byte 8)
                                  :if-does-not-exist if-does-not-exist
                                  :external-format external-format)
    ;; check for :if-does-not-exist nil failure
    (unless input (return-from load-bytecode nil))
    (load-bytecode-stream input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Operations on FASLs
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Saving
;;;
;;; Write a FASL object out to a stream/file.

(defun write-fasl (fasl stream)
  (write-bytecode (instructions fasl) (attributes fasl) stream))

(defun save-fasl (fasl output-path)
  (with-open-file (output output-path
                          :direction :output
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (write-fasl fasl output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Concatenation
;;; Make a new FASL that has the same effects as loading the original two
;;; fasls in order would have. This is a dumb concatenation that doesn't try
;;; to coalesce anything; that can be called "linking" maybe.

(defun concatenate-fasls (&rest fasls)
  (cond ((null fasls)
         ;; weird, but ok
         (make-instance 'fasl
           :major *major-version* :minor *minor-version* :nobjs 0
           :instructions #() :attributes #()))
        ((null (rest fasls)) (first fasls))
        (t
         (make-instance 'fasl
           :major *major-version* :minor *minor-version*
           :nobjs (reduce #'+ fasls :key #'object-count)
           ;; We could save a few bytes by skipping the mapcars,
           ;; but it would make the code uglier
           :instructions (apply #'concatenate 'vector
                                (mapcar #'instructions fasls))
           :attributes (apply #'concatenate 'vector
                              (mapcar #'attributes fasls))))))
