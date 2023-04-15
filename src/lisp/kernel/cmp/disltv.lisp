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
(defparameter *max-version* '(0 8))

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
   ;; obsolete as of 0.9.
   (%object-count :initarg :nobjs :reader object-count
                  :type (unsigned-byte 64))
   (%instructions :initarg :instructions :reader instructions
                  :type (simple-array t (*)))
   ;; obsolete as of 0.6.
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
  (let ((index (read-index stream)))
    (dbgprint " (nil ~d)" index)
    (setf (creator index)
          (make-instance 'singleton-creator :prototype nil))))

(defmethod %load-instruction ((mnemonic (eql 't)) stream)
  (let ((index (read-index stream)))
    (dbgprint " (t ~d)" index)
    (setf (creator index)
          (make-instance 'singleton-creator :prototype t))))

(defmethod %load-instruction ((mnemonic (eql 'cons)) stream)
  (let ((index (read-index stream)))
    (dbgprint " (cons ~d)" index)
    (setf (creator index)
          (make-instance 'cons-creator))))

(defmethod %load-instruction ((mnemonic (eql 'rplaca)) stream)
  (let ((cons (read-creator stream)) (value (read-creator stream)))
    (dbgprint " (rplaca ~s ~s)" cons value)
    (make-instance 'rplaca-init :cons cons :value value)))

(defmethod %load-instruction ((mnemonic (eql 'rplacd)) stream)
  (let ((cons (read-creator stream)) (value (read-creator stream)))
    (dbgprint " (rplacd ~s ~s)" cons value)
    (make-instance 'rplacd-init :cons cons :value value)))

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
    (dbgprint " (make-array ~d ~s ~s ~s)" index uaet packing-type dimensions)
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
  (let ((array (read-creator stream)) (index (read-ub16 stream))
        (value (read-creator stream)))
    (dbgprint " (setf (row-major-aref ~s ~d) ~s)" array index value)
    (make-instance 'setf-aref :array array :index index :value value)))

(defmethod %load-instruction ((mnemonic (eql 'make-hash-table)) stream)
  (let* ((index (read-index stream)) (testcode (read-byte stream))
         (test (ecase testcode
                 ((#b00) 'eq)
                 ((#b01) 'eql)
                 ((#b10) 'equal)
                 ((#b11) 'equalp)))
         (count (read-ub16 stream)))
    (dbgprint " (make-hash-table ~d ~s ~d)" index test count)
    (setf (creator index)
          (make-instance 'hash-table-creator :test test :count count))))

(defmethod %load-instruction ((mnemonic (eql 'setf-gethash)) stream)
  (let ((ht (read-creator stream))
        (key (read-creator stream)) (value (read-creator stream)))
    (dbgprint " (setf (gethash ~s ~s) ~s)" key ht value)
    (make-instance 'setf-gethash :hash-table ht :key key :value value)))

(defmethod %load-instruction ((mnemonic (eql 'make-sb64)) stream)
  (let ((index (read-index stream)) (fix (read-sb64 stream)))
    (dbgprint " (make-sb64 ~d ~d)" index fix)
    (setf (creator index)
          (make-instance 'sb64-creator :prototype fix))))

(defmethod %load-instruction ((mnemonic (eql 'find-package)) stream)
  (let ((index (read-index stream)) (name (read-creator stream)))
    (dbgprint " (find-package ~d ~s)" index name)
    (setf (creator index)
          (make-instance 'package-creator :name name))))

(defmethod %load-instruction ((mnemonic (eql 'make-bignum)) stream)
  (let* ((index (read-index stream))
         (ssize (read-sb64 stream)) (result 0) (size (abs ssize)))
    (loop repeat size
          do (let ((word (read-ub64 stream)))
               (setf result (logior (ash result 64) word))))
    (when (minusp ssize) (setf result (- result)))
    (dbgprint " (make-bignum ~d ~d ~d)" index ssize result)
    (setf (creator index) (make-instance 'bignum-creator :prototype result))))

(defmethod %load-instruction ((mnemonic (eql 'make-single-float)) stream)
  (let ((index (read-index stream))
        (float (ext:bits-to-single-float (read-ub32 stream))))
    (dbgprint " (make-single-float ~d ~e)" index float)
    (setf (creator index)
          (make-instance 'single-float-creator :prototype float))))

(defmethod %load-instruction ((mnemonic (eql 'make-double-float)) stream)
  (let ((index (read-index stream))
        (float (ext:bits-to-double-float (read-ub64 stream))))
    (dbgprint " (make-double-float ~d ~e)" index float)
    (setf (creator index)
          (make-instance 'double-float-creator :prototype float))))

(defmethod %load-instruction ((mnemonic (eql 'ratio)) stream)
  (let ((index (read-index stream))
        (num (read-creator stream)) (den (read-creator stream)))
    (dbgprint " (ratio ~d ~d ~d)" index num den)
    (setf (creator index)
          (make-instance 'ratio-creator :numerator num :denominator den))))

(defmethod %load-instruction ((mnemonic (eql 'complex)) stream)
  (let ((index (read-index stream))
        (real (read-creator stream)) (imag (read-creator stream)))
    (dbgprint " (complex ~d ~s ~s)" index real imag)
    (setf (creator index)
          (make-instance 'complex-creator :realpart real :imagpart imag))))

(defmethod %load-instruction ((mnemonic (eql 'make-symbol)) stream)
  (let ((index (read-index stream)) (name (read-creator stream)))
    (dbgprint " (make-symbol ~d ~s)" index name)
    (setf (creator index) (make-instance 'symbol-creator :name name))))

(defmethod %load-instruction ((mnemonic (eql 'intern)) stream)
  (let ((index (read-index stream))
        (pkg (read-creator stream)) (name (read-creator stream)))
    (dbgprint " (intern ~d ~s ~s)" index name pkg)
    (setf (creator index)
          (make-instance 'interned-symbol-creator
            :package pkg :name name))))

(defmethod %load-instruction ((mnemonic (eql 'make-character)) stream)
  (let ((index (read-index stream)) (char (code-char (read-ub32 stream))))
    (dbgprint " (make-character ~d ~:c)" index char)
    (setf (creator index)
          (make-instance 'character-creator :prototype char))))

(defmethod %load-instruction ((mnemonic (eql 'make-pathname)) stream)
  (let ((index (read-index stream))
        (host (read-creator stream)) (device (read-creator stream))
        (directory (read-creator stream)) (name (read-creator stream))
        (type (read-creator stream)) (version (read-creator stream)))
    (dbgprint " (make-pathname ~d ~s ~s ~s ~s ~s ~s)"
              index host device directory name type version)
    (setf (creator index)
          (make-instance 'pathname-creator
            :host host :device device
            :directory directory :name name
            :type type :version version))))

(defmethod %load-instruction ((mnemonic (eql 'make-bytecode-function)) stream)
  (let ((index (read-index stream))
        (entry-point (read-ub32 stream))
        (size (if (and (= *load-major* 0) (< *load-minor* 8))
                  0
                  (read-ub32 stream)))
        (nlocals (read-ub16 stream))
        (nclosed (read-ub16 stream)) (module (read-creator stream))
        (name (read-creator stream)) (lambda-list (read-creator stream))
        (docstring (read-creator stream)))
    (dbgprint " (make-bytecode-function ~d ~d ~d ~d ~d ~s ~s ~s ~s)"
              index entry-point size nlocals nclosed module
              name lambda-list docstring)
    (setf (creator index)
          (make-instance 'bytefunction-creator
            :entry-point entry-point :size size
            :nlocals nlocals :nclosed nclosed
            :module module
            :name name :lambda-list lambda-list
            :docstring docstring))))

(defmethod %load-instruction ((mnemonic (eql 'make-bytecode-module)) stream)
  (let ((index (read-index stream)) (len (read-ub32 stream)))
    (dbgprint " (make-bytecode-module ~d ~d)" index len)
    (setf (creator index)
          (let ((lispcode (make-array len :element-type '(unsigned-byte 8))))
            (read-sequence lispcode stream)
            (make-instance 'bytemodule-creator :lispcode lispcode)))))

(defmethod %load-instruction ((mnemonic (eql 'setf-literals)) stream)
  (if (and (= *load-major* 0) (<= *load-minor* 6))
      (let ((module (read-creator stream)) (literals (read-creator stream)))
        (dbgprint " (setf (literals ~s) ~s)" module literals)
        (make-instance 'setf-literals
          :module module :literals literals))
      (let* ((module (read-creator stream)) (nliterals (read-ub16 stream))
             (literals (make-array nliterals)))
        (loop for i below nliterals
              do (setf (aref literals i) (read-creator stream)))
        (dbgprint " (setf (literals ~s) ~s)" module literals)
        (make-instance 'setf-literals
          :module module :literals literals))))

(defmethod %load-instruction ((mnemonic (eql 'fdefinition)) stream)
  (let ((index (read-index stream)) (name (read-creator stream)))
    (dbgprint " (fdefinition ~d ~s)" index name)
    (setf (creator index) (make-instance 'fdefinition-lookup :name name))))

(defmethod %load-instruction ((mnemonic (eql 'funcall-create)) stream)
  (let ((index (read-index stream)) (fun (read-creator stream))
        (args (if (and (= *load-major* 0) (<= *load-minor* 4))
                  ()
                  (loop repeat (read-ub16 stream)
                        collect (read-creator stream)))))
    (dbgprint " (funcall-create ~d ~s~{ ~s~})" index fun args)
    (setf (creator index)
          (make-instance 'general-creator
            :function fun :arguments args))))

(defmethod %load-instruction ((mnemonic (eql 'funcall-initialize)) stream)
  (let ((fun (read-creator stream))
        (args (if (and (= *load-major* 0) (<= *load-minor* 4))
                  ()
                  (loop repeat (read-ub16 stream)
                        collect (read-creator stream)))))
    (dbgprint " (funcall-initialize ~s~{ ~s~})" fun args)
    (make-instance 'general-initializer :function fun :arguments args)))

(defmethod %load-instruction ((mnemonic (eql 'find-class)) stream)
  (let ((index (read-index stream)) (name (read-creator stream)))
    (dbgprint " (find-class ~d ~s)" index name)
    (setf (creator index) (make-instance 'class-creator :name name))))

(defmethod %load-instruction ((mnemonic (eql 'init-object-array)) stream)
  (let ((nobjs (read-ub64 stream)))
    (dbgprint " (init-object-array ~d)" nobjs)
    (setf *index-bytes* (max (ash 1 (1- (ceiling (integer-length nobjs) 8)))
                             1))
    (make-instance 'init-object-array :count nobjs)))

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
  ;; skip. we could record this as a pseudo-attribute, hypothetically
  (let ((nbytes (read-ub32 stream)))
    (dbgprint " (unknown-attribute ~s ~d)" ncreator nbytes)
    (loop repeat nbytes do (read-byte stream))))

;;; Results from non-string (invalid) attribute name.
(defmethod %load-attribute ((mnemonic null) ncreator stream)
  (let ((nbytes (read-ub32 stream)))
    (dbgprint " (unknown-attribute ~s ~d)" ncreator nbytes)
    (loop repeat nbytes do (read-byte stream))))

#+clasp
(defmethod %load-attribute ((mnemonic (eql 'source-pos-info)) ncreator stream)
  (let ((nbytes (read-ub32 stream))
        (fun (read-creator stream)) (path (read-creator stream))
        (line (read-ub64 stream)) (col (read-ub64 stream))
        (pos (read-ub64 stream)))
    (dbgprint " (source-pos-info ~s ~d ~s ~s ~d ~d ~d)"
              ncreator nbytes fun path line col pos)
    (make-instance 'spi-attr
      :name ncreator :function fun :pathname path
      :lineno line :column col :filepos pos)))

#+clasp
(defmethod %load-attribute ((mnemonic (eql 'module-debug-info)) ncreator stream)
  (let ((nbytes (read-ub32 stream))
        (mod (read-creator stream))
        (ncfunctions (read-ub16 stream)))
    (dbgprint " (module-debug-info ~s ~d ~s ~d)"
              ncreator nbytes mod ncfunctions)
    (let ((cfunctions (loop repeat ncfunctions
                            collect (read-creator stream)))
          (vars (loop repeat (read-ub32 stream)
                      collect (list (read-ub32 stream)
                                    (read-ub32 stream)
                                    (loop repeat (read-ub16 stream)
                                          collect (list (read-creator stream)
                                                        (ecase (read-byte stream)
                                                          (0 nil)
                                                          (1 t))
                                                        (read-ub16 stream)))))))
      (make-instance 'module-debug-attr
        :name ncreator :module mod :cfunctions cfunctions
        :vars vars))))


(defparameter *attr-map*
  (let ((ht (make-hash-table :test #'equal)))
    #+clasp (setf (gethash "clasp:source-pos-info" ht) 'source-pos-info)
    #+clasp (setf (gethash "clasp:module-debug-info" ht) 'module-debug-info)
    ht))

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

(defmethod %load-instruction ((mnemonic (eql 'attribute)) stream)
  (load-attribute stream))

;;;

(defun load-bytecode-stream (stream)
  (load-magic stream)
  (multiple-value-bind (*load-major* *load-minor*) (load-version stream)
    (let* ((nobjs (if (and (= *load-major* 0) (< *load-minor* 9))
                      (read-ub64 stream)
                      0))
           (*index-bytes* (max (ash 1 (1- (ceiling (integer-length nobjs) 8)))
                               1))
           (_1 (dbgprint "File reports ~d objects. Index length = ~d bytes."
                         nobjs *index-bytes*))
           (ninsts (read-ub64 stream))
           (_2 (dbgprint "Executing FASL bytecode~%File reports ~d instructions"
                         ninsts))
           ;; Unlike in the compiler, here instructions and attributes
           ;; are vectors, and in forward order.
           (*instructions* (make-array ninsts))
           (*index-to-creator* (make-array nobjs :initial-element nil))
           (_3
             (loop for i below ninsts
                   do (setf (aref *instructions* i)
                            (load-instruction stream))))
           (nattrs
             (when (and (= *load-major* 0) (< *load-minor* 6))
               (read-ub32 stream)))
           (*attributes*
             (when (and (= *load-major* 0) (< *load-minor* 6))
               (make-array nattrs))))
      (declare (ignore _1 _2 _3))
      (when (and (= *load-major* 0) (< *load-minor* 6))
        (dbgprint "File reports ~d attributes" nattrs)
        (loop for i below nattrs
              do (setf (aref *attributes* i) (load-attribute stream))))
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
  (write-bytecode (instructions fasl) stream))

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

(defun concatenate-fasl-files (&rest pathnames)
  (apply #'concatenate-fasls (mapcar #'load-bytecode pathnames)))

(defun link-bytecode-fasl-files (output-path input-paths)
  (save-fasl (apply #'concatenate-fasl-files input-paths)
             output-path))
