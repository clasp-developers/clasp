(in-package #:cleavir-ir-gml)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a node in the stream

(defun render-node (stream id name &key (color "white") (shape "rectangle"))
  (format stream "  node~%")
  (format stream "  [~%")
  (format stream "    id ~a~%" id)
  (format stream "    label ~s~%" (format nil "~a:~a" name id))
  (format stream "    graphics~%")
  (format stream "    [~%")
  (format stream "      type ~s~%" (string-downcase (string shape)))
  (format stream "      fill ~s~%" (string-downcase (string color)))
  (format stream "    ]~%")
  (format stream "  ]~%"))

(defun render-edge (stream source target label &key type)
  (when source
    (format stream "  edge~%")
    (format stream "  [~%")
    (format stream "    source ~a~%" source)
    (format stream "    target ~a~%" target)
    (format stream "    label ~s~%" label)
    (format stream "    graphics~%")
    (format stream "    [~%")
    (format stream "      width 2~%")
    (format stream "      type ~s~%" "line")
    (format stream "    ]~%")
    (format stream "  ]~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a datum on a stream.

(defgeneric draw-datum (datum stream))

;;; During the drawing process, the value of this variable is a hash
;;; table that contains data that have already been drawn.
(defparameter *datum-table* nil)

(defun datum-id (datum)
  #+(or)(clasp-cleavir:datum-gid datum)
  (gethash datum *datum-table*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum of unknown type

(defmethod draw-datum (datum stream)
  (break "problem with datum")
  (render-node stream "UNKNOWN-ID" "UNKNOWN-VAL" :color :red))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum CONSTANT-INPUT.

(defmethod draw-datum ((datum constant-input) stream)
  (render-node stream (datum-id datum) (value datum) :color :green))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum LEXICAL-LOCATION.

(defmethod draw-datum ((datum lexical-location) stream)
  (render-node stream (datum-id datum) (name datum) :color :yellow))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum VALUES-LOCATION.

(defmethod draw-datum ((datum values-location) stream)
  (render-node stream (datum-id datum) "V" :color :blue :shape :oval))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum IMMEDIATE-INPUT.

(defmethod draw-datum ((datum immediate-input) stream)
  (render-node stream (datum-id datum) (value datum) :color :cyan :shape :oval))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum LOAD-TIME-VALUE-INPUT.

(defmethod draw-datum ((datum load-time-value-input) stream)
  (render-node stream (datum-id datum) (form datum) :color :orange :shape :oval))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing instructions.

(defparameter *instruction-table* nil)

(defun instruction-id (instruction)
  #+(or)(clasp-cleavir:instruction-gid instruction)
  (gethash instruction *instruction-table*))

(defgeneric draw-instruction (instruction stream))

(defmethod draw-instruction :around (instruction stream)
  ;; Draw a numbered bold arrow to each successor.
  ;; Draw a numbered red dashed arrow from each input.
  #+(or)(loop for datum in (inputs instruction)
	for i from 1
	do (draw-datum datum stream))
  (call-next-method))

(defgeneric label (instruction))


(defmethod label (instruction)
  (class-name (class-of instruction)))

(defmethod draw-instruction (instruction stream)
  (render-node stream (instruction-id instruction)
	       (label instruction)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draw the edges
;;;
(defgeneric draw-instruction-edges (instruction-edges stream))

(defmethod draw-instruction-edges (instruction stream)
  nil)


(defmethod draw-instruction-edges :around (instruction stream)
  ;; Draw a numbered bold arrow to each successor.
  (loop for next in (successors instruction)
     for i from 1
     do (render-edge stream (instruction-id instruction) (instruction-id next) (format nil "~a" i) :type :flow))
  ;; Draw a numbered red dashed arrow from each input.
  (loop for datum in (inputs instruction)
	for i from 1
     do (render-edge stream (datum-id datum) (instruction-id instruction) (format nil "~a" i) :type :input))
  ;; Draw a numbered blue dashed arrow to each output
  (loop for datum in (outputs instruction)
     for i from 1
     do (render-edge stream (datum-id datum) (instruction-id instruction) (format nil "~a" i) :type :output)))



(defun draw-flowchart (initial-instruction filename)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede)
    (let ((*instruction-table* (make-hash-table :test #'eq))
	  (*datum-table* (make-hash-table :test #'eq)))
      (format stream "Creator \"meister\"~%")
      (format stream "Version 1.0~%")
      (format stream "graph~%")
      (format stream "[~%")
      (format stream " label \"\"~%")
      (format stream " directed 1~%")
      ;; Assign a unique ID to each instruction and each datum.
      (let ((id 0)
	    instructions
	    datums)
	(map-instructions-arbitrary-order
	 (lambda (instruction)
	   (when (null (gethash instruction *instruction-table*))
	     (let ((tid (incf id)))
	       (setf (gethash instruction *instruction-table*) tid)
	       (push instruction instructions))
	     (loop for datum in (append (inputs instruction) (inputs instruction))
		when (null (gethash datum *datum-table*))
		do (let ((tid (incf id)))
		     (setf (gethash datum *datum-table*) tid)
		     (push datum datums)))))
	 initial-instruction)
	(dolist (instr instructions)
	  (draw-instruction instr stream))
	(dolist (datum datums)
	  (draw-datum datum stream))
	;; Draw all edges
	(map-instructions-arbitrary-order
	 (lambda (instruction)
	   (draw-instruction-edges instruction stream))
	 initial-instruction)
	;; Draw a START label to indentify the initial instruction.
	(render-node stream 0 "START")
	(render-edge stream 0 (instruction-id initial-instruction) "1")
	(format stream "]~%"))
      (values *instruction-table* *datum-table*)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General-purpose instructions.

(defmethod draw-instruction ((instruction typeq-instruction) stream)
  (render-node stream (instruction-id instruction) (format nil "typeq ~a" (value-type instruction))))

(defun format-item (item)
  (cond ((symbolp item)
	 item)
	((listp item)
	 (mapcar #'format-item item))
	((typep item 'cleavir-ir:lexical-location)
	 (cleavir-ir:name item))
	(t
	 (error "unknown item in lambda list ~s" item))))

(defmethod label ((instruction enter-instruction))
  (with-output-to-string (stream)
    (format stream "enter ~a"
	    (mapcar #'format-item (cleavir-ir:lambda-list instruction)))))

(defmethod label ((instruction dynamic-allocation-instruction))
  "DX")

(defmethod label ((instruction nop-instruction)) "nop")

(defmethod label ((instruction assignment-instruction)) "<-")

(defmethod label ((instruction funcall-instruction)) "funcall")

(defmethod label ((instruction tailcall-instruction)) "tailcall")

(defmethod label ((instruction return-instruction)) "ret")

(defmethod label ((instruction fdefinition-instruction)) "fdefinition")

(defmethod draw-instruction ((instruction enclose-instruction) stream)
  (render-node stream (instruction-id instruction) "enclose"))

(defmethod draw-instruction-edges ((instruction enclose-instruction) stream)
  (render-edge stream (gethash (code instruction) *instruction-table*)
	       (instruction-id instruction) :type :enclose))


(defmethod draw-instruction ((instruction unwind-instruction) stream)
  (render-node stream (instruction-id instruction) "unwind"))

(defmethod draw-instruction-edges ((instruction unwind-instruction) stream)
  (render-edge stream (instruction-id instruction)
	       (gethash (destination instruction) *instruction-table*)
	       :type :unwind))

(defmethod label ((instruction catch-instruction)) "catch")

(defmethod label ((instruction eq-instruction)) "eq")

(defmethod label ((instruction consp-instruction)) "consp")

(defmethod label ((instruction fixnump-instruction)) "fixnump")

(defmethod label ((instruction phi-instruction)) "phi")

(defmethod label ((instruction symbol-value-instruction)) "symbol-value")

(defmethod label ((instruction set-symbol-value-instruction)) "set-symbol-value")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fixnum instructions.

(defmethod label ((instruction fixnum-add-instruction)) "fixnum +")

(defmethod label ((instruction fixnum-sub-instruction)) "fixnum -")

(defmethod label ((instruction fixnum-less-instruction)) "fixnum <")

(defmethod label ((instruction fixnum-not-greater-instruction)) "fixnum <=")

(defmethod label ((instruction fixnum-equal-instruction)) "fixnum =")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Floating-point arithmetic instructions.

(defmethod label ((instruction float-add-instruction)) "float +")
(defmethod label ((instruction float-sub-instruction)) "float -")
(defmethod label ((instruction float-mul-instruction)) "float *")
(defmethod label ((instruction float-div-instruction)) "float /")
(defmethod label ((instruction float-sin-instruction)) "float sin")
(defmethod label ((instruction float-cos-instruction)) "float cos")
(defmethod label ((instruction float-sqrt-instruction)) "float sqrt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General accessors.

(defmethod label ((instruction car-instruction)) "car")

(defmethod label ((instruction cdr-instruction)) "cdr")

(defmethod label ((instruction rplaca-instruction)) "rplaca")

(defmethod label ((instruction rplacd-instruction)) "rplacd")

(defmethod label ((instruction slot-read-instruction)) "slot-read")

(defmethod label ((instruction slot-write-instruction)) "slot-write")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions for multiple values.

(defmethod label ((instruction multiple-to-fixed-instruction)) "M->F")

(defmethod label ((instruction fixed-to-multiple-instruction)) "F->M")

(defmethod label ((instruction multiple-value-call-instruction)) "MVC")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions for boxing.

(defmethod label ((instruction box-instruction))
  (format nil "box ~a" (element-type instruction)))

(defmethod label ((instruction unbox-instruction))
  (format nil "unbox ~a" (element-type instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Array-related instructions.

(defmethod label ((instruction aref-instruction))
  (format nil "~:[non-simple~;simple~] ~s aref"
	  (simple-p instruction)
	  (element-type instruction)))

(defmethod label ((instruction aset-instruction))
  (format nil "~:[non-simple~;simple~] ~s aset"
	  (simple-p instruction)
	  (element-type instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions related to the static runtime environment.

(defmethod label ((instruction create-cell-instruction)) "Create cell")

(defmethod label ((instruction fetch-instruction)) "Fetch")

(defmethod label ((instruction read-cell-instruction)) "Read cell")

(defmethod label ((instruction write-cell-instruction)) "Write cell")

(defmethod label ((instruction add-activation-record-instruction)) "AddAR")

(defmethod label ((instruction remove-activation-record-instruction)) "RemAR")

(defmethod label ((instruction load-from-static-environment-instruction))
  "Load")

(defmethod label ((instruction store-to-static-environment-instruction))
  "Store")
