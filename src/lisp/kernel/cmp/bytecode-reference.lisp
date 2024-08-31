(in-package :cmpref)

(export '(constant-arg-p label-arg-p) :cmpref)

;;; ------------------------------------------------------------
;;;
;;; disassembler
;;;
;;;

(defun early-mask-field (size position integer)
  (logand (ash (lognot (ash -1 size))
	       position)
	  integer))

(defun dis-signed (x size)
  (logior x (- (early-mask-field 1 (1- size) x))))

(defun bc-unsigned (bytecode ip nbytes)
  ;; Read NBYTES of little-endian integer.
  (do* ((i 0 (1+ i))
        (s 0 (+ 8 s))
        (sum 0))
       ((= i nbytes) sum)
    (incf sum (ash (aref bytecode (+ ip i)) s))))

(defun bc-signed (bytecode ip nbytes)
  (dis-signed (bc-unsigned bytecode ip nbytes)
              (* 8 nbytes)))

(defun constant-arg-p (val)
  (= (logand +mask-arg+ val) +constant-arg+))

(defun label-arg-p (val)
  (= (logand +mask-arg+ val) +label-arg+))

(defun keys-arg-p (val)
  (= (logand +mask-arg+ val) +keys-arg+))

;;; *full-codes* contains descriptions of the instructions in the following format:
;;; (name opcode (args...) (long-args...))
;;; the name is a string.
;;; the args and long args are encoded as a number of bytes from 1 to 3, LOGIOR'd
;;; with the constant, label, and keys code that is appropriate, if any.
;;; One of these "instruction description" lists is what DECODE-INSTR returns.

(defun decode-instr (opcode)
  (let ((res (member opcode *full-codes* :key #'second)))
    (if res
        (first res)
        'illegal)))

;;; Return a list of all IPs that are jumped to.
(defun gather-labels (bytecode)
  (let ((ip 0)
        (end (length bytecode))
        (result nil)
        (longp nil)
        op)
    (loop (setq op (decode-instr (aref bytecode ip)))
          ;; If the opcode is illegal, stop.
          (when (eq op 'illegal)
            (return (sort result #'<)))
          ;; Go through the arguments, identifying any labels.
          (let ((opip (if longp (1- ip) ip))) ; IP of instruction start
            (incf ip)
            (dolist (argi (if longp (fourth op) (third op)))
              (let ((nbytes (logandc2 argi +mask-arg+)))
                (if (label-arg-p argi)
                    (push (+ opip (bc-signed bytecode ip nbytes)) result))
                (incf ip nbytes))))
          ;; If this is a LONG, set that for the next instruction.
          ;; (KLUDGE)
          ;; Otherwise reset longp to false.
          (setq longp (string= (first op) "long"))
          (if (>= ip end) (return (sort result #'<))))))

(defun %disassemble-bytecode (bytecode start length labels)
  (let* ((ip start)
         (end (+ start length))
         (result nil)
         (longp nil)
         op)
    (loop (setq op (decode-instr (aref bytecode ip)))
          ;; If this is a label position, mark that.
          (let ((labelpos (position ip labels)))
            (if labelpos (push (write-to-string labelpos) result)))
          ;; If we have an illegal opcode, record it and then give up
          ;; (as we have lost the instruction stream)
          (when (eq op 'illegal)
            (push (list (format nil "! ILLEGAL OPCODE #x~x" (aref bytecode ip)) nil nil) result)
            (return (nreverse result)))
          ;; Decode the instruction. If it's LONG, leave it to the next. KLUDGE
          (let ((opip (if longp (1- ip) ip)))
            (incf ip)
            (cond
              ((string= (first op) "long") (setq longp t))
              (t
               (push (list (first op) longp
                           (let ((args nil))
                             (dolist (argi (if longp (fourth op) (third op))
                                           (nreverse args))
                               (let ((nbytes (logandc2 argi +mask-arg+)))
                                 (push
                                  (cond ((constant-arg-p argi)
                                         (list :constant
                                               (bc-unsigned bytecode ip nbytes)))
                                        ((label-arg-p argi)
                                         (let* ((lip (+ opip (bc-signed bytecode ip nbytes)))
                                                (lpos (position lip labels)))
                                           (assert lpos)
                                           (list :label lpos)))
                                        ((keys-arg-p argi)
                                         (list :keys
                                               (bc-unsigned bytecode ip nbytes)))
                                        (t
                                         (list :operand
                                               (bc-unsigned bytecode ip nbytes))))
                                  args)
                                 (incf ip nbytes)))))
                     result)
               (setq longp nil))))
          (if (>= ip end) (return (nreverse result))))))

(defun disassemble-parse-key-args (name longp args literals)
  ;; We special case this despite the keys-arg thing because it's
  ;; just pretty weird all around.
  (let* ((more-start (second (first args)))
         (kci (second (second args)))
         (aokp (logbitp 0 kci))
         (key-count (ash kci -1))
         (keystart (second (third args)))
         (keys nil))
    ;; Gather the keys
    (do ((i 0 (1+ i)))
        ((= i key-count) (setq keys (nreverse keys)))
      (push (aref literals (+ keystart i)) keys))
    ;; Print
    (format t "~&  ~:[~;long ~]~a~:[~;-aok~] ~d ~d '~s"
            longp name aokp more-start key-count keys)))

(defvar *functions-to-disassemble*)

(defun disassemble-bytecode (module
                             &key (start 0) length labels
                               (function-name nil fnp))
  (let* ((bytecode (core:bytecode-module/bytecode module))
         (literals (core:bytecode-module/literals module))
         (length (or length (length bytecode)))
         (dis (%disassemble-bytecode bytecode start length labels)))
    (flet ((textify-operand (thing)
             (destructuring-bind (kind value) thing
               (cond ((eq kind :constant)
                      (let ((lit (aref literals value)))
                        ;; This may not be the best place for this check,
                        ;; but here we check for enclosed functions.
                        (when (and (typep lit 'core:bytecode-simple-fun)
                                   (eq (core:bytecode-simple-fun/code lit)
                                       module)
                                   (boundp '*functions-to-disassemble*))
                          (push lit *functions-to-disassemble*))
                        (format nil "'~s" (aref literals value))))
                     ((eq kind :label) (format nil "L~a" value))
                     ((eq kind :operand) (format nil "~d" value))
                     ;; :keys special cased below
                     (t (error "Illegal kind ~a" kind))))))
      (when fnp
        (format t "function ~s~%" function-name))
      (dolist (item dis)
        (cond
          ((consp item)
           ;; instruction
           (destructuring-bind (name longp args) item
             (if (string= name "parse-key-args")
                 (disassemble-parse-key-args name longp args literals)
                 (format t "~&  ~:[~;long ~]~a~{ ~a~}~%"
                         longp name (mapcar #'textify-operand args)))))
          ((or (stringp item) (symbolp item))
           ;; label
           (format t "~&L~a:~%" item))
          (t (error "Illegal item ~a" item))))))
  (values))

(defun %disassemble-bytecode-function (bcfunction labels)
  (let* ((simple (core:function/entry-point bcfunction))
         (module (core:bytecode-simple-fun/code simple))
         (start (core:bytecode-simple-fun/entry-pc-n simple))
         (length (core:bytecode-simple-fun/bytecode-size simple)))
    (disassemble-bytecode module
                          :start start :length length :labels labels
                          :function-name (core:function-name bcfunction)))
  (values))

(defun disassemble-bytecode-function (bcfunction)
  (let* ((disassembled-functions nil) ; prevent recursion
         (simple (core:function/entry-point bcfunction))
         (module (core:bytecode-simple-fun/code simple))
         (bytecode (core:bytecode-module/bytecode module))
         ;; We grab labels for the entire module, so that nonlocal exit points
         ;; are noted completely and deterministically.
         (labels (gather-labels bytecode))
         (*functions-to-disassemble* (list bcfunction)))
    (loop (let ((fun (pop *functions-to-disassemble*)))
            (unless (member fun disassembled-functions)
              (push fun disassembled-functions)
              (%disassemble-bytecode-function fun labels)))
          (when (null *functions-to-disassemble*)
            (return (values))))))

(export 'disassemble-bytecode-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Now the Generic-function dtree interpreter virtual machine instructions
;;;

#-sbcl(in-package :clos)
#+sbcl
(defpackage :clos
  (:use #:common-lisp ))

(defun dtree-op-byte-length (dtree-op long)
  (1+ (if long
          (let ((sum-bytes 0))
            (dolist (arg (dtree-op-long-arguments dtree-op))
              (let ((bytes (second arg)))
                (incf sum-bytes bytes)))
            sum-bytes)
          (let ((sum-bytes 0))
            (dolist (arg (dtree-op-arguments dtree-op))
              (incf sum-bytes (second arg)))
            sum-bytes))))

(eval-when (:execute :load-toplevel)
  ;; Ensure the dtree VM defined here is consistent with the one in C++
  (clos:validate-dtree-bytecode-vm (length *dtree-ops*)))


(export '(dump-gf-bytecode-virtual-machine
          dump-python-gf-bytecode-virtual-machine) :clos)
