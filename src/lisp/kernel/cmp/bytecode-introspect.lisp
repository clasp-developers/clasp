(in-package #:cmp)

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

;;; Return a list of all IPs that are jumped to.
(defun gather-labels (bytecode)
  (let ((ip 0)
        (end (length bytecode))
        (result nil)
        (longp nil)
        op)
    (loop (setq op (cmpref:decode-instr (aref bytecode ip)))
          ;; If the opcode is illegal, stop.
          (when (null op)
            (return (sort result #'<)))
          ;; Go through the arguments, identifying any labels.
          (let ((opip (if longp (1- ip) ip))) ; IP of instruction start
            (incf ip)
            (dolist (argi (if longp (fourth op) (third op)))
              (let ((nbytes (cmpref:unmask-arg argi)))
                (if (cmpref:label-arg-p argi)
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
    (loop (setq op (cmpref:decode-instr (aref bytecode ip)))
          ;; If this is a label position, mark that.
          (let ((labelpos (position ip labels)))
            (if labelpos (push (write-to-string labelpos) result)))
          ;; If we have an illegal opcode, record it and then give up
          ;; (as we have lost the instruction stream)
          (when (null op)
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
                               (let ((nbytes (cmpref:unmask-arg argi)))
                                 (push
                                  (cond ((cmpref:constant-arg-p argi)
                                         (list :constant
                                               (bc-unsigned bytecode ip nbytes)))
                                        ((cmpref:label-arg-p argi)
                                         (let* ((lip (+ opip (bc-signed bytecode ip nbytes)))
                                                (lpos (position lip labels)))
                                           (assert lpos)
                                           (list :label lpos)))
                                        ((cmpref:keys-arg-p argi)
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
  (let* ((simple bcfunction)
         (module (core:bytecode-simple-fun/code simple))
         (start (core:bytecode-simple-fun/entry-pc-n simple))
         (length (core:bytecode-simple-fun/bytecode-size simple)))
    (disassemble-bytecode module
                          :start start :length length :labels labels
                          :function-name (core:function-name bcfunction)))
  (values))

(defun disassemble-bytecode-function (bcfunction)
  (let* ((disassembled-functions nil) ; prevent recursion
         (simple bcfunction)
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

;;; ------------------------------------------------------------
;;;
;;; Other introspection
;;;


(defun bytecode-next-arg (argspec bytecode opip ip nbytes)
  (cond
    ((cmpref:constant-arg-p argspec)
     (cons :constant (bc-unsigned bytecode ip nbytes)))
    ((cmpref:label-arg-p argspec)
     (cons :label (+ opip (bc-signed bytecode ip nbytes))))
    ((cmpref:keys-arg-p argspec)
     (cons :keys (bc-unsigned bytecode ip nbytes)))
    (t (cons :operand (bc-unsigned bytecode ip nbytes)))))

(defun collect-pka-args (bytecode ip nbytes)
  ;; parse-key-args is eccentric, so we special case it.
  ;; we have more-start, key-count-info, key-literal-start, key-frame-start.
  ;; the first is an index into the arguments, the second is weird, the third
  ;; is an index into the literals that's used a bit differently than usual,
  ;; and the last is an index into the frame.
  (let* ((more-start
           (prog1 (bc-unsigned bytecode ip nbytes)
             (incf ip nbytes)))
         (key-count-info
           (prog1 (bc-unsigned bytecode ip nbytes)
             (incf ip nbytes)))
         (key-count (ash key-count-info -1))
         (aokp (logbitp 0 key-count-info))
         (key-literal-start
           (prog1 (bc-unsigned bytecode ip nbytes)
             (incf ip nbytes))))
    (list (cons :operand more-start)
          (cons :key-count-info (cons key-count aokp))
          (cons :keys key-literal-start))))

;;; Compute a list of annotations that start at the given IP.
;;; Return the list, and the index of the next annotation.
(defun new-annotations (annotations index ip)
  (values
   (loop with len = (length annotations)
         while (< index len)
         while (<= (core:bytecode-debug-info/start (aref annotations index)) ip)
         when (= (core:bytecode-debug-info/start (aref annotations index)) ip)
           collect (aref annotations index)
         do (incf index))
   index))

(defmacro do-instructions ((mnemonic args opip ip
                            &optional (annots (gensym "ANNOTATIONS")))
                           (bytecode &key (start 0) end annotations)
                           &body body)
  (let ((bsym (gensym "BYTECODE"))
        (gend (gensym "END"))
        (longp (gensym "LONGP"))
        (gannotations (gensym "ANNOTATIONS"))
        (next-annotation-index (gensym "NEXT-ANNOTATION-INDEX"))
        (op (gensym "OP")))
    `(loop with ,bsym = ,bytecode
           with ,ip = ,start
           with ,longp = nil
           with ,gend = ,(or end `(+ ,ip (length ,bsym)))
           with ,gannotations = ,annotations
           with ,next-annotation-index = 0
           with ,annots = nil
           for ,op = (cmpref:decode-instr (aref ,bsym ,ip))
           for ,mnemonic = (intern (string-upcase (first ,op)) "KEYWORD")
           if (eql ,mnemonic :long)
             do (setf ,longp t ,ip (1+ ,ip))
           else
             do (let ((,opip ,ip))
                  (incf ,ip)
                  (let ((,args
                          (if (eq ,mnemonic :parse-key-args)
                              (let ((nbytes (if ,longp 2 1)))
                                (prog1 (collect-pka-args ,bsym ,ip nbytes)
                                  (incf ,ip (* 3 nbytes))))
                              (loop for argspec
                                      in (if ,longp (fourth ,op) (third ,op))
                                    for nbytes = (cmpref:unmask-arg argspec)
                                    collect (bytecode-next-arg argspec ,bsym ,opip ,ip
                                                               nbytes)
                                    do (incf ,ip nbytes)))))
                    (declare (ignorable ,args ,ip))
                    (setf (values ,annots ,next-annotation-index)
                          (new-annotations ,gannotations
                                           ,next-annotation-index ,ip))
                    ,@body
                    (setf ,longp nil)))
           until (>= ,ip ,gend))))

(defmacro do-module-instructions ((mnemonic args opip ip
                                   &optional (annots (gensym "ANNOTATIONS")))
                                  (module)
                                  &body body)
  (let ((gmodule (gensym "MODULE")))
    `(let ((,gmodule ,module))
       (do-instructions (,mnemonic ,args ,opip ,ip ,annots)
         ((core:bytecode-module/bytecode ,gmodule)
          :annotations (core:bytecode-module/debug-info ,gmodule))
         ,@body))))
