(in-package :cmp)

;;; (lambda (x y) ...)
;;; (lambda (x y &optional (a ai as)) ...)

(defun transform-optionals (optargs)
  (let (opts optassign)
    (do* ((cur (cdr optargs) (cdddr cur))
          (optname (car cur) (car cur))
          (optinit (cadr cur) (cadr cur))
          (optp (caddr cur) (caddr cur)))
         ((null cur)
          (progn
            (values (nreverse opts) (nreverse optassign))))
      (let ((optgs (gensym "OPT"))
            (optpgs (gensym "OPTP")))
        (push (list optgs optpgs) opts)
        (push (list optname `(if ,optpgs ,optgs ,optinit)) optassign)
        (when optp (push (list optp optpgs) optassign))))))

(defun transform-keys (keyargs)
  (let (keys keyassign)
    (do* ((cur (cdr keyargs) (cddddr cur))
          (keykw (car cur) (car cur))
          (keyname (second cur) (second cur))
          (keyinit (third cur) (third cur))
          (keyp (fourth cur) (fourth cur)))
         ((null cur)
          (progn
            (values (nreverse keys) (nreverse keyassign))))
      (let ((keygs (gensym "KEY"))
            (keypgs (gensym "KEYP")))
        (push (list keykw #++(list keykw keyname) keygs keypgs) keys)
        (push (list keyname `(if ,keypgs ,keygs ,keyinit)) keyassign)
        (when keyp (push (list keyp keypgs) keyassign))))))

(defun compute-rest-alloc (restvar dspecs)
  (cond ((not restvar) nil)
        ((member `(ignore ,restvar) dspecs :test #'equal) 'ignore)
        ((member `(dynamic-extent ,restvar) dspecs :test #'equal) 'dynamic-extent)
        (t nil)))

;;;
;;; Transform the lambda form into a cleavir style lambda list
;;; and a LET* that binds lexical variables in registers into bclasp
;;; variables in an activation frame

(defun transform-lambda-parts (lambda-list declares code)
  "Transform the lambda-list into a cleavir-lambda-list and wrap the code in a let.
Return (values cleavir-lambda-list wrapped-code rest-alloc)."
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys auxargs varest-p)
      (core:process-lambda-list lambda-list 'function)
    (let ((creqs (mapcar (lambda (x) (declare (ignore x)) (gensym "REQ"))
                         (cdr reqargs)))
          (crest (if rest-var (gensym "REST") nil)))
      (multiple-value-bind (copts opt-assign)
          (transform-optionals optargs)
        (multiple-value-bind (ckeys key-assign)
            (transform-keys keyargs)
          (let ((aux-args-pairs (let (ta)
                                  (if auxargs
                                      (do* ((cur auxargs (cddr cur))
                                            (var (car cur) (car cur))
                                            (val (cadr cur) (cadr cur)))
                                           ((null cur))
                                        (push (list var val) ta)))
                                  (nreverse ta))))
            (let* ((cleavir-lambda-list
                     `(,@creqs
                       ,@(if copts (list* '&optional copts) nil)
                       ,@(if crest (list (if varest-p 'core:&va-rest '&rest) crest) nil)
                       ,@(if key-flag (list* '&key ckeys) nil)
                       ,@(if allow-other-keys (list '&allow-other-keys) nil)))
                   (assignments (append
                                 (mapcar (lambda (req creq) (list req creq)) (cdr reqargs) creqs)
                                 opt-assign
                                 (if rest-var (list (list rest-var crest)) nil)
                                 key-assign
                                 aux-args-pairs))
                   (cleavir-body `(let* (,@assignments)
                                    ,@(if declares (list `(declare ,@declares)) nil)
                                    ,@code)))
              (values cleavir-lambda-list cleavir-body
                      (compute-rest-alloc rest-var declares)))))))))


#+(or)(defun cleavir-arguments (cleavir-lambda-list)
  "Return the list of argument names for the cleavir-lambda-list"
  (let ((arglist '()))
    (dolist (item cleavir-lambda-list)
               (unless (symbolp item)
                 (if (consp item)
                     (case (length item)
                       (2
                        (push (first item) arglist)
                        (push (second item) arglist))
                       (3
                        (push (second item) arglist)
                        (push (third item) arglist))
                       (otherwise (error "Illegal length ~a" (length item))))
                     (push item arglist))))
    (nreverse arglist)))



(defun lambda-list-arguments (lambda-list)
  (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys auxargs varest-p)
      (core:process-lambda-list lambda-list 'function)
    (cmp-log "reqargs = %s%N" reqargs)
    (cmp-log "optargs = %s%N" optargs)
    (cmp-log "rest-var = %s%N" rest-var)
    (cmp-log "keyargs = %s%N" keyargs)
    (let ((args '()))
      (dolist (req (rest reqargs))
        (cmp-log "req-name = %s%N" req)
        (push req args))
      (do ((cur (rest optargs) (cdddr cur)))
          ((null cur) nil)
        (let ((opt-name (car cur))
              (opt-flag (cadr cur)))
          (cmp-log "opt cur = %s%N" cur)
          (cmp-log "opt-name = %s%N" opt-name)
          (cmp-log "opt-flag = %s%N" opt-flag)
          (push opt-name args)
          (when opt-flag (push opt-flag args))))
      (when rest-var (push rest-var args))
      (do ((cur (rest keyargs) (cddddr cur)))
          ((null cur) nil)
        (let ((key-name (caddr cur))
              (key-flag (cadddr cur)))
          (cmp-log "key-name = %s%N" key-name)
          (cmp-log "key-flag = %s%N" key-flag)
          (push key-name args)
          (when key-flag (push key-flag args))))
      (nreverse args))))
        
  
#| (#:REQ8085 #:REQ8086 &OPTIONAL (#:OPT8088 #:OPTP8089) &VA-REST #:REST8087 &KEY
 (:A #:KEY8090 #:KEYP8091) (:B #:KEY8092 #:KEYP8093) (:C #:KEY8094 #:KEYP8095))
(LET ((X #:REQ8085) (Y #:REQ8086))
  (LET* ((R #:REST8087)
         (Z (IF #:OPTP8089 #:OPT8088 NIL))
         (A (IF #:KEYP8091 #:KEY8090 1))
         (B (IF #:KEYP8093 #:KEY8092 (BINIT)))
         (BP #:KEYP8093)
         (C (IF #:KEYP8095 #:KEY8094 NIL)))
    (DECLARE (SPECIAL A))
    "Hello there"
    THE-BODY-CODE)) |#

