(in-package :cmp)

;;; (lambda (x y) ...)
;;; (lambda (x y &optional (a ai as)) ...)

(defun transform-optionals (optargs in-name-map)
  (let (opts optassign name-map)
    (do* ((cur (cdr optargs) (cdddr cur))
          (optname (car cur) (car cur))
          (optinit (cadr cur) (cadr cur))
          (optp (caddr cur) (caddr cur)))
         ((null cur)
          (progn
            (values (nreverse opts)
                    (nreverse optassign)
                    (append in-name-map (nreverse name-map)))))
      (let ((optgs (gensym "OPT"))
            (optpgs (gensym "OPTP")))
        (push (list optgs optpgs) opts)
        (push (list optname `(if ,optpgs ,optgs ,optinit)) optassign)
        (push (cons optname optgs) name-map)
        (when optp (push (list optp optpgs) optassign))
        (when optp (push (cons optp optpgs) name-map))))))

(defun transform-keys (keyargs in-name-map)
  (let (keys keyassign name-map)
    (do* ((cur (cdr keyargs) (cddddr cur))
          (keykw (car cur) (car cur))
          (keyname (second cur) (second cur))
          (keyinit (third cur) (third cur))
          (keyp (fourth cur) (fourth cur)))
         ((null cur)
          (progn
            (values (nreverse keys)
                    (nreverse keyassign)
                    (append in-name-map (nreverse name-map)))))
      (let ((keygs (gensym "KEY"))
            (keypgs (gensym "KEYP")))
        (push (list keykw #++(list keykw keyname) keygs keypgs) keys)
        (push (list keyname `(if ,keypgs ,keygs ,keyinit)) keyassign)
        (push (cons keyname keygs) name-map)
        (when keyp (push (list keyp keypgs) keyassign))
        (when keyp (push (cons keyp keypgs) name-map))))))

;;;
;;; Transform the lambda form into a cleavir style lambda list
;;; and a LET* that binds lexical variables in registers into bclasp
;;; variables in an activation frame

(defun transform-lambda-parts (lambda-list declares code)
  "Transform the lambda-list into a cleavir-lambda-list and wrap the code in a let.
Return (values cleavir-lambda-list wrapped-code name-map)."
  (let (name-map)
    (multiple-value-bind (reqargs optargs rest-var key-flag keyargs allow-other-keys auxargs varest-p)
        (core:process-lambda-list lambda-list 'function)
      (let ((creqs (mapcar (lambda (x)
                             (let ((new-name (gensym "REQ")))
                               (push (cons x new-name) name-map)
                               new-name))
                           (cdr reqargs)))
            (crest (if rest-var
                       (let ((new-name (gensym "REST")))
                         (push (cons rest-var new-name) name-map)
                         new-name)
                       nil)))
        (multiple-value-bind (copts opt-assign name-map)
            (transform-optionals optargs name-map)
          (multiple-value-bind (ckeys key-assign name-map)
              (transform-keys keyargs name-map)
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
                (values cleavir-lambda-list cleavir-body name-map)))))))))



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

