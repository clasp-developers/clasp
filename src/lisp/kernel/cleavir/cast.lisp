(in-package #:clasp-cleavir)

;;; Given an LLVM Value, its rtype, and a desired rtype, generate code to
;;; convert the value to the desired rtype.
(defun translate-cast (inputv inputrt outputrt)
  ;; most of this is special casing crap due to 1-value values not being
  ;; passed around as lists.
  (cond ((eq inputrt :multiple-values)
         (cond ((eq outputrt :multiple-values)
                ;; A NOP like this isn't generated within code, but the
                ;; translate-cast in layout-xep can end up here.
                inputv)
               ((not (listp outputrt)) (error "BUG: Bad rtype ~a" outputrt))
               ((= (length outputrt) 1)
                (cast-one :object (first outputrt)
                          (cmp:irc-tmv-primary inputv)))
               ((null outputrt) nil)
               (t (cons (cast-one :object (first outputrt)
                                  (cmp:irc-tmv-primary inputv))
                        (loop for i from 1
                              for ort in (rest outputrt)
                              for val = (cmp:irc-t*-load (return-value-elt i))
                              collect (cast-one :object ort val))))))
        ((eq inputrt :vaslist)
         (cond ((eq outputrt :multiple-values)
                (%intrinsic-call "cc_load_values"
                                 (list (cmp:irc-vaslist-nvals inputv)
                                       (cmp:irc-vaslist-values inputv))))
               ((and (listp outputrt) (= (length outputrt) 1))
                (cast-one :object (first outputrt)
                          (cmp:irc-vaslist-nth (%size_t 0) inputv)))
               (t (error "BUG: Cast from ~a to ~a" inputrt outputrt))))
        ((not (listp inputrt)) (error "BUG: Bad rtype ~a" inputrt))
        ;; inputrt must be a list (fixed values)
        ((= (length inputrt) 1)
         (cond ((eq outputrt :multiple-values)
                (cmp:irc-make-tmv (%size_t 1)
                                  (cast-one (first inputrt) :object inputv)))
               ((not (listp outputrt))
                (error "BUG: Cast from ~a to ~a" inputrt outputrt))
               ((null outputrt) nil)
               ((= (length outputrt) 1)
                (cast-one (first inputrt) (first outputrt) inputv))
               (t ;; pad with nil
                (assert (every (lambda (r) (eq r :object)) (rest outputrt)))
                (cons (cast-one (first inputrt) (first outputrt) inputv)
                      (loop repeat (length (rest outputrt))
                            collect (%nil))))))
        (t
         (cond ((eq outputrt :multiple-values)
                (%cast-to-mv
                 (loop for inv in inputv for irt in inputrt
                       collect (cast-one irt :object inv))))
               ((not (listp outputrt))
                (error "BUG: Cast from ~a to ~a" inputrt outputrt))
               ((= (length outputrt) 1)
                (cond ((null inputrt)
                       (ecase (first outputrt)
                         ((:object) (%nil))
                         ;; We can end up here with a variety of output vrtypes
                         ;; in some unusual situations where a primop expects
                         ;; a value, but control will never actually reach it.
                         ;; Ideally the compiler would not bother compiling
                         ;; such unreachable code, but sometimes it's stupid.
                         ((:fixnum) (llvm-sys:poison-value-get cmp:%fixnum%))
                         ((:single-float)
                          (llvm-sys:poison-value-get cmp:%float%))
                         ((:double-float)
                          (llvm-sys:poison-value-get cmp:%double%))))
                      (t
                       (cast-one (first inputrt) (first outputrt)
                                 (first inputv)))))
               (t (%cast-some inputrt outputrt inputv))))))

;;; Given a list of Values, generate code to return it as multiple (Lisp) values.
(defun %cast-to-mv (values)
  (cond ((null values) (cmp:irc-make-tmv (%size_t 0) (%nil)))
        (t
         (loop for i from 1 for v in (rest values)
               do (cmp:irc-store v (return-value-elt i)))
         (cmp:irc-make-tmv (%size_t (length values)) (first values)))))

;;; Generate code to cast the Value from the given vrtype to the desired.
(defgeneric cast-one (from to value)
  (:method (from to value)
    (if (eql from to)
        value
        (error "BUG: Don't know how to cast ~a ~a to ~a" from value to))))

(defmethod cast-one ((from (eql :boolean)) (to (eql :object)) value)
  ;; we could use a select instruction, but then we'd have a redundant memory load.
  ;; which really shouldn't be a big deal, but why risk it.
  (let* ((thenb (cmp:irc-basic-block-create "bool-t"))
         (elseb (cmp:irc-basic-block-create "bool-nil"))
         (_0 (cmp:irc-cond-br value thenb elseb))
         (merge (cmp:irc-basic-block-create "bool"))
         (_1 (cmp:irc-begin-block merge))
         (phi (cmp:irc-phi cmp:%t*% 2 "bool")))
    (declare (ignore _0 _1))
    (cmp:irc-begin-block thenb)
    (cmp:irc-phi-add-incoming phi (%t) thenb)
    (cmp:irc-br merge)
    (cmp:irc-begin-block elseb)
    (cmp:irc-phi-add-incoming phi (%nil) elseb)
    (cmp:irc-br merge)
    (cmp:irc-begin-block merge)
    phi))

(defmethod cast-one ((from (eql :object)) (to (eql :boolean)) value)
  (cmp:irc-icmp-ne value (%nil)))

(defmethod cast-one ((from (eql :single-float)) (to (eql :object)) value)
  (cmp:irc-box-single-float value))
(defmethod cast-one ((from (eql :object)) (to (eql :single-float)) value)
  (cmp:irc-unbox-single-float value))

(defmethod cast-one ((from (eql :double-float)) (to (eql :object)) value)
  (cmp:irc-box-double-float value))
(defmethod cast-one ((from (eql :object)) (to (eql :double-float)) value)
  (cmp:irc-unbox-double-float value))

(defmethod cast-one ((from (eql :base-char)) (to (eql :object)) value)
  (cmp:irc-tag-base-char value))
(defmethod cast-one ((from (eql :object)) (to (eql :base-char)) value)
  (cmp:irc-untag-base-char value))
(defmethod cast-one ((from (eql :character)) (to (eql :object)) value)
  (cmp:irc-tag-character value))
(defmethod cast-one ((from (eql :object)) (to (eql :character)) value)
  (cmp:irc-untag-character value))

(defmethod cast-one ((from (eql :base-char)) (to (eql :character)) value)
  (cmp:irc-zext value cmp:%i32%))
(defmethod cast-one ((from (eql :character)) (to (eql :base-char)) value)
  (cmp:irc-trunc value cmp:%i8%))

(defmethod cast-one ((from (eql :fixnum)) (to (eql :object)) value)
  (cmp:irc-int-to-ptr value cmp:%t*%))
(defmethod cast-one ((from (eql :object)) (to (eql :fixnum)) value)
  (cmp:irc-ptr-to-int value cmp:%fixnum%))

(defmethod cast-one ((from (eql :utfixnum)) (to (eql :fixnum)) value)
  (cmp:irc-shl value cmp:+fixnum-shift+ :nsw t))
(defmethod cast-one ((from (eql :fixnum)) (to (eql :utfixnum)) value)
  (cmp:irc-ashr value cmp:+fixnum-shift+ :exact t))

(defmethod cast-one ((from (eql :utfixnum)) (to (eql :object)) value)
  (cmp:irc-tag-fixnum value))
(defmethod cast-one ((from (eql :object)) (to (eql :utfixnum)) value)
  (cmp:irc-untag-fixnum value cmp:%fixnum%))

(defmethod cast-one ((from (eql :object)) (to (eql :vaslist)) value)
  ;; We only generate these when we know for sure the input is a vaslist,
  ;; so we don't do checking.
  (cmp:irc-unbox-vaslist value))

(defun %cast-some (inputrt outputrt inputv)
  (let ((Lin (length inputrt)) (Lout (length outputrt))
        (pref (mapcar #'cast-one inputrt outputrt inputv)))
    (cond ((<= Lout Lin) pref)
          (t
           (assert (every (lambda (r) (eq r :object)) (subseq outputrt Lin)))
           (nconc pref (loop repeat (- Lout Lin) collect (%nil)))))))
