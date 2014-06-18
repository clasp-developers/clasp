;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;
;;; Copyright (c) 2010, Alexander Gavrilov (angavrilov@gmail.com)
;;;
;;; This file contains definitions of abstract VOPs, macros
;;; and utility functions used to implement the intrinsics.
;;;

(in-package #:SSE)

;;; The specific pack types

(deftype int-sse-pack () '(sse-pack integer))
(deftype float-sse-pack () '(sse-pack single-float))
(deftype double-sse-pack () '(sse-pack double-float))

;;; Helper functions

(defconstant +uint32-mask+ #xFFFFFFFF)
(defconstant +uint64-mask+ #xFFFFFFFFFFFFFFFF)
(defconstant +min-int32+ (- (ash 1 31)))
(defconstant +max-int32+ (1- (ash 1 31)))

(defun type-name-to-primitive (lt)
  (primitive-type-name (primitive-type (specifier-type lt))))

(defun move-cmd-for-type (lt)
  (ecase lt
    (int-sse-pack 'movdqa)
    ((float-sse-pack double-sse-pack) 'movaps)))

(defun ensure-reg-or-mem (tn)
  (sc-case tn
    ((sse-pack-immediate immediate)
     (register-inline-constant (tn-value tn)))
    (t tn)))

(defmacro ensure-load (type tgt src)
  `(unless (location= ,tgt ,src)
     (inst ,(move-cmd-for-type type) ,tgt (ensure-reg-or-mem ,src))))

(defmacro ensure-move (type tgt src)
  `(unless (location= ,tgt ,src)
     (inst ,(move-cmd-for-type type) ,tgt ,src)))

(defmacro save-intrinsic-spec (name info)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get ',name 'intrinsic-spec) ',info)))

(defmacro def-splice-transform (name args &body code)
  (let* ((direct-args (mapcar (lambda (x) (if (consp x) (gensym) x)) args))
         (flat-args (mapcan (lambda (x) (if (consp x) (copy-list (rest x)) (list x))) args)))
    `(deftransform ,name ((,@direct-args) * *)
       ,(format nil "Simplify combination ~A" (cons name args))
       ,@(loop for spec in args and name in direct-args
            when (consp spec)
            collect `(splice-fun-args ,name ',(first spec) ,(1- (length spec))))
       (list* 'lambda ',flat-args ',code))))

;;; Index-offset splicing

(defun skip-casts (lvar)
  (let ((inside (lvar-uses lvar)))
    (if (and (cast-p inside)
             (policy inside (= sb-c::type-check 0)))
        (skip-casts (cast-value inside))
        lvar)))

(defun delete-casts (lvar)
  (loop for inside = (lvar-uses lvar)
     while (cast-p inside)
     do (delete-filter inside lvar (cast-value inside))))

(defun fold-index-addressing (fun-name index scale offset &key prefix-args postfix-args)
  (multiple-value-bind (func index-args)
      (extract-fun-args (skip-casts index) '(+ - * ash) 2)
    (destructuring-bind (x constant) index-args
      (declare (ignorable x))
      (unless (constant-lvar-p constant)
        (give-up-ir1-transform))
      (let ((value (lvar-value constant))
            (scale-value (lvar-value scale))
            (offset-value (lvar-value offset)))
        (unless (integerp value)
          (give-up-ir1-transform))
        (multiple-value-bind (new-scale new-offset)
            (ecase func
              (+   (values scale-value (+ offset-value (* value scale-value))))
              (-   (values scale-value (- offset-value (* value scale-value))))
              (*   (values (* scale-value value) offset-value))
              (ash (unless (>= value 0)
                     (give-up-ir1-transform "negative index shift"))
                   (values (ash scale-value value) offset-value)))
          (unless (and (typep new-scale '(signed-byte 32))
                       (typep new-offset 'signed-word))
            (give-up-ir1-transform "constant is too large for inlining"))
          (delete-casts index)
          (splice-fun-args index func 2)
          `(lambda (,@prefix-args thing index const scale offset ,@postfix-args)
             (declare (ignore const scale offset))
             (,fun-name ,@prefix-args thing (the signed-word index) ,new-scale ,new-offset ,@postfix-args)))))))

(deftransform fold-ref-index-addressing ((thing index scale offset) * * :defun-only t :node node)
  (fold-index-addressing (lvar-fun-name (basic-combination-fun node)) index scale offset))

(deftransform fold-xmm-ref-index-addressing ((value thing index scale offset) * * :defun-only t :node node)
  (fold-index-addressing (lvar-fun-name (basic-combination-fun node)) index scale offset :prefix-args '(value)))

(deftransform fold-set-index-addressing ((thing index scale offset value) * * :defun-only t :node node)
  (fold-index-addressing (lvar-fun-name (basic-combination-fun node)) index scale offset :postfix-args '(value)))

;;; Index-offset addressing

(defun is-tagged-load-scale (value)
  (not (logtest value (1- (ash 1 n-fixnum-tag-bits)))))

(deftype tagged-load-scale ()
  '(and fixnum (satisfies is-tagged-load-scale)))

(defun find-lea-scale (scale)
  (cond ((not (logtest scale 7)) (values (/ scale 8) 8))
        ((not (logtest scale 3)) (values (/ scale 4) 4))
        ((not (logtest scale 1)) (values (/ scale 2) 2))
        (t (values scale 1))))

(defun reduce-offset (ioffset scale offset)
  "Redistribute value from ioffset to offset, while keeping offset int32."
  (let* ((istep (if (< ioffset 0) -1 1))
         (icount (max 0
                      (if (< ioffset 0)
                          (- (1+ +min-int32+) ioffset) ; = (- +max-int32+)
                          (- ioffset +max-int32+))))
         (ostep (* istep scale))
         (ocount (truncate (- (if (> ostep 0) +max-int32+ +min-int32+) offset)
                           ostep))
         (count (min ocount icount)))
    (values (- ioffset (* count istep))
            (+ offset (* count ostep)))))

(defun split-offset (offset scale)
  (if (typep offset '(signed-byte 32))
      (values 0 offset)
      (multiple-value-bind (div rem) (floor offset scale)
        (assert (typep rem '(signed-byte 32)))
        (if (typep div '(signed-byte 32))
            (values div rem)
            (reduce-offset div scale rem)))))

(defun power-of-2? (scale)
  (and (> scale 0) (not (logtest scale (1- scale)))))

(defun find-power-of-2 (scale)
  (assert (power-of-2? scale))
  (loop for i from 0 and sv = scale then (ash sv -1)
     when (<= sv 1) return i))

(defun make-scaled-ea (size sap index scale offset tmp &key fixnum-index)
  "Returns an ea representing the given index*scale + offset formula.
May emit additional instructions using the temporary register."
  (assemble ()
    (if (or (sc-is index immediate) (= scale 0))
        ;; Fully constant offset
        (let ((value (if (= scale 0) offset
                         (+ (* (tn-value index) scale) offset))))
          (assert (typep value '(signed-byte 64)))
          (if (typep value '(signed-byte 32))
              (make-ea size :base sap :disp value)
              (progn
                (inst mov tmp (register-inline-constant value))
                (make-ea size :base sap :index tmp))))
        ;; Indexing
        (progn
          (when (sc-is index any-reg)
            (assert (and fixnum-index (is-tagged-load-scale scale)))
            (setf scale (ash scale (- n-fixnum-tag-bits))))
          (multiple-value-bind (rscale lscale) (find-lea-scale scale)
            ;; One-instruction case?
            (if (and (= rscale 1) (typep offset '(signed-byte 32)))
                (make-ea size :base sap :index index :scale scale :disp offset)
                ;; Use temporary
                (multiple-value-bind (roffset loffset) (split-offset offset lscale)
                  (labels ((negate-when-<0 (register scale)
                             (when (< scale 0)
                               (inst neg register)))
                           (emit-shift-mul (register scale)
                             (inst shl register (find-power-of-2 (abs scale)))
                             (negate-when-<0 register scale))
                           ;; Tries to compute tmp via LEA
                           (try-use-lea (scale &optional base)
                             (multiple-value-bind (rrscale rlscale) (find-lea-scale scale)
                               (when (and (= (abs rrscale) 1) (typep (* rrscale roffset) '(signed-byte 32)))
                                 (when (and (= roffset 0) (null base)) ; minimize loffset
                                   (multiple-value-setq (roffset loffset) (floor offset lscale)))
                                 (let ((xoffset (* rrscale roffset)))
                                   (inst lea tmp
                                         (if (and (= rlscale 1) (null base))
                                             (make-ea :byte :base index :disp xoffset)
                                             (make-ea :byte :base base :index index :scale rlscale :disp xoffset))))
                                 (negate-when-<0 tmp rrscale)
                                 :success))))
                    (declare (inline negate-when-<0 emit-shift-mul))
                    (cond
                      ;; same register shift?
                      ((and (= roffset 0) (location= tmp index) (power-of-2? (abs rscale)))
                       (emit-shift-mul tmp rscale))
                      ;; one LEA?
                      ((try-use-lea rscale))
                      ((try-use-lea (1- rscale) index))
                      ;; Generic case, use mul/shl and add
                      (t
                       (if (power-of-2? (abs rscale))
                           (progn
                             (move tmp index)
                             (emit-shift-mul tmp rscale))
                           (inst imul tmp index rscale))
                       (unless (= roffset 0)
                         ;; Make loffset as small as possible
                         (multiple-value-setq (roffset loffset) (floor offset lscale))
                         (if (typep roffset '(signed-byte 32))
                             (inst add tmp roffset)
                             (inst add tmp (register-inline-constant roffset))))))
                    (make-ea size :base sap :index tmp :scale lscale :disp loffset)))))))))

;; Initialization

(defmacro def-float-set-intrinsic (&whole whole pubname fname atype aregtype rtype move)
  (declare (ignore pubname))
  `(progn
     (save-intrinsic-spec ,fname ,whole)
     (defknown ,fname (,atype) ,rtype (foldable flushable))
     (define-vop (,fname)
       (:translate ,fname)
       (:args (arg :scs (,aregtype) :target dst))
       (:arg-types ,atype)
       (:results (dst :scs (sse-reg)))
       (:result-types ,(type-name-to-primitive rtype))
       (:policy :fast-safe)
       (:generator 1
         (unless (location= dst arg)
           (inst ,move dst arg))))))

;; Unary operations

(define-vop (sse-unary-base-op)
  ;; no immediate because expecting to be folded
  (:args (x :scs (sse-reg)))
  (:arg-types sse-pack)
  (:policy :fast-safe)
  (:note "inline SSE unary operation")
  (:vop-var vop)
  (:save-p :compute-only))

(define-vop (sse-unary-op sse-unary-base-op)
  (:args (x :scs (sse-reg) :target r))
  (:results (r :scs (sse-reg))))

(define-vop (sse-unary-to-int-op sse-unary-base-op)
  (:results (r :scs (signed-reg))))

(define-vop (sse-unary-to-uint-op sse-unary-base-op)
  (:results (r :scs (unsigned-reg))))

(defmacro def-unary-intrinsic (&whole whole name rtype insn cost c-name &key partial immediate-arg result-size arg-type)
  (declare (ignore c-name arg-type))
  (let* ((imm (if immediate-arg '(imm)))
         (immt (if immediate-arg (list immediate-arg))))
    (assert (or (not partial) (not (subtypep rtype 'integer))))
    `(progn
       (export ',name)
       (save-intrinsic-spec ,name ,whole)
       (defknown ,name (sse-pack ,@immt) ,rtype (foldable flushable))
       (define-vop (,name ,(cond ((subtypep rtype 'unsigned-byte)
                                  'sse-unary-to-uint-op)
                                 ((subtypep rtype 'integer)
                                  'sse-unary-to-int-op)
                                 (t 'sse-unary-op)))
         (:translate ,name)
         (:result-types ,(type-name-to-primitive rtype))
         ,@(if immediate-arg
               `((:arg-types sse-pack (:constant ,immediate-arg))
                 (:info imm)))
         (:generator ,cost
           ,@(ecase partial
                    (:one-arg `((ensure-move ,rtype r x)
                                (inst ,insn r ,@imm)))
                    (t        `((ensure-move ,rtype r x)
                                (inst ,insn r r ,@imm)))
                    ((nil)    `((inst ,insn
                                      ,(if result-size `(reg-in-size r ,result-size) 'r)
                                      x ,@imm)))))))))

;; Unary to int32 & sign-extend

(define-vop (sse-cvt-to-int32-op sse-unary-base-op)
  (:temporary (:sc signed-reg :offset rax-offset :target r :to :result) rax)
  (:results (r :scs (signed-reg))))

(defmacro def-cvt-to-int32-intrinsic (name rtype insn cost c-name &key arg-type)
  (declare (ignore arg-type))
  `(progn
     (export ',name)
     (save-intrinsic-spec ,name (def-unary-intrinsic ,name ,rtype ,insn ,cost ,c-name))
     (defknown ,name (sse-pack) (signed-byte 32) (foldable flushable))
     (define-vop (,name sse-cvt-to-int32-op)
       (:translate ,name)
       (:result-types ,(type-name-to-primitive rtype))
       (:generator ,cost
         (inst ,insn (reg-in-size rax :dword) x)
         (inst cdqe)
         (move r rax)))))

;; NOT intrinsics

(define-vop (sse-not-op sse-unary-op)
  (:temporary (:sc sse-reg) tmp))

(defmacro def-not-intrinsic (name rtype insn)
  `(progn
     (export ',name)
     (save-intrinsic-spec ,name (def-unary-intrinsic ,name ,rtype ,insn 3 nil))
     (defknown ,name (sse-pack) ,rtype (foldable flushable))
     (define-vop (,name sse-not-op)
       (:translate ,name)
       (:result-types ,(type-name-to-primitive rtype))
       (:generator 3
         (if (location= x r)
             (progn
               (inst pcmpeqd tmp tmp)
               (inst ,insn r tmp))
             (progn
               (inst pcmpeqd r r)
               (inst ,insn r x)))))))

;; Binary operations

(define-vop (sse-binary-base-op)
  (:args (x :scs (sse-reg sse-pack-immediate) :target r)
         (y :scs (sse-reg sse-pack-immediate)))
  (:results (r :scs (sse-reg)))
  (:arg-types sse-pack sse-pack)
  (:policy :fast-safe)
  (:note "inline SSE binary operation")
  (:vop-var vop)
  (:save-p :compute-only))

(define-vop (sse-binary-op sse-binary-base-op)
  (:temporary (:sc sse-reg) tmp))

(define-vop (sse-binary-comm-op sse-binary-base-op)
  (:args (x :scs (sse-reg sse-pack-immediate) :target r)
         (y :scs (sse-reg sse-pack-immediate) :target r)))

(defmacro def-binary-intrinsic (&whole whole name rtype insn cost c-name &key commutative tags immediate-arg x-type y-type)
  (declare (ignore c-name x-type y-type))
  (let* ((imm (if immediate-arg '(imm)))
         (immt (if immediate-arg (list immediate-arg))))
    `(progn
       (export ',name)
       (save-intrinsic-spec ,name ,whole)
       (defknown ,name (sse-pack sse-pack ,@immt) ,rtype (foldable flushable))
       (define-vop (,name ,(if commutative 'sse-binary-comm-op 'sse-binary-op))
         (:translate ,name)
         (:result-types ,(type-name-to-primitive rtype))
         ,@(if immediate-arg
               `((:arg-types sse-pack sse-pack (:constant ,immediate-arg))
                 (:info imm)))
         (:generator ,cost
           ,@(if commutative
                 `((when (location= y r)
                     (rotatef x y))
                   (ensure-load ,rtype r x)
                   (inst ,insn ,@tags r (ensure-reg-or-mem y) ,@imm))
                 `((unless (location= y r)
                     (setf tmp r))
                   (ensure-load ,rtype tmp x)
                   (inst ,insn ,@tags tmp (ensure-reg-or-mem y) ,@imm)
                   (ensure-move ,rtype r tmp))))))))

;;; XMM/Integer combination intrinsics

(define-vop (sse-int-base-op)
  (:results (r :scs (sse-reg)))
  (:policy :fast-safe)
  (:note "inline SSE/integer operation")
  (:vop-var vop)
  (:save-p :compute-only))

(define-vop (sse-int-op sse-int-base-op)
  (:args (x :scs (sse-reg sse-pack-immediate) :target r)
         (iv :scs (signed-reg signed-stack immediate)))
  (:arg-types sse-pack signed-num))

(define-vop (sse-uint-op sse-int-base-op)
  (:args (x :scs (sse-reg sse-pack-immediate) :target r)
         (iv :scs (unsigned-reg unsigned-stack immediate)))
  (:arg-types sse-pack unsigned-num))

(defmacro def-sse-int-intrinsic (&whole whole name itype rtype insn cost c-name &key make-temporary immediate-arg defun-body)
  (declare (ignore c-name defun-body))
  (let* ((imm (if immediate-arg '(imm)))
         (immt (if immediate-arg (list immediate-arg)))
         (unsigned? (subtypep itype 'unsigned-byte)))
    `(progn
       (export ',name)
       (save-intrinsic-spec ,name ,whole)
       (defknown ,name (sse-pack ,itype ,@immt) ,rtype (foldable flushable))
       (define-vop (,name ,(if unsigned? 'sse-uint-op 'sse-int-op))
         (:translate ,name)
         (:result-types ,(type-name-to-primitive rtype))
         ,@(if immediate-arg
               `((:arg-types sse-pack
                             ,(if unsigned? 'unsigned-num 'signed-num)
                             (:constant ,immediate-arg))
                 (:info imm)))
         ,@(if make-temporary
               `((:temporary (:sc sse-reg) tmp)))
         (:generator ,cost
           (ensure-load ,rtype r x)
           ,@(if (eq make-temporary t)
                 '((inst movd tmp (ensure-reg-or-mem iv)))
                 make-temporary)
           (inst ,insn r ,(if make-temporary 'tmp '(ensure-reg-or-mem iv)) ,@imm))))))

;;; Comparison predicate intrinsics

(define-vop (sse-comparison-op)
  (:args (x :scs (sse-reg))
         (y :scs (sse-reg sse-pack-immediate)))
  (:arg-types sse-pack sse-pack)
  (:policy :fast-safe)
  (:note "inline SSE binary comparison predicate")
  (:vop-var vop)
  (:save-p :compute-only))

(define-vop (sse-comparison-comm-op sse-comparison-op)
  (:args (x :scs (sse-reg)
            :load-if (not (and (sc-is x sse-pack-immediate)
                               (sc-is y sse-reg))))
         (y :scs (sse-reg sse-pack-immediate))))

(defmacro def-comparison-intrinsic (&whole whole name arg-type insn cost c-name &key commutative tags)
  (declare (ignore arg-type c-name))
  (let* ()
    `(progn
       (export ',name)
       (save-intrinsic-spec ,name ,whole)
       (defknown ,name (sse-pack sse-pack) boolean (foldable flushable))
       (define-vop (,name ,(if commutative 'sse-comparison-comm-op 'sse-comparison-op))
         (:translate ,name)
         (:conditional ,@tags)
         (:generator ,cost
           ,(if commutative
                `(if (sc-is x sse-reg)
                     (inst ,insn x y)
                     (inst ,insn y x))
                `(inst ,insn x y)))))))

;;; Memory intrinsics

(define-vop (sse-load-base-op)
  (:results (r :scs (sse-reg)))
  (:policy :fast-safe)
  (:note "inline SSE load operation"))

(define-vop (sse-load-op sse-load-base-op)
  (:args (sap :scs (sap-reg) :to :eval)
         (index :scs (signed-reg immediate) :target tmp))
  (:arg-types system-area-pointer signed-num
              (:constant fixnum) (:constant signed-word))
  (:temporary (:sc signed-reg :from (:argument 1)) tmp)
  (:info scale offset))

(define-vop (sse-load-op/tag sse-load-base-op)
  (:args (sap :scs (sap-reg) :to :eval)
         (index :scs (any-reg signed-reg immediate) :target tmp))
  (:arg-types system-area-pointer tagged-num
              (:constant tagged-load-scale) (:constant signed-word))
  (:temporary (:sc any-reg :from (:argument 1)) tmp)
  (:info scale offset))

(define-vop (sse-xmm-load-op sse-load-base-op)
  (:args (value :scs (sse-reg sse-pack-immediate) :target r)
         (sap :scs (sap-reg) :to :eval)
         (index :scs (signed-reg immediate) :target tmp))
  (:arg-types sse-pack system-area-pointer signed-num
              (:constant fixnum) (:constant signed-word))
  (:temporary (:sc signed-reg :from (:argument 2)) tmp)
  (:info scale offset))

(define-vop (sse-xmm-load-op/tag sse-load-base-op)
  (:args (value :scs (sse-reg sse-pack-immediate) :target r)
         (sap :scs (sap-reg) :to :eval)
         (index :scs (any-reg signed-reg immediate) :target tmp))
  (:arg-types sse-pack system-area-pointer tagged-num
              (:constant tagged-load-scale) (:constant signed-word))
  (:temporary (:sc any-reg :from (:argument 2)) tmp)
  (:info scale offset))

(define-vop (sse-load-ix-op sse-load-base-op)
  (:args (sap :scs (descriptor-reg) :to :eval)
         (index :scs (signed-reg immediate) :target tmp))
  (:arg-types * signed-num (:constant fixnum) (:constant signed-word))
  (:temporary (:sc signed-reg :from (:argument 1)) tmp)
  (:info scale offset))

(define-vop (sse-load-ix-op/tag sse-load-base-op)
  (:args (sap :scs (descriptor-reg) :to :eval)
         (index :scs (any-reg signed-reg immediate) :target tmp))
  (:arg-types * tagged-num (:constant tagged-load-scale) (:constant signed-word))
  (:temporary (:sc any-reg :from (:argument 1)) tmp)
  (:info scale offset))

(defmacro def-load-intrinsic (&whole whole name rtype insn c-name
                              &key register-arg tags postfix-fmt (size :qword))
  (declare (ignore c-name postfix-fmt))
  (let* ((vop (symbolicate "%" name))
         (ix-vop (symbolicate vop "/IX"))
         (valtype (if register-arg '(sse-pack)))
         (r-arg (if rtype '(r)))
         (rtypes (if rtype
                     `(:result-types ,(type-name-to-primitive rtype))
                     `(:results))))
    (assert (or rtype (not register-arg)))
    `(progn
       (export ',name)
       (save-intrinsic-spec ,name ,whole)
       (defknown ,vop (,@valtype system-area-pointer signed-word fixnum signed-word)
           ,(or rtype '(values)) (flushable always-translatable))
       (define-vop (,vop ,(if register-arg 'sse-xmm-load-op 'sse-load-op))
         (:translate ,vop)
         ,rtypes
         (:generator 5
           ,(if register-arg `(ensure-load ,rtype r value))
           (inst ,insn ,@tags ,@r-arg (make-scaled-ea ,size sap index scale offset tmp))))
       (define-vop (,(symbolicate vop "/TAG") ,(if register-arg 'sse-xmm-load-op/tag 'sse-load-op/tag))
         (:translate ,vop)
         ,rtypes
         (:generator 4
           ,(if register-arg `(ensure-load ,rtype r value))
           (inst ,insn ,@tags ,@r-arg (make-scaled-ea ,size sap index scale offset tmp :fixnum-index t))))
       (%deftransform ',vop '(function * *)
                      #',(if register-arg 'fold-xmm-ref-index-addressing 'fold-ref-index-addressing)
                      "fold semi-constant offset expressions")
       ,@(if (null register-arg)
             `(;; Vector indexing version
               (defknown ,ix-vop (simple-array signed-word fixnum signed-word) ,(or rtype '(values))
                   (flushable always-translatable))
               (define-vop (,ix-vop sse-load-ix-op)
                 (:translate ,ix-vop)
                 ,rtypes
                 (:generator 4
                   (inst ,insn ,@tags ,@r-arg (make-scaled-ea ,size sap index scale offset tmp))))
               (define-vop (,(symbolicate ix-vop "/TAG") sse-load-ix-op/tag)
                 (:translate ,ix-vop)
                 ,rtypes
                 (:generator 3
                   (inst ,insn ,@tags ,@r-arg (make-scaled-ea ,size sap index scale offset tmp :fixnum-index t))))
               (%deftransform ',ix-vop '(function * *) #'fold-ref-index-addressing
                              "fold semi-constant index expressions"))))))

(define-vop (sse-store-base-op)
  (:policy :fast-safe)
  (:note "inline SSE store operation"))

(define-vop (sse-store-op sse-store-base-op)
  (:args (sap :scs (sap-reg) :to :eval)
         (index :scs (signed-reg immediate) :target tmp)
         (value :scs (sse-reg)))
  (:arg-types system-area-pointer signed-num (:constant fixnum) (:constant signed-word) sse-pack)
  (:temporary (:sc signed-reg :from (:argument 1)) tmp)
  (:info scale offset))

(define-vop (sse-store-op/tag sse-store-base-op)
  (:args (sap :scs (sap-reg) :to :eval)
         (index :scs (any-reg signed-reg immediate) :target tmp)
         (value :scs (sse-reg)))
  (:arg-types system-area-pointer tagged-num (:constant tagged-load-scale) (:constant signed-word) sse-pack)
  (:temporary (:sc any-reg :from (:argument 1)) tmp)
  (:info scale offset))

(define-vop (sse-store-ix-op sse-store-base-op)
  (:args (sap :scs (descriptor-reg) :to :eval)
         (index :scs (signed-reg immediate) :target tmp)
         (value :scs (sse-reg)))
  (:arg-types * signed-num (:constant fixnum) (:constant signed-word) sse-pack)
  (:temporary (:sc signed-reg :from (:argument 1)) tmp)
  (:info scale offset))

(define-vop (sse-store-ix-op/tag sse-store-base-op)
  (:args (sap :scs (descriptor-reg) :to :eval)
         (index :scs (any-reg signed-reg immediate) :target tmp)
         (value :scs (sse-reg)))
  (:arg-types * tagged-num (:constant tagged-load-scale) (:constant signed-word) sse-pack)
  (:temporary (:sc any-reg :from (:argument 1)) tmp)
  (:info scale offset))

(defmacro def-store-intrinsic (&whole whole name rtype insn c-name &key setf-name)
  (declare (ignore rtype c-name))
  (let* ((vop (symbolicate "%" name))
         (ix-vop (symbolicate vop "/IX")))
    `(progn
       ,(unless setf-name `(export ',name))
       (save-intrinsic-spec ,name ,whole)
       (defknown ,vop (system-area-pointer signed-word fixnum signed-word sse-pack) (values)
           (unsafe always-translatable))
       (define-vop (,vop sse-store-op)
         (:translate ,vop)
         (:generator 5
           (inst ,insn (make-scaled-ea :qword sap index scale offset tmp) value)))
       (define-vop (,(symbolicate vop "/TAG") sse-store-op/tag)
         (:translate ,vop)
         (:generator 4
           (inst ,insn (make-scaled-ea :qword sap index scale offset tmp :fixnum-index t) value)))
       (%deftransform ',vop '(function * *) #'fold-set-index-addressing
                      "fold semi-constant offset expressions")
       ;; Vector indexing version
       (defknown ,ix-vop (simple-array signed-word fixnum signed-word sse-pack) (values)
           (unsafe always-translatable))
       (define-vop (,ix-vop sse-store-ix-op)
         (:translate ,ix-vop)
         (:generator 4
           (inst ,insn (make-scaled-ea :qword sap index scale offset tmp) value)))
       (define-vop (,(symbolicate ix-vop "/TAG") sse-store-ix-op/tag)
         (:translate ,ix-vop)
         (:generator 3
           (inst ,insn (make-scaled-ea :qword sap index scale offset tmp :fixnum-index t) value)))
       (%deftransform ',ix-vop '(function * *) #'fold-set-index-addressing
                      "fold semi-constant index expressions"))))

