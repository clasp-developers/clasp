;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;
;;; Copyright (c) 2010, Alexander Gavrilov (angavrilov@gmail.com)
;;;
;;; This file contains definitions for all SSE intrinsics.
;;;
;;; The macros are defined in the *-core.lisp files.
;;; On SBCL wrapping functions are defined by sbcl-functions.lisp.
;;;

(in-package #:SSE)

#+(and ecl (or ecl_min stage1 cross))
(eval-when (:compile-toplevel)
  ;; During the initial bootstrap sequence when the contribs are
  ;; compiled, the system does not load fasls after building them.
  ;; (For all it knows, it might be cross-compiling to another architecture.)
  ;; Work around by loading the macro definition file into the interpreter:
  (load (merge-pathnames #P"ecl-sse-core.lisp" *compile-file-truename*)))

;;; Prefetch

(def-load-intrinsic cpu-prefetch-t0 nil prefetch "_mm_prefetch" :tags (:t0) :size :byte :postfix-fmt ",_MM_HINT_T0")
(def-load-intrinsic cpu-prefetch-t1 nil prefetch "_mm_prefetch" :tags (:t1) :size :byte :postfix-fmt ",_MM_HINT_T1")
(def-load-intrinsic cpu-prefetch-t2 nil prefetch "_mm_prefetch" :tags (:t2) :size :byte :postfix-fmt ",_MM_HINT_T2")
(def-load-intrinsic cpu-prefetch-nta nil prefetch "_mm_prefetch" :tags (:nta) :size :byte :postfix-fmt ",_MM_HINT_NTA")

(def-load-intrinsic cpu-clflush nil clflush "_mm_clflush" :size :byte)

;;; CPU control

#+sbcl
(progn
  (defknown cpu-mxcsr () (unsigned-byte 32) (flushable))

  (define-vop (cpu-mxcsr)
    (:translate cpu-mxcsr)
    (:args) (:arg-types)
    (:results (result :scs (unsigned-reg)))
    (:result-types unsigned-num)
    (:temporary (:sc unsigned-stack) tmp)
    (:policy :fast-safe)
    (:generator 3
      (let ((ea (make-ea :dword :base rbp-tn
                         :disp (frame-byte-offset (tn-offset tmp)))))
        (inst stmxcsr ea)
        (inst mov (reg-in-size result :dword) ea))))

  (defknown %set-cpu-mxcsr ((unsigned-byte 32)) (unsigned-byte 32) (unsafe))

  (define-vop (%set-cpu-mxcsr)
    (:translate %set-cpu-mxcsr)
    (:args (value :scs (unsigned-reg unsigned-stack) :target result))
    (:arg-types unsigned-num)
    (:results (result :scs (unsigned-reg)
                      :load-if (not (and (sc-is result unsigned-stack)
                                         (or (sc-is value unsigned-reg)
                                             (location= value result))))))
    (:result-types unsigned-num)
    (:temporary (:sc unsigned-stack) tmp)
    (:policy :fast-safe)
    (:generator 3
      (cond ((sc-is value unsigned-stack)
             (setf tmp value))
            ((sc-is result unsigned-stack)
             (setf tmp result)))
      (move tmp value)
      (unless (location= result tmp)
        (move result value))
      (let ((ea (make-ea :dword :base rbp-tn
                         :disp (frame-byte-offset (tn-offset tmp)))))
        (inst ldmxcsr ea))))

  (macrolet ((defvoid (name insn)
               `(progn
                  (export ',name)
                  (defknown ,name () (values) ())
                  (define-vop (,name)
                    (:translate ,name)
                    (:policy :fast-safe)
                    (:generator 1
                      (inst ,insn))))))
    (defvoid cpu-load-fence lfence)
    (defvoid cpu-store-fence sfence)
    (defvoid cpu-memory-fence mfence)
    (defvoid cpu-pause pause)))

#+ecl
(progn
  (def-intrinsic cpu-mxcsr () fixnum "_mm_getcsr")
  (def-intrinsic %set-cpu-mxcsr (fixnum) fixnum "_mm_setcsr" :export nil :ret-arg 0)

  (defsetf cpu-mxcsr %set-cpu-mxcsr)

  (def-intrinsic cpu-load-fence () nil "_mm_lfence")
  (def-intrinsic cpu-store-fence () nil "_mm_sfence")
  (def-intrinsic cpu-memory-fence () nil "_mm_mfence")

  (def-intrinsic cpu-pause () nil "_mm_pause"))

;;; Single-float

;; Initialization

#+sbcl
(def-float-set-intrinsic set-ss %set-ss single-float single-reg float-sse-pack movaps)

#+ecl
(progn
  (def-intrinsic set-ss (single-float) float-sse-pack "_mm_set_ss")
  (def-intrinsic set1-ps (single-float) float-sse-pack "_mm_set1_ps")

  (def-intrinsic set-ps (single-float single-float single-float single-float) float-sse-pack "_mm_set_ps")
  (def-intrinsic setr-ps (single-float single-float single-float single-float) float-sse-pack "_mm_setr_ps")

  (def-intrinsic setzero-ps () float-sse-pack "_mm_setzero_ps"))

;; Memory

(def-load-intrinsic mem-ref-ss float-sse-pack movss "_mm_load_ss")

(def-load-intrinsic mem-ref-ps float-sse-pack movups "_mm_loadu_ps")
(def-load-intrinsic mem-ref-aps float-sse-pack movaps "_mm_load_ps")

(def-store-intrinsic mem-set-ss float-sse-pack movss "_mm_store_ss" :setf-name mem-ref-ss)

(def-store-intrinsic mem-set-ps float-sse-pack movups "_mm_storeu_ps" :setf-name mem-ref-ps)
(def-store-intrinsic mem-set-aps float-sse-pack movaps "_mm_store_ps" :setf-name mem-ref-aps)

(def-store-intrinsic stream-ps float-sse-pack movntps "_mm_stream_ps")

;; Arithmetics

(def-binary-intrinsic add-ss float-sse-pack addss 3  "_mm_add_ss")
(def-binary-intrinsic add-ps float-sse-pack addps 3  "_mm_add_ps" :commutative t)
(def-binary-intrinsic sub-ss float-sse-pack subss 3  "_mm_sub_ss")
(def-binary-intrinsic sub-ps float-sse-pack subps 3  "_mm_sub_ps")
(def-binary-intrinsic mul-ss float-sse-pack mulss 5  "_mm_mul_ss")
(def-binary-intrinsic mul-ps float-sse-pack mulps 5  "_mm_mul_ps" :commutative t)
(def-binary-intrinsic div-ss float-sse-pack divss 13 "_mm_div_ss")
(def-binary-intrinsic div-ps float-sse-pack divps 13 "_mm_div_ps")
(def-binary-intrinsic min-ss float-sse-pack minss 3  "_mm_min_ss")
(def-binary-intrinsic min-ps float-sse-pack minps 3  "_mm_min_ps":commutative t)
(def-binary-intrinsic max-ss float-sse-pack maxss 3  "_mm_max_ss")
(def-binary-intrinsic max-ps float-sse-pack maxps 3  "_mm_max_ps" :commutative t)

(def-unary-intrinsic sqrt-ss  float-sse-pack sqrtss 20 "_mm_sqrt_ss" :partial t)
(def-unary-intrinsic sqrt-ps  float-sse-pack sqrtps 20 "_mm_sqrt_ps")
(def-unary-intrinsic rsqrt-ss float-sse-pack rsqrtss 20 "_mm_rsqrt_ss" :partial t)
(def-unary-intrinsic rsqrt-ps float-sse-pack rsqrtps 20 "_mm_rsqrt_ps")
(def-unary-intrinsic rcp-ss   float-sse-pack rcpss 13 "_mm_rcp_ss" :partial t)
(def-unary-intrinsic rcp-ps   float-sse-pack rcpps 13 "_mm_rcp_ps")

;; Bitwise logic

#+sbcl
(def-not-intrinsic not-ps float-sse-pack xorps)

(def-binary-intrinsic and-ps float-sse-pack andps 1 "_mm_and_ps" :commutative t)
(def-binary-intrinsic andnot-ps float-sse-pack andnps 1 "_mm_andnot_ps")
(def-binary-intrinsic or-ps float-sse-pack orps 1 "_mm_or_ps" :commutative t)
(def-binary-intrinsic xor-ps float-sse-pack xorps 1 "_mm_xor_ps" :commutative t)

;; Comparisons

(def-binary-intrinsic =-ss float-sse-pack cmpss 3 "_mm_cmpeq_ss" :tags (:eq))
(def-binary-intrinsic =-ps float-sse-pack cmpps 3 "_mm_cmpeq_ps" :tags (:eq) :commutative t)
(def-binary-intrinsic <-ss float-sse-pack cmpss 3 "_mm_cmplt_ss" :tags (:lt))
(def-binary-intrinsic <-ps float-sse-pack cmpps 3 "_mm_cmplt_ps" :tags (:lt))
(def-binary-intrinsic <=-ss float-sse-pack cmpss 3 "_mm_cmple_ss" :tags (:le))
(def-binary-intrinsic <=-ps float-sse-pack cmpps 3 "_mm_cmple_ps" :tags (:le))
#+ecl
(def-binary-intrinsic >-ss float-sse-pack nil nil "_mm_cmpgt_ss")
#+ecl
(def-binary-intrinsic >-ps float-sse-pack nil nil "_mm_cmpgt_ps")
#+ecl
(def-binary-intrinsic >=-ss float-sse-pack nil nil "_mm_cmpge_ss")
#+ecl
(def-binary-intrinsic >=-ps float-sse-pack nil nil "_mm_cmpge_ps")

(def-binary-intrinsic /=-ss float-sse-pack cmpss 3 "_mm_cmpneq_ss" :tags (:neq))
(def-binary-intrinsic /=-ps float-sse-pack cmpps 3 "_mm_cmpneq_ps" :tags (:neq) :commutative t)
(def-binary-intrinsic /<-ss float-sse-pack cmpss 3 "_mm_cmpnlt_ss" :tags (:nlt))
(def-binary-intrinsic /<-ps float-sse-pack cmpps 3 "_mm_cmpnlt_ps" :tags (:nlt))
(def-binary-intrinsic /<=-ss float-sse-pack cmpss 3 "_mm_cmpnle_ss" :tags (:nle))
(def-binary-intrinsic /<=-ps float-sse-pack cmpps 3 "_mm_cmpnle_ps" :tags (:nle))
#+ecl
(def-binary-intrinsic />-ss float-sse-pack nil nil "_mm_cmpngt_ss")
#+ecl
(def-binary-intrinsic />-ps float-sse-pack nil nil "_mm_cmpngt_ps")
#+ecl
(def-binary-intrinsic />=-ss float-sse-pack nil nil "_mm_cmpnge_ss")
#+ecl
(def-binary-intrinsic />=-ps float-sse-pack nil nil "_mm_cmpnge_ps")

(def-binary-intrinsic cmpord-ss float-sse-pack cmpss 3 "_mm_cmpord_ss" :tags (:ord))  ; neither is NaN
(def-binary-intrinsic cmpord-ps float-sse-pack cmpps 3 "_mm_cmpord_ps" :tags (:ord) :commutative t)
(def-binary-intrinsic cmpunord-ss float-sse-pack cmpss 3 "_mm_cmpunord_ss" :tags (:unord))
(def-binary-intrinsic cmpunord-ps float-sse-pack cmpps 3 "_mm_cmpunord_ps" :tags (:unord) :commutative t)

(def-comparison-intrinsic =-ss? float-sse-pack comiss 3 "_mm_comieq_ss" :commutative t :tags (:e))
(def-comparison-intrinsic =-ssu? float-sse-pack ucomiss 3 "_mm_ucomieq_ss" :commutative t :tags (:e))
(def-comparison-intrinsic <-ss? float-sse-pack comiss 3 "_mm_comilt_ss" :tags (:b))
(def-comparison-intrinsic <-ssu? float-sse-pack ucomiss 3 "_mm_ucomilt_ss" :tags (:b))
(def-comparison-intrinsic <=-ss? float-sse-pack comiss 3 "_mm_comile_ss" :tags (:be))
(def-comparison-intrinsic <=-ssu? float-sse-pack ucomiss 3 "_mm_ucomile_ss" :tags (:be))
(def-comparison-intrinsic >-ss? float-sse-pack comiss 3 "_mm_comigt_ss" :tags (:a))
(def-comparison-intrinsic >-ssu? float-sse-pack ucomiss 3 "_mm_ucomigt_ss" :tags (:a))
(def-comparison-intrinsic >=-ss? float-sse-pack comiss 3 "_mm_comige_ss" :tags (:ae))
(def-comparison-intrinsic >=-ssu? float-sse-pack ucomiss 3 "_mm_ucomige_ss" :tags (:ae))
(def-comparison-intrinsic /=-ss? float-sse-pack comiss 3 "_mm_comineq_ss" :commutative t :tags (:ne))
(def-comparison-intrinsic /=-ssu? float-sse-pack ucomiss 3 "_mm_ucomineq_ss" :commutative t :tags (:ne))

;; Misc

(def-binary-intrinsic unpackhi-ps float-sse-pack unpckhps 1 "_mm_unpackhi_ps")
(def-binary-intrinsic unpacklo-ps float-sse-pack unpcklps 1 "_mm_unpacklo_ps")

(def-binary-intrinsic move-ss float-sse-pack movss 1 "_mm_move_ss")

(def-binary-intrinsic movehl-ps float-sse-pack movhlps 1 "_mm_movehl_ps")
(def-binary-intrinsic movelh-ps float-sse-pack movlhps 1 "_mm_movelh_ps")

(def-unary-intrinsic movemask-ps (unsigned-byte 4) movmskps 1 "_mm_movemask_ps" :arg-type float-sse-pack)

;; Shuffle

(def-binary-intrinsic shuffle-ps float-sse-pack shufps 1 "_mm_shuffle_ps" :immediate-arg (unsigned-byte 8))

;; Conversion

(def-unary-intrinsic convert-pi32-to-ps float-sse-pack cvtdq2ps 3 "_mm_cvtepi32_ps" :arg-type int-sse-pack)
(def-unary-intrinsic convert-ps-to-pi32 int-sse-pack cvtps2dq 3 "_mm_cvtps_epi32" :arg-type float-sse-pack)
(def-unary-intrinsic truncate-ps-to-pi32 int-sse-pack cvttps2dq 3 "_mm_cvttps_epi32" :arg-type float-sse-pack)

(def-sse-int-intrinsic convert-si32-to-ss (signed-byte 32) float-sse-pack cvtsi2ss 3 "_mm_cvtsi32_ss")
(def-cvt-to-int32-intrinsic convert-ss-to-si32 (signed-byte 32) cvtss2si 3 "_mm_cvtss_si32" :arg-type float-sse-pack)
(def-cvt-to-int32-intrinsic truncate-ss-to-si32 (signed-byte 32) cvttss2si 3 "_mm_cvttss_si32" :arg-type float-sse-pack)

#+(or x86_64 x86-64)
(def-sse-int-intrinsic convert-si64-to-ss (signed-byte 64) float-sse-pack cvtsi2ss 3
                       #-msvc "_mm_cvtsi64_ss" #+msvc "_mm_cvtsi64x_ss")
#+(or x86_64 x86-64)
(def-unary-intrinsic convert-ss-to-si64 (signed-byte 64) cvtss2si 3
                     #-msvc "_mm_cvtss_si64" #+msvc "_mm_cvtss_si64x" :arg-type float-sse-pack)
#+(or x86_64 x86-64)
(def-unary-intrinsic truncate-ss-to-si64 (signed-byte 64) cvttss2si 3
                     #-msvc "_mm_cvttss_si64" #+msvc "_mm_cvttss_si64x" :arg-type float-sse-pack)

;;; Double-float

;; Initialization

#+sbcl
(def-float-set-intrinsic set-sd %set-sd double-float double-reg double-sse-pack movapd)

#+ecl
(progn
  (def-intrinsic set-sd (double-float) double-sse-pack "_mm_set_sd")
  (def-intrinsic set1-pd (double-float) double-sse-pack "_mm_set1_pd")

  (def-intrinsic set-pd (double-float double-float) double-sse-pack "_mm_set_pd")
  (def-intrinsic setr-pd (double-float double-float) double-sse-pack "_mm_setr_pd")

  (def-intrinsic setzero-pd () double-sse-pack "_mm_setzero_pd"))

;; Memory

(def-load-intrinsic mem-ref-sd double-sse-pack movsd "_mm_load_sd")

(def-load-intrinsic mem-ref-pd double-sse-pack movupd "_mm_loadu_pd")
(def-load-intrinsic mem-ref-apd double-sse-pack movapd "_mm_load_pd")

(def-load-intrinsic loadh-pd double-sse-pack movhpd "_mm_loadh_pd" :register-arg t)
(def-load-intrinsic loadl-pd double-sse-pack movlpd "_mm_loadl_pd" :register-arg t)

(def-store-intrinsic mem-set-sd double-sse-pack movsd "_mm_store_sd" :setf-name mem-ref-sd)

(def-store-intrinsic mem-set-pd double-sse-pack movupd "_mm_storeu_pd" :setf-name mem-ref-pd)
(def-store-intrinsic mem-set-apd double-sse-pack movapd "_mm_store_pd" :setf-name mem-ref-apd)

(def-store-intrinsic storeh-pd double-sse-pack movhpd "_mm_storeh_pd")
(def-store-intrinsic storel-pd double-sse-pack movlpd "_mm_storel_pd")

(def-store-intrinsic stream-pd double-sse-pack movntpd "_mm_stream_pd")

;; Arithmetics

(def-binary-intrinsic add-sd double-sse-pack addsd 3 "_mm_add_sd")
(def-binary-intrinsic add-pd double-sse-pack addpd 3 "_mm_add_pd" :commutative t)
(def-binary-intrinsic sub-sd double-sse-pack subsd 3 "_mm_sub_sd")
(def-binary-intrinsic sub-pd double-sse-pack subpd 3 "_mm_sub_pd")
(def-binary-intrinsic mul-sd double-sse-pack mulsd 5 "_mm_mul_sd")
(def-binary-intrinsic mul-pd double-sse-pack mulpd 5 "_mm_mul_pd" :commutative t)
(def-binary-intrinsic div-sd double-sse-pack divsd 13 "_mm_div_sd")
(def-binary-intrinsic div-pd double-sse-pack divpd 13 "_mm_div_pd")
(def-binary-intrinsic min-sd double-sse-pack minsd 3 "_mm_min_sd")
(def-binary-intrinsic min-pd double-sse-pack minpd 3 "_mm_min_pd" :commutative t)
(def-binary-intrinsic max-sd double-sse-pack maxsd 3 "_mm_max_sd")
(def-binary-intrinsic max-pd double-sse-pack maxpd 3 "_mm_max_pd" :commutative t)

(def-binary-intrinsic sqrt-sd double-sse-pack sqrtsd 20 "_mm_sqrt_sd")
(def-unary-intrinsic sqrt-pd double-sse-pack sqrtpd 20 "_mm_sqrt_pd")

;; Bitwise logic

#+sbcl
(def-not-intrinsic not-pd double-sse-pack xorpd)

(def-binary-intrinsic and-pd double-sse-pack andpd 1 "_mm_and_pd" :commutative t)
(def-binary-intrinsic andnot-pd double-sse-pack andnpd 1 "_mm_andnot_pd")
(def-binary-intrinsic or-pd double-sse-pack orpd 1 "_mm_or_pd" :commutative t)
(def-binary-intrinsic xor-pd double-sse-pack xorpd 1 "_mm_xor_pd" :commutative t)

;; Comparisons

(def-binary-intrinsic =-sd double-sse-pack cmpsd 3 "_mm_cmpeq_sd" :tags (:eq))
(def-binary-intrinsic =-pd double-sse-pack cmppd 3 "_mm_cmpeq_pd" :tags (:eq) :commutative t)
(def-binary-intrinsic <-sd double-sse-pack cmpsd 3 "_mm_cmplt_sd" :tags (:lt))
(def-binary-intrinsic <-pd double-sse-pack cmppd 3 "_mm_cmplt_pd" :tags (:lt))
(def-binary-intrinsic <=-sd double-sse-pack cmpsd 3 "_mm_cmple_sd" :tags (:le))
(def-binary-intrinsic <=-pd double-sse-pack cmppd 3 "_mm_cmple_pd" :tags (:le))
#+ecl
(def-binary-intrinsic >-sd double-sse-pack nil nil "_mm_cmpgt_sd")
#+ecl
(def-binary-intrinsic >-pd double-sse-pack nil nil "_mm_cmpgt_pd")
#+ecl
(def-binary-intrinsic >=-sd double-sse-pack nil nil "_mm_cmpge_sd")
#+ecl
(def-binary-intrinsic >=-pd double-sse-pack nil nil "_mm_cmpge_pd")

(def-binary-intrinsic /=-sd double-sse-pack cmpsd 3 "_mm_cmpneq_sd" :tags (:neq))
(def-binary-intrinsic /=-pd double-sse-pack cmppd 3 "_mm_cmpneq_pd" :tags (:neq) :commutative t)
(def-binary-intrinsic /<-sd double-sse-pack cmpsd 3 "_mm_cmpnlt_sd" :tags (:nlt))
(def-binary-intrinsic /<-pd double-sse-pack cmppd 3 "_mm_cmpnlt_pd" :tags (:nlt))
(def-binary-intrinsic /<=-sd double-sse-pack cmpsd 3 "_mm_cmpnle_sd" :tags (:nle))
(def-binary-intrinsic /<=-pd double-sse-pack cmppd 3 "_mm_cmpnle_pd" :tags (:nle))
#+ecl
(def-binary-intrinsic />-sd double-sse-pack nil nil "_mm_cmpngt_sd")
#+ecl
(def-binary-intrinsic />-pd double-sse-pack nil nil "_mm_cmpngt_pd")
#+ecl
(def-binary-intrinsic />=-sd double-sse-pack nil nil "_mm_cmpnge_sd")
#+ecl
(def-binary-intrinsic />=-pd double-sse-pack nil nil "_mm_cmpnge_pd")

(def-binary-intrinsic cmpord-sd double-sse-pack cmpsd 3 "_mm_cmpord_sd" :tags (:ord))  ; neither is NaN
(def-binary-intrinsic cmpord-pd double-sse-pack cmppd 3 "_mm_cmpord_pd" :tags (:ord) :commutative t)
(def-binary-intrinsic cmpunord-sd double-sse-pack cmpsd 3 "_mm_cmpunord_sd" :tags (:unord))
(def-binary-intrinsic cmpunord-pd double-sse-pack cmppd 3 "_mm_cmpunord_pd" :tags (:unord) :commutative t)

(def-comparison-intrinsic =-sd? double-sse-pack comisd 3 "_mm_comieq_sd" :commutative t :tags (:e))
(def-comparison-intrinsic =-sdu? double-sse-pack ucomisd 3 "_mm_ucomieq_sd" :commutative t :tags (:e))
(def-comparison-intrinsic <-sd? double-sse-pack comisd 3 "_mm_comilt_sd" :tags (:b))
(def-comparison-intrinsic <-sdu? double-sse-pack ucomisd 3 "_mm_ucomilt_sd" :tags (:b))
(def-comparison-intrinsic <=-sd? double-sse-pack comisd 3 "_mm_comile_sd" :tags (:be))
(def-comparison-intrinsic <=-sdu? double-sse-pack ucomisd 3 "_mm_ucomile_sd" :tags (:be))
(def-comparison-intrinsic >-sd? double-sse-pack comisd 3 "_mm_comigt_sd" :tags (:a))
(def-comparison-intrinsic >-sdu? double-sse-pack ucomisd 3 "_mm_ucomigt_sd" :tags (:a))
(def-comparison-intrinsic >=-sd? double-sse-pack comisd 3 "_mm_comige_sd" :tags (:ae))
(def-comparison-intrinsic >=-sdu? double-sse-pack ucomisd 3 "_mm_ucomige_sd" :tags (:ae))
(def-comparison-intrinsic /=-sd? double-sse-pack comisd 3 "_mm_comineq_sd" :commutative t :tags (:ne))
(def-comparison-intrinsic /=-sdu? double-sse-pack ucomisd 3 "_mm_ucomineq_sd" :commutative t :tags (:ne))

;; Misc

(def-binary-intrinsic unpackhi-pd double-sse-pack unpckhpd 1 "_mm_unpackhi_pd")
(def-binary-intrinsic unpacklo-pd double-sse-pack unpcklpd 1 "_mm_unpacklo_pd")

(def-binary-intrinsic move-sd double-sse-pack movsd 1 "_mm_move_sd")

(def-unary-intrinsic movemask-pd (unsigned-byte 2) movmskpd 1 "_mm_movemask_pd" :arg-type double-sse-pack)

;; Shuffle

(def-binary-intrinsic shuffle-pd double-sse-pack shufpd 1 "_mm_shuffle_pd" :immediate-arg (unsigned-byte 2))

;; Conversion

(def-unary-intrinsic convert-ps-to-pd double-sse-pack cvtps2pd 3 "_mm_cvtps_pd" :arg-type float-sse-pack)
(def-unary-intrinsic convert-pd-to-ps float-sse-pack cvtpd2ps 3 "_mm_cvtpd_ps" :arg-type double-sse-pack)

(def-binary-intrinsic convert-ss-to-sd double-sse-pack cvtss2sd 3 "_mm_cvtss_sd" :y-type float-sse-pack)
(def-binary-intrinsic convert-sd-to-ss float-sse-pack cvtsd2ss 3 "_mm_cvtsd_ss" :y-type double-sse-pack)

(def-unary-intrinsic convert-pi32-to-pd double-sse-pack cvtdq2pd 3 "_mm_cvtepi32_pd" :arg-type int-sse-pack)
(def-unary-intrinsic convert-pd-to-pi32 int-sse-pack cvtpd2dq 3 "_mm_cvtpd_epi32" :arg-type double-sse-pack)
(def-unary-intrinsic truncate-pd-to-pi32 int-sse-pack cvttpd2dq 3 "_mm_cvttpd_epi32" :arg-type double-sse-pack)

(def-sse-int-intrinsic convert-si32-to-sd (signed-byte 32) double-sse-pack cvtsi2ss 3 "_mm_cvtsi32_sd")
(def-cvt-to-int32-intrinsic convert-sd-to-si32 (signed-byte 32) cvtsd2si 3 "_mm_cvtsd_si32" :arg-type double-sse-pack)
(def-cvt-to-int32-intrinsic truncate-sd-to-si32 (signed-byte 32) cvttsd2si 3 "_mm_cvttsd_si32" :arg-type double-sse-pack)

#+(or x86_64 x86-64)
(def-sse-int-intrinsic convert-si64-to-sd (signed-byte 64) double-sse-pack cvtsi2ss 3
                       #-msvc "_mm_cvtsi64_sd" #+msvc "_mm_cvtsi64x_sd")
#+(or x86_64 x86-64)
(def-unary-intrinsic convert-sd-to-si64 (signed-byte 64) cvtsd2si 3
                     #-msvc "_mm_cvtsd_si64" #+msvc "_mm_cvtsd_si64x" :arg-type double-sse-pack)
#+(or x86_64 x86-64)
(def-unary-intrinsic truncate-sd-to-si64 (signed-byte 64) cvttsd2si 3
                     #-msvc "_mm_cvttsd_si64" #+msvc "_mm_cvttsd_si64x" :arg-type double-sse-pack)

;;; Integer

;; Initialization

#+ecl
(progn
  (def-intrinsic set1-pi8 (fixnum) int-sse-pack "_mm_set1_epi8")
  (def-intrinsic set1-pi16 (fixnum) int-sse-pack "_mm_set1_epi16")
  (def-intrinsic set1-pi32 (ext:integer32) int-sse-pack "_mm_set1_epi32")
  #+x86_64
  (def-intrinsic set1-pi64 (ext:integer64) int-sse-pack "_mm_set1_epi64x")

  (def-intrinsic set1-pu32 (ext:byte32) int-sse-pack "_mm_set1_epi32")
  #+x86_64
  (def-intrinsic set1-pu64 (ext:byte64) int-sse-pack "_mm_set1_epi64x")

  ;;-----
  (def-intrinsic set-pi8 (fixnum fixnum fixnum fixnum
                                 fixnum fixnum fixnum fixnum
                                 fixnum fixnum fixnum fixnum
                                 fixnum fixnum fixnum fixnum) int-sse-pack "_mm_set_epi8")
  (def-intrinsic set-pi16 (fixnum fixnum fixnum fixnum
                                  fixnum fixnum fixnum fixnum) int-sse-pack "_mm_set_epi16")
  (def-intrinsic set-pi32 (ext:integer32 ext:integer32 ext:integer32 ext:integer32) int-sse-pack "_mm_set_epi32")
  #+x86_64
  (def-intrinsic set-pi64 (ext:integer64 ext:integer64) int-sse-pack "_mm_set_epi64x")

  (def-intrinsic set-pu32 (ext:byte32 ext:byte32 ext:byte32 ext:byte32) int-sse-pack "_mm_set_epi32")
  #+x86_64
  (def-intrinsic set-pu64 (ext:byte64 ext:byte64) int-sse-pack "_mm_set_epi64x")

  ;;-----
  (def-intrinsic setr-pi8 (fixnum fixnum fixnum fixnum
                                  fixnum fixnum fixnum fixnum
                                  fixnum fixnum fixnum fixnum
                                  fixnum fixnum fixnum fixnum) int-sse-pack "_mm_setr_epi8")
  (def-intrinsic setr-pi16 (fixnum fixnum fixnum fixnum
                                   fixnum fixnum fixnum fixnum) int-sse-pack "_mm_setr_epi16")
  (def-intrinsic setr-pi32 (ext:integer32 ext:integer32 ext:integer32 ext:integer32) int-sse-pack "_mm_setr_epi32")
  #+x86_64
  (def-intrinsic setr-pi64 (ext:integer64 ext:integer64) int-sse-pack "_mm_set_epi64x" :reorder-args t)

  (def-intrinsic setr-pu32 (ext:byte32 ext:byte32 ext:byte32 ext:byte32) int-sse-pack "_mm_setr_epi32")
  #+x86_64
  (def-intrinsic setr-pu64 (ext:byte64 ext:byte64) int-sse-pack "_mm_set_epi64x" :reorder-args t)

  ;;-----
  (def-intrinsic setzero-pi () int-sse-pack "_mm_setzero_si128"))

;; Memory

(def-load-intrinsic mem-ref-pi int-sse-pack movdqu "_mm_loadu_si128")
(def-load-intrinsic mem-ref-api int-sse-pack movdqa "_mm_load_si128")

(def-load-intrinsic mem-ref-si64 int-sse-pack movd "_mm_loadl_epi64")

(def-store-intrinsic mem-set-pi int-sse-pack movdqu "_mm_storeu_si128" :setf-name mem-ref-pi)
(def-store-intrinsic mem-set-api int-sse-pack movdqa "_mm_store_si128" :setf-name mem-ref-api)

(def-store-intrinsic mem-set-si64 int-sse-pack movd "_mm_storel_epi64" :setf-name mem-ref-si64)

(def-store-intrinsic stream-pi int-sse-pack movntdq "_mm_stream_si128")

;; Masked move

#+ecl
(def-mem-intrinsic maskmoveu-pi "char" nil "_mm_maskmoveu_si128" :prefix-args (int-sse-pack int-sse-pack))

#+sbcl
(progn
  (defknown %maskmoveu-pi (sse-pack sse-pack system-area-pointer fixnum) (values) (unsafe))

  (define-vop (%maskmoveu-pi)
    (:translate %maskmoveu-pi)
    (:args (value :scs (sse-reg))
           (mask :scs (sse-reg))
           (sap :scs (sap-reg) :target rdi)
           (offset :scs (signed-reg)))
    (:arg-types sse-pack sse-pack system-area-pointer signed-num)
    (:temporary (:sc sap-reg :offset rdi-offset :from :eval) rdi)
    (:policy :fast-safe)
    (:note "inline MASKMOVEU operation")
    (:generator 5
      (if (location= sap rdi)
          (inst add rdi offset)
          (inst lea rdi (make-ea :qword :base sap :index offset)))
      (inst maskmovdqu value mask)))

  (define-vop (%maskmoveu-pi-c)
    (:translate %maskmoveu-pi)
    (:args (value :scs (sse-reg))
           (mask :scs (sse-reg))
           (sap :scs (sap-reg) :target rdi))
    (:arg-types sse-pack sse-pack system-area-pointer (:constant (signed-byte 32)))
    (:info offset)
    (:temporary (:sc sap-reg :offset rdi-offset :from :eval) rdi)
    (:policy :fast-safe)
    (:note "inline MASKMOVEU operation")
    (:generator 4
      (if (location= sap rdi)
          (unless (= offset 0)
            (inst add rdi offset))
          (if (= offset 0)
              (inst mov rdi sap)
              (inst lea rdi (make-ea :qword :base sap :disp offset))))
      (inst maskmovdqu value mask)))

  (def-splice-transform %maskmoveu-pi (value mask (sap+ sap offset1) offset2)
    (%maskmoveu-pi value mask sap (+ offset1 offset2))))

;; Arithmetics

(def-binary-intrinsic add-pi8    int-sse-pack paddb 1 "_mm_add_epi8" :commutative t)
(def-binary-intrinsic add-pi16   int-sse-pack paddw 1 "_mm_add_epi16" :commutative t)
(def-binary-intrinsic add-pi32   int-sse-pack paddd 1 "_mm_add_epi32" :commutative t)
(def-binary-intrinsic add-pi64   int-sse-pack paddq 1 "_mm_add_epi64" :commutative t)

(def-binary-intrinsic adds-pi8   int-sse-pack paddsb 1 "_mm_adds_epi8" :commutative t)
(def-binary-intrinsic adds-pi16  int-sse-pack paddsw 1 "_mm_adds_epi16" :commutative t)
(def-binary-intrinsic adds-pu8   int-sse-pack paddusb 1 "_mm_adds_epu8" :commutative t)
(def-binary-intrinsic adds-pu16  int-sse-pack paddusw 1 "_mm_adds_epu16" :commutative t)

(def-binary-intrinsic avg-pu8    int-sse-pack pavgb 1 "_mm_avg_epu8" :commutative t)
(def-binary-intrinsic avg-pu16   int-sse-pack pavgw 1 "_mm_avg_epu16" :commutative t)

(def-binary-intrinsic madd-pi16  int-sse-pack pmaddwd 1 "_mm_madd_epi16" :commutative t)

(def-binary-intrinsic max-pu8    int-sse-pack pmaxub 1 "_mm_max_epu8" :commutative t)
(def-binary-intrinsic max-pi16   int-sse-pack pmaxsw 1 "_mm_max_epi16" :commutative t)
(def-binary-intrinsic min-pu8    int-sse-pack pminub 1 "_mm_min_epu8" :commutative t)
(def-binary-intrinsic min-pi16   int-sse-pack pminsw 1 "_mm_min_epi16" :commutative t)

(def-binary-intrinsic mulhi-pi16 int-sse-pack pmulhw 3 "_mm_mulhi_epi16" :commutative t)
(def-binary-intrinsic mulhi-pu16 int-sse-pack pmulhuw 3 "_mm_mulhi_epu16" :commutative t)
(def-binary-intrinsic mullo-pi16 int-sse-pack pmullw 3 "_mm_mullo_epi16" :commutative t)

(def-binary-intrinsic mul-pu32   int-sse-pack pmuludq 3 "_mm_mul_epu32" :commutative t)

(def-binary-intrinsic sad-pu8    int-sse-pack psadbw 1 "_mm_sad_epu8" :commutative t)

(def-binary-intrinsic sub-pi8    int-sse-pack psubb 1 "_mm_sub_epi8")
(def-binary-intrinsic sub-pi16   int-sse-pack psubw 1 "_mm_sub_epi16")
(def-binary-intrinsic sub-pi32   int-sse-pack psubd 1 "_mm_sub_epi32")
(def-binary-intrinsic sub-pi64   int-sse-pack psubq 1 "_mm_sub_epi64")

(def-binary-intrinsic subs-pi8   int-sse-pack psubsb 1 "_mm_subs_epi8")
(def-binary-intrinsic subs-pi16  int-sse-pack psubsw 1 "_mm_subs_epi16")
(def-binary-intrinsic subs-pu8   int-sse-pack psubusb 1 "_mm_subs_epu8")
(def-binary-intrinsic subs-pu16  int-sse-pack psubusw 1 "_mm_subs_epu16")

;; Bitwise logic

#+sbcl
(def-not-intrinsic not-pi int-sse-pack pxor)

(def-binary-intrinsic and-pi int-sse-pack pand 1 "_mm_and_si128" :commutative t)
(def-binary-intrinsic andnot-pi int-sse-pack pandn 1 "_mm_andnot_si128")
(def-binary-intrinsic or-pi int-sse-pack por 1 "_mm_or_si128" :commutative t)
(def-binary-intrinsic xor-pi int-sse-pack pxor 1 "_mm_xor_si128" :commutative t)

;; Shifts

(def-unary-intrinsic slli-pi int-sse-pack pslldq 1 "_mm_slli_si128" :partial :one-arg :immediate-arg (unsigned-byte 8))

(def-sse-int-intrinsic slli-pi16 fixnum int-sse-pack psllw 3 "_mm_slli_epi16" :make-temporary t
                       :defun-body "_mm_sll_epi16(#0,_mm_cvtsi32_si128(#1))")
(def-sse-int-intrinsic slli-pi32 fixnum int-sse-pack pslld 3 "_mm_slli_epi32" :make-temporary t
                       :defun-body "_mm_sll_epi32(#0,_mm_cvtsi32_si128(#1))")
(def-sse-int-intrinsic slli-pi64 fixnum int-sse-pack psllq 3 "_mm_slli_epi64" :make-temporary t
                       :defun-body "_mm_sll_epi64(#0,_mm_cvtsi32_si128(#1))")
(def-binary-intrinsic sll-pi16 int-sse-pack psllw 1 "_mm_sll_epi16")
(def-binary-intrinsic sll-pi32 int-sse-pack pslld 1 "_mm_sll_epi32")
(def-binary-intrinsic sll-pi64 int-sse-pack psllq 1 "_mm_sll_epi64")

(def-sse-int-intrinsic srai-pi16 fixnum int-sse-pack psraw 3 "_mm_srai_epi16" :make-temporary t
                       :defun-body "_mm_sra_epi16(#0,_mm_cvtsi32_si128(#1))")
(def-sse-int-intrinsic srai-pi32 fixnum int-sse-pack psrad 3 "_mm_srai_epi32" :make-temporary t
                       :defun-body "_mm_sra_epi32(#0,_mm_cvtsi32_si128(#1))")
(def-binary-intrinsic sra-pi16 int-sse-pack psraw 1 "_mm_sra_epi16")
(def-binary-intrinsic sra-pi32 int-sse-pack psrad 1 "_mm_sra_epi32")

(def-unary-intrinsic srli-pi int-sse-pack psrldq 1 "_mm_srli_si128" :partial :one-arg :immediate-arg (unsigned-byte 8))

(def-sse-int-intrinsic srli-pi16 fixnum int-sse-pack psrlw 3 "_mm_srli_epi16" :make-temporary t
                       :defun-body "_mm_srl_epi16(#0,_mm_cvtsi32_si128(#1))")
(def-sse-int-intrinsic srli-pi32 fixnum int-sse-pack psrld 3 "_mm_srli_epi32" :make-temporary t
                       :defun-body "_mm_srl_epi32(#0,_mm_cvtsi32_si128(#1))")
(def-sse-int-intrinsic srli-pi64 fixnum int-sse-pack psrlq 3 "_mm_srli_epi64" :make-temporary t
                       :defun-body "_mm_srl_epi64(#0,_mm_cvtsi32_si128(#1))")
(def-binary-intrinsic srl-pi16 int-sse-pack psrlw 1 "_mm_srl_epi16")
(def-binary-intrinsic srl-pi32 int-sse-pack psrld 1 "_mm_srl_epi32")
(def-binary-intrinsic srl-pi64 int-sse-pack psrlq 1 "_mm_srl_epi64")

#+sbcl
(macrolet ((defimm (name insn bits &key arithmetic)
             `(define-vop (,(symbolicate "%" name "-IMM") sse-int-base-op)
                (:translate ,name)
                (:args (x :scs (sse-reg) :target r))
                (:arg-types sse-pack (:constant fixnum))
                (:result-types sb-kernel:int-sse-pack)
                (:info immv)
                (:generator 1
                  ,@(let ((core `(progn
                                   (ensure-move int-sse-pack r x)
                                   (unless (= immv 0)
                                     (inst ,insn r immv)))))
                      (if arithmetic
                          `((when (or (< immv 0) (>= immv ,bits))
                              (setf immv ,bits))
                            ,core)
                          `((if (or (< immv 0) (>= immv ,bits))
                                (inst pxor r r)
                                ,core))))))))
  (defimm slli-pi16 psllw-imm 16)
  (defimm slli-pi32 pslld-imm 32)
  (defimm slli-pi64 psllq-imm 64)
  (defimm srai-pi16 psraw-imm 16 :arithmetic t)
  (defimm srai-pi32 psrad-imm 32 :arithmetic t)
  (defimm srli-pi16 psrlw-imm 16)
  (defimm srli-pi32 psrld-imm 32)
  (defimm srli-pi64 psrlq-imm 64))

;; Comparisons

(def-binary-intrinsic =-pi8  int-sse-pack pcmpeqb 1 "_mm_cmpeq_epi8")
(def-binary-intrinsic =-pi16 int-sse-pack pcmpeqw 1 "_mm_cmpeq_epi16")
(def-binary-intrinsic =-pi32 int-sse-pack pcmpeqd 1 "_mm_cmpeq_epi32")

#+ecl
(def-binary-intrinsic <-pi8  int-sse-pack nil nil "_mm_cmplt_epi8")
#+ecl
(def-binary-intrinsic <-pi16 int-sse-pack nil nil "_mm_cmplt_epi16")
#+ecl
(def-binary-intrinsic <-pi32 int-sse-pack nil nil "_mm_cmplt_epi32")

(def-binary-intrinsic >-pi8  int-sse-pack pcmpgtb 1 "_mm_cmpgt_epi8")
(def-binary-intrinsic >-pi16 int-sse-pack pcmpgtw 1 "_mm_cmpgt_epi16")
(def-binary-intrinsic >-pi32 int-sse-pack pcmpgtd 1 "_mm_cmpgt_epi32")

;; Misc

(def-binary-intrinsic packs-pi16 int-sse-pack packsswb 1 "_mm_packs_epi16")
(def-binary-intrinsic packs-pi32 int-sse-pack packssdw 1 "_mm_packs_epi32")
(def-binary-intrinsic packus-pi16 int-sse-pack packuswb 1 "_mm_packus_epi16")

(def-unary-intrinsic extract-pi16 (unsigned-byte 16) pextrw 1 "_mm_extract_epi16"
                     :immediate-arg (unsigned-byte 8) :arg-type int-sse-pack)
(def-sse-int-intrinsic insert-pi16 fixnum int-sse-pack pinsrw 1 "_mm_insert_epi16"
                       :immediate-arg (unsigned-byte 8))

(def-unary-intrinsic movemask-pi8 (unsigned-byte 16) pmovmskb 1 "_mm_movemask_epi8" :arg-type int-sse-pack)

(def-binary-intrinsic unpackhi-pi8  int-sse-pack punpckhbw 1 "_mm_unpackhi_epi8")
(def-binary-intrinsic unpackhi-pi16 int-sse-pack punpckhwd 1 "_mm_unpackhi_epi16")
(def-binary-intrinsic unpackhi-pi32 int-sse-pack punpckhdq 1 "_mm_unpackhi_epi32")
(def-binary-intrinsic unpackhi-pi64 int-sse-pack punpckhqdq 1 "_mm_unpackhi_epi64")

(def-binary-intrinsic unpacklo-pi8  int-sse-pack punpcklbw 1 "_mm_unpacklo_epi8")
(def-binary-intrinsic unpacklo-pi16 int-sse-pack punpcklwd 1 "_mm_unpacklo_epi16")
(def-binary-intrinsic unpacklo-pi32 int-sse-pack punpckldq 1 "_mm_unpacklo_epi32")
(def-binary-intrinsic unpacklo-pi64 int-sse-pack punpcklqdq 1 "_mm_unpacklo_epi64")

(def-unary-intrinsic move-pi64 int-sse-pack movq 1 "_mm_move_epi64")

;; Shuffle

(def-unary-intrinsic shuffle-pi32 int-sse-pack pshufd 1 "_mm_shuffle_epi32" :immediate-arg (unsigned-byte 8))
(def-unary-intrinsic shufflelo-pi16 int-sse-pack pshuflw 1 "_mm_shufflelo_epi16" :immediate-arg (unsigned-byte 8))
(def-unary-intrinsic shufflehi-pi16 int-sse-pack pshufhw 1 "_mm_shufflehi_epi16" :immediate-arg (unsigned-byte 8))

;; Conversion

#+sbcl
(progn
  (export 'convert-si32-to-pi)
  (defknown convert-si32-to-pi ((signed-byte 32)) int-sse-pack (foldable flushable))
  (export 'convert-su32-to-pi)
  (defknown convert-su32-to-pi ((unsigned-byte 32)) int-sse-pack (foldable flushable))
  (export 'convert-si64-to-pi)
  (defknown convert-si64-to-pi ((signed-byte 64)) int-sse-pack (foldable flushable))
  (export 'convert-su64-to-pi)
  (defknown convert-su64-to-pi ((unsigned-byte 64)) int-sse-pack (foldable flushable))
  (defknown %set-int ((signed-byte 64)) int-sse-pack (foldable flushable always-translatable))
  (defknown %set-uint ((unsigned-byte 64)) int-sse-pack (foldable flushable always-translatable))

  (define-vop (%set-int)
    (:translate %set-int %set-uint
                convert-si32-to-pi convert-su32-to-pi
                convert-si64-to-pi convert-su64-to-pi)
    (:args (arg :scs (signed-reg unsigned-reg signed-stack unsigned-stack)))
    (:arg-types untagged-num)
    (:results (dst :scs (sse-reg)))
    (:result-types sb-kernel:int-sse-pack)
    (:policy :fast-safe)
    (:generator 1
      (inst movd dst arg))))

#+ecl
(progn
  (def-intrinsic convert-si32-to-pi (ext:integer32) int-sse-pack "_mm_cvtsi32_si128")
  (def-intrinsic convert-su32-to-pi (ext:byte32) int-sse-pack "_mm_cvtsi32_si128")
  #+x86_64
  (def-intrinsic convert-si64-to-pi (ext:integer64) int-sse-pack #-msvc "_mm_cvtsi64_si128" #+msvc "_mm_cvtsi64x_si128")
  #+x86_64
  (def-intrinsic convert-su64-to-pi (ext:byte64) int-sse-pack #-msvc "_mm_cvtsi64_si128" #+msvc "_mm_cvtsi64x_si128"))

(def-cvt-to-int32-intrinsic convert-pi-to-si32 (signed-byte 32) movd 1 "_mm_cvtsi128_si32"
                            :arg-type int-sse-pack)
(def-unary-intrinsic convert-pi-to-su32 (unsigned-byte 32) movd 1 "_mm_cvtsi128_si32"
                     :result-size :dword :arg-type int-sse-pack)

#+(or x86_64 x86-64)
(def-unary-intrinsic convert-pi-to-si64 (signed-byte 64) movd 1
                     #-msvc "_mm_cvtsi128_si64" #+msvc "_mm_cvtsi128_si64x" :arg-type int-sse-pack)
#+(or x86_64 x86-64)
(def-unary-intrinsic convert-pi-to-su64 (unsigned-byte 64) movd 1
                     #-msvc "_mm_cvtsi128_si64" #+msvc "_mm_cvtsi128_si64x" :arg-type int-sse-pack)

