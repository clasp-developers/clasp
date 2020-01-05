(in-package "COMPILER")

;;; Inlining MAKE-ARRAY
;;; Right now we only do simple cases - :initial-element, :initial-contents, and :element-type only are ok.
;;; Ideally this probably shouldn't exist and rather it should be an inline definition Cleavir can work on-
;;;  buuuuut I'm not sure. That would involve folding upgraded-array-element-type.

;; returns names of a simple vector constructor and a simple mdarray constructor.
(defun uaet-info (uaet)
  (case uaet
    ((t) (values 'core:make-simple-vector-t 'core:make-simple-mdarray-t))
    ((bit) (values 'core:make-simple-vector-bit 'core:make-simple-mdarray-bit))
    ((base-char) (values 'core:make-simple-vector-base-char 'core:make-simple-mdarray-base-char))
    ((character) (values 'core:make-simple-vector-character 'core:make-simple-mdarray-character))
    ((single-float) (values 'core:make-simple-vector-single-float 'core:make-simple-mdarray-single-float))
    ((double-float) (values 'core:make-simple-vector-double-float 'core:make-simple-mdarray-double-float))
    ((ext:integer2) (values 'core:make-simple-vector-int2 'core:make-simple-mdarray-int2))
    ((ext:byte2) (values 'core:make-simple-vector-byte2 'core:make-simple-mdarray-byte2))
    ((ext:integer4) (values 'core:make-simple-vector-int4 'core:make-simple-mdarray-int4))
    ((ext:byte4) (values 'core:make-simple-vector-byte4 'core:make-simple-mdarray-byte4))
    ((ext:integer8) (values 'core:make-simple-vector-int8 'core:make-simple-mdarray-int8))
    ((ext:byte8) (values 'core:make-simple-vector-byte8 'core:make-simple-mdarray-byte8))
    ((ext:integer16) (values 'core:make-simple-vector-int16 'core:make-simple-mdarray-int16))
    ((ext:byte16) (values 'core:make-simple-vector-byte16 'core:make-simple-mdarray-byte16))
    ((ext:integer32) (values 'core:make-simple-vector-int32 'core:make-simple-mdarray-int32))
    ((ext:byte32) (values 'core:make-simple-vector-byte32 'core:make-simple-mdarray-byte32))
    ((ext:integer64) (values 'core:make-simple-vector-int64 'core:make-simple-mdarray-int64))
    ((ext:byte64) (values 'core:make-simple-vector-byte64 'core:make-simple-mdarray-byte64))
    ((fixnum) (values 'core:make-simple-vector-fixnum 'core:make-simple-mdarray-fixnum))
    ;; size_t?
    (t (values nil nil))))

(define-compiler-macro make-array (&whole form dimensions
                                          &key (element-type t)
                                          (adjustable nil ap) (fill-pointer nil fp)
                                          (initial-element nil iesp) (initial-contents nil icsp)
                                          (displaced-to nil dp) (displaced-index-offset 0 diop)
                                          &environment env)
  (if (constantp element-type env)
      (let ((et (ext:constant-form-value element-type env)))
        (multiple-value-bind (make-sv make-smdarray)
            (uaet-info (upgraded-array-element-type et env))
          (cond ((null make-sv) ; unknown UAET; fall back
                 (cmp:warn-undefined-type nil et)
                 form)
                ((or ap fp dp diop) ; complex array; for now punt, could be more specific later
                 form)
                ((and iesp icsp) ; error; let the full function handle it and warn.
                 (cmp:warn-icsp-iesp-both-specified nil)
                 form)
                ((constantp dimensions env)
                 ;; do constant dimensions ahead of time
                 ;; FIXME: ideally this clause wouldn't exist, and constant propagation/types
                 ;; would fix it from the next clause. probably.
                 (let* ((dimensions (ext:constant-form-value dimensions env))
                        (form
                          (typecase dimensions
                            (ext:array-index
                             `(,make-sv ,dimensions ,initial-element ,iesp))
                            ((cons ext:array-index null)
                             `(,make-sv ,(car dimensions) ,initial-element ,iesp))
                            (list
                             `(,make-smdarray ',dimensions ,initial-element ,iesp))
                            (t ; type error, but let the full function handle it. FIXME: warn
                             form))))
                   (if icsp
                       `(core::fill-array-with-seq ,form ,initial-contents)
                       form)))
                (t (let* ((dimsym (gensym "DIMENSIONS"))
                          (iesym (gensym "INITIAL-ELEMENT"))
                          (form
                            `(let ((,dimsym ,dimensions) (,iesym ,initial-element))
                               (etypecase ,dimsym
                                 ;; vectors are (probably) most common; check that first.
                                 (ext:array-index
                                  (,make-sv ,dimsym ,iesym ,iesp))
                                 ((cons ext:array-index null)
                                  (,make-sv (car ,dimsym) ,iesym ,iesp))
                                 (list
                                  (,make-smdarray ,dimsym ,iesym ,iesp))))))
                     (if icsp
                         `(core::fill-array-with-seq ,form ,initial-contents)
                         form))))))
      form))

(define-compiler-macro sys:make-vector
    (&whole form element-type dimension
            &optional (adjustable nil ap) (fill-pointer nil fp)
            (displaced-to nil dp) (displaced-index-offset 0 diop)
            initial-element iesp
            &environment env)
  ;; FIXME: This should do better for constant NIL adjustable, etc.
  ;; As is, we won't expand if initial-element is provided.
  (if (and (constantp element-type env)
           (not (or ap fp dp diop)))
      (let ((make-sv (uaet-info (ext:constant-form-value element-type env))))
        (if make-sv
            `(,make-sv ,dimension ,initial-element ,iesp)
            ;; unknown uaet, give up
            form))
      form))
