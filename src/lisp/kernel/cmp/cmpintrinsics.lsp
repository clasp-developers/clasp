;;
;;;    File: cmpintrinsics.lsp
;;;

;; Should be commented out
#+(or)
(eval-when (:execute)
  (setq core:*echo-repl-read* t))

;; Copyright (c) 2014, Christian E. Schafmeister
;;
;; CLASP is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Library General Public
;; License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;;
;; See directory 'clasp/licenses' for full details.
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; -^-


(in-package :cmp)

(defvar *irbuilder* nil
  "This is the IRBuilder that defines where all irc-xxx functions that generate IR code put the code.
Set this to other IRBuilders to make code go where you want")


(defvar *irbuilder-function-alloca* nil
  "Maintains an IRBuilder for function alloca instructions")
(defvar *irbuilder-function-body* nil
  "Maintains an IRBuilder for function body IR code")
(defvar *compilation-module-index* 0
  "Incremented for each module build within a compile-file.
   It's used to get the proper order for ctor initialization.")

;;
;; Create types
;;

;; OPEN / TODO:
;; - ptrdiff_t
;; - ssize_t
;; - time_t


(defun llvm-print (msg)
  (irc-intrinsic "debugMessage" (irc-bit-cast (module-make-global-string msg) %i8*%)))
(defun llvm-print-pointer (msg ptr)
  (llvm-print msg)
  (irc-intrinsic "debugPointer" (irc-bit-cast ptr %i8*%)))
(defun llvm-print-size_t (msg st)
  (llvm-print msg)
  (irc-intrinsic "debugPrint_size_t" (irc-bit-cast st %i64%)))


(defstruct (c++-struct :named (:type vector))
  name ; The symbol-macro name of the type
  tag  ; The tag of the objects of this type
  type-getter ; A single argument lambda that when passed an (thread-local-llvm-context) returns the type
  field-type-getters ; An alist of name/type-getter-functions
  field-offsets
  field-indices) 
  
(defmacro define-c++-struct (name tag fields)
  "Defines the llvm struct and the dynamic variable OFFSETS.name that contains an alist of field
names to offsets."
  (let ((layout (gensym))
        (gs-field (gensym))
        (field-index (gensym)))
    (let ((define-symbol-macro `(define-symbol-macro ,name
                                    (llvm-sys:struct-type-get
                                     (thread-local-llvm-context)
                                     (list ,@(mapcar #'first fields))
                                     nil))))
      (let ((field-offsets `(let ((,layout (llvm-sys:data-layout-get-struct-layout (system-data-layout) ,name))
                                  (,field-index 0))
                              (mapcar (lambda (,gs-field)
                                        (prog1
                                            (cons (second ,gs-field) (- (llvm-sys:struct-layout-get-element-offset ,layout ,field-index) ,tag))
                                          (incf ,field-index)))
                                      ',fields)))
            (field-indices `(let ((,field-index 0))    ;
                              (mapcar (lambda (,gs-field) ;
                                        (prog1         ;
                                            (cons (second ,gs-field) ,field-index) ;
                                          (incf ,field-index))) ;
                                      ',fields)))
            (field-type-getters-list (mapcar (lambda (type-name) ;
                                               #+(or)(format t "type-name -> ~s cadr -> ~s  ,car -> ~s~%" type-name (cadr type-name) (car type-name)) ;
                                               `(cons ',(cadr type-name) (lambda () (llvm-sys:type-get-pointer-to ,(macroexpand (car type-name))))))
                                             fields)))
        #+(or)
        (progn
          (format t "define-symbol-macro = ~s~%" define-symbol-macro)
          (format t "field-offsets -> ~s~%" field-offsets)
          (format t "field-indices -> ~s~%" field-indices)
          (format t "field-type-getters-list -> ~s~%" field-type-getters-list))
        (let ((final `(progn
                        ,define-symbol-macro
                        (defparameter ,(intern (core:bformat nil "INFO.%s" (string name)))
                          (make-c++-struct :name ,name
                                           :tag ,tag
                                           :type-getter (lambda () (progn ,name))
                                           :field-type-getters (list ,@field-type-getters-list)
                                           :field-offsets ,field-offsets
                                           :field-indices ,field-indices)))))
          final)))))

(defun c++-field-offset (field-name info)
  "Return the integer byte offset of the field for the c++-struct including the tag"
  (let ((entry (assoc field-name (c++-struct-field-offsets info))))
    (unless entry (error "Could not find field ~a in ~s" field-name info))
    (cdr entry)))

(defun c++-field-index (field-name info)
  "Return the index of the field "
  (let ((entry (assoc field-name (c++-struct-field-indices info))))
    (unless entry (error "Could not find field ~a in ~s" field-name info))
    (cdr entry)))

(defun c++-struct-type (struct-info)
  (funcall (c++-struct-type-getter struct-info)))

(defun c++-struct*-type (struct-info)
  (llvm-sys:type-get-pointer-to (funcall (c++-struct-type-getter struct-info))))
  
(defun c++-field-ptr (struct-info tagged-object field-name &optional (label ""))
  (let* ((tagged-object-i8* (irc-bit-cast tagged-object %i8*%))
         (field* (irc-gep tagged-object-i8* (list (jit-constant-i64 (c++-field-offset field-name struct-info)))))
         (field-type-getter (cdr (assoc field-name (c++-struct-field-type-getters struct-info))))
         (field-ptr (irc-bit-cast field* (funcall field-type-getter) label)))
    field-ptr))

                                    
(define-symbol-macro %i1% (llvm-sys:type-get-int1-ty (thread-local-llvm-context)))
(define-symbol-macro %i3% (llvm-sys:type-get-int-nty (thread-local-llvm-context) 3))

(define-symbol-macro %i8% (llvm-sys:type-get-int8-ty (thread-local-llvm-context))) ;; -> CHAR / BYTE
(define-symbol-macro %i8*% (llvm-sys:type-get-pointer-to %i8%))
(define-symbol-macro %i8**% (llvm-sys:type-get-pointer-to %i8*%))
(define-symbol-macro %i8*[1]% (llvm-sys:array-type-get %i8*% 1))



(define-symbol-macro %i16% (llvm-sys:type-get-int16-ty (thread-local-llvm-context))) ;; -> SHORT
(define-symbol-macro %i16*% (llvm-sys:type-get-pointer-to %i16%))
(define-symbol-macro %i16**% (llvm-sys:type-get-pointer-to %i16*%))

(define-symbol-macro %i32% (llvm-sys:type-get-int32-ty (thread-local-llvm-context))) ;; -> INT
(define-symbol-macro %i32*% (llvm-sys:type-get-pointer-to %i32%))
(define-symbol-macro %i32**% (llvm-sys:type-get-pointer-to %i32*%))

(define-symbol-macro %i64% (llvm-sys:type-get-int64-ty (thread-local-llvm-context))) ;; -> LONG, LONG LONG
(define-symbol-macro %i64*% (llvm-sys:type-get-pointer-to %i64%))
(define-symbol-macro %i64**% (llvm-sys:type-get-pointer-to %i64*%))

(define-symbol-macro %i128% (llvm-sys:type-get-int128-ty (thread-local-llvm-context))) ;; -> NOT USED !!!

(define-symbol-macro %fixnum% #+64-bit %i64%
                              #+32-bit %i32%)
(define-symbol-macro %word% #+64-bit %i64% #+32-bit %i32%)
(define-symbol-macro %uint% %i32%) ; FIXME: export from C++ probably

(define-symbol-macro %float% (llvm-sys:type-get-float-ty (thread-local-llvm-context)))
(define-symbol-macro %double% (llvm-sys:type-get-double-ty (thread-local-llvm-context)))
#+long-float (define-symbol-macro %long-float% (llvm-sys:type-get-long-float-ty (thread-local-llvm-context)))

(define-symbol-macro %size_t% #+64-bit %i64%
                              #+32-bit %i32%)
(define-symbol-macro %badge% %size_t%)
(define-symbol-macro %atomic<size_t>% %size_t%)
(define-symbol-macro %size_t*% (llvm-sys:type-get-pointer-to %size_t%))
(define-symbol-macro %size_t**% (llvm-sys:type-get-pointer-to %size_t*%))
(define-symbol-macro %size_t[0]% (llvm-sys:array-type-get %size_t% 0))

(define-symbol-macro %void% (llvm-sys:type-get-void-ty (thread-local-llvm-context)))
(define-symbol-macro %void*% (llvm-sys:type-get-pointer-to %void%))
(define-symbol-macro %void**% (llvm-sys:type-get-pointer-to %void*%))

(define-symbol-macro %vtable*% %i8*%)

;;(define-symbol-macro %exception-struct% (llvm-sys:struct-type-get (thread-local-llvm-context) (list %i8*% %i32%) "exception-struct" nil))
(define-symbol-macro %exception-struct% (llvm-sys:struct-type-get (thread-local-llvm-context) (list %i8*% %i32%) nil))
(define-symbol-macro %{i32.i1}% (llvm-sys:struct-type-get (thread-local-llvm-context) (list %i32% %i1%) nil))
(define-symbol-macro %{i64.i1}% (llvm-sys:struct-type-get (thread-local-llvm-context) (list %i64% %i1%) nil))


;;  "A ctor void ()* function prototype"
(define-symbol-macro %fn-ctor%
  (llvm-sys:function-type-get %void% nil))
(defvar +fn-ctor-argument-names+ nil)
;;;  "A pointer to the ctor function prototype")
(define-symbol-macro %fn-ctor*% (llvm-sys:type-get-pointer-to %fn-ctor%))

;;;  "A run-all void ()* function prototype")
(define-symbol-macro %fn-shut-down% (llvm-sys:function-type-get %void% nil))
(define-symbol-macro %fn-start-up% (llvm-sys:function-type-get %t*% (list %t*%)))
(defvar +fn-start-up-argument-names+ nil)
;;;  "A pointer to the run-all function prototype")
(define-symbol-macro %fn-start-up*% (llvm-sys:type-get-pointer-to %fn-start-up%))

(define-symbol-macro %global-ctors-struct% (llvm-sys:struct-type-get (thread-local-llvm-context) (list %i32% %fn-ctor*% %i8*%) nil))

;;;  "An array of pointers to the global-ctors-struct")
(define-symbol-macro %global-ctors-struct[1]% (llvm-sys:array-type-get %global-ctors-struct% 1))


(define-symbol-macro %intptr_t% #+64-bit %i64%
                                #+32-bit %i32%)
(define-symbol-macro %uintptr_t% #+64-bit %i64%
                                 #+32-bit %i32%)
(define-symbol-macro %uintptr_t*% (llvm-sys:type-get-pointer-to %uintptr_t%))
(defun make-uintptr_t (x)
  (and (> x most-positive-fixnum) (error "make sure the integer ~s fits in a %i64%" x))
  (cond
    ((= 8 +uintptr_t-size+) (jit-constant-i64 x))
    ((= 4 +uintptr_t-size+) (jit-constant-i32 x))
    (t (error "Add support for size uintptr_t = ~a" +uintptr_t-size+))))

;;; DO NOT CHANGE THE FOLLOWING STRUCT!!! IT MUST MATCH vaslist

(defun build-list-of-pointers (size type)
  (multiple-value-bind (num-pointers remainder)
      (floor size +void*-size+)
    (unless (= remainder 0)
      (error "The ~a size ~a is not a multiple of sizeof(void*) ~a"
             type size +void*-size+))
    (make-list num-pointers :initial-element %i8*%)))

(define-symbol-macro %sp-counted-base% (llvm-sys:struct-type-get (thread-local-llvm-context) (list %i32% %i32%) nil)) ;; "sp-counted-base-ty"
(define-symbol-macro %sp-counted-base-ptr% (llvm-sys:type-get-pointer-to %sp-counted-base%))
(define-symbol-macro %shared-count% (llvm-sys:struct-type-get (thread-local-llvm-context) (list %sp-counted-base-ptr%) nil)) ;; "shared_count"

;;
;; Setup setjmp_buf type
;;
;; setjmp_buf buffers consist of five words.
;; Word 0 - Stores the frame-ptr (set by the library)
;; Word 1 - Stores the longjmp destination address - we set this
;; Word 2..4 - Three words for target specific data.
;; I'm going to use Word 2.. to represent different things.
;; For TAGBODY/GO Word2 will contain an i32 and the rest is padding
;; For BLOCK/RETURN-FROM Word2..4 will contain a T_mv pointer - it should fit

(define-symbol-macro %setjmp.buf% (llvm-sys:struct-type-get (thread-local-llvm-context) (list %i8*% %i8*% %i8*% %i8*% %i8*%) nil))
(define-symbol-macro %setjmp.buf*% (llvm-sys:type-get-pointer-to %setjmp.buf%))

;;
;; Setup smart-ptr constants
;;
(multiple-value-bind (pointer-type pointer-px-offset)
    (smart-pointer-details)
  (declare (ignore pointer-type))
  #+(or)(defvar +using-intrusive-reference-count+
          (eq pointer-type 'core::intrusive-reference-counted-pointer))
  (defvar +smart-ptr-px-offset+ pointer-px-offset))


(defun smart-pointer-fields (data-ptr-type &rest additional-fields)
  "List the types that make up a smart_ptr.
Boehm and MPS use a single pointer"
  (list* data-ptr-type additional-fields))

;; Define the T_O struct - right now just put in a dummy i32 - later put real fields here
(define-symbol-macro %t% %i8%) ; (llvm-sys:struct-type-get (thread-local-llvm-context) nil  nil)) ;; "T_O"
(define-symbol-macro %t*% (llvm-sys:type-get-pointer-to %t%))
;; alias for bignum dumping
(define-symbol-macro %bignum% %t*%)
(define-symbol-macro %t**% (llvm-sys:type-get-pointer-to %t*%))
(define-symbol-macro %t*[0]% (llvm-sys:array-type-get %t*% 0))
(define-symbol-macro %t*[0]*% (llvm-sys:type-get-pointer-to %t*[0]%))
(define-symbol-macro %t*[DUMMY]% (llvm-sys:array-type-get %t*% 64))
(define-symbol-macro %t*[DUMMY]*% (llvm-sys:type-get-pointer-to %t*[DUMMY]%))
(define-symbol-macro %tsp% (llvm-sys:struct-type-get (thread-local-llvm-context) (smart-pointer-fields %t*%) nil))  ;; "T_sp"
(define-symbol-macro %atomic<tsp>% %tsp%)
(define-symbol-macro %tsp*% (llvm-sys:type-get-pointer-to %tsp%))
(define-symbol-macro %tsp**% (llvm-sys:type-get-pointer-to %tsp*%))
(define-symbol-macro %tsp[0]% (llvm-sys:array-type-get %tsp% 0))

(define-symbol-macro %metadata% (llvm-sys:type-get-metadata-ty (thread-local-llvm-context)))

(define-symbol-macro %fn-xep-anonymous-function% (llvm-sys:function-type-get %t*% nil))

(define-symbol-macro %entry-point-vector% (llvm-sys:array-type-get %i8*% (+ 1 (- +entry-point-arity-end+ +entry-point-arity-begin+))))


(define-c++-struct %global-entry-point% +general-tag+
                 ((%i8*% :vtable)
                  (%t*%  :function-description)
                  (%t*%  :code)
                  (%entry-point-vector% :entry-points)
                  (%i8%  :defined)
                  ))
(define-symbol-macro %global-entry-point*% (llvm-sys:type-get-pointer-to %global-entry-point%))
(define-symbol-macro %entry-point-vector*% (llvm-sys:type-get-pointer-to %entry-point-vector%))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (verify-global-entry-point (c++-struct-field-offsets info.%global-entry-point%)))


;;; MUST match WrappedPointer_O layout
(define-c++-struct %wrapped-pointer% +general-tag+
  (
   (%i8*%     :vtable)
   (%i64%     :stamp)
   (%t*%      :class)
   ))

(defconstant +wrapped-pointer.stamp-index+ (c++-field-index :stamp info.%wrapped-pointer%))
(define-symbol-macro %wrapped-pointer*% (llvm-sys:type-get-pointer-to %wrapped-pointer%))

(define-c++-struct %instance% +general-tag+
  ((%i8*%     :vtable)
   (%t*%      :Class)
   (%t*%      :Rack)
   ))


(defconstant +instance.rack-index+ (c++-field-index :rack info.%instance%))
(define-symbol-macro %instance*% (llvm-sys:type-get-pointer-to %instance%))

;;; Must match SimpleVector_O aka GCArray_moveable<T_sp>
(define-c++-struct %simple-vector% +general-tag+
  ((%i8*%     :vtable)
   (%size_t%      :length)
   (%tsp[0]%      :data)
   ))
(defconstant +simple-vector.length-index+ (c++-field-index :length info.%simple-vector%))
(defconstant +simple-vector.data-index+ (c++-field-index :data info.%simple-vector%))


(define-c++-struct %rack% +general-tag+
  ((%i8*%     :vtable)
   (%tsp%     :stamp)
   (%tsp%     :sig)
   (%size_t%  :length)
   (%t*[0]%   :data)
   ))

(define-symbol-macro %rack*% (llvm-sys:type-get-pointer-to %rack%))

(defconstant +rack.stamp-index+ (c++-field-index :stamp info.%rack%))
(defconstant +rack.length-index+ (c++-field-index :length info.%rack%))
(defconstant +rack.data-index+ (c++-field-index :data info.%rack%))

(define-c++-struct %mdarray% +general-tag+
  ((%i8*% :vtable)
   (%size_t% :Fill-Pointer-Or-Length-Or-Dummy)
   (%size_t% :Array-Total-Size)
   (%t*%     :Data)
   (%size_t% :Displaced-Index-Offset)
   (%size_t% :Flags)
   (%size_t% :rank)
   (%size_t[0]% :dimensions)))

(define-symbol-macro %mdarray*% (llvm-sys:type-get-pointer-to %mdarray%))

(define-c++-struct %value-frame% +general-tag+
  ((%i8*%     :vtable)
   (%tsp%     :parent)
   (%size_t%  :length)
   (%tsp[0]%  :data))
  )
(define-symbol-macro %value-frame*% (llvm-sys:type-get-pointer-to %value-frame%))
(defconstant +value-frame.parent-index+ (c++-field-index :parent info.%value-frame%))
(defconstant +value-frame.length-index+ (c++-field-index :length info.%value-frame%))
(defconstant +value-frame.data-index+ (c++-field-index :data info.%value-frame%))


;;; MUST match FuncallableInstance_O layout
(define-c++-struct %funcallable-instance% +general-tag+
  ((%i8*% :vtable)
   (%i8*% :function-description)
   (%t*% :rack)
   (%t*% :class)
   (%atomic<size_t>% :interpreted-calls)
   (%atomic<tsp>% :compiled-dispatch-function)))

(define-symbol-macro %funcallable-instance*% (llvm-sys:type-get-pointer-to %funcallable-instance%))
(defconstant +funcallable-instance.rack-index+ (c++-field-index :rack info.%funcallable-instance%))
(define-symbol-macro %funcallable-instance*% (llvm-sys:type-get-pointer-to %funcallable-instance%))

;;;
;;; The %symbol% type MUST match the layout and size of Symbol_O in symbol.h
;;;
(define-c++-struct %symbol% +general-tag+
  ((%i8*% :sym-vtable) ; index=0 offset=0
   (%t*% :name) ; index=1 offset=8
   (%t*% :home-package) ; index=2 offset=16
   (%t*% :global-value) ; index=3 offset=24
   (%t*% :function) ; index=4 offset=32
   (%t*% :setf-function) ; index=5 offset=40
   (%i32% :binding-idx) ; index=6 offset=48
   (%i32% :flags) ; index=7 offset=56
   (%t*% :property-list))) ; index=8 offset=64

(defconstant +symbol.function-index+ (c++-field-index :function info.%symbol%))
(defconstant +symbol.setf-function-index+ (c++-field-index :setf-function info.%symbol%))

(define-symbol-macro %symbol*% (llvm-sys:type-get-pointer-to %symbol%))
(define-symbol-macro %symsp% (llvm-sys:struct-type-get (thread-local-llvm-context) (smart-pointer-fields %symbol*%) nil)) ;; "Sym_sp"
(define-symbol-macro %symsp*% (llvm-sys:type-get-pointer-to %symsp%))

(define-c++-struct %cons% +cons-tag+
  ( (%t*% :car)
    (%t*% :cdr)))

(define-symbol-macro %cons*% (llvm-sys:type-get-pointer-to %cons%))

(defconstant +cons.car-index+ (c++-field-index :car info.%cons%))
(defconstant +cons.cdr-index+ (c++-field-index :cdr info.%cons%))
(let* ((cons-size (llvm-sys:data-layout-get-type-alloc-size (system-data-layout) %cons%))
       (cons-layout (llvm-sys:data-layout-get-struct-layout (system-data-layout) %cons%))
       (cons-car-offset (llvm-sys:struct-layout-get-element-offset cons-layout +cons.car-index+))
       (cons-cdr-offset (llvm-sys:struct-layout-get-element-offset cons-layout +cons.cdr-index+)))
  (core:verify-cons-layout cons-size cons-car-offset cons-cdr-offset))


;; This structure must match the gctools::GCRootsInModule structure
(define-c++-struct %gcroots-in-module% +general-tag+
  ((%size_t%  :index-offset)
   (%i8*%     :module-memory)
   (%size_t%  :num-entries)
   (%size_t%  :capacity)
   (%i8**%    :function-pointers)
   (%size_t%  :number-of-functions)))

(defun gcroots-in-module-initial-value (&optional literals size)
  (declare (ignore literals))
  (llvm-sys:constant-struct-get %gcroots-in-module%
                                (list
                                 (jit-constant-size_t 0)
                                 #+(or)(if literals
                                           (irc-bit-cast literals %i8*%)
                                           (llvm-sys:constant-pointer-null-get %i8*%))
                                 (llvm-sys:constant-pointer-null-get %i8*%)
                                 (if size
                                     (jit-constant-size_t size)
                                     (jit-constant-size_t 0))
                                 (jit-constant-size_t 0)
                                 (llvm-sys:constant-pointer-null-get %i8**%)
                                 (jit-constant-size_t 0)
                                 )))

(define-symbol-macro %gcroots-in-module*% (llvm-sys:type-get-pointer-to %gcroots-in-module%))

;; The definition of %tmv% doesn't quite match T_mv because T_mv inherits from T_sp
(define-symbol-macro %tmv% (llvm-sys:struct-type-get (thread-local-llvm-context) (smart-pointer-fields %t*% %size_t%) nil))  ;; "T_mv"
(define-symbol-macro %return-type% %tmv%)
(define-symbol-macro %tmv*% (llvm-sys:type-get-pointer-to %tmv%))
(define-symbol-macro %tmv**% (llvm-sys:type-get-pointer-to %tmv*%))

(define-symbol-macro %gcvector-tsp% (llvm-sys:struct-type-get (thread-local-llvm-context) (list %size_t% %size_t% %tsp%) nil))
(define-symbol-macro %gcvector-symsp% (llvm-sys:struct-type-get (thread-local-llvm-context) (list %size_t% %size_t% %symsp%) nil))
(define-symbol-macro %vec0-tsp% (llvm-sys:struct-type-get (thread-local-llvm-context) (list %gcvector-tsp%) nil))
(define-symbol-macro %vec0-symsp% (llvm-sys:struct-type-get (thread-local-llvm-context) (list %gcvector-symsp%) nil))

;; Define the LoadTimeValue_O struct - right now just put in a dummy i32 - later put real fields here
(define-symbol-macro %ltv% (llvm-sys:struct-type-get (thread-local-llvm-context) (list %vtable*% #+(or) %vec0-tsp%)  nil)) ;; "LoadTimeValue_O"
(define-symbol-macro %ltv*% (llvm-sys:type-get-pointer-to %ltv%))
(define-symbol-macro %ltv**% (llvm-sys:type-get-pointer-to %ltv*%))
(define-symbol-macro %ltvsp% (llvm-sys:struct-type-get (thread-local-llvm-context) (smart-pointer-fields %ltv*%) nil))  ;; "LoadTimeValue_sp"
#+(or)(defvar +ltvsp*+ (llvm-sys:type-get-pointer-to %ltvsp%))
#+(or)(defvar +ltvsp**+ (llvm-sys:type-get-pointer-to +ltvsp*+))


(define-symbol-macro %mv-limit% +multiple-values-limit+)
(define-symbol-macro %mv-values-array% (llvm-sys:array-type-get %t*% %mv-limit%))
(define-symbol-macro %mv-struct% (llvm-sys:struct-type-get (thread-local-llvm-context) (list %size_t% %mv-values-array%) nil #|| is-packed ||#))
(define-symbol-macro %mv-struct*% (llvm-sys:type-get-pointer-to %mv-struct%))
(define-symbol-macro %thread-info-struct% (llvm-sys:struct-type-get (thread-local-llvm-context) (list %mv-struct%) nil))



#+(or)(progn
        (defvar +af+ (llvm-sys:struct-type-get (thread-local-llvm-context) nil  nil)) ;; "ActivationFrame_O"
        (defvar +af*+ (llvm-sys:type-get-pointer-to +af+))
        (define-symbol-macro %afsp% (llvm-sys:struct-type-get (thread-local-llvm-context) (smart-pointer-fields +af*+)  nil)) ;; "ActivationFrame_sp"
        (define-symbol-macro %afsp*% (llvm-sys:type-get-pointer-to %afsp%))
        )

;; Substitute afsp* with tsp
(define-symbol-macro %af% %t%)
(define-symbol-macro %af*% %t*%)
(define-symbol-macro %afsp% %tsp%)
(define-symbol-macro %afsp*% %tsp*%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Vector access and unboxed value stuff

(defun element-type->llvm-type (element-type)
  (case element-type
    ((t) %t*%)
    (ext:byte8 %i8%)
    (ext:integer8 %i8%)
    (ext:byte16 %i16%)
    (ext:integer16 %i16%)
    (ext:byte32 %i32%)
    (ext:integer32 %i32%)
    (ext:byte64 %i64%)
    (ext:integer64 %i64%)
    (fixnum %i64%) ; FIXME: should we store fixnums in arrays tagged? we do now.
    (single-float %float%)
    (double-float %double%)
    ;; should be less hardcoded
    (base-char %i8%) ; in core as C unsigned char
    (character %i32%) ; in core as C int
    (otherwise
     (error "BUG: Unknown element type ~a" element-type))))

(defun simple-vector-llvm-type (element-type)
  (llvm-sys:struct-type-get
   (thread-local-llvm-context)
   (list
    ;; Spacer to get to the stuff that matters
    (llvm-sys:array-type-get %i8% (- +simple-vector._length-offset+ +general-tag+))
    ;; The length, an untagged integer
    %size_t%
    ;; The data, a flexible member
    (llvm-sys:array-type-get (element-type->llvm-type element-type) 0))
   ;; Not totally sure it should be packed.
   t))

(defvar +simple-vector-length-slot+ 1)
(defvar +simple-vector-data-slot+ 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stuff needed for generic functions
;;;
;;;
(progn
  ;; Tack on a size_t to store the number of remaining arguments

  (define-c++-struct %vaslist% +vaslist0-tag+  ;; TODO - there is going to be a problem here becaues of +vaslist1-tag+
    ((%t**% :args)
     (%size_t% :nargs)))
  (define-symbol-macro %vaslist*% (llvm-sys:type-get-pointer-to %vaslist%))

;;;    "Function prototype for generic functions")
  (define-symbol-macro %fn-gf% (llvm-sys:function-type-get %tmv% (list %t*% %t*%)))
  (define-symbol-macro %fn-gf-arguments% (list "gf" "args"))
  )

(defun vaslist*-args* (vaslist* &optional (label "args*"))
  (irc-struct-gep %vaslist% vaslist* 0 label))
(defun vaslist*-nargs* (vaslist* &optional (label "nargs*"))
  (irc-struct-gep %vaslist% vaslist* 1 label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide the arguments passed to the function in a convenient manner.
;; Either the register arguments are available in register-args
;;   or the vaslist is used to access the arguments
;;   one after the other with calling-convention.va-arg
(defstruct (calling-convention (:type vector) :named)
  closure
  nargs
  register-args ; The arguments that were passed in registers
  vaslist*      ; The address of the vaslist, or NIL
  cleavir-lambda-list-analysis ; analysis of cleavir-lambda-list
  rest-alloc ; whether we can dx or ignore a &rest argument
  )

;; Parse the function arguments into a calling-convention

(defun initialize-calling-convention (llvm-function arity &key debug-on cleavir-lambda-list-analysis rest-alloc)
  (cmp-log "llvm-function: %s%N" llvm-function)
  (let ((arguments (llvm-sys:get-argument-list llvm-function)))
    (cmp-log "llvm-function arguments: %s%N" (llvm-sys:get-argument-list llvm-function))
    (cmp-log "llvm-function isVarArg: %s%N" (llvm-sys:is-var-arg llvm-function))
    (let ((register-save-area* (when debug-on (alloca-register-save-area arity :label "register-save-area")))
          (closure (first arguments)))
      (cmp-log "A%N")
      (unless (first arguments)
        (error "initialize-calling-convention for arguments ~a - the closure is NIL" arguments))
      (cmp-log "A%N")
      (cond
        ((eq arity :general-entry)
         (cmp-log "B%N")
         (let* ((nargs (second arguments))
                (args (third arguments))
                (vaslist* (alloca-vaslist)))
           (vaslist-start vaslist* nargs args)
           (maybe-spill-to-register-save-area arity register-save-area* (list closure nargs args))
           (make-calling-convention :closure closure
                                    :nargs nargs
                                    :vaslist* vaslist*
                                    :cleavir-lambda-list-analysis cleavir-lambda-list-analysis
                                    :rest-alloc rest-alloc)))
        (t
         (let ((nargs (length (cdr arguments)))
               (register-args (cdr arguments)))
           (maybe-spill-to-register-save-area arity register-save-area* (list* closure register-args))
           (make-calling-convention :closure closure
                                    :nargs (jit-constant-i64 nargs)
                                    :register-args register-args
                                    :cleavir-lambda-list-analysis cleavir-lambda-list-analysis
                                    :rest-alloc rest-alloc)))))))

(defun vaslist-start (vaslist* nargs &optional args)
  (when args
    (irc-store args (vaslist*-args* vaslist*)))
  (irc-store nargs (vaslist*-nargs* vaslist*)))

;;;
;;; Read the next argument from the vaslist
(defun calling-convention-vaslist.va-arg (cc)
  (let* ((vaslist* (calling-convention-vaslist* cc))
         (args* (vaslist*-args* vaslist*))
         (args (irc-load args*))
         (val (irc-load args))
         (args-next (irc-gep args (list (jit-constant-i64 1))))
         (nargs* (vaslist*-nargs* vaslist*))
         (nargs (irc-load nargs*))
         (nargs-next (irc-sub nargs (jit-constant-i64 1))))
    (irc-store args-next args*)
    (irc-store nargs-next nargs*)
    val))

(defun fn-prototype (arity)
  (cond
    ((eq arity :general-entry)
     (llvm-sys:function-type-get %tmv% (list %t*% %size_t% %t**%)))
    ((fixnump arity)
     (llvm-sys:function-type-get %tmv% (make-list (+ 1 arity) :initial-element %t*%) nil))
    (t (error "fn-prototype Illegal arity ~a" arity))))

(defun fn-prototype-names (arity)
  (cond
    ((eq arity :general-entry)
     (list "closure" "nargs" "args"))
    ((and (fixnump arity) (< arity 9))
     (subseq (list "closure" "arg0" "arg1" "arg2" "arg3" "arg4" "arg5" "arg6" "arg7" "arg8")
             0 (+ 1 arity)))
    ((fixnump arity)
     (error "Arity is too high -add support for this ~a" arity))
    (t (error "fn-prototype-names Illegal arity ~a" arity))))

(defun dbg-parameter-var (name argno &optional (type-name "T_O*")
                                       (type llvm-sys:+dw-ate-address+))
  (dbg-create-parameter-variable :name name :argno argno
                                 :lineno *dbg-current-function-lineno*
                                 :type (llvm-sys:create-basic-type
                                        *the-module-dibuilder*
                                        type-name 64 type 0)
                                 :always-preserve t))


;; (Maybe) generate code to store registers in memory. Return value unspecified.  
(defun maybe-spill-to-register-save-area (arity register-save-area* registers)
  (cmp-log "maybe-spill-to-register-save-area register-save-area* -> %s%N" register-save-area*)
  (cmp-log "maybe-spill-to-register-save-area registers -> %s%N" registers)
  (when register-save-area*
    (flet ((spill-reg (idx reg addr-name)
             (let ((addr          (irc-gep register-save-area* (list 0 idx) addr-name))
                   (reg-i8*       (cond
                                    ((llvm-sys:type-equal (llvm-sys:get-type reg) %i64%)
                                     (irc-int-to-ptr reg %i8*% "nargs-i8*"))
                                    (t
                                     (irc-bit-cast reg %i8*% "reg-i8*")))))
               (irc-store reg-i8* addr t)
               (when (llvm-sys:current-debug-location *irbuilder*)
                 (let ((var (dbg-parameter-var addr-name (1+ idx))))
                   (%dbg-variable-value reg-i8* var)
                   (%dbg-variable-addr addr var)))
               addr)))
      (let* ((names (if (eq arity :general-entry)
                        (list "rsa-closure" "rsa-nargs" "rsa-args")
                        (list "rsa-closure" "rsa-arg0" "rsa-arg1" "rsa-arg2" "rsa-arg3" "rsa-arg4" "rsa-arg5" "rsa-arg6" "rsa-arg7")))
             (idx 0))
        (mapc (lambda (reg name)
                (spill-reg idx reg name)
                (incf idx))
              registers names)))))


(defun %dbg-variable-addr (addr var)
  (let* ((addrmd (llvm-sys:metadata-as-value-get
                  (thread-local-llvm-context)
                  (llvm-sys:value-as-metadata-get addr)))
         (varmd (llvm-sys:metadata-as-value-get
                 (thread-local-llvm-context)
                 var))
         (diexpr (llvm-sys:metadata-as-value-get
                  (thread-local-llvm-context)
                  (llvm-sys:create-expression-none *the-module-dibuilder*))))
    (irc-intrinsic "llvm.dbg.addr" addrmd varmd diexpr)))

;;; Put in debug information for a variable corresponding to an alloca.
(defun dbg-variable-alloca (alloca name spi
                                   &optional (type-name "T_O*")
                                     (type llvm-sys:+dw-ate-address+))
  (when spi ; don't bother if there's no info.
    (let* ((type (llvm-sys:create-basic-type
                  *the-module-dibuilder* type-name 64 type 0))
           (inlined-at (core:source-pos-info-inlined-at spi))
           (scope (if inlined-at
                      (cached-function-scope
                       (core:source-pos-info-function-scope spi))
                      *dbg-current-scope*))
           (auto-variable (dbg-create-auto-variable
                           :name name
                           :lineno (core:source-pos-info-lineno spi)
                           :scope scope
                           :type type)))
      (%dbg-variable-addr alloca auto-variable))))

(defun %dbg-variable-value (value var)
    (let* ((valuemd (llvm-sys:metadata-as-value-get
                     (thread-local-llvm-context)
                     (llvm-sys:value-as-metadata-get value)))
           (varmd (llvm-sys:metadata-as-value-get
                   (thread-local-llvm-context)
                   var))
           (diexpr (llvm-sys:metadata-as-value-get
                    (thread-local-llvm-context)
                    (llvm-sys:create-expression-none *the-module-dibuilder*))))
      (irc-intrinsic "llvm.dbg.value" valuemd varmd diexpr)))

;;; Put in debug information for a variable corresponding to an llvm Value.
(defun dbg-variable-value (value name spi
                                 &optional (type-name "T_O*")
                                   (type llvm-sys:+dw-ate-address+))
  (when spi
    (let* ((type (llvm-sys:create-basic-type
                  *the-module-dibuilder* type-name 64 type 0))
           (inlined-at (core:source-pos-info-inlined-at spi))
           (scope (if inlined-at
                      (cached-function-scope
                       (core:source-pos-info-function-scope spi))
                      *dbg-current-scope*))
           (auto-variable (dbg-create-auto-variable
                           :name name
                           :lineno (core:source-pos-info-lineno spi)
                           :scope scope
                           :type type)))
      (%dbg-variable-value value auto-variable))))

;;; This is the normal C-style prototype for a function
(define-symbol-macro %opaque-fn-prototype*% %i8*%)

#+(or)
(progn
;;;  "A pointer to the function prototype"
  (define-symbol-macro %fn-prototype*% (llvm-sys:type-get-pointer-to %fn-prototype%))
;;;  "A pointer to a pointer to the function prototype"
  (define-symbol-macro %fn-prototype**% (llvm-sys:type-get-pointer-to %fn-prototype*%))
;;;  "An array of pointers to the function prototype"
  (define-symbol-macro %fn-prototype*[0]% (llvm-sys:array-type-get %fn-prototype*% 0))
;;;  "An array of pointers to the function prototype"
  (define-symbol-macro %fn-prototype*[1]% (llvm-sys:array-type-get %fn-prototype*% 1))
;;;  "An array of pointers to the function prototype"
  (define-symbol-macro %fn-prototype*[2]% (llvm-sys:array-type-get %fn-prototype*% 2))
  )

;;
;; The %function% type MUST match the layout and size of Function_O in functor.h
;;
(define-c++-struct %Function% +general-tag+
  ((%i8*%      :vtable)
   (%global-entry-point*% :global-entry-point)))

(define-symbol-macro %Function*% (llvm-sys:type-get-pointer-to %Function%))
(define-symbol-macro %Function_sp% (llvm-sys:struct-type-get (thread-local-llvm-context) (smart-pointer-fields %Function*%) nil)) ;; "Cfn_sp"
(define-symbol-macro %Function_sp*% (llvm-sys:type-get-pointer-to %Function_sp%))

;;; ------------------------------------------------------------
;;;
;;; This must match FunctionDescription in functor.h
;;;
;;; source-info/function-name are stored in a CONS cell CAR/CDR
;;; lambda-list/docstring are stored in a CONS cell CAR/CDR
(define-symbol-macro %entry-points-vector% (llvm-sys:array-type-get %i8*% core:*number-of-entry-points*))
(define-symbol-macro %function-description%
    (llvm-sys:struct-type-get (thread-local-llvm-context)
                              (list %i8*%                  ;  1 vtable
                                    %t*%                   ;  2 function-name
                                    %t*%                   ;  3 source-info
                                    %t*%                   ;  4 lambda-list
                                    %t*%                   ;  5 docstring
                                    %t*%                   ;  6 declares
                                    %i32%                  ;  7 lineno
                                    %i32%                  ;  8 column
                                    %i32%                  ;  9 filepos
                                    ) nil ))
(define-symbol-macro %function-description*% (llvm-sys:type-get-pointer-to %function-description%))

(define-c++-struct %closure-with-slots% +general-tag+
  ((%i8*% vtable)
   (%t*% entry-point)
   (%i32% closure-type)
   (%size_t% data-length)
   (%tsp[0]% data0))
  )
(eval-when (:compile-toplevel :load-toplevel :execute)
  (verify-closure-with-slots (c++-struct-field-offsets info.%closure-with-slots%)))

(defun %closure-with-slots%.offset-of[n]/t* (index)
  "This assumes that the t* offset coincides with the tsp start"
  (let* ((offset-of-data (cdr (assoc 'data0 (c++-struct-field-offsets info.%closure-with-slots%))))
         (sizeof-element (llvm-sys:data-layout-get-type-alloc-size (system-data-layout) %tsp%)))
    (+ (* sizeof-element index) offset-of-data)))

#|
  (defun make-gv-file-scope-handle (module &optional handle)
    (if (null handle) (setq handle -1))
    (llvm-sys:make-global-variable module
                                   %i32%  ; type
                                   nil    ; constant
                                   'llvm-sys:internal-linkage
                                   (jit-constant-i32 handle)
                                   "file-scope-handle"))
|#

(defun add-llvm.used (module used-function)
  (or used-function (error "used-function must not be NIL"))
  (llvm-sys:make-global-variable
   module
   %i8*[1]%
   nil
   'llvm-sys:appending-linkage
   (llvm-sys:constant-array-get
    %i8*[1]%
    (list
     (irc-bit-cast used-function %i8*%)))
   "llvm.used"))


(defun add-global-ctor-function (module main-function &key position register-library)
  (declare (ignore register-library)) 
  "Create a function with the name core:+clasp-ctor-function-name+ and
have it call the main-function"
  #+(or)(unless (eql module (llvm-sys:get-parent main-function))
          (error "The parent of the func-ptr ~a (a module) does not match the module ~a" (llvm-sys:get-parent main-function) module))
;;;  (core::bformat t "add-global-ctor-function position: %s%N" position)
  (multiple-value-bind (startup-function-name startup-function-linkage)
      (core:startup-linkage-shutdown-names position)
    (let* ((*the-module* module)
           (ctor-fn (irc-simple-function-create
                     startup-function-name
                     %fn-ctor%
                     startup-function-linkage
                     *the-module*
                     :argument-names +fn-ctor-argument-names+)))
      (let* ((irbuilder-body (llvm-sys:make-irbuilder (thread-local-llvm-context)))
             (*current-function* ctor-fn)
             (entry-bb (irc-basic-block-create "entry" ctor-fn)))
        (irc-set-insert-point-basic-block entry-bb irbuilder-body)
        (with-landing-pad nil
          (with-irbuilder (irbuilder-body)
            (let ((bc-main-function
                    (irc-bit-cast main-function %fn-start-up*% "fnptr-pointer")))
              (irc-intrinsic "cc_register_startup_function"
                             (jit-constant-size_t position) bc-main-function)
              (irc-ret-void))))
        ;;(llvm-sys:dump fn)
        #+(or)(let* ((function-name "_claspObjectFileStartUp") ; (core:bformat nil "ObjectFileStartUp-%s" (core:next-number)))
                     #+(or)(_ (core:bformat t "add-global-ctor-function name: %s%N" function-name))
                     (outer-fn (irc-simple-function-create
                                function-name
                                %fn-ctor%
                                'llvm-sys:internal-linkage
                                *the-module*
                                :argument-names +fn-ctor-argument-names+))
                     (irbuilder-body (llvm-sys:make-irbuilder (thread-local-llvm-context)))
                     (*current-function* outer-fn)
                     (entry-bb (irc-basic-block-create "entry" outer-fn)))
                (irc-set-insert-point-basic-block entry-bb irbuilder-body)
                (with-landing-pad nil
                  (with-irbuilder (irbuilder-body)
                    (let* ((bc-main-function (irc-bit-cast main-function %fn-start-up*% "fnptr-pointer"))
                           (_                (irc-create-call-wft %fn-ctor% ctor-fn nil))
                           (_                (irc-ret-void))))))
                (add-llvm.used *the-module* outer-fn))
        (add-llvm.used *the-module* ctor-fn) ;; Try this instead of the thing above
        )
      ctor-fn)))

(defun add-main-function (module run-all-function)
  "Create an external function with the name main have it call the run-all-function"
  (let* ((*the-module* module)
         (fn (irc-simple-function-create
              "MAIN"
              %fn-start-up%
              cmp:*default-linkage*
              *the-module*
              :argument-names +fn-start-up-argument-names+)))
    (let* ((irbuilder-body (llvm-sys:make-irbuilder (thread-local-llvm-context)))
           (*current-function* fn)
           (entry-bb (irc-basic-block-create "entry" fn)))
      (irc-set-insert-point-basic-block entry-bb irbuilder-body)
      (with-irbuilder (irbuilder-body)
        (let ((bc-bf
                (irc-bit-cast run-all-function %fn-start-up*% "run-all-pointer")))
          (irc-intrinsic "cc_invoke_sub_run_all_function" bc-bf)
          (irc-ret-null-t*))
        ;;(llvm-sys:dump fn)
        fn))))

  (defun find-global-ctor-function (module)
    (let ((ctor (llvm-sys:get-function module core:+clasp-ctor-function-name+)))
      (or ctor (error "Couldn't find the ctor-function: ~a" core:+clasp-ctor-function-name+))
      ctor))

  (defun remove-llvm.global_ctors-if-exists (module)
    (let ((global (llvm-sys:get-named-global module "llvm.global_ctors")))
      (if global
          (llvm-sys:erase-from-parent global))))

  (defun remove-llvm.used-if-exists (module)
    (let ((global (llvm-sys:get-named-global module "llvm.used")))
      (if global
          (llvm-sys:erase-from-parent global))))

(defun add-llvm.global_ctors (module priority global-ctor-function)
  (or global-ctor-function (error "global-ctor-function must not be NIL"))
  (llvm-sys:make-global-variable
   module
   %global-ctors-struct[1]%
   nil
   'llvm-sys:appending-linkage
   (llvm-sys:constant-array-get
    %global-ctors-struct[1]%
    (list
     (llvm-sys:constant-struct-get %global-ctors-struct%
                                   (list
                                    (jit-constant-i32 priority)
                                    global-ctor-function
                                    (llvm-sys:constant-pointer-null-get %i8*%)))))
   "llvm.global_ctors"))

(defun make-boot-function-global-variable (module func-designator &key position register-library)
  "* Arguments
- module :: An llvm module
- func-designator :: An llvm function designator
* Description
Add the global variable llvm.global_ctors to the Module (linkage appending)
and initialize it with an array consisting of one function pointer."
  (let ((startup-fn (cond
                      ;; repl functions use an integer ID and we generate the startup-name and
                      ;;  then lookup the function
                      ((fixnump func-designator)
                       (multiple-value-bind (dummy-id startup-name shutdown-name)
                           (jit-startup-shutdown-function-names func-designator)
                         (declare (ignore dummy-id shutdown-name))
                         (llvm-sys:get-function module startup-name)))
                      ((stringp func-designator)
                       (llvm-sys:get-function module func-designator))
                      ((typep func-designator 'llvm-sys:function)
                       func-designator)
                      (t (error "~a must be a function name or llvm-sys:function" func-designator)))))
    (unless startup-fn
      (error "Could not find ~a in module" func-designator))
    #+(or)(unless (eql module (llvm-sys:get-parent func-ptr))
            (error "The parent of the func-ptr ~a (a module) does not match the module ~a" (llvm-sys:get-parent func-ptr) module))
    (let* ((global-ctor (add-global-ctor-function module startup-fn
                                                  :position position
                                                  :register-library register-library)))
      (incf *compilation-module-index*)
      (multiple-value-bind (startup-name linkage)
          (core:startup-linkage-shutdown-names 0) ; we only want to know linkage
        (declare (ignore startup-name))
        (when (eq linkage 'llvm-sys:internal-linkage)
          ;; Internal linkage means we can't look up a symbol to get the startup so we need to depend on
          ;; static constructors to initialize things.
          (add-llvm.global_ctors module *compilation-module-index* global-ctor))))))

;;
;; Ensure that the LLVM model of
;;   tsp matches shared_ptr<xxx> and
;;   tmv matches multiple_values<xxx>
;;
                                                   
(progn
  (let* ((data-layout (system-data-layout))
         (tsp-size (llvm-sys:data-layout-get-type-alloc-size data-layout %tsp%))
         (tmv-size (llvm-sys:data-layout-get-type-alloc-size data-layout %tmv%))
         (symbol-size (llvm-sys:data-layout-get-type-alloc-size data-layout %symbol%))
         (symbol-layout (llvm-sys:data-layout-get-struct-layout data-layout %symbol%))
         (symbol-function-offset (llvm-sys:struct-layout-get-element-offset symbol-layout +symbol.function-index+))
         (symbol-setf-function-offset (llvm-sys:struct-layout-get-element-offset symbol-layout +symbol.setf-function-index+))
         (function-size (llvm-sys:data-layout-get-type-alloc-size data-layout %function%))
         (global-entry-point-layout (llvm-sys:data-layout-get-struct-layout data-layout %global-entry-point%))
         (function-description-offset (c++-field-offset :function-description info.%global-entry-point%))
         (vaslist-size (llvm-sys:data-layout-get-type-alloc-size data-layout %vaslist%))
         (gcroots-in-module-size (llvm-sys:data-layout-get-type-alloc-size data-layout %gcroots-in-module%))
         (global-entry-point-size (llvm-sys:data-layout-get-type-alloc-size data-layout %global-entry-point%))
         (function-description-size (llvm-sys:data-layout-get-type-alloc-size data-layout %function-description%)))
    (declare (ignore global-entry-point-layout global-entry-point-size))
    (llvm-sys:throw-if-mismatched-structure-sizes :tsp tsp-size
                                                  :tmv tmv-size
                                                  :symbol symbol-size
                                                  :symbol-function-offset symbol-function-offset
                                                  :symbol-setf-function-offset symbol-setf-function-offset
                                                  :function function-size
                                                  :function-description-offset (+ function-description-offset +general-tag+)
                                                  :gcroots-in-module gcroots-in-module-size
                                                  :vaslist vaslist-size
                                                  :function-description function-description-size)

    (let* ((instance-size (llvm-sys:data-layout-get-type-alloc-size data-layout %instance%))
           (instance-layout (llvm-sys:data-layout-get-struct-layout data-layout %instance%))
           (instance-rack-offset (llvm-sys:struct-layout-get-element-offset instance-layout +instance.rack-index+)))
      (core:verify-instance-layout instance-size instance-rack-offset))

    (unless (= +instance.rack-index+ +funcallable-instance.rack-index+)
      (error "The +instance.rack-index+ ~d MUST match +funcallable-instance.rack-index+ ~d"
             +instance.rack-index+ +funcallable-instance.rack-index+))
    (let* ((funcallable-instance-size (llvm-sys:data-layout-get-type-alloc-size data-layout %funcallable-instance%))
           (funcallable-instance-layout (llvm-sys:data-layout-get-struct-layout data-layout %funcallable-instance%))
           (funcallable-instance-rack-offset (llvm-sys:struct-layout-get-element-offset funcallable-instance-layout +funcallable-instance.rack-index+)))
      (core:verify-funcallable-instance-layout funcallable-instance-size funcallable-instance-rack-offset))
    (let* ((simple-vector-layout (llvm-sys:data-layout-get-struct-layout data-layout %simple-vector%))
           (simple-vector-length-offset (llvm-sys:struct-layout-get-element-offset simple-vector-layout +simple-vector.length-index+))
           (simple-vector-data-offset (llvm-sys:struct-layout-get-element-offset simple-vector-layout +simple-vector.data-index+)))
      (core:verify-simple-vector-layout simple-vector-length-offset simple-vector-data-offset))
    (let* ((rack-layout (llvm-sys:data-layout-get-struct-layout data-layout %rack%))
           (rack-stamp-offset (llvm-sys:struct-layout-get-element-offset rack-layout +rack.stamp-index+))
           (rack-data-offset (llvm-sys:struct-layout-get-element-offset rack-layout +rack.data-index+)))
      (core:verify-rack-layout rack-stamp-offset rack-data-offset))
    (core:verify-mdarray-layout (c++-struct-field-offsets info.%mdarray%))
    (let* ((value-frame-layout (llvm-sys:data-layout-get-struct-layout data-layout %value-frame%))
           (value-frame-parent-offset (llvm-sys:struct-layout-get-element-offset value-frame-layout +value-frame.parent-index+))
           (value-frame-length-offset (llvm-sys:struct-layout-get-element-offset value-frame-layout +value-frame.length-index+))
           (value-frame-data-offset (llvm-sys:struct-layout-get-element-offset value-frame-layout +value-frame.data-index+)))
      (core:verify-value-frame-layout value-frame-parent-offset value-frame-length-offset value-frame-data-offset))
    (let* ((wrapped-pointer-layout (llvm-sys:data-layout-get-struct-layout data-layout %wrapped-pointer%))
           (wrapped-pointer-stamp-offset (llvm-sys:struct-layout-get-element-offset wrapped-pointer-layout +wrapped-pointer.stamp-index+)))
      (core:verify-wrapped-pointer-layout wrapped-pointer-stamp-offset))
    ))

;;
;; Define exception types in the module
;;
;; TODO:  Holy crap! We are using hard-coded mangled clang names for the typeinfo records for each
;;        exception type!  Find some portable way to do this!!!!!!!!
;; Currently these names are scraped from the libCore library using "nm"

(defvar *exceptions*
  '(
    (typeid-core-catch-throw "_ZTIN4core10CatchThrowE")
    (typeid-core-dynamic-go  "_ZTIN4core9DynamicGoE")
    (typeid-core-return-from "_ZTIN4core10ReturnFromE")
    (typeid-core-unwind      "_ZTIN4core6UnwindE")
    ))

(define-symbol-macro %jmp-buf-tag%
    (llvm-sys:array-type-get %i8% +jmp-buf-size+))
(define-symbol-macro %jmp-buf-tag*%
    (llvm-sys:type-get-pointer-to %jmp-buf-tag%))

(defvar *exception-types-hash-table* (make-hash-table :test #'eq)
  "Map exception names to exception class extern 'C' names")

(mapcar #'(lambda (x &aux (name (car x)) (cname (cadr x)))
	    (core::hash-table-setf-gethash *exception-types-hash-table* name cname))
	*exceptions*)

(defun exception-typeid*-from-name (name)
  (let* ((cname (gethash name *exception-types-hash-table*))
	 (i8* (llvm-sys:get-or-create-external-global *the-module* cname %i8% 'llvm-sys:external-linkage)))
    i8*))

(defun assert-result-isa-llvm-value (result)
  (unless (llvm-sys:llvm-value-p result)
    (error "result must be an instance of llvm-sys:Value_O but instead it has the value ~s" result)))


(defun codegen-startup-shutdown (module module-id THE-REPL-XEP-GROUP &optional gcroots-in-module roots-array-or-nil (number-of-roots 0) ordered-literals array)
  (declare (ignore array))
  (multiple-value-bind (startup-shutdown-id startup-function-name shutdown-function-name)
      (jit-startup-shutdown-function-names module-id)
    (let ((startup-fn (irc-simple-function-create startup-function-name
                                                  %fn-start-up%
                                                  'llvm-sys:external-linkage ; this should be internal and invoked by a ctor but that doesn't seem to be happening yet
                                                  module
                                                  :argument-names (list "values" )))
          (ordered-raw-literals-list nil))
      (llvm-sys:set-unnamed-addr startup-fn 'llvm-sys:none)
      (let* ((irbuilder-alloca (llvm-sys:make-irbuilder (thread-local-llvm-context)))
             (irbuilder-body (llvm-sys:make-irbuilder (thread-local-llvm-context)))
             (*irbuilder-function-alloca* irbuilder-alloca)
             (*irbuilder-function-body* irbuilder-body)
             (*current-function* startup-fn)
             (entry-bb (irc-basic-block-create "entry" startup-fn))
             (arguments (llvm-sys:get-argument-list startup-fn))
             (arg-values (first arguments)))
        (cmp:irc-set-insert-point-basic-block entry-bb irbuilder-alloca)
        (with-irbuilder (irbuilder-alloca)
          (let ((start (if roots-array-or-nil
                           (irc-gep roots-array-or-nil
                                    (list (jit-constant-size_t 0)
                                          (jit-constant-size_t 0)))
                           (llvm-sys:constant-pointer-null-get %t**%))))
            (multiple-value-bind (function-vector-length function-vector)
                (literal:setup-literal-machine-function-vectors cmp:*the-module*)
              (when gcroots-in-module
                (irc-intrinsic-call "cc_initialize_gcroots_in_module" (list gcroots-in-module ; holder
                                                                            start ; root_address
                                                                            (jit-constant-size_t number-of-roots) ; num_roots
                                                                            arg-values ; initial_data
                                                                            (llvm-sys:constant-pointer-null-get %i8**%) ; transient_alloca
                                                                            (jit-constant-size_t 0) ; transient_entries
                                                                            (jit-constant-size_t function-vector-length) ; function_pointer_count
                                                                            (irc-bit-cast
                                                                             (cmp:irc-gep function-vector
                                                                                          (list (cmp:jit-constant-size_t 0)
                                                                                                (cmp:jit-constant-size_t 0)))
                                                                             %i8**%) ; fptrs
                                                                            ))))
            ;; If the constant/literal list is provided - then we may need to generate code for closurettes
            (when ordered-literals
              (setf ordered-raw-literals-list (mapcar (lambda (x)
                                                        (cond
                                                          ((literal-node-runtime-p x)
                                                           (literal-node-runtime-object x))
                                                          ((and (literal:literal-node-creator-p x)
                                                                (literal:literal-node-closure-p (literal:literal-node-creator-object x)))
                                                           (literal:generate-run-time-code-for-closurette x)
                                                           nil)
                                                          (t (error "Illegal object in ordered-literals-list it is: ~s" x))))
                                                      ordered-literals))) 
            (when gcroots-in-module
              (irc-intrinsic-call "cc_finish_gcroots_in_module" (list gcroots-in-module)))
            (let ((global-entry-point (literal:constants-table-value (cmp:entry-point-reference-index (xep-group-entry-point-reference THE-REPL-XEP-GROUP)))))
              (irc-ret (irc-bit-cast global-entry-point %t*%))))))
      (let ((shutdown-fn (irc-simple-function-create shutdown-function-name
                                                     %fn-shut-down%
                                                     'llvm-sys::internal-linkage
                                                     module
                                                     :argument-names nil)))
        (let* ((irbuilder-alloca (llvm-sys:make-irbuilder (thread-local-llvm-context)))
               (irbuilder-body (llvm-sys:make-irbuilder (thread-local-llvm-context)))
               (*irbuilder-function-alloca* irbuilder-alloca)
               (*irbuilder-function-body* irbuilder-body)
               (*current-function* shutdown-fn)
               (entry-bb (irc-basic-block-create "entry" shutdown-fn)))
          (irc-set-insert-point-basic-block entry-bb irbuilder-alloca)
          (with-irbuilder (irbuilder-alloca)
            (progn
              (if gcroots-in-module
                  (irc-intrinsic-call "cc_remove_gcroots_in_module" (list gcroots-in-module)))
              (irc-ret-void))))
        (make-boot-function-global-variable module startup-shutdown-id :position startup-shutdown-id)
        (cmp-log-dump-module *the-module*)
        (values startup-shutdown-id ordered-raw-literals-list)))))


;;; Define what ltvc_xxx functions return
(define-symbol-macro %ltvc-return% %void%)


;;------------------------------------------------------------
;;
;; Setup dynamic variables
;;
;;

(defvar *compile-file-pathname* nil "Store the pathname of the currently compiled file")
(defvar *compile-file-truename* nil "Store the truename of the currently compiled file")
(defvar *compile-file-unique-symbol-prefix* "" "Store a unique prefix for symbols that are external-linkage")
;;; These variables are used to let compile-file insert debug information that does not
;;; correspond to the actual file being compiled. This is useful for editors (SLIME) that
;;; may present Clasp with a temporary file containing a portion of some other file; we want
;;; the debug data in the compilation of this file to reflect the other file, not the temp.
(defvar *compile-file-source-debug-pathname*) ; Pathname for source info
(defvar *compile-file-file-scope*) ; File scope bound by compile-file etc for source file info
(defvar *compile-file-source-debug-offset*) ; Offset bound by compile-file etc for SFIs
(defvar *compile-file-source-debug-lineno*) ; ditto

(defvar *gv-boot-functions* nil
  "A global value that stores a pointer to the boot function for the Module.
It has appending linkage.")
(defvar *current-function* nil "The current function")
(defvar *current-function-name* nil "Store the current function name")
(defvar *gv-current-function-name* nil "Store the global value in the module of the current function name ")

(defun compile-file-quick-module-pathname (file-name-modifier &optional (cfo-pathname *compile-file-output-pathname*))
  (let* ((name-suffix (bformat nil "%05d-%s" (core:next-number) file-name-modifier))
         (cfo-directory0 (pathname-directory cfo-pathname))
         (cfo-directory (make-pathname :directory
                                       (cond
                                         ((and (consp cfo-directory0)
                                               (eq (car cfo-directory0) :absolute))
                                          (list* :relative (cdr cfo-directory0)))
                                         (t cfo-directory0))))
         (full-directory (if cfo-directory
                             (merge-pathnames cfo-directory (core:monitor-directory))
                             (core:monitor-directory)))
         (output-path (make-pathname
                       :name (concatenate
                              'string
                              (pathname-name cfo-pathname)
                              "-" name-suffix)
                       :type "ll"
                       :defaults full-directory)))
    (cmp-log "Dumping module to %s%N" output-path)
    (ensure-directories-exist output-path)
    output-path))

(defun compile-file-quick-module-dump (module file-name-modifier compile-file-debug-dump-module)
  "Dump the module as a .ll file"
  (if compile-file-debug-dump-module
      (let* ((output-path (compile-file-quick-module-pathname file-name-modifier)))
        (let* ((output-name (namestring output-path))
               (fout (open output-name :direction :output)))
          (unwind-protect
               (llvm-sys:dump-module module fout)
            (close fout))))))

(defun compile-quick-module-pathname (file-name-modifier)
  (let* ((name-suffix (bformat nil "module-%05d-%s" (core:next-number) file-name-modifier))
         (output-path (make-pathname
                       :name name-suffix
                       :type "ll"
                       :defaults (core:monitor-directory))))
    (ensure-directories-exist output-path)
    output-path))

(defun compile-quick-module-dump (module file-name-modifier &optional (compile-debug-dump-module *compile-debug-dump-module*))
  "Dump the module as a .ll file"
  (if compile-debug-dump-module
      (let* ((output-path (compile-quick-module-pathname file-name-modifier))
             (output-name (namestring output-path))
             fout)
        (unwind-protect
             (progn
               (setf fout (open output-name :direction :output))
               (llvm-sys:dump-module module fout))
          (when fout (close fout))))))

(defun quick-module-pathname (name-modifier)
  "If called under COMPILE-FILE the modules are dumped into the
same directory as the COMPILE-FILE output.  If called under COMPILE
they are dumped into /tmp"
  (if *compile-file-output-pathname*
      (compile-file-quick-module-pathname name-modifier)
      (compile-quick-module-pathname name-modifier)))

(defun quick-module-dump (module name-modifier)
  "If called under COMPILE-FILE the modules are dumped into the
same directory as the COMPILE-FILE output.  If called under COMPILE
they are dumped into /tmp"
  (cmp-log "About to dump module - %s%N" name-modifier)
  (if *compile-file-output-pathname*
      (compile-file-quick-module-dump module name-modifier *compile-file-debug-dump-module*)
      (compile-quick-module-dump module name-modifier *compile-debug-dump-module*)))

(defmacro log-module ((info) &rest body)
  "Wrap quick-module-dump around a block of code so that the module
is dumped to a file before the block and after the block."
  `(progn
     (llvm-sys:sanity-check-module *the-module* 2)
     (quick-module-dump *the-module* ,(bformat nil "%s-begin" info))
     (multiple-value-prog1 (progn ,@body)
       (llvm-sys:sanity-check-module *the-module* 2)
       (quick-module-dump *the-module* ,(bformat nil "%s-end" info)))))


(defun module-report (module)
  (let ((total 0))
    (dolist (func (llvm-sys:module-get-function-list module))
      (let ((num-instructions 0))
        (dolist (bb (llvm-sys:basic-blocks func))
          (dolist (instr (llvm-sys:instructions bb))
            (incf num-instructions)
            (multiple-value-bind (valid lineno column)
                (llvm-sys:get-debug-loc-info instr)
              (if valid
                  (format t "(Line: ~s col: ~s) ~s~%" lineno column instr)
                  (format t "INVALID-DebugLoc ~s~%" instr)))
            (incf total num-instructions)))
        (format t "~a Function ~a~%" num-instructions func)))
    (format t "~a total~%" total)))

