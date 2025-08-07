;;
;;;    File: cmpintrinsics.lisp
;;;

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

(defstruct c++-struct
  name ; The symbol-macro name of the type
  tag  ; The tag of the objects of this type
  type-getter ; A single argument lambda that when passed an (thread-local-llvm-context) returns the type
  field-type-getters ; An alist of name/type-getter-functions
  field-pointee-type-getters ; An alist of name/pointee-type-getter-functions
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
            (field-indices `(let ((,field-index 0))       ;
                              (mapcar (lambda (,gs-field) ;
                                        (prog1            ;
                                            (cons (second ,gs-field) ,field-index) ;
                                          (incf ,field-index))) ;
                                      ',fields)))
            (field-type-getters-list (mapcar (lambda (type-name) ;
                                               #+(or)(format t "type-name -> ~s cadr -> ~s  ,car -> ~s~%" type-name (cadr type-name) (car type-name)) ;
                                               `(cons ',(cadr type-name) (lambda () (llvm-sys:type-get-pointer-to ,(macroexpand (car type-name))))))
                                             fields))
            (field-pointee-type-getters-list (mapcar (lambda (type-name) ;
                                                       #+(or)(format t "type-name -> ~s cadr -> ~s  ,car -> ~s~%" type-name (cadr type-name) (car type-name)) ;
                                                       `(cons ',(cadr type-name) (lambda () ,(macroexpand (car type-name)))))
                                                     fields)))
        #+(or)
        (progn
          (format t "define-symbol-macro = ~s~%" define-symbol-macro)
          (format t "field-offsets -> ~s~%" field-offsets)
          (format t "field-indices -> ~s~%" field-indices)
          (format t "field-type-getters-list -> ~s~%" field-type-getters-list)
          (format t "field-pointee-type-getters-list -> ~s~%" field-pointee-type-getters-list)
          )
        (let ((final `(progn
                        ,define-symbol-macro
                        (defparameter ,(intern (format nil "INFO.~a" name))
                          (make-c++-struct :name ,name
                                           :tag ,tag
                                           :type-getter (lambda () (progn ,name))
                                           :field-type-getters (list ,@field-type-getters-list)
                                           :field-pointee-type-getters (list ,@field-pointee-type-getters-list)
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

(defun c++-field-pointee-type (struct-info field-name)
  (let ((field-pointee-type-getter (cdr (assoc field-name (c++-struct-field-pointee-type-getters struct-info)))))
    (funcall field-pointee-type-getter)))
                                    
(define-symbol-macro %i1% (llvm-sys:type-get-int1-ty (thread-local-llvm-context)))
(define-symbol-macro %i3% (llvm-sys:type-get-int-nty (thread-local-llvm-context) 3))

(define-symbol-macro %i8% (llvm-sys:type-get-int8-ty (thread-local-llvm-context))) ;; -> CHAR / BYTE
(define-symbol-macro %i8*% (llvm-sys:type-get-pointer-to %i8%))
(define-symbol-macro %i8**% (llvm-sys:type-get-pointer-to %i8*%))
(define-symbol-macro %i8***% (llvm-sys:type-get-pointer-to %i8**%))
(define-symbol-macro %i8*[1]% (llvm-sys:array-type-get %i8*% 1))
(define-symbol-macro %register-save-area% %t*[0]%)



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

#+short-float/binary16
(define-symbol-macro %long-float% (llvm-sys:type-get-half-ty (thread-local-llvm-context)))
(define-symbol-macro %float% (llvm-sys:type-get-float-ty (thread-local-llvm-context)))
(define-symbol-macro %double% (llvm-sys:type-get-double-ty (thread-local-llvm-context)))
#+long-float/binary80
(define-symbol-macro %long-float% (llvm-sys:type-get-x86-fp80-ty (thread-local-llvm-context)))
#+long-float/binary128
(define-symbol-macro %long-float% (llvm-sys:type-get-fp128-ty (thread-local-llvm-context)))

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

;;(define-symbol-macro %exception-struct% (llvm-sys:struct-type-get (thread-local-llvm-context) (list %i8*% %i32%) "exception-struct" nil)
(define-symbol-macro %exn% %i8*%)
(define-symbol-macro %ehselector% %i32%)
(define-symbol-macro %go-index% %size_t%)
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

(defun llvm-print (msg)
  (irc-intrinsic "debugMessage" (irc-bit-cast (module-make-global-string msg) %i8*%)))
(defun llvm-print-pointer (msg ptr)
  (llvm-print msg)
  (irc-intrinsic "debugPointer" (irc-bit-cast ptr %i8*%)))
(defun llvm-print-size_t (msg st)
  (llvm-print msg)
  (irc-intrinsic "debugPrint_size_t" (irc-bit-cast st %i64%)))

(defun c++-field-ptr (struct-info tagged-object field-name &optional (label ""))
  (let* ((tagged-object-i8* (irc-bit-cast tagged-object %i8*%))
         (field* (irc-typed-gep %i8% tagged-object-i8* (list (jit-constant-i64 (c++-field-offset field-name struct-info)))))
         (field-type-getter (cdr (assoc field-name (c++-struct-field-type-getters struct-info))))
         (field-ptr (irc-bit-cast field* (funcall field-type-getter) label)))
    field-ptr))

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
(define-symbol-macro %t***% (llvm-sys:type-get-pointer-to %t**%))
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
   (%t*%  :entry-point)
   (%t*%  :function-description)
   (%t*%  :code)
   (%entry-point-vector% :entry-points)
   (%i8%  :defined)
   ))
(define-symbol-macro %global-entry-point*% (llvm-sys:type-get-pointer-to %global-entry-point%))
(define-symbol-macro %entry-point-vector*% (llvm-sys:type-get-pointer-to %entry-point-vector%))


(core:verify-global-entry-point (c++-struct-field-offsets info.%global-entry-point%))


;;; MUST match WrappedPointer_O layout
(define-c++-struct %wrapped-pointer% +general-tag+
  (
   (%i8*%     :vtable)
   (%i64%     :stamp)
   (%t*%      :class)
   ))

(defparameter +wrapped-pointer.stamp-index+ (c++-field-index :stamp info.%wrapped-pointer%))
(define-symbol-macro %wrapped-pointer*% (llvm-sys:type-get-pointer-to %wrapped-pointer%))

(define-c++-struct %instance% +general-tag+
  ((%i8*%     :vtable)
   (%t*%      :Class)
   (%t*%      :Rack)
   ))


(defparameter +instance.rack-index+ (c++-field-index :rack info.%instance%))
(define-symbol-macro %instance*% (llvm-sys:type-get-pointer-to %instance%))

;;; Must match SimpleVector_O aka GCArray_moveable<T_sp>
(define-c++-struct %simple-vector% +general-tag+
  ((%i8*%     :vtable)
   (%size_t%      :length)
   (%tsp[0]%      :data)
   ))
(defparameter +simple-vector.length-index+ (c++-field-index :length info.%simple-vector%))
(defparameter +simple-vector.data-index+ (c++-field-index :data info.%simple-vector%))


(define-c++-struct %rack% +general-tag+
  ((%i8*%     :vtable)
   (%tsp%     :stamp)
   (%tsp%     :sig)
   (%size_t%  :length)
   (%t*[0]%   :data)
   ))

(define-symbol-macro %rack*% (llvm-sys:type-get-pointer-to %rack%))

(defparameter +rack.stamp-index+ (c++-field-index :stamp info.%rack%))
(defparameter +rack.length-index+ (c++-field-index :length info.%rack%))
(defparameter +rack.data-index+ (c++-field-index :data info.%rack%))

(define-c++-struct %mdarray% +general-tag+
  ((%i8*% :vtable)
   (%size_t% :Fill-Pointer-Or-Length-Or-Dummy)
   (%size_t% :Array-Total-Size)
   (%t*%     :Data)
   (%size_t% :Displaced-Index-Offset)
   (%size_t% :Flags)
   (%size_t% :rank)
   (%size_t[0]% :dimensions)))
(define-symbol-macro %mdarray-dimensions-type% %size_t%)
(define-symbol-macro %mdarray*% (llvm-sys:type-get-pointer-to %mdarray%))

(define-c++-struct %value-frame% +general-tag+
  ((%i8*%     :vtable)
   (%tsp%     :parent)
   (%size_t%  :length)
   (%tsp[0]%  :data))
  )
(define-symbol-macro %value-frame*% (llvm-sys:type-get-pointer-to %value-frame%))
(defparameter +value-frame.parent-index+ (c++-field-index :parent info.%value-frame%))
(defparameter +value-frame.length-index+ (c++-field-index :length info.%value-frame%))
(defparameter +value-frame.data-index+ (c++-field-index :data info.%value-frame%))


;;; MUST match FuncallableInstance_O layout
(define-c++-struct %funcallable-instance% +general-tag+
  ((%i8*% :vtable)
   (%t*% :entry-point)
   (%t*% :rack)
   (%t*% :class)
   (%atomic<size_t>% :interpreted-calls)
   (%atomic<tsp>% :compiled-dispatch-function)))

(define-symbol-macro %funcallable-instance*% (llvm-sys:type-get-pointer-to %funcallable-instance%))
(defparameter +funcallable-instance.rack-index+ (c++-field-index :rack info.%funcallable-instance%))
(define-symbol-macro %funcallable-instance*% (llvm-sys:type-get-pointer-to %funcallable-instance%))

;;; Ditto for FunctionCell_O
(define-c++-struct %function-cell% +general-tag+
  ((%i8*% :vtable)
   (%t*% :entry-point)
   (%atomic<tsp>% :real-function)))

;;;
;;; The %symbol% type MUST match the layout and size of Symbol_O in symbol.h
;;;
(define-c++-struct %symbol% +general-tag+
  ((%i8*% :sym-vtable) ; index=0 offset=0
   (%t*% :name) ; index=1 offset=8
   (%t*% :home-package) ; index=2 offset=16
   (%t*% :value) ; index=3 offset=24
   (%t*% :function) ; index=4 offset=32
   (%t*% :setf-function) ; index=5 offset=40
   (%i32% :flags) ; index=6 offset=48
   (%t*% :property-list))) ; index=7 offset=56

(defparameter +symbol.function-index+ (c++-field-index :function info.%symbol%))
(defparameter +symbol.setf-function-index+ (c++-field-index :setf-function info.%symbol%))

(define-symbol-macro %symbol*% (llvm-sys:type-get-pointer-to %symbol%))
(define-symbol-macro %symsp% (llvm-sys:struct-type-get (thread-local-llvm-context) (smart-pointer-fields %symbol*%) nil)) ;; "Sym_sp"
(define-symbol-macro %symsp*% (llvm-sys:type-get-pointer-to %symsp%))

(define-c++-struct %cons% +cons-tag+
  ( (%t*% :car)
    (%t*% :cdr)))

(define-symbol-macro %cons*% (llvm-sys:type-get-pointer-to %cons%))

(defparameter +cons.car-index+ (c++-field-index :car info.%cons%))
(defparameter +cons.cdr-index+ (c++-field-index :cdr info.%cons%))
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
    ((%t**% :args)     ; This is a pointer to T*
     (%uintptr_t% :nargs)))
  (define-symbol-macro %vaslist*% (llvm-sys:type-get-pointer-to %vaslist%))

;;;    "Function prototype for generic functions")
  (define-symbol-macro %fn-gf% (llvm-sys:function-type-get %tmv% (list %t*% %t*%)))
  (define-symbol-macro %fn-gf-arguments% (list "gf" "args"))
  )

(defun vaslist*-args* (vaslist* &optional (label "args*"))
  (ensure-opaque-or-pointee-type-matches vaslist* %vaslist%)
  (irc-struct-gep %vaslist% vaslist* 0 label))

(defun vaslist*-shifted-nargs* (vaslist* &optional (label "nargs*"))
  (ensure-opaque-or-pointee-type-matches vaslist* %vaslist%)
  (irc-struct-gep %vaslist% vaslist* 1 label))

(defun vaslist*-set-nargs (vaslist* nargs)
  (let* ((pos* (vaslist*-shifted-nargs* vaslist*))
         (shifted-nargs (irc-shl nargs +vaslist-nargs-shift+)))
    (irc-store shifted-nargs pos*)))

(defun vaslist-start (vaslist* nargs &optional args)
  (when args
    (irc-maybe-check-word-aligned-load %t*% args)
    (irc-store args (vaslist*-args* vaslist*)))
  (vaslist*-set-nargs vaslist* nargs))

;;; Generate code to read the next argument from the vaslist*
;;; The vaslist* argument MUST be an untagged pointer to a %vaslist%
(defun gen-vaslist-pop (vaslist*)
  (irc-maybe-check-word-aligned-load %vaslist% vaslist*)
  (let* ((args* (vaslist*-args* vaslist* "gvp-args*"))
         (args (irc-typed-load %t**% args* "gvp-args"))
         (val (irc-t*-load args "gvp-val"))
         (args-next (irc-typed-gep %t*% args (list 1)))
         (shifted-nargs* (vaslist*-shifted-nargs* vaslist* "gvp-nargs*"))
         (shifted-nargs (irc-typed-load %i64% shifted-nargs*))
         (shifted-nargs-next (irc-sub shifted-nargs (jit-constant-i64 +vaslist-nargs-decrement+))))
    (irc-maybe-check-word-aligned-load %t*% args-next)
    (irc-store args-next args*)
    (irc-store shifted-nargs-next shifted-nargs*)
    val))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide the arguments passed to the function in a convenient manner.
;; Either the register arguments are available in register-args
;;   or the vaslist is used to access the arguments
;;   one after the other with calling-convention.va-arg
(defstruct calling-convention
  closure
  nargs
  register-args ; The arguments that were passed in registers
  vaslist*      ; The address of the vaslist, or NIL
  cleavir-lambda-list-analysis ; analysis of cleavir-lambda-list
  rest-alloc ; whether we can dx or ignore a &rest argument
  )

;; Parse the function arguments into a calling-convention

(defun initialize-calling-convention (llvm-function arity &key debug-on cleavir-lambda-list-analysis rest-alloc)
  (let ((arguments (llvm-sys:get-argument-list llvm-function)))
    (let ((register-save-area* (when debug-on (alloca-register-save-area arity :label "register-save-area")))
          (closure (first arguments)))
      (unless (first arguments)
        (error "initialize-calling-convention for arguments ~a - the closure is NIL" arguments))
      (cond
        ((eq arity :general-entry)
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

;;;
;;; Read the next argument from the vaslist
(defun calling-convention-vaslist.va-arg (cc)
  (let* ((vaslist (calling-convention-vaslist* cc)))
    (gen-vaslist-pop vaslist)))

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

;; (Maybe) generate code to store registers in memory. Return value unspecified.  
(defun maybe-spill-to-register-save-area (arity register-save-area* registers)
  (when register-save-area*
    (let ((words (irc-arity-info arity)))
      (flet ((spill-reg (idx reg addr-name)
               (let ((addr          (irc-typed-gep (llvm-sys:array-type-get %t*% words) register-save-area* (list 0 idx) addr-name))
                     (reg-i8*       (cond
                                      ((llvm-sys:type-equal (llvm-sys:get-type reg) %i64%)
                                       (irc-int-to-ptr reg %i8*% "nargs-i8*"))
                                      (t
                                       (irc-bit-cast reg %i8*% "reg-i8*")))))
                 (irc-store reg-i8* addr t)
                 addr)))
        (let* ((names (if (eq arity :general-entry)
                          (list "rsa-closure" "rsa-nargs" "rsa-args")
                          (list "rsa-closure" "rsa-arg0" "rsa-arg1" "rsa-arg2" "rsa-arg3" "rsa-arg4" "rsa-arg5" "rsa-arg6" "rsa-arg7")))
               (idx 0))
          (mapc (lambda (reg name)
                  (spill-reg idx reg name)
                  (incf idx))
                registers names))))))

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
;; The %function% type MUST match the layout and size of Function_O in function.h
;;
(define-c++-struct %Function% +general-tag+
  ((%i8*%      :vtable)
   (%global-entry-point*% :global-entry-point)))

(define-symbol-macro %Function*% (llvm-sys:type-get-pointer-to %Function%))
(define-symbol-macro %Function_sp% (llvm-sys:struct-type-get (thread-local-llvm-context) (smart-pointer-fields %Function*%) nil)) ;; "Cfn_sp"
(define-symbol-macro %Function_sp*% (llvm-sys:type-get-pointer-to %Function_sp%))

;;; ------------------------------------------------------------
;;;
;;; This must match FunctionDescription in function.h
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

(define-c++-struct %closure% +general-tag+
  ((%i8*% vtable)
   (%t*% entry-point)
   (%size_t% data-length)
   (%tsp[0]% data0))
  )
(core:verify-closure (c++-struct-field-offsets info.%closure%))

(defun %closure%.offset-of[n]/t* (index)
  "This assumes that the t* offset coincides with the tsp start"
  (let* ((offset-of-data (cdr (assoc 'data0 (c++-struct-field-offsets info.%closure%))))
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
    (typeid-core-unwind      "_ZTIN4core6UnwindE")
    ))

(define-symbol-macro %jmp-buf-tag%
    (llvm-sys:array-type-get %i8% +jmp-buf-size+))
(define-symbol-macro %jmp-buf-tag*%
    (llvm-sys:type-get-pointer-to %jmp-buf-tag%))

(defvar *exception-types-hash-table* (make-hash-table :test #'eq)
  "Map exception names to exception class extern 'C' names")

(mapcar #'(lambda (x &aux (name (car x)) (cname (cadr x)))
            (setf (gethash name *exception-types-hash-table*) cname))
	*exceptions*)

(defun exception-typeid*-from-name (name)
  (let* ((cname (gethash name *exception-types-hash-table*))
	 (i8* (llvm-sys:get-or-create-external-global *the-module* cname %i8% 'llvm-sys:external-linkage)))
    i8*))

(defun assert-result-isa-llvm-value (result)
  (unless (llvm-sys:llvm-value-p result)
    (error "result must be an instance of llvm-sys:Value_O but instead it has the value ~s" result)))


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
  (let* ((name-suffix (core:fmt nil "{:05d}-{}" (core:next-number) file-name-modifier))
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
  (let* ((name-suffix (core:fmt nil "module-{:05d}-{}" (core:next-number) file-name-modifier))
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
  (if *compile-file-output-pathname*
      (compile-file-quick-module-dump module name-modifier *compile-file-debug-dump-module*)
      (compile-quick-module-dump module name-modifier *compile-debug-dump-module*)))

(defmacro log-module ((info) &rest body)
  "Wrap quick-module-dump around a block of code so that the module
is dumped to a file before the block and after the block."
  `(progn
     (llvm-sys:sanity-check-module *the-module* 2)
     (quick-module-dump *the-module* ,(format nil "~a-begin" info))
     (multiple-value-prog1 (progn ,@body)
       (llvm-sys:sanity-check-module *the-module* 2)
       (quick-module-dump *the-module* ,(format nil "~a-end" info)))))


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

(defun find-intrinsic-name (&rest names)
  (dolist (name names (error "Unable to find a valid intrinsic from ~s" names))
    (unless (zerop (llvm-sys:lookup-intrinsic-id name))
      (return name))))

(defvar +intrinsic/llvm.eh.typeid.for.p0+
  (find-intrinsic-name "llvm.eh.typeid.for.p0" "llvm.eh.typeid.for"))

(defvar +intrinsic/llvm.stacksave.p0+
  (find-intrinsic-name "llvm.stacksave.p0" "llvm.stacksave"))

(defvar +intrinsic/llvm.stackrestore.p0+
  (find-intrinsic-name "llvm.stackrestore.p0" "llvm.stackrestore"))
