;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; memory.lisp --- Tests for memory referencing.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;; Adapted for testing Clasp's FLI implementation by
;;;   Frank Gönninger <frank.goenninger@goenninger.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(in-package #:clasp-ffi.tests)

(rt:deftest deref.char
    (cffi:with-foreign-object (p :char)
      (setf (cffi:mem-ref p :char) -127)
      (cffi:mem-ref p :char))
  -127)

(rt:deftest deref.unsigned-char
    (cffi:with-foreign-object (p :unsigned-char)
      (setf (cffi:mem-ref p :unsigned-char) 255)
      (cffi:mem-ref p :unsigned-char))
  255)

(rt:deftest deref.short
    (cffi:with-foreign-object (p :short)
      (setf (cffi:mem-ref p :short) -32767)
      (cffi:mem-ref p :short))
  -32767)

(rt:deftest deref.unsigned-short
    (cffi:with-foreign-object (p :unsigned-short)
      (setf (cffi:mem-ref p :unsigned-short) 65535)
      (cffi:mem-ref p :unsigned-short))
  65535)

(rt:deftest deref.int
    (cffi:with-foreign-object (p :int)
      (setf (cffi:mem-ref p :int) -131072)
      (cffi:mem-ref p :int))
  -131072)

(rt:deftest deref.unsigned-int
    (cffi:with-foreign-object (p :unsigned-int)
      (setf (cffi:mem-ref p :unsigned-int) 262144)
      (cffi:mem-ref p :unsigned-int))
  262144)

(rt:deftest deref.long
    (cffi:with-foreign-object (p :long)
      (setf (cffi:mem-ref p :long) -536870911)
      (cffi:mem-ref p :long))
  -536870911)

(rt:deftest deref.unsigned-long
    (cffi:with-foreign-object (p :unsigned-long)
      (setf (cffi:mem-ref p :unsigned-long) 536870912)
      (cffi:mem-ref p :unsigned-long))
  536870912)

#+(and darwin openmcl)
(pushnew 'deref.long-long rt::*expected-failures*)

(rt:deftest deref.long-long
    (cffi:with-foreign-object (p :long-long)
      (setf (cffi:mem-ref p :long-long) -9223372036854775807)
      (cffi:mem-ref p :long-long))
  -9223372036854775807)

(rt:deftest deref.unsigned-long-long
    (cffi:with-foreign-object (p :unsigned-long-long)
      (setf (cffi:mem-ref p :unsigned-long-long) 18446744073709551615)
      (cffi:mem-ref p :unsigned-long-long))
  18446744073709551615)

(rt:deftest deref.float.1
    (cffi:with-foreign-object (p :float)
      (setf (cffi:mem-ref p :float) 0.0)
      (cffi:mem-ref p :float))
  0.0)

(rt:deftest deref.double.1
    (cffi:with-foreign-object (p :double)
      (setf (cffi:mem-ref p :double) 0.0d0)
      (cffi:mem-ref p :double))
  0.0d0)

#+long-double
(rt:deftest deref.long-double.1
    (cffi:with-foreign-object (p :long-double)
      (setf (cffi:mem-ref p :long-double) 0.0l0)
      (cffi:mem-ref p :long-double))
  0.0l0)


;;; make sure the lisp doesn't convert NULL to NIL
(rt:deftest deref.pointer.null
    (cffi:with-foreign-object (p :pointer)
      (setf (cffi:mem-ref p :pointer) (cffi:null-pointer))
      (cffi:null-pointer-p (cffi:mem-ref p :pointer)))
  t)

;;; regression test. lisp-string-to-foreign should handle empty strings
(rt:deftest lisp-string-to-foreign.empty
    (cffi:with-foreign-pointer (str 2)
      (setf (cffi:mem-ref str :unsigned-char) 42)
      (cffi:lisp-string-to-foreign "" str 1)
      (cffi:mem-ref str :unsigned-char))
  0)

;;; regression test. with-foreign-pointer shouldn't evaluate
;;; the size argument twice.
(rt:deftest with-foreign-pointer.evalx2
    (let ((count 0))
      (cffi:with-foreign-pointer (x (incf count) size-var)
        (values count size-var)))
  1 1)

(defconstant +two+ 2)

;;; regression test. cffi-allegro's with-foreign-pointer wasn't
;;; handling constants properly.
(rt:deftest with-foreign-pointer.constant-size
    (cffi:with-foreign-pointer (p +two+ size)
      size)
  2)

(rt:deftest mem-ref.left-to-right
    (let ((i 0))
      (cffi:with-foreign-object (p :char 3)
        (setf (cffi:mem-ref p :char 0) 66 (cffi:mem-ref p :char 1) 92)
        (setf (cffi:mem-ref p :char (incf i)) (incf i))
        (values (cffi:mem-ref p :char 0) (cffi:mem-ref p :char 1) i)))
  66 2 2)

;;; This needs to be in a real function for at least Allegro CL or the
;;; compiler macro on %MEM-REF is not expanded and the test doesn't
;;; actually test anything!
(defun %mem-ref-left-to-right ()
  (let ((result nil))
    (cffi:with-foreign-object (p :char)
      (%mem-set 42 p :char)
      (%mem-ref (progn (push 1 result) p) :char (progn (push 2 result) 0))
      (nreverse result))))

;;; Test left-to-right evaluation of the arguments to %MEM-REF when
;;; optimized by the compiler macro.
(rt:deftest %mem-ref.left-to-right
    (%mem-ref-left-to-right)
  (1 2))

;;; This needs to be in a top-level function for at least Allegro CL
;;; or the compiler macro on %MEM-SET is not expanded and the test
;;; doesn't actually test anything!
(defun %mem-set-left-to-right ()
  (let ((result nil))
    (cffi:with-foreign-object (p :char)
      (%mem-set (progn (push 1 result) 0)
                (progn (push 2 result) p)
                :char
                (progn (push 3 result) 0))
      (nreverse result))))

;;; Test left-to-right evaluation of the arguments to %MEM-SET when
;;; optimized by the compiler macro.
(rt:deftest %mem-set.left-to-right
    (%mem-set-left-to-right)
  (1 2 3))

;; regression test. mem-aref's setf expansion evaluated its type argument twice.
(rt:deftest mem-aref.eval-type-x2
    (let ((count 0))
      (cffi:with-foreign-pointer (p 1)
        (setf (cffi:mem-aref p (progn (incf count) :char) 0) 127))
      count)
  1)

(rt:deftest mem-aref.left-to-right
    (let ((count -1))
      (cffi:with-foreign-pointer (p 2)
        (values
         (setf (cffi:mem-aref p (progn (incf count) :char) (incf count)) (incf count))
         (setq count -1)
         (cffi:mem-aref (progn (incf count) p) :char (incf count))
         count)))
  2 -1 2 1)

;; regression tests. nested mem-ref's and mem-aref's had bogus getters
(rt:deftest mem-ref.nested
    (cffi:with-foreign-object (p :pointer)
      (cffi:with-foreign-object (i :int)
        (setf (cffi:mem-ref p :pointer) i)
        (setf (cffi:mem-ref i :int) 42)
        (setf (cffi:mem-ref (mem-ref p :pointer) :int) 1984)
        (cffi:mem-ref i :int)))
  1984)

(rt:deftest mem-aref.nested
    (cffi:with-foreign-object (p :pointer)
      (cffi:with-foreign-object (i :int 2)
        (setf (cffi:mem-aref p :pointer 0) i)
        (setf (cffi:mem-aref i :int 1) 42)
        (setf (cffi:mem-aref (cffi:mem-ref p :pointer 0) :int 1) 1984)
        (cffi:mem-aref i :int 1)))
  1984)

(cffi:defcstruct mem-aref.bare-struct
    (a :uint8))

;;; regression test: although mem-aref was dealing with bare struct
;;; types as though they were pointers, it wasn't calculating the
;;; proper offsets. The offsets for bare structs types should be
;;; calculated as aggregate types.
(rt:deftest mem-aref.bare-struct
    (cffi:with-foreign-object (a 'mem-aref.bare-struct 2)
      (eql (- (cffi:pointer-address (cffi:mem-aref a 'mem-aref.bare-struct 1))
              (cffi:pointer-address (cffi:mem-aref a 'mem-aref.bare-struct 0)))
           (cffi:foreign-type-size '(:struct mem-aref.bare-struct))))
  t)

;;; regression tests. dereferencing an aggregate type. dereferencing a
;;; struct should return a pointer to the struct itself, not return the
;;; first 4 bytes (or whatever the size of :pointer is) as a pointer.
;;;
;;; This important for accessing an array of structs, which is
;;; what the deref.array-of-aggregates test does.
(cffi:defcstruct some-struct (x :int))

(rt:deftest deref.aggregate
    (cffi:with-foreign-object (s 'some-struct)
      (cffi:pointer-eq s (cffi:mem-ref s 'some-struct)))
  t)

(rt:deftest deref.array-of-aggregates
    (cffi:with-foreign-object (arr 'some-struct 3)
      (loop for i below 3
         do (setf (cffi:foreign-slot-value (cffi:mem-aref arr 'some-struct i)
                                           'some-struct 'x)
                  112))
      (loop for i below 3
         collect (cffi:foreign-slot-value (cffi:mem-aref arr 'some-struct i)
                                          'some-struct 'x)))
  (112 112 112))

;;; pointer operations
(rt:deftest pointer.1
    (cffi:pointer-address (cffi:make-pointer 42))
  42)

;;; I suppose this test is not very good. --luis
(rt:deftest pointer.2
    (cffi:pointer-address (cffi:null-pointer))
  0)

(rt:deftest pointer.null
    (nth-value 0 (ignore-errors (cffi:null-pointer-p nil)))
  nil)

(rt:deftest foreign-pointer-type.nil
    (typep nil 'cffi:foreign-pointer)
  nil)

;;; Ensure that a pointer to the highest possible address can be
;;; created using MAKE-POINTER.  Regression test for CLISP/X86-64.
(rt:deftest make-pointer.high
    (let* ((pointer-length (cffi:foreign-type-size :pointer))
           (high-address (1- (expt 2 (* pointer-length 8))))
           (pointer (cffi:make-pointer high-address)))
      (- high-address (cffi:pointer-address pointer)))
  0)

;;; Ensure that incrementing a pointer by zero bytes returns an
;;; equivalent pointer.
(rt:deftest inc-pointer.zero
    (cffi:with-foreign-object (x :int)
      (cffi:pointer-eq x (cffi:inc-pointer x 0)))
  t)

;;; Test the INITIAL-ELEMENT keyword argument to FOREIGN-ALLOC.
(rt:deftest foreign-alloc.1
    (let ((ptr (cffi:foreign-alloc :int :initial-element 42)))
      (unwind-protect
           (cffi:mem-ref ptr :int)
        (cffi:foreign-free ptr)))
  42)

;;; Test the INITIAL-ELEMENT and COUNT arguments to FOREIGN-ALLOC.
(rt:deftest foreign-alloc.2
    (let ((ptr (cffi:foreign-alloc :int :count 4 :initial-element 100)))
      (unwind-protect
           (loop for i from 0 below 4
              collect (cffi:mem-aref ptr :int i))
        (cffi:foreign-free ptr)))
  (100 100 100 100))

;;; Test the INITIAL-CONTENTS and COUNT arguments to FOREIGN-ALLOC,
;;; passing a list of initial values.
(rt:deftest foreign-alloc.3
    (let ((ptr (cffi:foreign-alloc :int :count 4 :initial-contents '(4 3 2 1))))
      (unwind-protect
           (loop for i from 0 below 4
              collect (cffi:mem-aref ptr :int i))
        (cffi:foreign-free ptr)))
  (4 3 2 1))

;;; Test INITIAL-CONTENTS and COUNT with FOREIGN-ALLOC passing a
;;; vector of initial values.
(rt:deftest foreign-alloc.4
    (let ((ptr (cffi:foreign-alloc :int :count 4 :initial-contents #(10 20 30 40))))
      (unwind-protect
           (loop for i from 0 below 4
              collect (cffi:mem-aref ptr :int i))
        (cffi:foreign-free ptr)))
  (10 20 30 40))

;;; Ensure calling FOREIGN-ALLOC with both INITIAL-ELEMENT and
;;; INITIAL-CONTENTS signals an error.
(rt:deftest foreign-alloc.5
    (values
     (ignore-errors
       (let ((ptr (cffi:foreign-alloc :int :initial-element 1
                                      :initial-contents '(1))))
         (cffi:foreign-free ptr))
       t))
  nil)

;;; Regression test: FOREIGN-ALLOC shouldn't actually perform translation
;;; on initial-element/initial-contents since MEM-AREF will do that already.
(cffi:define-foreign-type not-an-int ()
  ()
  (:actual-type :int)
  (:simple-parser not-an-int))

(defmethod cffi::translate-to-foreign (value (type not-an-int))
  (assert (not (integerp value)))
  0)

(rt:deftest foreign-alloc.6
    (let ((ptr (cffi:foreign-alloc 'not-an-int :initial-element 'foooo)))
      (cffi:foreign-free ptr)
      t)
  t)

;;; Ensure calling FOREIGN-ALLOC with NULL-TERMINATED-P and a non-pointer
;;; type signals an error.
(rt:deftest foreign-alloc.7
    (values
     (ignore-errors
       (let ((ptr (cffi:foreign-alloc :int :null-terminated-p t)))
         (cffi:foreign-free ptr))
       t))
  nil)

;;; The opposite of the above test.
(cffi:defctype pointer-alias :pointer)

(rt:deftest foreign-alloc.8
    (progn
      (cffi:foreign-free (cffi:foreign-alloc 'pointer-alias :count 0 :null-terminated-p t))
      t)
  t)

;;; Ensure calling FOREIGN-ALLOC with NULL-TERMINATED-P actually places
;;; a null pointer at the end. Not a very reliable test apparently.
(rt:deftest foreign-alloc.9
    (let ((ptr (cffi:foreign-alloc :pointer :count 0 :null-terminated-p t)))
      (unwind-protect
           (cffi:null-pointer-p (cffi:mem-ref ptr :pointer))
        (cffi:foreign-free ptr)))
  t)

;;; RT: FOREIGN-ALLOC with :COUNT 0 on CLISP signalled an error.
(rt:deftest foreign-alloc.10
    (cffi:foreign-free (cffi:foreign-alloc :char :count 0))
  nil)

;;; Tests for mem-ref with a non-constant type. This is a way to test
;;; the functional interface (without compiler macros).

(rt:deftest deref.nonconst.char
    (let ((type :char))
      (cffi:with-foreign-object (p type)
        (setf (cffi:mem-ref p type) -127)
        (cffi:mem-ref p type)))
  -127)

(rt:deftest deref.nonconst.unsigned-char
    (let ((type :unsigned-char))
      (cffi:with-foreign-object (p type)
        (setf (cffi:mem-ref p type) 255)
        (cffi:mem-ref p type)))
  255)

(rt:deftest deref.nonconst.short
    (let ((type :short))
      (cffi:with-foreign-object (p type)
        (setf (cffi:mem-ref p type) -32767)
        (cffi:mem-ref p type)))
  -32767)

(rt:deftest deref.nonconst.unsigned-short
    (let ((type :unsigned-short))
      (cffi:with-foreign-object (p type)
        (setf (cffi:mem-ref p type) 65535)
        (cffi:mem-ref p type)))
  65535)

(rt:deftest deref.nonconst.int
    (let ((type :int))
      (cffi:with-foreign-object (p type)
        (setf (cffi:mem-ref p type) -131072)
        (cffi:mem-ref p type)))
  -131072)

(rt:deftest deref.nonconst.unsigned-int
    (let ((type :unsigned-int))
      (cffi:with-foreign-object (p type)
        (setf (cffi:mem-ref p type) 262144)
        (cffi:mem-ref p type)))
  262144)

(rt:deftest deref.nonconst.long
    (let ((type :long))
      (cffi:with-foreign-object (p type)
        (setf (cffi:mem-ref p type) -536870911)
        (cffi:mem-ref p type)))
  -536870911)

(rt:deftest deref.nonconst.unsigned-long
    (let ((type :unsigned-long))
      (cffi:with-foreign-object (p type)
        (setf (cffi:mem-ref p type) 536870912)
        (cffi:mem-ref p type)))
  536870912)

#+(and darwin openmcl)
(pushnew 'deref.nonconst.long-long rt::*expected-failures*)

(rt:deftest deref.nonconst.long-long
    (let ((type :long-long))
      (cffi:with-foreign-object (p type)
        (setf (cffi:mem-ref p type) -9223372036854775807)
        (cffi:mem-ref p type)))
  -9223372036854775807)

(rt:deftest deref.nonconst.unsigned-long-long
    (let ((type :unsigned-long-long))
      (cffi:with-foreign-object (p type)
        (setf (cffi:mem-ref p type) 18446744073709551615)
        (cffi:mem-ref p type)))
  18446744073709551615)

(rt:deftest deref.nonconst.float.1
    (let ((type :float))
      (cffi:with-foreign-object (p type)
        (setf (cffi:mem-ref p type) 0.0)
        (cffi:mem-ref p type)))
  0.0)

(rt:deftest deref.nonconst.double.1
    (let ((type :double))
      (cffi:with-foreign-object (p type)
        (setf (cffi:mem-ref p type) 0.0d0)
        (cffi:mem-ref p type)))
  0.0d0)

(defun mem-ref-rt-1 ()
  (cffi:with-foreign-object (a :int 2)
    (setf (cffi:mem-aref a :int 0) 123
          (cffi:mem-aref a :int 1) 456)
    (values (cffi:mem-aref a :int 0) (cffi:mem-aref a :int 1))))

(rt:deftest mem-ref.rt.1
    (mem-ref-rt-1)
  123 456)

(defun mem-ref-rt-2 ()
  (cffi:with-foreign-object (a :double 2)
    (setf (cffi:mem-aref a :double 0) 123.0d0
          (cffi:mem-aref a :double 1) 456.0d0)
    (values (cffi:mem-aref a :double 0) (cffi:mem-aref a :double 1))))

(rt:deftest mem-ref.rt.2
    (mem-ref-rt-2)
  123.0d0 456.0d0)

(rt:deftest incf-pointer.1
    (let ((ptr (cffi:null-pointer)))
      (cffi:incf-pointer ptr)
      (cffi:pointer-address ptr))
  1)

(rt:deftest incf-pointer.2
    (let ((ptr (cffi:null-pointer)))
      (cffi:incf-pointer ptr 42)
      (cffi:pointer-address ptr))
  42)

(rt:deftest pointerp.1
    (values
     (cffi:pointerp (cffi:null-pointer))
     (cffi:null-pointer-p (cffi:null-pointer))
     (typep (cffi:null-pointer) 'cffi:foreign-pointer))
  t t t)

(rt:deftest pointerp.2
    (let ((p (cffi:make-pointer #xFEFF)))
      (values
       (cffi:pointerp p)
       (typep p 'cffi:foreign-pointer)))
  t t)

(rt:deftest pointerp.3
    (cffi:pointerp 'not-a-pointer)
  nil)

(rt:deftest pointerp.4
    (cffi:pointerp 42)
  nil)

(rt:deftest pointerp.5
    (cffi:pointerp 0)
  nil)

(rt:deftest pointerp.6
    (cffi:pointerp nil)
  nil)

(rt:deftest mem-ref.setf.1
    (cffi:with-foreign-object (p :char)
      (setf (cffi:mem-ref p :char) 42))
  42)

(cffi:define-foreign-type int+1 ()
  ()
  (:actual-type :int)
  (:simple-parser int+1))

(defmethod cffi:translate-to-foreign (value (type int+1))
  (1+ value))

(defmethod cffi:translate-from-foreign (value (type int+1))
  (1+ value))

(rt:deftest mem-ref.setf.2
    (cffi:with-foreign-object (p 'int+1)
      (values (setf (cffi:mem-ref p 'int+1) 42)
              (cffi:mem-ref p 'int+1)))
  42 ; should this be 43?
  44)

(rt:deftest pointer-eq.non-pointers.1
    (expecting-error (cffi:pointer-eq 1 2))
  :error)

(rt:deftest pointer-eq.non-pointers.2
    (expecting-error (cffi:pointer-eq 'a 'b))
  :error)

(rt:deftest null-pointer-p.non-pointer.1
    (expecting-error (cffi:null-pointer-p 'not-a-pointer))
  :error)

(rt:deftest null-pointer-p.non-pointer.2
    (expecting-error (cffi:null-pointer-p 0))
  :error)

(rt:deftest null-pointer-p.non-pointer.3
    (expecting-error (cffi:null-pointer-p nil))
  :error)
