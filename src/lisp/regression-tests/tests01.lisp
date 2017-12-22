#+use-mps
(progn (princ "Skipping test CXX-DERIVABILITY - CXX objects don't work in MPS yet (fix!)") (terpri))
#-use-mps
(test cxx-derivability (inherits-from-instance (make-cxx-object 'ast-tooling:match-callback))
      :description "A derivable class is a CLOS class that derives from a C++ class.
They are defined in the clbind library using Derivable<Foo>.
They must be seen as inheriting from the Instance_O class.
If they don't then any code that uses them won't work properly.
Check clasp/include/clasp/core/instance.h header file for the 
Instance_O specialization of TaggedCast")

;;; Array tests
(test equal-bit-vector (equal (make-array 3 :element-type 'bit :initial-contents '(1 1 1)) #*111))
(test equalp-ub8-vector
      (equalp (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 1 1)) #(1 1 1)))
(test string=0 (string= (make-array 3 :element-type 'character :initial-element #\a) "aaa"))
(test string=1 (string= (make-array 3 :element-type 'character :initial-contents '(#\a #\b #\c)) "abc"))
(defparameter *bitvec* (make-array 10 :element-type 'bit :initial-element 1))
(test equalp-bit-vector (equalp *bitvec* #*1111111111))
(setf (sbit *bitvec* 5) 0)
(test sbit (equalp *bitvec* #*1111101111))

(defparameter *bn* 23482395823512381241927312749127418274918273)
(defparameter *s* (make-array 256 :element-type 'base-char :fill-pointer 0))
(core:integer-to-string *s* *bn* 10 nil nil)
(test integer-to-string-positive (string= *s* "23482395823512381241927312749127418274918273"))
(defparameter *s* (make-array 256 :element-type 'base-char :fill-pointer 0))
(core:integer-to-string *s* (- *bn*) 10 nil nil)
(test integer-to-string-negative (string= *s* "-23482395823512381241927312749127418274918273"))
