(core:getpid)
(require :asdf)

(progn
  (asdf:load-asd (probe-file "source-dir:src;lisp;modules;clang-tool;clang-tool.asd"))
  (asdf:load-system :clang-tool))

(progn
  (format t "---- 1. Setup and load the compilation database~%")
  (defparameter *db* (clang-tool:load-compilation-tool-database
                      "/home/meister/Development/clasp/build/mpsprep/compile_commands.json")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define the ASTMatcher and the code to carry out the refactoring
;;;

(defparameter *refactor*
  (make-instance
   'clang-tool:code-match-callback
   :match-code
   (lambda (node)
     (let* ((call (clang-tool:mtag-node node :root))
            (call-source (clang-tool:mtag-source node :root)))
       (when (> (length call-source) 0)
         (let ((fixup (concatenate 'string "nil" (subseq call-source 4))))
           (clang-tool:mtag-replace node :root
                                    (lambda (match-info tag)
                                      fixup))))))
   :end-of-translation-unit-code
   (lambda ()
     (format t "!!!!!!!! Hit the end-of-translation-unit~%")
     (format t "*match-refactoring-tool* ~a~%" clang-tool:*match-refactoring-tool*)
     (format t "*run-and-save* ~a~%" clang-tool:*run-and-save*)
     (let ((repl (ast-tooling:replacements-as-list clang-tool:*match-refactoring-tool*)))
       (format t "~a~%" repl)))))

#+(or)(defparameter *refactor-method-call*
        (make-instance
         'clang-tool:code-match-callback
         :match-code
         (lambda (node)
           (let* ((call (clang-tool:mtag-node node :CALL))
                  (call-source (clang-tool:mtag-source node :CALL))
                  (loc (clang-tool:mtag-loc-start node :CALL))
                  (method-decl (cast:get-method-decl call)))
             (when method-decl
               (let* ((method-name (cast:get-name-as-string method-decl))
                      (expr (cast:get-implicit-object-argument call))
                      (expr-source (clang-tool:mtag-source-impl node expr))
                      (most-derived-type (cast:get-best-dynamic-class-type expr))
                      (m-d-t-name (cast:get-qualified-name-as-string most-derived-type)))
                 (when (and (string= m-d-t-name "core::HashGenerator") (string= method-name "isFilling"))
                   (format t "-------- loc: ~a~%" loc )
                   (format t "Call: ~a name: ~a~%" call method-name)
                   (format t "call-source: ~a~%" call-source)
                   (format t "expr: ~a~%" expr)
                   (format t "most-derived-type: ~a~%" most-derived-type)
                   (format t "m-d-t-name: ~a~%" m-d-t-name)
                   (format t "Source: ~a~%" expr-source)
                   (if (string/= call-source "")
                       (progn
                         (format t "About to do mtag-replace~%")
                         (when clang-tool:*match-refactoring-tool*
                           (clang-tool:mtag-replace node :CALL
                                                    (lambda (match-info tag)
                                                      (format t "Returning replacement thing~%")
                                                      expr-source))))
                       (format t "!!!!!Skipping replacement due to macro~%")))))))
         :end-of-translation-unit-code
         (lambda ()
           (format t "!!!!!!!! Hit the end-of-translation-unit~%")
           (format t "*match-refactoring-tool* ~a~%" clang-tool:*match-refactoring-tool*)
           (format t "*run-and-save* ~a~%" clang-tool:*run-and-save*)
           (let ((repl (ast-tooling:replacements-as-list clang-tool:*match-refactoring-tool*)))
             (format t "~a~%" repl)))))


(progn
  (format t "---- 2. Defining the matcher for class/field/method~%")
;;;
;;; Adjust the compiler arguments for refactoring
;;;
  (defparameter *arg-adjuster* (lambda (args filename)
                                 (declare (ignore filename))
                                 (prog1
                                     (concatenate 'vector args
                                                  #("-DUSE_BOEHM"))
                                   (format t "Leaving arg-adjuster~%"))))


  #|
#include <cstdint>
namespace gctools {
typedef std::size_t Tagged;
template <class T>
struct smart_ptr {
    T* _raw;
    smart_ptr(Tagged x) : _raw((T*)x) {};
};
}
template <class T>
gctools::smart_ptr<T> _Nil() {
  gctools::smart_ptr<T> x((gctools::Tagged)0);
  return x;
}    
int main(int argc, const char* argv[]) {
    gctools::smart_ptr<void> v = _Nil<void>();
}
|#
;;;
;;; Define the matcher for method calls with nullary (0) arguments
;;; Note: I could make this more specific to look for particular methods
;;;       but I'll do that checking in the callback
  (defparameter *matcher*
    '(:call-expr
      (:bind :root (:call-expr))
      (:callee
       (:function-decl
        (:has-name "_Nil")
        (:is-template-instantiation)))))

  #+(or)(defparameter *matcher*
    (ast-tooling:parse-dynamic-matcher "callExpr(callee(functionDecl(hasName(\"_Unbound\"),isTemplateInstantiation()).bind(\"id\")))"))

;;;
;;; Define the callback that looks for method calls like:
;;;    x->get() where x-> is dereferencing a Fixnum_O pointer
;;; rewrite it as unbox_fixnum(x)
;;;
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test and run 
;;;

;;; Testing
;;;
;;; Load a subset of the ASTs and run a quick test of the matcher on them
;;;
(defparameter *test-db* (clang-tool:copy-compilation-tool-database *db* :source-pattern "/array.cc"))


(defun do-match-test ()
  (clang-tool:batch-match-run *matcher*
                              :compilation-tool-database *test-db*
                              :the-code-match-callback *refactor*))

#+(or)(defparameter *asts* (clang-tool:load-asts *test-db*))

#+(or)(defun do-match-test()
  (clang-tool:match-run-loaded-asts *asts* *matcher* :callback *refactor*)
  (print "Done"))


;;; Testing - although there is no feedback yet on the Replacements generated
;;;
;;; Generate replacements but don't save them
;;;
(defun do-match-all ()
  (clang-tool:batch-match-run *matcher*
                              :compilation-tool-database *db*
                              :the-code-match-callback *refactor*))

;;; Production run
;;;
;;; Generate replacements and write them back to the C++ code
;;; WARNING: No backups are kept - use git to rewind changes if they don't work
;;;
(defun do-change-all ()
    (clang-tool:batch-match-run *matcher*
     :compilation-tool-database *db*
     :the-code-match-callback *refactor*
     :run-and-save t))
