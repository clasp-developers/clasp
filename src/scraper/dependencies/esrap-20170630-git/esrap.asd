;;;; Copyright (c) 2007-2013 Nikodemus Siivola <nikodemus@random-state.net>
;;;; Copyright (c) 2012-2017 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defsystem :esrap
  :version          "0.15"
  :description      "A Packrat / Parsing Grammar / TDPL parser for Common Lisp."
  :long-description "A Packrat / Parsing Grammar / TDPL parser for Common Lisp.

                     Notable features include

                     * dynamic redefinition of nonterminals
                     * inline grammars
                     * semantic predicates
                     * introspective facilities (describing grammars,
                       tracing, setting breaks)
                     * left-recursive grammars
                     * functions as terminals
                     * accurate, customizable parse error reports

                     See README.org and :homepage for more
                     information."
  :author           ("Nikodemus Siivola <nikodemus@random-state.net>"
                     "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>")
  :maintainer       "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :homepage         "https://scymtym.github.io/esrap"
  :bug-tracker      "https://github.com/scymtym/esrap/issues"
  :source-control   (:git "https://github.com/scymtym/esrap.git")
  :licence          "MIT"
  :depends-on       (:alexandria)
  :components       ((:module "src"
                      :serial t
                      :components ((:file "package")
                                   (:file "types")
                                   (:file "protocol")
                                   (:file "variables")
                                   (:file "conditions")
                                   (:file "expressions")
                                   (:file "rule")
                                   (:file "results")
                                   (:file "cache")
                                   (:file "evaluator")
                                   (:file "macros")
                                   (:file "interface")
                                   (:file "editor-support")))

                     (:module "examples"
                      :components ((:static-file "sexp.lisp")
                                   (:static-file "symbol-table.lisp")
                                   (:static-file "left-recursion.lisp")
                                   (:static-file "function-terminals.lisp")))

                     (:static-file "README.org"))
  :in-order-to      ((test-op (test-op :esrap/tests))))

(defmethod perform :after ((op load-op) (sys (eql (find-system :esrap))))
  ;; Since version 0.16
  ;; * DEFRULE accepts an :ERROR-REPORT option
  ;; Since version 0.15
  ;; * All transforms that support it, can access bounds via &BOUNDS.
  ;; Since version 0.14
  (pushnew :esrap.lookahead *features*)
  (pushnew :esrap.lookbehind *features*)
  ;; Since version 0.13
  (pushnew :esrap.expression-start-terminals *features*)
  ;; Since version 0.12
  (pushnew :esrap.function-terminals *features*)
  ;; Since version 0.11
  (pushnew :esrap.multiple-transforms *features*)
  ;; Since version 0.10
  (pushnew :esrap.can-handle-left-recursion *features*)

  ;; For consistency with examples which contain (require :esrap).
  (provide :esrap))

(defsystem :esrap/tests
  :description "Tests for ESRAP."
  :author      ("Nikodemus Siivola <nikodemus@random-state.net>"
                "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>")
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :licence     "MIT"
  :depends-on  (:esrap
                (:version :fiveam "1.3"))
  :serial      t
  :components  ((:module "examples"
                 :components ((:file "left-recursion")
                              (:file "function-terminals")))

                (:module "test"
                 :serial t
                 :components ((:file "package")
                              (:file "util")
                              (:file "tests")
                              (:file "examples")
                              (:file "readme")))))

(defmethod perform ((operation test-op)
                    (system    (eql (find-system :esrap/tests))))
  (funcall (intern "RUN-TESTS" :esrap-tests)))
