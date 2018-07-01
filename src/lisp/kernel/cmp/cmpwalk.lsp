;;;
;;;    File: cmpwalk.lsp
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

;;
;; The code-walker-function must return a form or an altered form to continue compilation
;;
;; The macros that are invoked here must be the same macros that are used
;; in compile-file to set up the environment that the code needs to compile forms
;; as compile-file does.
(defun code-walk-using-bclasp (form env &key code-walker-function)
  "This is used in clos/method.lsp to code walk defmethod bodies"
  (and core:*use-cleavir-compiler* (error "The core:*use-cleavir-compiler* is set to T"))
  (let* ((module (llvm-create-module "code-walk-for-defmethod"))
	 (*code-walker* code-walker-function))
    (with-compilation-unit ()
      (with-module ( :module module
                     :optimize nil)
        (with-source-pathnames (:source-pathname "code-walk-using-bclasp")
          (with-debug-info-generator (:module module
                                      :pathname #P"/dev/null")
            (with-make-new-run-all (run-all-function)
              (with-literal-table
                  (let ((fn (literal:with-top-level-form (compile-thunk 'walk-thunk form env nil))))
                    ;; Do nothing (irc-intrinsic-call "ltvc_toplevel_funcall" (list fn))
                    )))))
        (llvm-sys::module-delete module)))))

(defun code-walk (form env &key code-walker-function (errorp t))
  "Walk the form using whichever compiler we are currently using
   within env and call the code-walker-function on each internal form.
code-walker-function takes two arguments (form env).
Set errorp nil if you don't want an error to be generated if the code can't be walked.
Returns T if walked, NIL if not (if errorp NIL)."
  (let ((*code-walking* t))
    (if (not core:*use-cleavir-compiler*)
        (code-walk-using-bclasp form env :code-walker-function code-walker-function)
        (let* ((clasp-cleavir-pkg (find-package :clasp-cleavir))
               (code-walk-using-cleavir-symbol (find-symbol "CODE-WALK-USING-CLEAVIR" clasp-cleavir-pkg)))
          (if (fboundp code-walk-using-cleavir-symbol)
              (funcall (fdefinition code-walk-using-cleavir-symbol) form env :code-walker-function code-walker-function)
              (if errorp
                  (error "Could not walk the code using code-walk-using-cleavir - the symbol is present - but it is not fbound")
                  nil))))
    t))

(export '(code-walk code-walk-using-bclasp))

