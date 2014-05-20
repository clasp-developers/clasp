
;; Predicates
;;

(defun record-decl-p (x)
  (eq (type-of x) 'clang-ast:cxxrecord-decl))


(defun has-name-p (x name)
  (string= (cast:get-name x) name))


;;
;; Subclassed RecursiveASTVisitor
;;


(defclass find-named-class-visitor (ast-tooling:ASTVisitor)
  ((ast-context :initarg :ast-context :accessor ast-context))
  )

(defgeneric visit (visitor node))

;;
;; This is the main VISIT method that gets called with every clang AST node irrespective of type (Decl/Stmt/Type)
;;
(defmethod visit ((self find-named-class-visitor) node)
  (if (and (record-decl-p node) (has-name-p node "T_O"))
      (cast:dump node))
  t)

(core:defvirtual ast-tooling:visit-stmt ((self find-named-class-visitor) stmt) (visit self stmt))

(core:defvirtual ast-tooling:visit-decl ((self find-named-class-visitor) decl) (visit self decl))

(core:defvirtual ast-tooling:visit-type ((self find-named-class-visitor) type) (visit self type))



(defclass find-named-class-consumer (ast-tooling:astconsumer)
  ((visitor :initarg :visitor :accessor visitor)))

(core:defvirtual ast-tooling:handle-translation-unit ((self find-named-class-consumer) context)
  (ast-tooling:traverse-decl (visitor self) (ast-tooling:get-translation-unit-decl context))
  )



(defclass find-named-class-action (ast-tooling:syntax-only-action)
  ((consumer :initarg :consumer :accessor consumer)))

(core:defvirtual ast-tooling:create-astconsumer ((self find-named-class-action) compiler infile)
  (setf (consumer self) (make-instance 'find-named-class-consumer
                                       :visitor (make-instance 'find-named-class-visitor
                                                               :ast-context (ast-tooling:get-astcontext compiler))))
  (consumer self))



(defclass action-factory (ast-tooling:frontend-action-factory)
  ((accumulated-actions :initform nil :accessor accumulated-actions)))

(core:defvirtual ast-tooling:create ((self action-factory))
  (let ((action (make-instance 'find-named-class-action)))
    (push action (accumulated-actions self))
    action))


(defun run-test-tool ()
  (let ((action (make-instance 'find-named-class-action)))
    (ast-tooling:run-tool-on-code action "
struct X {int y;};
struct Y {char z;};
int main(int argc, char* argv[])
{
    for (int i(0);i<10;++i) {
       int x = i+1;
    }
    int j(10);
    while (j>0) {
       j--;
    }
}" "dummy.cc"
)))




(defun run-tool (&key test)
  (let* ((db (ast-tooling:jsoncompilation-database-load-from-file "compile_commands.json"))
         (tool (ast-tooling:make-clang-tool db
                                            (if test
                                                (subseq (ast-tooling:get-all-files db) 0 1)
                                                (ast-tooling:get-all-files db))))
         (syntax-only-adjuster (ast-tooling:make-clang-syntax-only-adjuster))
         (strip-output-adjuster (ast-tooling:make-clang-strip-output-adjuster))
         (factory (make-instance 'action-factory)))
    (ast-tooling:clear-arguments-adjusters tool)
    (ast-tooling:append-arguments-adjuster tool syntax-only-adjuster)
    (ast-tooling:append-arguments-adjuster tool strip-output-adjuster)
    (ast-tooling:run tool factory)
    ))

(print "Use (run-test-tool *action*)")
(print "Use (run-tool [:test t]) - to run on entire compilation database")

