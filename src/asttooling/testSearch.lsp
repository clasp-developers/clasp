
;;
;; Load the tooling code
;; Currently use load so we can edit/reload the code during development
;; Later just (require 'clang-tool)
;;
(load "src:lisp;clang-tool.lsp")

;;
;; Load the JSONCompilationDatabase
;; This will fill the global variable $* with a list of all the source files in the database
;;
(progn
  (load-compilation-database "app:Contents;Resources;buildDatabases;brcl_compile_commands.json")
  ;; 
  ;; Set up a subset of 10 source filenames in $TEST to search over interactively
  ;; You can set up any number of source filename lists to run matchers over
  ;;    for interactive matcher development
  (lclear $test)
  (ladd $test (subseq $* 0 10))

  ;;
  ;; Load the C++ ASTs for the filenames in $TEST
  (load-asts $test)
  )

#|
A demo ASTMatcher.
I'm looking for fields in class/structs where the class/struct does not inherit from StackBoundClass or GCObject
and the field has a type that contain smart_ptr's that would be stored on the heap
such as vector<XXX>  where XXX is a struct/class that contains smart_ptr's
The smart_ptr's in question will be Garbage Collected when I don't want them to be because
they won't be connected to the root 
|#
(progn
  (defparameter *heap-smart-ptr-matcher*
    '(:field-decl
      (:has-decl-context
       (:record-decl
        (:bind :outer-decl (:record-decl))
        (:unless (:any-of
                  (:is-derived-from (:matches-name ".*StackBoundClass.*"))
                  (:is-derived-from (:matches-name ".*GCObject.*"))))))
      (:has-type
       (:has-declaration
        (:class-template-specialization-decl
         (:bind :named-decl (:named-decl))
         (:has-any-template-argument
          (:refers-to-type
           (:has-declaration
            (:record-decl
             (:bind :recdecl (:record-decl))
             (:is-derived-from
              (:record-decl
               (:for-each
                (:field-decl
                 (:bind :leaf (:field-decl))
                 (:has-type
                  (:has-declaration
                   (:class-template-specialization-decl
                    (:matches-name ".*smart_ptr.*")
                    ))))))))))))))))

  ;;
  ;; Run the matcher on the currently loaded subset of ASTs
  ;;    Using just the loaded subset of ASTs allows the matching to be fast and
  ;;    enables interactive development of matchers.   Once a matcher is written
  ;;    it can be run on all source files using BATCH-MATCH-RUN (see below).
  ;;
  ;; Run code on each match, extracting info on the bound nodes using
  ;; mtag-xxx functions that take a node TAG that corresponds to a (:bind :TAG (NODE))
  ;; command in the matcher.
  ;; Print info on each match

  (match-count *heap-smart-ptr-matcher*
             :code #'(lambda () (format t "MATCH: ------------------~%~a~% :whole source-> ~a~%:leaf ~a~%~a~%"
                                        (mtag-loc-start :whole)
                                        (mtag-source :whole)
                                        (mtag-source :leaf)
                                        (list ":named-decl" (get-name (mtag-node :named-decl)))))
             :limit 10 ; This would limit the max number of matches processed to 10
             )
  )

;;
;; Run the matcher on source files one at a time - this allows us to run
;; matchers on lots of source files without having to load all of their ASTs into
;; memory at one time

(batch-match-run *heap-smart-ptr-matcher*
                 :filenames $*    ; $* is a global variable that contains a list of all source files
                 :code #'(lambda () (format t "MATCH: ------------------~%~a~% :whole source-> ~a~%:leaf ~a~%~a~%"
                                            (mtag-loc-start :whole)
                                            (mtag-source :whole)
                                            (mtag-source :leaf)
                                            (list ":named-decl" (get-name (mtag-node :named-decl)))))
                 )
