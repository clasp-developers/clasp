
(require :asdf)
(asdf:load-system "clasp-analyzer")

(clasp-analyzer:load-compilation-database "app-resources:build-databases;clasp_compile_commands.json")
 
(clasp-analyzer:lnew $tiny-test-search)

(setq $tiny-test-search (clasp-analyzer:lsel clasp-analyzer:$* ".*/cons\.cc"))

(clasp-analyzer:load-asts $tiny-test-search
           :arguments-adjuster-code (clasp-analyzer:build-arguments-adjuster))

(defparameter *test-matcher*
  '(:record-decl
    ;;        (:is-definition)
    ;;        (:is-template-instantiation)
    (:matches-name ".*GCInfo.*"))
  )
(clasp-analyzer:match-run
 *test-matcher*
 :limit 10
 ;;              :tag :point
;; :match-comments '( ".*mytest.*" )
 :the-code-match-callback
 #'(lambda ()
     (let* ((decl (mtag-node :whole))
            (args (cast:get-template-args decl))
            (arg (cast:template-argument-list-get args 0))
            (qtarg (cast:get-as-type arg))
            (tsty-new (cast:get-type-ptr-or-null qtarg))
            (key (record-key tsty-new))
            (classified (classify-ctype tsty-new))
            )
       (format t "MATCH: ------------------~%")
       (format t "        Start: ~a~%" (mtag-loc-start :whole))
       (format t "         Node: ~a~%" (mtag-node :whole))
       (format t "     type-of node: ~a~%" (type-of (mtag-node :whole)))
       (format t "         Name: ~a~%" (mtag-name :whole))
       (format t "          Arg: ~a~%" classified)
       (format t "          key: ~a~%" key)
       )))




