
;; https://sourceforge.net/p/ecls/bugs/282

(deftest sf282--mvb-not-evaled
         (assert
           (eq :ok
               (block nil
                      (tagbody
                        (return (multiple-value-bind () (go :fail) :bad))
                        :fail
                        (return :ok))))))
