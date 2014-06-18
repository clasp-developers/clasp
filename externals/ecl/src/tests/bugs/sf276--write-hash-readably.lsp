; https://sourceforge.net/p/ecls/bugs/276/


(print 
  (write-to-string (make-hash-table)
                   :readably t))

(deftest sf-276-write-hash-readable
         (hash-table-count
         (read-from-string 
           (write-to-string (make-hash-table)
                            :readably t)))
         0)
