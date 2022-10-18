(in-package :clos)

(defstruct (bc-node (:type vector) :named)
  name text
  (edges (make-array 4 :adjustable t :fill-pointer 0)))

(defstruct (bc-edge (:type vector) :named)
  name node-place)

(defstruct (bc-graph (:type vector) :named)
  (nodes (make-hash-table))
  links
  patches)

(defun do-graphviz-linearize (tree graph depth)
  (let ((node nil))
    (labels ((collect (tree &rest xs)
               (let ((seen (gethash tree (bc-graph-nodes graph))))
                 (unless seen
                   (let ((name (gensym)))
                     (setf node (make-bc-node :name name
                                              :text (format nil "~a-~a ~{ ~a~}"
                                                            (cond
                                                              ((symbolp tree)
                                                               tree)
                                                              (t (elt tree 0)))
                                                            (string name)
                                                            xs))
                           (gethash tree (bc-graph-nodes graph)) node)))))
             (wait (tree &optional (name (elt tree 0)))
               (let ((new-tail (list nil)))
                 (push (cons new-tail tree) (bc-graph-links graph))
                 (vector-push-extend (make-bc-edge :name name :node-place new-tail) (bc-node-edges node))))
             (next (tree name)
               (vector-push-extend (make-bc-edge :name name :node-place (list (do-graphviz-linearize tree graph (1+ depth))))
                                   (bc-node-edges node)))
             (cont ()
               (if (null (bc-graph-links graph))
                   ;; nothing more to do
                   (return-from do-graphviz-linearize node)
                   ;; go to the next tree
                   (destructuring-bind (patchpoint . subtree)
                       (pop (bc-graph-links graph))
                     (push (cons patchpoint subtree) (bc-graph-patches graph))
                     (next subtree "cont")))))
      (cond ((advance-p tree)
             (collect tree
               :count (advance-count tree))
             (next (advance-next tree) "next"))
            ((tag-test-p tree)
             (collect tree)
             (wait (elt (tag-test-tags tree) 0) "fixnum-tag")
             (wait (elt (tag-test-tags tree) 1) "cons-tag")
             (wait (elt (tag-test-tags tree) 2) "single-float-tag")
             (wait (elt (tag-test-tags tree) 3) "character-tag")
             (next (tag-test-default tree) "default"))
            ((stamp-read-p tree)
             (collect tree)
             (wait (stamp-read-c++ tree) "c++")
             (next (stamp-read-other tree) "other"))
            ((<-branch-p tree)
             (collect tree
               (<-branch-pivot tree))
             (wait (<-branch-left tree) "left")
             (next (<-branch-right tree) "right"))
            ((=-check-p tree)
             (collect tree
               (=-check-pivot tree))
             (next (=-check-next tree) "next"))
            ((range-check-p tree)
             (collect tree
               (range-check-min tree)
               (range-check-max tree))
             (next (range-check-next tree) "next"))
            ((eql-search-p tree)
             (loop for object across (eql-search-objects tree)
                   for next across (eql-search-nexts tree)
                   do (collect tree
                        object)
                      (wait next "next"))
             (next (eql-search-default tree) "default"))
            ((miss-p tree)
             (collect tree)
             (cont))
            ((optimized-slot-reader-p tree)
             (collect
                 (if (core:fixnump (optimized-slot-reader-index tree))
                     'optimized-slot-reader ; instance
                     'car)                  ; class
               (optimized-slot-reader-index tree)
               (optimized-slot-reader-slot-name tree))
             (cont))
            ((optimized-slot-writer-p tree)
             (collect
                 (if (core:fixnump (optimized-slot-writer-index tree))
                     'optimized-slot-writer ; instance
                     'rplaca)               ; class
               (optimized-slot-writer-index tree))
             (cont))
            ((effective-method-outcome-p tree)
             (collect tree
               (effective-method-outcome-function tree))
             (cont))
            (t (error "BUG: Unknown dtree: ~a" tree))))
    node))

(defun graphviz-linearize (tree)
  (let ((graph (make-bc-graph)))
    (do-graphviz-linearize tree graph 0)
    (loop for patch in (bc-graph-patches graph)
          do (destructuring-bind (place . subtree)
                 patch
               (let ((node (gethash subtree (bc-graph-nodes graph))))
                 (setf (car place) node))))
    graph))

(defun render-tree-graph (filename graph)
  (with-open-file (fout filename :direction :output :if-exists :supersede)
    (format fout "digraph {~%")
    (maphash (lambda (key node)
               (declare (ignore key))
               (format fout "~a [label=\"~a\"];~%" (string (bc-node-name node)) (bc-node-text node)))
             (bc-graph-nodes graph))
    (maphash (lambda (key node)
               (declare (ignore key))
               (loop for edge across (bc-node-edges node)
                     for to-node = (car (bc-edge-node-place edge))
                     for to-node-name = (cond
                                          ((symbolp to-node) to-node)
                                          (t (bc-node-name to-node)))
                     do (format fout "~a -> ~a [label=\"~a\"];~%"
                                (bc-node-name node)
                                to-node-name
                                (bc-edge-name edge))))
             (bc-graph-nodes graph))
    (format fout "}~%")))


