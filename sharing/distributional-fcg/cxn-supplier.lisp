(in-package :fcg)

(export '(cxn-supplier-cxn-sets-hashed-categorial-network sort-cxns-by-frequency-and-categorial-edge-weight))
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;;
;; Construction suplier  ;;
;;                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Constructions are hashed and make use of links in the categorial network ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cxn-supplier-cascading-cosine-similarity (cxn-supplier-cxn-sets)
  ((current-level-cxns
    :initarg :current-level-cxns :accessor current-level-cxns
    :documentation "The current level that is tried")
   (remaining-lower-levels
    :type list :initarg :remaining-lower-levels :accessor remaining-lower-levels :initform nil
    :documentation "All levels that have not been tried yet")
   (cxns-all-levels
    :type list :initarg :cxns-all-levels :accessor cxns-all-levels :initform nil
    :documentation "Current level"))
  (:documentation "Construction supplier that ranks cxns based on similarity from categorial links."))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :cascading-cosine-similarity)))
  "Creates an instance of the cxn-supplier and sets the cxn-sets for the applicable direction."
  (let ((constructions-per-level (constructions-per-level node)))
    (make-instance 'cxn-supplier-cascading-cosine-similarity
                   :current-level-cxns (mapcar #'car (first constructions-per-level))
                   :remaining-lower-levels (loop for level in (rest constructions-per-level)
                                                   collect (mapcar #'car level))
                   :cxns-all-levels (loop for level in constructions-per-level append level))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-cascading-cosine-similarity) (node cip-node))
  "Returns all constructions that satisfy the hash of the node or are
direct neighbours of the categories present in the node."
  (let ((cxns-of-current-level (current-level-cxns cxn-supplier)))
    (setf (current-level-cxns cxn-supplier) (pop (remaining-lower-levels cxn-supplier)))
  cxns-of-current-level))


(defun lexical-cxns-for-lemma (lemma cxn-inventory)
  (loop for cxn in (gethash lemma (constructions-hash-table cxn-inventory))
        when (attr-val cxn :lex-category)
          collect (cons cxn 1)))

(defun argument-structure-cxns-for-gram-category (gram-category cxn-inventory)
  (gethash gram-category (constructions-hash-table cxn-inventory)))

(defun word-sense-cxns-for-lemma (lemma cxn-inventory)
  (loop for cxn in (gethash lemma (constructions-hash-table cxn-inventory))
        when (attr-val cxn :sense-category)
          collect (cons cxn 1)))



(defun constructions-per-level (node)
  (let* ((cxn-inventory (construction-inventory node))
         (lexical-cxns (loop for lemma in (hash node (get-configuration node :hash-mode))
                             append (lexical-cxns-for-lemma lemma cxn-inventory)))
         (lex-categories-per-level (lex-categories-per-level (lex-categories node) cxn-inventory))
         (lex-cat-neighbours (remove-duplicates (loop for lex-category in (lex-categories node)
                                                      append (neighbouring-categories lex-category
                                                                                      (original-cxn-set (construction-inventory node))))))
         (gram-cat-neighbours (remove-duplicates (loop for gram-category in (gram-categories node)
                                                       append (neighbouring-categories gram-category
                                                                                       (original-cxn-set (construction-inventory node))))))
         (argument-structure-constructions-per-level (loop for level in lex-categories-per-level
                                                           collect (loop for (lex-cat . similarity) in level
                                                                         when (> similarity (get-configuration (construction-inventory node) :graph-cosine-similarity-threshold))
                                                                           
                                                                         append (mapcar #'(lambda (gram-cat)
                                                                                            (cons (first (gethash (graph-utils::lookup-node (graph (categorial-network cxn-inventory))
                                                                                                                                            gram-cat)
                                                                                                                  (constructions-hash-table cxn-inventory)))
                                                                                                  similarity))
                                                                                        (graph-utils::my-neighbours-by-node-type
                                                                                         (graph (categorial-network cxn-inventory))
                                                                                         (graph-utils:lookup-node (graph (categorial-network cxn-inventory)) lex-cat)
                                                                                         :node-type 'propbank-grammar::gram-category)))))
         (word-sense-cxns (loop for lemma in (hash node (get-configuration node :hash-mode))
                                append (word-sense-cxns-for-lemma lemma cxn-inventory))))
    ;; fallback: if there was a match through vector similarity, then there was no hash match through the lemma, so just take all
    ;; todo: give the n closest in similarity, see expansion operator for threshold
    (unless lexical-cxns
      (setf lexical-cxns
            #|(loop for lemma in (hash node :hash-lemma)
                  for threshold = (get-configuration cxn-inventory :cosine-similarity-threshold)
                  for lemmas-and-lexical-cxns = (multiple-value-list (loop for cxn in (constructions-list cxn-inventory)
                                                                           when (attr-val cxn :lex-category)
                                                                             collect (attr-val cxn :token) into lemmas
                                                                             and collect cxn into cxns
                                                                           finally (return (values lemmas cxns))))
                  for lemmas-above-threshold = (remove-if #'(lambda (x) (> x threshold))
                                                          (tokens-sorted-by-similarity (downcase (symbol-name lemma)) (first lemmas-and-lexical-cxns)) :key #'cdr)
                  append (loop with lexical-cxns = (second lemmas-and-lexical-cxns)
                                for lemma-and-cosine in lemmas-above-threshold
                                for cxn = (find (first lemma-and-cosine) lexical-cxns :key #'(lambda (x) (attr-val x :token)))
                                collect (cons cxn (cdr lemma-and-cosine)))
                    )|#
            (loop for cxn in (constructions-list cxn-inventory)
                  when (attr-val cxn :lex-category)
                    collect (cons cxn 0))))
    (unless word-sense-cxns
      (setf word-sense-cxns (loop for cxn in (constructions-list cxn-inventory)
                                  when
                                    (and 
                                     (attr-val cxn :sense-category)
                                     (member (attr-val cxn :sense-category) lex-cat-neighbours))
                                    collect (cons cxn 0))))
    (when word-sense-cxns
         (setf word-sense-cxns (sort word-sense-cxns #'> :key #'(lambda (x) (attr-val (first x) :score)))))
    ;(cons-if (append lexical-cxns word-sense-cxns (first argument-structure-constructions-per-level))
    ;         (rest argument-structure-constructions-per-level))
    (cons-if (append lexical-cxns word-sense-cxns (first argument-structure-constructions-per-level))
             (rest argument-structure-constructions-per-level))
    ))
         

                    

(defun lex-categories-per-level (lex-categories cxn-inventory)
  (loop for top-level-lex-category in lex-categories
        for similar-lex-categories = (when top-level-lex-category (get-similar-lex-categories top-level-lex-category (graph (categorial-network cxn-inventory))))
        if similar-lex-categories
          collect similar-lex-categories
            into levels-per-category
        finally (return (group-by-position levels-per-category))))

(defun group-by-position (list-of-lists)
  (loop for i from 1 upto (length (first list-of-lists))
        collect (mapcar #'(lambda (list) (nth1 i list)) list-of-lists)))



(defun get-similar-lex-categories (lex-category graph)
  (let ((similar-lex-categories (gethash lex-category (graph-utils::node-similarities graph))))
    (when (not similar-lex-categories)
       (setf similar-lex-categories (graph-utils::my-similar-nodes-weighted-cosine-same-node-type lex-category graph))
       (setf (gethash lex-category (graph-utils::node-similarities graph)) similar-lex-categories))
    similar-lex-categories))



(in-package :graph-utils)

;; todo: edgetype fixen!
(defmethod my-neighbors ((graph undirected-typed-graph) (node integer)
                      &key edge-type (return-ids? t))
  "Return a list of cons-cells of the neighbors of the node.
   The car is the edge type and the cdr is the id of the neighbor."
  (let ((neighbors nil))
      (if edge-type
          (find-neighbors (gethash edge-type (matrix graph)) edge-type)
          (maphash (lambda (etype matrix)
                     (push (hash-values matrix node) neighbors))
                   (matrix graph)))
      ;; return 
      (if return-ids?
          (nreverse (flatten neighbors))
          (mapcar (lambda (pair)
                    (lookup-node graph (cdr pair)))
                  (nreverse neighbors)))))

(defun my-similar-nodes-weighted-cosine-same-node-type (node graph)
  "loop through all nodes in graph, sort by weighted cosine similarity"
  (loop with node-type = (get-node-type (lookup-node graph node) graph)
        for other-node-id in (remove-duplicates (gethash node-type (node-types graph)))
        for other-node = (lookup-node graph other-node-id)
        for node-similarity = (my-weighted-graph-cosine-similarity other-node node graph)
        when (>= node-similarity 0)
          collect (cons other-node node-similarity)
            into similar-nodes
        finally (return (sort similar-nodes #'> :key #'cdr))))


(defun my-weighted-graph-cosine-similarity (node-1 node-2 graph)
  (let* ((node-id-1 (graph-utils:lookup-node graph node-1))
         (node-id-2 (graph-utils:lookup-node graph node-2))
         (neighbors-of-1 (remove-duplicates (graph-utils::my-neighbors graph node-id-1)))
         (neighbors-of-2 (remove-duplicates (graph-utils::my-neighbors graph node-id-2))))
    (when (and node-id-1 node-id-2)
      (let ((denominator (sqrt (* (reduce '+ (loop for neighbor-id in neighbors-of-1 ;; take the edge weight of all neighbouring edges
                                                   collect (expt (graph-utils:edge-weight graph neighbor-id node-id-1) 2))) ;; take the square of the weight, and sum it
                                  (reduce '+ (loop for neighbor-id in neighbors-of-2
                                                   collect (expt (graph-utils:edge-weight graph neighbor-id node-id-2) 2)))))))
        (if (> denominator 0)
          (/ (reduce '+ (loop for common-neighbour in (remove-duplicates (intersection neighbors-of-1 neighbors-of-2))
                              collect (* (graph-utils:edge-weight graph node-id-1 common-neighbour)
                                         (graph-utils:edge-weight graph node-id-2 common-neighbour))))
             denominator)
          0)))))

; original
#|(defmethod my-neighbours-by-node-type ((graph undirected-node-and-edge-typed-graph) (node integer)
                                       &key node-type &allow-other-keys)
  "Returns neigbours by node type."
  (let ((neighbours nil))
    (flet ((find-neighbours (matrix etype)
             (map-sarray-col (lambda (row-id value)
                               (when (and
                                      etype ;;edge type needs to be non-zero!!!!
                                      (equal (get-node-type row-id graph) node-type)
                                      (>= value 0)) ;; edge type value needs to be larger than 0
                                 (push row-id neighbours)))
                             matrix node)))
      (maphash (lambda (etype matrix)
                 (find-neighbours matrix etype))
               (matrix graph))
      neighbours)))|#

(defmethod my-neighbours-by-node-type ((graph undirected-node-and-edge-typed-graph) (node integer)
                                       &key node-type &allow-other-keys)
  "Returns neigbours by node type."
  (let ((neighbours nil))
      (maphash (lambda (etype matrix)
                 (loop for row-id in (hash-values matrix node)
                       when (equal (get-node-type row-id graph) node-type)
                         do (push row-id neighbours)))
               (matrix graph))
      (remove-duplicates neighbours)))


(defun hash-values (matrix node)
  (let ((ht (gethash node (matrix matrix))))
    (when ht
      (loop for node-id being the hash-keys of ht
        collect node-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :fcg)

(defclass cxn-supplier-all-lexical-and-link-for-arg-struct-and-sense (cxn-supplier-cxn-sets)
  ()
  (:documentation "Construction supplier that combines all lexical cxns and categorial links."))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :all-lex-and-categorial-network)))
  "Creates an instance of the cxn-supplier and sets the cxn-sets for the applicable direction."
  (make-instance 'cxn-supplier-all-lexical-and-link-for-arg-struct-and-sense))

(defmethod next-cxn ((cxn-supplier cxn-supplier-all-lexical-and-link-for-arg-struct-and-sense) (node cip-node))
  "Returns all constructions that satisfy the hash of the node or are
direct neighbours of the categories present in the node."
    (constructions-for-application-hashed-categorial-network-neigbours node))


(defun constructions-for-application-hashed-categorial-network-neigbours (node)
  "Computes all constructions that could be applied for this node
   based on the hash table and the constructions that are linked to
the node through the links in the categorial network."
  (let* ((cxn-inventory (construction-inventory node))
        #|(lexical-cxns (loop for cxn in (constructions-list cxn-inventory)
                             when (attr-val cxn :lex-category)
                               collect (cons cxn 0)))|#
         (lex-cat-neighbours (remove-duplicates (loop for lex-category in (lex-categories node)
                                                      append (neighbouring-categories lex-category
                                                                                      (original-cxn-set (construction-inventory node))))))
         (gram-cat-neighbours (remove-duplicates (loop for gram-category in (gram-categories node)
                                                       append (neighbouring-categories gram-category
                                                                                       (original-cxn-set (construction-inventory node))))))
        

         (constructions
          (remove nil (loop for cxn in (remove-duplicates (constructions-list cxn-inventory))
                            collect (cond ((attr-val cxn :gram-category)
                                           (when (member (attr-val cxn :gram-category) lex-cat-neighbours)
                                             cxn))
                                          ((attr-val cxn :sense-category)
                                           (when (and (member (attr-val cxn :sense-category) gram-cat-neighbours)
                                                      (member (attr-val cxn :sense-category) lex-cat-neighbours))
                                             cxn))
                                          ((attr-val cxn :lex-category)
                                           cxn))))))
    
    ;; return constructions
    constructions))