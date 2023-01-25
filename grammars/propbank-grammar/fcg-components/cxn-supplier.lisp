(in-package :fcg)

(export '(cxn-supplier-cxn-sets-hashed-categorial-network :cxn-sets-hashed-categorial-network))
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;;
;; Construction suplier  ;;
;;                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constructions are hashed and divided in sets ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cxn-supplier-cxn-sets-hashed-categorial-network (cxn-supplier-cxn-sets)
  ()
  (:documentation "Construction supplier that combines hashing and construction sets."))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :cxn-sets-hashed-categorial-network)))
  "Creates an instance of the cxn-supplier and sets the cxn-sets for the applicable direction."
  (make-instance 'cxn-supplier-cxn-sets-hashed-categorial-network
                 :cxn-sets (get-configuration (construction-inventory (cip node))
                                              (if (eq (direction (cip node)) '->)
                                                :production-order :parse-order))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-cxn-sets-hashed-categorial-network) (node cip-node))
  "Returns all constructions that satisfy the hash of the node, are in
the current or a later set and are connected through the categorial
network."
  (if (initial-node-p node)
    (constructions-for-application-hashed-categorial-network-neigbours node)
    (loop with current-set = (attr-val (first (applied-constructions node)) :label)
          with current-set-index = (position (symbol-name current-set) (cxn-sets cxn-supplier) :test #'equalp :key #'symbol-name)
          with remaining-sets = (subseq (cxn-sets cxn-supplier) current-set-index)
          for construction in (constructions-for-application-hashed-categorial-network-neigbours node)
          when (member (symbol-name (attr-val construction :label)) remaining-sets :test #'equalp :key #'symbol-name)
          collect construction)))


(defun constructions-for-application-hashed-categorial-network-neigbours (node)
  "computes all constructions that could be applied for this node
   plus nil hashed constructions"
  (let* ((lex-categories-node (lex-categories node))
         (lex-cat-neighbours (remove-duplicates (loop for lex-category in lex-categories-node
                                                      append (neighbouring-categories lex-category
                                                                                      (original-cxn-set (construction-inventory node))))))
         (gram-categories-node (gram-categories node))
         (gram-cat-neighbours (remove-duplicates (loop for gram-category in gram-categories-node
                                                       append (neighbouring-categories gram-category
                                                                                       (original-cxn-set (construction-inventory node))))))

         (constructions (remove nil
                                (loop for cxn in (remove-duplicates
                                                  (append
                                                   (gethash nil (constructions-hash-table (construction-inventory node)))
                                                   (loop for hash in (hash node (get-configuration node :hash-mode))
                                                         append (gethash hash (constructions-hash-table (construction-inventory node))))))
                                      collect (cond ((attr-val cxn :gram-category)
                                                     (when (member (attr-val cxn :gram-category) lex-cat-neighbours)
                                                       cxn))
                                                    ((attr-val cxn :sense-category)
                                                     (when (member (attr-val cxn :sense-category) gram-cat-neighbours)
                                                       cxn))
                                                    (t
                                                     cxn))))))
    ;; shuffle if requested
    (when (get-configuration node :shuffle-cxns-before-application)
      (setq constructions 
            (shuffle constructions)))
    ;; sort if requested
    (when (get-configuration node :sort-cxns-before-application)
      (setq constructions
            (funcall (get-configuration node :sort-cxns-before-application)
                     constructions :node node)))
    ;; return constructions
    constructions))

#|(defun constructions-for-label-propbank (node label)
  "returns all constructions that of label 'label'"
  (cond ;; For lexical constructions
        
        ;; For constructions bound to lex-categories
        
        ;; Word sense constructions
        ((equal label 'word-sense-cxn)
         (let* ((gram-categories-node (gram-categories node))
                (neighbours (remove-duplicates (loop for gram-category in gram-categories-node
                                                     append (neighbouring-categories gram-category (original-cxn-set (construction-inventory node))))))
                (constructions (loop for cxn in (loop for hash in (hash node (get-configuration node :hash-mode))
                                                      append (gethash hash (constructions-hash-table (construction-inventory node))))
                                     for cxn-category = (attr-val cxn :sense-category)
                                     when (member cxn-category neighbours)
                                     collect cxn)))
           (when (get-configuration node :shuffle-cxns-before-application)
             (setf constructions (shuffle constructions)))
           (sort constructions #'(lambda (cxn-1 cxn-2)
                                   (cond ((> (attr-val cxn-1 :score) (attr-val cxn-2 :score)))
                                         ((< (attr-val cxn-1 :score) (attr-val cxn-2 :score))
                                          nil)
                                         ((>= (find-highest-edge-weight gram-categories-node cxn-1 node)
                                              (find-highest-edge-weight gram-categories-node cxn-2 node)))
                                         (t
                                          nil))))))))

 
(defmethod next-cxn ((cxn-supplier cxn-supplier-propbank-english) (node cip-node))
  (cond ((remaining-constructions cxn-supplier)
         ;; there are remaining constructions. just return the next one
         (pop (remaining-constructions cxn-supplier)))
        ((loop for child in (children node)
               thereis (and (cxn-applied child)
                            (not (find 'double-role-assignment (statuses child)))))
         ;; when the node already has children where cxn application succeeded,
         ;;  then we don't move to the next label
         nil)
        ((remaining-labels cxn-supplier)
         ;; go to the next label
         (setf (current-label cxn-supplier) (car (remaining-labels cxn-supplier)))
         (setf (remaining-labels cxn-supplier) (cdr (remaining-labels cxn-supplier)))
         (setf (all-constructions-of-current-label cxn-supplier)
               (constructions-for-label-propbank node (current-label cxn-supplier)))
         (setf (remaining-constructions cxn-supplier)
               (all-constructions-of-current-label cxn-supplier))
         (next-cxn cxn-supplier node))))


(defmethod create-cxn-supplier ((node cip-node) (mode (eql :propbank-english)))
  (let* ((parent (car (all-parents node))))
    (if parent
      ;; copy most of the stuff from the the pool of the parent
      (make-instance 
       'cxn-supplier-propbank-english
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :remaining-constructions (all-constructions-of-current-label (cxn-supplier parent))
       :all-constructions-of-current-label (all-constructions-of-current-label (cxn-supplier parent)))
      ;; there is no parent, start from first label
      (let* ((labels (get-configuration (construction-inventory (cip node))
                                       (if (eq (direction (cip node)) '->)
                                         :production-order :parse-order)))
             (all-constructions-of-current-label (constructions-for-label-propbank node (car labels))))
        (make-instance 
         'cxn-supplier-propbank-english
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :remaining-constructions all-constructions-of-current-label
         :all-constructions-of-current-label all-constructions-of-current-label))))) |#




(defun find-highest-edge-weight (category-list cxn node)
  (loop with cxn-category = (or (attr-val cxn :gram-category)
                                (attr-val cxn :sense-category))
        for cat in category-list
        if (link-exists-p cat cxn-category (original-cxn-set (construction-inventory node)))
        maximize (link-weight cat cxn-category (original-cxn-set (construction-inventory node)))))

(defun lex-categories (node)
  (loop for unit in (fcg-get-transient-unit-structure node)
        for lex-category = (unit-feature-value unit 'lex-category)
        when lex-category
        collect it))

(defun gram-categories (node)
  (loop for unit in (fcg-get-transient-unit-structure node)
        for gram-category = (unit-feature-value unit 'gram-category)
        when gram-category
        collect it))


             