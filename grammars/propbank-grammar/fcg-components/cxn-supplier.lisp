(in-package :fcg)

(export '(cxn-supplier-cxn-sets-hashed-categorial-network sort-cxns))
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;;
;; Construction suplier  ;;
;;                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Constructions are hashed and make use of links in the categorial network ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cxn-supplier-hashed-categorial-network (cxn-supplier-cxn-sets)
  ()
  (:documentation "Construction supplier that combines hashing and categorial links."))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :hashed-categorial-network)))
  "Creates an instance of the cxn-supplier and sets the cxn-sets for the applicable direction."
  (make-instance 'cxn-supplier-hashed-categorial-network))

(defmethod next-cxn ((cxn-supplier cxn-supplier-hashed-categorial-network) (node cip-node))
  "Returns all constructions that satisfy the hash of the node or are
direct neighbours of the categories present in the node."
    (constructions-for-application-hashed-categorial-network-neigbours node))


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
                                (loop for cxn in ;(remove-duplicates
                                                  (append
                                                   (gethash nil (constructions-hash-table (construction-inventory node)))
                                                   (loop for hash in (hash node (get-configuration node :hash-mode))
                                                         append (gethash hash (constructions-hash-table (construction-inventory node)))))
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


(defun sort-cxns (constructions &key node)
  
  (sort constructions #'(lambda (cxn-1 cxn-2)
                          (cond ((> (attr-val cxn-1 :score) (attr-val cxn-2 :score)))
                                ((< (attr-val cxn-1 :score) (attr-val cxn-2 :score))
                                 nil)
                                ((>= (find-highest-edge-weight (lex-categories node) cxn-1 node)
                                     (find-highest-edge-weight (lex-categories node) cxn-2 node)))
                                ((>= (find-highest-edge-weight (gram-categories node) cxn-1 node)
                                     (find-highest-edge-weight (gram-categories node) cxn-2 node)))
                                (t
                                 nil)))))




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


             