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
    :documentation "All levels that have not been tried yet"))
  (:documentation "Construction supplier that ranks cxns based on similarity from categorial links."))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :cascading-cosine-similarity)))
  "Creates an instance of the cxn-supplier and sets the cxn-sets for the applicable direction."
  (let ((constructions-per-level (constructions-per-level node)))
    (make-instance 'cxn-supplier-cascading-cosine-similarity
                   :current-level-cxns (first constructions-per-level)
                   :remaining-lower-levels (rest constructions-per-level))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-cascading-cosine-similarity) (node cip-node))
  "Returns all constructions that satisfy the hash of the node or are
direct neighbours of the categories present in the node."
  (let ((cxns-of-current-level (current-level-cxns cxn-supplier)))
    (setf (current-level-cxns cxn-supplier) (pop (remaining-lower-levels cxn-supplier)))
  cxns-of-current-level))


(defun lexical-cxns-for-lemma (lemma cxn-inventory)
  (loop for cxn in (gethash lemma (constructions-hash-table cxn-inventory))
        when (attr-val cxn :lex-category)
          collect cxn))

(defun argument-structure-cxns-for-gram-category (gram-category cxn-inventory)
  (gethash gram-category (constructions-hash-table cxn-inventory)))

(defun word-sense-cxns-for-lemma (lemma cxn-inventory)
  (loop for cxn in (gethash lemma (constructions-hash-table cxn-inventory))
        when  (attr-val cxn :sense-category)
          collect cxn))

(defun constructions-per-level (node)
  (let* ((cxn-inventory  (construction-inventory node))
         (lexical-cxns (loop for lemma in (hash node (get-configuration node :hash-mode))
                             append (lexical-cxns-for-lemma lemma cxn-inventory)))
         (lex-categories-per-level (lex-categories-per-level (lex-categories node) cxn-inventory))
         (argument-structure-constructions-per-level (loop for level in lex-categories-per-level
                                                           append (loop for lex-cat in level
                                                                         for gram-neighbours =
                                                                           (graph-utils::neighbours-by-node-type
                                                                            (graph (categorial-network cxn-inventory))
                                                                            (graph-utils:lookup-node (graph (categorial-network cxn-inventory)) lex-cat)
                                                                            :node-type 'propbank-grammar::gram-category)
                                                                         append (mapcar #'(lambda (gram-cat)
                                                                                            (gethash (graph-utils::lookup-node (graph (categorial-network cxn-inventory))
                                                                                                                               gram-cat)
                                                                                                     (constructions-hash-table cxn-inventory))) gram-neighbours))))
         (word-sense-cxns (loop for lemma in (hash node (get-configuration node :hash-mode))
                                append (word-sense-cxns-for-lemma lemma cxn-inventory))))
    (cons-if  (append lexical-cxns word-sense-cxns (first argument-structure-constructions-per-level))
              (rest argument-structure-constructions-per-level))))
         


(defun lex-categories-per-level (lex-categories cxn-inventory)
  (loop for top-level-lex-category in lex-categories
        for similar-lex-categories = (graph-utils::similar-nodes-weighted-cosine-same-node-type
                                      top-level-lex-category
                                      (graph (categorial-network cxn-inventory)))
        collect (mapcar #'first similar-lex-categories)
          into levels-per-category
        finally (return (group-by-position levels-per-category))))

(defun group-by-position (list-of-lists)
  (loop for i from 1 upto (length (first list-of-lists))
        collect (mapcar #'(lambda (list) (nth1 i list)) list-of-lists)))




             