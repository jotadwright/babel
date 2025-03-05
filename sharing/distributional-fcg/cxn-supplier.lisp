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
         (argument-structure-constructions-per-level (loop for level in lex-categories-per-level
                                                           collect (loop for (lex-cat . similarity) in level
                                                                        for gram-neighbours =
                                                                          (graph-utils::neighbours-by-node-type
                                                                           (graph (categorial-network cxn-inventory))
                                                                           (graph-utils:lookup-node (graph (categorial-network cxn-inventory)) lex-cat)
                                                                           :node-type 'propbank-grammar::gram-category)
                                                                        append (mapcar #'(lambda (gram-cat)
                                                                                           (cons (first (gethash (graph-utils::lookup-node (graph (categorial-network cxn-inventory))
                                                                                                                                           gram-cat)
                                                                                                                 (constructions-hash-table cxn-inventory)))
                                                                                                 similarity)) gram-neighbours))))
         (word-sense-cxns (loop for lemma in (hash node (get-configuration node :hash-mode))
                                append (word-sense-cxns-for-lemma lemma cxn-inventory))))
    ;; fallback: if there was a match through vector similarity, then there was no hash match through the lemma, so just take all
    ;; todo: give the n closest in similarity, see expansion operator for threshold
    (unless lexical-cxns
      (setf lexical-cxns
            #|(loop for lemma in (hash node (get-configuration node :hash-mode))
                  for threshold = (get-configuration cxn-inventory :cosine-similarity-threshold)
                  for lemmas-and-lexical-cxns = (multiple-value-list (loop for cxn in (constructions-list cxn-inventory)
                                                                when (attr-val cxn :lex-category)
                                                                  collect (attr-val cxn :token) into lemmas
                                                                  and collect cxn into cxns
                                                                  finally (return (values lemmas cxns))))
                  for lemmas-above-threshold = (remove-if #'(lambda (x) (> x threshold)) (tokens-sorted-by-similarity lemma (first lemmas-and-lexical-cxns)) :key #'cdr)
                  collect (loop with lexical-cxns = (second lemmas-and-lexical-cxns)
                                for lemma-and-cosine in lemmas-above-threshold
                                for cxn = (find (first lemma-and-cosine) lexical-cxns :key #'(lambda (x) (attr-val x :token)))
                                collect (cons cxn (second lemma-and-cosine)))
                  )|#
            (loop for cxn in (constructions-list cxn-inventory)
                  when (attr-val cxn :lex-category)
                    collect (cons cxn 0))))
    (unless word-sense-cxns
      (setf word-sense-cxns (loop for cxn in (constructions-list cxn-inventory)
                               when (attr-val cxn :sense-category)
                                 collect (cons cxn 0))))
    (cons-if (append lexical-cxns word-sense-cxns (first argument-structure-constructions-per-level))
             (rest argument-structure-constructions-per-level))))
         

                    

(defun lex-categories-per-level (lex-categories cxn-inventory)
  (loop for top-level-lex-category in lex-categories
        for similar-lex-categories = (gethash top-level-lex-category (graph-utils::node-similarities (graph (categorial-network cxn-inventory))))
        collect similar-lex-categories
          into levels-per-category
        finally (return (group-by-position levels-per-category))))

(defun group-by-position (list-of-lists)
  (loop for i from 1 upto (length (first list-of-lists))
        collect (mapcar #'(lambda (list) (nth1 i list)) list-of-lists)))
