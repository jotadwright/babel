(in-package :propbank-english)

;; hashed-scored-labeled ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass cxn-supplier-propbank-english ()
  ((current-label 
    :initarg :current-label :accessor current-label
    :documentation "The current label that is tried")
   (remaining-labels
    :type list :initarg :remaining-labels :accessor remaining-labels
    :documentation "All labels that have not been tried yet")
   (all-constructions-of-current-label
    :type list :initarg :all-constructions-of-current-label
    :accessor all-constructions-of-current-label
    :documentation "All constructions that have the current label")
   (remaining-constructions
    :type list :initform nil :accessor remaining-constructions :initarg :remaining-constructions
    :documentation "A sublist of :all-constructions-of-current-label
                    that are still to try"))
  (:documentation "A construction pool that applies constructions of
                   different labels by a pre-specified order"))

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
         :all-constructions-of-current-label all-constructions-of-current-label)))))

(defun constructions-for-label-propbank (node label)
  "returns all constructions that of label 'label'"
  (cond ;; For lexical constructions
        ((or (equal label 'lexical-cxn)
             (equal label 'argm-leaf-cxn))
         (let ((constructions (loop for cxn in (loop for hash in (hash node (get-configuration node :hash-mode))
                                                     append (gethash hash (constructions-hash-table (construction-inventory node))))
                                    when (equal (attr-val cxn :label) label)
                                    collect cxn)))
           (when (get-configuration node :shuffle-cxns-before-application)
             (setf constructions (shuffle constructions)))
           (sort constructions #'(lambda (cxn-1 cxn-2) (>= (attr-val cxn-1 :frequency) (attr-val cxn-2 :frequency))))))

        ;; For constructions bound to lex-categories
        ((or (equal label 'argument-structure-cxn)
             (equal label 'argm-phrase-cxn))
         (let* ((lex-categories-node (lex-categories node))
                (neighbours (remove-duplicates (loop for lex-category in lex-categories-node
                                                     append (graph-utils::neighbors (type-hierarchies::graph (get-type-hierarchy (construction-inventory node))) lex-category
                                                                                    :return-ids? nil))))
                (constructions (loop for cxn in (if (equal label 'argument-structure-cxn)
                                                  (gethash nil (constructions-hash-table (construction-inventory node)))
                                                  (append
                                                   (gethash nil (constructions-hash-table (construction-inventory node)))
                                                   (loop for hash in (hash node (get-configuration node :hash-mode)) ;;argm-phrase cxns are hashed
                                                        append (gethash hash (constructions-hash-table (construction-inventory node))))))
                                     for cxn-category = (attr-val cxn :gram-category)
                                     when (and (member cxn-category neighbours)
                                               (equal (attr-val cxn :label) label))
                                     collect cxn)))
           (when (get-configuration node :shuffle-cxns-before-application)
             (setf constructions (shuffle constructions)))
           (sort constructions #'(lambda (cxn-1 cxn-2)
                                   (cond ((> (attr-val cxn-1 :score) (attr-val cxn-2 :score)))
                                         ((< (attr-val cxn-1 :score) (attr-val cxn-2 :score))
                                          nil)
                                         ((>= (find-highest-edge-weight lex-categories-node cxn-1 node)
                                              (find-highest-edge-weight lex-categories-node cxn-2 node)))
                                         (t
                                          nil))))))
        ;; Word sense constructions
        ((equal label 'word-sense-cxn)
         (let* ((gram-categories-node (gram-categories node))
                (neighbours (remove-duplicates (loop for gram-category in gram-categories-node
                                                     append (graph-utils::neighbors (type-hierarchies::graph (get-type-hierarchy (construction-inventory node)))
                                                                                    gram-category
                                                                                    :return-ids? nil))))
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


(defun find-highest-edge-weight (category-list cxn node)
  (loop with graph = (graph-utils::graph (get-type-hierarchy (construction-inventory node)))
        with gram-category = (or (attr-val cxn :gram-category)
                                 (attr-val cxn :sense-category))
        for cat in category-list
        if (graph-utils:edge-exists? graph cat gram-category)
        maximize (graph-utils:edge-weight graph cat gram-category)))



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
             