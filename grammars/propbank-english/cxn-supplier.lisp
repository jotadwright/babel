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
  (cond ((or (equal label 'lexical-cxn)
             (equal label 'word-sense-cxn))
         (let ((constructions (loop for cxn in (loop for hash in (hash node (get-configuration node :hash-mode))
                                                     append (gethash hash (constructions-hash-table (construction-inventory node))))
                                    for cxn-label = (attr-val cxn :label)
                                    when (or (and (symbolp cxn-label) (equalp (symbol-name label) (symbol-name cxn-label)))
                                             (and (listp cxn-label) (member label cxn-label)))
                                    collect cxn)))
           (when (get-configuration node :shuffle-cxns-before-application)
             (shuffle constructions))
           (sort constructions #'(lambda (cxn-1 cxn-2) (>= (attr-val cxn-1 :frequency) (attr-val cxn-2 :frequency))))))
        
        ((equal label 'argument-structure-cxn)
         (let* ((lex-categories-node (lex-categories node))
                (neighbours (remove-duplicates (loop for lex-category in lex-categories-node
                                                     append (graph-utils::neighbors (type-hierarchies::graph (get-type-hierarchy (construction-inventory node))) lex-category
                                                                                    :return-ids? t))))
                (constructions (loop for cxn in (gethash nil (constructions-hash-table (construction-inventory node)))
                                     for cxn-category = (attr-val cxn :gram-category)
                                     when (member cxn-category neighbours)
                                     collect cxn)))
           (when (get-configuration node :shuffle-cxns-before-application)
             (shuffle constructions))
           (sort constructions #'(lambda (cxn-1 cxn-2)
                                   (cond ((> (attr-val cxn-1 :score) (attr-val cxn-2 :score)))
                                         ((< (attr-val cxn-1 :score) (attr-val cxn-2 :score))
                                          nil)
                                         ((>= (attr-val cxn-1 :frequency) (attr-val cxn-2 :frequency))))))))))

(defun lex-categories (node)
  (loop for unit in (fcg-get-transient-unit-structure node)
        for lex-category = (unit-feature-value unit 'lex-category)
        when lex-category
        collect it))


(defmethod next-cxn ((cxn-supplier cxn-supplier-propbank-english) (node cip-node))
  (cond ((remaining-constructions cxn-supplier)
         ;; there are remaining constructions. just return the next one
         (pop (remaining-constructions cxn-supplier)))
        ((loop for child in (children node)
               thereis (cxn-applied child))
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
             