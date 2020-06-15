;; Copyright 2019 AI Lab, Vrije Universiteit Brussel - Sony CSL Paris

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================
(in-package :fcg)

(export '(constructions-for-application all-constructions-of-label))

(defun constructions-for-application (construction-inventory)
  (if (get-configuration construction-inventory :shuffle-cxns-before-application)
    (shuffle (copy-list (constructions construction-inventory)))
    (constructions construction-inventory)))

;; #########################################################
;; cxn-supplier-with-simple-queue
;; ---------------------------------------------------------

(export '(cxn-supplier-with-simple-queue))

(defclass cxn-supplier-with-simple-queue ()
  ((remaining-constructions 
    :type list :initarg :remaining-constructions :accessor remaining-constructions
    :documentation "A list of constructions that are still to try")))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :simple-queue)))
  (make-instance 'cxn-supplier-with-simple-queue
                 :remaining-constructions
                 (constructions-for-application (construction-inventory (cip node)))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-with-simple-queue) (node cip-node))
  (pop (remaining-constructions cxn-supplier)))

;; #########################################################
;; cxn-supplier-with-ordered-labels
;; ---------------------------------------------------------

(export '(cxn-supplier-with-ordered-labels
          current-label remaining-labels
          all-constructions-of-current-label
          remaining-constructions
          all-constructions-of-label
          all-tried-constructions))

(defclass cxn-supplier-with-ordered-labels ()
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
    :type list :initform nil :accessor remaining-constructions
    :documentation "A sublist of :all-constructions-of-current-label
                    that are still to try"))
  (:documentation "A construction pool that applies constructions of
                   different labels by a pre-specified order"))

(defmethod initialize-instance :after ((pool cxn-supplier-with-ordered-labels) &key)
  (setf (remaining-constructions pool) (all-constructions-of-current-label pool)))

(defun all-constructions-of-label (node label)
  "returns all constructions that of label 'label'"
  (loop for cxn in (constructions-for-application (construction-inventory (cip node)))
        for cxn-label = (attr-val cxn :label)
        when (or (and (symbolp cxn-label) (string= (symbol-name label) (symbol-name cxn-label)))
                 (and (listp cxn-label) (member label cxn-label)))
        collect cxn))

(defun all-tried-constructions (cxn-supplier-with-ordered-labels) 
  "returns all cxns before a particular cxn could apply"
  (set-difference (all-constructions-of-current-label
                   cxn-supplier-with-ordered-labels)
                  (remaining-constructions
                   cxn-supplier-with-ordered-labels)))
  
(require-configuration :production-order)

(require-configuration :parse-order)

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :ordered-by-label)))
  (let* ((parent (car (all-parents node))))
    (if parent
      ;; copy most of the stuff from the the pool of the parent
      (make-instance 
       'cxn-supplier-with-ordered-labels
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :all-constructions-of-current-label 
       (all-constructions-of-current-label (cxn-supplier parent)))
      ;; there is no parent, start from first label
      (let ((labels (get-configuration (construction-inventory (cip node))
                                       (if (eq (direction (cip node)) '->)
                                         :production-order :parse-order))))
        (make-instance 
         'cxn-supplier-with-ordered-labels
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :all-constructions-of-current-label
         (all-constructions-of-label node (car labels)))))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-with-ordered-labels) (node cip-node))
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
               (all-constructions-of-label node (current-label cxn-supplier)))
         (setf (remaining-constructions cxn-supplier)
               (all-constructions-of-current-label cxn-supplier))
         (next-cxn cxn-supplier node))))

(defclass cxn-supplier-ordered-by-label-and-score (cxn-supplier-with-ordered-labels)
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
    :type list :initform nil :accessor remaining-constructions
    :documentation "A sublist of :all-constructions-of-current-label
                    that are still to try"))
  (:documentation "A construction pool that applies constructions of
                   different labels by a pre-specified order"))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :ordered-by-label-and-score)))
  (let* ((parent (car (all-parents node))))
    (if parent
      ;; copy most of the stuff from the the pool of the parent
      (make-instance 
       'cxn-supplier-ordered-by-label-and-score
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :all-constructions-of-current-label (all-constructions-of-current-label (cxn-supplier parent)))
      ;; there is no parent, start from first label
      (let ((labels (get-configuration (construction-inventory (cip node))
                                       (if (eq (direction (cip node)) '->)
                                         :production-order :parse-order))))
        (make-instance 
         'cxn-supplier-ordered-by-label-and-score
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :all-constructions-of-current-label
         (all-constructions-of-label-by-score node (car labels)))))))

(defun all-constructions-of-label-by-score (node label)
  "returns all constructions that of label 'label'"
  (let ((list (copy-object
               (loop for cxn in (constructions-for-application (construction-inventory (cip node)))
                     for cxn-label = (attr-val cxn :label)
                     when (or (and (symbolp cxn-label) (equalp (symbol-name label) (symbol-name cxn-label)))
                              (and (listp cxn-label) (member label cxn-label))) ;;TODO can be in different packages!!!
                     collect cxn))))
    (if (eq (class-name (class-of (first (constructions-for-application (construction-inventory (cip node)))))) 'scored-construction)
      (sort list #'> :key #'score)
      (sort list #'> :key #'(lambda (x) (attr-val x :score))))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-ordered-by-label-and-score) (node cip-node))
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
               (all-constructions-of-label-by-score node (current-label cxn-supplier)))
         (setf (remaining-constructions cxn-supplier)
               (all-constructions-of-current-label cxn-supplier))
         (next-cxn cxn-supplier node))))
             


;; #########################################################
;; cxn-supplier-with-scores
;; ---------------------------------------------------------

(export '(cxn-supplier-with-scores))

(defclass cxn-supplier-with-scores ()
  ((remaining-constructions 
    :type list :initarg :remaining-constructions :accessor remaining-constructions
    :documentation "A list of constructions that are still to try")))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :scores)))
  (if (eq (class-name (class-of (first (constructions-for-application (construction-inventory (cip node)))))) 'scored-construction)
    (make-instance 'cxn-supplier-with-scores
                   :remaining-constructions
                   (sort (constructions-for-application (construction-inventory (cip node)))
                         #'> 
                         :key #'score))
    (make-instance 'cxn-supplier-with-scores
                   :remaining-constructions
                   (sort (constructions-for-application (construction-inventory (cip node)))
                         #'> 
                         :key #'(lambda (x) (attr-val x :score))))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-with-scores) (node cip-node))
  (pop (remaining-constructions cxn-supplier)))

;; #########################################################
;; cxn-supplier-with-hashed-simple-queue
;; ---------------------------------------------------------

(export '(cxn-supplier-with-hashed-simple-queue))

(defun constructions-for-application-hashed (node)
  "computes all constructions that could be applied for this node
   plus nil hashed constructions"
  (let ((constructions
         ;; get all constructions compatible
         ;; with the hashes of the node
         ;; append nil hashed constructions
         (remove-duplicates
          (append
           (loop
            for hash in (hash node (get-configuration node :hash-mode))
            append (gethash hash (constructions-hash-table (construction-inventory node))))
           (gethash nil (constructions-hash-table (construction-inventory node)))))))
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

(defclass cxn-supplier-with-hashed-simple-queue ()
  ((remaining-constructions
    :type list :initarg :remaining-constructions
    :accessor remaining-constructions
    :documentation "A list of constructions that are still to try")))

(defmethod create-cxn-supplier ((node cip-node)
                                (mode (eql :hashed-simple-queue)))
  (make-instance
   'cxn-supplier-with-simple-queue
   :remaining-constructions (constructions-for-application-hashed node)))

(defmethod next-cxn ((cxn-supplier cxn-supplier-with-hashed-simple-queue)
                     (node cip-node))
  (pop (remaining-constructions cxn-supplier)))


(defmethod hash ((construction construction)
                 (mode (eql :hash-word-entity-root-one-pole))
                 &key &allow-other-keys)
  "Returns the string and meaning from the attributes of the construction"
  (when (or (attr-val construction :string)
            (attr-val construction :meaning))
    (list (attr-val construction :string)
          (attr-val construction :meaning))))

(defmethod hash ((node cip-node)
                 (mode (eql :hash-word-entity-root-one-pole)) ;; For using hashed construction sets in the root.
                 &key &allow-other-keys)
  "Checks the root and returns entities (for IRL meanings) or predicates."
  (let ((transient-structure (car-resulting-cfs (cipn-car node))))
    (if (eq '<- (direction (cip node)))
      (let ((strings (extract-string (get-root (left-pole-structure transient-structure)))))
        (mapcar #'third strings))
      ;; In production return the meanings.
      (loop with meanings = (extract-meaning (get-root (left-pole-structure transient-structure)))
            for m in meanings
            ;; collect the "entity" or "predicate":
            collect (if (and (eq (first m) 'bind)
                             (fourth m))
                      (fourth m)
                      (first m))))))

;; #########################################################
;; cxn-supplier-with-ordered-labels
;; ---------------------------------------------------------

(export '(cxn-supplier-with-hash+ordered-labels))

(defclass cxn-supplier-with-hash+ordered-labels
          (cxn-supplier-with-ordered-labels)
  ()
  (:documentation
   "A construction pool that applies constructions of
    different labels by a pre-specified order and supports hashing"))

(defun all-constructions-of-label-hashed (node label)
  "returns all constructions of label 'label'"
  (loop for cxn in (constructions-for-application-hashed node)
        for cxn-label = (attr-val cxn :label)
        when (or (eq label cxn-label)
                 (and (listp cxn-label) (member label cxn-label)))
        collect cxn))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :hashed-ordered-by-label)))
  (let* ((parent (car (all-parents node))))
    (if parent
      ;; copy most of the stuff from the the pool of the parent
      (make-instance 
       'cxn-supplier-with-hash+ordered-labels
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :all-constructions-of-current-label 
       (all-constructions-of-current-label (cxn-supplier parent)))
      ;; there is no parent, start from first label
      (let ((labels (get-configuration (construction-inventory (cip node))
                                       (if (eq (direction (cip node)) '->)
                                         :production-order :parse-order))))
        (make-instance 
         'cxn-supplier-with-hash+ordered-labels
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :all-constructions-of-current-label
         (all-constructions-of-label-hashed node (car labels)))))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-with-hash+ordered-labels)
                     (node cip-node))
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
               (all-constructions-of-label-hashed node (current-label cxn-supplier)))
         (setf (remaining-constructions cxn-supplier)
               (all-constructions-of-current-label cxn-supplier))
         (next-cxn cxn-supplier node))))








;; #########################################################
;; cxn-supplier
;; ---------------------------------------------------------

(export '(cxn-supplier-with-hashed-simple-queue))

(defun all-cxns-except-incompatible-hashed-cxns (node)
  "computes all constructions that could be applied for this node
   plus nil hashed constructions"
  (let ((constructions
         ;; get all constructions compatible
         ;; with the hashes of the node
         ;; append nil hashed constructions
         (remove-duplicates
          (append
           (loop
            for hash in (hash node (get-configuration node :hash-mode))
            append (gethash hash (constructions-hash-table (construction-inventory node))))
           (gethash nil (constructions-hash-table (construction-inventory node)))))))
    ;; shuffle if requested
    (when (get-configuration node :shuffle-cxns-before-application)
      (setq constructions 
            (shuffle constructions)))
    ;; return constructions
    constructions))

(defclass cxn-supplier-all-cxns-except-incompatible-hashed-cxns ()
  ((remaining-constructions
    :type list :initarg :remaining-constructions
    :accessor remaining-constructions
    :documentation "A list of constructions that are still to try")))

(defmethod create-cxn-supplier ((node cip-node)
                                (mode (eql :all-cxns-except-incompatible-hashed-cxns)))
  (make-instance
   'cxn-supplier-all-cxns-except-incompatible-hashed-cxns
   :remaining-constructions (all-cxns-except-incompatible-hashed-cxns node)))

(defmethod next-cxn ((cxn-supplier cxn-supplier-all-cxns-except-incompatible-hashed-cxns)
                     (node cip-node))
  (let ((next-constructions (remaining-constructions cxn-supplier)))
    ;;now we need to remove the next-constructions from the list of remaining constructions
    (setf (remaining-constructions cxn-supplier) nil)
    ;;return next constructions:
    next-constructions))

(defmethod hash ((construction construction)
                 (mode (eql :hash-string-meaning-lex-id))
                 &key &allow-other-keys)
  "Returns the string and meaning from the attributes of the construction"
  (when (or (attr-val construction :string)
            (attr-val construction :meaning)
            (attr-val construction :lex-id))
    (remove-duplicates
     (remove nil (list (attr-val construction :string)
                       (attr-val construction :meaning)
                       (attr-val construction :lex-id))))))

(defmethod hash ((node cip-node)
                 (mode (eql :hash-string-meaning-lex-id)) ;; For using hashed construction sets in the root.
                 &key &allow-other-keys)
  "Checks the root and returns entities (for IRL meanings) or predicates."
  (let* ((units (fcg-get-transient-unit-structure node))
         (lex-ids (loop for unit in units
                        for lex-id = (unit-feature-value unit 'lex-id)
                        when lex-id collect it))
         (strings (mapcar #'third (extract-strings (list (get-root units)))))
         (meanings (loop for meaning in (extract-meaning (get-root units))
                         collect (if (and (= 4 (length meaning)) (eql 'bind (first meaning)))
                                     (fourth meaning)
                                     (first meaning)))))
    (if (eql (car-direction (cipn-car node)) '<-)
      (append strings lex-ids)
      (append meanings lex-ids))))


;; hashed-and-scored ;;
;;;;;;;;;;;;;;;;;;;;;;;


(defun constructions-for-application-hashed-and-scored (node)
  "computes all constructions that could be applied for this node
   plus nil hashed constructions"
  (let ((constructions
         ;; get all constructions compatible
         ;; with the hashes of the node
         ;; append nil hashed constructions
         (remove-duplicates
          (append
           (loop
            for hash in (hash node (get-configuration node :hash-mode))
            append (gethash hash (constructions-hash-table (construction-inventory node))))
           (gethash nil (constructions-hash-table (construction-inventory node)))))))
    ;; shuffle if requested
    (when (get-configuration node :shuffle-cxns-before-application)
      (setq constructions 
            (shuffle constructions)))
    ;; sort 
    (setq constructions
          (sort constructions #'> :key #'(lambda (cxn) (attr-val cxn :score))))
    ;; return constructions
    constructions))

(defclass cxn-supplier-hashed-and-scored ()
  ((remaining-constructions
    :type list :initarg :remaining-constructions
    :accessor remaining-constructions
    :documentation "A list of constructions that are still to try")))

(defmethod create-cxn-supplier ((node cip-node)
                                (mode (eql :hashed-and-scored)))
  (make-instance
   'cxn-supplier-hashed-and-scored
   :remaining-constructions (constructions-for-application-hashed-and-scored node)))

(defmethod next-cxn ((cxn-supplier cxn-supplier-hashed-and-scored)
                     (node cip-node))
  (pop (remaining-constructions cxn-supplier)))



;; hashed-scored-labeled ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass cxn-supplier-hashed-scored-labeled ()
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

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :hashed-scored-labeled)))
  (let* ((parent (car (all-parents node))))
    (if parent
      ;; copy most of the stuff from the the pool of the parent
      (make-instance 
       'cxn-supplier-hashed-scored-labeled
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :remaining-constructions (all-constructions-of-current-label (cxn-supplier parent))
       :all-constructions-of-current-label (all-constructions-of-current-label (cxn-supplier parent)))
      ;; there is no parent, start from first label
      (let* ((labels (get-configuration (construction-inventory (cip node))
                                       (if (eq (direction (cip node)) '->)
                                         :production-order :parse-order)))
             (all-constructions-of-current-label (all-constructions-of-label-by-hash-and-score node (car labels))))
        (make-instance 
         'cxn-supplier-hashed-scored-labeled
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :remaining-constructions all-constructions-of-current-label
         :all-constructions-of-current-label all-constructions-of-current-label)))))

(defun all-constructions-of-label-by-hash-and-score (node label)
  "returns all constructions that of label 'label'"
  (let ((constructions (loop for cxn in (remove-duplicates (append (loop
                                                                    for hash in (hash node (get-configuration node :hash-mode))
                                                                    append (gethash hash (constructions-hash-table (construction-inventory node))))
                                                                   (gethash nil (constructions-hash-table (construction-inventory node)))))
                             for cxn-label = (attr-val cxn :label)
                             when (or (and (symbolp cxn-label) (equalp (symbol-name label) (symbol-name cxn-label)))
                                      (and (listp cxn-label) (member label cxn-label)))
                             collect cxn)))
    (when (get-configuration node :shuffle-cxns-before-application)
      (setf constructions 
            (shuffle constructions)))
    ;; sort 
    (setf constructions
          (sort constructions #'(lambda (cxn-1 cxn-2)
                                  (cond ((> (attr-val cxn-1 :score) (attr-val cxn-2 :score)))
                                        ((< (attr-val cxn-1 :score) (attr-val cxn-2 :score))
                                         nil)
                                        ((>= (attr-val cxn-1 :frequency) (attr-val cxn-2 :frequency)))))))
    ;; return constructions
    constructions))

(defmethod next-cxn ((cxn-supplier cxn-supplier-hashed-scored-labeled) (node cip-node))
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
               (all-constructions-of-label-by-hash-and-score node (current-label cxn-supplier)))
         (setf (remaining-constructions cxn-supplier)
               (all-constructions-of-current-label cxn-supplier))
         (next-cxn cxn-supplier node))))
             