;;;; fcg-utils.lisp

(in-package :fcg)

;;;; custom de-render for clevr-grammar
;;;; ----------------------------------
(defmethod de-render ((utterance string) (mode (eql :de-render-string-meets-precedes-within-3))
                      &key &allow-other-keys)
  "There were too many precedes constraints in the root, slowing down the processing for very large
   sentences. However, only a few cxns actually need 'precedes' and don't need 'long distance' precedes.
   To solve this, we constrain the generation of precedes constraints to only contain precedes within 3."
  (de-render (clevr-grammar::preprocess-utterance utterance) :de-render-string-meets-precedes-within-3))

(defmethod de-render ((utterance list) (mode (eql :de-render-string-meets-precedes-within-3))
                      &key &allow-other-keys)
  "There were too many precedes constraints in the root, slowing down the processing for very large
   sentences. However, only a few cxns actually need 'precedes' and don't need 'long distance' precedes.
   To solve this, we constrain the generation of precedes constraints to only contain precedes within 3."
  (let ((strings nil)
        (sequence nil)
	(constraints nil))
    (loop for string in utterance
          for i from 0
          for unit-name = (make-const string nil)
          do (progn
               (push unit-name sequence)
               (loop for prev in strings
                     for j from (1- (length strings)) downto 0
                     when (< (- i j) 4)
                     do (push `(precedes ,(second prev) ,unit-name) constraints))
               (push `(string ,unit-name ,string) strings)))
    (do ((left strings (rest left)))
	((null (cdr left)))
      (push `(meets ,(second (second left)) ,(second (first left)))
	    constraints))
    (make-instance 'coupled-feature-structure 
		   :left-pole `((root (meaning ())
                                      (sem-cat ())
                                      (form ,(cons (cons 'sequence (reverse sequence))
                                                   (append strings constraints)))
                                      (syn-cat ())))
		   :right-pole '((root)))))

;;;; custom cxn-supplier for clevr-grammar
;;;; -------------------------------------
(defclass cxn-supplier-with-ordered-labels-hashed
          (cxn-supplier-with-ordered-labels)
  ()
  (:documentation
   "A construction pool that applies constructions of
    different labels by a pre-specified order and supports hashing"))

(defun all-constructions-of-label-hashed (node label)
  "returns all constructions of label 'label'"
  (loop for cxn in (constructions-for-application-hashed node)
        for cxn-label = (attr-val cxn :label)
        when (or (and (symbolp cxn-label)
                      (string= (symbol-name label)
                               (symbol-name cxn-label)))
                 (and (listp cxn-label)
                      (member (symbol-name label)
                              (mapcar #'symbol-name cxn-label)
                              :test #'string=)))
        collect cxn))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :ordered-by-label-hashed)))
  (let ((parent (car (all-parents node))))
    (if parent
      ;; copy most of the stuff from the the pool of the parent
      ;; recompute the hash-compatible cxns (with label) every time!
      (make-instance 
       'cxn-supplier-with-ordered-labels-hashed
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :all-constructions-of-current-label 
       (all-constructions-of-label-hashed node (current-label (cxn-supplier parent))))
      ;; there is no parent, start from first label
      (let ((labels (get-configuration (construction-inventory (cip node))
                                       (if (eq (direction (cip node)) '->)
                                         :production-order :parse-order))))
        (make-instance 
         'cxn-supplier-with-ordered-labels-hashed
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :all-constructions-of-current-label
         (all-constructions-of-label-hashed node (car labels)))))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-with-ordered-labels-hashed)
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