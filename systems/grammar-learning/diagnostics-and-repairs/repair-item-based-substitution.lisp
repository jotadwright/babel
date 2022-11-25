(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair from holistic+item-based to item-based cxn through substitution
;; Effectively combines two repairs in one:
;; 1. from holistic->item-based e.g. the ?x is what shape? + large gray object
;; 2. learns the substitution diff between two item-based cxns e.g. the ?x is what material? 
;; 3. creates new holistic cxns => material + shape learned
;; 4. runs holistic->item-based again to create an item-based cxn e.g. the ?x is what ?y?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass item-based->item-based--substitution (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))

(defmethod repair ((repair item-based->item-based--substitution)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (when (initial-node-p node)
    (let ((constructions-and-categorial-links (create-item-based-cxn-from-partial-holistic-analysis+similar-item-based-cxn--substitution problem node)))
      (when constructions-and-categorial-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-categorial-links)))))

(defun create-item-based-cxn-from-partial-holistic-analysis+similar-item-based-cxn--substitution (problem node)
  (do-create-item-based-cxn-from-partial-holistic-analysis+similar-item-based-cxn--substitution
   (form-constraints-with-variables
    (random-elt (get-data problem :utterances))
    (get-configuration (construction-inventory node) :de-render-mode))
   (meaning-predicates-with-variables
    (random-elt (get-data problem :meanings))
    (get-configuration (construction-inventory node) :meaning-representation-formalism))
   nil
   (construction-inventory node)
   node))
  
(defun do-create-item-based-cxn-from-partial-holistic-analysis+similar-item-based-cxn--substitution (form-constraints meaning parent-meaning cxn-inventory node)
  "Creates item-based construction around matching holistic constructions"
  (let* ((cxn-inventory (original-cxn-set cxn-inventory))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         ;; temp item-based cxn to diff with an existing one
         (cxns-and-links (do-create-item-based-cxn-from-partial-holistic-analysis form-constraints meaning parent-meaning (processing-cxn-inventory cxn-inventory) nil)))
    (when cxns-and-links
      (let* ((cxns-to-apply (first cxns-and-links))
             (intermediary-item-based-cxn (last-elt cxns-to-apply))
             (applied-holistic-cxns (remove intermediary-item-based-cxn cxns-to-apply)))
        (multiple-value-bind (non-overlapping-meaning-observation
                              non-overlapping-meaning-cxn
                              overlapping-meaning-observation
                              overlapping-meaning-cxn
                              non-overlapping-form-observation
                              non-overlapping-form-cxn
                              cxn)
            (select-item-based-cxn-for-making-item-based-cxn cxn-inventory intermediary-item-based-cxn meaning-representation-formalism) ;;todo: review args
          (when cxn
            ;; new holistic cxns based on substitution
            (let* ((cxns-and-links-holistic-part-observation (handle-potential-holistic-cxn non-overlapping-form-observation non-overlapping-meaning-observation overlapping-meaning-observation cxn-inventory))
                   (cxns-and-links-holistic-part-cxn (handle-potential-holistic-cxn non-overlapping-form-cxn non-overlapping-meaning-cxn overlapping-meaning-cxn cxn-inventory))
                   (temp-cxns-to-add (append
                                      (first cxns-and-links-holistic-part-observation)
                                      applied-holistic-cxns))
                   (temp-cats-to-add (append (mapcar #'extract-contributing-lex-class temp-cxns-to-add)
                                             (mappend #'get-all-conditional-unit-lex-classes temp-cxns-to-add)))
                   (temp-cxn-inventory (create-temp-cxn-inventory cxn-inventory :cxns-to-add temp-cxns-to-add :categories-to-add temp-cats-to-add))
                   ;; new item-based cxn
                   (item-based-cxn-and-links (do-create-item-based-cxn-from-partial-holistic-analysis form-constraints meaning parent-meaning (processing-cxn-inventory temp-cxn-inventory) nil))
                   ;; build result
                   (cxns-to-apply (first item-based-cxn-and-links))
                   (cat-links-to-add (append (second item-based-cxn-and-links)
                                             (second cxns-and-links-holistic-part-observation)
                                             (second cxns-and-links-holistic-part-cxn)
                                             (list (loop for link in (second item-based-cxn-and-links)
                                                         for holistic-cxn-lc = (car link)
                                                         for slot-lc = (cdr link)
                                                         when (equal (fifth cxns-and-links-holistic-part-observation) holistic-cxn-lc)
                                                         return (cons (fifth cxns-and-links-holistic-part-cxn) slot-lc)))))
                   (cxns-to-consolidate (append (third item-based-cxn-and-links)
                                                (third cxns-and-links-holistic-part-observation)
                                                (third cxns-and-links-holistic-part-cxn)
                                                (first cxns-and-links-holistic-part-cxn)))
                                                
                   (cats-to-add (append (fourth item-based-cxn-and-links)
                                        (fourth cxns-and-links-holistic-part-observation)
                                        (fourth cxns-and-links-holistic-part-cxn))))
              ;; overwrite repair status attribute

              (when item-based-cxn-and-links
                (setf (attr-val (last-elt cxns-to-apply) :repair) 'item-based->item-based--substitution)
                (apply-fix
                 cxns-to-apply
                 cat-links-to-add
                 cxns-to-consolidate
                 cats-to-add
                 (fifth item-based-cxn-and-links)
                 t
                 node
                 ))))))))) ;todo, debug the else case!
                   
                   
