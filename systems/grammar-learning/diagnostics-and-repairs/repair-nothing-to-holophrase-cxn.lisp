(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Add holophrase construction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass nothing->holistic (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))
  
(defmethod repair ((repair nothing->holistic)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new holophrase construction."
  (when (and (initial-node-p node)
             (form-constraints-with-variables (random-elt (get-data problem :utterances))
                                              (get-configuration (construction-inventory node) :de-render-mode)))
    (make-instance 'fcg::cxn-fix
                   :repair repair
                   :problem problem
                   :restart-data (create-holistic-cxn problem node))))
  
(defmethod repair ((repair nothing->holistic)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new holophrase construction."
  (when (and (initial-node-p node)
             (form-constraints-with-variables (random-elt (get-data problem :utterances))
                                              (get-configuration (construction-inventory node) :de-render-mode)))
    (make-instance 'fcg::cxn-fix
                   :repair repair
                   :problem problem
                   :restart-data (create-holistic-cxn problem node))))

(defun create-holistic-cxn (problem node)
  (do-create-holistic-cxn
   (random-elt (get-data problem :utterances))
   (random-elt (get-data problem :meanings))
   (construction-inventory node)))


(defun do-create-holistic-cxn (utterance gold-standard-meaning cxn-inventory)
  (let* ((cxn-inventory (original-cxn-set cxn-inventory))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         
         (meaning (meaning-predicates-with-variables gold-standard-meaning meaning-representation-formalism))
         (cxn-name (make-cxn-name utterance cxn-inventory :add-numeric-tail t))
         (cxn-name-holistic-cxn-apply-last (intern (concatenate 'string (symbol-name cxn-name) "-APPLY-LAST")))
         (cxn-name-holistic-cxn-apply-first (intern (concatenate 'string (symbol-name cxn-name) "-APPLY-FIRST")))
         (form-constraints (form-constraints-with-variables utterance (get-configuration cxn-inventory :de-render-mode)))
         (boundaries-holistic-cxn (get-boundary-units form-constraints))
         (leftmost-unit-holistic-cxn (first boundaries-holistic-cxn))
         (rightmost-unit-holistic-cxn (second boundaries-holistic-cxn))
         (args-holistic-cxn (extract-args-from-meaning-networks meaning nil meaning-representation-formalism))
         (lex-class-holistic-cxn (make-lex-class cxn-name :trim-cxn-suffix t))
         ;; take the last element of the form constraints (the last word) and use it for hashing
         (hash-string (loop for fc in form-constraints
                            when (equalp (first fc) 'string)
                            collect (third fc) into hash-strings
                            finally (return (last-elt hash-strings))))
    
         (holistic-cxn-apply-first (second (multiple-value-list  (eval
                                                                  `(def-fcg-cxn ,cxn-name-holistic-cxn-apply-first
                                                                                ((?holistic-unit
                                                                                  (args ,args-holistic-cxn)
                                                                                  (syn-cat (phrase-type holistic)
                                                                                           (lex-class ,lex-class-holistic-cxn))
                                                                                  (boundaries
                                                                                   (left ,leftmost-unit-holistic-cxn)
                                                                                   (right ,rightmost-unit-holistic-cxn)))
                                                                                 <-
                                                                                 (?holistic-unit
                                                                                  (HASH meaning ,meaning)
                                                                                  --
                                                                                  (HASH form ,form-constraints)))
                                                                                :attributes (:label fcg::routine
                                                                                             :cxn-type holistic
                                                                                             :bare-cxn-name ,cxn-name
                                                                                             :repair nothing->holistic
                                                                                             :meaning ,(fourth (find 'bind meaning :key #'first))
                                                                                             :string ,hash-string)
                                                                                :cxn-inventory ,(copy-object cxn-inventory))))))
         (holistic-cxn-apply-last (second (multiple-value-list  (eval
                                                                  `(def-fcg-cxn ,cxn-name-holistic-cxn-apply-last
                                                                (
                                                                 <-
                                                                 (?holistic-unit
                                                                  (HASH meaning ,meaning)
                                                                  (args ,args-holistic-cxn)
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-holistic-cxn))
                                                                  (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn)
                                                                   (right ,rightmost-unit-holistic-cxn))
                                                                  --
                                                                  (HASH form ,form-constraints)
                                                                  (args ,args-holistic-cxn)
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-holistic-cxn))
                                                                  (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn)
                                                                   (right ,rightmost-unit-holistic-cxn))))
                                                                :attributes (:label fcg::meta-only
                                                                             :cxn-type holistic
                                                                             :bare-cxn-name ,cxn-name
                                                                             :repair nothing->holistic
                                                                             :meaning ,(fourth (find 'bind meaning :key #'first))
                                                                             :string ,hash-string)
                                                                :cxn-inventory ,(copy-object cxn-inventory))))))
         (cxns-to-apply (list holistic-cxn-apply-first))
         (cxns-to-consolidate (list holistic-cxn-apply-last))
         (cats-to-add (list lex-class-holistic-cxn)))

         (list
           cxns-to-apply
           nil
           cxns-to-consolidate
           cats-to-add)
         ))
#|
(defun create-holophrase-cxn (problem node)
  "Creates a holophrase-cxn."
  (let* ((processing-cxn-inventory (construction-inventory node))
         (cxn-inventory (original-cxn-set processing-cxn-inventory))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         (utterance (random-elt (get-data problem :utterances)))
         (meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings)) meaning-representation-formalism))
         (cxn-name (make-cxn-name utterance cxn-inventory :add-numeric-tail t))
         (form-constraints (form-constraints-with-variables utterance (get-configuration cxn-inventory :de-render-mode)))
         (boundaries-holophrase-cxn (get-boundary-units form-constraints))
         (leftmost-unit-holophrase-cxn (first boundaries-holophrase-cxn))
         (rightmost-unit-holophrase-cxn (second boundaries-holophrase-cxn))
         (args-holophrase-cxn (extract-args-from-meaning-networks meaning nil meaning-representation-formalism))
         ;; take the last element of the form constraints (the last word) and use it for hashing
         (hash-string (loop for fc in form-constraints
                            when (equalp (first fc) 'string)
                              collect (third fc) into hash-strings
                            finally (return (last-elt hash-strings)))))
    (assert hash-string)
    (second (multiple-value-list  (eval
                                   `(def-fcg-cxn ,cxn-name
                                                 ((?holophrase-unit
                                                   (syn-cat (phrase-type holophrase))
                                                   (args ,args-holophrase-cxn)
                                                   (boundaries
                                                    (left ,leftmost-unit-holophrase-cxn)
                                                    (right ,rightmost-unit-holophrase-cxn)))
                                                  <-
                                                  (?holophrase-unit
                                                   (HASH meaning ,meaning)
                                                   --
                                                   (HASH form ,form-constraints)))
                                                 :attributes (:label fcg::routine
                                                              :cxn-type holophrase
                                                              :repair nothing->holophrase
                                                              :string ,hash-string)
                                                 :cxn-inventory ,(copy-object cxn-inventory)))))))

|#
;; uses standard handle-fix
;; see meta dot handle-fix
