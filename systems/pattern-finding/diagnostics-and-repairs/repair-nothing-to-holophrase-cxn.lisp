(in-package :pattern-finding)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair nothing to holistic ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass nothing->holistic (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))


(defmethod repair ((repair nothing->holistic)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new holophrase construction."
  (when (and (initial-node-p node)
             (get-data problem :utterance))
    (make-instance 'fcg::cxn-fix
                   :repair repair
                   :problem problem
                   :restart-data (create-holistic-cxn problem node))))


(defun create-holistic-cxn (problem node)
  (do-create-holistic-cxn
   ;; form constraints
   (get-data problem :utterance)
   ;; meaning
   (get-data problem :meaning)
   ;; form args
   nil
   ;; meaning args
   nil
   ;; cxn inventory
   (construction-inventory node)
   ;; node
   node))


(defun do-create-holistic-cxn (form-constraints meaning form-args meaning-args cxn-inventory node)
  (let* ((cxn-inventory
          (original-cxn-set cxn-inventory))
         (meaning-representation-formalism
          (get-configuration cxn-inventory :meaning-representation-formalism))
         ;; make the cxn name
         (cxn-name
          (make-cxn-name form-constraints cxn-inventory :add-numeric-tail t))
         (cxn-name-holistic-cxn-apply-last
          (intern (upcase (format nil "~a-apply-last" cxn-name))))
         (cxn-name-holistic-cxn-apply-first
          (intern (upcase (format nil "~a-apply-first" cxn-name))))
         ;; find an identical existing holistic cxn
         (existing-routine-holistic-cxn
          (find-identical-holistic-cxn form-constraints meaning form-args meaning-args cxn-inventory))
         (existing-meta-holistic-cxn
          (when existing-routine-holistic-cxn
            (alter-ego-cxn existing-routine-holistic-cxn cxn-inventory)))
         ;; lex class
         (lex-class-holistic-cxn
          (if existing-routine-holistic-cxn
            (extract-lex-class-holistic-cxn existing-routine-holistic-cxn)
            (make-lex-class cxn-name :trim-cxn-suffix t)))
         ;; hash keys
         (hash-string
          (unless existing-routine-holistic-cxn
            (form-predicates->hash-string form-constraints)))
         (hash-meaning
          (unless existing-routine-holistic-cxn
            (meaning-predicates->hash-meaning meaning meaning-representation-formalism)))
         (cxn-inventory-copy
          (unless existing-routine-holistic-cxn
            (copy-object cxn-inventory)))
         (holistic-cxn-apply-first
          (or existing-routine-holistic-cxn
              (second
               (multiple-value-list
                (eval
                 `(def-fcg-cxn ,cxn-name-holistic-cxn-apply-first
                               ((?holistic-unit
                                 (form-args ,form-args)
                                 (meaning-args ,meaning-args)
                                 (syn-cat (phrase-type holistic)
                                          (lex-class ,lex-class-holistic-cxn)))
                                <-
                                (?holistic-unit
                                 (HASH meaning ,meaning)
                                 --
                                 (HASH form ,form-constraints)))
                               :attributes (:label fcg::routine
                                            :cxn-type holistic
                                            :is-holophrase ,(and node (get-configuration cxn-inventory :mark-holophrases))
                                            :bare-cxn-name ,cxn-name
                                            :repair nothing->holistic
                                            :meaning ,hash-meaning
                                            :string ,hash-string)
                               :score ,(get-configuration cxn-inventory :initial-cxn-score)
                               :cxn-inventory ,cxn-inventory-copy))))))
         (holistic-cxn-apply-last
          (or existing-meta-holistic-cxn
              (second
               (multiple-value-list
                (eval
                 `(def-fcg-cxn ,cxn-name-holistic-cxn-apply-last
                               (
                                <-
                                (?holistic-unit
                                 (HASH meaning ,meaning)
                                 (meaning-args ,meaning-args)
                                 (syn-cat (phrase-type holistic)
                                          (lex-class ,lex-class-holistic-cxn))
                                 --
                                 (HASH form ,form-constraints)
                                 (form-args ,form-args)
                                 (syn-cat (phrase-type holistic)
                                          (lex-class ,lex-class-holistic-cxn))))
                               :attributes (:label fcg::meta-only
                                            :cxn-type holistic
                                            :is-holophrase ,(and node (get-configuration cxn-inventory :mark-holophrases))
                                            :bare-cxn-name ,cxn-name
                                            :repair nothing->holistic
                                            :meaning ,hash-meaning
                                            :string ,hash-string)
                               :score ,(get-configuration cxn-inventory :initial-cxn-score)
                               :cxn-inventory ,cxn-inventory-copy))))))
         (cxns-to-apply (list holistic-cxn-apply-first))
         (cxns-to-consolidate (list holistic-cxn-apply-last))
         (cats-to-add (list lex-class-holistic-cxn)))

    (apply-fix
     ;; form
     form-constraints
     ;; cxns to apply
     cxns-to-apply
     ;; categorial links
     nil
     ;; original cxns to consolidate
     cxns-to-consolidate
     ;; categories to add
     cats-to-add
     ;; top level category (?)
     lex-class-holistic-cxn
     ;; gold standard consulted p
     t
     ;; node
     node
     ;; repair name
     'nothing->holistic)))