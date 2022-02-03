(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Holophrase Single Deletion  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass holophrase->item-based+lexical+holophrase--deletion (repair) 
  ((trigger :initform 'fcg::new-node))) ;; it's always fcg::new-node, we created a new node in the search process

(defmethod repair ((repair holophrase->item-based+lexical+holophrase--deletion)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction, holophrase and lexical cxn."
  (when (initial-node-p node)
    (let ((constructions-and-th-links (create-repair-cxns-holophrase-single-deletion problem node)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))

(defmethod repair ((repair holophrase->item-based+lexical+holophrase--deletion)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction, holophrase and lexical cxn."
  (when (initial-node-p node)
    (let ((constructions-and-th-links (create-repair-cxns-holophrase-single-deletion problem node)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))

(defun create-repair-cxns-holophrase-single-deletion (problem node) ;;node = cip node (transient struct, applied cxns, cxn-inventory, ..)
  "Creates item-based construction, a holophrase and a lexical construction
   based on an existing holophrase construction of which the form/meaning are a superset of the observed phrase.

   Example:
   - cxn-inventory: contains a holophrase for 'the red cube'
   - new observation: 'the cube'

   Result:
   - holophrase-cxn: the-cube-cxn
   - item based-cxn: the-X-cube-cxn
   - lexical-cxn: red-cxn

   Edge cases to test:
   - center-deletion: 'the red cube' => 'the cube'
   - right-deletion: 'the cube red' => 'the cube' (e.g. French) or 'the cat jumps' => 'the cat'
   - left deletion: 'two red cubes' => 'red cubes'
   "
  (let* ((initial-transient-structure (initial-transient-structure node))
         (cxn-inventory (original-cxn-set (construction-inventory node)))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         (gold-standard-meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings))
                                                                   meaning-representation-formalism))
         
         (utterance (random-elt (get-data problem :utterances))))
    (multiple-value-bind (superset-holophrase-cxn
                          non-overlapping-form
                          non-overlapping-meaning)
        (find-superset-holophrase-cxn initial-transient-structure cxn-inventory gold-standard-meaning utterance)

      (when superset-holophrase-cxn
        (let* ((overlapping-form
                (set-difference (extract-form-predicates superset-holophrase-cxn) non-overlapping-form :test #'equal))
               (overlapping-meaning
                (set-difference (extract-meaning-predicates superset-holophrase-cxn) non-overlapping-meaning :test #'equal))
               (existing-lex-cxn
                (find-cxn-by-form-and-meaning non-overlapping-form non-overlapping-meaning cxn-inventory))
               (lex-cxn-name
                (make-cxn-name non-overlapping-form cxn-inventory))
               (cxn-name-item-based-cxn
                (make-cxn-name overlapping-form cxn-inventory :add-cxn-suffix nil))
               (unit-name-lex-cxn
                (second (find 'string non-overlapping-form :key #'first)))
               ;; lex-class
               (lex-class-lex-cxn
                (if existing-lex-cxn
                  (lex-class-cxn existing-lex-cxn)
                  (intern (get-base-name unit-name-lex-cxn) :grammar-learning)))
               (lex-class-item-based-cxn
                (intern (string-downcase (symbol-name cxn-name-item-based-cxn)) :grammar-learning)) 
               ;; type hierachy links
               (th-link
                (cons lex-class-lex-cxn lex-class-item-based-cxn))
               ;; args: 
               (args-lex-cxn
               (extract-args-from-predicate (first non-overlapping-meaning) meaning-representation-formalism))

               (meaning
                (meaning-predicates-with-variables (random-elt (get-data problem :meanings))
                                                   meaning-representation-formalism))
               (cxn-name
                (make-cxn-name utterance cxn-inventory))
               (form-constraints
                (form-constraints-with-variables utterance (get-configuration cxn-inventory :de-render-mode)))
               (holophrase-cxn
                (second (multiple-value-list  (eval
                                               `(def-fcg-cxn ,cxn-name
                                                             ((?holophrase-unit
                                                               (syn-cat (phrase-type holophrase)))
                                                              <-
                                                              (?holophrase-unit
                                                               (HASH meaning ,meaning)
                                                               --
                                                               (HASH form ,form-constraints)))
                                                             :attributes (:cxn-type holophrase
                                                                          :repair holophrase->item-based+lexical+holophrase--deletion)
                                                             :cxn-inventory ,(copy-object cxn-inventory))))))

                 
               (lex-cxn
                (or existing-lex-cxn
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,lex-cxn-name
                                                                ((,unit-name-lex-cxn
                                                                  (args (,args-lex-cxn))
                                                                  (syn-cat (phrase-type lexical)
                                                                           (lex-class ,lex-class-lex-cxn)))
                                                                 <-
                                                                 (,unit-name-lex-cxn
                                                                  (HASH meaning ,non-overlapping-meaning)
                                                                  --
                                                                  (HASH form ,non-overlapping-form)))
                                                                :attributes (:cxn-type lexical
                                                                             :repair holophrase->item-based+lexical+holophrase--deletion
                                                                             :meaning ,(fourth (find 'bind non-overlapping-meaning :key #'first))
                                                                             :string ,(third (find 'string non-overlapping-form :key #'first)))
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))));; trick to get the cxn without adding it to the cxn-inventory: make a copy of the cxn-inventory, make the cxn, get it, then forget about the copy
               (item-based-cxn
                (second (multiple-value-list (eval
                                              `(def-fcg-cxn ,(add-cxn-suffix cxn-name-item-based-cxn)
                                                            ((?item-based-unit
                                                              (syn-cat (phrase-type item-based))
                                                              (subunits (,unit-name-lex-cxn)))
                                                             (,unit-name-lex-cxn
                                                              (syn-cat (lex-class ,lex-class-item-based-cxn)))
                                                             <-
                                                             (?item-based-unit
                                                              (HASH meaning ,overlapping-meaning)
                                                              --
                                                              (HASH form ,overlapping-form))
                                                             (,unit-name-lex-cxn
                                                              (args (,args-lex-cxn))
                                                              --))
                                                            :attributes (:cxn-type item-based
                                                                         :repair holophrase->item-based+lexical+holophrase--deletion
                                                                         :meaning ,(loop for predicate in overlapping-meaning
                                                                                         unless (or
                                                                                                 (equal (first predicate) 'get-context)
                                                                                                 (equal (first predicate) 'bind))
                                                                                         return (first predicate))
                                                                         :string ,(third (find 'string overlapping-form :key #'first)))
                                                            :cxn-inventory ,(copy-object cxn-inventory)))))))

          ;; return the holophrase-cxn, item-based and lexical cxns
          (list holophrase-cxn lex-cxn item-based-cxn th-link))))))
            
          

(defmethod handle-fix ((fix fcg::cxn-fix) (repair holophrase->item-based+lexical+holophrase--deletion) (problem problem) (node cip-node) &key &allow-other-keys) 
  "Apply the constructions provided by fix to the result of the node and return the construction-application-result"
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications ;; avoid notifications in web interace
    (let* ((holophrase-cxn (first (restart-data fix)))
           (lexical-cxn (second (restart-data fix)))
           (item-based-cxn (third (restart-data fix)))
           (th-links (subseq (restart-data fix) 3))
           ;; we don't create a temp type hierarchy because we're only applying the holophrase
           ;; apply holophrase-cxn and add node
           ;; add new cip (green box) to node with first car-resulting cfs = resulting transient structure after application
           (new-node (fcg::cip-add-child node (first (fcg-apply (get-processing-cxn holophrase-cxn) (car-resulting-cfs (cipn-car node)) (direction (cip node))
                                                                    :configuration (configuration (construction-inventory node))
                                                                    :cxn-inventory (construction-inventory node)))))
           )

      ;; Add cxns to blackboard of second new node
      (set-data (car-resulting-cfs  (cipn-car new-node)) :fix-cxns (list holophrase-cxn lexical-cxn item-based-cxn))
      (set-data (car-resulting-cfs  (cipn-car new-node)) :fix-categorial-links th-links)
      ;; set cxn-supplier to second new node
      (setf (cxn-supplier new-node) (cxn-supplier node))
      ;; set statuses (colors in web interface)
      (push (type-of repair) (statuses new-node))
      (push 'added-by-repair (statuses new-node))
      ;; enqueue only second new node; never backtrack over the first applied lexical construction, we applied them as a block
      (cip-enqueue new-node (cip node) (get-configuration node :queue-mode)))))