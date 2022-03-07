(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Holophrase Deletion  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass holophrase->item-based+holistic+holophrase--deletion (repair) 
  ((trigger :initform 'fcg::new-node))) ;; it's always fcg::new-node, we created a new node in the search process

(defmethod repair ((repair holophrase->item-based+holistic+holophrase--deletion)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction, holophrase and holistic cxn."
  (when (initial-node-p node)
    (let ((constructions-and-categorial-links (create-repair-cxns-holophrase-single-deletion problem node)))
      (when constructions-and-categorial-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-categorial-links)))))

(defmethod repair ((repair holophrase->item-based+holistic+holophrase--deletion)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction, holophrase and holistic cxn."
  (when (initial-node-p node)
    (let ((constructions-and-categorial-links (create-repair-cxns-holophrase-single-deletion problem node)))
      (when constructions-and-categorial-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-categorial-links)))))

(defun create-repair-cxns-holophrase-single-deletion (problem node) ;;node = cip node (transient struct, applied cxns, cxn-inventory, ..)
  "Creates item-based construction, a holophrase and a holistic construction
   based on an existing holophrase construction of which the form/meaning are a superset of the observed phrase.

   Example:
   - cxn-inventory: contains a holophrase for 'the red cube'
   - new observation: 'the cube'

   Result:
   - holophrase-cxn: the-cube-cxn
   - item based-cxn: the-X-cube-cxn
   - holistic-cxn: red-cxn

   Edge cases to test:
   - center-deletion: 'the red cube' => 'the cube'
   - right-deletion: 'the cube red' => 'the cube' (e.g. French) or 'the cat jumps' => 'the cat'
   - left deletion: 'two red cubes' => 'red cubes'
   "
  (let* ((cxn-inventory (original-cxn-set (construction-inventory node)))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         (gold-standard-meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings))
                                                                   meaning-representation-formalism))
         
         (utterance (random-elt (get-data problem :utterances))))
    (multiple-value-bind (superset-holophrase-cxn
                          non-overlapping-form
                          non-overlapping-meaning)
        (find-superset-holophrase-cxn cxn-inventory gold-standard-meaning utterance)

      (when superset-holophrase-cxn
        (let* ((overlapping-form
                (set-difference (extract-form-predicates superset-holophrase-cxn) non-overlapping-form :test #'equal))
               (overlapping-meaning
                (set-difference (extract-meaning-predicates superset-holophrase-cxn) non-overlapping-meaning :test #'equal))
               (existing-holistic-cxn
                (find-cxn-by-form-and-meaning non-overlapping-form non-overlapping-meaning cxn-inventory))
               (boundaries-holistic-cxn (get-boundary-units non-overlapping-form))
               (leftmost-unit-holistic-cxn (first boundaries-holistic-cxn))
               (rightmost-unit-holistic-cxn (second boundaries-holistic-cxn))
               (holistic-cxn-name
                (make-cxn-name non-overlapping-form cxn-inventory :add-numeric-tail t))
               (cxn-name-item-based-cxn (make-cxn-name
                                         (substitute-slot-meets-constraints non-overlapping-form overlapping-form) cxn-inventory :add-numeric-tail t))
               (unit-name-holistic-cxn
                (unit-ify (make-cxn-name non-overlapping-form cxn-inventory :add-cxn-suffix nil)))
               ;; lex-class
               (lex-class-holistic-cxn
                (if existing-holistic-cxn
                  (lex-class-cxn existing-holistic-cxn)
                  (make-lex-class holistic-cxn-name :trim-cxn-suffix t)))
               (lex-class-item-based-cxn
                (make-lex-class cxn-name-item-based-cxn :trim-cxn-suffix t))
               ;; type hierachy links
               (categorial-link
                (cons lex-class-holistic-cxn lex-class-item-based-cxn))
               
               

               (meaning
                (meaning-predicates-with-variables (random-elt (get-data problem :meanings))
                                                   meaning-representation-formalism))
               ;; args: 
               (args-holistic-cxn
                (extract-args-from-irl-network non-overlapping-meaning))
               (args-holophrase-cxn (extract-args-from-irl-network meaning))
               (cxn-name
                (make-cxn-name utterance cxn-inventory :add-numeric-tail t))
               (form-constraints
                (form-constraints-with-variables utterance (get-configuration cxn-inventory :de-render-mode)))
               (boundaries-holophrase-cxn (get-boundary-units form-constraints))
               (leftmost-unit-holophrase-cxn (first boundaries-holophrase-cxn))
               (rightmost-unit-holophrase-cxn (second boundaries-holophrase-cxn))
               (holophrase-cxn
                (second (multiple-value-list  (eval
                                               `(def-fcg-cxn ,cxn-name
                                                             ((?holophrase-unit
                                                               (syn-cat (phrase-type holophrase))
                                                               (args ,args-holophrase-cxn)
                                                               (boundaries
                                                                   (left ,leftmost-unit-holophrase-cxn)
                                                                   (right ,rightmost-unit-holophrase-cxn))
                                                               )
                                                              <-
                                                              (?holophrase-unit
                                                               (HASH meaning ,meaning)
                                                               --
                                                               (HASH form ,form-constraints)))
                                                             :attributes (:cxn-type holophrase
                                                                          :repair holophrase->item-based+holistic+holophrase--deletion)
                                                             :cxn-inventory ,(copy-object cxn-inventory))))))

                 
               (holistic-cxn
                (or existing-holistic-cxn
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,holistic-cxn-name
                                                                ((,unit-name-holistic-cxn
                                                                  (args ,args-holistic-cxn)
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-holistic-cxn))
                                                                  (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn)
                                                                   (right ,rightmost-unit-holistic-cxn)))
                                                                 <-
                                                                 (,unit-name-holistic-cxn
                                                                  (HASH meaning ,non-overlapping-meaning)
                                                                  --
                                                                  (HASH form ,non-overlapping-form)))
                                                                :attributes (:cxn-type holistic
                                                                             :repair holophrase->item-based+holistic+holophrase--deletion
                                                                             :meaning ,(fourth (find 'bind non-overlapping-meaning :key #'first))
                                                                             :string ,(third (find 'string non-overlapping-form :key #'first)))
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))));; trick to get the cxn without adding it to the cxn-inventory: make a copy of the cxn-inventory, make the cxn, get it, then forget about the copy
               (item-based-cxn
                (second (multiple-value-list (eval
                                              `(def-fcg-cxn ,cxn-name-item-based-cxn
                                                            ((?item-based-unit
                                                              (syn-cat (phrase-type item-based))
                                                              (subunits (,unit-name-holistic-cxn)))
                                                             (,unit-name-holistic-cxn
                                                              (syn-cat (lex-class ,lex-class-item-based-cxn)))
                                                             <-
                                                             (?item-based-unit
                                                              (HASH meaning ,overlapping-meaning)
                                                              --
                                                              (HASH form ,overlapping-form))
                                                             (,unit-name-holistic-cxn
                                                              (args ,args-holistic-cxn)
                                                              --
                                                              (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn)
                                                                   (right ,rightmost-unit-holistic-cxn))))
                                                            :attributes (:cxn-type item-based
                                                                         :repair holophrase->item-based+holistic+holophrase--deletion
                                                                         :meaning ,(loop for predicate in overlapping-meaning
                                                                                         unless (or
                                                                                                 (equal (first predicate) 'get-context)
                                                                                                 (equal (first predicate) 'bind))
                                                                                         return (first predicate))
                                                                         :string ,(third (find 'string overlapping-form :key #'first)))
                                                            :cxn-inventory ,(copy-object cxn-inventory)))))))

          ;; return the holophrase-cxn, item-based and holistic cxns
          (list holophrase-cxn holistic-cxn item-based-cxn categorial-link))))))
            
          

(defmethod handle-fix ((fix fcg::cxn-fix) (repair holophrase->item-based+holistic+holophrase--deletion) (problem problem) (node cip-node) &key &allow-other-keys) 
  "Apply the constructions provided by fix to the result of the node and return the construction-application-result"
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications ;; avoid notifications in web interace
    (let* ((holophrase-cxn (first (restart-data fix)))
           (holistic-cxn (second (restart-data fix)))
           (item-based-cxn (third (restart-data fix)))
           (categorial-links (subseq (restart-data fix) 3))
           ;; we don't create a temp type hierarchy because we're only applying the holophrase
           ;; apply holophrase-cxn and add node
           ;; add new cip (green box) to node with first car-resulting cfs = resulting transient structure after application
           (new-node (fcg::cip-add-child node (first (fcg-apply (get-processing-cxn holophrase-cxn) (car-resulting-cfs (cipn-car node)) (direction (cip node))
                                                                    :configuration (configuration (construction-inventory node))
                                                                    :cxn-inventory (construction-inventory node)))))
           )

      ;; Add cxns to blackboard of second new node
      (set-data (car-resulting-cfs  (cipn-car new-node)) :fix-cxns (list holophrase-cxn holistic-cxn item-based-cxn))
      (set-data (car-resulting-cfs  (cipn-car new-node)) :fix-categorial-links categorial-links)
      ;; set cxn-supplier to second new node
      (setf (cxn-supplier new-node) (cxn-supplier node))
      ;; set statuses (colors in web interface)
      (push (type-of repair) (statuses new-node))
      (push 'added-by-repair (statuses new-node))
      ;; enqueue only second new node; never backtrack over the first applied holistic construction, we applied them as a block
      (cip-enqueue new-node (cip node) (get-configuration node :queue-mode)))))