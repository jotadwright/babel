(in-package :pattern-finding)


(defclass anti-unify-cxns (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))


(defmethod repair ((repair anti-unify-cxns)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by anti-unifying the observation with the subset of cxns
   that results in the smallest generalisation."
  (let ((cxns-and-categorial-links (create-cxns-by-anti-unification problem node)))
    (when cxns-and-categorial-links
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data cxns-and-categorial-links))))


(defun create-cxns-by-anti-unification (problem node)
  (do-repair
   (get-data problem :utterance)
   (get-data problem :meaning)
   nil
   nil
   (construction-inventory node)
   node
   'anti-unify-cxns))


(defmethod do-repair (observation-form observation-meaning form-args meaning-args
                                       (cxn-inventory construction-inventory)
                                       node (repair-type (eql 'anti-unify-cxns)))
  (destructuring-bind (cxns-to-apply cxns-to-consolidate cats-to-add links-to-add)
      (find-cxns-and-anti-unify observation-form observation-meaning (original-cxn-set cxn-inventory))
    (apply-fix observation-form
               cxns-to-apply
               links-to-add
               cxns-to-consolidate
               cats-to-add
               (extract-contributing-lex-class (last-elt cxns-to-apply))
               t
               node
               repair-type)))







;; TO DO
;; Information from partial analysis is now NOT used
;; Find the best partial analysis node and try to learn from that one??
;; How to define "the best" partial analysis?

;; Taking the lowest anti-unification cost with partial analyses is not
;; a good solution, since the cost will be high instead of low! With
;; partial analyses, the generalisation are the parts that are identical,
;; while the rest is in the delta's. Often there will be more in the rest
;; than in the overlap.

;;;;;;;;;;;;;;;;;;;;;;
;; anti-unify utils ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun anti-unify-form (observation set-of-cxns)
  "Anti-unify the observation with the given set of cxns
   on the form side."
  ;; anti-unify-string requires string, so need to render the observation and the cxn forms
  ;; but rendering can return multiple solutions, so try out all combinations
  
  ;; !!! TO DO
  ;; Introduce variables in the rendering of ihe sequence predicates of item-based cxns
  ;; - How to know how many variables? Variables on the edges?
  ;;   For example: ((sequence "what" ?lb1 ?rb1) (sequence "are there" ?lb2 ?rb2))
  ;;   => "?SLOT1 what ?SLOT2 are there ?SLOT3"
  ;;      "?SLOT1 what ?SLOT2 are there"
  ;;      "what ?SLOT2 are there ?SLOT3"
  ;;      "what ?SLOT2 are there"
  ;;   => can be solved by looking at the form args
  ;; - Does string-anti-unification work properly with variables included?
  
  (let* ((possible-source-forms
          (mapcar #'(lambda (lst) (list-of-strings->string lst :separator ""))
                  (render-all observation :render-sequences)))
         (cxn-forms (loop for cxn in set-of-cxns
                          append (fcg::extract-form-predicates cxn)))
         (possible-target-forms
          (mapcar #'(lambda (lst) (list-of-strings->string lst :separator ""))
                  (render-all cxn-forms :render-sequences)))
         (anti-unification-results
          (loop for (pattern-form source-form) in (combinations possible-source-forms possible-target-forms)
                append (fcg::anti-unify-strings pattern-form source-form :to-sequence-predicates-p t)))
         (results-with-non-empty-deltas
          (remove-if #'(lambda (result) (or (null (pattern-delta result)) (null (source-delta result))))
                     anti-unification-results)))
    (first (sort results-with-non-empty-deltas #'< :key #'fcg::cost))))

(defun anti-unify-meaning (source-meaning set-of-cxns)
  "Anti-unify the observation with the given set of cxns
   on the meaning side."
  (let* ((pattern-meaning
           (loop for cxn in set-of-cxns
                 append (fresh-variables(fcg::extract-meaning-predicates cxn))))
         (anti-unification-results
          (fcg::anti-unify-predicate-network pattern-meaning source-meaning))
         (results-with-non-empty-deltas
          (remove-if #'(lambda (result) (or (null (pattern-delta result)) (null (source-delta result))))
                     anti-unification-results)))
    (first (sort results-with-non-empty-deltas #'< :key #'fcg::cost))))

(defun sort-anti-unification-results (list-of-anti-unification-results)
  "Sort the anti-unifcation results based on cost (of both form- and
   meaning-anti-unification) and avg cxn score as a tie breaker."
  (labels ((total-au-cost (combined-anti-unification-result)
             (let ((avg-cxn-score (average (mapcar #'get-cxn-score (first combined-anti-unification-result))))
                   (form-au-cost (fcg::cost (second combined-anti-unification-result)))
                   (meaning-au-cost (fcg::cost (third combined-anti-unification-result))))
               (+ form-au-cost meaning-au-cost (- 1 avg-cxn-score)))))
    (sort list-of-anti-unification-results #'< :key #'total-au-cost)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find cxns and anti-unify ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-cxns-and-anti-unify (observation-form observation-meaning cxn-inventory)
  "Given form and meaning of an observation and a cxn inventory,
   find the set of cxns that leads to the smallest generalisation
   and learn new cxn(s) from this generalisation."
  (if (null (constructions cxn-inventory))
    ;; if the cxn inventory is empty, immediately learn a holistic cxn
    ;; for the entire observation
    (make-holistic-cxn observation-form observation-meaning cxn-inventory)
    (let* (;; 1) select cxns by hasing the observation according to processing direction
           ;(hash-compatible-cxns
           ; (constructions-for-anti-unification-hashed observation-form nil cxn-inventory))
         
           ;; 2) make powerset of (hash compatible) cxns, sort by length and by score
           ;;    sets with holophrase cxns do not make sense... these can be filtered
           (non-holophrase-routine-cxns-with-positive-score
            (remove-if #'holophrase-cxn-p
                       (remove-if-not #'non-zero-cxn-p
                                      (remove-if-not #'routine-cxn-p
                                                     (constructions cxn-inventory)))))
           (routine-holophrase-cxns-with-positive-score
            (remove-if-not #'holophrase-cxn-p
                           (remove-if-not #'non-zero-cxn-p
                                          (remove-if-not #'routine-cxn-p
                                                         (constructions cxn-inventory)))))
           (cxn-sets-for-anti-unification
            (sort (append (all-subsets non-holophrase-routine-cxns-with-positive-score)
                          (mapcar #'list routine-holophrase-cxns-with-positive-score))
                  #'(lambda (cxn-set-1 cxn-set-2)
                      ;; if two cxn sets have the same length
                      ;; take the one with the highest average score
                      (if (length= cxn-set-1 cxn-set-2)
                        (let ((avg-score-1 (average (mapcar #'get-cxn-score cxn-set-1)))
                              (avg-score-2 (average (mapcar #'get-cxn-score cxn-set-2))))
                          (> avg-score-1 avg-score-2))
                        (length> cxn-set-1 cxn-set-2)))))
           
           ;; 3) anti-unify each set of cxns with the observation
           ;;    and take the one with lowest cost
           (least-general-generalisation
            (loop for set-of-cxns in cxn-sets-for-anti-unification
                  for best-form-anti-unification-result
                    = (anti-unify-form observation-form set-of-cxns)
                  for best-meaning-anti-unification-result
                    = (anti-unify-meaning observation-meaning set-of-cxns)
                  when (and best-form-anti-unification-result
                            best-meaning-anti-unification-result
                            (<= (cost best-form-anti-unification-result) 5)
                            (<= (cost best-meaning-anti-unification-result) 5))
                  collect (list set-of-cxns
                                best-form-anti-unification-result
                                best-meaning-anti-unification-result)
                      into anti-unification-results
                  finally (return (first (sort-anti-unification-results anti-unification-results)))))
           
           ;; 4) learn cxn(s) from the anti-unification results
           ;;    - no anti-unification results -> learn holistic
           ;;    - yes anti-unification results -> learn cxns for generalisation and delta's
           (new-cxn-and-links
            (if least-general-generalisation
              (make-cxns-from-generalisations least-general-generalisation cxn-inventory)
              (make-holistic-cxn observation-form observation-meaning cxn-inventory))))
    new-cxn-and-links)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make cxns from generalisation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-cxns-from-generalisations (anti-unification-results cxn-inventory)
  (destructuring-bind (anti-unified-cxns
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (declare (ignore anti-unified-cxns))
    (let* (;; all form-args and meaning-args
           (form-args (compute-args form-anti-unification))
           (meaning-args (compute-args meaning-anti-unification))
           ;; dispatch to helper functions to make generalisation-cxn and delta cxns
           (generalisation-cxns-and-categories
            (make-generalisation-cxn (generalisation form-anti-unification)
                                     (generalisation meaning-anti-unification)
                                     (get-data form-args :top-lvl-args)
                                     (get-data meaning-args :top-lvl-args)
                                     (get-data form-args :generalisation-slot-args)
                                     (get-data meaning-args :generalisation-slot-args)
                                     cxn-inventory))
           (pattern-delta-cxns-and-categories
            (make-holistic-cxn (pattern-delta form-anti-unification)
                               (pattern-delta meaning-anti-unification)
                               cxn-inventory
                               (get-data form-args :pattern-args)
                               (get-data meaning-args :pattern-args)))
           (source-delta-cxns-and-categories
            (make-holistic-cxn (source-delta form-anti-unification)
                               (source-delta meaning-anti-unification)
                               cxn-inventory
                               (get-data form-args :source-args)
                               (get-data meaning-args :source-args)))
           ;; build result
           (cxns-to-apply
            (append (first source-delta-cxns-and-categories)
                    (list (first generalisation-cxns-and-categories))))
           (cxns-to-consolidate
            (append (list (second generalisation-cxns-and-categories))
                    (second source-delta-cxns-and-categories)
                    (first pattern-delta-cxns-and-categories)
                    (second pattern-delta-cxns-and-categories)))
           (categories-to-add
            (append (list (third generalisation-cxns-and-categories))
                    (list (fourth generalisation-cxns-and-categories))
                    (third pattern-delta-cxns-and-categories)
                    (third source-delta-cxns-and-categories)))
           (links-to-add
            (append (fourth pattern-delta-cxns-and-categories)
                    (fourth source-delta-cxns-and-categories)
                    (list (cons (fourth generalisation-cxns-and-categories)
                                (first (third pattern-delta-cxns-and-categories)))
                          (cons (fourth generalisation-cxns-and-categories)
                                (first (third source-delta-cxns-and-categories)))))))
      (list cxns-to-apply cxns-to-consolidate categories-to-add links-to-add))))


(defun make-generalisation-cxn (form meaning
                                top-lvl-form-args top-lvl-meaning-args
                                slot-form-args slot-meaning-args
                                cxn-inventory)
  (let* (;; cxn names
         (bare-cxn-name
          (make-cxn-name form cxn-inventory :item-based-suffix t :numeric-suffix t))
         (cxn-name-apply-last
          (intern (upcase (format nil "~a-apply-last" bare-cxn-name))))
         (cxn-name-apply-first
          (intern (upcase (format nil "~a-apply-first" bare-cxn-name))))
         ;; cxn inventory
         (cxn-inventory-copy (copy-object cxn-inventory))
         ;; lex classes
         (lex-class-item-based
          (make-lex-class (symbol-name bare-cxn-name) :trim-cxn-suffix t :numeric-suffix t))
         (lex-class-slot
          (make-lex-class (symbol-name bare-cxn-name) :trim-cxn-suffix t :numeric-suffix t :slotp t))
         ;; build cxns!
         (item-based-cxn-apply-last
          (second
           (multiple-value-list
            (eval
             (item-based-cxn-apply-last-skeleton bare-cxn-name cxn-name-apply-last
                                                 lex-class-item-based lex-class-slot
                                                 form meaning
                                                 top-lvl-form-args top-lvl-meaning-args
                                                 slot-form-args slot-meaning-args
                                                 (get-configuration cxn-inventory :initial-cxn-score)
                                                 cxn-inventory-copy)))))
         (item-based-cxn-apply-first
          (second
           (multiple-value-list
            (eval
             (item-based-cxn-apply-first-skeleton bare-cxn-name cxn-name-apply-first
                                                  lex-class-item-based lex-class-slot
                                                  form meaning
                                                  top-lvl-form-args top-lvl-meaning-args
                                                  slot-form-args slot-meaning-args
                                                  (get-configuration cxn-inventory :initial-cxn-score)
                                                  cxn-inventory-copy))))))
    (list item-based-cxn-apply-last
          item-based-cxn-apply-first
          lex-class-item-based
          lex-class-slot)))

;;;;;;;;;;;;;;;;;;;;
;; computing args ;;
;;;;;;;;;;;;;;;;;;;;

(defun compute-args (anti-unification-result)
  (let* ((connecting-vars
          ;; find args that connect the delta back to the generalisation
          (handle-connecting-vars anti-unification-result))
         (singleton-vars
          ;; find free variables in the delta that should be passed
          ;; along in the item-based cxn
          (handle-singleton-vars anti-unification-result connecting-vars))
         (all-slot-args
          ;; combine them
          (append-data-fields connecting-vars singleton-vars))
         (top-lvl-args
          ;; compute top lvl args for the item-based cxn
          (compute-top-lvl-args anti-unification-result all-slot-args)))
    ;; combine all args and return
    (append-data-fields all-slot-args top-lvl-args)))

(defun handle-connecting-vars (anti-unification-result)
  (with-slots (generalisation
               pattern-bindings
               source-bindings
               pattern-delta
               source-delta) anti-unification-result
    (let ((vars-in-generalisation
           (remove-duplicates (find-all-anywhere-if #'variable-p generalisation)))
          (connecting-vars (make-blackboard)))
      (loop for var in vars-in-generalisation
            for pattern-var = (first (rassoc var pattern-bindings))
            for source-var = (first (rassoc var source-bindings))
            when (and pattern-var source-var
                      (find-anywhere pattern-var pattern-delta)
                      (find-anywhere source-var source-delta))
            do (push-data connecting-vars :generalisation-slot-args var)
               (push-data connecting-vars :pattern-args  pattern-var)
               (push-data connecting-vars :source-args source-var))
      connecting-vars)))

(defun handle-singleton-vars (anti-unification-result connecting-vars)
  (with-slots (generalisation
               pattern-bindings
               source-bindings
               pattern-delta
               source-delta) anti-unification-result    
    (let* ((pattern-delta-vars
            (remove-duplicates (find-all-anywhere-if #'variable-p pattern-delta)))
           (source-delta-vars
            (remove-duplicates (find-all-anywhere-if #'variable-p source-delta)))
           (remaining-pattern-vars
            (reverse (set-difference pattern-delta-vars (get-data connecting-vars :pattern-args))))
           (remaining-source-vars
            (reverse (set-difference source-delta-vars (get-data connecting-vars :source-args))))
           (singleton-pattern-vars
            (loop for var in remaining-pattern-vars
                  when (= (count-anywhere var pattern-delta) 1)
                  collect var))
           (singleton-source-vars
            (loop for var in remaining-source-vars
                  when (= (count-anywhere var source-delta) 1)
                  collect var))
           (singleton-args (make-blackboard)))
      (multiple-value-bind (longest-delta-key other-delta-key longest-var-list other-var-list)
          (if (> (length singleton-pattern-vars) (length singleton-source-vars))
            (values :pattern-args :source-args singleton-pattern-vars singleton-source-vars)
            (values :source-args :pattern-args singleton-source-vars singleton-pattern-vars))
        ;; possibly dangerous to assign var-i and var-j the same position in the arg list...
        ;; we'll see when it breaks down
        (loop for var-i in longest-var-list
              for i from 0
              for var-j = (nth i other-var-list)
              do (push-data singleton-args longest-delta-key var-i)
                 (push-data singleton-args :generalisation-slot-args (make-var 'arg))
                 (push-data singleton-args other-delta-key (or var-j (make-var 'arg)))))
      singleton-args)))

(defun compute-top-lvl-args (anti-unification-result slot-args)
  (let ((unconnected-vars (get-unconnected-vars (generalisation anti-unification-result)))
        (slot-args (get-data slot-args :generalisation-slot-args))
        (top-lvl-args (make-blackboard)))
    (set-data top-lvl-args :top-lvl-args
              (set-difference
               (union unconnected-vars slot-args)
               (intersection unconnected-vars slot-args)))
    top-lvl-args))
        

;;;;;;;;;;;;;;;;;;;;;;;
;; make holistic cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;
        
(defun make-holistic-cxn (form meaning cxn-inventory &optional form-args meaning-args)
  (let* (;; make the cxn names
         (cxn-name
          (make-cxn-name form cxn-inventory :holistic-suffix t :numeric-suffix t))
         (cxn-name-apply-last
          (intern (upcase (format nil "~a-apply-last" cxn-name))))
         (cxn-name-apply-first
          (intern (upcase (format nil "~a-apply-first" cxn-name))))
         ;; unit name
         (unit-name-holistic-cxn
          (make-unit-name (mkstr cxn-name) cxn-inventory :trim-cxn-suffix t))
         ;; lex class
         (lex-class-holistic-cxn
          (make-lex-class cxn-name :trim-cxn-suffix t :numeric-suffix t))
         ;; temp cxn inventory
         (cxn-inventory-copy
          (copy-object cxn-inventory))
         ;; args
         (cxn-meaning-args (or meaning-args (get-unconnected-vars meaning)))
         (cxn-form-args (or form-args (get-unconnected-vars form)))
         ;; holophrasep
         (holophrasep (not (and form-args meaning-args)))
         ;; apply first cxn
         (holistic-cxn-apply-first
          (second
           (multiple-value-list
            (eval
             (holistic-cxn-apply-first-skeleton cxn-name cxn-name-apply-first
                                                unit-name-holistic-cxn lex-class-holistic-cxn
                                                form meaning cxn-form-args cxn-meaning-args
                                                (get-configuration cxn-inventory :initial-cxn-score)
                                                holophrasep cxn-inventory-copy)))))
         ;; apply last cxn
         (holistic-cxn-apply-last
          (second
           (multiple-value-list
            (eval
             (holistic-cxn-apply-last-skeleton cxn-name cxn-name-apply-last
                                               unit-name-holistic-cxn lex-class-holistic-cxn
                                               form meaning cxn-form-args cxn-meaning-args
                                               (get-configuration cxn-inventory :initial-cxn-score)
                                               holophrasep cxn-inventory-copy)))))
         ;; build the result
         (cxns-to-apply (list holistic-cxn-apply-first))
         (cxns-to-consolidate (list holistic-cxn-apply-last))
         (cats-to-add (list lex-class-holistic-cxn))
         (links-to-add nil))
    ;; done!
    (list cxns-to-apply cxns-to-consolidate cats-to-add links-to-add)))


