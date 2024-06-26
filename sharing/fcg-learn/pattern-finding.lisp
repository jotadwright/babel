(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                ;;
;; Anti-unification-based grammar induction       ;;
;;                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Learning through anti-unification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun learn-through-anti-unification (speech-act cip)
  "Learn constructions through anti-unification."

  ;; To each branch, try to apply the holphrase-cxns of the grammar.
  (extend-cip-with-holophrase-cxns cip)
  
  (let* ((cxn-inventory (original-cxn-set (construction-inventory cip)))
         (applicable-non-linking-cxns (remove-duplicates (mapcar #'original-cxn (mappend #'applied-constructions (children (top-node cip))))
                                                         :test #'other-cxn-w-same-form-and-meaning-p))
         (all-non-linking-non-applicable-cxns (remove-duplicates (remove-if #'(lambda (cxn)
                                                                                (or (eql (type-of cxn) 'linking-cxn)
                                                                                    (find (name cxn) applicable-non-linking-cxns :key #'name)
                                                                                    (find cxn applicable-non-linking-cxns
                                                                                          :test #'other-cxn-w-same-form-and-meaning-p)))
                                                                            (constructions-list cxn-inventory))
                                                                 :test #'other-cxn-w-same-form-and-meaning-p)))
    
    ;; returns list anti-unification solutions, each with its own fix cxn-inventory
    (search-anti-unification-fix speech-act applicable-non-linking-cxns all-non-linking-non-applicable-cxns cxn-inventory)))


(defun search-anti-unification-fix (speech-act applicable-non-linking-cxns all-non-linking-cxns cxn-inventory)
  "Searches for possible anti-unification fixes based on speech-act,
applicable non-linking constructions and all non-linking
non-applicable constructions."
  (loop with initial-state = (make-instance 'au-repair-state
                                            :all-cxns all-non-linking-cxns
                                            :remaining-applicable-cxns applicable-non-linking-cxns
                                            :remaining-form-speech-act (list (list 'sequence (form speech-act) (make-var "left") (make-var "right")))
                                            :remaining-meaning-speech-act (fresh-variables (pn::variablify-predicate-network
                                                                                            (meaning speech-act)
                                                                                            (get-configuration cxn-inventory :meaning-representation-format)))
                                            :fix-cxn-inventory (copy-fcg-construction-set-without-cxns cxn-inventory)
                                            :created-at 1)
        with au-repair-processor = (make-instance 'au-repair-processor
                                                  :top-state initial-state
                                                  :queue (list initial-state)
                                                  :all-states (list initial-state))
        initially (setf (au-repair-processor initial-state) au-repair-processor)
        while (queue au-repair-processor)
        for current-state = (pop (queue au-repair-processor))
        for new-states = (extend-au-repair-state current-state)
        do (loop for new-state in new-states
                 unless (find new-state (all-states au-repair-processor) :test #'equivalent-state)
                   do (push new-state (children current-state))
                      (setf (all-parents new-state) (cons current-state (all-parents current-state)))
                      (push new-state (all-states au-repair-processor))
                      (setf (new-cxns new-state) (loop for cxn in (constructions-list (fix-cxn-inventory new-state))
                                                       unless (find (name cxn) (constructions-list (fix-cxn-inventory current-state)) :key #'name)
                                                         collect cxn))
                      (setf (created-at new-state) (incf (state-counter au-repair-processor)))
                      (setf (au-repair-processor new-state) au-repair-processor)
                   and
                   if (or (remaining-form-speech-act new-state)
                          (remaining-meaning-speech-act new-state))
                     do (push new-state (queue au-repair-processor))
                   else do (push new-state (succeeded-states au-repair-processor)))
        finally (return (values (succeeded-states au-repair-processor) au-repair-processor))))


(defun extend-au-repair-state (current-state)
  "Extend current state with possible expansions resulting from anti-unification results."
  (let* ((form-au-meaning-au-combinations (loop for applicable-cxn in (remove-duplicates (remaining-applicable-cxns current-state) :test #'eql :key #'name)
                                                for au-form-results = (anti-unify-form (attr-val applicable-cxn :form)
                                                                                       (remaining-form-speech-act current-state) 
                                                                                       (get-configuration (fix-cxn-inventory current-state)
                                                                                                          :form-generalisation-mode))
                                                for au-meaning-results = (anti-unify-meaning (attr-val applicable-cxn :meaning)
                                                                                             (remaining-meaning-speech-act current-state)
                                                                                             (get-configuration (fix-cxn-inventory current-state)
                                                                                                                :meaning-generalisation-mode)
                                                                                             :cxn-inventory (fix-cxn-inventory current-state))
                                                append (loop for combination in (cartesian-product au-form-results au-meaning-results)
                                                             collect (cons applicable-cxn combination))))
         (valid-au-combinations (loop for au-combination in form-au-meaning-au-combinations
                                      when (and (generalisation (second au-combination))
                                                (generalisation (third au-combination)))
                                        collect au-combination)))
    
    (loop for (cxn au-form-result au-meaning-result) in valid-au-combinations
          for fix-cxn-inventory = (copy-object (fix-cxn-inventory current-state))
          for (resulting-integration-cat
               resulting-integration-form-args
               resulting-integration-meaning-args) = (multiple-value-list
                                                      (learn-cxns-from-au-result au-form-result au-meaning-result fix-cxn-inventory
                                                                                 :integration-cat (integration-cat current-state)
                                                                                 :integration-form-args (integration-form-args current-state)
                                                                                 :integration-meaning-args (integration-meaning-args current-state)))
          unless (= (length (constructions-list fix-cxn-inventory))
                    (length (constructions-list (fix-cxn-inventory current-state))))
            collect (make-instance 'au-repair-state
                                   :all-cxns (all-cxns current-state)
                                   :remaining-applicable-cxns (remove (name cxn) (remaining-applicable-cxns current-state) :key #'name)
                                   :remaining-form-speech-act (source-delta au-form-result)
                                   :remaining-meaning-speech-act (source-delta au-meaning-result)
                                   :integration-cat resulting-integration-cat
                                   :integration-form-args resulting-integration-form-args
                                   :integration-meaning-args resulting-integration-meaning-args
                                   :base-cxn cxn
                                   :fix-cxn-inventory fix-cxn-inventory)
              into new-states
          finally (let ((states-from-cxn-inventory (learn-from-cxn-inventory (remaining-form-speech-act current-state)
                                                                             (remaining-meaning-speech-act current-state)
                                                                             (integration-cat current-state) (integration-form-args current-state)
                                                                             (integration-meaning-args current-state)
                                                                             (fix-cxn-inventory current-state)
                                                                             (all-cxns current-state))))
                    (setf new-states (append new-states states-from-cxn-inventory)))
                  (return new-states))))

(defun learn-from-cxn-inventory (form-predicates-speech-act
                                 meaning-predicates-speech-act
                                 integration-cat integration-form-args
                                 integration-meaning-args parent-cxn-inventory
                                 all-cxns)
  "Learn cxns based on cxns from cxn-inventory which could not apply to the root."
  (loop for cxn in (find-all-if #'(lambda (cxn) (= (length (attr-val cxn :form)) 1)) all-cxns) ;; only keep cxns with single form predicate
        ;;FIRST check meaning au result!!
        for au-meaning-results = (anti-unify-meaning (attr-val cxn :meaning)
                                                     meaning-predicates-speech-act
                                                     (get-configuration parent-cxn-inventory :meaning-generalisation-mode)
                                                     :cxn-inventory parent-cxn-inventory)
        for valid-au-meaning-results = (loop for au-meaning-result in au-meaning-results
                                             for (connected-network-p nr-of-chunks) = (multiple-value-list
                                                                                       (connected-semantic-network (generalisation au-meaning-result)))
                                             when (and connected-network-p
                                                       (or (= nr-of-chunks 1)
                                                           (= nr-of-chunks 2)))
                                               collect au-meaning-result)
        when valid-au-meaning-results
          append (let* ((au-form-results (anti-unify-form (attr-val cxn :form)
                                                          form-predicates-speech-act 
                                                          (get-configuration parent-cxn-inventory :form-generalisation-mode)))
                        (valid-au-form-results (loop for au-form-result in au-form-results
                                                     when (and (generalisation au-form-result)
                                                               (<= (length (generalisation au-form-result)) 2)) ;; max one gap allowed, i.e. two sequence predicates
                                                       collect au-form-result)))
                   (when valid-au-form-results
                     (loop for (au-form au-meaning) in (cartesian-product valid-au-form-results valid-au-meaning-results)
                           for fix-cxn-inventory = (copy-object parent-cxn-inventory)
                           do 
                             (learn-cxns-from-au-result au-form au-meaning fix-cxn-inventory :integration-cat integration-cat
                                                        :integration-form-args integration-form-args :integration-meaning-args integration-meaning-args
                                                        :learn-cxns-from-deltas t)
                           collect (make-instance 'au-repair-state
                                                  :base-cxn cxn
                                                  :fix-cxn-inventory fix-cxn-inventory)))) into new-states
        finally (return (cons (make-instance 'au-repair-state
                                             :base-cxn nil
                                             :fix-cxn-inventory (learn-cxn-from-form-and-meaning-predicates
                                                                 form-predicates-speech-act meaning-predicates-speech-act integration-cat
                                                                 integration-form-args integration-meaning-args parent-cxn-inventory))
                              new-states))))
      

(defun equivalent-state (state-1 state-2)
  "Checks whether two au-repair-states are equivalent."
  (let ((cxn-inventory-1 (fix-cxn-inventory state-1))
        (cxn-inventory-2 (fix-cxn-inventory state-2)))

    (and (permutation-of? (remaining-meaning-speech-act state-1) (remaining-meaning-speech-act state-2) :test #'equalp)
         (pn::equivalent-predicate-networks-p (remaining-form-speech-act state-1) (remaining-form-speech-act state-2))
         (permutation-of? (constructions-list cxn-inventory-1) (constructions-list cxn-inventory-2) :test #'equivalent-cxn))))


(defun learn-cxns-from-au-result (au-form au-meaning fix-cxn-inventory &key integration-cat integration-form-args integration-meaning-args
                                          learn-cxns-from-deltas)
  "Learns cxns from anit-unification result."
  ;; Compute form args for filler-cxns (generalisation, pattern and source)
  (multiple-value-bind (generalisation-form-args pattern-form-args source-form-args)
      (compute-filler-args au-form)
    ;; Compute meaning args for filler-cxns (generalisation, pattern and source)
    (multiple-value-bind (generalisation-meaning-args pattern-meaning-args source-meaning-args)
        (compute-filler-args au-meaning)
      (let* (;; Relate passed integration form args to current au-result
             (integration-form-args-slot-1 (compute-linking-args-slots integration-form-args au-form 1))
             (integration-form-args-slot-2 (compute-linking-args-slots integration-form-args au-form 2))
             (integration-meaning-args-slot-1 (compute-linking-args-slots integration-meaning-args au-meaning 1))
             (integration-meaning-args-slot-2 (compute-linking-args-slots integration-meaning-args au-meaning 2))
             
             ;; Compute args for contributing unit of linking-cxn
             (linking-cxn-contributing-meaning-args (compute-linking-args-contributing integration-meaning-args integration-meaning-args-slot-1
                                                                                       integration-meaning-args-slot-2 au-meaning))
             (linking-cxn-contributing-form-args (compute-linking-args-contributing integration-form-args integration-form-args-slot-1
                                                                                    integration-form-args-slot-2 au-form))
             ;; Compute args for conditional units of linking-cxn
             (linking-cxn-form-args-slot-1 (append generalisation-form-args integration-form-args-slot-1))
             (linking-cxn-meaning-args-slot-1 (append generalisation-meaning-args integration-meaning-args-slot-1))
             (linking-cxn-form-args-slot-2 (append generalisation-form-args integration-form-args-slot-2))
             (linking-cxn-meaning-args-slot-2 (append generalisation-meaning-args integration-meaning-args-slot-2))

             ;; Compute args for filler constructions (based on generalisation, source and pattern)
             (generalisation-filler-cxn-form-args (append generalisation-form-args integration-form-args-slot-1))
             (generalisation-filler-cxn-meaning-args (append generalisation-meaning-args integration-meaning-args-slot-1))
             (source-filler-cxn-form-args (append source-form-args integration-form-args-slot-2))
             (source-filler-cxn-meaning-args (append source-meaning-args integration-meaning-args-slot-2))
             (pattern-filler-cxn-form-args (append pattern-form-args integration-form-args-slot-2))
             (pattern-filler-cxn-meaning-args (append pattern-meaning-args integration-meaning-args-slot-2))
             
             ;; Variables that will hold the constructions
             (linking-cxn nil)
             (generalisation-filler-cxn nil)
             (pattern-filler-cxn nil)
             (source-filler-cxn nil))

        ;; If the contributing form args don't map on the integration-form-args (in which case they will be nil), stop and don't learn cxns
        (when (and (if integration-form-args linking-cxn-contributing-form-args t)
                   (if integration-meaning-args linking-cxn-contributing-meaning-args t))

          ;; Learn linking-cxn
          (setf linking-cxn (create-linking-cxn :cxn-inventory fix-cxn-inventory
                                                :contributing-form-args linking-cxn-contributing-form-args
                                                :contributing-meaning-args linking-cxn-contributing-meaning-args
                                                :form-args-slot-1 linking-cxn-form-args-slot-1
                                                :form-args-slot-2 linking-cxn-form-args-slot-2
                                                :meaning-args-slot-1 linking-cxn-meaning-args-slot-1
                                                :meaning-args-slot-2 linking-cxn-meaning-args-slot-2))

          ;; Learn filler-cxn from generalisation
          (setf generalisation-filler-cxn (if linking-cxn
                                            (create-filler-cxn (generalisation au-form) (generalisation au-meaning)
                                                               generalisation-filler-cxn-form-args generalisation-filler-cxn-meaning-args
                                                               fix-cxn-inventory)
                                            (create-filler-cxn (generalisation au-form) (generalisation au-meaning)
                                                               integration-form-args-slot-1 integration-meaning-args-slot-1
                                                                fix-cxn-inventory)))

          ;; Learn filler-cxn from pattern-delta
          (setf pattern-filler-cxn (when (and learn-cxns-from-deltas linking-cxn)
                                     (create-filler-cxn (pattern-delta au-form) (pattern-delta au-meaning)
                                                        pattern-filler-cxn-form-args pattern-filler-cxn-meaning-args fix-cxn-inventory)))

          ;; Learn filler-cxn from source-delta
          (setf source-filler-cxn (when (and learn-cxns-from-deltas linking-cxn)
                                    (create-filler-cxn (source-delta au-form) (source-delta au-meaning)
                                                       source-filler-cxn-form-args source-filler-cxn-meaning-args fix-cxn-inventory))))
          

        ;; Adding new constructions, categories and links to fix-cxn-inventory
        (when linking-cxn
          (add-cxn linking-cxn fix-cxn-inventory)
          (when (attr-val linking-cxn :cxn-cat)
            (add-category (attr-val linking-cxn :cxn-cat) fix-cxn-inventory :recompute-transitive-closure nil)
            (when integration-cat
              (add-link (attr-val linking-cxn :cxn-cat) integration-cat fix-cxn-inventory :recompute-transitive-closure nil)))
          (add-categories (attr-val linking-cxn :slot-cats) fix-cxn-inventory :recompute-transitive-closure nil))
    
        (when generalisation-filler-cxn
          (add-cxn generalisation-filler-cxn fix-cxn-inventory)
          (add-category (attr-val generalisation-filler-cxn :cxn-cat) fix-cxn-inventory :recompute-transitive-closure nil)
          (if linking-cxn
            (add-link (attr-val generalisation-filler-cxn :cxn-cat) (first (attr-val linking-cxn :slot-cats))
                      fix-cxn-inventory :recompute-transitive-closure nil)
            (when integration-cat
              (add-link (attr-val generalisation-filler-cxn :cxn-cat) integration-cat fix-cxn-inventory :recompute-transitive-closure nil))))

        (when (and pattern-filler-cxn linking-cxn)
          (add-cxn pattern-filler-cxn fix-cxn-inventory)
          (add-category (attr-val pattern-filler-cxn :cxn-cat) fix-cxn-inventory :recompute-transitive-closure nil)
          (add-link (attr-val pattern-filler-cxn :cxn-cat) (second (attr-val linking-cxn :slot-cats)) fix-cxn-inventory :recompute-transitive-closure nil))

        (when (and source-filler-cxn linking-cxn)
          (add-cxn source-filler-cxn fix-cxn-inventory)
          (add-category (attr-val source-filler-cxn :cxn-cat) fix-cxn-inventory :recompute-transitive-closure nil)
          (add-link (attr-val source-filler-cxn :cxn-cat) (second (attr-val linking-cxn :slot-cats)) fix-cxn-inventory :recompute-transitive-closure nil))
    
        (values (when linking-cxn (second (attr-val linking-cxn :slot-cats))) ;;return values??
                (or (when (source-delta au-form) source-filler-cxn-form-args)
                    (when (pattern-delta au-form) pattern-filler-cxn-form-args))
                (or (when (source-delta au-form) source-filler-cxn-meaning-args)
                    (when (pattern-delta au-form) pattern-filler-cxn-meaning-args)))))))

  
(defun compute-filler-args (au-result)
  "Computes the args to be used in the filler constructions based on au-result."
  (loop for (delta-var-source . gen-var-source) in (source-bindings au-result)
        for (delta-var-pattern . nil) in (pattern-bindings au-result)
        when (or (find delta-var-source (source-delta au-result) :test (lambda (x y) (member x y)))
                 (find delta-var-pattern (pattern-delta au-result) :test (lambda (x y) (member x y))))
          collect gen-var-source into generalisation-args
          and
          collect delta-var-pattern into pattern-args
          and
          collect delta-var-source into source-args
        finally (return (values generalisation-args pattern-args source-args ))))


(defun compute-linking-args-slots (integration-args au-result slot-id)
  "Computes args for slots of linking cxns."
  (case slot-id
    (1
     (loop for arg in integration-args
           for corresponding-arg-in-generalisation = (or (cdr (assoc arg (pattern-bindings au-result)))
                                                         (cdr (assoc arg (source-bindings au-result))))
           when (and corresponding-arg-in-generalisation
                     (find corresponding-arg-in-generalisation (generalisation au-result) :test #'member)) ;; is this test needed?
             collect corresponding-arg-in-generalisation))
    (2
     (loop for arg in integration-args
           when (find arg (or (pattern-delta au-result) (source-delta au-result)) :test #'member)
             collect arg))))


(defun compute-linking-args-contributing (integration-args integration-args-slot-1 integration-args-slot-2 au-result)
  "Computes args for contributing unit of linking cxns."
  (loop for arg in integration-args
        collect (cond ((or (find arg integration-args-slot-1)
                           (find arg integration-args-slot-2))
                       arg)
                      ((assoc arg (source-bindings au-result))
                       (cdr (assoc arg (source-bindings au-result))))
                      ((assoc arg (pattern-bindings au-result))
                       (cdr (assoc arg (pattern-bindings au-result))))
                      (t
                       (return nil)))))


(defun extend-cip-with-holophrase-cxns (cip)
  "Add applications of all holophrase-cxns as children to initial node of cip."
  (let* ((cxn-inventory (construction-inventory cip))
         (cxn-supplier-mode (get-configuration cxn-inventory :cxn-supplier-mode)))

    ;; Set cxn-supplier-mode
    (set-configuration cxn-inventory :cxn-supplier-mode :holophrase-cxns-only)

    ;; Enqueue again all nodes that were not duplicates, and reset their cxn-supplier slot
    (setf (cxn-supplier (top-node cip)) nil)
    (setf (fully-expanded? (top-node cip)) nil)
    (cip-enqueue (top-node cip) cip (get-configuration cxn-inventory :search-algorithm))

    ;; no solution can be found by appling additionnal holophrase-cxns, so the whole tree will be extended
    (next-cip-solution cip :notify nil)
    
    (set-configuration cxn-inventory :cxn-supplier-mode cxn-supplier-mode)))
 


;; Learning holophrastic constructions ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-holophrastic-cxn (form-predicates meaning-predicates cxn-inventory)
  "Create a holophrastic construction based on form-predicates and meaning-predicates, returns the cxn."
  (let* ((form (second (first form-predicates)))
         (initial-score 0.5)
         (meaning-hash-key (compute-meaning-hash-key-from-predicates meaning-predicates)))
    (make-instance 'holophrastic-cxn
                   :name (make-cxn-name form-predicates)
                   :conditional-part (list (make-instance 'conditional-unit
                                                          :name (make-var "holophrastic-unit")
                                                          :formulation-lock `((HASH meaning ,meaning-predicates))
                                                          :comprehension-lock `((HASH form ,form-predicates))))
                   :cxn-inventory cxn-inventory
                   :feature-types (feature-types cxn-inventory)
                   :attributes `((:form . ,form-predicates)
                                 (:meaning . ,meaning-predicates)
                                 (:entrenchment-score . ,initial-score)
                                 (:form-hash-key . ,form)
                                 (:meaning-hash-key . ,meaning-hash-key)))))


;; Learning filler constructions ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun learn-cxn-from-form-and-meaning-predicates (form-predicates meaning-predicates integration-cat integration-form-args integration-meaning-args cxn-inventory)
    "Returns a fix-cxn-inventory with a single holophrase or filler-cxn added based on form-predicates and meaning-predicates and optionally cat and args."
    (let* ((fix-cxn-inventory (copy-object cxn-inventory))
           (new-cxn (if integration-cat
                      (create-filler-cxn form-predicates meaning-predicates integration-form-args integration-meaning-args fix-cxn-inventory)
                      (create-holophrastic-cxn form-predicates meaning-predicates fix-cxn-inventory))))
      ;; Add cxn
      (add-cxn new-cxn fix-cxn-inventory)
      (when integration-cat
        (add-category (attr-val new-cxn :cxn-cat) fix-cxn-inventory :recompute-transitive-closure nil)
        (add-link (attr-val new-cxn :cxn-cat) integration-cat fix-cxn-inventory :recompute-transitive-closure nil))
      fix-cxn-inventory))


(defun create-filler-cxn (form-sequence-predicates meaning-predicates form-filler-args meaning-filler-args cxn-inventory)
  "Create a filler construction based on form-predicates and meaning-predicates and args, returns the cxn."
  (when (and form-sequence-predicates meaning-predicates)
    (let* ((cxn-name (make-cxn-name form-sequence-predicates))
           (filler-cat (make-const (upcase (format nil "~a-filler-cat" (remove-cxn-tail (symbol-name cxn-name))))))
           (unit-name (make-var "filler-unit"))
           (initial-score 0.5))

      (make-instance 'filler-cxn
                     :name cxn-name
                     :contributing-part (list (make-instance 'contributing-unit
                                                             :name unit-name
                                                             :unit-structure `((category ,filler-cat)
                                                                               ,@(when form-filler-args
                                                                                   `((form-args ,form-filler-args)))
                                                                               ,@(when meaning-filler-args
                                                                                   `((meaning-args ,meaning-filler-args))))))
                     :conditional-part `(,(make-instance 'conditional-unit
                                                         :name unit-name
                                                         :formulation-lock `((HASH meaning ,meaning-predicates))
                                                         :comprehension-lock `((HASH form ,form-sequence-predicates))))
                     :cxn-inventory cxn-inventory
                     :feature-types (feature-types cxn-inventory)
                     :attributes `((:form . ,form-sequence-predicates)
                                   (:meaning . ,meaning-predicates)
                                   ,@(when form-filler-args
                                       `((:form-args . ,form-filler-args)))
                                   ,@(when meaning-filler-args
                                       `((:meaning-args . ,meaning-filler-args)))
                                   (:cxn-cat . ,filler-cat)
                                   (:entrenchment-score . ,initial-score))))))


;; Learning linking constructions ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-linking-cxn (&key (cxn-inventory *fcg-constructions*) contributing-form-args contributing-meaning-args
                                form-args-slot-1 form-args-slot-2 meaning-args-slot-1 meaning-args-slot-2)
  "Create a linking construction based on conditional and contributing args, returns the cxn."
  (when (and form-args-slot-1 meaning-args-slot-1 form-args-slot-2 meaning-args-slot-2)
    (let* ((cxn-name (make-id 'linking-cxn))
           (cxn-cat (make-id 'cxn-cat))
           (slot-cat-1 (make-id 'slot-cat))
           (slot-cat-2 (make-id 'slot-cat))
           (parent-unit-name (make-var "linking-unit"))
           (slot-unit-1-name (make-var "slot-unit"))
           (slot-unit-2-name (make-var "slot-unit"))
           (initial-score 0.5))

      (make-instance 'linking-cxn
                     :name cxn-name
                     :contributing-part (list (make-instance 'contributing-unit
                                                             :name parent-unit-name
                                                             :unit-structure `((subunits ,(list slot-unit-1-name slot-unit-2-name))
                                                                               ,@(when (or contributing-form-args contributing-meaning-args)
                                                                                   `((category ,cxn-cat)))
                                                                               ,@(when contributing-form-args
                                                                                   `((form-args ,contributing-form-args)))
                                                                               ,@(when contributing-meaning-args
                                                                                   `((meaning-args ,contributing-meaning-args))))))
                     :conditional-part (list (make-instance 'conditional-unit
                                                            :name slot-unit-1-name
                                                            :formulation-lock `((category ,slot-cat-1)
                                                                                ,@(when form-args-slot-1
                                                                                    `((form-args ,form-args-slot-1)))
                                                                                ,@(when meaning-args-slot-1
                                                                                    `((meaning-args ,meaning-args-slot-1))))
                                                            :comprehension-lock `((category ,slot-cat-1)
                                                                                  ,@(when form-args-slot-1
                                                                                      `((form-args ,form-args-slot-1)))
                                                                                  ,@(when meaning-args-slot-1
                                                                                      `((meaning-args ,meaning-args-slot-1)))))
                                             (make-instance 'conditional-unit
                                                            :name slot-unit-2-name
                                                            :formulation-lock `((category ,slot-cat-2)
                                                                                ,@(when form-args-slot-2
                                                                                    `((form-args ,form-args-slot-2)))
                                                                                ,@(when meaning-args-slot-2
                                                                                    `((meaning-args ,meaning-args-slot-2))))
                                                            :comprehension-lock `((category ,slot-cat-2)
                                                                                  ,@(when form-args-slot-2
                                                                                      `((form-args ,form-args-slot-2)))
                                                                                  ,@(when meaning-args-slot-2
                                                                                      `((meaning-args ,meaning-args-slot-2))))))
                     :cxn-inventory cxn-inventory
                     :feature-types (feature-types cxn-inventory)
                     :attributes `(,@(when contributing-form-args
                                       `((:form-args . ,contributing-form-args)))
                                   ,@(when contributing-meaning-args
                                       `((:meaning-args . ,contributing-meaning-args)))
                                   ,@(when (or contributing-form-args contributing-meaning-args) `((:cxn-cat . ,cxn-cat)))
                                   ,@(when form-args-slot-1
                                       `((:form-args-slot-1 . ,form-args-slot-1)))
                                   ,@(when form-args-slot-2
                                       `((:form-args-slot-2 . ,form-args-slot-2)))
                                   ,@(when meaning-args-slot-1
                                       `((:meaning-args-slot-1 . ,meaning-args-slot-1)))
                                   ,@(when meaning-args-slot-2
                                       `((:meaning-args-slot-2 . ,meaning-args-slot-2)))
                                   (:slot-cats ,slot-cat-1 ,slot-cat-2)
                                   (:entrenchment-score . ,initial-score))))))


;; Anti-unfication ;;
  ;;;;;;;;;;;;;;;;;;;;;


(defgeneric anti-unify-form (cxn-form speech-act-form mode &key cxn-inventory &allow-other-keys)
  (:documentation "Anti-unification of form."))

(defmethod anti-unify-form ((cxn-sequence-predicates list)
                            (speech-act-sequence-predicates list)
                            (mode (eql :needleman-wunsch)) &key &allow-other-keys)
  (loop with au-results = (anti-unify-sequences cxn-sequence-predicates speech-act-sequence-predicates)
        with cost = (cost (first au-results))
        for au-result in au-results
        if (= cost (cost au-result))
          collect au-result into au-form-results
        else
          do (return au-form-results)
        finally (return au-form-results)))


(defgeneric anti-unify-meaning (cxn-meaning speech-act-meaning mode &key cxn-inventory &allow-other-keys)
  (:documentation "Anti-unification of meaning."))

(defmethod anti-unify-meaning ((cxn-meaning-predicates list)
                               (speech-act-meaning-predicates list)
                               (mode (eql :k-swap)) &key cxn-inventory &allow-other-keys)
  "Anti-unify meaning predicates using the k-swap algorithm."
  (loop with au-results = (au-benchmark.msg.kswap-omega:anti-unify-predicate-networks cxn-meaning-predicates speech-act-meaning-predicates
                                                                                      :k (get-configuration cxn-inventory :k-swap-k)
                                                                                      :w (get-configuration cxn-inventory :k-swap-w))
        with cost = (cost (first au-results))
        for k-swap-au-result in au-results
        if (= cost (cost k-swap-au-result))
          collect (make-instance 'predicate-network-au-result 
                                 :pattern (au-benchmark.msg.kswap-omega::pattern k-swap-au-result)
                                 :source (au-benchmark.msg.kswap-omega::source k-swap-au-result)
                                 :generalisation (au-benchmark.msg.kswap-omega::generalisation k-swap-au-result)
                                 :pattern-bindings (au-benchmark.msg.kswap-omega::pattern-bindings k-swap-au-result)
                                 :source-bindings (au-benchmark.msg.kswap-omega::source-bindings k-swap-au-result)
                                 :pattern-delta (au-benchmark.msg.kswap-omega::pattern-delta k-swap-au-result)
                                 :source-delta (au-benchmark.msg.kswap-omega::source-delta k-swap-au-result)
                                 :cost (au-benchmark.msg.kswap-omega::cost k-swap-au-result))
            into au-meaning-results
        else
          do (return au-meaning-results)
        finally (return au-meaning-results)))

  
  

(defmethod anti-unify-meaning ((cxn-meaning-predicates list)
                               (speech-act-meaning-predicates list)
                               (mode (eql :exhaustive)) &key &allow-other-keys)
  "Anti-unify meaning predicates using the exhaustive algorithm."
  (loop with au-results = (anti-unify-predicate-network cxn-meaning-predicates speech-act-meaning-predicates)
        with cost = (cost (first au-results))
        for au-result in au-results
        if (= cost (cost au-result))
          collect au-result into au-meaning-results
        else
          do (return au-meaning-results)
        finally (return au-meaning-results)))
  




