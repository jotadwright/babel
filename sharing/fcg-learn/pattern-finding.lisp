(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                ;;
;; Anti-unification-based grammar induction       ;;
;;                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Learning holophrastic constructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun learn-holophrastic-cxn (speech-act cxn-inventory)
  "Learn a holophrastic construction from a speech act. Holophrastic constructions have no args."
  (let* ((form (form speech-act))
         (form-sequence-predicates `((sequence ,form ,(make-var "left") ,(make-var "right"))))
         (meaning-predicates (fresh-variables (pn::variablify-predicate-network (meaning speech-act)
                                                                                (get-configuration cxn-inventory :meaning-representation-format))))
         (initial-score 0.5)
         (meaning-hash-key (compute-meaning-hash-key-from-predicates meaning-predicates))
         (holophrastic-cxn (make-instance 'holophrastic-cxn
                                          :name (make-cxn-name form-sequence-predicates)
                                          :conditional-part (list (make-instance 'conditional-unit
                                                                                 :name (make-var "holophrastic-unit")
                                                                                 :formulation-lock `((HASH meaning ,meaning-predicates))
                                                                                 :comprehension-lock `((HASH form ,form-sequence-predicates))))
                                          :cxn-inventory cxn-inventory
                                          :feature-types (feature-types cxn-inventory)
                                          :attributes `((:form . ,form-sequence-predicates)
                                                        (:meaning . ,meaning-predicates)
                                                        (:entrenchment-score . ,initial-score)
                                                        (:form-hash-key . ,form)
                                                        (:meaning-hash-key . ,meaning-hash-key)))))
    ;; return cxn-inventory and new cxn
    (add-cxn holophrastic-cxn (copy-fcg-construction-set-without-cxns cxn-inventory))))

(defun compute-meaning-hash-key-from-predicates (meaning-predicates)
  "Computes meaning-hash-key based on set of predicates."
  (loop for predicate in meaning-predicates
        if (and (eql (first predicate) 'bind) ; for IRL
                (= 4 (length predicate)))
          collect (symbol-name (last-elt predicate)) into keys
        else
          collect (symbol-name (first predicate)) into keys
        finally (return (intern (upcase (format nil "~{~a~^-~}" (sort keys #'string<)))))))

(defmethod hash ((cxn construction)
                 (mode (eql :filler-and-linking))
                 &key &allow-other-keys)
  "Hash method for constructions."
  (cond ((or (eql (type-of cxn) 'holophrastic-cxn)
             (and (eql (type-of cxn) 'processing-construction)
                  (eql (type-of (original-cxn cxn)) 'holophrastic-cxn)))
         (list (attr-val cxn :form-hash-key) (attr-val cxn :meaning-hash-key) 'holophrastic-cxns))))

(defmethod hash ((node cip-node)
                 (mode (eql :filler-and-linking)) 
                 &key &allow-other-keys)
  "Hash method for nodes."
  (cond ((and (find 'initial (statuses node))
              (eql '<- (direction (cip node))))
         (list (second (first (unit-feature-value (get-root (fcg-get-transient-unit-structure node)) 'form)))))
        ((and (find 'initial (statuses node))
              (eql '-> (direction (cip node))))
         (list (compute-meaning-hash-key-from-predicates (unit-feature-value  (get-root (fcg-get-transient-unit-structure node)) 'meaning))))))

;; Learning based on existing constructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun learn-through-anti-unification (speech-act cip)
  (let* ((cxn-inventory (original-cxn-set (construction-inventory cip)))
         (form-sequence-predicates `((sequence ,(form speech-act) ,(make-var "left") ,(make-var "right"))))
         (meaning-predicates (fresh-variables (pn::variablify-predicate-network (meaning speech-act)
                                                                                (get-configuration cxn-inventory :meaning-representation-format)))))

    ;; returns list of cxn-inventories, each leading to a solution
    (induce-cxns form-sequence-predicates meaning-predicates cip (get-configuration cxn-inventory :induce-cxns-mode))))

(defgeneric induce-cxns (speech-act-form-predicates speech-act-meaning-predicates cip mode &key fix-cxn-inventory))







(defgeneric anti-unify-form (cxn-form speech-act-form mode &key cxn-inventory &allow-other-keys))

(defmethod anti-unify-form ((cxn-sequence-predicates list)
                            (speech-act-sequence-predicates list)
                            (mode (eql :needleman-wunsch)) &key &allow-other-keys)

  (first (anti-unify-sequences cxn-sequence-predicates speech-act-sequence-predicates)))


(defgeneric anti-unify-meaning (cxn-meaning speech-act-meaning mode &key cxn-inventory &allow-other-keys))

(defmethod anti-unify-meaning ((cxn-meaning-predicates list)
                               (speech-act-meaning-predicates list)
                               (mode (eql :k-swap)) &key cxn-inventory &allow-other-keys)
  (let ((k-swap-au-result
         (first (au-benchmark.msg.kswap-omega:anti-unify-predicate-networks cxn-meaning-predicates speech-act-meaning-predicates
                                                                            :k (get-configuration cxn-inventory :k-swap-k)
                                                                            :w (get-configuration cxn-inventory :k-swap-w)))))

    (make-instance 'predicate-network-au-result 
                   :pattern (au-benchmark.msg.kswap-omega::pattern k-swap-au-result)
                   :source (au-benchmark.msg.kswap-omega::source k-swap-au-result)
                   :generalisation (au-benchmark.msg.kswap-omega::generalisation k-swap-au-result)
                   :pattern-bindings (au-benchmark.msg.kswap-omega::pattern-bindings k-swap-au-result)
                   :source-bindings (au-benchmark.msg.kswap-omega::source-bindings k-swap-au-result)
                   :pattern-delta (au-benchmark.msg.kswap-omega::pattern-delta k-swap-au-result)
                   :source-delta (au-benchmark.msg.kswap-omega::source-delta k-swap-au-result)
                   :cost (au-benchmark.msg.kswap-omega::cost k-swap-au-result))))


(defmethod anti-unify-meaning ((cxn-meaning-predicates list)
                               (speech-act-meaning-predicates list)
                               (mode (eql :exhaustive)) &key &allow-other-keys)

  (first (anti-unify-predicate-network cxn-meaning-predicates speech-act-meaning-predicates)))




(defun learn-cxns-from-au-result (au-form au-meaning fix-cxn-inventory &key integration-cat integration-form-args integration-meaning-args (learn-cxns-from-deltas t))
  (let* ((generalisation-form-args (compute-slot-args au-form))
         (pattern-form-args  (loop for slot-arg in generalisation-form-args
                                   collect (car (rassoc slot-arg (pattern-bindings au-form)))))
         (source-form-args (loop for slot-arg in generalisation-form-args
                                 collect (car (rassoc slot-arg (source-bindings au-form)))))
         
         (generalisation-meaning-args (compute-slot-args au-meaning))
         (pattern-meaning-args (loop for slot-arg in generalisation-meaning-args
                                     collect (car (rassoc slot-arg (pattern-bindings au-meaning)))))
         (source-meaning-args (loop for slot-arg in generalisation-meaning-args
                                    collect (car (rassoc slot-arg (source-bindings au-meaning)))))
         (integration-form-args-slot-1 (when integration-form-args
                                         (loop for arg in integration-form-args
                                               for corresponding-arg-in-form = (or (cdr (assoc arg (pattern-bindings au-form)))
                                                                                   (cdr (assoc arg (source-bindings au-form))))
                                               when (and corresponding-arg-in-form
                                                         (find corresponding-arg-in-form
                                                               (generalisation au-form) :test #'member))
                                                 collect corresponding-arg-in-form)))
         (integration-form-args-slot-2 (when integration-form-args
                                         (loop for arg in integration-form-args
                                               when (find arg (or (pattern-delta au-form) (source-delta au-form)) :test #'member)
                                                 collect arg)))
         (integration-meaning-args-slot-1 (when integration-meaning-args
                                            (loop for arg in integration-meaning-args
                                                  for corresponding-arg-in-meaning = (or (cdr (assoc arg (pattern-bindings au-meaning)))
                                                                                         (cdr (assoc arg (source-bindings au-meaning))))
                                                  when (and corresponding-arg-in-meaning
                                                            (find corresponding-arg-in-meaning (generalisation au-meaning) :test #'member))
                                                    collect corresponding-arg-in-meaning)))
         (integration-meaning-args-slot-2 (when integration-meaning-args
                                            (loop for arg in integration-meaning-args
                                                  when (find arg (or (pattern-delta au-meaning) (source-delta au-meaning)) :test #'member)
                                                    collect arg)))
         (linking-cxn-contributing-meaning-args (loop for arg in integration-meaning-args
                                                           collect (cond ((or (find arg integration-meaning-args-slot-2)
                                                                              (find arg integration-meaning-args-slot-1))
                                                                          arg)
                                                                         ((assoc arg (source-bindings au-meaning))
                                                                          (cdr (assoc arg (source-bindings au-meaning))))
                                                                         ((assoc arg (pattern-bindings au-meaning))
                                                                          (cdr (assoc arg (pattern-bindings au-meaning))))
                                                                         (t
                                                                          (warn "Arg on contributing part of linking cxn not found on conditional part.")))))
         (linking-cxn-contributing-form-args (loop for arg in integration-form-args
                                                           collect (cond ((or (find arg integration-form-args-slot-2)
                                                                              (find arg integration-form-args-slot-1))
                                                                          arg)
                                                                         ((assoc arg (source-bindings au-form))
                                                                          (cdr (assoc arg (source-bindings au-form))))
                                                                         ((assoc arg (pattern-bindings au-form))
                                                                          (cdr (assoc arg (pattern-bindings au-form))))
                                                                         (t
                                                                          (warn "Arg on contributing part of linking cxn not found on conditional part.")))))
         (linking-cxn-form-args-slot-1 (append generalisation-form-args integration-form-args-slot-1))
         (linking-cxn-meaning-args-slot-1 (append generalisation-meaning-args integration-meaning-args-slot-1))
         (linking-cxn-form-args-slot-2 (append generalisation-form-args integration-form-args-slot-2))
         (linking-cxn-meaning-args-slot-2 (append generalisation-meaning-args integration-meaning-args-slot-2))
         
         (generalisation-filler-cxn-form-args (append generalisation-form-args integration-form-args-slot-1))
         (generalisation-filler-cxn-meaning-args (append generalisation-meaning-args integration-meaning-args-slot-1))
         (source-filler-cxn-form-args (append source-form-args integration-form-args-slot-2))
         (source-filler-cxn-meaning-args (append source-meaning-args integration-meaning-args-slot-2))
         (pattern-filler-cxn-form-args (append pattern-form-args integration-form-args-slot-2))
         (pattern-filler-cxn-meaning-args (append pattern-meaning-args integration-meaning-args-slot-2))
   
           
         ;; Create filler constructions for generalisation, pattern and source
         (generalisation-filler-cxn
          (create-filler-cxn (generalisation au-form) (generalisation au-meaning)
                             generalisation-filler-cxn-form-args generalisation-filler-cxn-meaning-args :cxn-inventory fix-cxn-inventory))
         (pattern-filler-cxn (when learn-cxns-from-deltas
                               (create-filler-cxn (pattern-delta au-form) (pattern-delta au-meaning)
                                                  pattern-filler-cxn-form-args pattern-filler-cxn-meaning-args :cxn-inventory fix-cxn-inventory)))
         (source-filler-cxn (when learn-cxns-from-deltas
                              (create-filler-cxn (source-delta au-form) (source-delta au-meaning)
                                                 source-filler-cxn-form-args source-filler-cxn-meaning-args :cxn-inventory fix-cxn-inventory)))
         ;; Create linking construction
         (linking-cxn
          (create-linking-cxn :cxn-inventory fix-cxn-inventory
                              :contributing-form-args linking-cxn-contributing-form-args
                              :contributing-meaning-args linking-cxn-contributing-meaning-args
                              :form-args-slot-1 linking-cxn-form-args-slot-1
                              :form-args-slot-2 linking-cxn-form-args-slot-2
                              :meaning-args-slot-1 linking-cxn-meaning-args-slot-1
                              :meaning-args-slot-2 linking-cxn-meaning-args-slot-2)))

    (when linking-cxn
      (add-cxn linking-cxn fix-cxn-inventory)
      (when (attr-val linking-cxn :cxn-cat)
        (add-category (attr-val linking-cxn :cxn-cat) fix-cxn-inventory)
        (add-link (attr-val linking-cxn :cxn-cat) integration-cat fix-cxn-inventory))
      (add-categories (attr-val linking-cxn :slot-cats) fix-cxn-inventory))
    
    (when (and generalisation-filler-cxn linking-cxn)
      (add-cxn generalisation-filler-cxn fix-cxn-inventory)
      (add-category (attr-val generalisation-filler-cxn :cxn-cat) fix-cxn-inventory)
      (add-link (attr-val generalisation-filler-cxn :cxn-cat) (first (attr-val linking-cxn :slot-cats)) fix-cxn-inventory))

    (when (and pattern-filler-cxn linking-cxn)
      (add-cxn pattern-filler-cxn fix-cxn-inventory)
      (add-category (attr-val pattern-filler-cxn :cxn-cat) fix-cxn-inventory)
      (add-link (attr-val pattern-filler-cxn :cxn-cat) (second (attr-val linking-cxn :slot-cats)) fix-cxn-inventory))

    (when (and source-filler-cxn linking-cxn)
      (add-cxn source-filler-cxn fix-cxn-inventory)
      (add-category (attr-val source-filler-cxn :cxn-cat) fix-cxn-inventory)
      (add-link (attr-val source-filler-cxn :cxn-cat) (second (attr-val linking-cxn :slot-cats)) fix-cxn-inventory))
      
    (values (when linking-cxn (second (attr-val linking-cxn :slot-cats)))
            (or (when (pattern-delta au-form) pattern-filler-cxn-form-args)
                (when (source-delta au-form) source-filler-cxn-form-args))
            (or (when (pattern-delta au-form) pattern-filler-cxn-meaning-args)
                (when (source-delta au-form) source-filler-cxn-meaning-args)))))

;(defun extend-cip-with-holphrase-cxns (cip cxn-inventory))

(defmethod induce-cxns ((speech-act-form-predicates list)
                        (speech-act-meaning-predicates list)
                        (cip construction-inventory-processor)
                        (mode (eql :filler-and-linking))
                        &key fix-cxn-inventory)

  (let* ((cxn-inventory (original-cxn-set (construction-inventory cip)))
        ; (cip-after-)
         (cxns-longest-branch (remove-if #'(lambda (cxn) (eql (type-of (original-cxn cxn)) 'linking-cxn))
                                         (applied-constructions (first (first (sort (mapcar #'upward-branch (get-cip-leaves cip)) #'> :key #'length))))))
         (candidate-cxn-w-au-results (unless cxns-longest-branch
                                       (loop for cxn in (constructions-with-hashed-meaning cxn-inventory)
                                           for cxn-form = (attr-val cxn :form)
                                           for cxn-meaning = (attr-val cxn :meaning)
                                           ;;; Only consider if there are less than x gaps in the form
                                           if (<= (length cxn-form) (+ (get-configuration cxn-inventory :max-nr-of-gaps-in-form-predicates) 1))
                                             collect (let ((au-form-result (anti-unify-form cxn-form speech-act-form-predicates 
                                                                                 (get-configuration cxn-inventory :form-generalisation-mode)
                                                                                 :cxn-inventory cxn-inventory))
                                                           (au-meaning-result (anti-unify-meaning cxn-meaning speech-act-meaning-predicates 
                                                                                       (get-configuration cxn-inventory :meaning-generalisation-mode)
                                                                                       :cxn-inventory cxn-inventory)))
                                                       (when (and ;(> (cost au-form-result) 0)
                                                                  (< (length (pattern-delta au-form-result)) 3)
                                                                  (< (length (source-delta au-form-result)) 3)
                                                                  (< (length (generalisation au-form-result)) 3)
                                                                  ;(> (cost au-meaning-result) 0)
                                                                  )
                                                         (list cxn au-form-result au-meaning-result)))
                                               into au-results
                                           finally (return (first (sort (remove nil au-results) #'< :key #'(lambda (r1)
                                                                                                             (+ (cost (second r1))
                                                                                                                (cost (third r1)))))))))))

    (cond (cxns-longest-branch
           (loop with fix-cxn-inventory = (or fix-cxn-inventory
                                              (copy-fcg-construction-set-without-cxns cxn-inventory))
                 with remaining-form-predicates = speech-act-form-predicates
                 with remaining-meaning-predicates = speech-act-meaning-predicates
                 with integration-form-args = nil
                 with integration-meaning-args = nil
                 with integration-cat = nil
                 for (cxn . remaining-cxns) on cxns-longest-branch
                 for au-form-result = (anti-unify-form (attr-val cxn :form)
                                                       remaining-form-predicates
                                                       (get-configuration cxn-inventory :form-generalisation-mode)
                                                       :cxn-inventory cxn-inventory)
                 for au-meaning-result = (anti-unify-meaning (attr-val cxn :meaning)
                                                             remaining-meaning-predicates
                                                             (get-configuration cxn-inventory :meaning-generalisation-mode)
                                                             :cxn-inventory cxn-inventory)
                 for (resulting-integration-cat
                      resulting-integration-form-args
                      resulting-integration-meaning-args) = (multiple-value-list (learn-cxns-from-au-result au-form-result au-meaning-result fix-cxn-inventory
                                                                                                            :integration-cat integration-cat
                                                                                                            :integration-form-args integration-form-args
                                                                                                            :integration-meaning-args integration-meaning-args
                                                                                                            :learn-cxns-from-deltas (not remaining-cxns)))

                 do (setf remaining-form-predicates (or (pattern-delta au-form-result) (source-delta au-form-result)))
                    (setf remaining-meaning-predicates (or (pattern-delta au-meaning-result) (source-delta au-meaning-result)))
                    (setf integration-cat resulting-integration-cat)
                    (setf integration-form-args resulting-integration-form-args)
                    (setf integration-meaning-args resulting-integration-meaning-args)
                 finally (append-data (blackboard fix-cxn-inventory) :base-cxns cxns-longest-branch)
                         (return (list fix-cxn-inventory))))

          
          (candidate-cxn-w-au-results
           (let* ((pattern-cxn (first candidate-cxn-w-au-results))
                  (au-form (second candidate-cxn-w-au-results))
                  (au-meaning (third candidate-cxn-w-au-results))
                  (generalisation-form-args (compute-slot-args au-form))
                  (pattern-form-args  (loop for slot-arg in generalisation-form-args
                                            collect (car (rassoc slot-arg (pattern-bindings au-form)))))
                  (source-form-args (loop for slot-arg in generalisation-form-args
                                          collect (car (rassoc slot-arg (source-bindings au-form)))))
         
                  (generalisation-meaning-args (compute-slot-args au-meaning))
                  (pattern-meaning-args (loop for slot-arg in generalisation-meaning-args
                                              collect (car (rassoc slot-arg (pattern-bindings au-meaning)))))
                  (source-meaning-args (loop for slot-arg in generalisation-meaning-args
                                             collect (car (rassoc slot-arg (source-bindings au-meaning)))))
           
                  ;; Create filler constructions for generalisation, pattern and source
                  (generalisation-filler-cxn
                   (create-filler-cxn (generalisation au-form)(generalisation au-meaning)
                                      generalisation-form-args generalisation-meaning-args  :cxn-inventory cxn-inventory))
                  (pattern-filler-cxn
                   (create-filler-cxn (pattern-delta au-form) (pattern-delta au-meaning)
                                      pattern-form-args pattern-meaning-args  :cxn-inventory cxn-inventory))
                  (source-filler-cxn
                   (create-filler-cxn (source-delta au-form) (source-delta au-meaning)
                                      source-form-args source-meaning-args :cxn-inventory cxn-inventory))
                  ;; Create linking construction
                  (linking-cxn
                   (create-linking-cxn :form-args-slot-1 generalisation-form-args
                                       :form-args-slot-2 generalisation-form-args
                                       :meaning-args-slot-1 generalisation-meaning-args
                                       :meaning-args-slot-2 generalisation-meaning-args
                                       :cxn-inventory cxn-inventory))
                  (fix-cxn-inventory (or fix-cxn-inventory
                                         (copy-fcg-construction-set-without-cxns cxn-inventory))))

             (when linking-cxn
               (add-cxn linking-cxn fix-cxn-inventory)
               (add-categories (attr-val linking-cxn :slot-cats) fix-cxn-inventory))
    
             (when (and generalisation-filler-cxn linking-cxn)
               (add-cxn generalisation-filler-cxn fix-cxn-inventory)
               (add-category (attr-val generalisation-filler-cxn :cxn-cat) fix-cxn-inventory)
               (add-link (attr-val generalisation-filler-cxn :cxn-cat) (first (attr-val linking-cxn :slot-cats)) fix-cxn-inventory))

             (when (and pattern-filler-cxn linking-cxn)
               (add-cxn pattern-filler-cxn fix-cxn-inventory)
               (add-category (attr-val pattern-filler-cxn :cxn-cat) fix-cxn-inventory)
               (add-link (attr-val pattern-filler-cxn :cxn-cat) (second (attr-val linking-cxn :slot-cats)) fix-cxn-inventory))

             (when (and source-filler-cxn linking-cxn)
               (add-cxn source-filler-cxn fix-cxn-inventory)
               (add-category (attr-val source-filler-cxn :cxn-cat) fix-cxn-inventory)
               (add-link (attr-val source-filler-cxn :cxn-cat) (second (attr-val linking-cxn :slot-cats)) fix-cxn-inventory))

             (append-data (blackboard fix-cxn-inventory) :base-cxns (list pattern-cxn))
        
             (list fix-cxn-inventory))))))


(defun create-filler-cxn (form-sequence-predicates meaning-predicates form-filler-args meaning-filler-args
                                                   &key (cxn-inventory *fcg-constructions*))
  "Create a filler construction that maps between form sequence
predicates and meaning predicates, while adding external form and
meaning args as well as a unique category to the unit that the
construction creates."
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

(defun create-linking-cxn (&key (cxn-inventory *fcg-constructions*) contributing-form-args contributing-meaning-args
                                form-args-slot-1 form-args-slot-2 meaning-args-slot-1 meaning-args-slot-2)
  "Create a linking construction."
  (when (or (and form-args-slot-1 meaning-args-slot-1) (and form-args-slot-2 meaning-args-slot-2))
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




(defun constructions-with-hashed-meaning (cxn-inventory)
  (remove-duplicates (loop for key being the hash-keys of (constructions-hash-table cxn-inventory)
                             when key
                             append (gethash key (constructions-hash-table cxn-inventory)))
                     :test #'eql
                     :key #'name))


















;; Helper functions
;;-------------------------------



(defun make-cxn-name (form-sequence-predicates)
  "Create a unique construction name based on the strings present in form-sequence-predicates."
  (make-const (upcase (substitute #\Space #\- (format nil "~{~a~^_~}-cxn" (mapcar #'second form-sequence-predicates))))))

(defun remove-cxn-tail (string)
  (let ((start-tail (search "-CXN" string)))
    (subseq string 0 start-tail)))

(defun compute-slot-args (au-result)
  (assert (= (length (pattern-bindings au-result)) (length (source-bindings au-result))))
  (loop for (delta-var-source . gen-var-source) in (source-bindings au-result)
        for (delta-var-pattern . nil) in (pattern-bindings au-result)
        when (or (find delta-var-source (source-delta au-result) :test (lambda (x y) (member x y)))
                 (find delta-var-pattern (pattern-delta au-result) :test (lambda (x y) (member x y))))
          collect gen-var-source))



(defmethod make-html-construction-title ((construction construction))
 `((span) 
      ,(format nil "~(~a~)" (name construction))))

(defmethod make-html-construction-title ((construction fcg-construction))
 `((span) 
      ,(format nil "~(~a~)" (name construction))))



(defun fresh-variables (set-of-predicates)
  (labels ((subst-bindings (bindings)
             (loop for predicate in set-of-predicates
                   collect (loop for elem in predicate
                                 for subst = (assoc elem bindings)
                                 if subst collect (cdr subst)
                                 else collect elem))))
    (let* ((all-variables (find-all-anywhere-if #'variable-p set-of-predicates))
           (unique-variables (remove-duplicates all-variables))
           (renamings (loop for var in unique-variables
                            for base-name = (get-base-name var)
                            collect (cons var (internal-symb (make-var base-name))))))
      (values (subst-bindings renamings) renamings))))



