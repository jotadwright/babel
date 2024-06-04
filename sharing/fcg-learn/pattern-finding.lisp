(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Learning the grammar through anti-unification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 4 types cxns:
;; -> filler-cxn (kunnen enkel fillen)  -> externe args maar zelf geen slots
;; -> slot-and-filler-cxn (kunnen fillen en hebben slots) -> interne en externe args
;; -> slot-cxn (hebben enkel slots) -> interne args maar geen externe
;; -> holophrastic-cxn -> geen externe en geen interne args
;; --> geen observatie van connectivity, niet geobserveerd hoe toplevel args ingebed kunnen zitten


(defclass holophrastic-cxn (fcg-construction)
  ())

(defclass filler-cxn (fcg-construction)
  ())

(defclass slot-cxn (fcg-construction)
  ())

(defclass linking-cxn (fcg-construction)
  ())

(defclass slot+filler-cxn (fcg-construction)
  ())



;; Learning holophrastic constructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun learn-holophrastic-cxn (speech-act cxn-inventory)
  "Learn a holophrastic construction from a speech act. Holophrastic constructions have no args."
  (let* ((form-sequence-predicates `((sequence ,(form speech-act) ,(make-var "left") ,(make-var "right"))))
         (meaning-predicates (fresh-variables (pn::variablify-predicate-network (meaning speech-act)
                                                                                (get-configuration cxn-inventory :meaning-representation-format))))
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
                                                        (:entrenchment-score . 0.5)))))
    ;; return cxn-inventory and new cxn
    (add-cxn holophrastic-cxn (copy-fcg-construction-set-without-cxns cxn-inventory))))



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

(defmethod induce-cxns ((speech-act-form-predicates list)
                        (speech-act-meaning-predicates list)
                        (cip construction-inventory-processor)
                        (mode (eql :slot-and-filler))
                        &key fix-cxn-inventory)
  (let* ((cxn-inventory (original-cxn-set (construction-inventory cip)))
         (candidate-cxn-w-au-results (loop for cxn in (constructions-list cxn-inventory)
                                           for cxn-form = (attr-val cxn :form)
                                           for cxn-meaning = (attr-val cxn :meaning)
                                           for au-form-result = (first (anti-unify-sequences speech-act-form-predicates cxn-form))
                                           for au-meaning-result = (first (au-benchmark.msg.kswap-omega:anti-unify-predicate-networks
                                                                           speech-act-meaning-predicates cxn-meaning :k 2 :w 1))
                                           when (and (> (cost au-form-result) 0)
                                                     (> (cost au-meaning-result) 0))
                                           collect (list cxn au-form-result au-meaning-result) into au-results
                                           finally (return (first (sort au-results #'< :key #'(lambda (r1)
                                                                                                (+ (cost (second r1))
                                                                                                   (cost (third r1))))))))))
    (when candidate-cxn-w-au-results
      (let* ((pattern-cxn (first candidate-cxn-w-au-results))
             (au-form (second candidate-cxn-w-au-results))
             (au-meaning (third candidate-cxn-w-au-results))
             (form-slot-args (compute-slot-args au-form))
             (form-filler-args-pattern  (loop for slot-arg in form-slot-args
                                              collect (car (rassoc slot-arg (pattern-bindings au-form)))))
             (form-filler-args-source (loop for slot-arg in form-slot-args
                                            collect (car (rassoc slot-arg (source-bindings au-form)))))
         
             (meaning-slot-args (compute-slot-args au-meaning))
             (meaning-filler-args-pattern (loop for slot-arg in meaning-slot-args
                                                collect (car (rassoc slot-arg (pattern-bindings au-meaning)))))
             (meaning-filler-args-source (loop for slot-arg in meaning-slot-args
                                               collect (car (rassoc slot-arg (source-bindings au-meaning)))))
           
             ;; Create and add filler constructions for pattern and source
             (pattern-filler-cxn
              (create-filler-cxn (pattern-delta au-form) (pattern-delta au-meaning)
                                 form-filler-args-pattern meaning-filler-args-pattern  :cxn-inventory cxn-inventory))
             (source-filler-cxn
              (create-filler-cxn (source-delta au-form) (source-delta au-meaning)
                                 form-filler-args-source meaning-filler-args-source :cxn-inventory cxn-inventory))
             ;; Create and add slot construction
             (slot-cxn
              (create-slot-cxn (generalisation au-form) (generalisation au-meaning) 
                               form-slot-args meaning-slot-args :cxn-inventory cxn-inventory))
             (fix-cxn-inventory (or fix-cxn-inventory
                                    (copy-fcg-construction-set-without-cxns cxn-inventory))))

        (when slot-cxn
          (add-cxn slot-cxn fix-cxn-inventory)
          (add-category (attr-val slot-cxn :cxn-cat) fix-cxn-inventory))
    
        (when (and pattern-filler-cxn slot-cxn)
          (add-cxn pattern-filler-cxn fix-cxn-inventory)
          (add-category (attr-val pattern-filler-cxn :cxn-cat) fix-cxn-inventory)
          (add-link (attr-val pattern-filler-cxn :cxn-cat) (attr-val slot-cxn :cxn-cat) fix-cxn-inventory))

        (when (and source-filler-cxn slot-cxn)
          (add-cxn source-filler-cxn fix-cxn-inventory)
          (add-category (attr-val source-filler-cxn :cxn-cat) fix-cxn-inventory)
          (add-link (attr-val source-filler-cxn :cxn-cat) (attr-val slot-cxn :cxn-cat) fix-cxn-inventory))

        (append-data (blackboard fix-cxn-inventory) :pattern-cxns (list pattern-cxn))
        
        (list fix-cxn-inventory)))))
    
    












#|

(defmethod induce-cxns ((form-meaning-pair-1 list) ;;observation
                        (existing-cxn fcg-construction) &key (cxn-inventory *fcg-constructions*) &allow-other-keys)
  "Learns new constructions through anti-unifying an observation and an holophrase construction."
  
  (let ((au-meaning-results (anti-unify-predicate-network (fresh-variables (meaning form-meaning-pair-1))
                                                          (attr-val existing-cxn :meaning)))
        (au-form-results (anti-unify-sequences (fresh-variables (form form-meaning-pair-1))
                                               (attr-val existing-cxn :sequence))))

    (assert (and au-meaning-results au-form-results))
      
    (let* (;; Calculate args for meaning
           (au-meaning (first au-meaning-results)) ;;!! For now, only the best result is used to learn constructions
           
           (meaning-slot-args (compute-slot-args au-meaning))
           (meaning-filler-args-pattern (loop for slot-arg in meaning-slot-args
                                              collect (car (rassoc slot-arg (pattern-bindings au-meaning)))))
           (meaning-filler-args-source (loop for slot-arg in meaning-slot-args
                                             collect (car (rassoc slot-arg (source-bindings au-meaning)))))
           ;; Calculate args for form
           (au-form (first au-form-results))
           (form-slot-args (compute-slot-args au-form))
           (form-filler-args-pattern  (loop for slot-arg in form-slot-args
                                            collect (car (rassoc slot-arg (pattern-bindings au-form)))))
           (form-filler-args-source (loop for slot-arg in form-slot-args
                                          collect (car (rassoc slot-arg (source-bindings au-form)))))
           ;; Create and add filler constructions for pattern and source
           (pattern-filler-cxn
            (learn-filler-cxn (pattern-delta au-form) (pattern-delta au-meaning)
                              form-filler-args-pattern meaning-filler-args-pattern  :cxn-inventory cxn-inventory))
           (source-filler-cxn
            (learn-filler-cxn (source-delta au-form) (source-delta au-meaning)
                              form-filler-args-source meaning-filler-args-source :cxn-inventory cxn-inventory))
           ;; Create and add slot construction
           (slot-cxn
            (unless (equivalent-slot-cxn-p (generalisation au-form) (generalisation au-meaning) existing-cxn)
              (learn-slot-cxn (generalisation au-form) (generalisation au-meaning) 
                              form-slot-args meaning-slot-args :cxn-inventory cxn-inventory)))
           ;; Always learn a holophrastic construction
           (holophrastic-cxn
            (learn-holophrastic-cxn (form form-meaning-pair-1) (meaning form-meaning-pair-1) :cxn-inventory cxn-inventory)))

      (if slot-cxn
        ;; Create links in the categorial network between existing slot and new filler
        (progn
          (when pattern-filler-cxn
            (add-link (attr-val pattern-filler-cxn :cxn-cat) (attr-val slot-cxn :cxn-cat) cxn-inventory))

          (when source-filler-cxn
            (add-link (attr-val source-filler-cxn :cxn-cat) (attr-val slot-cxn :cxn-cat) cxn-inventory))
        
          (values slot-cxn pattern-filler-cxn source-filler-cxn holophrastic-cxn))

        (progn
          (when pattern-filler-cxn
            (add-link (attr-val pattern-filler-cxn :cxn-cat) (attr-val existing-cxn :cxn-cat) cxn-inventory))

          (when source-filler-cxn
            (add-link (attr-val source-filler-cxn :cxn-cat) (attr-val existing-cxn :cxn-cat) cxn-inventory))

          (values existing-cxn pattern-filler-cxn source-filler-cxn holophrastic-cxn)

          )))))

|#


(defun create-filler-cxn (form-sequence-predicates meaning-predicates form-filler-args meaning-filler-args
                                                   &key (cxn-inventory *fcg-constructions*))
  "Create a filler construction that maps between form sequence
predicates and meaning predicates, while adding external form and
meaning args as well as a unique category to the unit that the
construction creates."
  (when (and form-sequence-predicates meaning-predicates)
    (let* ((cxn-name (make-cxn-name form-sequence-predicates))
           (filler-cat (make-const (upcase (format nil "~a-filler-cat" (remove-cxn-tail (symbol-name cxn-name))))))
           (unit-name (make-var "filler-unit")))

      (make-instance 'filler-cxn
                     :name cxn-name
                     :contributing-part (list (make-instance 'contributing-unit
                                                             :name unit-name
                                                             :unit-structure `((category ,filler-cat)
                                                                               (form-args ,form-filler-args)
                                                                               (meaning-args ,meaning-filler-args))))
                     :conditional-part `(,(make-instance 'conditional-unit
                                                         :name unit-name
                                                         :formulation-lock `((HASH meaning ,meaning-predicates))
                                                         :comprehension-lock `((HASH form ,form-sequence-predicates))))
                     :cxn-inventory cxn-inventory
                     :feature-types (feature-types cxn-inventory)
                     :attributes `((:form . ,(add-slot-wildcard-to-sequence-predicates form-sequence-predicates))
                                   (:meaning . ,meaning-predicates)
                                   (:form-args . ,form-filler-args)
                                   (:meaning-args . ,meaning-filler-args)
                                   (:cxn-cat . ,filler-cat)
                                   (:entrenchment-score . 0.5))))))

(defun create-slot-cxn (form-sequence-predicates meaning-predicates form-slot-args meaning-slot-args
                                                 &key (cxn-inventory *fcg-constructions*))
  "Create a slot construction."
  (when (and (or meaning-predicates meaning-slot-args)
             (or form-sequence-predicates form-slot-args))
    (let* ((cxn-name (make-cxn-name form-sequence-predicates))
           (slot-cat (make-const (upcase (format nil "~a-slot-cat" (remove-cxn-tail (symbol-name cxn-name))))))
           (slot-unit-name (make-var "slot-unit"))
           (super-unit-name (make-var "parent-unit")))

      (make-instance 'slot-cxn
                     :name cxn-name
                     :contributing-part (list (make-instance 'contributing-unit
                                                             :name super-unit-name
                                                             :unit-structure `((subunits ,(list slot-unit-name)))))
                     :conditional-part (list (make-instance 'conditional-unit
                                                            :name super-unit-name
                                                            :formulation-lock `((HASH meaning ,meaning-predicates))
                                                            :comprehension-lock `((HASH form ,form-sequence-predicates)))
                                             (make-instance 'conditional-unit
                                                            :name slot-unit-name
                                                            :formulation-lock `((category ,slot-cat)
                                                                                (form-args ,form-slot-args)
                                                                                (meaning-args ,meaning-slot-args))
                                                            :comprehension-lock `((category ,slot-cat)
                                                                                  (form-args ,form-slot-args)
                                                                                  (meaning-args ,meaning-slot-args))))
                     :cxn-inventory cxn-inventory
                     :feature-types (feature-types cxn-inventory)
                     :attributes `((:form . ,(add-slot-wildcard-to-sequence-predicates form-sequence-predicates))
                                   (:meaning . ,meaning-predicates)
                                   (:cxn-cat . ,slot-cat)
                                   (:entrenchment-score . 0.5))))))


;; Learning based on existing slot construction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

(defmethod induce-cxns ((form-meaning-pair-1 list) ;;observation
                        (existing-cxn slot-cxn) &key (cxn-inventory *fcg-constructions*) &allow-other-keys)
  
  "Learns new constructions through anti-unifying an observation and an existing slot construction."

  (let ((au-meaning-results (anti-unify-predicate-network (fresh-variables (meaning form-meaning-pair-1))
                                                          (attr-val existing-cxn :meaning)))
        (au-form-results (anti-unify-sequences (fresh-variables (form form-meaning-pair-1))
                                               (attr-val existing-cxn :sequence))))

    (assert (and au-meaning-results au-form-results))
      
    (let* (;; Calculate args for meaning
           (au-meaning (first au-meaning-results)) ;;!! For now, only the best result is used to learn constructions
           (meaning-slot-args (compute-slot-args au-meaning))
           (meaning-filler-args-pattern (loop for slot-arg in meaning-slot-args
                                              collect (car (rassoc slot-arg (pattern-bindings au-meaning)))))
           (meaning-filler-args-source (loop for slot-arg in meaning-slot-args
                                             collect (car (rassoc slot-arg (source-bindings au-meaning)))))
           
           ;; Calculate args for form
           (au-form (first au-form-results))
           (form-slot-args (compute-slot-args au-form))
           (form-filler-args-pattern  (loop for slot-arg in form-slot-args
                                            collect (car (rassoc slot-arg (pattern-bindings au-form)))))
           (form-filler-args-source (loop for slot-arg in form-slot-args
                                          collect (car (rassoc slot-arg (source-bindings au-form)))))
           
           ;; Create and add filler construction for pattern
           (pattern-filler-cxn
            (learn-filler-cxn (pattern-delta au-form) (pattern-delta au-meaning)
                              form-filler-args-pattern meaning-filler-args-pattern  :cxn-inventory cxn-inventory))
           
           ;; Create and add slot construction
           (slot-cxn
            (unless (equivalent-slot-cxn-p (generalisation au-form) (generalisation au-meaning) existing-cxn)
              (learn-slot-cxn (generalisation au-form) (generalisation au-meaning) 
                              form-slot-args meaning-slot-args :cxn-inventory cxn-inventory)))

           (source-filler-cxn
            (when slot-cxn
              (learn-filler-cxn (source-delta au-form) (source-delta au-meaning)
                                form-filler-args-source meaning-filler-args-source :cxn-inventory cxn-inventory)))
           
           ;; Always learn a holophrastic construction
           (holophrastic-cxn
            (learn-holophrastic-cxn (form form-meaning-pair-1) (meaning form-meaning-pair-1) :cxn-inventory cxn-inventory)))

      (assert pattern-filler-cxn)

      (if slot-cxn ;;new slot cxn learnt
        (progn
          (add-link (attr-val pattern-filler-cxn :cxn-cat) (attr-val slot-cxn :cxn-cat) cxn-inventory)
          (when source-filler-cxn
            (add-link (attr-val source-filler-cxn :cxn-cat) (attr-val slot-cxn :cxn-cat) cxn-inventory))
          (values slot-cxn pattern-filler-cxn source-filler-cxn holophrastic-cxn))
        (progn ;; only filler learnt
          (add-link (attr-val pattern-filler-cxn :cxn-cat) (attr-val existing-cxn :cxn-cat) cxn-inventory)
          (values existing-cxn pattern-filler-cxn nil holophrastic-cxn))))))


|#

;; Learning based on existing filler construction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defmethod induce-cxns ((form-meaning-pair-1 list) ;;observation
                        (existing-cxn filler-cxn) &key (cxn-inventory *fcg-constructions*) &allow-other-keys)
  "Learns new constructions through anti-unifying an observation and an existing filler construction."

  (let ((au-meaning-results (anti-unify-predicate-network (fresh-variables (meaning form-meaning-pair-1))
                                                          (attr-val existing-cxn :meaning)))
          (au-form-results (anti-unify-sequences (fresh-variables (form form-meaning-pair-1))
                                                 (attr-val existing-cxn :sequence))))

    (assert (and au-meaning-results au-form-results))
      
    (let* (;; Calculate args for meaning
           (au-meaning (first au-meaning-results)) ;;!! For now, only the best result is used to learn constructions
           (meaning-slot-args (compute-slot-args au-meaning))
           (meaning-filler-args-pattern (loop for slot-arg in meaning-slot-args
                                              collect (car (rassoc slot-arg (pattern-bindings au-meaning)))))
           ;; Calculate args for form
           (au-form (first au-form-results))
           (form-slot-args (compute-slot-args au-form))
           (form-filler-args-pattern  (loop for slot-arg in form-slot-args
                                            collect (car (rassoc slot-arg (pattern-bindings au-form)))))
           ;; Create and add slot construction
           (slot-cxn
            (learn-slot-cxn (pattern-delta au-form) (pattern-delta au-meaning)
                            form-filler-args-pattern meaning-filler-args-pattern  :cxn-inventory cxn-inventory))
           ;; Always learn a holophrastic construction
           (holophrastic-cxn
            (learn-holophrastic-cxn (form form-meaning-pair-1) (meaning form-meaning-pair-1) :cxn-inventory cxn-inventory)))

      (add-link (attr-val existing-cxn :cxn-cat) (attr-val slot-cxn :cxn-cat) cxn-inventory)
        
      (values slot-cxn existing-cxn nil holophrastic-cxn))))


|#

(defgeneric anti-unify-form (speech-act-form cxn-form mode &key cxn-inventory &allow-other-keys))

(defmethod anti-unify-form ((speech-act-sequence-predicates list)
                            (cxn-sequence-predicates list)
                            (mode (eql :needleman-wunsch)) &key &allow-other-keys)

  (first (anti-unify-sequences speech-act-sequence-predicates cxn-sequence-predicates)))


(defgeneric anti-unify-meaning (speech-act-meaning cxn-meaning mode &key cxn-inventory &allow-other-keys))

(defmethod anti-unify-meaning ((speech-act-meaning-predicates list)
                               (cxn-meaning-predicates list)
                               (mode (eql :k-swap)) &key cxn-inventory &allow-other-keys)
  (let ((k-swap-au-result
         (first (au-benchmark.msg.kswap-omega:anti-unify-predicate-networks speech-act-meaning-predicates cxn-meaning-predicates
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


(defmethod anti-unify-meaning ((speech-act-meaning-predicates list)
                               (cxn-meaning-predicates list)
                               (mode (eql :exhaustive)) &key &allow-other-keys)

  (first (anti-unify-predicate-network speech-act-meaning-predicates cxn-meaning-predicates)))




(defmethod induce-cxns ((speech-act-form-predicates list)
                        (speech-act-meaning-predicates list)
                        (cip construction-inventory-processor)
                        (mode (eql :filler-and-linking))
                        &key fix-cxn-inventory)

  (let* ((cxn-inventory (original-cxn-set (construction-inventory cip)))
         (candidate-cxn-w-au-results (loop for cxn in (constructions-with-hashed-meaning cxn-inventory)
                                           for cxn-form = (attr-val cxn :form)
                                           for cxn-meaning = (attr-val cxn :meaning)
                                           ;;; Only consider if there are less than x gaps in the form
                                           if (<= (length cxn-form) (+ (get-configuration cxn-inventory :max-nr-of-gaps-in-form-predicates) 1))
                                             collect (let ((au-form-result (anti-unify-form speech-act-form-predicates cxn-form
                                                                                 (get-configuration cxn-inventory :form-generalisation-mode)
                                                                                 :cxn-inventory cxn-inventory))
                                                           (au-meaning-result (anti-unify-meaning speech-act-meaning-predicates cxn-meaning
                                                                                       (get-configuration cxn-inventory :meaning-generalisation-mode)
                                                                                       :cxn-inventory cxn-inventory)))
                                                       (when (and (> (cost au-form-result) 0)
                                                                  (> (cost au-meaning-result) 0))
                                                         (list cxn au-form-result au-meaning-result)))
                                               into au-results
                                           finally (return (first (sort (remove nil au-results) #'< :key #'(lambda (r1)
                                                                                                             (+ (cost (second r1))
                                                                                                                (cost (third r1))))))))))
    (when candidate-cxn-w-au-results
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
              (create-linking-cxn generalisation-form-args generalisation-meaning-args :cxn-inventory cxn-inventory))
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
        
        (list fix-cxn-inventory)))))


(defun create-linking-cxn (form-args meaning-args  &key (cxn-inventory *fcg-constructions*))
  "Create a linking construction."
  (when (and meaning-args form-args)
    (let* ((cxn-name (make-id 'linking-cxn))
           (slot-cat-1 (make-id 'slot-cat))
           (slot-cat-2 (make-id 'slot-cat))
           (parent-unit-name (make-var "linking-unit"))
           (slot-unit-1-name (make-var "slot-unit"))
           (slot-unit-2-name (make-var "slot-unit")))

      (make-instance 'linking-cxn
                     :name cxn-name
                     :contributing-part (list (make-instance 'contributing-unit
                                                             :name parent-unit-name
                                                             :unit-structure `((subunits ,(list slot-unit-1-name slot-unit-2-name)))))
                     :conditional-part (list (make-instance 'conditional-unit
                                                            :name slot-unit-1-name
                                                            :formulation-lock `((category ,slot-cat-1)
                                                                                (form-args ,form-args)
                                                                                (meaning-args ,meaning-args))
                                                            :comprehension-lock `((category ,slot-cat-1)
                                                                                  (form-args ,form-args)
                                                                                  (meaning-args ,meaning-args)))
                                             (make-instance 'conditional-unit
                                                            :name slot-unit-2-name
                                                            :formulation-lock `((category ,slot-cat-2)
                                                                                (form-args ,form-args)
                                                                                (meaning-args ,meaning-args))
                                                            :comprehension-lock `((category ,slot-cat-2)
                                                                                  (form-args ,form-args)
                                                                                  (meaning-args ,meaning-args))))
                     :cxn-inventory cxn-inventory
                     :feature-types (feature-types cxn-inventory)
                     :attributes `((:form-args . ,form-args)
                                   (:meaning-args . ,meaning-args)
                                   (:slot-cats ,slot-cat-1 ,slot-cat-2)
                                   (:entrenchment-score . 0.5))))))




(defun constructions-with-hashed-meaning (cxn-inventory)
  (remove-duplicates (loop for key being the hash-keys of (constructions-hash-table cxn-inventory)
                             when key
                             append (gethash key (constructions-hash-table cxn-inventory)))
                     :test #'eql
                     :key #'name))


















;; Helper functions
;;-------------------------------

(defun add-slot-wildcard-to-sequence-predicates (sequence-predicates)
  (if (= (length sequence-predicates) 2)

    (let ((left-boundary-wildcard (fourth (first sequence-predicates)))
          (right-boundary-wildcard (third (second sequence-predicates))))
    
      (list (first sequence-predicates)
            `(sequence "_" ,left-boundary-wildcard ,right-boundary-wildcard)
            (second sequence-predicates)))

    sequence-predicates))



(defun compute-meaning-args-slot-and-filler-cxn (base-cxn au-meaning)

  (let* ((meaning-args-cxn (second (fcg-unit-feature (first (contributing-part base-cxn)) 'meaning-args)))
         (meaning-predicates-cxn (loop for unit in (conditional-part base-cxn)
                                       when (fcg-unit-feature unit 'meaning t)
                                         do (return (second (cdr (fcg-unit-feature unit 'meaning t))))))
         (meaning-bindings (first (unify (cons '== (intersection meaning-predicates-cxn (generalisation au-meaning) :test #'unify ))
                                         (generalisation au-meaning))))) ;;multiple solutions?

      (loop for arg in meaning-args-cxn
              if (assoc arg meaning-bindings)
                collect (cdr (assoc arg meaning-bindings))
              else collect arg)))


(defun make-cxn-name (form-sequence-predicates)
  "Create a unique construction name based on the strings present in form-sequence-predicates."
  (make-const (upcase (string-replace (format nil "~{~a~^_~}-cxn" (mapcar #'second form-sequence-predicates)) " " "-")))) 

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

#|
(defun form (observation)
  (let ((raw-observed-form (if (consp (first observation))
                             (cdr (assoc :form observation))
                             (first observation ))))
    (if (stringp raw-observed-form)
      `((sequence ,raw-observed-form ?left ?right))
      (progn
        (assert (eq (caar raw-observed-form) 'sequence))
        raw-observed-form))))

(defun form-string (observation)
  (let ((raw-observed-form (cdr (assoc :form observation))))
    (if (stringp raw-observed-form)
      raw-observed-form
      (progn
        (assert (eq (caar raw-observed-form) 'sequence))
        (second (first raw-observed-form))))))

(defun meaning (observation)
  (if (consp (first observation))
    (cdr (assoc :meaning observation))
    (rest observation)))
|#

(defmethod make-html-construction-title ((construction construction))
 `((span) 
      ,(format nil "~(~a~)" (name construction))))

(defmethod make-html-construction-title ((construction fcg-construction))
 `((span) 
      ,(format nil "~(~a~)" (name construction))))


(defun equivalent-slot-cxn-p (form-predicates meaning-predicates existing-cxn)
  "Existing construction has exactly the same form and meaning
predicates as form-predicates and meaning-predicates."
  (when (eql (type-of existing-cxn) 'slot-cxn)
    (and (unify form-predicates
                (remove-if #'(lambda (sequence-predicate)
                              (find "_" (second sequence-predicate) :test #'string=))
                               (attr-val existing-cxn :sequence)))
         (amr::equivalent-predicate-networks meaning-predicates
                                             (attr-val existing-cxn :meaning)))))

(defun equivalent-holophrastic-cxn? (form-predicates meaning-predicates cxn-inventory)
  "Retrieves a holophrastic cxn from the cxn-inventory with the same form and meaning
predicates as form-predicates and meaning-predicates."

  (loop for construction in (constructions cxn-inventory)
        when (and (eql (type-of construction) 'holophrastic-cxn)
                  (unify form-predicates (attr-val construction :sequence))
                  (amr::equivalent-predicate-networks meaning-predicates
                                             (attr-val construction :meaning)))
          do (return construction)))


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



(defgeneric equivalent-cxn (cxn-1 cxn-2))

(defmethod equivalent-cxn ((cxn-1 t) (cxn-2 t))
  nil)

(defmethod equivalent-cxn ((cxn-1 linking-cxn) (cxn-2 linking-cxn))
  (let ((meaning-args-cxn-1 (attr-val cxn-1 :meaning-args))
        (meaning-args-cxn-2 (attr-val cxn-2 :meaning-args))
        (form-args-cxn-1 (attr-val cxn-1 :form-args))
        (form-args-cxn-2 (attr-val cxn-2 :form-args)))
    
    (and (= (length meaning-args-cxn-1) (length meaning-args-cxn-2))
         (= (length form-args-cxn-1 )(length form-args-cxn-2)))))
  

  
(defmethod equivalent-cxn ((cxn-1 filler-cxn) (cxn-2 filler-cxn))
  (let ((meaning-cxn-1 (attr-val cxn-1 :meaning))
        (meaning-cxn-2 (attr-val cxn-2 :meaning))
        (form-cxn-1 (attr-val cxn-1 :form))
        (form-cxn-2 (attr-val cxn-2 :form))
        (meaning-args-cxn-1 (attr-val cxn-1 :meaning-args))
        (meaning-args-cxn-2 (attr-val cxn-2 :meaning-args))
        (form-args-cxn-1 (attr-val cxn-1 :form-args))
        (form-args-cxn-2 (attr-val cxn-2 :form-args)))
    
    (and (= (length meaning-cxn-1 )(length meaning-cxn-2))
         (= (length form-cxn-1 )(length form-cxn-2))
         (= (length meaning-args-cxn-1) (length meaning-args-cxn-2))
         (= (length form-args-cxn-1 )(length form-args-cxn-2))
         (let ((bindings-meaning (pn::equivalent-predicate-networks meaning-cxn-1 meaning-cxn-2)))
           (when bindings-meaning (loop
                                     for arg-1 in meaning-args-cxn-1
                                     for arg-2 in meaning-args-cxn-2
                                     always (eql (cdr (assoc arg-1 bindings-meaning)) arg-2))))
         (let ((bindings-form (pn::equivalent-predicate-networks form-cxn-1 form-cxn-2)))
           (when bindings-form (loop
                                  for arg-1 in form-args-cxn-1
                                  for arg-2 in form-args-cxn-2
                                  always (eql (cdr (assoc arg-1 bindings-form)) arg-2)))))))

