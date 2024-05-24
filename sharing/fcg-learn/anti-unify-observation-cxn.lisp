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

(defclass slot+filler-cxn (fcg-construction)
  ())


(defgeneric learn-cxns (speech-act cxn-inventory &key &allow-other-keys)
  (:documentation "Learns constructions based on speech-act and cxn-inventory. Ret"))


;; Grammar configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions empty-cxn-inventory
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (meaning set-of-predicates)
                  (form-args sequence)
                  (meaning-args sequence)
                  (subunits set)
                  (footprints set))
  :hashed t
  :fcg-configurations (;; to activate heuristic search
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       (:cxn-supplier-mode . :all-cxns) 
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched) ;; list of heuristic functions (modes of #'apply-heuristic)
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                     ;  (:hash-mode . :hash-sequence-meaning)
                       (:diagnostics diagnose-gold-standard-not-in-search-space)
                       (:repairs learn-cxns)
                       (:learning-mode . :pattern-finding)
                       (:alignment-mode . :no-alignment)
                       (:best-solution-mode . :highest-average-entrenchment-score)
                       (:consolidate-repairs . t)
                       (:de-render-mode . :de-render-sequence)
                       (:render-mode . :render-sequences)
                       (:category-linking-mode . :neighbours)
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network :no-sequence-in-root)
                       (:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure))
  :visualization-configurations ((:show-constructional-dependencies . nil)
                                 (:show-categorial-network . t)))



;; Learning holophrastic constructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod induce-cxns ((form-meaning-pair-1 list)
                        (form-meaning-pair-2 null) &key (cxn-inventory *fcg-constructions*) &allow-other-keys)
  "Learn a holophrastic construction for a given observation."
  (learn-holophrastic-cxn (fresh-variables (form form-meaning-pair-1))
                          (fresh-variables (meaning form-meaning-pair-1))
                          :cxn-inventory cxn-inventory))

(defun learn-holophrastic-cxn (form-sequence-predicates meaning-predicates &key (cxn-inventory *fcg-constructions*))
  "Create a holophrastic construction and add it to the construction inventory. Holophrastic constructions have no args."
  
  (assert (= (length form-sequence-predicates) 1))

  (unless (equivalent-holophrastic-cxn? form-sequence-predicates meaning-predicates cxn-inventory)
    
    (let ((holophrastic-cxn
           (create-holophrastic-cxn form-sequence-predicates meaning-predicates :cxn-inventory cxn-inventory)))

      (assert holophrastic-cxn)
      (add-cxn holophrastic-cxn cxn-inventory)
    
      holophrastic-cxn)))

(defun create-holophrastic-cxn (form-sequence-predicates meaning-predicates &key (cxn-inventory *fcg-constructions*))
  "Create a holophrastic construction that maps between form sequence predicates and meaning predicates."
  
  (make-instance 'holophrastic-cxn
                 :name (make-cxn-name form-sequence-predicates)
                 :conditional-part (list (make-instance 'conditional-unit
                                                        :name (make-var "holophrastic-unit")
                                                        :formulation-lock `((HASH meaning ,meaning-predicates))
                                                        :comprehension-lock `((HASH form ,form-sequence-predicates))))
                 :cxn-inventory cxn-inventory
                 :feature-types (feature-types cxn-inventory)
                 :attributes `((:sequence . ,form-sequence-predicates)
                               (:meaning . ,meaning-predicates)
                               (:entrenchment-score . 0.5))))






;; Learning based on existing holophrase construction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


(defun learn-filler-cxn (form-sequence-predicates meaning-predicates form-filler-args meaning-filler-args
                                                  &key (cxn-inventory *fcg-constructions*))
  "Create a filler construction and add it to the construction
inventory. Filler constructions have external args but no slots. They
can fill slots of other constructions. They are completely substantive."
  (when meaning-predicates
    (let ((filler-cxn
           (create-filler-cxn form-sequence-predicates meaning-predicates
                              form-filler-args meaning-filler-args :cxn-inventory cxn-inventory)))
      
      (assert filler-cxn)
      
      (add-cxn filler-cxn cxn-inventory)
      (add-category (attr-val filler-cxn :cxn-cat) cxn-inventory)
        
      filler-cxn)))

(defun create-filler-cxn (form-sequence-predicates meaning-predicates form-filler-args meaning-filler-args
                                                   &key (cxn-inventory *fcg-constructions*))
  "Create a filler construction that maps between form sequence
predicates and meaning predicates, while adding external form and
meaning args as well as a unique category to the unit that the
construction creates."
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
                   :attributes `((:sequence . ,(add-slot-wildcard-to-sequence-predicates form-sequence-predicates))
                                 (:meaning . ,meaning-predicates)
                                 (:cxn-cat . ,filler-cat)))))



(defun learn-slot-cxn (form-sequence-predicates meaning-predicates form-slot-args meaning-slot-args
                                                &key (cxn-inventory *fcg-constructions*))
  "Create a slot construction and add it to the construction
inventory. Slot constructions have only internal args. They cannot
fill slots of other constructions. They are partially substantive,
partially schematic."
  (let ((slot-cxn
          (create-slot-cxn form-sequence-predicates meaning-predicates form-slot-args meaning-slot-args)))

    (assert slot-cxn)
    
    (add-cxn slot-cxn cxn-inventory)
    (add-category (attr-val slot-cxn :cxn-cat) cxn-inventory)
      
    slot-cxn))


(defun create-slot-cxn (form-sequence-predicates meaning-predicates form-slot-args meaning-slot-args
                                                 &key (cxn-inventory *fcg-constructions*))
  "Create a slot construction."
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
                   :attributes `((:sequence . ,(add-slot-wildcard-to-sequence-predicates form-sequence-predicates))
                                 (:meaning . ,meaning-predicates)
                                 (:cxn-cat . ,slot-cat)))))


;; Learning based on existing slot construction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
  (make-const (upcase (string-replace (format nil "~{~a~^_~}-cxn" (mapcar #'second form-sequence-predicates)) " " ""))))

(defun remove-cxn-tail (string)
  (let ((start-tail (search "-CXN" string)))
    (subseq string 0 start-tail)))

(defun compute-slot-args (au-result)
  (assert (= (length (pattern-bindings au-result)) (length (source-bindings au-result))))
  (loop for (delta-var-source . gen-var-source)  in (source-bindings au-result)
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

