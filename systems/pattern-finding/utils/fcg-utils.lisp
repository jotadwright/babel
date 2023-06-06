(in-package :fcg)

(export '(extract-meaning-predicates extract-form-predicates
          create-cxn-inventory-for-sandbox apply-in-sandbox comprehend-in-sandbox
          ordered-fcg-apply))

(defun initial-node (node)
  "returns the first node in the cip"
  (if (all-parents node)
    (last-elt (all-parents node))
    node))

(defun find-feature-value (feature unit-body)
  (loop for feature-value in unit-body
        when (equal (feature-name feature-value) feature)
        return  (second feature-value)))

(defun find-hashed-feature-value (feature unit-body)
  (loop for feature-value in unit-body
        when (and (equal (first feature-value) 'HASH)
                  (equal (second feature-value) feature))
        return (third feature-value)))

;; ------------------------------
;; + extract meaning predicates +
;; ------------------------------

(defgeneric extract-meaning-predicates (object))

(defmethod extract-meaning-predicates ((cxn fcg-construction))
  (append (mappend #'extract-meaning-predicates (conditional-part cxn))
          (mappend #'extract-meaning-predicates (contributing-part cxn))))

(defmethod extract-meaning-predicates ((unit conditional-unit))
  (append (extract-meaning-predicates (comprehension-lock unit))
          (extract-meaning-predicates (formulation-lock unit))))

(defmethod extract-meaning-predicates ((unit contributing-unit))
  (extract-meaning-predicates (unit-structure unit)))

(defmethod extract-meaning-predicates ((unit-body list))
  (append (find-feature-value 'meaning unit-body)
          (find-hashed-feature-value 'meaning unit-body)))

;; ---------------------------
;; + extract form predicates +
;; ---------------------------

(defgeneric extract-form-predicates (object))

(defmethod extract-form-predicates ((cxn fcg-construction))
  (append (mappend #'extract-form-predicates (conditional-part cxn))
          (mappend #'extract-form-predicates (contributing-part cxn))))

(defmethod extract-form-predicates ((unit conditional-unit))
  (append (extract-form-predicates (comprehension-lock unit))
          (extract-form-predicates (formulation-lock unit))))

(defmethod extract-form-predicates ((unit contributing-unit))
  (extract-form-predicates (unit-structure unit)))

(defmethod extract-form-predicates ((unit-body list))
  (append (find-feature-value 'form unit-body)
          (find-hashed-feature-value 'form unit-body)))


;;
;; comprehend and formulate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comprehend (utterance &key
                                 (cxn-inventory *fcg-constructions*)
                                 (gold-standard-meaning nil)
                                 (silent nil))
  (let* ((de-render-mode (get-configuration cxn-inventory :de-render-mode))
         (meaning-formalism (get-configuration cxn-inventory :meaning-representation))
         (initial-cfs (de-render utterance de-render-mode :cxn-inventory cxn-inventory))
         (processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    ;; Add utterance and meaning to blackboard
    (set-data initial-cfs :utterance
              (pf::form-constraints-with-variables utterance de-render-mode))
    (set-data initial-cfs :meaning
              (pf::meaning-predicates-with-variables gold-standard-meaning meaning-formalism))
    ;; Notification
    (unless silent (notify parse-started (listify utterance) initial-cfs))
    ;; Construction application
    (multiple-value-bind (solution cip)
        (fcg-apply processing-cxn-inventory initial-cfs '<- :notify (not silent))
      (let ((meaning (and solution
                          (extract-meanings
                           (left-pole-structure
                            (car-resulting-cfs
                             (cipn-car solution)))))))
        ;; Notification
        (unless silent (notify parse-finished meaning processing-cxn-inventory))
        ;; Return value
        (values meaning solution cip)))))


(defmethod comprehend-all (utterance &key
                                     (cxn-inventory *fcg-constructions*)
                                     (gold-standard-meaning nil)
                                     (silent nil) (n nil))
  "comprehend the input utterance with a given FCG grammar, obtaining all possible combinations"
  (let* ((de-render-mode (get-configuration cxn-inventory :de-render-mode))
         (meaning-formalism (get-configuration cxn-inventory :meaning-representation))
         (initial-cfs (de-render utterance de-render-mode :cxn-inventory cxn-inventory))
         (processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    
    ;; Add utterance and meaning to blackboard
    (set-data initial-cfs :utterance
              (pf::form-constraints-with-variables utterance de-render-mode))
    (set-data initial-cfs :meaning
              (pf::meaning-predicates-with-variables gold-standard-meaning meaning-formalism))
    ;; Notification
    (unless silent (notify parse-all-started n (listify utterance)))
    ;; Construction application
    (multiple-value-bind (solutions cip)
        (if n
          (fcg-apply-with-n-solutions processing-cxn-inventory initial-cfs '<- n
                                      :notify (not silent))
          (fcg-apply-exhaustively processing-cxn-inventory initial-cfs '<-
                                  :notify (not silent)))
      (let ((meanings (mapcar #'(lambda(solution)
                                  (extract-meanings
                                   (left-pole-structure
                                    (car-resulting-cfs
                                     (cipn-car solution)))))
                              solutions)))
        (unless silent (notify parse-all-finished meanings
                               processing-cxn-inventory))
        (values meanings solutions cip)))))


(defun create-cxn-inventory-for-sandbox (original-cxn-inventory
                                         &key (cxns-to-add nil)
                                         (categories-to-add nil)
                                         (categorial-links-to-add nil)
                                         (category-linking-mode :categories-exist))
  (with-configurations ((cxn-supplier :cxn-supplier-mode)
                        (de-render-mode :de-render-mode)
                        (meaning-representation :meaning-representation))
      original-cxn-inventory
    (let* ((inventory-name (gensym))
           (temp-cxn-inventory
            (eval `(def-fcg-constructions ,inventory-name
                     :cxn-inventory ,inventory-name
                     :hashed t
                     :feature-types ((form set-of-predicates :handle-regex-sequences)
                                     (meaning set-of-predicates)
                                     (form-args sequence)
                                     (meaning-args sequence)
                                     (subunits set)
                                     (footprints set))
                     :fcg-configurations ((:construction-inventory-processor-mode . :heuristic-search)
                                          (:search-algorithm . :best-first)
                                          (:heuristics :nr-of-applied-cxns :nr-of-units-matched) 
                                          (:heuristic-value-mode . :sum-heuristics-and-parent)
                                          (:node-expansion-mode . :full-expansion)
                                          
                                          (:parse-order routine)
                                          (:production-order routine)
                                          (:max-nr-of-nodes . 250)
                                          (:cxn-supplier-mode . ,cxn-supplier)

                                          (:de-render-mode . ,de-render-mode)
                                          (:render-mode . :render-sequences)
                                          
                                          (:category-linking-mode . ,category-linking-mode)
                                          
                                          (:parse-goal-tests :no-applicable-cxns :connected-semantic-network :non-gold-standard-meaning)

                                          (:meaning-representation . ,meaning-representation)
                                          (:update-categorial-links . nil)
                                          (:consolidate-repairs . nil)
                                          (:use-meta-layer . nil)
                                          (:ignore-transitive-closure . t))))))
      (add-categories categories-to-add (categorial-network temp-cxn-inventory)
                      :recompute-transitive-closure nil)
      (dolist (categorial-link categorial-links-to-add)
        (add-categories (list (car categorial-link) (cdr categorial-link)) (categorial-network temp-cxn-inventory)
                        :recompute-transitive-closure nil)
        (add-link (car categorial-link) (cdr categorial-link) (categorial-network temp-cxn-inventory)
                  :recompute-transitive-closure nil))
      (dolist (cxn cxns-to-add)
        (add-cxn cxn temp-cxn-inventory))
      temp-cxn-inventory)))


(defun comprehend-in-sandbox (utterance cxn-inventory
                                        &key
                                        (apply-sequentially nil)
                                        (gold-standard-meaning nil)
                                        (cxns-to-add nil)
                                        (categories-to-add nil)
                                        (categorial-links-to-add nil))
  "Creates a copy of the cxn inventory and applies a list of original cxns. Returns the solution cipn."
  (let ((temp-cxn-inventory
         (create-cxn-inventory-for-sandbox cxn-inventory
                                           :cxns-to-add cxns-to-add
                                           :categories-to-add categories-to-add
                                           :categorial-links-to-add categorial-links-to-add)))
    (if apply-sequentially
      (let* ((initial-cfs (de-render utterance (get-configuration temp-cxn-inventory :de-render-mode)
                                     :cxn-inventory temp-cxn-inventory))
             (initial-node (top-node (create-construction-inventory-processor temp-cxn-inventory
                                                                              (get-configuration
                                                                               temp-cxn-inventory
                                                                               'construction-inventory-processor-mode)
                                                                              :initial-cfs initial-cfs
                                                                              :direction '<-))))
        (ordered-fcg-apply (mapcar #'get-processing-cxn cxns-to-add) initial-node '<- (processing-cxn-inventory temp-cxn-inventory)))
      ;; non-sequential normal comprehend
      (second (multiple-value-list (comprehend utterance :gold-standard-meaning gold-standard-meaning :cxn-inventory temp-cxn-inventory :silent t))))))

(defun apply-in-sandbox (initial-node
                         original-cxn-inventory
                         &key (cxns-to-add nil)
                         (categories-to-add nil)
                         (categorial-links-to-add nil))
  (let ((temp-cxn-inventory
         (create-cxn-inventory-for-sandbox original-cxn-inventory
                                           :cxns-to-add cxns-to-add
                                           :categories-to-add categories-to-add
                                           :categorial-links-to-add categorial-links-to-add
                                           :category-linking-mode :neighbours)))
    (multiple-value-bind (solution cip)
        (fcg-apply (processing-cxn-inventory temp-cxn-inventory)
                   (car-source-cfs (cipn-car initial-node))
                   '<- :notify nil)
      (declare (ignore cip))
      solution)))

(defun ordered-fcg-apply (processing-cxns-to-apply initial-node direction cxn-inventory)
  "Apply a list of processing cxns in the order they appear in the list. Returns the solution cipn."
  (with-disabled-monitor-notifications
    (loop with current-node = initial-node
          for cxn in processing-cxns-to-apply
          do (setf current-node (cip-add-child current-node
                                               (first (fcg-apply cxn (if (initial-node-p current-node)
                                                                       (car-source-cfs (cipn-car (initial-node current-node)))
                                                                       (car-resulting-cfs (cipn-car current-node)))
                                                                 direction
                                                                 :configuration (configuration cxn-inventory)
                                                                 :cxn-inventory cxn-inventory))))
          finally (return current-node))))


(defmethod formulate (meaning &key
                              (cxn-inventory *fcg-constructions*)
                              (gold-standard-utterance nil)
                              (silent nil))
  (let ((initial-cfs (create-initial-structure meaning
                                               (get-configuration cxn-inventory :create-initial-structure-mode)))
        (processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    ;; Add utterance and meaning to blackboard
    (set-data initial-cfs :meanings (if (atom (caar meaning)) (list meaning) meaning))
    (set-data initial-cfs :utterances (listify gold-standard-utterance))
    ;; Notification
    (unless silent (notify produce-started meaning cxn-inventory initial-cfs))
    ;; Construction application
    (multiple-value-bind (solution cip)
        (fcg-apply processing-cxn-inventory initial-cfs '-> :notify (not silent))
      (let ((utterance
             (and solution
                  (or (find-data (goal-test-data solution) 'utterance)
                      (render 
                       (car-resulting-cfs (cipn-car solution)) 
                       (get-configuration cxn-inventory :render-mode)
                      :node solution)))))
        ;; Notification
        (unless silent (notify produce-finished utterance))
        ;; Return value
        (values utterance solution cip)))))





