(in-package :fcg)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extracting form and meaning from fcg-constructions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(extract-meaning-predicates
          extract-form-predicates
          ordered-fcg-apply
          comprehend-in-sandbox
          initial-node
          create-temp-cxn-inventory))

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

;; (extract-meaning-predicates (first (constructions *fcg-constructions*)))

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

;; (extract-form-predicates (first (constructions *fcg-constructions*)))

(defun find-feature-value (feature unit-body)
  (loop for feature-value in unit-body
        when (equal (feature-name feature-value) feature)
        return  (second feature-value)))

(defun find-hashed-feature-value (feature unit-body)
  (loop for feature-value in unit-body
        when (and (equal (first feature-value) 'HASH)
                  (equal (second feature-value) feature))
        return (third feature-value)))

(defun initial-node (node)
  "returns the first node in the cip"
  (if (all-parents node)
    (last-elt (all-parents node))
    node))

(defun create-temp-cxn-inventory (original-cxn-inventory &key (cxns-to-add nil) (categories-to-add nil) (categorial-links-to-add nil))
  (let* ((inventory-name (gensym))
         (temp-cxn-inventory (eval `(def-fcg-constructions
                                       ,inventory-name
                                     :cxn-inventory ,inventory-name
                                     :hashed t
                                     :feature-types ((args sequence)
                                                     (form set-of-predicates)
                                                     (meaning set-of-predicates)
                                                     (subunits set)
                                                     (footprints set))
                                     :fcg-configurations ((:node-tests :restrict-nr-of-nodes :restrict-search-depth :check-duplicate)
                                                          (:cxn-supplier-mode . ,(get-configuration original-cxn-inventory :learner-cxn-supplier))
                                                          (:parse-goal-tests :no-strings-in-root :no-applicable-cxns :connected-semantic-network :connected-structure :non-gold-standard-meaning)
                                                          (:de-render-mode . ,(get-configuration original-cxn-inventory :de-render-mode))
                                                          (:parse-order routine)
                                                          (:max-nr-of-nodes . 250)
                                                          (:production-order routine)
                                                          (:meaning-representation-formalism . ,(get-configuration original-cxn-inventory :meaning-representation))
                                                          (:render-mode . :generate-and-test)
                                                          (:category-linking-mode . :categories-exist)
                                                          (:update-categorial-links . t)
                                                          (:consolidate-repairs . t)
                                                          (:use-meta-layer . nil)
                                                          (:update-categorial-links . nil)
                                                          (:consolidate-repairs . nil)
                                                          (:initial-categorial-link-weight . ,(get-configuration original-cxn-inventory :initial-categorial-link-weight))
                                                          (:ignore-transitive-closure . t)
                                                          (:hash-mode . :hash-string-meaning-lex-id))))))
    (add-categories categories-to-add (categorial-network temp-cxn-inventory) :recompute-transitive-closure nil)
    (dolist (categorial-link categorial-links-to-add)
      (add-link (car categorial-link) (cdr categorial-link) (categorial-network temp-cxn-inventory) :recompute-transitive-closure nil))
    (dolist (cxn cxns-to-add)
      (add-cxn cxn temp-cxn-inventory))
    temp-cxn-inventory))
    

(defmethod comprehend (utterance &key
                                 (cxn-inventory *fcg-constructions*)
                                 (gold-standard-meaning nil)
                                 (silent nil))
  (let ((initial-cfs (de-render utterance (get-configuration cxn-inventory :de-render-mode) :cxn-inventory cxn-inventory))
        (processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    ;; Add utterance and meaning to blackboard
    (set-data initial-cfs :utterances (listify utterance))
    (set-data initial-cfs :meanings (if (atom (caar gold-standard-meaning)) (list gold-standard-meaning) gold-standard-meaning))
    ;; Notification
    (unless silent (notify parse-started (listify utterance) initial-cfs))
    ;; Construction application
    (multiple-value-bind (solution cip)
        (fcg-apply processing-cxn-inventory initial-cfs '<- :notify (not silent))
      (let ((meaning (and solution
                          (extract-meanings
                           (left-pole-structure (car-resulting-cfs (cipn-car solution)))))))
        ;; Notification
        (unless silent (notify parse-finished meaning processing-cxn-inventory))
        ;; Return value
        (values meaning solution cip)))))


(defmethod comprehend-all (utterance &key (cxn-inventory *fcg-constructions*) (gold-standard-meaning nil) (silent nil) (n nil))
  "comprehend the input utterance with a given FCG grammar, obtaining all possible combinations"
  (let ((initial-cfs (de-render utterance (get-configuration cxn-inventory :de-render-mode) :cxn-inventory cxn-inventory))
        (processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    
    ;; Add utterance and meaning to blackboard
    (set-data initial-cfs :utterances (listify utterance))
    (set-data initial-cfs :meanings (if (atom (caar gold-standard-meaning)) (list gold-standard-meaning) gold-standard-meaning))
    
    (unless silent (notify parse-all-started n (listify utterance)))
    (multiple-value-bind (solutions cip)
        (if n
          (fcg-apply-with-n-solutions processing-cxn-inventory initial-cfs '<- n
                                      :notify (not silent))
          (fcg-apply-exhaustively processing-cxn-inventory initial-cfs '<-
                                  :notify (not silent)))
      (let ((meanings (mapcar #'(lambda(solution)
                                  (extract-meanings
                                   (left-pole-structure
                                    (car-resulting-cfs (cipn-car solution)))))
                              solutions)))
        (unless silent (notify parse-all-finished meanings
                               processing-cxn-inventory))
        (values meanings solutions cip)))))


(defun comprehend-in-sandbox (utterance cxn-inventory
                                        &key
                                        (apply-sequentially nil)
                                        (gold-standard-meaning nil)
                                        (cxns-to-add nil)
                                        (categories-to-add nil)
                                        (categorial-links-to-add nil))
  "Creates a copy of the cxn inventory and applies a list of original cxns. Returns the solution cipn."
  (let ((temp-cxn-inventory (create-temp-cxn-inventory cxn-inventory :cxns-to-add cxns-to-add :categories-to-add categories-to-add :categorial-links-to-add categorial-links-to-add)))
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
      (second (multiple-value-list (comprehend utterance :gold-standard-meaning gold-standard-meaning :cxn-inventory temp-cxn-inventory :silent t))))
      ))
  

(defun ordered-fcg-apply (processing-cxns-to-apply initial-node direction cxn-inventory)
  "Apply a list of processing cxns in the order they appear in the list. Returns the solution cipn."
  ;(with-disabled-monitor-notifications
    (loop with current-node = initial-node
          for cxn in processing-cxns-to-apply
          do (setf current-node (fcg::cip-add-child current-node
                                                    (first (fcg-apply cxn (if (initial-node-p current-node)
                                                                            (car-source-cfs (cipn-car (initial-node current-node)))
                                                                            (car-resulting-cfs (cipn-car current-node)))
                                                                                       
                                                                      direction
                                                                      :configuration (configuration cxn-inventory)
                                                                      :cxn-inventory cxn-inventory))))
          finally return current-node));)


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

(defmethod render ((node cip-node) (mode (eql :generate-and-test)) &key &allow-other-keys)
  (render (car-resulting-cfs (cipn-car node)) :generate-and-test))

(defmethod cip-enqueue ((node cip-node) (cip construction-inventory-processor)
                        (mode (eql :add-to-front)))
  (push node (queue cip)))

;; filter the cxns for a specific set
;; #########################################################
;; cxn-supplier-hashed-and-scored-default-cxn-set-only
;; ---------------------------------------------------------

(export '(cxn-supplier-hashed-and-scored-routine-cxn-set-only cxn-supplier-hashed-routine-set-only))


(defclass cxn-supplier-hashed-routine-set-only ()
  ()
  (:documentation "Construction supplier that returns all constructions except incompatible hashed ones."))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :hashed-routine-only)))
  "Creates an instance of the cxn-supplier."
  (make-instance 'cxn-supplier-hashed-routine-set-only))

(defmethod next-cxn ((cxn-supplier cxn-supplier-hashed-routine-set-only) (node cip-node))
  "Returns all constructions that satisfy the hash of the node."
  (all-constructions-of-label-hashed node :routine))
  



(defun constructions-for-application-hashed-and-scored-routine-cxn-set-only (node)
  "computes all constructions that could be applied for this node
   plus nil hashed constructions"
  (let ((constructions
         ;; get all constructions compatible
         ;; with the hashes of the node
         ;; append nil hashed constructions
         (remove-duplicates
          (append
           (loop
            for hash in (hash node (get-configuration node :hash-mode))
            append (gethash hash (constructions-hash-table (construction-inventory node))))
           (gethash nil (constructions-hash-table (construction-inventory node)))))))
    ;; shuffle if requested
    (when (get-configuration node :shuffle-cxns-before-application)
      (setq constructions 
            (shuffle constructions)))
    ;; filter the cxns for a specific cxn set here using (attr-val cxn :label)
    (setq constructions
          (loop for cxn in constructions
                when (equal (attr-val cxn :label) 'routine)
                collect cxn))
    
    ;; sort 
    (setq constructions
          (sort constructions #'> :key #'(lambda (cxn) (attr-val cxn :score))))
    ;; return constructions
    constructions))

(defclass cxn-supplier-hashed-and-scored-routine-cxn-set-only ()
  ((remaining-constructions
    :type list :initarg :remaining-constructions
    :accessor remaining-constructions
    :documentation "A list of constructions that are still to try")))

(defmethod create-cxn-supplier ((node cip-node)
                                (mode (eql :hashed-and-scored-routine-cxn-set-only)))
  (make-instance
   'cxn-supplier-hashed-and-scored-routine-cxn-set-only
   :remaining-constructions (constructions-for-application-hashed-and-scored-routine-cxn-set-only node)))

(defmethod next-cxn ((cxn-supplier cxn-supplier-hashed-and-scored-routine-cxn-set-only)
                     (node cip-node))
  (pop (remaining-constructions cxn-supplier)))


(export '(cxn-supplier-hashed-and-scored-meta-layer-cxn-set-only))

(defun constructions-for-application-hashed-and-scored-meta-layer-cxn-set-only (node)
  "computes all constructions that could be applied for this node
   plus nil hashed constructions"
  (let ((constructions
         ;; get all constructions compatible
         ;; with the hashes of the node
         ;; append nil hashed constructions
         (remove-duplicates
          (append
           (loop
            for hash in (hash node (get-configuration node :hash-mode))
            append (gethash hash (constructions-hash-table (construction-inventory node))))
           (gethash nil (constructions-hash-table (construction-inventory node)))))))
    ;; shuffle if requested
    (when (get-configuration node :shuffle-cxns-before-application)
      (setq constructions 
            (shuffle constructions)))
    ;; filter the cxns for a specific cxn set here using (attr-val cxn :label)
    (setq constructions
          (loop for cxn in constructions
                when (equal (attr-val cxn :label) 'meta-only)
                collect cxn))
    
    ;; sort 
    (setq constructions
          (sort constructions #'> :key #'(lambda (cxn) (attr-val cxn :score))))
    ;; return constructions
    constructions))

(defclass cxn-supplier-hashed-and-scored-meta-layer-cxn-set-only ()
  ((remaining-constructions
    :type list :initarg :remaining-constructions
    :accessor remaining-constructions
    :documentation "A list of constructions that are still to try")))

(defmethod create-cxn-supplier ((node cip-node)
                                (mode (eql :hashed-and-scored-meta-layer-cxn-set-only)))
  (make-instance
   'cxn-supplier-hashed-and-scored-meta-layer-cxn-set-only
   :remaining-constructions (constructions-for-application-hashed-and-scored-meta-layer-cxn-set-only node)))

(defmethod next-cxn ((cxn-supplier cxn-supplier-hashed-and-scored-meta-layer-cxn-set-only)
                     (node cip-node))
  (pop (remaining-constructions cxn-supplier)))