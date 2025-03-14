(in-package :fcg)
;; ! SPECIALISES METHODS IN :fcg

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ;;
;;  Diagnostics and repairs  ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Diagnostics ;;
;;;;;;;;;;;;;;;;;

(defclass diagnose-cip-conceptualisation-success (fcg::diagnostic)
  ((trigger :initform 'routine-processing-finished))
  (:documentation "Diagnostic called on cip after routine processing, checking if there is success somewhere in the tree."))

(defclass diagnose-cip-interpretation-success (fcg::diagnostic)
  ((trigger :initform 'routine-processing-finished))
  (:documentation "Diagnostic called on cip after routine processing, checking if there is success somewhere in the tree."))

(defmethod diagnose ((diagnostic diagnose-cip-conceptualisation-success) (cipn cip-node)
                     &key &allow-other-keys)
  "Check whether there are succeeded nodes in cip"
  (when (not (find 'succeeded (statuses cipn)))
    (make-instance 'no-cxn-to-conceptualise-topic)))

(defmethod diagnose ((diagnostic diagnose-cip-interpretation-success) (cipn cip-node)
                     &key &allow-other-keys)
  "Check whether there are succeeded nodes in cip"
  (when (not (find 'succeeded (statuses cipn)))
    (make-instance 'no-cxn-to-interpret-utterance)))


;; Problems ;;
;;;;;;;;;;;;;;

(defclass no-cxn-to-conceptualise-topic (problem)
  ()
  (:documentation "Problem class that specifies that agent has no cxn to conceptualise the topic."))

(defclass no-cxn-to-interpret-utterance (problem)
  ()
  (:documentation "Problem class that specifies that agent has no cxn to interpret the utterance."))


;; Fixes ;;
;;;;;;;;;;;

(defclass invention-fix (fix)
  ()
  (:documentation "Class for fixes created by repair-through-invention"))

(defclass adoption-fix (fix)
  ()
  (:documentation "Class for fixes created by repair-through-adoption"))

(defclass form-similarity-fix (fix)
  ()
  (:documentation "Class for fixes created by repair-through-form-similarity"))

;; Repairs ;;
;;;;;;;;;;;;;


(defclass repair-through-invention (repair)
  ((trigger :initform 'routine-processing-finished))
  (:documentation "Repair that invents"))

(defclass repair-through-form-similarity (repair)
  ((trigger :initform 'routine-processing-finished))
  (:documentation "Repair that finds most similar form"))

(defclass repair-through-adoption (repair)
  ((trigger :initform 'feedback-received))
  (:documentation "Repair that adopts"))

(defmethod repair ((repair repair-through-invention)
                   (problem no-cxn-to-conceptualise-topic)
                   (cipn cip-node)
                   &key &allow-other-keys)
  "Invent a construction through composition."
  (let* (;; copy cxn-inventory
         (cxn-inventory-copy (copy-fcg-construction-set-without-cxns (original-cxn-set (construction-inventory cipn))))

         ;; get topic from blackboard, in canonical naming game, only one object as the topic
         (topic (get-data (blackboard (construction-inventory cipn)) :topic)) 
         (topic-entity (first (crs-conventionality::entities topic))) 
         
         ;; get primitive inventory and scene
         (primitive-inventory (get-data (blackboard (construction-inventory cipn)) :primitive-inventory))
         (scene (get-data (blackboard (construction-inventory cipn)) :scene))

         ;; take scene as partial program 
         (partial-program `((bind ,(type-of scene) ?scene ,scene)))

         ;; start composing
         (composition-result (crs-conventionality::compose-program topic-entity partial-program primitive-inventory)))

    ;; when a composition result is found, get irl program and make a cxn
    (when (find 'irl::solution (irl::statuses (irl::node (first composition-result))))
      
      (let* (;; combine irl-program and bind-statements into the meaning
             (irl-program (irl::irl-program (irl::chunk (first composition-result)))) 
             (bind-statements (irl::bind-statements (first composition-result)))
             (meaning (append irl-program bind-statements))

             ;; invent a form
             (form (make-word))

             ;; make a cxn based on the topic, meaning and form
             (cxn (crs-conventionality::make-naming-game-cxn topic meaning cxn-inventory-copy form 0.5)))

        ;; add the cxn to the cxn-inventory
        (add-cxn cxn cxn-inventory-copy)
        ;; add the cxn to the fix
        (make-instance 'invention-fix :restart-data cxn)))))

(defmethod repair ((repair repair-through-form-similarity)
                   (problem no-cxn-to-interpret-utterance)
                   (cipn cip-node)
                   &key &allow-other-keys)
  
  (when (constructions-list (original-cxn-set (construction-inventory cipn)))
    (let* ((cxn-inventory (original-cxn-set (construction-inventory cipn)))
           (form-to-interpret (first (utterance (get-data (blackboard (construction-inventory cipn)) :agent))))
           
           ;; Get the Levenshtein distance between the utterance and each form in the hearer's inventory.
           (candidates (loop for cxn in (constructions-list cxn-inventory)
                             for candidate = (attr-val cxn :form)
                             for distance = (crs-conventionality::levenshtein-distance-strings form-to-interpret candidate)
                             collect (cons cxn distance)))
           
           ;; Get all candidates with lowest distance.
           (closest-candidates (all-smallest #'cdr candidates))
           
           ;; If there are multiple with same distance, get most entrenched word. In the case that there are multiple with the same distance and entrenchment score, choose between them at random.
           (best-candidate (random-elt (all-biggest #'(lambda (cxn-and-distance) (attr-val (first cxn-and-distance) :score)) closest-candidates))))
      (when best-candidate 
           
        (let* ((new-form (attr-val (car best-candidate) :form)) 
               (initial-cfs-copy (copy-object (car-source-cfs (cipn-car cipn))))
               (root-copy (pole-structure (left-pole initial-cfs-copy))))
          (loop for unit in root-copy
                for form-unit = (unit-feature unit 'form nil)
                do (setf (feature-value form-unit) (list new-form)) ;;
                   (setf new-initial-cfs initial-cfs-copy))

          (make-instance 'form-similarity-fix :restart-data (cons (first best-candidate) new-initial-cfs)))))))


(defmethod repair ((repair repair-through-adoption)
                   (problem no-cxn-to-interpret-utterance)
                   (cipn cip-node)
                   &key &allow-other-keys)

  ;; BE CAREFUL!!!
  ;; This relies on the topic and scene that were set in the blackboard of the cxn-inventory during coherence check (conceptualise)
  
  (let* (;; copy cxn-inventory
         (cxn-inventory-copy (copy-fcg-construction-set-without-cxns (original-cxn-set (construction-inventory cipn))))

         ;; get topic from blackboard, in canonical naming game, only one object as the topic
         (topic (get-data (blackboard (construction-inventory cipn)) :topic)) 
         (topic-entity (first (crs-conventionality::entities topic))) 
         
         ;; get primitive inventory and scene
         (primitive-inventory (get-data (blackboard (construction-inventory cipn)) :primitive-inventory))
         (scene (get-data (blackboard (construction-inventory cipn)) :scene))

         ;; start from a partial program that has the scene
         (partial-program `((bind ,(type-of scene) ?scene ,scene)))

         ;; compose a program that leads to the topic, starting from the partial program with the scene
         (composition-result (crs-conventionality::compose-program topic-entity partial-program primitive-inventory)))

    ;; make the construction 
    (let* (;; get meaning based on the irl-program and the bind-statements that are in the composition result
           (irl-program (irl::irl-program (irl::chunk (first composition-result))))
           (bind-statements (irl::bind-statements (first composition-result)))
           (meaning (append irl-program bind-statements))

           ;; form is stored in the utterance slot
           (form (first (utterance (get-data (blackboard (construction-inventory cipn)) :agent))))

           ;; make the construction based on the form, meaning and topic
           (cxn (crs-conventionality::make-naming-game-cxn topic meaning cxn-inventory-copy form 0.5)))

      ;; add cxn to the cxn-inventory
      (add-cxn cxn cxn-inventory-copy)
      (make-instance 'adoption-fix :restart-data cxn))))

(defmethod copy-object ((entity irl:entity))
  "Needs to be implemented"
  entity)

(defmethod copy-object ((agent agent))
  "Needs to be implemented"
  agent)

;; Repairs ;;
;;;;;;;;;;;;;

(defmethod handle-fix ((fix invention-fix) (repair repair-through-invention) (problem no-cxn-to-conceptualise-topic) (node cip-node) &key &allow-other-keys)
  "Apply the construction provided by fix to the result of the node and return the construction-application-result"
  "Should be removed"
  (call-next-method)
  (push fix (fixes (problem fix))) ;; we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (set-data fix 'fixed-cars
              (fcg-apply (get-processing-cxn (restart-data fix))
                         (car-resulting-cfs (cipn-car node))
                         (direction (cip node))
                         :configuration (configuration (construction-inventory node))
                         :cxn-inventory (construction-inventory node)))))


(defmethod handle-fix ((fix adoption-fix) (repair repair-through-adoption) (problem no-cxn-to-interpret-utterance) (node cip-node) &key &allow-other-keys)
  "Apply the construction provided by fix to the result of the node and return the construction-application-result"
  "Should be removed"
  (call-next-method)
  (push fix (fixes (problem fix))) ;; we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (set-data fix 'fixed-cars
              (fcg-apply (get-processing-cxn (restart-data fix))
                         (car-resulting-cfs (cipn-car node))
                         (direction (cip node))
                         :configuration (configuration (construction-inventory node))
                         :cxn-inventory (construction-inventory node)))))

(defmethod handle-fix ((fix form-similarity-fix) (repair repair-through-form-similarity) (problem no-cxn-to-interpret-utterance) (node cip-node) &key &allow-other-keys)
  "Apply the construction provided by fix to the result of the node and return the construction-application-result"
  "Should be removed"
  (call-next-method)
  (push fix (fixes (problem fix))) ;; we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (set-data fix 'fixed-cars
              (fcg-apply (get-processing-cxn (car (restart-data fix)))
                         (cdr (restart-data fix))
                         (direction (cip node))
                         :configuration (configuration (construction-inventory node))
                         :cxn-inventory (construction-inventory node)))))




(in-package :crs-conventionality)

(defun make-naming-game-cxn (topic meaning cxn-inventory form initial-score)
  "Make a cxn based on the topic, meaning, form."
  (let ((unit-name (make-var (format nil "~a-unit" form)))
        (cxn-name (make-symbol (upcase (format nil "~a-cxn" form)))))
    ;; the fcg-construction has a meaning, form and topic
    ;; initial score is 0.5
    (make-instance 'fcg-construction
                   :name cxn-name
                   :contributing-part (list (make-instance 'contributing-unit
                                                           :name unit-name
                                                           :unit-structure `((meaning ,meaning))))
                   :conditional-part (list (make-instance 'conditional-unit
                                                          :name unit-name
                                                          :comprehension-lock `((HASH form (,form))))
                                           (make-instance 'conditional-unit
                                                          :name 'root
                                                          :formulation-lock `((topic ,topic))))
                   :cxn-inventory cxn-inventory
                   :cxn-set 'cxn
                   :feature-types (feature-types cxn-inventory)
                   :attributes `((:score . ,initial-score)
                                 (:topic . ,(id (first (crs-conventionality::entities topic))))
                                 (:form . ,form)))))

 