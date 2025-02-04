(in-package :fcg)
;; ! SPECIALISES METHODS IN :fcg

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ;;
;;  Diagnostics and repairs  ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Diagnostics ;;
;;;;;;;;;;;;;;;;;

(defclass diagnose-cip-find-forms (fcg::diagnostic)
  ((trigger :initform 'routine-processing-finished))
  (:documentation "Diagnostic called on cip after routine processing, checking if there is a form somewhere in the tree."))


(defmethod diagnose ((diagnostic diagnose-cip-find-forms) (cipn cip-node)
                     &key &allow-other-keys)
  "Check whether there are succeeded nodes in cip"
  (when (not (find 'succeeded (statuses cipn)))
    (make-instance 'no-form-problem)))


;; Problems ;;
;;;;;;;;;;;;;;

(defclass no-form-problem (problem)
  ()
  (:documentation "Problem class that specifies that agent has no cxn to formulate the conceptualised meaning."))


;; Fixes ;;
;;;;;;;;;;;

(defclass invention-fix (fix)
  ()
  (:documentation "Class for fixes created by repair-through-invention"))

;; Repairs ;;
;;;;;;;;;;;;;

(defclass repair-through-invention (repair)
  ((trigger :initform 'routine-processing-finished))
  (:documentation "Repair that invents"))


(defmethod repair ((repair repair-through-invention)
                   (problem no-form-problem)
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
             (cxn (crs-conventionality::make-naming-game-cxn topic meaning cxn-inventory-copy form)))

        ;; add the cxn to the cxn-inventory
        (add-cxn cxn cxn-inventory-copy)
        ;; add the cxn to the fix
        (make-instance 'invention-fix :restart-data cxn)))))



(defmethod copy-object ((entity irl:entity))
  "Needs to be implemented"
  entity)

(defmethod copy-object ((agent agent))
  "Needs to be implemented"
  agent)

;; Repairs ;;
;;;;;;;;;;;;;

(defmethod handle-fix ((fix invention-fix) (repair repair-through-invention) (problem no-form-problem) (node cip-node) &key &allow-other-keys)
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



(in-package :crs-conventionality)

(defun make-naming-game-cxn (topic meaning cxn-inventory form)
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
                   :attributes `((:score . 0.5)
                                 (:topic . ,(id (first (crs-conventionality::entities topic))))
                                 (:form . ,form)))))

 