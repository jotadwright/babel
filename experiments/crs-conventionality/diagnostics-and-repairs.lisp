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
    (make-instance 'no-production-problem)))


;; Problems ;;
;;;;;;;;;;;;;;

(defclass no-production-problem (problem)
  ()
  (:documentation "Problem class that specifies that agent cannot produce."))


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
                   (problem no-production-problem)
                   (cipn cip-node)
                   &key &allow-other-keys)
  "Invent a construction through composition."
  (let* ((topic (get-data (blackboard (construction-inventory cipn)) :topic))
         (topic-entity (first (crs-conventionality::entities topic))) ;; in canonical naming game, only one object as the topic
         (cxn-inventory (construction-inventory cipn))
         (primitive-inventory (get-data (blackboard (construction-inventory cipn)) :primitive-inventory))
         (scene (get-data (blackboard (construction-inventory cipn)) :scene))
         (partial-program `((bind ,(type-of scene) ?scene ,scene))) ;; take scene as partial program to start composing
         (composition-result (crs-conventionality::compose-program topic-entity partial-program primitive-inventory))) 
    (when (find 'irl::solution (irl::statuses (irl::node (first composition-result))))
      (let* ((irl-program (irl::irl-program (irl::chunk (first composition-result)))) 
             (bind-statements (irl::bind-statements (first composition-result)))
             (meaning (append irl-program bind-statements))
             (form (make-word)) ;; invent a form
             (unit-name (intern (upcase (format nil "?~a-unit" form))))
             (cxn-name (intern (upcase (format nil "~a-cxn" form)))))
        (multiple-value-bind (cxn-set cxn)
            (eval `(def-fcg-cxn ,cxn-name
                                ((,unit-name
                                  (meaning ,meaning))
                                 <-
                                 (root
                                  (topic ,topic)
                                  --
                                  )
                                 (,unit-name
                                  --
                                  (HASH form (,form))))
                                :cxn-inventory ,(original-cxn-set cxn-inventory)
                                :attributes (:score 0.5 :topic ,(id (first (crs-conventionality::entities topic))) :form ,form)))
         
          (make-instance 'invention-fix :restart-data cxn))))))

;; Repairs ;;
;;;;;;;;;;;;;

(defmethod handle-fix ((fix invention-fix) (repair repair-through-invention) (problem no-production-problem) (node cip-node) &key &allow-other-keys)
  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  (call-next-method)
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (set-data fix 'fixed-cars
              (fcg-apply (processing-cxn (restart-data fix))
                         (car-resulting-cfs (cipn-car node))
                         (direction (cip node))
                         :configuration (configuration (construction-inventory node))
                         :cxn-inventory (construction-inventory node)))))
