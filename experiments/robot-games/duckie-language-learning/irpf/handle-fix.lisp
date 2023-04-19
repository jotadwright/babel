(in-package :duckie-language-learning)

;; events

(define-event diagnostic-trigger
  (node cip-node)
  (diagnostic diagnostic))



(define-event-handler (trace-fcg diagnostic-trigger)
  (add-element '((hr)))
  
  (let ((cip (cip node)))
    (add-element `((div)
                   ((table :class "two-col")
                    ((tbody)
                     ((tr)
                      ((td) "initial structure")
                      ((td) ,(make-html-fcg-light (initial-cfs cip)
                                                  :configuration nil
                                                  :feature-types (feature-types (original-cxn-set (construction-inventory cip))))))
                     ((tr)
                      ((td) "application process")
                      ((td) ((div :id ,(mkstr (make-id 'subtree-id)))
                                  ,(make-html-fcg-light node)))
                      )
                     )))))
  (add-element `((h2) ((b :style "color:#E65C00") "Comprehension unsuccesful - jump to meta-layer"))))

(define-event fix-applied (repair-name symbol) (form list) (learned-cxns list) (cip construction-inventory-processor) (th categorial-network) (new-links list))

(defun fix-applied-func (repair-name form learned-cxns cip th new-links)
  (add-element '((h2) "Meta-layer: Pattern Finding"))
  (add-element `((h3) ,(format nil "Applied repair: ~a with form: \"~{~a~^ ~}\" and learned:"
                               repair-name
                               (render form :render-string-meets)
                               (mapcar #'(lambda (cxn) (name cxn)) learned-cxns))))
  (add-element `((div :class "indent-irpf")
                 ,@(html-hide-rest-of-long-list 
                    learned-cxns 5
                    #'(lambda (construction) 
                        (make-html construction :expand-initially nil
                                   :configuration (visualization-configuration (construction-inventory cip))
                                   :wrap-in-paragraph nil)))))
  (if new-links
    (progn
      (add-element '((h3) "New links are added to the type hierarchy:"))
      (add-element `((div :class "indent-irpf") ,(s-dot->svg (new-th-links->s-dot th new-links))))))
  (add-element '((hr :style "border-top: 3px dashed #E65C00;background-color:#fff"))))

(define-event-handler (trace-fcg fix-applied)
  (fix-applied-func repair-name form learned-cxns cip th new-links))

;; --------------------
;; + Applying repairs +
;; --------------------

(defclass duckie-learning-repair (repair)
  ((trigger :initform 'fcg::new-node))
  (:documentation "Base class for all repairs."))

(defun apply-sequentially (starting-cipn constructions node)
  "Apply 'constructions' sequentially to the 'starting-cipn',
   using the configurations from 'node'. Returns all new
   cip nodes."
  (let ((working-cipn starting-cipn))
    (loop for cxn in constructions
          for cfs = (if (eq working-cipn (gl::initial-node working-cipn))
                      (car-source-cfs (cipn-car working-cipn))
                      (car-resulting-cfs (cipn-car working-cipn)))
          for car = (first
                     (fcg-apply (get-processing-cxn cxn)
                                cfs (direction (cip node))
                                :configuration
                                (configuration (construction-inventory node))
                                :cxn-inventory
                                (construction-inventory node)))
          for next-cipn = (when car (fcg::cip-add-child working-cipn car))
          when car
          do (setf working-cipn next-cipn)
          and collect next-cipn into new-cipns
          ;; return the last node as first in the list!
          finally (return (reverse new-cipns)))))

;; ---------------------------------
;; + HANDLE-FIX: all other repairs +
;; ---------------------------------
(defmethod handle-fix ((fix fcg::cxn-fix) (repair duckie-learning-repair)
                       (problem problem) (node cip-node)
                       &key &allow-other-keys)
  "Handle fix for all other repairs. Add the th-links, apply some constructions,
   and add some constructions to the inventory."
  (push fix (fixes problem))
  (destructuring-bind (existing-cxns-to-apply new-cxns-to-apply other-new-cxns th-links) (restart-data fix)
    (let* ((form-constraints (gl::form-constraints-with-variables
                              (cipn-utterance node)
                              (get-configuration (original-cxn-set (construction-inventory node)) :de-render-mode)))
           (orig-type-hierarchy (categorial-network (construction-inventory node)))
           (temp-type-hierarchy (copy-object (categorial-network (construction-inventory node))))
           (th (loop for (from . to) in th-links ;; th is never used, maybe refactor to get it out of the let
                     do (add-categories (list from to) temp-type-hierarchy)
                        (add-link from to temp-type-hierarchy :recompute-transitive-closure nil)
                     finally (set-categorial-network (construction-inventory node) temp-type-hierarchy)))
           (new-nodes (with-disabled-monitor-notifications
                        (apply-sequentially (gl::initial-node node)
                                            (append existing-cxns-to-apply new-cxns-to-apply)
                                            node))))
      (declare (ignorable th))
      (set-categorial-network (construction-inventory node) orig-type-hierarchy) ;; TODO: why does this happen???
      (set-data (car-resulting-cfs (cipn-car (first new-nodes)))
                :fix-cxns (append new-cxns-to-apply other-new-cxns))
      (set-data (car-resulting-cfs (cipn-car (first new-nodes)))
                :fix-categorial-links th-links)
      ;; write some message on the blackboard of the initial node
      ;; for more efficient diagnostics
      (set-data (gl::initial-node node) :some-repair-applied t)
      (setf (cxn-supplier (first new-nodes)) (cxn-supplier node))
      (loop for node in new-nodes
            unless (or (is-subset (mapcar #'name (applied-constructions node))
                                  existing-cxns-to-apply :key #'name)
                       (equal-sets (mapcar #'name (applied-constructions node))
                                   existing-cxns-to-apply :key #'name))
              do (push (type-of repair) (statuses node))
                 (push 'fcg::added-by-repair (statuses node)))
      (cip-enqueue (first new-nodes) (cip node)
                   (get-configuration node :queue-mode))
      (notify fix-applied
              (type-of (issued-by fix))
              form-constraints
              (append new-cxns-to-apply other-new-cxns)
              (cip node)
              temp-type-hierarchy
              th-links))))


