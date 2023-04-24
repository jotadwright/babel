(in-package :duckie-language-learning)

;; --------------
;; + Handle fix +
;; --------------

;; TODO move all web-monitor code to separate file
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
                             ,(make-html-fcg-light (top-node cip))))
                      )
                     )))))
  (add-element `((h2) ((b :style "color:#E65C00") "Comprehension unsuccesful - jump to meta-layer"))))

(define-event fix-applied (repair-name symbol) (form list) (learned-cxns list) (cip construction-inventory-processor) (categorial-network categorial-network) (new-links list))

(defun new-categorial-links->s-dot (categorial-network new-links)
  (let* ((g (fcg::graph categorial-network))
         (graph-properties '((s-dot::fontcolor "#000000")
                             (s-dot::fontsize "10.0")
                             (s-dot::fontname "Helvetica")
                             (s-dot::rankdir "LR")))
         (all-node-names
          (remove-duplicates
           (loop for (from . to) in new-links
                 append (list from to))))
         (all-node-ids
          (loop for node-name in all-node-names
                for id = (gethash node-name (graph-utils::nodes g))
                collect id))
         (all-edges
          (loop for (from . to) in new-links
                collect (cons (gethash from (graph-utils::nodes g))
                              (gethash to (graph-utils::nodes g)))))
         (s-dot-nodes
          (loop for node-name in all-node-names
                for node-id in all-node-ids
                collect (graph-utils::categorial-network-node->s-dot
                         node-name node-id)))
         (s-dot-edges
          (loop for (from-id . to-id) in all-edges
                for edge-weight = (graph-utils::edge-weight g from-id to-id)
                collect (graph-utils::categorial-network-edge->s-dot
                         from-id to-id
                         :weight edge-weight :directedp nil
                         :colored-edges-0-1 nil))))
    `(s-dot::graph ,graph-properties
                   ,@s-dot-nodes
                   ,@s-dot-edges)))

(defun fix-applied-func (repair-name form learned-cxns cip categorial-network new-links)
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
      (add-element '((h3) "New links are added to the categorial network:"))
      (add-element `((div :class "indent-irpf") ,(s-dot->svg (new-categorial-links->s-dot categorial-network new-links))))))
  (add-element '((hr :style "border-top: 3px dashed #E65C00;background-color:#fff"))))

(define-event-handler (trace-fcg fix-applied)
  (fix-applied-func repair-name form learned-cxns cip categorial-network new-links))

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
          for cfs = (if (eq working-cipn (initial-node working-cipn))
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
  "Handle fix for all other repairs. Add the categorial-links, apply some constructions,
   and add some constructions to the inventory."
  (push fix (fixes problem))
  (destructuring-bind (existing-cxns-to-apply new-cxns-to-apply other-new-cxns categorial-links) (restart-data fix)
    (let* ((form-constraints (form-constraints-with-variables
                              (cipn-utterance node)
                              (get-configuration (original-cxn-set (construction-inventory node)) :de-render-mode)))
           (orig-categorial-network (categorial-network (construction-inventory node)))
           (temp-categorial-network (copy-object (categorial-network (construction-inventory node))))
           (categorial-network (loop for (from . to) in categorial-links ;; categorial-network is never used, maybe refactor to get it out of the let
                                     do (add-categories (list from to) temp-categorial-network)
                                        (add-link from to temp-categorial-network :recompute-transitive-closure nil)
                                     finally (set-categorial-network (construction-inventory node) temp-categorial-network)))
           (new-nodes (with-disabled-monitor-notifications
                        (apply-sequentially (initial-node node)
                                            (append existing-cxns-to-apply new-cxns-to-apply)
                                            node))))
      (declare (ignorable categorial-network))
      (set-categorial-network (construction-inventory node) orig-categorial-network) ;; TODO: why does this happen???
      (set-data (car-resulting-cfs (cipn-car (first new-nodes)))
                :fix-cxns (append new-cxns-to-apply other-new-cxns))
      (set-data (car-resulting-cfs (cipn-car (first new-nodes)))
                :fix-categorial-links categorial-links)
      ;; write some message on the blackboard of the initial node
      ;; for more efficient diagnostics
      (set-data (initial-node node) :some-repair-applied t)
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
              temp-categorial-network
              categorial-links))))
