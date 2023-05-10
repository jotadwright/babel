(in-package :duckie-language-learning)

;; -----------
;; + Repairs +
;; -----------

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

;; ----------------------------------
;; + HANDLE-FIX: general handle fix +
;; ----------------------------------
(defmethod handle-fix ((fix fcg::cxn-fix) (repair duckie-learning-repair)
                       (problem problem) (node cip-node)
                       &key &allow-other-keys)
  "Handle fix for all repairs except add-holophrase. 
      Add the categorial-links, apply some constructions,
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
      ;; set to true if a repair has occured
      (set-data (initial-node node) :some-repair-applied t)
      ;; set to true if a repair (that is not 'add-categorial-links) has occurred
      ;; TODO: this is a 'quick fix', 
      ;;       because we don't want to count add-categorial-links as a repair
      ;;       when dealing with the failed-interpretation goal test
      ;;       Thus, should fix this in the future with more robust logic ;;'
      ;;       -> (specifically in the goal-tests.lisp logic)
      (when (not (eq (type-of repair) 'add-categorial-links))
        (set-data (initial-node node) :some-regular-repair-applied t))
      ;; set the construction supplier
      (setf (cxn-supplier (first new-nodes)) (cxn-supplier node))
      (loop for node in new-nodes
            unless (or (is-subset (mapcar #'name (applied-constructions node))
                                  existing-cxns-to-apply :key #'name)
                       (equal-sets (mapcar #'name (applied-constructions node))
                                   existing-cxns-to-apply :key #'name))
              do (push (type-of repair) (fcg::statuses node))
                 (push 'fcg::added-by-repair (fcg::statuses node)))
      (cip-enqueue (first new-nodes) (cip node)
                   (get-configuration node :queue-mode))
      (notify fix-applied
              (type-of (issued-by fix))
              form-constraints
              (append new-cxns-to-apply other-new-cxns)
              (cip node)
              temp-categorial-network
              categorial-links))))
