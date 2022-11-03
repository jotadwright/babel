(in-package :intention-reading)

(defclass clevr-learning-repair (repair)
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

(defmethod handle-fix ((fix fcg::cxn-fix) (repair clevr-learning-repair)
                       (problem problem) (node cip-node)
                       &key &allow-other-keys)
  "Handle fix for all other repairs. Add the th-links, apply some constructions,
   and add some constructions to the inventory."
  (push fix (fixes problem))
  (with-disabled-monitor-notifications
    (destructuring-bind (existing-cxns-to-apply new-cxns-to-apply other-new-cxns th-links) (restart-data fix)
      (let* ((orig-type-hierarchy (categorial-network (construction-inventory node)))
             (temp-type-hierarchy (copy-object (categorial-network (construction-inventory node)))) 
             (th (loop for (from . to) in th-links
                       do (add-categories (list from to) temp-type-hierarchy)
                          (add-link from to temp-type-hierarchy
                                    :weight (get-configuration node :initial-th-link-weight))
                       finally (set-categorial-network (construction-inventory node) temp-type-hierarchy)))
             (new-nodes
              (apply-sequentially (initial-node node)
                                  (append existing-cxns-to-apply new-cxns-to-apply)
                                  node)))
        (declare (ignorable th))
        (set-categorial-network (construction-inventory node) orig-type-hierarchy)
        (set-data (car-resulting-cfs (cipn-car (first new-nodes)))
                  :fix-cxns (append new-cxns-to-apply other-new-cxns))
        ;(set-data (car-resulting-cfs (cipn-car (first new-nodes)))
        ;          :fix-categories ...)
        (set-data (car-resulting-cfs (cipn-car (first new-nodes)))
                  :fix-categorial-links th-links)
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
                     (get-configuration node :queue-mode))))))
