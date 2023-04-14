(in-package :duckie-language-learning)

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

(defmethod handle-fix ((fix fcg::cxn-fix) (repair duckie-learning-repair)
                       (problem problem) (node cip-node)
                       &key &allow-other-keys)
  "Handle fix for all other repairs. Add the th-links, apply some constructions,
   and add some constructions to the inventory."
  (push fix (fixes problem))
  (with-disabled-monitor-notifications
    (destructuring-bind (existing-cxns-to-apply new-cxns-to-apply other-new-cxns th-links) (restart-data fix)
      (let* ((orig-type-hierarchy (categorial-network (construction-inventory node)))
             (temp-type-hierarchy (copy-object (categorial-network (construction-inventory node))))
             (weighted-th-links (loop for th-link in th-links
                                      collect (list (car th-link) (cdr th-link)))) 
             (th (loop for th-link in weighted-th-links
                       do (add-categories (list (first th-link) (second th-link)) temp-type-hierarchy)
                       (add-link (first th-link) (second th-link) temp-type-hierarchy
                                 :recompute-transitive-closure nil)
                       finally (set-categorial-network (construction-inventory node) temp-type-hierarchy)))
             (new-nodes
              (apply-sequentially (gl::initial-node node)
                                  (append existing-cxns-to-apply new-cxns-to-apply)
                                  node)))
        (declare (ignorable th))
        (set-categorial-network (construction-inventory node) orig-type-hierarchy)
        (set-data (car-resulting-cfs (cipn-car (first new-nodes)))
                  :fix-cxns (append new-cxns-to-apply other-new-cxns))
        (set-data (car-resulting-cfs (cipn-car (first new-nodes)))
                  :fix-th-links weighted-th-links)
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
                     (get-configuration node :queue-mode))))))