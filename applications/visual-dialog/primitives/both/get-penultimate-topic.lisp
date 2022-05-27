(in-package :visual-dialog)

;; -----------------
;; get-penultimate-topic primitive 
;; -----------------

(defprimitive get-penultimate-topic ((target-set world-model)
                                     (source-set world-model))
  ((source-set => target-set)
   (multiple-value-bind
      (last-set last-timestamp)
      (the-biggest #'timestamp (set-items source-set))
     (let* ((last-topic (find-penultimate-topic source-set))
            (last-topic-object-set
             (if (object-set last-set)
               (loop for obj in (objects (object-set last-set))
                     when (member (id obj) last-topic)
                       collect obj))))
       (bind (target-set 1.0
                         (make-instance 'world-model
                                        :set-items (list (make-instance 'turn
                                                                        :object-set (make-instance 'object-set
                                                                                                   :objects last-topic-object-set)))))))))
  :primitive-inventory (*symbolic-primitives* *subsymbolic-primitives*))


(defun find-penultimate-topic (source-set)
  (multiple-value-bind
      (last-set last-timestamp)
      (the-biggest #'timestamp (set-items source-set))
    (let* ((penultimate-set (find (- last-timestamp 1) (set-items source-set) :key #'timestamp))
           (last-topic (if penultimate-set (topic-list penultimate-set))))
      (if last-topic
        last-topic
        (find-last-topic (rest (set-items source-set)))))))