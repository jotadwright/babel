;;;; intersect.lisp

(in-package :mwm-evaluation)

;; ---------------------
;; INTERSECT primtive ;;
;; ---------------------
;; Find the intersection of two sets

;(export '(intersect))

(defprimitive intersect ((target-set mwm::mwm-object-set)
                         (source-set-1 mwm::mwm-object-set)
                         (source-set-2 mwm::mwm-object-set))
  ;; first case; given both source sets, compute the target set
  ((source-set-1 source-set-2 => target-set)
   (let ((intersected (intersection (objects source-set-1)
                                    (objects source-set-2)
                                    :key #'id)))
     (if intersected
       (bind (target-set 1.0 (make-instance 'mwm::mwm-object-set :objects intersected)))
       (bind (target-set 1.0 (make-instance 'mwm::mwm-object-set :id (make-id 'empty-set)))))))

  ;; second case; given a source and target set, compute the other source set
  ((source-set-1 target-set => source-set-2)
   (let ((context (get-data ontology 'clevr-context)))
     (loop for possible-set in (all-subsets (objects context))
           for intersected = (intersection (objects source-set-1)
                                           possible-set
                                           :key #'id)
           for intersected-set = (make-instance 'mwm::mwm-object-set :objects intersected)
           when (equal-entity target-set intersected-set)
           do (bind (source-set-2 1.0 intersected-set)))))

  ;; third case; given a source and target set, compute the other source set
  ((source-set-2 target-set => source-set-1)
   (let ((context (get-data ontology 'clevr-context)))
     (loop for possible-set in (all-subsets (objects context))
           for intersected = (intersection (objects source-set-2)
                                           possible-set
                                           :key #'id)
            for intersected-set = (make-instance 'mwm::mwm-object-set :objects intersected)
           when (equal-entity target-set intersected-set)
           do (bind (source-set-1 1.0 intersected-set)))))

  ;; fourth case; given both source sets and target set
  ;; check for consistency
  ((source-set-1 source-set-2 target-set =>)
   (let ((intersected (intersection (objects source-set-1)
                                    (objects source-set-2)
                                    :key #'id)))
     (equal-entity target-set
                   (make-instance 'mwm::mwm-object-set
                                  :objects intersected))))
  :primitive-inventory *mwm-primitives*)