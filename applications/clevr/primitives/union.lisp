;;;; union.lisp

(in-package :clevr)

;; -----------------
;; UNION primtive ;;
;; -----------------

(defprimitive union! ((target-set clevr-object-set)
                      (source-set-1 clevr-object-set)
                      (source-set-2 clevr-object-set))
  ;; first case; given both source sets, compute the target set
  ((source-set-1 source-set-2 => target-set)
   (let ((unioned (union (objects source-set-1)
                         (objects source-set-2)
                         :key #'id)))
     (if unioned
       (bind (target-set 1.0 (make-instance 'clevr-object-set :objects unioned)))
       (bind (target-set 1.0 (make-instance 'clevr-object-set :id (make-id 'empty-set)))))))

  ;; second case; given a source and target set, compute the other source set
  ((source-set-1 target-set => source-set-2)
   (let ((context (get-data ontology 'clevr-context)))
     (loop for possible-set in (all-subsets (objects context))
           for unioned = (union (objects source-set-1)
                                possible-set
                                :key #'id)
           for union-set = (make-instance 'clevr-object-set :objects unioned)
           when (equal-entity target-set union-set)
           do (bind (source-set-2 1.0 union-set)))))

  ;; third case; given a source and target set, compute the other source set
  ((source-set-2 target-set => source-set-1)
   (let ((context (get-data ontology 'clevr-context)))
     (loop for possible-set in (all-subsets (objects context))
           for unioned = (union (objects source-set-2)
                                possible-set
                                :key #'id)
           for union-set = (make-instance 'clevr-object-set :objects unioned)
           when (equal-entity target-set union-set)
           do (bind (source-set-1 1.0 union-set)))))

  ;; fourth case; given both source sets and target set
  ;; check for consistency
  ((source-set-1 source-set-2 target-set =>)
   (let ((unioned (union (objects source-set-1)
                         (objects source-set-2)
                         :key #'id)))
     (equal-entity target-set
                   (make-instance 'clevr-object-set
                                  :objects unioned)))))