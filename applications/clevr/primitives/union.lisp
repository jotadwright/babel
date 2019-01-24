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
  ;; the other source set contains at least the set-difference of the
  ;; target set and the given source set + all possible subsets of
  ;; the given source set
  ((source-set-1 target-set => source-set-2)
   (if (empty-set-p source-set-1)
     (bind (source-set-2 1.0 target-set))
     (let ((difference-set (set-difference (objects target-set)
                                           (objects source-set-1)
                                           :key #'id))
           (all-subsets-1 (all-subsets (objects source-set-1))))
       (loop for possible-addition-set in (cons '() all-subsets-1)
             for union-set = (make-instance 'clevr-object-set
                                            :objects (append difference-set possible-addition-set))
             do (bind (source-set-2 1.0 union-set))))))

  ;; third case; given a source and target set, compute the other source set
  ((source-set-2 target-set => source-set-1)
   (if (empty-set-p source-set-2)
     (bind (source-set-1 1.0 target-set))
     (let ((difference-set (set-difference (objects target-set)
                                           (objects source-set-2)
                                           :key #'id))
           (all-subsets-2 (all-subsets (objects source-set-2))))
       (loop for possible-addition-set in (cons '() all-subsets-2)
             for union-set = (make-instance 'clevr-object-set
                                            :objects (append difference-set possible-addition-set))
             do (bind (source-set-1 1.0 union-set))))))

  ;; have to implement other directions as well?
  ;; (source-set-1 => source-set-2 target-set) -> lots of solutions
  ;; (source-set-2 => source-set-1 target-set) -> lots of solutions
  ;; (target-set => source-set-1 source-set-2) -> easy

  ;; fourth case; given both source sets and target set
  ;; check for consistency
  ((source-set-1 source-set-2 target-set =>)
   (let ((unioned (union (objects source-set-1)
                         (objects source-set-2)
                         :key #'id)))
     (equal-entity target-set
                   (make-instance 'clevr-object-set
                                  :objects unioned)))))