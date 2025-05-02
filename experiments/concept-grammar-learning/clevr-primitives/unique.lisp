;;;; unique.lisp

(in-package :clg)

;; ------------------
;; UNIQUE primitive ;;
;; ------------------

;(export '(unique))

(defprimitive unique ((target-object clevr-object)
                      (source-set clevr-object-set))
  ;; first case; given source set, compute target object
  ((source-set => target-object)
   (if (and (objects source-set) (length= (objects source-set) 1)) ;; to remove
     (bind (target-object 1.0 (first (objects source-set)))) ;; to remove
     
     (let* ((result (loop for entity in (objects source-set)
                          for similarity = (get-sim-for-object source-set entity)
                          collect (list similarity entity) into results
                          finally (return (extremum results :key #'first :test #'>))))
            (similarity (first result))
            (entity (second result)))
       (when entity
         (bind (target-object similarity entity))))))

  ;; second case; given source set and target object
  ;; check for consistency
  #|((source-set target-object =>)
   (and (length= (objects source-set) 1)
        (equal-entity target-object (first (objects source-set)))))|#
  :primitive-inventory *clevr-primitives*)


(defun get-sim-for-object (source-set object)
  (loop for simmer in (similarities source-set)
        for category = (car simmer)
        for sims = (first (cdr simmer))
        for obj-id = (id object)
        for sim2 = (cdr (find obj-id sims :key #'first))
        sum sim2 into all-the-sims
        finally (return all-the-sims)))
        