(in-package :amr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comparing AMR networks and defining equivalence    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun equivalent-amr-predicate-networks (fcg-amr-network amr-predicates)
  (equivalent-predicate-networks
   fcg-amr-network
   (variablify-amr-network amr-predicates)))


(defun equivalent-predicate-networks (network-1 network-2)
  "If network-1 and network-2 are equal upto variable renamings, the renamings are returned,
   otherwise nil is returned."
  (cond
   ;; If networks are equal, return t
   ((equal network-1 network-2) t)
   ;; If networks do not have the same length, return nil
   ((/= (length network-1) (length network-2)) nil)
   ;; The networks do not consist of the same predicates (in terms of constants), return nil
   ((not (networks-with-equal-constants-p network-1 network-2)) nil)
   ;; Check the networks in terms of variable bindings
   ((loop with queue = (list (list network-1 network-2 '((T . T))))
          until (not queue)
          for state = (pop queue)
          for n1-left = (first  state)
          for n2-left = (second state)
          for bindings = (third state)
          ;; a solution is found
          when (null n1-left)
          do (return bindings)
          ;; no solution is found
          else do
          (let ((predicates-with-equal-constants (find-all (first n1-left) n2-left :test #'predicates-with-equal-constants-p)))
            (loop for p in predicates-with-equal-constants
                  for new-bindings = (make-renamings (first n1-left) p bindings)
                  if new-bindings
                  do
                  (push (list (rest n1-left) (remove p n2-left :count 1) new-bindings) queue)))))))

(defun networks-with-equal-constants-p (network-1 network-2)
  "Returns t if network-1 and network-2 are equal in terms of constants."
  (when (= (length network-1) (length network-2))
    (loop with predicates-left = (copy-object network-2)
          for predicate in (copy-object network-1)
          when (find predicate predicates-left :test #'predicates-with-equal-constants-p)
          do (setf predicates-left
                   (remove predicate predicates-left
                           :test #'predicates-with-equal-constants-p
                           :count 1))
          else do (return nil)
          finally (return t))))

(defun predicates-with-equal-constants-p (predicate-1 predicate-2)
  "Returns t if predicate-1 and predicate-2 are equal in terms of constants."
  (when (= (length predicate-1) (length predicate-2))
    (loop for el-1 in predicate-1
          for el-2 in predicate-2
          unless (or (equal el-1 el-2)
                     (and (variable-p el-1)
                          (variable-p el-2)))
          do (return nil)
          finally (return t))))

(defun make-renamings (el-1 el-2 bindings)
  "Finds renamings to ensure equality between el-1 and el-2 (no unification)"
  (cond ((eq bindings nil)
          nil)
        ((equal el-1 el-2)
         bindings)
        ((and (variable-p el-1)
              (variable-p el-2)
              (assoc el-1 bindings)
              (equal el-2 (cdr (assoc el-1 bindings))))
         bindings)
        ((and (variable-p el-1)
              (variable-p el-2)
              (not (assoc el-1 bindings))
              (not (find el-2 bindings :key #'cdr)))
         (extend-bindings el-1 el-2 bindings))
        ((and (listp el-1) (listp el-2))
         (let ((new-bindings (make-renamings (first el-1) (first el-2) bindings)))
           (make-renamings (rest el-1) (rest el-2) new-bindings)))
        (t
         nil)))

(defun extend-bindings (var val bindings)
  "Adds the binding of var to val to bindings."
  (cons (make-binding var val)
	(if (eq bindings '((T . T)))
	    nil
	    bindings)))

(defun make-binding (var val)
  (cons var val))
