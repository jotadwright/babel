(in-package :pn)

;; ############################################################################
;; Utility functions for IRL-style predicate networks
;; ----------------------------------------------------------------------------

(export '())

(defun all-bind-statements (irl-network)
  "Get all bind statements"
  (find-all 'bind irl-network :key #'car))

(defun all-predicates (irl-network)
  "Get all predicates"
  (find-all-if-not #'(lambda (p) (eql p 'bind))
                   irl-network :key #'car))

(defun binding-var (predicate)
  "Get the binding variable of a predicate.
   In IRL, this is typically the last argument
   of the predicate."
  (unless (eql (first predicate) 'bind)
    (last-elt predicate)))

(defun linked-bind-statement (predicate irl-network)
  "Get the bind-predicate linked to the given predicate"
  (let* ((var (binding-var predicate))
         (all-bind-statements (find-all 'bind irl-network :key #'first)))
    (find var all-bind-statements :key #'third)))

;; are target-var and open-vars IRL specific, or more general?
;; what is the target-var of an AMR network?

(defun get-target-var (irl-network)
  "Returns the one unconnected open-var that appears as first argument
   of a primitive, or NIL otherwise."
  (unless (length= 0 irl-network)
    (if (and (length= irl-network 1)
             (eq (first (first irl-network)) 'bind))
      (third (first irl-network))
      (let* ((open-vars (unconnected-variables irl-network))
             (target-vars (intersection open-vars (mapcar #'second irl-network)))
             (target-var (when (= 1 (length target-vars))
                           (first target-vars))))
        target-var))))

(defun get-target-predicate (irl-network)
  "Returns the predicate that has the target var
   as its first argument."
  (let ((target-variable (get-target-var irl-network)))
    (find target-variable irl-network :key #'second :test #'eql)))

(defun get-open-vars (irl-network)
  "Find all unconnected variables which are not target"
  (set-difference
   (unconnected-variables irl-network)
   (mapcar #'second irl-network)))
    