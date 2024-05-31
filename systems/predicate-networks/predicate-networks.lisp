(in-package :pn)

;; ############################################################################
;; Main functionality for all types of predicate networks
;; ----------------------------------------------------------------------------

(defgeneric variablify-predicate-network (network mode)
  (:documentation "Variablify the provided predicate network,
   according to the network mode, e.g. :irl or :amr."))

(defgeneric equivalent-predicate-networks (network-1 network-2)
  ;; see 'equivalent-predicate-networks' in
  ;; 'systems/fcg/utils/equivalent-predicate-networks.lisp'
  ;; This function checks if two networks are identical upto
  ;; variable renaming. However, this function can return
  ;; renamings such as ((?x . ?y) (?y . ?x)).
  ;;
  ;; see 'equivalent-amr-networks' in
  ;; 'systems/amr-parser/equivalent-amr-networks.lisp'
  ;; This function is identical to the one above, but for
  ;; AMR networks.
  ;;
  ;; and see 'equivalent-irl-programs?' in
  ;; 'systems/irl/composer/match-chunk.lisp'
  ;; This function tries to unify n1 with n2 and unify n2 with n1.
  ;; Checking in both directions is necessary
  (:documentation "Check if network-1 and network-2 are equivalent."))

(defmethod equivalent-predicate-networks ((network-1 list) (network-2 list))
  "Two predicate networks are equivalent when
   they can unify with each other."
  (let ((map-frames (irl::equivalent-irl-programs? network-1 network-2)))
    (when map-frames
      (irl::map-frame-bindings (first map-frames)))))

(defgeneric equivalent-predicate-networks-p (network-1 network-2)
  (:documentation "-p version of equivalent-predicate-networks."))

(defmethod equivalent-predicate-networks-p ((network-1 list) (network-2 list))
  "Two predicate networks are equivalent when
   they can unify with each other."
  (when (equivalent-predicate-networks network-1 network-2) t))

(defun instantiate-predicate-network (network)
 "Instantiate variables in the provided predicate network"
 (loop for predicate in network
       collect (loop for el in predicate
                     if (variable-p el)
                      collect (make-symbol (upcase (subseq (symbol-name el) 1)))
                       else collect el)))