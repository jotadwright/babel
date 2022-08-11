(in-package :pn)

(defmethod variablify-predicate-network (irl-network (mode (eql :irl)))
  "Variablify an IRL network. Return the variablified network
   and the used renamings."
  (assert (null (all-variables irl-network)))
  (let* ((all-arguments
          (loop for predicate in irl-network
                if (eql (first predicate) 'bind)
                collect (third predicate)
                else append (rest predicate)))
         (unique-arguments
          (remove-duplicates all-arguments))
         (renamings
          (loop for arg in unique-arguments
                collect (cons arg (variablify arg))))
         (variablified-network
          (loop for predicate in irl-network
                collect (loop for arg in predicate
                              collect (or (assqv arg renamings) arg)))))
    (values variablified-network renamings)))