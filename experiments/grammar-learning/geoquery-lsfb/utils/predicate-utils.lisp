(in-package :geoquery-lsfb)

;;-----------------;;
;; predicate utils ;;
;;-----------------;;

(defun compare-elem-to-first-arg (elem predicate)
  "a test function that compares element to the first argument of the predicate"
  (eql
   elem
   (second predicate)))


(defun sort-vars (predicates)
  "extracts all variables from predicates and sorts
them using the constraints imposed by the predicates"
  (Let* (;; find all variables that follow another
         (second-element-vars  
          (loop for predicate in predicates
                  collect (third predicate)))
         ;; find the first variable in the chain
         (first-arg
          (loop for predicate in predicates
                if (NOT
                    (member
                     (second predicate)
                     second-element-vars))
                  return (second predicate)))
         ;; start from first variable and complete chain
         (sorted-vars
          (loop with output-list = (list first-arg)
                with previous-arg = first-arg
                while previous-arg
                for adjacent-arg =
                  (loop for predicate in predicates
                        when
                          (eql
                           (second
                            predicate)
                           previous-arg)
                          return
                            (third
                             predicate))
                do (setf previous-arg nil)
                when adjacent-arg
                  do (push adjacent-arg output-list)
                     (setf previous-arg adjacent-arg)
                ;; result needs to be reversed!!
                finally (return (reverse output-list)))))
         sorted-vars)) ;; return result

(defun find-by-fcg-id (predicates fcg-id)
  "finds an articulation predicate using its fcg-id"
  (loop for predicate in predicates
        when (eql (second predicate) fcg-id)
          do (return predicate)))


(defun sort-predicates (predicates)
  "sorts predicates according to type and returns a hash table
   with the types of predicates as keys and the different predicate
   instances as values"
  (loop with sorted-predicates
          = (make-hash-table)
        for predicate in predicates
        for predicate-type
          = (first predicate)
        do
          (push
           predicate
           (gethash
            predicate-type
            sorted-predicates))
        finally
          (return
           sorted-predicates)))


