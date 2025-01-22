(in-package :geoquery-lsfb)

;;-----------------;;
;; predicate utils ;;
;;-----------------;;

(defun compare-elem-to-first-arg (elem predicate)
  "a test function that compares element to the first argument of the predicate"
  (eql
   elem
   (second predicate)))

(defun find-by-fcg-tag (predicates fcg-tag)
  "finds an articulation predicate using its fcg-tag"
  (loop for predicate in predicates
        when (eql (fcg-tag predicate) fcg-tag)
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


