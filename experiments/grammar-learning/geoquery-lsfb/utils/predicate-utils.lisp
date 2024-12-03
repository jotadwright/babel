(in-package :geoquery-lsfb)

;;-----------------;;
;; predicate utils ;;
;;-----------------;;

(defun compare-elem-to-first-arg (elem predicate)
  "a test function that compares element to the first argument of the predicate"
  (eql
   elem
   (second predicate)))

(defun filter-right-hand-adjacents (adjacent-predicates
                                    right-hand-predicates)
  
  "splits the list of adjacent-predicates into two: one
   list contains the adjacent predicates that establish
   a relationship between right hand articulations exclu
   sively and the other contains the remaining predicates"
  
  (loop with right-hand-exclusive-adjacents = '()
        with other-adjacents = '()
        with right-hand-vars
          = (loop for predicate in right-hand-predicates
                  collect
                    (second
                     predicate))
        for predicate in adjacent-predicates
        do (if (and
                (member
                 (second predicate)
                 right-hand-vars)
                (member
                 (third predicate)
                 right-hand-vars))
             (push predicate right-hand-exclusive-adjacents)
             (push predicate other-adjacents))
        finally (return
                 (values
                  right-hand-exclusive-adjacents
                  other-adjacents))))


(defun sort-vars (adjacent-predicates right-hand-predicates during-predicates end-coincides-predicates)
  "extracts all variables from adjacent-predicates and sorts
them using the constraints imposed by the adjacent-predicates"
  (multiple-value-bind
      (right-hand-exclusive-adjacents
       other-adjacents)
    (filter-right-hand-adjacents
     adjacent-predicates
     right-hand-predicates)
    (Let* (;; find all variables that follow another
           (second-element-vars  
            (loop for predicate in right-hand-exclusive-adjacents
                  collect (third predicate)))
           ;; find the first variable in the chain
           (first-arg
            (loop with first-var-candidates = '()
                  for predicate in right-hand-predicates
                  when (NOT
                        (member
                         (second predicate)
                         second-element-vars))
                    do (push
                        (second predicate)
                        first-var-candidates)
                  finally
                    (return (if (> (length
                            first-var-candidates)
                           0)
                      (loop with output = nil
                            with lh-second-element-vars
                              = (append
                                  (loop for adjacent-predicate in other-adjacents
                                        collect (third adjacent-predicate))
                                  (loop for during-predicate in during-predicates
                                        collect (second during-predicate))
                                  (loop for end-coincides-predicate in end-coincides-predicates
                                        collect (second end-coincides-predicate)))
                            for candidate in first-var-candidates
                            for dummy-var = (make-const "dummy")
                            do (if (member
                                    candidate
                                    lh-second-element-vars)
                                 (progn
                                   (loop for lh-predicate in other-adjacents
                                         do (cond
                                             ((eql (second lh-predicate) candidate)
                                              (push `(adjacent ,candidate ,dummy-var)
                                                    right-hand-exclusive-adjacents))
                                             ((eql (third lh-predicate) candidate)
                                              (push `(adjacent ,dummy-var ,candidate)
                                                    right-hand-exclusive-adjacents))))
                                   (loop for lh-during-predicate in during-predicates
                                         when (eql (second lh-during-predicate) candidate)
                                           do (loop for other-adjacent in other-adjacents
                                                    when (eql (third other-adjacent) (third lh-during-predicate))
                                                      do (push `(adjacent ,(second other-adjacent) ,dummy-var)
                                                               right-hand-exclusive-adjacents)
                                                         (push `(adjacent ,dummy-var ,candidate)
                                                               right-hand-exclusive-adjacents)
                                                         (push `(start-coincides ,dummy-var ,(third other-adjacent))
                                                               during-predicates)))
                                   (loop for end-coincides-predicate in end-coincides-predicates
                                         when (eql (second end-coincides-predicate) candidate)
                                           do (loop for other-adjacent in other-adjacents
                                                    when (eql (third other-adjacent) (third end-coincides-predicate))
                                                      do (push `(adjacent ,(second other-adjacent) ,dummy-var)
                                                               right-hand-exclusive-adjacents)
                                                         (push `(adjacent ,dummy-var ,candidate)
                                                               right-hand-exclusive-adjacents)
                                                         (push `(start-coincides ,dummy-var ,(third other-adjacent))
                                                               end-coincides-predicates))))
                                   (setf output candidate))
                            finally (return output))
                      (first first-var-candidates)))))
           ;; start from first variable and complete chain
           (sorted-vars
            (loop with output-list = (list first-arg)
                  with previous-arg = first-arg
                  while previous-arg
                  for adjacent-arg =
                    (loop for predicate in right-hand-exclusive-adjacents
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
      (list sorted-vars during-predicates end-coincides-predicates)))) ;; return result

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


