(in-package :au-benchmark.base)

(export '(anti-unify-predicate-sequence
          print-anti-unification-results))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anti-unify sequences ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anti-unify-predicate (pattern
                             source
                             &optional
                             pattern-bindings
                             source-bindings
                             pattern-delta
                             source-delta)
  "Generalises over pattern and source and returns 5 things:
generalisation, pattern-bindings, source-bindings, pattern-delta and source-delta."
  (cond
   ;; Case: pattern equals source -> simply return pattern = source as generalisation
   ((and (atom pattern)
         (atom source)
         (equalp pattern source))
    (values pattern
            pattern-bindings
            source-bindings
            pattern-delta
            source-delta))
   ;; Case: pattern and source are different, but already share a binding in the bindingslist -> return binding
   ;; as generalisation
   ((subs-lookup pattern-bindings source-bindings pattern source)
    (values (subs-lookup pattern-bindings source-bindings pattern source)
            pattern-bindings
            source-bindings
            pattern-delta
            source-delta))
   ;; Case: pattern and source are predicates with the same name and the same arity -> return predicate with
   ;; anti-unified arguments
   ((and (listp pattern) (listp source)
         (atom (car source)) (atom (car pattern))
         (equalp (car source) (car pattern))
         (= (length pattern) (length source)))
    (anti-unify-predicate-sequence pattern source nil pattern-bindings source-bindings pattern-delta source-delta))
   ;; None of the above (pattern and source are different and no binding is available yet) -> introduce new
   ;; binding as generalisation
   (t
    (let ((var (make-var (next-au-var))))
      (values var
              (extend-bindings pattern var pattern-bindings)
              (extend-bindings source var source-bindings)
              pattern-delta
              source-delta)))))

(defun anti-unify-predicate-sequence (pattern
                                      source
                                      &optional
                                      generalisation
                                      pattern-bindings
                                      source-bindings
                                      pattern-delta
                                      source-delta)
  "Generalises over two sequences of predicates of the same length, returns generalisation, bindingslists and deltas."

  ;; Assert that the length of the pattern and the source are equal
  (assert (= (length pattern) (length source)))
  (cond
   ;; Base case: no elements anymore, return accumulator and bindings-lists
   ((and (null pattern) (null source))
    (values generalisation
            pattern-bindings
            source-bindings
            pattern-delta
            source-delta))
   ;; Recursive case: still elements left, anti-unify first and then rest,
   ;; every time passing the new bindings and deltas, accumulating the generalisation
   (t
    (multiple-value-bind (resulting-generalisation
                          resulting-pattern-bindings
                          resulting-source-bindings
                          resulting-pattern-delta
                          resulting-source-delta)
        (anti-unify-predicate (first pattern) (first source) pattern-bindings source-bindings pattern-delta source-delta)
      (anti-unify-predicate-sequence (rest pattern)
                                     (rest source)
                                     (append generalisation (list resulting-generalisation))
                                     resulting-pattern-bindings
                                     resulting-source-bindings
                                     resulting-pattern-delta
                                     resulting-source-delta)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print anti-unification results ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-anti-unification-results (list-of-anti-unification-results &optional (stream t))
  "Prints a list of anti-unification results."
  
  (format t "~%~%~%### Anti-unifying ###~%~%")
  (format stream "------------------------------------------------------------~%")
  (format stream "- Pattern:~%~%")
  (let ((*print-pretty* t))
             (format stream "~(~a~)~%~%" (compute-network-from-anti-unification-result (first list-of-anti-unification-results) 'pattern)))
  (format stream "- Source:~%~%")
  (let ((*print-pretty* t))
    (format stream "~(~a~)~%~%" (compute-network-from-anti-unification-result (first list-of-anti-unification-results) 'source)))
  (format stream "------------------------------------------------------------~%~%")
  (loop for a-u-result in list-of-anti-unification-results
        for i from 1 upto (length list-of-anti-unification-results)
        do (with-slots (generalisation pattern-bindings source-bindings pattern-delta source-delta cost) a-u-result
             (format stream "--- Result ~a (cost: ~a) ---~%~%" i cost)
             (format stream "- Generalisation:~%~%")
             (let ((*print-pretty* t))
               (format stream "~(~a~)~%~%" generalisation))
             (format stream "- Pattern bindings:~%~%")
             (let ((*print-pretty* t))
               (format stream "~(~a~)~%~%" pattern-bindings))
             (format stream "- Source bindings:~%~%")
             (let ((*print-pretty* t))
               (format stream "~(~a~)~%~%" source-bindings))
             (format stream "- Pattern delta:~%~%")
             (let ((*print-pretty* t))
               (format stream "~(~a~)~%~%" pattern-delta))
             (format stream "- Source delta:~%~%")
             (let ((*print-pretty* t))
               (format stream "~(~a~)~%~%~%" source-delta)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reversibility check ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-network-from-anti-unification-result (au-result pattern-or-source)
  "Returns original network based on generalisation, bindings-list and delta."  
  (let* ((generalisation (generalisation au-result))
         (bindings-key (case pattern-or-source
                         (pattern #'pattern-bindings)
                         (source #'source-bindings)))
         (delta-key (case pattern-or-source
                      (pattern #'pattern-delta)
                      (source #'source-delta)))
         (bindings-list (funcall bindings-key au-result))
         (delta (funcall delta-key au-result)))
    (append (substitute-bindings (reverse-bindings bindings-list) generalisation)
            delta)))


                            
    
