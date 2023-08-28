(in-package :pf)

;;;;;;;;;;;;;;;;;;
;; compute args ;;
;;;;;;;;;;;;;;;;;;

(defgeneric compute-form-args (anti-unification-result cxn source-args)
  (:documentation "Compute the form-args from the anti-unification result
                   obtained by anti-unifying the observation with 'thing',
                   which can be a cxn or a cipn."))

(defgeneric compute-meaning-args (anti-unification-result cxn source-args)
  (:documentation "Compute the meaning-args from the anti-unification result
                   obtained by anti-unifying the observation with 'thing',
                   which can be a cxn or a cipn."))

(defmethod compute-form-args (anti-unification-result cxn source-args)
  (let ((form-representation (get-configuration (cxn-inventory cxn) :form-representation-formalism)))
    (compute-form-args-aux anti-unification-result cxn source-args form-representation)))

#|
(defun sequence-string (sequence-predicate)
  (second sequence-predicate))
(defun left-boundary (sequence-predicate)
  (third sequence-predicate))
(defun right-boundary (sequence-predicate)
  (fourth sequence-predicate))

(defmethod compute-form-args-aux (anti-unification-result
                                  (anti-unified-cxn fcg-construction)
                                  (source-args blackboard)
                                  (mode (eql :sequences)))
  (let ((args (make-blackboard)))
    (with-slots (generalisation
                 pattern-delta
                 source-delta
                 pattern
                 source) anti-unification-result
      (let* (;; determine outermost boundaries
             (leftmost-boundary
              (if (variable-p (first generalisation))
                (make-var 'leftmost)
                (left-boundary (first generalisation))))
             (rightmost-boundary
              (if (variable-p (last-elt generalisation))
                (make-var 'rightmost)
                (right-boundary (last-elt generalisation)))))
        (push-data args :generalisation-top-lvl-args rightmost-boundary)
        (push-data args :generalisation-top-lvl-args leftmost-boundary) 
        (dolist (elem generalisation)
          ;; when encountering a variable:
          (when (variable-p elem)
            (let* ((elem-pos (position elem generalisation))
                   (prev-seq (nth (1- elem-pos) generalisation))
                   (next-seq (nth (1+ elem-pos) generalisation))
                   (pd-seq (rest (assoc elem pattern-delta)))
                   (sd-seq (rest (assoc elem source-delta))))
              ;; the left boundary of the sequence after the variable
              ;; equals the right boundary of that var in both delta's
              (push-data args :generalisation-slot-args
                         (if (= elem-pos (1- (length generalisation))) ;; there is no next seq
                           rightmost-boundary
                           (left-boundary next-seq)))
              (push-data args :pattern-top-lvl-args (right-boundary pd-seq))
              (push-data args :source-top-lvl-args (right-boundary sd-seq))
              ;; the right boundary of the sequence before the variable
              ;; equals the left boundary of that var in both delta's
              (push-data args :generalisation-slot-args
                         (if (= elem-pos 0) ;; there is no prev seq
                           leftmost-boundary
                           (right-boundary prev-seq)))
              (push-data args :pattern-top-lvl-args (left-boundary pd-seq))
              (push-data args :source-top-lvl-args (left-boundary sd-seq)))))
        (set-data args :pattern-slot-args
                  ;; instantiate boundaries in generalisation using pattern delta
                  ;; instantiate boundaries in original pattern
                  ;; loop through variables in generalisation:
                  ;;    if instantiation in gen+pd matches instantiation in original pattern
                  ;;    -> that variable (+ its buddy) is a slot arg!!
                  (let ((var-bounds (variable-boundaries generalisation pattern-delta))
                        (pattern-bounds (instantiate-boundaries (extract-form-predicates anti-unified-cxn)))
                        (pattern-form-args (extract-slot-form-args anti-unified-cxn)))
                    (loop for (nil left right) in var-bounds
                          for lb-var = (find left pattern-bounds :key #'cdr :test #'=)
                          for rb-var = (find right pattern-bounds :key #'cdr :test #'=)
                          if (find lb-var pattern-form-args)
                            append (list lb-var (nth (1+ (position lb-var pattern-form-args)) pattern-form-args))
                          else if (find rb-var pattern-form-args)
                            append (list rb-var (nth (1- (position rb-var pattern-form-args)) pattern-form-args)))))
        ;; dirty tricks here....
        (setf generalisation (remove-if #'variable-p generalisation))
        (setf pattern-delta (mapcar #'rest pattern-delta))
        (setf source-delta (mapcar #'rest source-delta))))
    args))


(defun instantiate-boundaries (set-of-sequence-predicates)
  (let ((index 0))
    (loop for predicate in set-of-sequence-predicates
          for left-index = index
          for string-length = (length (sequence-string predicate))
          for right-index = (+ index string-length)
          do (incf index (length (sequence-string predicate)))
          append (list (cons (left-boundary predicate) left-index)
                       (cons (right-boundary predicate) right-index)))))

;(instantiate-boundaries '((sequence "what color is the " ?lb ?rb)))
;; ((?lb 0) (?rb 18))

(defun variable-boundaries (set-of-sequence-predicates bindings)
  (let ((index 0)
        positions)
    (dolist (elem set-of-sequence-predicates)
      (if (variable-p elem)
        (let* ((len (length (sequence-string (rest (assoc elem bindings)))))
               (start-pos index)
               (end-pos (+ index len)))
          (push (list elem start-pos end-pos) positions)
          (incf index len))
        (incf index (length (sequence-string elem)))))
    (reverse positions)))

;(variable-boundaries
; '((sequence "what " ?a ?b) ?x (sequence " is the " ?c ?d) ?y (sequence " ?" ?e ?f))
; '((?x sequence "color" ?a ?b) (?y sequence "ball" ?c ?d)))
;; ((?x 5 10) (?y 18 22))
|#

(defun restore-original-input (generalisation bindings delta)
  (append (substitute-bindings (fcg::reverse-bindings bindings) generalisation) delta))

(defmethod compute-form-args-aux (anti-unification-result
                                  (anti-unified-cxn fcg-construction)
                                  (source-args blackboard)
                                  (mode (eql :string+meets)))
  (let ((args (make-blackboard)))
    (with-slots (generalisation
                 pattern-bindings
                 source-bindings
                 pattern-delta
                 source-delta) anti-unification-result
      (let ((pattern-slot-args  ; group per category and alphabetically
             (sort (cxn-form-slot-args anti-unified-cxn :by-category-p t) #'string< :key #'car))
            (pattern-top-args (cxn-form-top-args anti-unified-cxn))
            (source-slot-args  ; grouped per category, sorted alphabetically
             (find-data source-args :slot-form-args))
            (source-top-args (or (find-data source-args :top-lvl-form-args)
                                 (holistic-form-top-args
                                  (restore-original-input generalisation source-bindings source-delta)
                                  (get-configuration (cxn-inventory anti-unified-cxn) :form-representation-formalism)))))
        (multiple-value-bind (gen-binding-vars pattern-binding-vars source-binding-vars)
            (loop for (pattern-var . generalisation-var) in (reverse pattern-bindings)
                  for (source-var . nil) in (reverse source-bindings)
                  when (or (find-anywhere pattern-var pattern-delta)
                           (find-anywhere source-var source-delta)
                           (find-anywhere pattern-var pattern-slot-args)
                           (find-anywhere source-var source-slot-args)
                           (> (count pattern-var pattern-bindings :key #'car) 1)
                           (> (count source-var source-bindings :key #'car) 1))
                  collect generalisation-var into gen-vars
                  and collect pattern-var into pattern-vars
                  and collect source-var into source-vars
                  finally (return (values gen-vars pattern-vars source-vars)))
          (set-data args :generalisation-top-lvl-args gen-binding-vars)
          (set-data args :pattern-slot-args (append (list pattern-binding-vars) (mapcar #'rest pattern-slot-args)))
          (set-data args :source-slot-args (append (list source-binding-vars) source-slot-args))
          (set-data args :pattern-top-lvl-args pattern-top-args)
          (set-data args :source-top-lvl-args source-top-args))))))
        
(defmethod compute-meaning-args (anti-unification-result
                                 (anti-unified-cxn fcg-construction)
                                 (source-args blackboard))
  (let ((args (make-blackboard)))
    (with-slots (generalisation
                 pattern-bindings
                 source-bindings
                 pattern-delta
                 source-delta) anti-unification-result
      (let ((pattern-slot-args  ; grouped per category, sorted alphabetically
             (sort (cxn-meaning-slot-args anti-unified-cxn :by-category-p t) #'string< :key #'car))
            (pattern-top-args (cxn-meaning-top-args anti-unified-cxn))
            (source-slot-args  ; grouped per category, sorted alphabetically
             (find-data source-args :slot-meaning-args))
            (source-top-args (or (find-data source-args :top-lvl-meaning-args)
                                 (holistic-meaning-top-args
                                  (restore-original-input generalisation source-bindings source-delta)
                                  (get-configuration (cxn-inventory anti-unified-cxn) :meaning-representation-formalism)))))
        (multiple-value-bind (gen-binding-vars pattern-binding-vars source-binding-vars)
            (loop for (pattern-var . generalisation-var) in (reverse pattern-bindings)
                  for (source-var . nil) in (reverse source-bindings)
                  when (or (find-anywhere pattern-var pattern-delta)
                           (find-anywhere source-var source-delta)
                           (find-anywhere pattern-var pattern-slot-args)
                           (find-anywhere source-var source-slot-args)
                           (> (count pattern-var pattern-bindings :key #'car) 1)
                           (> (count source-var source-bindings :key #'car) 1))
                  collect generalisation-var into gen-vars
                  and collect pattern-var into pattern-vars
                  and collect source-var into source-vars
                  finally (return (values gen-vars pattern-vars source-vars)))
          (set-data args :generalisation-top-lvl-args gen-binding-vars)
          (set-data args :pattern-slot-args (append (list pattern-binding-vars) (mapcar #'rest pattern-slot-args)))
          (set-data args :source-slot-args (append (list source-binding-vars) source-slot-args))
          (set-data args :pattern-top-lvl-args pattern-top-args)
          (set-data args :source-top-lvl-args source-top-args))))))
            
              
              
#|
(defmethod compute-form-args-aux (anti-unification-result
                                  (anti-unified-cxn fcg-construction)
                                  (source-args blackboard)
                                  (mode (eql :string+meets)))
  (let ((args (make-blackboard)))
    (with-slots (generalisation
                 pattern-bindings
                 source-bindings
                 pattern-delta
                 source-delta) anti-unification-result
      (let ((pattern-slot-args (extract-slot-form-args anti-unified-cxn))
            (pattern-top-args (extract-top-lvl-form-args anti-unified-cxn))
            (source-slot-args (find-data source-args :slot-form-args))
            (source-top-args (or (find-data source-args :top-lvl-form-args)
                                 ;; compute entirely new top args, like for a holophrase cxn,
                                 ;; by putting together the source delta and the generalisation
                                 (holistic-form-top-args
                                  (append (substitute-bindings (fcg::reverse-bindings source-bindings) generalisation) source-delta)
                                  (get-configuration (cxn-inventory anti-unified-cxn) :form-representation-formalism)))))
        (loop for (pattern-var . generalisation-var) in pattern-bindings
              for (source-var . nil) in source-bindings
              ;when (or (find-anywhere pattern-var pattern-delta)
              ;         (find-anywhere source-var source-delta)
              ;         (find pattern-var pattern-slot-args)
              ;         (find source-var source-slot-args)
              ;         (> (count pattern-var pattern-bindings :key #'car) 1)
              ;         (> (count source-var source-bindings :key #'car) 1))
              do (push-data args :pattern-top-lvl-args pattern-var)
                 (push-data args :source-top-lvl-args source-var)
                 (push-data args :generalisation-slot-args generalisation-var))
        (set-data args :pattern-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn) pattern-top-args)
                        ((item-based-cxn-p anti-unified-cxn) pattern-slot-args)))
        (set-data args :source-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn) source-top-args)
                        ((item-based-cxn-p anti-unified-cxn) source-slot-args)))
        (set-data args :generalisation-top-lvl-args
                  (loop for arg in pattern-top-args
                        collect (or (rest (assoc arg pattern-bindings)) arg)))))
    args))

(defmethod compute-meaning-args (anti-unification-result (anti-unified-cxn fcg-construction) (source-args blackboard))
  (let ((args (make-blackboard)))
    (with-slots (generalisation
                 pattern-bindings
                 source-bindings
                 pattern-delta
                 source-delta) anti-unification-result
      (let ((pattern-slot-args (extract-slot-meaning-args anti-unified-cxn))
            (pattern-top-args (extract-top-lvl-meaning-args anti-unified-cxn))
            (source-slot-args (find-data source-args :slot-meaning-args))
            (source-top-args (or (find-data source-args :top-lvl-meaning-args)
                                 ;; compute entirely new top args, like for a holophrase cxn
                                 ;; by putting together the source delta and the generalisation
                                 (holistic-meaning-top-args
                                  (append (substitute-bindings (fcg::reverse-bindings source-bindings) generalisation) source-delta)
                                  (get-configuration (cxn-inventory anti-unified-cxn) :meaning-representation-formalism)))))
        (loop for (pattern-var . generalisation-var) in pattern-bindings
              for (source-var . nil) in source-bindings
              ;when (or (find-anywhere pattern-var pattern-delta)
              ;         (find-anywhere source-var source-delta)
              ;         (find pattern-var pattern-slot-args)
              ;         (find source-var source-slot-args)
              ;         (> (count pattern-var pattern-bindings :key #'car) 1)
              ;         (> (count source-var source-bindings :key #'car) 1))
              do (push-data args :pattern-top-lvl-args pattern-var)
                 (push-data args :source-top-lvl-args source-var)
                 (push-data args :generalisation-slot-args generalisation-var))
        (set-data args :pattern-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn) pattern-top-args)
                        ((item-based-cxn-p anti-unified-cxn) pattern-slot-args)))
        (set-data args :source-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn) source-top-args)
                        ((item-based-cxn-p anti-unified-cxn) source-slot-args)))
        (set-data args :generalisation-top-lvl-args
                  (loop for arg in pattern-top-args
                        collect (or (rest (assoc arg pattern-bindings)) arg)))))
    args))
|#
