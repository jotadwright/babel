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

;;;;;;;;;;;;;;;
;; sequences ;;
;;;;;;;;;;;;;;;

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
  (with-slots (generalisation
               pattern-delta
               source-delta) anti-unification-result
    (let* ((starts-with-slot (consp (first generalisation)))
           (ends-with-slot (consp (last-elt generalisation)))
           ;; make sequence predicates from the generalisation and delta's
           (generalisation-predicates
            (loop for elem in generalisation
                  when (stringp elem)
                  collect (let* ((pos (position elem generalisation :test #'equal))
                                 (prev-elem (unless (= pos 0) (nth (1- pos) generalisation)))
                                 (next-elem (nth (1+ pos) generalisation))
                                 (left-boundary (if prev-elem (cdr prev-elem) (make-var 'lb)))
                                 (right-boundary (if next-elem (car next-elem) (make-var 'rb))))
                            (list 'sequence elem left-boundary right-boundary))))
           (pattern-delta-predicates
            (loop for (boundaries . seq-str) in pattern-delta
                  collect (list 'sequence seq-str (car boundaries) (cdr boundaries))))
           (source-delta-predicates
            (loop for (boundaries . seq-str) in source-delta
                  collect (list 'sequence seq-str (car boundaries) (cdr boundaries))))
           ;; slot/top-args
           (pattern-slot-args (extract-slot-form-args anti-unified-cxn))
           (pattern-top-args (extract-top-lvl-form-args anti-unified-cxn))
           (source-slot-args (find-data source-args :slot-form-args))
           (source-top-args (or (find-data source-args :top-lvl-form-args)
                                ;; compute entirely new top args, like for a holophrase cxn,
                                ;; by putting together the source delta and the generalisation
                                (holistic-form-top-args
                                 ;; to do: have a function for putting back the generalisation and the delta
                                 nil
                                 (get-configuration (cxn-inventory anti-unified-cxn) :form-representation-formalism))))
           ;; outermost boundaries
           (leftmost-boundary
            (if starts-with-slot
              (car (first generalisation))
              (left-boundary (first generalisation-predicates))))
           (rightmost-boundary
            (if ends-with-slot
              (cdr (last-elt generalisation))
              (right-boundary (last-elt generalisation-predicates))))
           ;; blackboard for args
           (args (make-blackboard)))
      ;; generalisation top lvl args
      (push-data args :generalisation-top-lvl-args rightmost-boundary)
      (push-data args :generalisation-top-lvl-args leftmost-boundary)
      ;; generalisation slot args + pattern/source top lvl args
      (loop with len = (length generalisation-predicates)
            for predicate in generalisation-predicates
            for index from 1
            do (cond ((and (= index 1) starts-with-slot)
                      (let ((boundaries (first generalisation)))
                        (push-data args :generalisation-slot-args
                                   (car boundaries))
                        (push-data args :generalisation-slot-args
                                   (cdr boundaries))))
                     ((= index 1)
                      (push-data args :generalisation-slot-args
                                 (fourth predicate)))
                     ((and (= index len) ends-with-slot)
                      (let ((boundaries (last-elt generalisation)))
                        (push-data args :generalisation-slot-args
                                   (car boundaries))
                        (push-data args :generalisation-slot-args
                                   (cdr boundaries))))
                     ((= index len)
                      (push-data args :generalisation-slot-args
                                 (third predicate)))
                     (t
                      (push-data args :generalisation-slot-args
                                 (third predicate))
                      (push-data args :generalisation-slot-args
                                 (fourth predicate)))))
      (set-data args :generalisation-slot-args
                (reverse (get-data args :generalisation-slot-args)))
      (set-data args :pattern-top-lvl-args
                (loop for predicate in (reverse pattern-delta-predicates)
                      append (list (third predicate) (fourth predicate))))
      (set-data args :source-top-lvl-args
                (loop for predicate in (reverse source-delta-predicates)
                      append (list (third predicate) (fourth predicate))))
      ;; pattern slot args + source slot args
      (set-data args :pattern-slot-args
                (loop for predicate in pattern-delta-predicates
                      when (and (find (third predicate) pattern-slot-args)
                                (find (fourth predicate) pattern-slot-args))
                      append (list (third predicate) (fourth predicate))))
      (set-data args :source-slot-args
                (loop for predicate in source-delta-predicates
                      when (and (find (third predicate) source-slot-args)
                                (find (fourth predicate) source-slot-args))
                      append (list (third predicate) (fourth predicate))))
      ;; cleanup
      (setf generalisation generalisation-predicates)
      (setf pattern-delta (remove-if #'(lambda (p) (string= (second p) "")) pattern-delta-predicates))
      (setf source-delta (remove-if #'(lambda (p) (string= (second p) "")) source-delta-predicates))
      ;; done!
      args)))

;;;;;;;;;;;;;;;;;;
;; string+meets ;;
;;;;;;;;;;;;;;;;;;

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
                           (find pattern-var pattern-top-args)
                           (find source-var source-top-args)
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
          (set-data args :source-top-lvl-args source-top-args))))
    args))
        
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
                           (find pattern-var pattern-top-args)
                           (find source-var source-top-args)
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
          (set-data args :source-top-lvl-args source-top-args))))
    args))
            
              
              
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
