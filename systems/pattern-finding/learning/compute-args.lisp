(in-package :pf)

;;;;;;;;;;;;;;;;;;
;; compute args ;;
;;;;;;;;;;;;;;;;;;

(defgeneric compute-form-args (anti-unification-result cxn source-args)
  (:documentation "Compute the form-args from the anti-unification result
                   obtained by anti-unifying the observation with 'thing',
                   which can be a cxn or a cipn."))

(defmethod compute-form-args (anti-unification-result cxn source-args)
  (let ((form-representation (get-configuration (cxn-inventory cxn) :form-representation-formalism)))
    (compute-form-args-aux anti-unification-result cxn source-args form-representation)))

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
        

#|
(defmethod compute-form-args-aux (anti-unification-result
                                  (anti-unified-cxn fcg-construction)
                                  (source-args blackboard)
                                  (mode (eql :sequences)))
  (let ((args (make-blackboard)))
    (with-slots (generalisation
                 pattern-delta
                 source-delta) anti-unification-result
      (let* (;; determine outermost boundaries
             (leftmost-boundary
              (if (variable-p (first generalisation))
                (make-var 'leftmost)
                (left-boundary (first generalisation))))
             (rightmost-boundary
              (if (variable-p (last-elt generalisation))
                (make-var 'rightmost)
                (right-boundary (last-elt generalisation))))
             ;; slot/top-args
             (pattern-slot-args (extract-slot-form-args anti-unified-cxn))
             (pattern-top-args (extract-top-lvl-form-args anti-unified-cxn))
             (source-slot-args (find-data source-args :slot-form-args))
             (source-top-args (find-data source-args :top-lvl-form-args)))
        ;; generalisation top lvl args
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
                  ;; check if there are variables in the pattern delta that also
                  ;; occur as a pattern-slot-arg. if so, take this variable (and
                  ;; its buddy) as :pattern-slot-arg
                  (loop for (var . seq-pred) in pattern-delta
                        when (find var pattern-slot-args)
                        append (let* ((position (position var pattern-slot-args))
                                      (buddy-position (if (evenp position) (1+ position) (1- position)))
                                      (buddy (nth buddy-position pattern-slot-args)))
                                 (list var buddy))))  
        ;; dirty tricks here....
        (setf generalisation (remove-if #'variable-p generalisation))
        (setf pattern-delta (mapcar #'rest pattern-delta))
        (setf source-delta (mapcar #'rest source-delta))))
    args))
|#
              

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
              when (or (find-anywhere pattern-var pattern-delta)
                       (find-anywhere source-var source-delta)
                       (find pattern-var pattern-slot-args)
                       (find source-var source-slot-args)
                       (> (count pattern-var pattern-bindings :key #'car) 1)
                       (> (count source-var source-bindings :key #'car) 1))
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



(defgeneric compute-meaning-args (anti-unification-result cxn source-args)
  (:documentation "Compute the meaning-args from the anti-unification result
                   obtained by anti-unifying the observation with 'thing',
                   which can be a cxn or a cipn."))


(defmethod compute-meaning-args (anti-unification-result (anti-unified-cxn fcg-construction) (source-args blackboard))
  (let ((args (make-blackboard)))
    (with-slots (generalisation
                 pattern-bindings
                 source-bindings
                 pattern-delta
                 source-delta) anti-unification-result
      (let (;; pattern slot args are meaning-args that are used in the units that
            ;; represent slots in the pattern (anti-unified-cxn)
            (pattern-slot-args (extract-slot-meaning-args anti-unified-cxn))
            ;; pattern top args are meaning-args that are used on the contributing
            ;; part of the pattern (anti-unified cxn)
            (pattern-top-args (extract-top-lvl-meaning-args anti-unified-cxn))
            ;; for now, this is always empty... but it will be used when adding recursion
            (source-slot-args (find-data source-args :slot-meaning-args))
            (source-top-args (or (find-data source-args :top-lvl-meaning-args)
                                 ;; compute entirely new top args, like for a holophrase cxn
                                 ;; by putting together the source delta and the generalisation
                                 (holistic-meaning-top-args
                                  (append (substitute-bindings (fcg::reverse-bindings source-bindings) generalisation) source-delta)
                                  (get-configuration (cxn-inventory anti-unified-cxn) :meaning-representation-formalism)))))
        ;; loop through both bindings lists (pattern and source) at the same time
        ;; whenever a variable from the bindings list is used in the delta
        ;; OR that variable is a slot-arg
        ;; OR that variable occurs more than once in the bindings (decoupled link)
        ;; THEN use that variable as an arg 
        (loop for (pattern-var . generalisation-var) in pattern-bindings
              for (source-var . nil) in source-bindings
              when (or (find-anywhere pattern-var pattern-delta)
                       (find-anywhere source-var source-delta)
                       (find pattern-var pattern-slot-args)
                       (find source-var source-slot-args)
                       (> (count pattern-var pattern-bindings :key #'car) 1)
                       (> (count source-var source-bindings :key #'car) 1))
              do (push-data args :pattern-top-lvl-args pattern-var)
                 (push-data args :source-top-lvl-args source-var)
                 (push-data args :generalisation-slot-args generalisation-var))
        ;; when anti-unifying with a holistic cxn, the pattern delta cxn will be item-based
        ;; and the top-args/slot-args are reversed, so pass on the top-lvl args of the
        ;; previous pattern as top-lvl args for the next pattern

        ;; when anti-unifying with an item-based cxn, the pattern delta cxn will also be
        ;; item-based, so pass on the slot args of the previous pattern as the slot args
        ;; for the next pattern
        (set-data args :pattern-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn) pattern-top-args)
                        ((item-based-cxn-p anti-unified-cxn) pattern-slot-args)))

        ;; when anti-unifying with a holistic cxn, the source delta cxn will be item-based
        ;; and the top-args/slot-args are reversed, so pass on the top-lvl args of the
        ;; previous source as the top-lvl args for the next source
        (set-data args :source-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn) source-top-args)
                        ((item-based-cxn-p anti-unified-cxn) source-slot-args)))

        ;; 
        (set-data args :generalisation-top-lvl-args
                  (loop for arg in pattern-top-args
                        collect (or (rest (assoc arg pattern-bindings)) arg)))))
    args))


#|
(defun compute-args (anti-unification-result anti-unified-cxn)
  "Loop over all variables in both bindings lists.
   Whenever a variable occurs in either one of the delta's,
   or it is a slot-arg; add it as an arg!"
  (let ((args (make-blackboard)))
    (with-slots (generalisation
                 pattern-bindings
                 source-bindings
                 pattern-delta
                 source-delta) anti-unification-result
      (let ((raw-pattern-delta (remove-arg-predicates pattern-delta))
            (raw-source-delta (remove-arg-predicates source-delta))
            (pattern-delta-slot-args (find-all 'slot-arg pattern-delta :key #'first))
            (source-delta-slot-args (find-all 'slot-arg source-delta :key #'first))
            (pattern-delta-top-args (find-all 'top-arg pattern-delta :key #'first))
            (source-delta-top-args (find-all 'top-arg source-delta :key #'first)))
        ;; determine the args that connect the generalisation to both delta's
        (loop for (pattern-var . generalisation-var) in pattern-bindings
              for (source-var . nil) in source-bindings
              when (or (find-anywhere pattern-var raw-pattern-delta)
                       (find-anywhere source-var raw-source-delta)
                       (find pattern-var pattern-delta-slot-args :key #'second)
                       (find source-var source-delta-slot-args :key #'second)
                       (> (count pattern-var pattern-bindings :key #'first) 1)
                       (> (count source-var source-bindings :key #'first) 1))
              do (push-data args :pattern-top-lvl-args pattern-var)
                 (push-data args :source-top-lvl-args source-var)
                 (push-data args :generalisation-slot-args generalisation-var))
        ;; determine the args that connect the cxns from the delta's to other cxns
        (set-data args :pattern-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn)
                         (reverse (mapcar #'second pattern-delta-top-args)))
                        ((item-based-cxn-p anti-unified-cxn)
                         (mapcar #'second pattern-delta-slot-args))))
        (set-data args :source-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn)
                         (reverse (mapcar #'second source-delta-top-args)))
                        ((item-based-cxn-p anti-unified-cxn)
                         (mapcar #'second source-delta-slot-args))))))
    args))
|#
