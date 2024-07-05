(in-package :muhai-cookingbot)



(defmethod fcg-expand ((type (eql :flatten-ingredients))
                       &key value source bindings merge? cxn-inventory)
  "Flatten ingredient list."
  (declare (ignore cxn-inventory))
  (if merge?
    (when (assoc "INGREDIENTS" bindings :key #'get-base-name :test #'string=)
      (let* ((all-ingredient-bindings (loop for (key . value) in bindings
                                       when (string= (get-base-name key) "INGREDIENTS")
                                       collect (cons key value)))
             (ingredient-list-binding (loop for (key . value) in all-ingredient-bindings
                                            unless (find key all-ingredient-bindings :key #'cdr)
                                            return (cons key value))))
        (if ingredient-list-binding
          (values (cdr ingredient-list-binding)
                  bindings)

          (values value bindings))))
    (values source bindings)))

(defmethod fcg-expand ((type (eql :lookup-in-ontology))
                       &key value source bindings merge? cxn-inventory)
  "Returns number from string."
  (declare (ignore merge? source cxn-inventory))
  (unless (null bindings)
    (let* ((class (handler-case (find-class value)
                    (error (c)
                      (error (format nil "Class ~S not found in ontology while expanding cxn." value)))))
           (superclasses (set-difference (mapcar #'class-name (all-superclasses class))
                                         (mapcar #'class-name (all-superclasses (find-class 'entity))))))
      (values `((ontological-class ,value)
                (ontological-types ,superclasses))
              bindings))))


(defmethod fcg-expand ((type (eql :compare-ontological-vectors))
                       &key value source bindings merge? cxn-inventory)
  "Use cosine similarity metric to compare ontological classes."
  (cond (merge?
         (values value bindings))
        (t
         (loop 
          for bindings-list in bindings
          for ontological-class-from-ts = source
          for value-base-name = (get-base-name value)
          for ontological-class-from-bindings
              = (cond ((search "ONTOLOGICAL-CLASS-UTTERANCE" value-base-name)
                       (let* ((suffix (when (> (length value-base-name) (length "ONTOLOGICAL-CLASS-UTTERANCE"))
                                        (subseq value-base-name (1+ (length "ONTOLOGICAL-CLASS-UTTERANCE")))))
                              (search-for (if suffix
                                            (format nil "ONTOLOGICAL-CLASS-WORLD-~a" (upcase suffix))
                                            "ONTOLOGICAL-CLASS-WORLD")))
                         (cdr (assoc search-for bindings-list :key #'get-base-name :test #'string=))))
                      ((search "ONTOLOGICAL-CLASS-WORLD" value-base-name)
                       (let* ((suffix (when (> (length value-base-name) (length "ONTOLOGICAL-CLASS-WORLD"))
                                        (subseq value-base-name (1+ (length "ONTOLOGICAL-CLASS-WORLD")))))
                              (search-for (if suffix
                                            (format nil "ONTOLOGICAL-CLASS-UTTERANCE-~a" (upcase suffix))
                                            "ONTOLOGICAL-CLASS-UTTERANCE")))
                         (cdr (assoc search-for bindings-list :key #'get-base-name :test #'string=)))))
              
          if (and ontological-class-from-ts
                  ontological-class-from-bindings
                  (> (cosine-similarity (ontological-vector ontological-class-from-ts cxn-inventory)
                                        (ontological-vector ontological-class-from-bindings cxn-inventory))
                     0.88))
          collect bindings-list into new-bindings
          else if (and ontological-class-from-ts
                       ontological-class-from-bindings
                       (<= (cosine-similarity (ontological-vector ontological-class-from-ts cxn-inventory)
                                              (ontological-vector ontological-class-from-bindings cxn-inventory))
                           0.88))
          collect +fail+ into new-bindings
          else
          collect bindings-list into new-bindings
          finally (return (values value new-bindings))))))

  
(defmethod fcg-expand ((type (eql :number-sequences))
                       &key value source bindings merge? cxn-inventory)
  "Matches on a number in a tag by means of sequence predicates."
  (declare (ignore cxn-inventory))
  (if (and (eq merge? t) (eq source nil)) 
    ;; When called in either merge or remove-special-operators, than just return the value (the pattern from the cxn).
    (values value bindings)
    ;; When called in matching and the source contains a predicate with an number
    ;; return the value (so that it can be removed from the root during merge)
    ;; and add a new binding between the variable from the value and the number
    ;; in the source.   
    (let ((sequence-predicates (remove-if-not #'(lambda (predicate)
                                                  (eql (first predicate) 'sequence)) (rest value))))
      (if sequence-predicates
        (let ((sequence-bindings-lists
               (match-pattern-sequence-predicates-in-source-sequence-predicates-numbers-only sequence-predicates source bindings)))
          (if sequence-bindings-lists
            ;; If sequence predicates could be matched, add bindings
            (let ((new-bindings (merge-bindings-lists bindings sequence-bindings-lists)))
              (values source (mapcar #'(lambda (b)
                                    (extend-bindings
                                     (second (first (remove-special-operators value bindings)))
                                     "120" ;;retrieve value in source
                                     b)) new-bindings)))
            ;; Fail if sequence predicates could not be matched
            nil))
        ;; If there are no sequence predicates (but only e.g. precedes constraints), continue and deal with it in merge (above).
        (values source bindings)))))


(defun match-pattern-sequence-predicates-in-source-sequence-predicates-numbers-only (pattern-sequence-predicates source-sequence-predicates bindings)
  "Matches two sets of sequence-predicates, returns validated bindings-lists with all options. Takes into account existing bindings."
  (when source-sequence-predicates
    (loop for pattern-sequence-predicate in pattern-sequence-predicates
          collect (match-pattern-sequence-predicate-in-source-sequence-predicates-numbers-only
                   pattern-sequence-predicate source-sequence-predicates)
            into possible-bindings-lists-per-pattern-predicate
          finally (return (loop for list-of-lr-pairs in (filter-valid-sequence-bindings-lists
                                                         (apply #'cartesian-product possible-bindings-lists-per-pattern-predicate) bindings)
                                collect (loop for lr-pair in  list-of-lr-pairs
                                              append lr-pair))))))

(defun match-pattern-sequence-predicate-in-source-sequence-predicates-numbers-only (pattern-sequence-predicate source-sequence-predicates)
  "Matches a sequence-predicate against a list of sequence-predicates, returns unfiltered potential bindings lists."
  (loop for source-sequence-predicate in source-sequence-predicates
        append (match-sequence-predicates-numbers-only pattern-sequence-predicate source-sequence-predicate)))

(defun match-sequence-predicates-numbers-only (pattern-predicate source-predicate)
  "Matches two sequence-predicates, returns unfiltered potential bindings lists."
    (let* ((source-string (second source-predicate))
           (index-offset (third source-predicate))
           (all-match-positions (cl-ppcre:all-matches "[0-9]+" source-string))
           (possible-bindings (loop for (left right) on all-match-positions by #'cddr
                                        collect (list (+ left index-offset) (+ right index-offset)))))
      (loop for pb in possible-bindings
            collect (list (fcg::make-binding (third pattern-predicate) (first pb))
                          (fcg::make-binding (fourth pattern-predicate) (second pb))))))

(defmethod fcg-expand ((type (eql :number))
                       &key value source bindings merge? cxn-inventory)
  "Matches on a number in a tag."
  (declare (ignore cxn-inventory))
  (cond (;; When called in either merge or remove-special-operators, than just return the value (the pattern from the cxn).
         (and (eq merge? t) (eq source nil))
         (values value bindings))
         ;; When called in matching and the source contains a predicate with an number
         ;; return the value (so that it can be removed from the root during merge)
         ;; and add a new binding between the variable from the value and the number
         ;; in the source.
         ((loop for predicate in source
                thereis (and (stringp (third predicate))
                             (numberp (handler-case (read-from-string (third predicate))
                                        (error (c) nil)))))
          (loop for predicate in source
                when (and (stringp (third predicate))
                          (numberp (handler-case (read-from-string (third predicate))
                                        (error (c) nil))))
                return
                (values value 
                        (mapcar #'(lambda (b)
                                    (extend-bindings
                                     (third (first (remove-special-operators value bindings)))
                                     (third predicate)
                                     b)) bindings))))
            
         (t nil)))

(defmethod fcg-expand ((type (eql :parse-integer))
                       &key value source bindings merge? cxn-inventory)
  "Returns number from string."
  (declare (ignore merge? source cxn-inventory))
  (unless (null bindings)
    (values (read-from-string value) bindings)))

#|
(defmethod fcg-expand ((type (eql :expand-ingredients))
                       &key value source bindings merge?)
  "Returns number from string."
  (declare (ignore merge? source))
  (unless (null bindings)
    (when (> (length (cdr value)) 1)
      (error (format nil "Meaning contains more than one predicate!!")))
    (let* ((ingredient-vars (last-elt (second value)))
           (meaning-predicate-without-ingredient-vars
            (remove ingredient-vars (second value) :test #'equalp)))
          
      (values `(,(append meaning-predicate-without-ingredient-vars
                         (if (listp ingredient-vars)
                           (first ingredient-vars)
                           (list ingredient-vars))))
              bindings))))
|#

