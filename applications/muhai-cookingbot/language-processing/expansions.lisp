(in-package :muhai-cookingbot)



(defmethod fcg-expand ((type (eql :flatten-ingredients))
                       &key value source bindings merge? cxn-inventory)
  "Flatten ingredient list."
  (declare (ignore source))
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
  (declare (ignore merge? source))
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
          for ontological-class-from-ts  = source
          for ontological-class-from-bindings = (if (string= (get-base-name value) "ONTOLOGICAL-CLASS-UTTERANCE")
                                                  (cdr (assoc "ONTOLOGICAL-CLASS-WORLD" bindings-list :key #'get-base-name :test #'string=))
                                                  (cdr (assoc "ONTOLOGICAL-CLASS-UTTERANCE" bindings-list :key #'get-base-name :test #'string=)))
          if (and ontological-class-from-ts
                  ontological-class-from-bindings
                  (> (cosine-similarity (ontological-vector ontological-class-from-ts cxn-inventory)
                                        (ontological-vector ontological-class-from-bindings cxn-inventory))
                     0.9))
          collect bindings-list into new-bindings
          else if (and ontological-class-from-ts
                       ontological-class-from-bindings
                       (<= (cosine-similarity (ontological-vector ontological-class-from-ts cxn-inventory)
                                              (ontological-vector ontological-class-from-bindings cxn-inventory))
                           0.9))
          collect +fail+ into new-bindings
          else
          collect bindings-list into new-bindings
          finally (return (values value new-bindings))))))

               

#|
  (if merge?
    (let ((ontological-class-utterance (cdr (assoc "ONTOLOGICAL-CLASS-UTTERANCE" ;;brittle!!
                                                   bindings :key #'get-base-name :test #'string=)))
          (ontological-class-world (cdr (assoc "ONTOLOGICAL-CLASS-WORLD"
                                               bindings :key #'get-base-name :test #'string=))))
      (when (and ontological-class-utterance ontological-class-world)
        (let* ((utterance-vector (hashtable-to-vector ontological-class-utterance cxn-inventory))
               (world-vector (hashtable-to-vector ontological-class-world cxn-inventory))
               (similarity-score (cosine-similarity utterance-vector world-vector)))
          (when (> similarity-score 0.9)
            ;(set-data (blackboard cxn-inventory) :similarity-score similarity-score)
            ;(format t "comparing ~a and ~a: ~a ~%" ontological-class-utterance ontological-class-world  (get-data (get-self) :similarity-score))
            (values value bindings)))))
    (values value bindings))|#
  
(defmethod fcg-expand ((type (eql :number))
                       &key value source bindings merge? cxn-inventory)
  "Matches on a number in a tag."
  (cond  ;; When called in either merge or remove-special-operators, than just
         ;; return the value (the pattern from the cxn).
         ((and (eq merge? t) (eq source nil))
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
  (declare (ignore merge? source))
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

