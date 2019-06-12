(in-package :frame-extractor)



;;copied from Babel2/sharing/fcg-hybrids (15/10/2018)
(defstruct word-dependency-spec string syn-role unit-name pos-tag node-id head-id)

(defun make-word-specs-for-boundaries (boundaries dependency-tree)
  "A useful function to have for customizing the translate-dependency-tree method."
  (loop for boundary in boundaries
        for dependency in dependency-tree
        collect (make-word-dependency-spec :string (dp-get-token dependency)
                                           :unit-name (first boundary)
                                           :syn-role (dp-get-dependency dependency)
                                           :pos-tag (dp-get-tag dependency)
                                           :node-id (parse-integer (dp-get-node-id dependency))
                                           :head-id (dp-get-head-id dependency))))

;;De-render-for-conll-evaluation

(defmethod de-render ((conll-sentence list) (mode (eql :de-render-conll))
                      &key cxn-inventory &allow-other-keys)
  (declare (ignorable mode))
  (let ((list-utterance (mapcar #'(lambda(word)
                                     (cdr (assoc :FORM word))) conll-sentence))
         (strings nil)
         (meets-constraints nil)
         (sequence nil)
         (units nil)
         (transient-structure nil))
    ;; collect strings and unit-names
    (loop for token in list-utterance
          for unit-name = (make-const token nil)
          do
          (setf strings (append strings (list `(string ,unit-name ,token))))
          (setf sequence (append sequence (list unit-name))))
    ;; collect meets constraints
    (do ((left strings (rest left)))
	((null (cdr left)))
      (setf meets-constraints (append meets-constraints (list `(meets ,(second (second left)) ,(second (first left)))))))
    ;; make units
    (setf units
          (loop for conll-word in conll-sentence
          for i from 0
          for unit-name = (nth i sequence)
          collect (make-unit :name unit-name 
                     :features `((head ,(when (numberp  (cdr (assoc :HEAD conll-word)))
                                          (nth (- (cdr (assoc :HEAD conll-word)) 1) sequence)))
                                 (dependents ,(find-dependents conll-word conll-sentence sequence))
                                 (form ((string ,unit-name
                                                ,(cdr (assoc :FORM conll-word)))))
                                 (dependency
                                  ((pos-tag
                                    ,(intern (upcase (cdr (assoc :XPOS conll-word))) :fcg))
                                   (edge
                                    ,(intern (upcase (cdr (assoc :DEPREL conll-word))) :fcg))))))))
    ;; Creating the actual initial transient structure
    (setf transient-structure
          (make-instance 'coupled-feature-structure 
                         :left-pole (append
                                     `((root (meaning ())
                                            (sem-cat ())
                                            (form ,meets-constraints)
                                            (syn-cat ())))
                                     units)
		   :right-pole '((root))))
    ;; Setting sequence and utterance data to the blackboard
    (set-data transient-structure :sequence sequence)
    (set-data transient-structure :utterance (list (list-of-strings->string list-utterance)))
    transient-structure))

(defun find-dependents (conll-word conll-sentence sequence)
  (let* ((conll-word-id (cdr (assoc :ID conll-word)))
        (dependents (loop for word in (remove conll-word-id conll-sentence :key #'(lambda(w) (cdr (assoc :ID w))) :test #'=)
                          for head-id = (cdr (assoc :HEAD word))
                          when (and (numberp head-id)
                                    (numberp conll-word-id)
                                    (= head-id conll-word-id))
                          collect (- (cdr (assoc :ID word)) 1))))
    (mapcar #'(lambda(index) (nth index sequence)) dependents)))


;;copied from Babel2/applications/frame-extractor (10/10/2018)
(defmethod de-render ((utterance string) (mode (eql :raw-dependency-translation))
                      &key cxn-inventory &allow-other-keys)
  (declare (ignorable mode))
  (let* ((dependency-tree (get-penelope-dependency-analysis utterance))
         (list-utterance (dp-build-utterance-as-list-from-dependency-tree dependency-tree)) ;;reusing dependency segmentation
         (base-transient-structure (de-render list-utterance
                                             :de-render-with-scope :cxn-inventory cxn-inventory)))
    (set-data base-transient-structure :utterance (list (list-of-strings->string list-utterance)))
    (translate-dependency-tree base-transient-structure dependency-tree)))

(defun translate-dependency-tree (base-transient-structure dependency-tree)
  (declare (ignore cxn-inventory))
  (let* ((boundaries (fcg-get-boundaries base-transient-structure))
         (word-specs (make-word-specs-for-boundaries boundaries dependency-tree))
         (units-id-and-name (loop for spec in word-specs
                                  collect (list (word-dependency-spec-node-id spec)
                                                (word-dependency-spec-unit-name spec))))
         (structure-to-append (loop for word-spec in word-specs
                                    for parent = (unless (= (word-dependency-spec-node-id word-spec)
                                                            (word-dependency-spec-head-id word-spec))
                                                   (second (assoc (word-dependency-spec-head-id word-spec)
                                                                  units-id-and-name)))
                                    for subunits = (loop for other-word-spec in (remove word-spec word-specs :test #'equal)
                                                         when (= (word-dependency-spec-head-id other-word-spec)
                                                                  (word-dependency-spec-node-id word-spec))
                                                         collect (word-dependency-spec-unit-name other-word-spec))
                                    collect (make-unit :name (word-dependency-spec-unit-name word-spec)
                                                       :features `((head ,parent)
                                                                   (dependents ,subunits)
                                                                   (form ((string ,(word-dependency-spec-unit-name word-spec)
                                                                                  ,(word-dependency-spec-string word-spec))
                                                                          ))
                                                                   (dependency
                                                                    ((pos-tag
                                                                      ,(intern (upcase (word-dependency-spec-pos-tag word-spec)) :fcg))
                                                                     (edge
                                                                      ,(intern (upcase (word-dependency-spec-syn-role word-spec)) :fcg)))))))))

    (setf (left-pole-structure base-transient-structure)
          (cons (get-root (left-pole-structure base-transient-structure)) ;;original root
                structure-to-append))
    base-transient-structure))



