(in-package :propbank-grammar)

;;;;;;;;;;;;;;;;;;
;;              ;;
;; De-rendering ;;
;;              ;;
;;;;;;;;;;;;;;;;;;

(defmethod de-render ((utterance conll-sentence) (mode (eql :de-render-constituents-dependents))
                      &key &allow-other-keys)
  "De-renders a conll-sentence."
  (initial-transient-structure utterance))


(defmethod de-render ((utterance string) (mode (eql :de-render-constituents-dependents))
                      &key (syntactic-analysis nil) (tokenize? t) &allow-other-keys)
  "De-renders an utterance as a combination of Spacy dependency structure and benepar constituency structure."
  (let ((syntactic-analysis (cond (syntactic-analysis)
                                  (tokenize?
                                   (nlp-tools:get-penelope-syntactic-analysis utterance))
                                  ((not tokenize?)
                                   (nlp-tools:get-penelope-syntactic-analysis
                                    (split-sequence:split-sequence #\Space utterance :remove-empty-subseqs t))))))
    (create-initial-transient-structure-based-on-benepar-analysis syntactic-analysis)))


(defun create-initial-transient-structure-based-on-benepar-analysis (spacy-benepar-analysis)
  (let* (;; Make unit names for the different units, and store them with the unit id.
         (unit-name-ids (loop for node in spacy-benepar-analysis
                              for node-id = (node-id node)
                              for node-type = (node-type node)
                              if (equal node-type 'phrase)
                              collect (cons node-id (make-const (format nil "~{~a-~}" (node-phrase-types node))))
                              else
                              collect (cons node-id (make-const (node-string node)))))
         ;; Make units
         (units (loop for node in spacy-benepar-analysis
                      ;; attributes
                      for node-type = (node-type node)
                      for node-string = (node-string node)
                      for parent-id = (if (and (or (equal (node-lex-class node) 'rp);;node itself is a particle
                                                   (equal (node-dependency-label node) 'prt))
                                               (equalp "V" (subseq (format nil "~a"
                                                                           (node-lex-class (get-node (node-dependency-head node)
                                                                                                     spacy-benepar-analysis)))
                                                                   0 1))
                                               (adjacent-nodes? (node-dependency-head node) node spacy-benepar-analysis))
                                        (node-dependency-head node) ;;we're dealing with a phrasal particle
                                        (node-parent node))
                      for node-id = (node-id node)
                      for unit-name = (cdr (assoc node-id unit-name-ids))
                      for syn-class = (cond ((equal node-type 'phrase)
                                             (node-phrase-types node))
                                            ((and (equal (node-dependency-label node) 'aux)
                                                  (not (equal (node-lex-class node) 'md)))
                                             '(AUX))
                                            ((equal (node-dependency-label node) 'auxpass)
                                             '(AUXPASS))
                                            ((equalp "V" (subseq (format nil "~a" (node-lex-class node)) 0 1))
                                             '(V))
                                            ((and (member (node-lex-class node) '(nnp nns nn nnps prp prp$) :test #'equalp)
                                                  ;;
                                                  (not (and (find parent-id spacy-benepar-analysis ;; has parent
                                                                  :test #'equalp :key #'node-id)
                                                            (member 'np ;; parent is np
                                                                    (node-phrase-types 
                                                                     (find parent-id spacy-benepar-analysis
                                                                           :test #'equalp :key #'node-id)) :test #'equalp))))
                                             '(NP))
                                            (t
                                             `(,(node-lex-class node))))

                                
                       collect `(,unit-name
                                     (node-type ,node-type)
                                     (string ,node-string)
                                     (span (,(node-start node) ,(node-end node)))
                                     (parent ,(cdr (assoc parent-id unit-name-ids)))
                                     (syn-class ,syn-class)
                                     ,@(when (equal node-type 'phrase)
                                         `((constituents ,(find-constituents node-id spacy-benepar-analysis unit-name-ids))
                                           (word-order ,(find-adjacency-constraints node-id spacy-benepar-analysis unit-name-ids))))
                                     ,@(when (and (equal node-type 'phrase)
                                                  (find 'vp syn-class))
                                         (loop for constituent in (find-constituent-units node-id spacy-benepar-analysis)
                                               for dependency-label = (assoc ':DEPENDENCY--LABEL constituent)
                                               when (and dependency-label
                                                         (string= (cdr dependency-label) "auxpass"))
                                               return '((passive +))
                                               finally (return '((passive -)))))
                                     ,@(when (equal node-type 'leaf)
                                         `((lemma ,(node-lemma node))
                                           (dependency-label ,(node-dependency-label node)))))))

         ;; Make transient structure
         (transient-structure (make-instance 'coupled-feature-structure 
                                             :left-pole (run-phrasal-verb-check units)
                                             :right-pole '((root)))))
    transient-structure))

(defun adjacent-nodes? (node-id-1 node-2 spacy-benepar-analysis)
  (let* ((node-1 (find node-id-1 spacy-benepar-analysis :key #'node-id))
         (end-index-node-1 (node-end node-1))
         (start-index-node-2 (node-start node-2)))
    (when (= start-index-node-2 end-index-node-1)
      t)))

(defun parent-unit (unit unit-structure)
  (let ((parent-unit-name (unit-feature-value unit 'parent)))
    (find parent-unit-name unit-structure :key #'unit-name)))
  
(defun run-phrasal-verb-check (units)
  "Checks if there are particle units in the unit structure, ."
  (let* ((phrasal-verb-units
          (loop for unit in units
                for parent-unit = (find (unit-feature-value unit 'parent) units :key #'unit-name)
                when (equal (unit-feature-value parent-unit 'node-type) 'leaf)
                collect (cons parent-unit unit)))
         (phrasal-vp-units
          (loop for unit in units
                for constituents = (unit-feature-value unit 'constituents)
                when (intersection (mapcar #'second phrasal-verb-units);;all particle units
                                   constituents)
                collect unit)))

    (loop for vp-unit in phrasal-vp-units
          for (phrasal-verb-unit . particle-unit) in phrasal-verb-units
          when (intersection (unit-feature-value vp-unit 'constituents)
                             (list (unit-name phrasal-verb-unit) (unit-name particle-unit)))
          do (let* ((additional-vp-unit (create-phrasal-vp-unit phrasal-verb-unit particle-unit))
                    (new-phrasal-verb-unit (update-unit-feature-value phrasal-verb-unit 'parent (unit-name additional-vp-unit)))
                    (new-particle-unit (update-unit-feature-value particle-unit 'parent (unit-name additional-vp-unit)))
                    (other-constituents (set-difference (unit-feature-value vp-unit 'constituents)
                                                        (list (unit-name phrasal-verb-unit) (unit-name particle-unit))))
                    (new-vp-unit
                     (if (equalp (unit-feature-value vp-unit 'span) (unit-feature-value additional-vp-unit 'span)) ;;double phrasal-vps
                       (let* ((parent-unit (parent-unit vp-unit units))
                              (other-parent-constituents (set-difference (unit-feature-value parent-unit 'constituents)
                                                                         (list (unit-name vp-unit)))))
                         (update-unit-feature-value (parent-unit vp-unit units) 'constituents
                                                    (append other-parent-constituents (list (unit-name additional-vp-unit)))))
                      (update-unit-feature-value vp-unit 'constituents (append other-constituents (list (unit-name additional-vp-unit)))))))
               
               ;;delete 3 old units
               (delete phrasal-verb-unit units :test #'equal)
               (delete particle-unit units :test #'equal)
               (delete vp-unit units :test #'equal)
               ;;add 4 new units
               (pushend new-vp-unit units)
               (unless (unit-feature additional-vp-unit 'parent)
                 (pushend  `(parent ,(unit-name new-vp-unit)) additional-vp-unit))
               (pushend additional-vp-unit units)
               (pushend new-phrasal-verb-unit units)
               (pushend new-particle-unit units)))
    (merge-units units)))


(defun merge-units (unit-structure)
  (let* ((unit-groups (group-by unit-structure #'unit-name))
         (unit-names (mapcar #'first unit-groups)))
    (loop for (nil . units) in unit-groups
          if (> (length units) 1)
          collect (let ((duplicated-unit (first units))
                        (constituents-feature-value
                         (remove-if-not #'(lambda (unit-name)
                                            (member unit-name unit-names))
                                        (remove-duplicates (loop for unit in units
                                                                 append (unit-feature-value unit 'constituents))))))
                  (update-unit-feature-value duplicated-unit 'constituents constituents-feature-value))          else append units)))

;(merge-units (left-pole-structure *saved-cfs*))

(defun update-unit-feature-value (unit feature-name new-feature-value)
  (assert new-feature-value)
  `(,(unit-name unit) 
    ,@(loop for unit-feature in (unit-body unit)
            if (eql (first unit-feature) feature-name)
            collect (list feature-name new-feature-value)
            else collect unit-feature)))

(defun create-phrasal-vp-unit (verb-unit particle-unit)
  (let ((phrasal-lemma (intern (upcase (format nil "~a-~a"
                                               (unit-feature-value verb-unit 'lemma)
                                               (unit-feature-value particle-unit 'lemma))))))
  `(,(make-const "PHRASAL-VP")
    (constituents (,(unit-name verb-unit) ,(unit-name particle-unit)))
    (node-type phrase)
    (span ,(merge-unit-spans verb-unit particle-unit))
    (string ,(merge-unit-strings verb-unit particle-unit))
    (syn-class (vp))
  ;  (parent ,(unit-name parent-unit))
    (word-order ((adjacent ,(unit-name verb-unit) ,(unit-name particle-unit))
                 (precedes ,(unit-name verb-unit) ,(unit-name particle-unit))))
    (lemma ,phrasal-lemma))))

                                             
(defun merge-unit-strings (unit-1 unit-2)
  (let ((string-1 (unit-feature-value unit-1 'string))
        (string-2 (unit-feature-value unit-2 'string)))
    (format nil "~a ~a" string-1 string-2)))

(defun merge-unit-spans (unit-1 unit-2)
  (let ((span-1 (unit-feature-value unit-1 'span))
        (span-2 (unit-feature-value unit-2 'span)))
    (list (first span-1) (second span-2))))

(defun find-adjacency-constraints (node-id spacy-benepar-analysis unit-name-ids)
  "Returns a set of adjacency constraints for a given node id."
  (let* ((constituent-nodes (find-all-if #'(lambda (node) (equal node-id (node-parent node))) spacy-benepar-analysis))
         (ordered-constituent-nodes (sort constituent-nodes #'< :key #'node-start)))
    (loop with previous-unit-names = nil
          for node in ordered-constituent-nodes
          for unit-name = (cdr (assoc (node-id node) unit-name-ids))
          ;; collect adjacency constraints
          if previous-unit-names
          collect `(adjacent ,(first previous-unit-names) ,unit-name)
          into adjacency-constraints
          ;; set the previous-word-id
          if previous-unit-names
          append (loop for p-un in (reverse previous-unit-names)
                       collect `(precedes ,p-un ,unit-name))
          into precedes-constraints
          do (push unit-name previous-unit-names)
          finally (return
                    (append adjacency-constraints precedes-constraints)))))

(defun find-constituents (node-id spacy-benepar-analysis unit-name-ids)
  "Returns unit names of constituents."
  (loop for node in spacy-benepar-analysis
        if (equalp node-id (node-parent node))
        collect (cdr (assoc (node-id node) unit-name-ids))))

(defun find-constituent-units (node-id spacy-benepar-analysis )
  "Returns list of constituents."
  (loop for node in spacy-benepar-analysis
        if (equalp node-id (node-parent node))
        collect node))
  
(defun find-dependents (node-id spacy-benepar-analysis unit-name-ids)
  "Returns unit names of dependents."
  (loop for node in spacy-benepar-analysis
        if (and (equalp node-id (node-dependency-head node))
                (not (equalp node-id (node-id node))))
        collect (cdr (assoc (node-id node) unit-name-ids))))

(defun node-type (spacy-benepar-analysis-node)
  "Returns the type of the node, i.e. 'phrase or 'leaf."
  (intern (upcase (cdr (assoc :node--type spacy-benepar-analysis-node)))))

(defun node-string (spacy-benepar-analysis-node)
  "Returns the string of the node."
  (cdr (assoc :string spacy-benepar-analysis-node)))

(defun node-phrase-types (spacy-benepar-analysis-node)
  "Returns the phrase types of the node"
  (mapcar (compose #'intern #'upcase)
          (cdr (assoc :phrase--types spacy-benepar-analysis-node))))

(defun node-id (spacy-benepar-analysis-node)
  "Returns the id of the node"
  (cdr (assoc :node--id spacy-benepar-analysis-node)))

(defun node-start (spacy-benepar-analysis-node)
  "Returns the start index of the node"
  (cdr (assoc :start spacy-benepar-analysis-node)))

(defun node-end (spacy-benepar-analysis-node)
  "Returns the end index of the node"
  (cdr (assoc :end spacy-benepar-analysis-node)))

(defun node-parent (spacy-benepar-analysis-node)
  "Returns the id of the parent node"
  (cdr (assoc :parent--constituent spacy-benepar-analysis-node)))

(defun node-lemma (spacy-benepar-analysis-leaf-node)
  "Returns the lemma of the leaf node"
  (intern (upcase (cdr (assoc :lemma spacy-benepar-analysis-leaf-node)))))

(defun node-lex-class (spacy-benepar-analysis-leaf-node)
  "Returns the lex-class of the leaf node"
  (intern (upcase (cdr (assoc :lex--class spacy-benepar-analysis-leaf-node)))))

(defun node-dependency-label (spacy-benepar-analysis-leaf-node)
  "Returns the dependency-label of the leaf node"
  (intern (upcase (cdr (assoc :dependency--label spacy-benepar-analysis-leaf-node)))))

(defun node-dependency-head (spacy-benepar-analysis-leaf-node)
  "Returns the id of the head of the leaf-node"
  (cdr (assoc :dependency--head spacy-benepar-analysis-leaf-node)))

(defun node-named-entity-type (spacy-benepar-analysis-leaf-node)
  "Returns the id of the head of the leaf-node"
  (let ((ent-type (cdr (assoc :named--entity--type spacy-benepar-analysis-leaf-node))))
    (when (and ent-type (not (equalp ent-type "")))
      (intern (upcase ent-type)))))

(defun get-node (node-id spacy-benepar-analysis)
  (find node-id spacy-benepar-analysis :key #'node-id))
