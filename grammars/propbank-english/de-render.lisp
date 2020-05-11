(in-package :propbank-english)

(defmethod de-render ((utterance string) (mode (eql :de-render-constituents-dependents))
                      &key cxn-inventory &allow-other-keys)
  "De-renders an utterance as a combination of Spacy dependency structure and benepar constituency structure."
  (let* (;; Query the penelope API for a syntactic analysis
         (spacy-benepar-analysis (nlp-tools:get-penelope-syntactic-analysis utterance))
         ;; Make unit names for the different units, and store them with the unit id.
         (unit-name-ids (loop for node in spacy-benepar-analysis
                              for node-id = (node-id node)
                              for node-type = (node-type node)
                              if (equal node-type 'phrase)
                              collect (cons node-id (make-const (format nil "~{~a-~^ ~}" (node-phrase-types node))))
                              else
                              collect (cons node-id (make-const (node-string node)))))
         ;; Make units
         (units (loop for node in spacy-benepar-analysis
                      
                      ;; attributes
                      for node-type = (node-type node)
                      for node-string = (node-string node)
                      for parent-id = (node-parent node)
                      for node-id = (node-id node)
                      
                      ;; for phrase nodes
                      if (equal node-type 'phrase)
                      collect `(,(cdr (assoc node-id unit-name-ids))
                                (node-type ,node-type)
                                (string ,node-string)
                                (span (,(node-start node) ,(node-end node)))
                                (parent ,(cdr (assoc parent-id unit-name-ids)))
                                (constituents ,(find-constituents node-id spacy-benepar-analysis unit-name-ids))
                                (phrase-type ,(node-phrase-types node))
                                ;(word-order ,(loop for constituent in (find-constituents node-id spacy-benepar-analysis unit-name-ids)))

                                )
                      
                      ;; for leaf nodes
                      else
                      collect `(,(cdr (assoc node-id unit-name-ids))
                                (node-type ,node-type)
                                (string ,node-string)
                                (span (,(node-start node) ,(node-end node)))
                                (parent ,(cdr (assoc parent-id unit-name-ids)))
                                (head ,(cdr (assoc (node-dependency-head node) unit-name-ids)))
                                (dependents ,(find-dependents node-id spacy-benepar-analysis unit-name-ids))
                                (lemma ,(node-lemma node))
                                (lex-class ,(node-lex-class node))
                                (dependency-label ,(node-dependency-label node))
                                (named-entity-type ,(node-named-entity-type node)))))
         
         ;; Make transient structure
         (transient-structure (make-instance 'coupled-feature-structure 
                                             :left-pole units
                                             :right-pole '((root)))))
    transient-structure))


(defun find-constituents (node-id spacy-benepar-analysis unit-name-ids)
  "Returns unit names of constituents."
  (loop for node in spacy-benepar-analysis
        if (equalp node-id (node-parent node))
        collect (cdr (assoc (node-id node) unit-name-ids))))
  

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