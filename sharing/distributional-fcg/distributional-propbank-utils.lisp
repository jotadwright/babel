(in-package :propbank-grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Heuristic that sums cosine similarity  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod apply-heuristic ((node cip-node) (mode (eql :embedding-similarity)))
  (let* ((applied-cxn (get-original-cxn (car-applied-cxn (cipn-car node))))
         (proto-role-embeddings (find-data (blackboard applied-cxn) :proto-role-embeddings))
         (lemma-embedding-pointer (attr-val applied-cxn :lemma-embedding-pointer))
         (bindings (car-second-merge-bindings (cipn-car node))))
    (if proto-role-embeddings
      (loop for proto-role-embedding in proto-role-embeddings
            for pointer = (variablify (car proto-role-embedding))
            for similarity = (cdr (assoc pointer bindings))
            when similarity
              summing similarity into similarity-total
            finally (return similarity-total))
      (if lemma-embedding-pointer
        (cdr (assoc (variablify lemma-embedding-pointer) bindings))
        0))))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions to make proto-embeddings  ;;
;;     and add them to the cxn-inventory      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-proto-role-embeddings (cxn-inventory)
  "For each argument-structure-cxn, set the proto-embedding in the corresponding slot."
  (let* ((arg-structure-cxns (get-argument-structure-cxns cxn-inventory)))
    (loop for arg-structure-cxn in arg-structure-cxns
          for strings-in-roles = (find-data (blackboard arg-structure-cxn) :strings-in-roles)
          for number-of-roles = (length (first strings-in-roles))
          for proto-role-embeddings = (loop for i from 0 to (- number-of-roles 1)
                                            for strings-in-role = (loop for str in strings-in-roles
                                                                        collect (cdr (nth i str)))
                                            for proto-role-embedding-pointer = (car (nth i (first strings-in-roles)))
                                            for proto-role-embedding = (make-proto-embedding strings-in-role)
                                            when proto-role-embedding
                                              collect (cons proto-role-embedding-pointer proto-role-embedding))
          do (append-data arg-structure-cxn :proto-role-embeddings proto-role-embeddings))))

;; use the sum-list-of-vectors of math in babel utils
(defmethod combine-word-embeddings ((embeddings-list list) &key (mode (eql 'addition)))
  "Combine word embeddings by adding them."
   (apply #'mapcar #'+ embeddings-list))

(defun make-proto-embedding (strings-in-role)
  "Expects a list of strings which can contain multiple words.
   Loop over these strings, split them and get for each word the word-embedding,
   then concatenate all these embeddings. "
  (let ((embeddings-in-role (loop for string in strings-in-role
                                  for splitted-strings = (split-sequence:split-sequence #\Space string :remove-empty-subseqs t)
                                
                                  append (loop for splitted-string in splitted-strings
                                               collect (last-elt (nlp-tools:get-word-embedding (downcase splitted-string)))))))
    (combine-word-embeddings embeddings-in-role :mode 'addition)))

(defun get-argument-structure-cxns (cxn-inventory)
  (let* ((cxns (constructions-list cxn-inventory)))
    (loop for cxn in cxns
          for cxn-type = (cdr (assoc :label (attributes cxn)))
          when (equal cxn-type 'argument-structure-cxn)
            collect cxn)))



;; Added function to add the embeddings from the learned cxns and the ones from the transient structures of the annotation files to the blackboard of the cxn-inventory.
(defun add-embeddings-to-cxn-inventory (cxn-inventory)
  "Retrieve all token embeddings for constructions that carry a token
attribute and store them in the cxn inventory's blackboard under the
field :cxn-token-embeddings"
  (remove-data (blackboard cxn-inventory) :cxn-token-embeddings)
  ;(remove-data (blackboard cxn-inventory) :ts-token-embeddings)
  (loop for cxn in (constructions cxn-inventory)
        for cxn-token = (attr-val cxn :token)
        for lemma-embedding-pointer = (attr-val cxn :lemma-embedding-pointer)
        for proto-embeddings = (find-data (blackboard cxn) :proto-role-embeddings)
        when cxn-token
          do (append-data (blackboard cxn-inventory) :cxn-token-embeddings (list (cons lemma-embedding-pointer
                                                                                       (second (nlp-tools:get-word-embedding cxn-token)))))
        when proto-embeddings
          do (append-data (blackboard cxn-inventory) :cxn-token-embeddings proto-embeddings)))

;; Added function to get the string of the lemma
(defun node-lemma-string (spacy-benepar-analysis-leaf-node)
  "Returns the lemma of the leaf node"
  (cdr (assoc :lemma spacy-benepar-analysis-leaf-node)))