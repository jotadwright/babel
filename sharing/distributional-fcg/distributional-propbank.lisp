(ql:quickload :propbank-grammar)
(ql:quickload :distributional-fcg)

(in-package :propbank-grammar)


;(activate-monitor trace-fcg)
;(deactivate-all-monitors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set host for nlp-tools: embedding-api and spacy-api ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")
;(setf nlp-tools::*embedding-host* "http://127.0.0.1:5001")
; 
;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting file paths ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *annotations-file-path*
  (merge-pathnames 
   (make-pathname :directory '(:relative "distributional-fcg")
                  :name "small-corpus" :type "conll")
   cl-user:*babel-corpora*))

(defparameter *annotations-file-path-teach*
  (merge-pathnames 
   (make-pathname :directory '(:relative "distributional-fcg")
                  :name "small-corpus-teach" :type "conll")
   cl-user:*babel-corpora*))

(defparameter *annotations-file-path*
  (merge-pathnames 
   (make-pathname :directory '(:relative "distributional-fcg")
                  :name "small-corpus-ditransitive" :type "conll")
   cl-user:*babel-corpora*))

(defparameter *ontonotes-annotations-storage-file* (merge-pathnames (make-pathname :directory (cons :relative '("propbank-annotations"))
                                                                                   :name "ontonotes-annotations"
                                                                                   :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                                                                    *babel-corpora*))


(defparameter *ewt-annotations-storage-file* (merge-pathnames (make-pathname :directory (cons :relative '("propbank-annotations"))
                                                                                   :name "ewt-annotations"
                                                                                   :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                                                                    *babel-corpora*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For ewt and ontonotes: loading from store files ;;
;;       Otherwise: read from conll file           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Loading propbank annotations can take a while

;(load-propbank-annotations 'ewt :ignore-stored-data nil) ; *ewt-annotations*
;(load-propbank-annotations 'ontonotes :ignore-stored-data nil)

;; small ewt-annotation corpus to test
;(defparameter *ewt-annotations-small* (loop for i from 0 to 10 collect (nth i (train-split *ewt-annotations*))))



;(defparameter *annotations* (read-propbank-conll-file *annotations-file-path*))
;(defparameter *annotations* (read-propbank-conll-file *annotations-file-path-teach*))
;(defparameter *annotations* (read-propbank-conll-file *annotations-file-path*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting training configurations for grammar  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *training-configuration*
    '((:de-render-mode .  :de-render-constituents-dependents)
      (:node-tests :check-double-role-assignment)
      (:parse-goal-tests :no-valid-children :meaning-extracted) ;
      (:construction-inventory-processor-mode . :heuristic-search)
      (:search-algorithm . :best-first)   
      (:heuristics
       :nr-of-applied-cxns
       :nr-of-units-matched-x2 ;;nr-of-units-matched
       :edge-weight
       :embedding-similarity
       )
      ;;Additional heuristics: :prefer-local-bindings :frequency
      (:heuristic-value-mode . :sum-heuristics-and-parent)
      (:sort-cxns-before-application . nil)
      (:node-expansion-mode . :full-expansion)
      (:hash-mode . :hash-lemma)
      (:replace-when-equivalent . nil)
      (:learning-modes
       :core-roles
       :argm-leaf
       :argm-pp
       :argm-sbar
       :argm-phrase-with-string)
      (:cxn-supplier-mode . :hashed-categorial-network)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Learning the grammar  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 
(learn-propbank-grammar
 (shuffle *annotations*)
 :selected-rolesets nil
 :excluded-rolesets nil
 :cxn-inventory '*train-grammar*
 :fcg-configuration *training-configuration*)

 |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make the embeddings (for roles and lexical items) ;;
;;   and add the embeddings to the cxn-inventory     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(set-proto-role-embeddings *train-grammar*)

;(add-embeddings-to-cxn-inventory *train-grammar*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comprehending to test  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(comprehend "he teaches a language" :timeout nil)
;(comprehend "he teaches the children" :timeout nil)
;(comprehend "jesus teaches english" :timeout nil)


;(comprehend "" :timeout nil)

;(comprehend "the man rides a bike" :timeout nil)

;(comprehend "the man gave his mother a book" :timeout nil)

;(comprehend "the husband handed his wife a book" :timeout nil)

;(comprehend "he abandoned his child" :timeout nil)
;(comprehend "he committed a crime" :timeout nil)
;(comprehend "the bank committed 1,2 million dollar" :timeout nil)

#|
(neighbouring-categories 'abandon-01 (categorial-network *train-grammar*))

(add-element (make-html (categorial-network *train-grammar*) :weights t))

(add-element (make-html *train-grammar*))

(gethash 'propbank-grammar::ABANDON\(V\)-1 (graph-utils::nodes (fcg::graph (categorial-network *train-grammar*))))

(graph-utils:neighbors (fcg::graph (categorial-network *train-grammar*)) 'propbank-grammar::ABANDON\(V\)-1)

(gethash 13 (graph-utils::ids (fcg::graph (categorial-network *train-grammar*))))

(set-configuration *train-grammar*  :category-linking-mode  :graph-cosine-similarity)

(categorial-network *train-grammar*)


(setf *similar-nodes* (graph-utils::similar-nodes-weighted-cosine 'propbank-grammar::send.01-20 (fcg::graph (categorial-network *train-grammar*))))
; => ((PROPBANK-GRAMMAR::SEND.01-20 . 1.0) (PROPBANK-GRAMMAR::ARG0\(NP\)+V\(V\)+ARG2\(NP\)+ARG1\(NP\)-25 . 0.40824828) (PROPBANK-GRAMMAR::ARG0\(NP\)+V\(V\)+ARG1\(NP\)+ARG2\(PP\)-56 . 0.40824828) (PROPBANK-GRAMMAR::SEND\(V\)-19 . 0.33333334) (PROPBANK-GRAMMAR::GIVE\(V\)-30 . 0.32732683) (PROPBANK-GRAMMAR::GIVE.01-29 . 0.32732683) (PROPBANK-GRAMMAR::SELL\(V\)-5 . 0.28867513) (PROPBANK-GRAMMAR::SELL.01-8 . 0.28867513) (PROPBANK-GRAMMAR::TRANSFER\(V\)-2 . 0.28867513) (PROPBANK-GRAMMAR::TRANSFER.01-2 . 0.28867513) (PROPBANK-GRAMMAR::READ\(V\)-10 . 0.16666667) (PROPBANK-GRAMMAR::READ.01-12 . 0.16666667))

(setf *similar-nodes* (graph-utils::similar-nodes-cosine 'propbank-grammar::send.01-20 (fcg::graph (categorial-network *train-grammar*))))
; => ((PROPBANK-GRAMMAR::SEND.01-20 . 1.0) (PROPBANK-GRAMMAR::SEND\(V\)-19 . 0.6666667) (PROPBANK-GRAMMAR::GIVE\(V\)-30 . 0.6666667) (PROPBANK-GRAMMAR::GIVE.01-29 . 0.6666667) (PROPBANK-GRAMMAR::SELL\(V\)-5 . 0.40824828) (PROPBANK-GRAMMAR::SELL.01-8 . 0.40824828) (PROPBANK-GRAMMAR::TRANSFER\(V\)-2 . 0.40824828) (PROPBANK-GRAMMAR::TRANSFER.01-2 . 0.40824828) (PROPBANK-GRAMMAR::READ\(V\)-10 . 0.33333334) (PROPBANK-GRAMMAR::READ.01-12 . 0.33333334) (PROPBANK-GRAMMAR::ARG0\(NP\)+V\(V\)+ARG2\(NP\)+ARG1\(NP\)-25 . 0.28867513) (PROPBANK-GRAMMAR::ARG0\(NP\)+V\(V\)+ARG1\(NP\)+ARG2\(PP\)-56 . 0.18257419))



(comprehend "the man smurfs a book to his wife")

(activate-monitor trace-fcg)
(set-configuration *train-grammar* :category-linking-mode :graph-cosine-similarity )

(comprehend "the man sent a book to his wife" :timeout nil)

(comprehend "he sold his mother the car" :timeout nil)

(comprehend "he sold the car to his mother" :timeout nil)

|#

;; Aan te passen: 


(defmethod categories-linked-p (category-1 category-2 (categorial-network categorial-network) (mode (eql :graph-cosine-similarity)))
  "Succeeds of nodes are similar based on weighted cosine similarity."
  (let ((similarity (graph-utils::weighted-graph-cosine-similarity category-1 category-2 (fcg::graph categorial-network))))
    (if (= similarity 0)
      t
      nil)))

(defmethod apply-heuristic ((node cip-node) (mode (eql :edge-weight)))
  "Returns the weight of the categorial network edge that was used in
matching."
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Heuristic that sums cosine similarity  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod apply-heuristic ((node cip-node) (mode (eql :embedding-similarity)))
  (let* ((applied-cxn (get-original-cxn (car-applied-cxn (cipn-car node))))
         (proto-role-embeddings (find-data (blackboard applied-cxn) :proto-role-embeddings))
         (bindings (car-second-merge-bindings (cipn-car node))))
    (loop for proto-role-embedding in proto-role-embeddings
          for pointer = (variablify (car proto-role-embedding))
          for similarity = (cdr (assoc pointer bindings))
          when similarity
            summing similarity into similarity-total
          finally (return similarity-total))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions to make proto-embeddings  ;;
;;     and add them to the cxn-inventory      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-proto-role-embeddings (cxn-inventory)
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
    
;(cosine-similarity (get-proto-embedding (list "the man" "the woman")) (last-elt (nlp-tools:get-word-embedding "bottle")))

;(cosine-similarity (get-proto-embedding (list "the man" "the woman")) (get-proto-embedding (list "the child" "the dog")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quickly overwrite to test. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add the embedding to the attributes (token)
;; changes: add an embedding pointer to the lexical cxn
(defun add-lexical-cxn (gold-frame v-unit cxn-inventory propbank-sentence)
   "Creates a new lexical construction if necessary, otherwise
 increments frequency of existing cxn. Also adds a new lexical category
 to the categorial network. Returns the lexical category."
   (let* ((lemma (feature-value (find 'lemma (unit-body v-unit) :key #'feature-name)))
          (syn-class (feature-value (find 'syn-class (unit-body v-unit) :key #'feature-name)))
          (lex-category (intern (symbol-name (make-id (format nil "~a~a" (truncate-frame-name (frame-name gold-frame)) syn-class)))))
          (cxn-name (intern (upcase (format nil "~a~a-cxn" lemma syn-class))))
          (equivalent-cxn (find-cxn cxn-name cxn-inventory :hash-key lemma :key #'name)))
     (if equivalent-cxn
       ;; If cxn already exists: increment frequency
       (progn
         (incf (attr-val equivalent-cxn :score))
         (attr-val equivalent-cxn :lex-category))
       ;; Else make new cxn
       (when lemma
         (let* ((lex-lemma (intern (subseq (symbol-name lemma) 0 (search "-" (symbol-name lemma)))))
                (lemma-embedding-pointer (intern (upcase (format nil "->~a" lex-lemma))))
                (token (feature-value (find 'token (unit-body v-unit) :key #'feature-name)))
                (token-string (feature-value (find 'string token :key #'feature-name))))
         (if (equalp syn-class '(vp))

               (eval
                `(def-fcg-cxn ,cxn-name
                              ((?phrasal-unit
                                (footprints (lex))
                                (lex-category ,lex-category))
                               (?lex-unit
                                (footprints (,lemma)))
                               <-
                               (?phrasal-unit
                                --
                                (footprints (NOT lex))
                                (lemma ,lemma)
                                (syn-class ,syn-class))
                               (?lex-unit
                                --
                                (footprints (NOT ,lemma))
                                
                                
                                (token (embedding ,lemma-embedding-pointer))
                                (parent ?phrasal-unit)))
                              
                              :attributes (:lemma ,lemma
                                           :lex-category ,lex-category
                                           :label lexical-cxn
                                           :score 1
                                           :token ,token-string)
                              :description ,(sentence-string propbank-sentence)
                              :disable-automatic-footprints t
                              :cxn-inventory ,cxn-inventory)))

               (eval
                `(def-fcg-cxn ,cxn-name
                              ((?lex-unit
                                (footprints (,lemma))
                             (lex-category ,lex-category))
                               <-
                               (?lex-unit
                                --
                                (footprints (NOT ,lemma))
                                
                                
                                (token (embedding ,lemma-embedding-pointer))
                                (syn-class ,syn-class)))
                              :attributes (:lemma ,lemma
                                           :lex-category ,lex-category
                                           :label lexical-cxn
                                           :score 1
                                           :token ,token-string)
                              :description ,(sentence-string propbank-sentence)
                              :disable-automatic-footprints t
                              :cxn-inventory ,cxn-inventory)))
             (add-category lex-category cxn-inventory :recompute-transitive-closure nil) 
             lex-category))))

;; remove lemma feature from cxn, otherwise cxns can never match if not exact lemma, what we want to avoid with the distributional experiment. 
(defun add-word-sense-cxn (gold-frame v-unit cxn-inventory propbank-sentence lex-category gram-category)
  "Creates a new word sense construction if necessary, otherwise
increments frequency of existing cxn. Adds a new sense category to the
categorial network and returns it."
  (let* ((lemma (or (feature-value (find 'lemma (unit-body v-unit) :key #'feature-name))
                    (feature-value (find 'string (unit-body v-unit) :key #'feature-name))))
         (cxn-name (intern (upcase (format nil "~a(~a)-cxn" (frame-name gold-frame) lemma))))
         (equivalent-cxn (find cxn-name (constructions-list cxn-inventory) :key #'name))
         (sense-category (intern (symbol-name (make-id (frame-name gold-frame))))))
    
    (if equivalent-cxn
      
      ;; If word sense cxn already exists
      ;;---------------------------------
      (progn
        (incf (attr-val equivalent-cxn :score))
        
        ;; edge between gram-category and sense-category
        (if (link-exists-p gram-category (attr-val equivalent-cxn :sense-category) cxn-inventory)
          ;;connection between gram and sense category exists: increase edge weight
          (progn
            (incf-link-weight gram-category (attr-val equivalent-cxn :sense-category) cxn-inventory :delta 1.0 :link-type nil)
            (incf-link-weight gram-category (attr-val equivalent-cxn :sense-category) cxn-inventory :delta 1.0 :link-type 'gram-sense))
          ;;add new link
          (progn
            (add-link gram-category
                      (attr-val equivalent-cxn :sense-category) cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
            (add-link gram-category
                      (attr-val equivalent-cxn :sense-category) cxn-inventory :weight 1.0 :link-type 'gram-sense :recompute-transitive-closure nil)))
        
        ;; edge between lex-category and sense-category
        (if (link-exists-p lex-category (attr-val equivalent-cxn :sense-category) cxn-inventory)
          (progn
            (incf-link-weight lex-category (attr-val equivalent-cxn :sense-category) cxn-inventory :delta 1.0 :link-type nil)
            (incf-link-weight lex-category (attr-val equivalent-cxn :sense-category) cxn-inventory :delta 1.0 :link-type 'lex-sense))
          (progn
            (add-link lex-category
                      (attr-val equivalent-cxn :sense-category) cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
            (add-link lex-category
                      (attr-val equivalent-cxn :sense-category) cxn-inventory :weight 1.0 :link-type 'lex-sense :recompute-transitive-closure nil)))

        (attr-val equivalent-cxn :sense-category))
      
      ;; Else make new cxn
      ;;-------------------
      (progn (assert lemma)
        (eval
         `(def-fcg-cxn ,cxn-name
                       ((?lex-unit
                         (footprints (ws)))
                        <-
                        (?lex-unit
                         --
                         (gram-category ,sense-category)
                         (lex-category ,sense-category)
                         (frame ,(intern (upcase (frame-name gold-frame))))
                         (footprints (NOT ws))))
                       :disable-automatic-footprints t
                       :attributes (:sense-category ,sense-category
                                    :label word-sense-cxn
                                    :score 1)
                       :description ,(sentence-string propbank-sentence)
                       :cxn-inventory ,cxn-inventory))
        
        (add-category sense-category cxn-inventory :recompute-transitive-closure nil)
        (add-link gram-category sense-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link gram-category sense-category cxn-inventory :weight 1.0 :link-type 'gram-sense :recompute-transitive-closure nil)
        (add-link lex-category sense-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link lex-category sense-category cxn-inventory :weight 1.0 :link-type 'lex-sense :recompute-transitive-closure nil)
        
        sense-category))))


(defun add-grammatical-cxn (gold-frame core-units-with-role cxn-inventory propbank-sentence lex-category)
  "Learns a grammatical construction capturing all core roles and adds
a grammatical category to the categorial network. Returns the
grammatical category."
  
  (let* ((ts-unit-structure (ts-unit-structure propbank-sentence cxn-inventory))
         (gram-category (make-gram-category core-units-with-role))
         (cxn-units-with-role (loop for unit in core-units-with-role
                                    collect (make-propbank-conditional-unit-with-role unit gram-category 'fee)))
         (cxn-units-without-role (make-propbank-conditional-units-without-role core-units-with-role
                                                                               cxn-units-with-role ts-unit-structure))
         (passive (loop for unit in cxn-units-without-role
                        when (eql '+ (unit-feature-value (cdr unit) 'passive))
                          return t))
         (contributing-unit (make-propbank-contributing-unit core-units-with-role gold-frame gram-category 'fee))
         (schema (make-cxn-schema core-units-with-role cxn-units-with-role :core-roles :passive? passive))
         (cxn-name (intern (upcase (format nil "~a+~a-cxn" gram-category (length cxn-units-without-role)))))
         (equivalent-cxn (find-equivalent-cxn schema
                                              (syn-classes (append cxn-units-with-role
                                                                   cxn-units-without-role))
                                              cxn-inventory)))
    
    (if equivalent-cxn
      
      ;; Grammatical construction already exists
      ;;----------------------------------------
      (progn
        ;;1) Increase its frequency
        (incf (attr-val equivalent-cxn :score))
        ;;2) Check if there was already a link in the categorial network between the lex-category and the gram-category:
        (if (link-exists-p lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory)
          ;;a) If yes, increase edge weight
          (progn
            (incf-link-weight lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :delta 1.0 :link-type nil)
            (incf-link-weight lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :delta 1.0 :link-type 'lex-gram))
          ;;b) Otherwise, add new connection (weight 1.0)
          (progn
            (add-link lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :weight 1.0 :link-type nil
                      :recompute-transitive-closure nil)
            (add-link lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :weight 1.0 :link-type 'lex-gram
                      :recompute-transitive-closure nil)))
        ;;2.2) (Added for distributional-propbank:)
        (append-data equivalent-cxn :strings-in-roles (list (loop for core-unit in core-units-with-role
                                                                  for role = (role-type (first core-unit))
                                                                  for string = (last-elt (fourth core-unit))
                                                                  for unit-name = (second core-unit)
                                                                  for class = (last-elt (last-elt (seventh core-unit)))
                                                                  when (not (string= role "V"))
                                                                    collect (cons (intern (upcase (format nil "->~a--~a" role gram-category))) string))))
        ;;3) Return gram-category
        (attr-val equivalent-cxn :gram-category))

      ;; Else: Create a new grammatical category for the observed pattern + add category and link to the categorial network
      ;;--------------------------------------------------------------------------------------------------------------------
      (when (and cxn-units-with-role (v-lemma core-units-with-role))
        
        (add-category gram-category cxn-inventory :recompute-transitive-closure nil)
        (add-link lex-category gram-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link lex-category gram-category cxn-inventory :weight 1.0 :link-type 'lex-gram :recompute-transitive-closure nil)
        (multiple-value-bind (cxn-inventory cxn)
        
            (eval `(def-fcg-cxn ,cxn-name
                                (,contributing-unit
                                 <-
                                 ,@cxn-units-with-role
                                 ,@cxn-units-without-role
                                 )
                                :disable-automatic-footprints t
                                :attributes (:schema ,schema
                                             :lemma nil
                                             :label argument-structure-cxn
                                             :score 1
                                             :gram-category ,gram-category)
                                :description ,(sentence-string propbank-sentence)
                                :cxn-inventory ,cxn-inventory))
          
          (append-data cxn :strings-in-roles (list (loop for core-unit in core-units-with-role
                                                         for role = (role-type (first core-unit))
                                                         for string = (last-elt (fourth core-unit))
                                                         for unit-name = (second core-unit)
                                                         for class = (last-elt (last-elt (seventh core-unit)))
                                                         when (not (string= role "V"))
                                                           collect (cons (intern (upcase (format nil "->~a--~a" role gram-category))) string))))
          gram-category)))))

;; added embedding pointer for proto-roles, the embedding itself is added after learning.
;; embedding only needed when not V, so only in the roles. 
(defun make-propbank-conditional-unit-with-role (unit-with-role category footprint &key (lemma nil) (string nil) (frame-evoking nil))
  "Makes a conditional unit for a propbank cxn based on a unit in the
initial transient structure that plays a role in the frame."
  (let* ((unit (cdr unit-with-role))
         (unit-name (variablify (unit-name unit)))
         (parent (when (cadr (find 'parent (unit-body unit) :key #'feature-name))
                   (variablify (cadr (find 'parent (unit-body unit) :key #'feature-name)))))
         (syn-class (find 'syn-class (unit-body unit) :key #'feature-name))
         (dependency-label (when (find 'rb (feature-value syn-class))
                             (find 'dependency-label (unit-body unit) :key #'feature-name)))
         (phrase-embedding-pointer (intern (upcase (format nil "->~a--~a" (role-type (first unit-with-role)) category))))) ;;phrase-embedding-pointer is cxn-specific, so reuse cxn-name
    ;;a FEE unit also has the features lemma and footprints
    (if (equalp "V" (role-type (car unit-with-role)))
      `(,unit-name
        --
        (parent ,parent)
        ,syn-class
        (footprints (NOT ,footprint))
        ,@(when frame-evoking
            '((frame-evoking +)))
        ,@(when category
            `((lex-category ,category))))
      `(,unit-name
        --
        (parent ,parent)
        ,syn-class
        (token (embedding ,phrase-embedding-pointer))
        ,@(when dependency-label `(,dependency-label))
        ,@(when lemma
            `((lemma ,lemma)))
         ,@(when string
            `((string ,string)))
        ))))

;; Changes: get embeddings of the lemmas and add them to the units of the TS and the blackboard of the TS.
(defun create-initial-transient-structure-based-on-benepar-analysis (spacy-benepar-analysis)
  (let* (;; Make unit names for the different units, and store them with the unit id.
         (unit-name-ids (loop for node in spacy-benepar-analysis
                              for node-id = (node-id node)
                              for node-type = (node-type node)
                              if (equal node-type 'fcg-server::phrase)
                              collect (cons node-id (make-const (format nil "~{~a-~}" (node-phrase-types node))))
                              else 
                              collect (cons node-id (make-const (node-string node)))))
         (embeddings-and-units (multiple-value-list
                                (loop for node in spacy-benepar-analysis
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
                                      for lemma-embedding = (when (equal node-type 'leaf) (nlp-tools:get-word-embedding (node-lemma-string node)))
                                       
                                      for lemma-embedding-pointer = (when lemma-embedding (intern (upcase (string-append "->" (node-lemma-string node)))))
                                      for phrase-embedding = (when (equal node-type 'phrase) (combine-word-embeddings (mapcar #'last-elt (nlp-tools:get-word-embeddings (mapcar #'downcase (split-sequence:split-sequence #\Space node-string :remove-empty-subseqs t))))))
                                      for phrase-embedding-pointer = (when phrase-embedding (intern (upcase (string-append "->" node-string))))
                                      when lemma-embedding
                                        collect  (cons lemma-embedding-pointer (first (cdr lemma-embedding))) into embeddings
                                      when phrase-embedding 
                                        collect  (cons phrase-embedding-pointer  phrase-embedding) into embeddings
                                        
                                      collect `(,unit-name
                                                (node-type ,node-type)
                                                (string ,node-string)
                                                (span (,(node-start node) ,(node-end node)))
                                                (parent ,(cdr (assoc parent-id unit-name-ids)))
                                                (syn-class ,syn-class)
                                                ,@(when (equal node-type 'phrase)
                                                    `((constituents ,(find-constituents node-id spacy-benepar-analysis unit-name-ids))
                                                      (word-order ,(find-adjacency-constraints node-id spacy-benepar-analysis unit-name-ids))
                                                      (token ((string ,node-string)
                                                              (embedding ,phrase-embedding-pointer)))
                                                      ))
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
                                                      (token ((string ,(node-lemma-string node))
                                                              (embedding ,lemma-embedding-pointer)))
                                                      (dependency-label ,(node-dependency-label node))))) into units
                                      finally (return (values embeddings units)))))
         
         ;; Make transient structure
         (transient-structure (make-instance 'coupled-feature-structure 
                                             :left-pole (run-phrasal-verb-check (second embeddings-and-units))
                                             :right-pole '((root))))
         )
    (setf (data transient-structure) (make-instance 'blackboard))
    (set-data (blackboard transient-structure) :ts-embeddings (first embeddings-and-units)) 
    transient-structure))

(defmethod comprehend ((utterance string) 
                       &key (syntactic-analysis nil) 
                       (cxn-inventory *fcg-constructions*)  (silent nil) (selected-rolesets nil) (timeout 60))
  (let* ((syntactic-analysis (nlp-tools:get-penelope-syntactic-analysis utterance
                                                                       :model (or (get-configuration cxn-inventory :model)
                                                                                  "en_benepar")))
         (initial-cfs (de-render utterance (get-configuration cxn-inventory :de-render-mode)
                                 :model (or (get-configuration cxn-inventory :model) "en_benepar")
                                 :cxn-inventory cxn-inventory :syntactic-analysis syntactic-analysis))
         (ts-token-embeddings (get-data (blackboard initial-cfs) :ts-embeddings)))
    (set-data (blackboard cxn-inventory) :ts-token-embeddings ts-token-embeddings)
    (unless silent (monitors:notify parse-started (listify utterance) initial-cfs))
    (multiple-value-bind (meaning cip-node cip)
        (handler-case (trivial-timeout:with-timeout (timeout)
                                                    (comprehend-with-rolesets initial-cfs cxn-inventory selected-rolesets utterance silent))
          (trivial-timeout:timeout-error (error)
            (values 'time-out 'time-out 'time-out)))
      (values meaning cip-node cip))))

;; Added function to get the string of the lemma
(defun node-lemma-string (spacy-benepar-analysis-leaf-node)
  "Returns the lemma of the leaf node"
  (cdr (assoc :lemma spacy-benepar-analysis-leaf-node)))

(def-fcg-constructions propbank-learned ;; formerly called "propbank-learned-english"
  :visualization-configurations ((:show-constructional-dependencies . nil)
                                 (:show-categorial-network . nil)
                                 (:hide-attributes . t)
                                 (:hide-features . nil))
  :hierarchy-features (constituents dependents)
  :feature-types ((constituents sequence)
                  (dependents sequence)
                  (span sequence)
                  (syn-class set)
                  (args set-of-predicates)
                  (word-order set-of-predicates)
                  (meaning set-of-predicates)
                  (footprints set)
                  (embedding default :compare-distributional-vectors))

  
  :hashed t)

;; Added function to add the embeddings from the learned cxns and the ones from the transient structures of the annotation files to the blackboard of the cxn-inventory.
(defun add-embeddings-to-cxn-inventory (cxn-inventory)
  "Retrieve all token embeddings for constructions that carry a token
attribute and store them in the cxn inventory's blackboard under the
field :cxn-token-embeddings"
  (remove-data (blackboard cxn-inventory) :cxn-token-embeddings)
  (remove-data (blackboard cxn-inventory) :ts-token-embeddings)
  (loop for cxn in (constructions cxn-inventory)
        for cxn-token = (attr-val cxn :token)
        for proto-embeddings = (find-data (blackboard cxn) :proto-role-embeddings)
        when cxn-token
          do (append-data (blackboard cxn-inventory) :cxn-token-embeddings (list (cons (intern (upcase (string-append "->" cxn-token)))
                                                                                       (second (nlp-tools:get-word-embedding cxn-token)))))
        when proto-embeddings
          do (append-data (blackboard cxn-inventory) :cxn-token-embeddings proto-embeddings))

  
  ;; this can be removed? 
  #|(loop for sentence in conll-annotations
        for ts-token-embeddings = (get-data (blackboard (initial-transient-structure sentence)) :ts-embeddings)
        when ts-token-embeddings
        do (append-data (blackboard cxn-inventory) :ts-token-embeddings ts-token-embeddings)
  )|#
  )

;; Changes: blackboard of the initial-cfs is already initialised so just do set-data on the blackboard instead of on the initial-cfs. 
(defun comprehend-with-rolesets (initial-cfs cxn-inventory selected-rolesets utterance silent)
  (let ((processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    (set-data (blackboard initial-cfs) :selected-rolesets selected-rolesets)
    (set-data (blackboard initial-cfs) :utterance utterance)
    ;; Construction application
    (multiple-value-bind (solution cip)
        (fcg-apply processing-cxn-inventory initial-cfs '<- :notify (not silent) :n 50)
      (let ((meaning (when solution
                       (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car solution)))))))
        
        ;; Notification
        (unless silent (monitors:notify parse-finished meaning processing-cxn-inventory))
        ;; Return value
        (values meaning solution cip)))))


(in-package :fcg)

;; Changes: Don't get the hashed cxns, but get all constructions. 
(defun constructions-for-application-hashed-categorial-network-neigbours (node)
  "Computes all constructions that could be applied for this node
   based on the hash table and the constructions that are linked to
the node through the links in the categorial network."
  (let* ((lex-cat-neighbours (remove-duplicates (loop for lex-category in (lex-categories node)
                                                      append (neighbouring-categories lex-category
                                                                                      (original-cxn-set (construction-inventory node))))))
         (gram-cat-neighbours (remove-duplicates (loop for gram-category in (gram-categories node)
                                                       append (neighbouring-categories gram-category
                                                                                       (original-cxn-set (construction-inventory node))))))
         (constructions
          (remove nil (loop for cxn in (constructions-list (construction-inventory node))
                            collect (cond ((attr-val cxn :gram-category)
                                           (when (member (attr-val cxn :gram-category) lex-cat-neighbours)
                                             cxn))
                                          ((attr-val cxn :sense-category)
                                           (when (member (attr-val cxn :sense-category) gram-cat-neighbours)
                                             cxn))
                                          (t
                                           cxn)))))
         (constructions (constructions-list (construction-inventory node))))
    ;; shuffle if requested
    (when (get-configuration node :shuffle-cxns-before-application)
      (setq constructions 
            (shuffle constructions)))
    ;; return constructions
    constructions))

;; New goal test that checks whether there is meaning in the transient structure. 
(defmethod cip-goal-test ((node cip-node) (mode (eql :meaning-extracted)))
  "Checks whether the resulting meaning network is fully integrated
(consists of a single connected chunk)."
  (let* ((meaning 
          (extract-meanings (left-pole-structure 
                             (car-resulting-cfs (cipn-car node))))))
    meaning))
