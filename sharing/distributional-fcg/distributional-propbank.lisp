(ql:quickload :propbank-grammar)
(ql:quickload :distributional-fcg)

(in-package :propbank-grammar)

(defparameter *annotations-file-path*
  (merge-pathnames 
   (make-pathname :directory '(:relative "distributional-fcg")
                  :name "small-corpus" :type "conll")
   cl-user:*babel-corpora*))
            
(defparameter *annotations*
  (read-propbank-conll-file *annotations-file-path*))


(defparameter *training-configuration*
  '((:de-render-mode .  :de-render-constituents-dependents)
    (:node-tests :check-double-role-assignment)
    (:parse-goal-tests :no-valid-children)
    (:construction-inventory-processor-mode . :heuristic-search)
    (:search-algorithm . :best-first)   
    (:heuristics
     :nr-of-applied-cxns
     :nr-of-units-matched-x2 ;;nr-of-units-matched
     :edge-weight)
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

(learn-propbank-grammar
 (shuffle *annotations*)
 :selected-rolesets nil
 :excluded-rolesets nil
  :cxn-inventory '*train-grammar-2*
  :fcg-configuration *training-configuration*)

 (activate-monitor trace-fcg)

 

(add-embeddings-to-cxn-inventory *train-grammar-2* *annotations*)

(comprehend "A man reads")

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; quickly overwrite to test. ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; add the embedding to the attributes (token)
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
                                (footprints (lex)))
                               <-
                               (?phrasal-unit
                                --
                                (footprints (NOT lex))
                                (lemma ,lemma)
                                (syn-class ,syn-class))
                               (?lex-unit
                                --
                                (footprints (NOT lex))
                                
                                
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
                                (footprints (lex))
                             (lex-category ,lex-category))
                               <-
                               (?lex-unit
                                --
                                (footprints (NOT lex))
                                
                                
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


;; Here we need to add the embeddings to the TS. 
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
                                      when lemma-embedding
                                      collect  (cons lemma-embedding-pointer (first (cdr lemma-embedding))) into embeddings
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



(defmethod de-render ((utterance string) (mode (eql :de-render-token-embeddings)) &key cxn-inventory &allow-other-keys)
  "Retrieves tokens and embeddings for string, creating one unit per token."
  (multiple-value-bind (units embedding-data)
      (loop with token-embeddings = (nlp-tools:get-word-embeddings (mapcar #'downcase (cl-ppcre:split "[ .-]"  utterance)))
            for (token embedding) in token-embeddings
            for unit-name = (make-id token)
            for embedding-pointer = (intern (upcase (string-append "->" token)))
            collect (cons embedding-pointer embedding)
              into embedding-data
            collect (make-unit :name unit-name
                               :features `((token ((string ,token)
                                                   (embedding ,embedding-pointer)))))
              into units
            finally (return (values units embedding-data)))
    ;; adjecency-constraints
    (let ((adjacency-constraints (loop for (unit-name . rest) on (mapcar #'first units)
                                       when rest
                                         collect `(adjacent ,unit-name ,(first rest)))))
      (set-data (blackboard cxn-inventory) :ts-token-embeddings embedding-data)
      (make-instance 'coupled-feature-structure
                     :left-pole `((root (meaning ())
                                        (sem-cat ())
                                        (form ,adjacency-constraints)
                                        (syn-cat ()))
                                  ,@units)
                     :right-pole '((root))))))

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


(defun add-embeddings-to-cxn-inventory (cxn-inventory conll-annotations)
  "Retrieve all token embeddings for constructions that carry a token
attribute and store them in the cxn inventory's blackboard under the
field :cxn-token-embeddings"
  (remove-data (blackboard cxn-inventory) :cxn-token-embeddings)
  (remove-data (blackboard cxn-inventory) :ts-token-embeddings)
  (loop for cxn in (constructions cxn-inventory)
        for cxn-token = (attr-val cxn :token)
        when cxn-token
          do (append-data (blackboard cxn-inventory) :cxn-token-embeddings (list (cons (intern (upcase (string-append "->" cxn-token)))
                                                                                       (second (nlp-tools:get-word-embedding cxn-token))))))
  (loop for sentence in conll-annotations
        for ts-token-embeddings = (get-data (blackboard (initial-transient-structure sentence)) :ts-embeddings)
        when ts-token-embeddings
        do (append-data (blackboard cxn-inventory) :ts-token-embeddings ts-token-embeddings)
  ))


(defun comprehend-with-rolesets (initial-cfs cxn-inventory selected-rolesets utterance silent)
  (let ((processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    (set-data (blackboard initial-cfs) :selected-rolesets selected-rolesets)
    (set-data (blackboard initial-cfs) :utterance utterance)
    ;; Construction application
    (multiple-value-bind (solution cip)
        (fcg-apply processing-cxn-inventory initial-cfs '<- :notify (not silent))
      (let ((meaning (when solution
                       (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car solution)))))))
        
        ;; Notification
        (unless silent (monitors:notify parse-finished meaning processing-cxn-inventory))
        ;; Return value
        (values meaning solution cip)))))
