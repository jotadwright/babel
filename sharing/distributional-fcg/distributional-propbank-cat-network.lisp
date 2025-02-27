(ql:quickload :propbank-grammar)
(ql:quickload :distributional-fcg)

(in-package :propbank-grammar)


;(activate-monitor trace-fcg)
;(deactivate-all-monitors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set host for nlp-tools: embedding-api and spacy-api ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")
(setf nlp-tools::*embedding-host* "http://127.0.0.1:5001")
; 
;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting file paths ;;
;;;;;;;;;;;;;;;;;;;;;;;;



(defparameter *annotations-file-path*
  (merge-pathnames 
   (make-pathname :directory '(:relative "distributional-fcg")
                  :name "slot-filler" :type "conll")
   cl-user:*babel-corpora*))


(defparameter *annotations* (read-propbank-conll-file *annotations-file-path*))

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
       :graph-cosine-similarity
       ;:embedding-similarity
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

;;;;;;;;;;;;;;;;;;;;;;
;; Draw the cat net ;;
;;;;;;;;;;;;;;;;;;;;;;
;;(add-element (make-html (categorial-network *train-grammar*) :weights t))

;;(add-element (make-html *train-grammar*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comprehending to test  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (graph-utils::pre-compute-cosine-similarities (fcg::graph (categorial-network *train-grammar*)))
  (set-configuration *train-grammar* :category-linking-mode :always-succeed)
  (set-configuration *train-grammar*  :node-expansion-mode  :multiple-cxns)
  (set-configuration *train-grammar* :cxn-supplier-mode :cascading-cosine-similarity))


#|

(comprehend "he sold his mother the car" :timeout nil)

(comprehend "he sold the car to his mother" :timeout nil)

;;;; random voorbeelden

(comprehend "the man smurfs a book to his wife")

(activate-monitor trace-fcg)

(set-configuration *train-grammar* :category-linking-mode :graph-cosine-similarity)

(comprehend "the man sent a book to his wife" :timeout nil)


(comprehend "he sold the car to his mother" :timeout nil)


(comprehend "he read his child a story" :timeout nil)


|#

         




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                  (footprints set))
  :hashed t
  :categorial-network `,(make-instance 'categorial-network :graph (graph-utils::make-undirected-node-and-edge-typed-graph)))

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
        (if (equalp syn-class '(vp))
          (let ((lex-lemma (intern (subseq (symbol-name lemma) 0 (search "-" (symbol-name lemma))))))
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
                             
                             (lemma ,lex-lemma)
                             (parent ?phrasal-unit)))
                          
                           :attributes (:lemma ,lemma
                                        :lex-category ,lex-category
                                        :label lexical-cxn
                                        :score 1)
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
                             
                             (lemma ,lemma)
                             (syn-class ,syn-class)))
                           :attributes (:lemma ,lemma
                                        :lex-category ,lex-category
                                        :label lexical-cxn
                                        :score 1)
                           :description ,(sentence-string propbank-sentence)
                           :disable-automatic-footprints t
                           :cxn-inventory ,cxn-inventory)))
        (add-category lex-category cxn-inventory :recompute-transitive-closure nil :node-type 'lex-category)
        lex-category))))



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
        ;;3) Return gram-category
        (attr-val equivalent-cxn :gram-category))

      ;; Else: Create a new grammatical category for the observed pattern + add category and link to the categorial network
      ;;--------------------------------------------------------------------------------------------------------------------
      (when (and cxn-units-with-role (v-lemma core-units-with-role))
        
        (add-category gram-category cxn-inventory :recompute-transitive-closure nil :node-type 'gram-category)
        (add-link lex-category gram-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link lex-category gram-category cxn-inventory :weight 1.0 :link-type 'lex-gram :recompute-transitive-closure nil)
        
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
        gram-category))))


(defun add-word-sense-cxn (gold-frame v-unit cxn-inventory propbank-sentence lex-category gram-category)
  "Creates a new word sense construction if necessary, otherwise
increments frequency of existing cxn. Adds a new sense category to the
categorial network and returns it."
  (let* ((lemma (or (feature-value (find 'lemma (unit-body v-unit) :key #'feature-name))
                    (feature-value (find 'string (unit-body v-unit) :key #'feature-name))))
         (cxn-name (intern (upcase (format nil "~a(~a)-cxn" (frame-name gold-frame) lemma))))
         
         (equivalent-cxn (find-cxn cxn-name cxn-inventory :hash-key (if (stringp lemma) ;;WERKT NIET MEER!
                                                                      (intern (upcase lemma))
                                                                      lemma) :key #'name))
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
                         ,@(if (stringp lemma)
                             `((string ,lemma))
                             `((lemma ,lemma)))
                         (gram-category ,sense-category)
                         (lex-category ,sense-category)
                         (frame ,(intern (upcase (frame-name gold-frame))))
                         (footprints (NOT ws))))
                       :disable-automatic-footprints t
                       :attributes (:lemma ,(if (stringp lemma)
                                              (intern (upcase lemma))
                                              lemma)
                                    :sense-category ,sense-category
                                    :label word-sense-cxn
                                    :score 1)
                       :description ,(sentence-string propbank-sentence)
                       :cxn-inventory ,cxn-inventory))
        
        (add-category sense-category cxn-inventory :recompute-transitive-closure nil :node-type 'sense-category)
        (add-link gram-category sense-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link gram-category sense-category cxn-inventory :weight 1.0 :link-type 'gram-sense :recompute-transitive-closure nil)
        (add-link lex-category sense-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link lex-category sense-category cxn-inventory :weight 1.0 :link-type 'lex-sense :recompute-transitive-closure nil)
        
        sense-category))))


