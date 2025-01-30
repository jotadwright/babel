(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;; Integrating distributional information into FCG grammars ;;
;;                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (ql:quickload :distributional-fcg)
;; (activate-monitor trace-fcg)

;; As long as we have not put the embedding-api webservice online, you will have to run it locally (python run.py in embedding-api repository),
;; and set your penelope-host

; (setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")

#|
1. Introduction
 
The most straightforward way to integrate distributional information into FCG grammars is to represent substantive material
in constructions by distributional vectors. The idea is that a cxn, say the substantive vehicle-cxn, does not match the
string/sequence "vehicle" from the construction with the string/sequence "vehicle" in the transient structure, but that it
matches a vector ->vehicle in the construction with a given token in the transient structure (typically "vehicle"). This way,
the vehicle-cxn could also be used to process distributionally similar tokens.

Imagine for example that we encounter the token "suv". We do not have any cxns that match specifically on the form "suv", but
we do have constructions that match on "girl" "cylinder" "vehicle" and "car". We should be able to use the embeddings in these
constructions to decide which construction to apply...

Let us first see how we can get access to static embeddings for given tokens.
|#

;; Embedding for a single token.
(nlp-tools:get-word-embedding "suv")
;; => (("suv" ->suv))

;; Embeddings for a list of tokens.
(nlp-tools:get-word-embeddings '("girl" "cylinder" "vehicle" "car" "hardly"))
;; => (("girl" ->girl) ("cylinder" ->cylinder) ("vehicle" ->vehicle) ("car" ->car) ("hardly" ->hardly))

;; Careful, we will not find embeddings for tokens that don't appear in glove, so tokenize and lowercase.
(nlp-tools:get-word-embeddings (mapcar #'downcase (cl-ppcre:split "[ \.-]" "The man drives an suv.")))
;; => (("the" ->the) ("man" ->man) ("drives" ->drives) ("an" ->an) ("suv" ->suv))

#|
Similarities between tokens can quantified as the cosine between their embedding vectors
|#

(cosine-similarity (second (fifth (nlp-tools:get-word-embeddings '("the" "man" "drives" "a" "car" "."))))
                   (second (fifth (nlp-tools:get-word-embeddings '("he" "will" "by" "that" "car" ".")))))
;; => 1

;; The cosine between two words...
(cosine-similarity (second (nlp-tools:get-word-embedding "man"))
                   (second (nlp-tools:get-word-embedding "cat")))
;;=> 0.5261842

;; And there's a shortcut for that...
(nlp-tools:get-word-similarity "man" "cat")
;; => 0.5261842

#|
For convenience, we can define a function that given a token computes the distributionally closest token from a list of tokens.
|#

(defun closest-token (target-token list-of-tokens)
  "Returns the token in list-of-tokens that is distributionally most similar to target-token."
  (loop with closest-token-so-far-with-score = (cons nil -inf)
        with target-token-embedding =  (second (nlp-tools::get-word-embedding target-token))
        for other-token in list-of-tokens
        for other-token-embedding = (nlp-tools::get-word-embedding other-token)
        for cosine-similarity = (cosine-similarity target-token-embedding (second other-token-embedding))
        when (> cosine-similarity (cdr closest-token-so-far-with-score))
          do (setf closest-token-so-far-with-score (cons other-token cosine-similarity))
        finally (return (values (car closest-token-so-far-with-score) (cdr closest-token-so-far-with-score)))))

(closest-token "suv" '("girl" "cylinder" "vehicle" "car" "hardly" "he" "drives" "an" "." "man"))
;; => "vehicle", 0.73386384

#|
2. Operationalisation in FCG

What do we need to configure in FCG?
- Pretokenisation of the input utterance + quering and storing of pretrained embeddings.
  => FCG solution: de-rendering into tokens and retrieving embeddings (using NLP-tools).
     The transient structure holds pointers to these embeddings, the embeddings themselves
     are stored in the :ts-token-embeddings field of the blackboard of the cxn-inventory.
- Storing embedding vectors in cxns.
  => FCG solution: when creating a construction, we do not only store strings
     but also precomputed embeddings of the corresponding tokens. For visualisation
     purposes, we store symbols in the cxn pointing to the embedding vectors in the blackbboard of the cxn-inventory.
     The field used is :cxn-token-embeddings.
- Matching based on distributional similarity instead of symbol equivalence.
  => FCG solution: declaring form as a feature type with procedural attachment (:compare-embeddings),
     matching the vector with the embedding of the tokens, letting matching succeed above a certain threshold,
     and integrating the distributional similarity into the heuristic value of the cip-node that is created.
|#

#|
Let us start by creating the initial transient structure. As we pretokenize, we can as well create units for
each token that hold the string and a pointer to its (precomputed) embedding.
|#

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


#|
Let us now visualise the initial transient structure (in comprehension) for the utterance
"the man drives an suv" using our fresh de-render method.
|#

(progn
  (add-element (make-html-fcg-light (de-render "the man drives an suv ."
                                               :de-render-token-embeddings
                                               :cxn-inventory *fcg-constructions*)
                                    :feature-types (feature-types *fcg-constructions*)
                                    :construction-inventory *fcg-constructions*))

  (add-element (make-html (get-data (blackboard *fcg-constructions*) :ts-token-embeddings)))
  )

#|
Let us create a small grammar fragment.
|#

(def-fcg-constructions distributional-fcg-grammar-ex-1
  :cxn-inventory *distributional-fcg-grammar-ex-1*
  :feature-types ((form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (args sequence)
                  (footprints set)
                  (boundaries default)
                  (embedding default :compare-distributional-vectors))
  :visualization-configurations ((:show-constructional-dependencies . nil))
  :fcg-configurations (;; --- (DE)RENDER ---
                       (:de-render-mode . :de-render-token-embeddings)
                       (:max-nr-of-nodes . 5000)

                       ;; --- HEURISTICS ---
                       ;; use dedicated cip
                       (:construction-inventory-processor-mode . :heuristic-search)
                       ;; always fully expands node immediately
                       (:node-expansion-mode . :full-expansion)
                       ;; returns all cxns at once
                       (:cxn-supplier-mode . :all-cxns)
                       ;; for using heuristics (alternatives: :depth-first, :breadth-first :random)
                       (:search-algorithm . :best-first)
                       ;; list of heuristic functions (modes of #'apply-heuristic) - only used with best-first search
                       (:heuristics :nr-of-applied-cxns :embedding-similarity)
                       ;; how to use results of heuristic functions for scoring a node
                       (:heuristic-value-mode . :sum-heuristics-and-parent))

  (def-fcg-cxn man-cxn
               ((?man-unit
                 (category noun)
                 (args (?m))
                 (footprints (token-matched)))
                <-
                (?man-unit
                 (HASH meaning ((man ?m)))
                 --
                 (footprints (NOT token-matched))
                 (token (embedding ->man))))
               :attributes (:token "man"))

  (def-fcg-cxn car-cxn
               ((?car-unit
                 (category noun)
                 (args (?c))
                 (footprints (token-matched)))
                <-
                (?car-unit
                 (HASH meaning ((car ?c)))
                 --
                 (footprints (NOT token-matched))
                 (token (embedding ->car))))
               :attributes (:token "car"))

  (def-fcg-cxn vehicle-cxn
               ((?vehicle-unit
                 (category noun)
                 (args (?v))
                 (footprints (token-matched)))
                <-
                (?vehicle-unit
                 (HASH meaning ((vehicle ?v)))
                 --
                 (footprints (NOT token-matched))
                 (token (embedding ->vehicle))))
               :attributes (:token "vehicle"))

  (def-fcg-cxn the-cxn
               ((?the-unit
                 (category determiner)
                 (args (?v))
                 (footprints (token-matched)))
                <-
                (?the-unit
                 (HASH meaning ((referent-status ?v accessible)))
                 --
                 (footprints (NOT token-matched))
                 (token (embedding ->the))))
               :attributes (:token "the"))

  (def-fcg-cxn a-cxn
               ((?a-unit
                 (category determiner)
                 (args (?v))
                 (footprints (token-matched)))
                <-
                (?a-unit
                 (HASH meaning ((referent-status ?v introducing)))
                 --
                 (footprints (NOT token-matched))
                 (token (embedding ->a))))
               :attributes (:token "a"))

  (def-fcg-cxn drives-cxn
               ((?drives-unit
                 (category verb)
                 (args (?d))
                 (footprints (token-matched)))
                <-
                (?drives-unit
                 (HASH meaning ((drive.01 ?d)))
                 --
                 (footprints (NOT token-matched))
                 (token (embedding ->drives))))
               :attributes (:token "drives"))

  (def-fcg-cxn np-cxn
               ((?np-unit
                 (category noun-phrase)
                 (args (?ref))
                 (subunits (?determiner-unit ?noun-unit))
                 (boundaries (left ?determiner-unit)
                             (right ?noun-unit)))
                <-
                (?determiner-unit
                 (category determiner)
                 (args (?ref))
                 --
                 (category determiner))
                (?noun-unit
                 (category noun)
                 (args (?ref))
                 --
                 (category noun))
                (?np-unit
                 --
                 (HASH form ((adjacent ?determiner-unit ?noun-unit))))))

  (def-fcg-cxn transitive-cxn
               ((?transitive-clause-unit
                 (category noun-phrase)
                 (args (?ref))
                 (subunits (?subject-unit ?verb-unit ?object-unit)))
                <-
                (?subject-unit
                 (category noun-phrase)
                 (args (?agent))
                 --
                 (category noun-phrase)
                 (boundaries (left ?subject-left)
                             (right ?subject-right)))
                (?verb-unit
                 (category verb)
                 (args (?event))
                 --
                 (category verb))
                (?object-unit
                 (category noun-phrase)
                 (args (?patient))
                 --
                 (category noun-phrase)
                 (boundaries (left ?object-left)
                             (right ?object-right)))
                (?transitive-clause-unit
                 (HASH meaning ((:arg0 ?event ?agent) (:arg1 ?event ?patient)))
                 --
                 (HASH form ((adjacent ?subject-right ?verb)
                             (adjacent ?verb ?object-left))))))

  )


#|
We retrieve the word embeddings of the tokens in de cxns and store them in the blackboard of the cxn-inventory (:cxn-token-embeddings)
|#

(defun add-grammar-token-embeddings (cxn-inventory)
  "Retrieve all token embeddings for constructions that carry a token
attribute and store them in the cxn inventory's blackboard under the
field :cxn-token-embeddings"
  (remove-data (blackboard cxn-inventory) :cxn-token-embeddings)
  (loop for cxn in (constructions cxn-inventory)
        for cxn-token = (attr-val cxn :token)
        when cxn-token
          do (append-data (blackboard cxn-inventory) :cxn-token-embeddings (list (cons (intern (upcase (string-append "->" cxn-token)))
                                                                                       (second  (nlp-tools:get-word-embedding cxn-token)))))))

(add-grammar-token-embeddings *distributional-fcg-grammar-ex-1*)
(get-data (blackboard *distributional-fcg-grammar-ex-1*) :cxn-token-embeddings)
(add-element (make-html (get-data (blackboard *distributional-fcg-grammar-ex-1*) :cxn-token-embeddings)))
  

#|
We hook into fcg-expand to change the unification function of symbols appearing as values of the 'embedding' feature.
Above a given threshold, unification succeeds. The value from the transient structure is kept and the cosine similarity
is stored in the bindings list so it can later be reused...
|#

(defmethod fcg-expand ((type (eql :compare-distributional-vectors))
                       &key value source bindings merge? cxn-inventory)
  "Use cosine similarity metric to match via token embeddings."
  (if merge?
    ;; in the merging phase, we keep the original embedding pointer
    (values source bindings)
    ;; in the matching phase...
    (cond (;; if both embedding pointers are eq, we just continue like in unification 
           (eq value source) 
           (values value bindings))
          (;; if they are not eq, but non-nil, we compute their cosine and return true above a given threshold
           ;; we also add a binding between both pointers (variablified versions to make merge succeed)
           (and value source)
           (let* ((token-embedding-cxn (cdr (assoc value (get-data (blackboard cxn-inventory) :cxn-token-embeddings))))
                  (token-embedding-ts (cdr (assoc source (get-data (blackboard cxn-inventory) :ts-token-embeddings))))
                  (cosine-similarity (cosine-similarity token-embedding-cxn token-embedding-ts)))
             (if (> cosine-similarity 0.7)
               (values source (mapcar #'(lambda (bindings-list)
                                          (extend-bindings (variablify value) cosine-similarity bindings-list))
                                      bindings))
               (values nil +fail+))))
          (t
           (values nil +fail+)))))

#|
The cosine similarity is integrated in a new heuristic. With straight unification, it returns 0. If cosine-based
unification was needed, it returns a negative value proportional to the dissimilarity. (e.g. cosine = 0.8 -> -0.2)
|#

(defmethod apply-heuristic ((node cip-node) (mode (eql :embedding-similarity)))
  "Discounts embedding bindings based on cosine."
  (let* ((cxn-inventory (construction-inventory node))
         (embedding-bindings (loop for binding in (car-second-merge-bindings (cipn-car node))
                                   for symbol-name = (symbol-name (car binding))
                                   when (and (>= (length symbol-name) 3)
                                             (equal "?->" (subseq symbol-name 0 3)))
                                     collect binding)))
    (if embedding-bindings
      (loop for embedding-binding in embedding-bindings
            for cosine-similarity = (cdr embedding-binding)
            sum cosine-similarity into total-similarity
            finally (return (- (- 1 (/ total-similarity (length embedding-bindings))))))
      0)))


#|
3. Testing !
|#

;; sentence covered by the grammar
(comprehend "the man drives a vehicle" :cxn-inventory *distributional-fcg-grammar-ex-1*)


;; sentence not covered by the grammar (no cxns for 'an' and 'suv')
(comprehend "the man drives an suv." :cxn-inventory *distributional-fcg-grammar-ex-1*)

;; Importantly: word embeddings are used as FORM representations, NOT meaning
;; constructions as mappings between distributional representations and meaning representations



