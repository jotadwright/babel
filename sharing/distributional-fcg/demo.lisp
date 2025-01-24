(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;; Integrating distributional information into FCG grammars ;;
;;                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (ql:quickload :distributional-fcg)
;; (activate-monitor trace-fcg)

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

Let us first see how we can get access to embeddings for given tokens.
|#

(nlp-tools::get-word-embeddings "suv")
;; => (("suv" ->suv))

(nlp-tools::get-word-embeddings "girl cylinder vehicle car hardly")
;; => (("girl" ->girl) ("cylinder" ->cylinder) ("vehicle" ->vehicle) ("car" ->car) ("hardly" ->hardly))

#|
Similarities between tokens can quantified as the cosine between their embedding vectors
|#

(cosine-similarity (second (first (nlp-tools::get-word-embeddings "suv")))
                   (second (first (nlp-tools::get-word-embeddings "vehicle"))))
;; => 0.8198127

(cosine-similarity (second (first (nlp-tools::get-word-embeddings "suv")))
                   (second (first (nlp-tools::get-word-embeddings "hardly"))))
;;=> 0.1414988

#|
For convenience, we can define a function that given a token computes the distributionally closest token from a list of tokens.
|#

(defun closest-token (target-token list-of-tokens)
  "Returns the token in list-of-tokens that is distributionally most similar to target-token."
  (loop with closest-token-so-far-with-score = (cons nil -inf)
        with target-token-embedding =  (second (first (nlp-tools::get-word-embeddings target-token)))
        for other-token in list-of-tokens
        for other-token-embedding = (nlp-tools::get-word-embeddings other-token)
        for cosine-similarity = (cosine-similarity target-token-embedding (second (first other-token-embedding)))
        when (> cosine-similarity (cdr closest-token-so-far-with-score))
          do (setf closest-token-so-far-with-score (cons other-token cosine-similarity))
        finally (return (values (car closest-token-so-far-with-score) (cdr closest-token-so-far-with-score)))))

(closest-token "suv" '("girl" "cylinder" "vehicle" "car" "hardly" "he" "drives" "an" "."))
;; => "vehicle", 0.8198127

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
     purposes, we store symbols in the cxn pointing to the embedding vectors in th the blackbboard of the cxn-inventory.
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
      (loop with token-embeddings = (nlp-tools::get-word-embeddings utterance)
            for (token embedding) in token-embeddings
            for unit-name = (make-id token)
            for embedding-pointer = (intern (upcase (string-append "->" token)))
            collect (cons embedding-pointer embedding)
              into embedding-data
            collect (make-unit :name unit-name
                               :features `((token ,token)
                                           (embedding ,embedding-pointer)))
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

(add-element (make-html-fcg-light (de-render "the man drives an suv."
                                             :de-render-token-embeddings
                                             :cxn-inventory *fcg-constructions*)
                                  :feature-types (feature-types *fcg-constructions*)
                                  :construction-inventory *fcg-constructions*))

#|
Let us create a small grammar fragment.
|#

(def-fcg-constructions distributional-fcg-grammar-ex-1 
                       :feature-types ((form set-of-predicates)
                                       (meaning set-of-predicates)
                                       (subunits set)
                                       (args sequence)
                                       (footprints set)
                                       (boundaries default))
                       :fcg-configurations (;; --- (DE)RENDER ---
                                            (:de-render-mode . :de-render-sequence-predicates)
                                            (:render-mode . :render-sequences)

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
                                            (:heuristics :nr-of-applied-cxns :nr-of-units-matched)
                                            ;; how to use results of heuristic functions for scoring a node
                                            (:heuristic-value-mode . :sum-heuristics-and-parent) 

                                            ;; --- GOAL TESTS ---
                                            ;; goal tests for comprehension
                                            (:parse-goal-tests
                                             :no-applicable-cxns ;; succeeds if node is fully expanded and no cxns could apply to its children
                                             :no-strings-in-root ;; succeeds if no string features remain in the root
                                             :connected-semantic-network ;; succeeds if the semantic network is fully connected
                                             :connected-structure ;; succeeds if all units are connected
                                             )
                                            ;; goal tests for formulation
                                            (:production-goal-tests
                                             :no-applicable-cxns ;; succeeds if node is fully expanded and no cxns could apply to its children
                                             :no-meaning-in-root ;; succeeds if no meaning predicates remain in root
                                             )))
 



;; Importantly: word embeddings are used as FORM representations, NOT meaning
;; constructions as mappings between distributional representations and meaning representations



