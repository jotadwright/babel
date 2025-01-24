(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;; Integrating distributional information into FCG grammars ;;
;;                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (ql:quickload :distributional-fcg)

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

(closest-token "suv" '("girl" "cylinder" "vehicle" "car" "hardly" "he" "drives" "an"))
;; => "vehicle", 0.8198127

#|
2. Operationalisation in FCG

What do we need to configure in FCG?
- Pretokenisation of the input utterance.
  => FCG solution: de-rendering into tokens (using NLP-tools or by space)
- Storing embedding vectors in cxns.
  => FCG solution: when creating a construction, we do not only store strings
     but also precomputed embeddings of the corresponding tokens. For visualisation
     purposes, we store symbols pointing to the embedding vectors in the cxns and
     the embedding vectors themselves in the :attributes of the cxns.
- Matching based on distributional similarity instead of symbol equivalence.
  => FCG solution: declaring form as a feature type with procedural attachment (:compare-embeddings),
     matching the vector with the embedding of the tokens, letting matching succeed above a certain threshold,
     and integrating the distributional similarity into the heuristic value of the cip-node that is created.
|#

#|
Let us start by creating the initial transient structure. As we pretokenize, we can as well create units for
each token that hold the string and a pointer to its (precomputed) embedding.
|#

(defmethod de-render ((utterance string) (mode (eql :de-render-token-embeddings)) &key &allow-other-keys)
  "Retrieves tokens and embeddings for string, creating one unit per token."
  (let* ((embedding-data nil) (adjacency-constraints nil)
         (token-embeddings (nlp-tools::get-word-embeddings utterance))
         (token-units (loop for (token embedding) in token-embeddings
                            for unit-name = (make-id token)
                            for embedding-pointer = (make-id (string-append token "->"))
                            do (push (cons embedding-pointer embedding) embedding-data)
                            collect (list unit-name
                                          `(token ,token)
                                          `(form ((embedding ,unit-name ,embedding-pointer)))))))

    
    (do ((left (mapcar #'first token-units) (rest left)))
	((null (cdr left)))
      (push `(adjacent ,(first left) ,(second left))
	    adjacency-constraints))

    (make-instance 'coupled-feature-structure
                   :data `((:token-embeddings ,@embedding-data))
		   :left-pole `((root (meaning ())
                                      (sem-cat ())
                                      (form ,(reverse adjacency-constraints))
                                      (syn-cat ()))
                                ,@token-units)
		   :right-pole '((root)))))
    
(setf *initial-cfs* (de-render "car suv vehicle" :de-render-token-embeddings))
(inspect *initial-cfs*)

(cdr (assoc (caar (get-data *initial-cfs* :token-embeddings)) (get-data *initial-cfs* :token-embeddings)))





;; Importantly: word embeddings are used as FORM representations, NOT meaning
;; constructions as mappings between distributional representations and meaning representations



