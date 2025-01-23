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

(closest-token "suv" '("girl" "cylinder" "vehicle" "car" "hardly"))
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
  (let ((token-embeddings (nlp-tools::get-word-embeddings utterance)))
    (


  
  (de-render (split-sequence:split-sequence #\Space utterance :remove-empty-subseqs t)
             :de-render-string-meets))

(defmethod de-render ((utterance list) (mode (eql :de-render-string-meets)) &key &allow-other-keys)
  "De-renders a list of strings into string and meets."
  (let ((strings nil)
	(constraints nil))
    (dolist (string utterance)
      (let ((new (make-const string nil)))
	(push `(string ,new ,string) strings)))
    (do ((left strings (rest left)))
	((null (cdr left)))
      (push `(meets ,(second (second left)) ,(second (first left)))
	    constraints))
    (make-instance 'coupled-feature-structure 
		   :left-pole `((root (meaning ())
                                      (sem-cat ())
                                      (form ,(append (reverse strings) constraints))
                                      (syn-cat ())))
		   :right-pole '((root)))))


;; Importantly: word embeddings are used as FORM representations, NOT meaning
;; constructions as mappings between distributional representations and meaning representations


We could use the embeddings of

, such as "sphere" or


(nlp-tools::get-word-embeddings "ball")
(nlp-tools:get-penelope-word-embeddings "sphere cylinder car")

(cosine-similarity (second (nlp-tools::get-word-embeddings "ball")))


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


(closest-token "suv" '("girl" "cylinder" "vehicle" "car"))

(closest-token "almost" '("hardly"))
(closest-token "liquorice" '("girl" "candy" "gear" "child" "bus" "boy"))
          
          other-token-embedding in (mapcar #'(lambda (token)
                                                 (second (first (nlp-tools::get-word-embeddings token))))
                                             list-of-tokens)

  )                   


  (first (nlp-tools::get-word-embeddings "sphere cylinder car"))

