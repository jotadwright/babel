(in-package :fcg)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                             ;;
;; Utility functions for the distributional-fcg system         ;;
;;                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun closest-token (target-token list-of-tokens)
  "Returns the token in list-of-tokens that is distributionally most similar to target-token."
  (loop with closest-token-so-far-with-score = (cons nil -inf)
        with target-token-embedding = (second (nlp-tools::get-word-embedding target-token))
        for other-token-embedding in (nlp-tools::get-word-embeddings list-of-tokens)
        for cosine-similarity = (cosine-similarity target-token-embedding (second other-token-embedding))
        when (> cosine-similarity (cdr closest-token-so-far-with-score))
          do (setf closest-token-so-far-with-score (cons (first other-token-embedding) cosine-similarity))
        finally (return (values (car closest-token-so-far-with-score) (cdr closest-token-so-far-with-score)))))

(defun tokens-sorted-by-similarity (target-token list-of-tokens)
  "Returns the list-of-tokens sorted by the similarity with the target-token."
  (loop with target-token-embedding = (second (nlp-tools::get-word-embedding target-token))
        for other-token-embedding in (nlp-tools::get-word-embeddings list-of-tokens)
        for cosine-similarity = (cosine-similarity target-token-embedding (second other-token-embedding))
        collect (cons (first other-token-embedding) cosine-similarity) into similarity-list
        finally (return (sort similarity-list #'> :key #'cdr))))

;;(closest-token "wire" '("send" "give" "pay"))
