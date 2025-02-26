(ql:quickload :propbank-grammar)
(ql:quickload :distributional-fcg)

(in-package :propbank-grammar)


;(activate-monitor trace-fcg)
;(deactivate-all-monitors)

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


;(comprehend "the man rides a bike" :timeout nil)

;(comprehend "the man gave his mother a book" :timeout nil)

;(comprehend "the husband handed his wife a book" :timeout nil)

;(comprehend "he abandoned his child" :timeout nil)
;(comprehend "he committed a crime" :timeout nil)
;(comprehend "the bank committed 1,2 million dollar" :timeout nil)

#|

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
    

;(cosine-similarity (get-proto-embedding (list "the man" "the woman")) (last-elt (nlp-tools:get-word-embedding "bottle")))

;(cosine-similarity (get-proto-embedding (list "the man" "the woman")) (get-proto-embedding (list "the child" "the dog")))



