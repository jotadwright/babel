(ql:quickload :propbank-grammar)
(ql:quickload :distributional-fcg)

(in-package :propbank-grammar)


;;;;;;;;;;;;;;;;;;;;;;;
;; Activate monitors ;;
;;;;;;;;;;;;;;;;;;;;;;;

;(activate-monitor trace-fcg)
;(deactivate-all-monitors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set host for nlp-tools: embedding-api and spacy-api ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")
;(setf nlp-tools::*embedding-host* "http://127.0.0.1:5001")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; This demo shows the integration of distributions in FCG using the PropBank grammar.


;; First, we add glove embeddings to the lexical constructions.



;; This file contains 2 sentences: "He gave a book to his mother." and "He gave his mother a book."

(defparameter *distributional-representations-of-tokens-and-types-file-path*
  (merge-pathnames 
   (make-pathname :directory '(:relative "distributional-fcg")
                  :name "tokens-and-types-sent-wire" :type "conll")
   cl-user:*babel-corpora*))


(defparameter *distributional-representations-of-tokens-and-types-annotations*
  (read-propbank-conll-file *distributional-representations-of-tokens-and-types-file-path*))


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

(learn-propbank-grammar
 (shuffle *distributional-representations-of-tokens-and-types-annotations*)
 :selected-rolesets nil
 :excluded-rolesets nil
 :cxn-inventory '*distributional-representations-of-tokens-and-types-grammar*
 :fcg-configuration *training-configuration*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make the embeddings (for roles and lexical items) ;;
;;   and add the embeddings to the cxn-inventory     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Needs to be set otherwise you will not be able to match since we learn cxns with embeddings.
(make-proto-role-embeddings *distributional-representations-of-tokens-and-types-grammar*)
(add-embeddings-to-cxn-inventory *distributional-representations-of-tokens-and-types-grammar*)

(progn
  (graph-utils::pre-compute-cosine-similarities (fcg::graph (categorial-network *distributional-representations-of-tokens-and-types-grammar*)))
  (set-configuration *distributional-representations-of-tokens-and-types-grammar* :cosine-similarity-threshold 0.3)
  (set-configuration *distributional-representations-of-tokens-and-types-grammar* :category-linking-mode :always-succeed)
  (set-configuration *distributional-representations-of-tokens-and-types-grammar* :node-expansion-mode  :multiple-cxns)
  (set-configuration *distributional-representations-of-tokens-and-types-grammar* :cxn-supplier-mode :cascading-cosine-similarity))

;;;;;;;;;;;;;;;;;;;;;;
;; Draw the cat net ;;
;;;;;;;;;;;;;;;;;;;;;;
;;(add-element (make-html (categorial-network *distributional-representations-of-tokens-and-types-grammar*) :weights t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     Comprehend!!!     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;corpus:
; (comprehend "I will send you the money tomorrow" :cxn-inventory *distributional-representations-of-tokens-and-types-grammar*)
(comprehend "I have sent him a postcard" :cxn-inventory *distributional-representations-of-tokens-and-types-grammar* :timeout nil)

;; example! 
(comprehend "I will wire you the money" :cxn-inventory *distributional-representations-of-tokens-and-types-grammar*)


(loop for cxn in (constructions-list *distributional-representations-of-tokens-and-types-grammar*)
      when (attr-val cxn :lex-category)
        collect (attr-val cxn :lemma) into lemmas
       and  collect cxn into cxns
      finally (return (values lemmas cxns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *distributional-representations-of-constructional-slots-file-path*
  (merge-pathnames 
   (make-pathname :directory '(:relative "distributional-fcg")
                  :name "constructional-slots" :type "conll")
   cl-user:*babel-corpora*))


(defparameter *distributional-representations-of-constructional-slots-annotations*
  (read-propbank-conll-file *distributional-representations-of-constructional-slots-file-path*))


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

(learn-propbank-grammar
 (shuffle *distributional-representations-of-constructional-slots-annotations*)
 :selected-rolesets nil
 :excluded-rolesets nil
 :cxn-inventory '*distributional-representations-of-constructional-slots-grammar*
 :fcg-configuration *training-configuration*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make the embeddings (for roles and lexical items) ;;
;;   and add the embeddings to the cxn-inventory     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Needs to be set otherwise you will not be able to match since we learn cxns with embeddings.
(make-proto-role-embeddings *distributional-representations-of-constructional-slots-grammar*)
(add-embeddings-to-cxn-inventory *distributional-representations-of-constructional-slots-grammar*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     Comprehend!!!     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; There are some ambiguous sentences in the corpus where the np that occurs after the verb (teach) can either be an arg1 or an arg2.
;; Only looking at constituency structure will not give you the best solution.
;; We can use the learner prototypical slot/role embeddings to see which argument structure can best apply. 
;; Now comprehend a new sentence and predict whether the second np is an arg0 or an arg1.

;; here the children is an arg2
(comprehend "he teaches children"
            :cxn-inventory *distributional-representations-of-constructional-slots-grammar*
            :timeout nil)

;;english is the arg1
(comprehend "he teaches english"
            :cxn-inventory *distributional-representations-of-constructional-slots-grammar*
            :timeout nil)


;;this one says that students is the arg1!!! not the arg2, heel gek want students zit in het training corpus
(comprehend "he teaches students"
            :cxn-inventory *distributional-representations-of-constructional-slots-grammar*
            :timeout nil)

;; this one predicts what we would expect (is also in training corpus)
(comprehend "the professor teaches students"
            :cxn-inventory *distributional-representations-of-constructional-slots-grammar*
            :timeout nil)

;;correct
(comprehend "the professor teaches english"
            :cxn-inventory *distributional-representations-of-constructional-slots-grammar*
            :timeout nil)




;;ontonotes
;; Jesus arg0 taught FEE the people arg2 in the Temple area every day
;; Mr. Locke arg0 teaches FEE English and comparative literature arg1 at Columbia University .
(comprehend "Jesus taught the people in the Temple area every day"
            :cxn-inventory *distributional-representations-of-constructional-slots-grammar*
            :timeout nil)

(comprehend "Mr. Locke teaches English and comparative literature at Columbia University"
            :cxn-inventory *distributional-representations-of-constructional-slots-grammar*
            :timeout nil)


;; We can also use this to desambiguate word senses!
;; open.01 en open.02 

