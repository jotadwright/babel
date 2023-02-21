;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                   ;;
;; Learning and evaluating PropBank-based grammars.  ;;
;;                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,;;;;

;; Loading the :propbank-grammar system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)

(activate-monitor export-categorial-network-to-jsonl)

;; Activating spacy-api locally
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")


;; Loading the Propbank annotations (takes a couple of minutes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-propbank-annotations 'ewt :ignore-stored-data nil) ; *ewt-annotations*
(load-propbank-annotations 'ontonotes :ignore-stored-data nil) ; *ontonotes-annotations*


;; Storing and restoring grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



#|(defparameter *restored-grammar-sbcl*
  (cl-store:restore
   (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                   :name "propbank-grammar-ontonotes-ewt-core-roles-no-aux-cleaned-sbcl"
                   :type "fcg")))|#

(defparameter *restored-grammar-lw*
  (cl-store:restore
   (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                   :name "propbank-grammar-ontonotes-no-aux-lw"
                   :type "fcg")))

(categorial-network *restored-grammar-lw*)


(cl-store:store *propbank-ontonotes-learned-cxn-inventory-no-aux* ;*propbank-ewt-ontonotes-learned-cxn-inventory*
                (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                                :name "propbank-grammar-ontonotes-no-aux-lw"
                                :type "fcg"))



;; Exporting argms + phrase strings to json file to gather training data for argm classifier
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *full-corpus* (append (train-split *ontonotes-annotations*)
                                    (test-split *ontonotes-annotations*)
                                    (dev-split *ontonotes-annotations*)
                                    (train-split *ewt-annotations*)
                                    (test-split *ewt-annotations*)
                                    (dev-split *ewt-annotations*)))

(export-argm-strings *full-corpus* :corpus-path
                     (babel-pathname :directory '("grammars" "propbank-grammar" "raw-data") :name "argm-phrases" :type "json"))


;; Learning grammars from the annotated data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *training-configuration*
  `((:de-render-mode .  :de-render-constituents-dependents)
    (:node-tests :check-double-role-assignment)
    (:parse-goal-tests :no-valid-children)
    (:max-nr-of-nodes . 100)

    (:construction-inventory-processor-mode . :heuristic-search)
    (:search-algorithm . :best-first)   
    (:cxn-supplier-mode . :hashed-categorial-network)
    
    (:heuristics
     :nr-of-applied-cxns
     :nr-of-units-matched-x2 ;;nr-of-units-matched
     :argm-prediction ;; Don't forget to activate the text-to-role-classification server!!!!!
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
     :argm-phrase-with-string)))

(defparameter *test-grammar* nil)

(defparameter *train-corpus* (train-split *ontonotes-annotations*))


;(subseq (shuffle (append (train-split *ontonotes-annotations*) (train-split *ewt-annotations*))) 0 1000))


(learn-propbank-grammar
 (subseq *train-corpus* 0 10000)
 :excluded-rolesets '("be.01" "be.02" "be.03"
                      "do.01" "do.02" "do.04" "do.11" "do.12"
                      "have.01" "have.02" "have.03" "have.04" "have.05" "have.06" "have.07" "have.08" "have.09" "have.10" "have.11"
                      "get.03" "get.06" "get.24")
 :cxn-inventory '*propbank-ontonotes-learned-cxn-inventory-no-aux-all-strategies*
 :fcg-configuration *training-configuration*)

;(add-element (make-html (find-cxn 'V\(IN\)+ARGM-ADV\(SBAR\:LIKE\)-11+3-CXN *propbank-ontonotes-learned-cxn-inventory-no-aux-all-strategies*)))

(set-configuration *propbank-ontonotes-learned-cxn-inventory-no-aux-all-strategies*
                   :heuristics '(:nr-of-applied-cxns
                                 :nr-of-units-matched
                                 :argm-prediction
                                 :edge-weight
                                 ))


;;by 1940 => TMP not MNR!!
;(comprehend-and-extract-frames (nth 21 *train-corpus*) :cxn-inventory *propbank-ontonotes-learned-cxn-inventory-no-aux-all-strategies*)

(comprehend-and-extract-frames (nth 25 *train-corpus*) :cxn-inventory *restored-grammar-lw*)

(comprehend-and-evaluate (list (nth 26 *train-corpus*)) *propbank-ontonotes-learned-cxn-inventory-no-aux-all-strategies*
                         :excluded-rolesets '("be.01" "be.02" "be.03"
                                              "do.01" "do.02" "do.04" "do.11" "do.12"
                                              "have.01" "have.02" "have.03" "have.04" "have.05" "have.06" "have.07" "have.08" "have.09" "have.10" "have.11"
                                              "get.03" "get.06" "get.24")
                         :core-roles-only nil)
                         ;:include-sentences-with-incomplete-role-constituent-mapping nil)

;(add-element (make-html *test-grammar*))
;(activate-monitor trace-fcg)

;; Testing learned grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;



(comprehend-and-extract-frames "The child fed the cat for her mother" :cxn-inventory *test-grammar*)

(comprehend-and-extract-frames "The walls crumbled to the ground." :cxn-inventory *test-grammar*)

(comprehend-and-extract-frames "The plumber unclogged the sink with a drain snake." :cxn-inventory *test-grammar*)



#|
(comprehend-and-extract-frames "This wage inflation is bleeding the NFL dry, the owners contend" :cxn-inventory *test-grammar* :timeout 600)
"The child fed the cat for her mother"


"This wage inflation is bleeding the NFL dry, the owners contend"


(comprehend-and-extract-frames "I gave her flowers at the restaurant" :cxn-inventory *test-grammar* :timeout 3000)

(comprehend-and-evaluate (list *test-sentence*) *test-grammar*
                         :excluded-rolesets '("be.01" "be.02" "be.03"
                                              "do.01" "do.02" "do.04" "do.11" "do.12"
                                              "have.01" "have.02" "have.03" "have.04" "have.05" "have.06" "have.07" "have.08" "have.09" "have.10" "have.11"
                                              "get.03" "get.06" "get.24")
                         :core-roles-only nil)

(loop for i from 0
      for sentence in (subseq *train-corpus* 0 5)
        do (pprint i)
           (comprehend-and-evaluate (list sentence) *test-grammar*
                         :excluded-rolesets '("be.01" "be.02" "be.03"
                                              "do.01" "do.02" "do.04" "do.11" "do.12"
                                              "have.01" "have.02" "have.03" "have.04" "have.05" "have.06" "have.07" "have.08" "have.09" "have.10" "have.11"
                                              "get.03" "get.06" "get.24")
                         :core-roles-only t
                         :include-sentences-with-incomplete-role-constituent-mapping nil))



;; Cleaning learned grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(monitors::deactivate-all-monitors)
(defparameter *dev-sentences* (append (dev-split *ewt-annotations*)
                                      (dev-split *ontonotes-annotations*)))

;; Use learned grammar on the development corpus to gather statistics on spurious construction applications
(defparameter *sorted-cxns*
  (sort-cxns-for-outliers *propbank-ewt-ontonotes-learned-cxn-inventory-no-aux*
                          (shuffle *dev-sentences*)
                          :timeout 20
                          :nr-of-training-sentences (get-data (blackboard *propbank-ewt-ontonotes-learned-cxn-inventory-no-aux*) :training-corpus-size)
                          :nr-of-test-sentences 500))

;; (Optionally store the ranked cxns for future cleaning)
(cl-store:store *sorted-cxns*
                (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                                :name "sorted-cxns-sbcl"
                                :type "store"))

(setf *sorted-cxns* (cl-store:restore
                     (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                                     :name "sorted-cxns-sbcl"
                                     :type "store")))

;; Delete constructions from the learned grammar that apply too often
(apply-cutoff *restored-grammar-sbcl* :cutoff 4 :sorted-cxn-list *sorted-cxns*)

;; Delete all constructions for be and have from the grammar
(delete-have-and-be-cxns *restored-grammar-sbcl*)
(size (processing-cxn-inventory *restored-grammar-sbcl*))
(size *restored-grammar-sbcl*)
;; Test whether cleaning worked:
;(deactivate-all-monitors)
(comprehend "Hello world" :cxn-inventory *restored-grammar*)
(comprehend-and-extract-frames "I love you" :cxn-inventory *restored-grammar*)


|#




;; Testing learned grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(monitors:activate-monitor trace-fcg)

(comprehend-and-extract-frames "It was all over the news that Germany and Belgium were thrown out of the world cup by two strong opponents" :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory-no-aux*)


                               
(comprehend-and-extract-frames "Oxygen levels in oceans have fallen 2% in 50 years due to climate change, affecting marine habitat and large fish such as tuna and sharks" :cxn-inventory *restored-grammar-lw*)

(comprehend-and-extract-frames "Studies show the different experiences of genders across many domains including education, life expectancy, personality, interests, family life, careers, and political affiliation" :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory-no-aux*)

(comprehend-and-extract-frames "Gender inequality is experienced differently across different cultures and also affects non-binary people ." :cxn-inventory *restored-grammar-lw*)

(comprehend-and-extract-frames "In simple societies, those that have few social roles and statuses occupied by its members, social inequality may be very low" :cxn-inventory *restored-grammar-lw*)


(comprehend-and-extract-frames "Oxygen levels in oceans have fallen 2% in 50 years due to climate change, affecting marine habitat and large fish such as tuna and sharks" :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory-no-aux*)

(comprehend-and-extract-frames "She did not send her mother a dozen roses" :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory-no-aux*)

(comprehend-and-extract-frames (sentence-string (nth 0 (train-split *ewt-annotations*))) :cxn-inventory *propbank-ewt-learned-cxn-inventory*)



(comprehend-and-extract-frames "It is feared if far-right candidate becomes French president she will try to destroy the bloc from inside" :cxn-inventory *restored-grammar-lw*)

(comprehend-and-extract-frames "Much of what the far-right Rassemblement National leader does want to do, however implies breaking the EU's rules, and her possible arrival in the Elysée Palace next weekend could prove calamitous for the 27-member bloc." :cxn-inventory *restored-grammar-lw*)



(comprehend-and-extract-frames "He told him a story" :cxn-inventory *restored-grammar*)
