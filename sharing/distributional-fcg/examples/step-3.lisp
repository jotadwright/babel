(ql:quickload :propbank-grammar)
(ql:quickload :distributional-fcg)

(in-package :propbank-grammar)


;;; set host for embeddings and syntactic analysis (needed for the examples)

(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")
(setf nlp-tools::*embedding-host* "http://127.0.0.1:5001")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               Load in ontonotes and ewt                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *ontonotes-annotations-storage-file*
      (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "propbank-annotations"))
                                      :name "ontonotes-annotations"
                                      :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                       *babel-corpora*))

(setf *ewt-annotations-storage-file* (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "propbank-annotations"))
                                                                       :name "ewt-annotations"
                                                                       :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                                                        *babel-corpora*))



  (load-propbank-annotations 'ewt :ignore-stored-data nil)
  (load-propbank-annotations 'ontonotes :ignore-stored-data nil)

; *ewt-annotations*
; *ontonotes-annotations*


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;         Restore the grammar or learn from scratch           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; learn 
(defparameter *training-configuration*
  `((:de-render-mode .  :de-render-constituents-dependents)
    (:node-tests :check-double-role-assignment)
    (:parse-goal-tests :no-valid-children :no-applicable-cxns)
    (:max-nr-of-nodes . 10)

    (:construction-inventory-processor-mode . :heuristic-search)
    (:search-algorithm . :best-first)   
    (:cxn-supplier-mode . :hashed-categorial-network)
    
    (:heuristics
     :nr-of-applied-cxns
     :nr-of-units-matched-x2 ;;nr-of-units-matched
     ) ;; edge-weight cannot be used, sometimes there are no neighbours
    ;;Additional heuristics: :prefer-local-bindings :frequency
    
    (:heuristic-value-mode . :sum-heuristics-and-parent)
    (:sort-cxns-before-application . nil)

    (:node-expansion-mode . :full-expansion)
    (:hash-mode . :hash-lemma)
    
    (:replace-when-equivalent . nil)
    (:learning-modes
     :core-roles)))

(defparameter *propbank-ewt-ontonotes-cxn-inventory-14-april* nil)


(learn-distributional-propbank-grammar
 (append (train-split *ewt-annotations*)
         (train-split *ontonotes-annotations*))

 :excluded-rolesets '("be.01" "be.02" "be.03"
                      "do.01" "do.02" "do.04" "do.11" "do.12"
                      "have.01" "have.02" "have.03" "have.04" "have.05" "have.06" "have.07" "have.08" "have.09" "have.10" "have.11"
                      "get.03" "get.06" "get.24")
 :cxn-inventory '*propbank-ewt-ontonotes-cxn-inventory-14-april*
 :fcg-configuration *training-configuration*)


(cl-store:store *propbank-ewt-ontonotes-cxn-inventory-14-april*
                (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "distributional-fcg"))
                                                :name "cxn-inventory-google-example"
                                                :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                                 *babel-corpora*))

(preprocessing-and-configs *propbank-ewt-ontonotes-cxn-inventory-14-april* :step-3 :make-role-embeddings nil)

(activate-monitor trace-fcg)

(comprehend-all "Try googling it for more info :-RRB-" :cxn-inventory *propbank-ewt-ontonotes-cxn-inventory-14-april* :n 1 :timeout 200)

(fcg::draw-categorial-network-node-and-neighbours *propbank-ewt-ontonotes-cxn-inventory-14-april*
                                                  'propbank-grammar::google\(v\)-1)

(pprint (graph-utils::neighbors
         (fcg::graph (categorial-network *propbank-ewt-ontonotes-cxn-inventory-14-april*))
         'propbank-grammar::google\(v\)-1 :return-ids? nil :edge-type 'lex-gram))


(fcg::draw-categorial-network-node-and-neighbours *propbank-ewt-ontonotes-cxn-inventory-14-april*
                                                  'propbank-grammar::disregard\(v\)-1)

(pprint (graph-utils::neighbors
         (fcg::graph (categorial-network *propbank-ewt-ontonotes-cxn-inventory-14-april*))
         'propbank-grammar::disregard\(v\)-1 :return-ids? nil :edge-type 'lex-gram))



(defparameter *options*
  (list "Nothing contained herein should be considered as an offer to sell or a solicitation of an offer to buy any financial instruments discussed herein ."
        "For me it is n't about fulfillment or finding my life 's purpose in my work ."
        "\" In the wake of Hurricane Katrina , affordable housing in New Orleans is more scarce than ever ."
        "Yes bus service is available ."
        "It was huge and scared the crap out of me ."
        "The pizzas are huge and super delicious ."
        "There has been at least consideration for the other side of the aisle before a stunt ."
        "Today President Bush unveiled a seven point one billion dollar plan to prepare for a possible flu pandemic ."
        "Um , I got , central , an inflammation of the central nervous system , so my whole body was paralyzed ."
        "Off the coast there was a dramatic rescue by a cruise ship ship ."
        "The drilling and cutting effort took several days ."
        "On the temple gate a fading couplet reads : \" He who is truly dedicated to his endeavor will become a great master , and shall make of his surroundings a land of delight ."
        "Then came the news that Chang Kwang-chih , the respected archeologist and former vice-president of the Academia Sinica , had died of illness in the United States ."
        "The European Union executive body said that the other chapters could be opened , but the temporary closure of these chapters would depend on Turkey 's fulfillment of opening its airports and harbors to Greek Cypriot traffic ."
        "They worry about their careers , drink too much and suffer through broken marriages and desultory affairs ."
        "The competition has grown more intense as bigger banks such as Norwest Corp. of Minneapolis and Chemical Banking Corp. of New York extend their market - share battles into small towns across the nation ."
        "The sum is more than double what the House had approved for the program , but the list of qualified airports would be cut by 22 under new distance requirements and limits on the level of subsidy ."
        "Offsetting the Remic - related purchases were continued heavy sales by mortgage originators , which are producing increased amounts of fixed - rate mortgage - backed issues with lower rates ." ;; V(V)
        "Metropolitan Detroit was written off economically during the early 1980s , as the domestic auto industry suffered a serious sales depression and adjustment ."
        "If the systematic slaughter of tens or hundreds of thousands of human beings does n't represent a `` breakdown in morality , '' what does ?"
        "The Engineer will interface with commercialization and development teams to create systems test plans , provide recommendations on path forward and problem resolution , and follow through on the implementation and verification ."
        ))



(defparameter *s*
  (loop for sentence in (append (dev-split *ewt-annotations*)
                                (dev-split *ontonotes-annotations*)
                                (test-split *ewt-annotations*)
                                (test-split *ontonotes-annotations*))
        for sentence-string = (sentence-string sentence)
        if (find sentence-string *options* :test #'string=)
          collect sentence))

(activate-monitor trace-fcg)

(defparameter *solutions*
  (comprehend-all (sentence-string (second *s*)) :cxn-inventory *propbank-ewt-ontonotes-cxn-inventory-8-april* :timeout 1000 :n 1)
  )

(comprehend-all "it is about purpose or fulfillment" :cxn-inventory *propbank-ewt-ontonotes-cxn-inventory-8-april* :timeout nil :n 1)

(comprehend-all "The pizzas are huge and super delicious ." :cxn-inventory *propbank-ewt-ontonotes-cxn-inventory-13-april*
                :timeout 100 :n 1)


(fcg::draw-categorial-network-node-and-neighbours *propbank-ewt-ontonotes-cxn-inventory-13-april*
                                                  'propbank-grammar::HUGE\(JJ\)-1)

(fcg::draw-categorial-network-node-and-neighbours *propbank-ewt-ontonotes-cxn-inventory-13-april*
                                                  'propbank-grammar::serve\(nn\)-1)

arg1(nn)+v(nn)-1


(loop for sentence in *s*
      do (comprehend-all (sentence-string sentence)
                         :cxn-inventory *propbank-ewt-ontonotes-cxn-inventory-13-april*
                         :timeout 120 :n 1))

(deactivate-all-monitors)


(pprint (graph-utils::neighbors
         (fcg::graph (categorial-network *propbank-ewt-ontonotes-cxn-inventory-8-april*))
         'propbank-grammar::fulfill\(nn\)-1 :return-ids? nil :edge-type 'lex-gram))

(pprint (graph-utils::neighbors
         (fcg::graph (categorial-network *propbank-ewt-ontonotes-cxn-inventory-8-april*))
         'propbank-grammar::be_like\(v\)-1 :return-ids? nil :edge-type 'lex-gram))

;; this one is used to find the gramm cxn!!!
(pprint (graph-utils::neighbors
         (fcg::graph (categorial-network *propbank-ewt-ontonotes-cxn-inventory-8-april*))
         'propbank-grammar::hold\(NN\)-9 :return-ids? nil :edge-type 'lex-gram))

(pprint (graph-utils::neighbors
         (fcg::graph (categorial-network *propbank-ewt-ontonotes-cxn-inventory-8-april*))
         'propbank-grammar::pass\(NN\)-5 :return-ids? nil :edge-type 'lex-gram))



(pprint (graph-utils::neighbors
         (fcg::graph (categorial-network *propbank-ewt-ontonotes-cxn-inventory-8-april*))
         'propbank-grammar::huge\(jj\)-3 :return-ids? nil :edge-type 'lex-gram))

(pprint (graph-utils::neighbors
         (fcg::graph (categorial-network *propbank-ewt-ontonotes-cxn-inventory-8-april*))
         'propbank-grammar::delicious\(jj\)-3 :return-ids? nil :edge-type 'lex-gram))






;(categorial-network-neighbours-of-node->s-dot (categorial-network (*propbank-ewt-ontonotes-cxn-inventory-8-april*))



(add-element (make-html (categorial-network propbank-grammar::*propbank-ewt-ontonotes-teach-cxn-inventory*)))


(draw-categorial-network-node-and-neighbours propbank-grammar::*propbank-ewt-ontonotes-teach-cxn-inventory* 'propbank-grammar::V\(V\)-9266)

(draw-categorial-network-node-and-neighbours propbank-grammar::*propbank-ewt-ontonotes-teach-cxn-inventory* 'propbank-grammar::teach\(V\)-1021)

