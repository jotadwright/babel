;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                 ;;
;; Learning grammars from propbank-annotated data. ;;
;;                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,;;

;; Loading the :propbank-english sytem
;;(ql:quickload :propbank-english)
(in-package :propbank-english)

;; Loading the Propbank annotations (takes a couple of minutes)
(load-propbank-annotations 'ewt)
(load-propbank-annotations 'ontonotes)
; *ewt-annotations*
; *ontonotes-annotations*


;; Activating spacy-api locally
(setf nlp-tools::*penelope-host* "http://localhost:5000")

;; Activating trace-fcg
(activate-monitor trace-fcg)


;;;;;;;;;;;
;; Data  ;;
;;;;;;;;;;;

(defparameter *opinion-sentences* (shuffle (loop for roleset in '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
                                                 append (all-sentences-annotated-with-roleset roleset
                                                                                              :split #'train-split
                                                                                              :corpus *ontonotes-annotations*)
                                                 append (all-sentences-annotated-with-roleset roleset
                                                                                              :split #'dev-split
                                                                                              :corpus *ontonotes-annotations*)
                                                 append (all-sentences-annotated-with-roleset roleset
                                                                                              :split #'train-split
                                                                                              :corpus *ewt-annotations*)
                                                 append (all-sentences-annotated-with-roleset roleset
                                                                                              :split #'dev-split
                                                                                              :corpus *ewt-annotations*))))


(defparameter *opinion-sentences-dev* (shuffle (loop for roleset in '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
                                                 append (all-sentences-annotated-with-roleset roleset
                                                                                              :split #'dev-split
                                                                                              :corpus *ontonotes-annotations*))))

(defparameter *opinion-sentences-test* (shuffle
                                        (spacy-benepar-compatible-sentences
                                         (loop for roleset in '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
                                               append (all-sentences-annotated-with-roleset roleset
                                                                                            :split #'test-split
                                                                                            :corpus *ontonotes-annotations*))
                                         nil)))
(length *opinion-sentences-test*)
(defparameter *believe-sentences* (shuffle (loop for roleset in '("BELIEVE.01")
                                                 append (all-sentences-annotated-with-roleset roleset :split #'train-split))))

(defparameter *believe-sentences-dev* (shuffle (loop for roleset in '("BELIEVE.01")
                                                 append (all-sentences-annotated-with-roleset roleset :split #'dev-split))))


(defparameter *test-sentences-all-frames* (subseq (spacy-benepar-compatible-sentences
                                                   (subseq (shuffle (test-split *ontonotes-annotations*)) 0 200) nil) 0 100))

(length *test-sentences-all-frames*)






;;Problems
;;--------------
;;Possessive 's linked to be.02 (existential be)
(graph-utils:edge-weight (graph-utils::graph (get-type-hierarchy *cleaned-grammar*))
                         (graph-utils:lookup-node (graph-utils::graph (get-type-hierarchy *cleaned-grammar*)) 6653)
                         (graph-utils:lookup-node (graph-utils::graph (get-type-hierarchy *cleaned-grammar*)) 3673))

(graph-utils:lookup-node (graph-utils::graph (get-type-hierarchy *cleaned-grammar*)) 3673) ;;'type-hierarchies::BE\(POS\)-35)
(graph-utils:lookup-node (graph-utils::graph (get-type-hierarchy *cleaned-grammar*)) 6653)

(node-p '#:BE.02-2249 (get-type-hierarchy *cleaned-grammar*))
(node-p 3673 (get-type-hierarchy *cleaned-grammar*))
 
(gethash be\(pos\)-35 (graph-utils::nodes (graph-utils::graph (get-type-hierarchy *cleaned-grammar*))))

(evaluate-predictions *evaluation-result* :core-roles-only t :include-timed-out-sentences nil :include-word-sense t)
(evaluate-predictions *evaluation-result-cleaned* :core-roles-only t :include-timed-out-sentences t :include-word-sense nil)


(defparameter *test* (first *evaluation-result*))

(evaluate-predictions (list (nth 15 *evaluation-result-cleaned*)) :core-roles-only t :include-word-sense nil)
(evaluate-predictions *evaluation-result* :core-roles-only t :include-timed-out-sentences nil :include-word-sense nil)


(activate-monitor trace-fcg)

(comprehend-and-extract-frames (nth 15 *test-sentences-all-frames*) :cxn-inventory *core-roles-cleaned-frequency-grammar*)
(comprehend-and-extract-frames "The water boils" :cxn-inventory *propbank-learned-cxn-inventory*)
(length *test-sentences-all-frames*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Storing and restoring grammars ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-store:store *propbank-learned-cxn-inventory*
                (babel-pathname :directory '("grammars" "propbank-english" "grammars")
                                :name "core-roles-ontonotes-train-cleaned"
                                :type "fcg"))

(defparameter *restored-grammar*
  (restore (babel-pathname :directory '("grammars" "propbank-english" "grammars")
                           :name "core-roles-ontonotes-train-cleaned"
                           :type "fcg")))
(size *restored-grammar*)

;(setf *th* (get-type-hierarchy *restored-grammar*))

;(clean-type-hierarchy *th*)

      

;;;;;;;;;;;;;;
;; Training ;;
;;;;;;;;;;;;;;

(defparameter *training-configuration*
  '((:de-render-mode .  :de-render-constituents-dependents)
    (:node-tests :check-double-role-assignment :restrict-nr-of-nodes)
    (:parse-goal-tests :no-valid-children)
    (:max-nr-of-nodes . 100)
    (:node-expansion-mode . :multiple-cxns)
    (:priority-mode . :nr-of-applied-cxns)
    (:queue-mode . :greedy-best-first)
    (:hash-mode . :hash-lemma )
    (:parse-order
     lexical-cxn
     argument-structure-cxn
     argm-cxn
     word-sense-cxn)
    (:replace-when-equivalent . nil)
    (:learning-modes
     :core-roles
     :argm-pp
     :argm-sbar
     :argm-leaf
     )
    (:cxn-supplier-mode . :propbank-english)))

;(set-configuration *propbank-learned-cxn-inventory* :node-tests '(:check-double-role-assignment :restrict-nr-of-nodes))
;(set-configuration *propbank-learned-cxn-inventory* :parse-goal-tests '( :no-valid-children ))
;;(set-configuration *propbank-learned-cxn-inventory* :cxn-supplier-mode :propbank-english)

(with-disabled-monitor-notifications
  (learn-propbank-grammar
   (train-split *ontonotes-annotations*)
   :selected-rolesets nil
   :cxn-inventory '*propbank-learned-cxn-inventory*
   :fcg-configuration *training-configuration*))

(loop for sentence in (subseq (train-split *ontonotes-annotations*) 0 10)
      do (comprehend-and-extract-frames sentence :cxn-inventory *propbank-learned-cxn-inventory*))

;;use.01 sense cxn past niet toe door afwezigheid gram-category!
(loop for sentence in (subseq (train-split *ontonotes-annotations*) 26 27)
      do (comprehend-and-extract-frames sentence :cxn-inventory *propbank-learned-cxn-inventory*))

(comprehend-and-extract-frames "He listened to the radio while doing the dishes" :cxn-inventory *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames "Old Li Jingtang still tells visitors old war stories" :cxn-inventory *restored-grammar*)
(set-configuration (visualization-configuration *propbank-learned-cxn-inventory*) :hide-features nil)
(comprehend-and-extract-frames "Only Nixon could go to China, he told a group of Americans" :cxn-inventory *restored-grammar*)
(defparameter *cleaned-grammar* (remove-cxns-under-frequency *propbank-learned-cxn-inventory* 5))

(clean-grammar *propbank-learned-cxn-inventory* :remove-faulty-cnxs t)

(clean-type-hierarchy (get-type-hierarchy *cleaned-grammar*) :remove-edges-with-freq-smaller-than 2)


(evaluate-propbank-corpus *test-sentences-all-frames* *propbank-learned-cxn-inventory* :timeout 60)

(defparameter *evaluation-result* (restore (babel-pathname :directory '(".tmp")
                                                           :name "2020-11-18-12-02-55-evaluation"
                                                           :type "store")))

(evaluate-predictions *evaluation-result* :core-roles-only t :include-timed-out-sentences nil :include-word-sense t)

;;;;;;;;;;;;;
;; Testing ;;
;;;;;;;;;;;;;


(setf *selected-sentence*
      (find "When traders become confident that the stock market has stabilized , oil prices are expected to rise as supply and demand fundamentals once again become the major consideration ." *opinion-sentences* :key #'sentence-string :test #'string=))

(learn-cxn-from-propbank-annotation *selected-sentence* "expect.01" *propbank-learned-cxn-inventory* :argm-subclause)
(evaluate-propbank-sentence *selected-sentence* *propbank-learned-cxn-inventory* :silent nil :selected-rolesets '("expect.01"))

(add-element (make-html-fcg-light (initial-transient-structure *selected-sentence*) :construction-inventory *propbank-learned-cxn-inventory*
                                  :feature-types (feature-types *propbank-learned-cxn-inventory* )))

(learn-propbank-grammar (list *selected-sentence*)
                        :cxn-inventory '*propbank-learned-cxn-inventory*
                        :fcg-configuration *training-configuration*
                        :selected-rolesets '("expect.01")
                        :silent t
                        :tokenize? nil)
(activate-monitor trace-fcg) 
(with-activated-monitor trace-fcg
  (add-element (make-html (second (multiple-value-list (comprehend *selected-sentence* :cxn-inventory *propbank-learned-cxn-inventory*)
                                                       )))))

(add-element (make-html (first (multiple-value-list (comprehend *selected-sentence* :cxn-inventory *propbank-learned-cxn-inventory*)
                                                       ))))

(add-element (make-html (de-render (sentence-string *selected-sentence*) :de-render-constituents-dependents-without-tokenisation)))

(add-element (make-html *propbank-learned-cxn-inventory*))

(add-element (make-html (find-cxn "BECOME.01-ARG1:PRP+V:BECOME+ARG2:NP+ARGM-ADV:PP(CC-FROM)+ARGM-ADV:PP(CC-FROM)+ARGM-TMP:NOW+3-CXN-1"
          *propbank-learned-cxn-inventory* :hash-key 'become :test #'string=)))
          
(evaluate-propbank-sentences
 (list *selected-sentence* *propbank-learned-cxn-inventory* :selected-rolesets  '("believe.01")  :silent t))

(activate-monitor trace-fcg)
(comprehend-and-extract-frames "He believed the man ." :cxn-inventory *propbank-learned-cxn-inventory*)

(comprehend-and-extract-frames "Luc could not believe his eyes" :cxn-inventory *cleaned-grammar*)
(comprehend-and-extract-frames "Luc could not believe that it is true" :cxn-inventory *cleaned-grammar*)

(comprehend-and-extract-frames "Luc would come to Venice" :cxn-inventory *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames "Carlo thinks that Luc would not come to Venice" :cxn-inventory *propbank-learned-cxn-inventory*)


(comprehend-and-extract-frames "" :cxn-inventory *propbank-learned-cxn-inventory*)



(defparameter *spacy-benepar-compatible-sentences* (remove-if-not #'(lambda (sentence)
                                                                      (loop for roleset in (all-rolesets  sentence)
                        always (spacy-benepar-compatible-annotation sentence roleset)))
                 (dev-split *ontonotes-annotations*)))


;;23/06/2020: GRAMMAR DEBUGGING (tests met lex-class als set gestaakt omdat connl-sentences eerst opnieuw geannoteerd moeten worden met initial ts die kloppen!)
;;-------------------------------

;;for such clues = argm-prp (wel gezien in combinatie met WATCH:VBP en WATCH:VBN en zelfde aantal extra units)
(setf *selected-sentence*
      (find "Inventories are closely watched for such clues , for instance ." *spacy-benepar-compatible-sentences* :key #'sentence-string :test #'string=))
;;ARG1:NNS nooit gezien
(comprehend-and-extract-frames "Inventories are closely watched for such clues , for instance ." :cxn-inventory *restored-grammar*)

;;manueel deze toepassen lukt wel (probleem met zwarte nodes!!):
(add-element (make-html (find-cxn "ALL-FRAMES-V:VBN+ARGM-MNR:CLOSELY+1-CXN-2" *restored-grammar* :hash-key 'closely :test #'string=)))


;;testing
(learn-propbank-grammar (list *selected-sentence*)
                        :cxn-inventory '*propbank-learned-cxn-inventory*
                        :selected-rolesets '("watch.01")
                        :fcg-configuration *training-configuration*)
(activate-monitor trace-fcg)
(comprehend-and-extract-frames "Anne sent her mother a dozen roses" :cxn-inventory *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames "Tsar Nicholas II gave his wife a Fabergé egg." :cxn-inventory *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames "It is a Fabergé egg that Tsar Nicholas II gave his wife." :cxn-inventory *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames "He called his mother while doing the dishes" :cxn-inventory *propbank-learned-cxn-inventory*)

(comprehend-and-extract-frames "Elise will not let the Pokemon escape" :cxn-inventory *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames "He usually takes the bus to school" :cxn-inventory *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames "He listened to the radio while doing the dishes" :cxn-inventory *propbank-learned-cxn-inventory*)
(comprehend "She had dinner in Paris." :cxn-inventory *cleaned-grammar*)

(original-cxn-set (processing-cxn-inventory *cleaned-grammar*))
;; Testing new sentences with learned grammar 
;; Guardian FISH article
;;;;;;;;;;;;;;;;;;;;;;;;;

(set-configuration *propbank-learned-cxn-inventory* :parse-goal-tests '(:no-valid-children))
(set-configuration *propbank-learned-cxn-inventory* :parse-order '(:multi-argument-core-roles :argm-pp :argm-with-lemma ))

(comprehend-and-extract-frames "Oxygen levels in oceans have fallen 2% in 50 years due to climate change, affecting marine habitat and large fish such as tuna and sharks" :cxn-inventory *cleaned-grammar*)

;;threaten.01 niet gevonden (cxns met enkel core roles zouden dit oplossen)
(comprehend-and-extract-frames "The depletion of oxygen in our oceans threatens future fish stocks and risks altering the habitat and behaviour of marine life, scientists have warned, after a new study found oceanic oxygen levels had fallen by 2% in 50 years." :cxn-inventory *propbank-learned-cxn-inventory*)

(comprehend-and-extract-frames "This brings us to another problem that comes up when dealing with natural language . " :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "The study, carried out at Geomar Helmholtz Centre for Ocean Research in Germany, was the most comprehensive of the subject to date." :cxn-inventory *restored-grammar*)

;;attribute.01 wordt niet gevonden > 'ARG1:NP - has been attributed - ARG2:PP nooit gezien in training'
(comprehend-and-extract-frames "The fall in oxygen levels has been attributed to global warming and the authors warn that if it continues unchecked, the amount of oxygen lost could reach up to 7% by 2100." :cxn-inventory *cleaned-grammar*)
(length (constructions-list *cleaned-grammar*))
(size *cleaned-grammar*)

;;adapt-cxn niet geleerd:
(comprehend-and-extract-frames "Very few marine organisms are able to adapt to low levels of oxygen." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "The paper contains analysis of wide-ranging data from 1960 to 2010, documenting changes in oxygen distribution in the entire ocean for the first time ." :cxn-inventory *restored-grammar*)

;;verkeerde analyse (mss quotes anders zetten?)
(comprehend-and-extract-frames "Since large fish in particular avoid or do not survive in areas with low oxygen content, these changes can have far-reaching biological consequences, said Dr Sunke Schmidtko, the report's lead author . " :cxn-inventory *restored-grammar*)

;;have? mss have uitschakelen voor toepassingen?
(comprehend-and-extract-frames "Some areas have seen a greater drop than others ." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "The Pacific - the planet's largest ocean - has suffered the greatest volume of oxygen loss, while the Arctic witnessed the sharpest decline by percentage ." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames " ' While the slight decrease of oxygen in the atmosphere is currently considered non-critical, the oxygen losses in the ocean can have far-reaching consequences because of the uneven distribution, ' added another of the report's authors, Lothar Stramma ." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "It is increasingly clear that the heaviest burden of climate change is falling on the planet's oceans, which absorb more than 30% of the carbon produced on land ." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "Rising sea levels are taking their toll on many of the world's poorest places ." :cxn-inventory *restored-grammar*)

;;DEVASTATE niet gevonden!(sparseness) ARG0:NP - have devastated - ARG1:NP
(comprehend-and-extract-frames "Warming waters have devastated corals - including the Great Barrier Reef - in bleaching events." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "Acidic oceans, caused by a drop in PH levels as carbon is absorbed, threaten creatures' ability to build their calcium-based shells and other structures." :cxn-inventory *restored-grammar*)

;;CAUSED niet gevonden! Triggered ook niet!
(comprehend-and-extract-frames "Warming waters have also caused reproductive problems in species such as cod, and triggered their migration to colder climates." :cxn-inventory *restored-grammar*)

;; goed
(comprehend-and-extract-frames "Lower oxygen levels in larger parts of the ocean are expected to force animals to seek out ever shrinking patches of habitable water, with significant impacts on the ecosystem and food web." :cxn-inventory *restored-grammar*)


(comprehend-and-extract-frames "Callum Roberts, the author of Ocean of Life and a marine conservation biologist at the University of York, is unsurprised by the latest findings." :cxn-inventory *restored-grammar*)

;goed maar veel be's en have's(!!) >> vreemde have cxn geleerd! enkel pronoun, geen v
(comprehend-and-extract-frames "'What we're seeing is fallout from global warming,' he says." :cxn-inventory *restored-grammar*)


(comprehend-and-extract-frames "'It's straightforward physics and chemistry playing out in front of our eyes, entirely in keeping with what we'd expect and yet another nail in coffin of climate change denial.'" :cxn-inventory *restored-grammar*)


(comprehend-and-extract-frames "Scientists have long predicted ocean deoxygenation due to climate change, but confirmation on this global scale, and at deep sea level, is concerning them." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "Last year, Matthew Long, an oceanographer at the National Center for Atmospheric Research in Colorado, predicted that oxygen loss would become evident 'across large regions of the oceans' between 2030 and 2040." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "Reacting to the German findings, Long said it was 'alarming to see this signal begin to emerge clearly in the observational data', while Roberts said, 'We now have a measurable change which is attributable to global warming.'" :cxn-inventory *restored-grammar*)



(comprehend-and-extract-frames "The report explains that the ocean's oxygen supply is threatened by global warming in two ways." :cxn-inventory *propbank-learned-cxn-inventory*)

(comprehend-and-extract-frames "Warmer water is less able to contain oxygen than cold, so as the oceans warm, oxygen is reduced." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames  "Warmer water is also less dense, so the oxygen-rich surface layer cannot easily sink and circulate. " :cxn-inventory *restored-grammar*)



(setf *x* (def-fcg-constructions-with-type-hierarchy grammar))

(add-categories '(a b c d e) (get-type-hierarchy *x*))

(add-link 'a 'b (get-type-hierarchy *x*) :weight 10.0)
(add-link 'c 'b (get-type-hierarchy *x*) :weight 5.0)
(add-link 'b 'c (get-type-hierarchy *x*) :weight 1.0)

(graph-utils::incf-edge-weight (graph-utils::graph (get-type-hierarchy *x*))  'b 'c :delta 1)

(graph-utils::)

(graph-utils:list-edges (graph-utils::graph (get-type-hierarchy *x*)))

(graph-utils:edge-weight (graph-utils::graph (get-type-hierarchy *x*)) 'c 'b)

(clean-type-hierarchy (get-type-hierarchy *x*))

(clean-type-hierarchy (get-type-hierarchy *propbank-learned-cxn-inventory*))
