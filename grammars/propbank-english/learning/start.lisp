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
                                                   (subseq (shuffle (test-split *ontonotes-annotations*)) 0 500) nil) 0 250))

(length *test-sentences-all-frames*)

(defparameter *train-sentences-all-frames* (subseq (spacy-benepar-compatible-sentences
                                                    (subseq (shuffle (train-split *ontonotes-annotations*)) 0 100) nil) 0 10))
(length *train-sentences-all-frames*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Storing and restoring grammars ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-store:store *propbank-learned-cxn-inventory*
                (babel-pathname :directory '("grammars" "propbank-english" "grammars")
                                :name "core-roles-ontonotes-train-clean-faulty"
                                :type "fcg"))

(defparameter *restored-grammar*
  (restore (babel-pathname :directory '("grammars" "propbank-english" "grammars")
                           :name "core-roles-ontonotes-train-clean-faulty"
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
     argm-phrase-cxn
     argm-leaf-cxn
     word-sense-cxn)
    (:replace-when-equivalent . nil)
    (:learning-modes
     :core-roles
     :argm-pp
     :argm-sbar
     :argm-leaf
     )
    (:cxn-supplier-mode . :propbank-english)))



(with-disabled-monitor-notifications
  (learn-propbank-grammar
   *train-sentences-all-frames* 
   :selected-rolesets nil
   :cxn-inventory '*propbank-learned-cxn-inventory*
   :fcg-configuration *training-configuration*))

;;>> Cleaning
;;--------------

(clean-grammar *propbank-learned-cxn-inventory* :remove-faulty-cnxs t)

(defparameter *cleaned-grammar* (remove-cxns-under-frequency *propbank-learned-cxn-inventory* 2))

(clean-type-hierarchy (get-type-hierarchy *cleaned-grammar*) :remove-edges-with-freq-smaller-than 2)


;;;;;;;;;;;;;;;;;
;; Evaluation  ;;
;;;;;;;;;;;;;;;;;
(loop for i from 1
      for sentence in *train-sentences-all-frames*
      do (format t "~%~% Sentence: ~a ~%" i)
      (comprehend-and-evaluate (list sentence ) *propbank-learned-cxn-inventory* :core-roles-only nil :silent nil))


(comprehend-and-evaluate (list (nth1 8 *train-sentences-all-frames*))
                         *propbank-learned-cxn-inventory* :core-roles-only nil :silent nil)

;((:PRECISION . 0.65467626) (:RECALL . 0.47894737) (:F1-SCORE . 0.55319155) (:NR-OF-CORRECT-PREDICTIONS . 91) (:NR-OF-PREDICTIONS . 139) (:NR-OF-GOLD-STANDARD-PREDICTIONS . 190))
(comprehend-and-evaluate (subseq *train-sentences-all-frames* 100003 100004)
                         *restored-grammar* :core-roles-only nil :silent nil)


(comprehend-and-extract-frames "Looking at the Japanese experience , the Asahi Shimbun has a daily circulation of about 10 million " :cxn-inventory *propbank-learned-cxn-inventory* )

(shuffle '(1 2 3))

(defparameter *th* (get-type-hierarchy *propbank-learned-cxn-inventory*))

(comprehend-and-extract-frames "He asked to go" :cxn-inventory *propbank-learned-cxn-inventory*)

(evaluate-propbank-corpus *train-sentences-all-frames* *propbank-learned-cxn-inventory* :timeout 60) ;;sanity check
(evaluate-propbank-corpus *test-sentences-all-frames* *propbank-learned-cxn-inventory* :timeout 60)

(evaluate-propbank-corpus *train-sentences-all-frames* *cleaned-grammar* :timeout 60)



(defparameter *evaluation-result-no-cleaning* (restore (babel-pathname :directory '(".tmp")
                                                           :name "2020-11-20-14-44-22-evaluation"
                                                           :type "store")))

(defparameter *evaluation-result-with-cleaning<2* (restore (babel-pathname :directory '(".tmp")
                                                           :name "2020-11-20-14-21-16-evaluation"
                                                           :type "store")))

(defparameter *evaluation-result-with-cleaning<6* (restore (babel-pathname :directory '(".tmp")
                                                           :name "2020-11-20-12-46-36-evaluation"
                                                           :type "store")))

(defparameter *evaluation-result-with-cleaning<4* (restore (babel-pathname :directory '(".tmp")
                                                           :name "2020-11-20-12-06-41-evaluation"
                                                           :type "store")))



(evaluate-predictions *evaluation-result-no-cleaning* :core-roles-only nil :include-timed-out-sentences nil :include-word-sense t)

(evaluate-predictions *evaluation-result-with-cleaning<2* :core-roles-only t :include-timed-out-sentences nil :include-word-sense t)
(evaluate-predictions *evaluation-result-with-cleaning<4* :core-roles-only t :include-timed-out-sentences nil :include-word-sense t)
(evaluate-predictions *evaluation-result-with-cleaning<6* :core-roles-only t :include-timed-out-sentences nil :include-word-sense t)







;;;;;;;;;;;;;;;;;;;
;; Demonstration ;;
;;;;;;;;;;;;;;;;;;;



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



(comprehend-and-extract-frames "Anne sent her mother a dozen roses" :cxn-inventory *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames "Tsar Nicholas II gave his wife a Fabergé egg." :cxn-inventory *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames "It is a Fabergé egg that Tsar Nicholas II gave his wife." :cxn-inventory *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames "He called his mother while doing the dishes" :cxn-inventory *propbank-learned-cxn-inventory*)

(comprehend-and-extract-frames "Elise will not let the Pokemon escape" :cxn-inventory *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames "He usually takes the bus to school" :cxn-inventory *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames "He listened to the radio while doing the dishes" :cxn-inventory *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames "She had dinner in Paris." :cxn-inventory *cleaned-grammar*)




;; Guardian FISH article
;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *demo-grammar* *propbank-learned-cxn-inventory*)

(comprehend-and-extract-frames "Oxygen levels in oceans have fallen 2% in 50 years due to climate change, affecting marine habitat and large fish such as tuna and sharks" :cxn-inventory *demo-grammar*)

;;threaten.01 niet gevonden (cxns met enkel core roles zouden dit oplossen)
(comprehend-and-extract-frames "The depletion of oxygen in our oceans threatens future fish stocks and risks altering the habitat and behaviour of marine life, scientists have warned, after a new study found oceanic oxygen levels had fallen by 2% in 50 years." :cxn-inventory *demo-grammar*)

(comprehend-and-extract-frames "This brings us to another problem that comes up when dealing with natural language . " :cxn-inventory *demo-grammar*)

(comprehend-and-extract-frames "The study, carried out at Geomar Helmholtz Centre for Ocean Research in Germany, was the most comprehensive of the subject to date." :cxn-inventory *demo-grammar*)

;;attribute.01 wordt niet gevonden > 'ARG1:NP - has been attributed - ARG2:PP nooit gezien in training'
(comprehend-and-extract-frames "The fall in oxygen levels has been attributed to global warming and the authors warn that if it continues unchecked, the amount of oxygen lost could reach up to 7% by 2100." :cxn-inventory *demo-grammar*)


(comprehend-and-extract-frames "The fall in oxygen levels has been attributed to global warming ." :cxn-inventory *demo-grammar*)

;;adapt-cxn niet geleerd:
(comprehend-and-extract-frames "Very few marine organisms are able to adapt to low levels of oxygen." :cxn-inventory *demo-grammar*)

(comprehend-and-extract-frames "The paper contains analysis of wide-ranging data from 1960 to 2010, documenting changes in oxygen distribution in the entire ocean for the first time ." :cxn-inventory *demo-grammar*)

;;verkeerde analyse (mss quotes anders zetten?)
(comprehend-and-extract-frames "Since large fish in particular avoid or do not survive in areas with low oxygen content, these changes can have far-reaching biological consequences, said Dr Sunke Schmidtko, the report's lead author . " :cxn-inventory *demo-grammar*)

;;have? mss have uitschakelen voor toepassingen?
(comprehend-and-extract-frames "Some areas have seen a greater drop than others ." :cxn-inventory *demo-grammar*)

(comprehend-and-extract-frames "The Pacific - the planet's largest ocean - has suffered the greatest volume of oxygen loss, while the Arctic witnessed the sharpest decline by percentage ." :cxn-inventory *demo-grammar*)

(comprehend-and-extract-frames " ' While the slight decrease of oxygen in the atmosphere is currently considered non-critical, the oxygen losses in the ocean can have far-reaching consequences because of the uneven distribution, ' added another of the report's authors, Lothar Stramma ." :cxn-inventory *demo-grammar*)

(comprehend-and-extract-frames "It is increasingly clear that the heaviest burden of climate change is falling on the planet's oceans, which absorb more than 30% of the carbon produced on land ." :cxn-inventory *demo-grammar*)

(comprehend-and-extract-frames "Rising sea levels are taking their toll on many of the world's poorest places ." :cxn-inventory *demo-grammar*)

;;DEVASTATE niet gevonden!(sparseness) ARG0:NP - have devastated - ARG1:NP
(comprehend-and-extract-frames "Warming waters have devastated corals - including the Great Barrier Reef - in bleaching events." :cxn-inventory *demo-grammar*)

(comprehend-and-extract-frames "Acidic oceans, caused by a drop in PH levels as carbon is absorbed, threaten creatures' ability to build their calcium-based shells and other structures." :cxn-inventory *demo-grammar*)

;;CAUSED niet gevonden! Triggered ook niet!
(comprehend-and-extract-frames "Warming waters have also caused reproductive problems in species such as cod, and triggered their migration to colder climates." :cxn-inventory *demo-grammar*)

;; goed
(comprehend-and-extract-frames "Lower oxygen levels in larger parts of the ocean are expected to force animals to seek out ever shrinking patches of habitable water, with significant impacts on the ecosystem and food web." :cxn-inventory *demo-grammar*)


(comprehend-and-extract-frames "Callum Roberts, the author of Ocean of Life and a marine conservation biologist at the University of York, is unsurprised by the latest findings." :cxn-inventory *demo-grammar*)

;goed maar veel be's en have's(!!) >> vreemde have cxn geleerd! enkel pronoun, geen v
(comprehend-and-extract-frames "'What we're seeing is fallout from global warming,' he says." :cxn-inventory *demo-grammar*)


(comprehend-and-extract-frames "'It's straightforward physics and chemistry playing out in front of our eyes, entirely in keeping with what we'd expect and yet another nail in coffin of climate change denial.'" :cxn-inventory *demo-grammar*)


(comprehend-and-extract-frames "Scientists have long predicted ocean deoxygenation due to climate change, but confirmation on this global scale, and at deep sea level, is concerning them." :cxn-inventory *demo-grammar*)

(comprehend-and-extract-frames "Last year, Matthew Long, an oceanographer at the National Center for Atmospheric Research in Colorado, predicted that oxygen loss would become evident 'across large regions of the oceans' between 2030 and 2040." :cxn-inventory *demo-grammar*)

(comprehend-and-extract-frames "Reacting to the German findings, Long said it was 'alarming to see this signal begin to emerge clearly in the observational data', while Roberts said, 'We now have a measurable change which is attributable to global warming.'" :cxn-inventory *demo-grammar*)



(comprehend-and-extract-frames "The report explains that the ocean's oxygen supply is threatened by global warming in two ways." :cxn-inventory *demo-grammar*)

(comprehend-and-extract-frames "Warmer water is less able to contain oxygen than cold, so as the oceans warm, oxygen is reduced." :cxn-inventory *demo-grammar*)

(comprehend-and-extract-frames  "Warmer water is also less dense, so the oxygen-rich surface layer cannot easily sink and circulate. " :cxn-inventory *demo-grammar*)




;;Data Armin

(comprehend-and-extract-frames "scientists warn climate change affecting greenland ice sheet more than previously thought" :cxn-inventory *propbank-learned-cxn-inventory*)
"young people are leading the way on climate change, and companies need to pay attention -"
(comprehend-and-extract-frames "young people are leading the way on climate change, and companies need to pay attention -" :cxn-inventory *propbank-learned-cxn-inventory*)