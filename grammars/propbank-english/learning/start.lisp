;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                 ;;
;; Learning grammars from propbank-annotated data. ;;
;;                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,;;

;; Loading the :propbank-english sytem
;;(ql:quickload :propbank-english)
(in-package :propbank-english)

;; Loading the Propbank frames (takes a few seconds)
(load-pb-data :store-data t :ignore-stored-data nil)
;(length *pb-data*)

;; Loading the Propbank annotations (takes a minute)
(load-propbank-annotations :store-data nil :ignore-stored-data nil)
;(length (train-split *propbank-annotations*))

;; Activating spacy-api locally
(setf nlp-tools::*penelope-host* "http://localhost:5000")

;; Activating trace-fcg
(activate-monitor trace-fcg)

;;Create an empty cxn inventory
(def-fcg-constructions propbank-learned-english
  :fcg-configurations ((:de-render-mode .  :de-render-constituents-dependents-without-tokenisation) ;;:de-render-constituents-dependents-without-tokenisation
                       (:node-tests :check-double-role-assignment :restrict-nr-of-nodes)
                       (:parse-goal-tests :gold-standard-meaning) ;:no-valid-children
                       (:max-nr-of-nodes . 100)
                       (:parse-order multi-argument-with-lemma multi-argument-without-lemma single-argument-with-lemma single-argument-without-lemma)
                       (:node-expansion-mode . :multiple-cxns)
                       (:priority-mode . :nr-of-applied-cxns)
                       (:queue-mode . :greedy-best-first)
                       (:hash-mode . :hash-lemma)
                       (:cxn-supplier-mode . :hashed-scored-labeled)
                       (:equivalent-cxn-fn . fcg::equivalent-propbank-construction)
                       (:equivalent-cxn-key . identity)
                       (:learning-mode ;:multi-argument-with-lemma :multi-argument-without-lemma
                        :single-argument-with-lemma))
  :visualization-configurations ((:show-constructional-dependencies . nil))
  :hierarchy-features (constituents dependents)
  :feature-types ((constituents set)
                  (dependents set)
                  (span sequence)
                  (phrase-type set)
                  (word-order set-of-predicates)
                  (meaning set-of-predicates)
                  (footprints set))
  :cxn-inventory *propbank-learned-cxn-inventory*
  :hashed t)


;;;;;;;;;;;
;; Data  ;;
;;;;;;;;;;;

(defparameter *opinion-sentences* (shuffle (loop for roleset in '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
                                                 append (all-sentences-annotated-with-roleset roleset :split #'train-split))))

(defparameter *opinion-sentences-dev* (shuffle (loop for roleset in '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
                                                 append (all-sentences-annotated-with-roleset roleset :split #'dev-split))))

(defparameter *believe-sentences* (shuffle (loop for roleset in '("BELIEVE.01")
                                                 append (all-sentences-annotated-with-roleset roleset :split #'train-split))))

(defparameter *believe-sentences-dev* (shuffle (loop for roleset in '("BELIEVE.01")
                                                 append (all-sentences-annotated-with-roleset roleset :split #'dev-split))))

(defparameter *believe-sentence* (third (all-sentences-annotated-with-roleset "believe.01")))

(defparameter *difficult-sentence* (nth 6063 (train-split *propbank-annotations*))) ;;13 frames!



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Storing and restoring grammars ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-store:store *propbank-learned-cxn-inventory*
                (babel-pathname :directory '("grammars" "propbank-english" "learning")
                                :name "learned-grammar-single-argument-without-lemma-opinion"
                                :type "fcg"))

(defparameter *restored-grammar*
  (restore (babel-pathname :directory '("grammars" "propbank-english" "learning")
                           :name "learned-grammar-single-argument-with-three-first-strategies-opinion"
                           :type "fcg")))


;;;;;;;;;;;;;;
;; Training ;;
;;;;;;;;;;;;;;

(defparameter *training-configuration*
  '((:de-render-mode .  :de-render-constituents-dependents-without-tokenisation)
    (:node-tests :check-double-role-assignment :restrict-nr-of-nodes)
    (:parse-goal-tests :gold-standard-meaning) ;:no-valid-children
    (:max-nr-of-nodes . 100)
    (:node-expansion-mode . :multiple-cxns)
    (:priority-mode . :nr-of-applied-cxns)
    (:queue-mode . :greedy-best-first)
    (:hash-mode . :hash-lemma)
    (:parse-order
     multi-argument-with-lemma
     multi-argument-without-lemma
     single-argument-with-lemma
     single-argument-without-lemma)
    (:equivalent-cxn-fn . fcg::equivalent-propbank-construction)
    (:equivalent-cxn-key . identity)
    (:learning-modes
     :multi-argument-with-lemma
     :multi-argument-without-lemma
     :single-argument-with-lemma
     :single-argument-without-lemma)
    (:cxn-supplier-mode . :hashed-scored-labeled)))s


(learn-propbank-grammar *opinion-sentences*
                        :cxn-inventory '*propbank-learned-cxn-inventory*
                        :configuration *training-configuration*
                        :selected-rolesets '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
                        :silent t
                        :tokenize? nil)

(learn-propbank-grammar-no-comprehension *opinion-sentences*
                                         :cxn-inventory '*propbank-learned-cxn-inventory*
                                         :configuration *training-configuration*
                                         :selected-rolesets '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
                                         :silent t
                                         :tokenize? nil)


;;;;;;;;;;;;;;;;
;; Evaluation ;;
;;;;;;;;;;;;;;;;

(evaluate-propbank-sentences
 *opinion-sentences*
 *propbank-learned-cxn-inventory*
 :selected-rolesets  '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
 :silent t)


;;;;;;;;;;;;;
;; Testing ;;
;;;;;;;;;;;;;


(setf *selected-sentence*
      (find "Investors here still expect Ford Motor Co. or General Motors Corp. to bid for Jaguar ." *opinion-sentences-dev* :key #'sentence-string :test #'string=))

(learn-cxn-from-propbank-annotation *selected-sentence* "expect.01" *propbank-learned-cxn-inventory* :single-argument-with-lemma)

(learn-propbank-grammar (list *selected-sentence*)
                        :cxn-inventory '*propbank-learned-cxn-inventory*
                        :configuration *training-configuration*
                        :selected-rolesets '("expect.01")
                        :silent t
                        :tokenize? nil)

(with-activated-monitor trace-fcg
  (comprehend-and-extract-frames *selected-sentence* :cxn-inventory *restored-grammar* :selected-rolesets '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")))

(evaluate-propbank-sentences
 (list *selected-sentence* *propbank-learned-cxn-inventory* :selected-rolesets  '("believe.01")  :silent t)