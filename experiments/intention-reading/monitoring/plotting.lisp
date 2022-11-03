(in-package :intention-reading)

(defun create-all-graphs-for-single-experiment (experiment-name)
  ;; This function creates all graphs that are relevant
  ;; for a single experiment

  ;; plot both success and grammar size
  (create-graph-for-single-strategy
   :experiment-name (downcase (mkstr experiment-name))
   :measure-names '("communicative-success"
                    "lexicon-size")
   :average-windows '(100 1)
   :y-axis '(1 2) :y1-max 1
   :xlabel "Number of Games"
   :y1-label "Communicative Success"
   :y2-label "Grammar Size"
   :captions '("communicative success"
               "grammar size")
   :open nil)

  ;; plot competition graphs
  #|
  (create-graph-for-single-strategy
   :experiment-name (downcase (mkstr experiment-name))
   :measure-names '("lexical-meanings-per-form"
                    "lexical-forms-per-meaning")
   :average-windows '(1000 1000)
   :y-axis '(1 1) :y1-max nil
   :xlabel "Number of Games"
   :y1-label "Number of Competitors"
   :captions '("meanings per form"
               "forms per meaning")
   :open nil)

  ;; plot avg cxn score (overall)
  (create-graph-for-single-strategy
   :experiment-name (downcase (mkstr experiment-name))
   :measure-names '("avg-cxn-score")
   :average-windows 1000
   :y-axis '(1) :y1-max 1
   :xlabel "Number of Games"
   :y1-label "Score of cxn"
   :captions '("score")
   :open nil)
  |#

  ;; composer search space
  (create-graph-for-single-strategy
   :experiment-name (downcase (mkstr experiment-name))
   :measure-names '("composer-search-space")
   :y-axis '(1) :y1-max nil
   :xlabel "Number of Games"
   :y1-label "Size of the Composer Search Space"
   :captions '("composer-search")
   :points t :open nil)

  ;; plot number of nodes and edges in the th
  (create-graph-for-single-strategy
   :experiment-name (downcase (mkstr experiment-name))
   :measure-names '("number-of-th-nodes"
                    "number-of-th-edges")
   :average-windows '(1 1)
   :y-axis '(1 2) :y1-max nil :y2-max nil
   :xlabel "Number of Games"
   :y1-label "Number of Nodes"
   :y2-label "Number of Edges"
   :captions '("nodes" "edges")
   :open nil)

  ;; grammar size per type
  (create-graph-for-single-strategy
   :experiment-name (downcase (mkstr experiment-name))
   :measure-names '("number-of-holophrase-cxns"
                    "number-of-item-based-cxns"
                    "number-of-lexical-cxns")
   :average-windows '(1 1 1)
   :y-axis '(1 1 1) :y1-max nil
   :xlabel "Number of Games"
   :y1-label "Number of Constructions"
   :captions '("holophrase" "item-based" "lexical")
   :open nil)

  ;; avg cxn score per type
  (create-graph-for-single-strategy
   :experiment-name (downcase (mkstr experiment-name))
   :measure-names '("avg-holophrase-cxn-score"
                    "avg-item-based-cxn-score"
                    "avg-lexical-cxn-score")
   :average-windows '(100 100 100)
   :y-axis '(1 1 1) :y1-max 1.0
   :xlabel "Number of Games"
   :y1-label "Score of cxn"
   :captions '("holophrase" "item-based" "lexical")
   :open nil)

  ;; number of slots
  (create-graph-for-single-strategy
   :experiment-name (downcase (mkstr experiment-name))
   :measure-names '("num-item-based-1"
                    "num-item-based-2"
                    "num-item-based-3"
                    "num-item-based-4"
                    "num-item-based-5")
   :average-windows '(1 1 1 1 1)
   :y-axis '(1 1 1 1 1) :y1-max nil
   :xlabel "Number of Games"
   :y1-label "Number of item-based Constructions"
   :captions '("1 slot" "2 slots" "3 slots" "4 slots" "5 slots")
   :open nil)

  ;; applied repairs
  (create-graph-for-single-strategy
   :experiment-name (downcase (mkstr experiment-name))
   :measure-names '("repair-add-holophrase"
                    "repair-holophrase-to-item-based"
                    "repair-add-th-links"
                    "repair-lexical-to-item-based"
                    "repair-item-based-to-lexical")
   :average-windows '(100 100 100 100 100)
   :y-axis '(1 1 1 1 1) :y1-max 1
   :xlabel "Number of Games"
   :y1-label "Applied Repair Strategies"
   :captions '("add holophrase" "holophrase to item-based"
               "add links" "lexical to item-based"
               "item-based to lexical")
   :open nil)
  )

(defun create-all-graphs-comparing-experiment (experiment-names
                                               captions)
  ;; compare communicative success
  (create-graph-comparing-strategies
   :experiment-names (mapcar (compose #'downcase #'mkstr) experiment-names)
   :measure-name "communicative-success"
   :y-min 0 :y-max 1 :y1-label "Communicative success"
   :average-windows 100 :end 25000
   :captions captions
   :open nil)

  ;; compare grammar size
  (create-graph-comparing-strategies
   :experiment-names (mapcar (compose #'downcase #'mkstr) experiment-names)
   :measure-name "lexicon-size"
   :y-min 0 :y-max nil :y1-label "Grammar Size"
   :average-windows 1
   :captions captions
   :open nil)

  ;; number of holophrase cxns
  (create-graph-comparing-strategies
   :experiment-names (mapcar (compose #'downcase #'mkstr) experiment-names)
   :measure-name "number-of-holophrase-cxns"
   :y-min 0 :y-max nil :y1-label "Number of Holophrase Constructions"
   :average-windows 1
   :captions captions
   :open nil)

  ;; number of lexical cxns
  (create-graph-comparing-strategies
   :experiment-names (mapcar (compose #'downcase #'mkstr) experiment-names)
   :measure-name "number-of-lexical-cxns"
   :y-min 0 :y-max nil :y1-label "Number of Lexical Constructions"
   :average-windows 1
   :captions captions
   :open nil)

  ;; number of item-based cxns
  (create-graph-comparing-strategies
   :experiment-names (mapcar (compose #'downcase #'mkstr) experiment-names)
   :measure-name "number-of-item-based-cxns"
   :y-min 0 :y-max nil :y1-label "Number of Item-Based Constructions"
   :average-windows 1
   :captions captions
   :open nil)
  
  )