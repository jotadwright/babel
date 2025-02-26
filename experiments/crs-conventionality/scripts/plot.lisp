(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;; All functionality for plotting dynamics of an experiment ;;
;;                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf naming-game-alignment-strategies
      (list "naming-game-lateral-inhibition"
            "naming-game-dont-punish-competitors"
            "naming-game-dont-punish-failure"
            "naming-game-never-punish"
            "naming-game-no-alignment"))

(setf naming-game-measures
      (list "communicative-success"
            "coherence-interacting-agents"
            "construction-inventory-size"))

(loop for strategy in naming-game-alignment-strategies
      do (create-graph-for-single-strategy strategy
                                           naming-game-measures
                                           :average-windows '(100 100 1) ;; TODO adapt?
                                           :use-y-axis '(1 1 2)
                                           :y1-min 0 :y1-max 1
                                           :y2-min 0 :y2-max nil
                                           :x-label "Number of Games"
                                           :y1-label "Communicative Success / Coherence"
                                           :y2-label "Construction Inventory Size"
                                           :captions '("communicative success"
                                                       "degree of coherence between interacting agents"
                                                       "construction inventory size")
                                           :plot-file-name strategy
                                           :open t))

(loop for measure in naming-game-measures 
      for window-size in '(100 100 1) 
      for y1-max in '(1 1 nil) 
      for label in '("communicative success" "degree of coherence between interacting agents" "construction inventory size") 
      do (create-graph-for-single-measure measure
                                          naming-game-alignment-strategies
                                          :average-windows window-size ;; TODO adapt?
                                          :y1-min 0 :y1-max y1-max
                                          :x-label "Number of Games"
                                          :y1-label label
                                          :captions '("Lateral inhibition"
                                                      "Don't punish competitors"
                                                      "Don't punish failure"
                                                      "Never punish"
                                                      "No alignment")
                                          :plot-file-name measure
                                          :open t))