(ql:quickload :crs-conventionality)
(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;; All functionality for plotting dynamics of an experiment ;;
;;                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For experiments run locally (i.e. Naming Game)
;; Create plots for each experimental setting and for each measure (compare measures across experimental settings).

(let ((naming-game-strategies (list "naming-game-alignment-noise-0.25"
                                    "naming-game-alignment-noise-0.5"
                                    "naming-game-alignment-noise-0.75"
                                    "naming-game-no-alignment-noise-0.25"
                                    "naming-game-no-alignment-noise-0.5"
                                    "naming-game-no-alignment-noise-0.75"))
      
      (monitors (list
                 "communicative-success"
                 "conventionalisation"
                 "construction-inventory-size")))
  
  (loop for strategy in naming-game-strategies
        for filename in '("ng-noise-0.25-alignment"
                          "ng-noise-0.50-alignment"
                          "ng-noise-0.75-alignment"
                          "ng-noise-0.25-no-alignment"
                          "ng-noise-0.50-no-alignment"
                          "ng-noise-0.75-no-alignment")
        do (create-graph-for-single-strategy strategy
                                             monitors
                                             :average-windows '(100 100 1)
                                             :use-y-axis '(1 1 2)
                                             :y1-min 0 :y1-max 1
                                             :y2-min 0 :y2-max 500
                                             :x-label "Number of Games"
                                             :y1-label "Comm. Success / Conventionalisation"
                                             :y2-label "Vocabulary Size"
                                             :captions '("communicative success"
                                                         "degree of conventionalisation"
                                                         "vocabulary size")
                                             :plot-file-name filename
                                             :fsize 16
                                             :show-legend nil
                                             :show-labels t
                                             :open t)))


(let ((naming-game-strategies (list "naming-game-alignment-noise-0.50"
                                    "naming-game-no-alignment-noise-0.50"
                                    "naming-game-no-alignment-noise-0.75"))
      
      (monitors (list "communicative-success"
                      "conventionalisation"
                      "construction-inventory-size")))
  
  (create-graph-for-multiple-strategies naming-game-strategies
                                        monitors
                                        :average-windows '(100 100 100 100 100 100 1 1 1) ;; TODO adapt?
                                        :use-y-axis '(1 1 1 1 1 1 2 2 2)
                                        :y1-min 0 :y1-max 1
                                        :y2-min 0 :y2-max nil
                                        :x-label "Number of Games"
                                        :y1-label "Communicative Success / Conventionalisation"
                                        :y2-label "Construction Inventory Size"
                                        :captions '("communicative success"
                                                    "degree of conventionalisation"
                                                    "construction inventory size")
                                        :plot-file-name "ng-no-alignment-noise"
                                        :
                                        :open t))


  (loop for measure in monitors 
        for window-size in '(100 100 1) 
        for y1-max in '(1 1 nil) 
        for label in '("communicative success" "degree of conventionalisation" "construction inventory size") 
        do (create-graph-for-single-measure measure
                                            naming-game-strategies
                                            :average-windows window-size ;; TODO adapt?
                                            :y1-min 0 :y1-max y1-max
                                            :x-label "Number of Games"
                                            :y1-label label
                                            :captions '("Lateral inhibition"
                                                      "No alignment")
                                            :plot-file-name measure
                                            :open t)))

;; For batch experiments run on the cluster
;; - merge data for different seeds
;; - create plots for each experimental setting

(let ((exp-top-dir "cle-cognitive-economy-4")
      (datasplit "train")
      (cl-experiments (list "cle-cognitive-economy-alignment"
                            "cle-cognitive-economy-shift-no-update"))
      (crs-monitors (list "communicative-success"
                          "conventionalisation"
                          "construction-inventory-size")))
  
  (loop for exp-name in cl-experiments
          
        do (merge-batch crs-monitors
                        exp-top-dir
                        datasplit
                        exp-name)
          
        do (create-graph-for-batch  crs-monitors
                                    exp-top-dir
                                    datasplit
                                    exp-name
                                    :average-windows '(1000 1000 1) 
                                    :use-y-axis '(1 1 2)
                                    :y1-min 0 :y1-max 1
                                    :y2-min 0 :y2-max 100
                                    :x-label "Number of Games"
                                    :y1-label "Communicative Success / Conventionalisation"
                                    :y2-label "Construction Inventory Size"
                                    :captions '("communicative success"
                                                "degree of conventionalisation"
                                                "construction inventory size")
                                    :plot-file-name exp-name
                                    :open t)))
  (loop for monitor in crs-monitors
        for window-size in '(100 100 1) 
        for y1-max in '(1 1 nil) 
        for label in '("communicative success" "degree of conventionalisation" "construction inventory size")
        do (create-graph-for-single-measure-from-batch  monitor
                                                        exp-top-dir
                                                        datasplit
                                                        cl-experiments
                                                        :average-windows window-size 
                                                        :y1-min 0 :y1-max y1-max
                                                        :x-label "Number of Games"
                                                        :y1-label label
                                                        :captions cl-experiments
                                                        :plot-file-name monitor
                                                        :open t)))

;; - create plots for each measure (comparing experimental settings). data must be merged first

(let ((exp-top-dir "cle-learnability-introduce-3")
      (datasplit "train")
      (cl-experiments (list "cle-introduce-agents-1-alignment"
                            "cle-introduce-agents-1-shift-no-update"))
      (crs-monitors (list "communicative-success-of-new-agents"
                          "conventionalisation-of-new-agents"
                          "construction-inventory-size-of-new-agents")))
  
  (loop for monitor in crs-monitors
        for window-size in '(100 100 1) 
        for y1-max in '(1 1 nil) 
        for label in '("communicative success" "degree of conventionalisation" "construction inventory size")
        do (create-graph-for-single-measure-from-batch  monitor
                                                        exp-top-dir
                                                        datasplit
                                                        cl-experiments
                                                        :average-windows window-size 
                                                        :y1-min 0 :y1-max y1-max
                                                        :x-label "Number of Games"
                                                        :y1-label label
                                                        :captions cl-experiments
                                                        :plot-file-name monitor
                                                        :open t
                                                        :start 100001
                                                        :end 120000)))

          

  