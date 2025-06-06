(ql:quickload :crs-conventionality)
(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;; All functionality for plotting dynamics of an experiment ;;
;;                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For experiments run locally (i.e. Naming Game)
;; Create plots for each experimental setting and for each measure (compare measures across experimental settings).

(let ((naming-game-alignment-strategies (list "robustness-conventional-original"
                                              "robustness-unconventional-original"))
      
      (naming-game-measures (list "communicative-success"
                                  "conventionalisation"
                                  "construction-inventory-size")))
  
  (loop for strategy in naming-game-alignment-strategies
        do (create-graph-for-single-strategy strategy
                                             naming-game-measures
                                             :average-windows '(100 100 1) ;; TODO adapt?
                                             :use-y-axis '(1 1 2)
                                             :y1-min 0 :y1-max 1
                                             :y2-min 0 :y2-max nil
                                             :x-label "Number of Games"
                                             :y1-label "Communicative Success / Conventionalisation"
                                             :y2-label "Construction Inventory Size"
                                             :captions '("communicative success"
                                                         "degree of conventionalisation"
                                                         "construction inventory size")
                                             :plot-file-name strategy
                                             :open t))


  (loop for measure in naming-game-measures 
        for window-size in '(100 100 1) 
        for y1-max in '(1 1 nil) 
        for label in '("communicative success" "degree of conventionalisation" "construction inventory size") 
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
                                            :open t)))

;; For batch experiments run on the cluster
;; - merge data for different seeds
;; - create plots for each experimental setting

(let ((exp-top-dir "cle-base-1")
      (datasplit "train")
      (cl-experiments (list "cle-replace-agents-0.5-alignment"
                            "cle-replace-agents-0.5-shift-no-update"))
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
                                    :average-windows '(100 100 1) 
                                    :use-y-axis '(1 1 2)
                                    :y1-min 0 :y1-max 1
                                    :y2-min 0 :y2-max nil
                                    :x-label "Number of Games"
                                    :y1-label "Communicative Success / Conventionalisation"
                                    :y2-label "Construction Inventory Size"
                                    :captions '("communicative success"
                                                "degree of conventionalisation"
                                                "construction inventory size")
                                    :plot-file-name exp-name
                                    :open t)))

;; - create plots for each measure (comparing experimental settings). data must be merged first

(let ((exp-top-dir "cle-learnability-3")
      (datasplit "train")
      (cl-experiments (list "cle-replace-agents-0.5-alignment"
                            "cle-replace-agents-0.5-shift-no-update"))
      (crs-monitors (list "communicative-success"
                          "conventionalisation"
                          "construction-inventory-size")))
  
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
                                                        :title monitor
                                                        :open t
                                                        :start 100001
                                                        :end 200000)))

          

  