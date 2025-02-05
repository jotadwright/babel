(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;; All functionality for plotting dynamics of an experiment ;;
;;                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-graph-for-single-strategy "Naming game"
                                  '("communicative-success" "conventionalisation" "construction-inventory-size")
                                  :average-windows '(100 100 1) ;; TODO adapt?
                                  :use-y-axis '(1 1 2)
                                  :y1-min 0 :y1-max 1
                                  :y2-min 0 :y2-max nil
                                  :x-label "Number of Games"
                                  :y1-label "Communicative Success / Conventionalisation"
                                  :y2-label "Construction Inventory Size"
                                  :captions '("communicative success" "degree of conventionalisation" "construction inventory size")
                                  :open t)