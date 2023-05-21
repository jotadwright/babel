(ql:quickload :cle)

(in-package :cle)

;; ---------------------------
;; + Web monitor experiments +
;; ---------------------------
(defun run-then-show-last (experiment interactions-count &key (show nil) (display nil))
  (when display
    (deactivate-all-monitors)
    ;(activate-monitor display-communicative-success)
    (activate-monitor print-a-dot-for-each-interaction)
    ;(activate-monitor export-attribute-type)
    )
  (loop for i from 1 to (- interactions-count 1)
        do (run-interaction experiment))
  (when show
    ;(activate-monitor trace-interaction-in-web-interface)
    (run-interaction experiment)))

(defun run-and-show-interactions (experiment interactions-count)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interaction-in-web-interface)
  (activate-monitor display-communicative-success)
  (loop for i from 1 to interactions-count
        do (run-interaction experiment))
  (run-interaction experiment))

(defun read-scene-ids (fname)
  (let* ((base-dir "~/Projects/babel/experiments/concept-emergence2/data/")
         (fpath (concatenate 'string base-dir fname))
         (raw (uiop:read-file-lines fpath))
         (scene-ids (map 'list #'parse-integer raw)))
    scene-ids))

(defun first-n (n list)
  "Returns the first N elements of the LIST."
  (butlast list (max (- (list-length list) n) 0)))

(progn
  (setf *scene-ids* (read-scene-ids "area.lisp"))
  (setf *subset-size* 100) ;; (length *scene-ids*)
  (defparameter *baseline-simulated*
    (make-configuration
     :entries `(;; setup interacting agents
                (:interacting-agents-strategy . :random)
                (:population-size . 10)
                ;; setup scene
                (:scene-sampling . :deterministic)
                (:topic-sampling . :english-concepts)
                (:clevr-channels
                 ,'area ;; size
                 )
                (:scene-ids . ,(first-n *subset-size* *scene-ids*))
                (:current-scene-idx . 0)

                 ;; entrenchment of constructions
                (:initial-cxn-entrenchement . 1/2)
                (:entrenchment-incf . 1/10)
                (:entrenchment-decf . -1/10)
                (:entrenchment-li . -1/10) ;; lateral inhibition
                
                ;; concept representations
                (:concept-representation . :distribution)
                (:distribution . :gaussian-welford)
                (:M2 . 0.0001)

                ;; prototype weight inits
                (:weight-update-strategy . :standard)
                (:initial-weight . 0)
                (:weight-incf . 1/10)
                (:weight-decf . -1/10)
                
                ;; conceptualisation
                (:concept-similarity-activation . 0.9)
                
                ;; alignment strategy
                (:punish-strategy . :punish-found-concepts)
                )))
  (defparameter *experiment* (make-instance 'cle-experiment :configuration *baseline-simulated*))
  (wi::reset))

(progn
  (wi::reset)
  ;;(activate-monitor trace-interaction-in-web-interface)
  (activate-monitor print-a-dot-for-each-interaction)
  ;(activate-monitor display-communicative-success)
  (deactivate-all-monitors))

(run-interaction *experiment*)

;; 1. run then show last
(run-then-show-last *experiment* (* *subset-size* 800) :show t :display t)

`((:determine-interacting-agents-mode :baseline)
                (:experiment-type . :baseline)
                (:reset-usage-count . 1000)
                (:dot-interval . 1000)
                ;; world config
                (:world-type . :simulated)
                (:normalise-channels . t)
                (:data-distribution . :gaussian)
                (:clevr-channels
                 ;,'xpos ,'ypos ,'zpos ;; position
                 ,'area ;; size
                 ;,'wh-ratio ;; shape
                 ;,'sides-and-corners ;; shape
                 ;,'color ;; color
                 ;,'roughness ;; material
                 ;,'xpos-3d ,'ypos-3d ,'zpos-3d ;; 3d-position
                 )
                (:population-size . 10)
                (:alignment-filter . :all) ; :none - :at-least-one - :all
                (:scene-sampling . :deterministic) ; deterministic, random-scene, :context-size
                (:import-data . t)
                ;(:context-size . 3)
                (:scene-ids . ,(first-n *subset-size* *scene-ids*))
                (:current-scene-idx . 0)
                (:topic-sampling . :english-concepts) ; english-concepts, deterministic, random-topic
                ;(:topic-ids . '(1 2 3))
                ;(:current-topic-idx . 0)
                 ;; concept search
                (:conceptualisation-strategy . :standard) ; standard, times !!!!
                (:concept-selection-strategy . :weighted-average) ; waterfall, weighted-average, times !!!
                (:concept-selection-weights
                 (:entrenchment-weight . 0)
                 (:topic-similarity-weight . 0)
                 (:similarity-distance-weight . 1))
                ;; concept similarity
                (:concept-similarity-activation . 0.99)
                (:M2 . 0.0001) ;; initial distribution ;; 0.0001 -> 0.01
                ;; certainty
                (:certainty-strategy . :fixed) ; fixed, saliency
                (:initial-certainty . 1/2)
                (:certainty-update-strategy . :standard) ; standard,  interpolation, j-interpolation
                (:certainty-incf . 1/100)
                (:certainty-decf . -1/100)
                ;; entrenchment
                (:initial-entrenchment . 1/2)
                (:entrenchment-incf . 1/10)
                (:entrenchment-decf . -1/10)
                (:punish-decf . -1/10) ;; punish entrenchment of duplicates
                ;; alignment and competitors
                (:hearer-alignment . :shift-when-successful)
                (:punish-strategy . :punish-found-concepts) ;; :punish-found-concepts, :punish-found-concepts-and-shift [DOES NOT WORK]
                (:competitor-strategy . :punish-duplicates) ;; :punish-duplicates, times !!!
                (:competitor-strategy-hearer . :punish-duplicates) ;; punish-duplicates, times !!!
                (:forget-concepts . nil)
                (:prototype-update-strategy . :welford) ; welford, replay-buffer
                (:replay-buffer-size . 100)
                )