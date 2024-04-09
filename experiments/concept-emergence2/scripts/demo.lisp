(ql:quickload :cle)

(in-package :cle)

;; experiments with entrenchment values - keep the same
(progn
  (defparameter *baseline-simulated*
    (make-configuration
     :entries `(
                ;; monitoring
                (:dot-interval . 10)
                (:usage-table-window . 100)
                (:save-distribution-history . nil)
                ;; setup interacting agents
                (:interacting-agents-strategy . :standard)
                (:population-size . 10)
                ;; setup data scene
                (:dataset . "gqaglove50")
                (:dataset-split . "train")
                ;(:data-fname . "all.lisp")
                (:available-channels ,@(get-all-channels :gqaglove50))
                ;; disable channels
                (:disable-channels . :none)
                (:amount-disabled-channels . 0)
                ;; noised channels
                (:sensor-noise . :none)
                (:sensor-std . 0.0)
                (:observation-noise . :none)
                (:observation-std . 0.0)
                ;; scene sampling
                (:scene-sampling . :random)
                (:topic-sampling . :discriminative)
                ;; general strategy
                (:align . t)
                (:similarity-threshold . 0.0)
                ;; entrenchment of constructions
                (:initial-cxn-entrenchement . 0.5)
                (:entrenchment-incf . 0.1)
                (:entrenchment-decf . -0.1)
                (:entrenchment-li . -0.02) ;; lateral inhibition
                ;; new options
                (:trash-threshold . 0.0)
                (:slow-threshold . -0.1)
                (:conceptualisation-heuristics . :heuristic-1)
                (:speaker-competitors . nil)
                (:hearer-competitors . nil)
                ;; concept representations
                (:concept-representation . :distribution)
                (:distribution . :gaussian-welford)
                (:M2 . 0.0001) ;; only for gaussian-welford
                ;; prototype weight inits
                (:weight-update-strategy . :j-interpolation)
                (:initial-weight . 0)
                (:weight-incf . 1)
                (:weight-decf . -5)
                ;; staging
                (:switch-condition . :none) ; :after-n-interactions)
                (:switch-conditions-after-n-interactions . 2500) 
                (:stage-parameters nil)
                ;; saving
                (:experiment-name . "test")
                (:output-dir . "test")
                )))
  (setf *experiment* (make-instance 'cle-experiment :configuration *baseline-simulated*))
  (notify reset-monitors)
  (wi::reset))

(progn
  (wi::reset)
  (notify reset-monitors)
  (deactivate-all-monitors)
  (activate-monitor export-communicative-success)
  (activate-monitor export-lexicon-coherence)
  ;(activate-monitor export-lexicon-size-fast)
  ;(activate-monitor export-lexicon-size-slow)
  ;(activate-monitor export-lexicon-size-trash)
    ;(activate-monitor export-unique-form-usage)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor record-time)
  (format t "~%---------- NEW GAME ----------~%")
  (time
   (loop for i from 1 to 5000
         do (run-interaction *experiment*))))

(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (loop for idx from 1 to 1
        do (run-interaction *experiment*)))






;;;;;;;


(setf lexor1 (lexicon (first (agents *experiment*))))
(setf lexor2 (lexicon (first (agents *experiment2*))))
(setf lexor2 (lexicon (first (agents *experiment3*))))


(setf inventory (loop for cxn being the hash-values of (fast-inventory (lexicon (first (agents *experiment3*))))
                      collect cxn))

(setf sorted-inventory (sort inventory #'(lambda (x y) (> (score x) (score y)))))

(agents *experiment3*)

(loop for experiment in (list *experiment* *experiment1* *experiment2* *experiment3*)
      do (loop for agent in (agents experiment)
               for lexicon = (lexicon agent)
               sum (hash-table-count (fast-inventory lexicon)) into fasts
               sum (hash-table-count (slow-inventory lexicon)) into slows
               sum (hash-table-count (trash-inventory lexicon)) into trashes
               finally (format t "~% ~a / ~a /~a "
                               (float (/ fasts (length (agents experiment))))
                               (float (/ slows (length (agents experiment))))
                               (float (/ trashes (length (agents experiment)))))))

(display-lexicon (second (agents *experiment*)) :entrenchment-threshold 0.1 :certainty-threshold 0.1 :sort t)

(similar-concepts
 (second (agents *experiment*))
 (meaning (find-form-in-lexicon (lexicon (second (agents *experiment*))) "woxuzu"))
 (meaning (find-form-in-lexicon (lexicon (second (agents *experiment*))) "xogiwo") ))



(initialise-world *experiment*)















