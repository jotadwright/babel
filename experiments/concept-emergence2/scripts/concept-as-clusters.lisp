(ql:quickload :cle)

(in-package :cle)

(defun train-experiment (n-clusters)
  (progn 
    (defparameter *baseline-simulated*
      (make-configuration
       :entries `(
                ;; monitoring
                  (:dot-interval . 1000)
                  (:usage-table-window . 100)
                  (:save-distribution-history . nil)
                  ;; setup interacting agents
                  (:interacting-agents-strategy . :standard)
                  (:population-size . 10)
                  ;; setup data scene
                  (:dataset . "clevr")
                  (:dataset-split . "train")
                  ;(:data-fname . "all.lisp")
                  (:feature-set . "clevr")
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
                  (:topic-sampling . :random)
                  ;; general strategy
                  (:align . t)
                  (:similarity-threshold . 0.0)
                  ;; entrenchment of constructions
                  (:initial-cxn-entrenchement . 0.5)
                  (:entrenchment-incf . 0.1)
                  (:entrenchment-decf . -0.1)
                  (:entrenchment-li . -0.02) ;; lateral inhibition
                  (:trash-concepts . t)
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
                  (:n-clusters . 10)


                  (:cluster-update-entrenchment . nil)
                  (:cluster-update-distribution . nil)
                  (:cluster-update-weights . nil)
                  (:cluster-new-cxns . nil)
                  ;; saving
                  (:exp-top-dir . "a")
                  (:exp-name . "b")
                  (:log-dir-name . "c")
                  (:prototype-distance . :paper)
                  )))
    (setf *experiment* (make-instance 'cle-experiment :configuration *baseline-simulated*))
    (notify reset-monitors)
    (wi::reset))
  

  ;(setf *fpath* (format nil "/Users/jerome/Projects/^dataset-generation/tabular-dataset-gen/notebooks/out/clevr-~a/train/" n-clusters))
  ;(setup-experiment-with-cacs *experiment* *fpath*)


  (wi::reset)
  (notify reset-monitors)
  (deactivate-all-monitors)
  (activate-monitor export-communicative-success)
  (activate-monitor export-lexicon-coherence)
  (activate-monitor export-communicative-success-given-conceptualisation)
  ;; (activate-monitor export-unique-form-usage)
  (activate-monitor print-a-dot-for-each-interaction)
  (format t "~%---------- NEW GAME ----------~%")
  (time
   (loop for i from 1 to 100000
         do (run-interaction *experiment*)))

  ;(store-experiment-clusters *experiment* n-clusters)
  )

(loop for n-clusters in (list 30)
      do (format t "~% -> n-clusters: ~a" n-clusters)
      do (train-experiment n-clusters))

(display-lexicon (first (agents *experiment*)) :sort t :weight-threshold 0.01)



; _012717.png -> 4 objects, topic is angle 0.14


#|
(progn
  (wi::reset)
  (notify reset-monitors)
  (deactivate-all-monitors)
  (activate-monitor export-communicative-success)
  (activate-monitor export-lexicon-coherence)
  (activate-monitor export-communicative-success-given-conceptualisation)
  ;; (activate-monitor export-unique-form-usage)
  (activate-monitor print-a-dot-for-each-interaction)
  (format t "~%---------- NEW GAME ----------~%")
  (time
   (loop for i from 1 to 1000000
         do (run-interaction *experiment*))))

(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (loop for idx from 1 to 1
        do (run-interaction *experiment*)))
                            ;:scene 12717)))


(run-interaction *experiment*
                 :scene (index (find-data (car (interacting-agents *experiment*)) 'context))
                 :topic (find-data (car (interacting-agents *experiment*)) 'topic)
                 :agents (interacting-agents *experiment*))

(display-lexicon (first (agents *experiment*)) :sort t)


(setf ag1 (loop for ag in (agents *experiment*)
                collect (lexicon ag)))
|#
