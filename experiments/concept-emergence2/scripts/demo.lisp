(ql:quickload :cle)

(in-package :cle)

(defun run-and-show-interactions (experiment interactions-count)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interaction-in-web-interface)
  (activate-monitor display-communicative-success)
  (loop for i from 1 to interactions-count
        do (run-interaction experiment))
  (run-interaction experiment))

(defun run-then-show-last (experiment interactions-count &key (show nil) (display nil))
  (when display
    (deactivate-all-monitors)
    (activate-monitor display-communicative-success)
    (activate-monitor print-a-dot-for-each-interaction)
    )
  (loop for i from 1 to (- interactions-count 1)
        do (run-interaction experiment))
  (when show
    (activate-monitor trace-interaction-in-web-interface)
    (run-interaction experiment)))

(progn
  (setf *scene-ids* (read-scene-ids "area.lisp"))
  (setf *subset-size* (length *scene-ids*)) ;; (length *scene-ids*)
  (defparameter *baseline-simulated*
    (make-configuration
     :entries `(;; monitoring
                (:dot-interval . 1000)
                ;; setup interacting agents
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
                ;; general strategy (:standard or :times)
                (:strategy . :times)

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
                (:initial-weight . 1/2)
                (:weight-incf . 1/10)
                (:weight-decf . -1/10)
                
                ;; conceptualisation
                (:concept-similarity-activation . 0.4)
                
                ;; alignment strategy
                (:punish-strategy . :punish-found-concepts)
                )))
  (setf *experiment* (make-instance 'cle-experiment :configuration *baseline-simulated*))
  (notify reset-monitors)
  (wi::reset))


;; 1. run then show last

(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (run-interaction *experiment*))

(loop for i from 1 to 50
      do (run-interaction *experiment*))


(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor display-communicative-success)
  ;(activate-monitor trace-interaction-in-web-interface)
  (loop for i from 1 to 10000
      do (run-interaction *experiment*)))


(run-then-show-last *experiment* (* *subset-size* 100) :show t :display t)

(run-then-show-last *experiment* 1 :show t :display t)


