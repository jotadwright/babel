
;; (ql:quickload :origins-of-syntax)

(in-package :origins-of-syntax)

;; Ensure that all monitors are deactivated
(deactivate-all-monitors)

;; Activate Export Monitors
(progn
  (activate-monitor print-a-dot-for-each-interaction+numbers)
  (activate-monitor export-categorial-coherence)
  (activate-monitor export-communicative-success)
  (activate-monitor export-nr-of-constructions)
  (activate-monitor export-nr-of-constructions+communicative-success)
  (activate-monitor export-coherence)
  (activate-monitor export-fcg-search)
  (activate-monitor plot-marker-evolution-in-population))
  ;(activate-monitor export-fcg-search))

;; Activate Display Monitors
(progn
  (activate-monitor display-communicative-success)
  (activate-monitor display-number-of-constructions)
  (activate-monitor print-a-dot-for-each-interaction+numbers)
  (activate-monitor display-marker-evolution-in-population))
  ;(activate-monitor display-fcg-search))



;;##############################################################
;; Word order
;;##############################################################

(setf *experiment*
      (make-instance 'syntax-experiment
                     :configuration '((:population-size . 10)
                                      (:strategy .  :categorisation-strategy)
                                      (:alignment . :type-hierarchy)
                                      (:li-incf-weight . 0.2)
                                      (:li-decf-weight . 0.1)
                                      (:trace-every-nth-interaction . 50)
                                      (:min-nr-of-objects-in-scene . 2)
                                      (:max-nr-of-objects-in-scene . 64)
                                      (:min-nr-of-objects-in-topic . 1)
                                      (:max-nr-of-objects-in-topic . 2)
                                      (:ontology . ((:shape ((square ."vierkant")
                                                             (circle . "cirkel")
                                                             (triangle . "driehoek")
                                                             (rectangle . "rechthoek")
                                                             ))
                                                    (:color ((yellow . "geel")
                                                             (red . "rood")
                                                             (blue . "blauw")
                                                             (green . "groen")
                                                             ))
                                                    (:size ((small . "klein")
                                                            (large . "groot")
                                                            (tiny . "minuscuul")
                                                            (huge . "reusachtig"))))))))


(run-interaction *experiment*)
(clear-page)
(run-series *experiment* 400)

(run-series *experiment* 500)
(set-configuration *experiment* :trace-every-nth-interaction 1)


(setf *th* (get-type-hierarchy (grammar (first (population *experiment*)))))
(add-element (make-html *th*
                                                  :weights? t
                                                  :colored-edges-0-1 t
                                                  :render-program "circo"))

(cip-goal-test *saved-cipn* :no-strings-in-root)

(mapcar #'(lambda (agent) (add-element (make-html (get-type-hierarchy  (grammar agent))
                                                  :weights? t
                                                  :colored-edges-0-1 t
                                                  :render-program "circo"))) (population *experiment*))


(add-element (make-html (world *experiment*)))
;; running 1 or more interactions
(run-interaction *experiment*)

(create-static-html-page "N-gram Strategy"
  (run-series *experiment* 10000))

(fcg-search *saved-cipn*)

(eq (priority *p*) (priority *saved-cipn*))

;; Inspecting the type hierarchies of the agents
(mapcar #'(lambda (agent)
            (add-element (make-html (get-type-hierarchy (grammar agent))
                                    :weights? t
                                    :colored-edges-0-1 t
                                    :render-program "circo")))
        (population *experiment*))

;;;;;;;;;;;;;;;;;;;;;
;; Running a batch ;;
;;;;;;;;;;;;;;;;;;;;;
                                
(run-batch 'syntax-experiment 100 1
            :configuration '((:population-size . 10)
                             (:strategy .  :categorisation-strategy)
                             (:alignment . :type-hierarchy)
                                      (:li-incf-weight . 0.2)
                                      (:li-decf-weight . 0.1)
                                      (:trace-every-nth-interaction . 100)
                                      (:min-nr-of-objects-in-scene . 1)
                                      (:max-nr-of-objects-in-scene . 64)
                                      (:min-nr-of-objects-in-topic . 1)
                                      (:max-nr-of-objects-in-topic . 2)
                                      (:ontology . ((:shape ((square ."vierkant")
                                                             (circle . "cirkel")
                                                             (triangle . "driehoek")
                                                             (rectangle . "rechthoek")
                                                             ))
                                                    (:color ((yellow . "geel")
                                                             (red . "rood")
                                                             (blue . "blauw")
                                                             (green . "groen")
                                                             ))
                                                    (:size ((small . "klein")
                                                            (large . "groot")
                                                            (tiny . "minuscuul")
                                                            (huge . "reusachtig")))))))

;; Plotting
(raw-files->evo-plot
 :raw-file-paths '(("experiments" "origins-of-syntax" "raw-data" "coherence")
		   )
  :average-windows 100
  :plot-file-name "xyz" 
  :plot-directory '("experiments" "origins-of-syntax" "graphs")
  :captions '("Coherence")
  :y1-min 0
  :y1-max 1
  :x-label "# Games"
  :y1-label "Coherence Success"
  :use-y-axis '(1)
  :error-bars '(:percentiles (25 75))
  :error-bar-modes '(:filled)
  :open t
  )

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "pattern-strategy" "fcg-search"))
 :average-windows 1000
 :plot-file-name "search"
 :plot-directory '("experiments" "origins-of-syntax" "graphs")
 :captions '("Search")
 :title "Pattern Strategy"
 :x-label "# Games per Agent"
 :y1-label "Search"
 :y1-min 1
 :y1-max 3
 :use-y-axis '(1)
 :error-bars '(:percentiles (25 75))
 :error-bar-modes '(:filled)
 :open t
 :points nil
 :fsize 16
 :key-location "bmargin"
 :divide-indices-by 5
 :colors '("blue")
 )

(fully-expanded? *saved-cipn*)

(left-pole-structure (car-resulting-cfs (cipn-car (second (multiple-value-list (formulate (get-data speaker :meaning) :cxn-inventory (grammar hearer)))))))


;;##############################################################
;; Markers
;;##############################################################

(setf *experiment*
      (make-instance 'syntax-experiment
                     :configuration '((:population-size . 5)
                                      (:strategy .  :categorisation-strategy)
                                      (:alignment . :type-hierarchy)
                                      (:li-incf-weight . 0.2)
                                      (:li-decf-weight . 0.1)
                                      (:trace-every-nth-interaction . 10)
                                      (:min-nr-of-objects-in-scene . 1)
                                      (:max-nr-of-objects-in-scene . 64)
                                      (:min-nr-of-objects-in-topic . 1)
                                      (:max-nr-of-objects-in-topic . 2)
                                      (:redundant-dimensions . (((:texture . :shape)
                                                                ((fluffy . square)
                                                                 (fluffy . rectangle)
                                                                 (rough . circle)
                                                                 (rough . triangle)
                                                                 ))))
                                      (:ontology . ((:shape ((square ."vierkant")
                                                             (circle . "cirkel")
                                                             (triangle . "driehoek")
                                                             (rectangle . "rechthoek")))
                                                    (:color ((yellow . "geel")
                                                             (red . "rood")
                                                             (blue . "blauw")
                                                             (green . "groen")))
                                                    (:texture ((soft . "zacht")
                                                               (fluffy . "pluizig")
                                                               (smooth . "glad")
                                                               (rough . "ruw")))
                                                    (:size ((small . "klein")
                                                            (large . "groot")
                                                            (tiny . "minuscuul")
                                                            (huge . "reusachtig"))))))))

;;(set-configuration *experiment* :trace-every-nth-interaction 100)

(setf *th* (get-type-hierarchy (grammar (first (population *experiment*)))))

(add-element (make-html *th* :render-program "circo" :weights? t))

(export '(th))

(add-element (make-html (world *experiment*)))
;; running 1 or more interactions
(run-interaction *experiment*)
(run-series *experiment* 100)


