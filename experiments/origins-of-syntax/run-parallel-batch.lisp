
;; (asdf:make :origins-of-syntax)

(in-package :origins-of-syntax)

;; Ensure that all monitors are deactivated
(deactivate-all-monitors)

;; Activate Export Monitors
(progn
  (activate-monitor print-a-dot-for-each-interaction+numbers)
  (activate-monitor export-communicative-success)
  (activate-monitor export-nr-of-constructions)
  (activate-monitor export-fcg-search))

;;;;;;

(run-parallel-batch-for-different-configurations
 :asdf-system "origins-of-syntax"
 :package "origins-of-syntax"
 :experiment-class "syntax-experiment"
 :number-of-interactions 100
 :number-of-series 4
 :monitors '("export-communicative-success" "export-nr-of-constructions" "export-fcg-search")
 :max-nr-parallel-processes 8
 :shared-configuration '((:population-size . 10)
                          (:trace-every-nth-interaction . nil)
                          (:min-nr-of-objects-in-scene . 10)
                          (:max-nr-of-objects-in-scene . 10)
                          (:min-nr-of-objects-in-topic . 2)
                          (:max-nr-of-objects-in-topic . 2)
                          (:ontology . ((:shape ((square ."vierkant")
                                                 (circle . "cirkel")
                                                 ))
                                        (:color ((red . "rood")
                                                 (blue . "blauw")
                                                 
                                                 ))
                                        (:size ((large . "groot")
                                                (tiny . "minuscuul")
                                                
                                                ))
                                        (:texture ((lined . "gestreept")
                                                (transparent . "doorzichtig")
                                                )))))
 :configurations '(
            #|       (n-gram-strategy-no-alignment-15000-4-2
		    ((:strategy . :n-gram-strategy)
                     (:alignment . :no-alignment)))
                   (n-gram-strategy-lateral-inhibition-15000-4-2
                    ((:strategy . :n-gram-strategy)
                     (:alignment . :lateral-inhibition)))
                   (grouping-strategy-15000-4-2
                    ((:strategy . :grouping-strategy)
                     (:alignment . :no-alignment)))|#
                   (categorisation-strategy-15000-4-2
                    ((:strategy . :categorisation-strategy)
                     (:alignment . :no-alignment)))
                   )
 :output-dir  (babel-pathname :directory '("experiments" "origins-of-syntax" "raw-data")))

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "origins-of-syntax" "raw-data" "grouping-strategy-15000-4-2" "communicative-success")
		   ("experiments" "origins-of-syntax" "raw-data" "grouping-strategy-15000-4-2" "nr-of-constructions"))
  :average-windows 500
  :plot-file-name "grouping-strategy-15000-4-2"
  :plot-directory '("experiments" "origins-of-syntax" "graphs")
  :captions '("Communicative Success" "Grammatical Constructions per Agent")
  :y1-min 0
  :y1-max 1
  :y2-min 0
  :x-label "# Games"
  :y1-label "Communicative Success"
  :y2-label "Grammatical Constructions per Agent"
  :use-y-axis '(1 2 1)
  :error-bars '(:percentiles (0 100))
  :error-bar-modes '(:filled)
  :open nil
  )

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "origins-of-syntax" "raw-data" "grouping-strategy-15000-4-2" "fcg-search"))
  :average-windows 1
  :plot-file-name "grouping-strategy-15000-4-2-search"
  :plot-directory '("experiments" "origins-of-syntax" "graphs")
  :captions '("Communicative Success" "FCG Search")
  :x-label "# Games"
  :y1-label "FCG Search"
  :use-y-axis '(1 2 1)
  :error-bars '(:percentiles (0 100))
  :error-bar-modes '(:filled)
  :points t
  :open nil
  )
