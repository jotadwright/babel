
(ql:quickload :origins-of-syntax)

(in-package :origins-of-syntax)

;;;;;;;;;;;;;;;;;;;;;;
;; Lexical Strategy ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Running the simulation

(run-parallel-batch-for-different-configurations
 :asdf-system "origins-of-syntax"
 :package "origins-of-syntax"
 :experiment-class "syntax-experiment"
 :number-of-interactions 20
 :number-of-series 1
 :monitors '("export-communicative-success" "export-nr-of-constructions" "export-fcg-search" "export-coherence" "export-categorial-coherence")
 :max-nr-parallel-processes 8
 :shared-configuration '((:population-size . 10)
                         (:trace-every-nth-interaction . nil)
                         (:min-nr-of-objects-in-scene . 1)
                         (:max-nr-of-objects-in-scene . 64)
                         (:min-nr-of-objects-in-topic . 1)
                         (:max-nr-of-objects-in-topic . 2)
                         (:ontology . ((:shape ((square ."vierkant")
                                                (circle . "cirkel")
                                                (triangle ."driehoek")
                                                (rectangle . "rechthoek")))
                                       (:color ((yellow . "geel")
                                                (red . "rood")
                                                (blue . "blauw")
                                                (green . "groen")))
                                       (:size ((small . "klein")
                                               (large . "groot")
                                               (tiny . "minuscuul")
                                               (huge ."reusachtig"))))))
 :configurations '((lexical-strategy
		   ((:strategy .  :lexical-strategy)
                    (:alignment . :no-alignment))))
 :output-dir (babel-pathname :directory
                             '("experiments""origins-of-syntax" "raw-data" "thesis-simulations")))

;; Plotting

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "lexical-strategy" "communicative-success")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "lexical-strategy" "coherence")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "lexical-strategy" "nr-of-constructions")
                   )
  :average-windows '(250 500 10)
  :plot-file-name "communicative-success" 
  :plot-directory '("experiments" "origins-of-syntax" "graphs" "thesis-simulations" "lexical-strategy")
  :captions '("Communicative Success" "Language Coherence" "Grammatical Constructions")
  :title "Lexical Strategy"
  :y1-min 0
  :y1-max 1
  :y2-min 0
  :y2-max 20
  :x-label "# Games per Agent"
  :y1-label "Communicative Success"
  :y2-label "Grammatical Constructions per Agent"
  :use-y-axis '(1 1 2)
  :error-bars '(:percentiles (25 75))
  :error-bar-modes '(:filled)
  :open t
  :fsize 16
  :key-location "bmargin"
  :colors '("#328888"  "light-green" "dark-goldenrod")
  :divide-indices-by 5)

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "lexical-strategy" "fcg-search"))
 :average-windows 500
 :plot-file-name "search"
 :plot-directory '("experiments" "origins-of-syntax" "graphs" "thesis-simulations" "lexical-strategy")
 :captions '("Search")
 :title "Lexical Strategy"
 :x-label "# Games per Agent"
 :y1-label "Search"
 :y1-min 1
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

;;;;;;;;;;;;;;;;;;;;;;;
;; Grouping Strategy ;;
;;;;;;;;;;;;;;;;;;;;;;;

(run-parallel-batch-for-different-configurations
 :asdf-system "origins-of-syntax"
 :package "origins-of-syntax"
 :experiment-class "syntax-experiment"
 :number-of-interactions 50000
 :number-of-series 6
 :monitors '("export-communicative-success" "export-nr-of-constructions" "export-fcg-search" "export-coherence")
 :max-nr-parallel-processes 8
 :shared-configuration '((:population-size . 10)
                         (:trace-every-nth-interaction . nil)
                         (:min-nr-of-objects-in-scene . 1)
                         (:max-nr-of-objects-in-scene . 64)
                         (:min-nr-of-objects-in-topic . 1)
                         (:max-nr-of-objects-in-topic . 2)
                         (:ontology . ((:shape ((square ."vierkant")
                                                (circle . "cirkel")
                                                (triangle ."driehoek")
                                                (rectangle . "rechthoek")))
                                       (:color ((yellow . "geel")
                                                (red . "rood")
                                                (blue . "blauw")
                                                (green . "groen")))
                                       (:size ((small . "klein")
                                               (large . "groot")
                                               (tiny . "minuscuul")
                                               (huge ."reusachtig"))))))
 :configurations '((grouping-strategy
                    ((:strategy .  :grouping-strategy)
                    (:alignment . :no-alignment))))
 :output-dir (babel-pathname :directory
                             '("experiments""origins-of-syntax" "raw-data" "thesis-simulations")))

;; Plotting



(raw-files->evo-plot
 :raw-file-paths '(
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "grouping-strategy" "communicative-success")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "grouping-strategy" "coherence")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "grouping-strategy" "nr-of-constructions")
                   )
  :average-windows '(250 500 10)
  :plot-file-name "communicative-success+nr-of-constructions" 
  :plot-directory '("experiments" "origins-of-syntax" "graphs" "thesis-simulations" "grouping-strategy")
  :captions '("Communicative Success" "Language Coherence" "Grammatical Constructions")
  :title "Grouping Strategy"
  :y1-min 0
  :y1-max 1
  :y2-min 0
  :y2-max 250
  :x-label "# Games per Agent"
  :y1-label "Communicative Success / Language Coherence"
  :y2-label "# Grammatical Constructions per Agent"
  :use-y-axis '(1 1 2)
  :error-bars '(:percentiles (25 75))
  :error-bar-modes '(:filled)
  :open t
  :fsize 16
  :key-location "bmargin"
  :divide-indices-by 5
  :colors '("#328888"  "light-green" "dark-goldenrod")
  :end 5000
 )

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "grouping-strategy" "fcg-search"))
 :average-windows 500
 :plot-file-name "search"
 :plot-directory '("experiments" "origins-of-syntax" "graphs" "thesis-simulations" "grouping-strategy")
 :captions '("Search")
 :title "Grouping Strategy"
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
 :end 5000
 :colors '("blue")
 )

;;;;;;;;;;;;;;;;;;;;;
;; N-gram Strategy ;;
;;;;;;;;;;;;;;;;;;;;;

;; No alignment
(run-parallel-batch-for-different-configurations
 :asdf-system "origins-of-syntax"
 :package "origins-of-syntax"
 :experiment-class "syntax-experiment"
 :number-of-interactions 50000
 :number-of-series 6
 :monitors '("export-communicative-success" "export-nr-of-constructions" "export-fcg-search" "export-coherence")
 :max-nr-parallel-processes 8
 :shared-configuration '((:population-size . 10)
                         (:trace-every-nth-interaction . nil)
                         (:min-nr-of-objects-in-scene . 1)
                         (:max-nr-of-objects-in-scene . 64)
                         (:min-nr-of-objects-in-topic . 1)
                         (:max-nr-of-objects-in-topic . 2)
                         (:ontology . ((:shape ((square ."vierkant")
                                                (circle . "cirkel")
                                                (triangle ."driehoek")
                                                (rectangle . "rechthoek")))
                                       (:color ((yellow . "geel")
                                                (red . "rood")
                                                (blue . "blauw")
                                                (green . "groen")))
                                       (:size ((small . "klein")
                                               (large . "groot")
                                               (tiny . "minuscuul")
                                               (huge ."reusachtig"))))))
 :configurations '((n-gram-strategy-no-alignment
                    ((:strategy .  :n-gram-strategy)
                     (:alignment . :no-alignment))))
 :output-dir (babel-pathname :directory
                             '("experiments" "origins-of-syntax" "raw-data" "thesis-simulations")))

;; lateral inhibition
(run-parallel-batch-for-different-configurations
 :asdf-system "origins-of-syntax"
 :package "origins-of-syntax"
 :experiment-class "syntax-experiment"
 :number-of-interactions 2000
 :number-of-series 6
 :monitors '("export-communicative-success" "export-nr-of-constructions" "export-fcg-search" "export-coherence")
 :max-nr-parallel-processes 8
 :shared-configuration '((:population-size . 10)
                         (:trace-every-nth-interaction . nil)
                         (:min-nr-of-objects-in-scene . 1)
                         (:max-nr-of-objects-in-scene . 64)
                         (:min-nr-of-objects-in-topic . 1)
                         (:max-nr-of-objects-in-topic . 2)
                         (:ontology . ((:shape ((square ."vierkant")
                                                (circle . "cirkel")
                                                (triangle ."driehoek")
                                                (rectangle . "rechthoek")))
                                       (:color ((yellow . "geel")
                                                (red . "rood")
                                                (blue . "blauw")
                                                (green . "groen")))
                                       (:size ((small . "klein")
                                               (large . "groot")
                                               (tiny . "minuscuul")
                                               (huge ."reusachtig"))))))
 :configurations '((n-gram-strategy
                    ((:strategy .  :n-gram-strategy)
                     (:alignment . :lateral-inhibition))))
 :output-dir (babel-pathname :directory
                             '("experiments" "origins-of-syntax" "raw-data" "thesis-simulations")))

;; Plotting

(raw-files->evo-plot
 :raw-file-paths '(
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy-no-alignment" "communicative-success")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy-no-alignment" "coherence")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy-no-alignment" "nr-of-constructions")
                   )
  :average-windows '(250 500 10)
  :plot-file-name "communicative-success+nr-of-constructions" 
  :plot-directory '("experiments" "origins-of-syntax" "graphs" "thesis-simulations" "n-gram-strategy-no-alignment")
  :captions '("Communicative Success" "Language Coherence" "Grammatical Constructions")
  :title "N-gram Strategy"
  :y1-min 0
  :y1-max 1
  :y2-min 0
  :y2-max 500
  :x-label "# Games per Agent"
  :y1-label "Communicative Success / Language Coherence"
  :y2-label "# Grammatical Constructions per Agent"
  :use-y-axis '(1 1 2)
  :error-bars '(:percentiles (25 75))
  :error-bar-modes '(:filled)
  :open t
  :fsize 16
  :key-location "bmargin"
  :divide-indices-by 5
  :colors '("#328888"  "light-green" "dark-goldenrod")
 )

(raw-files->evo-plot
 :raw-file-paths '(
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy" "communicative-success")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy" "coherence")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy" "nr-of-constructions")
                   )
  :average-windows '(250 250 10)
  :plot-file-name "communicative-success+nr-of-constructions" 
  :plot-directory '("experiments" "origins-of-syntax" "graphs" "thesis-simulations" "n-gram-strategy-no-alignment")
  :captions '("Communicative Success" "Language Coherence" "Grammatical Constructions per Agent")
  :title "N-gram Strategy"
  :y1-min 0
  :y1-max 1
  :y2-min 0
  :y2-max 500
  :x-label "# Games per Agent"
  :y1-label "Communicative Success / Language Coherence"
  :y2-label "# Grammatical Constructions per Agent"
  :use-y-axis '(1 1 2)
  :error-bars '(:percentiles (25 75))
  :error-bar-modes '(:filled)
  :open t
  :fsize 16
  :key-location "bmargin"
  :divide-indices-by 5
  :colors '("#328888"  "light-green" "dark-goldenrod")
 )

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy" "communicative-success")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy" "coherence")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy" "nr-of-constructions")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy-no-alignment" "communicative-success")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy-no-alignment" "coherence")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy-no-alignment" "nr-of-constructions"
                   ))
  :average-windows '(250 500 10 250 500 10)
  :plot-file-name "communicative-success+nr-of-constructions" 
  :plot-directory '("experiments" "origins-of-syntax" "graphs" "thesis-simulations" "n-gram-strategy")
  :captions '("Communicative Success (no alignment)"  "Language Coherence (no alignment)" "Grammatical Constructions per Agent (no alignment)" 
              "Communicative Success (lateral inhibition)"  "Language Coherence (lateral inhibition)" "Grammatical Constructions per Agent (lateral inhibition)" )
  :title "N-gram Strategy"
  :y1-min 0
  :y1-max 1
  :y2-min 0
  :y2-max 500
  :x-label "# Games per Agent"
  :y1-label "Communicative Success"
  :y2-label "# Grammatical Constructions per Agent"
  :use-y-axis '(1 1 2 1 1 2 )
  :error-bars '(:percentiles (25 75))
  :error-bar-modes '(:filled)
  :open t
  :fsize 16
  :key-location "bmargin"
  :divide-indices-by 5
  :colors  '("dark-blue" "blue" "light-blue" "dark-red" "red" "light-red"))


(raw-files->evo-plot
 :raw-file-paths '(("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy-no-alignment" "fcg-search")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy" "fcg-search"))
 :average-windows 500
 :plot-file-name "search"
 :plot-directory '("experiments" "origins-of-syntax" "graphs" "thesis-simulations" "n-gram-strategy")
 :captions '("Search (no alignment)" "Search (lateral inhibition)")
 :title "N-gram Strategy"
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
 ;:end 30000
 :colors '("red" "blue")
 )

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy-no-alignment" "fcg-search"))
 :average-windows 5000
 :plot-file-name "search"
 :plot-directory '("experiments" "origins-of-syntax" "graphs" "thesis-simulations" "n-gram-strategy-no-alignment")
 :captions '("Search")
 :title "N-gram Strategy (no alignment)"
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
 ;:end 5000
 :colors '("blue")
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Categorisation Strategy ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-parallel-batch-for-different-configurations
 :asdf-system "origins-of-syntax"
 :package "origins-of-syntax"
 :experiment-class "syntax-experiment"
 :number-of-interactions 2500
 :number-of-series 6
 :monitors '("export-communicative-success" "export-nr-of-constructions" "export-fcg-search" "export-coherence" "export-categorial-coherence")
 :max-nr-parallel-processes 8
 :shared-configuration '((:population-size . 10)
                         (:trace-every-nth-interaction . nil)
                         (:min-nr-of-objects-in-scene . 1)
                         (:max-nr-of-objects-in-scene . 64)
                         (:min-nr-of-objects-in-topic . 1)
                         (:max-nr-of-objects-in-topic . 2)
                         (:ontology . ((:shape ((square ."vierkant")
                                                (circle . "cirkel")
                                                (triangle ."driehoek")
                                                (rectangle . "rechthoek")))
                                       (:color ((yellow . "geel")
                                                (red . "rood")
                                                (blue . "blauw")
                                                (green . "groen")))
                                       (:size ((small . "klein")
                                               (large . "groot")
                                               (tiny . "minuscuul")
                                               (huge ."reusachtig"))))))
 :configurations '((pattern-strategy
                    ((:strategy .  :categorisation-strategy)
                     (:alignment . :type-hierarchy))))
 :output-dir (babel-pathname :directory
                             '("experiments" "origins-of-syntax" "raw-data" "thesis-simulations")))

(raw-files->evo-plot
 :raw-file-paths '(
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "pattern-strategy" "communicative-success")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "pattern-strategy" "coherence")
                  ; ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "pattern-strategy" "nr-of-constructions")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "pattern-strategy" "categorial-coherence")
                   )
  :average-windows '(250 500 10)
  :plot-file-name "communicative-success+nr-of-constructions" 
  :plot-directory '("experiments" "origins-of-syntax" "graphs" "thesis-simulations" "pattern-strategy")
  :captions '("Communicative Success" "Language Coherence" "Categorial Coherence")
  :title "Pattern Strategy"
  :y1-min 0
  :y1-max 1
  :y2-min 1
  :y2-max 2
  :x-label "# Games per Agent"
  :y1-label "Communicative Success / Language Coherence"
  :y2-label "Categorial Coherence"
  :use-y-axis '(1 1 2)
  :error-bars '(:percentiles (25 75))
  :error-bar-modes '(:filled)
  :open t
  :fsize 16
  :key-location "bmargin"
  :divide-indices-by 5
  :colors '("#328888"  "light-green" "dark-goldenrod")
  :end 1200
 )

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "pattern-strategy" "fcg-search"))
 :average-windows 500
 :plot-file-name "search"
 :plot-directory '("experiments" "origins-of-syntax" "graphs" "thesis-simulations" "pattern-strategy")
 :captions '("Search")
 :title "Pattern Strategy"
 :x-label "# Games per Agent"
 :y1-label "Search"
 :y1-min 1
 :use-y-axis '(1)
 :error-bars '(:percentiles (25 75))
 :error-bar-modes '(:filled)
 :open t
 :points nil
 :fsize 16
 :key-location "bmargin"
 :divide-indices-by 5
 :colors '("blue")
 :end 5000
 )



;;;;;;;;;;;;;;;;;;;;;;;
;; Comparison Graphs ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Communicative Success

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "lexical-strategy" "communicative-success")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "grouping-strategy" "communicative-success")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy" "communicative-success")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "pattern-strategy" "communicative-success")
                   )
  :average-windows 250
  :plot-file-name "communicative-success" 
  :plot-directory '("experiments" "origins-of-syntax" "graphs" "thesis-simulations" "comparison")
  :captions '("Lexical Strategy" "Grouping Strategy" "N-gram Strategy" "Pattern Strategy")
  :title "Communicative Success"
  :y1-min 0
  :y1-max 1
  :x-label "# Games per Agent"
  :y1-label "Communicative Success"
  :use-y-axis '(1)
  :error-bars '(:percentiles (25 75))
  :error-bar-modes '(:filled)
  :open t
  :fsize 16
  :key-location "bmargin"
  :divide-indices-by 5
  :end 15000 
 )


;; Coherence of the Language

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "lexical-strategy" "coherence")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "grouping-strategy" "coherence")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy" "coherence")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "pattern-strategy" "coherence")
                   )
  :average-windows 500
  :plot-file-name "coherence" 
  :plot-directory '("experiments" "origins-of-syntax" "graphs" "thesis-simulations" "comparison")
  :captions '("Lexical Strategy" "Grouping Strategy" "N-gram Strategy" "Pattern Strategy")
  :title "Coherence of the Language"
  :y1-min 0
  :y1-max 1
  :x-label "# Games per Agent"
  :y1-label "Language Coherence"
  :use-y-axis '(1)
  :error-bars '(:percentiles (25 75))
  :error-bar-modes '(:filled)
  :open t
  :fsize 16
  :key-location "bmargin"
  :divide-indices-by 5
 )

;; Number of Grammatical Constructions

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "lexical-strategy" "nr-of-constructions")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "grouping-strategy" "nr-of-constructions")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy" "nr-of-constructions")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "pattern-strategy" "nr-of-constructions")
                   )
  :average-windows 10
  :plot-file-name "nr-of-constructions" 
  :plot-directory '("experiments" "origins-of-syntax" "graphs" "thesis-simulations" "comparison")
  :captions '("Lexical Strategy" "Grouping Strategy" "N-gram Strategy" "Pattern Strategy")
  :title "Number of Grammatical Constructions"
  :y1-min 0
  :x-label "# Games per Agent"
  :y1-label "# Grammatical Constructions"
  :use-y-axis '(1)
  :error-bars '(:percentiles (25 75))
  :error-bar-modes '(:filled)
  :open t
  :fsize 16
  :key-location "bmargin"
  :divide-indices-by 5
 )

;; Search Effort

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "lexical-strategy" "fcg-search")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "grouping-strategy" "fcg-search")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "n-gram-strategy" "fcg-search")
                   ("experiments" "origins-of-syntax" "raw-data" "thesis-simulations" "pattern-strategy" "fcg-search")
                   )
  :average-windows 500
  :plot-file-name "search" 
  :plot-directory '("experiments" "origins-of-syntax" "graphs" "thesis-simulations" "comparison")
  :captions '("Lexical Strategy" "Grouping Strategy" "N-gram Strategy" "Pattern Strategy")
  :title "Search Effort"
  :y1-min 0
  :y1-max 15
  :x-label "# Games per Agent"
  :y1-label "Search"
  :use-y-axis '(1)
  :error-bars '(:percentiles (25 75))
  :error-bar-modes '(:filled)
  :open t
  :fsize 16
  :key-location "bmargin"
  :divide-indices-by 5
  :colors  *great-gnuplot-colors*
  :end 15000
 )