(ql:quickload :cle)

(in-package :cle)


(create-graph-for-single-strategy "similarity22june" "2023-06-21_16h22m41s-exp-0"
                                  (list "communicative-success" "lexicon-coherence")
                                  :average-windows '(500 500)
                                  :use-y-axis '(1 1)
                                  :y1-min 0 :y1-max 1
                                  :x-label "Number of Games"
                                  :y1-label "Communicative Success/Lexicon Coherence"
                                  :captions '("communicative success"
                                              "lexicon coherence")
                                  :error-bars '(:percentile 5 95)
                                  :error-bar-modes '(:lines)
                                  :key-location "bottom"
                                  :open t)

(create-graph-for-single-strategy "similarity/all-channels" "s0-2023-06-22_9h38m52s-exp-1"
                                  (list "communicative-success" "lexicon-coherence")
                                  :average-windows '(10000 10000)
                                  :use-y-axis '(1 1)
                                  :y1-min 0 :y1-max 1
                                  :x-label "Number of Games"
                                  :y1-label "Communicative Success/Lexicon Coherence"
                                  :captions '("communicative success"
                                              "lexicon coherence")
                                  :error-bars '(:percentile 5 95)
                                  :error-bar-modes '(:lines)
                                  :key-location "bottom"
                                  :open t)

(create-graph-comparing-strategies :title "title"
                                   :base-dir "similarity/10-channels"
                                   :experiment-names (mapcar (lambda (x) (first (last (pathname-directory x))))
                                           (uiop:subdirectories (babel-pathname :directory '("experiments"
                                                                                             "concept-emergence2"
                                                                                             "logging"
                                                                                             "similarity"
                                                                                             "10-channels"
                                                                                             ))))
                                   :measure-name "lexicon-coherence"
                                   :average-windows 5000
                                   :plot-file-name "compare-lex-coh")
