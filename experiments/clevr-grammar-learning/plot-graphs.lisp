
(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)

;; th edges and nodes

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "number-of-th-nodes")
                   ("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "number-of-th-edges"))
 :file-type "lisp"
 :plot-file-name "th-edges+th-nodes"
 :plot-directory '("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "lisp-plots")
 :captions '("nodes" "edges")
 :x-label "# Observations"
 :y1-label "# Nodes"
 :y2-label "# Edges"
 :use-y-axis '(1 2)
 :average-windows 1
 :y1-max nil
 :y2-max nil
 :error-bars '(:percentiles (25 75))
 :error-bar-modes '(:lines)
 :open t
 :key-location "bmargin"
 )

;; repair-per-type
(raw-files->evo-plot
 :raw-file-paths '(("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "repair-usage-add-categorial-links")
                   ("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "repair-usage-item-based-to-lexical")
                   ("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "repair-usage-holophrase-to-item-based+lexical+lexical--substitution")
                   ("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "repair-usage-holophrase-to-item-based+lexical+lexical--addition")
                   ("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "repair-usage-holophrase-to-item-based+lexical+lexical--deletion")
                   ("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "repair-usage-lexical-to-item-based")
                   ("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "repair-usage-nothing-to-holophrase"))
 :file-type "lisp"
 :plot-file-name "repair-per-type-1k"
 :plot-directory '("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "lisp-plots")
 :captions '("add-categorial-links"
             "item-based-->lexical"
             "holophrase-->item-based+lexical+lexical-substitution"
             "holophrase-->item-based+lexical-addition"
             "holophrase-->item-based+lexical+holophrase-deletion"
             "lexical-->item-based"
             "nothing-->holophrase")
 :x-label "# Observations"
 :y1-label "Repair Usage"
 :use-y-axis '(1 1 1 1 1 1 1)
 :y1-max 1
 :average-windows 100
 :error-bars '(:percentiles (25 75))
 :error-bar-modes '(:lines)
 :open t
 ;:key-location "bmargin"
 :end 1000
 )

;; grammar-size-per-type-1k

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "number-of-holophrase-cxns")
                   ("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "number-of-item-based-cxns")
                   ("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "number-of-lexical-cxns"))
 :file-type "lisp"
 :plot-file-name "grammar-size-per-type-1k"
 :plot-directory '("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "lisp-plots")
 :captions '("holophrase" "item-based" "lexical")
 :x-label "# Observations"
 :y1-label "# Constructions"
 :use-y-axis '(1 1 1)
 :y1-max nil
 :average-windows 1
 :error-bars '(:percentiles (25 75))
 :error-bar-modes '(:lines)
 :open t
 :key-location "bmargin"
 :end 1000
 )

;; grammar-size-per-type

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "number-of-holophrase-cxns")
                   ("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "number-of-item-based-cxns")
                   ("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "number-of-lexical-cxns"))
 :file-type "lisp"
 :plot-file-name "grammar-size-per-type"
 :plot-directory '("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "lisp-plots")
 :captions '("holophrase" "item-based" "lexical")
 :x-label "# Observations"
 :y1-label "# Constructions"
 :use-y-axis '(1 1 1)
 :y1-max nil
 :average-windows 1
 :error-bars '(:percentiles (25 75))
 :error-bar-modes '(:lines)
 :open t
 :key-location "bmargin"
 )

;; communicative-success+grammar-size-2k

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "communicative-success")
                   ("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "grammar-size"))
 :file-type "lisp"
 :plot-file-name "communicative-success+grammar-size-2k"
 :plot-directory '("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "lisp-plots")
 :captions '("communicative success" "grammar size")
 :x-label "# Observations"
 :y1-label "Communicative Success"
 :y1-max 1
 :y2-label "# Constructions"
 :use-y-axis '(1 2)
 :average-windows '(50 1)

 :error-bars '(:percentiles (25 75))
 :error-bar-modes '(:lines)
 :open t
 :key-location "bmargin"
 :end 2000
 )

;; communicative-success+grammar-size

(raw-files->evo-plot
 :raw-file-paths '(("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "communicative-success")
                   ("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "grammar-size"))
 :file-type "lisp"
 :plot-file-name "communicative-success+grammar-size"
 :plot-directory '("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1" "lisp-plots")
 :captions '("communicative success" "grammar size")
 :x-label "# Observations"
 :y1-label "Communicative Success"
 :y1-max 1
 :y2-label "# Constructions"
 :use-y-axis '(1 2)
 :average-windows '(50 1)

 :error-bars '(:percentiles (25 75))
 :error-bar-modes '(:lines)
 :open t
 :key-location "bmargin"
 )