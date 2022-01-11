(ql:quickload :emergent-rl)
(in-package :emergent-rl)

;(create-fm-competition-graph :experiment-dir '("experiments" "emergent-rl" "raw-data" "comp" "int" "monitors") :measure-name "form-competition-4")

(create-graph-for-single-strategy
    :experiment-name "paper/basic + no del"
    :measure-names '("communicative-success"
                    "lexicon-size"
                    "lexicon-coherence"
                    )
    :captions '(
                "communicative success"
                "lexicon size"
                "lexical coherence"
                )
    :y-axis '(1 2 1)
    :y1-max 1.0
    :y2-max 35
    :x-max 5000
    :avg-window '(100 1 100)
    :y1-label "communicative success / lexical coherence"
    :y2-label "lexicon size"
    :plot-file-name "basic_nodel-comm_coh_size-5k"
 )

(create-graph-for-single-strategy
    :experiment-name "paper/int. + no del"
    :measure-names '("communicative-success"
                    "lexicon-size"
                    "lexicon-coherence"
                    )
    :captions '(
                "communicative success"
                "lexicon size"
                "lexical coherence"
                )
    :y-axis '(1 2 1)
    :y1-max 1.0
    :y2-max 35
    :x-max 5000
    :avg-window '(100 1 100)
    :y1-label "communicative success / lexical coherence"
    :y2-label "lexicon size"
    :plot-file-name "int_nodel-comm_coh_size-5k"
 )