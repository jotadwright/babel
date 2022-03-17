(ql:quickload :mwm-evaluation)
(in-package :mwm-evaluation)

(activate-monitor trace-fcg)
(activate-monitor trace-irl)

;;--------------;;
;; The ontology ;;
;;--------------;;

(make-mwm-ontology (babel-pathname :directory (list "experiments""multidimensional-word-meanings" "learned-concepts" "thesis-main-results" "baseline-simulated-default-lexicon" "serie-1")))

;; Show the ontology in the web-interface:
;; (add-element (make-html *my-ontology*))

;;---------;;
;; Testing ;;
;;---------;;

;; Test sentences (see "Babel/grammars/clevr-grammar/start.lisp" for more examples):
;(test-utterance-in-first-scene "there is a big gray object that is the same shape as the purple rubber object; what is it made of?")
;(test-utterance-in-first-scene "What color is the large sphere?")
;(test-utterance-in-first-scene "How many things have the same shape as the large red thing?")
;(test-utterance-in-first-scene "How many things are left of the small gray sphere that is in front of the large sphere that is right of the large blue cube?")
;(test-utterance-in-first-scene "How many things are left of the purple sphere that is behind the yellow thing?")

;;------------;;
;; Evaluation ;;
;;------------;;

;; Evaluate on the ontology that is loaded manually
(evaluate-mwm-accuracy "val" "mwm-evaluation" "mwm-errors"  :nr-of-scenes 1)

;; Evaluate on all series of concepts by loading the different series into the ontology
(evaluate-all-series)
