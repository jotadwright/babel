(ql:quickload :mwm-evaluation)
(in-package :mwm-evaluation)

(activate-monitor trace-fcg)
(activate-monitor trace-irl)

;;--------------;;
;; The ontology ;;
;;--------------;;

(make-mwm-ontology
 (merge-pathnames (make-pathname :directory '(:relative "serie-1"))
                  *simulated-concepts-path*))

;; Show the ontology in the web-interface:
;; (add-element (make-html *my-ontology*))

;;---------;;
;; Testing ;;
;;---------;;

;; Test sentences (see "Babel/grammars/clevr-grammar/start.lisp" for more examples):
(let ((ontology
       (make-mwm-ontology
        (merge-pathnames (make-pathname :directory '(:relative "serie-1"))
                         *simulated-concepts-path*))))
  (test-utterance-in-first-scene "there is a big gray object that is the same shape as the purple rubber object; what is it made of?"
                                 ontology)
  (test-utterance-in-first-scene "What color is the large sphere?" ontology)
  (test-utterance-in-first-scene "How many things have the same shape as the large red thing?" ontology)
  (test-utterance-in-first-scene "How many things are left of the small gray sphere that is in front of the large sphere that is right of the large blue cube?"
                                 ontology)
  (test-utterance-in-first-scene "How many things are left of the purple sphere that is behind the yellow thing?"
                                 ontology))

;;------------;;
;; Evaluation ;;
;;------------;;

(let ((ontology
       (make-mwm-ontology
        (merge-pathnames (make-pathname :directory '(:relative "serie-1"))
                         *simulated-concepts-path*))))
  ;; Evaluate on the ontology that is loaded manually
  (evaluate-mwm-accuracy ontology :nr-of-scenes 10))

;; Evaluate one particular serie
(evaluate-mwm-serie 1)
(evaluate-mwm-serie 2)
(evaluate-mwm-serie 3)
(evaluate-mwm-serie 4)
(evaluate-mwm-serie 5)
(evaluate-mwm-serie 6)
(evaluate-mwm-serie 7)
(evaluate-mwm-serie 8)
(evaluate-mwm-serie 9)
(evaluate-mwm-serie 10)

;; Evaluate on all series of concepts by loading the different series into the ontology
(evaluate-all-series)
