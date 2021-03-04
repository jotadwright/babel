
;;#############################################################################;;
;;                                                                             ;;
;; Web demo 'neural heuristics for scaling constructional language processing' ;;
;;                                                                             ;;
;;#############################################################################;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading the package and setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :clevr-grammar)

(in-package :clevr-grammar)
(use-package :web-interface)
(activate-monitor trace-fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web demo                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun header-page ()
  (clear-page)
  ;; Title
  (add-element '((hr)))
  (add-element '((h1) "Neural Heuristics for Scaling Constructional Language Processing."))
  (add-element '((p) "This web demonstration accompanies the paper:"))
  (add-element '((p) "Van Eecke, P., Nevens, J. &amp; Beuls, K. (XXXX). "((a :href "" :target "_blank") "Neural Heuristics for Scaling Constructional Language Processing")". Submitted to "((i) "Computational Linguistics") "."))
  (add-element '((p) "Explanations on how to use this demo are " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))
  (add-element '((hr)))
  (add-element '((p)  ((a :href "#1") ((b) "1. Introduction"))))
  (add-element '((p)  ((a :href "#2") ((b) "2. Comprehension"))))
  (add-element '((p)  ((a :href "#21") "2.1 Depth-first strategy")))
  (add-element '((p)  ((a :href "#22") "2.2 Neural strategy")))
  (add-element '((p)  ((a :href "#3") ((b) "3. Production"))))
  (add-element '((p)  ((a :href "#31") "3.1 Depth-first strategy")))
  (add-element '((p)  ((a :href "#32") "3.2 Neural strategy")))
  (add-element '((h3) ""))
  (add-element '((hr))))

;; (header-page)

(defun introduction ()
  (add-element '((a :name "1")))
  (add-element '((h2) "1. Introduction"))
  (add-element '((p) "This web demonstration presents full examples of the use of neural heuristics for optimizing the search processes involved in constructional language processing. In particular, it includes contrastive examples of the same utterance processed using the baseline depth-first search strategy with duplicate detection and hashing, and using the neural strategy, both in the comprehension and production direction."))

  (add-element '((p) ""))
  
  (add-element '((hr))))

;; (introduction)

(defun comprehension ()
  (add-element '((a :name "2")))
  (add-element '((h2) "2. Comprehension"))

  
  )

(create-static-html-page "Neural Heuristics for CCxG"
(progn
  (header-page)
  (introduction)
  
)
