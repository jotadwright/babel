
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
(defparameter *meaning* (comprehend "Does the large yellow sphere have the same material as the sphere in front of the tiny blue object?"))

(defun introduction ()
  (add-element '((a :name "1")))
  (add-element '((h2) "1. Introduction"))
  (add-element '((p) "This web demonstration presents full examples of the use of neural heuristics for optimizing the search processes involved in constructional language processing. In particular, it includes contrastive examples of the same utterance processed using the baseline depth-first search strategy with duplicate detection and hashing, and using the neural strategy, both in the comprehension and production direction."))

  (add-element '((p) "We used the CLEVR dataset on visual question answering ("((a :href "https://cs.stanford.edu/people/jcjohns/clevr/") "Johnson et al. 2017")") and CLEVR construction grammar ("((a :href "https://ehai.ai.vub.ac.be/demos/clevr-grammar/") "Nevens et al. 2019")") to benchmark the effects of the neural heurstics that we introduced in the paper. The CLEVR grammar consists of 170 constructions, of which 55 are morphological and lexical constructions. Apart from these, the grammar also contains 115 grammatical constructions that capture phenomena including referential expressions, spatial relations, coordination and subordination structures, and a wide range of interrogative structures. On average, 25 constructions should be applied in order to successfully comprehend or produce an utterance from the dataset. This means that the average solution is found at depth 25 in the search tree."))
  (add-element '((p) "The utterance from the dataset that we will explore in this web demonstration is the following question:"))

  (add-element '((p) ((b) ((i) "Does the large yellow sphere have the same material as the sphere in front of the tiny blue object?"))))

  (add-element '((p) "It has been annotated with the following procedural semantic meaning representation:"))

  (add-element (predicate-network->svg *meaning*))

  

  ;(comprehend-and-formulate "Does the large yellow sphere have the same material as the sphere in front of the tiny blue object?")
  
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
