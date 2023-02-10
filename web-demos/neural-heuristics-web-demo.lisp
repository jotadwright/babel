
;;#############################################################################;;
;;                                                                             ;;
;; Web demo 'neural heuristics for scaling constructional language processing' ;;
;;                                                                             ;;
;;#############################################################################;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading the package and setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :clevr-grammar-v1)

(in-package :clevr-grammar-v1)
(use-package :web-interface)

(activate-monitor  trace-fcg)

;; have to restore the old clevr grammar for this demo...

(defparameter *baseline-configurations*
  '((:cxn-supplier-mode . :ordered-by-label-hashed)
    (:priority-mode . :nr-of-applied-cxns)
    (:parse-order hashed nom cxn)
    (:production-order hashed-lex nom cxn hashed-morph)
    (:max-nr-of-nodes . 10000)))

(defparameter *neural-configurations*
  '((:queue-mode . :greedy-best-first)
    (:cxn-supplier-mode . :ordered-by-label-hashed+seq2seq)
    (:hash-mode . :hash-string-meaning-lex-id)
    (:priority-mode . :seq2seq-additive-with-sets)
    (:parse-order hashed cxn)
    (:production-order hashed-lex cxn hashed-morph)
    (:cxn-sets-with-sequential-application hashed-lex hashed-morph)
    (:seq2seq-endpoint . #-ccl "http://localhost:8888/next-cxn"
                         #+ccl "http://127.0.0.1:8888/next-cxn")
    (:seq2seq-model-comprehension . "clevr_comprehension_model")
    (:seq2seq-model-formulation . "clevr_formulation_model")
    (:seq2seq-rpn-fn . clevr-grammar-v1::clevr-meaning->rpn)))

(defparameter *utterance* "Does the large yellow sphere have the same material as the sphere in front of the tiny blue object?")
(defparameter *meaning* (comprehend *utterance*))

(defparameter *baseline-solution-comprehension*
  (second (multiple-value-list
           (progn
             (set-configurations *fcg-constructions* *baseline-configurations* :replace t)
             (comprehend *utterance*)))))

;*baseline-solution-comprehension* ;;solution = 67

                           
(defparameter *neural-solution-comprehension*
  (second (multiple-value-list
           (progn
             (set-configurations *fcg-constructions* *neural-configurations* :replace t)
             (comprehend *utterance*)))))

;*neural-solution-comprehension* ;; solution = 34


(defparameter *baseline-solution-production*
  (second (multiple-value-list
           (progn
             (set-configurations *fcg-constructions* *baseline-configurations* :replace t)
             (formulate (fcg::instantiate-variables *meaning*))))))

;*baseline-solution-production* ;; solution = 250

(defparameter *neural-solution-production*
  (second (multiple-value-list
           (progn
             (set-configurations *fcg-constructions* *neural-configurations* :replace t)
             (formulate (fcg::instantiate-variables *meaning*) )))))

;*neural-solution-production* ;; solution = 37 (optimal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web demo                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Larger font for text in <p> tags
(define-css 'main "p {font-size: 11pt; text-align: justify}")

(defun header-page ()
  (clear-page)
  ;; Title
  (add-element '((hr)))
  (add-element '((h1) "Neural Heuristics for Scaling Constructional Language Processing"))
  (add-element '((p) "This web demonstration accompanies the paper:"))
  (add-element '((p) "Van Eecke, P., Nevens, J. &amp; Beuls, K. (2023). Neural Heuristics for Scaling Constructional Language Processing. " ((i) "Journal of Language Modelling") " (in press)"))
  
  (add-element '((hr)))

  (add-element '((h3) "Summary"))
  (add-element '((p) "Constructionist approaches to language,  as originally laid out by, among others, Fillmore, Goldberg, Kay and Croft, consider form-meaning mappings,  called constructions,  to be the basic unit of linguistic analysis.  Apart from the fact that they constitute form-meaning mappings,  constructions are subject to very few restrictions.  First of all, constructions do not necessarily correspond to tree-building operations.  Second,  constructions are non-local in the sense that they can access all information that is known during processing.  Third,  constructions can involve units of arbitrary size,  both on the form and the meaning side.  Finally,  constructions  are not restricted to continuous constituents and are not even required to include word order constraints.  As a consequence,  constructional language processing cannot straightforwardly be implemented in a faithful way using common grammar processing techniques,  such as chart parsing and generation. Instead,  faithful computational construction grammar implementations implement constructional language processing as a state-space search problem."))

  (add-element '((p) "In order to reliably scale to large problems,  state-space search methods rely on heuristics that can estimate the likelihood that a given state will lead to a solution.  While certain optimizations have in the past been applied to the case of computational construction grammar,  including construction sets and priming networks,  a lack of general and powerful heuristics remained a major obstacle to ongoing construction grammar research, in particular to research on representing, processing and learning large-scale construction grammars."))

  (add-element '((p) "The neural methodology that we have presented in this paper introduces a general and efficacious way to learn heuristics that substantially optimize the search processes involved in constructional language processing.  Analogous to recent successes in many subfields of artificial intelligence, including game playing and planning,  the methodology combines the predictive strengths of neural networks with the expressive representations,  sound logic operations and backtracking abilities of traditional search and unification methods."))

  (add-element '((p) "An integration of the proposed method in the Fluid Construction Grammar system and an evaluation of the method using the CLEVR benchmark dataset and the CLEVR construction grammar showed that the neural heuristics indeed outperformed the state-of-the-art priming strategy and could substantially reduce the search space and processing time in both the comprehension and the production direction,  especially in the case of utterances that otherwise gave rise to a large search space."))

  (add-element '((p) "This general methodology for learning neural heuristics that optimize the search processes involved in constructional language processing constitutes a crucial contribution towards the scaling of constructionist approaches to language.  It thereby has both theoretical and practical implications. On the theoretical side,  scalable processing models will allow construction grammarians to go beyond the study of constructions in isolation,  and model the intricate interactions that take place between constructions as part of a larger grammar.  On the practical side, the scaling of constructional language processing paves the way for achieving breakthroughs in ongoing research on learning large-scale construction grammars, which has in turn major implications on research in usage-based linguistics, models of language acquisition and the use of construction grammar in language technology applications.")) 
  
  (add-element '((hr)))
  
  (add-element '((h3) "Contents"))
  
  (add-element '((p)  ((a :href "#1") ((b) "1. Introduction"))))
  (add-element '((p)  ((a :href "#2") ((b) "2. Comprehension"))))
  (add-element '((p)  ((a :href "#21") "2.1 Depth-first strategy")))
  (add-element '((p)  ((a :href "#22") "2.2 Neural strategy")))
  (add-element '((p)  ((a :href "#3") ((b) "3. Production"))))
  (add-element '((p)  ((a :href "#31") "3.1 Depth-first strategy")))
  (add-element '((p)  ((a :href "#32") "3.2 Neural strategy")))
  (add-element '((h3) ""))

  (add-element '((hr)))
  
  (add-element '((p) "Explanations on how to use this demo are " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))

  (add-element '((p :style "color:darkred") "WARNING! A loading delay might occur when expanding parts of the search processes. If the page seems no longer responsive, refreshing might help."))
  (add-element '((h3) ""))
  
  (add-element '((hr))))

;; (header-page)


(defun introduction ()
  (add-element '((a :name "1")))
  (add-element '((h2) "1. Introduction"))
  (add-element '((p) "This web demonstration presents full examples of the use of neural heuristics for optimizing the search processes involved in constructional language processing. In particular, it includes contrastive examples of the same utterance processed using the baseline depth-first search strategy with backtracking, duplicate detection and hashing, and using the neural strategy, both in the comprehension and production direction."))

  (add-element '((p) "We used the CLEVR dataset on visual question answering ("((a :href "https://cs.stanford.edu/people/jcjohns/clevr/") "Johnson et al. 2017")") and the CLEVR construction grammar ("((a :href "https://ehai.ai.vub.ac.be/demos/clevr-grammar/") "Nevens et al. 2019")") to benchmark the effects of the neural heurstics that we introduced in the paper. The CLEVR grammar consists of 170 constructions, of which 55 are morphological and lexical constructions. Apart from these, the grammar also contains 115 grammatical constructions that capture phenomena including referential expressions, spatial relations, coordination and subordination structures, and a wide range of interrogative structures. On average, 25 constructions should be applied in order to successfully comprehend or produce an utterance from the dataset. This means that the average solution is found at depth 25 in the search tree."))
  (add-element '((p) "The utterance from the dataset that we will explore in this web demonstration is the following question:"))

  (add-element `((p) ((b) ((i) ,*utterance*))))

  (add-element '((p) "It has been annotated with the following procedural semantic representation, which corresponds to a query that answers the question given an image. For an explanation about this meaning representation, see "((a :href "https://ehai.ai.vub.ac.be/assets/pdfs/ccxg-for-vqa.pdf") "here") "."))

  (add-element (predicate-network->svg *meaning*)) ;;INSTANTIATE VARIABLES????????
  
  (add-element '((hr))))

;; (introduction)

(defun comprehension ()
  (add-element '((a :name "2")))
  (add-element '((h2) "2. Comprehension"))
  (add-element '((p) "In this section, we demonstrate the impact of the neural heuristics on the comprehension process. The FCG web interface contains the following parts: the initial transient structure, the construction application process, the resulting transient structure and finally the resulting meaning representation. Note that many of the boxes will reveal more information when you click on them. More information on how to use this demo can be found "((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "in the web demonstration guide.")))

  (add-element '((p) "The baseline search strategy in FCG is depth-first search with backtracking, duplicate detection and hashing. Backtracking occurs when a node has been fully expanded and does not satisfy all goal tests that have been specified for the current direction of processing (i.e. comprehension here). In the case of the CLEVR grammar the comprehension goal tests are: "))
  (add-element '((ol)
                 ((li) "No applicable constructions")
                 ((li) "Connected semantic network")
                 ((li) "Connected syntactic structure")
                 ((li) "No strings in input unit (called root in this web demo)")))
  (add-element '((p) "As soon as all four goal tests succeed, the search process will halt and the final transient structure is returned as the solution. The resulting meaning representation is then extracted from this transient structure and displayed."))

  (add-element '((hr)))
  (add-element '((a :name "21")))
  (add-element '((h3) ((b) "2.1 Depth-first strategy")))
    
  (add-element '((p) "We will first demonstrate the comprehension process using the baseline depth-first search algorithm with backtracking, duplicate detection and hashing. The construction application process is visualised here below in the form of a search tree with nodes (green boxes) that get expanded from left to right on the screen. After the initial application of a series of morphological and lexical constructions, the first split in the search tree occurs after node 15. What happens here is that the same construction can apply to different parts of the transient structure. The algorithm then chooses to expand the first of the three children and to explore that branch in a depth-first manner."))

  (add-element '((p) "Scrolling further right, you will see that the search tree becomes wider. These are the areas where backtracking took place. This means that all goal tests failed on a certain leaf node, so that it could not be returned as a solution. In that case, potential sister nodes of that node are explored, before we move higher up to aunts, great aunts, etc. You can follow the exact order in which the search tree was explored by inspecting the numbers that precede the label of every node. The first number is the node's id, reflecting the relative moment at which it was created, whereas the second number is the node's score, which is in the case of the depth-first search algorithm equal to the depth of the node in the tree. You can click on the '+' sign in the tree to unfold branches that contain duplicate nodes. Sometimes you will see that a number of sequential nodes have been grouped together into one chunk (indicated by an asterisk). You can unfold these by clicking on the chunk's label. "))

  (add-element `((p) ,(format nil "The solution node is visualised in dark green with a boldface label. The first number in the label again indicates the number of nodes that had to be created in order to get to the solution. You can keep this number in mind to compare it against the result that we obtain using the neural strategy. The second number indicates the depth of the solution, i.e. the number of construction applications in the solution branch, in this case ~a." (length (applied-constructions *baseline-solution-comprehension*)))))

  (add-element '((h4) ((b) "Initial transient structure:")))
  (add-element (make-html-fcg-light (initial-cfs (cip *baseline-solution-comprehension*)) :configuration (visualization-configuration *fcg-constructions*) :feature-types (feature-types *fcg-constructions*)))

  (add-element '((h4) ((b) "Search process:")))
  (add-element (make-html-fcg-light (top-node (cip *baseline-solution-comprehension*)) :configuration (visualization-configuration *fcg-constructions*)))

  (add-element '((h4) ((b) "Final transient structure (solution):")))
  (add-element (make-html-fcg-light (car-resulting-cfs (cipn-car *baseline-solution-comprehension*)) :configuration (visualization-configuration *fcg-constructions*) :feature-types (feature-types *fcg-constructions*)))

  (add-element '((h4) ((b) "Resulting meaning representation:")))
  (add-element (predicate-network->svg *meaning*))
  
  (add-element '((hr)))
  (add-element '((a :name "22")))
  (add-element '((h3) ((b) "2.2 Neural strategy")))
  (add-element '((p) "This section demonstrates the impact of the neural heuristics on the search process that is involved in constructional language processing. Scrolling to the right of the screen, you will see that there was no backtracking needed to find a solution thanks to the neural heuristics that guided the construction application process. After the hashed constructions have been processed, starting from transient structure 16, the (comprehension) sequence-to-sequence model was queried every time a new transient structure needed to be expanded. Given the input utterance and the sequence of the names of constructions that already applied, our neural model returns the most likely construction to apply next, together with a probability score. The new transient structure that results from the application of the predicted construction receives a heuristic value, which is the sum of the mother's heuristic value and the probability score."))

  (add-element '((p) "The only splits in the search tree occur when the same predicted construction can apply in multiple ways to a transient structure. The main reason as to why there was no backtracking needed here, lies in the early selection of the " ((tt) "the-same-t-as-compare-cxn")" (in node 16). Our sequence-to-sequence model learned that applying this construction even before all nominal constructions is indeed a good strategy to find a solution in a fast way. If you scroll up again to the baseline construction application process, you will see that this construction had a number of competitor constructions, including " ((tt) "the-same-t-as-relate-cxn") " and " ((tt) "the-same-t-cxn") "."))

  (add-element `((p) ,(format nil "If we compare the number of nodes that were created before finding a solution in the case of the baseline strategy and the neural strategy, we can see that instead of ~a nodes for the baseline strategy, the neural strategy only required ~a nodes."
                              (created-at *baseline-solution-comprehension*)
                              (created-at *neural-solution-comprehension*))))

  (add-element '((h4) ((b) "Initial transient structure:")))
  (add-element (make-html-fcg-light (initial-cfs (cip *neural-solution-comprehension*)) :configuration (visualization-configuration *fcg-constructions*) :feature-types (feature-types *fcg-constructions*)))

  (add-element '((h4) ((b) "Search process:")))
  (add-element (make-html-fcg-light (top-node (cip *neural-solution-comprehension*)) :configuration (visualization-configuration *fcg-constructions*)))

  (add-element '((h4) ((b) "Final transient structure (solution):")))
  (add-element (make-html-fcg-light (car-resulting-cfs (cipn-car *neural-solution-comprehension*)) :configuration (visualization-configuration *fcg-constructions*) :feature-types (feature-types *fcg-constructions*)))

  (add-element '((h4) ((b) "Resulting meaning representation:")))
  (add-element (predicate-network->svg *meaning*))
  
  (add-element '((hr)))

  
  )

(clear-page)

;; (comprehension)


(defun production ()
  (add-element '((a :name "3")))
  (add-element '((h2) "3. Production"))

  (add-element '((p) "In this section, we demonstrate the impact of the neural heuristics on the production process. The FCG web interface contains the following parts: the initial transient structure, the construction application process, the resulting transient structure and finally the utterance that has been constructed based on the form predicates that were present in the final transient structure."))

  (add-element '((p) "The importance of good heuristics is even more important in the production direction. Whereas in comprehension, a search tree comprises on average more than 3.5 times the number of construction applications that was needed to find a solution, this number grows to more than 29 in production."))


  (add-element '((p) "Note that the production process can yield many different utterances that adequately express the input meaning representation, including but not limited to the ones below. Apart from lexical variation (e.g. \"big\" as a synonym for \"large\", there is syntactic variation as well (e.g. \"there is a X ; is it Y ?\" vs. \"is the X made of Y ?\")."))
  (add-element `((ol)
                 ((li) "&quot;",(format nil "~{~a~^ ~}" '("there is a" "big" "yellow" "sphere" ";" "is it" "the same" "material" "as" "the" "ball" "that is" "in front of" "the" "small" "blue" "object")) "?&quot;")
                 ((li) "&quot;",(format nil "~{~a~^ ~}" '("there is a" "big" "yellow" "sphere" ";" "is it" "the same" "material" "as" "the" "ball" "that is" "in front of" "the" "tiny" "blue" "object")) "?&quot;")
                 
                 ((li) "&quot;",(format nil "~{~a~^ ~}" '("there is a" "big" "yellow" "sphere" ";" "does it have" "the same" "material" "as" "the" "ball" "that is" "in front of" "the" "small" "blue" "object")) "?&quot;")
                 ((li) "&quot;",(format nil "~{~a~^ ~}" '("there is a" "large" "yellow" "sphere" ";" "is its" "material" "the same as" "the" "sphere" "that is" "in front of" "the" "tiny" "blue" "thing")) "?&quot;")
                 ((li) "&quot;",(format nil "~{~a~^ ~}" '("there is a" "big" "yellow" "sphere" ";" "is it" "the same" "material" "as" "the" "ball" "in front of" "the" "small" "blue" "thing")) "?&quot;")
                 ((li) "&quot;",(format nil "~{~a~^ ~}" '("there is a" "big" "yellow" "sphere" ";" "does it have" "the same" "material" "as" "the" "ball" "in front of" "the" "small" "blue" "object")) "?&quot;")
                 ((li) "&quot;",(format nil "~{~a~^ ~}" '("is" "the" "large" "yellow" "sphere" "made of" "the same" "material" "as" "the" "ball" "that is" "in front of" "the" "small" "blue" "thing")) "?&quot;")
                 ((li) "&quot;",(format nil "~{~a~^ ~}" '("does" "the" "large" "yellow" "sphere" "have" "the same" "material" "as" "the" "ball" "that is" "in front of" "the" "small" "blue" "object")) "?&quot;")
                 ((li) "&quot;",(format nil "~{~a~^ ~}" '("is" "the" "big" "yellow" "sphere" "the same" "material" "as" "the" "ball" "that is" "in front of" "the" "small" "blue" "thing")) "?&quot;")
                 ((li) "&quot;",(format nil "~{~a~^ ~}" '("does" "the" "big" "yellow" "ball" "have" "the same" "material" "as" "the" "ball" "in front of" "the" "tiny" "blue" "thing")) "?&quot;")
                 ((li) "&quot;",(format nil "~{~a~^ ~}" '("is" "the" "big" "yellow" "sphere" "made of" "the same" "material" "as" "the" "sphere" "in front of" "the" "tiny" "blue" "thing")) "?&quot;")
                 ((li) "&quot;",(format nil "~{~a~^ ~}" '("is" "the" "large" "yellow" "sphere" "the same" "material" "as" "the" "ball" "in front of" "the" "small" "blue" "object")) "?&quot;")
                 
                 ))
                
      
  
  (add-element '((hr)))
  (add-element '((a :name "31")))
  (add-element '((h3) ((b) "3.1 Depth-first strategy")))
    
  (add-element `((p) "Let us first get an idea of the size of the search space using the depth-first baseline strategy with duplicate detection, backtracking and hashing. The solution is found here after a long exploration of branches that were initiated as a result of the application of the " ((tt) "has-what-t-cxn") ,(format nil ". Once we have backtracked over this choice point, there is more search evoked by a number of different determination constructions that can apply in a mutually exclusive manner (\"there is a X\" vs. \"the X\"). The solution is ultimately found at depth ~a in the search tree after ~a construction application operations have taken place."
                                                                                                                                                                      (length (all-parents *baseline-solution-production*))
                                                                                                                                                                      (created-at *baseline-solution-production*))))

  (add-element '((h4) ((b) "Initial transient structure:")))
  (add-element (make-html-fcg-light (initial-cfs (cip *baseline-solution-production*)) :configuration (visualization-configuration *fcg-constructions*) :feature-types (feature-types *fcg-constructions*)))

  (add-element '((h4) ((b) "Search process:")))
  (add-element (make-html-fcg-light (top-node (cip *baseline-solution-production*))
                                    :configuration (visualization-configuration *fcg-constructions*)))

  (add-element '((h4) ((b) "Final transient structure (solution):")))
  (add-element (make-html-fcg-light (car-resulting-cfs (cipn-car *baseline-solution-production*)) :configuration (visualization-configuration *fcg-constructions*) :feature-types (feature-types *fcg-constructions*)))

  (add-element '((h4) ((b) "Resulting utterance:")))
  (add-element `((h3) "&quot;",(format nil "~{~a~^ ~}" (render (car-resulting-cfs (cipn-car *baseline-solution-production*)) (get-configuration *fcg-constructions* :render-mode) :node *baseline-solution-production*)) "&quot;"))
  
  (add-element '((hr)))
  (add-element '((a :name "32")))
  (add-element '((h3) ((b) "3.2 Neural strategy")))
  (add-element `((p) ,(format nil "When we now compare the baseline search strategy with the neural heuristic search, we see that there was no backtracking needed in the example production process depicted here below. The solution is found after only ~a construction applications instead of ~a. If we take a closer look at what the sequence-to-sequence model seems to have learnt, we see that it first builds determined noun phrases (after having constructed nominal units) before any interrogative constructions are tried."

  (created-at *neural-solution-production*)
  (created-at *baseline-solution-production*))))

  (add-element '((h4) ((b) "Initial transient structure:")))
  (add-element (make-html-fcg-light (initial-cfs (cip *neural-solution-production*)) :configuration (visualization-configuration *fcg-constructions*) :feature-types (feature-types *fcg-constructions*)))

  (add-element '((h4) ((b) "Search process:")))
  (add-element (make-html-fcg-light (top-node (cip *neural-solution-production*)) :configuration (visualization-configuration *fcg-constructions*)))

  (add-element '((h4) ((b) "Final transient structure (solution):")))
  (add-element (make-html-fcg-light (car-resulting-cfs (cipn-car *neural-solution-production*)) :configuration (visualization-configuration *fcg-constructions*) :feature-types (feature-types *fcg-constructions*)))

  (add-element '((h4) ((b) "Resulting utterance:")))
  (add-element `((h3) "&quot;",(format nil "~{~a~^ ~}" (render (car-resulting-cfs (cipn-car *neural-solution-production*)) (get-configuration *fcg-constructions* :render-mode) :node *neural-solution-production*)) "&quot;")))

;(production)
  
  



(clear-page)
(create-static-html-page "Neural Heuristics for CCxG"
  (progn
    (header-page)
    (introduction)
    (comprehension)
    (production)))
