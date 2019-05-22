;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLEVR Grammar Web Demo ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jens Nevens, May 2019

(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)

;; Larger font for text in <p> tags
(define-css 'main "p {font-size: 11pt}")

(defun header ()
  (clear-page)
  (deactivate-all-monitors) ; just to be sure that there is no other stuff 
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl-in-web-browser)
  (activate-monitor clevr-web-monitor)
  (add-element '((hr)))
  (add-element
   '((h1) "Computational Construction Grammar for Visual Question Answering"))
  (add-element '((p) "This web demonstration accompanies the paper:"))
  (add-element '((p) "Nevens, J., Van Eecke, P. &amp; Beuls, K. (2019). "((a :href "#") "Computational Construction Grammar for Visual Question Answering.")" "((i) " Submitted to Linguistics Vanguard") "."))
  (add-element '((p) "Explanations on how to use this demo can be found " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))
  (add-element '((hr)))
  (add-element '((p) "This demonstration has the following parts:"))
  (add-element '((h3)  ((a :href "#section-1") "I. The CLEVR Grammar")))
  (add-element '((h3)  ((a :href "#section-2") "II. Comprehension")))
  (add-element '((h3)  ((a :href "#section-3") "III. Formulation")))
  (add-element '((h3)  ((a :href "#section-4") "IV. Operational VQA system")))
  (add-element '((p :style "color:darkred") "NOTE: It is recommended to use Firefox to optimally explore the contents of this page."))
  (add-element '((hr))))

;(header)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; I. The CLEVR Grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun section-1 ()
  (add-element '((h2 :id "section-1") "I. The CLEVR Grammar"))
  (add-element '((p) "This section provides a complete specification of the CLEVR Grammar. In total, the grammar consists of 170 constructions, 55 of which are morphological or lexical constructions. The remaining 115 constructions collaboratively capture the grammatical structures that are used in the dataset, e.g. noun phrases, prepositional phrases and a wide variety of interrogative structures."))
  (add-element '((p) "The complete construction inventory of the CLEVR Grammar is shown below. Every construction can be further expanded by clicking on it. To further explore the constructions of the CLEVR Grammar, a search box is given at the top of the construction inventory. To use it, first enter the name of a construction, e.g. \"cube-morph-cxn\". When clicking 'Search', the search result will be shown below the construction inventory."))
  (add-element (make-html *CLEVR* :expand-initially t))
  )

;(section-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; II. Comprehension
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun section-2 ()
  (add-element '((h2 :id "section-2") "II. Comprehension"))
  (add-element '((p) "In this section, we demonstrate the comprehension process for different questions from the CLEVR dataset. The FCG web interface contains the following parts: the initial transient structure, the construction application process, a list of applied constructions, the resulting transient structure and finally the semantic representation. Note that many of the boxes will reveil more information when you click on them. More information on how to use this demo can be found in the " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "web demonstration guide") "."))
  
  (add-element '((h3) "Example 1"))
  (add-element '((p) "As a first example, we demonstrate the comprehension process on the example sentence that is used throughout the paper: " ((i) "\"What material is the red cube?\"") "."))
  (comprehend (preprocess-sentence "What material is the red cube?"))

  (add-element '((h3) "Example 2"))
  (add-element '((p) "The first example is still rather small, containing only five predicates. In this example, we take a more complex question: " ((i) "\"What number of red cubes are the same size as the blue ball?\"") "."))
  (comprehend (preprocess-sentence "What number of red cubes are the same size as the blue ball?"))

  (add-element '((h3) "Example 3"))
  (add-element '((p) "In this third example, we show that the meaning representation does not necessarily have to be a sequence of predicates. It can also be a tree-structure, as demonstrated by the question " ((i) "\"Are there an equal number of blue things and green balls?\"") "."))
  (comprehend (preprocess-sentence "Are there an equal number of blue things and green balls?"))

  (add-element '((h3) "Example 4"))
  (add-element '((p) "There are many different question types in the CLEVR dataset. In this fourth and final example, we demonstrate yet another type of question: " ((i) "\"Do the large metal cube left of the red thing and the small cylinder have the same color?\"") "."))
  (comprehend (preprocess-sentence "Do the large metal cube left of the red thing and the small cylinder have the same color?"))
  nil
  )

;(section-2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; III. Formulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun section-3 ()
  (add-element '((h2 :id "section-3") "III. Formulation"))
  (add-element '((p) "Fluid Construction Grammar is a bidirectional formalism. It allows to map utterances to meanings, but also meanings to utterances. This is also true for the CLEVR Grammar. In this section, we demonstrate this formulation process. What is noteworthy, because of the design of the CLEVR dataset, is that multiple questions map to the same semantic representation, e.g. the questions \"What material is the red cube?\" and \"There is a red cube; what is its material?\". Using FCG's 'formulate-all' operation, which explores the entire search space instead of stopping at the first solution, we can show all different questions obtained from a single input meaning. We demonstrate this using the semantic representation of the example sentence " ((i) "\"What material is the red cube?\"")))
  (formulate-all '((get-context ?context)
                   (filter ?cube-set ?context ?shape-1)
                   (filter ?red-cube-set ?cube-set ?color-1)
                   (unique ?red-cube ?red-cube-set)
                   (query ?target ?red-cube ?attribute-1)
                   (bind shape-category ?shape-1 cube)
                   (bind color-category ?color-1 red)
                   (bind attribute-category ?attribute-1 material)))
  (add-element '((p) "The 'formulate-all' operation for this example returns six possible questions. In theory, there are many more possible solutions because of the large amount of synonyms for the different nouns and adjectives. A cube can also be a block, metal can also be shiny and so on. To avoid an enormous search space, the CLEVR Grammar was configured to only explore the grammatical variation and ignore this lexical variation."))
  )

;(section-3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; IV. Evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun section-4 ()
  (add-element '((h2 :id "section-4") "IV. Operational VQA system"))
  (add-element '((p) "In order to have a fully operational VQA system, the semantic representation that is the result of FCG's comprehension operation needs to be executed. In particular, it needs to be executed on a specific scene of objects. For this, we use a procedural semantics framework called Incremental Recruitment Language (IRL). In this section, we demonstrate both the comprehension process of an input question and the subsequent execution process of the resulting semantic representation on a scene of objects. The scene of objects is shown in the image below."))
  ;(add-element '((img :src "http://localhost/~jensnevens/example1080.png" :width "40%")))
  (add-element '((img :src "./example1080.png" :width "40%")))
  (let ((meaning (comprehend (preprocess-sentence "What material is the red cube?")))
        (scene (make-instance 'clevr-object-set :id 'clevr-context
                 :objects (list (make-instance 'clevr-object :shape 'cube :size 'large :color 'yellow :material 'metal)
                                (make-instance 'clevr-object :shape 'cylinder :size 'large :color 'purple :material 'rubber)
                                (make-instance 'clevr-object :shape 'cube :size 'large :color 'yellow :material 'metal)
                                (make-instance 'clevr-object :shape 'cylinder :size 'large :color 'cyan :material 'rubber)
                                (make-instance 'clevr-object :shape 'sphere :size 'large :color 'red :material 'metal)
                                (make-instance 'clevr-object :shape 'cube :size 'small :color 'brown :material 'metal)
                                (make-instance 'clevr-object :shape 'cube :size 'small :color 'red :material 'metal)
                                (make-instance 'clevr-object :shape 'cylinder :size 'small :color 'red :material 'metal)
                                (make-instance 'clevr-object :shape 'sphere :size 'small :color 'blue :material 'rubber)
                                (make-instance 'clevr-object :shape 'sphere :size 'small :color 'green :material 'rubber)))))
    (set-data *clevr-ontology* 'clevr-context scene)
    (evaluate-irl-program meaning *clevr-ontology*))
  nil
  )

;(section-4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Full Demo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun full-demo ()
  (header)
  (section-1)
  (section-2)
  (section-3)
  (section-4)
  )

;(full-demo)


;;;; Static web page
; (web-interface:create-static-html-page "clevr-grammar" (full-demo))