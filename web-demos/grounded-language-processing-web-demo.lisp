;;------------------------------------------------------------;;
;; Human Interpretable Grounded Language Processing: web demo ;;
;;------------------------------------------------------------;;
;; Liesbet De Vos, September 2022

(ql:quickload :mwm-evaluation)
(in-package :mwm-evaluation)


;; Larger font for text in <p> tags
(define-css 'main
            "p {font-size: 10pt}")

(defun header ()
  (clear-page)
  (deactivate-all-monitors) ; just to be sure that there is no other stuff 
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl)
  (activate-monitor mwm::trace-interaction-in-web-interface)
  (activate-monitor mwm::display-communicative-success)
  (add-element '((hr)))
  (add-element
   '((h1) "Human-Interpretable Grounded Language Processing"))
  (add-element '((p) "This web demonstration accompanies the paper:"))
  (add-element '((p)"***Add reference here***"))
  (add-element '((p) "Explanations on how to use this demo can be found " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))
  (add-element '((hr)))
  (add-element '((p) "This demonstration has the following parts:"))
  (add-element '((h3)  ((a :href "#section-1") "I. Tutor-learner interactions for grounded concept learning")))
  (add-element '((h3)  ((a :href "#section-2") "II. Semantic analysis of questions into procedural semantic representations")))
  (add-element '((h3)  ((a :href "#section-3") "III. Executing IRL network using grounded concepts")))
  (add-element '((h3)  ((a :href "#section-4") "IV. An integrated system for human-interpretable grounded language processing")))
  (add-element '((p :style "color:darkred") "NOTE: It is recommended to use Firefox to optimally explore the contents of this page."))
  (add-element '((hr))))

;(header)

;;------------------------------------------------------------;;
;; I. Tutor-learner interaction for grounded concept learning ;;
;;------------------------------------------------------------;;

(defun all-words-known-p (experiment)
    (= (length (mwm::lexicon (mwm::learner experiment))) 19))

(defun run-experiments-untill-all-words-known (experiment)
  (loop while (not (all-words-known-p experiment))
        do (run-interaction experiment)))

(defun make-baseline-simulated-experiments ()
  (let* ((config (make-configuration
                  :entries `((:data-source . :clevr)
                             (:scale-world . ,nil)
                             (:category-representation . :prototype)
                             (:determine-interacting-agents-mode . :tutor-speaks)
                             (:data-sets . ("val")))))
         (experiment (make-instance 'mwm::mwm-experiment :configuration config)))
    (add-element '((h3) "Baseline Simulated Experiment"))
    (activate-monitor mwm::trace-interaction-in-web-interface)
    (add-element '((p) "At the start, the learner has no repertoire of concepts. The word the tutor utters in the very first interaction is thus always unknown. The learner indicates that it does not know the word and the tutor provides feedback by pointing to the indended topic. Now, the learner creates its first concept, simply by storing an exact copy of the topic object. Indeed, the learner cannot yet know which attributes are important or what their prototypical values should be."))
    (run-interaction experiment)
    (deactivate-monitor mwm::trace-interaction-in-web-interface)
    (add-element '((p) "Now, we run a number of interactions untill the learner has acquired all 19 concepts."))
    (add-element '((p) ((b) "Running interactions...")))
    (run-experiments-untill-all-words-known experiment)
    (activate-monitor mwm::trace-interaction-in-web-interface)
    (add-element '((p) "If the learner does know the word, it will try to interpret this word in the current context. If the learner points to the tutor's intended topic, the interaction is a success. Independant of the success of the interaction, the tutor will indicate the intended topic at the end of the interaction. Now, the learner will compare this topic to the concept representation it used. First, the learner will slightly shift the prototypical value of all attributes towards the corresponding attribute values of the topic. Next, based on the notion of discrimination, the learner will decide which attributes to reward and punish. The former are indicated in green, the latter in red."))
    (run-interaction experiment)
    (deactivate-monitor mwm::trace-interaction-in-web-interface)
    (add-element '((p) "Finally, we run 1000 additional interactions and show the learner's lexicon."))
    (add-element '((p) ((b) "Running interactions...")))
    (run-series experiment 1000)
    (mwm::display-lexicon (mwm::learner experiment))
    (add-element '((p) ((a :href "#top") "Back to top")))
    ))

(defun make-baseline-extracted-experiments ()
  (let* ((config (make-configuration
                  :entries `((:data-source . :extracted)
                             (:scale-world . ,nil)
                             (:category-representation . :prototype)
                             (:determine-interacting-agents-mode . :tutor-speaks)
                             (:data-path . ,(merge-pathnames (make-pathname :directory '(:relative "CLEVR" "CLEVR-v1.0" "scenes" "val-ns-vqa"))
                                                             cl-user:*babel-corpora*)))))
         (experiment (make-instance 'mwm::mwm-experiment :configuration config)))
    (add-element '((h3) "Baseline Extracted Experiment"))
    (add-element '((p) "In the realistic environment, the continuous-valued attributes observed by the learner are obtained using computer vision techniques. More concretely, we use the Mask R-CNN object detection algorithm to detect and segment the objects directly from the input image. The object proposals returned by this algorithm are passed on to feature extractors, extracting various numeric attributes."))
    (add-element '((p) "Given that the object detection and feature extraction are not perfect, the scenes in this experiment are more noisy. This makes the task more difficult for the learner agent, which is also reflected in the communicative success. Below, we show the traces of the first and the 1000'th interaction. Also, we show the learner's concept repertoire after 1000 interactions."))
    (activate-monitor mwm::trace-interaction-in-web-interface)
    (run-interaction experiment)
    (deactivate-monitor mwm::trace-interaction-in-web-interface)
    (add-element '((p) ((b) "Running interactions...")))
    (run-series experiment 1000)
    (mwm::display-lexicon (mwm::learner experiment))
    (add-element '((p) ((a :href "#top") "Back to top")))
  ))

(defun section-1 ()
  (add-element '((h2 :id "section-1") "I. Tutor-learner interactions for grounded concept learning"))
  (make-baseline-simulated-experiments)
  (make-baseline-extracted-experiments)
  )

;(section-1)

;;-----------------------------------------------------------------------------;;
;; II. Semantic analysis of questions into procedural semantic representations ;;
;;-----------------------------------------------------------------------------;;

(defun section-2 ()
  (add-element '((h2 :id "section-2") "II. Semantic analysis of questions into procedural semantic representations"))
  (add-element '((h3) "In this section, we demonstrate the comprehension process for the question 'what color is the small sphere' from the CLEVR dataset. The FCG web interface contains the following parts: the initial transient structure, the construction application process, a list of applied constructions, the resulting transient structure and finally the semantic representation. Note that many of the boxes will reveil more information when you click on them."))
  (comprehend "What color is the small sphere?")
  )


;;----------------------------------------------------;;
;; III. Executing IRL network using grounded concepts ;;
;;----------------------------------------------------;;


(defun section-3 ()
  (add-element '((h2 :id "section-3") "III. Executing IRL network using grounded concepts"))
  (add-element '((h3) "This section shows an example of how a meaning-network can be executed using IRL and an inventory of human-interpretable grounded concepts."))
  (deactivate-all-monitors)
  (activate-monitor trace-irl)
  (test-utterance-in-scene "What color is the small sphere?" "CLEVR_val_000003" :simulated "serie-1")
  (test-utterance-in-scene "What color is the small sphere?" "CLEVR_val_000003" :extracted "serie-1")
  )


;;-------------------------------------------------------------------------------;;
;; IV. An integrated system for human-interpretable grounded language processing ;;
;;-------------------------------------------------------------------------------;;

(defun section-4 ()
  (activate-monitor trace-fcg)
  (activate-monitor mwm::trace-interaction-in-web-interface)
  (activate-monitor mwm::display-communicative-success)
  (activate-monitor trace-irl)
  (add-element '((h2 :id "section-3") "III. An integrated system for human-interpretable grounded language processing"))
  (add-element '((h3) "This section demonstrates how the integrated system answers one question about an image in the CLEVR-dataset ."))
  (test-utterance-in-scene "What is the size of the red object?" "CLEVR_val_000017" :simulated "serie-1")
    (test-utterance-in-scene "What is the size of the red object?" "CLEVR_val_000017" :extracted "serie-1")
  )


;;-----------;;
;; Full demo ;;
;;-----------;;


(defun full-demo ()
  (header)
  (section-1)
  (section-2)
  (section-3)
  (section-4)
  )

;(full-demo)


;;;; Static web page
; (web-interface:create-static-html-page "human-interpretable-grounded-language-processing" (full-demo))