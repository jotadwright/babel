;;------------------------------------------------------------;;
;; Human Interpretable Grounded Language Processing: web demo ;;
;;------------------------------------------------------------;;
;; Liesbet De Vos, September 2022

(ql:quickload :mwm-evaluation)
(in-package :mwm-evaluation)
(ql:quickload :clevr-evaluation)

;; Larger font for text in <p> tags
(define-css 'main
            "p {font-size: 10pt}")

(defun header ()
  (clear-page)
  (deactivate-all-monitors) ; just to be sure that there is no other stuff 
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl)
  (activate-monitor mwm::trace-interaction-in-web-interface)
  (add-element '((hr)))
  (add-element
   '((h1) "Human-Interpretable Grounded Language Processing"))
  (add-element '((p) "This web demonstration accompanies the paper:"))
  (add-element '((p)"Human-Interpretable Grounded Concept Learning. De Vos, L, Nevens, J, Van Eecke, P, and Beuls, K. In preparation"))
  (add-element '((p) "Explanations on how to use this demo can be found " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))
  (add-element '((h2) "Abstract"))
  (add-element '((p) "Grounded language processing is a central component in many artificial intelligence systems as it allows agents to communicate about their physical surroundings. Given its importance, tasks involving this issue are researched extensively, typically using deep learning techniques that perform end-to- end mappings between natural language expressions and representations grounded in the environment. Although these techniques achieve high levels of accuracy, they are often criticised for their reliance on large amounts of training data, their closed nature, and their lack of interpretability. As an alternative, we propose a fully explainable, data efficient architecture for open-ended grounded language processing. The architecture is based on two main components. The first component comprises an inventory of human-interpretable concepts learned through task-based communicative in- teractions. These concepts connect the continuous sensorimotor experiences of an agent to meaningful symbols that can be used for reasoning operations. The second component concerns a computational construction grammar that maps between natural language expressions and procedural semantic representations. These representations are grounded in the environment through their integration with the learned inventory of human-interpretable concepts. We validate the architecture using a variation on the CLEVR visual question answering benchmark and achieve an accuracy of 96%. Our architecture and experiments demonstrate that the integration of a compu- tational construction grammar with an inventory of explainable grounded concepts can effectively achieve fully human-interpretable grounded language processing."))

  (add-element '((hr)))
  (add-element '((p) "This demonstration has the following parts:"))
  (add-element '((h3)  ((a :href "#section-1") "I. Discrimination-based grounded concept learning")))
  (add-element '((h3)  ((a :href "#section-2") "II. A computational construction grammar for VQA")))
  (add-element '((h3)  ((a :href "#section-3") "III. Executing IRL networks using grounded concepts")))
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
  (add-element '((h2 :id "section-1") "I. Discrimination-based concept learning"))
  (add-element '((p) "To perform grounded language processing, an inventory of concepts in necessary that bridges the gap between the continuous and symbolic domain. Our architecture employs a mechanism for grounded concept learning that was introduced by Nevens et al. (2020) and uses the language game methodology (Steels, 2001). In this framework, a population of agents engages in task-oriented communicative interactions until they converge on a shared communication system through selection and self-organisation (Steels, 2001). In the context of concept learning for grounded language processing, Nevens et al. (2020) propose a scenario with only two agents: a tutor and a learner. While the tutor already has an inventory of concepts in place, the learner does not and begins the learning process with an empty inventory. The goal of the learner is to fill this inventory with concepts through interaction with the tutor."))
  (make-baseline-simulated-experiments)
  (make-baseline-extracted-experiments)
  )

;(section-1)

;;-----------------------------------------------------------------------------;;
;; II. Semantic analysis of questions into procedural semantic representations ;;
;;-----------------------------------------------------------------------------;;

(defun section-2 ()
  (add-element '((h2 :id "section-2") "II. Semantic analysis of questions into procedural semantic representations"))
  (add-element '((p) "Before being able to ground concepts into the physical environment and achieve grounded language processing, autonomous agents must analyse the natural language input presented to them in terms of its semantic structure. Given its human-interpretable nature, linguistically motivated processing, and ability to output procedural semantic representations immediately, we integrate the computational construction grammar from (Nevens et al., 2019) into our grounded language processing architecture."))
  (add-element '((h3) "In this section, we demonstrate the comprehension process for the question 'what color is the small sphere' from the CLEVR dataset. The FCG web interface contains the following parts: the initial transient structure, the construction application process, a list of applied constructions, the resulting transient structure and finally the semantic representation. Note that many of the boxes will reveil more information when you click on them."))
  (comprehend "What color is the small sphere?")
   (add-element '((hr)))
  )


;;----------------------------------------------------;;
;; III. Executing IRL network using grounded concepts ;;
;;----------------------------------------------------;;


(defun section-3 ()
  (add-element '((h2 :id "section-3") "III. Executing IRL network using grounded concepts"))
  (add-element '((p) "The final goal of grounded language processing is to retrieve the grounded meaning of a natural language expression. This is achieved when all variables of that expression's semantic network are bound, including the target variable. To perform this process, we use the framework of Incremental Recruitment Language (IRL)(Spranger et al. 2010)"))
  (add-element '((h3) "This section shows an example of how a meaning-network can be executed using IRL and an inventory of human-interpretable grounded concepts."))
  (deactivate-all-monitors)
  (activate-monitor trace-irl)
  (test-utterance-in-scene "What color is the small sphere?" "CLEVR_val_000003" :simulated "serie-1")
  )


;;-------------------------------------------------------------------------------;;
;; IV. An integrated system for human-interpretable grounded language processing ;;
;;-------------------------------------------------------------------------------;;

(defun section-4 ()
  (activate-monitor trace-fcg)
  (activate-monitor mwm::trace-interaction-in-web-interface)
  (activate-monitor trace-irl)
  (add-element '((h2 :id "section-3") "III. An integrated system for human-interpretable grounded language processing"))
  (add-element '((h3) "This section demonstrates how the integrated system answers a couple of questions about images in the CLEVR-dataset."))
  (add-element '((p) "First, we ask the question 'what is the size of the red object?' about the following scene:"))
  (add-element '((img :src "/Users/liesbetdevos/Projects/Corpora/CLEVR-v1.0/images/val/CLEVR_val_000017.png" :width "40%")))
  (test-utterance-in-scene "What is the size of the red object?" "CLEVR_val_000017" :simulated "serie-1")
  (add-element '((p) "Then, we ask the question 'How many yellow objects are there?' about the following scene:"))
  (add-element '((img :src "/Users/liesbetdevos/Projects/Corpora/CLEVR-v1.0/images/val/CLEVR_val_000008.png" :width "40%")))
  (test-utterance-in-scene "How many yellow objects are there?" "CLEVR_val_000008" :extracted "serie-1")
  (add-element '((p) "Lastly, we ask the question 'What is the color of the cylinder?' about the following scene:"))
  (add-element '((img :src "/Users/liesbetdevos/Projects/Corpora/CLEVR-v1.0/images/val/CLEVR_val_000095.png" :width "40%")))
  (add-element '((p) "Note that this is a trick question that does not have an unambiguous answer (there are two cylinders in the image). An advantage of our architecture is that it can pick up on this problem and choose to not give a solution. This contrasts with deep-learning techniques, that will always try to come up with an answer."))
  (test-utterance-in-scene "What is the color of the cylinder?" "CLEVR_val_000095" :extracted "serie-1")
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