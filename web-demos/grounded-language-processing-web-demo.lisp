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
  (activate-monitor trace-interaction-in-web-interface)
  (add-element '((hr)))
  (add-element
   '((h1) "Human-Interpretable Grounded Language Processing"))
  (add-element '((p) "This web demonstration accompanies the paper:"))
  (add-element '((p)"Human-Interpretable Grounded Language Processing. De Vos, L, Nevens, J, Van Eecke, P, and Beuls, K. In preparation"))
  (add-element '((p) "Explanations on how to use this demo can be found " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))
  (add-element '((h2) "Abstract"))
  (add-element '((p) "Grounded language processing is a central component in many artificial intelligence systems as it allows agents to communicate about their physical surroundings. Given its importance, tasks involving this issue are researched extensively, typically using deep learning techniques that perform end-to- end mappings between natural language expressions and representations grounded in the environment. Although these techniques achieve high levels of accuracy, they are often criticised for their reliance on large amounts of training data, their closed nature, and their lack of interpretability. As an alternative, we propose a fully explainable, data efficient architecture for open-ended grounded language processing. The architecture is based on two main components. The first component comprises an inventory of human-interpretable concepts learned through task-based communicative in- teractions. These concepts connect the continuous sensorimotor experiences of an agent to meaningful symbols that can be used for reasoning operations. The second component concerns a computational construction grammar that maps between natural language expressions and procedural semantic representations. These representations are grounded in the environment through their integration with the learned inventory of human-interpretable concepts. We validate the architecture using a variation on the CLEVR visual question answering benchmark and achieve an accuracy of 96%. Our architecture and experiments demonstrate that the integration of a compu- tational construction grammar with an inventory of explainable grounded concepts can effectively achieve fully human-interpretable grounded language processing."))
  (add-element '((hr))))

;(header)

;;-------------------------------------------------------------------------------;;
;; IV. An integrated system for human-interpretable grounded language processing ;;
;;-------------------------------------------------------------------------------;;

(defun section-1 ()
  (activate-monitor trace-fcg)
  (activate-monitor mwm::trace-interaction-in-web-interface)
  (activate-monitor trace-irl)
  (add-element '((h3) "This section demonstrates how the integrated system answers a couple of questions about images in the CLEVR-dataset."))
  (add-element '((p) "First, we ask the question 'what is the size of the red object?' about the following scene:"))
  (add-element '((img :src "/Users/liesbetdevos/Projects/Corpora/CLEVR-v1.0/images/val/CLEVR_val_000017.png" :width "40%")))
  (test-utterance-in-scene "What is the size of the red object?" "CLEVR_val_000017" :simulated "serie-1")
  (add-element '((p) "Then, we ask the question 'How many yellow objects are there?' about the following scene:"))
  (add-element '((img :src "/Users/liesbetdevos/Projects/Corpora/CLEVR-v1.0/images/val/CLEVR_val_000008.png" :width "40%")))
  (test-utterance-in-scene "How many yellow objects are there?" "CLEVR_val_000008" :extracted "serie-1")
  (add-element '((p) "Lastly, we ask the question 'What is the color of the cylinder?' about the following scene:"))
  (add-element '((img :src "/Users/liesbetdevos/Projects/Corpora/CLEVR-v1.0/images/val/CLEVR_val_000095.png" :width "40%")))
  (add-element '((p) "Note that this is a trick question that does not have an answer (there are three cylinders in the image). An advantage of our architecture is that it can pick up on this problem and choose to not give a solution. This contrasts with deep-learning techniques, that will always try to come up with an answer."))
  (test-utterance-in-scene "What is the color of the cylinder?" "CLEVR_val_000095" :extracted "serie-1")
  )

;;-----------;;
;; Full demo ;;
;;-----------;;


(defun full-demo ()
  (header)
  (section-1)
  )

;(full-demo)


;;;; Static web page
; (web-interface:create-static-html-page "human-interpretable-grounded-language-processing" (full-demo))