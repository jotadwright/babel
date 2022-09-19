;;------------------------------------------------------------;;
;; Human Interpretable Grounded Language Processing: web demo ;;
;;------------------------------------------------------------;;
;; Liesbet De Vos, September 2022

(ql:quickload :mwm-evaluation)
(ql:quickload :mwm)
(in-package :mwm-evaluation)
(in-package :mwm)

;; Larger font for text in <p> tags
(define-css 'main
            "p {font-size: 10pt}")

(defun header ()
  (clear-page)
  (deactivate-all-monitors) ; just to be sure that there is no other stuff 
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl)
  (activate-monitor trace-interaction-in-web-interface)
  (activate-monitor display-communicative-success)
  (add-element '((hr)))
  (add-element
   '((h1) "Human-Interpretable Grounded Language Processing"))
  (add-element '((p) "This web demonstration accompanies the paper:"))
  (add-element '((p)"***Add reference here***"))
  (add-element '((p) "Explanations on how to use this demo can be found " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))
  (add-element '((hr)))
  (add-element '((p) "This demonstration has the following parts:"))
  (add-element '((h3)  ((a :href "#section-1") "I. Tutor-learner interaction for grounded concept learning")))
  (add-element '((h3)  ((a :href "#section-2") "II. Semantic analysis of questions into procedural semantic representations")))
  (add-element '((h3)  ((a :href "#section-3") "III. Executing IRL network using grounded concepts")))
  (add-element '((h3)  ((a :href "#section-4") "IV. An integrated system for human-interpretable grounded language processing")))
  (add-element '((p :style "color:darkred") "NOTE: It is recommended to use Firefox to optimally explore the contents of this page."))
  (add-element '((hr))))

;(header)

;;------------------------------------------------------------;;
;; I. Tutor-learner interaction for grounded concept learning ;;
;;------------------------------------------------------------;;

(defparameter *baseline-simulated*
  (make-configuration
   :entries '((:experiment-type . :baseline)
              (:world-type . :simulated)
              (:determine-interacting-agents-mode . :default)
              (:alignment-filter . :all)
              (:concept-history-length . 500))))

(defparameter *experiment*
  (make-instance 'mwm-experiment
                 :configuration *baseline-simulated*))



(defun section-1 ()
  (add-element '((h2 :id "section-1") "I. Tutor-learner interaction for grounded concept learning"))
  (add-element '((p) "This section gives an example of one interaction between the tutor and learner in the grounded concept learning game. The interaction follows .. steps that are executed below and explained here:"))
  (add-element '((p) "1. The roles of the tutor and learner are defined in the experiment. For a baseline interaction in the grounded concept learning game, the tutor is always speaker and the learner is hearer."))
  (add-element '((p) "2. A random CLEVR-scene is chosen from the dataset. The symbolic representation of this scene is shown under CLEVR context. Click 'view scene' to see a visual representation of this scene. "))
  (add-element '((p) "3. The symbolic CLEVR-scene is transformed into a representation containing continuous values. Click the scene number under MWM-context to visualise the continuous scene"))
  (add-element '((p) "4. Tutor chooses a topic-object from the scene."))
  (add-element '((p) "5. Tutor searches for a concept that discriminates the topic-object from the others in the scene. If no concept is found for the topic, a new one is chosen."))
  (add-element '((p) "6. Tutor produces the discriminative concept as an utterance"))
  (add-element '((p) "7. Learner tries to parse the utterance. He either interprets the concept if it is known to him, or learns it otherwise. In this example, the interaction will always fail as it is the first interaction and the learner starts the experiment without any concepts."))
  (add-element '((p) "8. Learner makes a new concept using the topic-object that was pointed to by the tutor"))
  (add-element '((p) "9. The communicative success of the interaction is shown. In this case, the interaction failed."))
  (run-interaction *experiment*)
  )

;(section-1)

;;-----------------------------------------------------------------------------;;
;; II. Semantic analysis of questions into procedural semantic representations ;;
;;-----------------------------------------------------------------------------;;

;;----------------------------------------------------;;
;; III. Executing IRL network using grounded concepts ;;
;;----------------------------------------------------;;

;;----------------------------------------------------;;
;; III. Executing IRL network using grounded concepts ;;
;;----------------------------------------------------;;



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