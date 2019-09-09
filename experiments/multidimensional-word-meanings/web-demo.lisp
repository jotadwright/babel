;(ql:quickload :mwm)

(in-package :mwm)

(define-css 'main "p {font-size: 11pt}")

;; HEADER

(defun make-header ()
  (clear-page)
  (deactivate-all-monitors)
  (add-element '((hr)))
  (add-element
   '((h1 :id "top") "Interactive Learning of Grounded Concepts"))
  (add-element '((hr)))
  (add-element '((p) "This web demonstration accompanies the following paper:"))
  (add-element '((p) ((b) "From Continuous Observations to Symbolic Concepts: A Discrimination-based Strategy for Grounded Concept Learning. Nevens, J, Van Eecke, P, and Beuls, K. In preparation")))
  (add-element '((p) "The goal of this web demo is to provide more insight in the interaction script for the various grounded concept learning experiments. Here, you will find traces that are printed during experimental runs. The success measures, such as communicative success and repertoire size, can be found in the paper. They will not be repeated in this web demo."))
  (add-element '((hr)))
  (add-element '((p) "This demonstration has the following parts:"))
  (add-element '((h3)  ((a :href "#abstract") "I. Abstract")))
  (add-element '((h3)  ((a :href "#baseline") "II. Baseline Experiments")))
  (add-element '((h3)  ((a :href "#cogent") "III. Generalisation Experiments")))
  (add-element '((h3)  ((a :href "#incremental") "IV. Incremental Learning Experiments")))
  (add-element '((p :style "color:darkred") "NOTE: It is recommended to use Firefox to optimally explore the contents of this page."))
  (add-element '((hr)))
  )

;; ABSTRACT

(defun make-abstract ()
  (add-element '((h2 :id "abstract") "I. Abstract"))
  (add-element '((p) "Autonomous agents perceive the world through streams of continuous sensori-motor data. Yet, in order to reason and communicate about their environment, agents need to be able to distill meaningful concepts from their raw observations. Most current approaches bridge between the continuous and symbolic domain using deep learning techniques. While these approaches often achieve high levels of accuracy, they rely on large amounts of training data, and the resulting models lack transparency, generality and adaptivity. In this paper, we introduce a novel methodology for grounded concept learning. In a tutor-learner scenario, the method allows an agent to construct a conceptual system in which meaningful concepts are formed by discriminative combinations of prototypical values on human-interpretable feature channels. We evaluate our approach on the CLEVR dataset, using features that are either simulated or extracted using computer vision techniques. Through a range of experiments, we show that our method allows for incremental learning, needs few data points, and that the resulting concepts are general enough to be applied to previously unseen objects. These properties make the approach well-suited to be used in robotic agents as the module that maps from continuous sensori-motor input to grounded, symbolic concepts that can then be used for higher-level reasoning tasks."))
  )

;; HELPER FUNCTIONS

(defun all-words-known-p (experiment)
  (let ((learner (find 'learner (population experiment) :key #'id)))
    (= (length (constructions (grammar learner))) 19)))

(defun run-experiments-untill-all-words-known (experiment)
  (loop while (not (all-words-known-p experiment))
        do (run-interaction experiment)))

;; BASELINE EXPERIMENTS

(defun make-baseline-experiments ()
  (add-element '((h2 :id "baseline") "II. Baseline Experiments"))
  (add-element '((p) "The goal of these experiments is to validate the learning mechanisms. In a simulated environment, the agents should quickly achieve 100% communicative success. The realistic environment, using features extracted using computer vision techniques, is more difficult for the learner agent. Here, the agents are unable to achieve full communicative success. Note that in all experiments below, we use the 'prototype' representation."))
  (make-baseline-simulated-experiments)
  (make-baseline-extracted-experiments)
  )

(defun make-baseline-simulated-experiments ()
  (let* ((config (make-configuration
                  :entries `((:data-source . :clevr)
                             (:scale-world . ,nil)
                             (:category-representation . :prototype)
                             (:determine-interacting-agents-mode . :tutor-speaks)
                             (:data-sets . ("val")))))
         (experiment (make-instance 'mwm-experiment :configuration config)))
    (add-element '((h3) "Baseline Simulated Experiment"))
    (activate-monitor trace-interaction-in-web-interface)
    (add-element '((p) "At the start, the learner has no repertoire of concepts. The word the tutor utters in the very first interaction is thus always unknown. The learner indicates that it does not know the word and the tutor provides feedback by pointing to the indended topic. Now, the learner creates its first concept, simply by storing an exact copy of the topic object. Indeed, the learner cannot yet know which attributes are important or what their prototypical values should be."))
    (run-interaction experiment)
    (deactivate-monitor trace-interaction-in-web-interface)
    (add-element '((p) "Now, we run a number of interactions untill the learner has acquired all 19 concepts."))
    (add-element '((p) ((b) "Running interactions...")))
    (run-experiments-untill-all-words-known experiment)
    (activate-monitor trace-interaction-in-web-interface)
    (add-element '((p) "If the learner does know the word, it will try to interpret this word in the current context. If the learner points to the tutor's intended topic, the interaction is a success. Independant of the success of the interaction, the tutor will indicate the intended topic at the end of the interaction. Now, the learner will compare this topic to the concept representation it used. First, the learner will slightly shift the prototypical value of all attributes towards the corresponding attribute values of the topic. Next, based on the notion of discrimination, the learner will decide which attributes to reward and punish. The former are indicated in green, the latter in red."))
    (run-interaction experiment)
    (deactivate-monitor trace-interaction-in-web-interface)
    (add-element '((p) "Finally, we run 1000 additional interactions and show the learner's lexicon."))
    (add-element '((p) ((b) "Running interactions...")))
    (run-series experiment 1000)
    (display-lexicon (find 'learner (population experiment) :key #'id))
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
         (experiment (make-instance 'mwm-experiment :configuration config)))
    (add-element '((h3) "Baseline Extracted Experiment"))
    (add-element '((p) "In the realistic environment, the continuous-valued attributes observed by the learner are obtained using computer vision techniques. More concretely, we use the Mask R-CNN object detection algorithm to detect and segment the objects directly from the input image. The object proposals returned by this algorithm are passed on to feature extractors, extracting various numeric attributes."))
    (add-element '((p) "Given that the object detection and feature extraction are not perfect, the scenes in this experiment are more noisy. This makes the task more difficult for the learner agent, which is also reflected in the communicative success. Below, we show the traces of the first and the 1000'th interaction. Also, we show the learner's concept repertoire after 1000 interactions."))
    (activate-monitor trace-interaction-in-web-interface)
    (run-interaction experiment)
    (deactivate-monitor trace-interaction-in-web-interface)
    (add-element '((p) ((b) "Running interactions...")))
    (run-series experiment 1000)
    (display-lexicon (find 'learner (population experiment) :key #'id))
    (add-element '((p) ((a :href "#top") "Back to top")))
  ))

;; COGENT EXPERIMENTS

(defun make-cogent-experiments ()
  (add-element '((h2 :id "cogent") "III. Generalisation Experiments"))
  (add-element '((p) "The goal of these experiments is to show the generalisation capabilities of our concept learning approach. For this, we use the CLEVR CoGenT dataset. The learner first performs a number of interactions on condition A, after which the learning operators are turned off and the environment switches to condition B. Now, if the concepts acquired by the learner are bound to statistical distributions and co-occurences of condition A, the communicative success obtained by the learner would drop when switching from condition A to B. As we have demonstrated in the paper, this is not the case. From this, we conclude that the concepts are quite general and dataset-independant."))
  (make-cogent-simulated-experiment)
  (make-cogent-extracted-experiment)
  )

(defun make-cogent-simulated-experiment ()
  (add-element '((h3) "Generalisation Simulated Experiment"))
  (add-element '((p) ((a :href "#top") "Back to top")))
  )

(defun make-cogent-extracted-experiment ()
  (add-element '((h3) "Generalisation Extracted Experiment"))
  (add-element '((p) ((a :href "#top") "Back to top")))
  )

;; INCREMENTAL LEARNING EXPERIMENTS

(defun make-incremental-experiments ()
  (add-element '((h2 :id "incremental") "IV. Incremental learning Experiments"))
  (add-element '((p) "The goal of these experiments is to show the incremental learning capabilities of our approach. For this experiment, we created a custom dataset that consists of 3 conditions. In the first condition, there are only contains cubes. In the second condition spheres are added. The third and final condition contains all three types of shapes, as the original CLEVR dataset. Over the course of interactions, the environment transitions from one condition to the next. Here, we show that the learner agent can very quickly adapt to this changing environment. There is no need for complete or even partial retraining of the concepts, as adaptivity is integrated in the methodology and learning operators directly."))
  )

;; COMPLETE DEMO
(defun make-static-web-demo ()
  (make-header)
  (make-abstract)
  (make-baseline-experiments)
  (make-cogent-experiments)
  (make-incremental-experiments)
  )

;(make-static-web-demo)

;(web-interface:create-static-html-page "concept-learning" (make-static-web-demo))