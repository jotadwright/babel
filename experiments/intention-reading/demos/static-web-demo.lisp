
;;##################################################;;
;;                                                  ;;
;; Web demo 'intention reading and pattern finding' ;;
;;                                                  ;;
;;##################################################;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading the package and setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :clevr-learning)
(in-package :intention-reading)

(activate-monitor trace-fcg)
(activate-monitor trace-irl)
(activate-monitor trace-interactions-in-wi)

;;;; HELPER FUNCTION
;;;; ---------------

(defun load-file-data (file)
  "Function to load a specific data file
   for demo purposes."
  (let* ((file-data
          (with-open-file (stream file :direction :input)
            (read stream)))
         (question (first file-data))
         (meaning (second file-data))
         (scenes-and-answers (third file-data))
         (count-question-p
          (find 'count! meaning :key #'first))
         (usable-scenes-and-answers
          (if count-question-p
            (find-all-if-not #'(lambda (scene-answer-pair)
                                 (= 0 (cdr scene-answer-pair)))
                             scenes-and-answers)
            scenes-and-answers)))
    (cons question usable-scenes-and-answers)))

(defun cxn-with-meaning (cxn-inventory meaning)
  (loop for cxn in (constructions-list cxn-inventory)
        for cxn-meaning = (extract-meaning-predicates cxn)
        thereis (equivalent-irl-programs? cxn-meaning meaning)))

(defun run-interactions-until-cxn-with-meaning (experiment meaning)
  "Run interactions until the agent has learned a cxn with the given meaning"
  (loop
     do (run-interaction experiment)
     until (cxn-with-meaning (grammar (learner experiment)) meaning)))

(defun run-interactions-until-cxn-inventory-size (experiment n)
  "Run interactions until the agent's cxn inventory has size n"
  (loop 
     do (run-interaction experiment)
     until (length= (constructions-list (grammar (learner experiment))) n)))
        

;;;; EXPERIMENT
;;;; ----------

(defparameter *experiment*
   (make-instance 'clevr-learning-experiment
                  :entries '((:determine-interacting-agents-mode . :tutor-learner)
                             (:questions-per-challenge . 100)
                             (:scenes-per-question . 50)
                             (:confidence-threshold . 1.1)
                             (:tutor-sample-mode . :random)
                             (:cxn-decf-score . 0.4)
                             (:cxn-inhibit-score . 0.1)
                             (:primitives . :symbolic)
                             (:learner-cxn-supplier . :hashed-and-scored)
                             (:alignment-strategy . :lateral-inhibition)
                             (:hide-type-hierarchy . nil)
                             (:remove-cxn-on-lower-bound . t)
                             (:composer-strategy . :store-past-scenes)
                             (:th-link-repair-mode-comprehension . :no-path-required)
                             (:th-link-repair-mode-formulation . :path-required))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web demo                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-css 'text-box "div.text-box { word-wrap: break-word; width: 1000px; text-align: justify; text-justify: inter-word; }")

(defun header-page ()
  (clear-page)
  ;; Title
  (add-element '((hr)))
  (add-element '((a :name "top")))
  (add-element '((h1) "Language Acquisition through Intention Reading and Pattern Finding"))
  (add-element '((p) "This web demonstration accompanies the paper:")) 
  (add-element '((div :class "text-box") ((p) "Nevens, J., Doumen, J., Van Eecke, P. &amp; Beuls, K. (2022). "((a :href "" :target "_blank") "Language Acquisition through Intention Reading and Pattern Finding")". In "((i) "Proceedings of the 29th International Conference on Computational Linguistics") ".")))
  (add-element '((hr)))

  (add-element '((h2) "Introduction"))
  (add-element '((div :class "text-box") ((p) "This web demonstration provides concrete examples of the agent's learning mechanism (cf. Section 4.5). For the purposes of this demonstration, the experiment as described in this paper has been modified such that the tutor always acts as the speaker and the learner always acts as the listener. While the scenes are presented to the agents in random order, the questions are presented to best demonstrate the various learning mechanisms.")))
  (add-element '((div :class "text-box") ((p) "Below, you can find shortcuts to each of the learning mechanisms. Each section contains a brief description followed by a number of interactions that demonstrate the learning mechanism discussed.")))
  (add-element '((hr)))
  
  (add-element '((h2) "Contents"))
  (add-element '((h3) ((a :href "./holophrase/index.html") "1. Learning holophrases")))
  (add-element '((h3) ((a :href "./substitution/index.html") "2. Generalising over holophrases")))
  (add-element '((h3) ((a :href "./partialmeaning/index.html") "3. Learning from partial meanings")))
  ;(add-element '((h3 :style "text-indent: 25px;") ((a :href "#partiallexical") "3.1 Partial meaning provided by lexical construction(s)")))
  ;(add-element '((h3 :style "text-indent: 25px;") ((a :href "#partialitembased") "3.2 Partial meaning provided by item-based construction")))
  (add-element '((h3) ((a :href "./links/index.html") "4. Learning slot-argument links")))
  (add-element '((hr))))


(defun learn-holophrases ()
  (add-element '((h2) ((a :name "holophrase")) "1. Learning holophrases"))
  (add-element '((div :class "text-box") ((p) "We start with the very first interaction of the experiment. When observing the question, the only thing the learner can do is to use intention reading to create a meaning hypothesis and to learn a hollistic mapping between the observed question and the constructed meaning hypothesis. At this point, the learner cannot know which parts of the form correspond with which parts of the meaning.")))
  (add-element '((div :class "text-box") ((p) "In the interactions that follow, you will see that the agents are repeatedly presented with the same question. Many meaning hypotheses generated by intention reading are not adequate representations of the meaning of that question. Hence, they fail to generalise across scenes. When the agent reuses a previously acquired holophrase construction and fails to achieve communicative success, intention reading is used to make a new meaning hypothesis. Importantly, intention reading ensures that it does not generate exactly the same meaning hypothesis as before, as it turned out to be unsuccessful.")))
  (add-element '((hr)))

  ;; question_000210_len_005.lisp => "What material is the cyan cube?"
  (setf (question-data *experiment*)
        (list (load-file-data
               (merge-pathnames
                (make-pathname :directory '(:relative "stage-1")
                               :name "question_000210_len_005" :type "lisp")
                (get-configuration *experiment* :challenge-files-root)))))
  
  (run-interactions-until-cxn-with-meaning
   *experiment* '((get-context ?context)
                  (filter ?set-1 ?context ?shape-1)
                  (bind shape-category ?shape-1 cube)
                  (filter ?set-2 ?set-1 ?color-1)
                  (bind color-category ?color-1 cyan)
                  (unique ?object-1 ?set-2)
                  (query ?target ?object-1 ?attribute-1)
                  (bind attribute-category ?attribute-1 material)))
  (run-interactions-until-cxn-inventory-size *experiment* 1)

  (add-element '((div :class "text-box") ((p) "At this point, the construction inventory of the agent contains a single holophrase construction that covers the observed question. All other holophrase constructions failed to generalise across scenes. Because they were competitors of the single remaining holophrase construction, they were punished several times and eventually removed.")))
  (add-element '((p) ((a :href "#holophrase") "Back to top of this page.")))
  (add-element '((p) ((a :href "./../index.html") "Back to the main page.")))

  )


(defun substitution ()
  (add-element '((h2) ((a :name "substitution")) "2. Generalising over holophrases"))
  (add-element '((div :class "text-box") ((p) "In the next few interactions, the agents are presented with a different question. As before, the only thing the learner can do is create hollistic mappings between the observed question and the reconstructed meaning. However, most of these mappings will fail to generalise across scenes. At some point, the learner will find a form-meaning mapping that has a minimal difference with the previously acquired holophrase construction on both the form side and the meaning side. This is when the pattern finding mechanism that generalises over holophrases will come into play. The agent will capture the similarities in an item-based construction. This construction abstracts over the differences by providing open slots on both the form side and the meaning side. The agent also learns lexical constructions, for filling the item-based slots, and adds links to the categorial network. These links indicate which lexical items are suitable fillers for which item-based slots. Here, the 'substitution' variant of the generalisation learning mechanism is demonstrated, where the new form-meaning pairing differs from the holophrase construction by substituting a minimal difference. There also exists an 'addition' variant, where the new form-meaning pairing extends the holophrase constructions by a minimal difference, and the 'deletion' variant, where the new form-meaning pairing is a reduction of the holophrase constructions by a minimal difference.")))
  (add-element '((hr)))
  
  ;; question_000450_len_005.lisp => "What material is the big cube?"
  ;; Learn big-cxn, cyan-cxn and what-material-is-the-?X-cube-cxn
  (setf (question-data *experiment*)
        (list (load-file-data
               (merge-pathnames
                (make-pathname :directory '(:relative "stage-1")
                               :name "question_000450_len_005" :type "lisp")
                (get-configuration *experiment* :challenge-files-root)))))
  
  (run-interactions-until-cxn-with-meaning
   *experiment* '((bind size-category ?size large)))
  (run-interactions-until-cxn-inventory-size *experiment* 4)
  (add-element '((div :class "text-box") ((p) "At this point, the agent has acquired an item-based construction and two lexical constructions that are sufficient to cover both of the observed questions.")))
  (add-element '((p) ((a :href "#substitution") "Back to top of this page.")))
  (add-element '((p) ((a :href "./../index.html") "Back to the main page.")))
  )


(defun partial-meaning ()
  (add-element '((h2) ((a :name "partial")) "3. Learning from partial meanings"))
  (add-element '((div :class "text-box") ((p) "This learning mechanism creates new constructions that can combine with existing constructions in order to comprehend the observed utterance. The item-based and lexical constructions that were acquired in the previous interactions allow the agent to understand already parts of novel utterances. This allows the agent to retrieve already parts of the meaning of the observed question. When reconstructing a meaning hypothesis, intention reading makes use of these partial meanings in order to rule out large parts of the search space. Concretely, any meaning hypothesis that does not contain the partial meaning can immediately be ruled out. This is how the interplay between intention reading and pattern finding allows to overcome the intractability of the intention reading process. In what follows, partial meaning provided by lexical constructions (3.1) and by an item-based construction (3.2) is demonstrated.")))
  
  (add-element '((h3) ((a :name "partiallexical")) "3.1 Partial meaning provided by lexical construction(s)"))
  (add-element '((div :class "text-box") ((p) "In the following interaction, partial meaning is provided by lexical constructions. The agent can acquire an item-based construction with an equal number of slots.")))
  
  ;; question_007114_len_006.lisp => "What shape is the big cyan object?"
  ;; Learn what-shape-is-the-?X-?Y-object-cxn
  (setf (question-data *experiment*)
        (list (load-file-data
               (merge-pathnames
                (make-pathname :directory '(:relative "stage-1")
                               :name "question_007114_len_006" :type "lisp")
                (get-configuration *experiment* :challenge-files-root)))))
  (run-interaction *experiment*)

  (add-element '((h3) ((a :name "partialitembased")) "3.2 Partial meaning provided by item-based construction"))
  (add-element '((div :class "text-box") ((p) "In the following interaction, partial meaning is provided by an item-based construction. This learning mechanism can only apply if a single lexical item is missing, otherwise there would be referential uncertainty. In this case, the agent can acquire a single lexical construction. For the purposes of this demonstration, the agent keeps observing the same question until its hypothesis for the lexical construction corresponds to the ground truth.")))

  ;; question_146153_len_005.lisp => ""What material is the gray cube?""
  ;; Learn gray-cxn
  (setf (question-data *experiment*)
        (list (load-file-data
               (merge-pathnames
                (make-pathname :directory '(:relative "stage-1")
                               :name "question_146153_len_005" :type "lisp")
                (get-configuration *experiment* :challenge-files-root)))))
  
  (run-interactions-until-cxn-with-meaning
   *experiment* '((bind color-category ?color gray)))
  (run-interaction *experiment*)
  (add-element '((p) ((a :href "#partial") "Back to top of this page")))
  (add-element '((p) ((a :href "./../index.html") "Back to the main page.")))
  )


(defun learning-links ()
  (add-element '((h2) ((a :name "links")) "4. Learning slot-argument links"))
  (add-element '((div :class "text-box") ((p) "Using the previously explained learning mechanisms, the agent has acquired a number of item-based and lexical constructions. This learning mechanism covers the cases where previosuly acquired constructions cover the observed utterance, but where the slot-and-argument combination has not been observed before. In such cases, the agent simply adds the slot-argument link to the categorial network.")))
  (add-element '((hr)))
  
  ;; question_022307_len_006.lisp => "What shape is the big gray object?"
  ;; add link to categorial network!
  (setf (question-data *experiment*)
        (list (load-file-data
               (merge-pathnames
                (make-pathname :directory '(:relative "stage-1")
                               :name "question_022307_len_006" :type "lisp")
                (get-configuration *experiment* :challenge-files-root)))))
  (run-interaction *experiment*)
  (add-element '((p) ((a :href "#links") "Back to top of this page.")))
  (add-element '((p) ((a :href "./../index.html") "Back to the main page.")))
  )


(defun make-static-web-demo ()
  (header-page)
  (learn-holophrases)
  (substitution)
  (partial-meaning)
  (learning-links)
  (add-element '((h2) "The End")))

;(make-static-web-demo)


;(create-static-html-page "Intention Reading and Pattern Finding" (header-page))
;(create-static-html-page "Intention Reading and Pattern Finding" (learn-holophrases))
;(create-static-html-page "Intention Reading and Pattern Finding" (substitution))
;(create-static-html-page "Intention Reading and Pattern Finding" (partial-meaning))
;(create-static-html-page "Intention Reading and Pattern Finding" (learning-links))
  



