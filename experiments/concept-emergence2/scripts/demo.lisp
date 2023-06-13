(ql:quickload :cle)

(in-package :cle)


;; experiments with entrenchment values - keep the same
(progn
  (setf *scene-ids* (read-scene-ids "color-area-roughness.lisp"))
  (setf *subset-size* (length *scene-ids*))
  (defparameter *baseline-simulated*
    (make-configuration
     :entries `(;; monitoring
                (:dot-interval . 1000)
                (:log-interval . 50000)
                (:log-duration . 2000)
                (:save-distribution-history . nil)
                ;; setup interacting agents
                (:interacting-agents-strategy . :standard)
                (:population-size . 10)
                ;; setup scene
                (:scene-sampling . :deterministic)
                (:topic-sampling . :english-concepts)
                (:clevr-channels
                 ,'area ;; size
                 ,'color ;; color
                 ,'roughness
                 )
                (:scene-ids . ,(first-n *subset-size* *scene-ids*))
                (:current-scene-idx . 0)
                ;; general strategy
                (:strategy . :times)

                ;; entrenchment of constructions
                (:initial-cxn-entrenchement . 1/2)
                (:entrenchment-incf . 1/10)
                (:entrenchment-decf . -1/10)
                (:entrenchment-li . -1/50) ;; lateral inhibition
                
                ;; concept representations
                (:concept-representation . :distribution)
                (:distribution . :gaussian-welford)
                (:M2 . 0.0001) ;; only for gaussian-welford

                ;; prototype weight inits
                (:weight-update-strategy . :j-interpolation)
                (:initial-weight . 0)
                (:weight-incf . 1)
                (:weight-decf . -1)
                )))
  (setf *experiment* (make-instance 'cle-experiment :configuration *baseline-simulated*))
  (notify reset-monitors)
  (wi::reset))

(defmethod after-interaction ((experiment cle-experiment))
  (align (speaker experiment))
  (align (hearer experiment))
  )

;; 1. run x interactions
(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor export-communicative-success)
  (activate-monitor export-lexicon-coherence)
  (activate-monitor print-a-dot-for-each-interaction)
  (format t "~%---------- NEW GAME ----------~%")
  (loop for i from 1 to 20000
        do (run-interaction *experiment*)))


;; 2. run x interactions with tiiw
(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interaction-in-web-interface)
  (loop for idx from 1 to 25
        do (run-interaction *experiment*)))

(defmethod after-interaction ((experiment cle-experiment))
  #|(align (speaker experiment))
  (align (hearer experiment))|#
  )




(defun testi ()
  (progn
    (wi::reset)
    (deactivate-all-monitors)
    ;; run to find a scene without lex coherence
    (loop with saved = '()
          for i from 1 to 5000
          for lex-coherence = (find-data (current-interaction *experiment*) 'lexicon-coherence)
          for speaker = (speaker (first (interactions *experiment*)))
          for hearer = (hearer (first (interactions *experiment*)))
          if (not lex-coherence)
            do (progn
                 ;(setf saved (cons (format nil "% ~a: ~a [~a, ~a] -> topic: ~a" i (index (current-scene (world *experiment*))) (id speaker) (id hearer) (find-data speaker 'topic)) saved))
                 (setf saved (cons (list (interacting-agents (current-interaction *experiment*))
                                         (index (current-scene (world *experiment*)))
                                         (find-data (first (interacting-agents (current-interaction *experiment*))) 'topic))
                                   saved
                                   ))
                 (run-interaction *experiment*)
                 )
          else
            do (progn
                 ;(format t "~% ~a: ~a [~a, ~a] -> topic: ~a" i (index (current-scene (world *experiment*))) (id speaker) (id hearer) (find-data speaker 'topic))
                 (run-interaction *experiment*))
          finally (return saved))))

(setf saved (testi))

(length saved)

(let ((x (length saved)))
  (float (/ (- 5000 x) 5000)))

=> "6: 970 [AGENT-28, AGENT-23] -> topic: <cle-object: attributes: (AREA . 0.708), (R . 0.48700002), (G . 0.298), (B . 0.07), (ROUGHNESS . 0.80100006)>"

(progn
  (setf saved-agents (interacting-agents (current-interaction *experiment*)))
  (setf saved-scene  (index (current-scene (world *experiment*))))
  (setf saved-topic (find-data (first saved-agents) 'topic)))


(let ((index 1))
  (setf saved-agents (first (nth index saved)))
  (setf saved-scene  (second (nth index saved)))
  (setf saved-topic (third (nth index saved))))





(find saved-topic (objects (find-data (first saved-agents) 'context)) :test (lambda (x el) (equal (description x) (description el))))

(loop for object in (objects (find-data (first saved-agents) 'context))
      when (di)


saved-topic


;; run the saved scene agent
(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (add-element `((h3) ,(format nil "Topic ~a" (description saved-topic))))
  (run-interaction *experiment*
                   :scene saved-scene
                   :agents saved-agents
                   :topic saved-topic)
  (format t "~% ~a - coherence: ~a, and success: ~a"
          saved-scene
          (find-data (current-interaction *experiment*) 'lexicon-coherence)
          (communicated-successfully (current-interaction *experiment*)))
  #|(run-interaction *experiment*
                   :scene saved-scene
                   :agents (reverse saved-agents))|#
  )

;; experiment with a specific cxn
(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 83)) "fimomu"))
(setf cxn-test (find-form-in-lexicon (lexicon (find-agent 23)) "vereko"))

(string-equal "ragifa" (form (first (lexicon (car (agents *experiment*))))))

(loop for agent in (agents *experiment*)

      for cxn in (lexicon agent)
      collect cxn)
        
(find-form-in-lexicon (lexicon agent) "revuno")

(in-which-context-does-agent-use-cxn (find-agent 41) "vexune")

(defun in-which-context-does-agent-use-cxn (agent cxn)
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (loop for tuple in saved 
        for saved-agents = (first tuple)
        for saved-scene = (second tuple)
        for saved-topic = (third tuple)
        do (run-interaction *experiment*
                            :scene saved-scene
                            :agents saved-agents
                            :topic saved-topic)))

  





;; conceptualise a scene/topic combo with all agents
(how-would-the-population-conceptualise (find-data (first saved-agents) 'context)
                                        (third (objects (find-data (first saved-agents) 'context)))
                                        ;(find-data (first saved-agents) 'topic)
                                        )

;; same as previous, but only show a given list
(how-would-the-population-conceptualise2 (find-data (first saved-agents) 'context)
                                         (third (objects (find-data (first saved-agents) 'context)))
                                         ;(find-data (first saved-agents) 'topic)
                                         (list "bowopi" "piwowu")
                                         ;(list "nowite" "boworu")
                                         ;(list "gelowo" "fimomu")
                                         ;(list "deweti" "revuno")
                                         )

(let ((applied-cxn (find-form-in-lexicon (lexicon (first (agents *experiment*))) "serudo"))
      (other-cxn (find-form-in-lexicon (lexicon (first (agents *experiment*))) "fuvoki")))
  (similar-concepts2 (meaning applied-cxn) (meaning other-cxn) :times))

(add-cxn-to-interface (find-form-in-lexicon (lexicon (first (agents *experiment*))) "fuvoki")
                      :certainty-threshold 0.1)

(add-cxn-to-interface (find-form-in-lexicon (lexicon (first (agents *experiment*))) "serudo")
                      :certainty-threshold 0.1)

(add-cxn-to-interface (find-form-in-lexicon (lexicon (first (agents *experiment*))) "sagiri")
                      :certainty-threshold 0.1)

;; => ((CLE::ROUGHNESS . 0.99753345) (UTILS:B . 0.0) (CLE::G . 0.0024665243) (CLE::R . 0.0) (CLE::AREA . 0.0))


(defmethod similar-concepts2 ((concept1 concept) (concept2 concept) (mode (eql :times)) &key &allow-other-keys)
  (loop with concept1-weight-sum = (loop for proto in (prototypes concept1) sum (weight proto))
        with concept2-weight-sum = (loop for proto in (prototypes concept2) sum (weight proto))
        for proto1 in (prototypes concept1)
        for proto2 in (prototypes concept2)
        for avg-weight = (/ (+ (/ (weight proto1) concept1-weight-sum)
                               (/ (weight proto2) concept2-weight-sum))
                            2)
        for weight-similarity = (- 1 (abs (- (weight proto1) (weight proto2))))
        for prototype-similarity = (- 1 (f-divergence (distribution proto1) (distribution proto2) :hellinger))
        for sim-score = (* avg-weight prototype-similarity weight-similarity)
        ;sum sim-score))
        collect (cons (channel proto1) sim-score)))

=> ((CLE::ROUGHNESS . 0.062145736)
     (UTILS:B . 0.0)
     (CLE::G . 0.001204835)
     (CLE::R . 0.0)
     (CLE::AREA . 0.10917586))

=> ((CLE::ROUGHNESS . 0.0) (UTILS:B . 0.06408119) (CLE::G . 0.25138766) (CLE::R . 0.24252478) (CLE::AREA . 0.0))

(setf lezemu (find-form-in-lexicon (lexicon (first (agents *experiment*))) "lezemu"))



(setf positive (loop for cxn in (lexicon (second (agents *experiment*)))
                     if (> (score cxn) 0.1)
                       collect cxn))

(setf abba (loop for cxn in positive
                 for similarity = (loop for other-cxn in positive
                                        collect (format nil "~,2f" (similar-concepts (meaning cxn) (meaning other-cxn) :times)))
                 collect (cons (form cxn) similarity)))

(setf abba (loop for cxn in positive
                 for similarity = (loop for other-cxn in positive
                                        collect (format nil "~,2f" (similar-concepts (meaning cxn) (meaning other-cxn) :times)))
                 collect (form cxn)))

(loop for form in (list "zebola" "sovate" "sagiri" "serudo" "ziwuri" "gowito" "nowite" "nukaru" "ranomu" "ranime" "fuvoki" "boworu")
      for cxn = (find-form-in-lexicon (lexicon (second (agents *experiment*))) form)
      do (add-cxn-to-interface cxn :certainty-threshold 0.00000001))


(add-cxn-to-interface (nth 13 (lexicon (first (agents *experiment*)))))


(display-lexicon (first (agents *experiment*)) :sort t)





;;;;

(cl-store:store *experiment*
                (babel-pathname :directory '("experiments"
                                             "concept-emergence2")
                                :name "2023-06-09-area-roughness-color-720k"
                                :type "store"))



(defun run-and-show-interactions (experiment interactions-count)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interaction-in-web-interface)
  (activate-monitor display-communicative-success)
  (loop for i from 1 to interactions-count
        do (run-interaction experiment))
  (run-interaction experiment))

(defun run-then-show-last (experiment interactions-count &key (show nil) (display nil))
  (when display
    (deactivate-all-monitors)
    (activate-monitor display-communicative-success)
    (activate-monitor print-a-dot-for-each-interaction)
    )
  (loop for i from 1 to (- interactions-count 1)
        do (run-interaction experiment))
  (when show
    (activate-monitor trace-interaction-in-web-interface)
    (run-interaction experiment)))