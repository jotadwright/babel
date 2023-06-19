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
  ;(activate-monitor export-communicative-success)
  ;(activate-monitor export-lexicon-coherence)
  (activate-monitor print-a-dot-for-each-interaction)
  (format t "~%---------- NEW GAME ----------~%")
  (loop for i from 1 to 500000
        do (run-interaction *experiment*)))

(progn
  (wi::reset)
  (run-interaction *experiment*))

(length (lexicon (first (agents *experiment*))))

(display-lexicon (find-agent 41) :sort t)

(setf agent41 (find-agent 41))

((find-data agent41 'context)

(index (current-scene (world (experiment agent41))))

(loop for form in (list "bivexi" "vekudi" "gixaka" "nerota")
      for cxn = (find-form-in-lexicon (lexicon (find-agent 41)) form)
      for history = (history cxn)
      do (format t "~%~a: ~a" (form cxn) (first-n 10 history)))


;; 2. run x interactions with tiiw
(progn
  (wi::reset)
  (deactivate-all-monitors)
  ;(activate-monitor print-a-dot-for-each-interaction)
  ;(activate-monitor trace-interaction-in-web-interface)
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
                 (setf saved (cons (list (interacting-agents (current-interaction *experiment*))
                                         (index (current-scene (world *experiment*)))
                                         (find-data (first (interacting-agents (current-interaction *experiment*))) 'topic))
                                   saved
                                   ))
                 (run-interaction *experiment*)
                 )
          else
            do (progn
                 ;(format t "~a " i)
                 (run-interaction *experiment*))
          finally (return saved))))

(progn
  (setf saved (testi))
  (length saved))

bivexi: ((475459 . 13459) (468024 . 14614) (455309 . 3834) 
vekudi: ((480250 . 6018) 
gixaka: ((485189 . 500)  (474004 . 2040) (471148 . 9144))
nerota: ((485041 . 14410)  (457620 . 6990) (453294 . 2927) (448527 . 10199)  (436662 . 6004))

(run-interaction *experiment*
                 :scene saved-scene
                 :agents saved-agents
                 :topic saved-topic)
(progn
  (set-scene *experiment* 13459)
  (sample-topic *experiment* :english-concepts)
  (show-scene (find-data (find-agent 41) 'context) (find-data (find-agent 41) 'topic)))

=> "6: 970 [AGENT-28, AGENT-23] -> topic: <cle-object: attributes: (AREA . 0.708), (R . 0.48700002), (G . 0.298), (B . 0.07), (ROUGHNESS . 0.80100006)>"

(progn
  (setf saved-agents (interacting-agents (current-interaction *experiment*)))
  (setf saved-scene  (index (current-scene (world *experiment*))))
  (setf saved-topic (find-data (first saved-agents) 'topic)))


(let ((index 7))
  (setf saved-agents (first (nth index saved)))
  (setf saved-scene  (second (nth index saved)))
  (setf saved-topic (third (nth index saved))))
        

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
(loop for form in (list "giteru" "kavife" "gupaka")
      do (add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 30)) form)))


(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 30)) "zazoni")
                      :certainty-threshold 0.00001)


(let* ((agent (find-agent 23))
       (cxn-color (find-form-in-lexicon (lexicon agent) "gupaka"))
       (cxn-area (find-form-in-lexicon (lexicon agent) "kavife"))
       (cxn-roughness (find-form-in-lexicon (lexicon agent) "giteru"))
       (context (find-data agent 'context))
       (topic (find-data agent 'topic)))
  (loop for object in (objects context)
        collect (list (weighted-similarity object (meaning cxn-color))
                      (weighted-similarity object (meaning cxn-area))
                      (weighted-similarity object (meaning cxn-roughness)))))

(objects (find-data (find-agent 23) 'context))
(find-data (find-agent 23) 'topic)

  
(let* ((agent (find-agent 23))
       (cxn-color (find-form-in-lexicon (lexicon agent) "gupaka"))
       (cxn-area (find-form-in-lexicon (lexicon agent) "kavife"))
       (cxn-roughness (find-form-in-lexicon (lexicon agent) "giteru"))
       (context (find-data agent 'context))
       (topic (find-data agent 'topic)))
  (list (weighted-similarity topic (meaning cxn-color))
        (weighted-similarity topic (meaning cxn-area))
        (weighted-similarity topic (meaning cxn-roughness))))

   cxn1   cxn2   cxn3
(-0.817 -4.703 -7.472) ;; obj1
( 0.251  0.153  0.162) ;; obj2 topic
(-1.290 -4.929 -7.371) ;; obj3


(loop for agent in (agents *experiment*)
      for lex = (lexicon agent)
      for cxn = (find-form-in-lexicon lex "kemuba")
      for history = (history cxn)
      collect (cons (id agent) (mapcar #'cdr (remove-duplicates (first-n 200 history) :key #'cdr)))

(in-which-context-does-agent-use-cxn (find-agent 41) "kemuba")

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
                                        (find-data (first saved-agents) 'topic)
                                        ;(third (objects (find-data (first saved-agents) 'context)))
                                        ;(find-data (first saved-agents) 'topic)
                                        )

;; same as previous, but only show a given list
(how-would-the-population-conceptualise2 (find-data (first saved-agents) 'context)
                                         (find-data (first saved-agents) 'topic)
                                         ;(find-data (first saved-agents) 'topic)
                                         (list "sififo" "kemuba" )
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

(setf lezemu (find-form-in-lexicon (lexicon (first (agents *experiment*))) "lezemu"))
(setf positive (loop for cxn in (lexicon (second (agents *experiment*)))
                     if (> (score cxn) 0.1)
                       collect cxn))


(setf abba (loop for cxn in positive
                 for similarity = (loop for other-cxn in positive
                                        collect (format nil "~,2f" (similar-concepts (meaning cxn) (meaning other-cxn) :times)))
                 collect (form cxn)))

(loop for form in (list "zebola" "sovate" "sagiri" "serudo" "ziwuri" "gowito" "nowite" "nukaru" "ranomu" "ranime" "fuvoki" "boworu")
      for cxn = (find-form-in-lexicon (lexicon (second (agents *experiment*))) form)
      do (add-cxn-to-interface cxn :certainty-threshold 0.00000001))


(add-cxn-to-interface (nth 13 (lexicon (first (agents *experiment*)))))


(display-lexicon (find-agent 30) :sort t)


;;;;

(cl-store:store *experiment*
                (babel-pathname :directory '("experiments"
                                             "concept-emergence2")
                                :name "2023-06-19-area-roughness-color-421k"
                                :type "store"))
        
(setf *experiment*
      (cl-store:restore (babel-pathname :directory '("experiments"
                                                     "concept-emergence"
                                                     "logging"
                                                     "stored_experiments")
                                        :name "2023-03-21-exp22"
                                        :type "store")))



