(ql:quickload :cle)

(in-package :cle)


;; experiments with entrenchment values - keep the same
(progn
  (setf *scene-ids* (read-scene-ids "3-color-area-roughness.lisp"))
  (setf *subset-size* (length *scene-ids*))
  (defparameter *baseline-simulated*
    (make-configuration
     :entries `(;; monitoring
                (:dot-interval . 1000)
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
                 ;,'sides-and-corners
                 ;,'wh-ratio
                 ;,'xpos
                 ;,'ypos
                 ;,'zpos
                 )
                (:scene-ids . ,(first-n *subset-size* *scene-ids*))
                (:current-scene-idx . 0)
                ;; general strategy
                (:strategy . :times)
                (:similarity-threshold . 0)

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
                (:initial-weight . 35)
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


(display-lexicon (first (agents *experiment*)) :sort t)

#|(progn
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
      do (format t "~%~a: ~a" (form cxn) (first-n 10 history)))|#


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



(progn
  (setf saved (testi))
  (float (/ (- 5000 (length saved)) 5000)))

(length saved)


(progn
  (set-scene *experiment* 13459)
  (sample-topic *experiment* :english-concepts)
  (show-scene (find-data (find-agent 41) 'context) (find-data (find-agent 41) 'topic)))

=> "6: 970 [AGENT-28, AGENT-23] -> topic: <cle-object: attributes: (AREA . 0.708), (R . 0.48700002), (G . 0.298), (B . 0.07), (ROUGHNESS . 0.80100006)>"

(progn
  (setf saved-agents (interacting-agents (current-interaction *experiment*)))
  (setf saved-scene  (index (current-scene (world *experiment*))))
  (setf saved-topic (find-data (first saved-agents) 'topic)))


(let ((index 0))
  (setf saved-agents (first (nth index saved)))
  (setf saved-scene  (second (nth index saved)))
  (setf saved-topic (third (nth index saved))))

#|(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (run-interaction *experiment*
                   :scene 3818
                   :agents saved-agents)
  (format t "~% ~a - coherence: ~a, and success: ~a"
          saved-scene
          (find-data (current-interaction *experiment*) 'lexicon-coherence)
          (communicated-successfully (current-interaction *experiment*)))
  #|(run-interaction *experiment*
                   :scene saved-scene
                   :agents (reverse saved-agents))|#
  )|#

(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (loop for tuple in saved
        for saved-agents = (first tuple)
        for saved-scene =  (second tuple)
        for saved-topic = (third tuple)
        do (add-element `((h3) ,(format nil "Topic ~a" (description saved-topic))))
           (run-interaction *experiment*
                            :scene saved-scene
                            :agents saved-agents
                            :topic saved-topic)))
         
          
       


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
      collect (cons (id agent) (mapcar #'cdr (remove-duplicates (first-n 200 history) :key #'cdr))))

(in-which-context-does-agent-use-cxn (find-agent 23) "rapiva")

;;;;
(progn
  (setf possible-scenes (find-possible-scenes saved))


  (length saved)
  (setf not-possible-amount (- (length saved) (length possible-scenes)))



  (let ((x (length saved)))
    (format nil "~a vs ~a"
            (float (/ (- 5000 (length saved)) 5000))
            (float (/ (- 5000 (- (length saved) not-possible-amount)) 5000)))))




(progn
  (setf saved (testi2 (find-agent 31) "duzise" 100000))
  (float (/ (- 5000 (length saved)) 5000)))

(inspect (find-agent 31))

(length saved)

(let ((index 0))
  (setf saved-agents (first (nth index saved)))
  (setf saved-scene  (second (nth index saved)))
  (setf saved-topic (third (nth index saved))))



(let* ((agent (find-agent 31))
       (cxn (find-form-in-lexicon (lexicon agent) "duzise"))
       (context (find-data agent 'context))
       (topic (find-data agent 'topic)))
  (loop for object in (objects context)
        
        collect (list (id object) (weighted-similarity object (meaning cxn)))))

=> ((#:OBJ-1897755 0.7423133) ;; topic
    (#:OBJ-1897756 0.034696862)
    (#:OBJ-1897757 0.50630695))


(- 0.7423133 0.50630695)



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

(let ((total1 (sum (list 155 131 128 76 118 116 82 96 49 54)))
      (total2 (sum (list 3 3))))
  (float (/ total1 (+ total1 total2))))
;; 99.41%


(loop for tuple in saved
      for saved-scene-id = (second tuple)
      for saved-topic = (third tuple)
      for channels-in-play  = (get-configuration *experiment* :clevr-channels)
      for symbolic-clevr-context =  (get-scene-by-index (world *experiment*) saved-scene-id)
      for cle-context = (clevr->simulated symbolic-clevr-context channels-in-play)
      for cle-topic = (find saved-topic (objects cle-context)
                            :test (lambda (x el) (equal (description x) (description el))))
      do (show-scene cle-context cle-topic)
      do (format t "~%~a | ~a -> ~a" (is-discriminative-strict cle-topic (remove cle-topic (objects cle-context)))
                                                        saved-scene-id (description cle-topic)))



       

(display-lexicon (find-agent 42) :sort t)



#|(defun in-which-context-does-agent-use-cxn (agent form)
  (wi::reset)
  (deactivate-all-monitors)
  ;(activate-monitor trace-interaction-in-web-interface)
  (loop with saved-saved = '()
        for tuple in saved 
        for saved-agents = (first tuple)
        for saved-scene = (second tuple)
        for saved-topic = (third tuple)
        do (run-interaction *experiment*
                            :scene saved-scene
                            :agents saved-agents
                            :topic saved-topic)
        if (and
            (eq agent (first (interacting-agents (current-interaction *experiment*))))
            (eq form
                (form (get-data (first (interacting-agents (current-interaction *experiment*))) 'applied-cxn))))
          do (setf saved-saved (cons (list (interacting-agents (current-interaction *experiment*))
                                           (index (current-scene (world *experiment*)))
                                           (find-data (first (interacting-agents (current-interaction *experiment*))) 'topic))
                                     saved-saved
                                     ))))|#

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
                                         (list "rapiva" "masuvu" )
                                         ;(list "nowite" "boworu")
                                         ;(list "gelowo" "fimomu")
                                         ;(list "deweti" "revuno")
                                         )

(setf rapiva (find-form-in-lexicon (lexicon (find-agent 23)) "rapiva"))

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

(setf lezemu (find-form-in-lexicon (lexicon (find-agent 31)) "duzise"))
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


(display-lexicon (find-agent 32) :sort t)


;;;;

(cl-store:store *experiment*
                (babel-pathname :directory '("experiments"
                                             "concept-emergence2")
                                :name "2023-06-21-3-area-roughness-color-500k"
                                :type "store"))
        
(setf *experiment2*
      (cl-store:restore (babel-pathname :directory '("experiments"
                                                     "concept-emergence2"
                                                     "logging")
                                        :name "2023-06-21-3-area-roughness-color-500k"
                                        :type "store")))



