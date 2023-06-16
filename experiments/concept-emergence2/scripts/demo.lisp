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

(display-lexicon (first (agents *experiment*)) :sort t)

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
                 (format t "~a " i)
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


(let ((index 3))
  (setf saved-agents (first (nth index saved)))
  (setf saved-scene  (second (nth index saved)))
  (setf saved-topic (third (nth index saved))))

(loop for i in (list 1 2 3)
      collect i into plakker
      finally (return (sum plakker)))

(defun is-discriminative (object other-objects)
  "Checks if the object has a single channel dimension that is different from all other objects."
  (loop with found = nil
        for (attr . val) in (description object)
        if (and (is-channel-available attr (attributes object))
                (loop for other-object in other-objects
                                         for other-val = (assqv attr (description other-object))
                                         always (not (equal val other-val))))
            do (if found
                 (return nil)
                 (setf found t))
          finally (return t)))

(length saved)

(setf possible-scenes (loop for tuple in saved
      for saved-agent = (first tuple)
      for saved-scene = (second tuple)
      for saved-topic = (third tuple)
      for channels-in-play = (get-configuration *experiment* :clevr-channels)
      for symbolic-clevr-context = (get-scene-by-index (world *experiment*) saved-scene)
      for cle-context = (clevr->simulated symbolic-clevr-context channels-in-play)
      for cle-topic = (find saved-topic (objects cle-context)
                        :test (lambda (x el) (equal (description x) (description el))))
      for possible = (is-discriminative cle-topic (remove cle-topic (objects cle-context)))
      if possible
        collect (cons saved-scene possible)))


(length saved)
(setf not-possible-amount (- (length saved) (length possible-scenes)))

(let ((x (length saved) ))
  (float (/ (- 5000 x) 5000)))
            
          
            
#|        collect (cons attr (loop for other-object in other-objects
                                         for other-val = (assqv attr (description other-object))
                                         always (not (equal val other-val))))
          into discriminative
        finally (return discriminative)))

(let* ((channels-in-play (get-configuration *experiment* :clevr-channels))
       (symbolic-clevr-context (get-scene-by-index (world *experiment*) saved-scene))
       (cle-context (clevr->simulated symbolic-clevr-context channels-in-play))
       (cle-topic (find saved-topic (objects cle-context)
                        :test (lambda (x el) (equal (description x) (description el))))))
    (is-discriminative cle-topic (remove cle-topic (objects cle-context))))
|#
(defun only-one
(loop for answer in (list nil nil nil)
      sum (if answer 1 0) into res
      finally (return (eq res 1)))
        

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
(setf cxn-test (find-form-in-lexicon (lexicon (find-agent 23)) "vereko"))

(string-equal "ragifa" (form (first (lexicon (car (agents *experiment*))))))

(loop for agent in (agents *experiment*)

      for cxn in (lexicon agent)
      collect cxn)

(let* ((agent (find-agent 30))
       (cxn-color (find-form-in-lexicon (lexicon agent) "ruxipu"))
       (context (find-data agent 'context))
       (topic (find-data agent 'topic))
       (context2 (remove topic (objects context)))
       )
  (abs (- (sigmoid (weighted-similarity topic (meaning cxn-color)))
          (sigmoid (loop for object in context2
                         maximize (weighted-similarity object (meaning cxn-color)))))))

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


(abs (- (sigmoid 0.251) (sigmoid -0.817)))

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
                                        (find-data (first saved-agents) 'topic)
                                        ;(third (objects (find-data (first saved-agents) 'context)))
                                        ;(find-data (first saved-agents) 'topic)
                                        )

(let* ((agent (find-agent 30))
       (cxn-color (find-form-in-lexicon (lexicon agent) "lolife"))
       (context (find-data agent 'context))
       (topic (find-data agent 'topic))
       (context2 (remove topic (objects context)))
       )
  (abs (- (sigmoid (weighted-similarity topic (meaning cxn-color)))
          (sigmoid (loop for object in context2
                         maximize (weighted-similarity object (meaning cxn-color)))))))



(loop for i in (list 1 2 3)
      sum i)

(defmethod weighted-similarity ((object cle-object) (concept concept))
  "Compute the weighted similarity between an object and a concept."
  (loop with ledger = (loop for prototype in (prototypes concept) sum (weight prototype))
        for prototype in (prototypes concept)
        for observation = (get-channel-val object (channel prototype))
        for similarity = (observation-similarity observation prototype)
        ;collect (list (weight prototype) ledger similarity)))
        sum (* (/ (weight prototype) ledger) similarity)))

(weighted-similarity (find-data (find-agent 23) 'topic) (meaning (find-form-in-lexicon (lexicon (find-agent 23)) "lolife")))


=> ((0.18242553 2.2322293 0.555896) ;; area
    (0.18242553 2.2322293 0.8004624) ;; r
    (0.62245936 2.2322293 0.94885886) ;; g
    (0.62245936 2.2322293 0.44492367) ;; b
    (0.62245936 2.2322293 0.69560254)) ;; roughness


(let ((weight 0.0)
      (ledger 3.0)
      (sim 0.41590))
  (* (/ weight ledger) sim))

=> ((0.0 3.0 0.4145908)
    (1.0 3.0 0.81441015) ;;
    (1.0 3.0 0.6309847) ;; 
    (1.0 3.0 0.94933284) ;; 
    (0.0 3.0 0.37328514))


(let* ((agent (find-agent 30))
       (cxn-color (find-form-in-lexicon (lexicon agent) "ruxipu"))
       (context (find-data agent 'context))
       (topic (find-data agent 'topic))
       (context2 (remove topic (objects context)))
       )
  (abs (- (sigmoid (weighted-similarity topic (meaning cxn-color)))
          (sigmoid (loop for object in context2
                         maximize (weighted-similarity object (meaning cxn-color)))))))




;; same as previous, but only show a given list
(how-would-the-population-conceptualise2 (find-data (first saved-agents) 'context)
                                         (find-data (first saved-agents) 'topic)
                                         ;(find-data (first saved-agents) 'topic)
                                         (list "ronide" "lolife" )
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


(display-lexicon (find-agent 30) :sort t)





;;;;

(cl-store:store *experiment*
                (babel-pathname :directory '("experiments"
                                             "concept-emergence2")
                                :name "2023-06-16-area-roughness-color-1m"
                                :type "store"))
        
(setf *experiment*
      (cl-store:restore (babel-pathname :directory '("experiments"
                                                     "concept-emergence"
                                                     "logging"
                                                     "stored_experiments")
                                        :name "2023-03-21-exp22"
                                        :type "store")))



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