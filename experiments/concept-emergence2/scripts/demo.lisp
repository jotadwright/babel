(ql:quickload :cle)

(in-package :cle)

;; experiments with entrenchment values - keep the same
(progn
  (defparameter *baseline-simulated*
    (make-configuration
     :entries `(
                ;; monitoring
                (:dot-interval . 10)
                (:save-distribution-history . nil)
                ;; setup interacting agents
                (:interacting-agents-strategy . :standard)
                (:population-size . 10)
                ;; setup data scene
                (:dataset . "clevr-extracted")
                (:dataset-split . "val")
                (:data-fname . "discriminative.lisp")
                (:available-channels ,@(get-all-channels :clevr-extracted))
                #|(:available-channels 
                 ;,'xpos ,'ypos ,'zpos ;; position
                 )|#
                ;; disable channels
                (:disable-channels . :none)
                (:amount-disabled-channels . 0)
                ;; noised channels
                (:sensor-noise . :none)
                (:sensor-std . 0.05)
                (:observation-noise . :none)
                (:observation-std . 0.01)
                ;; scene sampling
                (:scene-sampling . :deterministic)
                (:topic-sampling . :discriminative)
                ;; general strategy
                (:strategy . :times)
                (:similarity-threshold . 0.0)

                ;; entrenchment of constructions
                (:initial-cxn-entrenchement . 1/2)
                (:entrenchment-incf . 1/10)
                (:entrenchment-decf . -1/10)
                (:entrenchment-li . -1/50) ;; lateral inhibition
                (:trash-concepts . nil)
                
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

(parse-keyword "XPOS")
(parse-keyword "YPOS")
(parse-keyword "ZPOS")


;; 1. run x interactions
(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor export-communicative-success)
  (activate-monitor export-lexicon-coherence)
  (activate-monitor print-a-dot-for-each-interaction)
  (format t "~%---------- NEW GAME ----------~%")
  (loop for i from 1 to 500000
        do (run-interaction *experiment*)))

(+ 1 2)
;; display lexicon
(display-lexicon (first (agents *experiment*)) :sort t :entrenchment-threshold 0.5)
(display-lexicon (second (agents *experiment*)) :sort t :entrenchment-threshold 0.5)


(let ((ag1 (first (agents *experiment*)))
      (ag2 (second (agents *experiment*)))
      (ag1-cxn (first (sort (lexicon ag1) #'(lambda (x y) (> (score x) (score y))))))
      (ag2-cxn (find-in-lexicon ag2 (form ag1-cxn))))
  (add-cxn-to-interface ag1-cxn)
  (add-cxn-to-interface ag2-cxn))


(first (sort (lexicon (first (agents *experiment*))) #'(lambda (x y) (> (score x) (score y)))))


(disabled-channels (first (agents *experiment*)))

;; types of constructions
(format-count-entries (count-entries (fourth (agents *experiment*))))

(progn
  (wi::reset)
  (deactivate-all-monitors)
  ;(activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interaction-in-web-interface)
  (loop for idx from 1 to 10
        do (run-interaction *experiment*)))



;; 2. run x interactions with tiiw
(defmethod after-interaction ((experiment cle-experiment))
  #|(align (speaker experiment))
  (align (hearer experiment))|#
  )


(setf tuples (list (list
                    (interacting-agents (first (interactions *experiment*)))
                    (index (find-data (first (interacting-agents (first (interactions *experiment*)))) 'context))
                   
                    (find-data (first (interacting-agents (first (interactions *experiment*)))) 'topic))))

(loop for tuple in tuples
      for saved-agents = (first tuple)
      for saved-scene =  (second tuple)
      for saved-topic = (third tuple)
      do (add-element `((h3) ,(format nil "Topic ~a" (attributes saved-topic))))
         (run-interaction *experiment*
                          :scene saved-scene
                          :agents saved-agents
                          :topic saved-topic))

(loop for cxn in (lexicon agent)
      for concept = (meaning cxn)
      for topic-similarity = (weighted-similarity topic concept)
      for best-other-similarity = (loop for object in (remove topic context)
                                        maximize (weighted-similarity object concept))
      when (> topic-similarity (+ best-other-similarity threshold))
        do (setf discriminating-cxns (cons (list (cons :cxn cxn)
                                                 (cons :topic-sim topic-similarity)
                                                 (cons :best-other-sim best-other-similarity))
                                           discriminating-cxns)))





;;;
(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (loop for tuple in (first-n 100 not-solvable+not-coherent-0);(list (nth 4 not-solvable+not-coherent-0))
        for saved-agents = (first tuple)
        for saved-scene =  (second tuple)
        for saved-topic = (third tuple)
        do (add-element `((h3) ,(format nil "Topic ~a" (description saved-topic))))
           (run-interaction *experiment*
                            :scene saved-scene
                            :agents saved-agents
                            :topic saved-topic)))




(progn
  (setf p-money (testi))
  1)


(progn
  

  (setf not-coherent (first p-money))
  (setf not-solvable+coherent-0 (second p-money))
  (setf not-solvable+coherent-2 (third p-money))
  (setf not-solvable+not-coherent-0 (fourth p-money))
  (setf not-solvable+not-coherent-2 (fifth p-money))
  (setf coherent (sixth p-money))
  1
  )

(defun testpp ()


  
  ;; not-coherent not-solvable+coherent-0 not-solvable+coherent-2 not-solvable+not-coherent-0 not-solvable+not-coherent-2 scenes-with-coherence
  (mapcar (lambda (x) (float (/ x (sum (mapcar #'length p-money))))) (mapcar #'length p-money))
  ;; => (0.0182 0.5592 0.0404 0.3822)
  ;; => (0.0184 0.3594 0.1952 0.031 0.0136 0.3824)
  ;; => (0.022 0.3506 0.1896 0.0348 0.0072 0.3958)
  (mapcar (lambda (lst)
            (float (/ (sum (mapcar (lambda (x) (if (fourth x) 1 0)) lst)) (length lst))))
          p-money)
  ;; => (0.98913044 1.0 1.0 0.8516129 1.0 1.0)

  ;; => (0.989011 1.0 0.9158416 1.0)
  (/ (sum (mapcar (lambda (lst) (float (sum (mapcar (lambda (x) (if (fourth x) 1 0)) lst)))) p-money))
     (sum (mapcar #'length p-money)))
  ;; => 0.9964
  (float (/ (+ (length not-solvable+coherent-0)
               (length not-solvable+coherent-2)
               (length coherent))
            (sum (mapcar #'length p-money))))
  ;; => 0.9414

(loop for el in (fourth p-money)
      for comm-success = (fourth el)
      if (not comm-success)
        collect el)


  (mapcar #'length p-money)

  (display-lexicon (first (agents *experiment*)) :sort t)
  (display-lexicon  (first (agents *experiment*)) :sort t :entrenchment-threshold 0.1)

  (loop for cxn in (lexicon (first (agents *experiment*)))
        if (> (length (history cxn)) 1)
          do (add-cxn-to-interface cxn))
  collect (length (history cxn)))

(setf abba (first (lexicon (first (agents *experiment*)))))


(history (first (first not-solvable+not-coherent)))

(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (loop for tuple in (first-n 100 not-solvable+not-coherent-0);(list (nth 4 not-solvable+not-coherent-0))
        for saved-agents = (first tuple)
        for saved-scene =  (second tuple)
        for saved-topic = (third tuple)
        do (add-element `((h3) ,(format nil "Topic ~a" (description saved-topic))))
           (run-interaction *experiment*
                            :scene saved-scene
                            :agents saved-agents
                            :topic saved-topic)))

(display-lexicon (caar (nth 4 not-solvable+not-coherent-0)) :sort t)

;; => (0.0216 0.7202 0.0626 0.1956)
;; not-coherent not-solvable+coherent not-solvable-and-coh-fail scenes-with-coherence


is-discriminative
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
      for channels-in-play  = (get-configuration *experiment* :channels)
      for symbolic-clevr-context =  (get-scene-by-index (world *experiment*) saved-scene-id)
      for cle-scene = (clevr->simulated symbolic-clevr-context channels-in-play)
      for cle-topic = (find saved-topic (objects cle-scene)
                            :test (lambda (x el) (equal (description x) (description el))))
      do (show-scene cle-scene cle-topic)
      do (format t "~%~a | ~a -> ~a" (is-discriminative-strict cle-topic (remove cle-topic (objects cle-scene)))
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
                                :name "clevr-extracted-random43k-93-84"
                                :type "store"))
        
(setf *experiment*
      (cl-store:restore (babel-pathname :directory '("experiments"
                                                     "concept-emergence2"
                                                     "logging"
                                                     "big-bench5"
                                                     "10-all-random"
                                                     "experiments"
                                                     "2023-06-26_16h35m10s-exp-0")
                                        :name "history"
                                        :type "store")))

(display-lexicon (first (agents *experiment*)) :sort t :entrenchment-threshold 0.00)


(float (/ (loop for agent in (agents *experiment*)
      collect (length (lexicon agent)))
   (length (agents *experiment*))))
(length (lexicon (first (agents *experiment*))))

(defun length-entrenched-lexicon (agent)
  (let ((lexicon (sort (lexicon agent) #'(lambda (x y) (> (score x) (score y))))))
    (length (loop for cxn in lexicon and idx from 0
                  when (>= (score cxn) 0.5)
                    collect cxn))))

(/
 (loop for agent in (agents *experiment*)
       sum (length-entrenched-lexicon agent))
 (length (agents *experiment*)))




