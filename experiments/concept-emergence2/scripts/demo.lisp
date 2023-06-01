(ql:quickload :cle)

(in-package :cle)

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

(progn
  (setf *scene-ids* (read-scene-ids "color.lisp"))
  (setf *subset-size* (length *scene-ids*)) ;; (length *scene-ids*)
  (defparameter *baseline-simulated*
    (make-configuration
     :entries `(;; monitoring
                (:dot-interval . 1000)
                ;; setup interacting agents
                (:interacting-agents-strategy . :standard) ;; i.e. random
                (:population-size . 10)
                ;; setup scene
                (:scene-sampling . :deterministic)
                (:topic-sampling . :english-concepts)
                (:clevr-channels
                 ;; ,'area ;; size
                 ,'color ;; color
                 )
                (:scene-ids . ,(first-n *subset-size* *scene-ids*))
                (:current-scene-idx . 0)
                ;; general strategy (:standard or :times)
                (:strategy . :times)

                 ;; entrenchment of constructions
                (:initial-cxn-entrenchement . 1/2)
                (:entrenchment-incf . 1/10)
                (:entrenchment-decf . -1/10)
                (:entrenchment-li . -1/100) ;; lateral inhibition
                
                ;; concept representations
                (:concept-representation . :distribution)
                (:distribution . :gaussian-welford)
                (:M2 . 0.0001)

                ;; prototype weight inits
                (:weight-update-strategy . :j-interpolation)
                (:initial-weight . 0)
                (:weight-incf . 1)
                (:weight-decf . -1)
                
                ;; conceptualisation
                ;(:concept-similarity-activation . 0.4)
                
                ;; alignment strategy
                (:punish-strategy . :punish-found-concepts)
                )))
  (setf *experiment* (make-instance 'cle-experiment :configuration *baseline-simulated*))
  (notify reset-monitors)
  (wi::reset))


;; 1. run then show last

(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interaction-in-web-interface)
  (run-interaction *experiment*))

(loop for i from 1 to 50
     do (run-interaction *experiment*))

(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor display-communicative-success)
  (activate-monitor print-a-dot-for-each-interaction)
  (loop for i from 1 to 50000
        do (run-interaction *experiment*)))

(progn
  (wi::reset)
  (deactivate-all-monitors)
  ;; run to find a scene without lex coherence
  (loop for i from 1 to 100
        for lex-coherence = (find-data (current-interaction *experiment*) 'lexicon-coherence)
        for speaker = (speaker (first (interactions *experiment*)))
        for hearer = (hearer (first (interactions *experiment*)))
        if (not lex-coherence)
          do (progn
               (format t "~%~a: ~a [~a, ~a]" i (index (current-scene (world *experiment*))) (id speaker) (id hearer))
               (return t))
        else
          do (run-interaction *experiment*)))

(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (run-interaction *experiment*
                   :scene (index (current-scene (world *experiment*)))
                   :agents (interacting-agents (first (interactions *experiment*))))
  (run-interaction *experiment*
                   :scene (index (current-scene (world *experiment*)))
                   :agents (reverse (interacting-agents (first (interactions *experiment*))))))

(setf saved-agents (interacting-agents (first (interactions *experiment*))))

(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (run-interaction *experiment*
                   :scene 2404
                   :agents saved-agents)
  (run-interaction *experiment*
                   :scene 2404
                   :agents (reverse saved-agents)))



(defun how-would-the-population-conceptualise (context topic)
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (notify event-context-determined *experiment*)
  (loop for agent in (agents *experiment*)
        do (clear-agent agent)
        do (set-data agent 'context context)
        do (set-data agent 'topic topic)
        do (add-element `((h2) ,(format nil "Agent ~a" (id agent))))
          
        do (let ((applied-cxn (speaker-conceptualise agent :times)))
             (add-element `((h3) ,(format nil "As a speaker applied: ~a" (form applied-cxn))))
             (add-element `((h3) ,(format nil " with competitors:")))
             (loop for other-cxn in (find-data agent 'meaning-competitors)
                   for similarity = (similar-concepts (meaning applied-cxn) (meaning other-cxn) :times)
                   for delta = (* similarity (get-configuration agent :entrenchment-li))
                     
                   do (add-element `((h4) ,(format nil "~a - punished by ~,4f [similarity: ~,2f]" (form other-cxn) delta similarity)))))
          
        do (let ((applied-cxn (hearer-conceptualise agent :times)))
             (add-element `((h3) ,(format nil "As a hearer applied: ~a" (form applied-cxn))))
             (add-element `((h3) ,(format nil " with competitors:")))
             (decide-competitors-hearer agent applied-cxn :times)
             (loop for other-cxn in (find-data agent 'meaning-competitors)
                   for similarity = (similar-concepts (meaning applied-cxn) (meaning other-cxn) :times)
                   for delta = (* similarity (get-configuration agent :entrenchment-li))
                     
                   do (add-element `((h4) ,(format nil "~a - punished by ~,4f [similarity: ~,2f]" (form other-cxn) delta similarity)))))
           ))

(defun how-would-the-population-conceptualise2 (context topic)
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (notify event-context-determined *experiment*)
  (loop for agent in (agents *experiment*)
        do (clear-agent agent)
        do (set-data agent 'context context)
        do (set-data agent 'topic topic)
        do (let* ((discriminating-cxns (search-discriminative-concepts agent))
                  (ledger (loop for tuple in discriminating-cxns
                                ;; discriminative-power
                                for topic-sim = (sigmoid (assqv :topic-sim tuple)) ;; sigmoid-ed!
                                for best-other-sim = (sigmoid (assqv :best-other-sim tuple)) ;; sigmoid-ed!
                                sum (abs (- topic-sim best-other-sim))))
                  (pomiwu (find (find-form-in-lexicon (lexicon agent) "pomiwu") discriminating-cxns :key #'(lambda (tuple) (assqv :cxn tuple))))
                  (vodixo (find (find-form-in-lexicon (lexicon agent) "vodixo") discriminating-cxns :key #'(lambda (tuple) (assqv :cxn tuple))))
                  (pomiwu-power (abs (- (sigmoid (assqv :topic-sim pomiwu)) (sigmoid (assqv :best-other-sim pomiwu)))))
                  (vodixo-power (abs (- (sigmoid (assqv :topic-sim vodixo)) (sigmoid (assqv :best-other-sim vodixo)))))
                  (applied-cxn (hearer-conceptualise agent :times)))
             (add-element `((h2) ,(format nil "Agent ~a: ~a with ~,3f" (id agent) (form applied-cxn) (score applied-cxn))))
             (add-element `((h3) ,(format nil "pomiwu: ent: ~,3f - power: ~,3f [~,3f/~,3f] => ~,3f"
                                          (score (assqv :cxn pomiwu))
                                          (/ pomiwu-power ledger)
                                          pomiwu-power
                                          ledger
                                          (* (/ pomiwu-power ledger) (score (assqv :cxn pomiwu)))
                                          )))
             (add-element `((h3) ,(format nil "vodixo: ent: ~,3f - power: ~,3f [~,3f/~,3f] => ~,3f"
                                          (score (assqv :cxn vodixo))
                                          (/ vodixo-power ledger)
                                          vodixo-power
                                          ledger
                                          (* (/ vodixo-power ledger) (score (assqv :cxn vodixo)))
                                          )))
             )))



;; three problems
;; 1. does it converge
;; 2. balance entrenchment vs lateral inhibition
;; 3. concept shifts very very very slowly its mean/stdev


;; 




(how-would-the-population-conceptualise (find-data (first saved-agents) 'context)
                                        (find-data (first saved-agents) 'topic))

(how-would-the-population-conceptualise2 (find-data (first saved-agents) 'context)
                                         (find-data (first saved-agents) 'topic))

(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 37)) "pomiwu"))
(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 40)) "pomiwu"))
(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 40)) "vodixo"))
           
(setf pomiwu (find-form-in-lexicon (lexicon (find-agent 40)) "pomiwu"))
(loop for el in (reverse (history (distribution (second (prototypes (meaning pomiwu))))))
      do (format t "~%~a" (if (eq (type-of el) 'cons) (second el) el)))

(first (history (distribution (second (prototypes (meaning pomiwu))))))

;; finding 1: welford works approximately!
;; => (33552 0.808 0.7995722 0.09013078)
                   0.7995723 0.09013091


(cons (average bingbong) (stdev bingbong)) ;; => (0.7995723 . 0.09013091)
(cons (average (append bingbong bingbong bingbong bingbong))
      (stdev (append bingbong bingbong bingbong bingbong))) ;; => (0.80880666 . 0.054536745) => (0.8132242 . 0.021056204)


(defun update-with-n-samples (concept nr-of-samples new-observation)
  (let* ((data (cons (second (reverse (history (distribution (second (prototypes concept))))))
                     (reverse (loop for el in (history (distribution (second (prototypes concept))))
                                    if (eq (type-of el) 'cons)
                                      collect (second el)))))
         (new-data (append data (loop for idx from 1 to nr-of-samples
                                      collect new-observation)))
         (new-concept (copy-object concept)))
    (setf (mean (distribution (second (prototypes new-concept)))) (average new-data))
    (setf (st-dev (distribution (second (prototypes new-concept)))) (stdev new-data))
    new-concept))

(defun discriminative-power (concept topic context)
  (abs (- (sigmoid (weighted-similarity topic concept))
          (sigmoid (loop for object in (remove topic (objects context))
                         maximize (weighted-similarity object concept))))))
  
;;  how many datapoints before pomiwu becomes better?
(let* ((topic (find-data (first saved-agents) 'topic))
       (context (find-data (first saved-agents) 'context))
       ;(concept (copy-object (meaning pomiwu)))
       (original (meaning pomiwu))
       (new-one (update-with-n-samples (meaning pomiwu) 10000 0.812))
       (original-discr (discriminative-power original topic context))
       (new-one-discr (discriminative-power new-one topic context))
       (old-ledger 1.908)
       (new-ledger (+ old-ledger (- new-one-discr original-discr))))
  (cons
   (* 0.996 (/ new-one-discr new-ledger))
   (* 0.970 (/ 0.626 new-ledger))))

;; original => (0.1358508 . 0.31824952)
;; + 1000   => (0.19540282 . 0.29621568)
;; + 5000   => (0.26276073 . 0.2712937)
;; + 10000  => (0.2793192 . 0.26516718)
      

        


(run-then-show-last *experiment* (* *subset-size* 100) :show t :display t)

(run-then-show-last *experiment* 1 :show t :display t)

;;;

(defun find-form-in-lexicon (lexicon form)
  "Given a lexicon and a form in underscore-notation, returns the associated cxn of the form."
  (setf res (loop for cxn in lexicon      
      if (string= (form cxn) form)
        collect cxn))
  (if res
    (car res)
    nil))

(defun find-agent (id)
  "Given an integer id, returns the associated agent"
  (let ((agent (loop for agent in (agents *experiment*)
                     for found-id = (second (split-sequence:split-sequence #\- (mkstr (id agent))))
                       do (when (equal (mkstr id) found-id)
                         (return agent)))))
    agent))


(setf dasoso (find-in-lexicon (lexicon (find-agent 98)) "dasoso"))

(display-lexicon (find-agent 98) :sort t)



