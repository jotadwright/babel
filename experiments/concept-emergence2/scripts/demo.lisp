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
  (setf *subset-size* (length *scene-ids*))
  (defparameter *baseline-simulated*
    (make-configuration
     :entries `(;; monitoring
                (:dot-interval . 1000)
                ;; setup interacting agents
                (:interacting-agents-strategy . :standard)
                (:population-size . 10)
                ;; setup scene
                (:scene-sampling . :deterministic)
                (:topic-sampling . :english-concepts)
                (:clevr-channels
                 ;,'area ;; size
                 ,'color ;; color
                 ;,'roughness
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
                (:M2 . 0.0001)

                ;; prototype weight inits
                (:weight-update-strategy . :j-interpolation)
                (:initial-weight . 0)
                (:weight-incf . 1)
                (:weight-decf . -1)
                
                ;; alignment strategy
                (:punish-strategy . :punish-found-concepts)
                )))
  (setf *experiment* (make-instance 'cle-experiment :configuration *baseline-simulated*))
  (notify reset-monitors)
  (wi::reset))


;; 1. run x interactions
(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor display-communicative-success)
  (activate-monitor print-a-dot-for-each-interaction)
  (loop for i from 1 to 35000
        do (run-interaction *experiment*)))


;; 2. run 1 interaction
(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interaction-in-web-interface)
  (loop for idx from 1 to 10
        do (run-interaction *experiment*)))

(defmethod after-interaction ((experiment cle-experiment))
  #|(align (speaker experiment))
  (align (hearer experiment))|#
  )

(defmethod after-interaction ((experiment cle-experiment))
  (align (speaker experiment))
  (align (hearer experiment))
  )



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
               (return (format nil "~a: ~a [~a, ~a]" i (index (current-scene (world *experiment*))) (id speaker) (id hearer))))
        else
          do (run-interaction *experiment*)))

=> "1: 13397 [AGENT-173, AGENT-179]"

(setf saved-agents (interacting-agents (current-interaction *experiment*)))
(setf saved-scene  (index (current-scene (world *experiment*))))

(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (run-interaction *experiment*
                   :scene saved-scene
                   :agents saved-agents)
  (run-interaction *experiment*
                   :scene saved-scene
                   :agents (reverse saved-agents)))

(display-lexicon (find-agent 237))
;; fiwezo and vobuwi


(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 23)) "vereko"))
(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 32)) "rafime"))
(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 32)) "mofufe"))


(setf vereko (find-form-in-lexicon (lexicon (find-agent 23)) "vereko"))


(let* ((copy-distrib (copy-object (distribution (third (prototypes (meaning vereko)))))))
  (format t "~%0: ~a" copy-distrib)
  ;(setf (nr-of-samples copy-distrib) 100)
  (loop for idx from 0 to 20
        do (welford-update 0.295 copy-distrib)
        do (format t "~%~a: ~a" idx copy-distrib)))

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
           ))

(defun add-raw-powers (agent discriminating-cxns form ledger)
  (let* ((cxn (find (find-form-in-lexicon (lexicon agent) form) discriminating-cxns :key #'(lambda (tuple) (assqv :cxn tuple))))
         (power (if cxn
                  (abs (- (sigmoid (assqv :topic-sim cxn)) (sigmoid (assqv :best-other-sim cxn))))
                  nil)))
    (if cxn
      (add-element `((h3) ,(format nil "~a: ent: ~,3f - power: ~,3f [~,3f/~,3f] => ~,3f"
                                   form
                                   (score (assqv :cxn cxn))
                                   (/ power ledger)
                                   power
                                   ledger
                                   (* (/ power ledger) (score (assqv :cxn cxn)))
                                   )))
      (add-element `((h3) ,(format nil "~a: no entry"
                                   form))))))

(defun how-would-the-population-conceptualise2 (context topic options)
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
                  (applied-cxn (hearer-conceptualise agent :times)))
             (add-element `((h2) ,(format nil "Agent ~a: ~a with ~,3f" (id agent) (form applied-cxn) (score applied-cxn))))
             (loop for option in options
                   do (add-raw-powers agent discriminating-cxns option ledger))
             (loop for option in options
                   for search = (find (find-form-in-lexicon (lexicon agent) option) discriminating-cxns :key #'(lambda (tuple) (assqv :cxn tuple)))
                   if search
                     do (add-cxn-to-interface (assqv :cxn search)))
             
             )))



;; three problems
;; 1. does it converge
;; 2. balance entrenchment vs lateral inhibition
;; 3. concept shifts very very very slowly its mean/stdev


;; 


(weighted-similarity (find-data (first saved-agents) 'topic) (meaning (find-form-in-lexicon (lexicon (find-agent 23)) "fiwezo")))
(weighted-similarity (find-data (first saved-agents) 'topic) (meaning (find-form-in-lexicon (lexicon (find-agent 23)) "vobuwi")))


(loop for tuple in (search-discriminative-concepts (find-agent 23))
      for topic-sim = (assqv :topic-sim tuple )
      for best-other-sim = (assqv :best-other-sim tuple )
      collect (list (cons :cxn (assqv :cxn tuple))
                    (cons :topic-sim topic-sim)
                    (cons :topic-sim-sigmoid (sigmoid (assqv :topic-sim tuple )))
                    (cons :best-other-sim best-other-sim)
                    (cons :best-other-sim-sigmoid (sigmoid (assqv :best-other-sim tuple)))
                    (cons :diff (abs (- topic-sim best-other-sim)))
                    (cons :diff-sigmoid (abs (- (sigmoid (assqv :topic-sim tuple)) (sigmoid (assqv :best-other-sim tuple)))))
                    ))


(lexicon (first (agents *experiment*)))
(how-would-the-population-conceptualise (find-data (first saved-agents) 'context)
                                        (second (objects (find-data (first saved-agents) 'context)))
                                        ;(find-data (first saved-agents) 'topic)
                                        )

(how-would-the-population-conceptualise2 (find-data (first saved-agents) 'context)
                                         (second (objects (find-data (first saved-agents) 'context)))
                                         ;(find-data (first saved-agents) 'topic)
                                         (list "vereko" "pexiba" "xusabi"))

(loop for form in (list "sulamo" "razifu" "zizisu")
      do (add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 43)) form)))

(objects (find-data (first saved-agents) 'context))


(display-lexicon (find-agent 43) :sort t)

(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 37)) "pomiwu"))
(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 23))  "fiwezo"))
(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 23)) "vobuwi"))
(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 23)) "rewunu"))
           
(setf pefudi (find-form-in-lexicon (lexicon (find-agent 23)) "vereko"))
(loop for el in (reverse (history (distribution (third (prototypes (meaning pefudi))))))
      do (format t "~%~,3f" (second el)))

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



