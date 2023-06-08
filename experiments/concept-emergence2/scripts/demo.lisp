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
                ;(:run-fast . t)
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
  (loop for i from 1 to 10000
        do (run-interaction *experiment*)))


;; 2. run x interactions with tiiw
(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interaction-in-web-interface)
  (loop for idx from 1 to 2
        do (run-interaction *experiment*)))

(defmethod after-interaction ((experiment cle-experiment))
  #|(align (speaker experiment))
  (align (hearer experiment))|#
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
               (return (format nil "~a: ~a [~a, ~a] -> topic: ~a" i (index (current-scene (world *experiment*))) (id speaker) (id hearer) (find-data speaker 'topic))))
        else
          do (run-interaction *experiment*)))

=> "7: 5862 [AGENT-91, AGENT-83] -> topic: <cle-object: attributes: (R . 0.177), (G . 0.289), (B . 0.822)>"
(progn
  (setf saved-agents (interacting-agents (current-interaction *experiment*)))
  (setf saved-scene  (index (current-scene (world *experiment*))))
  (setf saved-topic (find-data (first saved-agents) 'topic)))

;; run the saved scene agent
(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  ;(add-element `((h2) ,(format nil "Topic: ~a" saved-topic)))
  (run-interaction *experiment*
                   :scene saved-scene
                   :agents saved-agents)
  (run-interaction *experiment*
                   :scene saved-scene
                   :agents (reverse saved-agents)))

;; experiment with a specific cxn
(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 83)) "fimomu"))
(setf cxn-test (find-form-in-lexicon (lexicon (find-agent 23)) "vereko"))

(string-equal "ragifa" (form (first (lexicon (car (agents *experiment*))))))

(loop for agent in (agents *experiment*)

      for cxn in (lexicon agent)
      collect cxn)
        
(find-form-in-lexicon (lexicon agent) "revuno")



;; conceptualise a scene/topic combo with all agents
(how-would-the-population-conceptualise (find-data (first saved-agents) 'context)
                                        (third (objects (find-data (first saved-agents) 'context)))
                                        ;(find-data (first saved-agents) 'topic)
                                        )

;; same as previous, but only show a given list
(how-would-the-population-conceptualise2 (find-data (first saved-agents) 'context)
                                         (third (objects (find-data (first saved-agents) 'context)))
                                         ;(find-data (first saved-agents) 'topic)
                                         ;(list)
                                         ;(list "gelowo" "fimomu")
                                         (list "deweti" "revuno")
                                         )

(let ((applied-cxn (find-form-in-lexicon (lexicon (first (last (agents *experiment*)))) "xiseto"))
      (other-cxn (find-form-in-lexicon (lexicon (first (last (agents *experiment*)))) "lopete")))
  (similar-concepts (meaning applied-cxn) (meaning other-cxn) :times))


(display-lexicon (first (last (agents *experiment*))) :sort t)







;; three problems
;; 1. does it converge
;; 2. balance entrenchment vs lateral inhibition
;; 3. concept shifts very very very slowly its mean/stdev

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


(loop for form in (list "sulamo" "razifu" "zizisu")
      do (add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 43)) form)))

(objects (find-data (first saved-agents) 'context))


(display-lexicon (find-agent 43) :sort t)

(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 37)) "pomiwu"))
(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 23))  "fiwezo"))
(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 23)) "vobuwi"))
(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 23)) "rewunu"))
           
(setf pefudi (find-form-in-lexicon (lexicon (find-agent 89)) "revuno"))
(setf prototype-b (first (prototypes (meaning pefudi))))
(loop for el in (reverse (history (distribution (first (prototypes (meaning pefudi))))))
      do (format t "~%~,3f" (second el)))

(first (history (distribution (second (prototypes (meaning pomiwu))))))

;; finding 1: welford works approximately!
;; => (33552 0.808 0.7995722 0.09013078)
                   0.7995723 0.09013091


(cons (average bingbong) (stdev bingbong)) ;; => (0.7995723 . 0.09013091)
(cons (average (append bingbong bingbong bingbong bingbong))
      (stdev (append bingbong bingbong bingbong bingbong))) ;; => (0.80880666 . 0.054536745) => (0.8132242 . 0.021056204)


  
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




(setf dasoso (find-in-lexicon (lexicon (find-agent 98)) "dasoso"))

(display-lexicon (find-agent 98) :sort t)

(defmethod exemplar-similarity ((exemplar number) (prototype prototype) &key (max-z-score 1)) ;; TODO: MAJOR QUESTION (see obsidian)
  "Similarity on the level of a single prototype."
  ;; similarity measure between [-inf,1]
  (let* ((distribution (distribution prototype))
         (st-dev (st-dev distribution))
         (z-score (if (not (eq st-dev 0.0))
                    ;; z-score formula + absolute value
                    (abs (/ (- exemplar (mean distribution)) st-dev))
                    0))
         (sim (/ (- max-z-score z-score) max-z-score)))
    sim))


(defun z-scorer (exemplar mu st-dev &key max-z-score)
  (/ (- max-z-score (abs (/ (- exemplar mu) st-dev))) max-z-score))

(z-scorer 0 0 1 :max-z-score 2)
(z-scorer 1 0 1 :max-z-score 2)
(z-scorer 2 0 1 :max-z-score 2)

(z-scorer 0 0 2 :max-z-score 2)
(z-scorer 1 0 2 :max-z-score 2)
(z-scorer 2 0 2 :max-z-score 2)

;;;;

(cl-store:store *experiment*
                (babel-pathname :directory '("experiments"
                                             "concept-emergence2")
                                :name "2023-06-07-exp-colors"
                                :type "store"))



(monitors::print-all-monitors)

(float 121/125)

(monitors::get-values (monitors::get-monitor 'record-lexicon-coherence))

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