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
  (loop for idx from 1 to 500
        do (run-interaction *experiment*)))

(defmethod after-interaction ((experiment cle-experiment))
  #|(align (speaker experiment))
  (align (hearer experiment))|#
  )



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
               (setf saved (cons (format nil "% ~a: ~a [~a, ~a] -> topic: ~a" i (index (current-scene (world *experiment*))) (id speaker) (id hearer) (find-data speaker 'topic)) saved))
               (run-interaction *experiment*))
        else
          do (progn
               (format nil "% ~a: ~a [~a, ~a] -> topic: ~a" i (index (current-scene (world *experiment*))) (id speaker) (id hearer) (find-data speaker 'topic))
               (run-interaction *experiment*))))

=> "6: 970 [AGENT-28, AGENT-23] -> topic: <cle-object: attributes: (AREA . 0.708), (R . 0.48700002), (G . 0.298), (B . 0.07), (ROUGHNESS . 0.80100006)>"

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
                                        (first (objects (find-data (first saved-agents) 'context)))
                                        ;(find-data (first saved-agents) 'topic)
                                        )

(add-cxn-to-interface (find-form-in-lexicon (lexicon (find-agent 23)) "dosune"))

;; same as previous, but only show a given list
(how-would-the-population-conceptualise2 (find-data (first saved-agents) 'context)
                                         (third (objects (find-data (first saved-agents) 'context)))
                                         ;(find-data (first saved-agents) 'topic)
                                         (list "bewabu" "pipufe")
                                         ;(list "gelowo" "fimomu")
                                         ;(list "deweti" "revuno")
                                         )

(let ((applied-cxn (find-form-in-lexicon (lexicon (first (agents *experiment*))) "lezemu"))
      (other-cxn (find-form-in-lexicon (lexicon (first (agents *experiment*))) "fosopi")))
  (similar-concepts2 (meaning applied-cxn) (meaning other-cxn) :times))

(let ((applied-cxn (find-form-in-lexicon (lexicon (first (agents *experiment*))) "lezemu"))
      (other-cxn (find-form-in-lexicon (lexicon (first (agents *experiment*))) "lezemu")))
  (similar-concepts2 (meaning applied-cxn) (meaning other-cxn) :times))

(add-cxn-to-interface (find-form-in-lexicon (lexicon (first (agents *experiment*))) "lezemu")
                      :certainty-threshold 0.1)

(add-cxn-to-interface (find-form-in-lexicon (lexicon (first (agents *experiment*))) "fosopi")
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
        sum sim-score))
        collect (cons (channel proto1) sim-score)))

=> ((CLE::ROUGHNESS . 0.062145736)
     (UTILS:B . 0.0)
     (CLE::G . 0.001204835)
     (CLE::R . 0.0)
     (CLE::AREA . 0.10917586))

(setf lezemu (find-form-in-lexicon (lexicon (first (agents *experiment*))) "lezemu"))


(display-lexicon (first (last (agents *experiment*))) :sort t)

(setf positive (loop for cxn in (lexicon (first (agents *experiment*)))
                     if (> (score cxn) 0.1)
                       collect cxn))



(setf abba (loop for cxn in (lexicon (first (agents *experiment*)))
                 for similarity = (loop for other-cxn in (lexicon (first (agents *experiment*)))
                                        collect (format nil "~,2f" (similar-concepts (meaning cxn) (meaning other-cxn) :times)))
                 collect similarity))

(setf abba (loop for cxn in positive
                 for similarity = (loop for other-cxn in positive
                                        collect (format nil "~,2f" (similar-concepts2 (meaning cxn) (meaning other-cxn) :times)))
                 collect (cons (form cxn) similarity)))

(setf abba (loop for cxn in positive
                 for similarity = (loop for other-cxn in positive
                                        collect (format nil "~,2f" (similar-concepts (meaning cxn) (meaning other-cxn) :times)))
                 collect (form cxn)))

(loop for form in (list "wubabi" "gopipe" "sotuxu" "pagipu" "netuma" "mavemi" "vupose" "vasadi" "botuwi" "gitipe" "kizovi" "wuzadu")
      for cxn = (find-form-in-lexicon (lexicon (first (agents *experiment*))) form)
      do (add-cxn-to-interface cxn :certainty-threshold 0.00000001))

(add-cxn-to-interface (first (lexicon (first (agents *experiment*)))))
(add-cxn-to-interface (nth 13 (lexicon (first (agents *experiment*)))))

(add-cxn-to-interface (nth 7 (lexicon (first (agents *experiment*)))))

(loop for idx from 1 to (lexicon (first (agents *experiment*)))
      for lst in abba
      for cxn = (nth idx lst)
      for rest = (remove cxn lst)
      for max-pos = (extremum-position rest :test #'>)
      collect (list cxn max-pos

(let ((most-similar (extremum-position (rest (first abba)) :test #'>))
      (


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


(display-lexicon (first (agents *experiment*)) :sort t)

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