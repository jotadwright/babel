(in-package :cle)

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

(defun add-raw-powers (agent discriminating-cxns form)
  (let* ((cxn (find (find-form-in-lexicon (lexicon agent) form) discriminating-cxns :key #'(lambda (tuple) (assqv :cxn tuple))))
         (power (if cxn
                  (abs (- (sigmoid (assqv :topic-sim cxn)) (sigmoid (assqv :best-other-sim cxn))))
                  nil)))
    (if cxn
      (add-element `((h3) ,(format nil "~a: ent: ~,3f - power: ~,3f => ~,3f"
                                   form
                                   (score (assqv :cxn cxn))
                                   power
                                   (* power (score (assqv :cxn cxn)))
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
                  (applied-cxn (hearer-conceptualise agent :times)))
             (add-element `((h2) ,(format nil "Agent ~a: ~a with ~,3f" (id agent) (form applied-cxn) (score applied-cxn))))
             (loop for option in options
                   do (add-raw-powers agent discriminating-cxns option))
             (loop for option in options
                   for search = (find (find-form-in-lexicon (lexicon agent) option) discriminating-cxns :key #'(lambda (tuple) (assqv :cxn tuple)))
                   if search
                     do (add-cxn-to-interface (assqv :cxn search)))
             
             )))


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

(defun count-cxn-usage (cxn last-interaction window)
  (let ((history (history cxn))
        (stop-condition (- last-interaction window)))
    (loop with counter = 0
          for el in history
          if (> el stop-condition)
            do (incf counter)
          else
            do (return counter))))


(progn
  
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



  (let ((x (length saved)))
    (format nil "~a vs ~a"
            (float (/ (- 5000 (length saved)) 5000))
            (float (/ (- 5000 (- (length saved) not-possible-amount)) 5000)))))

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