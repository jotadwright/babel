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
                   for delta = (* similarity (get-configuration (experiment agent) :entrenchment-li))
                     
                   do (add-element `((h4) ,(format nil "~a - punished by ~,4f [similarity: ~,2f]" (form other-cxn) delta similarity)))))
           ))

(defun add-raw-powers (agent discriminating-cxns form)
  (let* ((cxn (find (find-form-in-lexicon (lexicon agent) form) discriminating-cxns :key #'(lambda (tuple) (assqv :cxn tuple))))
         (power (if cxn
                  (abs (- (assqv :topic-sim cxn) (assqv :best-other-sim cxn)))
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


(defun is-discriminative-cases (object other-objects)
  "Checks if the object has a single channel dimension that is different from all other objects."
  (loop with found = nil
        for (attr . val) in (description object)
        if (and (is-channel-available attr (attributes object))
                (loop for other-object in other-objects
                      for other-val = (assqv attr (description other-object))
                      always (not (equal val other-val))))
          do (if found
               (return 2) ;; returns nil if more than once you find a duplicate attribute
               (setf found t))
          ;; returns nil if there is no discriminative channel
        finally (return (if found 1 0))))

(defun is-discriminative-strict (object other-objects)
  "Checks if the object has a single channel dimension that is different from all other objects."
  (loop with found = nil
        for (attr . val) in (description object)
        if (and (is-channel-available attr (attributes object))
                (loop for other-object in other-objects
                      for other-val = (assqv attr (description other-object))
                      always (not (equal val other-val))))
          do (if found
               ;; returns nil if more than once you find a duplicate attribute
               (return nil)
               ;; set found to true (for the first match)
               (setf found t))
          ;; returns nil if there is no discriminative channel
        finally (return found)))

(defun find-possible-scenes (saved)
  (loop for tuple in saved
        for saved-agent = (first tuple)
        for saved-scene = (second tuple)
        for saved-topic = (third tuple)
        for channels-in-play = (get-configuration *experiment* :channels)
        for symbolic-clevr-context = (get-scene-by-index (world *experiment*) saved-scene)
        for cle-scene = (clevr->simulated symbolic-clevr-context channels-in-play)
        for cle-topic = (find saved-topic (objects cle-scene)
                              :test (lambda (x el) (equal (description x) (description el))))
        if cle-topic
          do (setf possible (is-discriminative-strict cle-topic (remove cle-topic (objects cle-scene))))
        if possible
          collect (cons saved-scene possible)))

(defun testi ()
  (progn
    (wi::reset)
    (deactivate-all-monitors)
    ;; run to find a scene without lex coherence
    (loop with not-coherent = '()
          with not-solvable+coherent-0 = '()
          with not-solvable+not-coherent-0 = '()
          with not-solvable+coherent-2 = '()
          with not-solvable+not-coherent-2 = '()
          with scenes-with-coherence = '()
          for i from 1 to 5000
          for speaker = (speaker (first (interactions *experiment*)))
          for hearer = (hearer (first (interactions *experiment*)))
          for lex-coherence = (find-data (current-interaction *experiment*) 'lexicon-coherence)
          for comm-success = (communicated-successfully (first (interactions *experiment*)))
          if (and (not lex-coherence)
                  (eq (is-discriminative-cases (find-data (first (interacting-agents (current-interaction *experiment*))) 'topic)
                                               (remove (find-data (first (interacting-agents (current-interaction *experiment*))) 'topic)
                                                       (objects (find-data (first (interacting-agents (current-interaction *experiment*))) 'context))))
                      0)
                  )
            do (progn
                 (setf not-solvable+not-coherent-0 (cons (list (interacting-agents (current-interaction *experiment*))
                                                               (index (current-scene (world *experiment*)))
                                                               (find-data (first (interacting-agents (current-interaction *experiment*))) 'topic)
                                                               comm-success)
                                                         not-solvable+not-coherent-0
                                                         ))
                 (run-interaction *experiment*))
          else if (and (not lex-coherence)
                       (eq (is-discriminative-cases (find-data (first (interacting-agents (current-interaction *experiment*))) 'topic)
                                                    (remove (find-data (first (interacting-agents (current-interaction *experiment*))) 'topic)
                                                            (objects (find-data (first (interacting-agents (current-interaction *experiment*))) 'context))))
                           2)
                       )
                 do (progn
                      (setf not-solvable+not-coherent-2 (cons (list (interacting-agents (current-interaction *experiment*))
                                                                    (index (current-scene (world *experiment*)))
                                                                    (find-data (first (interacting-agents (current-interaction *experiment*))) 'topic)
                                                                    comm-success)
                                                              not-solvable+not-coherent-2
                                                              ))
                      (run-interaction *experiment*))
            else if (eq (is-discriminative-cases (find-data (first (interacting-agents (current-interaction *experiment*))) 'topic)
                                                  (remove (find-data (first (interacting-agents (current-interaction *experiment*))) 'topic)
                                                          (objects (find-data (first (interacting-agents (current-interaction *experiment*))) 'context))))
                        0)
                   do (progn
                        (setf not-solvable+coherent-0 (cons (list (interacting-agents (current-interaction *experiment*))
                                                                  (index (current-scene (world *experiment*)))
                                                                  (find-data (first (interacting-agents (current-interaction *experiment*))) 'topic)
                                                                  comm-success)
                                                            not-solvable+coherent-0
                                                            ))
                        (run-interaction *experiment*))
                 else if (eq (is-discriminative-cases (find-data (first (interacting-agents (current-interaction *experiment*))) 'topic)
                                                       (remove (find-data (first (interacting-agents (current-interaction *experiment*))) 'topic)
                                                               (objects (find-data (first (interacting-agents (current-interaction *experiment*))) 'context))))
                             2)
                        do (progn
                             (setf not-solvable+coherent-2 (cons (list (interacting-agents (current-interaction *experiment*))
                                                                       (index (current-scene (world *experiment*)))
                                                                       (find-data (first (interacting-agents (current-interaction *experiment*))) 'topic)
                                                                       comm-success)
                                                                 not-solvable+coherent-2
                                                                 ))
                             (run-interaction *experiment*))
                   else if (not lex-coherence)
                          do (progn
                               (setf not-coherent (cons (list (interacting-agents (current-interaction *experiment*))
                                                              (index (current-scene (world *experiment*)))
                                                              (find-data (first (interacting-agents (current-interaction *experiment*))) 'topic)
                                                              comm-success)
                                                        not-coherent
                                                        ))
                               (run-interaction *experiment*)
                               )
                        else
                          do (progn
                               (setf scenes-with-coherence (cons (list (interacting-agents (current-interaction *experiment*))
                                                                       (index (current-scene (world *experiment*)))
                                                                       (find-data (first (interacting-agents (current-interaction *experiment*))) 'topic)
                                                                       comm-success)
                                                                 scenes-with-coherence
                                                                 ))
                               (format t "~a " i)
                               (run-interaction *experiment*))
          finally (return (list not-coherent
                                not-solvable+coherent-0 not-solvable+coherent-2
                                not-solvable+not-coherent-0 not-solvable+not-coherent-2
                                scenes-with-coherence)))))




(defun testi2 (agent form interactions)
  (progn
    (wi::reset)
    (deactivate-all-monitors)
    ;; run to find a scene without lex coherence
    (loop with saved = '()
          for i from 1 to interactions
          for lex-coherence = (find-data (current-interaction *experiment*) 'lexicon-coherence)
          for speaker = (speaker (first (interactions *experiment*)))
          for hearer = (hearer (first (interactions *experiment*)))
          if (and (not lex-coherence)
                  (eq agent (first (interacting-agents (current-interaction *experiment*))))
                  (string-equal form
                                (form (get-data (first (interacting-agents (current-interaction *experiment*))) 'applied-cxn))))
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
          finally (return saved ))))





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


(defun count-entries (agent)
  (loop with options = (loop for cxn in (lexicon agent)
                             for channels = (loop for prototype in (prototypes (meaning cxn))
                                                  for channel = (channel prototype)
                                                  for weight = (weight prototype)
                                                  if (> weight 0.9)
                                                    collect channel)
                             if (> (score cxn) 0.5)
                               collect channels)
        with counter = (make-hash-table)
        with keys = (mapcar (lambda (lst)
                              (list-of-strings->string (mapcar (lambda (x) (mkstr x)) lst)
                                                       :separator "-"))
                            options)
        for raw-key in keys
        for key = (intern raw-key)

          
        if (gethash key counter)
          do (setf (gethash key counter) (1+ (gethash key counter)))
        else
          do (setf (gethash key counter) 1)
        finally (return counter)))

(defun format-count-entries (counter)
  (loop with all-keys = (mapcar #'mkstr (hash-keys counter))
        with sorted-keys = (sort all-keys #'string<)
        for key in sorted-keys
        for value = (gethash (intern key) counter)
        do (format t "The value associated with the key ~S is ~S~%" key value)))