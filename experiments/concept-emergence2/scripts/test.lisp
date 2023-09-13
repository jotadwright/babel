(ql:quickload :cle)

(in-package :cle)

(progn 
    (setf *experiment* (cl-store:restore (babel-pathname :directory `("experiments"
                                                                      "concept-emergence2"
                                                                      "storage"
                                                                      "experiments"
                                                                      "7-supervised"
                                                                      "credit-fraud"
                                                                      "2023-09-12_0h32m23s-exp-c"
                                                                      "stores")
                                                         :name "1-history"
                                                         :type "store")))
    (set-configuration *experiment* :dot-interval 10)
    (set-configuration *experiment* :dataset-split "test")
    (set-configuration *experiment* :topic-sampling :random)
    (set-configuration *experiment* :align nil)
    (initialise-world *experiment*))


(setf (lexicon agent) (remove cxn (lexicon agent)))

(loop for agent in (agents *experiment*)
      for cxn = (find-in-lexicon agent "finaro")
      do (setf (lexicon agent) (remove cxn (lexicon agent))))

(loop for agent in (agents *experiment*)
      for cxn = (find-in-lexicon agent "tobetu")
      do (setf (lexicon agent) (remove cxn (lexicon agent))))

(loop for agent in (agents *experiment*)
      for cxn = (find-in-lexicon agent "poxuvo")
      do (setf (lexicon agent) (remove cxn (lexicon agent))))

(loop for agent in (agents *experiment*)
      for cxn = (find-in-lexicon agent "tavafo")
      do (setf (lexicon agent) (remove cxn (lexicon agent))))
(loop for agent in (agents *experiment*)
      for cxn = (find-in-lexicon agent "xokaze")
      do (setf (lexicon agent) (remove cxn (lexicon agent))))


(progn
  (wi::reset)
  (notify reset-monitors)
  (deactivate-all-monitors)
  (activate-monitor export-communicative-success)
  (activate-monitor export-lexicon-coherence)
    ;(activate-monitor export-unique-form-usage)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interaction-in-web-interface)
  (format t "~%---------- NEW GAME ----------~%")
  (time
   (loop with count = 0
         for i from 1 to 200
         do (run-interaction *experiment*)
         do (loop for agent in (agents *experiment*)
                  do (set-data agent 'context (get-data (car (interacting-agents *experiment*)) 'context))
                  do (set-data agent 'topic (get-data (car (interacting-agents *experiment*)) 'topic)))
         do (when (equal (majority-vote) "target")
              (incf count))
         do (format t "~% ~a: ~a |||| ~a/10, from ~a in scene ~a"
                    i
                    (majority-vote)
                    (count "target" (loop for i from 1 to 10
                                          for found-topic = (find-anomaly (find-agent i *experiment*))
                                          if found-topic
                                            collect (assqv :function (description found-topic))) :test #'equal)
                    (length (objects (get-data (car (interacting-agents *experiment*)) 'context)))
                    (id (current-scene (world *experiment*))))
         finally (return (/ count 200)))))


=> 157/200

(


(find-anomaly2 (find-agent 1 *experiment*))


(progn
  (wi::reset)
  (deactivate-all-monitors)
  
  ;(activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interaction-in-web-interface)
  (loop for idx from 1 to 1
        do (run-interaction *experiment*)
        do (loop for agent in (agents *experiment*)
                 do (set-data agent 'context (get-data (car (interacting-agents *experiment*)) 'context))
                 do (set-data agent 'topic (get-data (car (interacting-agents *experiment*)) 'topic)))
        ;do (display-lexicon (car (interacting-agents *experiment*)) :sort t)
        do (format t "~% ~a: ~a |||| ~a/10, from ~a"
                   idx
                   ;(majority-vote)
                   "nothing"
                   (count "target" (loop for i from 1 to 1
                                         collect (assqv :function (description (find-anomaly2 (find-agent 1 *experiment*))))) :test #'equal)
                   (length (objects (get-data (car (interacting-agents *experiment*)) 'context)))
                   )



           ))

(displa


(setf finaro (find-in-lexicon (find-agent 1 *experiment*) "finaro"))
(add-cxn-to-interface finaro)


(loop for obj in (objects (get-data (find-agent 1 *experiment*) 'context))
      do (format t "~% ~a -> ~,3f"
                 (id obj)
                 (weighted-similarity (find-agent 1 *experiment*) obj (meaning (find-in-lexicon (find-agent 1 *experiment*) "finaro")))))

(loop for obj in (objects (get-data (find-agent 1 *experiment*) 'context))
      do (format t "~% ~a -> ~,3f"
                 (id obj)
                 (weighted-similarity (find-agent 1 *experiment*) obj (meaning (find-in-lexicon (find-agent 1 *experiment*) "binitu")))))

;=> ((CLE::CLE-OBJECT-2041 . 2.889992E-9)
;   (CLE::CLE-OBJECT-2042 . 0.12158221) (CLE::CLE-OBJECT-2043 . 0.97671104) (CLE::CLE-OBJECT-2044 . 0.01743452) (CLE::CLE-OBJECT-2045 . 0.0026957854) (CLE::CLE-OBJECT-2046 . 0.0027299344) (CLE::CLE-OBJECT-2047 . 0.090485715) (CLE::CLE-OBJECT-2048 . 0.25366044))

(setf obj3 (nth 2 (objects (get-data (find-agent 1 *experiment*) 'context))))

(weighted-similarity (find-agent 1 *experiment*) obj3 (meaning finaro))




(display-lexicon (find-agent 1 *experiment*) :sort t)
(display-lexicon (find-agent 8 *experiment*) :sort t)

(defun majority-vote ()
  (let ((big-ht (loop with ht = (make-hash-table)
                      for i from 1 to 10
                      for selected-object = (find-anomaly (find-agent i *experiment*))
                      for id = (id selected-object)
                      for function = (assqv :function (description selected-object))
                      if (gethash id ht)
                        do (setf (gethash id ht) (cons selected-object (gethash id ht)))
                      else
                        do (setf (gethash id ht) (list selected-object))
                      finally (return ht))))
    (loop with biggest-key = nil
          with largest = -1
          for key being the hash-keys of big-ht
            using (hash-value value)
          if (> (length value) largest)
            do (progn
                 (setf biggest-key key)
                 (setf largest (length value)))
          finally (return (assqv :function (description (car (gethash biggest-key big-ht))))))))

(defun find-anomaly (agent)
  "Finds the best concept (and its direct competitors) for a given scene and topic.

   The best concept corresponds to the concept that maximises
   the multiplication of its entrenchment score and its discriminative power."
  (let* ((threshold (get-configuration (experiment agent) :similarity-threshold))
         ;(topic (get-data agent 'topic))
         (context (objects (get-data agent 'context)))
         (best-topic nil)
         (best-score -1)
         (best-cxn nil))
    ;; case 1: look only at entrenched concepts first
    ;; this heuristic is possible as the score is based on the multiplication
    (loop for topic in context
          do (loop for cxn in (lexicon agent)
                   for concept = (meaning cxn)
                   for rest-context = (remove topic context)
                   for topic-sim = (weighted-similarity agent topic concept)
                   for best-other-sim = (loop for object in rest-context
                                              maximize (weighted-similarity agent object concept))
                   for discriminative-power = (abs (- topic-sim best-other-sim))
                   if (and (> topic-sim (+ best-other-sim threshold))
                           (< best-other-sim 0.12)
                           (> (* discriminative-power (score cxn)) best-score))
                     do (progn
                          #|(when best-cxn
                            (setf competitors (cons best-cxn competitors)))|#
                          (setf best-score (* discriminative-power (score cxn)))
                          (setf best-topic topic)
                          (setf best-cxn cxn))
                   ;else
                   ;  do (setf competitors (cons cxn competitors))
                        ))
    best-topic))

(defun find-anomaly2 (agent)
  "Finds the best concept (and its direct competitors) for a given scene and topic.

   The best concept corresponds to the concept that maximises
   the multiplication of its entrenchment score and its discriminative power."
  (let* ((threshold (get-configuration (experiment agent) :similarity-threshold))
         ;(topic (get-data agent 'topic))
         (context (objects (get-data agent 'context)))
         (best-topic nil)
         (best-score -1)
         (best-cxn nil))
    ;; case 1: look only at entrenched concepts first
    ;; this heuristic is possible as the score is based on the multiplication
    (loop for topic in context
          do (format t "~% ====== NEXT OBJECT: ~a" (id topic))
          do (loop for cxn in (lexicon agent)
                   for concept = (meaning cxn)
                   for rest-context = (remove topic context)
                   for topic-sim = (weighted-similarity agent topic concept)
                   for best-other-sim = (loop for object in rest-context
                                              maximize (weighted-similarity agent object concept))
                   for discriminative-power = (abs (- topic-sim best-other-sim))
                   do (format t "~% ~a => ~,3f vs ~,3f => ~,3f, ~,3f"
                              (form cxn)
                              topic-sim
                              best-other-sim
                              (- topic-sim best-other-sim)
                              (* discriminative-power (score cxn)))
                   if (and (> topic-sim (+ best-other-sim threshold))
                           (< best-other-sim 0.12)
                           (> (* discriminative-power (score cxn)) best-score))
                     do (progn
                          #|(when best-cxn
                            (setf competitors (cons best-cxn competitors)))|#
                          (setf best-score (* discriminative-power (score cxn)))
                          (setf best-topic topic)
                          (setf best-cxn cxn)))
          if best-topic
            do (format t "~% === CURRENT BEST OBJECT: ~a based on ~a w/ ~,3f" (id best-topic) (form best-cxn) best-score)
                   
                   ;else
                   ;  do (setf competitors (cons cxn competitors))
             )
    (when best-topic
      (format t "~% ====== CHOSEN OBJECT: ~a based on ~a" (id best-topic) (form best-cxn)))
    best-topic))

(setf htt (majority-vote))

#|(defun how-would-the-population-conceptualise (context topic)
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
        do (format t "The value associated with the key ~S is ~S~%" key value)))|#