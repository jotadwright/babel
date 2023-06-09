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








;; test




(defun similarity-option1 (weights-c1 weights-c2 distances &key &allow-other-keys)
  (loop with concept1-weight-sum = (loop for weight in weights-c1 sum weight)
        with concept2-weight-sum = (loop for weight in weights-c2 sum weight)
        for w1 in weights-c1
        for w2 in weights-c2
        for sim in distances
        for avg-weight = (/ (+ (/ w1 concept1-weight-sum) (/ w2 concept2-weight-sum)) 2)
        for prototype-similarity = sim
        for sim-score = (* avg-weight prototype-similarity)
        sum sim-score))

(defun similarity-option2 (weights-c1 weights-c2 distances &key &allow-other-keys)
  (loop for w1 in weights-c1
        for w2 in weights-c2
        for sim in distances
        for avg-weight = (/ (/ (+ w1 w2) 2) (length distances))
        for prototype-similarity = sim
        for sim-score = (* avg-weight prototype-similarity)
        sum sim-score))

(defun similarity-option3 (weights-c1 weights-c2 distances &key &allow-other-keys)
  (loop for w1 in weights-c1
        for w2 in weights-c2
        for sim in distances
        for avg-weight = (/ (/ (+ w1 w2) 2) (length distances))
        for weight-similarity = (- 1 (abs (- weight1 weight2)))
        for prototype-similarity = sim
        for sim-score = (- 1
                           (/ (euclidian-distance (list avg-weights weight-similarity prototype-similarity)
                                                  (list 1 1 1))
                              (euclidian-distance (list 1 1 0)
                                                  (list 1 1 1))))
        sum sim-score))


(euclidean 0 0)

(defun euclidian-distance (point1 point2)
  "Euclidian distance between two points in n-dimensional space."
  (loop with sum = 0
        for idx from 0 to (- (length point1) 1)
        do (setf sum (+ sum (expt (- (nth idx point2) (nth idx point1)) 2)))
        finally (return (sqrt sum))))


(determine-similarity-with-weights 0 0 0)



(setf test-cases (list
                  (list
                   ;       R G B
                     (list 0 0 0) ;; weights concept 1
                     (list 0 0 0) ;; weights concept 2
                     (list 0 0 0) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 0 0 0) ;; weights concept 1
                     (list 1 1 1) ;; weights concept 2
                     (list 0 0 0) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 1 1 1) ;; weights concept 1
                     (list 0 0 0) ;; weights concept 2
                     (list 0 0 0) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 1 1 1) ;; weights concept 1
                     (list 1 1 1) ;; weights concept 2
                     (list 0 0 0) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 0 0 0) ;; weights concept 1
                     (list 0 0 0) ;; weights concept 2
                     (list 1 1 1) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 0 0 0) ;; weights concept 1
                     (list 1 1 1) ;; weights concept 2
                     (list 1 1 1) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 1 1 1) ;; weights concept 1
                     (list 0 0 0) ;; weights concept 2
                     (list 1 1 1) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 1 1 1) ;; weights concept 1
                     (list 1 1 1) ;; weights concept 2
                     (list 1 1 1) ;; sim between c1 and c2 in channel of column
                   )
                  
                  ))



(loop for test in test-cases
      for weights-c1 = (nth 0 test)
      for weights-c2 = (nth 1 test)
      for distances = (nth 2 test)
      collect (similarity weights-c1 weights-c2 distances))

(defun add-small-var (lst)
  (loop for el in lst
        for new-el = (if (< el 0.5)
                       (+ el 0.01)
                       (- el 0.01))
        collect new-el))

(loop for test in test-cases
      for weights-c1 = (nth 0 test)
      for weights-c2 = (nth 1 test)
      for distances = (nth 2 test)
      collect (similarity (add-small-var weights-c1)
                          (add-small-var weights-c2)
                          (add-small-var distances)))


;;;;;

(setf test-cases (list
                  (list
                   ;       R G B
                     (list 0 0 0) ;; weights concept 1
                     (list 0 0 0) ;; weights concept 2
                     (list 0 0 0) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 0 0 0) ;; weights concept 1
                     (list 1 1 1) ;; weights concept 2
                     (list 0 0 0) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 1 1 1) ;; weights concept 1
                     (list 1 1 1) ;; weights concept 2
                     (list 0 0 0) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 1 1 1) ;; weights concept 1
                     (list 1 1 1) ;; weights concept 2
                     (list 1 1 1) ;; sim between c1 and c2 in channel of column
                   )
                  
                  ))

(loop for test in test-cases
      for weights-c1 = (nth 0 test)
      for weights-c2 = (nth 1 test)
      for distances = (nth 2 test)
      collect (similarity weights-c1 weights-c2 distances))
