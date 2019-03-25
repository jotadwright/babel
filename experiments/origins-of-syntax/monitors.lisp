(in-package :origins-of-syntax)

(define-event success-determined (communicated-successfully? t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing a dot for each interaction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor print-a-dot-for-each-interaction+numbers)

(define-event-handler (print-a-dot-for-each-interaction+numbers interaction-finished)
  (if (= 0 (mod (interaction-number interaction) 100))
    (format t ". (~a)~%" (interaction-number interaction))
    (format t ".")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monitoring Communicative Success) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor record-communicative-success
    :class 'data-recorder
    :average-window 100
    :documentation "Records the game outcome of each game (1 or 0).")

(define-event-handler (record-communicative-success success-determined)
  (if communicated-successfully?
    (record-value monitor 1)
    (record-value monitor 0)))

(define-monitor export-communicative-success
    :class 'lisp-data-file-writer
    :documentation "Exports communicative success"
    :data-sources '(record-communicative-success)
    :file-name (babel-pathname :name "communicative-success" :type "lisp" 
                               :directory '("experiments" "origins-of-syntax" "raw-data"))
    :add-time-and-experiment-to-file-name nil
    :column-separator " "
    :comment-string "#")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monitoring Number of Constructions per Agent ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor record-nr-of-constructions
    :class 'data-recorder
    :average-window 10
    :documentation "Records the number of constructions per agent.")

(define-event-handler (record-nr-of-constructions interaction-finished)
  (let* ((all-agents (population experiment))
         (nr-of-agents (length all-agents))
         (total-nr-of-cxns (sum (mapcar #'(lambda (agent)
                                            (let ((grammatical-cxns-w-non-zero-score (loop for cxn in (constructions (grammar agent))
                                                                                           when (and (> (attr-val cxn :score) 0.0)
                                                                                                     (not (string= "LEX" (attr-val cxn :label))))
                                                                                           collect cxn)))
                                              (length grammatical-cxns-w-non-zero-score)))
                                        all-agents))))
    (record-value monitor (/ total-nr-of-cxns nr-of-agents))))

(define-monitor export-nr-of-constructions
    :class 'lisp-data-file-writer
    :documentation "Exports communicative success"
    :data-sources '(record-nr-of-constructions)
    :file-name (babel-pathname :name "nr-of-constructions" :type "lisp" 
                               :directory '("experiments" "origins-of-syntax" "raw-data"))
    :add-time-and-experiment-to-file-name nil
    :column-separator " "
    :comment-string "#")


(define-monitor export-nr-of-constructions+communicative-success
    :class 'lisp-data-file-writer
    :documentation "Exports nr of constructions and the communicative success"
    :data-sources '(record-communicative-success
                    record-nr-of-constructions)
    :file-name (babel-pathname :name "nr-of-constructions+communicative-success" :type "lisp" 
                               :directory '("experiments" "origins-of-syntax" "raw-data"))
    :add-time-and-experiment-to-file-name nil
    :column-separator " "
    :comment-string "#")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monitoring Markers with Alist monitors       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor record-marker-evolution-in-population
    :class 'alist-recorder
    :documentation "Records for the evolution of markers in the population."
    :average-window 1)


(define-event-handler (record-marker-evolution-in-population interaction-finished)
  (let ((all-markers-in-population 
	 (remove-duplicates (remove nil
				    (loop for agent in (population experiment)
                                          append (markers (grammar agent))))
			    :key #'name :test #'string=)))
    (update-marker-development-monitor all-markers-in-population experiment monitor)))

(defun add-zeros (list n)
  (append list
	  (loop for i from 1 to n collect 0)))

(defun complete-usage-with-zero-usage (marker-feature/scores population)
  (loop for (marker . scores) in marker-feature/scores
       if (= (length scores) (length population))
       collect (cons marker scores)
       else collect (cons marker (add-zeros scores (- (length population)
						       (length scores))))))

(defun init-marker-development (all-markers-in-population)
  (loop for marker in all-markers-in-population
       collect (list (name marker))))

(defun update-marker-development-monitor (markers experiment monitor)
  (loop with marker-feature/scores = (init-marker-development markers)
     for agent in (population experiment)
     for cxns = (markers (grammar agent))
     do (loop for cxn in cxns
	   for marker-features = (name cxn)
	   for existing-entry = (assoc marker-features
				       marker-feature/scores :test #'string=)
	   if existing-entry do (push (attr-val cxn :score) (rest existing-entry)))
     finally (loop for (marker . scores) in (complete-usage-with-zero-usage
					      marker-feature/scores (population experiment))
		do (set-value-for-symbol monitor marker (average scores)))))

(define-monitor display-marker-evolution-in-population
    :class 'alist-gnuplot-display
    :documentation "Displays the evolution of marker scores in the population."
    :recorder 'record-marker-evolution-in-population
    :draw-y-grid t :y-max 1.0 :y-min 0.0  :update-interval 5)

(define-monitor plot-marker-evolution-in-population
    :class 'alist-gnuplot-graphic-generator
    :documentation "Plots the evolution of marker scores in the population."
    :recorder 'record-marker-evolution-in-population
    :draw-y-grid t :y-max nil :y-min 0.0  :update-interval 10
    :graphic-type "pdf"
    :file-name 
    (babel-pathname :directory '("experiments" "origins-of-syntax" "graphs")
                    :name "markers-evolution" :type "pdf"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monitoring cxn-application-search            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cipn-made-by-cxn-of-label-p (cipn label)
  "Returns true if this cip node was made by a cxn with label (cxn-set) label, nil otherwise."
  (when (and (applied-constructions cipn)
             (or (fully-expanded? cipn) (children cipn))
             (not (find 'duplicate (statuses cipn) :test #'equal))
             (string= label (attr-val (first (applied-constructions cipn)) :label)))
    cipn))

(defun fcg-search (cip-solution)
  "Returns the number of nodes made by grammatical cxns, divided by the number of grammatical cxns needed for finding the solution."
  (let ((depth-of-solution (length (remove-if-not #'(lambda (cipn)
                                              (cipn-made-by-cxn-of-label-p cipn "CXN"))
                                          (cons cip-solution (all-parents cip-solution)))))

        (nodes-in-tree (length (remove nil (traverse-depth-first (last-elt (all-parents cip-solution))
                                             :collect-fn #'(lambda (cipn)
                                                             (cipn-made-by-cxn-of-label-p cipn "CXN")))))))
    (if (= 0 depth-of-solution)
      1
      (float (/ nodes-in-tree depth-of-solution)))))

(define-monitor record-fcg-search
    :class 'data-recorder
    :average-window 1
    :documentation "Records the search in FCG")

(define-event-handler (record-fcg-search interaction-finished)
  (let ((hearer (find 'hearer (interacting-agents interaction) :key #'discourse-role :test #'equal)))
    (record-value monitor (fcg-search (get-data hearer :cipn)))))

(define-monitor export-fcg-search
    :class 'lisp-data-file-writer
    :documentation "Exports FCG search"
    :data-sources '(record-fcg-search)
    :file-name (babel-pathname :name "fcg-search" :type "lisp" 
                               :directory '("experiments" "origins-of-syntax" "raw-data"))
    :add-time-and-experiment-to-file-name nil
    :column-separator " "
    :comment-string "#")


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monitoring coherence ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun all-subunits (unit structure)
   (loop for subunit-name in (remove-special-operators (feature-value (get-subunits-feature unit)) +no-bindings+)
         for subunit = (structure-unit structure subunit-name)
         collect subunit
         append (all-subunits subunit structure)))

(defun grammatical-unit-p (unit)
  (let ((unit-type (second (find 'unit-type (unit-body unit) :key #'first :test #'equal))))
    (and unit-type (not (equal unit-type 'word)))))

(defun extract-groups-from-ts (ts strategy)
  (cond ((eq strategy :lexical-strategy)
         (list (render (make-instance 'coupled-feature-structure
                                :left-pole ts
                                :right-pole '(root))
                 :render-string-meets)))
        (t
         (let* (;;get pattern units
                (grammatical-units (remove-if-not #'grammatical-unit-p ts))
                (groups-units (mapcar #'(lambda (grammatical-unit) (cons grammatical-unit (all-subunits grammatical-unit ts))) grammatical-units))
                (groups (mapcar #'(lambda (group-units)
                                    (render (make-instance 'coupled-feature-structure
                                                           :left-pole group-units
                                                           :right-pole '(root))
                                            :render-string-meets)) groups-units)))
           groups))))

(defun record-coherence-fn (interaction)
  (let* (;; Get agents
         (speaker (find 'speaker (interacting-agents interaction) :key #'discourse-role :test #'equal))
         (hearer (find 'hearer (interacting-agents interaction) :key #'discourse-role :test #'equal))
         ;; Get cip nodes
         (speaker-ts (left-pole-structure (car-resulting-cfs (cipn-car (get-data speaker :cipn)))))
         (hearer-ts (left-pole-structure (car-resulting-cfs (cipn-car
                                        (second (multiple-value-list (produce (get-data speaker :meaning)
                                                                              (processing-cxn-inventory (grammar hearer))
                                                                              t)))))))
         ;; Extract groups
         (speaker-groups (extract-groups-from-ts speaker-ts (get-configuration (experiment interaction) :strategy)))
         (hearer-groups (extract-groups-from-ts hearer-ts (get-configuration (experiment interaction) :strategy))))
    ;; Compute monitor value
    (if (and (communicated-successfully interaction)
             (permutation-of? speaker-groups hearer-groups :test #'equal))
      1
      0)))

(define-monitor record-coherence
      :class 'data-recorder
      :average-window 1
      :documentation "Records the coherence")

(define-event-handler (record-coherence interaction-finished)
  "Records 1 if hearer would have uttered the same if he were speaker."
  (record-value monitor (record-coherence-fn interaction)))

(define-monitor export-coherence
    :class 'lisp-data-file-writer
    :documentation "Exports coherence "
    :data-sources '(record-coherence)
    :file-name (babel-pathname :name "coherence" :type "lisp" 
                               :directory '("experiments" "origins-of-syntax" "raw-data"))
    :add-time-and-experiment-to-file-name nil
    :column-separator " "
    :comment-string "#")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monitoring Categorial Coherence   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor record-categorial-coherence
    :class 'data-recorder
    :average-window 100
    :documentation "Records the average categorial coherence of all agents")

(define-event-handler (record-categorial-coherence interaction-finished)
  (let* ((coherences (loop for agent in (population experiment)
                          collect
                          (coherence (get-type-hierarchy (grammar agent)))))
         (average-coherence (average coherences)))
    (record-value monitor average-coherence)))

(define-monitor export-categorial-coherence
    :class 'lisp-data-file-writer
    :documentation "Exports categorial cohrence"
    :data-sources '(record-categorial-coherence)
    :file-name (babel-pathname :name "categorial-coherence" :type "lisp" 
                               :directory '("experiments" "origins-of-syntax" "raw-data"))
    :add-time-and-experiment-to-file-name nil
    :column-separator " "
    :comment-string "#")

;;;;;;;;;;;;;;;;
;; Displayers ;;
;;;;;;;;;;;;;;;;

(define-monitor display-fcg-search
    :class 'gnuplot-display
    :documentation "Plots the communicative success."
    :data-sources '((average record-fcg-search))
    :update-interval 50
    :average-window 250
    ;:points t
    ;:lines nil
    :caption '("Fcg Search")
    :x-label "games" 
    :y1-label "" 
    :y1-min 0 
    :draw-y1-grid t)

(define-monitor display-number-of-constructions
    :class 'gnuplot-display
    :documentation "Plots the number of cxns"
    :data-sources '((average record-nr-of-constructions))
    :update-interval 50
    :caption '("Number of Constructions")
    :x-label "games" 
    :y1-label "Average number of constructions" 
    :draw-y1-grid t)

(define-monitor display-communicative-success
    :class 'gnuplot-display
    :documentation "Plots the communicative success "
    :data-sources '((average record-communicative-success))
    :update-interval 50
    :caption '("Communicative Success")
    :x-label "games" 
    :y1-label "Average communicative success."
    :y1-min 0
    :y1-max 1
    :draw-y1-grid t)
