;;;; interaction.lisp

(in-package :pf)

;; ----------------------
;; + Before Interaction +
;; ----------------------

(defmethod initialize-agent ((agent pattern-finding-agent)
                             utterance gold-standard-meaning)
  (setf (utterance agent) utterance
        (meaning agent) gold-standard-meaning
        (communicated-successfully agent) t))

(defun get-interaction-data (interaction)
  "retrieve the nth utterance and gold standard meaning from the dataset"
  (let* ((sample (nth (- (interaction-number interaction) 1)
                      (corpus (experiment interaction))))
         (utterance (car sample))
         (gold-standard-meaning (cdr sample)))
    (values utterance gold-standard-meaning)))

(define-event interaction-before-finished
  (utterance string) (gold-standard-meaning t))

(defmethod interact :before ((experiment pattern-finding-experiment)
                             interaction &key)
  (multiple-value-bind (utterance gold-standard-meaning)
      (get-interaction-data interaction)
    (loop for agent in (interacting-agents experiment)
          do (initialize-agent agent utterance gold-standard-meaning))
    (set-data interaction :utterance utterance)
    (set-data interaction :gold-standard-meaning gold-standard-meaning)
    (notify interaction-before-finished utterance gold-standard-meaning)))

;; ---------------
;; + Interaction +
;; ---------------

(defun determine-communicative-success (cipn)
  (assert (succeeded-cipn-p cipn))
  (let ((all-node-statuses (mappend #'statuses (cons cipn (all-parents cipn))))
        (communicative-success t))
    (when (find 'gold-standard-consulted all-node-statuses)
      (setf communicative-success nil))
    communicative-success))

(defun get-last-repair-symbol (cipn success?)
  (let ((all-node-statuses (mappend #'statuses (cons cipn (all-parents cipn)))))
    (if (not (find 'added-by-repair all-node-statuses))
      (if success? "." "x")
      (cond ((member 'add-cxn all-node-statuses) "h")
            ((member 'anti-unify-cxns all-node-statuses) "a")
            ((member 'anti-unify-cipn all-node-statuses) "p")
            ((member 'add-categorial-links all-node-statuses) "l")))))

(defun set-cxn-last-used (agent cxn)
  (let ((current-interaction-nr
         (interaction-number
          (current-interaction
           (experiment agent)))))
    (setf (attr-val cxn :last-used) current-interaction-nr)))

(define-event constructions-chosen (constructions list))
(define-event cipn-statuses (cipn-statuses list))

(defun run-sanity-check (experiment agent cipn)
  ;; gather the anti-unified cxns, check in which interaction they were learned
  ;; and comprehend the utterances from those interactions again.
  ;; Check if there is a solution cipn that includes cxns that were learned in
  ;; this interaction, as the anti-unification repair should replace anti-unified cxns
  ;; with equivalents.
  ;; !! this does not hold for cxns that were learned from partial analysis (??)
  (let* ((anti-unified-cxns (find-data (blackboard (construction-inventory cipn)) :anti-unified-cxns))
         (observations-to-check
          (remove-duplicates
           (loop for cxn in anti-unified-cxns
                 for learned-at = (attr-val cxn :learned-at)
                 for repair-type = (attr-val cxn :repair)
                 for observation-num = (parse-integer (subseq learned-at 1))
                 ;unless (eql repair-type 'anti-unify-cipn)
                 collect observation-num)))
         (afr (restart-data (first (fixes (first (problems cipn))))))
         (new-cxns
          (loop with interaction-nr = (interaction-number (current-interaction experiment))
                for cxn in (append (afr-cxns-to-apply afr) (afr-cxns-to-consolidate afr))
                when (string= (format nil "@~a" interaction-nr) (attr-val cxn :learned-at))
                collect cxn)))
    (when observations-to-check
      (assert
          (loop for n in observations-to-check
                for sample = (nth (- n 1) (corpus experiment))
                for utterance = (car sample)
                for gold-meaning = (cdr sample)
                for cip-nodes
                  = (progn
                      (set-configuration (grammar agent) :update-categorial-links nil)
                      (set-configuration (grammar agent) :use-meta-layer nil)
                      (set-configuration (grammar agent) :consolidate-repairs nil)
                      (multiple-value-bind (meanings cip-nodes cip)
                          (comprehend-all utterance :cxn-inventory (grammar agent)
                                          :gold-standard-meaning gold-meaning)
                        (set-configuration (grammar agent) :update-categorial-links t)
                        (set-configuration (grammar agent) :use-meta-layer t)
                        (set-configuration (grammar agent) :consolidate-repairs t)
                        cip-nodes))
                always (loop for node in cip-nodes
                             for node-applied-cxns = (original-applied-constructions node)
                             thereis (and (succeeded-cipn-p node)
                                          (intersection node-applied-cxns new-cxns))))))))
                         

(defmethod interact ((experiment pattern-finding-experiment)
                     interaction &key)
  "the learner attempts to comprehend the utterance with its grammar,
   and applies repairs if necessary"
  (let ((agent (first (population experiment))))
    ;; set the current interaction number in the blackboard of the grammar
    (set-data (blackboard (grammar agent))
              :current-interaction-nr (interaction-number interaction))
    ;; run comprehension
    (multiple-value-bind (meanings cipns)
        (comprehend-all (utterance agent)
                        :cxn-inventory (grammar agent)
                        :gold-standard-meaning (meaning agent)
                        :n (get-configuration experiment :comprehend-all-n))
      (declare (ignore meanings))
      (let* ((solution-cipn (first cipns))
             (competing-cipns (rest cipns))
             (applied-cxns (original-applied-constructions solution-cipn))
             (successp (determine-communicative-success solution-cipn)))
        ;; notify
        (notify constructions-chosen applied-cxns)
        (notify cipn-statuses (statuses solution-cipn))
        ;; run santiy check...
        ;(unless successp
        ;  (run-sanity-check experiment agent solution-cipn))
        ;; run alignment
        (run-alignment agent solution-cipn competing-cipns
                       (get-configuration agent :alignment-strategy))
        ;; update the :last-used property of the cxns
        (dolist (cxn applied-cxns)
          (set-cxn-last-used agent cxn))
        ;; store applied repair
        (set-data interaction :applied-repair (get-last-repair-symbol solution-cipn successp))
        ;; set the success of the agent
        (setf (communicated-successfully agent) successp)))))