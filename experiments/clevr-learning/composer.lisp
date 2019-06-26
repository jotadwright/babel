(in-package :clevr-learning)

;; + Composer Class +
(defclass program-composer (single-topic-composer)
  (;:expand-chunk-fns
   ;:check-chunk-fns
   ;:node-rating-fn
   ;:initial-chunk-score-fn
   ;:chunk-wrapper-fn
   ;:check-evaluation-result-fn
   ;:evaluation-result-scoring-fn
   )
  (:documentation "The program composer"))

;; + Check-chunk functions +
(defun counts-allowed-p (primitive-counts)
  (let ((allowed t))
    (loop for (primitive . count) in primitive-counts
          when allowed
          do (case primitive
               (count! (when (> count 2) (setf allowed nil)))
               (equal-integer (when (> count 1) (setf allowed nil)))
               (less-than (when (> count 1) (setf allowed nil)))
               (greater-than (when (> count 1) (setf allowed nil)))
               (equal? (when (> count 1) (setf allowed nil)))
               (exist (when (> count 1) (setf allowed nil)))
               (filter ())
               (get-context (when (> count 1) (setf allowed nil)))
               (intersect (when (> count 1) (setf allowed nil)))
               (query (when (> count 2) (setf allowed nil)))
               (relate (when (> count 3) (setf allowed nil)))
               (same (when (> count 1) (setf allowed nil)))
               (union! (when (> count 1) (setf allowed nil)))
               (unique (when (> count 4) (setf allowed nil)))))
    allowed))

(defun check-primitive-occurrence-counts (chunk)
  "check for every primitive if the allowed occurrence count
   is not violated, e.g. there can be only 1 'exist' primitive"
  (let* ((irl-program (irl-program chunk))
         (unique-primitives (remove-duplicates (mapcar #'first irl-program)))
         (primitive-counts (loop for primitive in unique-primitives
                                 collect (cons primitive (count primitive irl-program :key #'first)))))
    (counts-allowed-p primitive-counts)))

; + Expand-chunk functions +
(defun add-context-var-to-open-vars (chunk)
  "The get-context var is allowed to occur 3 times in the irl-program,
   even though the get-context primitive is allowed to occur once.
   The variable needs to be shared. To enforce this, it is added to the
   open variables of the chunk."
  (let ((get-context-predicate (find 'get-context (irl-program chunk) :key #'first)))
    (when get-context-predicate
      (let* ((context-var (last-elt get-context-predicate))
             (all-variables (find-all-anywhere-if #'variable-p (irl-program chunk)))
             (context-var-count (count context-var all-variables)))
        (when (< context-var-count 3)
          (push (cons context-var 'clevr-object-set) (open-vars chunk))))))
  chunk)

(defun check-duplicate-variables (expand-chunk-solutions)
  "Since the chunk was expanded using 'link-open-variables',
   it can happen that duplicate variables inside a predicate
   were introduced, e.g. (union ?out ?in ?in). This is not
   allowed. This function will filter out those bad chunks."
  (loop for (new-chunk . other-chunk) in expand-chunk-solutions
        when (loop for expr in (irl-program new-chunk)
                   for variables = (cdr expr)
                   always (length= variables (remove-duplicates variables)))
        collect (cons new-chunk other-chunk)))

(defun expand-chunk-for-clevr (chunk composer)
  "This chunk expander combines two existing chunk expanders.
   First, it will check if the 'get-context' predicate is present.
   The variable ?context is allowed to occur 3 times in the CLEVR
   irl-programs. So, if occurring less than 3 times, it is added to
   the open variables of the chunk. Using this modified chunk, the
   link-open-variables chunk expander is tried and bad results are
   filtered out. These solutions are combines with the regular
   combine-program chunk expander that will try to add a primitive
   to the chunk."
  (append (check-duplicate-variables
           (expand-chunk-link-open-variables
            (add-context-var-to-open-vars chunk)
            composer))
          (expand-chunk-combine-program chunk composer)))


;; + Make default composer +
(defun make-default-composer (agent target-category)
  "Make the composer"
  (make-instance 'program-composer
                 ;; what to look for
                 :topic target-category
                 ;; where to start from
                 :initial-chunk (make-instance 'chunk :id 'initial
                                               :target-var '(?answer . t)
                                               :open-vars '((?answer . t)))
                 ;; available primitives
                 :chunks (mapcar #'create-chunk-from-primitive (primitives agent))
                 ;; search depth (depending on question types)
                 :max-search-depth 10
                 ;; available categories (for bind statements)
                 :ontology (ontology agent)
                 ;; filter out bad chunks
                 :check-chunk-fns (list #'check-primitive-occurrence-counts)
                 ;; how to expand chunks
                 :expand-chunk-fns (list #'expand-chunk-for-clevr)))

;; + compose-until +
(defun compose-until (composer fn)
  "Generate composer solutions until the
   function 'fn' returns t on the solution"
  (loop with solution = nil
        while (not solution)
        for solutions = (get-next-solutions composer)
        do (setf solution
                 (loop for s in solutions
                       when (funcall fn s)
                       return s))
        finally
        (return solution)))

;; + Lateral Inhibition Strategy +
(define-event check-chunks-started (list-of-chunks list))

(defun different-meaning (solution list-of-chunks)
  "Check if the composed irl-program is never equivalent
   to any of the chunks."
  (let ((irl-program (append (irl-program (chunk solution))
                             (bind-statements solution))))
    (notify check-chunks-started list-of-chunks)
    (loop for chunk in list-of-chunks
          never (equivalent-irl-programs? irl-program (irl-program chunk)))))

(defmethod compose-new-program (agent target-category (strategy (eql :lateral-inhibition)))
  "The :lateral-inhibition strategy will compose a new program that is different
   from the other programs that already exist for this question. Than, lateral
   inhibition should make sure that only the best one remains"
  (let* ((composer (make-default-composer agent target-category))
         (consider-chunk-ids
          (append
           (mapcar #'(lambda (cxn)
                       (attr-val cxn :meaning))
                   (find-all (utterance agent) (constructions (grammar agent))
                             :key #'(lambda (cxn) (attr-val cxn :form))
                             :test #'string=))
           (mapcar #'(lambda (cxn)
                       (attr-val cxn :meaning))
                   (find-all (utterance agent) (find-data (blackboard (grammar agent)) 'trash)
                             :key #'(lambda (cxn) (attr-val cxn :form))
                             :test #'string=))))
         (consider-chunks
          (mapcar #'(lambda (id)
                      (get-chunk agent id))
                  consider-chunk-ids)))
    (if consider-chunks
      (compose-until composer
                     (lambda (s)
                       (different-meaning s consider-chunks)))
      (random-elt (get-next-solutions composer)))))
  

;; + Sample Strategy +
(define-event check-samples-started (list-of-samples list))

(defun check-all-samples (solution list-of-samples agent)
  "A sample is a triple of (context-id utterance answer). The irl-program
  of the evaluation result has to return the correct answer for all samples
  of the same utterance."
  (let ((original-context (find-data (ontology agent) 'clevr-context))
        (irl-program (append (irl-program (chunk solution)) (bind-statements solution)))
        (world (world (experiment agent)))
        (success t))
    (notify check-samples-started list-of-samples)
    (setf success
          (loop for sample in list-of-samples
                for context = (let* ((path (nth (first sample) (scenes world)))
                                     (scene (load-clevr-scene path)))
                                (set-data (ontology agent) 'clevr-context scene)
                                scene)
                for solutions = (evaluate-irl-program irl-program (ontology agent))
                for solution = (when (length= solutions 1) (first solutions))
                for found-answer = (when solution (get-target-value irl-program solution))
                always (equal-entity found-answer (third sample))))
    (set-data (ontology agent) 'clevr-context original-context)
    success))
              
(defmethod compose-new-program (agent target-category (strategy (eql :keep-samples)))
  "The :keep-samples strategy will ensure that
   the found program holds true for all past scenes where the
   same utterance was used."
  (let* ((composer (make-default-composer agent target-category))
         (consider-samples (find-all (utterance agent) (samples agent)
                                     :key #'second :test #'string=)))
    (if consider-samples
      (compose-until composer
                     (lambda (s)
                       (check-all-samples s consider-samples agent)))
      (random-elt (get-next-solutions composer)))))


;; + Trash Strategy +
(define-event check-trash-started (list-of-trash-programs list))

(defun check-trash (solution list-of-trash-programs)
  "Check if the composed irl-program is never equivalent
   to any of the trash programs for the same utterance"
  (notify check-trash-started list-of-trash-programs)
  (let ((irl-program (append (irl-program (chunk solution))
                             (bind-statements solution))))
    (loop for trash-program in list-of-trash-programs
          never (equivalent-irl-programs? irl-program trash-program))))

(defmethod compose-new-program (agent target-category (strategy (eql :keep-trash)))
  "The :keep-trash strategy will ensure that
   the composer does not reinvent a program that failed before
   for this utterance."
  (let ((composer (make-default-composer agent target-category))
        (consider-trash (rest (assoc (utterance agent) (trash agent) :test #'string=))))
    (if consider-trash
      (compose-until composer
                     (lambda (s)
                       (check-trash s consider-trash)))
      (random-elt (get-next-solutions composer)))))