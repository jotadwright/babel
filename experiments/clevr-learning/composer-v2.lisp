(in-package :irl-2)

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

(defmethod check-node ((node chunk-composer-node)
                       (composer chunk-composer)
                       (mode (eql :clevr-primitive-occurrence-count)))
  (let* ((chunk (chunk node))
         (irl-program (irl-program chunk))
         (unique-primitives (remove-duplicates (mapcar #'first irl-program)))
         (primitive-counts (loop for primitive in unique-primitives
                                 collect (cons primitive
                                               (count primitive irl-program
                                                      :key #'first)))))
    (counts-allowed-p primitive-counts)))

; + Check chunk evaluation result +
(defun traverse-irl-program (irl-program &key first-predicate-fn next-predicate-fn do-fn)
  "General utility function that traverses a meaning network.
   first-predicate-fn is used to compute the first meaning predicate from the network.
   next-predicate-fn takes a predicate and the network and computes the next predicate(s)
   do-fn is called on every predicate"
  (let ((stack (list (funcall first-predicate-fn irl-program)))
        visited)
    (loop while stack
          for current-predicate = (pop stack)
          for next-predicates = (funcall next-predicate-fn current-predicate irl-program)
          do (mapcar #'(lambda (p) (push p stack)) next-predicates)
          do (unless (find current-predicate visited :test #'equal)
               (funcall do-fn current-predicate)
               (push current-predicate visited)))))

(defun collect-filter-groups (irl-program)
  "Collect groups of filter operations using traverse-meaning-network."
  (let (filter-groups prev-was-filter)
    (traverse-irl-program
     irl-program
     ;; use get-context predicates as start of the search
     :first-predicate-fn #'(lambda (program)
                             (find 'clevr-primitives::get-context
                                   program :key #'first))
     ;; traverse the network by following in/out variables
     :next-predicate-fn #'(lambda (predicate program)
                            (find-all (second predicate)
                                      program :key #'third))
     ;; collect filter predicates in groups
     :do-fn #'(lambda (predidate)
                (cond ((and prev-was-filter
                            (eql (first predidate)
                                 'clevr-primitives::filter))
                       (push predidate (first filter-groups)))
                      ((eql (first predidate) 'clevr-primitives::filter)
                       (push (list predidate) filter-groups)
                       (setf prev-was-filter t))
                      (prev-was-filter
                       (setf prev-was-filter nil)))))
    ;; only return groups of more than 1 filter predicate
    (remove-if #'(lambda (l) (= l 1)) filter-groups :key #'length)))

(defun all-different-bind-types (chunk-evaluation-result filter-group)
  ;; collect all bind-statements that belong with the filter-group
  ;; check if the types are all different
  (let ((bind-types
         (loop for filter in filter-group
               collect (second
                        (find (last-elt filter)
                              (bind-statements chunk-evaluation-result)
                              :key #'third)))))
    (= (length bind-types)
       (length (remove-duplicates bind-types)))))

(defmethod check-chunk-evaluation-result ((result chunk-evaluation-result)
                                          (composer chunk-composer)
                                          (mode (eql :clevr-coherent-filter-groups)))
  ;; find filter groups
  (if (> (count 'clevr-primitives::filter
                (irl-program (chunk result))
                :key #'first) 1)
    (let ((filter-groups (collect-filter-groups (irl-program (chunk result)))))
      (if filter-groups
        (loop for group in filter-groups
              ;; check if the bindings make sense
              ;; if not, refuse the node
              always (all-different-bind-types result group))
        t))
    t))

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

(defmethod expand-chunk ((chunk chunk)
                         (composer chunk-composer)
                         (mode (eql :clevr-expand-chunk)))
  (let ((new-chunks
         (expand-chunk chunk composer :combine-program)))
    (if new-chunks new-chunks
      (check-duplicate-variables
       (expand-chunk
        (add-context-var-to-open-vars chunk)
        composer :link-open-variables)))))

;; + node rating mode +
(defmethod rate-node ((node chunk-composer-node)
                      (composer chunk-composer)
                      (mode (eql :clevr-node-rating)))
  "Prefer short programs with few open vars and few
   duplicate primitives, except for filter."
  (let ((chunk (chunk node)))
    (/ (+ (node-depth node)            ;; the less depth the better
          (length (open-vars chunk))   ;; less open vars are better
          (length (irl-program chunk)) ;; less primitives are better
          ;; less duplicate primitives are better
          ;; (bind statements and filter primitives
          ;;  are not considered)
          (let ((predictes (remove 'filter
                                   (all-predicates
                                    (irl-program chunk))
                                   :key #'car)))
            (* 5 (- (length predictes)
                    (length (remove-duplicates predictes))))))
       ;; the higher the score the better
       (score chunk))))






(in-package :clevr-learning)


;; + Make default composer +
(defun make-default-composer (agent target-category)
  (make-chunk-composer
   :topic target-category
   :initial-chunk (make-instance 'chunk :id 'initial
                                 :target-var '(?answer . t)
                                 :open-vars '((?answer . t)))
   :chunks (mapcar #'(lambda (p) (create-chunk-from-primitive
                                  p :primitive-inventory (primitives agent)))                       
                   (primitives-list (primitives agent)))
   :ontology (ontology agent) :primitive-inventory (primitives agent)
   :configurations '((:max-search-depth . 10)
                     (:check-node-modes :check-duplicate
                      :clevr-primitive-occurrence-count)
                     (:expand-chunk-modes :clevr-expand-chunk)
                     (:node-rating-mode . :clevr-node-rating)
                     (:check-chunk-evaluation-result-mode . :clevr-coherent-filter-groups))))

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
   from the other programs that already exist for this question. Then, lateral
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
      (progn (format t "~%Composing a new program. Checking against ~a older programs"
                     (length consider-chunks))
        (compose-until composer
                       (lambda (s)
                         (different-meaning s consider-chunks))))
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
                for solutions = (evaluate-irl-program irl-program (ontology agent)
                                                      :primitive-inventory (primitives agent))
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