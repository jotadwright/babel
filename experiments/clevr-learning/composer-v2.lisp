(in-package :irl-2)

;; + Check-chunk functions +
(defun counts-allowed-p (primitive-counts)
  (let ((allowed t))
    (loop for (primitive . count) in primitive-counts
          when allowed
          do (case primitive
               (clevr-world:count! (when (> count 2) (setf allowed nil)))
               (clevr-world:equal-integer (when (> count 1) (setf allowed nil)))
               (clevr-world:less-than (when (> count 1) (setf allowed nil)))
               (clevr-world:greater-than (when (> count 1) (setf allowed nil)))
               (clevr-world:equal? (when (> count 1) (setf allowed nil)))
               (clevr-world:exist (when (> count 1) (setf allowed nil)))
               (clevr-world:filter ())
               (clevr-world:get-context (when (> count 2) (setf allowed nil)))
               (clevr-world:intersect (when (> count 1) (setf allowed nil)))
               (clevr-world:query (when (> count 2) (setf allowed nil)))
               (clevr-world:relate (when (> count 3) (setf allowed nil)))
               (clevr-world:same (when (> count 1) (setf allowed nil)))
               (clevr-world:union! (when (> count 1) (setf allowed nil)))
               (clevr-world:unique (when (> count 4) (setf allowed nil)))))
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

(defmethod check-node ((node chunk-composer-node)
                       (composer chunk-composer)
                       (mode (eql :clevr-context-links)))
  ;; inputs from count!, exist, intersect, union!
  ;; and unique cannot be the output of get-context
  (let* ((irl-program (irl-program (chunk node)))
         (context-predicates (find-all 'clevr-world:get-context
                                       irl-program :key #'car)))
    (if context-predicates
      (loop for context-predicate in context-predicates
            for context-var = (second context-predicate)
            always (loop for p in '(clevr-world:count! clevr-world:exist
                                    clevr-world:intersect clevr-world:union!
                                    clevr-world:unique)
                        for preds = (find-all p irl-program :key #'car)
                        never (loop for pred in preds
                                    thereis (or (eql (third pred) context-var)
                                                (eql (fourth pred) context-var)))))
      t)))


(defmethod check-node ((node chunk-composer-node)
                       (composer chunk-composer)
                       (mode (eql :clevr-open-vars)))
  ;; the last element from filter, query, equal?,
  ;; relate and same must always be an open variable
  (let ((irl-program (irl-program (chunk node))))
    (loop for p in '(clevr-world:filter clevr-world:query clevr-world:equal?
                     clevr-world:relate clevr-world:same)
          for preds = (find-all p irl-program :key #'car)
          always (loop for pred in preds
                       always (member (last-elt pred)
                                      (open-vars (chunk node))
                                      :key #'car)))))

(defmethod check-node ((node chunk-composer-node)
                       (composer chunk-composer)
                       (mode (eql :clevr-filter-group-length)))
  (let* ((irl-program (irl-program (chunk node)))
         (filter-groups (collect-filter-groups irl-program)))
    (loop for group in filter-groups
          never (length> group 4))))


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
                             (find (internal-symb 'get-context)
                                   program :key #'first))
     ;; traverse the network by following in/out variables
     :next-predicate-fn #'(lambda (predicate program)
                            (or (find-all (second predicate)
                                          program :key #'third)
                                (find-all (second predicate)
                                          program :key #'fourth)))
     ;; collect filter predicates in groups
     :do-fn #'(lambda (predidate)
                (cond ((and prev-was-filter
                            (eql (first predidate)
                                 (internal-symb 'filter)))
                       (push predidate (first filter-groups)))
                      ((eql (first predidate) (internal-symb 'filter))
                       (push (list predidate) filter-groups)
                       (setf prev-was-filter t))
                      (prev-was-filter
                       (setf prev-was-filter nil)))))
    filter-groups))

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
  (if (> (count (internal-symb 'filter)
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
   open variables of the chunk, unless it is already shared."
  (let ((get-context-predicate (find (internal-symb 'get-context)
                                     (irl-program chunk)
                                     :key #'first)))
    (when get-context-predicate
      (let ((context-var (last-elt get-context-predicate)))
        (unless (find context-var (open-vars chunk) :key #'car)
          (let* ((all-variables (find-all-anywhere-if #'variable-p (irl-program chunk)))
                 (context-var-count (count context-var all-variables)))
            (when (< context-var-count 3)
              (push (cons context-var (internal-symb 'clevr-object-set))
                    (open-vars chunk)))))))
    chunk))

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
  (if (loop for predicate in (irl-program chunk)
            thereis (member (first predicate)
                            (mapcar #'internal-symb
                                    '(union! intersect equal?
                                      equal-integer less-than
                                      greater-than))))
    (append (expand-chunk chunk composer :combine-program)
            (check-duplicate-variables
             (expand-chunk
              (add-context-var-to-open-vars chunk)
              composer :link-open-variables)))
    (expand-chunk chunk composer :combine-program)))

;; + node rating mode +
;; maybe add a preference for chain-type programs
;; over tree-type programs?
(defmethod rate-node ((node chunk-composer-node)
                      (composer chunk-composer)
                      (mode (eql :clevr-node-rating)))
  "Prefer short programs with few open vars and few
   duplicate primitives."
  (let ((chunk (chunk node)))
    (/ (+ (node-depth node)            ;; the less depth the better
          (length (open-vars chunk))   ;; less open vars are better
          (length (irl-program chunk)) ;; less primitives are better
          ;; less duplicate primitives are better
          ;; (bind statements and filters are not considered)
          (let ((predictes (remove 'clevr-world:filter
                                   (all-predicates
                                    (irl-program chunk))
                                   :key #'car)))
            (* 5 (- (length predictes)
                    (length (remove-duplicates predictes)))))
          ;; chain-type programs are preferred over tree-type programs
          ;; (i.e. get-context occurs once instead of twice)
          (count 'clevr-world:get-context
                 (irl-program chunk)
                 :key #'car))
       ;; the higher the score the better
       (score chunk))))

;; IRL program evaluation node tests
;; This test checks whether there are
;; no duplicate solutions AND
;; no solutions that are permutations
;; of each other, in particular concerning
;; the filter primitives

(defun duplicate-solution-p (solution node)
  (let ((duplicatep
         (loop for value in (mapcar #'value solution)
               for node-value in (mapcar #'value (bindings node))
               always (or (and (null value) (null node-value))
                          (and value node-value
                               (equal-entity value node-value))))))
    (when duplicatep
      (setf (status node) 'duplicate))
    duplicatep))

(defun permutation-solution-p (solution node)
  (let* ((irl-program (irl-program (processor node)))
         (filter-groups
          (remove-if #'(lambda (l) (= l 1))
                     (collect-filter-groups irl-program)
                     :key #'length))
         (permutationp
          (when filter-groups
            (loop for group in filter-groups
                  for vars = (mapcar #'last-elt group)
                  for solution-values
                  = (loop for v in vars
                          collect (value (find v solution :key #'var)))
                  for group-values
                  = (loop for v in vars
                          collect (value (find v (bindings node) :key #'var)))
                  always (permutation-of? solution-values group-values
                                          :test #'equal-entity)))))
    (when permutationp
      (setf (status node) 'duplicate))
    permutationp))


(defmethod node-test ((node irl-program-processor-node)
                      (mode (eql :no-duplicate-and-permutation-solutions)))
  ;; if there are primitives remaining, don't run the node test
  (if (primitives-remaining node) t
    ;; a node is a good one if there is not a single other solution
    ;; that is a duplicate or a permutation of the current node
    (loop for solution in (solutions (processor node))
          never (or (duplicate-solution-p solution node)
                    (permutation-solution-p solution node)))))
                        


(in-package :clevr-learning)


;; + Make default composer +
(defun make-default-composer (agent target-category)
  (make-chunk-composer
   :topic target-category
   :initial-chunk (make-instance 'chunk :id 'initial
                                 :target-var `(?answer . ,(type-of target-category))
                                 :open-vars `((?answer . ,(type-of target-category))))
   :chunks (get-data (ontology agent) 'composer-chunks)
   :ontology (ontology agent)
   :primitive-inventory (primitives agent)
   :configurations '((:max-search-depth . 25)
                     (:check-node-modes :check-duplicate 
                      :clevr-primitive-occurrence-count  
                      :clevr-open-vars
                      :clevr-context-links
                      :clevr-filter-group-length)
                     (:expand-chunk-modes :combine-program)
                     (:node-rating-mode . :clevr-node-rating)
                     (:check-chunk-evaluation-result-modes
                      :clevr-coherent-filter-groups))
   :primitive-inventory-configurations '((:node-tests :no-duplicate-and-permutation-solutions
                                          :restrict-nr-of-nodes)
                                         (:max-nr-of-nodes . 10000))))

;; + compose-until +
(defun compose-until (composer fn &optional timeout)
  "Generate composer solutions until the
   function 'fn' returns t on the solution"
  (loop with solution = nil
        with timeout-passed-p = nil
        while (or (not solution) timeout-passed-p)
        for solutions
        = (handler-case
              (with-timeout (timeout)
                (get-next-solutions composer))
            (timeout-error (error)
              (setf timeout-passed-p t)
              (return-from compose-until 'timeout-passed)))
        do (setf solution
                 (loop for s in solutions
                       for i from 1
                       when (funcall fn s i)
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
(define-event check-samples-started
  (list-of-samples list)
  (solution-index number))              

(defun check-all-samples (solution solution-index list-of-samples agent)
  "A sample is a triple of (context-id utterance answer). The irl-program
  of the evaluation result has to return the correct answer for all samples
  of the same utterance."
  (let ((original-context (find-data (ontology agent) 'clevr-context))
        (irl-program (append (irl-program (chunk solution)) (bind-statements solution)))
        (world (world (experiment agent)))
        (success t))
    (notify check-samples-started list-of-samples solution-index)
    (setf success
          (loop for sample in list-of-samples
                for context
                = (let* ((path (nth (first sample) (scenes world)))
                         (scene (load-clevr-scene path)))
                    (set-data (ontology agent) 'clevr-context scene)
                    scene)
                for solutions
                = (evaluate-irl-program irl-program (ontology agent)
                                        :silent t :primitive-inventory (primitives agent))
                for solution = (when (length= solutions 1) (first solutions))
                for found-answer = (when solution (get-target-value irl-program solution))
                for correct = (equal-entity found-answer (third sample))
                always correct))
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
                     (lambda (s idx)
                       (check-all-samples s idx consider-samples agent))
                     (get-configuration agent :composer-timeout))
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