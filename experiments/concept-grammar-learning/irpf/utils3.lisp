;;;; composer-utils.lisp

(in-package :irl)

;; + chunk-node-tests +
(defparameter *allowed-primitive-counts*
  '((clevr-world:count! . 2)
    (clevr-world:equal-integer . 1)
    (clevr-world:less-than . 1)
    (clevr-world:greater-than . 1)
    (clevr-world:equal? . 1)
    (clevr-world:exist . 1)
    (clevr-world:filter . 50)
    (clevr-world:get-context . 2)
    (clevr-world:intersect . 1)
    (clevr-world:query . 2)
    (clevr-world:relate . 3)
    (clevr-world:same . 1)
    (clevr-world:union! . 1)
    (clevr-world:unique . 4)))

(defun counts-allowed-p (primitive-counts)
  (let ((allowed t))
    (loop for (primitive . count) in primitive-counts
          for allowed-count = (rest (assoc primitive *allowed-primitive-counts*))
          for count-exceeded = (> count allowed-count)
          when count-exceeded
          do (setf allowed nil) (return allowed))
    allowed))

(defmethod chunk-node-test ((node chunk-composer-node)
                            (composer chunk-composer)
                            (mode (eql :clevr-primitive-occurrence-count)))
  ;; each clevr primitive can only occur a maximum number of times
  ;; in a program. Check these amounts.
  (let* ((chunk (chunk node))
         (irl-program (irl-program chunk))
         (unique-primitives (remove-duplicates (mapcar #'first irl-program)))
         (primitive-counts (loop for primitive in unique-primitives
                                 collect (cons primitive
                                               (count primitive irl-program
                                                      :key #'first)))))
    (counts-allowed-p primitive-counts)))


(defmethod chunk-node-test ((node chunk-composer-node)
                            (composer chunk-composer)
                            (mode (eql :clevr-context-links)))
  ;; inputs from count!, exist, intersect, union!
  ;; and unique cannot be the output of get-context
  (let* ((irl-program (irl-program (chunk node)))
         (context-predicates (find-all 'clevr-world:get-context  irl-program :key #'car)))
    (if context-predicates
      (loop for context-predicate in context-predicates
            for context-var = (second context-predicate)
            for predicates-with-var = (find-all context-var irl-program :test #'member)
            always (loop for predicate in predicates-with-var
                         never (member (first predicate)
                                       '(clevr-world:count! clevr-world:exist
                                         clevr-world:intersect clevr-world:union!
                                         clevr-world:unique))))
      t)))


(defmethod chunk-node-test ((node chunk-composer-node)
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


(defmethod chunk-node-test ((node chunk-composer-node)
                            (composer chunk-composer)
                            (mode (eql :clevr-filter-group-length)))
  ;; filter predicates chained together can be maximally 4 long
  (let ((irl-program (irl-program (chunk node))))
    (if (> (count 'clevr-world:filter irl-program :key #'first) 1)
      (let ((filter-groups (collect-filter-groups irl-program)))
        (loop for group in filter-groups
              never (length> group 4)))
      t)))


(defmethod chunk-node-test ((node chunk-composer-node)
                            (composer chunk-composer)
                            (mode (eql :no-circular-primitives)))
  ;; Primitives with duplicate variables, e.g. (filter ?set ?set ?bind)
  ;; are not allowed
  (let ((irl-program (irl-program (chunk node))))
    (loop for predicate in irl-program
          for arguments = (cdr predicate)
          always (length= arguments (remove-duplicates arguments)))))


(defmethod chunk-node-test ((node chunk-composer-node)
                            (composer chunk-composer)
                            (mode (eql :fully-connected-meaning)))
  ;; The meaning has to be fully connected
  (let ((irl-program (irl-program (chunk node))))
    (fcg:connected-semantic-network irl-program)))

(defmethod chunk-node-test ((node chunk-composer-node)
                            (composer chunk-composer)
                            (mode (eql :one-target-var)))
  ;; there has to be one and only one target var in the irl program
  (let* ((chunk-target-var (car (target-var (chunk node))))
         (program-target-var (get-target-var (irl-program (chunk node))))
         (test-pass
          (and chunk-target-var program-target-var
               (equal chunk-target-var program-target-var))))
    (if test-pass test-pass
      (progn (format nil "hello!") nil))))


; + Check chunk evaluation result +
(defun traverse-irl-program (irl-program &key first-predicate-fn next-predicate-fn do-fn)
  "General utility function that traverses a meaning network.
   first-predicate-fn is used to compute the first meaning predicate from the network.
   next-predicate-fn takes a predicate and the network and computes the next predicate(s)
   do-fn is called on every predicate"
  (let* ((first-predicate (funcall first-predicate-fn irl-program))
         (stack (when first-predicate
                  (list first-predicate)))
         visited)
    (loop while stack
          for current-predicate = (pop stack)
          for next-predicates = (funcall next-predicate-fn current-predicate irl-program)
          unless (find current-predicate visited :test #'equal)
          do (mapcar #'(lambda (p) (push p stack)) next-predicates)
          (funcall do-fn current-predicate)
          (push current-predicate visited))))

(defun collect-filter-groups (irl-program)
  "Collect groups of filter operations using traverse-meaning-network."
  (let (filter-groups prev-was-filter)
    (traverse-irl-program
     irl-program
     ;; use get-context predicates as start of the search
     :first-predicate-fn #'(lambda (program)
                             (find 'clevr-world:get-context program :key #'first))
     ;; traverse the network by following in/out variables
     :next-predicate-fn #'(lambda (predicate program)
                            (or (find-all (second predicate) program :key #'third)
                                (find-all (second predicate) program :key #'fourth)))
     ;; collect filter predicates in groups
     :do-fn #'(lambda (predidate)
                (cond ((and prev-was-filter (eql (first predidate) 'clevr-world:filter))
                       (push predidate (first filter-groups)))
                      ((eql (first predidate) 'clevr-world:filter)
                       (push (list predidate) filter-groups)
                       (setf prev-was-filter t))
                      (prev-was-filter
                       (setf prev-was-filter nil)))))
    filter-groups))

(defmethod chunk-evaluation-goal-test ((result chunk-evaluation-result)
                                       (composer chunk-composer)
                                       (mode (eql :check-bindings)))
  ;; Check if the bindings in this solution match the partial
  ;; bindings that were passed along, if not, refuse this solution
  (when (find-data composer 'partial-bindings)
    (let ((partial-bindings (find-data composer 'partial-bindings))
          (found-bindings (bind-statements result)))
      (when (length>= found-bindings partial-bindings)
        (let* ((bind-values-no-duplicates
                (remove-duplicates
                 (mapcar #'fourth partial-bindings)))
               (occurrence-counts
                (loop for b in bind-values-no-duplicates
                      collect (cons b (count b partial-bindings :key #'fourth)))))
          (loop for (b . count) in occurrence-counts
                for found? = (find-all b found-bindings :key #'fourth)
                always (= count (length found?))))))))

(defmethod chunk-evaluation-goal-test ((result chunk-evaluation-result)
                                       (composer chunk-composer)
                                       (mode (eql :at-least-one-shape-category)))
  ;; Check the bindings and make sure there is at least one shape category
  ;; that is not the target variable!
  ;; This goal test is for debugging purposes and should not make it into
  ;; the final experiment...
  (let* ((complete-program
          (append (irl-program (chunk result))
                  (bind-statements result)))
         (target-var (get-target-var complete-program)))
    (loop with solution-bindings = (remove target-var (bindings result) :key #'var)
          for binding in solution-bindings
          thereis (eql (type-of (value binding))
                       'clevr-world:shape-category))))


; + Expand-chunk functions +
(defun add-context-var-to-open-vars (chunk)
  "The get-context var is allowed to occur 3 times in the irl-program,
   even though the get-context primitive is allowed to occur once.
   The variable needs to be shared. To enforce this, it is added to the
   open variables of the chunk, unless it is already shared."
  (let ((get-context-predicate (find 'clevr-world:get-context
                                     (irl-program chunk)
                                     :key #'first)))
    (when get-context-predicate
      (let ((context-var (last-elt get-context-predicate)))
        (unless (find context-var (open-vars chunk) :key #'car)
          (let* ((all-variables (find-all-anywhere-if #'variable-p (irl-program chunk)))
                 (context-var-count (count context-var all-variables)))
            (when (< context-var-count 3)
              (push (cons context-var 'clevr-world:clevr-object-set)
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
                            '(clevr-world:union!
                              clevr-world:intersect clevr-world:equal?
                              clevr-world:equal-integer clevr-world:less-than
                              clevr-world:greater-than)))
    (append (expand-chunk chunk composer :combine-program)
            (check-duplicate-variables
             (expand-chunk
              (add-context-var-to-open-vars chunk)
              composer :link-open-variables)))
    (expand-chunk chunk composer :combine-program)))


;; + node rating mode +
;; maybe add a preference for chain-type programs
;; over tree-type programs?
(defmethod node-cost ((node chunk-composer-node)
                      (composer chunk-composer)
                      (mode (eql :clevr-node-cost)))
  "Prefer short programs with few open vars and few
   duplicate primitives."
  (let ((chunk (chunk node)))
    (/ (+ (depth node)                 ;; the less depth the better
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

;; ---------------------------------------------------------
;; clevr filter permutation detection

(defparameter *allowed-type-map*
  '((clevr-world:shape-category clevr-world:material-category
                                clevr-world:color-category
                                clevr-world:size-category)
    (clevr-world:material-category clevr-world:color-category
                                   clevr-world:size-category)
    (clevr-world:color-category clevr-world:size-category)
    (clevr-world:size-category . nil)))

(defun valid-permutation-p (permutation)
  ;; the permutation is ordered such that the node that was
  ;; evaluated first is also first in the list.
  ;; the ordering is as follows:
  ;; shape > material > color > size
  ;; Thus, when looking at the type of the first binding
  ;; (e.g. a material), only color and size are allowed
  ;; to appear in the following bindings.
  (loop with allowed-next-types = '(clevr-world:shape-category
                                    clevr-world:material-category
                                    clevr-world:color-category
                                    clevr-world:size-category)
        for binding-value in permutation
        for binding-type = (type-of binding-value)
        if (member binding-type allowed-next-types)
        do (setf allowed-next-types
                 (rest (assoc binding-type *allowed-type-map*)))
        else do (return nil)
        finally (return t)))

(defun duplicate-permutation-p (permutation node)
  (loop with processed-permutations
        = (find-data (blackboard (pip node))
                     'processed-permutations)
        for permut in processed-permutations
        thereis (and (length= permut permutation)
                     (permutation-of? permut permutation
                                      :test #'equal-entity))))

(defmethod pip-node-test ((node pip-node) (mode (eql :remove-clevr-filter-permutations)))
  ;; Detect and remove permutations in bindings of filter predicates
  ;; as soon as possible. Also make sure that only the correct permutations
  ;; are kept.
  (if (eql (first (primitive-under-evaluation node)) 'clevr-world:filter)
    (let ((filter-group
           (loop with group = nil
                 with current-node = node
                 while (eql (first (primitive-under-evaluation current-node))
                            'clevr-world:filter)
                 do (push current-node group)
                 do (setf current-node (parent current-node))
                 finally (return group))))
      (if (length> filter-group 1)
        (let ((filter-bindings
               (loop for node in filter-group
                     for pue = (primitive-under-evaluation node)
                     for bind-var = (last-elt pue)
                     for binding = (find bind-var (bindings node) :key #'var)
                     for bind-val = (value binding)
                     collect bind-val)))
          (valid-permutation-p filter-bindings))
        t))
    t))

;; ---------------------------------------------------------
;; clevr coherent filter groups

(defmethod pip-node-test ((node pip-node) (mode (eql :remove-clevr-incoherent-filter-groups)))
  ;; Detect and remove incoherent filter groups as soon as possible
  (if (eql (first (primitive-under-evaluation node)) 'clevr-world:filter)
    (let ((filter-group
           (loop with group = nil
                 with current-node = node
                 while (eql (first (primitive-under-evaluation current-node)) 'clevr-world:filter)
                 do (push current-node group)
                 do (setf current-node (parent current-node))
                 finally (return group))))
      (if (length> filter-group 1)
        (let ((filter-binding-types
               (loop for node in filter-group
                     for pue = (primitive-under-evaluation node)
                     for bind-var = (last-elt pue)
                     for binding = (find bind-var (bindings node) :key #'var)
                     for bind-val = (value binding)
                     collect (type-of bind-val))))
          (length= filter-binding-types
                   (remove-duplicates filter-binding-types)))
        t))
    t))
              