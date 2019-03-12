;;;; preprocess-program.lisp

(in-package :clevr-evaluation)

(export '(preprocess-program
          preprocess-program-for-web-service))

(define-event duplicate-context-finished (irl-program list))
(define-event filter-things-finished (irl-program list))
(define-event reverse-nominal-groups-finished (irl-program list))
(define-event process-network-finished (irl-program list))

(defun duplicate-context (irl-program)
  "When the CLEVR program is a tree, the meaning network has a single
   get-context predicate that links to 2 (or more) other predicates.
   Decouple these get-context predicates such that the meaning network
   is also a tree"
  (let* ((context-predicate (find 'get-context irl-program :key #'first))
         (context-var (second context-predicate))
         (next-predicates (all-linked-predicates context-predicate context-var irl-program)))
    (when (> (length next-predicates) 1)
      (let ((new-vars (loop repeat (length next-predicates)
                            collect (make-var 'context))))
        (loop for var in new-vars
              for predicate in next-predicates
              do (progn
                   (setf irl-program (remove predicate irl-program :test #'equal))
                   (push `(get-context ,var) irl-program)
                   (push (subst var context-var predicate) irl-program))
              finally
              (setf irl-program (remove context-predicate irl-program :test #'equal)))))
    (notify duplicate-context-finished irl-program)
    irl-program))

(defun filter-things (irl-program)
  "Remove 'filter' predicate bound to the shape 'thing' since this
   is not used in the CLEVR corpus. These predicates
   are removed and the variables of both input and output
   are stitched together. Also, the bind statements are removed."
  (let* ((filter-predicates (find-all 'filter irl-program :key #'first))
         (filter-thing-predicates-and-bindings
          (loop for filter-pred in filter-predicates
                for bind-statement = (linked-bind-statement filter-pred irl-program)
                when (eql (bind-statement-value bind-statement) 'thing)
                collect (cons filter-pred bind-statement))))
    (loop for (filter-pred . bind-statement) in filter-thing-predicates-and-bindings
          for input-var = (first (input-vars filter-pred))
          for output-var = (output-var filter-pred)
          for outgoing-predicate = (first (all-linked-predicates filter-pred output-var irl-program))
          for incoming-predicate = (first (all-linked-predicates filter-pred input-var irl-program))
          for new-linking-var = (make-var)
          do (progn
               (setf irl-program (remove filter-pred irl-program :test #'equal))
               (setf irl-program (remove bind-statement irl-program :test #'equal))
               (when outgoing-predicate
                 (setf irl-program (remove outgoing-predicate irl-program :test #'equal))
                 (push (subst new-linking-var output-var outgoing-predicate) irl-program))
               (when incoming-predicate
                 (setf irl-program (remove incoming-predicate irl-program :test #'equal))
                 (push (subst new-linking-var input-var incoming-predicate) irl-program))))
    (notify filter-things-finished irl-program)
    irl-program))

(defun traverse-meaning-network (meaning-network &key first-predicate-fn next-predicate-fn do-fn)
  "General utility function that traverses a meaning network.
   first-predicate-fn is used to compute the first meaning predicate from the network.
   next-predicate-fn takes a predicate and the network and computes the next predicate(s)
   do-fn is called on every predicate"
  (let ((stack (funcall first-predicate-fn meaning-network))
        visited)
    (loop while stack
          for current-predicate = (pop stack)
          for next-predicates = (funcall next-predicate-fn current-predicate meaning-network)
          do (mapcar #'(lambda (p) (push p stack)) next-predicates)
          do (unless (find current-predicate visited :test #'equal)
               (funcall do-fn current-predicate)
               (push current-predicate visited)))))

(defun collect-filter-groups (irl-program)
  "Collect groups of filter operations using traverse-meaning-network."
  (let (filter-groups prev-was-filter)
    (traverse-meaning-network irl-program
                              ;; use get-context predicates as start of the search (can be multiple)
                              :first-predicate-fn #'(lambda (program) (find-all 'get-context program :key #'first))
                              ;; traverse the network by following in/out variables
                              :next-predicate-fn #'(lambda (predicate program) (all-linked-predicates predicate (output-var predicate) program))
                              ;; collect filter predicates in groups
                              :do-fn #'(lambda (predidate)
                                         (cond ((and prev-was-filter (eql (first predidate) 'filter))
                                                (push predidate (first filter-groups)))
                                               ((eql (first predidate) 'filter)
                                                (push (list predidate) filter-groups)
                                                (setf prev-was-filter t))
                                               (prev-was-filter
                                                (setf prev-was-filter nil)))))
    ;; only return groups of more than 1 filter predicate
    (remove-if #'(lambda (l) (= l 1)) filter-groups :key #'length)))

(defun swap-in-out (predicate)
  "Swap input and output variables of a predicate."
  (let ((in (first (input-vars predicate)))
        (out (output-var predicate))
        (rest (subseq predicate 3)))
    (append (list (first predicate) in out) rest)))

(defun reverse-filter-groups (filter-groups)
  "Reverse the filter group, making sure inputs and outputs remain correct."
  ;; this function can be made simpler
  (loop for group in filter-groups
        for new-group = (list)
        ;; collect the input and output of the group
        for group-input = (first (input-vars (last-elt group)))
        for group-output = (output-var (first group))
        for temp-var = (make-var)
        ;; swap the inputs and outputs of the group internally
        ;; this loop goes through the predicates in reverse order
        ;; (i.e. moving towards the 'get-context predicate)
        do (loop with next-var = nil
                 for predicate in group
                 for i from 0
                 for new-var = (make-var)
                 ;; when predicate is first or last, swap input and output
                 do (when (or (= i 0)
                              (= i (- (length group) 1)))
                      (setf predicate (swap-in-out predicate)))
                 do (cond
                     ;; when predicate is last, only overwrite input with next-var
                     ((and next-var (= i (- (length group) 1)))
                      (push (subst next-var (third predicate) predicate)
                            new-group))
                     ;; when predicate is in between, overwrite input with next-var and output with new-var
                     (next-var
                      (let ((rest (subseq predicate 3)))
                        (push (append (list (first predicate) new-var next-var)
                                      rest)
                              new-group)))
                     ;; else (when predicate is first), overwrite input with new-var
                     (t
                      (push (subst new-var (second predicate) predicate)
                            new-group)))
                 ;; store new-var as next-var
                 do (setf next-var new-var))
        ;; swap the input and output of the group
        do (nsubst temp-var group-input new-group)
        do (nsubst group-input group-output new-group)
        do (nsubst group-output temp-var new-group)
        collect new-group))

(defun reverse-nominal-groups (irl-program)
  "Collect and reverse nominal groups."
  (let* ((filter-groups (collect-filter-groups irl-program))
         (reversed-groups (reverse-filter-groups filter-groups)))
    (loop for old-group in filter-groups
          for new-group in reversed-groups
          do (loop for old-predicate in old-group
                   for new-predicate in new-group
                   do (setf irl-program (remove old-predicate irl-program :test #'equal))
                   do (push new-predicate irl-program)))
    (notify reverse-nominal-groups-finished irl-program)
    irl-program))

(defun preprocess-program (irl-program)
  "Preprocess the meaning network for evaluation. Make sure the final
   predicate remains at the end of the meaning network. If comprehension
   was not successful, there could be multiple top-units. When this is
   the case, don't even bother to process the network further."
  (let* ((final-primitive (predicate-name (get-target-predicate irl-program))))
    (when final-primitive
      (let* ((processed-irl-program
              (reverse-nominal-groups
               (filter-things
                (duplicate-context irl-program)))))
        (notify process-network-finished processed-irl-program)
        processed-irl-program))))

(defun preprocess-program-for-web-service (irl-program)
  "Preprocess the meaning network for evaluation. Make sure the final
   predicate remains at the end of the meaning network. If comprehension
   was not successful, there could be multiple top-units. When this is
   the case, don't even bother to process the network further."
  (let* ((final-primitive (predicate-name (get-target-predicate irl-program))))
    (when final-primitive
      (let* ((processed-irl-program
              ;(reverse-nominal-groups
               (filter-things
                (duplicate-context irl-program))))
        (notify process-network-finished processed-irl-program)
        processed-irl-program))))