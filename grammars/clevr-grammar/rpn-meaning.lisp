(in-package :clevr-grammar)

(defun all-linked-predicates (predicate var irl-program)
  "Find the next predicate, given a variable"
  (when (member var predicate)
    (remove predicate (find-all var irl-program :test #'member) :test #'equal)))

(defun linked-bind-statement (predicate irl-program)
  "Get the bind-predicate linked to the given predicate"
  (let* ((var (last-elt predicate))
         (all-linked (all-linked-predicates predicate var irl-program))
         (binding-list (remove-if-not #'(lambda (pred)
                                          (eql (first pred) 'bind))
                                      all-linked)))
    (when binding-list
      (first binding-list))))

(defun input-vars (predicate)
  "Get the (possibly multiple) input variable(s) of a predicate"
  (unless (eql (first predicate) 'bind)
    (if (member (first predicate) '(filter query same equal? relate))
      (subseq predicate 2 (- (length predicate) 1))    
      (subseq predicate 2))))

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
                when (eql (fourth bind-statement) 'thing)
                collect (cons filter-pred bind-statement))))
    (loop for (filter-pred . bind-statement) in filter-thing-predicates-and-bindings
          for input-var = (first (input-vars filter-pred))
          for output-var = (second filter-pred)
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
    irl-program))

(defun preprocess-program (irl-program)
  "Preprocess the meaning network for evaluation. Make sure the final
   predicate remains at the end of the meaning network. If comprehension
   was not successful, there could be multiple top-units. When this is
   the case, don't even bother to process the network further."
  (let* ((target-var (get-target-var irl-program))
         (target-pred (find target-var irl-program :key #'second :test #'eql))
         (pred-name (first target-pred)))
    (when pred-name
      (let* ((processed-irl-program
               (filter-things
                (duplicate-context irl-program))))
        processed-irl-program))))

(defun predicate->polish (predicate bind-statement)
  "Write a predicate in polish notation"
  (if bind-statement
    (if (eql (first predicate) 'filter)
      (list (first predicate)
            (read-from-string (downcase (first (split (mkstr (second bind-statement)) #\-))))
            (fourth bind-statement))
      (list (first predicate)
            (fourth bind-statement)))
    (list (first predicate))))

(defun program->rpn (irl-program)
  (let* ((target-var (get-target-var irl-program))
         (target-predicate (find target-var irl-program :key #'second :test #'eql))
         (stack (list target-predicate))
         rpn)
    (loop while stack
          for current-predicate = (pop stack)
          for in-vars = (input-vars current-predicate)
          for bind-statement = (linked-bind-statement current-predicate irl-program)
          do (progn
               (push (predicate->polish current-predicate bind-statement) rpn)
               (dolist (var in-vars)
                 (when (variable-p var)
                   (let ((all-linked (all-linked-predicates current-predicate var irl-program)))
                     (dolist (p all-linked) (when p (push p stack))))))))
    rpn))

(defun variablify-program (irl-program)
  (let* ((consider-symbols
          (loop for predicate in irl-program
                if (eql (first predicate) 'bind)
                append (list (third predicate))
                else
                append (subseq predicate 1)))
         (unique-symbols
          (remove-duplicates consider-symbols))
         (mappings
          (loop for s in unique-symbols
                collect (cons s (make-var s)))))
    (loop for predicate in irl-program
          collect (loop for sym in predicate
                        if (assoc sym mappings)
                        append (list (cdr (assoc sym mappings)))
                        else append (list sym)))))

(defun rpn->str (program)
  (loop for elem in program
        collect
        (remove-if #'(lambda (char)
                       (or (eql char #\!)
                           (eql char #\?)))
                   (downcase
                    (cond ((= (length elem) 1)
                           (mkstr (first elem)))
                          ((= (length elem) 2)
                           (format nil "~a_~a"
                                   (first elem)
                                   (second elem)))
                          ((= (length elem) 3)
                           (format nil "~a_~a[~a]"
                                   (first elem)
                                   (second elem)
                                   (third elem))))))))

(defun clevr-meaning->rpn (irl-program)
  (rpn->str (program->rpn (preprocess-program (variablify-program irl-program)))))