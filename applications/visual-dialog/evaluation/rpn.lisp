(in-package :visual-dialog)

;;;; UTILS
;;;; -----

(defun all-linked-predicates (predicate var irl-program)
  "Find the next predicate, given a variable"
  (let ((var-base-name (downcase (get-base-name var))))
    (when (and (member var predicate)
               (not (string= var-base-name "scene")))
      (remove predicate (find-all var irl-program :test #'member) :test #'equal))))

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
    (if (member (first predicate) '(query relate extreme-relate immediate-relate))
      (subseq predicate 2 (- (length predicate) 1))    
      (subseq predicate 2))))

(defun output-position-p (variable predicate)
  (eql (second predicate) variable))

(defun input-position-p (variable predicate)
  (find variable (subseq predicate 2)))


;;;; 1. VARIABLIFY PROGRAM
;;;; ---------------------

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

;;;; 2. PREPROCESS PROGRAM
;;;; ---------------------

(defun preprocess-program (irl-program)
  "Preprocess the meaning network for evaluation. Make sure the final
   predicate remains at the end of the meaning network. If comprehension
   was not successful, there could be multiple top-units. When this is
   the case, don't even bother to process the network further."
  (let ((target-var (get-target-var irl-program)))
    (unless target-var
      (setf irl-program (remove-caption-bind-statement irl-program)))
    (filter-things
     (duplicate-sub-networks 
      (add-get-memory
       (duplicate-context irl-program))))))


(defun remove-caption-bind-statement (irl-program)
  (let* ((all-bind-statements (find-all 'bind irl-program :key #'first)))
    (loop for bind-statement in all-bind-statements
          for connected-predicate
            = (first (all-linked-predicates bind-statement (third bind-statement) irl-program))
          when (eql (third bind-statement) (second connected-predicate))
          do (setf irl-program
                   (remove bind-statement irl-program :test #'equal)))
    irl-program))


(defun duplicate-context (irl-program)
  "When the CLEVR program is a tree, the meaning network has a single
   get-context predicate that links to 2 (or more) other predicates.
   Decouple these get-context predicates such that the meaning network
   is also a tree"
  (let* ((context-predicate (find 'segment-scene irl-program :key #'first))
         (context-var (second context-predicate))
         (next-predicates (all-linked-predicates context-predicate context-var irl-program)))
    (when (> (length next-predicates) 1)
      (let ((new-vars (loop repeat (length next-predicates)
                            collect (make-var 'segmented-scene))))
        (loop for var in new-vars
              for predicate in next-predicates
              do (progn
                   (setf irl-program (remove predicate irl-program :test #'equal))
                   (push `(segment-scene ,var ,(make-var 'scene)) irl-program)
                   (push (subst var context-var predicate) irl-program))
              finally
              (setf irl-program (remove context-predicate irl-program :test #'equal)))))
    irl-program))


(defun add-get-memory (irl-program)
  "Find an open variable that is used as input and is not ?scene,
   this variable is used for the conversation memory."
  (let ((memory-var
         (loop for var in (get-open-vars irl-program)
               for base-name = (downcase (get-base-name var))
               for predicate = (first (find-all var irl-program :test #'member)) ;; ???
               when (and (not (string= base-name "scene"))
                         (input-position-p var predicate))
               return var)))
    (push `(get-memory ,memory-var) irl-program)))


(defun filter-things (irl-program)
  "Remove 'filter' predicate bound to the shape 'thing' since this
   is not used in the CLEVR corpus. These predicates
   are removed and the variables of both input and output
   are stitched together. Also, the bind statements are removed."
  (let* ((filter-predicates (find-all 'filter-by-attribute irl-program :key #'first))
         (filter-thing-predicates-and-bindings
          (loop for filter-pred in filter-predicates
                for bind-statement = (linked-bind-statement filter-pred irl-program)
                when (eql (fourth bind-statement) 'thing)
                collect (cons filter-pred bind-statement))))
    (loop for (filter-pred . bind-statement) in filter-thing-predicates-and-bindings
          for input-var = (third filter-pred)
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


(defun traverse-meaning-network (meaning-network first-predicate &key next-predicate-fn do-fn)
  "General utility function that traverses a meaning network.
   first-predicate-fn is used to compute the first meaning predicate from the network.
   next-predicate-fn takes a predicate and the network and computes the next predicate(s)
   do-fn is called on every predicate"
  (let ((stack (list first-predicate))
        visited)
    (loop while stack
          for current-predicate = (pop stack)
          for next-predicates = (funcall next-predicate-fn current-predicate meaning-network)
          do (mapcar #'(lambda (p) (push p stack)) next-predicates)
          do (unless (find current-predicate visited :test #'equal)
               (funcall do-fn current-predicate)
               (push current-predicate visited)))))


(defun collect-sub-network (sub-network-target-var irl-program)             
  (let ((initial-predicate (find-if #'(lambda (pred) (output-position-p sub-network-target-var pred)) irl-program))
        (sub-network nil))
    (traverse-meaning-network irl-program initial-predicate
                              :next-predicate-fn #'(lambda (pred program)
                                                     (unless (eql (first pred) 'bind)
                                                       (remove-duplicates
                                                        (loop for arg in (subseq pred 2)
                                                              append (all-linked-predicates pred arg program)))))
                              :do-fn #'(lambda (pred) (push pred sub-network)))
    sub-network))


(defun duplicate-sub-networks (irl-program)
  "detect if variable is used once in output position and >1 in input position
   if so, gather the sub-network and duplicate it"
  (let ((sub-network-variables
         (loop for var in (remove-duplicates (all-variables irl-program))
               for predicates = (find-all var irl-program :test #'member)
               for output-position-count = (count-if #'(lambda (pred) (output-position-p var pred)) predicates)
               for input-position-count = (count-if #'(lambda (pred) (input-position-p var pred)) predicates)
               when (and (= output-position-count 1) (> input-position-count 1))
               collect var)))
    (loop for var in sub-network-variables
          for sub-network = (collect-sub-network var irl-program)     
          for input-preds = (find-all-if #'(lambda (pred) (input-position-p var pred))
                                         (set-difference irl-program sub-network :test #'equal))
          ;; remove the sub network
          do (setf irl-program (set-difference irl-program sub-network :test #'equal))
          ;; insert the input predicates again + a copy of the sub-network with all fresh variables!
          do (loop for p in input-preds
                   for fresh-var = (make-var)
                   for sub-network-fresh-vars
                     = (loop for v in (remove-duplicates (all-variables sub-network))
                             for base-name = (downcase (get-base-name v))
                             if (string= base-name "scene")
                             collect (cons v (make-var 'scene))
                             else if (not (eql v var))
                             collect (cons v (make-var)))
                   for sub-network-with-fresh-vars
                     = (fcg::substitute-bindings sub-network-fresh-vars (copy-list sub-network))
                   do (setf irl-program (remove p irl-program :test #'equal))
                      (push (subst fresh-var var p) irl-program)
                      (setf irl-program (append (subst fresh-var var sub-network-with-fresh-vars) irl-program))))
    irl-program))
          
          
  

;;;; 3. PROGRAM -> RPN
;;;; -----------------

(defun program->rpn (irl-program)
  (let* ((target-var (get-target-var irl-program))
         (target-predicate (find target-var irl-program :key #'second :test #'eql))
         (stack (list target-predicate))
         processed
         rpn)
    (loop while stack
          for current-predicate = (pop stack)
          for in-vars = (input-vars current-predicate)
          for bind-statement = (linked-bind-statement current-predicate irl-program)    
          do (progn
               (push (predicate->polish current-predicate bind-statement) rpn)
               (dolist (var (reverse in-vars))
                 (when (variable-p var)
                   (let ((all-linked (all-linked-predicates current-predicate var irl-program)))
                     (dolist (p all-linked)
                       (when p
                         (unless (or (find p stack :test #'equal)
                                     (find p processed :test #'equal)
                                     (eql (first p) 'bind))
                           (push p stack)))))))
               (push current-predicate processed)))
    rpn))

(defun predicate->polish (predicate bind-statement)
  "Write a predicate in polish notation"
  (if bind-statement
    (if (eql (first predicate) 'clevr-dialog-grammar::filter-by-attribute)
      (list 'filter
            (read-from-string (downcase (first (split (mkstr (second bind-statement)) #\-))))
            (fourth bind-statement))
      (list (first predicate) (fourth bind-statement)))
    (if (eql (first predicate) 'clevr-dialog-grammar::filter-by-attribute)
      (list 'filter)
      (list (first predicate)))))
         

;;;; 4. RPN -> STRING
;;;; ----------------

(defun rpn->str (program)
  (loop for elem in program
        collect
        (remove-if
         #'(lambda (char)
             (or (eql char #\!)
                 (eql char #\?)))
         (cond ((= (length elem) 1)
                (lisp->camel-case (downcase (mkstr (first elem))) :from-first nil))
                ((= (length elem) 2)
                 (format nil "~a_~a"
                         (lisp->camel-case (downcase (first elem)) :from-first nil)
                         (downcase (second elem))))
                ((= (length elem) 3)
                 (format nil "~a_~a[~a]"
                         (downcase (first elem))
                         (downcase (second elem))
                         (downcase (third elem))))))))


;;;; #### MAIN ####
;;;; --------------

(defun clevr-meaning->rpn (irl-program)
  "This function takes care of all the steps to transform
   an IRL program to RPN notation for the seq2seq heuristics:
   - variablify the network; all symbols except the predicate names
     are turned to variables. For bind statements, only the third
     element become variables.
   - preprocess the network; e.g. duplicate the get-context if needed
     and remove filter(thing) operations
   - transform to RPN notation; each operation is a list of symbols,
     e.g. ((get-context) (filter color red))
   - RPN to list of string; using the facebook notation,
     e.g. get-context filter_color[red]
   - list of string to single string"
  (list-of-strings->string 
   (rpn->str
    (program->rpn
     (preprocess-program
      (read-from-string
       (mkstr irl-program)))))))
      ;(variablify-program irl-program))))))