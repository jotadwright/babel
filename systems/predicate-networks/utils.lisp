(in-package :pn)

;; ############################################################################
;; Common utility functions for all types of predicate networks
;; ----------------------------------------------------------------------------

(export '())

;; For drawing predicate networks, use 'draw-predicate-network'
;; and 'predicate-network->svg' from the 'web-interface' package.

(defun all-variables (predicate-network)
  "Get all variables from the predicate network"
  (find-all-anywhere-if #'variable-p predicate-network))

(defun unconnected-variables (predicate-network)
  "Find all unconnected variables, i.e., all those that
   appear just once in the predicate network."
  (loop with variables = (all-variables predicate-network)
        for var in (remove-duplicates variables)
        when (= (count var variables) 1)
        collect var))

(defun linked-predicates (predicate predicate-network &key var)
  "Get all predicates that are linked to the given predicate
   in the predicate network. When 'var' is provided, only consider
   other predicates that are linked through that specific variable."
  (when var (assert (member var predicate)))
  (let ((link-variables (if var (list var) (find-all-if #'variable-p predicate))))
    (remove-duplicates
     (loop with network = (remove predicate predicate-network :test #'equal)
           for v in link-variables
           for predicates = (find-all v network :test #'member)
           append predicates))))

(defun predicate-network-connected-p (predicate-network)
  "Checks whether the predicate network is connected.
   Returns a boolean, the number of sub-graphs and the
   sub-graphs themselves."
  ;; see also 'connected-semantic-network' in
  ;; systems/fcg/construction-inventory-processor/goal-tests.lisp
  (loop with classes = nil
        with sub-networks = nil
        for x in predicate-network
        for variables = (find-all-if #'variable-p x)
        for (cs subs) = (multiple-value-list
                         (loop for class in classes
                               for sub-network in sub-networks
                               when (loop for var in variables
                                          thereis (member var class))
                               collect class into cs
                               collect sub-network into subs
                               finally (return (values cs subs))))
        if cs
        do
        (loop for class in cs do (setf classes (remove class classes)))
        (push (remove-duplicates (reduce #'append (cons variables cs))) classes)
        (loop for sub in subs do (setf sub-networks (remove sub sub-networks)))
        (push (cons x (reduce #'append subs)) sub-networks)
        else
        do
        (push variables classes)
        (push (list x) sub-networks)
        finally (return (values
                         (= (length classes) 1)
                         (length classes)
                         sub-networks))))

(defun predicate-network-p (thing)
  "Returns t if thing conforms to
   the basic syntax of predicate networks,
   which is list of lists."
  (and (listp thing)
       (loop for s in thing
             always (listp s))))

(defun traverse-predicate-network (predicate-network &key first-predicate-fn next-predicate-fn do-fn)
  "General utility function that traverses a predicate network.
   'first-predicate-fn' is used to get the first predicate from the network.
   'next-predicate-fn' takes the current predicate and the network as arguments
   and computes the next predicate(s) to consider.
   'do-fn' is called on every predicate."
  (let ((stack (funcall first-predicate-fn predicate-network))
        visited)
    (loop while stack
          for current-predicate = (pop stack)
          for next-predicates = (funcall next-predicate-fn current-predicate predicate-network)
          do (mapcar #'(lambda (p) (push p stack)) next-predicates)
          do (unless (find current-predicate visited :test #'equal)
               (funcall do-fn current-predicate)
               (push current-predicate visited)))))