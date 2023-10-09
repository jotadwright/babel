;(ql:quickload :muhai-cookingbot)
(in-package :muhai-cookingbot)

(activate-monitor trace-fcg)
(activate-monitor trace-irl)

(load (babel-pathname :directory '("applications" "muhai-cookingbot" "demo") :name "composer-utils" :type "lisp"))
(load (babel-pathname :directory '("applications" "muhai-cookingbot" "demo") :name "composer" :type "lisp"))
(load (babel-pathname :directory '("applications" "muhai-cookingbot" "demo") :name "initial-kitchen-state" :type "lisp"))

(defparameter *cxn-inventory*
  (cl-store::restore
   (babel-pathname :directory '("applications" "muhai-cookingbot" "demo")
                   :name "ingredient-grammar" :type "fcg")))

(progn
  (set-configuration *cxn-inventory* :update-categorial-links nil)
  (set-configuration *cxn-inventory* :use-meta-layer nil)
  (set-configuration *cxn-inventory* :consolidate-repairs nil)
  (set-configuration *cxn-inventory* :parse-goal-tests
                     '(:no-strings-in-root
                       :no-applicable-cxns
                       :connected-semantic-network
                       :connected-structure)))

(defun intern-network (meaning-network)
  (loop for predicate in meaning-network
        collect (loop for elem in predicate
                      collect (internal-symb elem))))

(defun parse-quantity-numbers (meaning-network)
  (loop for predicate in meaning-network
        if (and (eql (first predicate) 'bind)
                (eql (second predicate) 'quantity))
        collect (list 'bind 'quantity (third predicate) (hcl::parse-float (mkstr (fourth predicate))))
        else collect predicate))

(defun process-utterances (list-of-utterances personal-dynamic-memory &key silent)
  (loop with cxn-inventory = (grammar personal-dynamic-memory)
        with meaning-network = nil
        with final-set-of-bindings = nil
        for utterance in list-of-utterances
        for current-world-state = (first (world-states personal-dynamic-memory))
        do (multiple-value-bind (bindings-lists parsed-meaning)
               (understand-and-execute utterance cxn-inventory current-world-state :silent silent)
             (loop for bindings-list in bindings-lists
                   when bindings-list
                   do (push (make-instance 'world-state
                                           :accessible-entities bindings-list
                                           :personal-dynamic-memory personal-dynamic-memory)
                            (world-states personal-dynamic-memory))
                      (setf meaning-network (append meaning-network (reverse parsed-meaning)))
                      (setf final-set-of-bindings bindings-list)))
        finally (return (values final-set-of-bindings meaning-network))))

        
(defun understand-and-execute (utterance
                               cxn-inventory
                               world-state
                               &key silent)
  (let* ((comprehension-result (multiple-value-list (comprehend utterance :cxn-inventory cxn-inventory :silent silent)))
         (parsed-meaning (parse-quantity-numbers (intern-network (first comprehension-result))))
         (existing-bindings (accessible-entities world-state))
         (extended-meaning (append-meaning-and-irl-bindings parsed-meaning existing-bindings))
         (connected-meaning (connect-accessible-entities extended-meaning))
         (resulting-bindings-lists (evaluate-irl-program connected-meaning nil))
         (open-variables (get-unconnected-vars connected-meaning))
         (resulting-bindings-with-open-variables
          (loop with all-vars = (all-variables connected-meaning)
                for bindings-list in resulting-bindings-lists
                collect (loop for var in open-variables
                              when (and (or (find var existing-bindings :key #'var)
                                            (find var all-vars))
                                        (available-at (find var bindings-list :key #'var)))
                              collect (find var bindings-list :key #'var)))))
    (values resulting-bindings-with-open-variables parsed-meaning)))

(defparameter *init-op* `((get-kitchen ,(make-var 'kitchen-state))))

(defparameter *pdm*
  (initialise-personal-dynamic-memory *cxn-inventory* *init-op*))

(process-utterances
 '("1 head fresh broccoli (do not use frozen!)"
   "50 grams red onion, chopped"
   "450 grams cooked bacon"
   "2 1/2 tablespoons cider vinegar"
   "230 grams mayonnaise"
   "70 grams sugar"
   "170 grams grated mozzarella cheese"
   ;; at this point, you end up with a bunch of accessible entities (bowls with contents)
   ;; now if the next sentence comes in:
   ;; "Cut cooked bacon into pieces."
   ;; there will be many possible solutions...
   ;; and no information from language can be used by the composer to make the right link... :-(
   )
 *pdm*)


