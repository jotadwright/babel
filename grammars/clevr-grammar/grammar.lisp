;;;; grammar.lisp

(in-package :clevr-grammar)

;; + Apply terminal cxns last +

(defclass cxn-supplier-with-ordered-labels-and-terminals-last (cxn-supplier-with-ordered-labels)
  ())

(defmethod create-cxn-supplier ((node cip-node)
                                (mode (eql :ordered-labels-and-terminals-last)))
  (let* ((parent (car (all-parents node))))
    (if parent
      ;; copy most of the stuff from the the pool of the parent
      (make-instance 
       'cxn-supplier-with-ordered-labels-and-terminals-last
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :all-constructions-of-current-label (all-constructions-of-current-label (cxn-supplier parent)))
      ;; there is no parent, start from first label
      (let ((labels (get-configuration (construction-inventory (cip node))
                                       (if (eq (direction (cip node)) '->)
                                         :production-order :parse-order))))
        (make-instance 
         'cxn-supplier-with-ordered-labels-and-terminals-last
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :all-constructions-of-current-label
         (all-constructions-of-label-with-terminals-last node (car labels)))))))

(defun all-constructions-of-label-with-terminals-last (node label)
  "Shuffles the cxns, except for the 'cxn' label. Here, the cxns
   are divided in 2 groups: terminal and non-terminal. Terminal cxns
   are cxns that add the get-context predicate to the meaning network.
   They are the last cxns that need to be applied. Therefore, the non-
   terminal cxns preceed the terminal cxns. Within the groups, the
   cxns are again shuffled."
  (let ((cxns-of-label (copy-object
                        (loop for cxn in (constructions-for-application (construction-inventory (cip node)))
                              for cxn-label = (attr-val cxn :label)
                              when (equalp (symbol-name label) (symbol-name cxn-label))
                              collect cxn))))
    (cond
     ((eql label 'cxn)
      (let ((terminal-cxns (remove-if-not (lambda (cxn) (attr-val cxn :terminal)) cxns-of-label))
            (non-terminal-cxns (remove-if (lambda (cxn) (attr-val cxn :terminal)) cxns-of-label)))
        (append (shuffle non-terminal-cxns)
                (shuffle terminal-cxns))))
     (t
      (shuffle cxns-of-label)))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-with-ordered-labels-and-terminals-last)
                     (node cip-node))
  (cond ((remaining-constructions cxn-supplier)
         ;; there are remaining constructions. just return the next one
         (pop (remaining-constructions cxn-supplier)))
        ((loop for child in (children node)
               thereis (cxn-applied child))
         ;; when the node already has children where cxn application succeeded,
         ;;  then we don't move to the next label
         nil)
        ((remaining-labels cxn-supplier)
         ;; go to the next label
         (setf (current-label cxn-supplier) (car (remaining-labels cxn-supplier)))
         (setf (remaining-labels cxn-supplier) (cdr (remaining-labels cxn-supplier)))
         (setf (all-constructions-of-current-label cxn-supplier)
               (all-constructions-of-label-with-terminals-last node (current-label cxn-supplier)))
         (setf (remaining-constructions cxn-supplier)
               (all-constructions-of-current-label cxn-supplier))
         (next-cxn cxn-supplier node))))

;; + Goal test: meaning network should contain get-context predicate +
;; These are currently not being used

(defmethod cip-goal-test ((node cip-node) (mode (eql :meaning-contains-context-predicate)))
  "Checks whether the resulting meaning contains a get-context predicate"
  (let ((meaning (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car node))))))
    (find 'get-context meaning :key #'first)))

(defmethod cip-goal-test ((node cip-node) (mode (eql :context-predicate-is-taken-from-root)))
  "Checks whether the get-context predicate has been removed from the root"
  (let ((root-meanings (extract-meaning (get-root (left-pole-structure (car-resulting-cfs (cipn-car node)))))))
    (not (find 'get-context root-meanings :key #'first))))

(defparameter *CLEVR*
  (def-fcg-constructions clevr-grammar
    :feature-types ((args set-of-predicates)
                    (form set-of-predicates)
                    (meaning set-of-predicates)
                    (subunits set)
                    (superunits set)
                    (footprints set))
    :fcg-configurations ((:de-render-mode . :de-render-string-meets-precedes-within-3) ;; special de-render mode: precedes within N
                         (:render-mode . :generate-and-test) ;; using the new renderer
                         (:form-predicates meets precedes)
                         (:node-tests  :check-duplicate :restrict-nr-of-nodes)
                         (:parse-goal-tests :no-applicable-cxns
                                            :connected-semantic-network
                                            :connected-structure
                                            :no-strings-in-root) ;; !!! also :connected-structure in comprehension
                         (:production-goal-tests :no-applicable-cxns
                                                 :connected-structure
                                                 :no-meaning-in-root)
                         ;; For guiding search:
                         (:cxn-supplier-mode . :all-cxns-except-incompatible-hashed-cxns)
                         (:node-expansion-mode . :multiple-cxns)
                         (:priority-mode . :nr-of-applied-cxns)
                         (:queue-mode . :greedy-best-first)
                         (:max-nr-of-nodes . 5000))
    :visualization-configurations ((:show-constructional-dependencies . nil)
                                   (:hide-features . (footprints superunits))
                                   (:with-search-debug-data . t))
    :hierarchy-features (subunits)
    :hashed t))