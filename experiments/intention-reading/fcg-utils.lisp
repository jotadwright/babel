;;;; fcg-utils.lisp

(in-package :intention-reading)

(defun all-cip-nodes (cip)
  (remove nil
          (traverse-depth-first
           (top-node cip)
           :collect-fn #'(lambda (node)
                           (unless (or (find 'fcg::duplicate (fcg::statuses node))
                                       (find 'fcg::second-merge-failed (fcg::statuses node)))
                             node)))))

(defun all-cipn-statuses (cipn)
  (flatten
   (traverse-depth-first
    (initial-node cipn)
    :collect-fn #'statuses)))

(defun initial-node-p (node)
  (find 'fcg::initial (fcg::statuses node)))

(defun toggle-th-connected-mode (cxn-inventory mode)
  (set-configuration cxn-inventory :th-connected-mode mode :replace t))

(defun extract-meanings-from-cipn (cipn)
  (extract-meanings
   (left-pole-structure
    (car-resulting-cfs
     (cipn-car cipn)))))

(defun extract-forms-from-cipn (cipn)
  (extract-forms
   (left-pole-structure
    (car-resulting-cfs
     (cipn-car cipn)))))

(defun handle-clevr-punctuation (utterance)
  ;; The utterance should start with an uppercase
  ;; letter and have a question mark at the end.
  ;; The semicolon (if present) should be attached
  ;; to the word in front, otherwise it cannot be
  ;; processed by the CLEVR grammar.
  (format nil "~@(~a~)?"
          (if (search ";" utterance)
            (loop with words = nil
                  for word in (split utterance #\space)
                  if (string= word ";")
                  do (push (mkstr (pop words) word) words)
                  else do (push word words)
                  finally (return
                           (list-of-strings->string
                            (reverse words))))
            utterance)))

(defun cipn-utterance (cipn)
  (handle-clevr-punctuation
   (list-of-strings->string
    (render
     (extract-forms
      (left-pole-structure
       (initial-cfs (cip cipn))))
     (get-configuration
      (construction-inventory cipn)
      :render-mode)))))

(defun cipn-meaning (cipn)
  (extract-meanings 
   (left-pole-structure
    (initial-cfs (cip cipn)))))

(defun get-cxn-type (cxn)
  (attr-val cxn :cxn-type))

(defun get-cxn-chunk (cxn)
  (attr-val cxn :chunk))

(defun cxn-score (cxn)
  (attr-val cxn :score))

(defun cxn-added-at (cxn)
  (attr-val cxn :added-at))

(defun item-based-number-of-slots (cxn)
  (when (eql (get-cxn-type cxn) 'item-based)
    (1- (length (contributing-part cxn)))))

(defun get-strings-from-root (node)
  (form-predicates-with-variables
   (extract-string
    (get-root
     (left-pole-structure
      (car-resulting-cfs
       (cipn-car node)))))))

(defun get-meaning-from-root (node)
  (meaning-predicates-with-variables
   (extract-meaning
    (get-root
     (left-pole-structure
      (car-resulting-cfs
       (cipn-car node)))))))

(defun set-cxn-last-used (agent cxn)
  (let ((current-interaction-nr
         (interaction-number
          (current-interaction
           (experiment agent)))))
    (setf (attr-val cxn :last-used) current-interaction-nr)))

(defun extract-and-render (cxn)
  (list-of-strings->string
   (render (extract-form-predicates cxn)
           (get-configuration (cxn-inventory cxn) :render-mode))))

(defun get-cxns-of-type (agent type)
  (if (eql type 'all)
    (constructions-list (grammar agent))
    (find-all type (constructions-list (grammar agent))
              :key #'get-cxn-type)))

(defun find-cxn-by-type-form-and-meaning (type form meaning cxn-inventory)
  "returns a cxn with the same meaning and form if it's in the cxn-inventory"
  (loop for cxn in (find-all type (constructions-list cxn-inventory) :key #'get-cxn-type)
        when (and (irl:equivalent-irl-programs? form (extract-form-predicates cxn))
                  (irl:equivalent-irl-programs? meaning (extract-meaning-predicates cxn)))
        return cxn))

(defun subunit-blocks-for-lex-cxns (lex-cxns lex-subunit-names args th-links)
  (loop for lex-cxn in lex-cxns
        for arg in args
        for lex-cxn-unit-name in lex-subunit-names
        for th-link in th-links
        for lex-slot-lex-class = (cdr th-link)
        collect `(,lex-cxn-unit-name
                  (syn-cat (fcg::lex-class ,lex-slot-lex-class)))
        into contributing-units
        collect `(,lex-cxn-unit-name
                  (args (,arg))
                  --)
        into conditional-units
        finally (return (values conditional-units contributing-units))))

(defun form-predicates->hash-string (form-predicates)
  ;; the last string predicate
  (third
   (last-elt
    (find-all 'string form-predicates
              :key #'first))))

(defun meaning-predicates->hash-meaning (meaning-predicates)
  (let* ((all-primitives
          (mapcar #'first meaning-predicates))
         (all-primitives-but-bind
          (remove 'bind all-primitives))
         (target-variable
          (get-target-var meaning-predicates)))
    ;; if there are only bind statements
    (if (null all-primitives-but-bind)
      ;; take the last element of the first binding
      (last-elt (first (find-all 'bind meaning-predicates :key #'first)))
      ;; otherwise, take the primitive that holds the target var
      (first (find target-variable meaning-predicates :key #'second)))))


(defun find-matching-lex-cxns-in-item-based-constraints (form-constraints meaning-constraints applicable-cxns)
  "return all lexical cxns that can be extracted by checking
   whether they are a subset of the form and meaning
   of an item-based construction. This function takes into
   account lexical cxns that are applicable more than once
   by checking each unification separately, instead of using
   set-difference."
  (let ((remaining-form form-constraints)
        (remaining-meaning meaning-constraints))
    (loop for cxn in applicable-cxns
          for cxn-form
          = (form-predicates-with-variables
             (extract-form-predicates cxn))
          for cxn-meaning
          = (extract-meaning-predicates cxn)
          for unified-form-elem
          = (loop for elem in remaining-form
                  when (unify-irl-programs cxn-form (list elem))
                  return elem)
          for unified-meaning-elem
          = (loop for elem in remaining-meaning
                  when (unify-irl-programs cxn-meaning (list elem))
                  return elem)
          when (and unified-form-elem unified-meaning-elem)
          do (progn
               (setf remaining-form
                     (remove unified-form-elem remaining-form :test #'equal))
               (setf remaining-meaning
                     (remove unified-meaning-elem remaining-meaning :test #'equal)))
          and collect cxn)))

(defmethod cip-leafs ((cip construction-inventory-processor))
  "Get all leafs from the cipn"
  (labels ((leafs-rec (node acc)
             (if (null (children node))
               (cons node acc)
               (loop for child in (children node)
                     append (leafs-rec child acc)))))
    (leafs-rec (top-node cip) nil)))

(defun same-th-link-p (link-1 link-2)
  (or (and (eq (car link-1) (car link-2))
           (eq (cdr link-1) (cdr link-2)))
      (and (eq (car link-1) (cdr link-2))
           (eq (cdr link-1) (car link-2)))))
