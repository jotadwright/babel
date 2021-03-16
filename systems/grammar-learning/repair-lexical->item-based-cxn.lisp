(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair from lexical to item-based cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass repair-lexical->item-based-cxn (repair) 
  ((trigger :initform 'fcg::new-node)))

(defmethod repair ((repair repair-lexical->item-based-cxn)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (when (initial-node-p node)
    (let ((constructions-and-th-links (create-item-based-cxn-from-lex problem node)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))

(defmethod repair ((repair repair-lexical->item-based-cxn)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (when (initial-node-p node)
    (let ((constructions-and-th-links (create-item-based-cxn-from-lex problem node)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))

(defun find-matching-lex-cxns (cxn-inventory observed-form gold-standard-meaning utterance)
  "return all lexical cxns that can apply by checking whether they are a subset of the observed form and meaning"
  ;; if a certain item matches twice, we'll discard it to avoid ambiguity
  ;; e.g.: is there a cylinder next to the blue cylinders? '(blue cylinder)
  (let ((remaining-form (form-predicates-with-variables observed-form)))
    (sort (loop for cxn in (constructions cxn-inventory)
                when (and (eql (phrase-type cxn) 'lexical) 
                          (irl:unify-irl-programs (extract-form-predicates cxn) remaining-form)
                          (setf remaining-form (set-difference remaining-form (extract-form-predicates cxn) :test #'irl:unify-irl-programs))
                          (irl:unify-irl-programs (extract-meaning-predicates cxn) gold-standard-meaning)
                          ;;we need to check if a cxn could match twice based on the meaning and discard these cases,
                          ;; if it matches multiple times, the size of the set diff will be larger than 1
                          (= 1 (- (length gold-standard-meaning)
                                  (length (set-difference gold-standard-meaning (extract-meaning-predicates cxn) :test #'irl:unify-irl-programs)))))   
                collect cxn)
          #'(lambda (x y)
              (<
               (search (third (first (extract-form-predicates x))) utterance)
               (search (third (first (extract-form-predicates y))) utterance))))))
  
#|
(defun diff-non-overlapping-meaning (gold-standard-meaning matching-lex-cxns)
  "subtract all lexical meanings from the gold standard"
  (let ((resulting-meaning gold-standard-meaning))
    (loop for lex-cxn in matching-lex-cxns
          for lex-cxn-meaning = (extract-meaning-predicates lex-cxn)
          do (setf resulting-meaning (set-difference resulting-meaning lex-cxn-meaning :test #'irl:unify-irl-programs)))
    resulting-meaning))
|#

(defun diff-non-overlapping-form (observed-form matching-lex-cxns)
  "subtract all lexical forms from the gold standard"
  (let ((resulting-form observed-form)
        (lex-unit-names nil))
    (loop for lex-cxn in matching-lex-cxns
          for lex-cxn-form = (extract-form-predicates lex-cxn)
          do (let ((prev-res-form resulting-form))
               (setf resulting-form (set-difference resulting-form lex-cxn-form :test #'irl:unify-irl-programs))
               (setf lex-unit-names (append lex-unit-names (list (second (find 'string (set-difference prev-res-form resulting-form :test #'irl:unify-irl-programs) :key #'first)))))))
    (values lex-unit-names resulting-form)))

(defun diff-non-overlapping-meaning (gold-standard-meaning matching-lex-cxns)
    "subtract all lexical meanings (bind statements) from the gold standard"
  (let ((resulting-meaning gold-standard-meaning)
        (args nil))
    (loop for lex-cxn in matching-lex-cxns
          for lex-cxn-meaning = (first (extract-meaning-predicates lex-cxn))
          do (let ((prev-res-meaning resulting-meaning))
               ;; problem! set difference will remove both instances if there are identical meanings (e.g cylinder & cylinders)
               ;; solution: take only one e.g with find and remove by index
               ;(setf resulting-meaning (remove lex-cxn-meaning resulting-meaning :test #'equal :count 1))
               (setf resulting-meaning (set-difference resulting-meaning (list lex-cxn-meaning) :test #'irl:unify-irl-programs))
               ;(setf args (append args (list (third lex-cxn-meaning))))))
               (setf args (append args (list (third (find 'bind (set-difference prev-res-meaning resulting-meaning :test #'irl:unify-irl-programs) :key #'first)))))))
    (values args resulting-meaning)))


 
(defun subunit-names-for-lex-cxns (lex-cxns)
  (loop for lex-cxn in lex-cxns
        for lex-cxn-form = (extract-form-predicates lex-cxn)
        for lex-cxn-unit-name = (second (find 'string lex-cxn-form :key #'first))
        collect lex-cxn-unit-name))

(defun subunit-block-for-lex-cxns (lex-cxns lex-subunit-names args th-links)
  (loop for lex-cxn in lex-cxns
        for arg in args
        for lex-cxn-unit-name in lex-subunit-names
        for th-link in th-links
        for lex-slot-lex-class = (cdr th-link)
        collect `(,lex-cxn-unit-name
                  (args (,arg))
                  (syn-cat (lex-class ,lex-slot-lex-class)))))

(defun create-type-hierarchy-links (lex-cxns item-based-name placeholders)
  "Creates all TH links for matching lexical cxns using their original lex-class."
  (loop for lex-cxn in lex-cxns
        for lex-cxn-lex-class = (lex-class-cxn lex-cxn)
        for placeholder = (when (< 1 (length placeholders))
                            (format nil "-(~a)" (nth (position lex-cxn lex-cxns) placeholders)))
        for item-slot-lex-class = (make-lex-class (concatenate 'string item-based-name placeholder))
        collect (cons lex-cxn-lex-class item-slot-lex-class)))
        ;collect (list (cons lex-cxn-lex-class item-slot-lex-class)
        ;              (cons item-slot-lex-class lex-cxn-lex-class))))

(defun create-item-based-cxn-from-lex (problem node)
  "Creates item-based construction and lexical constructions
based on existing construction with sufficient overlap."
  (let* ((cxn-inventory (original-cxn-set (construction-inventory node)))
         (utterance (random-elt (get-data problem :utterances)))
         (gold-standard-meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings))))
         (observed-form (extract-forms (left-pole-structure (car-source-cfs (cipn-car (initial-node node))))))
         (matching-lex-cxns (find-matching-lex-cxns cxn-inventory observed-form gold-standard-meaning utterance)))
         
    ;; we need at least one matching lex cxn
    (when (< 0 (length matching-lex-cxns))
      (let* (
             (var-form (form-constraints-with-variables utterance (get-configuration (cxn-inventory (first matching-lex-cxns)) :de-render-mode)))
             (subunit-names-and-non-overlapping-form (multiple-value-list (diff-non-overlapping-form var-form matching-lex-cxns)))
             (subunit-names (first subunit-names-and-non-overlapping-form))
             (non-overlapping-form (second subunit-names-and-non-overlapping-form))
             ;(non-overlapping-meaning (diff-non-overlapping-meaning gold-standard-meaning matching-lex-cxns))
             (args-and-non-overlapping-meaning (multiple-value-list (diff-non-overlapping-meaning gold-standard-meaning matching-lex-cxns)))
             (args (first args-and-non-overlapping-meaning))
             (non-overlapping-meaning (second args-and-non-overlapping-meaning))

             (cxn-name-item-based-cxn (make-cxn-name non-overlapping-form cxn-inventory :add-cxn-suffix nil))
               (rendered-cxn-name-list (make-cxn-placeholder-name non-overlapping-form cxn-inventory))
               (placeholder-list (extract-placeholder-var-list rendered-cxn-name-list))
               (th-links (create-type-hierarchy-links matching-lex-cxns (format nil "~{~a~^-~}" rendered-cxn-name-list) placeholder-list))
               (lex-cxn-subunit-blocks (subunit-block-for-lex-cxns matching-lex-cxns subunit-names args th-links))
               (item-based-cxn (second (multiple-value-list (eval
                                                             `(def-fcg-cxn ,(add-cxn-suffix cxn-name-item-based-cxn)
                                                                           ((?item-based-unit
                                                                             (syn-cat (phrase-type item-based))
                                                                             (subunits ,subunit-names))
                                                                            ,@lex-cxn-subunit-blocks
                                                                            <-
                                                                            (?item-based-unit
                                                                             (HASH meaning ,non-overlapping-meaning)
                                                                             --
                                                                             (HASH form ,non-overlapping-form)))
                                                                           :attributes (:cxn-type item-based
                                                                                   :repair lexical->item-based)
                                                                           :cxn-inventory ,(copy-object cxn-inventory))))))
               )
            ;;(wi:add-element (make-html item-based-cxn))
            (list item-based-cxn matching-lex-cxns th-links)))))

(defmethod handle-fix ((fix fcg::cxn-fix) (repair repair-lexical->item-based-cxn) (problem problem) (node cip-node) &key &allow-other-keys) 
  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (let* ((item-based-cxn (get-processing-cxn (first (restart-data fix))))
           (lex-cxns (map 'list #'get-processing-cxn (second (restart-data fix))))
           (th-links (third (restart-data fix)))
           ;; temporarily store the original type hierarchy, copy it and add the links, and set it to the cxn-inventory
           (orig-type-hierarchy (get-type-hierarchy (construction-inventory node)))
           (temp-type-hierarchy (copy-object (get-type-hierarchy (construction-inventory node))))
           (th-flat-list nil)
           (th (loop for th-list in th-links
                     do (loop for th-link in th-list
                              do (add-categories (list (car th-link) (cdr th-link)) temp-type-hierarchy)
                              (add-link (car th-link) (cdr th-link) temp-type-hierarchy :weight 0.5)
                              (setf th-flat-list (append th-flat-list (list th-link))))
                     finally (set-type-hierarchy (construction-inventory node) temp-type-hierarchy)))
           (lex-nodes (loop for lex-cxn in lex-cxns
                            with last-node = (initial-node node)
                            do (setf last-node (fcg::cip-add-child last-node (first (fcg-apply lex-cxn (if (initial-node-p last-node)
                                                                                                         (car-source-cfs (cipn-car last-node))
                                                                                                         (car-resulting-cfs (cipn-car last-node)))
                                                                                                         (direction (cip node))
                                                                                                      :configuration (configuration (construction-inventory node))
                                                                                                      :cxn-inventory (construction-inventory node)))))
                            collect last-node))
          
           (new-node-item-based (fcg::cip-add-child (last-elt lex-nodes) (first (fcg-apply item-based-cxn (car-resulting-cfs (cipn-car (last-elt lex-nodes))) (direction (cip node))
                                                                                   :configuration (configuration (construction-inventory node))
                                                                                   :cxn-inventory (construction-inventory node))))))
      ;; ignore
      ;; Reset type hierarchy
      (set-type-hierarchy (construction-inventory node) orig-type-hierarchy)
      ;; Add cxns to blackboard of second new node
      (set-data (car-resulting-cfs  (cipn-car new-node-item-based)) :fix-cxns (append (second (restart-data fix)) (list (original-cxn item-based-cxn))))
      (set-data (car-resulting-cfs  (cipn-car new-node-item-based)) :fix-th-links th-flat-list)
      ;; set cxn-supplier to second new node
      (setf (cxn-supplier new-node-item-based) (cxn-supplier node))
      ;; set statuses (colors in web interface)
      (push (type-of repair) (statuses new-node-item-based))
      (push 'added-by-repair (statuses new-node-item-based))
      ;; enqueue only second new node; never backtrack over the first applied lexical construction, we applied them as a block
      (cip-enqueue new-node-item-based (cip node) (get-configuration node :queue-mode)))))
