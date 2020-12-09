(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Add TH links             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
in de repair doe comprehend zonder metalayer :use-meta-layer nil :consolidate-repairs nil :th-connected mode :path-exists
-> check welke cxns toegepast werden
-> maak th links
-> doe fcg apply in handle fix
 |#

(defclass add-th-links (repair) 
  ((trigger :initform 'fcg::new-node)))
  
(defmethod repair ((repair add-th-links)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by adding new th links for existing nodes that were not previously connected."
  (let ((lex-cxn-and-th-link (create-lexical-cxn problem node)))
    (when lex-cxn-and-th-link
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data lex-cxn-and-th-link))))
  
(defmethod repair ((repair add-th-links)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new lexical construction."
  (let ((lex-cxn-and-th-link (create-lexical-cxn problem node)))
    (when lex-cxn-and-th-link 
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data lex-cxn-and-th-link))))

(defun create-lexical-cxn (problem node)
  "Creates a lexical cxn."
  (let* ((observation (left-pole-structure (car-resulting-cfs (cipn-car node))))
         (string-predicates-in-root (form-predicates-with-variables (extract-string (get-root observation)))))
    ;; Only 1 string in root
    (when (and (= 1 (length string-predicates-in-root))
               (find 'item-based (applied-constructions node) :key #'(lambda (cxn) (phrase-type (original-cxn cxn)))))
      (let* ((cxn-inventory (original-cxn-set (construction-inventory node)))
             (meaning-predicates-gold (meaning-predicates-with-variables (first (get-data problem :meanings))))
             (meaning-predicates-observed (extract-meanings observation))
             (meaning-predicates-lex-cxn (set-difference meaning-predicates-gold meaning-predicates-observed :test #'unify))
             (existing-lex-cxn (find-cxn-by-form-and-meaning string-predicates-in-root meaning-predicates-lex-cxn cxn-inventory))
             (cxn-name (make-cxn-name (third (first string-predicates-in-root)) cxn-inventory))
             (unit-name (second (first string-predicates-in-root)))
             (lex-class (if existing-lex-cxn
                          (lex-class-cxn existing-lex-cxn)
                          (intern (symbol-name (make-const unit-name)) :type-hierarchies)))
             (args (mapcar #'third meaning-predicates-lex-cxn))
             (th-link (when (lex-class (find  (make-symbol (subseq (symbol-name unit-name) 1)) observation
                                              :key #'unit-name :test #'string=))
                        (cons lex-class (lex-class (find  (make-symbol (subseq (symbol-name unit-name) 1)) observation
                                                          :key #'unit-name :test #'string=)))))
             (lex-cxn (or existing-lex-cxn
                          (second (multiple-value-list (eval
                                                        `(def-fcg-cxn ,cxn-name
                                                                      ((,unit-name
                                                                        (syn-cat (phrase-type lexical)
                                                                                 (lex-class ,lex-class))
                                                                        (args ,args))
                                                                       <-
                                                                       (,unit-name
                                                                        (HASH meaning ,meaning-predicates-lex-cxn)
                                                                        --
                                                                        (HASH form ,string-predicates-in-root)))
                                                                      :cxn-inventory ,(copy-object cxn-inventory))))))))
        (list
         lex-cxn
         th-link
         (cons (cdr th-link) (car th-link)))))))

(defmethod handle-fix ((fix fcg::cxn-fix) (repair add-th-links) (problem problem) (node cip-node) &key &allow-other-keys) 
  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (let* ((lexical-cxn (get-processing-cxn (first (restart-data fix))))
           ;; temporarily store the original type hierarchy, copy it and add the links, and set it to the cxn-inventory
           (orig-type-hierarchy (get-type-hierarchy (construction-inventory node)))
           (temp-type-hierarchy (copy-object (get-type-hierarchy (construction-inventory node))))
           (th (loop for th-link in (cdr (restart-data fix))
                     do (add-categories (list (car th-link) (cdr th-link)) temp-type-hierarchy)
                     (add-link (car th-link) (cdr th-link) temp-type-hierarchy :weight 0.5)
                     finally (set-type-hierarchy (construction-inventory node) temp-type-hierarchy))) 
           ;; apply lexical-cxn and add node
           (new-node (fcg::cip-add-child node (first (fcg-apply lexical-cxn (car-resulting-cfs (cipn-car node)) (direction (cip node))
                                                                    :configuration (configuration (construction-inventory node))
                                                                    :cxn-inventory (construction-inventory node))))))
      ;; ignore
      ;; Reset type hierarchy
      (set-type-hierarchy (construction-inventory node) orig-type-hierarchy)
      ;; Add cxns to blackboard of second new node
      (set-data (car-resulting-cfs  (cipn-car new-node)) :fix-cxns (list (first (restart-data fix))))
      (set-data (car-resulting-cfs  (cipn-car new-node)) :fix-th-links (cdr (restart-data fix)))
      ;; set statuses
      (push (type-of repair) (statuses new-node))
      (push 'added-by-repair (statuses new-node))
      ;; enqueue only second new node
      (cip-enqueue new-node (cip node) (get-configuration node :queue-mode)))))
