;;;; add-th-links.lisp

(in-package :intention-reading)

;;  ADD-TH-LINKS
;; --------------

(define-event add-th-links-repair-started)
(define-event add-th-links-new-th-links
  (th categorial-network) (new-links list))

(defclass add-th-links (clevr-learning-repair)
  ((trigger :initform 'fcg::new-node)))

(defmethod repair ((repair add-th-links)
                   (problem partial-utterance-problem)
                   (node cip-node) &key
                   &allow-other-keys)
  (let* ((agent (find-data problem :owner))
         (repair-mode (get-configuration agent :th-link-repair-mode-comprehension))
         (cxns-and-th-links
          (case repair-mode
            (:path-required (create-th-links-with-path problem node))
            (:no-path-required (create-th-links-no-path problem node)))))
    (when cxns-and-th-links
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data cxns-and-th-links))))

;; PATH REQUIRED
;; -------------

(defun create-th-links-with-path (problem node)
  (let* ((agent (find-data problem :owner))
         (cxn-inventory (original-cxn-set (construction-inventory node)))
         (type-hierarchy (categorial-network cxn-inventory))
         (utterance (cipn-utterance node)))
    (with-disabled-monitor-notifications
      (disable-meta-layer-configuration cxn-inventory)
      (multiple-value-bind (meaning cipn)
          (comprehend utterance :cxn-inventory cxn-inventory :silent t)
        (declare (ignorable meaning))
        (enable-meta-layer-configuration cxn-inventory)
        (when (find 'fcg::succeeded (fcg::statuses cipn))
          (let* ((applied-cxns (original-applied-constructions cipn))
                 (lex-cxns (find-all 'lexical applied-cxns :key #'get-cxn-type))
                 (sorted-lex-cxns
                  (sort-cxns-by-form-string
                   lex-cxns (remove-punctuation utterance)))
                 (lex-classes-lex-cxns
                  (when sorted-lex-cxns (mapcar #'lex-class-cxn sorted-lex-cxns)))
                 (item-based-cxn (find 'item-based applied-cxns :key #'get-cxn-type))
                 (lex-classes-item-based-units
                  (when item-based-cxn (get-all-unit-lex-classes item-based-cxn)))
                 (th-links
                  (when (and lex-classes-lex-cxns lex-classes-item-based-units
                             (length= lex-classes-lex-cxns lex-classes-item-based-units))
                    (create-new-th-links
                     lex-classes-lex-cxns
                     lex-classes-item-based-units
                     type-hierarchy))))
            (when th-links
              (set-data (current-interaction (experiment agent))
                        :applied-repair 'add-th-links)
              (list applied-cxns nil nil th-links))))))))


;; NO PATH REQUIRED
;; ----------------

(defun get-strings-from-root-second-merge-failed (node)
  (form-predicates-with-variables
   (extract-string
    (get-root
     (car-first-merge-structure
      (cipn-car node))))))

(defun create-th-links-no-path (problem node)
  (let* ((agent (find-data problem :owner))
         (cxn-inventory (original-cxn-set (construction-inventory node)))
         (utterance (cipn-utterance node))
         (second-merge-failed-children
          (when (children node)
            (find-all 'fcg::second-merge-failed (children node)
                      :key #'statuses :test #'member))))
    (when second-merge-failed-children
      (loop for smf-node in second-merge-failed-children
            for applied-cxns = (original-applied-constructions smf-node)
            for applied-lex-cxns = (find-all 'lexical applied-cxns :key #'get-cxn-type)
            for applied-item-based-cxn = (find 'item-based applied-cxns :key #'get-cxn-type)
            for strings-in-root = (get-strings-from-root-second-merge-failed smf-node)
            when (and (not (null applied-lex-cxns))
                      (not (null applied-item-based-cxn))
                      (= (length applied-lex-cxns)
                         (item-based-number-of-slots applied-item-based-cxn))
                      (null strings-in-root))
            ;; need to do an additional check here?
            ;; run comprehension with a temp type hierarchy?
            ;; or just go with it?
            do (let* ((sorted-lex-cxns
                       (sort-cxns-by-form-string
                        applied-lex-cxns
                        (remove-punctuation utterance)))
                      (lex-classes-lex-cxns
                       (mapcar #'lex-class-cxn sorted-lex-cxns))
                      (lex-classes-item-based
                       (get-all-unit-lex-classes applied-item-based-cxn))
                      (th-links
                       (when (and lex-classes-lex-cxns lex-classes-item-based
                                  (length= lex-classes-lex-cxns lex-classes-item-based))
                         (create-new-th-links lex-classes-lex-cxns lex-classes-item-based
                                                  (categorial-network cxn-inventory)))))
                 (when th-links
                   (set-data (current-interaction (experiment agent))
                             :applied-repair 'add-th-links)
                   (return (list applied-cxns nil nil th-links))))))))

