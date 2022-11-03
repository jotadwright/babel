;;;; add-th-links-formulation.lisp

(in-package :intention-reading)

;;  ADD-TH-LINKS-FORMULATION
;; -------------------------

(define-event add-th-links-formulation-repair-started)
(define-event add-th-links-formulation-new-th-links
  (th categorial-network) (new-links list))

(defclass add-th-links-formulation (clevr-learning-repair)
  ((trigger :initform 'fcg::new-node)))

(defmethod repair ((repair add-th-links-formulation)
                   (problem partial-meaning-problem)
                   (node cip-node) &key
                   &allow-other-keys)
  (let* ((agent (find-data problem :owner))
         (repair-mode (get-configuration agent :th-link-repair-mode-formulation))
         (cxns-and-th-links
          (case repair-mode
            (:path-required (create-formulation-th-links-with-path problem node))
            (:no-path-required (create-formulation-th-links-no-path problem node)))))
    (when cxns-and-th-links
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data cxns-and-th-links))))

;; PATH REQUIRED
;; -------------

(defun create-formulation-th-links-with-path (problem node)
  (let* ((agent (find-data problem :owner))
         (cxn-inventory (original-cxn-set (construction-inventory node)))
         (type-hierarchy (categorial-network cxn-inventory))
         (meaning (cipn-meaning node)))
    (with-disabled-monitor-notifications 
      (disable-meta-layer-configuration cxn-inventory)
      (multiple-value-bind (utterance cipn)
          (formulate meaning :cxn-inventory cxn-inventory :silent t)
        (enable-meta-layer-configuration cxn-inventory)
        ;; there is a solution with connected links in the TH
        (when (find 'fcg::succeeded (fcg::statuses cipn))
          (let* ((applied-cxns (original-applied-constructions cipn))
                 (applied-lex-cxns
                  (find-all 'lexical applied-cxns :key #'get-cxn-type))
                 (sorted-lex-cxns
                  (sort-cxns-by-form-string
                   applied-lex-cxns
                   (list-of-strings->string utterance)))
                 (lex-classes-lex-cxns
                  (when sorted-lex-cxns
                    (mapcar #'lex-class-cxn sorted-lex-cxns)))
                 (applied-item-based-cxn
                  (find 'item-based applied-cxns :key #'get-cxn-type))
                 (lex-classes-item-based-units
                  (when applied-item-based-cxn
                    (get-all-unit-lex-classes applied-item-based-cxn)))
                 (th-links
                  (when (and lex-classes-lex-cxns
                             lex-classes-item-based-units
                             (= (length lex-classes-lex-cxns)
                                (length lex-classes-item-based-units)))
                    (create-new-th-links lex-classes-lex-cxns lex-classes-item-based-units type-hierarchy))))
            (when th-links
              (set-data (current-interaction (experiment agent))
                        :applied-repair 'add-th-links)
              ;; store the th links on the agent's blackboard,
              ;; because some may be removed again later on!
              (set-data agent :formulation-th-links th-links)
              (list applied-cxns nil nil th-links))))))))    
                  

;; NO PATH REQUIRED
;; ----------------

(defun get-meaning-from-root-first-merge-failed (node)
  (meaning-predicates-with-variables
   (extract-meaning
    (get-root
     (left-pole-structure
      (car-source-cfs
       (cipn-car node)))))))

(defun create-formulation-th-links-no-path (problem node)
  (let* ((agent (find-data problem :owner))
         (cxn-inventory (original-cxn-set (construction-inventory node)))
         (fmf-children
          (loop for child in (children node)
                when (find 'fcg::first-merge-failed (fcg::statuses child))
                collect child into fmf-children
                finally (return (sort fmf-children #'< :key #'created-at)))))
    (when fmf-children
      (loop for fmf-node in fmf-children
            for applied-cxns = (original-applied-constructions fmf-node)
            for applied-lex-cxns = (find-all 'lexical applied-cxns :key #'get-cxn-type)
            for applied-item-based-cxn = (find 'item-based applied-cxns :key #'get-cxn-type)
            for meaning-in-root = (get-meaning-from-root-first-merge-failed fmf-node)
            for meaning-in-cxn = (extract-meaning-predicates (first applied-cxns))                        
            when (and (not (null applied-lex-cxns))
                      (not (null applied-item-based-cxn))
                      (= (length applied-lex-cxns)
                         (item-based-number-of-slots applied-item-based-cxn))
                      (null (set-difference meaning-in-root meaning-in-cxn
                                            :test #'unify-irl-programs))
                      (null (set-difference meaning-in-cxn meaning-in-root
                                            :test #'unify-irl-programs)))
            ;; here we cannot sort the lex cxns according to the utterance
            ;; to find out which links should be made, because there is no utterance
            ;; So, we make all possible links and store them somewhere in the agent
            ;; such that bad links can be removed later on during alignment.
            ;; However, if there is already a link between any of the fillers and
            ;; any of the slots, do not provide new links for this filler and this slot
            do (let* ((lex-classes-lex-cxns
                       (mapcar #'lex-class-cxn applied-lex-cxns))
                      (lex-classes-item-based
                       (get-all-unit-lex-classes applied-item-based-cxn))
                      (th (categorial-network cxn-inventory))
                      (exclude-categories
                       (loop for slot in lex-classes-item-based
                             append (loop for filler in lex-classes-lex-cxns
                                          when (neighbouring-categories-p slot filler th)
                                          append (list slot filler))))
                      (all-th-links
                       (loop for slot in lex-classes-item-based
                             unless (member slot exclude-categories)
                             append (loop with link-trash = (find-data agent :th-link-trash)
                                          for filler in lex-classes-lex-cxns
                                          unless (or (member filler exclude-categories)
                                                     (find (cons slot filler) link-trash
                                                           :test #'same-th-link-p))
                                          collect (cons slot filler)))))
                 (when all-th-links
                   (set-data (current-interaction (experiment agent))
                             :applied-repair 'add-th-links)
                   ;; store the th links on the agent's blackboard,
                   ;; because some may be removed again later on!
                   (set-data agent :formulation-th-links all-th-links)
                   (return (list (reverse applied-cxns) nil nil all-th-links))))))))