(in-package :clg)

;; -------------
;; + Alignment +
;; -------------

(defmethod run-speaker-alignment ((agent clevr-learning-learner)
                                  process-input
                                  (strategy (eql :lateral-inhibition)))
  (let* ((success (find-data process-input 'success))
         (applied-cxns (find-data process-input 'applied-cxns))
         (cipn (find-data process-input 'cipn))
         (added-th-links (find-data agent :formulation-th-links)))
    (when added-th-links
      (set-data agent :formulation-th-links nil)
      (let* ((applied-th-links (extract-applied-th-links cipn))
             (th-links-to-remove (if (not success)
                                   added-th-links
                                   (set-difference added-th-links
                                                   applied-th-links
                                                   :test #'same-th-link-p))))
        (loop for (slot . filler) in th-links-to-remove
              do (remove-link slot filler (categorial-network (grammar agent)))
              when (not success)
                do (push-data agent :th-link-trash (cons slot filler))))
      (set-data (current-interaction (experiment agent)) :formulation-th-link (if success :success :failed)))
    (speaker-lateral-inhibition agent applied-cxns success)))


(defmethod run-hearer-alignment ((agent clevr-learning-learner)
                                 process-input
                                 (strategy (eql :lateral-inhibition)))
  "Run alignment.

       If no repair applied, use standard lateral inhibition.

       If a repair applied, and it was a failed interpretation problem,
       then punish the cxns for which interpretation failed.

       For all other repairs, collect cxns for which the repair failed (if any) and punish them.

       Add-th-links is an exception here, as this repair will not set
       repair-applied-p to true! When this repair has applied, also default
       lateral inhibition is used."
  (let* ((success (find-data process-input 'success))
         (applied-cxns (find-data process-input 'applied-cxns))
         (cipn (find-data process-input 'cipn))
         (repair-applied-p (find-data process-input 'repair-applied))
         (failed-interpretation-p (find-data process-input 'failed-interpretation)))
    (if repair-applied-p
      (cond (failed-interpretation-p
             (let ((other-applied-cxns (collect-failed-interpretation-cxns cipn applied-cxns)))
               (when other-applied-cxns
                 (punish-cxns agent other-applied-cxns))))
            (t
             (let ((failed-repair-cxns (collect-failed-repair-cxns cipn applied-cxns)))
               (when failed-repair-cxns
                 (punish-cxns agent failed-repair-cxns)))))
      (hearer-lateral-inhibition agent applied-cxns success))))

;; ----------------------------------------
;; + Utils to find specific constructions +
;; ----------------------------------------
(defun collect-failed-repair-cxns (cipn applied-cxns)
  "Collect constructions for which a repair failed."
  (let ((all-cxns (remove-duplicates (flatten (traverse-depth-first (initial-node cipn)
                                                                    :collect-fn #'(lambda (node)
                                                                                    (when (find 'fcg::repair-failed (statuses node))
                                                                                      (original-applied-constructions node))))))))
    (set-difference all-cxns applied-cxns :key #'name)))

(defun collect-failed-interpretation-cxns (cipn applied-cxns)
  "Collect constructions for which interpretation failed."
  (let ((all-cxns (remove-duplicates (flatten (traverse-depth-first (initial-node cipn)
                                                                    :collect-fn #'(lambda (node)
                                                                                    (multiple-value-bind (success foundp)
                                                                                        (find-data (goal-test-data node) :interpretation-success)
                                                                                      (when (and foundp (null success))
                                                                                        (original-applied-constructions node)))))))))
    (set-difference all-cxns applied-cxns :key #'name)))

(defun extract-applied-th-links (cipn)
  (when (find 'fcg::succeeded (fcg::statuses cipn))
    (let* ((applied-cxns (original-applied-constructions cipn))
           (applied-lex-cxns (find-all 'lexical applied-cxns :key #'get-cxn-type))
           (applied-item-based-cxn (find 'item-based applied-cxns :key #'get-cxn-type)))
      (when (and applied-lex-cxns applied-item-based-cxn)
        (let* ((utterance (handle-clevr-punctuation (list-of-strings->string (render (extract-forms-from-cipn cipn)
                                                                                     (get-configuration (construction-inventory cipn) :render-mode)))))
               (sorted-lex-cxns (sort-cxns-by-form-string applied-lex-cxns (remove-punctuation utterance)))
               (lex-classes-lex-cxns (mapcar #'lex-class-cxn sorted-lex-cxns))
               (lex-classes-item-based (get-all-unit-lex-classes applied-item-based-cxn))
               (th (categorial-network (construction-inventory cipn)))
               (th-links (when (and lex-classes-lex-cxns lex-classes-item-based
                                    (length= lex-classes-lex-cxns lex-classes-item-based))
                           (loop for lex-cxn-lex-class in lex-classes-lex-cxns
                                 for item-slot-lex-class in lex-classes-item-based
                                 when (neighbouring-categories-p lex-cxn-lex-class item-slot-lex-class th)
                                   collect (cons item-slot-lex-class lex-cxn-lex-class)))))
          th-links)))))
