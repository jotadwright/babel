;;;; alignment.lisp

(in-package :clevr-learning)

;;;; store past scenes (composer strategy)
;;;; ==> store all past scenes with the applied utterance and correct answer
;;;;     and use these when composing a new program (the new program must
;;;;     lead to the correct answer in all of these scenes).

(defun add-past-scene (agent)
  ;; Add the current scene and answer to the list
  ;; of past scenes for the current utterance
  (let* ((clevr-scene (find-data (ontology agent) 'clevr-context))
         (sample (cons (index clevr-scene) (topic agent)))
         (hash-key (sxhash (utterance agent)))
         (window-size (get-configuration agent :composer-past-scenes-window)))
    (if (gethash hash-key (memory agent))
      (if (>= (length (gethash hash-key (memory agent))) window-size)
        (setf (gethash hash-key (memory agent))
              (cons sample (butlast (gethash hash-key (memory agent)))))
        (push sample (gethash hash-key (memory agent))))
      (setf (gethash hash-key (memory agent)) (list sample)))))


;;;; store past programs (composer strategy)
;;;; ==> store all past programs and use these when composing a new program
;;;;     (the new program must be different from all previous programs).

(defun add-past-program (agent irl-program)
  ;; Add the current irl-program to the list of
  ;; failed programs for the current utterance
  (when irl-program
    (let ((hash-key (sxhash (utterance agent))))
      (if (gethash hash-key (memory agent))
        (push irl-program (gethash hash-key (memory agent)))
        (setf (gethash hash-key (memory agent)) (list irl-program))))))

        


(define-event alignment-started)
(define-event cxns-rewarded (cxns list))
(define-event cxns-punished (cxns list))

;;;; lateral-inhibition (alignment strategy)

(defun hearer-lateral-inhibition (agent cxns success)
  ;; Do the actual alignment. If success, reward
  ;; the applied cxns and punish competitors. If
  ;; no success, punish the applied cxns.
  (notify alignment-started)
  (if success
    (progn
      (loop with cxn-incf = (get-configuration agent :cxn-incf-score)
            for cxn in cxns do (inc-cxn-score cxn :delta cxn-incf)
            finally (notify cxns-rewarded cxns))
      (loop with delta = (get-configuration agent :cxn-inhibit-score) ;; separate inhibit delta!
            with remove-on-lower-bound = (get-configuration agent :remove-cxn-on-lower-bound)
            for competitor in (get-meaning-competitors agent cxns (utterance agent))
            do (dec-cxn-score agent competitor :delta delta
                              :remove-on-lower-bound
                              remove-on-lower-bound)
            collect competitor into competitors
            finally (notify cxns-punished competitors)))
    (when cxns
      (punish-cxns agent cxns))))


(defun punish-cxns (agent cxns)
  "Punish the list of provided cxns"
  (loop with delta = (get-configuration agent :cxn-decf-score) ;; separate failure delta!
        with remove-on-lower-bound = (get-configuration agent :remove-cxn-on-lower-bound)
        for cxn in cxns
        do (dec-cxn-score agent cxn :delta delta
                          :remove-on-lower-bound
                          remove-on-lower-bound)
        finally (notify cxns-punished cxns)))


(defun collect-failed-repair-cxns (cipn applied-cxns)
  ;; Collect constructions for which a repair failed
  (let ((all-cxns
         (remove-duplicates
          (flatten
           (traverse-depth-first
            (gl::initial-node cipn)
            :collect-fn #'(lambda (node)
                            (when (find 'fcg::repair-failed (statuses node))
                              (original-applied-constructions node))))))))
    (set-difference all-cxns applied-cxns :key #'name)))

(defun collect-failed-interpretation-cxns (cipn applied-cxns)
  ;; Collect constructions for which interpretation failed
  (let ((all-cxns
         (remove-duplicates
          (flatten
           (traverse-depth-first
            (gl::initial-node cipn)
            :collect-fn #'(lambda (node)
                            (multiple-value-bind (success foundp)
                                (find-data (goal-test-data node) :interpretation-success)
                              (when (and foundp (null success))
                                (original-applied-constructions node)))))))))
    (set-difference all-cxns applied-cxns :key #'name)))

(defmethod run-hearer-alignment ((agent clevr-learning-learner)
                                  process-input (strategy (eql :lateral-inhibition)))
  ;; Run alignment. If no repair applied, use standard lateral inhibition.
  ;; If a repair applied, and it was a failed interpretation problem, then
  ;; punish the cxns for which interpretation failed. For all other repairs,
  ;; collect cxns for which the repair failed (if any) and punish them.
  ;; Add-th-links is an exception here, as this repair will not set
  ;; repair-applied-p to true! When this repair has applied, also default
  ;; lateral inhibition is used.
  (let* ((success (find-data process-input 'success))
         (applied-cxns (find-data process-input 'applied-cxns))
         (cipn (find-data process-input 'cipn))
         (repair-applied-p (find-data process-input 'repair-applied))
         (failed-interpretation-p (find-data process-input 'failed-interpretation)))
    (if repair-applied-p
      (cond (failed-interpretation-p
             (let ((other-applied-cxns
                    (collect-failed-interpretation-cxns cipn applied-cxns)))
               (when other-applied-cxns
                 (punish-cxns agent other-applied-cxns))))
            (t
             (let ((failed-repair-cxns
                    (collect-failed-repair-cxns cipn applied-cxns)))
               (when failed-repair-cxns
                 (punish-cxns agent failed-repair-cxns)))))
      (hearer-lateral-inhibition agent applied-cxns success))))




(defun speaker-lateral-inhibition (agent cxns success)
  ;; Do the actual alignment. If success, reward
  ;; the applied cxns. If no success, punish the applied cxns.
  ;; Competitors are not considered in formulation, because
  ;; there is lots of synonymy in CLEVR. A meaning can be
  ;; expressed in many different, equally correct ways. We
  ;; do not want to converge on one meaning here.
  (notify alignment-started)
  (if success
    (loop with cxn-incf = (get-configuration agent :cxn-incf-score)
          for cxn in cxns do (inc-cxn-score cxn :delta cxn-incf)
          finally (notify cxns-rewarded cxns))
    (when cxns (punish-cxns agent cxns))))

(defun extract-applied-th-links (cipn)
  (when (find 'fcg::succeeded (fcg::statuses cipn))
    (let* ((applied-cxns (original-applied-constructions cipn))
           (applied-lex-cxns (find-all 'lexical applied-cxns :key #'get-cxn-type))
           (applied-item-based-cxn (find 'item-based applied-cxns :key #'get-cxn-type)))
      (when (and applied-lex-cxns applied-item-based-cxn)
        (let* ((utterance
                (handle-clevr-punctuation
                 (list-of-strings->string
                  (render
                   (extract-forms-from-cipn cipn)
                   (get-configuration
                    (construction-inventory cipn)
                    :render-mode)))))
               (sorted-lex-cxns
                (gl::sort-cxns-by-form-string
                 applied-lex-cxns
                 (remove-punctuation utterance)))
               (lex-classes-lex-cxns
                (mapcar #'gl::lex-class-cxn sorted-lex-cxns))
               (lex-classes-item-based
                (gl::get-all-unit-lex-classes applied-item-based-cxn))
               (th (get-type-hierarchy (construction-inventory cipn)))
               (th-links
                (when (and lex-classes-lex-cxns lex-classes-item-based
                           (length= lex-classes-lex-cxns lex-classes-item-based))
                  (loop for lex-cxn-lex-class in lex-classes-lex-cxns
                        for item-slot-lex-class in lex-classes-item-based
                        when (neighbours-p lex-cxn-lex-class item-slot-lex-class th)
                        collect (cons item-slot-lex-class lex-cxn-lex-class)))))
          th-links)))))
             
(defmethod run-speaker-alignment ((agent clevr-learning-learner)
                                 process-input (strategy (eql :lateral-inhibition)))
  (let* ((success (find-data process-input 'success))
         (applied-cxns (find-data process-input 'applied-cxns))
         (cipn (find-data process-input 'cipn))
         (added-th-links (find-data agent :formulation-th-links)))
    (when added-th-links
      (set-data agent :formulation-th-links nil)
      (let* ((applied-th-links (extract-applied-th-links cipn))
             (th-links-to-remove
              (if (not success) added-th-links
                (set-difference added-th-links applied-th-links
                                :test #'same-th-link-p))))
        (loop for (slot . filler) in th-links-to-remove
              do (delete-link slot filler (get-type-hierarchy (grammar agent)))
              when (not success)
              do (push-data agent :th-link-trash (cons slot filler))))
      (set-data
       (current-interaction (experiment agent))
       :formulation-th-link
       (if success :success :failed)))
    (speaker-lateral-inhibition agent applied-cxns success)))
                            
