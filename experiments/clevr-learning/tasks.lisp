;;;; tasks.lisp

(in-package :clevr-learning)

;; -----------------------
;; + Learner Hearer Task +
;; -----------------------

(defclass learner-hearer-task (task-w-learning)
  () (:documentation "Task executed by the learner when it is the hearer"))

(defgeneric run-learner-hearer-task (agent)
  (:documentation "Entry point for the tutor speaker task"))

(defmethod run-learner-hearer-task (agent)
  (let* ((diagnostics
          (loop for diagnostic in '(diagnose-parsing-result
                                    diagnose-aligment-result)
                collect (make-instance diagnostic)))
         (repairs
          (loop for repair in '(repair-add-th-links
                                repair-item-based->lexical
                                repair-lexical->item-based
                                repair-holophrase->item-based-substitution
                                repair-holophrase->item-based-addition
                                repair-holophrase->item-based-deletion
                                repair-item-based+lexical->item-based
                                repair-make-hypotheses
                                repair-make-holophrase-cxn)
                collect (make-instance repair)))
         (task
          (make-instance 'learner-hearer-task
                         :owner agent
                         :label 'parse-interpret-and-align-task
                         :processes '(initial-process
                                      parse
                                      interpret
                                      determine-success
                                      align)
                         :diagnostics diagnostics
                         :repairs repairs))
         (all-task-results (object-run-task agent task)))
    ;; there should be only one result
    (setf (task-result agent) (first all-task-results))
    (find-data (task-result agent) 'success)))



;; ------------------------
;; + Learner Speaker Task +
;; ------------------------

(defclass learner-speaker-task (task-w-learning)
  () (:documentation "Task executed by the learner when it is the speaker"))

(defgeneric run-learner-speaker-task (agent)
  (:documentation "Entry point for the learner speaker task"))

(defmethod run-learner-speaker-task (agent)
  (let* ((diagnostics nil)
         (repairs nil)
         (task
          (make-instance 'learner-speaker-task
                         :owner agent
                         :label 'compose-and-produce-task
                         :processes '(initial-process
                                      produce)
                         :diagnostics diagnostics
                         :repairs repairs))
         (all-task-results (object-run-task agent task)))
    (setf (task-result agent) (first all-task-results))
    (first all-task-results)))




;; ---------------------
;; + Tutor Hearer Task +
;; ---------------------

(defclass tutor-hearer-task (task-w-learning)
  () (:documentation "Task executed by the tutor when it is the hearer"))

(defgeneric run-tutor-hearer-task (agent)
  (:documentation "Entry point for the tutor hearer task"))

(defmethod run-tutor-hearer-task (agent)
  (let* ((diagnostics nil)
         (repairs nil)
         (task
          (make-instance 'tutor-hearer-task
                         :owner agent
                         :label 'parse-and-interpret-task
                         :processes '(initial-process
                                      parse
                                      interpret)
                         :diagnostics diagnostics
                         :repairs repairs)))
    (let* ((all-task-results (object-run-task agent task))
           (task-result (first all-task-results)))
      (find-data task-result 'found-topic))))


;; --------------------------
;; + Learner Alignment Task +
;; --------------------------

(defclass learner-alignment-task (task-w-learning)
  () (:documentation "Task executed by the learner when it is the speaker"))

(defgeneric run-learner-alignment-task (agent learner-speaks-task-result gold-answer)
  (:documentation "Entry point for the learner alignment task"))

(defmethod run-learner-alignment-task (agent learner-speaks-task-result gold-answer)
  (let* ((diagnostics
          (loop for diagnostic in '(diagnose-speaker-alignment-result)
                collect (make-instance diagnostic)))
         (repairs
          (loop for repair in '(repair-holophrase->item-based-substitution
                                repair-holophrase->item-based-addition
                                repair-holophrase->item-based-deletion
                                repair-item-based+lexical->item-based
                                speaker-repair)
                collect (make-instance repair)))
         (task
          (make-instance 'learner-alignment-task
                         :owner agent
                         :label 'alignment-task
                         :processes '(initial-process
                                      receive-gold-answer
                                      determine-success
                                      speaker-align)
                         :diagnostics diagnostics
                         :repairs repairs)))
    (set-data task 'gold-answer gold-answer)
    (set-data task 'applied-cxns (find-data learner-speaks-task-result 'applied-cxns))
    (set-data task 'cipn (find-data learner-speaks-task-result 'cipn))
    (let* ((all-task-results (object-run-task agent task))
           (task-result (first all-task-results)))
      (find-data task-result 'success))))