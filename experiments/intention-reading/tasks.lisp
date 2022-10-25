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
  (let* ((task
          (make-instance 'learner-hearer-task
                         :owner agent
                         :label 'parse-interpret-and-align-task
                         :processes '(initial-process
                                      parse-and-interpret
                                      determine-success
                                      hearer-align)
                         :diagnostics nil
                         :repairs nil))
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
  (let* ((task
          (make-instance 'learner-speaker-task
                         :owner agent
                         :label 'compose-and-produce-task
                         :processes '(initial-process
                                      produce)
                         :diagnostics nil
                         :repairs nil))
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
  (let* ((task
          (make-instance 'tutor-hearer-task
                         :owner agent
                         :label 'parse-and-interpret-task
                         :processes '(initial-process
                                      parse
                                      interpret)
                         :diagnostics nil
                         :repairs nil)))
    (let* ((all-task-results (object-run-task agent task))
           (task-result (first all-task-results)))
      (find-data task-result 'computed-topic))))


;; --------------------------
;; + Learner Alignment Task +
;; --------------------------

(defclass learner-alignment-task (task-w-learning)
  () (:documentation "Task executed by the learner when it is the speaker"))

(defgeneric run-learner-alignment-task (agent learner-speaks-task-result gold-answer)
  (:documentation "Entry point for the learner alignment task"))

(defmethod run-learner-alignment-task (agent learner-speaks-task-result gold-answer)
  (let* ((task
          (make-instance 'learner-alignment-task
                         :owner agent
                         :label 'alignment-task
                         :processes '(initial-process
                                      receive-gold-answer
                                      determine-success
                                      speaker-align)
                         :diagnostics nil
                         :repairs nil)))
    (set-data task 'gold-answer gold-answer)
    (set-data task 'applied-cxns (find-data learner-speaks-task-result 'applied-cxns))
    (set-data task 'cipn (find-data learner-speaks-task-result 'cipn))
    (set-data task 'irl-program (find-data learner-speaks-task-result 'irl-program))
    (let* ((all-task-results (object-run-task agent task))
           (task-result (first all-task-results)))
      (find-data task-result 'success))))