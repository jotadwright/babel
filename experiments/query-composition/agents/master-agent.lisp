(in-package :qc)

(defclass master-agent ()
  ((questions :accessor questions
              :initarg :questions
              :initform '()
              :type string
              :documentation "List of tuple (Question, Query)")))

(defclass question ()
  ((question :accessor question
             :initarg :question
             :initform ""
             :type string
             :documentation "Question in natural language.")
   (query-associated :accessor query-associated 
       :initarg :query-associated
       :initform nil
       :type string
       :documentation "The query relative of the question in natural language")))

(defmethod initialize-instance :after ((agent master-agent) &key)
  (let ((questions (cl-csv:read-csv #P "./experiments/query-composition/question-generator/questions.csv"))
          (questions-obj '()))
    (dolist (question questions)
      (push (make-instance 'question :question (first question) :query-associated (car (last question))) questions-obj))
    (setf (questions agent) questions-obj)))


(defmethod get-question ((agent master-agent))
  (let ((question-index (random-between 0 (length (questions agent)))))
    (nth question-index (questions agent))))