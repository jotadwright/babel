(in-package :ont-alignment)

;-----------------------;
;core of the experiment:;
;----the interaction----;

(defclass ont-alignment-experiment (experiment)
  ())

(defmethod initialize-instance :after ((experiment ont-alignment-experiment) &key)
  "creates new instances"
  (setf (agents experiment) (make-ont-agents 3 experiment)))

(defmethod interact ((experiment ont-alignment-experiment) (interaction interaction) &key)
  "main function of the experiment : create an interaction between two agents chosen randomly out the population"
  (let* ((interacting-agents (interacting-agents interaction))
         (tutor (first interacting-agents))
         (learner (second interacting-agents)))
    ;1-the learner connects to the desired db if it is not already the case
    (if (= (interaction-number interaction) 1)
      (connecting-to-db learner "db2" "db2_actors_films_simple_table.db"))
    ;2-the tutor chooses a random question-answer pair
    (setf (qa-pair tutor) (pick-random-elem (read-json-data "experiments/ont-alignment/data/question_answer_pairs.json")))
    (format t "The tutor is ~d and the learner is ~d.~%" tutor learner)
    (format t "The tutor chose the following question : ~d and gave it to the learner.~%" (first (qa-pair tutor)))
    ;3-the tutor asks the question to the learner and the learner checks whether it already knows the answer
    (if (not (find (first (qa-pair tutor)) (dictionary learner)))
    ;4-if it doesn't know the answer, the tutor reveals it and the learner stocks it
      (if (> (length (cdr (qa-pair tutor))) 1)
        (progn (push (add-to-dictionary (first (qa-pair tutor)) (cdr (qa-pair tutor)) learner) (dictionary learner))
          (format t "The answer is a ~d.~%" (type-of (cdr (qa-pair tutor))))
          (try-queries-until-success (cdr (qa-pair tutor))))
        (progn (push (add-to-dictionary (first (qa-pair tutor)) (second (qa-pair tutor)) learner) (dictionary learner))
          (format t "The answer is ~d.~%" (type-of (second (qa-pair tutor))))
          (try-queries-until-success (second (qa-pair tutor))))))))
    ;5-the learner tries to reconstruct a query to get to the answer
     ;"SELECT value FROM actorsfilms WHERE value  = answer"