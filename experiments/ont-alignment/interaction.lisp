(in-package :ont-alignment)

;-----------------------;
;functions to pick elems;
;--------from list------;

(defun pick-random-elem (list)
  "function to get a random element from a given list"
  (nth (random (length list)) list))

(defun get-nth-element (lst n)
  "Returns the nth element of list lst"
  (nth n lst))

;-----------------------;
;functions to parse json;
;-----------------------;

(defun read-json-data (json-file)
  "function to read data from json file"
  (let* ((file-stream (open json-file :if-does-not-exist nil))
         (json-data (when file-stream (json:decode-json file-stream))))
    (close file-stream)
    json-data))


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
    ;1-the tutor choses a random question-answer pair
    (setf (qa-pair tutor) (pick-random-elem (read-json-data "experiments/ont-alignment/data/question_answer_pairs.json")))
    (format t "The tutor is ~d and the learner is ~d.~%" tutor learner)
    (format t "The tutor chose the following question : ~d and gave it to the learner.~%" (first (qa-pair tutor)))
    ;2-the tutor asks the question to the learner and the learner checks whether it already knows the answer
    (if (not (find (first (qa-pair tutor)) (dictionary learner)))
    ;3-if it doesn't know the answer, the tutor reveals it and the learner stocks it
      ;(print (second (qa-pair tutor)))
      (push (add-to-dictionary (first (qa-pair tutor)) (second (qa-pair tutor)) learner) (dictionary learner)))
    ;4-the learner tries to reconstruct a query leading to the same answer
    ;first it connects to the desired db if it is not already the case
    (if (= (interaction-number interaction) 1)
      (connecting-to-db learner "db2" "db2_actors_films_simple_table.db"))
    ;5-the learner tries to reconstruct a query to get to the answer
    (try-queries-until-success (second (qa-pair tutor))) ;"SELECT value FROM actorsfilms WHERE value  = answer"
    ))