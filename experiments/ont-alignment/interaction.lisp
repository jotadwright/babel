(in-package :ont-alignment)

;-----------------------;
;functions to get random;
;-------elements--------;

(defun random-n-elems (n lst)
  "function that gets n random and diverse elements out of a list without taking into account the nil element"
  (let* ((non-nil-elems (remove nil lst))
        (duo (loop repeat (min n (length non-nil-elems))
                   collect (pop non-nil-elems))))
    duo))

(defun pick-random-elem (list)
  "function to get a random element from a given list"
  (nth (random (length list)) list))

(defun read-json-data (json-file)
  "function to read data from json file"
  (let* ((file-stream (open json-file :if-does-not-exist nil))
         (json-data (when file-stream (json:decode-json file-stream))))
    (close file-stream)
    json-data))


;-----------------------;
;core of the experiment:;
;----the interaction----;

(defun create-interaction ()
  "main function of the experiment : create an interaction between two agents chosen randomly out the population"
  (let* ((agent-duo (random-n-elems 2 (list "agent-3" "agent-4" "agent-5")))
         (tutor (car agent-duo))
         (learner (cadr agent-duo))
         (question-answer-pair (pick-random-elem (read-json-data "experiments/ont-alignment/data/question_answer_pairs.json"))))
    (format nil "The tutor is ~d and the learner is ~d." tutor learner)
    (format nil "The tutor chose the following question : ~d and gave it to the learner." (car question-answer-pair))))
