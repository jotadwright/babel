(in-package :cle)

;; ---------------------------
;; + Web monitor experiments +
;; ---------------------------

(defun read-scene-ids (fname)
  (let* ((base-dir "~/Projects/babel/experiments/concept-emergence2/data/")
         (fpath (concatenate 'string base-dir fname))
         (raw (uiop:read-file-lines fpath))
         (scene-ids (map 'list #'parse-integer raw)))
    scene-ids))

(defun first-n (n list)
  "Returns the first N elements of the LIST."
  (butlast list (max (- (list-length list) n) 0)))

;; ---------------------
;; + Utility fucntions +
;; ---------------------

(defun find-scenes-with-size (context-size)
  (let* ((world (make-instance 'clevr-world :data-sets (list "t-val")))
         (scenes (all-scenes world))
         (filtered-scenes (loop for scene in scenes
                                if (length= (objects scene) context-size)
                                  collect scene)))
    filtered-scenes))

(defun find-scenes-with-discriminative-topics (scenes feature-channels)
    (loop for symbolic-scene in scenes
          for ecl-context = (clevr->simulated symbolic-scene
                                              feature-channels)
          for candidate-topics = (loop for candidate in (objects ecl-context) and idx from 0
                                       for other-objects = (remove candidate (objects ecl-context))
                                       when (is-discriminative candidate other-objects)
                                         collect (cons idx (get-symbolic-discriminative-feature candidate ecl-context)))
          if candidate-topics
            collect (cons (index symbolic-scene) candidate-topics)))

#|
 
(setf all-scenes (find-scenes-with-size 3))
(length all-scenes)

(setf res (find-scenes-with-discriminative-topics all-scenes (list 'color 'area 'roughness)))
(length res)
(setf scene-ids (loop for (scene-id . candidate-topics) in res collect scene-id))

(setf test (loop for (scene-id . candidate-topics) in res
      for scenes = (loop for candidate-topic in candidate-topics
                         collect (cons scene-id candidate-topic))
      append scenes))

(defun dupes (lst)
  (cond ((null lst) '())
        ((member (car lst) (cdr lst)) (cons (car lst) (dupes (cdr lst))))
        (t (dupes (cdr lst)))))

(dupes (loop for scene in res
             collect (first scene)))

(setf all-scenes (find-scenes-with-size 5))
          if (length> candidate-topics 0)
              do (loop for ecl-topic in candidate-topics
                       for types = (get-symbolic-discriminative-feature ecl-topic ecl-context)
                               
            do (let ((ecl-topic (random-elt candidate-topics)))
                 (set-data interaction 'attribute-type (get-symbolic-discriminative-feature ecl-topic ecl-context))
                 (loop for agent in (interacting-agents experiment)
                       do (set-data agent 'topic ecl-topic))))))|#
            
          