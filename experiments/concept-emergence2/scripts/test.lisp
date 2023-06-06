;; test

(defun similarity (weights-c1 weights-c2 distances &key &allow-other-keys)
  (loop with concept1-weight-sum = (loop for weight in weights-c1 sum weight)
        with concept2-weight-sum = (loop for weight in weights-c2 sum weight)
        for w1 in weights-c1
        for w2 in weights-c2
        for sim in distances
        for avg-weight = (/ (+ (/ w1 concept1-weight-sum)
                               (/ w2 concept2-weight-sum))
                            2)
        for prototype-similarity = sim
        for sim-score = (* avg-weight prototype-similarity)
        sum sim-score))

(defun similarity (weights-c1 weights-c2 distances &key &allow-other-keys)
  (loop for w1 in weights-c1
        for w2 in weights-c2
        for sim in distances
        for avg-weight = (/ (/ (+ w1 w2) 2) (length distances))
        for prototype-similarity = sim
        for sim-score = (* avg-weight prototype-similarity)
        sum sim-score))

(defun determine-similarity-with-weights (weight1 weight2 prototype-similarity)
  "Euclidian distance to max-similarity."
  (let* ((avg-weights (/ (+ weight1 weight2) 2)) ;; (w1 + w2)/2
         (weight-similarity (- 1 (abs (- weight1 weight2))))) ; (1 - |w1 - w2|)
    ;; distance between our scores and max score (1,1),
    ;; then normalize,
    ;; and subtract from 1 to get again a sim score between 0 and 1
    (- 1
       (/ (euclidian-distance (list avg-weights weight-similarity prototype-similarity)
                              (list 1 1 1))
          (euclidian-distance (list 1 1 0)
                              (list 1 1 1))))))

(defun euclidian-distance (point1 point2)
  "Euclidian distance between two points in n-dimensional space."
  (loop with sum = 0
        for idx from 0 to (- (length point1) 1)
        do (setf sum (+ sum (expt (- (nth idx point2) (nth idx point1)) 2)))
        finally (return (sqrt sum))))


(determine-similarity-with-weights 0 0 0)



(setf test-cases (list
                  (list
                   ;       R G B
                     (list 0 0 0) ;; weights concept 1
                     (list 0 0 0) ;; weights concept 2
                     (list 0 0 0) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 0 0 0) ;; weights concept 1
                     (list 1 1 1) ;; weights concept 2
                     (list 0 0 0) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 1 1 1) ;; weights concept 1
                     (list 0 0 0) ;; weights concept 2
                     (list 0 0 0) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 1 1 1) ;; weights concept 1
                     (list 1 1 1) ;; weights concept 2
                     (list 0 0 0) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 0 0 0) ;; weights concept 1
                     (list 0 0 0) ;; weights concept 2
                     (list 1 1 1) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 0 0 0) ;; weights concept 1
                     (list 1 1 1) ;; weights concept 2
                     (list 1 1 1) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 1 1 1) ;; weights concept 1
                     (list 0 0 0) ;; weights concept 2
                     (list 1 1 1) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 1 1 1) ;; weights concept 1
                     (list 1 1 1) ;; weights concept 2
                     (list 1 1 1) ;; sim between c1 and c2 in channel of column
                   )
                  
                  ))



(loop for test in test-cases
      for weights-c1 = (nth 0 test)
      for weights-c2 = (nth 1 test)
      for distances = (nth 2 test)
      collect (similarity weights-c1 weights-c2 distances))

(defun add-small-var (lst)
  (loop for el in lst
        for new-el = (if (< el 0.5)
                       (+ el 0.01)
                       (- el 0.01))
        collect new-el))

(loop for test in test-cases
      for weights-c1 = (nth 0 test)
      for weights-c2 = (nth 1 test)
      for distances = (nth 2 test)
      collect (similarity (add-small-var weights-c1)
                          (add-small-var weights-c2)
                          (add-small-var distances)))


;;;;;

(setf test-cases (list
                  (list
                   ;       R G B
                     (list 0 0 0) ;; weights concept 1
                     (list 0 0 0) ;; weights concept 2
                     (list 0 0 0) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 0 0 0) ;; weights concept 1
                     (list 1 1 1) ;; weights concept 2
                     (list 0 0 0) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 1 1 1) ;; weights concept 1
                     (list 1 1 1) ;; weights concept 2
                     (list 0 0 0) ;; sim between c1 and c2 in channel of column
                   )
                  (list
                   ;       R G B
                     (list 1 1 1) ;; weights concept 1
                     (list 1 1 1) ;; weights concept 2
                     (list 1 1 1) ;; sim between c1 and c2 in channel of column
                   )
                  
                  ))

(loop for test in test-cases
      for weights-c1 = (nth 0 test)
      for weights-c2 = (nth 1 test)
      for distances = (nth 2 test)
      collect (similarity weights-c1 weights-c2 distances))
