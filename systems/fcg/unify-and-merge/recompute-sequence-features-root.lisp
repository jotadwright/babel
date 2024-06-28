(in-package :fcg)

(defun compute-matched-positions (pattern-sequence-predicates bindings)
  ""
  (let* ((list-of-matched-indices (flatten (mapcar #'(lambda (x) (when (equal (feature-name x) 'SEQUENCE) ;;why flatten??
                                                                   (rest (rest x))))
                                                   pattern-sequence-predicates)))
         (matched-positions (loop with matched-positions = nil
                                  for index in list-of-matched-indices
                                  for binding = (cdr (assoc index bindings :test #'string=))
                                  when binding
                                    do (push binding matched-positions)
                                  finally (return matched-positions))))
    
    (when matched-positions ;;only recalculate in comprehension
      (when (find-if #'variable-p matched-positions)
        (setf matched-positions (loop for position in matched-positions
                                      if (variable-p position)
                                        collect (lookup-binding position bindings) ;;lookup binding recursively
                                      else collect position)))
    
      (setf matched-positions (sort matched-positions #'<)))))

  
(defun recompute-root-sequence-features-based-on-bindings (pattern-sequence-predicates source-sequence-predicates bindings)
  "Makes new set of sequence predicates based on the indices that are present in the bindings."

  (let ((matched-positions (compute-matched-positions pattern-sequence-predicates bindings)))
   
    (when matched-positions
      (let* ((matched-intervals (loop for i from 1 to (- (length matched-positions) 1)
                                      for interval = (list (nth1 i matched-positions) (nth1 (+ i 1) matched-positions))
                                      do (setf i (+ i 1))
                                      collect interval))
             (non-matched-intervals (calculate-unmatched-intervals matched-intervals (mapcar #'(lambda (feat)
                                                                                                 (list (third feat) (fourth feat)))
                                                                                             source-sequence-predicates))))

        ;; Based on the non-matched intervals (e.g. '((0 4) (12 28))), create sequence new features to add to the root
        (when non-matched-intervals
          (loop for (feat-name string start end) in source-sequence-predicates ;;(sequence "what is the color of the cube?" 12 18)
                for offset = (abs (- 0 start))
                append (loop for (left right) in non-matched-intervals
                             for normalised-left = (- left offset)
                             for normalised-right = (- right offset)
                             if (overlapping-lr-pairs-p (list start end) (list left right))
                               collect (let ((unmatched-substring (subseq string normalised-left normalised-right)))
                                         `(,feat-name ,unmatched-substring ,left ,right))) into new-sequence-features
                finally (return (sort new-sequence-features #'< :key #'third))))))))

(defun lookup-binding (target-var bindings)
  "Recursively lookup binding of target-var in bindings."
  (let ((binding (cdr (assoc target-var bindings))))
    (if (variable-p binding)
      (lookup-binding binding bindings)
      binding)))

(defun calculate-unmatched-intervals (matched-intervals root-intervals)
  "Based on matched intervals and root intervals (before matching), calculate intervals in root that are unmatched and should stay in root."
  (loop with final-intervals = nil
        for root-interval in root-intervals
        for expanded-interval = (expand-interval root-interval)
        for car-equality-position = nil
        for cdr-equality-position = nil
        do (loop for matched-interval in matched-intervals ;; we set car-equality-position and cdr-equality-position
                 do (loop for i in expanded-interval
                          when (equal (car i) (car matched-interval))
                            do (setf car-equality-position (position i expanded-interval))
                          when (equal (cdr i) (first (cdr matched-interval)))
                            do (setf cdr-equality-position (position i expanded-interval)))
                 when (and car-equality-position cdr-equality-position)
                   do (let* ((excluded-positions (loop for i from car-equality-position to cdr-equality-position collect i))
                             (excluded-items (loop for i in excluded-positions collect (nth i expanded-interval))))
                        (loop for excluded-item in excluded-items
                              do (setf expanded-interval (remove excluded-item expanded-interval)))
                        (setf car-equality-position nil)
                        (setf cdr-equality-position nil)))
        when expanded-interval 
          do (loop with collapsed-intervals = (collapse-intervals expanded-interval)
                   for collapsed-interval in collapsed-intervals
                   do (pushend collapsed-interval final-intervals))
        finally (return final-intervals)))

(defun expand-interval (interval)
  "From a given interval expands it into a list of adjacent cons cells"
  (loop with interval-cons-cells = nil
        for i from (first interval) to (- (first (last interval)) 1)
        do (pushend (cons i (+ i 1)) interval-cons-cells)
        finally (return interval-cons-cells)))


;; (expand-interval '(0 30))
;; (expand-interval '(12 25))

(defun collapse-intervals (interval-cons-cells)
  "From a list of cons-cells, renders a list or multiple lists of adjacent cons cells"
  (let ((intervals '())
        (provisory-cons-cells-list '())
        (interval '()))
    (loop for i in interval-cons-cells
          for pos-i = (position i interval-cons-cells) ;; position of the current element
          for i+1 = nil ;; the next element, for now it is nil
          do (pushend i provisory-cons-cells-list)
             (if (not (eq i (first (last interval-cons-cells)))) ;; when we are not considering the last element of the list
               (progn
                 (setf i+1 (nth (+ pos-i 1) interval-cons-cells)) ;; we set the next-element
                 (when (not (= (cdr i) (car i+1))) ;; if the cdr of the current element is not equal to the car of the next element
                   ;; then we make an interval out of the previous considered cons cells that are stored in the provisory-cons-cells-list
                   (pushend (car (first provisory-cons-cells-list)) interval) ;; left elem of the interval
                   (pushend (cdr (first (last provisory-cons-cells-list))) interval) ;; right elem of the interval
                   (pushend interval intervals) ;; we push the interval to the list of intervals
                   (setf interval nil) ;; we set the interval to nil
                   (setf provisory-cons-cells-list nil))) ;; we also set the provisory-cons-cells-list to nil
               ;; and for the last element of the list:
               (progn
                 (pushend i provisory-cons-cells-list)
                 (pushend (car (first provisory-cons-cells-list)) interval) ;; left elem of the interval
                 (pushend (cdr (first (last provisory-cons-cells-list))) interval) ;; right elem of the interval
                 (pushend interval intervals)))) ;; we push the interval to the list of intervals
    intervals))




;; (collapse-intervals '((0 . 1) (1 . 2) (2 . 3) (3 . 4) (4 . 5) (5 . 6) (6 . 7) (7 . 8) (8 . 9) (9 . 10) (10 . 11) (11 . 12) (25 . 26) (26 . 27) (27 . 28) (28 . 29) (29 . 30)))
;; expected: '((0 12) (25 30))

;; (collapse-intervals '((12 . 13) (24 . 25)))
;; expected: '((12 13) (24 25))

;; (collapse-intervals '((7 . 8) (9 . 10)))


                   
;; using cons cells of the intervals
;; (calculate-unmatched-intervals '((8 9)) '((3 4) (7 10) (17 18)))
;; expected: '((3 4) (7 8) (9 10) (17 18))


;; (calculate-unmatched-intervals '((29 30) (0 4)) '((0 12) (17 30)))
;; expected: '((4 12) (17 29))

;; (calculate-unmatched-intervals '((0 4) (29 30)) '((0 12) (17 30)))
