(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maximal string alignments ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric maximal-string-alignments (pattern source &key match mismatch gap)
  (:documentation "Computes the maximal alignments of two input strings using the Needleman-Wunsch algorithm"))

(defmethod maximal-string-alignments ((pattern string) (source string) &key (match 1) (mismatch -1) (gap -1))
  (maximal-string-alignments (coerce pattern 'list) (coerce source 'list)
                             :match match :mismatch mismatch :gap gap))

(defmethod maximal-string-alignments ((pattern list) (source list) &key (match 1) (mismatch -1) (gap -1))
  (let* ((nx (length pattern))
         (ny (length source))
         (scores (make-array (list (+ nx 1) (+ ny 1))))
         (pointers (make-array (list (+ nx 1) (+ ny 1)))))
    ;; Fill in the scores for the first row and the first column.
    (setf-matrix-column scores 0 (list->array (mapcar #'- (iota (abs (* gap (+ nx 1))) :step (abs gap)))))
    (setf-matrix-row scores 0 (list->array (mapcar #'- (iota (abs (* gap (+ ny 1))) :step (abs gap)))))

    ;; Calculate the score for each cell by looking at the left cell, the top cell
    ;; and the top-left cell and adding the appropriate score for match, mismatch
    ;; or gap.
    (loop for i from 0 below nx
          do (loop for j from 0 below ny
                   do (let* ((candidate-scores
                              (compute-candidate-scores pattern source i j scores
                                                        :match match :mismatch mismatch :gap gap))
                             (best-score (apply #'max (mapcar #'cdr candidate-scores)))
                             (origin (mapcar #'car (find-all best-score candidate-scores :key #'cdr :test #'=))))
                        (setf (aref scores (+ i 1) (+ j 1)) best-score)
                        (setf (aref pointers (+ i 1) (+ j 1)) origin))))

    ;; Trace back pointers from the bottom-right cell to the top-left cell.
    ;; Cells may contain multiple pointers, so there may be multiple paths.
    ;; Return all alignments with the highest score.
    (let ((alignments (extract-alignments pattern source pointers
                                          :match match :mismatch mismatch :gap gap)))
      (all-biggest #'score alignments))))

(defun compute-candidate-scores (pattern source i j scores &key (match 1) (mismatch -1) (gap -1))
  `((:top-left . ,(if (eql (nth i pattern) (nth j source))
                    (+ (aref scores i j) match)
                    (+ (aref scores i j) mismatch)))
    (:top . ,(+ (aref scores i (+ j 1)) gap))
    (:left . ,(+ (aref scores (+ i 1) j) gap))))

(defclass string-alignment-state ()
  ((aligned-pattern
    :initarg :aligned-pattern :accessor aligned-pattern :initform nil :type list)
   (aligned-source
    :initarg :aligned-source :accessor aligned-source :initform nil :type list)
   (i :initarg :i :accessor i :initform 0 :type number)
   (j :initarg :j :accessor j :initform 0 :type number)
   (score :initarg :score :accessor score :initform 0 :type number)))

(defun make-initial-string-alignment-state (i j)
  (make-instance 'string-alignment-state :i i :j j))

(defun extract-alignments (pattern source pointers &key (match 1) (mismatch -1) (gap -1))
  (loop with solutions = nil
        with queue = (list (make-initial-string-alignment-state
                            (length pattern) (length source)))
        while queue
        for state = (pop queue)
        do (with-slots (aligned-pattern aligned-source i j status score) state
             (if (null (aref pointers i j))
               (push state solutions)
               (loop for pointer in (aref pointers i j)
                     for next-state
                     = (cond ((eql pointer :top-left)
                              (let ((expanded-pattern (cons (nth (- i 1) pattern) aligned-pattern))
                                    (expanded-source (cons (nth (- j 1) source) aligned-source))
                                    (matchp (eql (nth (- i 1) pattern) (nth (- j 1) source))))
                                (make-instance 'string-alignment-state
                                               :aligned-pattern expanded-pattern
                                               :aligned-source expanded-source
                                               :i (- i 1) :j (- j 1)
                                               :score (+ score (if matchp match mismatch)))))
                             ((eql pointer :top)
                              (let ((expanded-pattern (cons (nth (- i 1) pattern) aligned-pattern))
                                    (expanded-source (cons #\_ aligned-source)))
                                (make-instance 'string-alignment-state
                                               :aligned-pattern expanded-pattern
                                               :aligned-source expanded-source
                                               :i (- i 1) :j j
                                               :score (+ score gap))))
                             ((eql pointer :left)
                              (let ((expanded-pattern (cons #\_ aligned-pattern))
                                    (expanded-source (cons (nth (- j 1) source) aligned-source)))
                                (make-instance 'string-alignment-state
                                               :aligned-pattern expanded-pattern
                                               :aligned-source expanded-source
                                               :i i :j (- j 1)
                                               :score (+ score gap)))))
                     do (push next-state queue))))
        finally (return (sort solutions #'> :key #'score))))

(defun print-string-alignments (string-alignment)
  (format t "~%~%~s" (coerce (aligned-pattern string-alignment) 'string))
  (format t "~%~s" (coerce (aligned-source string-alignment) 'string))
  (format t "~%Score: ~a" (score string-alignment)))

#|
;; do we want to avoid mismatches as much as possible?
 
(mapcar #'print-string-alignments
        (maximal-string-alignments "GATTACA" "GCATGCG" :mismatch -5))
(mapcar #'print-string-alignments
        (maximal-string-alignments "What color is the blue cube?"
                                   "What color is the red cube?" :mismatch -5))
|#


;;;;;;;;;;;;;;;;;;;;;;;;
;; Anti-unify strings ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun anti-unify-strings (pattern source)
  "Anti-unify strings by (1) using needleman-wunsch to compute the
   maximal alignments, (2) make generalisations of overlapping characters
   and place the rest in delta's. The results are converted back to strings
   and the anti-unification cost corresponds to the number of elements in
   the delta's. Solutions are returned sorted by cost."
  (let* ((maximal-alignments (maximal-string-alignments pattern source))
         (anti-unification-results
          (loop for alignment in maximal-alignments
                for (generalisation pattern-delta source-delta)
                  = (multiple-value-list
                     (anti-unify-aligned-strings (aligned-pattern alignment)
                                                 (aligned-source alignment)))
                collect (list (generalisation-chars->strings generalisation)
                              (loop for (var . chars) in pattern-delta
                                    collect (cons var (coerce chars 'string)))
                              (loop for (var . chars) in source-delta
                                    collect (cons var (coerce chars 'string)))
                              ;; cost = sum of length of delta's
                              (+ (length pattern-delta)
                                 (length source-delta))))))
    (sort anti-unification-results #'< :key #'fourth)))


(defun anti-unify-aligned-strings (pattern source)
  "Takes Needleman-Wunsch ALIGNED sequences and outputs a generalisation with bindings (deltas)"
  (let (generalisation pattern-delta source-delta
        pattern-delta-temp source-delta-temp flag)
    (loop for char-p in pattern
          for char-s in source
          do (cond ((and (eql char-p char-s) flag)
                    ;; same characters and flag is set
                    ;; -> reached the end of a diff
                    ;;    store the diff in the delta's
                    ;;    add a var to the generalisation
                    ;;    and clear the temp buffers
                    (let ((var (make-var)))       
                      (push var generalisation)    
                      (push (cons var (reverse pattern-delta-temp)) pattern-delta)                               
                      (push (cons var (reverse source-delta-temp)) source-delta)
                      (setf pattern-delta-temp nil
                            source-delta-temp nil
                            flag nil)
                      (unless (eql char-p #\_)                                      
                        (push char-p generalisation))))
                   ((eql char-p char-s)
                    ;; same characters
                    ;; -> add to the generalisation
                    (unless (eql char-p #\_)                                      
                      (push char-p generalisation)))
                   (t
                    ;; different characters
                    ;; -> reached the start of a diff
                    ;;    or still in a diff
                    ;;    set the flag and push characters
                    ;;    to temp buffers
                    (setf flag t)           
                    (unless (eql char-p #\_)                                    
                      (push char-p pattern-delta-temp))                              
                    (unless (eql char-s #\_)
                      (push char-s source-delta-temp))))
           finally
           ;; handle trailing characters in the temp buffers
           (when flag              
             (let ((var (make-var)))                                     
               (push var generalisation)
               (push (cons var (reverse pattern-delta-temp)) pattern-delta)                                
               (push (cons var (reverse source-delta-temp)) source-delta)
               (setf pattern-delta-temp nil
                     source-delta-temp nil
                     flag nil))))
    (values (reverse generalisation)  pattern-delta  source-delta)))

(defun generalisation-chars->strings (generalisation)
  (let (buffer result)
    (dolist (elem generalisation)
      (cond (; collect characters in the buffer
             (characterp elem)
             (push elem buffer))
            (; when reaching a variable
             (variable-p elem)
             ; turn the buffer into string
             ; and push to the result
             (when buffer
               (push (coerce (reverse buffer) 'string) result))
             (setf buffer nil)
             (push elem result))))
    (when buffer
      (push (coerce (reverse buffer) 'string) result))
    (reverse result)))


;; TO DO; add keyword arguments for setting the match/mismatch/gap scores

;(anti-unify-strings "GCATGCG" "GATTACA")
;(anti-unify-strings "What size is the cube?" "What size is the red cube?")
;(anti-unify-strings "What is the color of the sphere?" "What is the size of the cube?")



;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequence predicates ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-anti-unification-result->sequence-predicates (generalisation pattern-delta source-delta)
  "Convert the string anti-unification result to sequence predicates.
   Left and right boundaries are set such that the combination of the
   pattern/source delta and the generalisation is identical to the
   original pattern/source."
  (let (generalisation-predicates
        pattern-delta-predicates
        source-delta-predicates
        prev-was-variable-p)
    (dolist (elem generalisation)
      (cond ((stringp elem)
             (if prev-was-variable-p
               (let ((left-boundary (fourth (first pattern-delta-predicates))))
                 (push `(sequence ,elem ,left-boundary ,(make-var 'rb))
                       generalisation-predicates)
                 (setf prev-was-variable-p nil))
               (push `(sequence ,elem ,(make-var 'lb) ,(make-var 'rb))
                     generalisation-predicates)))
            ((variable-p elem)
             (let ((left-boundary (or (fourth (first generalisation-predicates))
                                      (make-var 'lb)))
                   (right-boundary (make-var 'rb))
                   (pattern-string (rest (assoc elem pattern-delta)))
                   (source-string (rest (assoc elem source-delta))))
               (push `(sequence ,pattern-string ,left-boundary ,right-boundary)
                     pattern-delta-predicates)
               (push `(sequence ,source-string ,left-boundary ,right-boundary)
                     source-delta-predicates)
               (setf prev-was-variable-p t)))))
    (values (reverse generalisation-predicates)
            (reverse pattern-delta-predicates)
            (reverse source-delta-predicates))))


#|

(multiple-value-bind (gen pd sd cost)
    (anti-unify-strings "What is the color of the sphere?" "What is the size of the cube?")
  (string-anti-unification-result->sequence-predicates gen pd sd))

"What is the color of the sphere?"
"What is the size of the cube?"
=> Generalisation: ((SEQUENCE "What is the " #:?LB-18 #:?RB-65) (SEQUENCE " of the " #:?RB-66 #:?RB-67) (SEQUENCE "e?" #:?RB-68 #:?RB-69)),
   Pattern delta: ((SEQUENCE "color" #:?RB-65 #:?RB-66) (SEQUENCE "spher" #:?RB-67 #:?RB-68)),
   Source delta: ((SEQUENCE "size" #:?RB-65 #:?RB-66) (SEQUENCE "cub" #:?RB-67 #:?RB-68))

|#
