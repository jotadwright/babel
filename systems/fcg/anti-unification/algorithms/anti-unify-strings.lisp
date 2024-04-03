(in-package :fcg)

(export '(anti-unify-sequences
          anti-unify-strings))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Anti-unify strings ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun anti-unify-sequences (pattern source)
  "Anti-unify sets of sequence predicates.
   1. Render all pattern sequences
   2. Render all source sequences
   3. Anti-unify all combinations of pattern-strings and source-strings
   4. Remove duplicate results
   5. Transform the result back to sequence predicates"
  ;; to do: improve anti-unify sequences to have full reversibility, up to the original variables.
  (multiple-value-bind (pattern-renders all-pattern-boundaries) (render-all pattern :render-sequences)
    (multiple-value-bind (source-renders all-source-boundaries) (render-all source :render-sequences)  
      (let* ((all-anti-unification-results
              (loop for pattern-render in pattern-renders
                    for pattern-boundaries in all-pattern-boundaries
                    for pattern-string = (list-of-strings->string pattern-render :separator "_")
                    append (loop for source-render in source-renders
                                 for source-string = (list-of-strings->string source-render :separator "_")
                                 for source-boundaries in all-source-boundaries 
                                 append (loop with possible-alignments = (maximal-sequence-alignments pattern-string source-string pattern-boundaries source-boundaries)
                                              for alignment in possible-alignments
                                              for pattern-in-alignment = (aligned-pattern alignment)
                                              for source-in-alignment = (aligned-source alignment)
                                              for pattern-boundaries = (aligned-pattern-boundaries alignment)
                                              for source-boundaries = (aligned-source-boundaries alignment)
                                              collect (multiple-value-bind (resulting-generalisation
                                                                            resulting-pattern-delta
                                                                            resulting-source-delta
                                                                            resulting-pattern-bindings
                                                                            resulting-source-bindings)
                                                          (anti-unify-aligned-sequences pattern-in-alignment source-in-alignment pattern-boundaries source-boundaries)
                                                        (let ((au-result (make-instance 'sequences-au-result
                                                                                        :pattern pattern
                                                                                        :source source
                                                                                        :generalisation (merge-adjacent-sequence-predicates resulting-generalisation)
                                                                                        :pattern-delta (merge-adjacent-sequence-predicates resulting-pattern-delta)
                                                                                        :source-delta (merge-adjacent-sequence-predicates resulting-source-delta)
                                                                                        :pattern-bindings (remove-bindings-not-in-generalisation  resulting-pattern-bindings (merge-adjacent-sequence-predicates resulting-generalisation))
                                                                                        :source-bindings (remove-bindings-not-in-generalisation resulting-source-bindings (merge-adjacent-sequence-predicates resulting-generalisation))
                                                                                        )))
                                                          (setf (cost au-result) (anti-unification-cost au-result))
                                                          au-result))))))
             (unique-sorted-results
              (sort all-anti-unification-results #'< :key #'cost)))
        unique-sorted-results))))



;;(print-anti-unification-results (anti-unify-sequences '((sequence "what size is the cube" ?l1 ?r1)) '((sequence "what color is the cube" ?l3 ?r3))))

;;(print-anti-unification-results (anti-unify-sequences '((sequence "onelittle" ?l1 ?r1)) '((sequence "twolittle" ?l3 ?r3))))

;;(print-anti-unification-results (anti-unify-sequences '((sequence "the red cube" ?l1 ?r1)) '((sequence "the blue cube" ?l2 ?r2))))

;;(print-anti-unification-results (anti-unify-sequences '((sequence "the cube" ?l1 ?r1)) '((sequence "the red cube" ?l2 ?r2))))

;;(print-anti-unification-results (anti-unify-sequences '((sequence "the red cube" ?l1 ?r1)) '((sequence "the cube" ?l2 ?r2))))

;;(print-anti-unification-results (anti-unify-sequences '((sequence "ABA" ?l1 ?r1)) '((sequence "A" ?l2 ?r2))))

;;(print-anti-unification-results (anti-unify-sequences '((sequence "A" ?l1 ?r1) (sequence "BA" ?l2 ?r2)) '((sequence "A" ?l3 ?r3))))

;;(print-anti-unification-results (anti-unify-sequences '((sequence "A" ?l1 ?r1) (sequence "BA" ?l2 ?r2)) '((sequence "AB" ?l3 ?r3))))

;; (print-anti-unification-results (anti-unify-sequences '((sequence "AA" ?l1 ?r1)) '((sequence "ABA" ?l2 ?r2))))

;; what do we want from reversibility check in this case? 
;; (print-anti-unification-results (anti-unify-sequences '((sequence "the " ?l1 ?r1) (sequence "orange" ?r1 ?l2) (sequence " cube" ?l2 ?r2)) '((sequence "the yellow cube" ?l3 ?r3))))


;;(print-anti-unification-results (anti-unify-sequences '((sequence "what size is the block?" ?l6 ?r6)) '((sequence "what size is the sphere?" ?l7 ?r7))))
;;(print-anti-unification-results (anti-unify-sequences '((sequence "what size is the cube?" ?l6 ?r6)) '((sequence "what color is the cube?" ?l7 ?r7))))


(defun anti-unify-strings (pattern source &key to-sequence-predicates-p)
  "Anti-unify strings by (1) using needleman-wunsch to compute the
   maximal alignments, (2) make generalisations of overlapping characters
   and place the rest in delta's. The results are converted back to strings
   and the anti-unification cost corresponds to the number of elements in
   the delta's. Solutions are returned sorted by cost."
  (let* ((all-anti-unification-results 
          (loop with possible-alignments = (maximal-string-alignments pattern source)
                for alignment in possible-alignments
                for pattern-in-alignment = (aligned-pattern alignment)
                for source-in-alignment = (aligned-source alignment)
                collect (multiple-value-bind (resulting-generalisation
                                              resulting-pattern-delta
                                              resulting-source-delta)
                            (anti-unify-aligned-strings pattern-in-alignment source-in-alignment)
                          (let ((au-result (make-instance 'string-au-result
                                                          :pattern pattern
                                                          :source source
                                                          :generalisation (generalisation-chars->strings resulting-generalisation)
                                                          :pattern-delta (loop for (var . chars) in resulting-pattern-delta
                                                                               collect (cons var (coerce chars 'string)))
                                                          :source-delta (loop for (var . chars) in resulting-source-delta
                                                                              collect (cons var (coerce chars 'string))))))
                            (setf (cost au-result) (anti-unification-cost au-result))
                            au-result))))
         (unique-sorted-results
          (sort (remove-duplicates all-anti-unification-results
                                   :test #'duplicate-string-anti-unification-results)
                #'< :key #'cost)))
    (if to-sequence-predicates-p
      (mapcar #'string-anti-unification-result->sequence-predicates unique-sorted-results)
      unique-sorted-results)))


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
                    (let ((var (make-var (next-au-var))))       
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
             (let ((var (make-var (next-au-var))))                                     
               (push var generalisation)
               (push (cons var (reverse pattern-delta-temp)) pattern-delta)                                
               (push (cons var (reverse source-delta-temp)) source-delta)
               (setf pattern-delta-temp nil
                     source-delta-temp nil
                     flag nil))))
    (values (reverse generalisation)
            pattern-delta
            source-delta)))

(defun anti-unify-aligned-sequences (pattern source pattern-boundaries source-boundaries)
  "Takes Needleman-Wunsch ALIGNED sequences and outputs a generalisation with bindings (deltas)"
  (let (generalisation pattern-delta source-delta
        pattern-bindings source-bindings)
    (loop for pattern-char in pattern
          for source-char in source
          for pattern-bounds in pattern-boundaries
          for source-bounds in source-boundaries
          do (if (eql pattern-char source-char)
               ;; same chars, put them in generalisation, rename variables
               (let* ((renamed-left-gen-boundary (rename-boundary (car pattern-bounds) (car source-bounds) pattern-bindings source-bindings))
                      (renamed-right-gen-boundary (rename-boundary (cdr pattern-bounds) (cdr source-bounds) pattern-bindings source-bindings))
                      (gen `(sequence ,(mkstr pattern-char) ,renamed-left-gen-boundary ,renamed-right-gen-boundary)))
                 (if (not (equal pattern-char #\_)) (push gen generalisation))
                 (setf pattern-bindings
                       (adjoin (cons (car pattern-bounds) renamed-left-gen-boundary) pattern-bindings :test 'equal))
                 (setf pattern-bindings
                       (adjoin (cons (cdr pattern-bounds) renamed-right-gen-boundary) pattern-bindings :test 'equal))
                 (setf source-bindings
                       (adjoin (cons (car source-bounds) renamed-left-gen-boundary) source-bindings :test 'equal))
                 (setf source-bindings
                       (adjoin (cons (cdr source-bounds) renamed-right-gen-boundary) source-bindings :test 'equal)))
               ;; different characters, push to delta, only if char is not #'_
               (let ((pattern-delta-pred `(sequence ,(mkstr pattern-char) ,(car pattern-bounds) ,(cdr pattern-bounds)))
                     (source-delta-pred `(sequence ,(mkstr source-char) ,(car source-bounds) ,(cdr source-bounds))))
                 (if (not (equal pattern-char #\_)) (push pattern-delta-pred pattern-delta))
                 (if (not (equal source-char #\_)) (push source-delta-pred source-delta)))))
    (setf pattern-bindings (loop for binding in pattern-bindings if (car binding) collect binding))
    (setf source-bindings (loop for binding in source-bindings if (car binding) collect binding))
    (values (reverse generalisation)
            (reverse pattern-delta)
            (reverse source-delta)
            (reverse pattern-bindings)
            (reverse source-bindings))))

(defun rename-boundary (pattern-boundary source-boundary pattern-bindings source-bindings)
  (let ((gen-var-pattern (cdr (assoc pattern-boundary pattern-bindings)))
        (gen-var-source (cdr (assoc source-boundary source-bindings))))
  (if (and gen-var-pattern gen-var-source
       (equal
        gen-var-pattern
        gen-var-source))
    (cdr (assoc pattern-boundary pattern-bindings))
    (make-var 'gb))))
  

(defun duplicate-string-anti-unification-results (au-1 au-2)
  (and (loop for (nil . pd-1) in (pattern-delta au-1)
             for (nil . pd-2) in (pattern-delta au-2)
             always (string= pd-1 pd-2))
       (loop for (nil . sd-1) in (source-delta au-1)
             for (nil . sd-2) in (source-delta au-2)
             always (string= sd-1 sd-2))))


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


                 
;(print-anti-unification-results (anti-unify-strings "GCATGCG" "GATTACA" :to-sequence-predicates-p t))
;(print-anti-unification-results (anti-unify-strings "What size is the cube?" "What size is the red cube?" :to-sequence-predicates-p t))
;(print-anti-unification-results (anti-unify-strings "What size is the blue cube?" "What size is the red cube?"))
;(anti-unify-strings "What is the color of the sphere?" "What is the size of the cube?")

#|
(length
 (remove-if #'(lambda (x) (< (score x) 0))
            (maximal-string-alignments "The tiny shiny cylinder has what color?"
                                       "What is the material of the big purple object?")))
(length
 (anti-unify-strings "The tiny shiny cylinder has what color?"
                     "What is the material of the big purple object?"))

=> 3196800 possible alignments 
=> 0 possible alignments with positive score
|#


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
    ;; Fill in the pointers for the first row and the first column.
    (setf-matrix-column pointers 0 (make-array (list (+ nx 1)) :initial-element '(:top)))
    (setf-matrix-row pointers 0 (make-array (list (+ ny 1)) :initial-element '(:left)))
    (setf (aref pointers 0 0) nil)

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


(defgeneric maximal-sequence-alignments (pattern source pattern-boundaries source-boundaries &key match mismatch gap)
  (:documentation "Computes the maximal alignments of two input strings using the Needleman-Wunsch algorithm"))


(defmethod maximal-sequence-alignments ((pattern string) (source string) (pattern-boundaries list) (source-boundaries list) &key (match 1) (mismatch -1) (gap -1))
  (maximal-sequence-alignments (coerce pattern 'list) (coerce source 'list) pattern-boundaries source-boundaries
                             :match match :mismatch mismatch :gap gap))


(defmethod maximal-sequence-alignments ((pattern list) (source list) (pattern-boundaries list) (source-boundaries list) &key (match 1) (mismatch -1) (gap -1))
  (let* ((nx (length pattern))
         (ny (length source))
         (scores (make-array (list (+ nx 1) (+ ny 1))))
         (pointers (make-array (list (+ nx 1) (+ ny 1)))))
    ;; Fill in the scores for the first row and the first column.
    (setf-matrix-column scores 0 (list->array (mapcar #'- (iota (abs (* gap (+ nx 1))) :step (abs gap)))))
    (setf-matrix-row scores 0 (list->array (mapcar #'- (iota (abs (* gap (+ ny 1))) :step (abs gap)))))
    ;; Fill in the pointers for the first row and the first column.
    (setf-matrix-column pointers 0 (make-array (list (+ nx 1)) :initial-element '(:top)))
    (setf-matrix-row pointers 0 (make-array (list (+ ny 1)) :initial-element '(:left)))
    (setf (aref pointers 0 0) nil)

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
    (let ((alignments (extract-sequence-alignments pattern source pointers pattern-boundaries source-boundaries
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
   (aligned-pattern-boundaries
    :initarg :aligned-pattern-boundaries :accessor aligned-pattern-boundaries :initform nil :type list)
   (aligned-source-boundaries
    :initarg :aligned-source-boundaries :accessor aligned-source-boundaries :initform nil :type list)
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
        do (with-slots (aligned-pattern aligned-source i j score) state
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


(defun extract-sequence-alignments (pattern source pointers pattern-boundaries source-boundaries &key (match 1) (mismatch -1) (gap -1))
  (loop with solutions = nil
        with queue = (list (make-initial-string-alignment-state
                            (length pattern) (length source)))
        while queue
        for state = (pop queue)
        do (with-slots (aligned-pattern aligned-source aligned-pattern-boundaries aligned-source-boundaries i j score) state
             (if (null (aref pointers i j))
               (push state solutions)
               (loop for pointer in (aref pointers i j)
                     for next-state
                     = (cond ((eql pointer :top-left)
                              (let* ((expanded-pattern (cons (nth (- i 1) pattern) aligned-pattern))
                                     (expanded-source (cons (nth (- j 1) source) aligned-source))
                                     (matchp (eql (nth (- i 1) pattern) (nth (- j 1) source)))
                                     (current-left-source-boundary (car (first aligned-source-boundaries)))
                                     (current-left-pattern-boundary (car (first aligned-pattern-boundaries)))
                                     (source-boundary-vars (make-boundary-vars j source-boundaries current-left-source-boundary) )
                                     (pattern-boundary-vars (make-boundary-vars i pattern-boundaries current-left-pattern-boundary)))
                                (make-instance 'string-alignment-state
                                               :aligned-pattern expanded-pattern
                                               :aligned-source expanded-source
                                               :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                               :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                               :i (- i 1) :j (- j 1)
                                               :score (+ score (if matchp match mismatch)))))
                             ((eql pointer :top)
                              (let* ((expanded-pattern (cons (nth (- i 1) pattern) aligned-pattern))
                                     (expanded-source (cons #\_ aligned-source))
                                     (current-left-source-boundary (car (first aligned-source-boundaries)))
                                     (current-left-pattern-boundary (car (first aligned-pattern-boundaries)))
                                     (source-boundary-vars (make-boundary-vars nil source-boundaries current-left-source-boundary :gap t))
                                     (pattern-boundary-vars (make-boundary-vars i pattern-boundaries current-left-pattern-boundary)))
                                (make-instance 'string-alignment-state
                                               :aligned-pattern expanded-pattern
                                               :aligned-source expanded-source
                                               :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                               :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                               :i (- i 1) :j j
                                               :score (+ score gap))))
                             ((eql pointer :left)
                              (let* ((expanded-pattern (cons #\_ aligned-pattern))
                                     (expanded-source (cons (nth (- j 1) source) aligned-source))
                                     (current-left-source-boundary (car (first aligned-source-boundaries)))
                                     (current-left-pattern-boundary (car (first aligned-pattern-boundaries)))
                                     (source-boundary-vars (make-boundary-vars j source-boundaries current-left-source-boundary))
                                     (pattern-boundary-vars (make-boundary-vars nil pattern-boundaries current-left-pattern-boundary :gap t)))
                                (make-instance 'string-alignment-state
                                               :aligned-pattern expanded-pattern
                                               :aligned-source expanded-source
                                               :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                               :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                               :i i :j (- j 1)
                                               :score (+ score gap)))))
                     do (push next-state queue))))
        finally (return (sort solutions #'> :key #'score))))

;;;;;;;;;;;
;; UTILS ;;
;;;;;;;;;;;

(defun remove-bindings-not-in-generalisation (bindings generalisation-list)
  (let ((generalisation-vars (loop for seq in generalisation-list
                                   append (list (third seq) (fourth seq)))))
  (loop for binding in bindings
          if (find (cdr binding) generalisation-vars)
          collect binding)))

(defun make-boundary-vars (position boundaries current-left &key (gap nil))
  (let ((right-boundary-var (if position  (car (rassoc position  boundaries))))
        (left-boundary-var (if position (car (rassoc (- position 1) boundaries)))))
    (cond ((and (not right-boundary-var) (not current-left))
           (setf right-boundary-var (make-var 'rb)))
          ((and (not right-boundary-var) current-left)
           (setf right-boundary-var current-left)))
    (if (not left-boundary-var)
      (setf left-boundary-var (if gap current-left (make-var 'lb))))
    (cons left-boundary-var right-boundary-var)))

     
(defun print-string-alignments (string-alignment)
  (format t "~%~%~s" (coerce (aligned-pattern string-alignment) 'string))
  (format t "~%~s" (coerce (aligned-source string-alignment) 'string))
  (format t "~%Score: ~a" (score string-alignment)))


                 
#| 
(mapcar #'print-string-alignments
        (maximal-string-alignments "GATTACA" "GCATGCG"))
(mapcar #'print-string-alignments
        (maximal-string-alignments "What size is the cube?"
                                   "What size is the red cube?"))
(mapcar #'print-string-alignments
        (maximal-string-alignments "What size is the blue cube?"
                                   "What size is the red cube?"))
(mapcar #'print-string-alignments
        (maximal-string-alignments "What is the color of the sphere?"
                                   "What is the size of the cube?"))

(mapcar #'print-string-alignments
        (maximal-string-alignments "How large is the sphere?"
                                   "What size is the cube?"))

(length
 (maximal-string-alignments "The tiny shiny cylinder has what color?"
                            "What is the color of the large shiny sphere?"))
=> 216 possible alignments

(length
 (maximal-string-alignments "The tiny shiny cylinder has what color?"
                            "What is the material of the big purple object?"))
=> 3196800 possible alignments 
==> 

(length
 (anti-unify-predicate-network
  (form-constraints-with-meets (split "The tiny shiny cylinder has what color?" #\space) :variables t)
  (form-constraints-with-meets (split "What is the material of the big purple object?" #\space) :variables t)))
=> 40320 anti-unification results

|#

;;;;;;;;;;
;; Cost ;;
;;;;;;;;;;

(defmethod anti-unification-cost ((au-result string-au-result))
  "Cost of a string anti-unification result."
  ;; TO DO: determine cost
  (+ (length (pattern-delta au-result))
     (length (source-delta au-result))))

(defmethod anti-unification-cost ((au-result sequences-au-result))
  "Cost of a string anti-unification result."
  ;; TO DO: determine cost
  (+ (length (pattern-delta au-result))
     (length (source-delta au-result))))



;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequence predicates ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-anti-unification-result->sequence-predicates (string-anti-unification-result)
  (with-slots (generalisation pattern-delta source-delta cost)
      string-anti-unification-result
    (let (;; make seq predicaes for the generalisation and
          ;; insert NIL placeholders for where the delta's are
          (generalisation-predicates
           (loop for elem in generalisation
                 when (stringp elem)
                 collect `(sequence ,elem ,(make-var (next-au-var)) ,(make-var (next-au-var)))
                 else collect nil))
          pattern-delta-predicates source-delta-predicates
          pattern-bindings source-bindings)
      ;; loop over the elements in the pattern delta
      ;; find the neighbouring seq predicates in the generalisation
      ;; add the variable renamings to the bindings list
      ;; when the delta contains the empty string,
      ;; connect the seq predicates from the generalisation directly
      (loop for (pattern-var . pattern-elem) in pattern-delta
            for pos-in-gen = (position pattern-var generalisation)
            for left-neighbour = (unless (= pos-in-gen 0)
                                   (nth (- pos-in-gen 1) generalisation-predicates))
            for right-neighbour = (unless (= pos-in-gen (- (length generalisation) 1))
                                    (nth (+ pos-in-gen 1) generalisation-predicates))
            if (string= pattern-elem "")
            do (let ((fresh-var (make-var (next-au-var))))
                 (when right-neighbour
                   (push (cons fresh-var (third right-neighbour)) pattern-bindings))
                 (when left-neighbour
                   (push (cons fresh-var (fourth left-neighbour)) pattern-bindings)))
            else
            do (let ((pattern-left-boundary (make-var (next-au-var)))
                     (pattern-right-boundary (make-var (next-au-var))))
                 (push `(sequence ,pattern-elem ,pattern-left-boundary ,pattern-right-boundary) pattern-delta-predicates)
                 (when right-neighbour
                   (push (cons pattern-right-boundary (third right-neighbour)) pattern-bindings))
                 (when left-neighbour
                   (push (cons pattern-left-boundary (fourth left-neighbour)) pattern-bindings))))
      ;; loop over the elements in the source delta
      ;; find the neighbouring seq predicates in the generalisation
      ;; add the variable renamings to the bindings list
      ;; when the delta contains the empty string,
      ;; connect the seq predicates from the generalisation directly
      (loop for (source-var . source-elem) in source-delta
            for pos-in-gen = (position source-var generalisation)
            for left-neighbour = (unless (= pos-in-gen 0)
                                   (nth (- pos-in-gen 1) generalisation-predicates))
            for right-neighbour = (unless (= pos-in-gen (- (length generalisation) 1))
                                    (nth (+ pos-in-gen 1) generalisation-predicates))
            if (and (string= source-elem "") left-neighbour right-neighbour)
            do (let ((fresh-var (make-var (next-au-var))))
                 (push (cons fresh-var (third right-neighbour)) source-bindings)
                 (push (cons fresh-var (fourth left-neighbour)) source-bindings))
            else
            do (let ((source-left-boundary (make-var (next-au-var)))
                     (source-right-boundary (make-var (next-au-var))))
                 (push `(sequence ,source-elem ,source-left-boundary ,source-right-boundary) source-delta-predicates)
                 (when right-neighbour
                   (push (cons source-right-boundary (third right-neighbour)) source-bindings))
                 (when left-neighbour
                   (push (cons source-left-boundary (fourth left-neighbour)) source-bindings))))
      ;; make an anti-unification result
      ;; remove the NIL placeholders
      (make-instance 'sequences-au-result
                     :pattern (pattern string-anti-unification-result)
                     :source (source string-anti-unification-result)
                     :generalisation (remove nil generalisation-predicates)
                     :pattern-bindings pattern-bindings
                     :source-bindings source-bindings
                     :pattern-delta pattern-delta-predicates
                     :source-delta source-delta-predicates
                     :cost cost))))



#|
(setf *test* (anti-unify-strings "GCATGCG" "GATTACA" :to-sequence-predicates-p t))

;; check that the anti-unification results have the same structure as on the meaning side
;; bindings in the same order? (first gen-var, then delta-var)
;; what about decoupled vars?

(setf *test*
      (anti-unify-strings "What size is the blue cube?" "What size is the red cube?"
                          :to-sequence-predicates-p t))

(setf *test*
      (anti-unify-strings "What size is the cube?" "What size is the red cube?"
                          :to-sequence-predicates-p t))

(anti-unify-strings "What is the color of the sphere?" "What is the size of the cube?"
                    :to-sequence-predicates-p t)

(setf *test*
      (anti-unify-strings "what size is the cube?" "what color is the"))
  
|#

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reversibility check ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

#|(defun merge-adjacent-sequence-predicates (sequence-predicates)
  "When there are adjacent sequence predicates, merge them together."
  (loop for predicate in sequence-predicates
        for left-bound = (third predicate)
        for right-bound = (fourth predicate)
        for adjacent-left = (find left-bound sequence-predicates :key #'fourth)
        for adjacent-right = (find right-bound sequence-predicates :key #'third)
        if adjacent-left
          return (let* ((new-predicate `(sequence ,(mkstr (second adjacent-left) (second predicate))
                                                  ,(third adjacent-left) ,(fourth predicate)))
                        (updated-predicates
                         (cons new-predicate (remove adjacent-left (remove predicate sequence-predicates :test #'equal) :test #'equal))))
                   (merge-adjacent-sequence-predicates updated-predicates))
        else if adjacent-right
           return (let* ((new-predicate `(sequence ,(mkstr (second predicate) (second adjacent-right))
                                                   ,(third predicate) ,(fourth adjacent-right)))
                         (updated-predicates
                          (cons new-predicate (remove adjacent-right (remove predicate sequence-predicates :test #'equal) :test #'equal))))
                    (merge-adjacent-sequence-predicates updated-predicates))
        finally (return sequence-predicates)))|#


(defun merge-adjacent-sequence-predicates (sequence-predicates)
  "When there are adjacent sequence predicates, merge them together."
  (loop for predicate in sequence-predicates
        for left-bound = (third predicate)
        for right-bound = (fourth predicate)
        for adjacent-left = (find left-bound sequence-predicates :key #'fourth)
        for adjacent-right = (find right-bound sequence-predicates :key #'third)
        if adjacent-left
          return (let ((new-predicate `(sequence ,(mkstr (second adjacent-left) (second predicate))
                                                 ,(third adjacent-left) ,(fourth predicate)))
                       (copied-sequence-predicates (copy-list sequence-predicates)))
                   (setf (nth (position predicate copied-sequence-predicates) copied-sequence-predicates) new-predicate)
                   (merge-adjacent-sequence-predicates (remove adjacent-left copied-sequence-predicates :test #'equal)))
        else if adjacent-right
           return (let ((new-predicate `(sequence ,(mkstr (second predicate) (second adjacent-right))
                                                  ,(third predicate) ,(fourth adjacent-right)))
                        (copied-sequence-predicates (copy-list sequence-predicates)))
                    (setf (nth (position predicate copied-sequence-predicates) copied-sequence-predicates) new-predicate)
                    (merge-adjacent-sequence-predicates (remove adjacent-right copied-sequence-predicates :test #'equal)))
        finally (return sequence-predicates)))


(defmethod compute-network-from-anti-unification-result ((au-result sequences-au-result) pattern-or-source)
  "Returns original network based on generalisation, bindings-list and delta."  
  (let* ((generalisation (generalisation au-result))
         (bindings-key (case pattern-or-source
                         (pattern #'pattern-bindings)
                         (source #'source-bindings)))
         (delta-key (case pattern-or-source
                      (pattern #'pattern-delta)
                      (source #'source-delta)))
         (bindings-list (funcall bindings-key au-result))
         (delta (funcall delta-key au-result)))
    (merge-adjacent-sequence-predicates
     (append (substitute-bindings (reverse-bindings bindings-list) generalisation)
             delta))))
