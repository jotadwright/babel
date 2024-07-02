(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maximal Sequence Alignment ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric maximal-sequence-alignments (pattern source pattern-boundaries source-boundaries
                                         &key match-cost mismatch-cost gap-opening-cost gap-extension-cost
                                         remove-duplicate-alignments n-optimal-alignments max-nr-of-gaps)
  (:documentation "Computes the maximal alignments of two input strings according to 'mode'.
                   Mode can be set to :needleman-wunsch, which uses linear gap penalties,
                   or :gotoh, which uses affine gap penalties. For the algorithm of Gotoh,
                   the implementation of Altshul and Erickson (1986) is used. Both algorithms
                   expect costs for matches, mismatches, and gaps. Needleman-Wunsch only uses
                   the gap-extension-cost, whereas Gotoh uses both gap-opening-cost and
                   gap-extension-cost. Since the algorithms uses costs (as opposed to scores),
                   they are distance minimization algorithms."))


(defmethod maximal-sequence-alignments ((pattern string) (source string) (pattern-boundaries list) (source-boundaries list)
                                        &key (match-cost -1) (mismatch-cost 1) (gap-opening-cost 5) (gap-extension-cost 1)
                                        (remove-duplicate-alignments t) n-optimal-alignments max-nr-of-gaps)
  (maximal-sequence-alignments (coerce pattern 'list) (coerce source 'list)
                               pattern-boundaries source-boundaries
                               :match-cost match-cost
                               :mismatch-cost mismatch-cost
                               :gap-opening-cost gap-opening-cost
                               :gap-extension-cost gap-extension-cost
                               :remove-duplicate-alignments remove-duplicate-alignments
                               :n-optimal-alignments n-optimal-alignments
                               :max-nr-of-gaps max-nr-of-gaps))
  

(defmethod maximal-sequence-alignments ((pattern list) (source list) (pattern-boundaries list) (source-boundaries list)
                                        &key (match-cost -1) (mismatch-cost 1) (gap-opening-cost 5) (gap-extension-cost 1)
                                        (remove-duplicate-alignments t) n-optimal-alignments max-nr-of-gaps)
  (let* ((nx (length pattern)) ;; number of rows
         (ny (length source))  ;; number of columns
         ;; matrices to store costs
         (P (make-array (list (+ nx 1) (+ ny 1))))
         (Q (make-array (list (+ nx 1) (+ ny 1))))
         (R (make-array (list (+ nx 1) (+ ny 1))))
         ;; matrices to store graph edges
         (a (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; vertical full edge
         (b (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; horizontal full edge
         (c (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; diagonal full edge
         (d (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; vertical half edge - top part
         (e (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; vertical half edge - bottom part
         (f (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; horizontal half edge - left part
         (g (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; horizontal half edge - right part
         ) 
         
    ;; An entry R_{i,j} represents the best score for the alignment of the
    ;; prefixes pattern_{1...i} with source_{1...j}. An entry in P_{i,j} and Q_{i,j}
    ;; provides the best score under the additional constraints that the
    ;; alignment ends in a gap within pattern or source, respectively.
    
    ;; Initalize the matrices
    (setf-matrix-row P 0 (make-array (+ ny 1) :initial-element most-positive-fixnum))
    (setf-matrix-column Q 0 (make-array (+ nx 1) :initial-element most-positive-fixnum))
    (setf-matrix-row R 0 (list->array (loop for j from 0 to ny collect (+ gap-opening-cost (* gap-extension-cost j)))))
    (setf-matrix-column R 0 (list->array (loop for i from 0 to nx collect (+ gap-opening-cost (* gap-extension-cost i)))))
    (setf (aref R 0 0) 0)
    (setf (aref c (+ nx 1) (+ ny 1)) 1)

    ;; Run the Gotoh algorithm according to the implementation
    ;; provided by Altschul and Ericksson (1986)
    (gotoh-cost-assignment pattern source nx ny P Q R a b c d e f g
                           :match-cost match-cost
                           :mismatch-cost mismatch-cost
                           :gap-opening-cost gap-opening-cost
                           :gap-extension-cost gap-extension-cost)
    (gotoh-edge-assignment nx ny a b c d e f g)    

    ;; Trace back pointers from the bottom-right cell to the top-left cell.
    ;; Cells may contain multiple pointers, so there may be multiple paths.
    ;; Return all alignments and optionally remove duplicates.
    (let ((all-optimal-alignments
           (extract-optimal-alignments pattern source a b c d e f g
                                       pattern-boundaries
                                       source-boundaries
                                       :match-cost match-cost
                                       :mismatch-cost mismatch-cost
                                       :gap-opening-cost gap-opening-cost
                                       :gap-extension-cost gap-extension-cost
                                       :n-optimal-alignments n-optimal-alignments
                                       :max-nr-of-gaps max-nr-of-gaps)))
      (if remove-duplicate-alignments
        (remove-duplicates all-optimal-alignments :key #'match-positions :test #'equal)
        all-optimal-alignments))))


(defun gotoh-cost-assignment (pattern source nx ny P Q R a b c d e f g
                              &key (match-cost -1) (mismatch-cost 1)
                              (gap-opening-cost 5) (gap-extension-cost 1))
  (loop for i from 0 to nx
        do (loop for j from 0 to ny
                 for matchp = (when (and (> i 0) (> j 0))
                                (eql (nth (- i 1) pattern) (nth (- j 1) source)))
                 do
                   ;; 1) find the minimum cost of a path ending at node N_{i,j} using vertical edge
                   ;; 2) determine if cost P_{i,j} can be achieved with and without edge V_{i-1,j}, i.e. vertical edge above
                   (when (> i 0)
                     (let* ((extended-gap-cost (+ (aref P (- i 1) j) gap-extension-cost))
                            (new-gap-cost (+ (aref R (- i 1) j) gap-opening-cost gap-extension-cost))
                            (min-cost (min extended-gap-cost new-gap-cost)))
                       (setf (aref P i j) min-cost)
                       (when (= min-cost extended-gap-cost)
                         (setf (aref d (- i 1) j) 1))
                       (when (= min-cost new-gap-cost)
                         (setf (aref e (- i 1) j) 1))))

                   ;; 3) find the minimum cost of a path ending at node N_{i,j using horizontal edge
                   ;; 4) determine if cost Q_{i,j} can be achieved with and without edge H_{i,j-1}, i.e. horizontal edge left
                   (when (> j 0)
                     (let* ((extended-gap-cost (+ (aref Q i (- j 1)) gap-extension-cost))
                            (new-gap-cost (+ (aref R i (- j 1)) gap-opening-cost gap-extension-cost))
                            (min-cost (min extended-gap-cost new-gap-cost)))
                       (setf (aref Q i j) min-cost)
                       (when (= min-cost extended-gap-cost)
                         (setf (aref f i (- j 1)) 1))
                       (when (= min-cost new-gap-cost)
                         (setf (aref g i (- j 1)) 1))))
                     
                   ;; 5) find the minimum cost of a path ending at node N_{i,j}
                   ;; 6) determine if cost R_{i,j} can be achieved by vertical, horizontal or diagonal edges
                   (when (and (> i 0) (> j 0))
                     (let* ((vertical-edge-cost (aref P i j))
                            (horizontal-edge-cost (aref Q i j))
                            (diagonal-edge-cost (+ (aref R (- i 1) (- j 1)) (if matchp match-cost mismatch-cost)))
                            (min-cost (min vertical-edge-cost horizontal-edge-cost diagonal-edge-cost)))
                       (setf (aref R i j) min-cost)))
                   (when (= (aref R i j) (aref P i j))
                     (setf (aref a i j) 1))
                   (when (= (aref R i j) (aref Q i j))
                     (setf (aref b i j) 1))
                   (when (and (> i 0) (> j 0)
                              (= (aref R i j)
                                 (+ (aref R (- i 1) (- j 1))
                                    (if matchp match-cost mismatch-cost))))
                     (setf (aref c i j) 1)))))


(defun gotoh-edge-assignment (nx ny a b c d e f g)
  (loop for i from nx downto 0
        do (loop for j from ny downto 0
                 do ;; 1) if there is no optimal path passing through node N_{i,j} which has cost R_{i,j}
                   ;;     remove the full edges vertically, horizontally and diagonally
                   (when (and (or (= (aref a (+ i 1) j) 0)
                                  (= (aref e i j) 0))
                              (or (= (aref b i (+ j 1)) 0)
                                  (= (aref g i j) 0))
                              (= (aref c (+ i 1) (+ j 1)) 0))
                     (setf (aref a i j) 0
                           (aref b i j) 0
                           (aref c i j) 0))
                   ;; 2) if no optimal path passes through node N_{i,j}, proceed to the next node
                   (if (and (= (aref a (+ i 1) j) 0)
                            (= (aref b i (+ j 1)) 0)
                            (= (aref c (+ i 1) (+ j 1)) 0))
                     nil ;; skip
                     (progn
                       ;; 3) if edge V_{i+1,j} is in an optimal path and requires edge V_{i,j} to be in an optimal path,
                       ;;    determine if an optimal path that uses edge V_{i+1,j} must use edge V_{i,j} and the converse:
                       (if (and (= (aref a (+ i 1) j) 1) (= (aref d i j) 1))
                         (setf (aref d (+ i 1) j) (- 1 (aref e i j))
                               (aref e i j) (- 1 (aref a i j))
                               (aref a i j) 1)
                         (setf (aref d (+ i 1) j) 0
                               (aref e i j) 0))
                       ;; 4) if edge H_{i,j+1} is in an optimal path and requires edge H_{i,j} to be in an optimal path,
                       ;;    determine if an optimal path that uses edge H_{i,j+1} must use edge H_{i,j} and the converse:
                       (if (and (= (aref b i (+ j 1)) 1) (= (aref f i j) 1))
                         (setf (aref f i (+ j 1)) (- 1 (aref g i j))
                               (aref g i j) (- 1 (aref b i j))
                               (aref b i j) 1)
                         (setf (aref f i (+ j 1)) 0
                               (aref g i j) 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extract Optimal Alignments ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sequence-alignment-state ()
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
   (cost :initarg :cost :accessor cost :initform 0 :type number)
   (match-positions :initarg :match-positions :accessor match-positions :initform nil :type list)
   (gap-counter :initarg :gap-counter :accessor gap-counter :initform 0 :type number)
   (prev-edge :initarg :prev-edge :accessor prev-edge :initform nil)
   (next-edge :initarg :next-edge :accessor next-edge :initform nil)))


(defun make-initial-sequence-alignment-state (i j)
  (make-instance 'sequence-alignment-state :i i :j j))


(defun extract-optimal-alignments (pattern source a b c d e f g
                                           pattern-boundaries source-boundaries
                                           &key (match-cost -1)
                                           (mismatch-cost 1)
                                           (gap-opening-cost 5)
                                           (gap-extension-cost 1)
                                           n-optimal-alignments
                                           max-nr-of-gaps)
  (loop with solutions = nil
        ;; start at position (M, N)
        with queue = (list (make-initial-sequence-alignment-state
                            (length pattern) (length source)))
        ;; stop when n-optimal-alignments is reached
        ;; or when the queue is empty
        until (or (and (numberp n-optimal-alignments)
                       (= (length solutions) n-optimal-alignments))
                  (null queue))
        for state = (pop queue)
        ;; access all the slots of the alignment state
        do ;; reached index (0,0) -> push to solutions!
          (with-slots (i j next-edge) state
            (if (and (= i 0) (= j 0))
              (push state solutions)
              ;; otherwise, make the next state(s)
              (let* ((next-states
                      (cond (;; next-edge is set to vertical -> only need to check vertical edges
                             (eql next-edge 'vertical)
                             ;; as a sanity check, we could assert that horizontal and diagonal edges here are 0
                             (let ((next-state (check-vertical-edges pattern source pattern-boundaries source-boundaries state a d e
                                                                     :gap-opening-cost gap-opening-cost :gap-extension-cost gap-extension-cost)))
                               (when next-state
                                 (list next-state))))
                            
                            (;; next-edge is set to horizontal -> only need to check horizontal edges
                             (eql next-edge 'horizontal)
                             ;; as a sanity check, we could assert that vertical and diagonal edges here are 0
                             (let ((next-state (check-horizontal-edges pattern source pattern-boundaries source-boundaries state b f g
                                                                       :gap-opening-cost gap-opening-cost :gap-extension-cost gap-extension-cost)))
                               (when next-state
                                 (list next-state))))
                            
                            (;; next-edge is not set -> check vertical, horizontal and diagonal edges
                             t
                             (let ((next-state-diagonal (check-diagonal-edges pattern source pattern-boundaries source-boundaries state c
                                                                              :match-cost match-cost :mismatch-cost mismatch-cost))
                                   (next-state-vertical (check-vertical-edges pattern source pattern-boundaries source-boundaries state a d e
                                                                              :gap-opening-cost gap-opening-cost :gap-extension-cost gap-extension-cost))
                                   (next-state-horizontal (check-horizontal-edges pattern source pattern-boundaries source-boundaries state b f g
                                                                                  :gap-opening-cost gap-opening-cost :gap-extension-cost gap-extension-cost)))
                               (remove nil (list next-state-diagonal next-state-vertical next-state-horizontal))))))
                     (next-states-with-max-gaps
                      (if max-nr-of-gaps
                        (remove-if #'(lambda (state) (> (gap-counter state) max-nr-of-gaps)) next-states)
                        next-states)))
                (loop for ns in next-states-with-max-gaps
                      do (push ns queue)))))
                        
        finally (return solutions)))


(defun check-vertical-edges (pattern source pattern-boundaries source-boundaries state a d e
                                     &key (gap-opening-cost 5) (gap-extension-cost 1))
  (with-slots (aligned-pattern aligned-source
               aligned-pattern-boundaries aligned-source-boundaries
               i j cost match-positions gap-counter prev-edge next-edge) state
    (when (= (aref a i j) 1)
      (let* ((expanded-pattern (cons (nth (- i 1) pattern) aligned-pattern))
             (expanded-source (cons #\_ aligned-source))
             (current-left-source-boundary (car (first aligned-source-boundaries)))
             (current-left-pattern-boundary (car (first aligned-pattern-boundaries)))
             (source-boundary-vars (make-boundary-vars nil source-boundaries current-left-source-boundary :gap t))
             (pattern-boundary-vars (make-boundary-vars i pattern-boundaries current-left-pattern-boundary))
             (new-gap-p (equal (first match-positions) (cons i j)))
             (cost-increase (if new-gap-p (+ gap-extension-cost gap-opening-cost) gap-extension-cost))
             (next-state (make-instance 'sequence-alignment-state
                                        :aligned-pattern expanded-pattern
                                        :aligned-source expanded-source
                                        :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                        :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                        :i (- i 1) :j j
                                        :cost (+ cost cost-increase)
                                        :match-positions match-positions
                                        :gap-counter (if new-gap-p (+ 1 gap-counter) gap-counter)
                                        :prev-edge 'vertical)))
        ;; when d is set, the next edge has to be vertical
        (when (= (aref d i j) 1)
          (setf (next-edge next-state) 'vertical))
        ;; when e is set, the prev edge has to be vertical
        ;; if not, remove the next state!
        (when (= (aref e i j) 1)
          (unless (eql prev-edge 'vertical)
            (setf next-state nil)))
        ;; return the next state
        next-state))))


(defun check-horizontal-edges (pattern source pattern-boundaries source-boundaries state b f g
                                       &key (gap-opening-cost 5) (gap-extension-cost 1))
  (with-slots (aligned-pattern aligned-source
               aligned-pattern-boundaries aligned-source-boundaries
               i j cost match-positions gap-counter prev-edge next-edge) state
    (when (= (aref b i j) 1)
      (let* ((expanded-pattern (cons #\_ aligned-pattern))
             (expanded-source (cons (nth (- j 1) source) aligned-source))
             (current-left-source-boundary (car (first aligned-source-boundaries)))
             (current-left-pattern-boundary (car (first aligned-pattern-boundaries)))
             (source-boundary-vars (make-boundary-vars j source-boundaries current-left-source-boundary))
             (pattern-boundary-vars (make-boundary-vars nil pattern-boundaries current-left-pattern-boundary :gap t))
             (new-gap-p (equal (first match-positions) (cons i j)))
             (cost-increase (if new-gap-p (+ gap-extension-cost gap-opening-cost) gap-extension-cost))
             (next-state (make-instance 'sequence-alignment-state
                                        :aligned-pattern expanded-pattern
                                        :aligned-source expanded-source
                                        :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                        :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                        :i i :j (- j 1)
                                        :cost (+ cost cost-increase)
                                        :match-positions match-positions
                                        :gap-counter (if new-gap-p (+ 1 gap-counter) gap-counter)
                                        :prev-edge 'horizontal)))
        ;; when f is set, the next edge has to be horizontal
        (when (= (aref f i j) 1)
          (setf (next-edge next-state) 'horizontal))
        ;; when g is set, the prev edge has to be horizontal
        ;; if not, remove the next state!
        (when (= (aref g i j) 1)
          (unless (eql prev-edge 'horizontal)
            (setf next-state nil)))
        ;; return the next state
        next-state))))


(defun check-diagonal-edges (pattern source pattern-boundaries source-boundaries state c
                                     &key (match-cost -1) (mismatch-cost 1))
  (with-slots (aligned-pattern aligned-source
               aligned-pattern-boundaries aligned-source-boundaries
               i j cost match-positions gap-counter prev-edge next-edge) state
    (when (= (aref c i j) 1)
      (let* ((expanded-pattern (cons (nth (- i 1) pattern) aligned-pattern))
             (expanded-source (cons (nth (- j 1) source) aligned-source))
             (current-left-source-boundary (car (first aligned-source-boundaries)))
             (current-left-pattern-boundary (car (first aligned-pattern-boundaries)))
             (matchp (eql (nth (- i 1) pattern) (nth (- j 1) source)))
             (source-boundary-vars (make-boundary-vars j source-boundaries current-left-source-boundary))
             (pattern-boundary-vars (make-boundary-vars i pattern-boundaries current-left-pattern-boundary))
             (new-gap-p (or (and (null match-positions) (null matchp))
                            (and (first match-positions)
                                 (equal (first match-positions) (cons i j))
                                 (null matchp))))
             (next-state (make-instance 'sequence-alignment-state
                                        :aligned-pattern expanded-pattern
                                        :aligned-source expanded-source
                                        :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                        :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                        :i (- i 1) :j (- j 1)
                                        :cost (+ cost (if matchp match-cost mismatch-cost))
                                        :match-positions (if matchp (cons (cons (- i 1) (- j 1)) match-positions) match-positions)
                                        :gap-counter (if new-gap-p (+ 1 gap-counter) gap-counter)
                                        :prev-edge 'diagonal)))
        ;; return the next state
        next-state))))

