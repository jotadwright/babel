; (ql:quickload :fcg)
(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maximal Sequence Alignment - nv algorithm - mismatches ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric maximal-sequence-alignments (pattern source pattern-boundaries source-boundaries
                                         &key match-cost mismatch-cost mismatch-opening-cost gap-opening-cost gap-cost
                                         remove-duplicate-alignments n-optimal-alignments max-nr-of-au-gaps)
  (:documentation "Computes the maximal alignments of two input strings, using the algorithm of Altschul and Erickson (1986).
                   If gap-opening-cost is set to 0, the linear sequence alignment is returned.
                   Example costs for linear sequence alignment (alignment results will be the same as results from Needleman-Wunsch algorithm with the same costs):
                       -> match-cost -1, mismatch-cost 1, gap-opening-cost 0, gap-cost 1.
                   If gap-opening-cost is higher than 0, the affine gap sequence alignment is returned.
                   Example costs for affine gap sequence alignment (costs used in Altschul & Erickson paper):
                       -> match-cost 0, mismatch-cost 1, gap-opening-cost 1, gap-cost 1
                   Since the algorithm uses costs (as opposed to scores),
                   it is a distance minimization algorithm.
                   :remove-duplicate-alignments removes alignments that will lead to the same anti-unification result.
                   :n-optimal-alignment stops backtrace procedure of sequence alignment when number of optimal alignments is reached.
                   :max-nr-of-au-gaps returns only the optimal alignments that don't exceed the max number of gaps (in terms of gaps in the generalisation of the anti-unification"))


(defmethod maximal-sequence-alignments ((pattern string) (source string) (pattern-boundaries list) (source-boundaries list)
                                        &key (match-cost -1) (mismatch-cost 1) (mismatch-opening-cost 1) (gap-opening-cost 5) (gap-cost 1)
                                        (remove-duplicate-alignments t) n-optimal-alignments max-nr-of-au-gaps)
  (maximal-sequence-alignments (coerce pattern 'list) (coerce source 'list)
                               pattern-boundaries source-boundaries
                               :match-cost match-cost
                               :mismatch-cost mismatch-cost
                               :mismatch-opening-cost mismatch-opening-cost
                               :gap-opening-cost gap-opening-cost
                               :gap-cost gap-cost
                               :remove-duplicate-alignments remove-duplicate-alignments
                               :n-optimal-alignments n-optimal-alignments
                               :max-nr-of-au-gaps max-nr-of-au-gaps))

;; introduce new matrix S to keep track of opened mismatches
(defmethod maximal-sequence-alignments ((pattern list) (source list) (pattern-boundaries list) (source-boundaries list)
                                        &key (match-cost -1) (mismatch-cost 1) (mismatch-opening-cost 1) (gap-opening-cost 5) (gap-cost 1)
                                        (remove-duplicate-alignments t) n-optimal-alignments max-nr-of-au-gaps)
  (let* ((nx (length pattern)) ;; number of rows
         (ny (length source))  ;; number of columns
         ;; matrices to store costs
         (P (make-array (list (+ nx 1) (+ ny 1))))
         (Q (make-array (list (+ nx 1) (+ ny 1))))
         (R (make-array (list (+ nx 1) (+ ny 1))))
         (S (make-array (list (+ nx 1) (+ ny 1))))
         ;; matrices to store graph edges
         (a (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; vertical full edge
         (b (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; horizontal full edge
         (c (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; diagonal full edge
         (d (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; vertical half edge - top part
         (e (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; vertical half edge - bottom part
         (f (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; horizontal half edge - left part
         (g (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; horizontal half edge - right part
         (k (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; mismatch half edge - left part
         (l (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; mismatch half edge - right part
         )
         
    ;; An entry R_{i,j} represents the best score for the alignment of the
    ;; prefixes pattern_{1...i} with source_{1...j}. An entry in P_{i,j} and Q_{i,j}
    ;; provides the best score under the additional constraints that the
    ;; alignment ends in a gap within pattern or source, respectively.
    
    ;; Initalize the matrices
    (setf-matrix-row P 0 (make-array (+ ny 1) :initial-element +inf))
    (setf-matrix-column Q 0 (make-array (+ nx 1) :initial-element +inf))
    (setf-matrix-row S 0 (make-array (+ ny 1) :initial-element +inf))
    (setf-matrix-column S 0 (make-array (+ nx 1) :initial-element +inf))
    (setf-matrix-row R 0 (list->array (loop for j from 0 to ny collect (+ gap-opening-cost (* gap-cost j)))))
    (setf-matrix-column R 0 (list->array (loop for i from 0 to nx collect (+ gap-opening-cost (* gap-cost i)))))
    (setf (aref R 0 0) 0)
    (setf (aref c (+ nx 1) (+ ny 1)) 1)

    ;; Run the Gotoh algorithm according to the implementation
    ;; provided by Altschul and Ericksson (1986)
    (cost-assignment pattern source nx ny P Q R S a b c d e f g k l
                           :match-cost match-cost
                           :mismatch-opening-cost mismatch-opening-cost
                           :mismatch-cost mismatch-cost
                           :gap-opening-cost gap-opening-cost
                           :gap-cost gap-cost)
    (edge-assignment nx ny a b c d e f g k l)    

    ;; Trace back pointers from the bottom-right cell to the top-left cell.
    ;; Cells may contain multiple pointers, so there may be multiple paths.
    ;; Return all alignments and optionally remove duplicates.
    (let* ((optimal-cost (aref R nx ny))
           (all-optimal-alignments
            (extract-optimal-alignments pattern source a b c d e f g k l
                                        pattern-boundaries
                                        source-boundaries
                                        optimal-cost
                                        :match-cost match-cost
                                        :mismatch-cost mismatch-cost
                                        :gap-opening-cost gap-opening-cost
                                        :gap-cost gap-cost
                                        :n-optimal-alignments n-optimal-alignments
                                        :max-nr-of-au-gaps max-nr-of-au-gaps)))
      (if remove-duplicate-alignments
        (remove-duplicates all-optimal-alignments :key #'match-positions :test #'equal)
        all-optimal-alignments))))

(defun cost-assignment (pattern source nx ny P Q R S a b c d e f g k l 
                                &key (match-cost -1) (mismatch-opening-cost 1) (mismatch-cost 1)
                                (gap-opening-cost 5) (gap-cost 1))
  (loop for i from 0 to nx
        do (loop for j from 0 to ny
                 for matchp = (when (and (> i 0) (> j 0))
                                (eql (nth (- i 1) pattern) (nth (- j 1) source)))
                 do
                   ;; 1) find the minimum cost of a path ending at node N_{i,j} using vertical edge
                   ;; 2) determine if cost P_{i,j} can be achieved with and without edge V_{i-1,j}, i.e. vertical edge above
                   (when (> i 0)
                     (let* ((extended-gap-cost (+ (aref P (- i 1) j) gap-cost))
                            (new-gap-cost (+ (aref R (- i 1) j) gap-opening-cost gap-cost))
                            (min-cost (min extended-gap-cost new-gap-cost)))
                       (setf (aref P i j) min-cost)
                       (when (= min-cost extended-gap-cost)
                         (setf (aref d (- i 1) j) 1))
                       (when (= min-cost new-gap-cost)
                         (setf (aref e (- i 1) j) 1))))

                   ;; 3) find the minimum cost of a path ending at node N_{i,j using horizontal edge
                   ;; 4) determine if cost Q_{i,j} can be achieved with and without edge H_{i,j-1}, i.e. horizontal edge left
                   (when (> j 0)
                     (let* ((extended-gap-cost (+ (aref Q i (- j 1)) gap-cost))
                            (new-gap-cost (+ (aref R i (- j 1)) gap-opening-cost gap-cost))
                            (min-cost (min extended-gap-cost new-gap-cost)))
                       (setf (aref Q i j) min-cost)
                       (when (= min-cost extended-gap-cost)
                         (setf (aref f i (- j 1)) 1))
                       (when (= min-cost new-gap-cost)
                         (setf (aref g i (- j 1)) 1))))
                   
                   (when (and (> i 0) (> j 0))
                     (let* ((extended-mismatch-cost
                             (if (not matchp) ;; check for mismatch
                               (+ (aref S (- i 1) (- j 1)) mismatch-cost)
                               +inf)
                             ;(+ (aref S (- i 1) (- j 1)) mismatch-cost)
                             )
                            ;; still got optimal solutions when removing the matchp check here:
                            (new-mismatch-cost
                             (if (not matchp) ;; check for mismatch
                               (+ (aref R (- i 1) (- j 1)) mismatch-opening-cost mismatch-cost)
                               +inf)
                             ;(+ (aref R (- i 1) (- j 1)) mismatch-opening-cost mismatch-cost)
                             )
                            (min-cost (min extended-mismatch-cost new-mismatch-cost)))
                       (setf (aref S i j) min-cost)
                       ;; do NOT set k and l when no mismatch...
                       (when (and (= min-cost extended-mismatch-cost)
                                  (not (= min-cost +inf)))
                         (setf (aref k (- i 1) (- j 1)) 1))
                       (when (and (= min-cost new-mismatch-cost)
                                  (not (= min-cost +inf)))
                         (setf (aref l (- i 1) (- j 1)) 1))))
                     
                   ;; 5) find the minimum cost of a path ending at node N_{i,j}
                   ;; 6) determine if cost R_{i,j} can be achieved by vertical, horizontal or diagonal edges
                   (when (and (> i 0) (> j 0))
                     (let* ((vertical-edge-cost (aref P i j))
                            (horizontal-edge-cost (aref Q i j))
                            (mismatch-edge-cost (aref S i j)) ;; check if we need to check for mismatches in this matrix
                            (diagonal-edge-cost (+ (aref R (- i 1) (- j 1)) (if matchp match-cost +inf)))
                            ;; only look at match here, if mismatch it should be covered in matrix S
                            (min-cost (min vertical-edge-cost horizontal-edge-cost diagonal-edge-cost mismatch-edge-cost)))
                       (setf (aref R i j) min-cost)))
                   (when (= (aref R i j) (aref P i j))
                     (setf (aref a i j) 1))
                   (when (= (aref R i j) (aref Q i j))
                     (setf (aref b i j) 1))
                   (when (and (> i 0) (> j 0)
                              (= (aref R i j) (aref S i j)))
                     (setf (aref c i j) 1))
                   (when (and (> i 0) (> j 0)
                              (= (aref R i j)
                                 (+ (aref R (- i 1) (- j 1))
                                    (if matchp match-cost +inf))))
                     (setf (aref c i j) 1)))))

(defun edge-assignment (nx ny a b c d e f g k l)
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
                               (aref g i j) 0))
                       ;; 5) if edge D_{i+1,j+1} is in an optimal path and requires edge D_{i,j} to be in an optimal path,
                       ;;    determine if an optimal path that uses edge D_{i+1,j+1} must use edge D_{i,j} and the converse:
                       (if (and (= (aref c (+ i 1) (+ j 1)) 1) (= (aref k i j) 1))
                         (setf (aref k (+ i 1) (+ j 1)) (- 1 (aref l i j))
                               (aref l i j) (- 1 (aref c i j))
                               (aref c i j) 1)
                         (setf (aref k (+ i 1) (+ j 1)) 0
                               (aref l i j) 0)))))))

(defun extract-optimal-alignments (pattern source a b c d e f g k l
                                           pattern-boundaries source-boundaries
                                           optimal-cost
                                           &key (match-cost -1)
                                           (mismatch-cost 1)
                                           (gap-opening-cost 5)
                                           (gap-cost 1)
                                           n-optimal-alignments
                                           max-nr-of-au-gaps)
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
                                                                     :gap-opening-cost gap-opening-cost :gap-cost gap-cost)))
                               (when next-state
                                 (list next-state))))
                            
                            (;; next-edge is set to horizontal -> only need to check horizontal edges
                             (eql next-edge 'horizontal)
                             ;; as a sanity check, we could assert that vertical and diagonal edges here are 0
                             (let ((next-state (check-horizontal-edges pattern source pattern-boundaries source-boundaries state b f g
                                                                       :gap-opening-cost gap-opening-cost :gap-cost gap-cost)))
                               (when next-state
                                 (list next-state))))

                            (;; next-edge is set to diagonal-mismatch
                             (eql next-edge 'diagonal-mismatch)
                             (let ((next-state (check-diagonal-edges pattern source pattern-boundaries source-boundaries state c k l
                                                                     :match-cost match-cost :mismatch-cost mismatch-cost)))
                               (when next-state
                                 (list next-state))))
                            
                            (;; next-edge is not set -> check vertical, horizontal and diagonal edges
                             t
                             (let ((next-state-diagonal (check-diagonal-edges pattern source pattern-boundaries source-boundaries state c k l
                                                                              :match-cost match-cost :mismatch-cost mismatch-cost))
                                   (next-state-vertical (check-vertical-edges pattern source pattern-boundaries source-boundaries state a d e
                                                                              :gap-opening-cost gap-opening-cost :gap-cost gap-cost))
                                   (next-state-horizontal (check-horizontal-edges pattern source pattern-boundaries source-boundaries state b f g
                                                                                  :gap-opening-cost gap-opening-cost :gap-cost gap-cost)))
                               (remove nil (list next-state-diagonal next-state-vertical next-state-horizontal))))))
                     (next-states-with-max-gaps
                      (if max-nr-of-au-gaps
                        (remove-if #'(lambda (state) (> (gap-counter state) max-nr-of-au-gaps)) next-states)
                        next-states)))
                (loop for ns in next-states-with-max-gaps
                      do (push ns queue)))))
                        
        finally
          (progn
            ;; the cost reconstructed by retracing the optimal alignments
            ;; should be equal to the cost at the bottom right of the cost matrix
            (assert (loop for solution in solutions
                          always (= (cost solution) optimal-cost)))
            (return solutions))))

(defun check-diagonal-edges (pattern source pattern-boundaries source-boundaries state c k l
                                     &key (match-cost -1) (mismatch-opening-cost 1) (mismatch-cost 1))
  (with-slots (aligned-pattern aligned-source
               aligned-pattern-boundaries aligned-source-boundaries
               i j cost match-positions gap-counter prev-edge next-edge) state
    (when (= (aref c i j) 1)  ;; check if there is a diagonal edge in this state
      (let* (;; indexing in pattern and source string is offset by -1 w.r.t. index in matrix (i,j)
             (pattern-char (nth (- i 1) pattern))
             (source-char (nth (- j 1) source))
             (expanded-pattern (cons pattern-char aligned-pattern))
             (expanded-source (cons source-char aligned-source))
             (current-left-source-boundary (car (first aligned-source-boundaries)))
             (current-left-pattern-boundary (car (first aligned-pattern-boundaries)))
             (matchp (eql pattern-char source-char))
             (source-boundary-vars (make-boundary-indices j source-boundaries current-left-source-boundary))
             (pattern-boundary-vars (make-boundary-vars i pattern-boundaries current-left-pattern-boundary))
             (new-mismatch (if (eql prev-edge 'diagonal-mismatch) mismatch-cost (+ mismatch-cost mismatch-opening-cost)))
             (new-gap-p (or (and (null match-positions) (null matchp))
                            (and (first match-positions)
                                 (equal (first match-positions) (cons (+ i 1) (+ j 1)))
                                 (null matchp))))
             (next-state (make-instance 'sequence-alignment-state
                                        :aligned-pattern expanded-pattern
                                        :aligned-source expanded-source
                                        :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                        :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                        :i (- i 1) :j (- j 1)
                                        :cost (+ cost (if matchp match-cost new-mismatch))
                                        :match-positions (if matchp (cons (cons i j) match-positions) match-positions)
                                        :gap-counter (if new-gap-p (+ 1 gap-counter) gap-counter)
                                        :prev-edge (if matchp 'diagonal 'diagonal-mismatch))))
        ;; when k is set, the next edge has to be diagonal
        (when (= (aref k i j) 1)
          (setf (next-edge next-state) 'diagonal-mismatch))
        ;; when l is set, the prev edge has to be diagonal
        ;; if not, remove the next state!
        (when (= (aref l i j) 1)
          (unless (eql prev-edge 'diagonal-mismatch)
            (setf next-state nil)))
        ;; return the next state
        next-state))))

#|

 ;; here we want to have a mismatch-extension and mismatch-opening cost
 ;; cost originally is 2
 ;; cost needs to be something else depending on the mismatch-opening and mismatch-cost, probably 3 (1 mismatch-opening and 3 mismatch 'extensions' and 1 match
(print-sequence-alignments (maximal-sequence-alignments "AGAT" "CTCT" nil nil
                             :match-cost -1 :mismatch-cost 1 :mismatch-opening-cost 1 :gap-opening-cost 1 :gap-cost 1
                             :remove-duplicate-alignments nil))

 ;; cost should be 2
(print-sequence-alignments (maximal-sequence-alignments "AGAT" "CGCT" nil nil
                             :match-cost -1 :mismatch-cost 1  :mismatch-opening-cost 1 :gap-opening-cost 1 :gap-cost 1
                             :remove-duplicate-alignments nil))

 ;; find examples with non-optimal path 
 ;; misschien is probleem van non-optimal path hier ook wel te danken aan de gaps + mismatches  -> nee want dan nog wil je zo weinig mogelijk nieuwe mismatch gaps openen
 ;; -> de pijlen moeten aangeven dat ze vanuit een mismatch komen en die willen extenden, ik denk dat dat momenteel het probleem is
 ;; optimale cost hier: -2 
 (print-sequence-alignments (maximal-sequence-alignments "what color is the sphere" "what size is the cube" nil nil
                             :match-cost -1 :mismatch-cost 1  :mismatch-opening-cost 3 :gap-opening-cost 0 :gap-cost 1
                             :remove-duplicate-alignments nil))

 ;; find examples with non-optimal path 
 ;; misschien is probleem van non-optimal path hier ook wel te danken aan de gaps + mismatches  -> nee want dan nog wil je zo weinig mogelijk nieuwe mismatch gaps openen
 ;; -> de pijlen moeten aangeven dat ze vanuit een mismatch komen en die willen extenden, ik denk dat dat momenteel het probleem is
 ;; optimale cost hier: -2 
 (print-sequence-alignments (maximal-sequence-alignments "xabcy" "xefy" nil nil
                             :match-cost -1 :mismatch-cost 1  :mismatch-opening-cost 1 :gap-opening-cost 0 :gap-cost 1
                             :remove-duplicate-alignments nil))

 
 
 
|#