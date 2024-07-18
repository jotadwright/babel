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

         (d-v (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; vertical half edge - top part
         (d-h (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
         (d-m (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
         (e (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
         (e-v (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
         (e-h (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
         (e-m (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0));; vertical half edge - bottom part

         (f-v (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; horizontal half edge - left part
         (f-h (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
         (f-m (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
         (g (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
         (g-h (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
         (g-v (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
         (g-m (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0));; horizontal half edge - right part

         (k-v (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) ;; mismatch half edge - left part
         (k-h (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
         (k-m (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
         (l (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
         (l-v (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
         (l-h (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
         (l-m (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0));; mismatch half edge - right part
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
    (cost-assignment pattern source nx ny P Q R S a b c d-v d-h d-m e e-v e-h e-m f-v f-h f-m g g-v g-h g-m k-v k-h k-m l l-v l-h l-m
                           :match-cost match-cost
                           :mismatch-opening-cost mismatch-opening-cost
                           :mismatch-cost mismatch-cost
                           :gap-opening-cost gap-opening-cost
                           :gap-cost gap-cost)
    (edge-assignment nx ny a b c d-v d-h d-m e e-v e-h e-m f-v f-h f-m g g-v g-h g-m k-v k-h k-m l l-v l-h l-m)    

    ;; Trace back pointers from the bottom-right cell to the top-left cell.
    ;; Cells may contain multiple pointers, so there may be multiple paths.
    ;; Return all alignments and optionally remove duplicates.
    (let* ((optimal-cost (aref R nx ny))
           (all-optimal-alignments
            (extract-optimal-alignments pattern source a b c d-v d-h d-m e e-v e-h e-m f-v f-h f-m g g-v g-h g-m k-v k-h k-m l l-v l-h l-m
                                        pattern-boundaries
                                        source-boundaries
                                        optimal-cost
                                        :match-cost match-cost
                                        :mismatch-cost mismatch-cost
                                        :mismatch-opening-cost mismatch-opening-cost
                                        :gap-opening-cost gap-opening-cost
                                        :gap-cost gap-cost
                                        :n-optimal-alignments n-optimal-alignments
                                        :max-nr-of-au-gaps max-nr-of-au-gaps)))
      (if remove-duplicate-alignments
        (remove-duplicates all-optimal-alignments :key #'match-positions :test #'equal)
        all-optimal-alignments))))

(defun cost-assignment (pattern source nx ny P Q R S a b c d-v d-h d-m e e-v e-h e-m f-v f-h f-m g g-v g-h g-m k-v k-h k-m l l-v l-h l-m 
                                &key (match-cost -1) (mismatch-opening-cost 1) (mismatch-cost 1)
                                (gap-opening-cost 5) (gap-cost 1))
  (loop for i from 0 to nx
        do (loop for j from 0 to ny
                 for matchp = (when (and (> i 0) (> j 0))
                                (eql (nth (- i 1) pattern) (nth (- j 1) source)))
                 do
                   ;; 1) find the minimum cost of a path ending at node N_{i,j} using vertical edge
                   ;; 2) determine if cost P_{i,j} can be achieved with and without edge V_{i-1,j}, i.e. vertical edge before vertical,
                   ;;    with and without edge H_{i-1,j}, i.e. horizontal edge before vertical, and
                   ;;    with and without edge D_{i-1,j}, i.e. diagonal edge before vertical
                   (when (> i 0)
                     (let* ((extended-gap-cost (+ (aref P (- i 1) j) gap-cost))
                            (extended-gap-cost-other-side (+ (aref Q (- i 1) j) gap-cost))
                            (extended-mismatch (+ (aref S (- i 1) j) gap-cost))
                            (new-gap-cost (+ (aref R (- i 1) j) gap-opening-cost gap-cost))
                            (min-cost (min extended-gap-cost extended-gap-cost-other-side extended-mismatch new-gap-cost)))
                       (setf (aref P i j) min-cost)
                       ;; Here, we are inserting a vertical edge
                       ;; If the min-cost was extended-gap-cost, then we have 2 consecutive vertical edges to reach i j
                       ;; -> set d_v i-1 j to 1
                       ;; If the min-cost was new-gap-cost, then we know that the optimal path to reach i j did not come from
                       ;; a vertical edge, because we are opening the new gap only now
                       ;; -> set e i-1 j to 1
                       ;; If the min-cost was extended-gap-cost-other-side, then we know that the optimal path to reach i j
                       ;; is a horizontal edge followed by a vertical edge
                       ;; -> set f_v i-1 j to 1
                       ;; If the min-cost was extended-mismatch, then we know that the optimal path to reach i j
                       ;; is a diagonal edge followed by a vertical edge
                       ;; -> set k_v i-1 j to 1
                       (when (= min-cost extended-gap-cost)
                         (setf (aref d-v (- i 1) j) 1))
                       (when (= min-cost new-gap-cost)
                         (setf (aref e (- i 1) j) 1))
                       (when (= min-cost extended-gap-cost-other-side)
                         (setf (aref f-v (- i 1) j) 1))
                       (when (= min-cost extended-mismatch)
                         (setf (aref k-v (- i 1) j) 1))))

                   ;; 3) find the minimum cost of a path ending at node N_{i,j} using horizontal edge
                   ;; 4) determine if cost Q_{i,j} can be achieved with and without edge H_{i,j-1}, i.e. horizontal edge before horizontal,
                   ;;    with and without edge V_{i,j-1}, i.e. vertical edge before horizontal, and
                   ;;    with and without edge D_{i,j-1}, i.e. diagonal edge before horizontal
                   (when (> j 0)
                     (let* ((extended-gap-cost (+ (aref Q i (- j 1)) gap-cost))
                            (extended-gap-cost-other-side (+ (aref P i (- j 1)) gap-cost))
                            (extended-mismatch (+ (aref S i (- j 1)) gap-cost))
                            (new-gap-cost (+ (aref R i (- j 1)) gap-opening-cost gap-cost))
                            (min-cost (min extended-gap-cost extended-gap-cost-other-side extended-mismatch new-gap-cost)))
                       (setf (aref Q i j) min-cost)
                       (when (= min-cost extended-gap-cost)
                         (setf (aref f-h i (- j 1)) 1))
                       (when (= min-cost new-gap-cost)
                         (setf (aref g i (- j 1)) 1))
                       (when (= min-cost extended-gap-cost-other-side)
                         (setf (aref d-h i (- j 1)) 1))
                       (when (= min-cost extended-mismatch)
                         (setf (aref k-h i (- j 1)) 1))))

                   ;; 5) find the minimum cost of a path ending at node N_{i,j} using diagonal edge with mismatch
                   ;; 6) determine if cost S_{i,j} can be achieved with and without edge S_{i-1,j-1}, i.e. diagonal edge before diagonal,
                   ;;    with and without edge V_{i-1,j-1}, i.e. vertical edge before diagonal, and
                   ;;    with and without edge H_{i-1,j-1}, i.e. horizontal edge before diagonal
                   (when (and (> i 0) (> j 0))
                     (let* ((extended-mismatch-cost
                             (if (not matchp) ;; check for mismatch
                               (+ (aref S (- i 1) (- j 1)) mismatch-cost)
                               +inf))
                            (extended-vertical-gap-cost
                             (if (not matchp)
                               (+ (aref P (- i 1) (- j 1)) mismatch-cost)
                               +inf))
                            (extended-horizontal-gap-cost
                             (if (not matchp)
                               (+ (aref Q (- i 1) (- j 1)) mismatch-cost)
                               +inf))
                            (new-mismatch-cost
                             (if (not matchp) ;; check for mismatch
                               (+ (aref R (- i 1) (- j 1)) mismatch-opening-cost mismatch-cost)
                               +inf))
                            (min-cost (min extended-mismatch-cost extended-vertical-gap-cost extended-horizontal-gap-cost new-mismatch-cost)))
                       (setf (aref S i j) min-cost)
                       ;; do NOT set k and l when no mismatch...
                       (when (and (= min-cost extended-mismatch-cost)
                                  (not (= min-cost +inf)))
                         (setf (aref k-m (- i 1) (- j 1)) 1))
                       (when (and (= min-cost new-mismatch-cost)
                                  (not (= min-cost +inf)))
                         (setf (aref l (- i 1) (- j 1)) 1))
                       (when (and (= min-cost extended-vertical-gap-cost)
                                  (not (= min-cost +inf)))
                         (setf (aref d-m (- i 1) (- j 1)) 1))
                       (when (and (= min-cost extended-horizontal-gap-cost)
                                  (not (= min-cost +inf)))
                         (setf (aref f-m (- i 1) (- j 1)) 1))))
                     
                   ;; 7) find the minimum cost of a path ending at node N_{i,j}
                   ;; 8) determine if cost R_{i,j} can be achieved by vertical, horizontal or diagonal edges
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

(defun edge-assignment (nx ny a b c d-v d-h d-m e e-v e-h e-m f-v f-h f-m g g-v g-h g-m k-v k-h k-m l l-v l-h l-m)
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

                       ;; IF there is a vertical edge to N i+1 j (a i+1 j == 1)
                       ;; and this vertical edge is preceded by another vertical edge (d_v i j == 1)
                       ;; THEN vertical edge to N i+1 j HAS to be preceded by a vertical edge (setf d i+1 j to 1)
                       ;; if there is no horizontal edge entering N i j (f-v i j == 0)
                       ;; no diagonal edge entering N i j (k-v i j == 0)
                       ;; and all optimal paths to N i+1 j make use of N i j (e i j == 0)
                       ;; THEN vertical edge to N i j HAS to be followed by a vertical edge (setf e i j to 1)
                       ;; if there are no other optimal paths that enter N i j (a i j = 0)
                       ;; because we don't know how in what direction they leave N i j
                       ;; THEN we set a vertical edge to N i j (setf a i j to 1)
                       ;; ELSE
                       ;; the vertical edge to N i+1 j is NOT preceded by a vertical edge (setf d i+1 j to 0)
                       ;; the vertical edge to N i j is NOT followed by a vertical edge (setf e i j to 0)
                       (if (and (= (aref a (+ i 1) j) 1) (= (aref d-v i j) 1))
                         (setf (aref d-v (+ i 1) j) (- 1 (max (aref f-v i j) (aref k-v i j) (aref e i j)))
                               (aref e-v i j) (- 1 (aref a i j))
                               (aref a i j) 1)
                         (setf (aref d-v (+ i 1) j) 0 
                               (aref e-v i j) 0))


                       ;; 4) if edge H_{i,j+1} is in an optimal path and requires edge V_{i,j} to be in an optimal path,
                       ;;    determine if an optimal path that uses edge H_{i,j+1} must use edge V_{i,j} and the converse:
                       
                       ;; IF there is a horizontal edge to N i j+1 (b i j+1 == 1)
                       ;: and this horizontal edge is preceded by a vertical edge (d_h i j == 1)
                       ;; THEN horizontal edge to N i j+1 HAS to be preceded by a vertical edge (setf d-h i j+1 to 1)
                       ;; if there is no horizontal edge entering N i j (f-h i j == 0)
                       ;; no diagonal edge entering N i j (k-h i j == 0)
                       ;; and all optimal paths to N i j+1 make use of N i j (g i j = 0)
                       ;; THEN we set a vertical edge to N i j (setf a i j to 1)
                       ;; ELSE
                       ;; the horizontal edge to N i j+1 is NOT preceded by a vertical edge (setf d i j+1 to 0)
                       (if (and (= (aref b i (+ j 1)) 1) (= (aref d-h i j) 1))
                         (setf (aref d-h i (+ j 1)) (- 1 (max (aref f-h i j) (aref k-h i j) (aref g i j)))
                               (aref e-h i j) (- 1 (aref a i j))  ;; we cannot say anything about e or g or l here
                               (aref a i j) 1)
                         (setf (aref d-h i (+ j 1)) 0 
                               (aref e-h i j) 0)  ;; we cannot say anything about e or g or l here
                         )

                       ;; 5) if edge D_{i+1,j+1} is in an optimal path and requires edge V_{i,j} to be in an optimal path,
                       ;;    determine if an optimal path that uses edge D_{i+1,j+1} must use edge V_{i,j} and the converse:

                       ;; IF there is a diagonal edge to N i+1 j+1 (c i+1 j+1 == 1)
                       ;; and this diagonal edge is preceded by a vertical edge (d_m i j == 1)
                       ;; THEN diagonal edge to N i+1 j+1 HAS to be preceded by a vertical edge (setf d-m i+1 j+1 to 1)
                       ;; if there is no horizontal edge entering N i j (f-m i j == 0)
                       ;; no diagonal edge entering N i j (k-m i j == 0)
                       ;; and all optimal paths to N i+1 j+1 make use of N i j (l i j == 0)
                       ;; THEN we set a vertical edge to N i j (setf a i j to 1)
                       ;; ELSE
                       ;; the diagonal edge to N i+1 j+1 is NOT preceded by a vertical edge (setf d-m i+1 j+1 to 0)
                       (if (and (= (aref c (+ i 1) (+ j 1)) 1) (= (aref d-m i j) 1))
                         (setf (aref d-m (+ i 1) (+ j 1)) (- 1 (max (aref f-m i j) (aref k-m i j) (aref l i j)))
                               (aref e-m i j) (- 1 (aref a i j))  ;; we cannot say anything about e or g or l here
                               (aref a i j) 1)
                         (setf (aref d-m (+ i 1) (+ j 1)) 0 
                               (aref e-m i j) 0)  ;; we cannot say anything about e or g or l here
                         )
                       
                       ;; 6) if edge H_{i,j+1} is in an optimal path and requires edge H_{i,j} to be in an optimal path,
                       ;;    determine if an optimal path that uses edge H_{i,j+1} must use edge H_{i,j} and the converse:
                       (if (and (= (aref b i (+ j 1)) 1) (= (aref f-h i j) 1))
                         (setf (aref f-h i (+ j 1)) (- 1 (max (aref d-h i j) (aref k-h i j) (aref g i j)))
                               (aref g-h i j) (- 1 (aref b i j))
                               (aref b i j) 1)
                         (setf (aref f-h i (+ j 1)) 0
                               (aref g-h i j) 0))
                       
                       (if (and (= (aref a (+ i 1) j) 1) (= (aref f-v i j) 1))
                         (setf (aref f-v (+ i 1) j) (- 1 (max (aref d-v i j) (aref k-v i j) (aref e i j)))
                               (aref g-v i j) (- 1 (aref b i j))
                               (aref b i j) 1)
                         (setf (aref f-v (+ i 1) j) 0
                               (aref g-v i j) 0)
                         )
                       
                       (if (and (= (aref c (+ i 1) (+ j 1)) 1) (= (aref f-m i j) 1))
                         (setf (aref f-m (+ i 1) (+ j 1)) (- 1 (max (aref d-m i j) (aref k-m i j) (aref l i j)))
                               (aref g-m i j) (- 1 (aref b i j))
                               (aref b i j) 1)
                         (setf (aref f-m (+ i 1) (+ j 1)) 0
                               (aref g-m i j) 0)
                         )
                       
                       
                       ;; 7) if edge D_{i+1,j+1} is in an optimal path and requires edge D_{i,j} to be in an optimal path,
                       ;;    determine if an optimal path that uses edge D_{i+1,j+1} must use edge D_{i,j} and the converse:
                       (if (and (= (aref c (+ i 1) (+ j 1)) 1) (= (aref k-m i j) 1))
                         (setf (aref k-m (+ i 1) (+ j 1)) (- 1 (aref d-m i j) (aref f-m i j) (aref l i j))
                               (aref l-m i j) (- 1 (aref c i j))
                               (aref c i j) 1)
                         (setf (aref k-m (+ i 1) (+ j 1)) 0
                               (aref l-m i j) 0))
                       
                       (if (and (= (aref b i (+ j 1)) 1) (= (aref k-h i j) 1))
                         (setf (aref k-h i (+ j 1)) (- 1 (aref d-h i j) (aref f-h i j) (aref g i j))
                               (aref l-h i j) (- 1 (aref c i j))
                               (aref c i j) 1)
                         (setf (aref k-h i (+ j 1)) 0
                               (aref l-h i j) 0)
                         )
                       
                       (if (and (= (aref a (+ i 1) j) 1) (= (aref k-v i j) 1))
                         (setf (aref k-v (+ i 1) j) (- 1 (aref d-v i j) (aref f-v i j) (aref e i j))
                               (aref l-v i j) (- 1 (aref c i j))
                               (aref c i j) 1)
                           (setf (aref k-v (+ i 1) j) 0
                                 (aref l-v i j) 0)
                           ))))))

(defun extract-optimal-alignments (pattern source a b c d-v d-h d-m e e-v e-h e-m f-v f-h f-m g g-v g-h g-m k-v k-h k-m l l-v l-h l-m
                                           pattern-boundaries source-boundaries
                                           optimal-cost
                                           &key (match-cost -1)
                                           (mismatch-cost 1)
                                           (mismatch-opening-cost 1)
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
                             (let ((next-state (check-vertical-edges pattern source pattern-boundaries source-boundaries state
                                                                     a b c d-v d-h d-m e e-v e-h e-m f-v f-h f-m g g-v g-h g-m k-v k-h k-m l l-v l-h l-m
                                                                     :gap-opening-cost gap-opening-cost :gap-cost gap-cost)))
                               (when next-state
                                 (list next-state))))
                            
                            (;; next-edge is set to horizontal -> only need to check horizontal edges
                             (eql next-edge 'horizontal)
                             ;; as a sanity check, we could assert that vertical and diagonal edges here are 0
                             (let ((next-state (check-horizontal-edges pattern source pattern-boundaries source-boundaries state
                                                                       a b c d-v d-h d-m e e-v e-h e-m f-v f-h f-m g g-v g-h g-m k-v k-h k-m l l-v l-h l-m
                                                                       :gap-opening-cost gap-opening-cost :gap-cost gap-cost)))
                               (when next-state
                                 (list next-state))))

                            (;; next-edge is set to diagonal-mismatch
                             (eql next-edge 'diagonal-mismatch)
                             (let ((next-state (check-diagonal-edges pattern source pattern-boundaries source-boundaries state
                                                                     a b c d-v d-h d-m e e-v e-h e-m f-v f-h f-m g g-v g-h g-m k-v k-h k-m l l-v l-h l-m
                                                                     :match-cost match-cost :mismatch-cost mismatch-cost
                                                                     :mismatch-opening-cost mismatch-opening-cost)))
                               (when next-state
                                 (list next-state))))
                            
                            (;; next-edge is not set -> check vertical, horizontal and diagonal edges
                             t
                             (let ((next-state-diagonal (check-diagonal-edges pattern source pattern-boundaries source-boundaries state
                                                                              a b c d-v d-h d-m e e-v e-h e-m f-v f-h f-m g g-v g-h g-m k-v k-h k-m l l-v l-h l-m
                                                                              :match-cost match-cost :mismatch-cost mismatch-cost
                                                                              :mismatch-opening-cost mismatch-opening-cost))
                                   (next-state-vertical (check-vertical-edges pattern source pattern-boundaries source-boundaries state
                                                                              a b c d-v d-h d-m e e-v e-h e-m f-v f-h f-m g g-v g-h g-m k-v k-h k-m l l-v l-h l-m
                                                                              :gap-opening-cost gap-opening-cost :gap-cost gap-cost))
                                   (next-state-horizontal (check-horizontal-edges pattern source pattern-boundaries source-boundaries state
                                                                                  a b c d-v d-h d-m e e-v e-h e-m f-v f-h f-m g g-v g-h g-m k-v k-h k-m l l-v l-h l-m
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
            #|(loop for solution in solutions
                  do (assert (= (cost solution) optimal-cost) ()
                       "The cost obtained by retracing the optimal alignment (~a) is not equal to the optimal cost from the cost matrix (~a)~%~a~%~a"
                       (cost solution) optimal-cost
                       (aligned-pattern solution)
                       (aligned-source solution)))|#
            (return solutions))))

(defun check-diagonal-edges (pattern source pattern-boundaries source-boundaries state a b c d-v d-h d-m e e-v e-h e-m f-v f-h f-m g g-v g-h g-m k-v k-h k-m l l-v l-h l-m
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
             (new-mismatch (if (or (eql prev-edge 'diagonal-mismatch)
                                   (eql prev-edge 'horizontal)
                                   (eql prev-edge 'vertical))
                             mismatch-cost (+ mismatch-cost mismatch-opening-cost)))
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
        (cond ((= (aref k-m i j) 1)
               (setf (next-edge next-state) 'diagonal-mismatch))
              ((= (aref d-m i j) 1)
               (setf (next-edge next-state) 'vertical))
              ((= (aref f-m i j) 1)
               (setf (next-edge next-state) 'horizontal)))


        (cond ((= (aref l-m i j) 1)
               (unless (eql prev-edge 'diagonal-mismatch)
                 (setf next-state nil)))
              ((= (aref l-h i j) 1)
               (unless (eql prev-edge 'horizontal)
                 (setf next-state nil)))
              ((= (aref l-v i j) 1)
               (unless (eql prev-edge 'vertical)
                 (setf next-state nil))))
        
        ;; return the next state
        next-state))))

(defun check-vertical-edges (pattern source pattern-boundaries source-boundaries state a b c d-v d-h d-m e e-v e-h e-m f-v f-h f-m g g-v g-h g-m k-v k-h k-m l l-v l-h l-m
                                     &key (gap-opening-cost 5) (gap-cost 1))
  (with-slots (aligned-pattern aligned-source
               aligned-pattern-boundaries aligned-source-boundaries
               i j cost match-positions gap-counter prev-edge next-edge) state
    (when (= (aref a i j) 1)  ;; check if there is a vertical edge in this state
      (let* ((expanded-pattern (cons (nth (- i 1) pattern) aligned-pattern))
             (expanded-source (cons #\_ aligned-source))
             (current-left-source-boundary (car (first aligned-source-boundaries)))
             (current-left-pattern-boundary (car (first aligned-pattern-boundaries)))
             (source-boundary-vars (make-boundary-indices nil source-boundaries current-left-source-boundary :gap t))
             (pattern-boundary-vars (make-boundary-vars i pattern-boundaries current-left-pattern-boundary))
             (new-gap-p (not (or (eql (first aligned-source) #\_)
                                 (eql (first aligned-pattern) #\_)
                                 (eql prev-edge 'diagonal-mismatch))))  ;; new gap in terms of nv (i.e. a _)
             (gap-counter-new-gap-p (equal (first match-positions) (cons (+ i 1) (+ j 1))))  ;; gap counter in terms of AU gaps
             (cost-increase (if new-gap-p (+ gap-cost gap-opening-cost) gap-cost))
             (next-state (make-instance 'sequence-alignment-state
                                        :aligned-pattern expanded-pattern
                                        :aligned-source expanded-source
                                        :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                        :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                        :i (- i 1) :j j
                                        :cost (+ cost cost-increase)
                                        :match-positions match-positions
                                        :gap-counter (if gap-counter-new-gap-p (+ 1 gap-counter) gap-counter)
                                        :prev-edge 'vertical)))
        ;; when d is set, the next edge has to be vertical
        (cond ((= (aref d-v i j) 1)
               (setf (next-edge next-state) 'vertical))
              ((= (aref f-v i j) 1)
               (setf (next-edge next-state) 'horizontal))
              ((= (aref k-v i j) 1)
               (setf (next-edge next-state) 'diagonal-mismatch)))

       
        
        ;; when e is set, the prev edge has to be vertical
        ;; if not, remove the next state!
        (cond ((= (aref e-v i j) 1)
               (unless (eql prev-edge 'vertical)
                 (setf next-state nil)))
              ((= (aref e-h i j) 1)
               (unless (eql prev-edge 'horizontal)
                 (setf next-state nil)))
              ((= (aref e-m i j) 1)
               (unless (eql prev-edge 'diagonal-mismatch)
                 (setf next-state nil))))
        ;; return the next state
        next-state))))

(defun check-horizontal-edges (pattern source pattern-boundaries source-boundaries state a b c d-v d-h d-m e e-v e-h e-m f-v f-h f-m g g-v g-h g-m k-v k-h k-m l l-v l-h l-m
                                       &key (gap-opening-cost 5) (gap-cost 1))
  (with-slots (aligned-pattern aligned-source
               aligned-pattern-boundaries aligned-source-boundaries
               i j cost match-positions gap-counter prev-edge next-edge) state
    (when (= (aref b i j) 1)  ;; check if there is a horizontal edge in this state
      (let* ((expanded-pattern (cons #\_ aligned-pattern))
             (expanded-source (cons (nth (- j 1) source) aligned-source))
             (current-left-source-boundary (car (first aligned-source-boundaries)))
             (current-left-pattern-boundary (car (first aligned-pattern-boundaries)))
             (source-boundary-vars (make-boundary-indices j source-boundaries current-left-source-boundary))
             (pattern-boundary-vars (make-boundary-vars nil pattern-boundaries current-left-pattern-boundary :gap t))
             (new-gap-p (not (or (eql (first aligned-pattern) #\_)
                                 (eql (first aligned-source) #\_)
                                 (eql prev-edge 'diagonal-mismatch))))  ;; new gap in terms of nv (i.e. a _)
             (gap-counter-new-gap-p (equal (first match-positions) (cons (+ i 1) (+ j 1))))  ;; gap counter in terms of AU gaps
             (cost-increase (if new-gap-p (+ gap-cost gap-opening-cost) gap-cost))
             (next-state (make-instance 'sequence-alignment-state
                                        :aligned-pattern expanded-pattern
                                        :aligned-source expanded-source
                                        :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                        :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                        :i i :j (- j 1)
                                        :cost (+ cost cost-increase)
                                        :match-positions match-positions
                                        :gap-counter (if gap-counter-new-gap-p (+ 1 gap-counter) gap-counter)
                                        :prev-edge 'horizontal)))
        ;; when f is set, the next edge has to be horizontal
        (cond ((= (aref f-h i j) 1)
               (setf (next-edge next-state) 'horizontal))
              ((= (aref d-h i j) 1)
               (setf (next-edge next-state) 'vertical))
              ((= (aref k-h i j) 1)
               (setf (next-edge next-state) 'diagonal-mismatch)))

        ;; when g is set, the prev edge has to be horizontal
        ;; if not, remove the next state!
        (cond ((= (aref g-h i j) 1)
               (unless (eql prev-edge 'horizontal)
                 (setf next-state nil)))
              ((= (aref g-v i j) 1)
               (unless (eql prev-edge 'vertical)
                 (setf next-state nil)))
              ((= (aref g-m i j) 1)
               (unless (eql prev-edge 'diagonal-mismatch)
                 (setf next-state nil))))
        ;; return the next state
        next-state))))

#|

 ;; cost should be 3
(print-sequence-alignments (maximal-sequence-alignments "AGAT" "CTCT" nil nil
                             :match-cost -1 :mismatch-cost 1 :mismatch-opening-cost 1 :gap-opening-cost 1 :gap-cost 1
                             :remove-duplicate-alignments nil))

 ;; cost should be 3
(print-sequence-alignments (maximal-sequence-alignments "BAGAT" "CGCT" nil nil
                             :match-cost -1 :mismatch-cost 1  :mismatch-opening-cost 1 :gap-opening-cost 1 :gap-cost 1
                             :remove-duplicate-alignments nil))

 ;; cost should be 0  (50 solutions)
 (print-sequence-alignments (maximal-sequence-alignments "what color is the sphere" "what size is the cube" nil nil
                             :match-cost -1 :mismatch-cost 1  :mismatch-opening-cost 2 :gap-opening-cost 2 :gap-cost 1
                             :remove-duplicate-alignments nil))

 ;; optimale cost hier: 2
 (print-sequence-alignments (maximal-sequence-alignments "xabcy" "xefy" nil nil
                             :match-cost -1 :mismatch-cost 1  :mismatch-opening-cost 1 :gap-opening-cost 1 :gap-cost 1
                             :remove-duplicate-alignments nil))

  ;; optimale cost hier: 2 (9 solutions)
 (print-sequence-alignments (maximal-sequence-alignments "xabcyklz" "xefydefz" nil nil
                             :match-cost -2 :mismatch-cost 1  :mismatch-opening-cost 1 :gap-opening-cost 1 :gap-cost 1
                             :remove-duplicate-alignments nil))

  ;; optimale cost hier: 4 (9 solutions)
(print-sequence-alignments (maximal-sequence-alignments "xabcykl" "xefydef" nil nil
                             :match-cost -2 :mismatch-cost 1  :mismatch-opening-cost 1 :gap-opening-cost 1 :gap-cost 1
                             :remove-duplicate-alignments nil))
 
;; optimale cost hier: 14 (48 solutions)
 (print-sequence-alignments (maximal-sequence-alignments "how many cubes are there" "what size is the cube" nil nil
                             :match-cost -1 :mismatch-cost 1  :mismatch-opening-cost 1 :gap-opening-cost 1 :gap-cost 1
                             :remove-duplicate-alignments nil))


 ;; optimale cost hier 29, 6 solutions
 (print-sequence-alignments (maximal-sequence-alignments "AGAT" "CTCT" nil nil
                             :match-cost -10 :mismatch-cost 10 :mismatch-opening-cost 1 :gap-opening-cost 25 :gap-cost 1
                             :remove-duplicate-alignments nil))


 (print-sequence-alignments (maximal-sequence-alignments "what size is the red ball" "what size is the blue cube" nil nil
                             :match-cost -1 :mismatch-cost 1  :mismatch-opening-cost 2 :gap-opening-cost 2 :gap-cost 1
                             :remove-duplicate-alignments nil))

 
 GA__
__TC

__GA
TC__

G__A
_TC_

_GA_
T__C

_G_A
T_C_

G_A_
_T_C
 
|#