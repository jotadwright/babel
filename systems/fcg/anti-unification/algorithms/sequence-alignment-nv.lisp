; (ql:quickload :fcg)
(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maximal Sequence Alignment - nv algorithm - mismatches ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod maximal-sequence-alignments ((pattern string) (source string) (pattern-boundaries list) (source-boundaries list) (mode (eql :nv))
                                        &key (match-cost -1) (mismatch-cost 1) (mismatch-opening-cost 1) (gap-opening-cost 5) (gap-cost 1)
                                        (remove-duplicate-alignments t) n-optimal-alignments max-nr-of-au-gaps (debugging nil))
  (maximal-sequence-alignments (coerce pattern 'list) (coerce source 'list)
                               pattern-boundaries source-boundaries mode
                               :match-cost match-cost
                               :mismatch-cost mismatch-cost
                               :mismatch-opening-cost mismatch-opening-cost
                               :gap-opening-cost gap-opening-cost
                               :gap-cost gap-cost
                               :remove-duplicate-alignments remove-duplicate-alignments
                               :n-optimal-alignments n-optimal-alignments
                               :max-nr-of-au-gaps max-nr-of-au-gaps
                               :debugging debugging))


(defmethod maximal-sequence-alignments ((pattern list) (source list) (pattern-boundaries list) (source-boundaries list) (mode (eql :nv))
                                        &key (match-cost -1) (mismatch-cost 1) (mismatch-opening-cost 1) (gap-opening-cost 5) (gap-cost 1)
                                        (remove-duplicate-alignments t) n-optimal-alignments max-nr-of-au-gaps (debugging nil))
  (reset-id-counters)
  (let* ((nx (length pattern)) ;; number of rows
         (ny (length source))  ;; number of columns
         ;; matrices to store costs
         (P (make-array (list (+ nx 1) (+ ny 1))))
         (Q (make-array (list (+ nx 1) (+ ny 1))))
         (R (make-array (list (+ nx 1) (+ ny 1))))
         (S (make-array (list (+ nx 1) (+ ny 1)))) ;; introduce new matrix S to keep track of opened mismatches
         ;; matrices to store graph edges
         (arrays (make-instance 'arrays :nx nx :ny ny))
         (boundary-matrix (make-boundary-vars-matrix nx ny)))
         
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
    (setf (aref (diagonal arrays) (+ nx 1) (+ ny 1)) 1)

    ;; Run the Gotoh algorithm according to the implementation
    ;; provided by Altschul and Ericksson (1986)
    (nv-cost-assignment pattern source nx ny P Q R S arrays match-cost mismatch-opening-cost mismatch-cost gap-opening-cost gap-cost)
    
    (when debugging 
      (visualise-arrays pattern source arrays))
    
    (nv-edge-assignment pattern source nx ny arrays)

    (when debugging
      (visualise-arrays pattern source arrays))

    ;; Trace back pointers from the bottom-right cell to the top-left cell.
    ;; Cells may contain multiple pointers, so there may be multiple paths.
    ;; Return all alignments and optionally remove duplicates.
    (let* ((optimal-cost (aref R nx ny))
           (all-optimal-alignments
            (nv-extract-optimal-alignments pattern source arrays
                                           pattern-boundaries
                                           source-boundaries
                                           optimal-cost
                                           match-cost
                                           mismatch-cost
                                           mismatch-opening-cost
                                           gap-opening-cost
                                           gap-cost
                                           boundary-matrix
                                           :n-optimal-alignments n-optimal-alignments
                                           :max-nr-of-au-gaps max-nr-of-au-gaps
                                           :debugging debugging)))
      ;(check-optimal-alignments all-optimal-alignments arrays)
      (when debugging
        (loop for alignment in all-optimal-alignments
              do (visualise-path (path alignment))))
      (if remove-duplicate-alignments
        (remove-duplicates all-optimal-alignments :key #'match-positions :test #'equal)
        all-optimal-alignments))))


(defclass arrays ()
  ((vertical :initarg :vertical :accessor vertical :initform nil :type array)
   (horizontal :initarg :horizontal :accessor horizontal :initform nil :type array)
   (diagonal :initarg :diagonal :accessor diagonal :initform nil :type array)
   (v-v :initarg :v-v :accessor v-v :initform nil :type array) ;;after cost assignment: v-v = 1 iff among (i+1,j) paths through N_i,j, an optimal one uses V_i,j
   (v-h :initarg :v-h :accessor v-h :initform nil :type array) ;;after cost assignment: v-h = 1 iff among (i,j+1) paths through N_i,j, an optimal one uses V_i,j
   (v-m :initarg :v-m :accessor v-m :initform nil :type array) ;;after cost assignment: v-m = 1 iff among (i+1,j+1) paths through N_i,j, an optimal one uses V_i,j
   (v-new :initarg :v-new :accessor v-new :initform nil :type array) ;;after cost assignment: v-new iff among (i+1,j) paths through N_i,j, an optimal one does not use V_i,j
   (h-v :initarg :h-v :accessor h-v :initform nil :type array) ;;after cost assignment: h-v = 1 iff among (i+1,j) paths through N_i,j, an optimal one uses H_i,j
   (h-h :initarg :h-h :accessor h-h :initform nil :type array) ;;after cost assignment: h-h = 1 iff among (i,j+1) paths through N_i,j, an optimal one uses H_i,j
   (h-m :initarg :h-m :accessor h-m :initform nil :type array) ;;after cost assignment: h-m = 1 iff among (i+1,j+1) paths through N_i,j, an optimal one uses H_i,j
   (h-new :initarg :h-new :accessor h-new :initform nil :type array) ;;after cost assignment: h-new iff among (i,j+1) paths through N_i,j, an optimal one does not use H_i,j
   (d-v :initarg :d-v :accessor d-v :initform nil :type array) ;;after cost assignment: d-v = 1 iff among (i+1,j) paths through N_i,j, an optimal one uses D_i,j
   (d-h :initarg :d-h :accessor d-h :initform nil :type array) ;;after cost assignment: d-h = 1 iff among (i,j+1) paths through N_i,j, an optimal one uses D_i,j
   (d-m :initarg :d-m :accessor d-m :initform nil :type array) ;;after cost assignment: v-h = 1 iff among (i+1,j+1) paths through N_i,j, an optimal one uses D_i,j
   (d-new :initarg :d-new :accessor d-new :initform nil :type array) ;;after cost assignment: d-new iff among (i+1,j+1) paths through N_i,j, an optimal one does not use D_i,j
   ;; --> but we don't know this, because it can be a diagonal match!!
   (e-v :initarg :e-v :accessor e-v :initform nil :type array) ;;after cost assignment: v-v = 1 iff among (i+1,j) paths through N_i,j, an optimal one uses V_i,j
   (e-h :initarg :e-h :accessor e-h :initform nil :type array) ;;after cost assignment: v-h = 1 iff among (i,j+1) paths through N_i,j, an optimal one uses V_i,j
   (e-m :initarg :e-m :accessor e-m :initform nil :type array) ;;after cost assignment: v-m = 1 iff among (i+1,j+1) paths through N_i,j, an optimal one uses V_i,j
   (g-v :initarg :g-v :accessor g-v :initform nil :type array) ;;after cost assignment: v-v = 1 iff among (i+1,j) paths through N_i,j, an optimal one uses V_i,j
   (g-h :initarg :g-h :accessor g-h :initform nil :type array) ;;after cost assignment: v-h = 1 iff among (i,j+1) paths through N_i,j, an optimal one uses V_i,j
   (g-m :initarg :g-m :accessor g-m :initform nil :type array) ;;after cost assignment: v-m = 1 iff among (i+1,j+1) paths through N_i,j, an optimal one uses V_i,j
   (l-v :initarg :l-v :accessor l-v :initform nil :type array) ;;after cost assignment: v-v = 1 iff among (i+1,j) paths through N_i,j, an optimal one uses V_i,j
   (l-h :initarg :l-h :accessor l-h :initform nil :type array) ;;after cost assignment: v-h = 1 iff among (i,j+1) paths through N_i,j, an optimal one uses V_i,j
   (l-m :initarg :l-m :accessor l-m :initform nil :type array) ;;after cost assignment: v-m = 1 iff among (i+1,j+1) paths through N_i,j, an optimal one uses V_i,j
   ))

(defmethod initialize-instance :after ((arrays arrays) &key nx ny)
  (setf (vertical arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) 
  (setf (horizontal arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)) 
  (setf (diagonal arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (v-v arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (v-h arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (v-m arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (h-v arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (h-h arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (h-m arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (d-v arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (d-h arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (d-m arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (v-new arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (h-new arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (d-new arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (e-v arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (e-h arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (e-m arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (g-v arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (g-h arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (g-m arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (l-v arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (l-h arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0))
  (setf (l-m arrays) (make-array (list (+ nx 2) (+ ny 2)) :initial-element 0)))


(defun nv-cost-assignment (pattern source nx ny P Q R S arrays match-cost mismatch-opening-cost mismatch-cost gap-opening-cost gap-cost)
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
                         (setf (aref (v-v arrays) (- i 1) j) 1))
                       (when (= min-cost new-gap-cost)
                         (setf (aref (v-new arrays) (- i 1) j) 1))
                       (when (= min-cost extended-gap-cost-other-side)
                         (setf (aref (h-v arrays) (- i 1) j) 1))
                       (when (= min-cost extended-mismatch)
                         (setf (aref (d-v arrays) (- i 1) j) 1))))

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
                         (setf (aref (h-h arrays) i (- j 1)) 1))
                       (when (= min-cost new-gap-cost)
                         (setf (aref (h-new arrays) i (- j 1)) 1))
                       (when (= min-cost extended-gap-cost-other-side)
                         (setf (aref (v-h arrays) i (- j 1)) 1))
                       (when (= min-cost extended-mismatch)
                         (setf (aref (d-h arrays) i (- j 1)) 1))))

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
                       (when (and (not matchp)
                                  (= min-cost extended-mismatch-cost)
                                  (not (= min-cost +inf)))
                         (setf (aref (d-m arrays) (- i 1) (- j 1)) 1))
                       (when (and (not matchp)
                                  (= min-cost new-mismatch-cost)
                                  (not (= min-cost +inf)))
                         (setf (aref (d-new arrays) (- i 1) (- j 1)) 1))
                       (when (and (not matchp)
                                  (= min-cost extended-vertical-gap-cost)
                                  (not (= min-cost +inf)))
                         (setf (aref (v-m arrays) (- i 1) (- j 1)) 1))
                       (when (and (not matchp)
                                  (= min-cost extended-horizontal-gap-cost)
                                  (not (= min-cost +inf)))
                         (setf (aref (h-m arrays) (- i 1) (- j 1)) 1))))
                     
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
                     (setf (aref (vertical arrays) i j) 1))
                   (when (= (aref R i j) (aref Q i j))
                     (setf (aref (horizontal arrays) i j) 1))
                   (when (and (> i 0) (> j 0)
                              (not matchp)
                              (= (aref R i j) (aref S i j)))
                     (setf (aref (diagonal arrays) i j) 1))
                   (when (and (> i 0) (> j 0)
                              matchp
                              (= (aref R i j)
                                 (+ (aref R (- i 1) (- j 1))
                                    (if matchp match-cost +inf))))
                     (setf (aref (diagonal arrays) i j) 1)))))


(defun nv-edge-assignment (pattern source nx ny arrays)
  (loop for i from nx downto 0
        do (loop for j from ny downto 0
                 do ;; 1) if there is no optimal path passing through node N_{i,j} which has cost R_{i,j}
                   ;;     remove the edges vertically, horizontally and diagonally
                   ;;     if the ith is found in pattern and the jth is found in source (so we are not at the end of the matrix),
                   ;;     then they need to be not equal in order for checking d-new, because they only say something about mismatches.
                   ;;     i and j in the pattern and source are always 1 behind, so it is actually the next that needs to be a mismatch. 
                   (when 
                     (and ;; no outgoing vertical edge
                          (or (= (aref (vertical arrays) (+ i 1) j) 0)
                              (= (aref (v-new arrays) i j) 0))
                          ;; no outgoing horizontal edge
                          (or (= (aref (horizontal arrays) i (+ j 1)) 0)
                              (= (aref (h-new arrays) i j) 0))
                          ;; no outgoing diagonal edge
                          (or (= (aref (diagonal arrays) (+ i 1) (+ j 1)) 0)
                              (and (nth i pattern) (nth j source)
                                   (not (equal (nth i pattern) (nth j source)))
                                   (= (aref (d-new arrays) i j) 0))))
                     (setf (aref (vertical arrays) i j) 0
                           (aref (horizontal arrays) i j) 0
                           (aref (diagonal arrays) i j) 0))
                   ;; 2) if no optimal path passes through node N_{i,j}, proceed to the next node
                   (if (and (= (aref (vertical arrays) (+ i 1) j) 0)
                            (= (aref (horizontal arrays) i (+ j 1)) 0)
                            (= (aref (diagonal arrays) (+ i 1) (+ j 1)) 0))
                     nil ;; skip
                     (progn
                       ;; 3) if edge V_{i+1,j} is in an optimal path and requires edge V_{i,j} to be in an optimal path,
                       ;;    determine if an optimal path that uses edge V_{i+1,j} must use edge V_{i,j} and the converse:

                       ;; IF there is a vertical edge to N i+1 j (a i+1 j == 1)
                       ;; and this vertical edge is preceded by another vertical edge (v-v i j == 1)
                       ;; THEN vertical edge to N i+1 j HAS to be preceded by a vertical edge (setf v-v i+1 j to 1)
                       ;; if there is no horizontal edge entering N i j (h-v i j == 0)
                       ;; no diagonal edge entering N i j (d-v i j == 0)
                       ;; and all optimal paths to N i+1 j make use of N i j (v-new i j == 0)
                       ;; THEN vertical edge to N i j HAS to be followed by a vertical edge (setf e i j to 1)
                       ;; if there are no other optimal paths that enter N i j (a i j = 0)
                       ;; because we don't know how in what direction they leave N i j
                       ;; THEN we set a vertical edge to N i j (setf a i j to 1)
                       ;; ELSE
                       ;; the vertical edge to N i+1 j is NOT preceded by a vertical edge (setf d i+1 j to 0)
                       ;; the vertical edge to N i j is NOT followed by a vertical edge (setf e i j to 0)
                       (if (and (= (aref (vertical arrays) (+ i 1) j) 1)
                                (= (aref (v-v arrays) i j) 1)
                               ; (= (aref (vertical arrays) i j) 1)
                                )
                         (setf (aref (v-v arrays) (+ i 1) j) (- 1 (max (aref (h-v arrays) i j)
                                                                       (aref (d-v arrays) i j)
                                                                       (aref (v-new arrays) i j)))
                               (aref (e-v arrays) i j) (- 1 (max (if (= (aref (horizontal arrays) i (+ j 1)) 1)
                                                                   (aref (v-h arrays) i j)
                                                                   0)
                                                                 (if (= (aref (diagonal arrays) (+ i 1) (+ j 1)) 1)
                                                                   (aref (v-m arrays) i j)
                                                                   0)
                                                                 ;; verticaal staat op 0 iff v-v=1 of h-v=1 of d-v=1
                                                                 (aref (vertical arrays) i j)))
                               (aref (vertical arrays) i j) 1)
                         (setf (aref (v-v arrays) (+ i 1) j) 0 
                               (aref (e-v arrays) i j) 0))

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
                       (if (and (= (aref (horizontal arrays) i (+ j 1)) 1)
                                (= (aref (v-h arrays) i j) 1)
                               ; (= (aref (vertical arrays) i j) 1)
                                )
                         (setf (aref (v-h arrays) i (+ j 1)) (- 1 (max (aref (h-h arrays) i j)
                                                                       (aref (d-h arrays) i j) 
                                                                       (aref (h-new arrays) i j)))
                               (aref (e-h arrays) i j) (- 1 (max (if (= (aref (vertical arrays) (+ i 1) j) 1)
                                                                   (aref (v-v arrays) i j)
                                                                   0)
                                                                 (if (= (aref (diagonal arrays) (+ i 1) (+ j 1)) 1)
                                                                   (aref (v-m arrays) i j)
                                                                   0)
                                                                 (aref (vertical arrays) i j)
                                                                 ))
                               (aref (vertical arrays) i j) 1)
                         (setf (aref (v-h arrays) i (+ j 1)) 0 
                               (aref (e-h arrays) i j) 0))

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
                       (if (and (= (aref (diagonal arrays) (+ i 1) (+ j 1)) 1)
                                (= (aref (v-m arrays) i j) 1)
                              ;  (= (aref (vertical arrays) i j) 1)
                                )
                         (setf (aref (v-m arrays) (+ i 1) (+ j 1)) (- 1 (max (aref (h-m arrays) i j)
                                                                             (aref (d-m arrays) i j)
                                                                             (aref (d-new arrays) i j)))
                               (aref (e-m arrays) i j) (- 1 (max (if (= (aref (vertical arrays) (+ i 1) j) 1)
                                                                   (aref (v-v arrays) i j)
                                                                   0)
                                                                 (if (= (aref (horizontal arrays) i (+ j 1)) 1)
                                                                   (aref (v-h arrays) i j)
                                                                   0)
                                                                 (aref (vertical arrays) i j)))
                               (aref (vertical arrays) i j) 1)
                         (setf (aref (v-m arrays) (+ i 1) (+ j 1)) 0 
                               (aref (e-m arrays) i j) 0))

                       ;; 6) if edge H_{i,j+1} is in an optimal path and requires edge H_{i,j} to be in an optimal path,
                       ;;    determine if an optimal path that uses edge H_{i,j+1} must use edge H_{i,j} and the converse:
                       (if (and (= (aref (horizontal arrays) i (+ j 1)) 1)
                                (= (aref (h-h arrays) i j) 1)
                               ; (= (aref (horizontal arrays) i j) 1)
                                )
                         (setf (aref (h-h arrays) i (+ j 1)) (- 1 (max (aref (v-h arrays) i j)
                                                                       (aref (d-h arrays) i j)
                                                                       (aref (h-new arrays) i j)))
                               (aref (g-h arrays) i j) (- 1 (max (if (= (aref (vertical arrays) (+ i 1) j) 1)
                                                                   (aref (h-v arrays) i j)
                                                                   0)
                                                                 (if (= (aref (diagonal arrays) (+ i 1) (+ j 1)) 1)
                                                                   (aref (h-m arrays) i j)
                                                                   0)
                                                                 (aref (horizontal arrays) i j)))
                               (aref (horizontal arrays) i j) 1)
                         (setf (aref (h-h arrays) i (+ j 1)) 0
                               (aref (g-h arrays) i j) 0))

                       
                       (if (and (= (aref (vertical arrays) (+ i 1) j) 1)
                                (= (aref (h-v arrays) i j) 1)
                               ; (= (aref (horizontal arrays) i j) 1)
                                )
                         (setf (aref (h-v arrays) (+ i 1) j) (- 1 (max (aref (v-v arrays) i j)
                                                                       (aref (d-v arrays) i j)
                                                                       (aref (v-new arrays) i j)))
                               (aref (g-v arrays) i j) (- 1 (max (if (= (aref (horizontal arrays) i (+ j 1)) 1)
                                                                   (aref (h-h arrays) i j)
                                                                   0)
                                                                 (if (= (aref (diagonal arrays) (+ i 1) (+ j 1)) 1)
                                                                   (aref (h-m arrays) i j)
                                                                   0)
                                                                 (aref (horizontal arrays) i j)
                                                                 ))
                               (aref (horizontal arrays) i j) 1)
                         (setf (aref (h-v arrays) (+ i 1) j) 0
                               (aref (g-v arrays) i j) 0))

                       
                       (if (and (= (aref (diagonal arrays) (+ i 1) (+ j 1)) 1)
                                (= (aref (h-m arrays) i j) 1)
                               ; (= (aref (horizontal arrays) i j) 1)
                                )
                         (setf (aref (h-m arrays) (+ i 1) (+ j 1)) (- 1 (max (aref (v-m arrays) i j)
                                                                             (aref (d-m arrays) i j)
                                                                             (aref (d-new arrays) i j)))
                               (aref (g-m arrays) i j) (- 1 (max (if (= (aref (horizontal arrays) i (+ j 1)) 1)
                                                                   (aref (h-h arrays) i j)
                                                                   0)
                                                                 (if (= (aref (vertical arrays) (+ i 1) j) 1)
                                                                   (aref (h-v arrays) i j)
                                                                   0)
                                                                 (aref (horizontal arrays) i j)
                                                                 ))
                               (aref (horizontal arrays) i j) 1)
                         (setf (aref (h-m arrays) (+ i 1) (+ j 1)) 0
                               (aref (g-m arrays) i j) 0))
              
                       ;; 7) if edge D_{i+1,j+1} is in an optimal path and requires edge D_{i,j} to be in an optimal path,
                       ;;    determine if an optimal path that uses edge D_{i+1,j+1} must use edge D_{i,j} and the converse:
                       (if (and (= (aref (diagonal arrays) (+ i 1) (+ j 1)) 1)
                                (= (aref (d-m arrays) i j) 1)
                               ; (= (aref (diagonal arrays) i j) 1)
                                )
                         (setf (aref (d-m arrays) (+ i 1) (+ j 1)) (- 1 (max (aref (v-m arrays) i j)
                                                                             (aref (h-m arrays) i j)
                                                                             (aref (d-new arrays) i j)))
                               (aref (l-m arrays) i j) (- 1 (max (if (= (aref (horizontal arrays) i (+ j 1)) 1)
                                                                   (aref (d-h arrays) i j)
                                                                   0)
                                                                 (if (= (aref (vertical arrays) (+ i 1) j) 1)
                                                                   (aref (d-v arrays) i j)
                                                                   0)
                                                                 (aref (diagonal arrays) i j)
                                                                 ))
                               (aref (diagonal arrays) i j) 1)
                         (setf (aref (d-m arrays) (+ i 1) (+ j 1)) 0
                               (aref (l-m arrays) i j) 0))
                       
                       (if (and (= (aref (horizontal arrays) i (+ j 1)) 1)
                                (= (aref (d-h arrays) i j) 1)
                               ; (= (aref (diagonal arrays) i j) 1)
                                )
                         (setf (aref (d-h arrays) i (+ j 1)) (- 1 (max (aref (v-h arrays) i j)
                                                                       (aref (h-h arrays) i j)
                                                                       (aref (h-new arrays) i j)))
                               (aref (l-h arrays) i j) (- 1 (max (if (= (aref (vertical arrays) (+ i 1) j) 1)
                                                                   (aref (d-v arrays) i j)
                                                                   0)
                                                                 (if (= (aref (diagonal arrays) (+ i 1) (+ j 1)) 1)
                                                                   (aref (d-m arrays) i j)
                                                                   0)
                                                                 (aref (diagonal arrays) i j)
                                                                 ))
                               (aref (diagonal arrays) i j) 1)
                         (setf (aref (d-h arrays) i (+ j 1)) 0
                               (aref (l-h arrays) i j) 0))
                       
                       (if (and (= (aref (vertical arrays) (+ i 1) j) 1)
                                (= (aref (d-v arrays) i j) 1)
                               ; (= (aref (diagonal arrays) i j) 1)
                                )
                         (setf (aref (d-v arrays) (+ i 1) j) (- 1 (max (aref (v-v arrays) i j)
                                                                       (aref (h-v arrays) i j)
                                                                       (aref (v-new arrays) i j)))
                               (aref (l-v arrays) i j) (- 1 (max (if (= (aref (horizontal arrays) i (+ j 1)) 1)
                                                                   (aref (d-h arrays) i j)
                                                                   0)
                                                                 (if (= (aref (diagonal arrays) (+ i 1) (+ j 1)) 1)
                                                                   (aref (d-m arrays) i j)
                                                                   0)
                                                                 (aref (diagonal arrays) i j)
                                                                 ))
                               (aref (diagonal arrays) i j) 1)
                         (setf (aref (d-v arrays) (+ i 1) j) 0
                               (aref (l-v arrays) i j) 0)))))))


(defclass nv-sequence-alignment-state ()
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
   (mismatch-positions :initarg :mismatch-positions :accessor mismatch-positions :initform nil :type list)
   (vertical-positions :initarg :vertical-positions :accessor vertical-positions :initform nil :type list)
   (horizontal-positions :initarg :horizontal-positions :accessor horizontal-positions :initform nil :type list)
   (gap-counter :initarg :gap-counter :accessor gap-counter :initform 0 :type number)
   (prev-edge :initarg :prev-edge :accessor prev-edge :initform nil)
   (next-edge :initarg :next-edge :accessor next-edge :initform nil)
   (path :initarg :path :accessor path :initform nil)))

(defun make-initial-nv-sequence-alignment-state (i j)
  (make-instance 'nv-sequence-alignment-state :i i :j j))

(defun nv-extract-optimal-alignments (pattern source arrays
                                              pattern-boundaries source-boundaries
                                              optimal-cost
                                              match-cost
                                              mismatch-cost
                                              mismatch-opening-cost
                                              gap-opening-cost
                                              gap-cost
                                              boundary-matrix
                                              &key n-optimal-alignments
                                              max-nr-of-au-gaps
                                              debugging)
  (loop with solutions = nil
        ;; start at position (M, N)
        with queue = (list (make-initial-nv-sequence-alignment-state
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
                             (let ((next-state (nv-check-vertical-edges pattern source pattern-boundaries source-boundaries state arrays boundary-matrix gap-opening-cost gap-cost)))
                               next-state))
                            
                            (;; next-edge is set to horizontal -> only need to check horizontal edges
                             (eql next-edge 'horizontal)
                             ;; as a sanity check, we could assert that vertical and diagonal edges here are 0
                             (let ((next-state (nv-check-horizontal-edges pattern source pattern-boundaries source-boundaries state arrays boundary-matrix gap-opening-cost gap-cost)))
                               next-state))

                            (;; next-edge is set to diagonal-mismatch
                             (or (eql next-edge 'diagonal-mismatch) (eql next-edge 'diagonal))
                             (let ((next-state (nv-check-diagonal-edges pattern source pattern-boundaries source-boundaries state arrays boundary-matrix match-cost mismatch-opening-cost mismatch-cost)))
                               next-state))
                            
                            (;; next-edge is not set -> check vertical, horizontal and diagonal edges
                             t
                             (let ((next-state-diagonal (nv-check-diagonal-edges pattern source pattern-boundaries source-boundaries state arrays boundary-matrix match-cost mismatch-opening-cost mismatch-cost))
                                   (next-state-vertical (nv-check-vertical-edges pattern source pattern-boundaries source-boundaries state arrays boundary-matrix gap-opening-cost gap-cost))
                                   (next-state-horizontal (nv-check-horizontal-edges pattern source pattern-boundaries source-boundaries state arrays boundary-matrix gap-opening-cost gap-cost)))
                               (append next-state-diagonal next-state-vertical next-state-horizontal)))))
                     (next-states-with-max-gaps
                      (if max-nr-of-au-gaps
                        (remove-if #'(lambda (state) (> (gap-counter state) max-nr-of-au-gaps)) next-states)
                        next-states)))
                (when debugging
                  (format t "--------------------------~%")
                  (loop for alignment-state in next-states-with-max-gaps
                      for i from 1
                      for symbols = nil
                      do (loop for x in (aligned-pattern alignment-state)
                               for y in (aligned-source alignment-state)
                               do (cond ((eql x #\_) (push #\Space symbols))
                                        ((eql y #\_) (push #\Space symbols))
                                        ((eql x y) (push #\| symbols))
                                        ((not (eql x y)) (push #\. symbols))))
                         (format t "--- Result ~a (cost: ~a) ---~%~%" i (cost alignment-state))
                         (format t "~s~%" (coerce (aligned-pattern alignment-state) 'string))
                         (format t "~s~%" (coerce (reverse symbols) 'string))
                         (format t "~s~%~%" (coerce (aligned-source alignment-state) 'string))))
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
  

(defun nv-check-diagonal-edges (pattern source pattern-boundaries source-boundaries state arrays boundary-matrix match-cost mismatch-opening-cost mismatch-cost)
  (with-slots (aligned-pattern aligned-source
               aligned-pattern-boundaries aligned-source-boundaries
               i j cost match-positions mismatch-positions vertical-positions
               horizontal-positions gap-counter prev-edge) state
    (when (= (aref (diagonal arrays) i j) 1)  ;; check if there is a diagonal edge in this state
      ;; check if there is a previous edge set and if it is correct
      (unless (or (and (= (aref (l-m arrays) i j) 1)
                       (not (eql prev-edge 'diagonal-mismatch)))
                  (and (= (aref (l-h arrays) i j) 1)
                       (not (eql prev-edge 'horizontal)))
                  (and (= (aref (l-v arrays) i j) 1)
                       (not (eql prev-edge 'vertical))))
        ;(setf *diagonal-edges-counter* (+ *diagonal-edges-counter* 1))
        (let* (;; indexing in pattern and source string is offset by -1 w.r.t. index in matrix (i,j)
               (pattern-char (nth (- i 1) pattern))
               (source-char (nth (- j 1) source))
               (expanded-pattern (cons pattern-char aligned-pattern))
               (expanded-source (cons source-char aligned-source))
               (current-left-source-boundary (car (first aligned-source-boundaries)))
               (current-left-pattern-boundary (car (first aligned-pattern-boundaries)))
               (matchp (eql pattern-char source-char))
               (source-boundary-vars (make-boundary-indices j source-boundaries current-left-source-boundary))
               (pattern-boundary-vars (make-boundary-vars i pattern-boundaries current-left-pattern-boundary boundary-matrix i j))
               (new-mismatch (if (or (eql prev-edge 'diagonal-mismatch)
                                     (eql prev-edge 'horizontal)
                                     (eql prev-edge 'vertical))
                               mismatch-cost (+ mismatch-cost mismatch-opening-cost)))
               (new-gap-p (or (and (null match-positions) (null matchp))
                              (and (first match-positions)
                                   (equal (first match-positions) (cons (+ i 1) (+ j 1)))
                                   (null matchp))))
               (next-vertical-state
                (when (= (aref (v-m arrays) i j) 1)
                  (make-instance 'nv-sequence-alignment-state
                                 :aligned-pattern expanded-pattern
                                 :aligned-source expanded-source
                                 :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                 :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                 :i (- i 1) :j (- j 1)
                                 :cost (+ cost (if matchp match-cost new-mismatch))
                                 :match-positions (if matchp (cons (cons i j) match-positions) match-positions)
                                 :mismatch-positions (if (not matchp) (cons (cons i j) mismatch-positions) mismatch-positions)
                                 :vertical-positions vertical-positions
                                 :horizontal-positions horizontal-positions
                                 :gap-counter (if new-gap-p (+ 1 gap-counter) gap-counter)
                                 :next-edge 'vertical
                                 :prev-edge (if matchp 'diagonal 'diagonal-mismatch)
                                 :path (cons (if matchp 'diagonal 'diagonal-mismatch) (path state)))))
               (next-horizontal-state
                (when (= (aref (h-m arrays) i j) 1)
                  (make-instance 'nv-sequence-alignment-state
                                 :aligned-pattern expanded-pattern
                                 :aligned-source expanded-source
                                 :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                 :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                 :i (- i 1) :j (- j 1)
                                 :cost (+ cost (if matchp match-cost new-mismatch))
                                 :match-positions (if matchp (cons (cons i j) match-positions) match-positions)
                                 :mismatch-positions (if (not matchp) (cons (cons i j) mismatch-positions) mismatch-positions)
                                 :gap-counter (if new-gap-p (+ 1 gap-counter) gap-counter)
                                 :next-edge 'horizontal
                                 :prev-edge (if matchp 'diagonal 'diagonal-mismatch)
                                 :path (cons (if matchp 'diagonal 'diagonal-mismatch) (path state)))))
               (next-diagonal-state
                (when (= (aref (d-m arrays) i j) 1)
                  (make-instance 'nv-sequence-alignment-state
                                 :aligned-pattern expanded-pattern
                                 :aligned-source expanded-source
                                 :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                 :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                 :i (- i 1) :j (- j 1)
                                 :cost (+ cost (if matchp match-cost new-mismatch))
                                 :match-positions (if matchp (cons (cons i j) match-positions) match-positions)
                                 :mismatch-positions (if (not matchp) (cons (cons i j) mismatch-positions) mismatch-positions)
                                 :vertical-positions vertical-positions
                                 :horizontal-positions horizontal-positions
                                 :gap-counter (if new-gap-p (+ 1 gap-counter) gap-counter)
                                 :next-edge 'diagonal-mismatch
                                 :prev-edge (if matchp 'diagonal 'diagonal-mismatch)
                                 :path (cons (if matchp 'diagonal 'diagonal-mismatch) (path state)))))
               next-states)
          
          (if (not (or next-vertical-state next-horizontal-state next-diagonal-state))
            (setf next-states (list (make-instance 'nv-sequence-alignment-state
                                                   :aligned-pattern expanded-pattern
                                                   :aligned-source expanded-source
                                                   :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                                   :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                                   :i (- i 1) :j (- j 1)
                                                   :cost (+ cost (if matchp match-cost new-mismatch))
                                                   :match-positions (if matchp (cons (cons i j) match-positions) match-positions)
                                                   :mismatch-positions (if (not matchp) (cons (cons i j) mismatch-positions) mismatch-positions)
                                                   :vertical-positions vertical-positions
                                                   :horizontal-positions horizontal-positions
                                                   :gap-counter (if new-gap-p (+ 1 gap-counter) gap-counter)
                                                   :prev-edge (if matchp 'diagonal 'diagonal-mismatch)
                                                   :path (cons (if matchp 'diagonal 'diagonal-mismatch) (path state)))))
            (setf next-states (remove nil (list next-diagonal-state next-vertical-state next-horizontal-state))))
        
          ;; return the next state
          next-states)))))

(defun nv-check-vertical-edges (pattern source pattern-boundaries source-boundaries state arrays boundary-matrix gap-opening-cost gap-cost)
  (declare (ignore source))
  (with-slots (aligned-pattern aligned-source
               aligned-pattern-boundaries aligned-source-boundaries
               i j cost match-positions mismatch-positions vertical-positions
               horizontal-positions gap-counter prev-edge) state
    (when (= (aref (vertical arrays) i j) 1)  ;; check if there is a vertical edge in this state
      ;; check if there is a previous edge set and if it is correct
      (unless (or (and (= (aref (e-v arrays) i j) 1)
                       (not (eql prev-edge 'vertical)))
                  (and (= (aref (e-h arrays) i j) 1)
                       (not (eql prev-edge 'horizontal)))
                  (and (= (aref (e-m arrays) i j) 1)
                       (not (eql prev-edge 'diagonal-mismatch))))
        ;(setf *vertical-edges-counter* (+ *vertical-edges-counter* 1))
        (let* ((expanded-pattern (cons (nth (- i 1) pattern) aligned-pattern))
               (expanded-source (cons #\_ aligned-source))
               (current-left-source-boundary (car (first aligned-source-boundaries)))
               (current-left-pattern-boundary (car (first aligned-pattern-boundaries)))
               (source-boundary-vars (make-boundary-indices nil source-boundaries current-left-source-boundary :gap t))
               (pattern-boundary-vars (make-boundary-vars i pattern-boundaries current-left-pattern-boundary boundary-matrix i j))
               (new-gap-p (not (or (eql (first aligned-source) #\_)
                                   (eql (first aligned-pattern) #\_)
                                   (eql prev-edge 'diagonal-mismatch))))  ;; new gap in terms of nv (i.e. a _)
               (gap-counter-new-gap-p (equal (first match-positions) (cons (+ i 1) (+ j 1))))  ;; gap counter in terms of AU gaps
               (cost-increase (if new-gap-p (+ gap-cost gap-opening-cost) gap-cost))
               (next-vertical-state
                (when (= (aref (v-v arrays) i j) 1)
                  (make-instance 'nv-sequence-alignment-state
                                 :aligned-pattern expanded-pattern
                                 :aligned-source expanded-source
                                 :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                 :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                 :i (- i 1) :j j
                                 :cost (+ cost cost-increase)
                                 :match-positions match-positions
                                 :mismatch-positions mismatch-positions
                                 :vertical-positions (cons (cons i j) vertical-positions)
                                 :horizontal-positions horizontal-positions
                                 :gap-counter (if gap-counter-new-gap-p (+ 1 gap-counter) gap-counter)
                                 :next-edge 'vertical
                                 :prev-edge 'vertical
                                 :path (cons 'vertical (path state)))))
               (next-horizontal-state
                (when (= (aref (h-v arrays) i j) 1)
                  (make-instance 'nv-sequence-alignment-state
                                 :aligned-pattern expanded-pattern
                                 :aligned-source expanded-source
                                 :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                 :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                 :i (- i 1) :j j
                                 :cost (+ cost cost-increase)
                                 :match-positions match-positions
                                 :mismatch-positions mismatch-positions
                                 :vertical-positions (cons (cons i j) vertical-positions)
                                 :horizontal-positions horizontal-positions
                                 :gap-counter (if gap-counter-new-gap-p (+ 1 gap-counter) gap-counter)
                                 :next-edge 'horizontal
                                 :prev-edge 'vertical
                                 :path (cons 'vertical (path state)))))
               (next-diagonal-state
                (when (= (aref (d-v arrays) i j) 1)
                  (make-instance 'nv-sequence-alignment-state
                                 :aligned-pattern expanded-pattern
                                 :aligned-source expanded-source
                                 :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                 :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                 :i (- i 1) :j j
                                 :cost (+ cost cost-increase)
                                 :match-positions match-positions
                                 :mismatch-positions mismatch-positions
                                 :vertical-positions (cons (cons i j) vertical-positions)
                                 :horizontal-positions horizontal-positions
                                 :gap-counter (if gap-counter-new-gap-p (+ 1 gap-counter) gap-counter)
                                 :next-edge 'diagonal-mismatch ;; sure this is only mismatch? can it be match too?
                                 :prev-edge 'vertical
                                 :path (cons 'vertical (path state))))) 
               next-states)
          
          (if (not (or next-vertical-state next-horizontal-state next-diagonal-state))
            (setf next-states (list (make-instance 'nv-sequence-alignment-state
                                                   :aligned-pattern expanded-pattern
                                                   :aligned-source expanded-source
                                                   :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                                   :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                                   :i (- i 1) :j j
                                                   :cost (+ cost cost-increase)
                                                   :match-positions match-positions
                                                   :mismatch-positions mismatch-positions
                                                   :vertical-positions (cons (cons i j) vertical-positions)
                                                   :horizontal-positions horizontal-positions
                                                   :gap-counter (if gap-counter-new-gap-p (+ 1 gap-counter) gap-counter)
                                                   :prev-edge 'vertical
                                                   :path (cons 'vertical (path state)))))
            (setf next-states (remove nil (list next-diagonal-state next-vertical-state next-horizontal-state))))

          ;; return the next state
          next-states)))))


(defun nv-check-horizontal-edges (pattern source pattern-boundaries source-boundaries state  arrays boundary-matrix gap-opening-cost gap-cost)
  (declare (ignore pattern))
  (with-slots (aligned-pattern aligned-source
               aligned-pattern-boundaries aligned-source-boundaries
               i j cost match-positions mismatch-positions vertical-positions
               horizontal-positions gap-counter prev-edge) state
    (when (= (aref (horizontal arrays) i j) 1)  ;; check if there is a horizontal edge in this state
      ;; when g-h is set, the prev edge has to be horizontal
      ;; if not, remove the next state! same for g-v and vertical, g-m and diagonal-mismatch
      (unless (or (and (= (aref (g-h arrays) i j) 1)
                       (not (eql prev-edge 'horizontal)))
                  (and (= (aref (g-v arrays) i j) 1)
                       (not (eql prev-edge 'vertical)))
                  (and (= (aref (g-m arrays) i j) 1)
                       (not (eql prev-edge 'diagonal-mismatch))))
        ;(setf *horizontal-edges-counter* (+ *horizontal-edges-counter* 1))
        (let* ((expanded-pattern (cons #\_ aligned-pattern))
               (expanded-source (cons (nth (- j 1) source) aligned-source))
               (current-left-source-boundary (car (first aligned-source-boundaries)))
               (current-left-pattern-boundary (car (first aligned-pattern-boundaries)))
               (source-boundary-vars (make-boundary-indices j source-boundaries current-left-source-boundary))
               (pattern-boundary-vars (make-boundary-vars nil pattern-boundaries current-left-pattern-boundary boundary-matrix i j :gap t))
               (new-gap-p (not (or (eql (first aligned-pattern) #\_)
                                   (eql (first aligned-source) #\_)
                                   (eql prev-edge 'diagonal-mismatch))))  ;; new gap in terms of nv (i.e. a _)
               (gap-counter-new-gap-p (equal (first match-positions) (cons (+ i 1) (+ j 1))))  ;; gap counter in terms of AU gaps
               (cost-increase (if new-gap-p (+ gap-cost gap-opening-cost) gap-cost))
               (next-horizontal-state
                (when (= (aref (h-h arrays) i j) 1)
                  (make-instance 'nv-sequence-alignment-state
                                 :aligned-pattern expanded-pattern
                                 :aligned-source expanded-source
                                 :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                 :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                 :i i :j (- j 1)
                                 :cost (+ cost cost-increase)
                                 :match-positions match-positions
                                 :mismatch-positions mismatch-positions
                                 :vertical-positions vertical-positions
                                 :horizontal-positions (cons (cons i j) horizontal-positions)
                                 :gap-counter (if gap-counter-new-gap-p (+ 1 gap-counter) gap-counter)
                                 :next-edge 'horizontal
                                 :prev-edge 'horizontal
                                 :path (cons 'horizontal (path state)))))
               (next-vertical-state
                (when (= (aref (v-h arrays) i j) 1)
                  (make-instance 'nv-sequence-alignment-state
                                 :aligned-pattern expanded-pattern
                                 :aligned-source expanded-source
                                 :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                 :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                 :i i :j (- j 1)
                                 :cost (+ cost cost-increase)
                                 :match-positions match-positions
                                 :mismatch-positions mismatch-positions
                                 :vertical-positions vertical-positions
                                 :horizontal-positions (cons (cons i j) horizontal-positions)
                                 :gap-counter (if gap-counter-new-gap-p (+ 1 gap-counter) gap-counter)
                                 :next-edge 'vertical
                                 :prev-edge 'horizontal
                                 :path (cons 'horizontal (path state)))))
               (next-diagonal-state
                (when (= (aref (v-m arrays) i j) 1)
                  (make-instance 'nv-sequence-alignment-state
                                 :aligned-pattern expanded-pattern
                                 :aligned-source expanded-source
                                 :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                 :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                 :i i :j (- j 1)
                                 :cost (+ cost cost-increase)
                                 :match-positions match-positions
                                 :mismatch-positions mismatch-positions
                                 :vertical-positions vertical-positions
                                 :horizontal-positions (cons (cons i j) horizontal-positions)
                                 :gap-counter (if gap-counter-new-gap-p (+ 1 gap-counter) gap-counter)
                                 :next-edge 'diagonal-mismatch
                                 :prev-edge 'horizontal
                                 :path (cons 'horizontal (path state)))))
               next-states)
          
          (if (not (or next-vertical-state next-horizontal-state next-diagonal-state))
            (setf next-states (list (make-instance 'nv-sequence-alignment-state
                                                   :aligned-pattern expanded-pattern
                                                   :aligned-source expanded-source
                                                   :aligned-pattern-boundaries (cons pattern-boundary-vars aligned-pattern-boundaries)
                                                   :aligned-source-boundaries (cons source-boundary-vars aligned-source-boundaries)
                                                   :i i :j (- j 1)
                                                   :cost (+ cost cost-increase)
                                                   :match-positions match-positions
                                                   :mismatch-positions mismatch-positions
                                                   :vertical-positions vertical-positions
                                                   :horizontal-positions (cons (cons i j) horizontal-positions)
                                                   :gap-counter (if gap-counter-new-gap-p (+ 1 gap-counter) gap-counter)
                                                   :prev-edge 'horizontal
                                                   :path (cons 'horizontal (path state)))))
            (setf next-states (remove nil (list next-diagonal-state next-vertical-state next-horizontal-state))))
     
          ;; return the next state
          next-states)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; visualisations ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun visualise-matrix (pattern source matrix direction)
  (declare (ignore direction))
  (let ((char #|(cond ((equal direction 'vertical) (code-char 8595))
                    ((equal direction 'diagonal) (code-char 8600))
                    ((equal direction 'horizontal) (code-char 8594)))|#
          1))
    (format t "~%    ")
    (loop for y from 0 to (- (second (array-dimensions matrix)) 1)
          do (format t "----" (aref matrix 0 y)))
    (format t "~%    | _ ")
    (loop for i in pattern
          do (format t "| ~a " (capitalise i)))
    (format t "| _ ")
    (format t "|~%")
    (loop for y from 0 to (second (array-dimensions matrix))
          do (format t "----"))
    (loop for x from 0 to (- (first (array-dimensions matrix)) 1)
          do (format t "~%")
             (format t "| ~a " (if (and (> x 0)
                                        (< x (+ (length source) 1)))
                                 (capitalise (nth (- x 1) source))
                                 "_"))
             (loop for y from 0 to (- (second (array-dimensions matrix)) 1)
                   do (format t "| ~a " (if (= (aref matrix x y) 1) char " ")))
             (format t "|~%")
             (loop for y from 0 to (second (array-dimensions matrix))
                   do (format t "----")))))


(defun visualise-path (path)
  (format t "~%Optimal Alignment: ~%")
  (loop with i = 0
        with previous-edge = nil
        for edge in  path
        do (cond ((equal edge 'horizontal)
                  (when (not (or (equal previous-edge 'horizontal) (equal previous-edge 'diagonal-mismatch)))
                    (format t "~%~a" (make-string (* i 2) :initial-element #\Space)))
                  (format t "~a "  (code-char 8594)))
                 ((equal edge 'vertical)
                  (if (equal previous-edge 'vertical)
                    (format t "~%~a" (make-string (- (* i 2) 2) :initial-element #\Space))
                    (format t "~%~a" (make-string (* i 2) :initial-element #\Space)))
                  (format t "~a " (code-char 8595)))
                 ((or (equal edge 'diagonal) (equal edge 'diagonal-mismatch))
                  (when (not (or (equal previous-edge 'horizontal)))
                    (format t "~%~a" (make-string (* i 2) :initial-element #\Space)))
                  (format t "~a " (code-char 8600))))
        (setf i (+ i 1))
        (setf previous-edge edge))
  (print (reverse path)))


(defun visualise-arrays (pattern source arrays)
  (format t "~%-------------------------------------------------------~%")
  (format t "~%~%Matrix vertical: ~%")
  (visualise-matrix  source pattern (vertical arrays) 'vertical)
  (format t "~%~%Matrix v-v: ~%")
  (visualise-matrix  source pattern (v-v arrays) 'vertical)
  (format t "~%~%Matrix v-h: ~%")
  (visualise-matrix  source pattern (v-h arrays) 'vertical)
  (format t "~%~%Matrix v-m: ~%")
  (visualise-matrix  source pattern (v-m arrays) 'vertical)
  (format t "~%~%Matrix v-new: ~%")
  (visualise-matrix  source pattern (v-new arrays) 'vertical)
  (format t "~%~%Matrix horizontal: ~%")
  (visualise-matrix  source pattern (horizontal arrays) 'horizontal)
  (format t "~%~%Matrix h-v: ~%")
  (visualise-matrix  source pattern (h-v arrays) 'horizontal)
  (format t "~%~%Matrix h-h: ~%")
  (visualise-matrix  source pattern (h-h arrays) 'horizontal)
  (format t "~%~%Matrix h-m: ~%")
  (visualise-matrix  source pattern (h-m arrays) 'horizontal)
  (format t "~%~%Matrix h-new: ~%")
  (visualise-matrix  source pattern (h-new arrays) 'horizontal)
  (format t "~%~%Matrix diagonal: ~%")
  (visualise-matrix  source pattern (diagonal arrays) 'diagonal)
  (format t "~%~%Matrix d-v: ~%")
  (visualise-matrix  source pattern (d-v arrays) 'diagonal)
  (format t "~%~%Matrix d-h: ~%")
  (visualise-matrix  source pattern (d-h arrays) 'diagonal)
  (format t "~%~%Matrix d-m: ~%")
  (visualise-matrix  source pattern (d-m arrays) 'diagonal)
  (format t "~%~%Matrix d-new: ~%")
  (visualise-matrix  source pattern (d-new arrays) 'diagonal)

  (format t "~%~%Matrix e-v: ~%")
  (visualise-matrix  source pattern (e-v arrays) 'diagonal)
  (format t "~%~%Matrix e-h: ~%")
  (visualise-matrix  source pattern (e-h arrays) 'diagonal)
  (format t "~%~%Matrix e-m: ~%")
  (visualise-matrix  source pattern (e-m arrays) 'diagonal)
  
  (format t "~%~%Matrix g-v: ~%")
  (visualise-matrix  source pattern (g-v arrays) 'diagonal)
  (format t "~%~%Matrix g-h: ~%")
  (visualise-matrix  source pattern (g-h arrays) 'diagonal)
  (format t "~%~%Matrix g-m: ~%")
  (visualise-matrix  source pattern (g-m arrays) 'diagonal)

  (format t "~%~%Matrix l-v: ~%")
  (visualise-matrix  source pattern (l-v arrays) 'diagonal)
  (format t "~%~%Matrix l-h: ~%")
  (visualise-matrix  source pattern (l-h arrays) 'diagonal)
  (format t "~%~%Matrix l-m: ~%")
  (visualise-matrix  source pattern (l-m arrays) 'diagonal)
  (format t "~%-------------------------------------------------------~%"))





