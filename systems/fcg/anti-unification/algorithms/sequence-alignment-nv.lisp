;(ql:quickload :fcg)
(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maximal Sequence Alignment - nv algorithm ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; changes to original algorithm:
;;;   - cost assignment:
;;;         also look in opposite matrix to see if extension of gap-opening would be less costly
;;;         if so, set gap-extension array (of the opposite side) on 1
;;;   - edge assignment:
;;;         also look at edges on the other side when assigning edges
;;;         order is very important to get all solutions!!!
;;;   - check-vertical and check-horizontal:
;;;         check for gaps also on other side for right cost calculation (cost calculation happens again here)

(defun cost-assignment (pattern source nx ny P Q R a b c d e f g
                              &key (match-cost -1) (mismatch-cost 1)
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
                            (extended-gap-cost-other-side (if (> j 0) (+ (aref Q (- i 1) j) gap-cost) +inf)) ;extend the gap of the horizontal matrix (look at the cost of position ((- i 1) j), since we are adding a vertical gap, we need to look at the same position for both the extended-gap-cost and the extended-gap-cost-other-side)
                            (new-gap-cost (+ (aref R (- i 1) j) gap-opening-cost gap-cost))
                            (min-cost (min extended-gap-cost new-gap-cost extended-gap-cost-other-side))) ;min cost between opening a gap after match/mismatch, vertical gap-extension or horizontal gap-extension
                       (setf (aref P i j) min-cost)
                       (when (= min-cost extended-gap-cost)
                         (setf (aref d (- i 1) j) 1))
                       (when (= min-cost new-gap-cost)
                         (setf (aref e (- i 1) j) 1))
                       (when (= min-cost extended-gap-cost-other-side)
                         (setf (aref f (- i 1) j) 1))))

                   ;; 3) find the minimum cost of a path ending at node N_{i,j using horizontal edge
                   ;; 4) determine if cost Q_{i,j} can be achieved with and without edge H_{i,j-1}, i.e. horizontal edge left
                   (when (> j 0)
                     (let* ((extended-gap-cost (+ (aref Q i (- j 1)) gap-cost))
                            (extended-gap-cost-other-side (if (> i 0) (+ (aref P i (- j 1)) gap-cost) +inf)) ;extend the gap of the vertical matrix (look at the cost of position (i (- j 1)), since we are adding a horizontal gap, we need to look at the same position for both the extended-gap-cost and the extended-gap-cost-other-side)
                            (new-gap-cost (+ (aref R i (- j 1)) gap-opening-cost gap-cost))
                            (min-cost (min extended-gap-cost new-gap-cost extended-gap-cost-other-side))) ;min cost between opening a gap after match/mismatch, vertical gap-extension or horizontal gap-extension
                       (setf (aref Q i j) min-cost)
                       (when (= min-cost extended-gap-cost)
                         (setf (aref f i (- j 1)) 1))
                       (when (= min-cost new-gap-cost)
                         (setf (aref g i (- j 1)) 1))
                       (when (= min-cost extended-gap-cost-other-side)
                         (setf (aref d i (- j 1)) 1))))
                     
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


(defun edge-assignment (nx ny a b c d e f g)
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
                       (if (or (and (= (aref a (+ i 1) j) 1) (= (aref d i j) 1)) ; if there is a vertical edge and the previous edge was also vertical 
                               (and (= (aref b i (+ j 1)) 1) (= (aref d i j) 1))) ; or if there is a horizontal edge and the previous edge was vertical
                             (setf (aref d (+ i 1) j) (- 1 (aref e i j)) ; then you know that the next edge d (+ i 1) j has as previous edge a vertical edge depending on whether you came from a vertical edge or not (- 1 (aref e i j))
                                   (aref e i j) (- 1 (aref a i j)) ; you also know that the next edge stored in (aref e i j) depends on the edge that is there or not (- 1 (aref a i j))
                                   (aref a i j) 1)) ; you know that the edge is horizontal
                         (when (not (and (= (aref a (+ i 1) j) 1) (= (aref d i j) 1))) ; if no vertical edge and not previous vertical edge, then you don't know anythng about the previous and next edge, so set to 0
                           (setf (aref d (+ i 1) j) 0 
                                 (aref e i j) 0))
                       ;; 4) if edge H_{i,j+1} is in an optimal path and requires edge H_{i,j} to be in an optimal path,
                       ;;    determine if an optimal path that uses edge H_{i,j+1} must use edge H_{i,j} and the converse:
                       (if (or (and (= (aref b i (+ j 1)) 1) (= (aref f i j) 1))
                               (and (= (aref a (+ i 1) j) 1) (= (aref f i j) 1)))
                            (setf (aref f i (+ j 1)) (- 1 (aref g i j))
                                  (aref g i j) (- 1 (aref b i j))
                                  (aref b i j) 1))
                          (when (not (and (= (aref b i (+ j 1)) 1) (= (aref f i j) 1)))
                            (setf (aref f i (+ j 1)) 0
                                  (aref g i j) 0)))))))


(defun check-vertical-edges (pattern source pattern-boundaries source-boundaries state a d e
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
             (new-gap-p (not (or (eql (first aligned-source) #\_) (eql (first aligned-pattern) #\_))))  ;; new gap in terms of nv (i.e. a _)
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
             (new-gap-p (not (or (eql (first aligned-pattern) #\_) (eql (first aligned-source) #\_))))  ;; new gap in terms of nv (i.e. a _)
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
        (when (= (aref f i j) 1)
          (setf (next-edge next-state) 'horizontal))
        ;; when g is set, the prev edge has to be horizontal
        ;; if not, remove the next state!
        (when (= (aref g i j) 1)
          (unless (eql prev-edge 'horizontal)
            (setf next-state nil)))
        ;; return the next state
        next-state))))

#|
;; cost should be 21 (originally 46)
(print-sequence-alignments (maximal-sequence-alignments "AGAT" "CTCT" nil nil
                             :match-cost -10 :mismatch-cost 30 :gap-opening-cost 25 :gap-cost 1
                             :remove-duplicate-alignments nil))

;; cost should be 19
(print-sequence-alignments (maximal-sequence-alignments "AGT" "CTT" nil nil
                             :match-cost -10 :mismatch-cost 30 :gap-opening-cost 25 :gap-cost 1
                             :remove-duplicate-alignments nil))

;; cost should be 11 (originally 36)
(print-sequence-alignments (maximal-sequence-alignments "XAGAT" "XCTCT" nil nil
                             :match-cost -10 :mismatch-cost 30 :gap-opening-cost 25 :gap-cost 1
                             :remove-duplicate-alignments nil))

;; cost should be 31 (originally 56)
(print-sequence-alignments (maximal-sequence-alignments "ABA" "XYZ" nil nil
                             :match-cost -10 :mismatch-cost 30 :gap-opening-cost 25 :gap-cost 1
                             :remove-duplicate-alignments nil))

;; cost should be 26 ??????  --> wrong ! there is a cost that is lower (13) because it is better to not align the L
(print-sequence-alignments (maximal-sequence-alignments "XAGLAT" "XCLTCT" nil nil
                             :match-cost -10 :mismatch-cost 30 :gap-opening-cost 25 :gap-cost 1
                             :remove-duplicate-alignments nil))

;; cost should be 34 --> wrong!! there is a cost that is lower (21) because it is better to not align the L 
(print-sequence-alignments (maximal-sequence-alignments "ALAT" "CLCT" nil nil
                             :match-cost -10 :mismatch-cost 30 :gap-opening-cost 25 :gap-cost 1
                             :remove-duplicate-alignments nil))

;; cost should be 7
 (print-sequence-alignments(maximal-sequence-alignments "LAT" "LCT" nil nil
                             :match-cost -10 :mismatch-cost 30 :gap-opening-cost 25 :gap-cost 1
                             :remove-duplicate-alignments nil))


;; cost should be 14
 (print-sequence-alignments(maximal-sequence-alignments "AXXXAT" "CXXXCT" nil nil
                             :match-cost -10 :mismatch-cost 30 :gap-opening-cost 25 :gap-cost 1
                             :remove-duplicate-alignments nil))



(print-sequence-alignments (maximal-sequence-alignments "AGGTCC" "AGCCT" nil nil
                             :match-cost 0 :mismatch-cost 1 :gap-opening-cost 1 :gap-cost 1
                             :remove-duplicate-alignments nil))


(print-sequence-alignments (maximal-sequence-alignments "AAATTTGC" "CGCCTTAC" nil nil
                                                   :match-cost -10 :mismatch-cost 30 :gap-opening-cost 40 :gap-cost 1
                                                   :remove-duplicate-alignments nil))


(print-sequence-alignments (maximal-sequence-alignments "AJCJNRCKCRBP" "ABCNJRQCLCRPM" nil nil
                                                 :match-cost -1 :mismatch-cost 1 :gap-opening-cost 0 :gap-cost 1
                                                 :remove-duplicate-alignments nil))

cost should be 1 
(print-sequence-alignments (maximal-sequence-alignments "xabcy" "xefy" nil nil
                                                 :match-cost -1 :mismatch-cost 1 :gap-opening-cost 0 :gap-cost 1
                                                 :remove-duplicate-alignments nil))



(test-alignments) ;; two tests need to fail
|#