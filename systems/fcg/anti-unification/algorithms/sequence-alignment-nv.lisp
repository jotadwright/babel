(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maximal Sequence Alignment - nv algorithm ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
                            (extended-gap-cost-other-side (if (> j 0) (+ (aref Q (- i 1) j) gap-extension-cost) most-positive-fixnum)) ;extend the gap of the horizontal matrix (look at the cost of position ((- i 1) j), since we are adding a vertical gap, we need to look at the same position for both the extended-gap-cost and the extended-gap-cost-other-side)
                            (new-gap-cost (+ (aref R (- i 1) j) gap-opening-cost gap-extension-cost))
                            (min-cost (min extended-gap-cost new-gap-cost extended-gap-cost-other-side))) ;min cost between opening a gap after match/mismatch, vertical gap-extension or horizontal gap-extension
                       (setf (aref P i j) min-cost)
                       (when (= min-cost extended-gap-cost)
                         (setf (aref d (- i 1) j) 1))
                       (when (= min-cost new-gap-cost)
                         (setf (aref e (- i 1) j) 1))))

                   ;; 3) find the minimum cost of a path ending at node N_{i,j using horizontal edge
                   ;; 4) determine if cost Q_{i,j} can be achieved with and without edge H_{i,j-1}, i.e. horizontal edge left
                   (when (> j 0)
                     (let* ((extended-gap-cost (+ (aref Q i (- j 1)) gap-extension-cost))
                            (extended-gap-cost-other-side (if (> i 0) (+ (aref P i (- j 1)) gap-extension-cost) most-positive-fixnum)) ;extend the gap of the vertical matrix (look at the cost of position (i (- j 1)), since we are adding a horizontal gap, we need to look at the same position for both the extended-gap-cost and the extended-gap-cost-other-side)
                            (new-gap-cost (+ (aref R i (- j 1)) gap-opening-cost gap-extension-cost))
                            (min-cost (min extended-gap-cost new-gap-cost extended-gap-cost-other-side))) ;min cost between opening a gap after match/mismatch, vertical gap-extension or horizontal gap-extension
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


#|
;; cost should be 21 (originally 46)
(print-sequence-alignments (maximal-sequence-alignments "AGAT" "CTCT" nil nil
                             :match-cost -10 :mismatch-cost 30 :gap-opening-cost 25 :gap-extension-cost 1
                             :remove-duplicate-alignments nil))

;; cost should be 11 (originally 36)
(maximal-sequence-alignments "XAGAT" "XCTCT" nil nil
                             :match-cost -10 :mismatch-cost 30 :gap-opening-cost 25 :gap-extension-cost 1
                             :remove-duplicate-alignments nil)

;; cost should be 31 (originally 56)
(maximal-sequence-alignments "ABA" "XYZ" nil nil
                             :match-cost -10 :mismatch-cost 30 :gap-opening-cost 25 :gap-extension-cost 1
                             :remove-duplicate-alignments nil)

;; cost should be 26 ??????  --> wrong ! there is a cost that is lower (13) because it is better to not align the L
(maximal-sequence-alignments "XAGLAT" "XCLTCT" nil nil
                             :match-cost -10 :mismatch-cost 30 :gap-opening-cost 25 :gap-extension-cost 1
                             :remove-duplicate-alignments nil)

;; cost should be 34 --> wrong!! there is a cost that is lower (21) because it is better to not align the L 
(maximal-sequence-alignments "ALAT" "CLCT" nil nil
                             :match-cost -10 :mismatch-cost 30 :gap-opening-cost 25 :gap-extension-cost 1
                             :remove-duplicate-alignments nil)

;; cost should be 7
(maximal-sequence-alignments "LAT" "LCT" nil nil
                             :match-cost -10 :mismatch-cost 30 :gap-opening-cost 25 :gap-extension-cost 1
                             :remove-duplicate-alignments nil)


;; cost should be 14
(maximal-sequence-alignments "AXXXAT" "CXXXCT" nil nil
                             :match-cost -10 :mismatch-cost 30 :gap-opening-cost 25 :gap-extension-cost 1
                             :remove-duplicate-alignments nil)



(print-sequence-alignments (maximal-sequence-alignments "AGGTCC" "AGCCT" nil nil
                             :match-cost 0 :mismatch-cost 1 :gap-opening-cost 1 :gap-extension-cost 1
                             :remove-duplicate-alignments nil))


(alignments-flouri-3)

|#