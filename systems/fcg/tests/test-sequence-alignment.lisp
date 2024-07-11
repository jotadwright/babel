(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing sequence alignments algorithm with examples from papers ;;
;; Testing remove-duplicates                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun alignments-needleman-wunsch-1 ()
  ;; Wikipedia example 
  ;; GCATGCG & GATTACA
  ;; score 0 ; match 0, mismatch 1, gap-opening 0, gap-extension 1
  (let ((alignments (maximal-sequence-alignments "GCATGCG" "GATTACA" nil nil
                                                 :match-cost -1 :mismatch-cost 1 :gap-opening-cost 0 :gap-cost 1
                                                 :remove-duplicate-alignments nil)))
     (loop for alignment in alignments
               for cost = (cost alignment)
               always (= cost 0))))

(defun alignments-needleman-wunsch-2 ()
  ;; Example from original needleman wunsch paper: A General Method Applicable to the Search for Similarities in the Amino Acid Sequence of Two Proteins
  ;; AJCJNRCKCRBP & ABCNJRQCLCRPM
  ;; A J C _ J N R _ C K C R B P _
  ;; A B C N J _ R Q C L C R _ P M
  
  ;; A J C J N _ R _ C K C R B P _
  ;; A B C _ N J R Q C L C R _ P M 
  ;; score -1 (obtained from online tool) ; match 0, mismatch 1, gap-opening 0, gap-extension 1
  (let ((alignments (maximal-sequence-alignments "AJCJNRCKCRBP" "ABCNJRQCLCRPM" nil nil
                                                 :match-cost -1 :mismatch-cost 1 :gap-opening-cost 0 :gap-cost 1
                                                 :remove-duplicate-alignments nil)))
     (and (loop for alignment in alignments
                for cost = (cost alignment)
                always (= cost -1))
          (= (length alignments) 2))))


(defun alignments-altschul-erickson-1 ()
  ;; Paper:  Optimal sequence alignment using affine gap costs - Stephen F. Altschul & Bruce W. Erickson 
  ;; AGT & TGAGTT
  ;; 3 solutions, cost 5 ; match 0, mismatch 1, gap-opening 1, gap-extension 1
  ;; _ _ A G T _
  ;; T G A G T T

  ;; _ _ A G _ T
  ;; T G A G T T

  ;; A G _ _ T _
  ;; T G A G T T

  (let ((alignments (maximal-sequence-alignments "AGT" "TGAGTT" nil nil
                                                 :match-cost 0 :mismatch-cost 1 :gap-opening-cost 1 :gap-cost 1
                                                 :remove-duplicate-alignments nil)))
    (and (loop for alignment in alignments
               for cost = (cost alignment)
               always (= cost 5))
         (= (length alignments) 3))))

(defun alignments-altschul-erickson-2 ()
  ;; Paper:  Optimal sequence alignment using affine gap costs - Stephen F. Altschul & Bruce W. Erickson 
  ;; AAAGGG & TTAAAAGGGGTT
  ;; 1 solution, cost 15 ; match 0, mismatch 1, gap-opening 5, gap-extension 1
  ;; A A A _ _ _ _ _ _ G G G
  ;; T T A A A A G G G G T T
  
  (let ((alignments (maximal-sequence-alignments "AAAGGG" "TTAAAAGGGGTT" nil nil
                                                 :match-cost 0 :mismatch-cost 1 :gap-opening-cost 5 :gap-cost 1
                                                 :remove-duplicate-alignments nil)))
    (and (loop for alignment in alignments
               for cost = (cost alignment)
               always (= cost 15))
         (= (length alignments) 1))))

(defun alignments-freiburg-tool-1 ()
  ;; example from the Freiburg tool that is an implementation of Gotoh's algorithm
  ;; http://rna.informatik.uni-freiburg.de/Teaching/index.jsp?toolName=Gotoh
  ;; CG & CCGA
  ;; 2 solutions, cost 5 ; match 1, mismatch -1, gap-opening -3, gap-extension -1 (we have to switch the negative and positive number since we compute a cost, not a score)
  ;; C _ _ G
  ;; C C G A
  
  ;; C G _ _
  ;; C C G A
  
  (let ((alignments (maximal-sequence-alignments "CG" "CCGA" nil nil
                                                 :match-cost -1 :mismatch-cost 1 :gap-opening-cost 3 :gap-cost 1
                                                 :remove-duplicate-alignments nil)))
     (and (loop for alignment in alignments
                            for cost = (cost alignment)
                            always (= cost 5))
                      (= (length alignments) 2))))

(defun alignments-freiburg-tool-2 ()
    ;; example from the Freiburg tool that is an implementation of Needleman-wunsch algorithm
    ;; http://rna.informatik.uni-freiburg.de/Teaching/index.jsp?toolName=Needleman-Wunsch
    ;; AATCG & AACG
    ;; 1 solution, cost 2 ; match 1, mismatch -1, gap-opening 0, gap-extension -2 (we have to switch the negative and positive number since we compute a cost, not a score)
    ;; A A T C G
    ;; A A _ C G
    
    (let ((alignments (maximal-sequence-alignments "AATCG" "AACG" nil nil
                                                   :match-cost -1 :mismatch-cost 1 :gap-opening-cost 0 :gap-cost 2
                                                   :remove-duplicate-alignments nil)))
      (and (loop for alignment in alignments
                              for cost = (cost alignment)
                              always (= cost -2))
                        (= (length alignments) 1))))


(defun alignments-flouri-1 ()
    ;; example from paper Are all global alignment algorithms and implementations correct? -  Flouri et al.
    ;; ? solutions, score -15, match 0, mismatch -1, gap opening -5, gap extension -1
    ;; A A A _ _ _ _ _ _ G G G
    ;; T T A A A A G G G G T T
    (let ((alignments (maximal-sequence-alignments "AAAGGG" "TTAAAAGGGGTT" nil nil
                                                   :match-cost 0 :mismatch-cost 1 :gap-opening-cost 5 :gap-cost 1
                                                   :remove-duplicate-alignments nil)))
    (loop for alignment in alignments
            for cost = (cost alignment)
            always (= cost 15))))

(defun alignments-flouri-2 ()
    ;; AAATTTGC & CGCCTTAC
    ;; ? solutions, score -72, match 10, mismatch -30, gap opening -40, gap extension -1
    ;; C G C C T T A _ _ _ _ _ _ C
    ;; _ _ _ _ _ _ A A A T T T G C
    (let ((alignments (maximal-sequence-alignments "AAATTTGC" "CGCCTTAC" nil nil
                                                   :match-cost -10 :mismatch-cost 30 :gap-opening-cost 40 :gap-cost 1
                                                   :remove-duplicate-alignments nil)))
      (loop for alignment in alignments
            for cost = (cost alignment)
            always (= cost 72))))

(defun alignments-flouri-3 ()
    ;; AGAT & CTCT 
    ;;  ? solutions, score -46, match 10, mismatch -30, gap opening -25, gap extension -1
    ;; _ _ _ A G A T
    ;; C T C _ _ _ T
    (let ((alignments (maximal-sequence-alignments "AGAT" "CTCT" nil nil
                                                   :match-cost -10 :mismatch-cost 30 :gap-opening-cost 25 :gap-cost 1
                                                   :remove-duplicate-alignments nil)))
       (loop for alignment in alignments
            for cost = (cost alignment)
            always (= cost 46))))



(deftest test-alignments ()
  ;; tests the correct behaviour of maximal-sequence-alignments, examples taken from papers
  (test-assert (alignments-altschul-erickson-1))
  (test-assert (alignments-altschul-erickson-2))
  (test-assert (alignments-freiburg-tool-1))
  (test-assert (alignments-freiburg-tool-2))
  (test-assert (alignments-flouri-1))
  (test-assert (alignments-flouri-2))
  (test-assert (alignments-flouri-3))
  (test-assert (alignments-needleman-wunsch-1))
  (test-assert (alignments-needleman-wunsch-2)))

;; (test-alignments)


(deftest test-remove-duplicates ()
  (let ((alignments (maximal-sequence-alignments "CG" "CCGA" nil nil
                                                 :match-cost -1 :mismatch-cost 1 :gap-opening-cost 3 :gap-cost 1
                                                 :remove-duplicate-alignments t)))
     (test-assert (and (loop for alignment in alignments
                            for cost = (cost alignment)
                            always (= cost 5))
                      (= (length alignments) 1))))
  
  (let ((alignments (maximal-sequence-alignments "AGT" "TGAGTT" nil nil
                                                    :match-cost 0 :mismatch-cost 1 :gap-opening-cost 1 :gap-cost 1
                                                    :remove-duplicate-alignments t)))
    (test-assert (and (loop for alignment in alignments
                            for cost = (cost alignment)
                            always (= cost 5))
                      (= (length alignments) 3)))))


;(test-remove-duplicates)
