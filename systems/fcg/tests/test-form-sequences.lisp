;; Copyright 2019 AI Lab, Vrije Universiteit Brussel - Sony CSL Paris

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================
(in-package :fcg)

(deftest test-calculate-unmatched-root-intervals ()
  
  (test-equalp (calculate-unmatched-intervals '((19 22)) ;; intervals matched by cxn
                                              '((0 4) (12 28))) ;; intervals present in root before matching
               '((0 4) (12 19) (22 28)))

  (test-equalp (calculate-unmatched-intervals '((12 17)) ;;cxn
                                              '((12 17) (25 29))) ;;root
               '((25 29))) 

  (test-equalp (calculate-unmatched-intervals '((12 17))
                                              '((0 30)))
               '((0 12) (17 30)))

  (test-equalp (calculate-unmatched-intervals '((0 12) (17 25) (29 30))
                                              '((0 30)))
               '((12 17) (25 29)))

  (test-equalp (calculate-unmatched-intervals '((0 12) (17 25))
                                              '((0 30)))
               '((12 17) (25 30)))

  (test-equalp (calculate-unmatched-intervals '((0 12) (17 25) (29 30))
                                              '((0 12) (17 30)))
               '((25 29)))

  (test-equalp (calculate-unmatched-intervals '((0 12) (17 25) (29 30))
                                              '((0 25) (29 30)))
               '((12 17)))

  (test-equalp (calculate-unmatched-intervals '((2 12) (17 25) (29 30))
                                              '((0 25) (29 30)))
               '((0 2) (12 17)))
  
  (test-equalp (calculate-unmatched-intervals '((2 5))
                                              '((0 6) (7 12) (15 25)))
               '((0 2) (5 6) (7 12) (15 25)))

  (test-equalp (calculate-unmatched-intervals '((3 7))
                                              '((0 8) (10 15) (16 25)))
               '((0 3) (7 8) (10 15) (16 25)))

  (test-equalp (calculate-unmatched-intervals '((19 22))
                                              '((0 16) (17 23)))
               '((0 16) (17 19) (22 23)))
  
  (test-equalp (calculate-unmatched-intervals '((19 22))
                                              '((0 4) (12 28)))
               '((0 4) (12 19) (22 28))))

;;(test-calculate-unmatched-root-intervals)


(deftest test-recompute-root-sequence-features-based-on-bindings ()
  
  (test-equalp (recompute-root-sequence-features-based-on-bindings '(?left-cxn ?right-cxn)
                                                                   '((sequence "what is the color of the cube?" 0 30))
                                                                   '((?left-cxn . 8) (?right-cxn . 11)))
               '((SEQUENCE "what is " 0 8) (SEQUENCE " color of the cube?" 11 30)))

  (test-equalp (recompute-root-sequence-features-based-on-bindings '(?left-cxn ?right-cxn)
                                                                   '((sequence "what is the " 0 12)
                                                                     (sequence " of the cube?" 17 30))
                                                                   '((?left-cxn . 29) (?right-cxn . 30)))
               '((SEQUENCE "what is the " 0 12) (SEQUENCE " of the cube" 17 29)))

  (test-equalp (recompute-root-sequence-features-based-on-bindings '(?right-2 ?left-cxn ?right-cxn ?left-2)
                                                                   '((sequence "what is the " 0 12)
                                                                     (sequence " of the cube?" 17 30))
                                                                   '((?left-cxn . 29) (?right-cxn . 30)
                                                                     (?left-2 . 0) (?right-2 . 4)))
               '((SEQUENCE " is the " 4 12) (SEQUENCE " of the cube" 17 29)))
  
  (test-equalp (recompute-root-sequence-features-based-on-bindings '(?SUBJECT-RIGHT-10378 ?TO-BE-LEFT-6253)
                                                                   '((SEQUENCE " " 1 2) (SEQUENCE " ch" 5 8) (SEQUENCE "irm" 9 12) (SEQUENCE " of " 14 18) (SEQUENCE " committee" 21 31))
                                                                   '((?X-BE-UNIT-15438 . #:X-BE-UNIT-1088) (?TAG-271548 FCG:FORM NIL) (?TO-BE-RIGHT-6253 . 5) (?TO-BE-LEFT-6253 . 2) (?TO-BE-STRING-6253 . "was") (?TO-BE-UNIT-23889 . #:WAS-UNIT-1580) (?SUBJECT-RIGHT-10378 . 1) (?SUBJECT-LEFT-10378 . 0) (?SUBJECT-STRING-10378 . "I") (?NUMBER-47265 . FCG::SINGULAR) (?SUBJECT-UNIT-27834 . #:I-UNIT-2165)))
               '((SEQUENCE " ch" 5 8) (SEQUENCE "irm" 9 12) (SEQUENCE " of " 14 18) (SEQUENCE " committee" 21 31))))

;;(test-recompute-root-sequence-features-based-on-bindings)