;(ql:quickload :amr-grammar)

(in-package :amr-grammar)

(activate-monitor trace-fcg)

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 1. President Obama
;; '((PRESIDENT P) (NAME N) (:NAME P N) (:OP1 N "Obama")))
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(comprehend "President Obama")
(equivalent-amr-predicate-networks (comprehend "President Obama")
           '((PRESIDENT P) (NAME N) (:NAME P N) (:OP1 N "Obama")))

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 2. The marble is white
;; '((WHITE W) (MARBLE M) (:DOMAIN W M)))
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(comprehend "the marble is white") 
(equivalent-amr-predicate-networks (comprehend "the marble is white")
             '((WHITE W) (MARBLE M) (:DOMAIN W M)))

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 3. The boy cannot go 
;; '((POSSIBLE P) (GO-01 G) (BOY B) (:DOMAIN P G) (:POLARITY P -) (:ARG0 G B)))
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(comprehend "the boy cannot go")
(equivalent-amr-predicate-networks (comprehend "the boy cannot go")
            '((POSSIBLE P) (GO-01 G) (BOY B) (:DOMAIN P G) (:POLARITY P -) (:ARG0 G B)))

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 4. "The girl's opinion" and "What the girl opined": same meaning but different constructions
;; '((THING T) (OPINE-01 O) (GIRL G) (:ARG1-OF T O) (:ARG0 O G)))
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(comprehend "the girl 's opinion") 
(equivalent-amr-predicate-networks (comprehend "the girl 's opinion")
             '((THING T) (OPINE-01 O) (GIRL G) (:ARG1-OF T O) (:ARG0 O G)))

(comprehend "what the girl opined") 
(equivalent-amr-predicate-networks (comprehend "what the girl opined")
            '((THING T) (OPINE-01 O) (GIRL G) (:ARG1-OF T O) (:ARG0 O G)))

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 5. "A taxable fund" and "An edible sandwich" : two AMR meaning for quite similar adjectives
;;             1. ((FUND F) (TAX-01 T) (:ARG1-OF F T)))
;;             2. ((SANDWICH S) (EAT-01 E) (POSSIBLE P) (:ARG1-OF S E) (:DOMAIN-OF E P)))
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(comprehend "an edible sandwich") 
(equivalent-amr-predicate-networks (comprehend "an edible sandwich")
           '((SANDWICH S) (EAT-01 E) (POSSIBLE P) (:ARG1-OF S E) (:DOMAIN-OF E P)))

(comprehend-all "a taxable fund") 
(equivalent-amr-predicate-networks (comprehend "a taxable fund")
                                   '((FUND F) (TAX-01 T) (:ARG1-OF F T)))

(evaluate-amr-grammar)

