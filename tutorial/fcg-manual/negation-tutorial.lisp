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
;; (ql:quickload :fcg)

(in-package :fcg)
(activate-monitor trace-fcg)

;; Without negation: both noun-phrase-cxns apply ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions negation-tutorial
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits sequence))
  
(def-fcg-cxn the-cxn
             ((?the-word
               (args (?x))
               (sem-cat (sem-class referent))
               (syn-cat (lex-class article))
               (boundaries (left-most-subunit ?the-word)
                           (right-most-subunit ?the-word)))
              <-
              (?the-word
               (HASH meaning ((unique ?x)))                     
               --
               (HASH form ((string ?the-word  "the"))))))

(def-fcg-cxn mouse-cxn
             ((?mouse-word
               (args (?x))
               (sem-cat (sem-class physical-entity))
               (syn-cat (lex-class noun))
               (boundaries (left-most-subunit ?mouse-word)
                           (right-most-subunit ?mouse-word)))
              <-
              (?mouse-word
               (HASH meaning ((mouse ?x)))                     
               --
               (HASH form ((string ?mouse-word  "mouse"))))))

(def-fcg-cxn simple-noun-phrase-cxn
             ((?noun-phrase
               (args (?x))
               (sem-cat (sem-class referring-expression))
               (syn-cat (lex-class noun-phrase))
               (subunits (?noun))
               (boundaries (left-most-subunit ?noun)
                           (right-most-subunit ?noun)))
              <-
              (?noun
               (args (?x))
               (sem-cat (sem-class physical-entity))
               --
               (syn-cat (lex-class noun)))))

(def-fcg-cxn noun-phrase-cxn
             ((?noun-phrase
               (args (?x))
               (sem-cat (sem-class referring-expression))
               (syn-cat (lex-class noun-phrase))
               (subunits (?article ?noun))
               (boundaries (left-most-subunit ?article)
                           (right-most-subunit ?noun)))
              <-
              (?article
               (args (?x))
               (sem-cat (sem-class referent))
               --
               (syn-cat (lex-class article)))
              (?noun
               (args (?x))
               (sem-cat (sem-class physical-entity))
               --
               (syn-cat (lex-class noun)))
              (?noun-phrase
               --
               (HASH form ((meets ?article ?noun)))))))


(comprehend-all '("the" "mouse"))
(formulate-all '((unique o-1) (mouse o-1)))


;; With negation: only one applies ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions negation-tutorial
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits sequence))
  
(def-fcg-cxn the-cxn
             ((?the-word
               (args (?x))
               (sem-cat (sem-class referent))
               (syn-cat (lex-class article))
               (boundaries (left-most-subunit ?the-word)
                           (right-most-subunit ?the-word)))
              <-
              (?the-word
               (HASH meaning ((unique ?x)))                     
               --
               (HASH form ((string ?the-word  "the"))))))

(def-fcg-cxn mouse-cxn
             ((?mouse-word
               (args (?x))
               (sem-cat (sem-class physical-entity))
               (syn-cat (lex-class noun))
               (boundaries (left-most-subunit ?mouse-word)
                           (right-most-subunit ?mouse-word)))
              <-
              (?mouse-word
               (HASH meaning ((mouse ?x)))                     
               --
               (HASH form ((string ?mouse-word  "mouse"))))))

(def-fcg-cxn simple-noun-phrase-cxn
             ((?noun-phrase
               (args (?x))
               (sem-cat (sem-class referring-expression))
               (syn-cat (lex-class noun-phrase))
               (subunits (?noun))
               (boundaries (left-most-subunit ?noun)
                           (right-most-subunit ?noun)))
              (?noun
               (flow-control (applied noun-phrase-cxn)))
              <-
              (?noun
               (args (?x))
               (sem-cat (sem-class physical-entity))
               (flow-control (not (applied noun-phrase-cxn)))
               --
               (syn-cat (lex-class noun))
               (flow-control (not (applied noun-phrase-cxn))))))

(def-fcg-cxn noun-phrase-cxn
             ((?noun-phrase
               (args (?x))
               (sem-cat (sem-class referring-expression))
               (syn-cat (lex-class noun-phrase))
               (subunits (?article ?noun))
               (boundaries (left-most-subunit ?article)
                           (right-most-subunit ?noun)))
              (?noun
               (flow-control (applied noun-phrase-cxn)))
              <-
              (?article
               (args (?x))
               (sem-cat (sem-class referent))
               --
               (syn-cat (lex-class article)))
              (?noun
               (args (?x))
               (sem-cat (sem-class physical-entity))
               (flow-control (not (applied noun-phrase-cxn)))
               --
               (syn-cat (lex-class noun))
               (flow-control (not (applied noun-phrase-cxn))))
              (?noun-phrase
               --
               (HASH form ((meets ?article ?noun)))))
             :score 0.6))

(comprehend-all '("the" "mouse"))
(formulate-all '((unique o-1) (mouse o-1)))


