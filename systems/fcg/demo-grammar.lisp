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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toy Grammar for Demos Purposes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; This toy grammar serves in many demos.
;; It is used by FCG's (load-demo-grammar) function which can be used
;; to automatically load this grammar for tutorial purposes.

(def-fcg-constructions demo-grammar
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (constituents sequence)
                  (dependents sequence)
                  (footprints set))
  :hierarchy-features (constituents dependents)
  
  ;; Lexical constructions
  (def-fcg-cxn the-cxn
               ((?the-word
                 (args (?x))
                 (sem-cat (sem-class referent))
                 (syn-cat (lex-class article)))
                <-
                (?the-word
                 (HASH meaning ((unique ?x)))                     
                 --
                 (HASH form ((string ?the-word  "the"))))))

  (def-fcg-cxn mouse-cxn
               ((?mouse-word
                 (args (?x))
                 (sem-cat (sem-class physical-entity))
                 (syn-cat (lex-class noun)))
                <-
                (?mouse-word
                 (HASH meaning ((mouse ?x)))                     
                 --
                 (HASH form ((string ?mouse-word  "mouse"))))))
  
  (def-fcg-cxn likes-cxn
               ((?likes-word
                 (args (?x ?y))
                 (sem-cat (sem-class relation))
                 (syn-cat (lex-class verb)
                          (type transitive)))
                <-
                (?likes-word
                 (HASH meaning ((deep-affection ?x ?y)))                     
                 --
                 (HASH form ((string ?likes-word  "likes"))))))
  
  (def-fcg-cxn linguist-cxn
               ((?linguist-word
                 (args (?x))
                 (sem-cat (sem-class physical-entity))
                 (syn-cat (lex-class noun)))
                <-
                (?linguist-word
                 (HASH meaning ((linguist ?x)))                     
                 --
                 (HASH form ((string ?linguist-word  "linguist"))))))
  
  ;;Grammatical Constructions
  ;; NP -> ART NOUN
  (def-fcg-cxn noun-phrase-cxn
               ((?noun-phrase
                 (args (?x))
                 (sem-cat (sem-class referring-expression))
                 (syn-cat (lex-class noun-phrase))
                 (constituents (?article ?noun)))
                (?noun
                 (dependents (?article)))
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
                 (HASH form ((meets ?article ?noun))))))
  
  ;; VP -> V
  (def-fcg-cxn verb-phrase-cxn
               ((?verb-phrase
                 (args (?x ?y))
                 (sem-cat (sem-class relational-expression))
                 (syn-cat (lex-class verb-phrase)
                          (type transitive))
                 (constituents (?verb)))
                <-
                (?verb
                 (args (?x ?y))
                 (sem-cat (sem-class relation))
                 --
                 (syn-cat (lex-class verb)
                          (type transitive)))))
  
  ;; Transitive-clause -> NP VP NP
  (def-fcg-cxn transitive-clause-cxn
               ((?transitive-clause
                 (args (?x ?y))
                 (sem-cat (sem-class predicating-expression))
                 (syn-cat (lex-class transitive-clause))
                 (constituents (?subject-noun-phrase ?verb-phrase ?object-noun-phrase)))
                (?verb
                 (dependents (?subject-noun ?object-noun)))
                <-
                (?subject-noun-phrase
                 (args (?x))
                 (sem-cat (sem-class referring-expression))
                 (constituents (?subject-article ?subject-noun))
                 --
                 (syn-cat (lex-class noun-phrase))
                 (constituents (?subject-article ?subject-noun)))
                (?verb-phrase
                 (args (?x ?y))
                 (sem-cat (sem-class relational-expression))
                 (constituents (?verb))
                 --
                 (syn-cat (lex-class verb-phrase)
                          (type transitive))
                 (constituents (?verb)))
                (?object-noun-phrase
                 (args (?y))
                 (sem-cat (sem-class referring-expression))
                 (constituents (?object-article ?object-noun))
                 --
                 (syn-cat (lex-class noun-phrase))
                 (constituents (?object-article ?object-noun)))
                (?transitive-clause
                 --
                 (HASH form ((meets ?subject-noun ?verb)
                             (meets ?verb ?object-article)))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toy Grammar for Demos Purposes (update April 2023 after introduction of form as set of sequence predicates)  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; This toy grammar serves in many demos.
;; It is used by FCG's (load-demo-grammar) function which can be used
;; to automatically load this grammar for tutorial purposes.


(def-fcg-constructions demo-grammar-sequences
  :cxn-inventory *fcg-constructions-sequences*
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (meaning set-of-predicates)
                  (args sequence)
                  (sequences set-of-predicates)
                  (constituents sequence)
                  (dependents sequence)
                  (boundaries sequence)
                  (footprints set))
  :fcg-configurations ((:de-render-mode . :de-render-sequence)
                       (:render-mode . :render-sequences))
  :hierarchy-features (constituents dependents)
  
  ;; Lexical constructions
  (def-fcg-cxn the-cxn
               ((?the-word
                 (args (?x))
                 (sem-cat (sem-class referent))
                 (syn-cat (lex-class article))
                 (sequences ((sequence "the" ?left ?right))))
                <-
                (?the-word
                 (HASH meaning ((unique ?x)))                     
                 --
                 (HASH form ((sequence "the" ?left ?right))))))

  (def-fcg-cxn mouse-cxn
               ((?mouse-word
                 (args (?x))
                 (sem-cat (sem-class physical-entity))
                 (syn-cat (lex-class noun))
                 (sequences ((sequence "mouse" ?left ?right))))
                <-
                (?mouse-word
                 (HASH meaning ((mouse ?x)))                     
                 --
                 (HASH form ((sequence "mouse" ?left ?right))))))
  
  (def-fcg-cxn likes-cxn
               ((?likes-word
                 (args (?x ?y))
                 (sem-cat (sem-class relation))
                 (syn-cat (lex-class verb)
                          (type transitive))
                 (sequences ((sequence "likes" ?left ?right))))
                <-
                (?likes-word
                 (HASH meaning ((deep-affection ?x ?y)))                     
                 --
                 (HASH form ((sequence "likes" ?left ?right))))))
  
  (def-fcg-cxn linguist-cxn
               ((?linguist-word
                 (args (?x))
                 (sem-cat (sem-class physical-entity))
                 (syn-cat (lex-class noun))
                 (sequences ((sequence "linguist" ?left ?right))))
                <-
                (?linguist-word
                 (HASH meaning ((linguist ?x)))                     
                 --
                 (HASH form ((sequence "linguist" ?left ?right))))))
  
  ;;Grammatical Constructions
  ;; NP -> ART NOUN
  (def-fcg-cxn noun-phrase-cxn
               ((?noun-phrase
                 (args (?x))
                 (sem-cat (sem-class referring-expression))
                 (syn-cat (lex-class noun-phrase))
                 (constituents (?article ?noun))
                 (boundaries (?article-left ?noun-right)))
                (?noun
                 (dependents (?article)))
                <-
                (?article
                 (args (?x))
                 (sem-cat (sem-class referent))
                 --
                 (sequences ((sequence ?article-string ?article-left ?article-right)))
                 (syn-cat (lex-class article)))
                (?noun
                 (args (?x))
                 (sem-cat (sem-class physical-entity))                   
                 --
                 (syn-cat (lex-class noun))
                 (sequences ((sequence ?noun-string ?noun-left ?noun-right))))
                (?noun-phrase
                 --
                 (HASH form ((sequence " " ?article-right ?noun-left))))))
  
  ;; VP -> V
  (def-fcg-cxn verb-phrase-cxn
               ((?verb-phrase
                 (args (?x ?y))
                 (sem-cat (sem-class relational-expression))
                 (syn-cat (lex-class verb-phrase)
                          (type transitive))
                 (boundaries (?verb-left ?verb-right))
                 (constituents (?verb)))
                <-
                (?verb
                 (args (?x ?y))
                 (sem-cat (sem-class relation))
                 --
                 (sequences ((sequence ?verb-string ?verb-left ?verb-right)))
                 (syn-cat (lex-class verb)
                          (type transitive)))))
  
  ;; Transitive-clause -> NP VP NP
  (def-fcg-cxn transitive-clause-cxn
               ((?transitive-clause
                 (args (?x ?y))
                 (sem-cat (sem-class predicating-expression))
                 (syn-cat (lex-class transitive-clause))
                 (boundaries (?subject-np-left ?object-np-right))
                 (constituents (?subject-noun-phrase ?verb-phrase ?object-noun-phrase)))
                (?verb
                 (dependents (?subject-noun ?object-noun)))
                <-
                (?subject-noun-phrase
                 (args (?x))
                 (sem-cat (sem-class referring-expression))
                 (constituents (?subject-article ?subject-noun))
                 --
                 (boundaries (?subject-np-left ?subject-np-right))
                 (syn-cat (lex-class noun-phrase))
                 (constituents (?subject-article ?subject-noun)))
                (?verb-phrase
                 (args (?x ?y))
                 (sem-cat (sem-class relational-expression))
                 (constituents (?verb))
                 --
                 (boundaries (?vp-left ?vp-right))
                 (syn-cat (lex-class verb-phrase)
                          (type transitive))
                 (constituents (?verb)))
                (?object-noun-phrase
                 (args (?y))
                 (sem-cat (sem-class referring-expression))
                 (constituents (?object-article ?object-noun))
                 --
                 (boundaries (?object-np-left ?object-np-right))
                 (syn-cat (lex-class noun-phrase))
                 (constituents (?object-article ?object-noun)))
                (?transitive-clause
                 --
                 (HASH form ((sequence " " ?subject-np-right ?vp-left)
                             (sequence " " ?vp-right ?object-np-left)))))))

;;  (comprehend-and-formulate "the linguist likes the mouse" :cxn-inventory *fcg-constructions-sequences*)

(defun test-render-all ()
  (multiple-value-bind (utterance cipn) (comprehend-and-formulate "the linguist the mouse" :cxn-inventory *fcg-constructions-sequences*)
    (render-all (fcg-extract-selected-form-constraints (car-resulting-cfs (cipn-car cipn)) '(sequence))
                :render-sequences)))
;(test-render-all)


;(comprehend-and-formulate "the linguist" :cxn-inventory *fcg-constructions-sequences*)
