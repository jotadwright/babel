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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     ;;
;;    How to Use NLP-TOOLS package     ;;
;;                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Loading the system
(ql:quickload :nlp-tools)
(in-package :nlp-tools)

;; Noun Chunking
(get-penelope-noun-chunks "April is the fourth month of the year.")
(get-penelope-noun-chunks "The study, carried out at Geomar Helmholtz Centre for Ocean Research in Germany, was the most comprehensive of the subject to date.")

;; Part Of Speech Tagging
(get-penelope-pos-tags "April is the fourth month of the year.")
(get-penelope-pos-tags "Part-of-speech tagging is considered a solved problem.")

;; Named Entity Recognition
(get-penelope-named-entities "The study, carried out at Geomar Helmholtz Centre for Ocean Research in Germany, was the most comprehensive of the subject to date.")
(get-penelope-named-entities "Germany, France and Italy, are countries in Europe.")

;; Dependency Parsing
(run-penelope-dependency-parser "April is the fourth month of the year.")
(run-penelope-dependency-parser "The man ate an apple.")

;; Tokenizer
(get-penelope-tokens "Paul kicked the ball. Mary caught it.")
(get-penelope-tokens "The study, carried out at Geomar Helmholtz Centre for Ocean Research in Germany, was the most comprehensive of the subject to date.")

;; Sentence Tokenizer
(get-penelope-sentence-tokens "Paul kicked the ball. Mary caught it.")
(get-penelope-sentence-tokens "Mrs. Smith returned the favour. She bought him a pie.")
(get-penelope-sentence-tokens "The study, carried out at Geomar Helmholtz Centre for Ocean Research in Germany, was the most comprehensive of the subject to date.")

;; Word similarity (distributional semantics)
(get-word-similarity "boy" "girl")
(get-word-similarity "boy" "soup")

;; Get word embeddings (distributional semantics)
(get-word-embeddings "Hello world!")

;; Phrase similarity (distributional semantics)
(get-phrase-similarity "handsome boy" "pretty girl")
(get-phrase-similarity "handsome boy" "vegetable chair")