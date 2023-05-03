;;;;; -----------------------------------------------------------------------------------
;;;;; (c) Sony Computer Science Laboratories Paris
;;;;;     Author: Remi van Trijp - remi.vantrijp@sony.com
;;;;; -----------------------------------------------------------------------------------

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

(in-package :cl-wikimedia)

;; These examples are inspired by the examples from: 
;; https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries/examples
;; -----------------------------------------------------------------------------

;; SIMPLE QUERIES
;; -----------------------------------------------------------------------------

;; Return all labels of entities that are instances of (P31) a housecat (Q146):
(wikidata-sparql-query
 (compose-sparql-query
  :select '("?item" "?itemLabel")
  :where '("?item wdt:P31 wd:Q146"
           (service-wikibase "label")))
 :lisp-format :alist)

;; Humans without children:
(wikidata-sparql-query
 (compose-sparql-query
  :select '("?human" "?humanLabel")
  :where '("?human wdt:P31 wd:Q5" ;; find humans
           "?human p:P40 ?childStatement" ;; with at least one child statement
           "?childStatement rdf:type wdno:P40" ;; where the statement is defined to be "no value"
           (service-wikibase "label"))))


;; LEXEME QUERIES
;; -----------------------------------------------------------------------------
;; Lemmatize a word in English:
(wikidata-sparql-query
 (compose-sparql-query
  :select-distinct '("?l" "?word" "?lemma")
  :where '("VALUES ?word {'bought'@en}"
           "?l a ontolex:LexicalEntry ;
                dct:language wd:Q1860 ;
                wikibase:lemma ?lemma ;
                ontolex:lexicalForm ?form"
           "?form ontolex:representation ?word")))

;; Dutch idioms:
(wikidata-sparql-query
 (compose-sparql-query
  :select '("?l" "?lemma")
  :where '("?l dct:language wd:Q7411; 
               wikibase:lemma ?lemma;
               wikibase:lexicalCategory wd:Q184511")))

;; Examples of lexemes in Dutch that demonstrate both form and meaning:
(wikidata-sparql-query
 (compose-sparql-query 
  :SELECT '("?lexemeId" "?lemma" "?example")
  :where '("?lexemeId <http://purl.org/dc/terms/language> wd:Q7411;
                      wikibase:lemma ?lemma"
           "?lexemeId p:P5831 ?statement"
           "?statement ps:P5831 ?example;
                       pq:P6072 [];
                       pq:P5830 []")))

;; Color lexemes:
(wikidata-sparql-query
 (compose-sparql-query
  :SELECT '("?l" "?lemma" "?languageLabel")
  :WHERE '("?l a ontolex:LexicalEntry; 
                 dct:language ?language; 
                 wikibase:lemma ?lemma"
           "?l wdt:P31 wd:Q376431"
           (service-wikibase "label"))
  :order-by "?languageLabel"))


;; FUN EXAMPLES
;; -----------------------------------------------------------------------------
;; Gotta catch them all!
(wikidata-sparql-query
 (compose-sparql-query
  :select-distinct "?pokemon ?pokemonLabel ?pokedexNumber"
  :where '("?pokemon wdt:P31/wdt:P279* wd:Q3966183"
           "?pokemon p:P1685 ?statement"
           "?statement ps:P1685 ?pokedexNumber;
                       pq:P972 wd:Q20005020"
           "FILTER (! wikibase:isSomeValue(?pokedexNumber) )"
           (service-wikibase "label"))
  :order-by "(?pokedexNumber)"))