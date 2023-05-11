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

;; 1. SIMPLE QUERIES
;; -----------------------------------------------------------------------------
;; 1.1 Return all labels of entities that are instances of (P31) a housecat (Q146):
(wikidata-sparql-query
 (compose-sparql-query
  :select '("?item" "?itemLabel")
  :where '("?item wdt:P31 wd:Q146"
           (service-wikibase "label")))
 :lisp-format :alist)

;; 1.2 Goats (this time using our helper function)
(wikidata-sparql-query
 (sparql-wikidata-item-is-instance-of "Q2934"))

;; 1.3 Horses (showing some info about them)
;; This query looks at items whose value of instance of (P31) is horse (Q726) or any subclass of (subclass of (P279)) horse (Q726). It displays the value of mother (P25), father (P22), sex or gender (P21) and computes birth year using date of birth (P569) death year using date of death (P570). Items are ordered using the horses qid.
(wikidata-sparql-query
 (compose-sparql-query
  :select-distinct "?horse ?horseLabel ?mother ?motherLabel ?father ?fatherLabel (year(?birthdate) as ?birthyear) (year(?deathdate) as ?deathyear) ?genderLabel"
  :where '("?horse wdt:P31/wdt:P279* wd:Q726"
           (sparql-optional '("?horse wdt:P25 ?mother"
                              "?horse wdt:P22 ?father"
                              "?horse wdt:P569 ?birthdate"
                              "?horse wdt:P570 ?deathdate"
                              "?horse wdt:P21 ?gender")))
  :order-by "?horse"))

;; 1.4 Cats with pictures
;; This query looks at all items with value of instance of (P31) equals to house cat (Q146) with a image (P18).
(wikidata-sparql-query
 (compose-sparql-query
  :select "?item ?itemLabel ?pic"
  :where '("?item wdt:P31 wd:Q146"
           "?item wdt:P18 ?pic"
           (service-wikibase "label"))))

;; 1.5 Map of hospitals
;; The query displays all items whose value of instance of (P31) is hospital (Q16917) or any subclass of (P279) of hospital (Q16917) with coordinate location (P625).
(wikidata-sparql-query
 (compose-sparql-query
  :select "?human ?humanLabel"
  :where '("?human wdt:P31 wd:Q5"
           "?human rdf:type wdno:P40"
           (service-wikibase "label"))))

;; 1.6 Humans without children (includes non-truthy values):
;; A similar query which also considers non-truthy (for example, values with deprecated rank) "no value" statements:
(wikidata-sparql-query
 (compose-sparql-query
  :select '("?human" "?humanLabel")
  :where '("?human wdt:P31 wd:Q5" ;; find humans
           "?human p:P40 ?childStatement" ;; with at least one child statement
           "?childStatement rdf:type wdno:P40" ;; where the statement is defined to be "no value"
           (service-wikibase "label"))))

;; 1.7 Humans born in New York City
;; This example highlights the correct way to use the place of birth (P19) property, and by extension the place of death (P20) property. place of birth (P19) is the most specific known place of birth. For example, it is known that Donald Trump (Donald Trump (Q22686)) was born in the Jamaica Hospital (Jamaica Hospital Medical Center (Q23497866)) in New York City. Therefore, he wouldn't show up in direct query for humans born in New York City.
(wikidata-sparql-query
 (compose-sparql-query
  :select-distinct "?item ?itemLabel ?itemDescription ?sitelinks"
  :where '("?item wdt:P31 wd:Q5;           
                  wdt:P19/wdt:P131* wd:Q60;
                  wikibase:sitelinks ?sitelinks"
           (service-wikibase "label"))
  :order-by "DESC(?sitelinks)"))


;; 2. LEXEME QUERIES
;; -----------------------------------------------------------------------------
;; 2.1 Lemmatize a word in English:
(wikidata-sparql-query
 (compose-sparql-query
  :select-distinct '("?l" "?word" "?lemma")
  :where '("VALUES ?word {'bought'@en}"
           "?l a ontolex:LexicalEntry ;
                dct:language wd:Q1860 ;
                wikibase:lemma ?lemma ;
                ontolex:lexicalForm ?form"
           "?form ontolex:representation ?word")))

;; 2.2 Dutch idioms:
(wikidata-sparql-query
 (compose-sparql-query
  :select '("?l" "?lemma")
  :where '("?l dct:language wd:Q7411; 
               wikibase:lemma ?lemma;
               wikibase:lexicalCategory wd:Q184511")))

;; 2.3 Examples of lexemes in Dutch that demonstrate both form and meaning:
(wikidata-sparql-query
 (compose-sparql-query 
  :SELECT '("?lexemeId" "?lemma" "?example")
  :where '("?lexemeId <http://purl.org/dc/terms/language> wd:Q7411;
                      wikibase:lemma ?lemma"
           "?lexemeId p:P5831 ?statement"
           "?statement ps:P5831 ?example;
                       pq:P6072 [];
                       pq:P5830 []")))

;; 2.4 Color lexemes:
(wikidata-sparql-query
 (compose-sparql-query
  :SELECT '("?l" "?lemma" "?languageLabel")
  :WHERE '("?l a ontolex:LexicalEntry; 
                 dct:language ?language; 
                 wikibase:lemma ?lemma"
           "?l wdt:P31 wd:Q376431"
           (service-wikibase "label"))
  :order-by "?languageLabel"))

;; 3. FUN EXAMPLES
;; -----------------------------------------------------------------------------
;; 3.1 Gotta catch them all!
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

;; 3.2 Fictional universes with the most fictional planets
(wikidata-sparql-query
 (compose-sparql-query
  :select "?universe (SAMPLE(?label) AS ?label) (COUNT(?planet) AS ?count)"
  :where '("?planet wdt:P31 wd:Q2775969;
                    wdt:P1080 ?universe"
           "?universe rdfs:label ?label"
           "FILTER(LANG(?label) = \"en\")")
  :group-by "?universe"
  :order-by "DESC(?count)")
 :lisp-format :alist)

;; 3.3 Popes with children (limited at 50)
(wikidata-sparql-query
 (compose-sparql-query
  :select "(SAMPLE(?father) as ?father) ?fatherLabel  (SAMPLE(?picture) as ?picture) (COUNT(?father) as ?children)"
  :where '("?subj wdt:P22 ?father"
           "?father wdt:P31 wd:Q5"
           "?father wdt:P39 wd:Q19546"
           "OPTIONAL { ?father wdt:P18 ?picture . }"
           (service-wikibase "label"))
  :limit 50
  :group-by "?fatherLabel"
  :order-by "DESC(?children)")
 :lisp-format :alist)

;; 3.4 Sony research institutes on wikidata
(wikidata-sparql-query
 (compose-sparql-query
  :select "?institute ?instituteLabel"
  :where '("?institute wdt:P31 wd:Q31855;
                       wdt:P749 wd:Q41187"
           (service-wikibase "label" "en")))
 :lisp-format :alist)

;; 4. Wikibase predicates
;; -----------------------------------------------------------------------------
;; 4.1 Properties grouped by their parent property:
(wikidata-sparql-query
 (compose-sparql-query
  :select "?property2 ?property2Label ?property1 ?property1Label"
  :where '("?property1 rdf:type wikibase:Property"
           "?property1 wdt:P1647 ?property2"
           (service-wikibase "label"))))

;; 4.2 Subproperties of "location"
(wikidata-sparql-query
 (compose-sparql-query
  :select-distinct "?subProperties ?subPropertiesLabel"
  :where '("?subProperties wdt:P1647* wd:P276"
           (service-wikibase "label"))))

;; 4.3 All properties with descriptions and aliases and types
(wikidata-sparql-query
 (compose-sparql-query
  :select "?property ?propertyType ?propertyLabel ?propertyDescription ?propertyAltLabel"
  :where '("?property wikibase:propertyType ?propertyType"
           (service-wikibase "label"))
  :order-by "ASC(xsd:integer(STRAFTER(STR(?property), 'P')))"))

;; 4.4 Properties connecting items of type zoo (Q43501) with items of type animal (Q729)
(wikidata-sparql-query
 (compose-sparql-query
  :select "?p ?pLabel (count (*) as ?count)"
  :where '("?s ?pd ?o"
           "?p wikibase:directClaim ?pd"
           "?s wdt:P31/wdt:P279* wd:Q729"
           "?o wdt:P31/wdt:P279* wd:Q43501"
           (service-wikibase "label"))
  :group-by "?p ?pLabel" 
  :order-by "desc(?count)"))

;; 5. GEOGRAPHY
;; -----------------------------------------------------------------------------
;; 5.1 List of UN member states
(wikidata-sparql-query
 (compose-sparql-query
  :select-distinct "?state"
  :where '("?state wdt:P31/wdt:P279* wd:Q3624078;
                   p:P463 ?memberOfStatement"
           "?memberOfStatement a wikibase:BestRank;
                               ps:P463 wd:Q1065"
           "MINUS { ?memberOfStatement pq:P582 ?endTime. }"
           "MINUS { ?state wdt:P576|wdt:P582 ?end. }")))

;; 5.2 Languages spoken in The Netherlands with their optional wikipedia editions
(wikidata-sparql-query
 (compose-sparql-query
  :select-distinct "?language ?languageLabel ?wikipediaLabel"
  :where '("?language wdt:P31 ?instance;
                      wdt:P17 wd:Q55"
           "FILTER (?instance in (wd:Q34770, wd:Q33384))"
           (sparql-optional '("?wikipedia wdt:P31 wd:Q10876391;
                                          wdt:P407 ?language"))
           (service-wikibase "label" "nl,en"))
  :group-by "?language ?languageLabel ?wikipediaLabel"))

;; 5.3 Destinations from Antwerp airport
(wikidata-sparql-query
 (compose-sparql-query
  :select "?connectsairport ?connectsairportLabel ?place_served ?place_servedLabel ?coor"
  :where '("VALUES ?airport { wd:Q17480 }"
           "?airport wdt:P81 ?connectsairport ;
                     wdt:P625 ?base_airport_coor"
           "?connectsairport wdt:P931 ?place_served ;
                             wdt:P625 ?coor"
           (service-wikibase "label"))))

;; 6. Demography
;; -----------------------------------------------------------------------------
;; 6.1 Birthplaces of humans named "Remi" (Q20001985)
(wikidata-sparql-query
 (compose-sparql-query
  :select "?item ?itemLabel ?coord"
  :where '("?item wdt:P31 wd:Q5" ; human
           "?item wdt:P735 wd:Q20001985"
           "?item wdt:P19 ?place"
           "?place wdt:P625 ?coord"
           (service-wikibase "label" "nl")))
 :lisp-format :alist)

;; 7. Politics
;; -----------------------------------------------------------------------------
;; 7.1 Members of the French National Assembly born outside of France.
(wikidata-sparql-query
 (compose-sparql-query
  :select-distinct "?item ?itemLabel ?placeLabel ?countryLabel"
  :where '("?item wdt:P39 wd:Q3044918"
           "?item wdt:P19 ?place"
           "?place wdt:P17 ?country"
           "FILTER NOT EXISTS { ?place wdt:P17 wd:Q142 . }"
           (service-wikibase "label" "fr"))
  :order-by "?countryLabel ?itemLabel")
 :lisp-format :alist)

;; 7.2 Timeline of mayors of Amsterdam
(wikidata-sparql-query
 (compose-sparql-query
  :select "?mayor ?mayorLabel ?start ?end"
  :where '("?mayor p:P39 ?position"
           "?position ps:P39 wd:Q13423495;
                      pq:P580 ?start;
                      pq:P582 ?end"
           (service-wikibase "label" "nl,en")))
 :lisp-format :alist)

;; 8. Economics and Business
;; -----------------------------------------------------------------------------
;; 8.1 Distinct billionaires
(wikidata-sparql-query
 (compose-sparql-query
  :select "?locationLabel ?item ?itemLabel (MAX(?billion) as ?billions)"
  :where '("?item wdt:P2218 ?worth"
           "?item wdt:P19 ?location"
           "FILTER(?worth>1000000000)"
           "BIND(?worth/1000000000 AS ?billion)"
           (service-wikibase "label"))
  :group-by "?locationLabel ?item ?itemLabel"
  :order-by "DESC(?billions)")
 :lisp-format :alist)

;; 8.2 Countries that have adopted a cryptocurrency as legal tender
(wikidata-sparql-query
 (compose-sparql-query
  :select "?country ?countryLabel ?currency ?currencyLabel"
  :where '("?country wdt:P31 wd:Q6256" ; Instances of country
           "?country wdt:P38 ?currency" ; Country has currency
           "?currency wdt:P31/wdt:P279* wd:Q13479982" ; Currency is instance or subclass of cryptocurrency
           (service-wikibase "label")))
 :lisp-format :alist)

;; 9. Science
;; -----------------------------------------------------------------------------
;; 9.1 The number of Wikidata items on Diseases and the percentage of those with a pointer to the Disease Ontology
(wikidata-sparql-query
 (compose-sparql-query
  :select "(COUNT(?disease) AS ?total) (SUM(?ref) AS ?byDO) (100*?byDO/?total AS ?percent)"
  :where '("{?disease wdt:P31 wd:Q12136 } UNION {?disease wdt:P279 wd:Q12136 .}"
           "OPTIONAL {
               ?disease p:P699 ?statement.
               BIND(1 AS ?ref).
               }"))
 :lisp-format :alist)

;; 9.2 Mosquito species
(wikidata-sparql-query
 (compose-sparql-query
  :select "?item ?taxonname"
  :where '("?item wdt:P31 wd:Q16521 ;
                  wdt:P105 wd:Q7432 ;
                  wdt:P171* wd:Q7367 ;
                  wdt:P225 ?taxonname")))

;; 9.3 Linguists with twitter accounts
(wikidata-sparql-query
 (compose-sparql-query
  :select-distinct "?personLabel (CONCAT(\"https://twitter.com/\",?twitterName) AS ?twitterlink) ?pic"
  :where '("?person wdt:P2002 ?twitterName ;
                    wdt:P106 ?occupation"
           "OPTIONAL { ?person wdt:P18 ?pic . }"
           "?occupation wdt:P279* wd:Q14467526" ; All subclasses of linguists
           (service-wikibase "label")))
 :lisp-format :alist)

;; 9.4 Software written in Go programming language
(wikidata-sparql-query
 (compose-sparql-query
  :select-distinct "?instance_of ?instance_ofDescription ?instance_ofLabel ?official_website"
  :where '((service-wikibase "label")
           "?instance_of (wdt:P31/(wdt:P279*)) wd:Q341"
           "OPTIONAL { ?instance_of wdt:P856 ?official_website. }"
           "?instance_of wdt:P277 wd:Q37227"))
 :lisp-format :alist)

;; 9.4 Websites with openAPI endpoints
(wikidata-sparql-query
 (compose-sparql-query
  :select "?database ?databaseLabel ?license ?licenseLabel ?value"
  :where '("?database ?p ?wds"
           "OPTIONAL { ?database wdt:P275 ?license }"
           "?wds ?v ?value"
           "?wdP wikibase:statementProperty ?v"
           "?wdP wikibase:claim ?p"
           "?wds pq:P31 wd:Q27075870"
           (service-wikibase "label"))
  :order-by "ASC(?databaseLabel)")
 :lisp-format :alist)

;; 9.5 100 chemical elements and their properties
(wikidata-sparql-query
 (compose-sparql-query
  :select "?elementLabel ?_boiling_point ?_melting_point ?_electronegativity ?_density ?_mass"
  :where '("?element wdt:P31 wd:Q11344"
           "?element wdt:P2102 ?_boiling_point"
           "?element wdt:P2101 ?_melting_point"
           "?element wdt:P1108 ?_electronegativity"
           "?element wdt:P2054 ?_density"
           "?element wdt:P2067 ?_mass"
           (service-wikibase "label"))
  :limit 100))

;; 9.6 Inventors killed by their own invention
(wikidata-sparql-query
 (compose-sparql-query
  :select "?inventor ?inventorLabel ?gadget ?gadgetLabel"
  :where '("?inventor wdt:P157 ?gadget"
           "?gadget wdt:P61 ?inventor"
           (service-wikibase "label")))
 :lisp-format :alist)

;; 10. Culture
;; -----------------------------------------------------------------------------
;; 10.1 Cathedrals in Paris
(wikidata-sparql-query
 (compose-sparql-query
  :select "?item ?itemLabel ?placeLabel ?coords ?image"
  :where '("?item wdt:P31 wd:Q2977"
           "?item wdt:P131 ?place"
           "?place wdt:P131 wd:Q90"
           (sparql-optional '("?item wdt:P625 ?coords"
                              "?item wdt:P18 ?image"))
           (service-wikibase "label" "fr"))
  :order-by "?placeLabel ?itemLabel")
 :lisp-format :alist)

;; 10.2 Museums in Antwerp with their coordinates
(wikidata-sparql-query
 (compose-sparql-query
  :select "?item ?itemLabel ?coordinates"
  :where '("?item wdt:P31/wdt:P279* wd:Q33506 ;
                  wdt:P131 wd:Q12892 ;
                  wdt:P625 ?coordinates"
           (service-wikibase "label" "nl, en")))
 :lisp-format :alist)

;; 10.3 Winner of the Oscars by award and time
(wikidata-sparql-query
 (compose-sparql-query
  :select-distinct "?item ?itemLabel ?awardLabel ?time"
  :where '("?item wdt:P106/wdt:P279* wd:Q3455803 ;
                  p:P166 ?awardStat"
           "?awardStat pq:P805 ?award ;
                       ps:P166 wd:Q103360"
           "?award wdt:P585 ?time"
           (service-wikibase "label"))
  :order-by "DESC(?time)")
 :lisp-format :alist)

;; 10.4 Movies with Gal Gadot
(wikidata-sparql-query
 (compose-sparql-query
  :SELECT "?item ?itemLabel (MIN(?date) AS ?firstReleased) ?_image"
  :where '("?item wdt:P161 wd:Q185654;
                  wdt:P577 ?date"
           (service-wikibase "label")
           "OPTIONAL { ?item wdt:P18 ?_image. }")
  :group-by "?item ?itemLabel ?_image"
  :order-by "(?date)")
 :lisp-format :alist)

;; 10.5 Contemporary French actresses
(wikidata-sparql-query
 (compose-sparql-query
  :select "?item ?itemLabel ?itemDescription (SAMPLE(?img) AS ?image) (SAMPLE(?dob) AS ?dob) ?sl"
  :where '("?item wdt:P106 wd:Q33999 ;
                  wdt:P27 wd:Q142 ;
                  wdt:P21 wd:Q6581072"
           "MINUS { ?item wdt:P570 [] }"
           (sparql-optional '("?item wdt:P18 ?img"
                              "?item wdt:P569 ?dob"
                              "?item wikibase:sitelinks ?sl"))
           (service-wikibase "label" "fr"))
  :group-by "?item ?itemLabel ?itemDescription ?sl"
  :order-by "DESC(?sl)")
 :lisp-format :alist)

;; 10.6 All James Bond performers
(wikidata-sparql-query
 (compose-sparql-query
  :select "?performer ?performerLabel"
  :where '("?bond wdt:P31 wd:Q4359230"
           "?bond rdfs:label \"James Bond\"@en"
           "?bond p:P175 / ps:P175 ?performer"
           (service-wikibase "label")))
 :lisp-format :alist)

;; 10.7 Albums by U2 ordered chronologically
(wikidata-sparql-query
 (compose-sparql-query
  :SELECT "?album ?albumLabel ?publication_date"
  :where '("VALUES ?performer { wd:Q396 }"
           "?album wdt:P31 wd:Q482994;
                   wdt:P175 ?performer ;
                   wdt:P577 ?publication_date"
           (service-wikibase "label"))
  :order-by "(?publication_date)")
 :lisp-format :alist)

;; 10.7 Paintings depicting the goddess Venus
(wikidata-sparql-query
 (compose-sparql-query
  :select "?item ?itemLabel ?object ?objectLabel ?image"
  :where '("?item wdt:P31/wdt:P279* wd:Q3305213" ; Paintings 
           "?item wdt:P180 ?object" ; Depicts an object
           "?object wdt:P279* wd:Q47652" ; Venus
           "?item wdt:P18 ?image"
           (service-wikibase "label")))
 :lisp-format :alist)

;; 11. Food & Drink
;; -----------------------------------------------------------------------------
;; 11.1 Belgian breweries and their coordinates
(wikidata-sparql-query
 (compose-sparql-query
  :select "?breweryLabel ?breweryDescription ?coord"
  :where '("?brewery wdt:P31/wdt:P279* wd:Q131734 ;
                     wdt:P17 wd:Q31 ;
                     wdt:P625 ?coord"
           (service-wikibase "label" "nl,fr,de,en")))
 :lisp-format :alist)

;; 12. Some complex queries
;; -----------------------------------------------------------------------------
;; 12.1 Find a mother and her son in The Terminator universe
(wikidata-sparql-query
 (compose-sparql-query
  :select "*"
  :where '("?p wdt:P1080 wd:Q620588" ; from fictional universe: Terminator
           "?p rdfs:label ?pl"
           "FILTER (lang(?pl) = \"en\")"
           "?p wdt:P25 ?m" ; ?m is mother of ?p
           "?m rdfs:label ?ml"
           "FILTER (lang(?ml) = \"en\")"))
 :lisp-format :alist)

;; 12.2 Popular surnames among fictional characters
(wikidata-sparql-query
 (compose-sparql-query
  :select "?surname ?surnameLabel ?count"
  :where '((format nil "{ ~a }"
                   (compose-sparql-query
                    :select "?surname (COUNT(?person) AS ?count)"
                    :where '("?person (wdt:P31/wdt:P279*) wd:Q95074"
                             "?person wdt:P734 ?surname")
                    :group-by "?surname"))
           (service-wikibase "label"))
  :order-by "DESC(?count)"
  :limit 100)
 :lisp-format :alist)
