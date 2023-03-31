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

; --------------------------------------------------------------------------------------------------------
;; SPARQL is the standard query language and protocol for Linked Open Data  
;; on the web or for RDF triplestores: https://www.ontotext.com/knowledgehub/fundamentals/what-is-sparql/
;; 
;; Learn more about how SPARQL works here:
;; https://www.w3.org/TR/rdf-sparql-query/
;; --------------------------------------------------------------------------------------------------------

;; Basics
;; ------------------------------------------------------------------------ 
(export '(*wikidata-sparql-endpoint*
          wikidata-sparql-query))

(defparameter *wikidata-sparql-endpoint* "https://query.wikidata.org/sparql")

(defun wikidata-sparql-query (query &key (lisp-format :hash-table))
  "Perform a sparql-query on wikidata."
  (wikimedia-action-api *wikidata-sparql-endpoint*
                        :query query
                        :content-type "application/x-www-form-urlencoded"
                        :lisp-format lisp-format))

;; Examples of how to store query templates
;; ------------------------------------------------------------------------ 

(export '(sparql-wikidata-property-id
          sparql-wikidata-item-is-instance-of))

(defun sparql-wikidata-property-id (label &optional (language "en"))
  "Given the label of a property, check what its property ID is."
  (format nil
          "SELECT DISTINCT ?property 
             WHERE {
               ?property a wikibase:Property .
               ?property rdfs:label ~s@~a }
             LIMIT 1"
          label language))
;; (sparql-wikidata-property-id "depicts")
;; (wikidata-sparql-query (sparql-wikidata-property-id "depicts"))

;; Simple queries:
(defun sparql-wikidata-item-is-instance-of (parent-id &optional (language "en"))
  (format nil 
          "SELECT ?item ?itemLabel 
           WHERE 
           { ?item wdt:P31 wd:~a.
             SERVICE wikibase:label { bd:serviceParam wikibase:language \"[AUTO_LANGUAGE],~a\". } }"
          parent-id language))
;; Get all instances of a house cat:
;; (sparql-wikidata-item-is-instance-of "Q146")
;; (wikidata-sparql-query (sparql-wikidata-item-is-instance-of "Q146"))
