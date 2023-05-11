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

;; Note: this package contains limited sparql-support. For a complete implementation,
;;       see cl-sparql (https://quickref.common-lisp.net/cl-sparql.html)

; --------------------------------------------------------------------------------------------------------
;; SPARQL is the standard query language and protocol for Linked Open Data  
;; on the web or for RDF triplestores: https://www.ontotext.com/knowledgehub/fundamentals/what-is-sparql/
;; 
;; Learn more about how SPARQL works here:
;; https://www.w3.org/TR/rdf-sparql-query/
;; --------------------------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------ 
;; Basics
;; ------------------------------------------------------------------------ 
(export '(*wikidata-sparql-endpoint*
          wikidata-sparql-query
          cl-sparql-query))

(defparameter *wikidata-sparql-endpoint* "https://query.wikidata.org/sparql")

(defmacro cl-sparql-query (query &rest parameters
                                 &key &allow-other-keys)
  (destructuring-bind (&whole whole
                              &key 
                              (sparql-endpoint "http://www.sparql.org/sparql")
                              (lisp-format :hash-table)
                              (content-type "application/x-www-form-urlencoded")
                              &allow-other-keys)
      parameters
    (dolist (indicator '(:sparql-endpoint :lisp-format :content-type))
      (remf whole indicator))
    `(wikimedia-action-api ,sparql-endpoint
                           :query ,query
                           :content-type ,content-type
                           :lisp-format ,lisp-format
                           ,@whole)))

(defmacro wikidata-sparql-query (query 
                                 &rest parameters
                                 &key &allow-other-keys)
  `(cl-sparql-query ,query 
                    :sparql-endpoint *wikidata-sparql-endpoint*
                    ,@parameters))

;; ------------------------------------------------------------------------ 
;; Some help for composing sparql queries
;; ------------------------------------------------------------------------ 

(export '(sparql-wikidata-property-id
          sparql-wikidata-item-is-instance-of
          wikidata-format-statement-query
          wikidata-format-subject-with-label
          compose-sparql-query
          service-wikibase
          sparql-optional))

;; You can write useful templates of full queries:
;; ------------------------------------------------------------------------ 
(defun sparql-wikidata-property-id (label &optional (language "en"))
  "Given the label of a property, check what its property ID is."
  (format nil
          "SELECT DISTINCT ?id 
             WHERE {
               ?id a wikibase:Property .
               ?id rdfs:label ~s@~a }
             LIMIT 1"
          label language))
;; (sparql-wikidata-property-id "depicts")
;; (wikidata-sparql-query (sparql-wikidata-property-id "depicts"))

(defun sparql-wikidata-item-is-instance-of (parent-id &optional (language "en"))
  (format nil 
          "SELECT ?item ?itemLabel 
           WHERE 
           { ?item wdt:P31 wd:~a.
             SERVICE wikibase:label { bd:serviceParam wikibase:language \"[AUTO_LANGUAGE],~a\". } }"
          parent-id language))
;; All instances of a housecat:
;; (sparql-wikidata-item-is-instance-of "Q146")
;; (wikidata-sparql-query (sparql-wikidata-item-is-instance-of "Q146"))

;; Or compose them in a more open-ended way:
;; ------------------------------------------------------------------------ 
(defun sparql-prefix (&rest declarations)
  (format nil "~{PREFIX ~a~^~%~}"
          (if (listp (first declarations))
            (first declarations)
            declarations)))
; (sparql-prefix "foo: <http://example.com/resources/>")

(defun sparql-select (&rest variables)
  (format nil "SELECT ~{~a ~}"
          (if (listp (first variables))
            (first variables)
            variables)))
; (sparql-select "?subject" "?predicate")

(defun sparql-select-distinct (&rest variables)
  (format nil "SELECT DISTINCT ~{~a ~}"
          (if (listp (first variables))
            (first variables)
            variables)))
; (sparql-select-distinct "?subject ?predicate")

(defun sparql-where (list-of-constraints)
  (format nil "WHERE {
     ~{~a .~^~%     ~}
     }"
          (loop for constraint in list-of-constraints
                collect (if (stringp constraint)
                          constraint
                          (eval constraint)))))
; (sparql-where '("?subject ?predicate ?object" "?subject rdfs:label ?label"))

(defun sparql-with (constraints as)
  (format nil "WITH { ~a } AS ~a"
          constraints as))
;; (sparql-with "constraints" "%1")

(defun compose-sparql-query (&key prefix
                                  with
                                  from
                                  select select-distinct
                                  (where "")
                                  limit
                                  group-by
                                  order-by)
  (format nil "~a~%~a~%~a~%~a~%~a~%~a~%~a~%~a"
          (if prefix (sparql-prefix prefix) "")
          (if with (sparql-with with) "")
          (cond (select (sparql-select select))
                (select-distinct (sparql-select-distinct select-distinct))
                (t
                 (error "Please specify which variables to select")))
          (if from (format nil "FROM <~a>" from) "") 
          (sparql-where where)
          (if group-by (format nil "GROUP BY ~a" group-by) "")
          (if order-by (format nil "ORDER BY ~a" order-by) "")
          (if limit (format nil "LIMIT ~a" limit) "")))

;; Useful helpers:
(defun service-wikibase (service &optional (default-languages "en"))
  (format nil "SERVICE wikibase:~a { bd:serviceParam wikibase:language \"[AUTO_LANGUAGE],~a\". }" 
          service default-languages))
; (service-wikibase "label")

(defun sparql-optional (list-of-constraints)
  (format nil "   ~{OPTIONAL{~a .}~%       ~}" list-of-constraints))
; (sparql-optional '("?subject ?predicate ?object" "?subject rdfs:label ?itemLabel"))

(defun wikidata-format-statement-query (property-id
                                        &key (subject "?subject")
                                        (statement "?statement")
                                        (object "?object"))
  (format nil "   ~a p:~a ~a.
       ~a ps:~a ~a."
          subject property-id statement
          statement property-id object))
; (wikidata-format-statement-query "P170" :subject "?uri" :object "?creator")

(defun wikidata-format-subject-with-label (label
                                           &key (subject "?subject")
                                           (language "en"))
  (format nil "   ~a rdfs:label ~s@~a."
          subject label language))
; (wikidata-format-subject-with-label "Venus")

#|
;; ----------------------------------------------------------------
;; EXAMPLES (see wikidata-sparql-examples.lisp for more)
;; ----------------------------------------------------------------
;;
;; WIKIDATA:
;; ---------
;; Return all labels of entities that are instances of (P31) a housecat (Q146):

(wikidata-sparql-query
 (compose-sparql-query
  :select '("?item" "?itemLabel")
  :where '("?item wdt:P31 wd:Q146"
           (service-wikibase "label"))))

;; Generic examples:
;; -----------------
(cl-sparql-query
 (compose-sparql-query
  :prefix "foaf: <http://xmlns.com/foaf/0.1/>"
  :from "http://www.w3.org/People/Berners-Lee/card"
  :select "?name"
  :where '("?person foaf:name ?name")))

(cl-sparql-query
 (compose-sparql-query
  :prefix '("foaf: <http://xmlns.com/foaf/0.1/>"
            "card: <http://www.w3.org/People/Berners-Lee/card#>")
  :select "?homepage"
  :from "http://www.w3.org/People/Berners-Lee/card"
  :where '("card:i foaf:knows ?known"
           "?known foaf:homepage ?homepage")))

;; DBPedia:
;; --------
(cl-sparql-query
 (compose-sparql-query
  :prefix '("rdfs: <http://www.w3.org/2000/01/rdf-schema#>"
            "type: <http://dbpedia.org/class/yago/>"
            "prop: <http://dbpedia.org/property/>")
  :select '("?country_name" "?population")
  :where '("?country a type:LandLockedCountries ;
                     rdfs:label ?country_name ;
                     prop:populationEstimate ?population"
           "FILTER (?population > 15000000)"))
 :sparql-endpoint "http://dbpedia.org/sparql")
|#