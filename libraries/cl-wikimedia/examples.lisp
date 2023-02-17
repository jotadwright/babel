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

;; First set you user-agent to your bot or e-mail address!!!
(setf *user-agent* "REPLACE WITH YOUR BOT NAME or EMAIL ADDRESS")

;;=========================================================================
;; Wikimedia ACTION API
;;=========================================================================

;; Default behaviour (Alist):
(wikimedia-action-api "http://en.wikipedia.org/w/api.php"
                      :action "query"
                      :list "search"
                      :srsearch "Fluid construction grammar")

;; Example that returns a hash-table:
(wikimedia-action-api "http://en.wikipedia.org/w/api.php"
                      :action "query"
                      :list "search"
                      :srsearch "Luc Steels"
                      :lisp-format :hash-table)

;; Example for Wikidata:
(wikimedia-action-api "https://wikidata.org/w/api.php"
                      :action "query"
                      :list "search"
                      :srsearch "Venus")

;; Querying Wikipedia
;; ------------------------------------------------------------------------
;; Search
(wikipedia-query :list "search"
                 :srsearch "Luc Steels"
                 :language "fr")

;; Alternative search convenience:
(wikipedia-search "Luc Steels")
(wikipedia-search "Luc Steels" :language "fr")

;; Getting properties from pages:
(wikipedia-query :titles "Wonder_Woman"
                 :prop "categories|images")

;; Getting aggregations of data with list
(wikipedia-query :list "pagepropnames" ;; Return all page property names
                 :titles "Luc_Steels")

;; First ten pages of the linguistics category:
(wikipedia-query :list "categorymembers" ;; Return pages that match this criterion
                 :cmtitle "Category:Linguistics")

;; First 500 pages of the linguistics category (max)
(wikipedia-query :list "categorymembers" ;; Return pages that match this criterion
                 :cmtitle "Category:Linguistics"
                 :cmlimit "500")

;; Querying Wikidata (will only work if you have set your user-agent!!)
;; ------------------------------------------------------------------------
(wikidata-query :titles "Q47652")
(wikidata-query :list "search"
                :srsearch "Venus")
(wikidata-search "Venus")

;; Requesting parameter information
;; ------------------------------------------------------------------------

;; Modules:
(wikimedia-paraminfo :modules "parse")
(wikimedia-paraminfo :modules "parse|query+info")


;; Parsing Pages
;; ------------------------------------------------------------------------
;; Using the macro directly:
;; Page names:
(wikipedia-parse :page "Luc_Steels" :language "nl")
(wikipedia-parse :pageid "3276454")
(wikipedia-parse :page "Luc_Steels" :prop "text")
(wikipedia-parse :page "Luc_Steels" :prop "images")
(wikipedia-parse :page "Luc_Steels" :prop "properties")

(wikipedia-parse-summary "Gal_Gadot")

;; Getting a page summary
;; ------------------------------------------------------------------------
(wikimedia-summary "Gal Gadot")
(wikimedia-summary "Gal Gadot" :lisp-format :alist)

;; If you store the summary as a hash-table, you can use
;; convenient helper functions
(defparameter *my-wikimedia-summary* (wikimedia-summary "Venus (mythology)"))
(wikimedia-summary-title *my-wikimedia-summary*)
(wikimedia-summary-extract *my-wikimedia-summary*)
(wikimedia-summary-extract-html *my-wikimedia-summary*)
(wikimedia-summary-thumbnail *my-wikimedia-summary*)

;; Wikidata-specific helper functions
;; ------------------------------------------------------------------------
(wikidata-get-entity "Q47652")
(wikidata-get-entity-statements "Q47652")
(wikidata-get-statement "Q47652$1491D330-19A6-4344-A752-2C8175D8C23A")

;; If you store the wikidata entity information as a hash-table, you can use
;; convenience functions (prefix wd-):
(defparameter *my-wikidata-entity* (wikidata-get-entity "Q47652" :lisp-format :hash-table))
(wd-entity-id *my-wikidata-entity*)
(wd-entity-description *my-wikidata-entity* "en")
(wd-entity-aliases *my-wikidata-entity* :return-as-list-p t)
(wd-entity-wikipedia-title *my-wikidata-entity* "en")

;; Combining wikidata with wikipedia information
;; ------------------------------------------------------------------------
(wd-entity-wikipedia-summary *my-wikidata-entity* "en")
(wd-entity-wikipedia-summary *my-wikidata-entity* "en" :lisp-format :alist)

;; Performaing a SPARQL query
;; ------------------------------------------------------------------------

(wikidata-sparql-query
 ;; Return 1 URI whose English label is "Venus and Cupid" and which has 
 ;; a Creator statement whose English label is "Lorenzo Lotto"
 "SELECT ?uri
   WHERE { 
   ?uri rdfs:label \"Venus and Cupid\"@en.
   ?uri rdfs:label ?itemLabel.
   ?uri p:P170 ?statement.
   ?statement ps:P170 ?creator.
   ?creator rdfs:label \"Lorenzo Lotto\"@en.
 } limit 1"
 :lisp-format :alist)