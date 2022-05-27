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
